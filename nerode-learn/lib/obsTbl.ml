open Stdlib
open Nerode

module type WordSig = sig
  (** [t] is the representation type for words, aka w*)
  type t

  (** [compare] . It is (potentially) necessary depending on the 
  implementation of the table*)
  val compare : t -> t -> int

  (**[epsilon] is the empty string ε*)
  val epsilon : t

  (**[suffixes w] takes a word [w] and returns a list of the suffixes of [w],
  with the suffixes being type Word.t. 
  For example, suffixes HELLO would return [HELLO; ELLO; LLO; LO; O; []]*)
  val suffixes : t -> t list 

  (**[append_letter l w] returns the word [w] with the letter [l] attached to the back
  E.g., append_letter HELL O = HELLO*)
  val append_letter : t -> Alphabet.symbol -> t

  (**[concat w1 w2] return the concatenation of [w1] and [w2], i.e. [w1+w2].
  For example, [concat one two] is onetwo*)
  val concat : t -> t -> t

  (*creates a word from a list of ints, where each int is a letter in the word,
  and the letters are in the same order as the word*)
  val of_intlist : int list -> t  

  val to_intlist : t -> int list
  val to_string : t -> string
  val to_symlist : t -> Alphabet.symbol list
  val of_symlist : Alphabet.symbol list -> t
end

module Word : WordSig = struct
  (**AF: A word [Word.t] is implemented as an symbol list, with each symbol 
  representing a letter in the word, with the same order in the list as in the word.*)
  type t = Alphabet.symbol list
  let compare = List.compare Alphabet.compare
  let epsilon = []
  let suffixes w = 
    List.fold_right (fun l acc -> (l::List.hd acc)::acc) w [epsilon]
  let append_letter w (l : Alphabet.symbol) : t = w @ [l]
  let concat w1 w2 = w1 @ w2  
  let of_intlist lst = List.map Alphabet.sym_of_int lst
  let to_intlist (w: t) = List.map Alphabet.sym_to_int w
  let to_string (w: t) = if w = epsilon then "ε" else List.fold_left 
    (fun acc l -> acc^(l |> Alphabet.sym_to_int |> string_of_int)) "" w
  let to_symlist = Fun.id
  let of_symlist = Fun.id
end

module WordSet = Set.Make(Word)
module WordMap = Map.Make(Word)
module RowLabels = WordSet
module ColLabels = WordSet

module type TableSig = sig
  type entry =
    | True
    | False
    | Blank
  val compare : entry -> entry -> int

  type t
  
  (**[upper_rows tbl] returns the upper rows (specifically the row labels, not the entries) 
  in table [tbl]*)
  val upper_row_labels : t -> RowLabels.t

  (**[max_upper_row_label tbl] returns the upper row label of [tbl] which is
  lexicographically greatest. *)
  val max_upper_row_label : t -> Word.t

  (**[lower_rows] returns the unique set containing all possible
  one-letter extensions of the words/row labels in [upper_rows], minus those already
  in [upper_rows]*)
  val lower_row_labels : t -> RowLabels.t

  (**[cols tbl] returns the columns (specifically the column labels, not the entries) 
  in table [tbl]*)
  val col_labels : t -> ColLabels.t

  (**[get_blanks tbl] returns the set of words whose corresponding entries in
  the table [tbl] are blanks*)
  val get_blanks : t -> WordSet.t

  (**[lookup_by_word_opt w t] returns [Some entry] corresponding to word [w] in 
  table [t] if there is an entry for [w], or [None] if there is no entry for 
  [w] in [t]*)
  val lookup_by_word_opt : Word.t -> t -> entry option
  
  (**[lookup_by_word_opt w t] returns [Some entry] in row [row] and column [col]
  in table [t] if there is an entry in row [row] and column [col], or [None] if 
  there is no entry for row [row] and column [col] in [t]*)
  val lookup_by_rowcol_opt : Word.t -> Word.t -> t -> entry option

  (**[add_row get_entry w tbl] takes a label of a row [w] and adds the 
  corresponding entries for the row in each of the columns 
  in the table [tbl], returning the table with the added entries. The new table
  also includes the one-letter extensions of [w] to the bottom rows 
  (if not already in the upper rows) as well as the corresponding entries for 
  those rows if they are not already in the table. It takes in a
  function [get_entry] that returns a corresponding entry for a given word*)
  val add_row : (Word.t -> entry) -> entry WordMap.t -> Word.t -> t -> t * entry WordMap.t

  (**[add_col get_entry w tbl] returns a table containing everything from [tbl]
  plus the column with label [w] and the corresponding entries for the column
  in each of the (upper and lower) rows in [tbl]. It takes in a 
  function [get_entry] that returns a corresponding entry for a given word*)
  val add_col : (Word.t -> entry) -> entry WordMap.t -> Word.t -> t -> t * entry WordMap.t
  
  (**[add_cols get_entry col_lst tbl] returns a table containing everything 
  from [tbl] plus the columns with label l for each l in [col_lst] as well as 
  the corresponding entries for each column in each of the (upper and lower)
  rows in [tbl]. It takes in a function [get_entry] that returns a 
  corresponding entry for a given word*)
  val add_cols : (Word.t -> entry) -> entry WordMap.t -> ColLabels.t -> t -> t * entry WordMap.t

  (**[row_entries row table] returns a list of the entries in the row with 
  label [row] in [table]. If the row is not in the table, an error is raised*)
  val row_entries : Word.t -> t -> entry list 

  (**[up_rows_labels_entries table] returns a list of unique pairs, where each 
  pair is a row label in upper row of [table] and a list of the entries in that 
  row in the same order as columns in the table. *)
  val up_rows_labels_entries : t -> (Word.t * entry list) list

  (**[low_rows_labels_entries table] returns a list of unique pairs, where each 
  pair is a row label in lower rows of [table] and a list of the entries in that
  row in the same order as columns in the table.*)
  val low_rows_labels_entries : t -> (Word.t * entry list) list
  
  (*[closed table] returns [None] if the [table] is closed, otherwise returns 
  [Some lower_row_label] whose row should be added to the upper rows 
  (the lower row is not similar to any of the upper rows)*)
  val closed : t -> Word.t option

  (**[similarity table] gives a _heuristic_ measure of the similarity of its rows.
  Lower numbers mean _less_ similar, with 0 indicating that all distinctions
  are supported by evidence strings*)
  val similarity : t -> int

  (**[filled_blanks table map] returns a table with the contents of [table], 
  except that the blanks in the table 
  Precondition: [map] must contain a binding for a word to a non-blank entry 
  for every word that has a corresponding blank in [table], and no other keys
  can exist in [map]*)
  val fill_blanks : t -> entry Map.Make(Word).t -> t

  (**[init_epsilon get_entry] initializes a table with a function [get_entry]
  mapping words to their corresponding entry, returning a 1x1 table with 
  row ε and column ε, thus having one entry corresponding the string ε*)
  val init_epsilon : (Word.t -> entry) -> t * (entry WordMap.t)
  
  (** [print_table t] prints the table [t]:
  The column labels are in the horizontal axis in lexicographic order.
  The upper row labels are in the vertical axis first, in lexicogrphic order.
  The lower row labels are in the vertical axis next, separated from the upper
  row by "---", in lexicogrphic order.
  The entry in row with label r and column with label c in the table is the entry 
  corresponding to the word r^c, or rc*)
  val print_table : t -> unit

  (*Precondition: the table cannot contain any blank entries*)
  val to_dfa : t -> Dfa.t

  val equivalent_pairs : t -> (Word.t * Word.t) list

  (*val consistent : t -> w option
  val find_blank_from : t -> w list -> w option
  val find_blank_opt : t -> w option*)
end

module Table : TableSig = struct
  
  type entry =
    | True
    | False
    | Blank

  let compare e1 e2 =
    match e1, e2 with
    | True, True
    | False, False
    | Blank, _
    | _, Blank -> 0
    | True, _ -> 1
    | _, True -> -1

  let string_of_entry = function
    | True -> " + "
    | False -> " - "
    | Blank -> "[ ]"

  let similar b1 b2 = compare b1 b2 = 0
  
  (*NOTE: is this module necessary?*)
  module RowEntries = struct include List type t = entry list end

  let identical = RowEntries.equal ( = ) (*check to confirm that this is right*)

  module TblMap = WordMap

  (*AF: the table is a record, where [upper_rows] contains the row labels of the table,
  [cols] contains the column labels of the table, and [tbl] is a map matching 
  words to entries of type entry, where the word is a prefix-suffix combination 
  of a row and column label in the table, and the corresponding entry describes 
  the entry for that word in the table. 
  [lower_rows] are all the possible one-letter extensions 
  of the words in [upper_rows], not including those extensions that are already 
  in [upper_rows]. The entries along its rows are also stored in [tbl]. 
  RI: For all S in [upper_rows] and [lower_rows] and for all E in [cols], 
  there is a corresponding word with an entry assigned to it in [tbl] for the 
  prefix-suffix concatenation/word SE; conversely, for every word W with an 
  entry in [tbl], there exists an S in [upper_rows] and E in [cols] such that W = SE, 
  aka the concatenation of S and E. 
  Additionally, [lower_rows] must be the unique set containing all possible
  one-letter extensions of the words/row labels in [upper_rows], minus those already
  in [upper_rows].*)
  type t = {
    upper_row_labels : RowLabels.t;
    lower_row_labels : RowLabels.t;
    col_labels : ColLabels.t; (*Columns are maintained in lexicographical order currently*)
    tbl : entry TblMap.t} 
  
  let upper_row_labels tbl = tbl.upper_row_labels
  let max_upper_row_label tbl = RowLabels.max_elt tbl.upper_row_labels
  let col_labels tbl = tbl.col_labels
  let lower_row_labels tbl = tbl.lower_row_labels
  let lookup_by_word_opt w table = TblMap.find_opt w table.tbl
  let lookup_by_rowcol_opt rowlbl collbl table = 
    lookup_by_word_opt (Word.concat rowlbl collbl) table
  
  let get_blanks table : WordSet.t = 
    TblMap.(filter (fun _ e -> e = Blank) table.tbl |> bindings) 
    |> List.split |> fst |> WordSet.of_list

  (*helper for [add_rows] and [init_epsilon]*)
  let rows_from_adding_letter row = 
    (*accumulating possible words from adding a letter (either 0 or 1)
    to a given word [row]*)
    List.fold_left (fun acc a -> 
      let new_w = Word.append_letter row (Alphabet.sym_of_int a) in
      RowLabels.add (new_w) acc) RowLabels.empty [0;1]

  (*helper for [add_row] and [add_col]*)
  let add_entries get_entry e_map pre_and_suf (table : t) = 
    let tbl', e_map' = List.fold_left (fun (tblacc, em_acc) w -> 
      match TblMap.find_opt w em_acc with
        | Some entry -> TblMap.add w entry tblacc, em_acc
        | None -> let w_ent = get_entry w in
                  TblMap.add w w_ent tblacc, TblMap.add w w_ent em_acc
      ) (table.tbl, e_map) pre_and_suf in
    {table with tbl = tbl'}, e_map'

  (*Implementation feels goofy, 
  aka converting all the encapsulated stuff into lists and then operating on that,
  not sure if there's something nicer/cleaner that I'm missing.*)
  let add_row get_entry e_map (w: Word.t) (table: t) : t * entry TblMap.t = 
    let col_lst = ColLabels.elements table.col_labels in
    let u_r_ls = table.upper_row_labels in
    let new_lowers = RowLabels.(rows_from_adding_letter w (*new possible additions to lower_labels*)
      |> filter (fun sa -> not (mem sa u_r_ls))) in(*filtering out words already in upper_labels*)
    let new_l_r_l = (*new lower_rows_labels, add new lowers to old and remove [w] from it*)
      RowLabels.(union table.lower_row_labels new_lowers |> remove w) in
    let tbl_newrows = {table with 
      upper_row_labels = RowLabels.add w u_r_ls; 
      lower_row_labels = new_l_r_l} in
    let pre_and_suf = (*list of all possible new rowlabel-column label combinations to add to table*)
      (*Something to think about: do we need to include [w] here if 
        the new row [w] is already in lower_rows and just being moved up
        to upper rows, and therefore doesn't need its entries put in?
        Answer: still need to include, don't know how client will use function
        and if they'll only add rows already in lower_rows*)
      let new_rs = RowLabels.add w new_lowers in
      RowLabels.fold (fun row lstacc -> 
        lstacc @ (List.map (fun col -> Word.concat row col) col_lst)) new_rs [] 
    in
    add_entries get_entry e_map pre_and_suf tbl_newrows
  
  let add_col get_entry e_map (w: Word.t) (table: t) : t * entry TblMap.t = 
    let all_rows_lst = 
      RowLabels.(union table.upper_row_labels table.lower_row_labels |> elements) in
    let pre_and_suf = 
      List.map (fun row -> Word.concat row w) all_rows_lst in
    let tbl' = {table with col_labels = ColLabels.add w table.col_labels} in
    add_entries get_entry e_map pre_and_suf tbl'
  
  let add_cols get_entry e_map (new_cols: ColLabels.t) table = 
    ColLabels.fold (fun w (tblacc, em_acc) -> add_col get_entry em_acc w tblacc) new_cols (table, e_map)

  (**Initializes the 1x1 table with S and E as both just epsilon, 
  so the only entry is for the empty string*)
  let init_epsilon get_entry : t * entry TblMap.t = 
    let s : RowLabels.t = RowLabels.(empty |> add Word.epsilon) in
    let sa : RowLabels.t = rows_from_adding_letter Word.epsilon in
    let e : ColLabels.t = ColLabels.(empty |> add Word.epsilon) in
    let tbl : entry TblMap.t = RowLabels.(fold (fun w tblacc ->
      TblMap.add w (get_entry w) tblacc) (union s sa) TblMap.empty) in
    {upper_row_labels = s; 
    lower_row_labels = sa; 
    col_labels = e; 
    tbl = tbl}, tbl

  (**[row_entries row table] returns a list of the entries in the row with 
  label [row] in [table]. If the row is not in the table, an error is raised*)
  let row_entries (row: Word.t) table : RowEntries.t =
    table.col_labels |> ColLabels.elements 
      |> List.map (fun col -> lookup_by_rowcol_opt row col table |> Option.get)
  
  let rows_labels_entries rowlbls table : (Word.t*RowEntries.t) list = 
    RowLabels.fold 
      (fun row acc -> (row, row_entries row table)::acc) rowlbls []

  (**[up_row_labels_entries table] returns a list, whose elements are a pair
  containing a row's label and a list of its entries, for each upper row in [table]
  NOTE: could be made into set instead*)
  let up_rows_labels_entries table : (Word.t*RowEntries.t) list = 
    rows_labels_entries table.upper_row_labels table

  let low_rows_labels_entries table = 
    rows_labels_entries table.lower_row_labels table
  
  let rows_similar rlbl1 rlbl2 t = 
    RowEntries.equal similar (row_entries rlbl1 t) (row_entries rlbl2 t)


(*---------- Similarity heuristic functions: -------------------------------- *)
  (* These functions give heuristics for estimating how likely S is to contain
     a redundant member. Low similarity is *not likely* to have a redundant row *)
  let rows_similarity init rlbl1 rlbl2 t =
    let r1 = row_entries rlbl1 t in
    let r2 = row_entries rlbl2 t in
    if rows_similar rlbl1 rlbl2 t then
      RowEntries.fold_left2 (fun a e1 e2 ->
        a +  match e1,e2 with
             | True, True
             | False, False -> 1
             | Blank, _
             | _, Blank -> 0
             | _, _ -> failwith "Impossible for similar rows") init r1 r2
    else
      0

  let allpairs_similarity (tbl:t) : int =
    RowLabels.fold (fun r1 a1 ->
      RowLabels.fold (fun r2 a2 ->
        if r1 < r2 then
          a2 + rows_similarity 0 r1 r2 tbl
        else
          a2
      ) tbl.upper_row_labels a1) tbl.upper_row_labels 0

  let blanks_count (tbl: t) : int =
    RowLabels.fold (fun r a1 ->
      ColLabels.fold (fun e a2 ->
        a2 + match lookup_by_rowcol_opt r e tbl with
             | None -> failwith "table entry does not exist"
             | Some Blank -> 1
             | _ -> 0
      ) tbl.col_labels a1
    ) tbl.upper_row_labels 0

  let blank_word_count (tbl: t) : int =
    let blank_words = RowLabels.fold (fun r a1 ->
      ColLabels.fold (fun e a2 ->
        WordSet.union a2 (match lookup_by_rowcol_opt r e tbl with
                          | None -> failwith "table entry does not exist"
                          | Some Blank -> WordSet.singleton (Word.concat r e)
                          | _ -> WordSet.empty)
      ) tbl.col_labels a1
    ) tbl.upper_row_labels WordSet.empty in
    WordSet.fold (fun w a -> a + match lookup_by_word_opt w tbl with
                                 | None -> failwith "table entry does not exist"
                                 | Some Blank -> 1
                                 | _ -> 0 ) blank_words 0


  let last_similarity (tbl:t) : int =
    let m = max_upper_row_label tbl in
    let sim s a =
      max (rows_similarity 0 m s tbl) a in
    RowLabels.fold sim tbl.upper_row_labels (-1)

  let similarity = allpairs_similarity
(*--------------------------------------------------------------------------- *)

  (*returns None if closed, otherwise Some lower row label which is not similar
  with any of the upper rows*)
  (*NOTE: not yet tested, consider renaming to be more clear?*)
  let closed table: Word.t option =
    let not_in_s sa = table.upper_row_labels 
      |> RowLabels.for_all (fun s -> not (rows_similar s sa table)) in 
    table.lower_row_labels|> RowLabels.elements |> List.find_opt not_in_s
  
  let fill_blanks table (map: entry TblMap.t): t = 
    let tbl' = TblMap.merge (fun _ mape tble -> 
        match mape, tble with
        | None, None -> None
        | _, None -> failwith "tried to fill word that doesn't exist in table"
        | Some Blank, _ -> failwith "tried to fill a blank with a blank"
        | Some entry, Some Blank -> Some entry
        | None, o -> o
        | _, _ -> failwith "tried to fill/replace a non-blank entry") 
      map table.tbl in
    {table with tbl = tbl'}

  let print_row rlbl table = 
    Word.to_string rlbl |> Printf.printf "%s ";
    RowEntries.iter 
      (fun e -> string_of_entry e |> Printf.printf "%s ") 
      (row_entries rlbl table);
    Printf.printf "\n%!"
  
  let print_table table = 
    ColLabels.iter 
      (fun clbl -> Word.to_string clbl |> Printf.printf " %s") table.col_labels;
    Printf.printf "\n%!";
    RowLabels.iter (fun w -> print_row w table) table.upper_row_labels;
    Printf.printf "---\n%!";
    RowLabels.iter (fun w -> print_row w table) table.lower_row_labels

  (* [is_same_state s0 s1] Returns true if s0 and s1 are the same state.*)
  let is_same_state (table,(w0:Word.t)) (_,(w1:Word.t)) = 
    RowEntries.compare compare (row_entries w0 table) (row_entries w1 table) = 0

  (*[delta a q] Returns the derivative of q wrt a; that is, the state we should
  transition to on a from q.*)
  let delta (letter:Alphabet.symbol) (table,w) = 
    let new_row_e = row_entries (Word.append_letter w letter) table in
    let ur_ls_es = up_rows_labels_entries table in
    let (label, _) = 
      List.find (fun (_, es) -> List.compare compare new_row_e es = 0) ur_ls_es in 
    (table, label)

  (*[is_accepting q] Returns true if q is an accepting state.*)
  let is_accepting (table, w) = match lookup_by_word_opt w table with
    | Some True -> true
    | None -> failwith "couldn't find entry in table for word"
    | Some Blank -> failwith "unfilled blank in table"
    | _ -> false
  
  let to_dfa (table: t) = 
    let start = (table, Word.epsilon) in 
    let alpha = Alphabet.intalph 2 (*creates alphabet [0;1]*) 
    (*NOTE: [Alphabet.inalph] still has imperative implementation
    since Alphaet.t is implamented as an array*) 
    in
    Dfa.mk_dfa {eq=is_same_state; d=delta; e=is_accepting} alpha start

  let equivalent_pairs table =  
    let ur_ls_es = up_rows_labels_entries table in
    RowLabels.fold 
      (fun lrl acc -> 
        let new_row_e = row_entries lrl table in
        let (url, _) = List.find (fun (_, es) -> 
            List.compare compare new_row_e es = 0) ur_ls_es in 
        (url, lrl)::acc) 
      table.lower_row_labels []
end
