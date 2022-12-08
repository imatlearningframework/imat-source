open Core
open Teacher
open ActiveLearner
open Stdlib
open Nerode

module Word = ObsTbl.Word
module Table = ObsTbl.Table
module WordSet = ObsTbl.WordSet
module RowLabels = ObsTbl.RowLabels
module ColLabels = ObsTbl.ColLabels
module TWordSet = Teacher.WordSet

module type WorklistSig = sig
  (**the contents contained within the worklist*)
  type contents
  type t
  val empty : t
  val enqueue_low_pri : contents -> t -> t
  val head : t -> contents

  (**[head] returns first element of the worklist*)
  val tail : t -> t

  val enqueue_high_pri : contents -> t -> t
  val map : (contents -> contents) -> t -> t
  val fold : (contents * int -> 'a -> 'a) -> t -> 'a -> 'a
  val of_list : (contents * int) list -> t

  (**[map f wl] is the worklist whose contents are [f c0, f c1... f cN],
  where c0, c1... cN are the contents of [wl]*)
  val exists : (contents -> bool) -> t -> bool

  (**[exists f [c1; ...; cn]] checks if at least one content of the worklist 
  satisfies the predicate f. That is, it returns (f c1) || (f c2) || ... 
  || (f cn) for a non-empty worklist and false if the worklist is empty*)
  val length : t -> int
end

module Worklist : WorklistSig with type contents = Table.t = struct
  type contents = Table.t
  module S = Set.Make(struct
    type t = contents * int
    let compare (tbl0, priority0) (tbl1, priority1) =
      let s0 = Table.upper_row_labels tbl0 in
      let s1 = Table.upper_row_labels tbl1 in
      let size0 = WordSet.cardinal s0 in
      let size1 = WordSet.cardinal s1 in
      let sim0 = Table.similarity tbl0 in
      let sim1 = Table.similarity tbl1 in
      (* s0, s1 needed at end because otherwise the equivalence is too coarse;
         different tables would compare equal and some tables will be dropped *)
      if CliOpt.unsat_cores_on () then
        Stdlib.compare (priority0, size0, sim0, s0) (priority1, size1, sim1, s1)
      else if CliOpt.uniq_on () then
        Stdlib.compare (size0, priority0, sim0, s0) (size1, priority1, sim1, s1)
      else
        Stdlib.compare (size0, s0) (size1, s1)
  end)
  type t = S.t
  let empty = S.empty
  let enqueue_high_pri tbl = S.add (tbl, 0)
  let head wl = S.min_elt wl |> fst
  let tail wl = S.remove (S.min_elt wl) wl
  let enqueue_low_pri tbl = S.add (tbl, 1)
  let map f = S.map (fun (tbl, pri) -> (f tbl, pri))
  let exists f = S.exists (fun (tbl, pri) -> f tbl)
  let fold = S.fold
  let of_list = S.of_list
  let length = S.cardinal
end

module Make(Teacher : Teacher) : ActiveLearner with type teacher = Teacher.t = struct
  let learntime = ref 0.0
  let z3_time = ref 0.0
  let conjectures = ref 0
  let conjecture_time = ref 0.0
  let success_fill = ref 0
  let fail_fill = ref 0
  let popped_items = ref 0
  let cols_update_time = ref 0.0
  let unclosed_prefill = ref 0

  let cumul_learntime = ref 0.0
  let cumul_z3_time = ref 0.0
  let cumul_conjectures = ref 0
  let cumul_conjecture_time = ref 0.0
  let cumul_success_fill = ref 0
  let cumul_fail_fill = ref 0
  let cumul_popped_items = ref 0
  let cumul_cols_update_time = ref 0.0
  let cumul_unclosed_prefill = ref 0
  (* let real_z3_time = ref 0.0 *)
  (* let real_z3_queries = ref 0   *)

  let accum_metrics () =
    cumul_learntime := !cumul_learntime +. !learntime;
    cumul_z3_time := !cumul_z3_time +. !z3_time;
    cumul_conjectures := !cumul_conjectures + !conjectures;
    cumul_conjecture_time := !cumul_conjecture_time +. !conjecture_time;
    cumul_success_fill := !cumul_success_fill + !success_fill;
    cumul_fail_fill := !cumul_fail_fill + !fail_fill;
    cumul_popped_items := !cumul_popped_items + !popped_items;
    cumul_cols_update_time := !cumul_cols_update_time +. !cols_update_time;
    cumul_unclosed_prefill := !cumul_unclosed_prefill + !unclosed_prefill

  let reset_metrics () =
    learntime := 0.0;
    z3_time := 0.0;
    conjectures := 0;
    conjecture_time := 0.0;
    success_fill := 0;
    fail_fill := 0;
    popped_items := 0;
    cols_update_time := 0.0;
    unclosed_prefill := 0

  let print_metrics size query_count () =
    if CliOpt.csv () then
      Printf.printf "%d,%d,%d,%d,%f,%f,%f\n%!"
                    size
                    query_count
                    !popped_items
                    !conjectures
                    !conjecture_time
                    !z3_time
                    !learntime
    else
      begin
      Printf.printf "Dfa_size:\t%d\n%!" size;
      Printf.printf "Popped_Items:\t%d\n%!" !popped_items; (*# of tables popped 
        from the worklist and passed through algorithm. Tables that produce 
        counterexample and are put back in head of worklist are counted twice*)
      Printf.printf "Query Count:\t%d\n%!" query_count;
      Printf.printf "Conjectures:\t%d\n%!" !conjectures;
      Printf.printf "Conjecture_Time:\t%f\n%!" !conjecture_time; (*table_to_dfa+conjecture call to teacher*)
      Printf.printf "Cols_Update_Time:\t%f\n%!" !cols_update_time; (*time to add
        new columns to tables in worklist*)
      Printf.printf "BlankSMT_Time:\t%f\n%!" !z3_time;
      Printf.printf "Learn Time:\t%f\n%!" !learntime;
      end

  let print_cumul_metrics () = 
    if CliOpt.csv () then
      () (* If we're going to csv, then the consumer can sum easily enough anyway *)
    else
      begin
      Printf.printf "Total Popped_Items:\t%d\n%!" !cumul_popped_items; (*# of tables popped 
        from the worklist and passed through algorithm. Tables that produce 
        counterexample and are put back in head of worklist are counted twice*)
      Printf.printf "Total Conjectures:\t%d\n%!" !cumul_conjectures;
      Printf.printf "Total Conjecture_Time:\t%f\n%!" !cumul_conjecture_time; (*table_to_dfa+conjecture call to teacher*)
      Printf.printf "Total Cols_Update_Time:\t%f\n%!" !cumul_cols_update_time; (*time to add
        new columns to tables in worklist*)
      Printf.printf "Total BlankSMT_Time:\t%f\n%!" !cumul_z3_time;
      Printf.printf "Total Learn Time:\t%f\n%!" !cumul_learntime
      end

  module RowsSet = Set.Make(RowLabels)
  module EntryMap = ObsTbl.WordMap

  type teacher = Teacher.t (*here just to satisfy ActiveLearner mli for now*)

  let aword w = w |> Word.to_intlist |> Alphabet.w_of_ints

  let query teacher (w : Word.t) : Table.entry = 
    (*for now, [aword] conversion is necessary since Word.t and 
    alphabet.word aren't recognized as the same*)
    match Teacher.query teacher (aword w) with
    | None -> Blank
    | Some true -> True
    | Some false -> False

  let distinguish teacher (w1: Word.t) (w2: Word.t) : bool =
    let aword1 = aword w1 in
    let aword2 = aword w2 in
    match Teacher.distinguish teacher aword1 aword2 with
    | None -> false
    | _ -> true
  
  let redisplay wl uncpre conj frac tbl : unit = 
    if CliOpt.verbose () then 
      begin 
        Printf.printf "Popped:[%d] Unclosed pre-fill:[%d] Conj:[%d] 
          Perc filled:[%.2f] UpperTblSize:[%d]\n%!" 
          wl uncpre conj frac (tbl |> Table.upper_row_labels |> RowLabels.cardinal);
        Table.print_table tbl
      end  

  let dump_words (label: string) (s: WordSet.t) : unit =
    if CliOpt.verbose () then begin
      Printf.printf "%s: " label;
      WordSet.iter (Fn.compose (Printf.printf "%s ") Word.to_string) s;
      Printf.printf "\n%!" end
  
  (**returns a worklist with [wl] plus, for each row in [rows_w_letter], 
  the table [tbl] plus the row is added to the worklist, provided its rows are
  not already in [rows_history] and a table with the same rows isn't already in
  worklist [wl].*)
  let update_worklist (rows_w_letter: RowLabels.t) (unsat_core: WordSet.t option) get_entry 
    (tbl: Table.t) rows_history (teacher: Teacher.t) wl e_map = 
      let wl_tl = Worklist.tail wl in
      let words = if CliOpt.uniq_on () then
        let lowerbound = Table.max_upper_row_label tbl in
        let upperbound = match unsat_core with
                         | Some u -> WordSet.max_elt_opt u
                         | _ -> None in
        (* Printf.printf "Bounding %b\n" (upperbound = None); *)
        RowLabels.filter (fun s -> Word.compare s lowerbound > 0 &&
                                   match upperbound with
                                   | None -> true
                                   | Some u -> Word.compare s u <= 0) rows_w_letter
      else if CliOpt.unsat_cores_on () then
        match unsat_core with
        | None -> failwith "No unsat core"
        | Some uc when WordSet.is_empty uc -> Table.print_table tbl;
                                              failwith "Empty unsat core!"
        | Some uc -> uc

      else
        rows_w_letter in
      let () = dump_words "words" words in
      let words' = if CliOpt.distinguish_on () then
        let distinguishable_from_upper (w: Word.t) =
          let upper_lst = RowLabels.elements (Table.upper_row_labels tbl) in
          match List.find_opt (fun s -> not (distinguish teacher w s)) upper_lst with
          | None -> true
          | _ -> false  in
        match RowLabels.filter distinguishable_from_upper words with
          | s when WordSet.is_empty s -> words
          | s -> WordSet.choose s |> WordSet.singleton
      else
        words in
      let () = dump_words "words'" words' in
      RowLabels.fold (fun sa (wl_acc, em_acc) -> 
          let rows_sa = Table.upper_row_labels tbl |> RowLabels.add sa in
          (*checks if this table has already passed thru worklist or is in worklist*)
          if RowsSet.mem rows_sa rows_history 
            || Worklist.exists 
              (fun tab -> Table.upper_row_labels tab |> RowLabels.equal rows_sa ) wl
          then
            begin
              if CliOpt.uniq_on () then failwith "impossible dup!\n"
              else if CliOpt.verbose () then Printf.printf "Dup avoided!\n%!"
              else ();
              wl_acc, em_acc
            end
          else 
            let t', em_acc' = Table.add_row get_entry em_acc sa tbl in 
            if CliOpt.verbose () then begin
              Printf.printf "Enqueued to back of wl:\n%!"; 
              Table.print_table t' end;
            match unsat_core with
            | None -> Worklist.enqueue_low_pri t' wl_acc, em_acc'
            | Some u when WordSet.is_empty u -> 
                Worklist.enqueue_low_pri t' wl_acc, em_acc'
            | Some u -> if WordSet.mem sa u then
                          Worklist.enqueue_high_pri t' wl_acc, em_acc'
                        else
                          Worklist.enqueue_low_pri t' wl_acc, em_acc')
        words' (wl_tl, e_map)
  
  let conjecture_by_distinguish table teacher : Alphabet.word option = 
    let cols = Table.col_labels table |> ColLabels.elements 
      |> List.map (fun e -> e|> Word.to_intlist |> Alphabet.w_of_ints) 
      |>  TWordSet.of_list in
    Table.equivalent_pairs table |> List.fold_left 
    (fun acc (urow, lrow) -> match acc with 
      | None -> Teacher.distinguish_blanks teacher (aword urow) (aword lrow) cols
      | counterex -> counterex) None

  (**Lstar with blanks main loop*)
  let rec lstar_blanks ((wl : Worklist.t), rows_history, teacher, g_cols, e_map) : Dfa.t = 
    let hd_t = Worklist.head wl in
    let tbl, e_map = if CliOpt.global_e () then
      let cur_cols = Table.col_labels hd_t in 
      let new_cols = ColLabels.diff g_cols cur_cols in
      Table.add_cols (query teacher) e_map new_cols hd_t
    else hd_t, e_map in


    let () = popped_items := !popped_items + 1 in
      redisplay 
        !popped_items 
        !unclosed_prefill
        !conjectures 
        ((!success_fill |> float_of_int) /. 
          (!success_fill + !fail_fill |> float_of_int) *. 100.) 
        tbl;

    let urows = Table.upper_row_labels tbl in
    let rows_history' = RowsSet.add urows rows_history in
    (*checking if table can't be closed even before filling blanks*)
    match if CliOpt.uniq_on () || CliOpt.unsat_cores_on () then None else Table.closed tbl with
    | Some lower_row -> 
      unclosed_prefill := !unclosed_prefill + 1;
      let tbl', e_map' = Table.add_row (query teacher) e_map lower_row tbl in
      let wl' = Worklist.(wl |> tail |> enqueue_high_pri tbl') in
      if CliOpt.verbose () then begin 
        Printf.printf "Failed to close even with blanks due to \
          row [%s]. Enqueued to back of wl:\n%!" (Word.to_string lower_row);
        Table.print_table tbl' end;
      lstar_blanks (wl', rows_history', teacher, g_cols, e_map')
    | None ->
      let start = Core_unix.gettimeofday () in
      let filled = Solver.fill_blanks tbl in
      let () = z3_time := !z3_time +. (Core_unix.gettimeofday () -. start) in
      
      match filled with
        | None -> 
          fail_fill := !fail_fill + 1;
          let rows_w_letter = Table.lower_row_labels tbl in
          let wl_updated, e_map' =
            if CliOpt.unsat_cores_on () then
              begin
              let unsat_core = Solver.get_unsats () in
              let () = dump_words "Unsat core" unsat_core in
              update_worklist rows_w_letter (Some unsat_core) (query teacher) tbl rows_history' teacher wl e_map
              end
            else
              update_worklist rows_w_letter None (query teacher) tbl rows_history' teacher wl e_map in
          lstar_blanks (wl_updated, rows_history', teacher, g_cols, e_map')
        | Some obs ->
          success_fill := !success_fill + 1;
          if CliOpt.verbose () then 
            begin
              Printf.printf "Filling blanks was successful:\n%!";
              Table.print_table obs;
              Printf.printf "Making dfa...\n%!";
            end;
          let () = conjectures := !conjectures + 1 in
          let start = Core_unix.gettimeofday () in 
          let dfa = Table.to_dfa obs in 
          if CliOpt.verbose () then begin
              Dfa.print dfa;
              Printf.printf "Conjecturing dfa...\n%!" end;
          let result = if CliOpt.d_blanks_on () 
            then conjecture_by_distinguish obs teacher
            else Teacher.conjecture teacher dfa in
          let () = conjecture_time := 
            !conjecture_time +. (Core_unix.gettimeofday () -. start) in 
          
          match result with
          | None -> 
            if CliOpt.d_blanks_on () 
              (*checking to see if distinguish_blanks produced a correct DFA*)
              then begin match Teacher.conjecture teacher dfa with
                | Some cex -> failwith "Distinguish_blanks returned an incompatible DFA! Uh-oh!"
                | None -> dfa
                end
            else dfa
          | Some counterex ->
            if CliOpt.verbose () then 
              Printf.printf "Conjecture rejected due to counterexample [%s]:\n%!"
                Word.(counterex |> of_symlist |> to_string);
            let start = Core_unix.gettimeofday () in
            let e = Table.col_labels tbl in
            let new_suffixes = counterex 
              |> Alphabet.w_to_ints |> Word.of_intlist 
              |> Word.suffixes 
              (*filter gets rid of suffixes that are already column labels in table.
              NOTE: filtering could be moved into Table module to be automatically 
              done when adding row or column (by implementation of table, duplicates
              are already removed). It is more efficient to have filter once here though,
              since we add the suffixes to each table in the worklist and we then know
              via the algorithm that all the table columns in the worklist are identical*)
              |> List.filter (fun w -> not (ColLabels.mem w e)) 
              |> ColLabels.of_list 
            in
            let update_cols t em = Table.add_cols (query teacher) em new_suffixes t in
            let wl', e_map'' = if CliOpt.global_e () 
              then (*E's not shared, so no updating excapt for global cols*)
                wl, e_map
                (* let tbl', e_map' = update_cols tbl e_map in
                Worklist.(enqueue_high_pri tbl' (tail wl)), e_map' *)
              else (*adding suffixes of counterexample to columns of each table in worklist*)
                (*no fold_map for sets*)
                let wl_lst, em' = 
                  Worklist.(fold (fun (tbl, pri) (wl_acc, em_acc) -> 
                    let tbl', em' = update_cols tbl em_acc in 
                    (tbl', pri)::wl_acc, em') wl ([], e_map))
                in Worklist.of_list wl_lst, em'
            in
            let g_cols' = ColLabels.union g_cols new_suffixes in
            let () = cols_update_time := 
              !cols_update_time +. (Core_unix.gettimeofday () -. start) in 
            lstar_blanks (wl', rows_history, teacher, g_cols', e_map'')
  
  (**starting point, command visible to client
  As of now, [alpha] is unused/ignored in the implementation; we assume an 
  alphabet of {0,1} - [alpha] is there as an parameter just to satisfy the 
  ActiveLearner mli*)
  let learn (alpha : Alphabet.t) teacher = 
    let () = reset_metrics () in
    let learnstart = Core_unix.gettimeofday () in
    let (tbl, e_map) : Table.t * Table.entry EntryMap.t = Table.init_epsilon (query teacher) in
    let wl : Worklist.t = (*Worklist is implemented as a list of tables (type Table.t)*)
       Worklist.(empty |> enqueue_low_pri tbl) in
    (* Printf.printf "Wl starting length:\t%d\n%!" ((Worklist.length wl)); *)

    (*[rows_history] is represented as a Set of RowLabels, no duplicates. As 
    implemented now, RowsHistory maintains a history of rows of tables that 
    have been in the worklist and have failed to be closed in the algorithm, 
    to make sure tables with the same rows aren't added to the worklist again.*)
    let rows_history = RowsSet.empty in
    (*[g_cols] is a *)
    let g_cols = ColLabels.empty in
    let dfa = lstar_blanks (wl, rows_history, teacher, g_cols, e_map) in
    let () = learntime := (*total amount of time it took to learn dfa*)
              !learntime +. (Core_unix.gettimeofday () -. learnstart) in
    let () = print_metrics (Dfa.size dfa) (Teacher.number_queries teacher) () in
    let () = accum_metrics () in
    let () = print_cumul_metrics () in
    dfa
end
