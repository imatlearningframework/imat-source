open Core
open Nerode
open Nerodelearn

module SS = Set.Make(String)

let load_input fn =
  let get_ints s a =
    let (s', a') = String.fold s ~init:([],a) ~f:(fun (sl, a) x -> 
                    ((String.make 1 x)::sl, SS.add a (String.make 1 x))) in
    (List.rev s', a') in
    
  let get_alt s a =
    let strs = String.split_on_chars ~on:[' '] s in
    (* allow multiple spaces to be used as a single delimeter: *)
    let s' = if List.length strs = 0 then 
                strs 
             else
                List.filter strs ~f:(fun s -> String.length s <> 0) in
    let a' = List.fold_left s' ~init:a ~f:(fun acc s -> SS.add acc s) in
    (s', a') in

  let proc_line scan_str (p, n, a) line =
    let st = String.strip line in
    let (w, a') = scan_str (String.drop_suffix st 2) a in
    match String.suffix st 1 with
    | "+" -> (w::p, n, a')
    | "-" -> (p, w::n, a')
    | _ -> failwith ("Malformed input string: " ^ st) in

  let lines = In_channel.read_lines fn in
  let scan_str = match lines with
             | [] -> failwith "Input file was empty"
             | line::_ -> if String.contains line ';' then get_alt
                                                      else get_ints in
  let p_str, n_str, a_set = 
          List.fold_left lines ~init:([], [], SS.empty) ~f:(proc_line scan_str) in

  (* Remove "X" which has special meaning (wildcard) *)
  let a_set' = SS.remove a_set "X" |> SS.elements  in
  let alpha = if List.length a_set' = 0 then 
                Alphabet.intalph 2
              else
                Array.of_list a_set' |> Alphabet.of_string_array in

  let f = Alphabet.ws_of_strings alpha in
  (List.concat (List.map ~f p_str),
   List.concat (List.map ~f n_str), 
   alpha)

let set_verbose v tex c = CliOpt.(set_verb (match v,tex,c with
  | true,_,_ -> On
  | _,true,_ -> Latex
  | _,_,true -> Csv
  | _,_,_ -> Off))

module WLS = Lstarblanks.Make(TeacherIndifferent)
module WLSep = Lstarblanks.Make(TeacherSep)

let learn_from_sep_rx r1 r2 () =
  let alpha = Alphabet.intalph 2 in
  let rx1 = Parser.parse_string r1 in
  let rx2 = Parser.parse_string r2 in
  let teacher = TeacherSep.make alpha rx1 rx2 in
  let () = CliOpt.set_unsat_cores true in
  let () = Solver.produce_unsats () in
  let d = WLSep.learn alpha teacher in
  Dfa.print d;
  Printf.printf "Dfa size: %d\n%!" (Dfa.size d);
  Printf.printf "%s\n%!" (Dfa.to_rx d |> Rx.to_string alpha)

let learn_from_examples v tex uniq sc dist db ge (csv:bool) fn () =
  let () = set_verbose v false csv in (*no latex for now :( *)
  let () = CliOpt.set_ge ge in
  let () = CliOpt.set_uniq uniq in
  let () = CliOpt.set_unsat_cores sc in
  let () = CliOpt.set_distinguish dist in
  let () = CliOpt.set_d_blanks db in
  if sc then
    Solver.produce_unsats ();
  let (pos, neg, alpha) = load_input fn in
  let teacher = TeacherIndifferent.make pos neg in
  let d = WLS.learn alpha teacher in
  match CliOpt.csv () with
  | false -> begin
             Dfa.print d;
             if not (Dfa.validate d pos neg) then failwith "DFA failed validation" else
               Printf.printf "%s\n%!" (Dfa.to_rx d |> Rx.to_string alpha)
            end
  | _ -> ()

let learn_from_dir_examples v tex uniq sc dist db ge (csv:bool) dn () = 
  let () = set_verbose v tex csv in
  let () = if CliOpt.csv () then
              Printf.printf "\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\",\"%s\"\n"
                  "filename"
                  "dfa-size"
                  "membership-queries"
                  "items-processed"
                  "conjectures"
                  "conjecture-time"
                  "z3-time"
                  "learn-time"
           else
              () in
  let file_arr = Sys_unix.readdir dn in 
  let n = Array.length file_arr in
  Array.iteri file_arr 
    ~f:(fun i fn -> 
          let () = Printf.eprintf "%s. [%d / %d files]\n%!" fn i n  in
          let () = if CliOpt.csv () then
            Printf.printf "\"%s\"," fn
          else
            Printf.printf "%s:\n%!" fn in

          learn_from_examples v tex uniq sc dist db ge csv (dn^"/"^fn) ())

let lsb_sep_cmd =
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run the L* with Blanks on two regular languages"
    (empty
    +> anon ("L+" %: string)
    +> anon ("L-" %: string))
    learn_from_sep_rx

let lsb_cmd =
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run L* with Blanks on a finite example set"
    (empty
     +> flag "-v" no_arg ~doc: ("Verbose")
     +> flag "-l" no_arg ~doc: ("Latex")
     +> flag "-u" no_arg ~doc: ("Use Word-ordering to prevent investigating duplicate tables")
     +> flag "-s" no_arg ~doc: ("Use unsat cores, but *not* word-ordering (use the visited set instead)")
     +> flag "-d" no_arg ~doc: ("Use the teacher's `distinguish' query to improve learner")
     +> flag "-db" no_arg ~doc: ("Use distinguish_blanks queries as substitute for conjecture")
     +> flag "-ge" no_arg ~doc: ("Make Columns global/Global E maintained as argument in main loop")
     +> flag "-f" no_arg ~doc: ("Write data in csv format.")
     +> anon ("file" %: string))
    learn_from_examples

let lsb_dir_cmd = 
  let open Command.Spec in
  Command.basic_spec
    ~summary: "Run the Worklist-SAT algorithm"
    (empty
     +> flag "-v" no_arg ~doc: ("Verbose")
     +> flag "-l" no_arg ~doc: ("Latex")
     +> flag "-u" no_arg ~doc: ("Use Word-ordering to prevent investigating duplicate tables")
     +> flag "-s" no_arg ~doc: ("Use Z3 unsat cores, and *not* word-ordering (use the visited set instead)")
     +> flag "-d" no_arg ~doc: ("Use the teacher's `distinguish' query to improve learner")
     +> flag "-db" no_arg ~doc: ("Use distinguish_blanks queries as substitute for conjecture")
     +> flag "-ge" no_arg ~doc: ("Make Columns not global/Tables can have different columns")
     +> flag "-f" no_arg ~doc: ("Write data in csv format.")
     +> anon ("file" %: string))
    learn_from_dir_examples

let main : Command.t =
  Command.group
    ~summary:"Execute an automaton learning algorithm"
    [
     ("lsblanks", lsb_cmd);
     ("lsblanks-dir", lsb_dir_cmd);
     ("lsblanks-sep", lsb_sep_cmd);
    ]

let () = Command_unix.run ~version: "0.1.1" main
