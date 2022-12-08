open Core
open Nerode
module Dfa = Intrx.Dfa

let rec to_string lst = match lst with
  | s::t -> (Intalph.to_string s) ^ (to_string t)
  | [] -> ""

let () =
  let sysargs = Sys.get_argv () in

  if Array.length sysargs < 2 then failwith "usage: enum_dfa <size>" else

  let size = Array.get sysargs 1 |> int_of_string in

  let dfas = Dfa.enum_bin_dfas size in

  List.iter ~f:(fun d -> Dfa.dfa_to_channel d Out_channel.stdout) dfas
