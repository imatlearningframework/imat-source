(* module CliOpt is only used for managing global variables for command line options *)

(* Verbose *)
type v = Off | On | Latex | Csv
let verb_setting: v ref = ref Off
let set_verb (v:v): unit =
  verb_setting := v
let verbose_log if_on if_latex arg: unit =
  match !verb_setting with
  | On -> if_on arg
  | Latex -> if_latex arg
  | _ -> ()
let csv () = (!verb_setting = Csv)
let verbose () = (!verb_setting = On)

(* Last guess / Word ordering / (i.e., Unique tables) *)
let uniq = ref false
let set_uniq (b:bool): unit =
  uniq := b
let uniq_on () = !uniq

(* Unsat cores optimization *)
let unsat_cores = ref false
let set_unsat_cores (b:bool): unit =
  unsat_cores := b
let unsat_cores_on () = !unsat_cores

let distinguish = ref false
let set_distinguish (b:bool): unit =
  distinguish := b
let distinguish_on () = !distinguish

(*conjecturing with distinguish queries instead*)
let d_blanks = ref false
let set_d_blanks (b:bool): unit =
  d_blanks := b
let d_blanks_on () = !d_blanks

(* Global E maintained as argument in main loop *)
let ge = ref false
let set_ge (b:bool): unit =
  ge := b
let global_e () = !ge
