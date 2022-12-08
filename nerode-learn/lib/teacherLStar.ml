open Nerode

type word = Alphabet.word

type t = Dfa.t

let conjecture (t: t) (c: Dfa.t) =
  let d1 = Dfa.diff t c in
  if Dfa.is_empty d1 then
    let d2 = Dfa.diff c t in
      if Dfa.is_empty d2 then
        None
      else
        Some (Dfa.rep d2)
  else
    Some (Dfa.rep d1)

let query (t: t) (w: word) =
  Some (Dfa.accept t w)

let distinguish _ _ _ = failwith "LStar teacher (MAT) does not support distinguish!"
