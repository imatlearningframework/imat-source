open Nerode

type word = Alphabet.word

module WordSet = Set.Make(struct
  type t = word
  let compare = Stdlib.compare
end)

module type Teacher = sig
  open Nerode
  type t
  val conjecture : t -> Dfa.t -> word option
  val query : t -> word -> bool option
  val distinguish : t -> word -> word -> word option
  val distinguish_blanks : t -> word -> word -> 
    WordSet.t -> word option
  val number_queries : t -> int
end
