open Nerode

module type ActiveLearner = sig
  type teacher
  val learn : Alphabet.t -> teacher -> Dfa.t
end
