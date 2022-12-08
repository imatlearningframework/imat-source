open Nerode
type word = Alphabet.word

module type HybridLearner = sig
  val learn : Alphabet.t -> word list -> word list -> Dfa.t
end
