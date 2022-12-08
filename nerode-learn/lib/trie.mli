open Nerode

type symbol = Alphabet.symbol
type word = Alphabet.word

(* ['a t] is a map from a prefix-closed set of words into 'a.
   When words are added, the caller provides a callback (of type word -> 'a)
   for filling in values for keys that were not already in the trie. *)
type 'a t

module SymMap : Map.S with type key = symbol

val empty : 'a t
val eps : 'a -> 'a t
val print : 'a t -> Alphabet.t -> ('a -> string) -> unit
val eps_extend : (word -> 'a) -> Alphabet.t -> 'a t
val find : 'a t -> word -> 'a
val find_opt : (word -> 'a -> bool) -> 'a t -> (word*'a) option
val map : (word -> 'a -> 'b) -> 'a t -> 'b t
val of_word : word -> (word -> 'a) -> 'a t
val add : (word -> 'a) -> 'a t -> word -> 'a t
val add_extend : (word -> 'a) -> Alphabet.t ->  'a t -> word -> 'a t
val of_word_extend : word -> Alphabet.t -> (word -> 'a) -> 'a t
val of_wordlist : (word -> 'a) -> word list -> 'a t
val of_wordlist_extend : (word -> 'a) -> Alphabet.t -> word list -> 'a t
val keys : 'a t -> word list
val node_keys : 'a t -> word list
val leaf_keys : 'a t -> word list
val node_bindings : 'a t -> (word * 'a) list
val leaf_bindings : 'a t -> (word * 'a) list
val fold : ('a -> word -> 'a) -> 'a -> 'b t -> 'a
val iter : (word -> unit) -> 'a t -> unit
