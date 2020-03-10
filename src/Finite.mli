(** Represents the finite version of a {!Lambda_streams.Sync} stream *)
module Sync : sig
  type 'a input = 'a Signal.t Sync.input

  val pure : 'a -> 'a input

  val empty : unit -> 'a input

  val from_list : 'a list -> 'a input

  val from_array : 'a array -> 'a input

  val map : ('a -> 'b) -> 'a input -> 'b input

  val keep : int -> 'a Sync.input -> 'a input

  val until : ('a -> bool) -> 'a input -> 'a input

  val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b input -> 'a

  val to_rev_list : 'a input -> 'a list

  val to_list : 'a input -> 'a list

  val to_array : 'a input -> 'a array
end

(** Represents the finite version of a {!Lambda_streams.Async} stream *)
module Async : sig end
