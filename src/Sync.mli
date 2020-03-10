(**
   Generalizes the notion of a syncronous readable stream. It has some interesting properties
   compared to traditional ({i potentially} asyncronous) streams:
   {ul
     {- Since the function returns a {!Signal.t}, it implies that the function is both side-effecting
       and utilizes a closure because it can return a different result on each invocation.}
     {- Like traditional streams:
       {ul
         {- It is a transducer}}}
     {- Unlike traditional streams:
       {ul
         {- There is no inversion of control: the stream is not passed another stream to which it
           communicates with.}
         {- There is direct control: a value is available at all times because the stream can be
            invoked to recieve a new value. It's pull-based.}
         {- As a result, functions like [filter] can't be implemented, but [until] can.}}}
    }
   *)

type 'a input = private unit -> 'a

type 'a output = private 'a -> unit

val make_input : (unit -> 'a) -> 'a input

val make_output : ('a -> unit) -> 'a output

val pure : 'a -> 'a input

val enumerate : unit -> int input

val next : 'a input -> 'a

(** Like fold_left but for infinite streams, ending at signal n *)
val accumulate : int -> ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b

val map : ('a -> 'b) -> 'a input -> 'b input

val scan : ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b input
