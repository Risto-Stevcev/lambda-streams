module Signal = struct
  (** Represents a signal that either has data or is terminated *)

  type 'a t =
    | Data of 'a
    | EndOfSignal

  let pure (value : 'a) : 'a t = Data value

  let empty () : 'a t = EndOfSignal

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t =
    match ma with
    | Data value -> Data (f value)
    | EndOfSignal -> EndOfSignal

  let filter (f : 'a -> bool) (ma : 'a t) : 'a t =
    match ma with
    | Data value when f value -> Data value
    | Data _ | EndOfSignal -> EndOfSignal

  let fold_left (f : 'a -> 'b -> 'a) (init : 'a) (ma : 'b t) : 'a =
    match ma with
    | Data value -> f init value
    | EndOfSignal -> init

  let%test_module "Signal" =
    (module struct
      let%test "pure" = pure 123 = Data 123

      let%test "empty" = empty () = EndOfSignal

      let%test_module "map" =
        (module struct
          let%test _ = map (( + ) 1) (pure 123) = pure 124

          let%test _ = map (( + ) 1) (empty ()) = empty ()
        end)

      let%test_module "filter" =
        (module struct
          let%test _ = filter (( < ) 3) (pure 1) = EndOfSignal

          let%test _ = filter (( < ) 3) (pure 4) = Data 4
        end)

      let%test_module "fold_left" =
        (module struct
          let%test _ = fold_left ( + ) 4 (pure 6) = 10

          let%test _ = fold_left ( + ) 4 (empty ()) = 4
        end)
    end)
end

module Sync : sig
  type 'a input = private unit -> 'a

  type 'a output = private 'a -> unit

  val make_input : (unit -> 'a) -> 'a input

  val make_output : ('a -> unit) -> 'a output

  val pure : 'a -> 'a input

  val enumerate : unit -> int input

  val next : 'a input -> 'a

  (** Like fold_left but for infinite streams *)
  val accumulate : int -> ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b

  val map : ('a -> 'b) -> 'a input -> 'b input

  val scan : ('b -> 'a -> 'b) -> 'b -> 'a input -> 'b input
end = struct
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
            invoked to recieve a new value.}
         {- As a result, functions like [filter] can't be implemented, but [until] can.}}}
    }
   *)

  type 'a input = unit -> 'a

  and 'a output = 'a -> unit

  let make_input f = f

  let make_output f = f

  let pure value () = value

  let enumerate () =
    let index = ref 0 in
    fun () ->
      index := !index + 1;
      !index

  let next stream = stream ()

  let accumulate n f init stream =
    let index = ref 0 and acc = ref init in
    while !index < n do
      index := !index + 1;
      acc := f !acc (stream ())
    done;
    !acc

  let map f stream () = stream () |> f

  let scan f init stream =
    let acc = ref init in
    fun () ->
      acc := f !acc (stream ());
      !acc

  (*
  let%test_module "Sync" =
    (module struct
      let%test "pure" =
        pure 123 |> accumulate 1 ( + ) 0 = 123 && pure 123 |> accumulate 2 ( + ) 0 = 246

      let%test "map" =
        enumerate () |> map (( * ) 2) |> Finite.keep 5 |> Finite.to_list = [2; 4; 6; 8; 10]
    end)
    *)
end

module Async = struct
  type 'a t = 'a Sync.output -> unit

  type 'a input = 'a t

  type 'a output = 'a t
end

module Finite = struct
  module Sync : sig
    type 'a input = private 'a Signal.t Sync.input

    val pure : 'a -> 'a input

    val empty : unit -> 'a input

    val from_array : 'a array -> 'a input

    val map : ('a -> 'b) -> 'a input -> 'b input

    val keep : int -> 'a Sync.input -> 'a input

    val until : ('a -> bool) -> 'a input -> 'a input

    val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b input -> 'a

    val to_rev_list : 'a input -> 'a list

    val to_list : 'a input -> 'a list

    val to_array : 'a input -> 'a array
  end = struct
    type 'a input = 'a Signal.t Sync.input

    (*
    type nonrec 'a output = 'a Signal.t output
    *)

    let pure (value : 'a) : 'a input =
      let was_sent = ref false in
      Sync.make_input @@ fun () ->
      match !was_sent with
      | true -> Signal.EndOfSignal
      | false ->
          was_sent := true;
          Data value

    let empty () : 'a input = Sync.make_input @@ fun () -> Signal.EndOfSignal

    let from_array (ma : 'a array) : 'a input =
      let index = ref 0 in
      Sync.make_input @@ fun () ->
      match !index with
      | index' when index' < Array.length ma ->
          index := index' + 1;
          Signal.Data ma.(index')
      | _ -> EndOfSignal

    let map f ma = Sync.make_input @@ fun () -> ma |> Sync.next |> Signal.map f

    let keep n stream =
      let index = ref 0 in
      Sync.make_input @@ fun () ->
      let value = if !index < n then Signal.Data (Sync.next stream) else Signal.EndOfSignal in
      index := !index + 1;
      value

    let until f ma = Sync.make_input @@ fun () -> ma |> Sync.next |> Signal.filter f

    let fold_left f init stream =
      let more = ref true and result = ref init in
      while !more do
        match Sync.next stream with
        | Signal.Data value -> result := f !result value
        | EndOfSignal -> more := false
      done;
      !result

    let to_rev_list stream =
      let list = ref [] and more = ref true in
      while !more do
        match Sync.next stream with
        | Signal.Data value -> list := value :: !list
        | EndOfSignal -> more := false
      done;
      !list

    let to_list x = to_rev_list x |> List.rev

    let to_array x = to_list x |> Array.of_list

    let%test_module "Sync.Finite" =
      (module struct
        let%test_module "Transducer" =
          (module struct
            let%test "Arrays are not transducers" =
              let filter' f x = x |> Array.to_list |> List.filter f |> Array.of_list in
              let invocations = ref [] in
              let result =
                [|1; 2; 3|]
                |> Array.map (fun e ->
                       invocations := "map" :: !invocations;
                       e * 2)
                |> filter' (fun e ->
                       invocations := "filter" :: !invocations;
                       e < 10)
                |> Array.fold_left
                     (fun acc e ->
                       invocations := "reduce" :: !invocations;
                       acc + e)
                     0
              in
              result = 12
              && !invocations
                 |> List.rev
                 = ["map"; "map"; "map"; "filter"; "filter"; "filter"; "reduce"; "reduce"; "reduce"]

            let%test "Streams are transducers" =
              let invocations = ref [] in
              let result =
                from_array [|1; 2; 3|]
                |> map (fun e ->
                       invocations := "map" :: !invocations;
                       e * 2)
                |> until (fun e ->
                       invocations := "filter" :: !invocations;
                       e < 10)
                |> fold_left
                     (fun acc e ->
                       invocations := "reduce" :: !invocations;
                       acc + e)
                     0
              in
              result = 12
              && !invocations
                 |> List.rev
                 = ["map"; "filter"; "reduce"; "map"; "filter"; "reduce"; "map"; "filter"; "reduce"]
          end)

        let%test_module "pure" =
          (module struct
            let%test _ = pure 123 |> Sync.next = Data 123

            let%test _ =
              let stream = pure 123 in
              Sync.next stream |> ignore;
              Sync.next stream = EndOfSignal
          end)

        let%test "empty" = empty () |> Sync.next = EndOfSignal

        let%test_module "from_array" =
          (module struct
            let%test _ = from_array [||] |> Sync.next = EndOfSignal

            let%test _ =
              let stream = from_array [|1; 2; 3|] in
              stream |> Sync.next = Data 1
              && stream |> Sync.next = Data 2
              && stream |> Sync.next = Data 3
              && stream |> Sync.next = EndOfSignal
              && stream |> Sync.next = EndOfSignal
          end)

        let%test_module "map" =
          (module struct
            let%test _ =
              let stream = pure 123 |> map (( + ) 1) in
              stream |> Sync.next = Data 124

            let%test _ =
              let stream = empty () |> map (( + ) 1) in
              stream |> Sync.next = EndOfSignal

            let%test _ =
              let stream = from_array [|1; 2; 3|] |> map (( * ) 2) in
              stream |> Sync.next = Data 2
              && stream |> Sync.next = Data 4
              && stream |> Sync.next = Data 6
              && stream |> Sync.next = EndOfSignal
              && stream |> Sync.next = EndOfSignal
          end)

        let%test_module "until" =
          (module struct
            let%test _ =
              let stream = pure 123 |> until (( < ) 3) in
              stream |> Sync.next = Data 123

            let%test _ =
              let stream = pure 1 |> until (( < ) 3) in
              stream |> Sync.next = EndOfSignal

            let%test _ =
              let stream = from_array [|1; 2; 3|] |> until (( > ) 3) in
              stream |> Sync.next = Data 1
              && stream |> Sync.next = Data 2
              && stream |> Sync.next = EndOfSignal
          end)

        let%test "fold_left" = from_array [|1; 2; 3|] |> fold_left ( + ) 0 = 6

        let%test "to_rev_list" = from_array [|1; 2; 3|] |> to_rev_list = [3; 2; 1]

        let%test "to_list" = from_array [|1; 2; 3|] |> to_list = [1; 2; 3]

        let%test "to_array" = from_array [|1; 2; 3|] |> to_array = [|1; 2; 3|]
      end)
  end
end
