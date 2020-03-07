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

module Sync = struct
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

  type 'a t = unit -> 'a Signal.t

  let pure (value : 'a) : 'a t =
    let was_sent = ref false in
    fun () ->
      match !was_sent with
      | true -> EndOfSignal
      | false ->
          was_sent := true;
          Data value

  let empty () : 'a t = fun () -> EndOfSignal

  let from_array (ma : 'a array) : 'a t =
    let index = ref 0 in
    fun () ->
      match !index with
      | index' when index' < Array.length ma ->
          index := index' + 1;
          Data ma.(index')
      | _ -> EndOfSignal

  let map (f : 'a -> 'b) (ma : 'a t) : 'b t = fun () -> Signal.map f @@ ma ()

  let until (f : 'a -> bool) (ma : 'a t) : 'a t = fun () -> Signal.filter f @@ ma ()

  let fold_left (f : 'a -> 'b -> 'a) (init : 'a) (ma : 'b t) : 'a =
    let more = ref true and result = ref init in
    while !more do
      match ma () with
      | Data value -> result := f !result value
      | EndOfSignal -> more := false
    done;
    !result

  let to_rev_list (x : 'a t) : 'a list =
    let list = ref [] and more = ref true in
    while !more do
      match x () with
      | Data value -> list := value :: !list
      | EndOfSignal -> more := false
    done;
    !list

  let to_list x = to_rev_list x |> List.rev

  let to_array x = to_list x |> Array.of_list

  let%test_module "Sync" =
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
          let%test _ = pure 123 () = Data 123

          let%test _ =
            let stream = pure 123 in
            stream () |> ignore;
            stream () = EndOfSignal
        end)

      let%test "empty" = empty () () = EndOfSignal

      let%test_module "from_array" =
        (module struct
          let%test _ = from_array [||] () = EndOfSignal

          let%test _ =
            let stream = from_array [|1; 2; 3|] in
            stream () = Data 1
            && stream () = Data 2
            && stream () = Data 3
            && stream () = EndOfSignal
            && stream () = EndOfSignal
        end)

      let%test_module "map" =
        (module struct
          let%test _ =
            let stream = pure 123 |> map (( + ) 1) in
            stream () = Data 124

          let%test _ =
            let stream = empty () |> map (( + ) 1) in
            stream () = EndOfSignal

          let%test _ =
            let stream = from_array [|1; 2; 3|] |> map (( * ) 2) in
            stream () = Data 2
            && stream () = Data 4
            && stream () = Data 6
            && stream () = EndOfSignal
            && stream () = EndOfSignal
        end)

      let%test_module "until" =
        (module struct
          let%test _ =
            let stream = pure 123 |> until (( < ) 3) in
            stream () = Data 123

          let%test _ =
            let stream = pure 1 |> until (( < ) 3) in
            stream () = EndOfSignal

          let%test _ =
            let stream = from_array [|1; 2; 3|] |> until (( > ) 3) in
            stream () = Data 1 && stream () = Data 2 && stream () = EndOfSignal
        end)

      let%test "fold_left" = from_array [|1; 2; 3|] |> fold_left ( + ) 0 = 6

      let%test "to_rev_list" = from_array [|1; 2; 3|] |> to_rev_list = [3; 2; 1]

      let%test "to_list" = from_array [|1; 2; 3|] |> to_list = [1; 2; 3]

      let%test "to_array" = from_array [|1; 2; 3|] |> to_array = [|1; 2; 3|]
    end)
end

module Async = struct

end
