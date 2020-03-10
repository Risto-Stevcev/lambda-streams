module Sync = struct
  type 'a input = 'a Signal.t Sync.input

  (*
   type nonrec 'a output = 'a Signal.t output
   *)

  let pure value =
    let was_sent = ref false in
    Sync.make_input @@ fun () ->
    match !was_sent with
    | true -> Signal.EndOfSignal
    | false ->
        was_sent := true;
        Data value

  let empty () : 'a input = Sync.make_input @@ fun () -> Signal.EndOfSignal

  let from_list list =
    let list' = ref list in
    Sync.make_input @@ fun () ->
    match !list' with
    | value :: rest ->
        list' := rest;
        Signal.Data value
    | [] -> EndOfSignal

  let from_array array =
    let index = ref 0 in
    Sync.make_input @@ fun () ->
    match !index with
    | index' when index' < Array.length array ->
        index := index' + 1;
        Signal.Data array.(index')
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
end

module Async = struct end
