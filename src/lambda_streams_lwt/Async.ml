open Lambda_streams

let ( >>= ) = Lwt.( >>= )

let to_lwt_stream stream =
  let lwt_stream, push = Lwt_stream.create () in
  stream |> Async.listen (fun value -> value |> Signal.to_option |> push);
  lwt_stream

let last_to_lwt stream = stream |> to_lwt_stream |> Lwt_stream.last_new

let to_lwt_list stream = stream |> to_lwt_stream |> Lwt_stream.to_list

let from_lwt promise =
  Async.make @@ fun cb ->
  Lwt.on_success promise (fun value ->
      cb @@ Signal.Data value;
      cb EndOfSignal)

let from_lwt_stream lwt_stream =
  let rec iterate cb s =
    try
      Lwt_stream.next s >>= fun value ->
      cb @@ Signal.Data value;
      iterate cb s
    with Lwt_stream.Empty | Lwt_stream.Closed ->
      cb EndOfSignal;
      Lwt.return ()
  in
  Async.make @@ fun cb -> lwt_stream |> iterate cb |> ignore
