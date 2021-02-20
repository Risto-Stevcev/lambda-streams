open Lambda_streams

let delay d (stream : 'a Finite.Async.t) : 'a Finite.Async.t =
  Async.make @@ fun cb ->
  stream
  |> Async.listen (function
         | Signal.Data _ as data ->
             Lwt.on_success (Lwt_unix.sleep d) (fun _ ->
                 print_endline "foo";
                 cb data)
         | EndOfSignal as data -> Lwt.on_success (Lwt_unix.sleep d) (fun _ -> cb data))

let all : 'a Finite.Async.t list -> 'a Finite.Async.t =
 fun streams ->
  let openStreams = ref (List.length streams) in
  Async.make (fun send ->
      streams
      |> List.iter
           (Async.listen (function
               | Signal.Data _ as d -> send d
               | EndOfSignal as d ->
                   openStreams := !openStreams - 1;
                   if !openStreams = 0 then send d)))

let main _ () =
  print_endline "Foo.main";

  let arr = ref [] in

  (*
  Lwt.on_success (Lwt_unix.sleep 12.) (fun _ ->
      print_endline "foo";

      Alcotest.(check (list string)) "" !arr ["foo"]);
      *)

  let mvar = Lwt_mvar.create_empty () in


  Lwt.on_success (Lwt_unix.sleep 2.) (fun _ ->
      Lwt_mvar.put mvar () |> ignore);

  all
    [
      Lwt_stream.of_list ["1"; "2"; "3"] |> Lambda_streams_lwt.Async.from_lwt_stream |> delay 2.;
      Lwt_stream.of_list ["X"; "Y"; "Z"] |> Lambda_streams_lwt.Async.from_lwt_stream |> delay 1.;
    ]
  |> Async.listen (function
         | Signal.Data d ->
             print_endline d;
             arr := List.append !arr [d]
         | EndOfSignal ->
             arr := List.append !arr ["done"];
             Alcotest.(check (list string)) "" !arr ["foo"];
             Lwt_mvar.put mvar () |> ignore;
             print_endline "done");
  (* Lwt_unix.sleep 13. *)
  Lwt_mvar.take mvar;
