let test_lwt _ () =
  let open Lwt in
  (Lwt.return 123 >>= fun value -> Lwt.return @@ (value * 2)) >>= fun value ->
  Alcotest.(check int) "" value 246 |> Lwt.return

let () =
  print_newline ();
  Lwt_main.run
  @@ Alcotest_lwt.run
       "Lambda_streams_lwt"
       ["somesuite", [Alcotest_lwt.test_case "one" `Quick test_lwt]]
