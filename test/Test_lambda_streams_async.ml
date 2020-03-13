
let test_async _ =
  let open Async in
  (Async.return 123 >>= fun value -> Async_kernel.return @@ (value * 2)) >>= fun value ->
  Alcotest.(check int) "" value 246 |> Async_kernel.return

let () =
  print_newline ();
  let _ = Alcotest_async.run
       "Lambda_streams_async"
       ["somesuite", [Alcotest_async.test_case "one" `Quick test_async]]
  in
  ()
