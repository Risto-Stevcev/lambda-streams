let () =
  print_newline ();
  Alcotest.run
    "Lambda_streams"
    [
      "Signal", Test_Signal.suite;
      "Finite.Sync", Test_Finite_Sync.suite;
      "Finite.Async", Test_Finite_Async.suite;
      "Sync", Test_Sync.suite;
      "Async", Test_Async.suite;
    ]
