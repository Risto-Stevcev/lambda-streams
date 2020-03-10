open Lambda_streams

let signal_fmt ?none:(pp_none = Fmt.nop) pp_v ppf = function
  | Signal.EndOfSignal -> pp_none ppf ()
  | Data v -> pp_v ppf v

let signal e =
  let eq x y =
    match x, y with
    | Signal.Data a, Signal.Data b -> Alcotest.equal e a b
    | EndOfSignal, EndOfSignal -> true
    | _ -> false
  in
  Alcotest.testable (signal_fmt (Alcotest.pp e)) eq

let test_pure _ = Alcotest.(check (signal int)) "" (Signal.pure 123) (Data 123)

let test_empty _ = Alcotest.(check (signal int)) "" (Signal.empty ()) EndOfSignal

let test_map _ =
  Alcotest.(check (signal int)) "" (Signal.pure 123 |> Signal.map (( + ) 1)) (Signal.pure 124);
  Alcotest.(check (signal int)) "" (Signal.empty () |> Signal.map (( + ) 1)) (Signal.empty ())

let test_filter _ =
  Alcotest.(check (signal int)) "" (Signal.pure 1 |> Signal.filter (( < ) 3)) EndOfSignal;
  Alcotest.(check (signal int)) "" (Signal.pure 4 |> Signal.filter (( < ) 3)) (Signal.pure 4)

let test_fold _ =
  Alcotest.(check int) "" (Signal.pure 6 |> Signal.fold ( + ) 4) 10;
  Alcotest.(check int) "" (Signal.empty () |> Signal.fold ( + ) 4) 4

let test_from_option _ =
  Alcotest.(check (signal int)) "" (Some 123 |> Signal.from_option) (Signal.pure 123)

let test_to_option _ =
  Alcotest.(check (option int)) "" (Signal.pure 123 |> Signal.to_option) (Some 123)

let suite =
  [
    "pure", `Quick, test_pure;
    "empty", `Quick, test_empty;
    "map", `Quick, test_map;
    "filter", `Quick, test_filter;
    "fold", `Quick, test_fold;
    "from_option", `Quick, test_from_option;
    "to_option", `Quick, test_to_option;
  ]
