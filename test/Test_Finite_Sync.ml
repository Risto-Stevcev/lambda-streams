open Lambda_streams

let signal = Test_Signal.signal

let rec finite_sync_fmt ?none:(pp_none = Fmt.nop) pp_v ppf value =
  match Sync.next value with
  | Signal.Data value' ->
      pp_v ppf value';
      finite_sync_fmt ~none:pp_none pp_v ppf value
  | EndOfSignal -> pp_none ppf ()

let finite_sync e =
  let rec eq x y =
    match Sync.next x, Sync.next y with
    | Signal.Data a, Signal.Data b -> Alcotest.equal e a b && eq x y
    | EndOfSignal, EndOfSignal -> true
    | _ -> false
  in
  Alcotest.testable (finite_sync_fmt (Alcotest.pp e)) eq

let test_arrays_not_transducers _ =
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

  Alcotest.(check int) "" result 12;
  Alcotest.(check (list string))
    ""
    (List.rev !invocations)
    ["map"; "map"; "map"; "filter"; "filter"; "filter"; "reduce"; "reduce"; "reduce"]

let test_streams_transducers _ =
  let invocations = ref [] in
  let result =
    Finite.Sync.from_array [|1; 2; 3|]
    |> Finite.Sync.map (fun e ->
           invocations := "map" :: !invocations;
           e * 2)
    |> Finite.Sync.until (fun e ->
           invocations := "filter" :: !invocations;
           e < 10)
    |> Finite.Sync.fold_left
         (fun acc e ->
           invocations := "reduce" :: !invocations;
           acc + e)
         0
  in

  Alcotest.(check int) "" result 12;
  Alcotest.(check (list string))
    ""
    (List.rev !invocations)
    ["map"; "filter"; "reduce"; "map"; "filter"; "reduce"; "map"; "filter"; "reduce"]

let test_pure _ =
  let stream = Finite.Sync.pure 123 in
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 123);
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal

let test_empty _ = Alcotest.(check (signal int)) "" (Finite.Sync.empty () |> Sync.next) EndOfSignal

let test_from_list _ =
  Alcotest.(check (signal int)) "" (Finite.Sync.from_list [] |> Sync.next) EndOfSignal;

  let stream = Finite.Sync.from_list [1; 2; 3] in
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 1);
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 2);
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 3);
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal;
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal

let test_from_array _ =
  Alcotest.(check (signal int)) "" (Finite.Sync.from_array [||] |> Sync.next) EndOfSignal;

  let stream = Finite.Sync.from_array [|1; 2; 3|] in
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 1);
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 2);
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 3);
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal;
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal

let test_map _ =
  let stream = Finite.Sync.pure 123 |> Finite.Sync.map (( + ) 1) in
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 124);
  Alcotest.(check (signal int)) "" (Sync.next stream) EndOfSignal;

  let stream2 = Finite.Sync.empty () |> Finite.Sync.map (( + ) 1) in
  Alcotest.(check (signal int)) "" (Sync.next stream2) EndOfSignal;

  let stream3 = Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.map (( * ) 2) in
  Alcotest.(check (signal int)) "" (Sync.next stream3) (Data 2);
  Alcotest.(check (signal int)) "" (Sync.next stream3) (Data 4);
  Alcotest.(check (signal int)) "" (Sync.next stream3) (Data 6);
  Alcotest.(check (signal int)) "" (Sync.next stream3) EndOfSignal;
  Alcotest.(check (signal int)) "" (Sync.next stream3) EndOfSignal

let test_until _ =
  let stream = Finite.Sync.pure 123 |> Finite.Sync.until (( < ) 3) in
  Alcotest.(check (signal int)) "" (Sync.next stream) (Data 123);

  let stream2 = Finite.Sync.pure 1 |> Finite.Sync.until (( < ) 3) in
  Alcotest.(check (signal int)) "" (Sync.next stream2) EndOfSignal;

  let stream3 = Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.until (( > ) 3) in
  Alcotest.(check (signal int)) "" (Sync.next stream3) (Data 1);
  Alcotest.(check (signal int)) "" (Sync.next stream3) (Data 2);
  Alcotest.(check (signal int)) "" (Sync.next stream3) EndOfSignal

let test_fold_left _ =
  Alcotest.(check int) "" (Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.fold_left ( + ) 0) 6

let test_to_rev_list _ =
  Alcotest.(check (list int))
    ""
    (Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.to_rev_list)
    [3; 2; 1]

let test_to_list _ =
  Alcotest.(check (list int))
    ""
    (Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.to_list)
    [1; 2; 3]

let test_to_array _ =
  Alcotest.(check (array int))
    ""
    (Finite.Sync.from_array [|1; 2; 3|] |> Finite.Sync.to_array)
    [|1; 2; 3|]

let suite =
  [
    "Arrays are not transducers", `Quick, test_arrays_not_transducers;
    "Streams are transducers", `Quick, test_streams_transducers;
    "pure", `Quick, test_pure;
    "empty", `Quick, test_empty;
    "from_list", `Quick, test_from_list;
    "from_array", `Quick, test_from_array;
    "map", `Quick, test_map;
    "until", `Quick, test_until;
    "fold_left", `Quick, test_fold_left;
    "to_rev_list", `Quick, test_to_rev_list;
    "to_list", `Quick, test_to_list;
    "to_array", `Quick, test_to_array;
  ]
