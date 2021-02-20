let () =
  Finite.Sync.from_list [1; 1; 2; 3; 5; 8]
  |> Finite.Sync.map (( * ) 2)
  |> Finite.Sync.pipe (Finite.Sync.make_output Js.log)
