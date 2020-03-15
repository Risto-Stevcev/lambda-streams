
This thing:

```ocaml
# #require "lambda_streams";;
# open Lambda_streams;;
# let foo = Finite.Sync.from_list [1; 2; 3];;
val foo : int Finite.Sync.input = <fun>
# let bar = foo |> Finite.Sync.map (( * ) 2) |> Finite.Sync.to_list;;
val bar : int list = [2; 4; 6]
```



