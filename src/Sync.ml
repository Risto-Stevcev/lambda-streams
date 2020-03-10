type 'a input = unit -> 'a

and 'a output = 'a -> unit

let make_input f = f

let make_output f = f

let pure value () = value

let enumerate () =
  let index = ref 0 in
  fun () ->
    index := !index + 1;
    !index

let next stream = stream ()

let accumulate n f init stream =
  let index = ref 0 and acc = ref init in
  while !index < n do
    index := !index + 1;
    acc := f !acc (stream ())
  done;
  !acc

let map f stream () = stream () |> f

let scan f init stream =
  let acc = ref init in
  fun () ->
    acc := f !acc (stream ());
    !acc
