(* open Util *)

let run_test i code =
  try
    code |> Compiler.compile |> Generator.string_of_prog
    |> ( ^ ) @@ string_of_int i ^ " :\n"
    |> print_endline
  with
  | Corelang.CompileError message ->
      prerr_endline @@ "Compile Error: " ^ message
  | e ->
      let msg = Printexc.to_string e and stack = Printexc.get_backtrace () in
      Printf.eprintf "Fatal error: exception %s%s\n" msg stack

let run_tests = List.iteri run_test

let () =
  run_tests
    [
      "a";
      "a, b";
      "a, b. c";
      "a :- b";
      "a :- b. c";
      "R = append(nil, L) :- R = L. R = append(cons(H, T), L) :- R = cons(H, \
       append(T, L)).";
      "R = append([], L) :- R = L. R = append([H | T], L) :- R = [H | \
       append(T, L)]. ";
      "R = append([], L) :- R = L. R = append([H | T], L) :- R = [H | \
       append(T, L)]. hoge = [a, b, [c, d|X], Y |[d, g|nil]], X = Y.";
    ]
