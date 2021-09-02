(** test.ml *)

open Test_compiler

let () =
  print_endline "testing...";

  test_compiler
    "append(cons(a, cons(b, nil)), cons(c, nil)). \n\
    \     R -> append(cons(H, T), L) :- R -> cons(H, append(T, L)). \n\
    \     R -> append(nil, L) :- R -> L";

  test_compiler "\\X.X -> a(X)";

  test_compiler "a, a, a :- b"
