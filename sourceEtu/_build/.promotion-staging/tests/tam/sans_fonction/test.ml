open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar /mnt/n7fs/.../tools/runtam/runtam.jar" *)

(* Execute the TAM code obtained from the rat file and return the ouptut of this code *)
let runtamcode cmde ratfile =
  let tamcode = compiler ratfile in
  let (tamfile, chan) = Filename.open_temp_file "test" ".tam" in
  output_string chan tamcode;
  close_out chan;
  let ic = Unix.open_process_in (cmde ^ " " ^ tamfile) in
  let printed = input_line ic in
  close_in ic;
  (* Sys.remove tamfile;    à commenter si on veut étudier le code TAM. *)
  String.trim printed

(* Compile and run ratfile, then print its output *)
let runtam ratfile =
  print_string (runtamcode runtamcmde ratfile)

(****************************************)
(** Chemin d'accès aux fichiers de test *)
(****************************************)

let pathFichiersRat = "../../../../../tests/tam/sans_fonction/fichiersRat/"

(**********)
(*  TESTS *)
(**********)

(* requires ppx_expect in jbuild, and `opam install ppx_expect` *)

let%expect_test "testprintint" =
  runtam (pathFichiersRat^"testprintint.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 37, characters 2-45
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testprintint.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testprintbool" =
  runtam (pathFichiersRat^"testprintbool.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 41, characters 2-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testprintbool.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testprintrat" =
   runtam (pathFichiersRat^"testprintrat.rat");
   [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 45, characters 3-46
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testprintrat.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testaddint" =
  runtam (pathFichiersRat^"testaddint.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 49, characters 2-43
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testaddint.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testaddrat" =
  runtam (pathFichiersRat^"testaddrat.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 53, characters 2-43
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testaddrat.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testmultint" =
  runtam (pathFichiersRat^"testmultint.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 57, characters 2-44
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testmultint.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testmultrat" =
  runtam (pathFichiersRat^"testmultrat.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 61, characters 2-44
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testmultrat.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testnum" =
  runtam (pathFichiersRat^"testnum.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 65, characters 2-40
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testnum.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testdenom" =
  runtam (pathFichiersRat^"testdenom.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 69, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testdenom.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testwhile1" =
  runtam (pathFichiersRat^"testwhile1.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 73, characters 2-43
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testwhile1.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testif1" =
  runtam (pathFichiersRat^"testif1.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 77, characters 2-40
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testif1.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "testif2" =
  runtam (pathFichiersRat^"testif2.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 81, characters 2-40
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/testif2.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "factiter" =
  runtam (pathFichiersRat^"factiter.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 85, characters 2-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/factiter.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

let%expect_test "complique" =
  runtam (pathFichiersRat^"complique.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  ("Rat.Lexer.Error(\"Unexpected char: \\r at 6-7\")")
  Raised at Rat__Lexer.error in file "lexer.mll", line 9, characters 4-182
  Called from Rat__Parser._menhir_run_13 in file "parser.ml", line 1249, characters 17-45
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 93, characters 14-45
  Re-raised at Rat__Compilateur.compiler in file "compilateur.ml", line 98, characters 6-13
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 89, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19

  Trailing output
  ---------------
  File "../../../../../tests/tam/sans_fonction/fichiersRat/complique.rat", line 1, characters 7-8: lexical error (unexpected character). |}]

