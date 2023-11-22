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
  Sys.remove tamfile;    (* à commenter si on veut étudier le code TAM. *)
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
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testprintbool" =
  runtam (pathFichiersRat^"testprintbool.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testprintrat" =
   runtam (pathFichiersRat^"testprintrat.rat");
   [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testaddint" =
  runtam (pathFichiersRat^"testaddint.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testaddrat" =
  runtam (pathFichiersRat^"testaddrat.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testmultint" =
  runtam (pathFichiersRat^"testmultint.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testmultrat" =
  runtam (pathFichiersRat^"testmultrat.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testnum" =
  runtam (pathFichiersRat^"testnum.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testdenom" =
  runtam (pathFichiersRat^"testdenom.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testwhile1" =
  runtam (pathFichiersRat^"testwhile1.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Erreur blanc")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Rat__PasseTdsRat.analyse_tds_expression in file "passeTdsRat.ml", line 25, characters 94-125
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 75, characters 23-51
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 118, characters 17-43
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Rat__PasseTdsRat.analyser in file "passeTdsRat.ml", line 170, characters 11-41
  Called from Rat__Compilateur.Compilateur.analyser in file "compilateur.ml", line 33, characters 15-32
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 94, characters 28-57
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 73, characters 2-43
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

let%expect_test "testif1" =
  runtam (pathFichiersRat^"testif1.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "testif2" =
  runtam (pathFichiersRat^"testif2.rat");
  [%expect{| Syntaxic error: asm.SyntaxicError: Error : Syntax error |}]

let%expect_test "factiter" =
  runtam (pathFichiersRat^"factiter.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Erreur blanc")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Rat__PasseTdsRat.analyse_tds_expression in file "passeTdsRat.ml", line 25, characters 94-125
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 75, characters 23-51
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 118, characters 17-43
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Rat__PasseTdsRat.analyser in file "passeTdsRat.ml", line 170, characters 11-41
  Called from Rat__Compilateur.Compilateur.analyser in file "compilateur.ml", line 33, characters 15-32
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 94, characters 28-57
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 85, characters 2-41
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

let%expect_test "complique" =
  runtam (pathFichiersRat^"complique.rat");
  [%expect.unreachable]
[@@expect.uncaught_exn {|
  (* CR expect_test_collector: This test expectation appears to contain a backtrace.
     This is strongly discouraged as backtraces are fragile.
     Please change this test to not include a backtrace. *)

  (Failure "Erreur blanc")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Rat__PasseTdsRat.analyse_tds_expression in file "passeTdsRat.ml", line 25, characters 94-125
  Called from Rat__PasseTdsRat.analyse_tds_expression in file "passeTdsRat.ml", line 25, characters 94-125
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 102, characters 15-43
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Rat__PasseTdsRat.analyse_tds_instruction in file "passeTdsRat.ml", line 118, characters 17-43
  Called from Stdlib__list.map in file "list.ml", line 92, characters 20-23
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Stdlib__list.map in file "list.ml", line 92, characters 32-39
  Called from Rat__PasseTdsRat.analyser in file "passeTdsRat.ml", line 170, characters 11-41
  Called from Rat__Compilateur.Compilateur.analyser in file "compilateur.ml", line 33, characters 15-32
  Called from Rat__Compilateur.compiler in file "compilateur.ml", line 94, characters 28-57
  Called from Sans_fonction_tam__Test.runtamcode in file "tests/tam/sans_fonction/test.ml", line 10, characters 16-32
  Called from Sans_fonction_tam__Test.runtam in file "tests/tam/sans_fonction/test.ml" (inlined), line 22, characters 15-46
  Called from Sans_fonction_tam__Test.(fun) in file "tests/tam/sans_fonction/test.ml", line 89, characters 2-42
  Called from Expect_test_collector.Make.Instance_io.exec in file "collector/expect_test_collector.ml", line 262, characters 12-19 |}]

