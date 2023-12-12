open Rat
open Compilateur

(* Changer le chemin d'accès du jar. *)
let runtamcmde = "java -jar ../../../../../tests/runtam.jar"
(* let runtamcmde = "java -jar mnt/n7fs/...//tools/runtam/runtam.jar" *)

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
  [%expect{|
    LOADL 42

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testprintbool" =
  runtam (pathFichiersRat^"testprintbool.rat");
  [%expect{| tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testprintrat" =
   runtam (pathFichiersRat^"testprintrat.rat");
   [%expect{| tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testaddint" =
  runtam (pathFichiersRat^"testaddint.rat");
  [%expect{|
    SUBR IAdd

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testaddrat" =
  runtam (pathFichiersRat^"testaddrat.rat");
  [%expect{| tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testmultint" =
  runtam (pathFichiersRat^"testmultint.rat");
  [%expect{|
    SUBR IMul

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testmultrat" =
  runtam (pathFichiersRat^"testmultrat.rat");
  [%expect{| tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testnum" =
  runtam (pathFichiersRat^"testnum.rat");
  [%expect{|
    POP (0) 1

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testdenom" =
  runtam (pathFichiersRat^"testdenom.rat");
  [%expect{|
    POP (1) 1

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testwhile1" =
  runtam (pathFichiersRat^"testwhile1.rat");
  [%expect{|
    LOAD (1) 0[SB]

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testif1" =
  runtam (pathFichiersRat^"testif1.rat");
  [%expect{|
    LOADL 18

    SUBR IOut

    LOADL 21

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "testif2" =
  runtam (pathFichiersRat^"testif2.rat");
  [%expect{|
    LOADL 18

    SUBR IOut

    LOADL 21

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "factiter" =
  runtam (pathFichiersRat^"factiter.rat");
  [%expect{|
    LOAD (1) 0[SB]

    SUBR IOut

    tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

let%expect_test "complique" =
  runtam (pathFichiersRat^"complique.rat");
  [%expect{| tam.TamException: Tam error : Program has failed due to an invalid code address. |}]

