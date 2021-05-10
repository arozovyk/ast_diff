open Stdio

type t = A of t | Null | B of int | Foo 

(* let _ = compare_longident (Lident "sd") (Lident "s") |> print_int *)
let write_to_file content path =
  let tmp_path = path in
  let oc = Out_channel.create tmp_path in
  Out_channel.fprintf oc "%s\n" content;
  Out_channel.close oc

 

let _ =
  let args = Sys.argv in
  if Array.length args != 1 then (
    let path = Array.get Sys.argv 1 in
    let origin = Ast.get_preprocessed_structure path in
    let reparsed = Ast.get_reparsed_structure path in
    write_to_file (Ast.show_structure origin) "/tmp/original";
    write_to_file (Ast.show_structure reparsed) "/tmp/reparsed")
  else ()
