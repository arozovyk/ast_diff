open Stdio

let parse_ast =
  object
    inherit Ast_diff.find_diff
  end

(* let parse_ast =
  object (_self)
    inherit [string list] Comp.fold

    method string s s' a = ("s:" ^ s ^ "-s':" ^ s') :: a

    method option f o o' a =
      match (o, o') with Some x, Some y -> f x y a @ a | _ -> a

    method list f l l' a =
      if List.length l = List.length l' then
        List.map2 (fun x y -> f x y a) l l' |> List.flatten
      else a

    method int i i' a =
      ("i:" ^ Int.to_string i ^ "-i':" ^ Int.to_string i') :: a

    method char c c' a = ("c:" ^ Char.escaped c ^ "-b':" ^ Char.escaped c') :: a

    method bool b b' a =
      ("b:" ^ Bool.to_string b ^ "-c':" ^ Bool.to_string b') :: a
  end *)

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
    let open Ast_diff in
    parse_ast#structure origin reparsed []
    |> List.iter (fun x -> print_endline (diff_to_string x));
    write_to_file (Ast.show_structure origin) "/tmp/original";
    write_to_file (Ast.show_structure reparsed) "/tmp/reparsed")
  else ()
