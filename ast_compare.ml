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
  if Array.length args != 1 then
    let path = Array.get Sys.argv 1 in
    try
      let origin = Ast.get_preprocessed_structure path in
      let reparsed = Ast.get_reparsed_structure path in
      let open Ast_diff in
      let x = parse_ast#structure origin reparsed [] |> List.rev in
      if List.length x > 0 then (
        print_endline ("Found some diffs for " ^ path);
        let res_o, res_r =
          List.map
            (fun { method_name; fst_node_pp; snd_node_pp } ->
              ( method_name ^ "\n\n" ^ fst_node_pp,
                method_name ^ "\n\n" ^ snd_node_pp ))
            x
          |> Base.List.unzip
        in

        let str_res_o = List.fold_left (fun x y -> x ^ y) "" res_o in
        let str_res_r = List.fold_left (fun x y -> x ^ y) "" res_r in
        let re1 = Re.Perl.compile_pat "\\w+\\.pp\\.ml" in
        let r = Re.matches re1 path |> List.hd in
        let r = String.sub r 0 (String.length r - 6) in
        let name1 = r ^ "-origin" in
        let name2 = r ^ "-reparsed" in
        let re2 = Re.Perl.compile_pat "\\/" in
        let fname1 =
          Re.replace_string re2 ~by:"?" path |> Re.replace_string re1 ~by:name1
        in
        let fname2 =
          Re.replace_string re2 ~by:"?" path |> Re.replace_string re1 ~by:name2
        in
        (*         print_endline ("writing:"^path);
 *)
        write_to_file str_res_o ("/tmp/diffs/" ^ fname1);
        write_to_file str_res_r ("/tmp/diffs/" ^ fname2))
      else (* print_endline ("ASTs are identical for : " ^ path) *) ()
    with e -> print_endline (Printexc.to_string e)
  else
    print_endline
      ("Invalid number of arguments\nUsage: " ^ Array.get Sys.argv 0 ^ " [file]")
