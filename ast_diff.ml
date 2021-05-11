open Ast

type diff = { method_name : string; fst_node_pp : string; snd_node_pp : string }
[@@deriving ord]

let diff_to_string { method_name; fst_node_pp; snd_node_pp } =
  "method_name: " ^ method_name ^ "\n\n" ^ "fst_node_pp:\n\n" ^ fst_node_pp
  ^ "\n\n" ^ "snd_node_pp:\n\n" ^ snd_node_pp

let i = ref 0

let add_diff method_name fst_node_pp snd_node_pp acc =
  let r = { method_name; fst_node_pp; snd_node_pp } in
  i := !i + 1;
  print_endline ("adding:" ^ Int.to_string !i);
  print_endline ("size add diff is :" ^ Int.to_string (List.length acc));
  (*       print_endline ("adding : " ^ diff_to_string r); 
 *) r :: acc

class find_diff =
  object (self)
    method bool : bool -> bool -> diff list -> diff list = fun _ _ d -> d

    method char : char -> char -> diff list -> diff list = fun _ _ d -> d

    method int : int -> int -> diff list -> diff list = fun _ _ d -> d

    method list
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a list ->
          'a list ->
          diff list ->
          diff list =
      fun f l l' a ->
        if List.length l = List.length l' && List.length l > 0 then
          (* print_endline
             ("size before flatten is :" ^ Int.to_string (List.length a)); *)
          let a =
            List.map2 (fun x y -> f x y a) l l' |> List.flatten |> ( @ ) a
          in
          (* print_endline
             ("size after flatten is :" ^ Int.to_string (List.length a)); *)
          let a = Base.List.dedup_and_sort ~compare:compare_diff a in
          a
        else (
          print_endline "different list sizes";
          a)

    method option
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a option ->
          'a option ->
          diff list ->
          diff list =
      fun f o o' a ->
        match (o, o') with Some x, Some y -> f x y a @ a | _ -> a

    method string : string -> string -> diff list -> diff list = fun _ _ d -> d

    method position : position -> position -> diff list -> diff list =
      (* fun ({ pos_fname; pos_lnum; pos_bol; pos_cnum } as p)
          ({
             pos_fname = pos_fname';
             pos_lnum = pos_lnum';
             pos_bol = pos_bol';
             pos_cnum = pos_cnum';
           } as p')  acc ->
            if equal_position p p' then
              let acc = self#string pos_fname pos_fname' acc in
              let acc = self#int pos_lnum pos_lnum' acc in
              let acc = self#int pos_bol pos_bol' acc in
              let acc = self#int pos_cnum pos_cnum' acc in
              acc
            else add_diff "position" (show_position p) (show_position p') acc *)
      fun _ _ a -> a

    method location : location -> location -> diff list -> diff list =
      (* fun ({ loc_start; loc_end; loc_ghost } as p)
           ({
              loc_start = loc_start';
              loc_end = loc_end';
              loc_ghost = loc_ghost';
            } as p') acc ->
         if equal_location p p' then
           let acc = self#position loc_start loc_start' acc in
           let acc = self#position loc_end loc_end' acc in
           let acc = self#bool loc_ghost loc_ghost' acc in
           acc
         else add_diff "location" (show_location p) (show_location p') acc *)
      fun _ _ a -> a

    method location_stack
        : location_stack -> location_stack -> diff list -> diff list =
      (* fun l l' a ->
         if equal_location_stack l l' then self#list self#location l l' a
         else
           add_diff "location_stack" (show_location_stack l)
             (show_location_stack l') a *)
      fun _ _ a -> a

    method loc
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a loc ->
          'a loc ->
          diff list ->
          diff list =
      (* fun _a { txt; loc } { txt = txt'; loc = loc' } acc ->
         let acc = _a txt txt' acc in
         let acc = self#location loc loc' acc in
         acc *)
      fun _ _ _ a -> a

    method longident : longident -> longident -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Lident a, Lident a' -> self#string a a' acc
        | Ldot (a, b), Ldot (a', b') ->
            let acc = self#longident a a' acc in
            let acc = self#string b b' acc in
            acc
        | Lapply (a, b), Lapply (a', b') ->
            let acc = self#longident a a' acc in
            let acc = self#longident b b' acc in
            acc
        | _, _ -> acc

    method longident_loc
        : longident_loc -> longident_loc -> diff list -> diff list =
      fun l l' a -> self#loc self#longident l l' a

    method rec_flag : rec_flag -> rec_flag -> diff list -> diff list =
      fun _ _ acc -> acc

    method direction_flag
        : direction_flag -> direction_flag -> diff list -> diff list =
      fun _ _ acc -> acc

    method private_flag : private_flag -> private_flag -> diff list -> diff list
        =
      fun _ _ acc -> acc

    method mutable_flag : mutable_flag -> mutable_flag -> diff list -> diff list
        =
      fun _ _ acc -> acc

    method virtual_flag : virtual_flag -> virtual_flag -> diff list -> diff list
        =
      fun _ _ acc -> acc

    method override_flag
        : override_flag -> override_flag -> diff list -> diff list =
      fun _ _ acc -> acc

    method closed_flag : closed_flag -> closed_flag -> diff list -> diff list =
      fun _ _ acc -> acc

    method label : label -> label -> diff list -> diff list = self#string

    method arg_label : arg_label -> arg_label -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Nolabel, Nolabel -> acc
        | Labelled a, Labelled a' -> self#string a a' acc
        | Optional a, Optional a' -> self#string a a' acc
        | _ -> acc

    method variance : variance -> variance -> diff list -> diff list =
      fun _ _ acc -> acc

    method injectivity : injectivity -> injectivity -> diff list -> diff list =
      fun _ _ acc -> acc

    method constant : constant -> constant -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pconst_integer (a, b), Pconst_integer (a', b') ->
            let acc = self#string a a' acc in
            let acc = self#option self#char b b' acc in
            acc
        | Pconst_char a, Pconst_char a' -> self#char a a' acc
        | Pconst_string (a, b, c), Pconst_string (a', b', c') ->
            let acc = self#string a a' acc in
            let acc = self#location b b' acc in
            let acc = self#option self#string c c' acc in
            acc
        | Pconst_float (a, b), Pconst_float (a', b') ->
            let acc = self#string a a' acc in
            let acc = self#option self#char b b' acc in
            acc
        | _ -> acc

    method attribute : attribute -> attribute -> diff list -> diff list =
      fun { attr_name; attr_payload; attr_loc }
          {
            attr_name = attr_name';
            attr_payload = attr_payload';
            attr_loc = attr_loc';
          } acc ->
        let acc = self#loc self#string attr_name attr_name' acc in
        let acc = self#payload attr_payload attr_payload' acc in
        let acc = self#location attr_loc attr_loc' acc in
        acc

    method extension : extension -> extension -> diff list -> diff list =
      fun (a, b) (a', b') acc ->
        let acc = self#loc self#string a a' acc in
        let acc = self#payload b b' acc in
        acc

    method attributes : attributes -> attributes -> diff list -> diff list =
      self#list self#attribute

    method payload : payload -> payload -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | PStr a, PStr a' -> self#structure a a' acc
        | PSig a, PSig a' -> self#signature a a' acc
        | PTyp a, PTyp a' -> self#core_type a a' acc
        | PPat (a, b), PPat (a', b') ->
            let acc = self#pattern a a' acc in
            let acc = self#option self#expression b b' acc in
            acc
        | _ -> acc

    method core_type : core_type -> core_type -> diff list -> diff list =
      fun { ptyp_desc; ptyp_loc; ptyp_loc_stack; ptyp_attributes }
          {
            ptyp_desc = ptyp_desc';
            ptyp_loc = ptyp_loc';
            ptyp_loc_stack = ptyp_loc_stack';
            ptyp_attributes = ptyp_attributes';
          } acc ->
        let acc = self#core_type_desc ptyp_desc ptyp_desc' acc in
        let acc = self#location ptyp_loc ptyp_loc' acc in
        let acc = self#location_stack ptyp_loc_stack ptyp_loc_stack' acc in
        let acc = self#attributes ptyp_attributes ptyp_attributes' acc in
        acc

    method core_type_desc
        : core_type_desc -> core_type_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Ptyp_any, Ptyp_any -> acc
        | Ptyp_var a, Ptyp_var a' -> self#string a a' acc
        | Ptyp_arrow (a, b, c), Ptyp_arrow (a', b', c') ->
            let acc = self#arg_label a a' acc in
            let acc = self#core_type b b' acc in
            let acc = self#core_type c c' acc in
            acc
        | Ptyp_tuple a, Ptyp_tuple a' -> self#list self#core_type a a' acc
        | Ptyp_constr (a, b), Ptyp_constr (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#list self#core_type b b' acc in
            acc
        | Ptyp_object (a, b), Ptyp_object (a', b') ->
            let acc = self#list self#object_field a a' acc in
            let acc = self#closed_flag b b' acc in
            acc
        | Ptyp_class (a, b), Ptyp_class (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#list self#core_type b b' acc in
            acc
        | Ptyp_alias (a, b), Ptyp_alias (a', b') ->
            let acc = self#core_type a a' acc in
            let acc = self#string b b' acc in
            acc
        | Ptyp_variant (a, b, c), Ptyp_variant (a', b', c') ->
            let acc = self#list self#row_field a a' acc in
            let acc = self#closed_flag b b' acc in
            let acc = self#option (self#list self#label) c c' acc in
            acc
        | Ptyp_poly (a, b), Ptyp_poly (a', b') ->
            let acc = self#list (self#loc self#string) a a' acc in
            let acc = self#core_type b b' acc in
            acc
        | Ptyp_package a, Ptyp_package a' -> self#package_type a a' acc
        | Ptyp_extension a, Ptyp_extension a' -> self#extension a a' acc
        | _ -> acc

    method package_type : package_type -> package_type -> diff list -> diff list
        =
      fun (a, b) (a', b') acc ->
        let acc = self#longident_loc a a' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') acc ->
              let acc = self#longident_loc a a' acc in
              let acc = self#core_type b b' acc in
              acc)
            b b' acc
        in
        acc

    method row_field : row_field -> row_field -> diff list -> diff list =
      fun { prf_desc; prf_loc; prf_attributes }
          {
            prf_desc = prf_desc';
            prf_loc = prf_loc';
            prf_attributes = prf_attributes';
          } acc ->
        let acc = self#row_field_desc prf_desc prf_desc' acc in
        let acc = self#location prf_loc prf_loc' acc in
        let acc = self#attributes prf_attributes prf_attributes' acc in
        acc

    method row_field_desc
        : row_field_desc -> row_field_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Rtag (a, b, c), Rtag (a', b', c') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#bool b b' acc in
            let acc = self#list self#core_type c c' acc in
            acc
        | Rinherit a, Rinherit a' -> self#core_type a a' acc
        | _ -> acc

    method object_field : object_field -> object_field -> diff list -> diff list
        =
      fun { pof_desc; pof_loc; pof_attributes }
          {
            pof_desc = pof_desc';
            pof_loc = pof_loc';
            pof_attributes = pof_attributes';
          } acc ->
        let acc = self#object_field_desc pof_desc pof_desc' acc in
        let acc = self#location pof_loc pof_loc' acc in
        let acc = self#attributes pof_attributes pof_attributes' acc in
        acc

    method object_field_desc
        : object_field_desc -> object_field_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Otag (a, b), Otag (a', b') ->
            let acc = self#loc self#label a a' acc in
            let acc = self#core_type b b' acc in
            acc
        | Oinherit a, Oinherit a' -> self#core_type a a' acc
        | _ -> acc

    method pattern : pattern -> pattern -> diff list -> diff list =
      fun { ppat_desc; ppat_loc; ppat_loc_stack; ppat_attributes }
          {
            ppat_desc = ppat_desc';
            ppat_loc = ppat_loc';
            ppat_loc_stack = ppat_loc_stack';
            ppat_attributes = ppat_attributes';
          } acc ->
        let acc = self#pattern_desc ppat_desc ppat_desc' acc in
        let acc = self#location ppat_loc ppat_loc' acc in
        let acc = self#location_stack ppat_loc_stack ppat_loc_stack' acc in
        let acc = self#attributes ppat_attributes ppat_attributes' acc in
        acc

    method pattern_desc : pattern_desc -> pattern_desc -> diff list -> diff list
        =
      fun x x' acc ->
        match (x, x') with
        | Ppat_any, Ppat_any -> acc
        | Ppat_var a, Ppat_var a' -> self#loc self#string a a' acc
        | Ppat_alias (a, b), Ppat_alias (a', b') ->
            let acc = self#pattern a a' acc in
            let acc = self#loc self#string b b' acc in
            acc
        | Ppat_constant a, Ppat_constant a' -> self#constant a a' acc
        | Ppat_interval (a, b), Ppat_interval (a', b') ->
            let acc = self#constant a a' acc in
            let acc = self#constant b b' acc in
            acc
        | Ppat_tuple a, Ppat_tuple a' -> self#list self#pattern a a' acc
        | Ppat_construct (a, b), Ppat_construct (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#option self#pattern b b' acc in
            acc
        | Ppat_variant (a, b), Ppat_variant (a', b') ->
            let acc = self#label a a' acc in
            let acc = self#option self#pattern b b' acc in
            acc
        | Ppat_record (a, b), Ppat_record (a', b') ->
            let acc =
              self#list
                (fun (a, b) (a', b') acc ->
                  let acc = self#longident_loc a a' acc in
                  let acc = self#pattern b b' acc in
                  acc)
                a a' acc
            in
            let acc = self#closed_flag b b' acc in
            acc
        | Ppat_array a, Ppat_array a' -> self#list self#pattern a a' acc
        | Ppat_or (a, b), Ppat_or (a', b') ->
            let acc = self#pattern a a' acc in
            let acc = self#pattern b b' acc in
            acc
        | Ppat_constraint (a, b), Ppat_constraint (a', b') ->
            let acc = self#pattern a a' acc in
            let acc = self#core_type b b' acc in
            acc
        | Ppat_type a, Ppat_type a' -> self#longident_loc a a' acc
        | Ppat_lazy a, Ppat_lazy a' -> self#pattern a a' acc
        | Ppat_unpack a, Ppat_unpack a' ->
            self#loc (self#option self#string) a a' acc
        | Ppat_exception a, Ppat_exception a' -> self#pattern a a' acc
        | Ppat_extension a, Ppat_extension a' -> self#extension a a' acc
        | Ppat_open (a, b), Ppat_open (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#pattern b b' acc in
            acc
        | _ -> acc

    method expression : expression -> expression -> diff list -> diff list =
      fun { pexp_desc; pexp_loc; pexp_loc_stack; pexp_attributes }
          {
            pexp_desc = pexp_desc';
            pexp_loc = pexp_loc';
            pexp_loc_stack = pexp_loc_stack';
            pexp_attributes = pexp_attributes';
          } acc ->
        let acc = self#expression_desc pexp_desc pexp_desc' acc in
        let acc = self#location pexp_loc pexp_loc' acc in
        let acc = self#location_stack pexp_loc_stack pexp_loc_stack' acc in
        let acc = self#attributes pexp_attributes pexp_attributes' acc in
        acc

    method expression_desc
        : expression_desc -> expression_desc -> diff list -> diff list =
      fun x x' acc ->
        if equal_expression_desc x x' then
          match (x, x') with
          | Pexp_ident a, Pexp_ident a' -> self#longident_loc a a' acc
          | Pexp_constant a, Pexp_constant a' -> self#constant a a' acc
          | Pexp_let (a, b, c), Pexp_let (a', b', c') ->
              let acc = self#rec_flag a a' acc in
              let acc = self#list self#value_binding b b' acc in
              let acc = self#expression c c' acc in
              acc
          | Pexp_function a, Pexp_function a' -> self#cases a a' acc
          | Pexp_fun (a, b, c, d), Pexp_fun (a', b', c', d') ->
              let acc = self#arg_label a a' acc in
              let acc = self#option self#expression b b' acc in
              let acc = self#pattern c c' acc in
              let acc = self#expression d d' acc in
              acc
          | Pexp_apply (a, b), Pexp_apply (a', b') ->
              let acc = self#expression a a' acc in
              let acc =
                self#list
                  (fun (a, b) (a', b') acc ->
                    let acc = self#arg_label a a' acc in
                    let acc = self#expression b b' acc in
                    acc)
                  b b' acc
              in
              acc
          | Pexp_match (a, b), Pexp_match (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#cases b b' acc in
              acc
          | Pexp_try (a, b), Pexp_try (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#cases b b' acc in
              acc
          | Pexp_tuple a, Pexp_tuple a' -> self#list self#expression a a' acc
          | Pexp_construct (a, b), Pexp_construct (a', b') ->
              let acc = self#longident_loc a a' acc in
              let acc = self#option self#expression b b' acc in
              acc
          | Pexp_variant (a, b), Pexp_variant (a', b') ->
              let acc = self#label a a' acc in
              let acc = self#option self#expression b b' acc in
              acc
          | Pexp_record (a, b), Pexp_record (a', b') ->
              let acc =
                self#list
                  (fun (a, b) (a', b') acc ->
                    let acc = self#longident_loc a a' acc in
                    let acc = self#expression b b' acc in
                    acc)
                  a a' acc
              in
              let acc = self#option self#expression b b' acc in
              acc
          | Pexp_field (a, b), Pexp_field (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#longident_loc b b' acc in
              acc
          | Pexp_setfield (a, b, c), Pexp_setfield (a', b', c') ->
              let acc = self#expression a a' acc in
              let acc = self#longident_loc b b' acc in
              let acc = self#expression c c' acc in
              acc
          | Pexp_array a, Pexp_array a' -> self#list self#expression a a' acc
          | Pexp_ifthenelse (a, b, c), Pexp_ifthenelse (a', b', c') ->
              let acc = self#expression a a' acc in
              let acc = self#expression b b' acc in
              let acc = self#option self#expression c c' acc in
              acc
          | Pexp_sequence (a, b), Pexp_sequence (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_while (a, b), Pexp_while (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_for (a, b, c, d, e), Pexp_for (a', b', c', d', e') ->
              let acc = self#pattern a a' acc in
              let acc = self#expression b b' acc in
              let acc = self#expression c c' acc in
              let acc = self#direction_flag d d' acc in
              let acc = self#expression e e' acc in
              acc
          | Pexp_constraint (a, b), Pexp_constraint (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#core_type b b' acc in
              acc
          | Pexp_coerce (a, b, c), Pexp_coerce (a', b', c') ->
              let acc = self#expression a a' acc in
              let acc = self#option self#core_type b b' acc in
              let acc = self#core_type c c' acc in
              acc
          | Pexp_send (a, b), Pexp_send (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#loc self#label b b' acc in
              acc
          | Pexp_new a, Pexp_new a' -> self#longident_loc a a' acc
          | Pexp_setinstvar (a, b), Pexp_setinstvar (a', b') ->
              let acc = self#loc self#label a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_override a, Pexp_override a' ->
              self#list
                (fun (a, b) (a', b') acc ->
                  let acc = self#loc self#label a a' acc in
                  let acc = self#expression b b' acc in
                  acc)
                a a' acc
          | Pexp_letmodule (a, b, c), Pexp_letmodule (a', b', c') ->
              let acc = self#loc (self#option self#string) a a' acc in
              let acc = self#module_expr b b' acc in
              let acc = self#expression c c' acc in
              acc
          | Pexp_letexception (a, b), Pexp_letexception (a', b') ->
              let acc = self#extension_constructor a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_assert a, Pexp_assert a' -> self#expression a a' acc
          | Pexp_lazy a, Pexp_lazy a' -> self#expression a a' acc
          | Pexp_poly (a, b), Pexp_poly (a', b') ->
              let acc = self#expression a a' acc in
              let acc = self#option self#core_type b b' acc in
              acc
          | Pexp_object a, Pexp_object a' -> self#class_structure a a' acc
          | Pexp_newtype (a, b), Pexp_newtype (a', b') ->
              let acc = self#loc self#string a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_pack a, Pexp_pack a' -> self#module_expr a a' acc
          | Pexp_open (a, b), Pexp_open (a', b') ->
              let acc = self#open_declaration a a' acc in
              let acc = self#expression b b' acc in
              acc
          | Pexp_letop a, Pexp_letop a' -> self#letop a a' acc
          | Pexp_extension a, Pexp_extension a' -> self#extension a a' acc
          | Pexp_unreachable, Pexp_unreachable -> acc
          | _ -> acc
        else (
          print_endline ("inside expressiondesc:" ^ Int.to_string !i);
          print_endline ("size bef expr is :" ^ Int.to_string (List.length acc));
          let acc =
            add_diff "expression_desc" (show_expression_desc x)
              (show_expression_desc x') acc
          in
          print_endline
            ("size after expr is :" ^ Int.to_string (List.length acc));
          acc)

    method case : case -> case -> diff list -> diff list =
      fun { pc_lhs; pc_guard; pc_rhs }
          { pc_lhs = pc_lhs'; pc_guard = pc_guard'; pc_rhs = pc_rhs' } acc ->
        let acc = self#pattern pc_lhs pc_lhs' acc in
        let acc = self#option self#expression pc_guard pc_guard' acc in
        let acc = self#expression pc_rhs pc_rhs' acc in
        acc

    method letop : letop -> letop -> diff list -> diff list =
      fun { let_; ands; body } { let_ = let_'; ands = ands'; body = body' } acc ->
        let acc = self#binding_op let_ let_' acc in
        let acc = self#list self#binding_op ands ands' acc in
        let acc = self#expression body body' acc in
        acc

    method binding_op : binding_op -> binding_op -> diff list -> diff list =
      fun { pbop_op; pbop_pat; pbop_exp; pbop_loc }
          {
            pbop_op = pbop_op';
            pbop_pat = pbop_pat';
            pbop_exp = pbop_exp';
            pbop_loc = pbop_loc';
          } acc ->
        let acc = self#loc self#string pbop_op pbop_op' acc in
        let acc = self#pattern pbop_pat pbop_pat' acc in
        let acc = self#expression pbop_exp pbop_exp' acc in
        let acc = self#location pbop_loc pbop_loc' acc in
        acc

    method value_description
        : value_description -> value_description -> diff list -> diff list =
      fun { pval_name; pval_type; pval_prim; pval_attributes; pval_loc }
          {
            pval_name = pval_name';
            pval_type = pval_type';
            pval_prim = pval_prim';
            pval_attributes = pval_attributes';
            pval_loc = pval_loc';
          } acc ->
        let acc = self#loc self#string pval_name pval_name' acc in
        let acc = self#core_type pval_type pval_type' acc in
        let acc = self#list self#string pval_prim pval_prim' acc in
        let acc = self#attributes pval_attributes pval_attributes' acc in
        let acc = self#location pval_loc pval_loc' acc in
        acc

    method type_declaration
        : type_declaration -> type_declaration -> diff list -> diff list =
      fun {
            ptype_name;
            ptype_params;
            ptype_cstrs;
            ptype_kind;
            ptype_private;
            ptype_manifest;
            ptype_attributes;
            ptype_loc;
          }
          {
            ptype_name = ptype_name';
            ptype_params = ptype_params';
            ptype_cstrs = ptype_cstrs';
            ptype_kind = ptype_kind';
            ptype_private = ptype_private';
            ptype_manifest = ptype_manifest';
            ptype_attributes = ptype_attributes';
            ptype_loc = ptype_loc';
          } acc ->
        let acc = self#loc self#string ptype_name ptype_name' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') acc ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') acc ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            ptype_params ptype_params' acc
        in
        let acc =
          self#list
            (fun (a, b, c) (a', b', c') acc ->
              let acc = self#core_type a a' acc in
              let acc = self#core_type b b' acc in
              let acc = self#location c c' acc in
              acc)
            ptype_cstrs ptype_cstrs' acc
        in
        let acc = self#type_kind ptype_kind ptype_kind' acc in
        let acc = self#private_flag ptype_private ptype_private' acc in
        let acc =
          self#option self#core_type ptype_manifest ptype_manifest' acc
        in
        let acc = self#attributes ptype_attributes ptype_attributes' acc in
        let acc = self#location ptype_loc ptype_loc' acc in
        acc

    method type_kind : type_kind -> type_kind -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Ptype_abstract, Ptype_abstract -> acc
        | Ptype_variant a, Ptype_variant a' ->
            self#list self#constructor_declaration a a' acc
        | Ptype_record a, Ptype_record a' ->
            self#list self#label_declaration a a' acc
        | Ptype_open, Ptype_open -> acc
        | _ -> acc

    method label_declaration
        : label_declaration -> label_declaration -> diff list -> diff list =
      fun { pld_name; pld_mutable; pld_type; pld_loc; pld_attributes }
          {
            pld_name = pld_name';
            pld_mutable = pld_mutable';
            pld_type = pld_type';
            pld_loc = pld_loc';
            pld_attributes = pld_attributes';
          } acc ->
        let acc = self#loc self#string pld_name pld_name' acc in
        let acc = self#mutable_flag pld_mutable pld_mutable' acc in
        let acc = self#core_type pld_type pld_type' acc in
        let acc = self#location pld_loc pld_loc' acc in
        let acc = self#attributes pld_attributes pld_attributes' acc in
        acc

    method constructor_declaration
        : constructor_declaration ->
          constructor_declaration ->
          diff list ->
          diff list =
      fun { pcd_name; pcd_args; pcd_res; pcd_loc; pcd_attributes }
          {
            pcd_name = pcd_name';
            pcd_args = pcd_args';
            pcd_res = pcd_res';
            pcd_loc = pcd_loc';
            pcd_attributes = pcd_attributes';
          } acc ->
        let acc = self#loc self#string pcd_name pcd_name' acc in
        let acc = self#constructor_arguments pcd_args pcd_args' acc in
        let acc = self#option self#core_type pcd_res pcd_res' acc in
        let acc = self#location pcd_loc pcd_loc' acc in
        let acc = self#attributes pcd_attributes pcd_attributes' acc in
        acc

    method constructor_arguments
        : constructor_arguments ->
          constructor_arguments ->
          diff list ->
          diff list =
      fun x x' acc ->
        match (x, x') with
        | Pcstr_tuple a, Pcstr_tuple a' -> self#list self#core_type a a' acc
        | Pcstr_record a, Pcstr_record a' ->
            self#list self#label_declaration a a' acc
        | _ -> acc

    method type_extension
        : type_extension -> type_extension -> diff list -> diff list =
      fun {
            ptyext_path;
            ptyext_params;
            ptyext_constructors;
            ptyext_private;
            ptyext_loc;
            ptyext_attributes;
          }
          {
            ptyext_path = ptyext_path';
            ptyext_params = ptyext_params';
            ptyext_constructors = ptyext_constructors';
            ptyext_private = ptyext_private';
            ptyext_loc = ptyext_loc';
            ptyext_attributes = ptyext_attributes';
          } acc ->
        let acc = self#longident_loc ptyext_path ptyext_path' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') acc ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') acc ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            ptyext_params ptyext_params' acc
        in
        let acc =
          self#list self#extension_constructor ptyext_constructors
            ptyext_constructors' acc
        in
        let acc = self#private_flag ptyext_private ptyext_private' acc in
        let acc = self#location ptyext_loc ptyext_loc' acc in
        let acc = self#attributes ptyext_attributes ptyext_attributes' acc in
        acc

    method extension_constructor
        : extension_constructor ->
          extension_constructor ->
          diff list ->
          diff list =
      fun { pext_name; pext_kind; pext_loc; pext_attributes }
          {
            pext_name = pext_name';
            pext_kind = pext_kind';
            pext_loc = pext_loc';
            pext_attributes = pext_attributes';
          } acc ->
        let acc = self#loc self#string pext_name pext_name' acc in
        let acc = self#extension_constructor_kind pext_kind pext_kind' acc in
        let acc = self#location pext_loc pext_loc' acc in
        let acc = self#attributes pext_attributes pext_attributes' acc in
        acc

    method type_exception
        : type_exception -> type_exception -> diff list -> diff list =
      fun { ptyexn_constructor; ptyexn_loc; ptyexn_attributes }
          {
            ptyexn_constructor = ptyexn_constructor';
            ptyexn_loc = ptyexn_loc';
            ptyexn_attributes = ptyexn_attributes';
          } acc ->
        let acc =
          self#extension_constructor ptyexn_constructor ptyexn_constructor' acc
        in
        let acc = self#location ptyexn_loc ptyexn_loc' acc in
        let acc = self#attributes ptyexn_attributes ptyexn_attributes' acc in
        acc

    method extension_constructor_kind
        : extension_constructor_kind ->
          extension_constructor_kind ->
          diff list ->
          diff list =
      fun x x' acc ->
        match (x, x') with
        | Pext_decl (a, b), Pext_decl (a', b') ->
            let acc = self#constructor_arguments a a' acc in
            let acc = self#option self#core_type b b' acc in
            acc
        | Pext_rebind a, Pext_rebind a' -> self#longident_loc a a' acc
        | _ -> acc

    method class_type : class_type -> class_type -> diff list -> diff list =
      fun { pcty_desc; pcty_loc; pcty_attributes }
          {
            pcty_desc = pcty_desc';
            pcty_loc = pcty_loc';
            pcty_attributes = pcty_attributes';
          } acc ->
        let acc = self#class_type_desc pcty_desc pcty_desc' acc in
        let acc = self#location pcty_loc pcty_loc' acc in
        let acc = self#attributes pcty_attributes pcty_attributes' acc in
        acc

    method class_type_desc
        : class_type_desc -> class_type_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pcty_constr (a, b), Pcty_constr (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#list self#core_type b b' acc in
            acc
        | Pcty_signature a, Pcty_signature a' -> self#class_signature a a' acc
        | Pcty_arrow (a, b, c), Pcty_arrow (a', b', c') ->
            let acc = self#arg_label a a' acc in
            let acc = self#core_type b b' acc in
            let acc = self#class_type c c' acc in
            acc
        | Pcty_extension a, Pcty_extension a' -> self#extension a a' acc
        | Pcty_open (a, b), Pcty_open (a', b') ->
            let acc = self#open_description a a' acc in
            let acc = self#class_type b b' acc in
            acc
        | _ -> acc

    method class_signature
        : class_signature -> class_signature -> diff list -> diff list =
      fun { pcsig_self; pcsig_fields }
          { pcsig_self = pcsig_self'; pcsig_fields = pcsig_fields' } acc ->
        let acc = self#core_type pcsig_self pcsig_self' acc in
        let acc =
          self#list self#class_type_field pcsig_fields pcsig_fields' acc
        in
        acc

    method class_type_field
        : class_type_field -> class_type_field -> diff list -> diff list =
      fun { pctf_desc; pctf_loc; pctf_attributes }
          {
            pctf_desc = pctf_desc';
            pctf_loc = pctf_loc';
            pctf_attributes = pctf_attributes';
          } acc ->
        let acc = self#class_type_field_desc pctf_desc pctf_desc' acc in
        let acc = self#location pctf_loc pctf_loc' acc in
        let acc = self#attributes pctf_attributes pctf_attributes' acc in
        acc

    method class_type_field_desc
        : class_type_field_desc ->
          class_type_field_desc ->
          diff list ->
          diff list =
      fun x x' acc ->
        match (x, x') with
        | Pctf_inherit a, Pctf_inherit a' -> self#class_type a a' acc
        | Pctf_val a, Pctf_val a' ->
            (fun (a, b, c, d) (a', b', c', d') acc ->
              let acc = self#loc self#label a a' acc in
              let acc = self#mutable_flag b b' acc in
              let acc = self#virtual_flag c c' acc in
              let acc = self#core_type d d' acc in
              acc)
              a a' acc
        | Pctf_method a, Pctf_method a' ->
            (fun (a, b, c, d) (a', b', c', d') acc ->
              let acc = self#loc self#label a a' acc in
              let acc = self#private_flag b b' acc in
              let acc = self#virtual_flag c c' acc in
              let acc = self#core_type d d' acc in
              acc)
              a a' acc
        | Pctf_constraint a, Pctf_constraint a' ->
            (fun (a, b) (a', b') acc ->
              let acc = self#core_type a a' acc in
              let acc = self#core_type b b' acc in
              acc)
              a a' acc
        | Pctf_attribute a, Pctf_attribute a' -> self#attribute a a' acc
        | Pctf_extension a, Pctf_extension a' -> self#extension a a' acc
        | _ -> acc

    method class_infos
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a class_infos ->
          'a class_infos ->
          diff list ->
          diff list =
      fun _a
          { pci_virt; pci_params; pci_name; pci_expr; pci_loc; pci_attributes }
          {
            pci_virt = pci_virt';
            pci_params = pci_params';
            pci_name = pci_name';
            pci_expr = pci_expr';
            pci_loc = pci_loc';
            pci_attributes = pci_attributes';
          } acc ->
        let acc = self#virtual_flag pci_virt pci_virt' acc in
        let acc =
          self#list
            (fun (a, b) (a', b') acc ->
              let acc = self#core_type a a' acc in
              let acc =
                (fun (a, b) (a', b') acc ->
                  let acc = self#variance a a' acc in
                  let acc = self#injectivity b b' acc in
                  acc)
                  b b' acc
              in
              acc)
            pci_params pci_params' acc
        in
        let acc = self#loc self#string pci_name pci_name' acc in
        let acc = _a pci_expr pci_expr' acc in
        let acc = self#location pci_loc pci_loc' acc in
        let acc = self#attributes pci_attributes pci_attributes' acc in
        acc

    method class_description
        : class_description -> class_description -> diff list -> diff list =
      self#class_infos self#class_type

    method class_type_declaration
        : class_type_declaration ->
          class_type_declaration ->
          diff list ->
          diff list =
      self#class_infos self#class_type

    method class_expr : class_expr -> class_expr -> diff list -> diff list =
      fun { pcl_desc; pcl_loc; pcl_attributes }
          {
            pcl_desc = pcl_desc';
            pcl_loc = pcl_loc';
            pcl_attributes = pcl_attributes';
          } acc ->
        let acc = self#class_expr_desc pcl_desc pcl_desc' acc in
        let acc = self#location pcl_loc pcl_loc' acc in
        let acc = self#attributes pcl_attributes pcl_attributes' acc in
        acc

    method class_expr_desc
        : class_expr_desc -> class_expr_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pcl_constr (a, b), Pcl_constr (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#list self#core_type b b' acc in
            acc
        | Pcl_structure a, Pcl_structure a' -> self#class_structure a a' acc
        | Pcl_fun (a, b, c, d), Pcl_fun (a', b', c', d') ->
            let acc = self#arg_label a a' acc in
            let acc = self#option self#expression b b' acc in
            let acc = self#pattern c c' acc in
            let acc = self#class_expr d d' acc in
            acc
        | Pcl_apply (a, b), Pcl_apply (a', b') ->
            let acc = self#class_expr a a' acc in
            let acc =
              self#list
                (fun (a, b) (a', b') acc ->
                  let acc = self#arg_label a a' acc in
                  let acc = self#expression b b' acc in
                  acc)
                b b' acc
            in
            acc
        | Pcl_let (a, b, c), Pcl_let (a', b', c') ->
            let acc = self#rec_flag a a' acc in
            let acc = self#list self#value_binding b b' acc in
            let acc = self#class_expr c c' acc in
            acc
        | Pcl_constraint (a, b), Pcl_constraint (a', b') ->
            let acc = self#class_expr a a' acc in
            let acc = self#class_type b b' acc in
            acc
        | Pcl_extension a, Pcl_extension a' -> self#extension a a' acc
        | Pcl_open (a, b), Pcl_open (a', b') ->
            let acc = self#open_description a a' acc in
            let acc = self#class_expr b b' acc in
            acc
        | _ -> acc

    method class_structure
        : class_structure -> class_structure -> diff list -> diff list =
      fun { pcstr_self; pcstr_fields }
          { pcstr_self = pcstr_self'; pcstr_fields = pcstr_fields' } acc ->
        let acc = self#pattern pcstr_self pcstr_self' acc in
        let acc = self#list self#class_field pcstr_fields pcstr_fields' acc in
        acc

    method class_field : class_field -> class_field -> diff list -> diff list =
      fun { pcf_desc; pcf_loc; pcf_attributes }
          {
            pcf_desc = pcf_desc';
            pcf_loc = pcf_loc';
            pcf_attributes = pcf_attributes';
          } acc ->
        let acc = self#class_field_desc pcf_desc pcf_desc' acc in
        let acc = self#location pcf_loc pcf_loc' acc in
        let acc = self#attributes pcf_attributes pcf_attributes' acc in
        acc

    method class_field_desc
        : class_field_desc -> class_field_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pcf_inherit (a, b, c), Pcf_inherit (a', b', c') ->
            let acc = self#override_flag a a' acc in
            let acc = self#class_expr b b' acc in
            let acc = self#option (self#loc self#string) c c' acc in
            acc
        | Pcf_val a, Pcf_val a' ->
            (fun (a, b, c) (a', b', c') acc ->
              let acc = self#loc self#label a a' acc in
              let acc = self#mutable_flag b b' acc in
              let acc = self#class_field_kind c c' acc in
              acc)
              a a' acc
        | Pcf_method a, Pcf_method a' ->
            (fun (a, b, c) (a', b', c') acc ->
              let acc = self#loc self#label a a' acc in
              let acc = self#private_flag b b' acc in
              let acc = self#class_field_kind c c' acc in
              acc)
              a a' acc
        | Pcf_constraint a, Pcf_constraint a' ->
            (fun (a, b) (a', b') acc ->
              let acc = self#core_type a a' acc in
              let acc = self#core_type b b' acc in
              acc)
              a a' acc
        | Pcf_initializer a, Pcf_initializer a' -> self#expression a a' acc
        | Pcf_attribute a, Pcf_attribute a' -> self#attribute a a' acc
        | Pcf_extension a, Pcf_extension a' -> self#extension a a' acc
        | _ -> acc

    method class_field_kind
        : class_field_kind -> class_field_kind -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Cfk_virtual a, Cfk_virtual a' -> self#core_type a a' acc
        | Cfk_concrete (a, b), Cfk_concrete (a', b') ->
            let acc = self#override_flag a a' acc in
            let acc = self#expression b b' acc in
            acc
        | _ -> acc

    method class_declaration
        : class_declaration -> class_declaration -> diff list -> diff list =
      self#class_infos self#class_expr

    method module_type : module_type -> module_type -> diff list -> diff list =
      fun { pmty_desc; pmty_loc; pmty_attributes }
          {
            pmty_desc = pmty_desc';
            pmty_loc = pmty_loc';
            pmty_attributes = pmty_attributes';
          } acc ->
        let acc = self#module_type_desc pmty_desc pmty_desc' acc in
        let acc = self#location pmty_loc pmty_loc' acc in
        let acc = self#attributes pmty_attributes pmty_attributes' acc in
        acc

    method module_type_desc
        : module_type_desc -> module_type_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pmty_ident a, Pmty_ident a' -> self#longident_loc a a' acc
        | Pmty_signature a, Pmty_signature a' -> self#signature a a' acc
        | Pmty_functor (a, b), Pmty_functor (a', b') ->
            let acc = self#functor_parameter a a' acc in
            let acc = self#module_type b b' acc in
            acc
        | Pmty_with (a, b), Pmty_with (a', b') ->
            let acc = self#module_type a a' acc in
            let acc = self#list self#with_constraint b b' acc in
            acc
        | Pmty_typeof a, Pmty_typeof a' -> self#module_expr a a' acc
        | Pmty_extension a, Pmty_extension a' -> self#extension a a' acc
        | Pmty_alias a, Pmty_alias a' -> self#longident_loc a a' acc
        | _ -> acc

    method functor_parameter
        : functor_parameter -> functor_parameter -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Unit, Unit -> acc
        | Named (a, b), Named (a', b') ->
            let acc = self#loc (self#option self#string) a a' acc in
            let acc = self#module_type b b' acc in
            acc
        | _ -> acc

    method signature : signature -> signature -> diff list -> diff list =
      self#list self#signature_item

    method signature_item
        : signature_item -> signature_item -> diff list -> diff list =
      fun { psig_desc; psig_loc }
          { psig_desc = psig_desc'; psig_loc = psig_loc' } acc ->
        let acc = self#signature_item_desc psig_desc psig_desc' acc in
        let acc = self#location psig_loc psig_loc' acc in
        acc

    method signature_item_desc
        : signature_item_desc -> signature_item_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Psig_value a, Psig_value a' -> self#value_description a a' acc
        | Psig_type (a, b), Psig_type (a', b') ->
            let acc = self#rec_flag a a' acc in
            let acc = self#list self#type_declaration b b' acc in
            acc
        | Psig_typesubst a, Psig_typesubst a' ->
            self#list self#type_declaration a a' acc
        | Psig_typext a, Psig_typext a' -> self#type_extension a a' acc
        | Psig_exception a, Psig_exception a' -> self#type_exception a a' acc
        | Psig_module a, Psig_module a' -> self#module_declaration a a' acc
        | Psig_modsubst a, Psig_modsubst a' -> self#module_substitution a a' acc
        | Psig_recmodule a, Psig_recmodule a' ->
            self#list self#module_declaration a a' acc
        | Psig_modtype a, Psig_modtype a' ->
            self#module_type_declaration a a' acc
        | Psig_open a, Psig_open a' -> self#open_description a a' acc
        | Psig_include a, Psig_include a' -> self#include_description a a' acc
        | Psig_class a, Psig_class a' ->
            self#list self#class_description a a' acc
        | Psig_class_type a, Psig_class_type a' ->
            self#list self#class_type_declaration a a' acc
        | Psig_attribute a, Psig_attribute a' -> self#attribute a a' acc
        | Psig_extension (a, b), Psig_extension (a', b') ->
            let acc = self#extension a a' acc in
            let acc = self#attributes b b' acc in
            acc
        | _ -> acc

    method module_declaration
        : module_declaration -> module_declaration -> diff list -> diff list =
      fun { pmd_name; pmd_type; pmd_attributes; pmd_loc }
          {
            pmd_name = pmd_name';
            pmd_type = pmd_type';
            pmd_attributes = pmd_attributes';
            pmd_loc = pmd_loc';
          } acc ->
        let acc = self#loc (self#option self#string) pmd_name pmd_name' acc in
        let acc = self#module_type pmd_type pmd_type' acc in
        let acc = self#attributes pmd_attributes pmd_attributes' acc in
        let acc = self#location pmd_loc pmd_loc' acc in
        acc

    method module_substitution
        : module_substitution -> module_substitution -> diff list -> diff list =
      fun { pms_name; pms_manifest; pms_attributes; pms_loc }
          {
            pms_name = pms_name';
            pms_manifest = pms_manifest';
            pms_attributes = pms_attributes';
            pms_loc = pms_loc';
          } acc ->
        let acc = self#loc self#string pms_name pms_name' acc in
        let acc = self#longident_loc pms_manifest pms_manifest' acc in
        let acc = self#attributes pms_attributes pms_attributes' acc in
        let acc = self#location pms_loc pms_loc' acc in
        acc

    method module_type_declaration
        : module_type_declaration ->
          module_type_declaration ->
          diff list ->
          diff list =
      fun { pmtd_name; pmtd_type; pmtd_attributes; pmtd_loc }
          {
            pmtd_name = pmtd_name';
            pmtd_type = pmtd_type';
            pmtd_attributes = pmtd_attributes';
            pmtd_loc = pmtd_loc';
          } acc ->
        let acc = self#loc self#string pmtd_name pmtd_name' acc in
        let acc = self#option self#module_type pmtd_type pmtd_type' acc in
        let acc = self#attributes pmtd_attributes pmtd_attributes' acc in
        let acc = self#location pmtd_loc pmtd_loc' acc in
        acc

    method open_infos
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a open_infos ->
          'a open_infos ->
          diff list ->
          diff list =
      fun _a { popen_expr; popen_override; popen_loc; popen_attributes }
          {
            popen_expr = popen_expr';
            popen_override = popen_override';
            popen_loc = popen_loc';
            popen_attributes = popen_attributes';
          } acc ->
        let acc = _a popen_expr popen_expr' acc in
        let acc = self#override_flag popen_override popen_override' acc in
        let acc = self#location popen_loc popen_loc' acc in
        let acc = self#attributes popen_attributes popen_attributes' acc in
        acc

    method open_description
        : open_description -> open_description -> diff list -> diff list =
      self#open_infos self#longident_loc

    method open_declaration
        : open_declaration -> open_declaration -> diff list -> diff list =
      self#open_infos self#module_expr

    method include_infos
        : 'a.
          ('a -> 'a -> diff list -> diff list) ->
          'a include_infos ->
          'a include_infos ->
          diff list ->
          diff list =
      fun _a { pincl_mod; pincl_loc; pincl_attributes }
          {
            pincl_mod = pincl_mod';
            pincl_loc = pincl_loc';
            pincl_attributes = pincl_attributes';
          } acc ->
        let acc = _a pincl_mod pincl_mod' acc in
        let acc = self#location pincl_loc pincl_loc' acc in
        let acc = self#attributes pincl_attributes pincl_attributes' acc in
        acc

    method include_description
        : include_description -> include_description -> diff list -> diff list =
      self#include_infos self#module_type

    method include_declaration
        : include_declaration -> include_declaration -> diff list -> diff list =
      self#include_infos self#module_expr

    method with_constraint
        : with_constraint -> with_constraint -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pwith_type (a, b), Pwith_type (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#type_declaration b b' acc in
            acc
        | Pwith_module (a, b), Pwith_module (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#longident_loc b b' acc in
            acc
        | Pwith_typesubst (a, b), Pwith_typesubst (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#type_declaration b b' acc in
            acc
        | Pwith_modsubst (a, b), Pwith_modsubst (a', b') ->
            let acc = self#longident_loc a a' acc in
            let acc = self#longident_loc b b' acc in
            acc
        | _ -> acc

    method module_expr : module_expr -> module_expr -> diff list -> diff list =
      fun { pmod_desc; pmod_loc; pmod_attributes }
          {
            pmod_desc = pmod_desc';
            pmod_loc = pmod_loc';
            pmod_attributes = pmod_attributes';
          } acc ->
        let acc = self#module_expr_desc pmod_desc pmod_desc' acc in
        let acc = self#location pmod_loc pmod_loc' acc in
        let acc = self#attributes pmod_attributes pmod_attributes' acc in
        acc

    method module_expr_desc
        : module_expr_desc -> module_expr_desc -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Pmod_ident a, Pmod_ident a' -> self#longident_loc a a' acc
        | Pmod_structure a, Pmod_structure a' -> self#structure a a' acc
        | Pmod_functor (a, b), Pmod_functor (a', b') ->
            let acc = self#functor_parameter a a' acc in
            let acc = self#module_expr b b' acc in
            acc
        | Pmod_apply (a, b), Pmod_apply (a', b') ->
            let acc = self#module_expr a a' acc in
            let acc = self#module_expr b b' acc in
            acc
        | Pmod_constraint (a, b), Pmod_constraint (a', b') ->
            let acc = self#module_expr a a' acc in
            let acc = self#module_type b b' acc in
            acc
        | Pmod_unpack a, Pmod_unpack a' -> self#expression a a' acc
        | Pmod_extension a, Pmod_extension a' -> self#extension a a' acc
        | _ -> acc

    method structure : structure -> structure -> diff list -> diff list =
      self#list self#structure_item

    method structure_item
        : structure_item -> structure_item -> diff list -> diff list =
      fun { pstr_desc; pstr_loc }
          { pstr_desc = pstr_desc'; pstr_loc = pstr_loc' } acc ->
        let acc = self#structure_item_desc pstr_desc pstr_desc' acc in
        let acc = self#location pstr_loc pstr_loc' acc in
        acc

    method structure_item_desc
        : structure_item_desc -> structure_item_desc -> diff list -> diff list =
      fun x x' acc ->
        (*  if equal_structure_item_desc x x' then *)
        match (x, x') with
        | Pstr_eval (a, b), Pstr_eval (a', b') ->
            let acc = self#expression a a' acc in
            let acc = self#attributes b b' acc in
            acc
        | Pstr_value (a, b), Pstr_value (a', b') ->
            let acc = self#rec_flag a a' acc in
            let acc = self#list self#value_binding b b' acc in
            acc
        | Pstr_primitive a, Pstr_primitive a' -> self#value_description a a' acc
        | Pstr_type (a, b), Pstr_type (a', b') ->
            let acc = self#rec_flag a a' acc in
            let acc = self#list self#type_declaration b b' acc in
            acc
        | Pstr_typext a, Pstr_typext a' -> self#type_extension a a' acc
        | Pstr_exception a, Pstr_exception a' -> self#type_exception a a' acc
        | Pstr_module a, Pstr_module a' -> self#module_binding a a' acc
        | Pstr_recmodule a, Pstr_recmodule a' ->
            self#list self#module_binding a a' acc
        | Pstr_modtype a, Pstr_modtype a' ->
            self#module_type_declaration a a' acc
        | Pstr_open a, Pstr_open a' -> self#open_declaration a a' acc
        | Pstr_class a, Pstr_class a' ->
            self#list self#class_declaration a a' acc
        | Pstr_class_type a, Pstr_class_type a' ->
            self#list self#class_type_declaration a a' acc
        | Pstr_include a, Pstr_include a' -> self#include_declaration a a' acc
        | Pstr_attribute a, Pstr_attribute a' -> self#attribute a a' acc
        | Pstr_extension (a, b), Pstr_extension (a', b') ->
            let acc = self#extension a a' acc in
            let acc = self#attributes b b' acc in
            acc
        | _ -> acc
    (* else
       add_diff "structure_item_desc"
         (show_structure_item_desc x)
         (show_structure_item_desc x')
         acc *)

    method value_binding
        : value_binding -> value_binding -> diff list -> diff list =
      fun { pvb_pat; pvb_expr; pvb_attributes; pvb_loc }
          {
            pvb_pat = pvb_pat';
            pvb_expr = pvb_expr';
            pvb_attributes = pvb_attributes';
            pvb_loc = pvb_loc';
          } acc ->
        let acc = self#pattern pvb_pat pvb_pat' acc in
        let acc = self#expression pvb_expr pvb_expr' acc in
        let acc = self#attributes pvb_attributes pvb_attributes' acc in
        let acc = self#location pvb_loc pvb_loc' acc in
        acc

    method module_binding
        : module_binding -> module_binding -> diff list -> diff list =
      fun { pmb_name; pmb_expr; pmb_attributes; pmb_loc }
          {
            pmb_name = pmb_name';
            pmb_expr = pmb_expr';
            pmb_attributes = pmb_attributes';
            pmb_loc = pmb_loc';
          } acc ->
        let acc = self#loc (self#option self#string) pmb_name pmb_name' acc in
        let acc = self#module_expr pmb_expr pmb_expr' acc in
        let acc = self#attributes pmb_attributes pmb_attributes' acc in
        let acc = self#location pmb_loc pmb_loc' acc in
        acc

    method toplevel_phrase
        : toplevel_phrase -> toplevel_phrase -> diff list -> diff list =
      fun x x' acc ->
        match (x, x') with
        | Ptop_def a, Ptop_def a' -> self#structure a a' acc
        | Ptop_dir a, Ptop_dir a' -> self#toplevel_directive a a' acc
        | _ -> acc

    method toplevel_directive
        : toplevel_directive -> toplevel_directive -> diff list -> diff list =
      fun { pdir_name; pdir_arg; pdir_loc }
          { pdir_name = pdir_name'; pdir_arg = pdir_arg'; pdir_loc = pdir_loc' }
          acc ->
        let acc = self#loc self#string pdir_name pdir_name' acc in
        let acc = self#option self#directive_argument pdir_arg pdir_arg' acc in
        let acc = self#location pdir_loc pdir_loc' acc in
        acc

    method directive_argument
        : directive_argument -> directive_argument -> diff list -> diff list =
      fun { pdira_desc; pdira_loc }
          { pdira_desc = pdira_desc'; pdira_loc = pdira_loc' } acc ->
        let acc = self#directive_argument_desc pdira_desc pdira_desc' acc in
        let acc = self#location pdira_loc pdira_loc' acc in
        acc

    method directive_argument_desc
        : directive_argument_desc ->
          directive_argument_desc ->
          diff list ->
          diff list =
      fun x x' acc ->
        match (x, x') with
        | Pdir_string a, Pdir_string a' -> self#string a a' acc
        | Pdir_int (a, b), Pdir_int (a', b') ->
            let acc = self#string a a' acc in
            let acc = self#option self#char b b' acc in
            acc
        | Pdir_ident a, Pdir_ident a' -> self#longident a a' acc
        | Pdir_bool a, Pdir_bool a' -> self#bool a a' acc
        | _ -> acc

    method cases : cases -> cases -> diff list -> diff list =
      self#list self#case
  end
