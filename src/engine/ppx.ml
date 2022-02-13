open Js_of_ocaml_compiler.Stdlib

let ppx_rewriters = ref []

let () = Ast_mapper.register_function := fun _ f -> ppx_rewriters := f :: !ppx_rewriters

let preprocess_structure str =
  let open Ast_mapper in
  List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
      let mapper = ppx_rewriter [] in
      mapper.structure mapper str)

let preprocess_signature str =
  let open Ast_mapper in
  List.fold_right !ppx_rewriters ~init:str ~f:(fun ppx_rewriter str ->
      let mapper = ppx_rewriter [] in
      mapper.signature mapper str)

let preprocess_phrase phrase =
  let open Parsetree in
  match phrase with
  | Ptop_def str -> Ptop_def (preprocess_structure str)
  | Ptop_dir _ as x -> x
