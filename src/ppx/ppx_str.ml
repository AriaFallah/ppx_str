open Ppxlib
open Str_parser
open Ast_builder.Default

let expand ~loc ~path:_ str =
  let lst = str
    |> parse_str
    |> List.map (fun t ->
      match t with
      | TStr s ->
        estring ~loc s
      | TVar v ->
        v
          |> Lexing.from_string
          |> Parse.expression
    )
    |> elist ~loc
  in
  [%expr String.concat "" [%e lst]]

let name = "str"
let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand
let () =
  Ppxlib.Driver.register_transformation name ~extensions:[ext]