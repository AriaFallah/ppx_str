open Ppxlib

let name = "str"

let expand ~loc ~path:_ str =
  let open Ast_builder.Default in
  let open Re.Str in 
  let strs = str
    |> full_split (regexp {|{[^{}]+}|})
    |> List.map (fun r -> 
      match r with 
        | Text s  -> estring ~loc s
        | Delim s ->
          let start = 1 in
          let len = String.length s in
          let lexbuf = Lexing.from_string (String.sub s start (len - 1 - start)) in
          Parse.expression lexbuf
    ) in
  [%expr String.concat "" [%e elist ~loc strs]]

let ext =
  Extension.declare
    name
    Extension.Context.expression
    Ast_pattern.(single_expr_payload (estring __))
    expand

let () =
  Ppxlib.Driver.register_transformation name ~extensions:[ext]