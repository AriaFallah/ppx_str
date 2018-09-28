open Ppxlib
open Ast_builder.Default

exception Parse_error of string
type tstr =
  | Str of string
  | Var of string

let no_curly str =
  (String.sub str 1 ((String.length str) - 2))

let lex_str str =
  let rec lexer lst lexbuf =
    match%sedlex lexbuf with
    | "\\{" | Opt white_space, "\\}" ->
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      lexbuf |> lexer (Str (String.sub buf 1 1) :: lst)
    | "{}" ->
      raise (Parse_error "No expression inside curly braces")
    | "{", Plus (Compl '}'), eof ->
      raise (Parse_error "Missing closing }")
    | Plus (Compl '{'), "}" ->
      raise (Parse_error "Missing opening {")
    | Plus (Compl ('{' | '\\')) ->
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      lexbuf |> lexer (Str buf :: lst)
    | "{", Plus (Compl ('{' | '}')) , "}" ->
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      lexbuf |> lexer (Var buf :: lst)
    | eof ->
      lst
    | _ ->
      raise (Parse_error "Unexpected pattern")
  in 
  str 
    |> Sedlexing.Utf8.from_string
    |> lexer []
    |> List.fold_left (fun lst y ->
      match lst with
      | [] -> [y]
      | (x::xs) -> (
        match x, y with
        | Str x, Str y -> (Str (String.concat "" [y; x])) :: xs
        | _ -> y :: x :: xs
      )
    ) []

let expand ~loc ~path:_ str =
  let lst = str
    |> lex_str
    |> List.map (fun t ->
      match t with
      | Str s ->
        estring ~loc s
      | Var v ->
        v |> no_curly |> Lexing.from_string |> Parse.expression
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