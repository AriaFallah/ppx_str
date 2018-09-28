open Ppxlib
open Ast_builder.Default

exception Parse_error of string
type tstr =
  | TStr of string
  | TVar of string

let not_curly = [%sedlex.regexp? Compl ('{' | '}')]

let lex_str str =
  let rec lexer lst lexbuf =
    match%sedlex lexbuf with
    | Opt (Plus not_curly), Opt ("\\{" | "\\}")  ->
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      let buf = Str.global_replace (Str.regexp {|\\{|}) "{" buf in
      let buf = Str.global_replace (Str.regexp {|\\}|}) "}" buf in
      lexbuf |> lexer (TStr buf :: lst)
    | "{", Plus not_curly , "}" ->
      let buf = Sedlexing.Utf8.lexeme lexbuf in
      lexbuf |> lexer (TVar buf :: lst)
    | eof ->
      lst
    | "{}" ->
      raise (Parse_error "No expression inside curly braces")
    | "{", Plus (Compl '}'), eof ->
      raise (Parse_error "Missing closing }")
    | Plus (Compl '{'), "}" ->
      raise (Parse_error "Missing opening {")
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
        | TStr x, TStr y ->
          (TStr (String.concat "" [y; x])) :: xs
        | _ ->
          y :: x :: xs
      )
    ) []

let expand ~loc ~path:_ str =
  let lst = str
    |> lex_str
    |> List.map (fun t ->
      match t with
      | TStr s ->
        estring ~loc s
      | TVar v ->
        String.sub v 1 ((String.length v) - 2)
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