open Ppxlib
open Ast_builder.Default

exception Parse_error of string
type tstr =
  | TStr of string
  | TVar of string
type parse_state =
  | InString
  | InCurly of int

let parse_str str =
  let len = String.length str in
  let buf = Buffer.create 500 in

  let add c = Buffer.add_char buf c in
  let contents () =
    let s = Buffer.contents buf in
    Buffer.clear buf;
    s
  in

  let rec parser ~i ~state ~lst str =
    if i == len then
      if Buffer.length buf == 0 then
        lst
      else
        match state with
        | InString ->
          let s = TStr (contents ()) in
          s :: lst
        | InCurly _ ->
          raise (Parse_error "Missing closing }")
    else
      let c = str.[i] in
      match state with
      | InString ->
          if i >= len - 1 then
            if c == '{' then
              raise (Parse_error "Missing closing }")
            else (
              add c;
              str |> parser ~i:(i + 1) ~state:InString ~lst
            )
          else
            let c2 = str.[i + 1] in (
            match c, c2 with
            | '}', _ ->
              raise (Parse_error "Missing opening {")
            | '\\', ('{' | '}') ->
              add c2;
              str |> parser ~i:(i + 2) ~state:InString ~lst
            | '{', _ ->
              let s = TStr (contents ()) in
              str |> parser ~i:(i + 1) ~state:(InCurly 0) ~lst:(s :: lst)
            | _ ->
              add c;
              str |> parser ~i:(i + 1) ~state:InString ~lst
            )
      | InCurly n ->
        
        match c with
        | '{' ->
          add c;
          str |> parser ~i:(i + 1) ~state:(InCurly (n + 1)) ~lst
        | '}' ->
          if n == 0 then (
            let s = TVar (contents ()) in
            str |> parser ~i:(i + 1) ~state:InString ~lst:(s :: lst)
          )
          else (
            add c;
            str |> parser ~i:(i + 1) ~state:(InCurly (n - 1)) ~lst
          )
        | _ ->
          add c;
          str |> parser ~i:(i + 1) ~state:(InCurly n) ~lst
    in
  str
    |> parser ~i:0 ~state:InString ~lst:[]
    |> List.rev

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