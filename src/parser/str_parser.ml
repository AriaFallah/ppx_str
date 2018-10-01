type tstr =
  | TStr of string
  | TVar of string
type parse_state =
  | InString
  | InCurly of int
type parse_error =
  | NO_EXPRESSION
  | NO_OPENING
  | NO_CLOSING

exception Parse_error of int * parse_error

let () =
  Printexc.register_printer
    (function
      | Parse_error (i, e) ->
        let s = match e with
        | NO_EXPRESSION -> "{} has no expression inside"
        | NO_OPENING -> "Missing opening {"
        | NO_CLOSING -> "Missing closing }"
        in
        Some (s ^ " at character " ^ (string_of_int i))
      | _ -> None 
    )

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
    if i = len then
      match state with
      | InString ->
        if Buffer.length buf = 0 then
          lst
        else
          let s = TStr (contents ()) in
          s :: lst
      | InCurly _ ->
        raise (Parse_error (i, NO_CLOSING))
    else
      let c = str.[i] in
      match state with
      | InString ->
          if i >= len - 1 then
            match c with
            | '{' ->
              raise (Parse_error (i, NO_CLOSING))
            | '}' ->
              raise (Parse_error (i, NO_OPENING))
            | _ ->
              add c;
              str |> parser ~i:(i + 1) ~state:InString ~lst
          else
            let c2 = str.[i + 1] in (
            match c, c2 with
            | '}', _ ->
              raise (Parse_error (i, NO_OPENING))
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
          if Buffer.length buf = 0 then
            raise (Parse_error (i, NO_EXPRESSION))
          else if n = 0 then (
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

let%test _ = 
  parse_str "Hello there {name} I am {name2} and {string_of_int 1}"
  =
  [
    TStr "Hello there ";
    TVar "name";
    TStr " I am ";
    TVar "name2";
    TStr " and ";
    TVar "string_of_int 1";
  ]

let%test _ = 
  parse_str "Hi {some_func {a=1; b=2;}} you're \\{great!\\}"
  =
  [
    TStr "Hi ";
    TVar "some_func {a=1; b=2;}";
    TStr " you're {great!}";
  ]

let%test _ = 
  try 
    ignore (parse_str "Hi {}");
    false
  with Parse_error (_, NO_EXPRESSION) -> true

let%test _ = 
  try 
    ignore (parse_str "Hi {");
    false
  with Parse_error (_, NO_CLOSING) -> true

let%test _ = 
  try
    ignore (parse_str "Hi }");
    false
  with Parse_error (_, NO_OPENING) -> true

let%test _ = 
  try 
    ignore (parse_str "Hi {{}");
    false
  with Parse_error (_, NO_CLOSING) -> true

let%test _ = 
  try 
    ignore (parse_str "Hi {name}}");
    false
  with Parse_error (_, NO_OPENING) -> true