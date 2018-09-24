# PPX Str

Lets you do

```ocaml
let name = "aria"
let name2 = "ppx_str"

let my_str = [%str
  {|Hello there {name} I am {name2} and {string_of_int 1}|}
]
```
