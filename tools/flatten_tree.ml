#! /usr/bin/ocaml
#load "str.cma"

let (>>) v f = f v

let leading_whitespaces_re = Str.regexp "^[ \t\n]+"
let trailing_whitespaces_re = Str.regexp "[ \t\n]+$"

let trim s =
  s >> Str.replace_first leading_whitespaces_re ""
    >> Str.replace_first trailing_whitespaces_re ""

(****)

let rec flatten indent ch =
  try
    while true do
      let l = input_line ch in
      let s = trim l in
      if s <> "" && s.[0] <> '#' then begin
        if s <> "" && s.[0] = '@' then begin
          let i = String.index l '@' in
          let ch = open_in (String.sub s 1 (String.length s - 1)) in
          flatten (indent ^ String.make i ' ') ch;
          close_in ch
        end else
          Format.printf "%s%s@." indent l
      end
    done
  with End_of_file ->
    ()

let _ = flatten "" stdin
