#! /usr/bin/ocaml
#load "str.cma"

let (>>) x f = f x

let leading_whitespaces_re = Str.regexp "^[ \t\n]+"
let trailing_whitespaces_re = Str.regexp "[ \t\n]+$"
let whitespaces_re = Str.regexp "[ \t\n]+"

let trim s =
  s >> Str.replace_first leading_whitespaces_re ""
    >> Str.replace_first trailing_whitespaces_re ""

(****)

type 'a tree = Node of 'a * 'a tree list

let rec parse_node lst =
  match lst with
    [] ->
      assert false
  | (i, l) :: rem ->
      begin match rem with
        (j, _) :: _ when j > i ->
          let (ch, rem) = parse_children rem j [] in
          (Node (l, ch), rem)
      | _ ->
          (Node (l, []), rem)
      end

and parse_children lst i ch =
  let (n, rem) = parse_node lst in
  let ch = n :: ch in
  match rem with
    [] ->
      (List.rev ch, [])
  | (j, l) :: _ ->
      assert (j <= i);
      if j = i then parse_children rem i ch else (List.rev ch, rem)

let parse_tree lst =
  let (n, rem) = parse_node lst in
begin match rem with
(_, s) :: _ -> Format.eprintf "XXX %s@." s
  | _       -> ()
end;
  assert (rem = []);
  n

let not_space_re = Str.regexp "[^ ]"

let relative_file ref file =
  if Filename.is_relative file then
    Filename.concat (Filename.dirname ref) file
  else
    file

let rec read_tree lines indent filename =
  let ch = open_in filename in
  try
    while true do
      let l = input_line ch in
      let s = trim l in
      if s <> "" && s.[0] <> '#' then begin
        if s <> "" && s.[0] = '@' then begin
          let i = String.index l '@' in
          let file_ref = String.sub s 1 (String.length s - 1) in
          let included_file = relative_file filename file_ref in
          read_tree lines (indent ^ String.make i ' ') included_file;
        end else
          lines := (indent ^ l) :: !lines
      end
    done
  with End_of_file ->
    close_in ch

let load_tree ch =
  let l = ref [] in
  read_tree l "" ch;
  !l
  >> List.rev_map
       (fun l ->
         let i = Str.search_forward not_space_re l 0 in
         (i, String.sub l i (String.length l - i)))
  >> parse_tree

let rec print_tree space t =
  let Node (s, ch) = t in
  Format.printf "%s%s@." space (if s = "" then "-" else s);
  List.iter (print_tree (space ^ " ")) ch

(****)

let json_int buffer i = Buffer.add_string buffer (string_of_int i)

let json_string buffer s =
  Buffer.add_char buffer '\"';
  for i = 0 to String.length s - 1 do
    match s.[i] with
      | '\"' -> Buffer.add_string buffer "\\\""
      | '\\' -> Buffer.add_string buffer "\\\\"
      | '\b' -> Buffer.add_string buffer "\\b"
      | '\x0C' -> Buffer.add_string buffer "\\f"
      | '\n' -> Buffer.add_string buffer "\\n"
      | '\r' -> Buffer.add_string buffer "\\r"
      | '\t' -> Buffer.add_string buffer "\\t"
      | c when c <= '\x1F' -> (* Other control characters are escaped. *)
        Printf.bprintf buffer "\\u%04X" (int_of_char c)
      | c ->
        Buffer.add_char buffer c
  done;
  Buffer.add_char buffer '\"'

let json_array buffer a =
  if Array.length a = 0 then
    Buffer.add_string buffer "[0]"
  else begin
    Buffer.add_string buffer "[0,";
    for i = 0 to Array.length a - 1 do
      if i > 0 then Buffer.add_string buffer ",";
      a.(i) buffer
    done;
    Buffer.add_string buffer "]"
  end

(****)

let is_image info = info <> "" && info.[0] = '|'
let important_nodes = Hashtbl.create 101
let important_node info =
  is_image info ||
  if Hashtbl.mem important_nodes info then begin
    Hashtbl.replace important_nodes info true; true
  end else
    false

let dummy_info = ""

let compress_nodes node =
  let rec compute is_root n =
    let Node (info, ch) = n in
    let ch = List.map (fun n -> compute false n) ch in
    let ch =
      List.fold_right
        (fun n rem ->
           let Node (info, ch) = n in
           if important_node info then
             n :: rem
           else
             match ch with
               [n] -> n :: rem
             | []  -> rem
             | _   -> n :: rem)
        ch []
    in
    match ch with
      [n] when not (is_root || important_node info) ->
        n (* Remove nodes with a single child *)
    | [Node (info', ch')] when not (important_node info') ->
        Node (info, ch')
    | _ ->
        let info = if important_node info then info else dummy_info in
        Node (info, ch)
  in
  compute true node

(****)

let rec check_images t =
  let Node (s, ch) = t in
  if is_image s then begin
    let name = String.sub s 1 (String.length s - 1) in
    let file = "images/" ^ name ^ ".jpg" in
    if not (Sys.file_exists file) then Format.eprintf "Missing image %s@." name
  end;
  List.iter check_images ch

(****)

let rec output_tree b t =
  let Node (s, ch) = t in
  json_array b [|(fun b -> json_string b s); (fun b -> output_children b ch)|]

and output_children b ch =
  let a = Array.of_list ch in
  json_array b (Array.map (fun t b -> output_tree b t) a)
(*
and output_children b ch =
  match ch with
    []     -> json_int b 0
  | t :: r -> json_array b [|(fun b -> output_tree b t);
                             (fun b -> output_children b r)|]
*)

(*
let rec count depth t =
  let Node(s, ch) = t in
  let c = 
    try
      ignore (String.index s '|');
      1
    with Not_found ->
      0
  in
  if depth = 0 then c else
  List.fold_right (fun t n -> count (depth - 1) t + n) ch c
*)

let compute_tree filename =
  let t = load_tree filename in
  compress_nodes t
(*
for i = 0 to 15 do
Format.eprintf "%d: %d@." i (count i t)
done;
*)

(*
  let b = Buffer.create 1024 in
  output_tree b t;
  output_string stdout (Buffer.contents b);
  flush stdout
*)

let load_key_value_file filename =
  let ch = open_in filename in
  let lines = ref [] in
  begin try
    while true do
      match Str.bounded_split whitespaces_re (input_line ch) 2 with
        [key; value] -> lines := (key, value) :: !lines
      | _            -> ()
    done
  with End_of_file -> () end;
  close_in ch;
  List.rev !lines

let load_file filename =
  let ch = open_in filename in
  let lines = ref [] in
  begin try
    while true do
      lines := input_line ch :: !lines
    done
  with End_of_file -> () end;
  close_in ch;
  List.rev !lines

let load_about refer lang =
  let filename = relative_file refer ("about-" ^ lang ^ ".txt") in
  String.concat "\n" (load_file filename)

let load_nodes refer lang =
  let filename = relative_file refer ("nodes-" ^ lang ^ ".txt") in
  let res = Array.of_list (load_key_value_file filename) in
  Array.iter (fun (key, _) -> Hashtbl.replace important_nodes key false) res;
  res

let output_i18n b a =
  json_array b
    (Array.map
       (fun (lang, tbl, about) b ->
          json_array b
            [|(fun b -> json_string b lang);
              (fun b ->
                 json_array b
                   (Array.map
                      (fun (k, v) b ->
                         json_array b
                            [|(fun b -> json_string b k);
                              (fun b -> json_string b v)|])
                      tbl));
              (fun b -> json_string b about)|])
       a)

let _ =
  let desc = Sys.argv.(1) in
  let info = load_key_value_file desc in
  let lang =
    Array.of_list (Str.split whitespaces_re (List.assoc "lang" info)) in
  let i18n =
    Array.map (fun l -> (l, load_nodes desc l, load_about desc l)) lang in
  let tree = compute_tree (relative_file desc (List.assoc "root" info)) in
  Hashtbl.replace important_nodes "<TITLE>" true;
  Hashtbl.iter
    (fun k v -> if not v then Format.eprintf "Missing node: %s@." k)
    important_nodes;
  let b = Buffer.create 1024 in
  json_array b [|(fun b -> output_tree b tree);
                 (fun b -> output_i18n b i18n)|];
  output_string stdout (Buffer.contents b);
  flush stdout;
(*
  check_images tree
*)
(*
print_tree "" tree
*)
