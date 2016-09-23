#! /usr/bin/ocaml
#load "str.cma"

let (>>) x f = f x

type 'a tree = Node of 'a * 'a tree list

let important_node info = info.[0] <> '('

let compress_nodes node =
  let rec compute is_root n =
    let Node (info, ch) = n in
    let ch = List.map (fun n -> compute false n) ch in
    let ch =
      List.fold_right
        (fun n rem ->
           let Node (info, ch) = n in
           if is_root || important_node info then
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
    | _ ->
        Node (info, ch)
  in
  compute true node

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

let load_tree ch =
  let l = ref [] in
  begin try
    while true do l := input_line ch :: !l done
  with End_of_file -> () end;
  !l
  >> List.rev_map
       (fun l ->
         let i = Str.search_forward not_space_re l 0 in
         (i, String.sub l i (String.length l - i)))
  >> parse_tree

(****)

let rec print_tree space t =
  let Node (s, ch) = t in
  Format.printf "%s%s@." space (if important_node s then s else "(");
  List.iter (print_tree (space ^ " ")) ch

let _ =
  let t = load_tree stdin in
  let t = compress_nodes t in
  print_tree "" t
