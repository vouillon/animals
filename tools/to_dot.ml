(*
ocaml tools/to_dot.ml  < trees/mammalia-simple.txt > /tmp/g.dot
dot /tmp/g.dot -Tsvg > /tmp/g.svg

2
pdfposter -p 2x1a4 /tmp/g.pdf /tmp/out.pdf
4
pdfposter -p 2x1a4 /tmp/g.pdf /tmp/out.pdf
8
pdfposter -p 4x4a4 /tmp/g.pdf /tmp/o.pdf


pdf2ps out.pdf
pstops "0@0.95(0.025w,0.025h)" out.ps o.ps

*)

#load "str.cma"

let (>>) x f = f x

type 'a tree = Node of 'a * 'a tree list

let compute_nodes node =
  let rec compute is_root n =
    let Node (info, ch) = n in
    let ch = List.map (fun n -> compute false n) ch in
    let ch =
      List.fold_right
        (fun n rem ->
           let Node (info, ch) = n in
           if is_root || info.[0] <> '(' then
             n :: rem
           else
             match ch with
               [n] -> n :: rem
             | []  -> rem
             | _   -> n :: rem)
        ch []
    in
    match ch with
      [n] when info.[0] = '(' && not is_root ->
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

let last = ref 0

let rec print_tree n t =
  let Node (s, ch) = t in
  if s.[0] = '|' then
    Format.printf "\"%d\" [image=\"%s\",shape=\"none\",margin=\"0,0\",label=\"\"];" n
      ("images/" ^ String.sub s 1 (String.length s - 1))
  else if s.[0] <> '(' then
    Format.printf "\"%d\" [label=\"%s\"];" n s
  else
    Format.printf "\"%d\" [label=\"\",shape=\"none\",width=0,height=0];" n;
  List.iter (print_edges n) ch

and print_edges src t =
  incr last;
  let dst = !last in
  Format.printf "\"%d\" -> \"%d\";@." src dst;
  print_tree dst t

let rec randomize_tree n =
  let Node (info, l) = n in
  let a = Array.of_list l in
  for i = Array.length a - 1 downto 0 do
    let v = a.(i) in
    let j = Random.int (i + 1) in
    a.(i) <- a.(j);
    a.(j) <- v
  done;
  Node (info, Array.to_list (Array.map randomize_tree a))

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
  >> compute_nodes
  >> randomize_tree

let _ =
  let t = load_tree stdin in
Format.printf "digraph G {@.";
Format.printf "graph [ratio=0.35]@.";
(*
Format.printf "graph [nodesep=0.,ranksep=0.2]@.";
*)
Format.printf "graph [nodesep=0.05,ranksep=0.2]@.";
Format.printf "graph [splines=polyline]@.";
Format.printf "edge [dir=none,penwidth=4]@.";
Format.printf "node [penwidth=4,fontsize=28,shape=box]@.";
print_tree 0 t;
Format.printf "}@."
