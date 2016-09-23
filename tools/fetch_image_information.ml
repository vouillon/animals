(*
XXX Check for missing entries
XXX Cache image sizes
*)

let _ =
if Array.length Sys.argv < 5 then begin
  Format.eprintf "Usage: %s <refs> <cache_dir> <image_dir> <output_file>@." Sys.argv.(0);
  exit 1
end

let input_file = Sys.argv.(1)
let cache_dir = Sys.argv.(2)
let image_dir = Sys.argv.(3)
let output_file = Sys.argv.(4)


let thumbnail_cache = Filename.concat cache_dir "thumbnail"
let html_cache = Filename.concat cache_dir "html"
let usage_cache = Filename.concat cache_dir "usage"

(****)

let (>>) v f = f v

let leading_whitespaces_re = Str.regexp "^[ \t\n]+"
let trailing_whitespaces_re = Str.regexp "[ \t\n]+$"

let trim s =
  s >> Str.replace_first leading_whitespaces_re ""
    >> Str.replace_first trailing_whitespaces_re ""

let load_file f =
  let ch = open_in f in
  let b = Buffer.create 4096 in
  let s = String.create 4096 in
  let rec load () =
    let n = input ch s 0 4096 in
    if n > 0 then begin
      Buffer.add_substring b s 0 n;
      load ()
    end
  in
  load ();
  close_in ch;
  Buffer.contents b

let utf8_protect s =
  let get s i = Char.code (String.unsafe_get s i) in
  let rec protect s i l =
    if i < l then begin
      let c = get s i in
      if c < 0x80 then
        protect s (i + 1) l
      else if c < 0xE0 then begin
        (* 80 - 7FF *)
        if c < 0xc2 || i + 1 >= l then fail s i l else
        let c1 = get s (i + 1) in
        if c1 land 0xc0 <> 0x80 then fail s i l else
        protect s (i + 2) l
      end else if c < 0xF0 then begin
        (* 800 - FFFF *)
        if i + 2 >= l then fail s i l else
        let c1 = get s (i + 1) in
        let c2 = get s (i + 2) in
        if c1 land 0xc0 <> 0x80 || c2 land 0xc0 <> 0x80 then fail s i l else
        let v = c lsl 12 + c1 lsl 6 + c2 - 0xe2080 in
        if v < 0x800 then fail s i l else
        protect s (i + 3) l
      end else begin
        (* 10000 - 10FFFF *)
        if i + 3 >= l then fail s i l else
        let c1 = get s (i + 1) in
        let c2 = get s (i + 2) in
        let c3 = get s (i + 3) in
        if
          c1 land 0xc0 <> 0x80 || c2 land 0xc0 <> 0x80 || c3 land 0xc0 <> 0x80
        then fail s i l else
        let v = c lsl 18 + c1 lsl 12 + c2 lsl 6 + c3 - 0x03c82080 in
        if v < 0x10000 || v > 0x10ffff then fail s i l else
        protect s (i + 4) l
      end
    end
  and fail s i l = s.[i] <- '?'; protect s (i + 1) l in
  protect s 0 (String.length s)

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

let http_get url file =
  let tmp = file ^ ".tmp" in
  let res = Sys.command (Format.sprintf "curl -f -# -o %s '%s'" tmp url) in
  if res = 0 then
    Sys.rename tmp file
  else begin
    try Sys.remove tmp with Sys_error _ -> ()
  end

(****)

let image_url name =
  Format.sprintf
    "https://commons.wikimedia.org/w/api.php?format=txt&\
     action=query&prop=imageinfo&iiprop=url&iiurlwidth=800&titles=File:%s"
    name

let field_re = Str.regexp "^ *\\[\\([^]]+\\)\\] => *\\(.*\\)$"

let parse_thumbnail_info_remote name =
  let h = Hashtbl.create 17 in
  let ch = open_in (Filename.concat thumbnail_cache name) in
  begin try
    while true do
      let l = input_line ch in
      if Str.string_match field_re l 0 then begin
        Hashtbl.add h (Str.matched_group 1 l) (Str.matched_group 2 l)
      end
    done
  with End_of_file -> () end;
  close_in ch;
  let url = Hashtbl.find h "thumburl" in
  let width = Hashtbl.find h "thumbwidth" in
  let height = Hashtbl.find h "thumbheight" in
  (Some url, width, height)

let size_re = Str.regexp " \\([0-9]+\\)x\\([0-9]+\\) "

let parse_thumbnail_info_local name =
  let f = Filename.concat image_dir (name ^ ".jpg") in
  if not (Sys.file_exists f) then begin
    Format.eprintf "XXX %s@." name;
    assert false
  end;
  let ch = Unix.open_process_in (Format.sprintf "convert %s info:-" f) in
  let l = input_line ch in
  close_in ch;
  ignore (Str.search_forward size_re l 0);
  (None, Str.matched_group 1 l, Str.matched_group 2 l)

let parse_thumbnail_info =
  if image_dir = "/no/image" then parse_thumbnail_info_remote
  else parse_thumbnail_info_local

(****)

let usage_url ?cont name =
  "https://commons.wikimedia.org/w/api.php?format=xml&action=query&\
   prop=globalusage&gulimit=500&titles=File:" ^ name ^
  match cont with
    None      -> ""
  | Some cont -> "&gucontinue=" ^ cont

let load_usage ?cont name dest =
  http_get (usage_url ?cont name) dest

let parse_usage short_name name =
Format.eprintf "%s@." short_name;
  let l = ref [] in
  let rec parse n cont =
    let file =
      Filename.concat usage_cache
        (if n = 0 then short_name else short_name ^ "." ^ string_of_int n) in
    if not (Sys.file_exists file) then load_usage ?cont name file;
    let ch = open_in file in
    let i = Xmlm.make_input (`Channel ch) in
    let cont = ref "" in
    ignore
      (Xmlm.input_doc_tree
         ~el:(fun ((_, nm), attrs) _ ->
                if nm = "gu" then begin
                  let title = String.copy (List.assoc ("", "title") attrs) in
                  for i = 0 to String.length title - 1 do
                    if title.[i] = '_' then title.[i] <- ' '
                  done;
                  l := (title,
                        List.assoc ("", "wiki") attrs,
                        List.assoc ("", "url") attrs) :: !l
                end else if nm = "globalusage" then begin
                  try
                    cont := List.assoc ("", "gucontinue") attrs
                  with Not_found ->
                    ()
                end)
         ~data:(fun _ -> ())
         i);
    close_in ch;
    if !cont <> "" then parse (n + 1) (Some !cont)
  in
  parse 0 None;
  !l

let interesting_wikis = ["en.wikipedia.org"; "fr.wikipedia.org"]

let has_prefix pr s =
  String.length s > String.length pr && String.sub s 0 (String.length pr) = pr

let rec truncate n l =
  if n = 0 then [] else
  match l with
    []     ->  []
  | x :: r -> x :: truncate (n - 1) r

let filter_usage l =
  l
  >> List.filter
       (fun (title, wiki, _) ->
          not (has_prefix "List " title || has_prefix "Liste " title) &&
          try ignore (String.index title ':'); false with Not_found -> true)
  >> fun l ->
       List.map
         (fun wiki' ->
            l
            >> List.filter (fun (_, wiki, _) -> wiki = wiki')
            >> truncate 15)
         interesting_wikis
  >> List.flatten

(****)

let space_re = Str.regexp "[ ]+"
let elt_classes (nm, attribs) =
  try
    Str.split space_re (List.assoc ("", "class") attribs)
  with Not_found ->
    []

let elt_id (nm, attribs) =
  try
    [List.assoc ("", "id") attribs]
  with Not_found ->
    []

let parse_contents f i =
  let rec traverse_rec n =
    let s = Xmlm.input i in
    match s with
      `Dtd _ ->
        traverse_rec n
    | `Data _ ->
        f s;
        traverse_rec n
    | `El_start _ ->
        f s;
        traverse_rec (n + 1);
        traverse_rec n
    | `El_end ->
        if n > 0 then f s
  in
  traverse_rec 0

let skip i = parse_contents (fun _ -> ()) i

let text i =
  let b = Buffer.create 80 in
  parse_contents
    (fun s -> match s with `Data t -> Buffer.add_string b t | _ -> ()) i;
  Buffer.contents b

let html i =
  let b = Buffer.create 80 in
  let o = Xmlm.make_output (`Buffer b) in
  Xmlm.output o (`Dtd None);
  Xmlm.output o (`El_start (("", "p"), []));
  Xmlm.output o (`Data " ");
  Buffer.clear b;
  parse_contents
    (fun s ->
       let s =
         match s with
           `El_start ((_, nm), attrs) ->
             let attrs =
               if nm <> "a" then attrs else begin
                 let url = List.assoc ("", "href") attrs in
                 let url =
                   if url = "" || url.[0] = '/' then begin
                     if String.length url < 2 || url.[1] <> '/' then
                       "https://commons.wikimedia.org" ^ url
                     else
                       "https:" ^ url
                   end else
                     url
                 in
                 [(("", "target"), "_blank"); (("", "href"), url)]
               end
             in
             `El_start (("", nm), attrs)
        | _ -> s
       in
       Xmlm.output o s)
    i;
  let res = Buffer.contents b in
  Xmlm.output o `El_end;
  trim res

let rec accept i nm =
  match Xmlm.input i with
    `Data s                 -> accept i nm
  | `El_start ((_, nm'), _) -> assert (nm = nm')
  | _                       -> assert false

let traverse action i =
  let rec traverse_rec n =
    match Xmlm.input i with
      `Dtd _ ->
        traverse_rec n
    | `Data s ->
  (*
  Format.printf "%s@." s;
  *)
        traverse_rec n
    | `El_start tag ->
        begin match action tag with
          `Done ->
            ()
        | `After f ->
            traverse_rec (n + 1);
            f ()
        | `Cont ->
            traverse_rec (n + 1);
        end;
        if n > 0 then
          traverse_rec n
        else
          assert (Xmlm.eoi i)
    | `El_end ->
        ()
  in
  traverse_rec 0

(****)

let read_td_as_text i = skip i; accept i "td"; trim (text i)
let read_td_as_html i = skip i; accept i "td"; html i

type st =
  { tbl : (string, string) Hashtbl.t;
    mutable licenses : (string, string) Hashtbl.t list }

let debug = false

let get_text i st id =
  let txt = read_td_as_text i in
if debug then Format.eprintf "%s: %s@." id txt;
  Hashtbl.replace st.tbl id txt;
  `Done

let get_subtext i st id =
  let txt = trim (text i) in
if debug then Format.eprintf "%s: %s@." id txt;
  Hashtbl.replace st.tbl id txt;
  `Done

let get_html i st id =
  let txt = read_td_as_html i in
if debug then Format.eprintf "%s: %s@." id txt;
  Hashtbl.replace st.tbl id txt;
  `Done

let get_subhtml i st id =
  let txt = html i in
if debug then Format.eprintf "%s: %s@." id txt;
  Hashtbl.replace st.tbl id txt;
  `Done

(****)

let license_infos =
  ["licensetpl_aut"; "licensetpl_link"; "licensetpl_short"; "licensetpl_long";
   "licensetpl_attr"; "licensetpl_link_req"; "licensetpl_attr_req"]

let get_license i st _ =
  List.iter (fun k -> Hashtbl.replace st.tbl k "") license_infos;
  `After (fun () -> if Hashtbl.find st.tbl "licensetpl_short" <> "" then
                      st.licenses <- Hashtbl.copy st.tbl :: st.licenses)

let id_actions =
  ["fileinfotpl_aut", get_text;
   "fileinfotpl_src", get_text;
   "fileinfotpl_credit", get_html;
   "creator", get_subtext;
   "own-work", get_text]

let cl_actions =
  ["licensetpl_attr", get_subhtml;
   "licensetpl_aut", get_subhtml;
   "licensetpl_link", get_subtext;
   "licensetpl_short", get_subtext;
   "licensetpl", get_license]

let actions i st tag =
  let rec perform tbl l =
    match l with
      [] ->
        `Cont
    | nm :: r ->
        try
          List.assoc nm tbl i st nm
        with Not_found ->
          perform tbl r
  in
  let res = perform id_actions (elt_id tag) in
  if res <> `Cont then res else
  perform cl_actions (elt_classes tag)

let first_occurrence st key =
  let rec first_rec l =
    match l with
      h :: r ->
        let s = Hashtbl.find h key in if s = "" then first_rec r else s
    | [] ->
        raise Not_found
  in
  first_rec (List.rev st.licenses)

let nl_re = Str.regexp "\n"

let attribution st =
  try
    Hashtbl.find st.tbl "fileinfotpl_credit"
  with Not_found -> try
    first_occurrence st "licensetpl_attr"
  with Not_found -> try
    first_occurrence st "licensetpl_aut"
  with Not_found -> try
    Hashtbl.find st.tbl "creator"
  with Not_found ->
    let author =
      try Hashtbl.find st.tbl "fileinfotpl_aut" with Not_found -> "" in
    let source =
      try Hashtbl.find st.tbl "fileinfotpl_src" with Not_found -> "" in
    let author =
      if
        author = "This file is lacking author information." ||
        author = "Unknown"
      then
        ""
      else
        author
    in
    let author =
      let l = String.length author in
      if l > 22 && String.sub author 0 22 = "Original uploader was " then
        String.sub author 22 (l - 22)
      else
        author
    in
    let source =
      let l = String.length source in
      if
        List.mem (String.lowercase source)
          ["own work"; "own work (personal work)"; "\"own work\"";
           "own work (own photo)"; "own work (photo taken by me)"] ||
(*
        (l >= 8 && String.sub source 0 8 = "Own work") ||
*)
        source = "photo taken by me" ||
        (author <> "" && l > 50)
      then
        ""
      else
        source
    in
    let source = trim source in
    let author =
      if String.length author > 200 then
        List.hd (Str.split nl_re author)
      else
        author
    in

    if author <> "" then
      "By " ^ author ^ (if source <> "" then " (" ^ source ^ ")" else "")
    else if source <> "" then
      source
    else
      "Author information on Wikimedia Commons"

let get_license st =
  if st.licenses = [] then
    "[license on Wikimedia Commons]"
  else begin
    let l =
      List.map
        (fun tbl ->
          let name = Hashtbl.find tbl "licensetpl_short" in
          let link = Hashtbl.find tbl "licensetpl_link" in
          if link <> "" then
            Format.sprintf
              "<a target=\"_blank\" href =\"%s\">%s</a>" link name
          else
            name)
        (List.rev st.licenses)
    in
    "[" ^ String.concat " or " l ^ "]"
  end


let lst = ref []

let add_attribution short_name name url file =
  let contents = load_file file in
  utf8_protect contents;
  let i = Xmlm.make_input (`String (0, contents)) in
  try
    let st = {tbl = Hashtbl.create 17; licenses = []} in
    traverse (fun tag -> actions i st tag) i;
    let attribution =
      try
        Hashtbl.find st.tbl "fileinfotpl_credit"
      with Not_found ->
        attribution st ^ " " ^ get_license st
    in
    Format.printf "===> %s@." attribution;
    lst := (short_name, name, url, attribution) :: !lst
  with Xmlm.Error ((l, c), err) ->
    Format.eprintf "Error @@ %d, %d: %s@." l c (Xmlm.error_message err);
    ((*exit 1*))

let write_attributions () =
  let b = Buffer.create 1024 in
  begin
    !lst
    >> Array.of_list
    >> Array.map
         (fun (short_name, name, url, attrib) b ->
            let (thumbnail_url, width, height) =
              parse_thumbnail_info short_name in
            let usage =
              parse_usage short_name name >> filter_usage >> Array.of_list >>
              Array.map
                (fun (title, wiki, url) b ->
                   let lang = String.sub wiki 0 2 in
                   let prefix = "https://" ^ lang ^ ".wikipedia.org/wiki/" in
                   let len = String.length prefix in
                   let len' = String.length url in
                   assert (len' > len && String.sub url 0 len = prefix);
                   let refer = String.sub url len (len' - len) in
                   let refer = if refer = title then "" else refer in
                   json_array b
                     [|(fun b -> json_string b title);
                       (fun b -> json_string b lang);
                       (fun b -> json_string b refer)|])
            in
            json_array b
              [|(fun b -> json_string b short_name);
                (fun b -> json_string b name);
                (fun b -> json_string b attrib);
                (fun b -> json_int b (int_of_string width));
                (fun b -> json_int b (int_of_string height));
                (fun b -> json_array b usage);
                (fun b -> match thumbnail_url with
                            None     -> json_int b 0
                          | Some url -> json_array b
                                          [|(fun b -> json_string b url)|])|])
    >> json_array b
  end;
  let ch = open_out output_file in
  output_string ch (Buffer.contents b);
  close_out ch

(****)

let url_res =
  List.map Str.regexp
  ["^ *http://commons\\.wikimedia\\.org/wiki/File:\\([^?]*\\)$";
   "^ *http://fr\\.wikipedia\\.org/wiki/Fichier:\\([^?]*\\)$";
   "^ *http://en\\.wikipedia\\.org/wiki/File:\\([^?]*\\)$"]
let suffix_re = Str.regexp "[.][^.]*$"

let _ =
  let ch = open_in input_file in
  let tbl = Hashtbl.create 128 in
  begin try
    while true do
      let short_name = input_line ch in
      let url = trim (input_line ch) in
      Hashtbl.add tbl short_name url
    done
  with End_of_file -> () end;
  close_in ch;
  let add_field short_name =
    let url = Hashtbl.find tbl short_name in
    if List.exists (fun re -> Str.string_match re url 0) url_res then begin
      let name = Str.matched_group 1 url in
      let url = "https://commons.wikimedia.org/wiki/File:" ^ name in

      let html = Filename.concat html_cache short_name in
      if not (Sys.file_exists html) then begin
        Format.printf "Fetching image page for %s@." short_name;
        http_get url html
      end;

      let thumbnail = Filename.concat thumbnail_cache short_name in
      if not (Sys.file_exists thumbnail) then begin
        Format.printf "Fetching image info for %s@." short_name;
        http_get (image_url name) thumbnail
      end;

      let usage = Filename.concat usage_cache short_name in
      if not (Sys.file_exists usage) then begin
        Format.printf "Fetching usage info for %s@." short_name;
        http_get (usage_url name) usage
      end;

      if List.for_all Sys.file_exists [html; thumbnail; usage] then begin
        Format.printf "Analyzing %s <%s>@." short_name url;
        add_attribution short_name name url html
      end
    end else
      Format.eprintf "Bad URL: %s@." url
  in
  begin try
    while true do
      let l = Str.split space_re (input_line stdin) in
      List.iter add_field l
    done
  with End_of_file -> () end;
  write_attributions ()
