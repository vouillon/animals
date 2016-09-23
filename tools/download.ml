#! /usr/bin/ocaml unix.cma
#load "str.cma"

let success = ref true

let _ =
if Array.length Sys.argv < 3 then begin
  Format.eprintf "Usage: %s <refs> <cache_dir>@." Sys.argv.(0);
  exit 1
end

let http_get url file =
  let tmp = file ^ ".tmp" in
  let res = Sys.command (Format.sprintf "curl -f -# -o %s '%s'" tmp url) in
  if res = 0 then
    Sys.rename tmp file
  else begin
    try Sys.remove tmp with Sys_error _ -> ()
  end;
  res = 0

let dest_dir = Sys.argv.(2)

let url name =
  Format.sprintf
    "https://commons.wikimedia.org/w/api.php?\
     format=json&action=query&titles=%s&prop=imageinfo&iiprop=url"
    name

let re = Str.regexp ".*\"url\":\"\\([^\"]*+\\)\""

let get_url name =
  Format.printf "%s@." name;
  let ch = Unix.open_process_in ("curl -s '" ^ url name ^ "'") in
  let url = ref None in
  begin try
    while true do
      let l = input_line ch in
      (*      Format.printf "%s@." l;*)
      if Str.string_match re l 0 then
        url := Some (Str.matched_group 1 l)
    done
  with End_of_file -> () end;
  ignore (Unix.close_process_in ch);
  !url

let url_res =
  List.map Str.regexp
  ["^ *http://commons\\.wikimedia\\.org/wiki/File:\\(.*\\)$";
   "^ *http://fr\\.wikipedia\\.org/wiki/Fichier:\\(.*\\)$";
   "^ *http://en\\.wikipedia\\.org/wiki/File:\\(.*\\)$"]
let suffix_re = Str.regexp "[.][^.]*$"

let suffix nm =
  let i = Str.search_forward suffix_re nm 0 in
  String.sub nm i (String.length nm - i)

let target short_name suf = Format.sprintf "%s/%s%s" dest_dir short_name suf

let _ =
  let ch = open_in Sys.argv.(1) in
  begin try
    while true do
      let short_name = input_line ch in
      let url = input_line ch in
      if
        not (Sys.file_exists (target short_name ".jpg") ||
             Sys.file_exists (target short_name ".png"))
      then begin
        Format.printf "%s@." short_name;
        if List.exists (fun re -> Str.string_match re url 0) url_res then begin
          let name = "File:" ^ Str.matched_group 1 url in
          match get_url name with
            Some url ->
              Format.printf "===> %s@." url;
              let suf =
                match suffix url with
                  ".JPG" | ".jpeg" | ".jpg" | ".JPEG" -> ".jpg"
                | ".png" | ".PNG"                     -> ".png"
                | _                                   -> assert false
              in
              let target = Format.sprintf "%s/%s%s" dest_dir short_name suf in
              if not (Sys.file_exists target) then
                if not (http_get url target) then success := false
(*
                ignore
                  (Sys.command
                     (Format.sprintf "curl -f -# -o %s '%s'" target url))
*)
          | None ->
              Format.eprintf "Image not found@.";
              success := false
        end else begin
          Format.eprintf "Bad URL: %s@." url;
          success := false
        end
      end
    done
  with End_of_file -> () end;
  close_in ch

let _ = if not !success then exit 1
