let init_cwd = Sys.getcwd ()

let root_witness = ["dune-project"; ".git"; ".hg"]

let rec look_for fnames path =
  let curr_els = Sys.readdir path in
  let found = List.fold_left (fun found fn -> 
    match found with
    | Some x -> Some x
    | None -> if Array.mem fn curr_els then Some (Filename.concat path fn) else None) None fnames
  in
  match found with
  | Some f -> Some f
  | None -> 
    let is_root =
      List.exists (fun x -> Array.mem x curr_els) root_witness
    in
    if is_root then None else look_for fnames (Filename.concat path Filename.parent_dir_name) 


let names_with_profile suffix = function
  | None -> [ suffix ]
  | Some profile -> [ "." ^ profile ^ suffix; suffix ]

let find_files (profile: string option) =
  let config_file_names = names_with_profile  ".headacheconfig" profile in
  let header_file_names = names_with_profile ".headacheheader" profile in
  let header_file = look_for header_file_names "." in
  let config_file = look_for config_file_names "." in
  (config_file, header_file)