type node =
  { data : int;
    mutable left : node option;
    mutable right : node option;
  }

let tree_insert (root : node option ref) (data : int) =
  let rec tree_insert' (cur : node option) (data : int) : node =
    match cur with
    | None ->
       { data = data;
         left = None;
         right = None;
       }
    | Some _node ->
       match data < _node.data, data > _node.data with
       | (true, false) -> _node.left <- Some (tree_insert' _node.left data); _node
       | (false, true) -> _node.right <- Some (tree_insert' _node.right data); _node
       | _ -> failwith "unreachable"
  in root := Some (tree_insert' !root data)

let tree_dump (root : node option ref) =
  let rec tree_dump' (cur : node option) =
    match cur with
    | None -> ()
    | Some _node ->
       let _ = tree_dump' _node.left in
       Printf.printf "%d\n" _node.data;
       tree_dump' _node.right
  in tree_dump' !root

let () =
  let root : node option ref = ref None in
  [10;8;9;5;3] |> List.iter (fun s -> s |> tree_insert root);
  tree_dump root
