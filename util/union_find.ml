(** union find *)


module UnionFind =
  functor (X: Map.OrderedType) ->
  struct 
      type t = X.t
      module UnionFind_ = Map.Make(X)

    (** x の代表元を返す
    - 引数や戻り値の順番は逆の方が自然な気はする
      （fold_left_map のためにこうしているだけ）
     *)
    let rec find uf_tree x =
      match UnionFind_.find_opt x uf_tree with
      | None -> uf_tree, x
      | Some z ->
	 let uf_tree, z = find uf_tree z in
	 UnionFind_.add x z uf_tree, z


    (** fusion *)
    let fusion uf_tree (x, y) =
      let uf_tree, z = find uf_tree y in
      UnionFind_.add x z uf_tree
  end
