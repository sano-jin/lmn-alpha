(** Utility functions.
    
*)


(** load Doubly linked list *)
module DList = Dlist
		 


(** 基本的なコンビネータなど *)


(** some very basic combinators *)

(** *)
let flip f x y = f y x  

let id x = x
let const x _ = x

let curry f x y = f (x, y)
let uncurry f (x, y) = f x y


		 
(** tuple の操作のためのコンビネータ *)

(** *)
let first f (a, b) = (f a, b)
let second f (a, b) = (a, f b)
let both f (a, b) = (f a, f b)

let pair x y = (x, y)
let swap (x, y) = (y, x)

		    

(** compositional functions *)

(** *)
let (<.) f g = fun x -> f (g x)
let (<..) f g = fun x y -> f (g x y)
let (<...) f g = fun x y z -> f (g x y z)


	 

(** monadic combinators for the Option type *)

(** *)
let (>>=) = Option.bind
let ( let* ) = Option.bind


let (<$>) = Option.map
let ( let+ ) x f = Option.map f x


let (<|>) l r = 
  if Option.is_some l then l
  else r ()


(** f を適用してどれか一つでも Some を返したらそれを返して終わりにする *)
let rec one_of f = function
  | [] -> None
  | h::t -> f h <|> fun _ -> one_of f t


let maybe default = function
  | None -> default
  | Some s -> s 



(** monadic combinators for the traversible type *)

(** monadic cons *)
let (<::>) h t = List.cons h <$> t


(** monadic [fold_left] *)
let rec foldM f acc = function
  | [] -> Some acc
  | h::t -> f acc h >>= flip (foldM f) t			     




(** 集合演算
    - Set を用いるようにリファクタリングしても良いかも 
*)
let set_minus l r = List.filter (not <. flip List.mem r) l
let sym_diff l r = set_minus l r @ set_minus r l



(** Either 型の要素のリストを左右に振り分ける *)
let partitionEithers l = List.partition_map id l


(** zip/unzip *)

(** uncurried monadic combine (possibly renamed as [safe_combine]) *)
let rec uncurried_safe_unzip = function
  | ([], []) -> Some []
  | (xh::xt, yh::yt) -> (xh, yh) <::> uncurried_safe_unzip (xt, yt)
  | _ -> None

(** monadic combine (possibly renamed as [safe_combine]) *)
let safe_unzip t = curry uncurried_safe_unzip t





(** Add 4 * n white spaces to the head of the string
    - ["\t"] の方が良いかも
 *)
let indent n = (^) @@ String.make (4 * n) ' '



(** 入出力のための関数 *)


(** read lines from the given file *)
let read_file name =
  let ic = open_in name in
  let try_read () =
    try Some (input_line ic) with End_of_file -> None in
  let rec loop acc = match try_read () with
    | Some s -> loop (s :: acc)
    | None ->
       close_in ic;
       String.concat "\n" @@ List.rev acc
  in
  loop []





(** リスト系 *)

(** リストの要素の添字番号を取得する *)
let index_of elem =
  let rec helper index = function
    | [] -> None
    | h::t -> if h = elem then Some index
	      else helper (succ index) t
  in helper 0
       


let fold_left_map2 l =
  second (second List.concat <. List.split) <.. List.fold_left_map l
								   


(** 要素に Option を返す関数を適用して，初めて Some になったところでリストを分割する
    - Some になった値も返す
    - None になった部分のリストは反転して返すことに注意
      - [rev_appned] をすると元のリストから一つ要素を除いたものになる
    - [break_opt (fun x -> if x > 3 then Some x else None) [1; 2; 3; 4; 5; 6]
    ---> Some (4, ([3; 2; 1], [5; 6]))
    ]
    - Tail-recursive
    @return a list, (Some (f a, a list) | None)
 *)
let rev_break_opt f =
  let rec helper left = function
  | [] -> None
  | h::t ->
     match f h with
     | None -> helper (h::left) t
     | Some s -> Some (s, (left, t))
  in		   
  helper []




(** Tail-recursive List.concat with List.rev_append
    - リストのリストを反転させながら結合する
    - [rev_concat [[6; 5; 4]; [3; 2]; [1]] [7; 8]
    ---> rev_append [1] (rev_append [3; 2] (rev_append [6; 5; 4] [7; 8]))
    ---> [1; 2; 3; 4; 5; 6; 7; 8]
    ]
 *)
let rev_concat_append lists list =
  List.fold_left (flip List.rev_append) list lists
  


(** Tail-recursive List.concat
    - リストのリストを反転させながら結合する
    - [rev_concat [[1; 2; 3]; [4; 5]; [6]]
    ---> rev_append [6] (rev_append [4; 5] (rev_append [1; 2; 3] []))
    ---> [6; 5; 4; 3; 2; 1]
    ]
 *)
let rev_concat lists = rev_concat_append [] lists

  




(** [List.fold_left] with indices  *)		
let fold_lefti f =
  let rec helper i acc = function
    | [] -> acc
    | h::t -> helper (succ i) (f i acc h) t
  in
  helper 0 


(** [List.fold_left_map] with indices  *)		
let fold_left_mapi f acc xs =
  let rec helper i acc = function
    | [] -> acc, []
    | h::t ->
       let acc, h = f i acc h in
       let acc, t = helper (succ i) acc t in
       acc, h::t
  in
  helper 0 acc xs



(** リストを "回転" する
    - [roll [1; 2; 3; 4] ---> [2; 3; 4; 1]]
    - TODO: もっと効率の良い実装にする．キューを使うなど
 *)
let roll = function
  | [] -> []
  | h::t -> t@[h]



(** Monadic while.
    Tail recursive.
    @param f Option 型を返す関数
    @param x 最初の入力値
    @return f をゼロ回以上適用して None になったら，その直前の x を返す
 *)
let rec whileM f x =
  match f x with
  | None -> x
  | Some x -> whileM f x


(** リストの末尾を除去したリストを返す *)		     
let rec dropLast1 = function
  | [] -> failwith "cannot drop the last element from an empty list"
  | [_] -> []
  | h::t -> h::dropLast1 t
  
