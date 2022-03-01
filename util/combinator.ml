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
let ( <. ) f g x = f (g x)

let ( <.. ) f g x y = f (g x y)
let ( <... ) f g x y z = f (g x y z)

(** monadic combinators for the Option type *)

(** *)
let ( >>= ) = Option.bind

let ( let* ) = Option.bind
let ( <$> ) = Option.map
let ( let+ ) x f = Option.map f x
let ( <|> ) l r = if Option.is_some l then l else r ()

(** f を適用してどれか一つでも Some を返したらそれを返して終わりにする *)
let rec one_of f = function [] -> None | h :: t -> f h <|> fun _ -> one_of f t

let maybe default = function None -> default | Some s -> s

(** monadic combinators for the traversible type *)

(** monadic cons *)
let ( <::> ) h t = List.cons h <$> t

(** monadic [fold_left] 
    - f を適用して，Some が帰ってきたら fold を続ける．
    - もし一度でも None が帰ってきたら，None を返す
*)
let rec foldM f acc = function
  | [] -> Some acc
  | h :: t -> f acc h >>= flip (foldM f) t
