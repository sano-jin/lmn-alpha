(** Instructions of the intermediate code *)

open Util

type reg_i = int
(** レジスタ番号 *)

type functor_ = string * int
(** ファンクタ := (アトム名, リンクの数) *)

(** アトムのデータ構造としてとりあえず前提としているもの
     - [type atom = string * link list
        and  link = NormalLink of int * atom ref (* | ... *)
       ]
     - ただし，[int * atom ref] はそれぞれアトムへの参照とその先でどの番号のポートに接続されているか
       を表す
*)

(** ルール左辺におけるマッチングのための中間命令
    - failable は失敗する可能性があるということ
    - rewind は後続の命令が失敗したときに巻き戻しの起点になるということ
    - レジスタに格納するデータはアトムへの参照のみ
    - [type register = atom ref]
 *)
type lhs_inst =
  | PeakAtom of reg_i * functor_
      (** アトムリストの先頭から随時，ファンクタが [functor_] であるアトムへの参照を，
      レジスタ [reg_i] に格納してゆく 
      - failable and possibly rewind
   *)
  | CheckFunctor of reg_i * functor_
      (** [reg_i] に格納したアトムのファンクタが [functor_] であることを確認する
      - failable and does not rewind
   *)
  | DerefAtom of reg_i * reg_i * int * int
      (** [DerefAtom dst_reg_i src_reg_i src_port_i dst_port_i] は，
      レジスタ [src_reg_i] が参照するアトムの 
      [src_port_i] 番目の引数の持つアトムへの参照を
      このリンクが相手先で [dst_port_i] 番目の引数に接続されていることを確認して
      レジスタ [dst_reg_i] に格納する
      - failable and does not rewind
      - （通常のリンクではなかったときも失敗するようにしようと思っていたけど，そうしないことにする？）
      - 基本的に局所リンクのマッチングに用いる
   *)
  | CheckRefEq of reg_i * reg_i
      (** [CheckRefEq reg_i reg_j] は
      レジスタ [reg_i] に格納されているアドレスと
      レジスタ [reg_j] に格納されているアドレスが等しいことを確認する
      - failable and does not rewind
   *)
  | CheckRefNeq of reg_i * reg_i
      (** [CheckRefEq reg_i reg_j] は
      レジスタ [reg_i] に格納されているアドレスと
      レジスタ [reg_j] に格納されているアドレスが異なることを確認する
      - failable and does not rewind
   *)
  | BindData of string * reg_i * int
      (** [BindData var src_reg_i src_port_i] は
      レジスタ [src_reg_i] が参照するアトムのポート [src_port_i] 番目のデータアトムを
      変数 [var] に束縛する
      - 今は完全にインタプリタ方式だが，より低レベルな命令列に変更する必要がある．
   *)
  | FailMatching of string  (** 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)

type lhs_insts = lhs_inst list

(** ルール右辺におけるマッチングのための中間命令．
    どれも失敗することはない．
 *)
type rhs_inst =
  | PushAtom of reg_i * functor_
      (** [PushAtom reg_i functor_] は，
      ルール右辺で生成する，ファンクタ [functor_] の（シンボル）アトムを生成し，レジスタ [reg_i] に代入する
      - アトムリストへの追加も行う
      - ただし，リンクの値は正しい値にセットされない
   *)
  | FreeAtom of reg_i
      (** [FreeAtom reg_i] は，
      レジスタ [reg_i] が参照する先のアトムをアトムリストから除去し，メモリを解放する
   *)
  | SetLink of reg_i * int * reg_i * int
      (** [SetLink reg_i port_i reg_j port_j] は，
      レジスタ [reg_i] が参照するアトムの [port_i] 番目のリンクと
      レジスタ [reg_j] が参照するアトムの [port_j] 番目のリンクを結ぶ
      - 基本的に局所リンクのための命令
   *)
  | ReLink of reg_i * int * reg_i * int
      (** [ReLink reg_i port_i reg_j port_j] は，
      レジスタ [reg_i] が参照するルール右辺で生成したアトムの [port_i] 番目のリンクと
      レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクを繋ぐ
      - リンクオブジェクトは，基本的には，参照先のアトムと，参照先でのポート番号の組になっている
      - 基本的に自由リンクのための命令
   *)
  | Connect of reg_i * int * reg_i * int
      (** [Connect reg_i reg_j] は，
      レジスタ [reg_i] が参照するルール左辺でマッチしたアトムの [port_i] 番目のリンクと
      レジスタ [reg_j] が参照するルール左辺でマッチしたアトムの [port_j] 番目のリンクとを結ぶ
      - リンクオブジェクトは，基本的には，参照先のアトムと，参照先でのポート番号の組になっている
      - 自由リンク同士を接続するために用いる
   *)
  | SetData of reg_i * int * string
      (** [PushData reg_i port_i var] は，
      ルール右辺で生成する，変数 [var] に束縛されたデータアトムを生成し，
      レジスタ [reg_i] 番目のアトムの，ポート [port_i] にセットする
      - アトムリストへの追加は行わない
      - 今は完全にインタプリタ方式だが，より低レベルな命令列に変更する必要がある．
   *)
  | FailPushout of string  (** 仮想マシンを（途中で）強制終了する．デバッグのための命令 *)

type rhs_insts = rhs_inst list

(** 中間コードを文字列へ変換する
    - 中間命令列のファイルを生成するため
    - とデバッグ時の dump のため
 *)

(** *)
let string_of_functor (p, arity) = Printf.sprintf "'%s'_%d" p arity

let string_of_lhs_inst = function
  | PeakAtom (reg_i, functor_) ->
      Printf.sprintf "PeakAtom     [ %d %s ]" reg_i
      @@ string_of_functor functor_
  | CheckFunctor (reg_i, functor_) ->
      Printf.sprintf "CheckFunctor [ %d %s ]" reg_i
      @@ string_of_functor functor_
  | DerefAtom (dst_reg_i, src_reg_i, src_port_i, dst_port_i) ->
      Printf.sprintf "DerefAtom    [ %d %d %d %d ]" dst_reg_i src_reg_i
        src_port_i dst_port_i
  | CheckRefEq (reg_i, reg_j) ->
      Printf.sprintf "CheckRefEq   [ %d %d ]" reg_i reg_j
  | CheckRefNeq (reg_i, reg_j) ->
      Printf.sprintf "CheckRefNeq  [ %d %d ]" reg_i reg_j
  | BindData (var, reg_i, reg_j) ->
      Printf.sprintf "BindData     [ %s %d %d ]" var reg_i reg_j
  | FailMatching message -> Printf.sprintf "FailMatching [ %s ]" message

let string_of_rhs_inst = function
  | PushAtom (reg_i, functor_) ->
      Printf.sprintf "PushAtom     [ %d %s ]" reg_i
      @@ string_of_functor functor_
  | FreeAtom reg_i -> Printf.sprintf "FreeAtom     [ %d ]" reg_i
  | SetLink (reg_i, port_i, reg_j, port_j) ->
      Printf.sprintf "SetLink      [ %d %d %d %d ]" reg_i port_i reg_j port_j
  | ReLink (reg_i, port_i, reg_j, port_j) ->
      Printf.sprintf "ReLink       [ %d %d %d %d ]" reg_i port_i reg_j port_j
  | Connect (reg_i, port_i, reg_j, port_j) ->
      Printf.sprintf "Connect      [ %d %d %d %d ]" reg_i port_i reg_j port_j
  | SetData (reg_i, port_i, var) ->
      Printf.sprintf "SetData      [ %d %d %s ]" reg_i port_i var
  | FailPushout message -> Printf.sprintf "FailPushout  [ %s ]" message

let string_of_lhs_insts =
  String.concat "\n" <. List.map (indent 1 <. string_of_lhs_inst)

let string_of_rhs_insts =
  String.concat "\n" <. List.map (indent 1 <. string_of_rhs_inst)

let string_of_rule (rule_name, (reg_size, (lhs_insts, guard, rhs_insts))) =
  String.concat "\n"
  @@ [
       rule_name ^ " @@";
       indent 1 @@ "alloc [" ^ string_of_int reg_size ^ "]";
       "--head:";
       string_of_lhs_insts lhs_insts;
       "--guard:";
       Parse.string_of_guard guard;
       "--body:";
       string_of_rhs_insts rhs_insts;
       "";
     ]

let break_line str = if str = "" then "" else str ^ "\n"

let string_of_prog ((init_reg_size, init_pushout), ruleset) =
  String.concat "\n"
  @@ "init @@"
     :: indent 1 ("alloc [" ^ string_of_int init_reg_size ^ "]")
     :: break_line (string_of_rhs_insts init_pushout)
     :: List.map string_of_rule ruleset
