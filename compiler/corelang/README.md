# Corelang
_Translate the source code to the Core-language_


## Core language 概要

後の工程や，他のアプリケーション等で使いやすくなるようにした中間言語へ変換する


- 局所リンクに一意な id を割り当て
- ルールとグラフを分割
- グラフは一本のリストへ分解
- 略記法は解消（特に吸収可能なコネクタの除去）
- サブルールが存在しないことのチェック（今回の実装ではルール右辺におけるサブルールもサポート外）
- ルール右辺に存在する自由リンク同士を接続するコネクタをその他のアトムから分離
- ルール左辺・プログラム全体に吸収不可能なコネクタがないかをチェック


基本的に syntactic sugar を解消するだけ
- 構文的エラーのチェックも行う


## プログラム構成

- [syntax.ml](syntax.ml)
  - Syntax of the core language

- [alpha.ml](alpha.ml)
  - Convert local link names to fresh ids

- [compile_error.ml](compile_error.ml)
  - Defines a type for the exception of the compiling errors

- [connector.ml](connector.ml)
  - Absorb local fusion/connector

- [link.ml](link.ml)
  - Collect link information and check them

- [partition.ml](partition.ml)
  - Partition graph and rules

- [rule.ml](rule.ml)
  - Check rule condition and returns the core language representation of a rule

- [corelang.ml](corelang.ml)
  - The top-level of this module. 
    Analyze the given AST and obtain the core language representation
  
