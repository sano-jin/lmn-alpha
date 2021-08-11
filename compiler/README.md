# Compiler


## コンパイラ概要


字句解析・構文解析器
- ソースコードから AST を生成する

Core language への変換
- 後の工程や，他のアプリケーション等で使いやすくなるようにした中間言語へ変換する
  - 局所リンクに一意な id を割り当て
  - ルールとグラフを分割
  - グラフは一本のリストへ分解
  - 略記法は解消（特に吸収可能なコネクタの除去）
  - サブルールが存在しないことのチェック（今回の実装ではルール右辺におけるサブルールもサポート外）
  - ルール右辺に存在する自由リンク同士を接続するコネクタをその他のアトムから分離
  - ルール左辺・プログラム全体に吸収不可能なコネクタがないかをチェック


意味解析器
- それぞれのアトムに一意な id を割り当てる
- 局所リンクの接続先のアトムの id・ポート位置などの情報を収集
- Core language にポート情報を付加した Semantic graph を生成する


中間命令列生成器
- 意味解析を行った後に得られる Semantic graph から中間命令列を生成する



## プログラム構成


- [parser/](parser)
  - Lexical/Syntax analyzer

- [corelang/](corelang)
  - Translator to the Core language 

- [analyzer/](analyzer)
  - Semantic analyzer

- [generator/](generator)
  - Intermediate code generator

- [compiler.ml](compiler.ml)
  - The toplevel of the compiler

- [test/](test)
  - Test codes
