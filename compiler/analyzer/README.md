# Analyzer
_Semantic analyzer_


## 意味解析器

Core language に変換する段階で，既にある程度意味解析を行ってしまっているので，
現状やれることはあまりない

将来的には
- （アトムの流量解析による）アトムの並び替え
- 何らかの static type checker
- ルール間の confluence property の解析（ファンクタの集合を比較したりとか）
- 何らかのプログラム変換

などがここに位置するのかも知れない


現状は
- それぞれのアトムに一意な id を割り当てる
- 局所リンクの接続先のアトムの id・ポート位置などの情報を収集

くらい


## プログラム構成

- [sem_graph.ml](sem_graph.ml)
  - Semantic graph definition

- [port.ml](port.ml)
  - Collect port indices 

- [analyze.ml](analyze.ml)
  - The top-level of the front end
  
