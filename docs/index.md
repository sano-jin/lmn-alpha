# lmn-alpha

<https://github.com/sano-jin/lmn-alpha>


flat lmntal の簡単なコンパイラと仮想機械



## プログラム構成

全部で 897 LOC

[ocamldoc により生成したドキュメント](https://sano-jin.github.io/lmn-alpha/ocamldoc/lmn/index.html)


### Compiler
546 LOC

- parse: 169 LOC
    - 字句解析・構文解析を行う
- corelang: 139 LOC
	- 糖衣構文の解消・リンクのチェック 
- analyzer: 42 LOC
    - 意味解析：ポート情報の付加
- generator: 167 LOC
	- [中間命令列](https://sano-jin.github.io/lmn-alpha/ocamldoc/lmn/Generator__/Instruction/index.html)
	  を生成する

### Runtime
233 LOC

- vm: 129 LOC
    - 仮想マシン

### Utility
- util: 118 LOC
    - 共用モジュール




