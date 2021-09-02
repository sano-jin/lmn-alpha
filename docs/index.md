# lmn-alpha

<https://github.com/sano-jin/lmn-alpha>


flat lmntal の簡単なコンパイラと仮想機械


紹介スライド（lmn-alpha）：
<object data="./lmn-alpha.pdf" type="application/pdf" width="100%" height="700px">
	<embed src="./lmn-alpha.pdf" />
		<p>This browser does not support PDFs. Please download the PDF to view it:
			<a href="./lmn-alpha.pdf">Download PDF</a>
		</p>
</object>
	

## プログラム構成

全部で 997 LOC

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
296 LOC

- vm: 153 LOC
    - 仮想マシン

- pretty: 69 LOC
    - Pretty printer


### Utility
- util: 155 LOC
    - 共用モジュール




