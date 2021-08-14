#!/bin/bash
# slim 性能測定用コード
# 引数として，測定したいプログラムを受け取る
# 途中結果は tmp ディレクトリ内にファイルを作ってそこへ書き込む

# エラーが起きた時は終了するようにする
set -eu

################################################################################
# コンパイルオプション，測定したいファイルの場所など

SLIM='/Users/sano/work/slim/bin/slim'
LMNTAL='/Users/sano/work/lmntal-compiler/bin/lmntal'

COMPILE_OPTION='--slimcode -O3 --use-swaplink'
EXEC_OPTION='-p'

COMPILE="$LMNTAL $COMPILE_OPTION"
EXEC="$SLIM $EXEC_OPTION"


# 実行時に指定された引数の数、つまり変数 $# の値が 1 でなければエラー終了
if [ $# -ne 1 ]; then
  echo "指定された引数は $# 個です" >&2
  echo "実行するには 1 個の引数が必要です" >&2
  exit 1
fi

# 引数として，実行したいプログラムのファイルパスを受け取る
PROG="$1"


################################################################################

# スクリプトが置かれている場所の一段上のディレクトリへ移動する
# プロジェクトが root_of_the_project/script/this_file.sh のようになっていることを前提にして，
# プロジェクトのルートディレクトリへ移動している
# （そうでないと実行できない・実行しづらいアプリも想定）
cd "`dirname "$0"`"'/..' 


# 中間データの出力用に temporary directory を作る
# -p で，元々その directory が存在していた場合もエラーを吐かなくなる
mkdir -p tmp


################################################################################
# コンパイルと実行

# コンパイル
echo '>>>> compiling' >&2
$COMPILE $PROG > tmp/prog.il

# 実行
echo '>>>> performance measuring' >&2
# 標準エラー出力のみファイルに書き込む
# 実行結果は標準エラー出力へリダイレクトしておくことにした（たぶんいらないので）
(time $EXEC tmp/prog.il) 2>tmp/exec_result.txt | tee >&2



################################################################################
# 以下，実行時間の抽出を行う

echo '>>>> organizing the result' >&2
cat tmp/exec_result.txt >&2

# slim -p の結果の CPU Usage, Exec time のみ抽出する
awk 'NR == 7 {print $3}' tmp/exec_result.txt > tmp/core_peformance.txt


# time コマンドの結果の user（このアプリケーション）が使った時間を抽出する
# 1. time コマンドの実行結果は下3行に出るのでそれを抽出
# 2. awk で user の実行時間を取得する
# 3. MmS.SSSs のようになっているので，sed を使って，M S.SSS のように単位を消してスペースを開けて出力
# 4. awk で M S.SSS のようになっているものを秒に直す
tail -n3 tmp/exec_result.txt \
    | awk 'NR == 2 {print $2}' \
    | sed -e 's/\(\d*\)m\(.*\)s/\1 \2/' \
    | awk '{print $1 * 60 + $2}' \
    > tmp/all_performance.txt


################################################################################
# 結果の出力

echo '>>>> result' >&2
# 「paste -d 区切り文字 ファイル1 ... ファイルn」で区切り文字で区切ってファイルの内容を出力する
paste -d ', ' tmp/core_peformance.txt tmp/all_performance.txt 



# rm -rf tmp


