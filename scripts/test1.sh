#!/bin/bash
# 性能測定用コード

# エラーが起きた時は終了するようにする
set -eu



################################################################################

# スクリプトが置かれている場所の一段上のディレクトリへ移動する
cd "`dirname "$0"`"'/..' 


# 中間データの出力用に temporary directory を作る
# -p で，元々その directory が存在していた場合もエラーを吐かなくなる
mkdir -p tmp

TEST_PROG='example/quadrant.lmn'
TMP_PROG='tmp/tmp_quadrant.lmn'
SLIM_RESULT='tmp/slim_result_acc.csv'
LMN_ALPHA_RESULT='tmp/lmn_alpha_result_acc.csv'


touch $SLIM_RESULT
echo "i, stepN, -p (s), time (s)" > $SLIM_RESULT

touch $LMN_ALPHA_RESULT
echo "i, stepN, time (s)" > $LMN_ALPHA_RESULT


for i in `seq 1 7`
do
    echo "$i th iteration" >&2
    stepN=$((2 ** (2 * $i)))

    cp $TEST_PROG $TMP_PROG
    echo "t$i, t$i, t$i." >> $TMP_PROG

    echo '>>>> lmn-alpha'
    tmp_lmn_alpha_result=`./scripts/test_lmn_alpha.sh $TMP_PROG`
    echo "$i, $stepN, $tmp_lmn_alpha_result" >> $LMN_ALPHA_RESULT    
    
    echo '>>>> slim'
    tmp_slim_result=`./scripts/test_slim.sh $TMP_PROG`
    echo "$i, $stepN, $tmp_slim_result" >> $SLIM_RESULT
    
    rm $TMP_PROG
done

echo '>>>> end measuring' >&2

echo 'slim result'
cat $SLIM_RESULT

echo '-----'

echo 'lmn-alpha result'
cat $LMN_ALPHA_RESULT


rm -rf tmp
