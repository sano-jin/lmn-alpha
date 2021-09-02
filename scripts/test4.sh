#!/bin/bash -eu

SLIM='/Users/sano/work/slim/bin/slim'
LMNTAL='/Users/sano/work/lmntal-compiler/bin/lmntal'
LMN_BETA='/Users/sano/work/lmn-beta/_build/install/default/bin/lmn'
time=/usr/local/opt/gnu-time/libexec/gnubin/time

mkdir -p tmp

echo "beta" > beta_result.txt
echo "slim" > slim_result.txt


for n in $(seq -f "%.0f" 5000000 5000000 100000000); do
    echo $n
    sed "s/NUM/$n/g" example/intN.lmn > tmp/prog.lmn

    # cat tmp/prog.lmn

    # %e: 経過した実時間（秒単位）
    # %M: RSS（Resident Set Size：物理メモリの使用量）の最大値（KB単位）
    $time -f "  ${n}00, %e, %M" $LMN_BETA tmp/prog.lmn 2>> beta_result.txt

    $LMNTAL --slimcode -O3 --use-swaplink tmp/prog.lmn | $time -f "  ${n}00, %e, %M"  $SLIM - 2>> slim_result.txt
done
