#!/bin/bash -eu

SLIM='/Users/sano/work/slim/bin/slim'
LMNTAL='/Users/sano/work/lmntal-compiler/bin/lmntal'
LMN_BETA='/Users/sano/work/lmn-beta/_build/install/default/bin/lmn'
time=/usr/local/opt/gnu-time/libexec/gnubin/time

mkdir -p tmp

echo "beta" > beta_result.txt
echo "slim" > slim_result.txt



a100=""
for i in {1..1000}; do
  a100="${a100},a"
done

str="(a:-)"
for n in {1..100}; do
    echo $n
    str="${str}${a100}"
  
  # %e: 経過した実時間（秒単位）
  # %M: RSS（Resident Set Size：物理メモリの使用量）の最大値（KB単位）
  echo $str > tmp/prog.lmn
  $time -f "  ${n}00, %e, %M" $LMN_BETA tmp/prog.lmn >/dev/null 2>> beta_result.txt

  echo $str | $LMNTAL --stdin-lmn --slimcode -O3 --use-swaplink | $time -f "  ${n}00, %e, %M"  $SLIM - > /dev/null 2>> slim_result.txt
done
