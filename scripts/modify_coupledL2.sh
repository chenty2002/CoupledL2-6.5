#!/bin/bash

cd coupledL2 || exit
patch_file="../scripts/coupledL2.diff"

# 检查补丁是否可以应用
git apply --check "$patch_file" &> /dev/null
conflict=$?

# 检查补丁是否已经被用过(是否可以反向应用)
git apply --reverse --check $patch_file &> /dev/null
applied=$?

# 条件判断逻辑
if [ $conflict -eq 0 ] && [ $applied -ne 0 ]; then
  echo "  补丁可以应用，正在应用..."
  git apply "$patch_file"
elif [ $conflict -ne 0 ] && [ $applied -eq 0 ]; then
  echo "  补丁已应用，无需重复应用。"
elif [ $conflict -ne 0 ] && [ $applied -ne 0 ]; then
  echo "  [ERROR] 补丁无法应用，与当前代码存在冲突！"
  exit 1
else
  echo "  不应该 apply 和 reverse 都成功，疑问？"
fi

cd ..