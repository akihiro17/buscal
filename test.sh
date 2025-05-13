#!/usr/bin/env bash
a=1
b="abcd efg"
function add() {
  local a="$1"
  local b="$2"
  echo "$((${a} + ${b}))"
}
res=$(add 1 3)
echo ${res}
echo "true"
now=$("date")
if echo ${res}; then
  echo $now
  echo "true cond"
fi
