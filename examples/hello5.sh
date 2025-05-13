#!/usr/bin/env bash
set -e
# ref. https://github.com/oreilly-japan/mastering-linux-shell-scripting-2e-ja/blob/c1d6b1a17e169603a146a9fc88ee3d6396b1f481/ch03/hello5.sh
if [ "$#" -lt 1 ]; then
  echo "Usage: $0 <name>"
  exit 1
fi

echo "Hello $1"
exit 0
