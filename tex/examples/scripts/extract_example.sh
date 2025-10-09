#!/usr/bin/env bash 
set -euo pipefail

: "${SED:=sed}"

if [ "$#" -ne 3 ]; then
  echo "usage: $0 PRELUDE_NAME NAME ALL_CML" >&2
  exit 1
fi 

prelude_name="$1"
name="$2"
all_cml="$3"

echo "open ${prelude_name}"


let_pat="^let ${name}[[:space:]]"

# Explanation:
# Suppose we're extracting 'ex1'
# 1) $let_pat selects 'let ex1 ' (the space is used to prevent selecting 'let ex11')
# 2) Address the start of the file up until the first matching 'let ex1'
# 2) Print {p} the first matching line, goto 'slice'. Otherwise drop lines {d}
# 3) At first matching line, read line, select up until next 'let'
# 4) Print {p}
"$SED" -n "
  0,/$let_pat/ {
    /$let_pat/ { p; b slice }
    d
  }
  :slice
  n
  /^let / q
  p 
" "$all_cml"
