#!/usr/bin/env bash 
set -euo pipefail 

: "${SED:=sed}"

# Explanation: 
# 1) [ e : τ ] -> object method f : τ = e end
# 2) < e > -> e#f
"$SED" \
  -e 's/\[[[:space:]]*\([^][[:space:]][^][]*\)[[:space:]]*:[[:space:]]*\([^][][^]]*\)[[:space:]]*\]/object method f : \2 = \1 end/g' \
  -e 's/<[[:space:]]*\([^<>]*\)[[:space:]]*>/\1#f/g' \
  "$1"

