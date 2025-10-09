#!/usr/bin/env bash 
set -euo pipefail 

: "${SED:=sed}"

# Usage: scripts/extract_programs.sh INPUT_TEX OUTPUT_CML 
# Extracts all checked program blocks from INPUT_TEX

if [[ $# -ne 2 ]]; then 
  echo "usage: $0 INPUT_TEX OUTPUT_CML" >&2 
  exit 1 
fi 

input_tex="$1"
output_cml="$2"

extract_lang() {
  local lang="$1"
  local src="$2"

  # Explanation: 
  # - keep only \begin{program}[$gate] ... \end{program}
  # - drop the begin/end lines 
  # - map $langflags <digits> -> CML (*=<digits>=*)
  # - strip display math $....$, inline math $..$, °..° comments
  # - turn x_1 -> x1
  # - strip any non-'let' lines
  cat "$src" \
  | "$SED" -n -e '/\\begin{program}[[].*check'"$lang"'.*[]]/,/\\end{program}/p' \
  | "$SED" -e '/\\begin{program}/d' -e '/\\end{program}/d' \
  | "$SED" -e 's/.\\'"$lang"'flags *\([0-9]\)[0-9]. */  (*=\1=*)/' \
  | "$SED" -e 's/[$]$//' \
           -e 's/^[$]//' \
           -e 's/°[^°]*°//' \
           -e 's/[$][^$]*[$]//' \
  | "$SED" -e 's/_\([0-9]\)/\1/g' \
  | "$SED" -n -e '/^let /p'
}

tmp="$(mktemp)"; trap 'rm -f "$tmp"' EXIT 

# Extract ocaml and polyml examples to $tmp
extract_lang ocaml  "$input_tex" >> "$tmp"
extract_lang polyml "$input_tex" >> "$tmp"

# Sort examples
sort -V "$tmp" > "$output_cml"
