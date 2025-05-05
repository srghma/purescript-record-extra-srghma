#!/usr/bin/env bash

# File: regen-index.sh

set -euo pipefail

MODULE_NAME="Record.ExtraSrghma"
SRC_DIR="./src/Record/ExtraSrghma"
OUTPUT_FILE="./src/Record/ExtraSrghma.purs"

echo "--- DONT EDIT, this file was gen by ./regen-index.sh" > "$OUTPUT_FILE"
echo "module $MODULE_NAME" >> "$OUTPUT_FILE"
echo "  ( module Export" >> "$OUTPUT_FILE"
echo "  ) where" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

find "$SRC_DIR" -type f -name "*.purs" | while read -r file; do
  # Skip the index file itself if re-run in-place
  [[ "$file" == "$OUTPUT_FILE" ]] && continue

  modpath="${file#./src/}"
  modpath="${modpath%.purs}"
  modulename="${modpath//\//.}"
  echo "import $modulename as Export" >> "$OUTPUT_FILE"
done
