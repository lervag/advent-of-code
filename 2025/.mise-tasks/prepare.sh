#!/usr/bin/env bash

file="inputs/day$(date +%d).txt"
if [ ! -f "$file" ]; then
  elf input > "$file"
else
  echo "Input file already exists!"
fi
