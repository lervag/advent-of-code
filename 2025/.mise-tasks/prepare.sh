#!/usr/bin/env bash

file="inputs/day$(date +%d).txt"
if [ ! -f "$file" ]; then
  mkdir -p inputs
  elf input > "$file"
else
  echo "Input file already exists!"
fi
