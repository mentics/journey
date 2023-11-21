#!/bin/bash
echo "|$1|"
if [ -z "${1}" ]; then
  DISPLAY=:0 xvfb-run -s '-screen 0 1024x768x24' -- julia --threads auto --project
else
  DISPLAY=:0 xvfb-run -s '-screen 0 1024x768x24' -- julia --threads auto --project --load scripts/repl-${1}.jl
fi
