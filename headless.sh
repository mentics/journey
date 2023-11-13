#!/bin/bash
DISPLAY=:0 xvfb-run -s '-screen 0 1024x768x24' -- julia --threads auto --project --load scripts/repl-${1}.jl
