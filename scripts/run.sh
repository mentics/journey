#!/bin/sh
julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=scripts/repl-sched.jl

# julia --threads=auto --project --load=scripts/repl-base.jl