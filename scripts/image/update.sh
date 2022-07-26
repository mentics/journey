#!/bin/bash
echo "#### Updating packages ####"
julia --threads=auto --project --load=scripts/image/update-pkg.jl
echo "#### Gathering trace data ####"
julia --trace-compile=C:/data/tmp/precomp-journey.jl --threads=12 --project --load=scripts/image/trace-all.jl
echo "#### Making new image ####"
julia --threads=auto --project --load=scripts/image/make-image.jl
echo "#### Checking image ####"
julia --sysimage=C:/data/tmp/sysimage-journey.dll --threads=auto --project --load=scripts/image/check-image.jl