prefiles = readdir("C:/data/tmp/precompile"; join=true)
for f in prefiles
    include(f)
end