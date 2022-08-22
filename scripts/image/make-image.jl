function findUsings(base, fil)
    res = String[]
    for line in eachline(joinpath(base, fil))
        spl = split(line, '#')
        if length(spl) > 1
            line = spl[1]
        end
        if startswith(line, "using")
            append!(res, split(line, r"[,\s]+")[2:end])
        elseif startswith(line, "import")
            if !occursin(":", line)
                append!(res, split(line, r"[,\s]+")[2:end])
            else
                m = match(r"\s(.+?)(?:\:|$)", line)
                if isnothing(m)
                    error(line)
                end
                push!(res, m[1])
            end
        end
    end
    return res
end

mods = String[]
usings = String[]
for (root, dirs, files) in walkdir("modules")
    occursin("ignore", root) && continue
    if !isempty(filter(x->endswith(x, ".jl"), files))
        foreach(files) do fil
            append!(usings, findUsings(root, fil))
            name, ext = splitext(fil)
            push!(mods, name)
        end
    end
end
unique!(usings)
ignore = ["Base.Threads"]
filter!(us -> length(us) > 0 && !(us in mods) && !(us in ignore), usings)

precomp = "C:/data/tmp/precomp-journey.jl"
precompFiltered = "C:/data/tmp/precomp-filtered.jl"
open(precompFiltered, "w") do out
    for line in eachline(precomp)
        for mod in mods
            if occursin(mod*'.', line)
                @goto SKIP
            end
        end
        println(out, line)
        @label SKIP
    end
end

using PackageCompiler
fn = "sysimage-journey.dll"
BackupImageDir = mkpath("C:/data/tmp/backup")
NewImagePath = "C:/data/tmp/new/$(fn)"
UseImagePath = "C:/data/tmp/$(fn)"
PackageCompiler.create_sysimage(usings; sysimage_path=NewImagePath, precompile_statements_file=precompFiltered)

# using Filesystem
temp = mktempdir(BackupImageDir; prefix="img", cleanup=false)
!isfile(UseImagePath) || mv(UseImagePath, joinpath(temp, fn))
mv(NewImagePath, UseImagePath)

exit()