module ModuleUtil

export findCycles

function findCycles()
    # isdefined(@__MODULE__, :MAPPING) ||
    makeMapping()
    return findCycle(MAPPING, "root", keys(MAPPING), 1)
end

function findCycle(mapping, from, working, len)
    len < 20 || return true
    for item in working
        haskey(mapping, item) || continue
        if findCycle(mapping, item, mapping[item], len+1)
            println("Found cycle: ", from, ' ', item, ' ', len)
            return true
        end
    end
    return false
end

function makeMapping()
    mods = String[]
    usings = String[]
    mapping = Dict()
    for (root, dirs, files) in walkdir("modules")
        occursin("ignore", root) && continue
        if !isempty(filter(x->endswith(x, ".jl"), files))
            foreach(files) do fil
                uses = findUsings(root, fil)
                append!(usings, uses)
                name, ext = splitext(fil)
                push!(mods, name)
                mapping[name] = uses
            end
        end
    end
    global MAPPING = mapping
    # unique!(usings)
    # ignore = ["Base.Threads"]
    # filter!(us -> length(us) > 0 && !(us in mods) && !(us in ignore), usings)
end

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

end