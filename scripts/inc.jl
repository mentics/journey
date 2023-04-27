if !(".\\modules\\lev1-type\\base" in LOAD_PATH)
    isdefined(Main, :CODE_DIR) || (CODE_DIR = ".")
    for (root, dirs, files) in walkdir(joinpath(CODE_DIR, "modules"))
        occursin("ignore", root) && continue
        if !isempty(filter(x->endswith(x, ".jl"), files))
            push!(LOAD_PATH, root)
        end
    end
end