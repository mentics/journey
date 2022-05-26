using LoggingExtras, Dates
const date_format = "yyyy-mm-ddTHH:MM:SS"
timestamp_logger(logger) = TransformerLogger(logger) do log
  merge(log, (; message = "[$(Dates.format(now(), date_format))] $(log.message)"))
end
ConsoleLogger(stdout, Logging.Info; show_limited=false, right_justify=80) |> timestamp_logger |> global_logger

isdefined(Main, :CODE_DIR) || (CODE_DIR = ".")

for (root, dirs, files) in walkdir(joinpath(CODE_DIR, "modules"))
    occursin("ignore", root) && continue
    if !isempty(filter(x->endswith(x, ".jl"), files))
        push!(LOAD_PATH, root)
    end
end

sobj = nothing
function saveObj(x)
    global sobj = x
    return "[Set obj type $(typeof(x))]"
end
