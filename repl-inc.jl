using LoggingExtras, Dates
const date_format = "yyyy-mm-ddTHH:MM:SS"
timestamp_logger(logger) = TransformerLogger(logger) do log
  merge(log, (; message = "[$(Dates.format(now(), date_format))] $(log.message)"))
end
ConsoleLogger(stdout, Logging.Info; show_limited=false, right_justify=80) |> timestamp_logger |> global_logger

for (root, dirs, files) in walkdir("modules")
    occursin("ignore", root) && continue
    if !isempty(filter(x->endswith(x, ".jl"), files))
        push!(LOAD_PATH, root)
    end
end

using CheckUtil