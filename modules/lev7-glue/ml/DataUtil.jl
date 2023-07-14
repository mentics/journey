module DataUtil
using Dates
using Parquet2, DataFrames, Impute
using HistData

const CONFIG = Ref(Dict{Symbol,String}())
CONFIG_WIN = Dict(
  :PATH_CHECKPOINTS => "D:/data/ml/journey/models",
  :PATH_DATA => "W:\\home\\jshellman\\ray\\alone",
)
CONFIG_LINUX = Dict(
  :PATH_CHECKPOINTS => "/home/jshellman/data/ml/journey/checkpoints",
  :PATH_DATA => "/home/jshellman/data/ml/input",
)
if Sys.iswindows()
  CONFIG[] = CONFIG_WIN
else
  CONFIG[] = CONFIG_LINUX
end

path_checkpoint() = CONFIG[][:PATH_CHECKPOINTS]
path_data() = CONFIG[][:PATH_DATA]

function make_data_under()
    fileinterp = joinpath(path_data(), "underinterp.parquet")
    if isfile(fileinterp)
        println("Loading interp parquet")
        pq = Parquet2.Dataset(fileinterp)
        # df = DataFrame(pq; copycols=false)
        df = DataFrame(pq; copycols=true)
    else
        tss = DateTime(2010,01,01):Minute(30):DateTime(2022,10,1)
        println("Loading under parquet and interpolating")
        pq = Parquet2.Dataset(joinpath(path_data(), "under.parquet"))
        # df = DataFrame(pq; copycols=false)
        df = DataFrame(pq; copycols=true)
        # disallowmissing!(df, [:quote_ts, :under])

        # Convert to float so interpolate won't complain about can't round
        df[!,:under] = Float32.(df[!,:under])
        df = DataUtil.interpolate(df, tss, :quote_ts)

        dfint[!,:under] = round.(Int, dfint[!,:under])
        for (row1, row2) in partition(eachrow(dfint), 2, 1)
            if (row2.quote_ts - row1.quote_ts) != Minute(30)
                @show row1
                @show row2
                error("data missing")
            end
            if abs(row2.under - row1.under) > 12000
                @show row1
                @show row2
                error("found thing")
            end
        end
        return dfint

        Parquet2.writefile(fileinterp, df)
    end

    rename!(df, :quote_ts => :ts, :under => :x)
end

function make_data_vix()
    tss = Date(2010,01,01):Day(1):Date(2022,9,30)
    dd = HistData.dataDaily(tss.start, tss.stop, "VIX")
    df = DataFrame((;ts=x.date, x=x.open) for x in dd)
    df = DataUtil.interpolate(df, tss, :ts)
    return df
end

function interpolate(df, xs, colname)
    dftimes = DataFrame([xs], [colname])
    joined = leftjoin(dftimes, df; on=colname)
    sort!(joined, [colname])
    dfint = Impute.interp(joined)
    dropmissing!(dfint) # Should just remove from beginning, maybe ending
    return dfint
end

using JLD2
function checkpoint_save(names, model, opt_state)
    path = joinpath(path_checkpoint(), "$(join(names, "-"))-$(round(Int, datetime2unix(now(UTC)))).jld2")
    println("Saving checkpoint to $(path)")
    jldsave(path, model_state=Flux.state(cpu(model)), opt_state=cpu(opt_state))
end

most_recent_file(path) = CollUtil.findMaxDom(mtime, readdir(path))

function checkpoint_load(fname = nothing)
    path = isnothing(fname) ? most_recent_file(path_checkpoint()) : joinpath(path_checkpoint(), fname)
    println("Loading checkpoint from $(path)")
    model = make_model(hypers())
    d = JLD2.load(path)
    Flux.loadmodel!(model, d["model_state"])
    global kmodelgpu = gpu(model)
    opt_state = d["opt_state"]
    global kopt_state = gpu(opt_state)
    return (;model, opt_state)
end

calclossbase(lossfunc, batch; baseyhat=zeros(Float32, size(batch))) = lossfunc(batch, baseyhat) / size(batch)[end]

end