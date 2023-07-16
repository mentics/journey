module DataFiles
using Dates
using DelimitedFiles, DataFrames
using Arrow

#region Public
callstable(y, m) = Arrow.Table(pathcalls(y, m))
putstable(y, m) = Arrow.Table(pathcalls(y, m))
tsstable() = Arrow.Table(pathtss())

function updatetss()
    tss = Vector{DateTime}()
    for path in readdir(pathchains(); join=true)
        append!(tss, unique(Arrow.Table(path).ts))
        unique!(tss)
    end
    sort!(tss)
    Arrow.write(pathtss(), tss)
    return tss
end

function procincoming(y, m)
    df = dfincoming(y, m)
    transformraw(df)
    dfcalls = extractdf(df, "c_")
    dfputs = extractdf(df, "p_")
    Arrow.write(pathcalls(y, m), dfcalls)
    Arrow.write(pathputs(y, m), dfputs)
end
#endregion

const DF_FILE = DateFormat("yyyymm")

datestr(y, m) = Dates.format(Date(y, m, 1), DF_FILE)
basepath() = joinpath("C:\\", "data", "db")
baseincoming() = joinpath(basepath(), "incoming")
pathincoming(y, m) = joinpath(baseincoming(), string(y), "spy_30x_$(datestr(y, m)).txt")

# patharrowtest() = joinpath(basepath(), "arrow")
pathtss() = joinpath(basepath(), "market", "spy", "tss.arrow")
pathchains() = joinpath(basepath(), "market", "spy", "chains")
pathcalls(y, m) = joinpath(pathchains(), "calls-$(datestr(y, m)).arrow")
pathputs(y, m) = joinpath(pathchains(), "puts-$(datestr(y, m)).arrow")

#region Incoming
# function update(df, path)
#     if isfile(path)
#         ts1 = df.quote_unixtime[1]
#         a = Arrow.Table(path)
#         if ts1 in a.quote_unixtime
#             println("data already in $(path)")
#         else
#             Arrow.append(path, df)
#         end
#     else
#         open(Arrow.Writer, path) do writer
#             Arrow.write(writer, df)
#         end
#     end
# end

# "quote_unixtime, underlying_last, expire_unix, c_delta, c_gamma, c_vega, c_theta, c_rho, c_iv, c_volume, c_last, c_size, c_bid, c_ask, strike, p_bid, p_ask"
COLS_FINAL = [:ts, :under, :expiration, :strike, :bid, :ask, :last, :volume, :size, :delta, :gamma, :vega, :theta, :rho, :iv]
COLS_COMMON = Dict(:quote_unixtime => :ts, :underlying_last => :under, :expire_unix => :expiration, :strike => :strike)
function extractdf(df, prefix)
    colnames = vcat(collect(keys(COLS_COMMON)), Symbol.(filter(x -> startswith(x, prefix), names(df))))
    # df = filter!(r -> !ismissing(r[prefix * "bid"]) && !ismissing(r[prefix * "ask"]), select(df, colnames))
    df = select(df, colnames)
    rename!(df) do colname
        startswith(colname, prefix) ? colname[3:end] : COLS_COMMON[Symbol(colname)]
    end
    dropmissing!(df, [:bid, :ask]; disallowmissing=true)
    return select!(df, COLS_FINAL)
end

function dfincoming(y, m)
    path = pathincoming(y, m)
    data, header = readdlm(path, ','; header=true)
    return DataFrame(data, vec(header))
end

fixcolname(col) = occursin('[', col) ? lowercase(strip(col)[2:end-1]) : col

tofloat(x::Float64) = Float32(x)
tofloat(x::AbstractString) = isempty(strip(x)) ? missing : tofloat_(x)
tofloat_(x) = parse(Float32, x)
topents(x::Float64) = round(Int32, x * 1000)
topents(x::AbstractString) = isempty(strip(x)) ? missing : topents_(x)
topents_(x) = topents(Float64(x))
totimestamp(x::Int) = unix2datetime(x)

# [QUOTE_UNIXTIME], [QUOTE_READTIME], [QUOTE_DATE], [QUOTE_TIME_HOURS], [UNDERLYING_LAST], [EXPIRE_DATE], [EXPIRE_UNIX], [DTE], [C_DELTA], [C_GAMMA], [C_VEGA], [C_THETA], [C_RHO], [C_IV], [C_VOLUME], [C_LAST], [C_SIZE], [C_BID], [C_ASK], [STRIKE], [P_BID], [P_ASK], [P_SIZE], [P_LAST], [P_DELTA], [P_GAMMA], [P_VEGA], [P_THETA], [P_RHO], [P_IV], [P_VOLUME], [STRIKE_DISTANCE], [STRIKE_DISTANCE_PCT]
COLS_DROP = ["quote_readtime", "quote_date", "quote_time_hours", "expire_date", "dte", "strike_distance", "strike_distance_pct"]
COLS_PROC = Dict(
    "quote_unixtime" => totimestamp,
    "expire_unix" => totimestamp,
    "underlying_last" => topents,
    "strike" => topents
)

function transformraw(df)
    rename!(fixcolname, df)
    select!(df, Not(Symbol.(COLS_DROP)))
    for colname in names(df)
        if haskey(COLS_PROC, colname)
            df[!,colname] = (COLS_PROC[colname]).(df[!,colname])
        elseif occursin("size", colname)
            # do nothing
            df[!,colname] = strip.(df[!,colname])
        elseif occursin("bid", colname) || occursin("ask", colname)
            df[!,colname] = topents.(df[!,colname])
        elseif occursin("c_", colname) || occursin("p_", colname)
            df[!,colname] = tofloat.(df[!,colname])
        else
            error("Unexpected col: $(colname)")
        end
    end
end
#endregion

end