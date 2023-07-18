module DataUtil
using Dates
using Parquet2, DataFrames, Impute
using Flux
using CollUtil
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

function make_data_under_wrong()
    fileinterp = joinpath(path_data(), "underinterp.parquet")
    if isfile(fileinterp)
        println("Loading interp parquet")
        pq = Parquet2.Dataset(fileinterp)
        # df = DataFrame(pq; copycols=false)
        df = DataFrame(pq; copycols=true)
    else
        df = get_data_under()
        tss = DateTime(2010,01,01):Minute(30):DateTime(2022,10,1)
        df = DataUtil.impute(df, tss, :quote_ts)

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

import DateUtil
function get_data_under()
    println("Loading under parquet")
    pq = Parquet2.Dataset(joinpath(path_data(), "under.parquet"))
    df = DataFrame(pq; copycols=true)
    filter!(r -> DateUtil.isBusDay(Date(r.quote_ts)), df)
    disallowmissing!(df, [:quote_ts, :under])
    replace!(ts -> floor(ts, Minute), df.quote_ts)
    filter!(r -> minute(r.quote_ts) == 0 || minute(r.quote_ts) == 30, df)

    # Convert to float so interpolate won't complain about can't round
    df[!,:under] = Float32.(df[!,:under])
    rename!(df, :quote_ts => :ts, :under => :x)
    sort!(df, :ts; rev=true)
    return df
end

function make_data_vix()
    tss = Date(2010,01,01):Day(1):Date(2022,9,30)
    dd = HistData.dataDaily(tss.start, tss.stop, "VIX")
    df = DataFrame((;ts=x.date, x=x.open) for x in dd)
    filter!(r -> DateUtil.isBusDay(Date(r.ts)), df)
    df = DataUtil.impute(df, tss, :ts)
    return df
end

function get_data_vix()
    tss = Date(2010,01,01):Day(1):Date(2022,9,30)
    dd = HistData.dataDaily(tss.start, tss.stop, "VIX")
    df = DataFrame((;ts=x.date, x=x.open) for x in dd)

    # TODO: why no under data for this date?
    filter!(r -> r.ts != Date("2022-05-11"), df)
    filter!(r -> r.ts != Date("2019-06-07"), df)
    filter!(r -> r.ts != Date("2019-05-24"), df)
    filter!(r -> r.ts != Date("2019-03-01"), df)
    filter!(r -> r.ts != Date("2017-09-27"), df)
    filter!(r -> r.ts != Date("2016-04-18"), df)
    filter!(r -> r.ts != Date("2014-08-04"), df)
    filter!(r -> r.ts != Date("2012-02-29"), df)
    filter!(r -> r.ts != Date("2011-08-22"), df)
    filter!(r -> r.ts != Date("2010-11-15"), df)

    sort!(df, :ts; rev=true)
    return df
end

function impute(df, step::Period, colname; imputer=Impute.locf)
    tss = df[!,colname]
    impute(df, tss[end]:step:tss[1], colname)
end
function impute(df, xs, colname; imputer=Impute.locf)
    dftimes = DataFrame([xs], [colname])
    joined = leftjoin(dftimes, df; on=colname)
    sort!(joined, [colname])
    dfint = imputer(joined)
    # dropmissing!(dfint) # Should just remove from beginning, maybe ending
    reverse!(dfint)
    # sort!(dfint, [colname]; rev=true)
    return dfint
end

using JLD2
function checkpoint_save(names, model, opt_state)
    path = joinpath(path_checkpoint(), "$(join(names, "-"))-$(round(Int, datetime2unix(now(UTC)))).jld2")
    println("Saving checkpoint to $(path)")
    jldsave(path, model_state=Flux.state(cpu(model)), opt_state=cpu(opt_state))
end

most_recent_file(path) = CollUtil.findMaxDom(mtime, readdir(path; join=true))

function checkpoint_load(model, fname = nothing)
    path = isnothing(fname) ? most_recent_file(path_checkpoint()) : joinpath(path_checkpoint(), fname)
    println("Loading checkpoint from $(path)")
    d = JLD2.load(path)
    Flux.loadmodel!(model, d["model_state"])
    opt_state = d["opt_state"]
    return (;model, opt_state)
end

calclossbase(lossfunc, batch; baseyhat=zeros(Float32, size(batch))) = lossfunc(batch, baseyhat) / size(batch)[end]

function windowextrema(x, winlen)
    res = Vector{Tuple{Int,eltype(x)}}()
    left = 1
    right = winlen
    # (mni, mn), (mxi, mx) = findextrema(x, left, right)
    # @show mni mn mxi mx
    # push!(res, (mni, mn))
    # push!(res, (mxi, mx))
    push!(res, (1, x[1]))
    push!(res, (length(x), x[end]))

    while right <= length(x)
        (mni, mn), (mxi, mx) = CollUtil.findextrema(x, left, right)
        # @show mni mn mxi mx
        if res[end][1] != mni && res[end-1][1] != mni
            push!(res, (mni, mn))
        end
        if res[end][1] != mxi && res[end-1][1] != mxi
            push!(res, (mxi, mx))
        end
        left += 1
        right += 1
    end
    sort!(res; by=(x -> x[1]))
    unique!(x -> x[1], res)
    return res
end

import Peaks
function windowlined(x)
    width = 1
    # we = windowextrema(x, winlen)
    # return interpolate(DataFrame(we), eachindex(x), Symbol(1))[!,2]
    # indsmin, valsmin = Peaks.findmaxima((-).(x), width)
    indsmin, valsmin = Peaks.findminima(x, width; strict=false)
    # valsmin = (-).(valsmin)
    indsmax, valsmax = Peaks.findmaxima(x, width; strict=false)
    inds = vcat(indsmin, indsmax)
    vals = vcat(valsmin, valsmax)
    perms = sortperm(inds)
    inds = inds[perms]
    vals = vals[perms]
    # TODO: uniquify?
    if inds[1] != 1
        insert!(inds, 1, 1)
        insert!(vals, 1, x[1])
    end
    lasti = lastindex(x)
    if inds[end] != lasti
        push!(inds, lasti)
        push!(vals, x[end])
    end
    return impute(DataFrame(;inds, vals), eachindex(x), :inds)[!,2]
end

# function winder!(x, len=1, iters=1)
#     for _ in 1:iters
#         for righti in (len+2):length(x)
#             lefti = righti - len - 1
#             left = x[lefti]
#             right = x[righti]
#             for i in (lefti + 1):(righti - 1)
#                 val = x[i]
#                 if sign(right - val) != sign(val - left)
#                     @goto breakout
#                 end
#             end
#             step = (right - left) / (righti - lefti)
#             for i in (lefti + 1):(righti - 1)
#                 x[i] = left + step * (i - lefti)
#             end
#             @label breakout
#         end
#     end
# end

function winder2!(x, lenmax=48, iters=4)
    for len in 1:lenmax
        for _ in 1:iters
            for righti in (len+2):length(x)
                lefti = righti - len - 1
                left = x[lefti]
                right = x[righti]
                for i in (lefti + 1):(righti - 1)
                    val = x[i]
                    if sign(right - val) != sign(val - left)
                        @goto breakout
                    end
                end
                step = (right - left) / (righti - lefti)
                for i in (lefti + 1):(righti - 1)
                    x[i] = left + step * (i - lefti)
                end
                @label breakout
                yield()
            end
        end
    end
end

function winder3(x, lenmax=48, iters=4)
    res = Vector{Union{eltype(x),Missing}}(undef, length(x))
    res[1] = x[1]
    res[2] = x[2]
    for len in 1:lenmax
        for righti in (len+2):length(x)
            lefti = righti - len - 1
            right = x[righti]
            (_, mn), (_, mx) = CollUtil.findextrema(x, lefti, righti-1)
            res[righti] = mn < right < mx ? missing : right
            yield()
        end
    end
    res[end] = x[end]
    return res
end

function winder4(dfx, lenmax=48)
    df = copy(dfx)
    todelete = Vector{Int}()
    for len in 1:lenmax
        for righti in (len+2):(size(df, 1) - 1)
            lefti = righti - len - 1
            # right = dfx.under[righti]
            # (_, mn), (_, mx) = CollUtil.findextrema(df.under, lefti, righti-1)
            # if mn < right < mx
            (mni, mn), (mxi, mx) = CollUtil.findextrema(df.under, lefti, righti)
            # if 323 in lefti:righti
            #     @show mni mn mxi mx
            # end
            if !(mni == righti || mxi == righti)
                # if 323 in lefti:righti
                #     println("Deleting $(righti) $(df[(righti-len-1):righti,:])")
                # end
                push!(todelete, righti)
            end
            yield()
        end
    end
    # global ktodelete = todelete
    sort!(todelete)
    unique!(todelete)
    delete!(df, todelete)
    return df
end

using DrawUtil, Impute
function runtest(dfunder, lenmax)
    df2 = winder4(dfunder, lenmax)
    draw(:lines, datetime2unix.(dfunder.quote_ts), dfunder.under) ; draw!(:lines, datetime2unix.(df2.quote_ts), df2.under);
    # dfunderint = interpolate(dfunder, Minute(30), "quote_ts")
    # df2int = interpolate(df2, Minute(30), "quote_ts")
    # draw(:lines, dfunderint.under) ; draw!(:lines, df2int.under);
end


function rt(dfunder, distmin)
    dfunderinterp = impute(dfunder, Minute(30), "quote_ts")
    dfmoves = dfunder[moves(dfunder.under, distmin),:]
    dfmovesinterp = impute(dfmoves, Minute(30), "quote_ts")
    draw(:lines, dfunderinterp.under)
    draw!(:lines, dfmovesinterp.under)
end

function moves(v, distmin)
    res = Vector{Int}()
    push!(res, 1)
    mn = v[1]
    mx = v[1]
    mni = 1
    mxi = 1
    lookformax = false
    i = 2
    while i <= lastindex(v)
        x = v[i]
        # println((;lookformax, mni, mn, mxi, mx, x))
        if x < mn
            mni = i
            mn = x
        end
        if x > mx
            mxi = i
            mx = x
        end
        if mx - mn >= distmin
            wasmax = mxi < mni
            ind = wasmax ? mxi : mni
            lookformax = !wasmax
            if ind == 1
            else
                push!(res, ind)
            end
            break
        end
        i += 1
    end

    while i <= lastindex(v)
        x = v[i]
        # println((;lookformax, mni, mn, mxi, mx, x))
        if !lookformax
            if (x - mn) >= distmin
                lookformax = !lookformax
                mxi = i
                mx = x
                push!(res, mni)
            else
                if x < mn
                    mni = i
                    mn = x
                end
            end
        else
            if (mx - x) >= distmin
                lookformax = !lookformax
                mni = i
                mn = x
                push!(res, mxi)
            else
                if x > mx
                    mxi = i
                    mx = x
                end
            end
        end
        i += 1
    end

    res[end] == length(v) || push!(res, length(v))
    return res
end

# struct MoveIter{D,T}
#     data::D
#     distmin::T
# end
# struct MoveIterState{IT}
#     iter::IT
#     lastwasmax::Bool
# end

# # function Base.iterate(iter::MoveIter{D,T})
# #     iter = Iterators.Stateful(enumerate(iter.data))
# #     lasti, val = popfirst!(iter)
# #     return ((;ind, val, ), MoveIterState(iter, val, false))
# # end

# function Base.iterate(miter::MoveIter{D,T})
#     !isempty(miter.data) || return nothing
#     x0, iter = peel(miter.data)
#     mn = x0
#     mx = x0
#     mni = 1
#     mxi = 1
#     i = 1
#     while !isempty(iter)
#         x, iter = peel(iter)
#         if x < mn
#             mni = i
#             mn = x
#         end
#         if x > mx
#             mxi = i
#             mx = x
#         end
#         if mx - mn > miter.distmin
#             wasmax = mxi < mni
#             ind = wasmax ? mxi : mni
#             iternext = Iterators.Stateful(enumerate(miter.data))
#             collect(Iterators.take(iternext, ind-1)) # TODO: ugly
#             return ((;ind=1, val=x0, wasmax), MoveIterState(iternext, wasmax))
#         end
#         i += 1
#     end
#     # return ((;ind=1, val=x0), MoveIterState(iter, wasmax))
#     return iterate(x0, miter.data[end]) # TODO: handle this case
# end

# function Base.iterate(iter::MoveIter, state::MoveIterState)
#     if state.wasmax
#         return findnextmin(state.iter)
#     else
#         return findnextmax(state.iter)
#     end
# end

# function findnextmin(iter)
#     i0, x0 = popfirst!(iter)
#     mn = x0
#     mx = x0
#     mni = i0
#     mxi = i0
#     i = i0
#     while !isempty(iter)
#         i, x = popfirst!(iter)
#         if x < mn
#             mni = i
#             mn = x
#         end
#         if x > mx
#             mxi = i
#             mx = x
#         end
#         if mx - mn > miter.distmin
#             wasmax = mxi < mni
#             ind = wasmax ? mxi : mni
#             iternext = Iterators.Stateful(enumerate(miter.data))
#             collect(Iterators.take(iternext, ind-1)) # TODO: ugly
#             return ((;ind=1, val=x0, wasmax), MoveIterState(iternext, wasmax))
#         end
#     end
# end

#     return (;ind=i, val=x0)
# end

# function simplify(df, starti, len)
#     i = starti
#     while
#         i += 1
#     end
# end

end
