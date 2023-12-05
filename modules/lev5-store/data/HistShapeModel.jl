module HistShapeModel
using DataRead

params_data() = Dict{Symbol,Any}(
)

#=
make_input acquires all the data it needs from wherever, creates a single dataframe representing the input to the model,
and saves that in the input area.

Format for input files is an Arrow table with columns:
    obs key: unique identifier representing the observation (eg. timestamp)
    x: the datastructure for the observation that will be passed into the model for training and inference.
       Typically a vector of predictors, or a tuple of (predictors, meta, mask)
    y: the datastructure that is the expected output of the model for this observation
       Typically a vector of outputs, or a single value
=#
TODO: What's the typical file format for other frameworks for their input data?

function make_input(params)
    weeks_count = params.data_weeks_count
    df_prices = DataRead.get_prices()
    df_vix = DataRead.get_vix()

    under = dat.ts_allperiods_df()
    unders = replace(under.under, missing => 0f0)
    # res = Dict{DateTime,DataMetaMask2}()
    # sizehint!(res, 40000)
    res = DataFrame(ts = DateTime[], v = Vector{Float32}[], meta=Vector{Float32}[], mask=BitVector[])

    len = DateUtil.TIMES_PER_WEEK * weeks_count
    skip_back_count = len + 1 # +1 for the current ts
    # tss = under.ts[skip_back_count:end]
    inds = skip_back_count:length(under.ts)
    # prev_ts = DateTime(0)
    # ex = WorkStealingEx()
    # @floop ex for ind in inds
    # @floop for ind in inds
    for ind in inds
        cur_under = unders[ind]
        if iszero(cur_under)
            # println("skipping due to 0 cur $(ind)")
            continue
        end
        # !iszero(cur_under) || continue
        cur_ts = under.ts[ind]
        # @assert cur_ts > prev_ts

        include_ind = ind - 1
        if iszero(unders[include_ind])
            # TODO: maybe not skip these? these will skip the first ts after holidays
            # println("skipping due to 0 include_ind $(include_ind)")
            continue
        end
        include_ts = under.ts[include_ind]
        from_ts = DateUtil.week_first_ts(include_ts - Week(weeks_count - 1))
        from_ind = searchsortedfirst(under.ts, from_ts)
        # to_ts = DateUtil.week_last_ts(include_ts)
        # to_ind = searchsortedfirst(under.ts, to_ts)
        include_inds = from_ind:include_ind

        # weeks_tss = DateUtil.get_weeks_tss(ts, weeks_count)
        # inds = inds_for_sorted(under.ts, weeks_tss[1], weeks_tss[end])
        # include_ts = DateUtil.prev_weekday_ts(ts)
        # @assert under.ts[inds] == weeks_tss
        @assert length(include_inds) <= len

        vw = @view unders[include_inds]
        !too_many_zeros(vw) || continue

        v = fill(0f0, len)
        v[axes(vw, 1)] .= vw
        mask = (!iszero).(v)
        @assert typeof(mask) == BitVector
        v .= cur_under ./ v .* mask

        μ, σ = mean_and_std(filter(!iszero, v))
        @assert -2f0 < μ < 2f0 "-2f0 < μ ($(μ)) < 2f0"
        @assert 0.001f0 < σ < 1f0 "0.01f0 < σ ($(σ)) < 1f0"
        zscore!(v, μ, σ)
        v .*= mask # restore 0's for missing

        @assert iszero(@view v[(length(include_inds) + 1):end])
        @assert findlast(!iszero, v) > (len - DateUtil.TIMES_PER_WEEK) # shouldn't have a full week of zeros trailing

        # res[cur_ts] = DataMetaMask2(v, [μ, σ], mask)
        push!(res, (;ts = cur_ts, v, meta=[μ, σ], mask))

        # p = cur_ts => DataMetaMask2(v, [μ, σ], mask)
        # @reduce(res = vcat(Pair{DateTime,DataMetaMask2}[], p))


        # @reduce(res = push!(Dict{DateTime,DataMetaMask2}(), p))
        # @reduce(res = push!(Dict{DateTime,DataMetaMask2}(), cur_ts => DataMetaMask2(v, [μ, σ], mask)))
        # @reduce() do (res = Dict{DateTime,DataMetaMask2}(); p)
        #     # res[cur_ts] = DataMetaMask2(v, [μ, σ], mask)
        #     res[p.first] = p.second
        #     # push!(res, p)
        # end

        # prev_ts = cur_ts
    end

    # return Dict(res)
    return res
end

end