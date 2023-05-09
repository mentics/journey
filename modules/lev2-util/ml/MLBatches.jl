module MLBatches
import MaxLFSR

# const CacheSeq = Ref{AbstractArray}(nothing)
# const Cache = Ref{AbstractArray}(nothing)

#=
Returns a Vector of Arrays (or whatever is returned by modify_batch) with size = (size(data)..., batch_len)
=#
function make_batches(
            data::Vector{AbstractArray{T,N}},
            batch_len::N2;
            update=false,
            modify_sample=identity,
            modify_batch=identity,
            seed=1)::Vector where {T,N,N2}
    data_len = length(data)
    batch_count = ceil(N2, data_len / batch_len)
    println("Creating $(batch_count) batches")
    lfsr = MaxLFSR.LFSR(seq_count; seed)

    res = []
    ind = seed
    for _ in 1:(batch_count-1)
        batch = stack(begin
            sample = data[ind]
            ind, _ = iterate(lfsr, ind)
            modify_sample(seq)
        end for _ in 1:batch_len)
        push!(res, modify_batch(batch))
    end

    # there might be a smaller last batch
    if !isnothing(iterate(lfsr, ind))
        push!(res, modify_batch(stack(modify_seq(data[ind]) for ind in Iterators.rest(lfsr, ind))))
    end

    # @assert size(batches[1]) == (size(data)..., batch_len)
    return res
end

#=
Returns a Vector of Arrays (or whatever is returned by modify_batch) with size = (size(data)..., batch_len)
=#
function make_batches_seq(
            data::AbstractArray{T,N},
            seq_len::N2,
            batch_len::N2;
            update=false,
            modify_seq=identity,
            modify_batch=identity,
            seed=1)::Vector where {T,N,N2}
    # if !update && !isnothing(CacheSeq[])
    #     println("Using cached batches: $(batch_size(BatchesCache.train.x)) training batches, $(batch_size(BatchesCache.validation.x)) validation batches")
    #     return BatchesCache
    # end
    data_len = size(data, ndims(data)) # time is always the last dimension when unbatched
    seq_count = data_len - seq_len
    batch_count = ceil(N2, seq_count / batch_len)
    println("Creating $(batch_count) batches")
    lfsr = MaxLFSR.LFSR(seq_count; seed)

    res = []
    ind = seed
    for _ in 1:(batch_count-1)
        batch = stack(begin
            seq = input_subseq(data, ind:(ind + seq_len - 1))
            ind, _ = iterate(lfsr, ind)
            modify_seq(seq)
        end for _ in 1:batch_len)
        push!(res, modify_batch(batch))
    end

    # there might be a smaller last batch
    if !isnothing(iterate(lfsr, ind))
        push!(res, modify_batch(stack(modify_seq(input_subseq(data, ind:(ind + seq_len - 1))) for ind in Iterators.rest(lfsr, ind))))
    end

    # @assert size(batches[1]) == (size(data)..., batch_len)
    return res
end

function counts(batch_count::T, hold_ratio::Real)::NTuple{2,T} where {T<:Integer}
    @show batch_count hold_ratio
    count2 = max(one(T), ceil(T, hold_ratio * batch_count))
    count1 = batch_count - count2
    return (count1, count2)
end

function split_batches(data::Vector{D}, hold_ratio::Real)::NamedTuple{(:train, :validation), NTuple{2,Vector{D}}} where {D}
    training_batch_count, validation_batch_count = counts(length(data), hold_ratio)
    if validation_batch_count <= 0
        error("Insufficient data $(length(data)) for seq_size $(size(data[1], ndims(data[1])-1)) and batch len $(size(data[1], ndims(data[1])))")
    end
    println("Split batches into $(training_batch_count) training and $(validation_batch_count) validation")
    return (; train = (@view data[1:training_batch_count]), validation = (@view data[training_batch_count+1:end]))
end

# function batches(cfg, count, lfsr, ind; modify_seq=identity, modify_batch=identity)
#     seqlen = seq_len_train(cfg)
#     batchlen = cfg.batch_len
#     # x = Array{Float32, ndims(dataTokens)+1}[]
#     x = []
#     for _ in 1:count
#         batch = stack(begin
#             seq = input_subseq(dataTokens, ind:(ind + seqlen - 1))
#             ind, _ = iterate(lfsr, ind)
#             attachPositional(seq, pos_dim(seq))
#         end for _ in 1:batchlen)
#         push!(x, ins_and_outs(cfg, batch))
#         @assert size(batch) == (cfg.channel_width, seqlen, batchlen) "$(size(batch)) == $((cfg.channel_width, seqlen, batchlen))"
#     end
#     return x, ind
# end

input_subseq(seq, inds) = selectdim(seq, ndims(seq), inds)

end