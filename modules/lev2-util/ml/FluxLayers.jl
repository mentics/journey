module FluxLayers
import Flux:Flux,Parallel,Dense
import Transformers.Basic:Embed,PositionEmbedding
# import MLUtil:N

export fl
const fl = @__MODULE__

cat1(xs...) = cat(xs...; dims=1)

struct EncoderLayer{PE<:PositionEmbedding,E<:Embed,D<:Dense}
    embed::E
    posEmbed::PE
    den::D
end
EncoderLayer(embedSize, vocabSize, width, len, outSize) = EncoderLayer(
    Embed(embedSize, vocabSize),
    PositionEmbedding(embedSize, len),
    Dense(width * embedSize => outSize, Flux.relu)
)
Flux.@functor EncoderLayer
Flux.trainable(layer::EncoderLayer) = (layer.embed, layer.den)
function (m::EncoderLayer)(x::AbstractArray)
    # TODO: store embedSize
    s1 = m.embed(x, inv(sqrt(size(m.embed.embedding)[1])))
    s2 = s1 .+ m.posEmbed(s1)
    sz = size(s2)
    # println("enclay sz: ", sz)
    s3 = reshape(s2, (sz[1]*sz[2], sz[3:end]...))
    # println("enclay resh sz: ", size(s3))
    s4 = m.den(s3)
    return s4
end

struct EmbedMulti{E,N}
    embeds::NTuple{N,E}
end
EmbedMulti(xs::Pair{Int}...) = EmbedMulti(Tuple(Flux.Embedding(first(x), last(x)) for x in xs))
Flux.@functor EmbedMulti
function (m::EmbedMulti)(x::AbstractArray)
    @assert size(x, 1) == length(m.embeds)
    reduce(cat1, m.embeds[i](selectdim(x, 1, i)) for i in eachindex(m.embeds))
end

struct SeqCombLayer{INT,T}
    plus::INT
    layer::T
    function SeqCombLayer(pair::Pair; σ=identity)
        plus = last(first(pair))-1
        den = Dense((*)(first(pair)...) => last(pair), σ)
        new{typeof(plus),typeof(den)}(plus, den)
    end
    SeqCombLayer(plus::Real, layer; σ=identity) = new{typeof(plus),typeof(layer)}(plus, layer)
end
Flux.@functor SeqCombLayer
function (m::SeqCombLayer)(x::AbstractArray)
    dim = ndims(x)-1
    sz = size(x)
    lenOut = length(m.layer.bias)
    szOut = (lenOut, sz[2:(dim-1)]..., 1, sz[dim+1:end]...)
    mapreduce(
        i -> reshape(m.layer(Flux.flatten(selectdim(x, dim, i:i+m.plus))), szOut),
        (x1, x2) -> cat(x1, x2; dims=dim),
        1:(size(x, dim)-m.plus)
    )
end

    # reduce(
    #     (x1, x2) -> cat(x1, x2; dims=dim),
    #     reshape(m.layer(Flux.flatten(selectdim(x, dim, i:i+m.plus))), szOut) for i in 1:(size(x, dim)-m.plus)
    # )


#=
layer = fl.SeqCombLayer((8, 5) => 7)
out = layer(rand(8, 100, 12))
@assert size(out) == (7, 96, 12)

using Flux, CUDA
CUDA.allowscalar(false)
den = Flux.Dense(30 => 4) |> gpu
x = [i + j / 10 for i in 1:10, j in 1:20, b in 1:5] |> gpu
x2 = Flux.flatten(selectdim(x, 2, 1:3))
den(x2)
=#

# struct CombLayer{INT,T}
#     dim::INT
#     plus::INT
#     layer::T
# end
# CombLayer(dim, pair; σ=identity) = CombLayer(dim, last(first(pair))-1, Dense((*)(first(pair)...) => last(pair), σ))
# Flux.@functor CombLayer
# function (m::CombLayer)(x::AbstractArray)
#     sz = size(x)
#     lenOut = length(m.layer.bias)
#     @show sz  (sz[1:(m.dim-1)]..., 1, sz[m.dim+1:end]...)
#     reduce(
#         (x1, x2) -> cat(x1, x2; dims=m.dim),
#         reshape(m.layer(Flux.flatten(selectdim(x, m.dim, i:i+m.plus))), (lenOut, sz[1:(m.dim-1)]..., sz[m.dim+1:end]...)) for i in 1:(size(x, m.dim)-m.plus)
#     )
# end

# struct SeqEncodeLayer{T,P}
#     encoders::T
#     inputs::P
# end
# SELayer(encIns::Tuple...) = SeqEncodeLayer(first.(encIns), last.(encIns))
# Flux.@functor SeqEncodeLayer
# (m::SeqEncodeLayer)(x::AbstractArray) = mapreduce((enc, inds) -> enc(x[inds,:,:]), vcat, m.encoders, m.inputs)
# Flux.trainable(sel::SeqEncodeLayer) = (sel.encoders,)

# struct RepeatLayer{INT,T}
#     dim::INT
#     layer::T
# end
# Flux.@functor RepeatLayer
# (m::RepeatLayer)(x::AbstractArray) = reduce((x1, x2) -> cat(x1, x2; dims=m.dim), m.layer(selectdim(x, m.dim, i)) for i in axes(x, m.dim))

end