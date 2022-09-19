module FluxLayers
import Flux:Flux,Parallel,Dense
import Transformers.Basic:Embed,PositionEmbedding

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

# struct SeqEncodeLayer{T,P}
#     encoders::T
#     inputs::P
# end
# SELayer(encIns::Tuple...) = SeqEncodeLayer(first.(encIns), last.(encIns))
# Flux.@functor SeqEncodeLayer
# (m::SeqEncodeLayer)(x::AbstractArray) = mapreduce((enc, inds) -> enc(x[inds,:,:]), vcat, m.encoders, m.inputs)
# Flux.trainable(sel::SeqEncodeLayer) = (sel.encoders,)

# struct RepeatLayer{T}
#     encoder::T
# end
# Flux.@functor RepeatLayer
# (m::RepeatLayer)(x::AbstractArray) = mapreduce((enc, inds) -> enc(x[inds,:,:]), vcat, m.encoders, m.inputs)
# Flux.trainable(sel::RepeatLayer) = (sel.encoder,)

end