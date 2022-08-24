using Flux, Transformers, Transformers.Basic, CUDA
using Flux: onehot
#function for created batched data
using Transformers.Datasets: batched

#flux function for update parameters
using Flux: gradient
using Flux.Optimise: update!

# 1) Setup data set
labels = map(string, 1:10)
startsym = "11"
endsym = "12"
unksym = "0"
labels = [unksym, startsym, endsym, labels...]
vocab = Vocabulary(labels, unksym)

# function for generate training datas
sample_data() = (d = map(string, rand(1:10, 10)); (d,d))
# function for adding start & end symbol
preprocess(x) = [startsym, x..., endsym]

@show sample = preprocess.(sample_data())
@show encoded_sample = vocab(sample[1]) #use Vocabulary to encode the training data

# 2) Setup model
InsideWidth = 32 # 512
NumHeads = 2 # 8
HiddenSize = 128 # 2048
# ps = 64 third arg to transformer

#define a Word embedding layer which turn word index to word vector
embed = Embed(InsideWidth, length(vocab)) |> gpu
#define a position embedding layer metioned above
pe = PositionEmbedding(InsideWidth) |> gpu

#define 2 layer of transformer
encode_t1 = Transformer(InsideWidth, NumHeads, HiddenSize) |> gpu
encode_t2 = Transformer(InsideWidth, NumHeads, HiddenSize) |> gpu

#define 2 layer of transformer decoder
decode_t1 = TransformerDecoder(InsideWidth, NumHeads, HiddenSize) |> gpu
decode_t2 = TransformerDecoder(InsideWidth, NumHeads, HiddenSize) |> gpu

#define the layer to get the final output probabilities
linear = Positionwise(Dense(InsideWidth, length(vocab)), logsoftmax) |> gpu

#wrapper for get embedding
function embedding(x)
    we = embed(x, inv(sqrt(InsideWidth)))
    e = we .+ pe(we)
    return e
end

function encoder_forward(x)
    e = embedding(x)
    t1 = encode_t1(e)
    t2 = encode_t2(t1)
    return t2
end

function decoder_forward(x, m)
    e = embedding(x)
    t1 = decode_t1(e, m)
    t2 = decode_t2(t1, m)
    p = linear(t2)
    return p
end

# 3) Run data through model
enc = encoder_forward(encoded_sample)
probs = decoder_forward(encoded_sample, enc)

#collect all the parameters
ps = Flux.params(embed, pe, encode_t1, encode_t2, decode_t1, decode_t2, linear)
opt = ADAM(1e-4)

function smooth(et)
    sm = fill!(similar(et, Float32), 1e-6/size(embed, 2))
    p = sm .* (1 .+ -et)
    label = p .+ et .* (1 - convert(Float32, 1e-6))
    label
end
Flux.@nograd smooth

#define loss function
function loss(x, y)
    label = onehot(vocab, y) #turn the index to one-hot encoding
    label = smooth(label) #perform label smoothing
    enc = encoder_forward(x)
    probs = decoder_forward(y, enc)
    l = logkldivergence(label[:, 2:end, :], probs[:, 1:end-1, :])
    return l
end

#define training loop
function train!()
    @info "start training"
    for i = 1:1000
        # data = batched([sample_data() for i = 1:32]) #create 32 random sample and batched
        data = batched([sample_data() for i = 1:128]) #create 32 random sample and batched
        x, y = preprocess.(data[1]), preprocess.(data[2])
        x, y = vocab(x), vocab(y) #encode the data
        x, y = todevice(x, y) #move to gpu
        grad = gradient(()->loss(x, y), ps)
        if i % 8 == 0
            l = loss(x, y)
            println("loss = $l")
        end
        update!(opt, ps, grad)
    end
end

using Flux: onecold
function translate(x)
    ix = todevice(vocab(preprocess(x)))
    seq = [startsym]

    enc = encoder_forward(ix)

    len = length(ix)
    for i = 1:2len
        trg = todevice(vocab(seq)) # 1: [2]
        dec = decoder_forward(trg, enc) # 1: [len13]
        #move back to gpu due to argmax wrong result on CuArrays
        ntok = onecold(collect(dec), labels)
        push!(seq, ntok[end])
        ntok[end] == endsym && break
    end
    seq[2:end-1]
end

train!()

function test(num=1000)
    pass = 0
    for i in 1:num
        x, y = sample_data()
        a = translate(x)
        pass += a == y ? 1 : 0
    end
    return pass / num
end
