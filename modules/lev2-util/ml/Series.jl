module Series
using Flux, Transformers, Transformers.Basic, CUDA

#region TestCopyTask
function testCopyTask()
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

    #define a Word embedding layer which turn word index to word vector
    embed = Embed(512, length(vocab)) |> gpu
    #define a position embedding layer metioned above
    pe = PositionEmbedding(512) |> gpu

    #define 2 layer of transformer
    encode_t1 = Transformer(512, 8, 64, 2048) |> gpu
    encode_t2 = Transformer(512, 8, 64, 2048) |> gpu

    #define 2 layer of transformer decoder
    decode_t1 = TransformerDecoder(512, 8, 64, 2048) |> gpu
    decode_t2 = TransformerDecoder(512, 8, 64, 2048) |> gpu

    #define the layer to get the final output probabilities
    linear = Positionwise(Dense(512, length(vocab)), logsoftmax) |> gpu

    # 3) Run data through model
end

#wrapper for get embedding
function embedding(x)
  we = embed(x, inv(sqrt(512)))
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
#endregion

#region Local
__init__() = enable_gpu(true)
#endregion

end