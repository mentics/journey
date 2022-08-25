using Flux
using Transformers
using Transformers.Basic
using TimeSeries
using Flux: gradient
using Flux.Optimise: update!
# using BSON: @save
# using TensorBoardLogger, Logging
using CUDA
using Statistics
# using MarketData

function get_src_trg(
    sequence,
    enc_seq_len,
    dec_seq_len,
    target_seq_len
)
	nseq = size(sequence)[2]

	@assert  nseq == enc_seq_len + target_seq_len
	src = sequence[:,1:enc_seq_len,:]
	trg = sequence[:,enc_seq_len:nseq-1,:]
	@assert size(trg)[2] == target_seq_len
	trg_y = sequence[:,nseq-target_seq_len+1:nseq,:]
	@assert size(trg_y)[2] == target_seq_len
	if size(trg_y)[1] == 1
	 	return src, trg, dropdims(trg_y; dims=1)
	else
		return src, trg, trg_y
	end
end

gpu_enabled = enable_gpu(true)

## Model parameters
# dim_val = 512 # This can be any value. 512 is used in the original transformer paper.
# n_heads = 8 # The number of attention heads (aka parallel attention layers). dim_val must be divisible by this number
# n_decoder_layers = 4 # Number of times the decoder layer is stacked in the decoder
# n_encoder_layers = 4 # Number of times the encoder layer is stacked in the encoder
# input_size = 1 # The number of input variables. 1 if univariate forecasting.
# dec_seq_len = 92 # length of input given to decoder. Can have any integer value.
# enc_seq_len = 153 # length of input given to encoder. Can have any integer value.
# output_sequence_length = 58 # Length of the target sequence, i.e. how many time steps should your forecast cover
# in_features_encoder_linear_layer = 2048 # As seen in Figure 1, each encoder layer has a feed forward layer. This variable determines the number of neurons in the linear layer inside the encoder layers
# in_features_decoder_linear_layer = 2048 # Same as above but for decoder
# max_seq_len = enc_seq_len # What's the longest sequence the model will encounter? Used to make the positional encoder

dim_val = 32 # This can be any value. 512 is used in the original transformer paper.
n_heads = 4 # The number of attention heads (aka parallel attention layers). dim_val must be divisible by this number
n_decoder_layers = 2 # Number of times the decoder layer is stacked in the decoder
n_encoder_layers = 2 # Number of times the encoder layer is stacked in the encoder
input_size = 1 # The number of input variables. 1 if univariate forecasting.
dec_seq_len = 31 # length of input given to decoder. Can have any integer value.
enc_seq_len = 73 # length of input given to encoder. Can have any integer value.
output_sequence_length = 17 # Length of the target sequence, i.e. how many time steps should your forecast cover
in_features_encoder_linear_layer = 512 # As seen in Figure 1, each encoder layer has a feed forward layer. This variable determines the number of neurons in the linear layer inside the encoder layers
in_features_decoder_linear_layer = 512 # Same as above but for decoder
max_seq_len = enc_seq_len # What's the longest sequence the model will encounter? Used to make the positional encoder


#define 2 layer of transformer
encode_t1 = Transformer(dim_val, n_heads, 64, 2048;future=false,pdrop=0.2)|> gpu
encode_t2 = Transformer(dim_val, n_heads, 64, 2048;future=false,pdrop=0.2)|> gpu
#define 2 layer of transformer decoder
decode_t1 = TransformerDecoder(dim_val, n_heads, 64, 2048,pdrop=0.2) |> gpu
decode_t2 = TransformerDecoder(dim_val, n_heads, 64, 2048,pdrop=0.2) |> gpu
encoder_input_layer = Dense(input_size,dim_val) |> gpu
decoder_input_layer = Dense(input_size,dim_val) |> gpu
positional_encoding_layer = PositionEmbedding(dim_val) |> gpu
p = 0.2
dropout_pos_enc = Dropout(p) |> gpu

#define the layer to get the final output probabilities
#linear = Positionwise(Dense(dim_val, output_sequence_length))
linear = Dense(output_sequence_length*dim_val,output_sequence_length) |> gpu
function encoder_forward(x)
    println("ef ", typeof(x), ' ', typeof(encoder_input_layer))
    error("stop")
    x = encoder_input_layer(x)
    e = positional_encoding_layer(x)
    t1 = x .+ e
    t1 = dropout_pos_enc(t1)
    t1 = encode_t1(t1)
    t1 = encode_t2(t1)
    return t1
end

function decoder_forward(tgt, t1)
    decoder_output = decoder_input_layer(tgt)
    t2 = decode_t1(decoder_output,t1)
    t2 = decode_t1(decoder_output,t2)
    t2 = Flux.flatten(t2)
    p = linear(t2)
    return p
end

function generate_seq(x, seq_len)
	result = Matrix{Float64}[]
	for i in 1:length(x)-seq_len+1
		ele = reshape(x[i:i+seq_len-1],(seq_len,1))
		push!(result,ele)
	end
	return result
end

function loss(src, trg, trg_y)
  enc = encoder_forward(src)
  dec = decoder_forward(trg, enc)
  err = Flux.Losses.mse(dec,trg_y)
  return err
end


sincurve1 = Vector{Float64}()
for i in 1:500
    append!(sincurve1,sin(i))
end


length(sincurve1)

generate_seq(sincurve1,enc_seq_len+output_sequence_length)

# ta_mv = moving(mean,ta,15)


# lg=TBLogger("tensorboard_logs/run", min_level=Logging.Info)
# data = generate_seq(values(moving(mean,cl,15)),enc_seq_len+output_sequence_length)
#data = generate_seq(values(ta_mv[:"10 YR"]),enc_seq_len+output_sequence_length)
data = generate_seq(sincurve1,enc_seq_len+output_sequence_length)
@show size(data)
data = reduce(hcat,data)
data = convert(Array{Float32,2}, data)
data_sz = size(data)
thd = floor(Int,data_sz[2]/2)
testdata = data[:,thd+1:end]
data = data[:,1:thd]
ps = Flux.params(encoder_input_layer, positional_encoding_layer, encode_t1, encode_t2, decoder_input_layer, decode_t1,decode_t2,  linear)
all_layers = [ encoder_input_layer, positional_encoding_layer, encode_t1, encode_t2, decoder_input_layer, decode_t1, decode_t2, linear ]
opt = ADAM(1e-4)
train_loader = Flux.Data.DataLoader(data, batchsize=32)
l = 100
for i = 1:1000
    for x in train_loader
        sz = size(x)
        sub_sequence = reshape(x,(1,sz[1],sz[2]))
           src, trg, trg_y = get_src_trg(
                            sub_sequence,
                            enc_seq_len,
                            dec_seq_len,
                            output_sequence_length
                            )
        src, trg, trg_y = todevice(src, trg, trg_y) #move to gpu
        grad = gradient(()->loss(src, trg, trg_y), ps)
        Flux.update!(opt, ps, grad)
        global l = collect(loss(src, trg, trg_y))[1]
        if l < 1e-3
            continue
        end
    end
    if i % 10 == 0
        for (j,layer) in enumerate(all_layers)
            println("could be saving checkpoint")
            # @save "checkpoint/model-"*string(j)*".bson" layer
        end
        # with_logger(lg) do
            @info "train" loss=l log_step_increment=1
        # end
    end
    if l < 1e-3
            continue
    end
end

function prediction(test_data)
	seq = Array{Float32}[]
	test_loader = Flux.Data.DataLoader(test_data, batchsize=32)
	for x in test_loader
		sz = size(x)
		sub_sequence = reshape(x,(1,sz[1],sz[2]))
		#@show enc_seq_len
		ix = sub_sequence[:,1:enc_seq_len+output_sequence_length,:]
		#@show size(x),size(ix)
		ix = todevice(ix)
		enc = encoder_forward(ix[:,1:enc_seq_len,:])
		trg = ix[:,enc_seq_len:sz[1]-1,:]
		#@show size(trg),size(enc)
		dec = decoder_forward(trg, enc)
		#@show size(dec[end,:])
		seq = vcat(seq,collect(dec[end,:]))
	end
	return seq
end

seq_train = Vector{Float64}(prediction(data))

# ╔═╡ 77e65945-2a10-4723-979a-f33170e71500
seq_test = Vector{Float64}(prediction(testdata))

# ╔═╡ d917655a-56e3-42e4-bfb6-9dcb4833be27
using GLMakie
function myplot(seq1,seq2)
	# plot(seq1)
	# plot!(seq2)
	display(lines(seq1))
	lines!(Vector{Float64}(seq2))
end

fig = myplot(data[end,:],seq_train)

# savefig(fig,"train_fig.png")

myplot(testdata[end,:],seq_test)

# savefig(fig,"test_fig.png")

Flux.Losses.mse(testdata[end,:],seq_test)

Flux.Losses.mse(data[end,:],seq_train)

Flux.Losses.mse(testdata[end,:],testdata[end-1,:])

a,b = size(testdata)

row1 = reshape(testdata[:,1], (a,1))

for i in 1:a
    row1 = todevice(row1)
    seq = prediction(row1)
    seq = collect(seq)
    row1 = collect(row1)
    global row1 = vcat(row1, reshape(seq,(1,1)))
    #@show size(row1)
    global row1 = row1[2:end,:]
end
plot(row1[:,1])
plot!(testdata[:,1])

row1



