using Flux, SliceMap

# Define the ground truth model. We aim to recover W_truth and b_truth using
# only examples of ground_truth()
W_truth = [1 2 3 4 5;
            5 4 3 2 1] |> gpu
b_truth = [-1.0; -2.0] |> gpu
ground_truth(x) = W_truth*x .+ b_truth

# Generate the ground truth training data as vectors-of-vectors
x_train = [ 5 .* rand(5) for _ in 1:10_000 ] |> gpu
y_train = [ ground_truth(x) + 0.2 .* (randn(2) |> gpu) for x in x_train ] |> gpu

# Define and initialize the model we want to train
cmodel(x) = W*x .+ b
model = cmodel |> gpu
W = rand(2, 5) |> gpu
b = rand(2) |> gpu

# Define pieces we need to train: loss function, optimiser, examples, and params
function loss(x, y)
    # println(size(x), ' ', size(y))
  ŷ = model(x)
  r = slicemap(x -> [sum(x)], ŷ; dims=1)
  return sum(y .- r[1])
#   sum((y .- ŷ).^2)
end
opt = Descent(0.01)
train_data = zip(x_train, y_train)
ps = Flux.params(W, b)

# Execute a training epoch
for (x,y) in train_data
  gs = gradient(ps) do
    loss(x,y)
  end
  Flux.Optimise.update!(opt, ps, gs)
end

# An alternate way to execute a training epoch
# Flux.train!(loss, Flux.params(W, b), train_data, opt)

# Print out how well we did
@show W
@show maximum(abs, W .- W_truth)