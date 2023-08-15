module MLExplore7

#region Explore
function findmaxxi(data)
    batcher = make_batcher(data, hypers().seqlen)
    lss = allbatchlosses(data, batcher)
    _, batchi = findmax(lss)
    b = batcher(batchi, 0)
    mx = maxforbatch(b)
    global kmx = mx
    return mx
end
function allbatchlosses(data, batcher)
    return [Autoencoder2.calcloss(Autoencoder2.kmodelgpu, batcher(i, 0) |> gpu) for i in 1:43]
end
function maxforbatch(batch)
    maxloss = -Inf
    maxx = nothing
    maxi = 0
    i = 1
    for x in eachslice(batch; dims=ndims(batch))
        xm = collect(reshape(x, (size(x)..., 1)))
        xgpu = xm |> gpu
        ls = calcloss(kmodelgpu, xgpu)
        @show ls
        if ls > maxloss
            maxloss = ls
            maxx = xm
            maxi = i
        end
        i += 1
    end
    return (maxi, maxloss, maxx)
end
#endregion

end