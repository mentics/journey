module ForecastUtil
import Dates
import CudaUtil:DEV
import MLUtil:MLUtil,BinDef,N
import Forecast

# randomInput(cfg) = rand(cfg.inputWidth, cfg.inputLen, cfg.batchLen)
# randomOutput(cfg) = Flux.onehotbatch([Forecast.toBin(cfg.binDef, randn() ./ 20) for _ in 1:cfg.castLen, _ in 1:cfg.batchLen], 1:cfg.binCnt)
# randomLoss(cfg, los) = Statistics.mean(1:100) do _
#     los((randomInput(cfg), randomOutput(cfg)))
# end

function forecasted(cfg, model, seq)
    res = []
    for castOut in 1:cfg.castLen
        starts = 1:(size(seq)[end-1] - cfg.inputLen - castOut)
        yoff = cfg.inputLen + castOut
        yinds = (starts.start + yoff):(starts.stop + yoff)
        yvs = map(i -> seq[cfg.outputInds,i], yinds)
        ys = map(x -> Forecast.toBin(cfg.binDef, x[1]), yvs)
        xscpu = Iterators.map(Flux.unsqueeze(;dims=3), (seq[:,i:(i+cfg.inputLen-1)] for i in starts))
        yhat = cfg.useCpu ? map(x -> model(x)[:,castOut], xscpu) : map(x -> Flux.cpu(model(gpu(x))[:,castOut]), xscpu)
        misses = count(x -> x == 0, [yhat[i][ys[i]] for i in eachindex(ys)])
        println("% miss: ", (misses/length(ys)))
        push!(res, (yhat, ys))
    end
    # collect(zip(res[1][2], [x[1:3] for x in sortperm.(res[1][1]; rev=true)]))
    return res
end

function data(cfg, model, seq)
    starts = 1:(size(seq)[2] - cfg.inputLen - cfg.castLen)
    xs = collect(Iterators.map(Flux.unsqueeze(;dims=3), (seq[:,i:(i-1+cfg.inputLen)] for i in starts)))
    # ys = [seq[:,(i-1+cfg.inputLen+1):(i-1+cfg.inputLen+cfg.castLen)] for i in starts]
    # ys = mapslices(x -> Forecast.toBin(cfg.binDef, x[cfg.outputIndes[1]]), [seq[:,(i-1+cfg.inputLen+1):(i-1+cfg.inputLen+cfg.castLen)] for i in starts]; dims=[2,3])
    ys = map(y -> mapslices(c -> Forecast.toBin(cfg.binDef, c[cfg.outputInds[1]]), y; dims=1), seq[:,(i-1+cfg.inputLen+1):(i-1+cfg.inputLen+cfg.castLen)] for i in starts)
    yhs = collect(map(x -> model(gpu(x)), xs) |> Flux.cpu)
    return (;xs, yhs, ys)
end

# RUN:
# data = fu.data(fs.cfg, fs.mod.model, fs.seq)
# fu.stable1(fs.cfg, data, 1800)
function stable1(cfg, data, off=0)
    xs, yhs, ys = data
    # cc = mapreduce(i -> vcat(i+off + (cfg.castLen+1-i)/1000, yhs[i+off][:,cfg.castLen+1-i]), hcat, 1:cfg.castLen) |> Flux.cpu
    cc = mapreduce(i -> vcat(
                            i+off + (cfg.castLen+1-i)/1000,
                            ys[i+off][:,cfg.castLen+1-i],
                            sortperm(yhs[i+off][:,cfg.castLen+1-i]; rev=true)[1:5]
                        ),
                        hcat, 1:cfg.castLen) |> Flux.cpu
end

function stability(cfg, model, seq)
    # for i in axes(yhat, 3)[2:end]
    #     yh1 = yhs[i-1][2:end]
    #     yh2 = yhs[i][1:end-1]
    # end

    # for i in cfg.castLen
    #     yhs[i][:,cfg.castLen+1-i]
    # end

    # yhat = cfg.useCpu ? map(x -> model(x)[:,castOut], xscpu) : map(x -> Flux.cpu(model(gpu(x))[:,castOut]), xs)
end

end