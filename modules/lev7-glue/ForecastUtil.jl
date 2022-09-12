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

# import Calendars
# import SliceMap
# function makeSeq(cfg)
#     spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
#     vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
#     @assert length(spy) == length(vix)
#     seqLen = length(spy) - 1
#     seq = (Array{UInt8}(undef, 8, seqLen), Array{N}(undef, 12, seqLen), Array{N}(undef, 4, seqLen))
#     for i in axes(spy, 1)[2:end]
#         @assert spy[i].date == vix[i].date
#         prevSpy = spy[i-1]
#         prevSpyClose = prevSpy.close
#         # prevVixClose = vix[i-1].close
#         s = spy[i]
#         v = vix[i]
#         durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
#         seq[1][:,i-1] .= (
#             prep2(s.open, prevSpyClose, cfg.binner),
#             prep2(s.high, prevSpyClose, cfg.binner),
#             prep2(s.low, prevSpyClose, cfg.binner),
#             prep2(s.close, prevSpyClose, cfg.binner),
#             cfg.binnerVix(v.open),
#             cfg.binnerVix(v.high),
#             cfg.binnerVix(v.low),
#             cfg.binnerVix(v.close)
#         )
#         seq[2][:,i-1] .= durFrom
#         seq[3][:,i-1] .= (
#             N(Dates.dayofweek(s.date) / 7),
#             N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
#             N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
#             N(Dates.dayofyear(s.date) / daysinyear(s.date))
#         )
#     end
#     global pe = PositionEmbedding(cfg.embedSize) |> DEV
#     global embed = Embed(cfg.embedSize, cfg.binCnt) |> DEV
#     global den = Flux.Dense(8 * cfg.embedSize => 16) |> DEV
#     global encodeVals = x -> begin
#         we = embed(x, inv(sqrt(cfg.embedSize)))
#         v = we .+ pe(we)
#         sz = size(v)
#         v2 = reshape(v, (sz[1]*sz[2], sz[3:end]...))
#         # println(sz)
#         return den(v2)
#     end
#     global encodeDur = Flux.Dense(12 => 2, Flux.relu) |> DEV
#     global encodeDates = Flux.Dense(4 => 2, Flux.relu) |> DEV
#     # encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
#     encoder = Parallel((xs...) -> cat(xs...; dims=1), encodeVals, encodeDur, encodeDates) |> DEV
#     batcher = MLUtil.makeBatchIter
#     println("Encoded size: ", size(encoder(first(batcher(cfg, seq))[1] |> DEV)))
#     return (;seq, batcher, encoder, params=Flux.params(embed, encodeDur, encodeDates, den))
# end

# function makeSeqSimple()
#     spy = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("SPY")))
#     vix = filter!(x -> x.date > Date(2000,1,1), reverse(HistData.dataDaily("VIX")))
#     @assert length(spy) == length(vix)
#     seq = Array{N}(undef, 17, length(spy)-1)
#     for i in axes(spy, 1)[2:end]
#         @assert spy[i].date == vix[i].date
#         prevSpy = spy[i-1]
#         prevSpyClose = prevSpy.close
#         prevVixClose = vix[i-1].close
#         s = spy[i]
#         v = vix[i]
#         durFrom = durToLogits(Calendars.calcDurCloses(prevSpy.date, s.date))
#         seq[:,i-1] .= (
#             prep1(s.open, prevSpyClose),
#             prep1(s.high, prevSpyClose),
#             prep1(s.low, prevSpyClose),
#             prep1(s.close, prevSpyClose),
#             prep1(v.open, prevVixClose),
#             prep1(v.high, prevVixClose),
#             prep1(v.low, prevVixClose),
#             prep1(v.close, prevVixClose),
#             N(Dates.dayofweek(s.date) / 7),
#             N(Dates.dayofmonth(s.date) / Dates.daysinmonth(s.date)),
#             N(Dates.dayofquarter(s.date) / daysinquarter(s.date)),
#             durFrom...
#         )
#     end
#     encodeDates = Flux.Dense(3 => 6)
#     encodeDur = Flux.Dense(6 => 2, Flux.relu)
#     encoder = SELayer((identity, 1:8), (encodeDates, 9:11), (encodeDur, 12:17))
#     return (seq, encoder)
# end

# function test((x, y)...)
#     @show x y
#     @show typeof(x) typeof(y)
#     @show size(x) size(y)
# end

function trainModel(cfg, mod, seq)
    seqTrain, _ = MLUtil.splitTrainTest(seq, cfg.testHoldOut)
    Forecast.train(cfg, mod.model, mod.params, mod.opt, mod.loss, seqTrain)
end

function testModel(cfg, mod, seq)
    _, seqTest = MLUtil.splitTrainTest(seq, cfg.testHoldOut)
    Forecast.test(cfg, mod.model, mod.loss, seqTest)
end

function forecasted(cfg, model, seq)
    res = []
    for castOut in 1:cfg.castLen
        starts = 1:(size(seq)[2] - cfg.inputLen - castOut)
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

prep1(x, xp)::N = (x - xp) / xp
prep2(x, xp, binner)::UInt8 = binner((x - xp) / xp)
daysinquarter(d)::UInt16 = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )
daysinyear(d)::UInt16 = ( q1 = Dates.firstdayofquarter(d) ; (q1 + Dates.Month(3) - q1).value )

end