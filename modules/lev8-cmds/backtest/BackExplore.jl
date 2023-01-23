module BackExplore

#region Explore
struct Spec
    style::Symbol
    rat::Float64
    off::Currency
    side::Side.T
end

function find(ts, xpir, curp)
    global search = ChainUtil.toSearch(SS.getOqs(ts, xpir))
    specs = [
        Spec(:put, .8, CZ, Side.long),
        Spec(:put, .95, C(-19.2), Side.long),
        Spec(:put, .95, CZ, Side.short),
        Spec(:call, 1.1, CZ, Side.short),
        Spec(:call, 1.1, C(19.2), Side.long),
        Spec(:call, 1.2, CZ, Side.long),
    ]
    global lmso = [fromSpec(curp, search, spec) for spec in specs]
    return lmso
end

function test1()
    from = Date(2020,3,22)
    to = Date(2020,4,17)

    ts1 = SS.tsFirst(from)
    date1 = Date(ts1)
    # xpir = CollUtil.gtee(SS.getExpirs(ts1), bdaysAfter(date1, 80))
    xpir = CollUtil.gtee(SS.getExpirs(ts1), to)
    curp = SS.getUnder(ts1).under
    global curpOrig = curp

    lms = find(ts1, xpir, curp)
    println("neto: $(pricing(lmso))")
    println("curp:$(curp)")
    show(from, to, lms)
    return curp, lms
end

function fromSpec(curp, search, spec)
    strikes = getfield(search, spec.style).strikes
    oqs = getfield(search, spec.style).oqs
    fsearch = spec.rat < 0 ? searchsortedlast : searchsortedfirst
    oq = oqs[clamp(fsearch(strikes, curp * spec.rat + spec.off), firstindex(oqs), lastindex(oqs))]
    return LegMetaOpen(oq, spec.side, 1.0)
end

using ColorSchemes
using PlotUtils: optimize_ticks
pricing(x) = bap(x, .5)
function show(from, to, lmso)
    neto = pricing(lmso)
    tss = SS.getTss(SS.NoFirstLast, from, to)
    # colors = [:blue, :green, :yellow, :red, ]
    # dots = [Vector{Tuple{DateTime,Currency}}() for _ in eachindex(lmso)]
    # dots = [Vector{Tuple{Float64,Currency}}() for _ in eachindex(lmso)]
    global dots = [Vector{Tuple{Int,Currency}}() for _ in eachindex(lmso)]
    global prices = [Vector{Tuple{DateTime,Currency}}() for _ in eachindex(lmso)]
    global unders = Vector{Tuple{Int,Currency}}()
    global nets = Vector{Tuple{Int,Currency}}()
    global netExpireds = Vector{Tuple{Int,Currency}}()
    global priceBase = round_step(SS.getUnder(first(tss)).under, 10)
    for ts in tss
        tsPlot = datetime2unix(ts)
        lup = leg -> SS.getOq(ts, getExpiration(leg), getStyle(leg), getStrike(leg))
        curp = SS.getUnder(ts).under
        try
            lmsc = Between.reqlms(lup, lmso, Action.close)
            vals = netVal(lmso, lmsc)
            for i in eachindex(vals)
                strike = getStrike(lmso[i])
                # push!(dots[i], (datetime2unix(ts), vals[i]))
                push!(dots[i], (tsPlot, strike + vals[i]))
                push!(prices[i], (ts, vals[i]))
            end
            push!(unders, (tsPlot, curp))
            net = neto + pricing(lmsc)
            push!(nets, (tsPlot, priceBase + net))
            push!(netExpireds, (tsPlot, priceBase + neto + OptionUtil.netExpired(lmso, curp)))
        catch e
            println("Could not requote, skipping $(ts)")
            continue
        end
    end

    fig = Figure()
    DataInspector(fig; textcolor=:blue)
    global dateticks = optimize_ticks(tss[1], tss[end])[1]
    ax1 = Axis(fig[1,1])
    ax1.xticks[] = (datetime2unix.(dateticks) , Dates.format.(dateticks, "mm/dd/yyyy"));

    lines!(ax1, unders; color=:white)
    scatter!(ax1, nets; color=:green)
    scatter!(ax1, netExpireds; color=:blue)

    colors = resample_cmap(:viridis, length(dots))
    for i in eachindex(dots)
        scatter!(ax1, dots[i]; color=colors[i])
    end
    display(fig)

    return
end

function netVal(lmso::Coll, lmsc::Coll)
    return map(zip(lmso, lmsc)) do (lmo, lmc)
        neto = pricing(lmo)
        netc = pricing(lmc)
        curVal = neto + netc
        return curVal
    end
end

end