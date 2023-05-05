module SeqMarket
using Dates
using BaseTypes, MarketDurTypes
using DateUtil
import SimpleStore as SS
import HistData, ChainUtil
using Calendars

const N2 = Float32

struct InputSequence{N}
    time_count::Int
    width::Int
    matrix::Array{Float32,N}
    InputSequence(time_count, width) = new{2}(time_count, width, Array{Float32,2}(undef, time_count, width))
end
function insert!(seq::InputSequence, time, values)
    seq.matrix[time,:] = values
end

function make(;update=false)
    if !update && hasproperty(@__MODULE__, :Cache)
        return Cache
    end
    InputWidth = 23

    date_from = Date(2016,1,1)
    date_to = today()
    time_count = SS.countTss(date_from, date_to) - 1 # -1 because we're using deltas
    seq = InputSequence(time_count, InputWidth)
    i = 0
    tss = SS.getTss(date_from, date_to)
    ts1 = tss[1]
    tim_prev, chain_prev = SS.runFirst(ts1)
    tss_rest = @view tss[2:end]
    SS.run(tss_rest; maxSeconds=1000) do tim, chain
        i += 1
        vix = F(HistData.vixOpen(tim.date)) / 100
        dur = durToInputs(Calendars.calcDur(tim_prev.ts, tim.ts))
        curp, curp_prev = ChainUtil.getCurp(chain), ChainUtil.getCurp(chain_prev)
        atm = calcAtm(chain)
        values = Float32[
            curp / curp_prev - 1.0

            atm...

            log(vix)

            dur...

            N2(Dates.dayofweek(tim.date) / 7)
            N2(Dates.dayofmonth(tim.date) / daysinmonth(tim.date))
            N2(Dates.dayofquarter(tim.date) / daysinquarter(tim.date))
            N2(Dates.dayofyear(tim.date) / daysinyear(tim.date))

            N2(tim.atClose)
            N2(tim.lastOfDay)
            N2(tim.firstOfDay)
        ]
        insert!(seq, i, values)
        tim_prev = tim
        chain_prev = chain
    end
    println("set $i rows, expected $time_count")
    global Cache = seq
    return seq
end

#region Util
# N(b::Bool)::Float32 = b ? 1.0 : 0.0
# N(x::Real)::Float32 = Float32(x)
durToInput(seconds::Second)::N2 = seconds.value/3600
function durToInputs(dur::MarketDur)
    inputs = durToInput.((
        dur.closed,
        dur.pre,
        dur.open,
        dur.post,
        dur.weekend,
        dur.holiday
    ))
    # return N.((inputs..., sqrt.(inputs)...))
    return N2.(inputs)
end

import OptionUtil
function calcAtm(chinfo)
    ss = ChainUtil.toSearch(chinfo, 1)
    calls, puts = (ss.call, ss.put)
    # return Iterators.flatten(map(x -> OptionUtil.calcExtrin(x, calls.curp), ChainUtil.getAtm(calls)..., ChainUtil.getAtm(puts)...))
    return Iterators.flatten(map(x -> N2.(OptionUtil.calcExtrin(x, calls.curp)), (ChainUtil.getAtm(calls)..., ChainUtil.getAtm(puts)...)))
end
#endregion

end