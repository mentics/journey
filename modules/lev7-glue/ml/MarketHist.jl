module MarketHist
using Dates
import DateUtil
import Calendars
import SimpleStore as SS
import HistData

const Tex0 = Ref(DateTime(2023,1,1))

#region Curp
const Curps = Vector{NTuple{2,Float64}}() # (tex, curp)

function populate_curps()
    empty!(Curps)
    SS.run(Date(2014,1,1), Date(2023,1,1)) do tim, chain
        curp = chain.under.under
        tex = Calendars.calcTex(tim.ts, Tex0[])
        push!(Curps, (tex, curp))
    end
    sort!(Curps; by=x->x[1])
end

function curp_for_tex(base::DateTime, tex_ago::Float64)
    tex = Calendars.calcTex(base, Tex0[]) + tex_ago
    ind_left = searchsortedlast(Curps, tex; by = x -> x[1])
    if ind_left <= 0 || ind_left >= length(Curps)
        @show base tex_ago tex extrema(Curps)
        error("Not found")
    end
    left = Curps[ind_left]
    right = Curps[ind_left+1]
    dx = right[1] - left[1]
    dy = right[2] - left[2]
    return left[2] + dy * (tex - left[1]) / dx
end
#endregion

#region Vix
const Vixs = Vector{NTuple{2,Float64}}() # (tex, vix)

function populate_vixs()
    empty!(Vixs)
    from = DateUtil.nextTradingDay(Date(2014,1,1))
    to = DateUtil.lastTradingDay(Date(2023,1,1))
    date = from
    while true
        tex = Calendars.calcTex(Calendars.getMarketOpen(date), Tex0[])
        push!(Vixs, (tex, Float64(HistData.vixOpen(date)) / 100))
        date = DateUtil.bdaysAfter(date, 1)
        date <= to || break
    end
end

function vix_for_tex(base::DateTime, tex_ago::Float64)
    tex = Calendars.calcTex(base, Tex0[]) + tex_ago
    ind_left = searchsortedlast(Vixs, tex; rev=true, by = x -> x[1])
    left = Vixs[ind_left]
    right = Vixs[ind_left+1]
    dx = right[1] - left[1]
    dy = right[2] - left[2]
    return left[2] + dy * (tex - left[1]) / dx
end
#endregion

end