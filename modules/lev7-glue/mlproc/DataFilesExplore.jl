module DataFilesExplore
using Dates
using DataFrames
using BaseTypes, SmallTypes, ChainUtil
# using OptionUtil
# using Pricing
import DataFiles as dat
import Explore as ore
import ProbMultiKde as pmk
import SH, DateUtil, Pricing, Calendars

# TODO: check how close :under_at_xpir and :xpir_value are

function explore(yms = dat.make_yms())
    global kdf = nothing
    global kr1 = nothing
    tsi_index = dat.ts_indexed()
    ts_min = dat.estimate_min_ts() + Month(1)
    y_min, m_min = year(ts_min), month(ts_min)
    filter!(yms) do (y, m)
        y > y_min || (y == y_min && m >= m_min)
    end
    # TODO: add threads
    for (y, m) in yms
        oqs_df = dat.oqs_df(y, m)
        # TODO: save a file that's already filtered?
        oqs_df = filter(:ts => dat.filter_tsx_prob, oqs_df)
        sdfs = groupby(oqs_df, :ts)
        for sdf in sdfs[1:1]
            ts = first(sdf.ts)
            global kts = ts
            curp = dat.pents_to_c(dat.lup_under(tsi_index, ts))
            kde = pmk.make_kde(ts)
            ctx = ore.make_ctx(ts, kde, curp)
            # TODO: could be more efficient leveraging group by below? get keys on xpir_lookup?
            xpirtss = filter!(d -> Date(d) > ctx.date, unique(sdf.expiration))

            # xpir_lookup = Dict(map(sdfs_xpir) do sdf_xpir
            #     return first(sdfx_xpir.expiration) => sdf_xpir
            # end)
            # for sdf_xpir in groupby(sdf, :expiration)
            #     xpir_lookup[first(sdf_xpir.expiration)] = sdf_xpir
            # end
            xpir_lookup = Dict([first(sdf_xpirts.expiration) => sdf_xpirts for sdf_xpirts in groupby(sdf, :expiration)])

            # global kdf = sdf
            res = ore.findkel(ctx, xpirtss, 1:3) do xpirts, curp
                ChainUtil.oqssEntry(to_oqs(xpir_lookup[xpirts]), curp)
            end

            if !isempty(res) && res[1].evrate > 1e-4
                r1 = res[1]
                # global kr1 = r1
                lms = r1.lms
                target = Pricing.price(Action.open, lms)
                bdaysout = DateUtil.bdays(ctx.date, SH.getExpir(lms))
                println("$(ts): Found evrate:$(r1.evrate) probprofit:$(r1.probprofit) min:$(r1.neto) target:$(target) risk:$(r1.risk) daysout:$(bdaysout)");
                (;neto, netc, pnl) = df_calc_pnl(sdf, lms)
                println("  estimated pnl: $(pnl) ($((;neto, netc))")
                ore.drawres(r1.lms)
                return lms
            else
                println("$(ts): No results")
            end
        end
    end
end

#region Local
function df_calc_pnl(df, lms)
    xpirts = dat.market_close(SH.getExpir(lms))
    strikes = SH.getStrike.(lms)
    rows = dfrow.(Ref(df), xpirts, strikes)
    neto = sum(dfneto.(rows, lms))
    netc = sum(dfnetc.(rows, lms))
    return (;neto, netc, pnl=neto + netc)
end
function dfrow(df, xpirts, strike)
    s = dat.topents(strike)
    return only(df[(df.expiration .== xpirts) .& (df.strike .== s),:])
end
function dfneto(dfrow, lm)
    dat.pents_to_c(dfrow[dfsym(SH.getStyle(lm), SH.getSide(lm), Action.open)])
end
function dfnetc(dfrow, lm)
    dat.pents_to_c(dfrow[dfsym_xpir(SH.getStyle(lm), SH.toOther(SH.getSide(lm)))])
end
dfsym(style, side, action) = Symbol("$(style)_$(side)_$(SH.toCode(action))")
dfsym_xpir(style, side) = Symbol("$(style)_xpir_price_$(side)")

using OptionTypes, OptionQuoteTypes, OptionMetaTypes, QuoteTypes
to_oqs(df::DataFrames.AbstractDataFrame) = collect(Iterators.flatten(map(to_oq_pair, eachrow(df))))
function to_oq_pair(row)
    oc = Option(Style.call, Date(row.expiration), dat.pents_to_c(row.strike))
    op = Option(Style.put, Date(row.expiration), dat.pents_to_c(row.strike))
    qc = Quote(dat.pents_to_c(row.call_bid), dat.pents_to_c(row.call_ask))
    qp = Quote(dat.pents_to_c(row.put_bid), dat.pents_to_c(row.put_ask))
    return OptionQuote(oc, qc, OptionMetaTypes.MetaZero), OptionQuote(op, qp, OptionMetaTypes.MetaZero)
end
#endregion Local

end