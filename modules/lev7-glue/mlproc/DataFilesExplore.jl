module DataFilesExplore
using Dates
using DataFrames
using BaseTypes, SmallTypes, ChainUtil
# using OptionUtil
# using Pricing
import DataFiles as dat
import Explore as ore
import ProbMultiKde

# TODO: check how close :under_at_xpir and :xpir_value are

function explore(yms = dat.make_yms())
    tsi_index = dat.ts_indexed()
    ts_min = dat.estimate_min_ts() + Month(1)
    y_min, m_min = year(ts_min), month(ts_min)
    filter!(yms) do (y, m)
        y > y_min || (y == y_min && m >= m_min)
    end
    # TODO: add threads
    for (y, m) in yms
        oqs_df = dat.oqs_df(y, m)
        sdfs = groupby(oqs_df, :ts)
        for sdf in sdfs[1:1]
            ts = first(sdf.ts)
            curp = dat.pents_to_c(dat.lup_under(tsi_index, ts))
            kde = ProbMultiKde.make_kde(ts)
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

            global kdf = sdf
            res = ore.findkel(ctx, xpirtss, 1:3) do xpirts, curp
                ChainUtil.oqssEntry(to_oqs(xpir_lookup[xpirts]), curp)
            end
        end
    end
end

#region Local
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