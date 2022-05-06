module ProcSched
using Dates
using SH, Globals, TradierUtil, LogUtil
using TradierAccount, Store

export procExpired

# function procDayCanceled()
#     select("select * from Ord o, OrdLeg ol on o.oid=ol.oid where ol.expiration < strftime('%s', 'now') * 1000 and not exists (select olr.olid from OrdLegResolve olr where ol.olid=olr.olid)")
#     unrOrdLegs = Store.unresolvedExpiredLegs()
#     for l in unrOrdLegs
#         if l.act == -1 && l.orderType == "market" && l.side == -1
#             @info "procDayCanceled: resolving market order" l
#             Store.resolveOrderAndLegs(l.oid, Canceled, msMarketClose(Date(tims(l.expiration))))
#         else
#             @warn "procDayCanceled: didn't resolve" l
#         end
#     end
# end

function procExpired()
    isnothing(snap()) || error("Do not procExpired when snapped")
    @log debug "procExpired"
    histExpired = tradierHistory(today() - Day(30), today(), "optexp")
    for h in histExpired
        hopt = h["option"]
        opt = tier.occToOpt(hopt["description"])
        qtyExpDir = hopt["quantity"]
        if getExpiration(opt) < Date("2022-03-23")
            continue # ignore previous to when we started storing data
        end
        dbLegs = findLegUnclosed(opt, Side.T(-sign(qtyExpDir)))
        isempty(dbLegs) && continue # This is normal: probably already processed it
        # if length(dbLegs) > 1
        #     # TODO: if concern about multiple runs of this method might use the same expirations multiple times, can query for the expired leg use for it
        #     @error "procExpired: didn't find unique leg for expired" opt length(dbLegs) qtyExpDir dbLegs
        #     continue
        # end
        qtyExpDir != 0 || error("why is qtyExpDir 0? ", hopt)
        qtyUsed = findQtyUsedPrev(opt, Side.T(-sign(qtyExpDir)))
        # @info "before" qtyUsed qtyExpDir
        if qtyUsed != 0
            qtyExpDir -= copysign(qtyUsed, qtyExpDir)
            if qtyExpDir == 0
                @error "qty already used. does status need updated?" qtyUsed qtyExpDir hopt
            else
                before = qtyExpDir
                qtyExpDir -= copysign(qtyUsed, qtyExpDir)
                @warn "Found qtyUsed" before qtyExpDir qtyUsed
                if qtyExpDir == 0
                    continue
                end
            end
        end
        for dbLeg in dbLegs
            if dbLeg.qtyremain > abs(qtyExpDir)
                @warn "Check quantity" qtyExpDir dbLeg
            else
                # TODO: ??? This results in the expired leg 10 showing up in unmatched with increasing negative qty. need to clean that up.
                update("insert into LegUsed (lid, olid, act, quantity) values (?, ?, ?, ?)", dbLeg.lid, 100, -1, dbLeg.qtyremain)
                update("update Trade set status = vt.status from VTrade vt where vt.tid=? and Trade.tid=?", dbLeg.tid, dbLeg.tid)
                qtyExpDir -= copysign(dbLeg.qtyremain, qtyExpDir)
            end
        end
    end
    dbChecks()
end

#region Local
const PROC_EXPIRED = :procExpired
whenUpdate(from::DateTime, isMktOpen::Bool, nextMktChange::DateTime) = nextMktChange + Minute(5)

using OptionTypes, SmallTypes, StoreUtil
# TODO: where? for proc expired
findLegUnclosed(opt::Option, side::Side.T) = select(sqlLegsUnclosed(), getStyle(opt), getExpiration(opt), getStrike(opt), side)
findQtyUsedPrev(opt::Option, side::Side.T)::Int = select(sqlPrevUsedExpiredQty(), getStyle(opt), getExpiration(opt), getStrike(opt), side)[1].qtyused

sqlLegsUnclosed() = """
select tid, lid, (quantity - qtyUsed) qtyRemain from VLegFilled where act=-1 and qtyUsed < quantity
    and lid in (select lid from VLegFilled where act=1 and qtyUsed = quantity)
    and lid in (select lid from LegTrade where style=? and expiration=? and strike=? and side=?)
order by lid asc
"""

sqlPrevUsedExpiredQty() = """
select coalesce(sum(lu.quantity),0) qtyUsed from LegTrade lt join VLegUsed lu on lu.lid=lt.lid where
	lt.style=? and lt.expiration=? and lt.strike=? and lt.side=?
	and olid=100
"""
#endregion

end

# TODO: use this to reconcile things and check Reconcile.jl
# function findUnmatchedInHistory()
#     hist = filter(h -> h["type"] == "trade", tradierHistory(today() - Day(30), today(), "trade"))
#     lidsUnExpClose = select("select tl.* from ViewUnmatchedLidClose un, TradeLeg tl on un.lid=tl.lid where expiration < strftime('%s', 'now') * 1000")
#     # TODO: filter out if we're not after market open of next day after expiration (or whenever the API includes yesterday's activity in history)
#     for leg in lidsUnExpClose
#         for h in hist
#             hopt = h["trade"]
#             optH = tier.occToOpt(hopt["symbol"])
#             # qtyDirH = hopt["quantity"]
#             # qtyDirL = leg.
#             optL = Option(Style.T(leg.style), msToDate(leg.expiration), dbToC(leg.strike))
#             if optH == optL
#                 @info "findUnmatchedInHistory: found" optH optL leg h["date"]
#                 println(h)
#             end
#         end
#     end
# end