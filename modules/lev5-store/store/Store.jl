module Store
using Globals, DateUtil, LogUtil, StoreUtil, OutputUtil
using StatusTypes
using Markets

export dirOrderBackup, pathOrderBackup, dbChecks

dirOrderBackup() = dirData(joinpath("bak", "orders"))
pathOrderBackup(oid::Int) = joinpath(dirOrderBackup(), "$(oid).json")

function dbChecks()
    @log debug "dbChecks"

    rows = select("select t.tid from Trade t where status='Starting' and tsCreated<?", now(UTC) - Hour(2))
    !isempty(rows) && report("Trade still starting after 2 hours", rows)

    rows = select("select lid from vlegtrade where expiration < current_date and status != 'Closed'")
    !isempty(rows) && report("LegTrade expired but not Closed", rows)

    rows = select("select t.tid, t.status, t.targetDate, lt.exp from Trade t join (select tid, max(expiration) as exp from LegTrade group by tid) lt on lt.tid=t.tid where status != 'Closed' and exp < current_date")
    !isempty(rows) && report("Trades not closed but all legs expired", rows)

    rows = select("select olid from VUnusedLegOrd where remain < 1")
    !isempty(rows) && report("VUnusedLegOrd with remain < 1", rows)

    rows = select("select lid, quantity, qtyUsed from VLegFilled where quantity < qtyUsed")
    !isempty(rows) && report("VLegFilledOpen with quantity < qtyUsed", rows)

    rows = select("select oid, orderType, class, primitDir, prillDir from Ord where class != '3' and (orderType = 3 and (primitDir <= 0 or prillDir <= 0)) or (orderType = 2 and (primitDir >= 0 or prillDir >= 0)) and oid not in (16579946, 16609492, 17398981, 17873157)")
    !isempty(rows) && report("Ord primitDir or prillDir wrong sign", rows)

    rows = select(sqlTimeCheck())
    !isempty(rows) && report("Trade has unusually long spread in created or filled times", rows)

    # TODO: and opened yesterday: this is normal when just opened trade before ord fills, so Starting + since this morning's market open is ok
    # rows = select("select * from VLegFilled where qtyUsed < quantity and act=1 and tid not in (select tid from Trade where status=? and tsCreated>?)", Starting, msMarketOpen(today()))
    rows = select(sqlUnfilledCheck(), Starting, now(UTC) - Hour(6)) # msMarketOpen(today()))
    !isempty(rows) && report("LegTrade with unfilled open", rows)

    rows = select("select * from VLegTrade where status != 'Closed' and style=-1 and side=-1 and expiration > current_date and strike - 4 > ?", market().curp)
    !isempty(rows) && report("Dangerous puts found", rows)

    rows = select("select * from legord where prillDir != 0 and sign(prillDir) = sign(side)")
    !isempty(rows) && report("Found LegOrd.prillDir with the wrong sign", rows)

    rows = select("select lt.* from Trade t join LegTrade lt on lt.tid=t.tid where lt.expiration < t.targetDate")
    !isempty(rows) && report("Found LegTrade with expiration < Trade.targetDate", rows)

    unpos = findUnknownPositions()
    !isempty(unpos) && report("Unknown positions found", unpos)

    return
end

report(msg, out::Vector{<:NamedTuple}) = ( pretyble(out; title=msg) ; (@log error spretyble(out; title=msg)) )
report(msg, out) = ( (@error msg out) ; (@log error msg out) )

# The > 1649267718928 is to skip over known mistakes
# Only using act=1 because I'm experimenting with keeping worthless long calendar positions because there's no reason to close them and there's a chance price could move and make them worth something.
sqlTimeCheck() = """
select tid, min(tsCreated) c1, max(tsCreated) c2, (max(tsCreated) - min(tsCreated)) cd, min(tsFilled) f1, max(tsFilled) f2, (max(tsFilled) - min(tsFilled)) fd
from VLegUsed vlu join LegTrade vt on vt.lid=vlu.lid
where act=1 and tsFilled > timestamp '2022-04-06 17:55:18.928'
group by tid
having (max(tsCreated) - min(tsCreated)) > interval '10 seconds' or (max(tsFilled) - min(tsFilled)) > interval '10 seconds'
"""

sqlUnfilledCheck() = """
select f.tid, f.lid from VLegFilled f join Trade t on t.tid=f.tid where f.qtyUsed < f.quantity and f.act=1
    and f.tid not in (select tid from Trade where status=? and tsCreated>?)
"""

using PositionTypes, Positions, SH, Dates, StoreTrade
function findUnknownPositions()
    notFound = Tuple{Position,Ref{Float64}}[]
    tposs = [(p, Ref(getQuantity(p))) for p in positions(;age=Second(0))] # filter(p -> getExpiration(getOption(p)) == today(), positions())
    tlegs = [(leg, Ref(getQuantity(leg))) for leg in StoreTrade.loadLegTrade.(select("select * from vlegtrade where status='Filled'"))]
    # @info "findUnknownPositions" length(tposs) length(tlegs)
    for tpos in tposs
        pos = tpos[1]
        opt = getOption(pos)
        side = getSide(pos)
        qtyPos = getQuantity(pos)
        for tleg in tlegs
            # TODO: more efficient by removing legs?
            tleg[2][] > 0 || continue
            leg = tleg[1]
            if getOption(leg) == opt && getSide(leg) == side
                qtyUse = min(tpos[2][], tleg[2][])
                tpos[2][] -= qtyUse
                tleg[2][] -= qtyUse
                # @info "Found match" qtyUse getLeg(tpos[1]) getLeg(tleg[1]) tpos[2][] tleg[2][]
                tpos[2][] == 0 && @goto Found
            end
        end
        push!(notFound, tpos)
        @label Found
    end
    return notFound
end

end