module StoreTrade
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes, TradeTypes, LegTradeTypes, StatusTypes, LegMetaTypes
using Globals, BaseUtil, DateUtil, StoreUtil, FileUtil

export newTrade, loadTrade, loadLegTrade, findTrades
export queryLegStatus, queryNumLegs, queryLeftovers, queryEntered, queryLegsEntered
export findTradeEntered

function newTrade(primitDir::PriceT, legs::Coll{LegMeta,4}, underBid::Currency, underAsk::Currency)::Int
    @assert sum(getBid, legs) <= primitDir <= sum(getAsk, legs) "Invalid primitDir $(primitDir) $(sum(getBid, legs)) $(sum(getAsk, legs))\n$(legs)"
    exp = minimum(getExpiration, legs)
    tid = 0
    inTransaction() do
        res = select("insert into Trade (status, tsCreated, primitDir, targetDate) values (?, ?, ?, ?) returning tid", Starting, now(UTC), primitDir, exp)
        tid = first(res).tid
        update("insert into TradeMeta (tid, underBid, underAsk) values (?, ?, ?)", tid, underBid, underAsk)
        for lq in legs
            resLeg = select("insert into LegTrade (tid, style, expiration, strike, side, quantity) values (?, ?, ?, ?, ?, ?) returning lid",
                    tid, Int(getStyle(lq)), getExpiration(lq), getStrike(lq), Int(getSide(lq)), getQuantity(lq))
            lid = first(resLeg).lid
            update("insert into LegTradeMeta (lid, bid, ask, iv) values (?, ?, ?, ?)",
                    lid, getBid(lq), getAsk(lq), getIv(lq))
        end
    end
    return tid
end

# TODO: implement cache for trade loading
# TODO: implement updating just the fields that might change (eg. status) in bulk for the cache
function loadTrade(tid::Int)
    ts = select("select * from VTrade where tid=?", tid)
    !isempty(ts) || error("Trade not found ", tid)
    t = ts[1] # Search by primary key, can't be more than one.
    # @info "loadTrade" t
    # legs = loadLegTrade.(select("select * from VLegTrade where tid = ?", tid), t.tsCreated)
    # return Trade{strToStatus(t.status)}(t.tid, dbToC(t.primitDir), dbToC(t.prillDirOpen), dbToC(t.prillDirClose),
    #         legs, t.tsCreated, noth(t.tsFilled), noth(t.tsClosed), TradeMeta(t.underBid, t.underAsk))
    legs = loadLegTrade.(select("select * from VLegTrade where tid=?", tid))
    return Trade{strToStatus(t.status)}(t.tid, t.targetdate, dbdc(t.primitdir), dbdc(t.prilldiropen), dbdc(t.prilldirclose),
            legs, t.tscreated, noth(t.tsfilled), noth(t.tsclosed), TradeMeta(dbdc(t.underbid), dbdc(t.underask)))
end

function loadLegTrade(lid::Int)
    loadLegTrade(select("select * from VLegTrade where lid=?", lid)[1])
end

dbdc(x) = isSomething(x) ? C(Float64(x)) : nothing
noth(x) = ismissing(x) ? nothing : x

findTrades(states::Type{<:Status}...)::Vector{Trade} = loadTrade.(selectCol("select tid from Trade where status in ($(join(repeat(['?'], length(states)), ',')))", states...))
function findTrades(exp::Date, states::Type{<:Status}...)::Vector{Trade}
    strStates = join(repeat(['?'], length(states)), ',')
    tids = selectCol("select tid from Trade where targetDate = ? and status in ($(strStates))", exp, states...)
    loadTrade.(tids)
end
# TODO: make this work with local timezone to date properly
findTradeEntered(d::Date)::Vector{Trade} = loadTrade.(selectCol("select tid from Trade where cast(cast(tsCreated//1000 as timestamp) as date)=?", d))

# TODO: needs timezone
queryEntered(d::Date)::Vector{NamedTuple} = select("select tid, cast(tsCreated as date) enteredDate, targetDate from Trade where cast(tsCreated as date)=?", d)
queryLegsEntered(d::Date)::Vector{LegTrade} =
    loadLegTrade.(select("select * from VLegTrade where cast(tsCreated as date)=?", d)) # TODO: doesn't use timezone as it should

# queryLegStatus(lid::Int)::T where T<:Status = strToStatus(select("select status from VLegTrade where lid=?", lid)[1].status)
queryLegStatus(lid::Int) = strToStatus(select("select status from VLegTrade where lid=?", lid)[1].status)

queryNumLegs(tid::Int)::Union{Nothing,Int} = (res = selectCol("select count(lid) from LegTrade where tid=?", tid) ; isempty(res) ? nothing : res[1] )

function queryLeftovers()::Vector{LegTrade}
    rows = select("select * from vlegtrade where tscreated is not null and tsclosed is null and tid in (select tid from Trade where targetDate < current_date)")
    legs = Vector{LegTrade}()
    for row in rows
        leg = loadLegTrade(row)
        push!(legs, leg) # these are filled legs, so we can count on tsCreated being there
    end
    return legs
end

#region Local
loadLegTrade(row::NamedTuple) =
    LegTrade(row.lid, row.tid, strToStatus(row.status), dbdc(row.prilldiropen), dbdc(row.prilldirclose),
             Leg(Option(Style.T(row.style), row.expiration, dbdc(row.strike)), dbdc(row.quantity), Side.T(row.side)),
             row.tscreated, noth(row.tsfilled), noth(row.tsclosed), LegTradeMeta(dbdc(row.bid), dbdc(row.ask), row.iv))

using JsonConfig, DictUtil
function deleteTrade(tid::Int)
    @warn "Deleting trade" tid
    trade = loadTrade(tid)
    isnothing(getPrillDirOpen(trade)) || error("Trying to delete trade that has fill ", tid)
    # print("Are you sure this trade should be deleted? (N/y)")
    # input = readline()
    # if input == "y"
        writeStr(dirData("save/deletedTrades/$(tid).json"), DictUtil.jsonPretty(trade))
        res = update("delete from Trade where tid=?", tid)
        @info "Deleted." res
    # else
    #     @info "Not deleted."
    # end
end
function undeleteTrade(tid::Int)
    t = loadJson(dirData("save/deletedTrades/$(tid).json"), Trade)
    inTransaction() do
        update("insert into Trade (tid, status, tsCreated, primitDir, targetDate) values (?, ?, ?, ?, ?)", getId(t), Starting, tsCreated(t), getPrimitDir(t), getTargetDate(t))
        tmet = getMeta(t)
        update("insert into TradeMeta (tid, underBid, underAsk) values (?, ?, ?)", tid, tmet.bid, tmet.ask)
        for leg in getLegs(t)
            update("insert into LegTrade (lid, tid, style, expiration, strike, side, quantity) values (?, ?, ?, ?, ?, ?, ?)",
                    getId(leg), tid, Int(getStyle(leg)), getExpiration(leg), getStrike(leg), Int(getSide(leg)), getQuantity(leg))
            lmet = getMeta(leg)
            update("insert into LegTradeMeta (lid, bid, ask, iv) values (?, ?, ?, ?)",
                    getId(leg), getBid(lmet), getAsk(lmet), getIv(lmet))
        end
    end
end
#endregion

#region ForTesting
hasTrades() = !isempty(selectCol("select tid from Trade where status not in ('Starting')"))
hasTrades(states::Type{<:Status}...) = !isempty(selectCol("select tid from Trade where status in ($(join(repeat(['?'], length(states)), ',')))", states))
#endregion

end