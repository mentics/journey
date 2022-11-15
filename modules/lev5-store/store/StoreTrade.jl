module StoreTrade
using Dates
using SH, BaseTypes, SmallTypes, OptionTypes, LegTypes, TradeTypes, LegTradeTypes, StatusTypes, LegMetaTypes
using Globals, BaseUtil, DateUtil, StoreUtil, FileUtil, LogUtil

export ST
const ST = @__MODULE__

export newTrade, loadTrade, loadLegTrade, findTrades
export queryLegStatus, queryNumLegs, queryLeftovers, queryEntered, queryLegsEntered
export findTradeEntered

function newTrade(primitDir::PriceT, legs::Coll{LegMeta}, underBid::Currency, underAsk::Currency)::Int
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
    updateTradesOpen()
    return tid
end

# # TODO: implement cache for trade loading
# # TODO: implement updating just the fields that might change (eg. status) in bulk for the cache
# function loadTrade(tid::Int)
#     ts = select("select * from VTrade where tid=?", tid)
#     !isempty(ts) || error("Trade not found ", tid)
#     t = ts[1] # Search by primary key, can't be more than one.
#     # @info "loadTrade" t
#     # legs = loadLegTrade.(select("select * from VLegTrade where tid = ?", tid), t.tsCreated)
#     # return Trade{strToStatus(t.status)}(t.tid, dbToC(t.primitDir), dbToC(t.prillDirOpen), dbToC(t.prillDirClose),
#     #         legs, t.tsCreated, noth(t.tsFilled), noth(t.tsClosed), TradeMeta(t.underBid, t.underAsk))
#     legs = loadLegTrade.(select("select * from VLegTrade where tid=?", tid))
#     return Trade{strToStatus(t.status)}(t.tid, t.targetdate, dbdc(t.primitdir), dbdc(t.prilldiropen), dbdc(t.prilldirclose),
#             legs, t.tscreated, noth(t.tsfilled), noth(t.tsclosed), TradeMeta(dbdc(t.underbid), dbdc(t.underask)))
# end

# function loadLegTrade(lid::Int)
#     loadLegTrade(select("select * from VLegTrade where lid=?", lid)[1])
# end

qmarks(num) = "$(join(repeat(['?'], num), ','))"

using IterTools
const VEC_EMPTY_TRADES = Trade[]
loadTrades(tids::Coll{Int})::Vector{Trade} = loadTrades(qmarks(length(tids)), tids)
function loadTrades(clauseIn::String, clauseArgs...)::Vector{Trade}
    ts = select("select * from VTrade where tid in ($(clauseIn))", clauseArgs...)
    !isempty(ts) || return VEC_EMPTY_TRADES
    legsRows = select("select * from VLegTrade where tid in ($(clauseIn)) order by tid asc, strike asc", clauseArgs...)
    return map(zip(ts, groupby(x -> x.tid, legsRows))) do (t, tlrs)
        legs = loadLegTrade.(tlrs)
        if length(legs) == 4
            if getStyle(legs[1]) != getStyle(legs[2])
                @assert getStrike(legs[2]) == getStrike(legs[3])
                tmp = legs[3]
                legs[3] = legs[2]
                legs[2] = tmp
            end
            @assert getStyle(legs[1]) == getStyle(legs[2])
            @assert getStyle(legs[3]) == getStyle(legs[4])
        end
        return Trade{strToStatus(t.status)}(t.tid, t.targetdate, dbdc(t.primitdir), dbdc(t.prilldiropen), dbdc(t.prilldirclose),
              legs, t.tscreated, noth(t.tsfilled), noth(t.tsclosed), TradeMeta(dbdc(t.underbid), dbdc(t.underask)))
    end
end

using Caches
const TradesCacheType = Tuple{Vector{Trade},Dict{Int,Trade}}
const CACHE_TRADES_OPEN = :TradesOpen
const CACHE_TRADES_CLOSED = :TradesClosed
export tradesClosed, tradesOpen, tradesOpenEntered

getTradeAll(tid::Int) = @coalesce get(tradictOpen(), tid, nothing) get(tradictClosed(), tid, nothing)
getTradeOpen(tid::Int) = get(tradictOpen(), tid, nothing)
getTradeClosed(tid::Int) = get(tradictClosed(), tid, nothing)

tradesOpen(xpir::Date; kws...)::Vector{Trade} = tradesOpen(x -> getTargetDate(x) == xpir) # filter(x -> getTargetDate(x) == xpir, tradesOpen(; kws...))
tradesOpenEntered(d::Date; kws...)::Vector{Trade} = tradesOpen(x -> toDateMarket(tsCreated(x)) == d; kws...) # filter(x -> toDateMarket(tsCreated(x)) == d, tradesOpen(; kws...))
tradesOpen(f; kws...)::Vector{Trade} = filter(f, tradesOpen(; kws...))

tradictOpen(;age=Minute(10))::Dict{Int,Trade} =
        tradesCache(loadOpen, CACHE_TRADES_OPEN; age)[2]
tradictClosed(;age=Minute(10), since=bdaysBefore(today(), 7))::Dict{Int,Trade} =
        tradesCache(() -> loadClosed(since), CACHE_TRADES_CLOSED; age)[2]

tradesOpen(;age=Minute(10))::Vector{Trade} =
        tradesCache(loadOpen, CACHE_TRADES_OPEN; age)[1]
tradesClosed(;age=Minute(10), since=bdaysBefore(today(), 7))::Vector{Trade} =
        tradesCache(() -> loadClosed(since), CACHE_TRADES_CLOSED; age)[1]

function tradesCache(updater, sym::Symbol;age=Minute(10))::TradesCacheType
    return cache!(TradesCacheType, sym, age) do
        trades = updater()
        dict = toDict(getId, trades)
        return (trades, dict)
    end
end

loadOpen()::Vector{Trade} = loadTrades("select tid from Trade where status not in ($(qmarks(length(StatusClosed))))", StatusClosed...)
#     trades = loadTrades("select tid from Trade where status not in ($(qmarks(length(StatusClosed))))", StatusClosed...)
#     return toDict(getId, trades)
# end

loadClosed(since::Date)::Vector{Trade} = loadTrades("select tid from Trade where cast(tsCreated as date) >= ? and status in ($(qmarks(length(StatusClosed))))", since, StatusClosed...)
#     trades = loadTrades("select tid from Trade where cast(tsCreated as date) >= ? and status in ($(qmarks(length(StatusClosed))))", since, StatusClosed...)
#     return toDict(getId, trades)
# end

# function loadTradesCache()
#     # trades = loadTrades("select tid from VTrade where status != ? or tsCreated >= now()+'-24 hour' or tsfilled >= now()+'-24 hour' or tsclosed >= now()+'-24 hour'", Closed)
#     trades = loadTrades(
#     """
#     select tid from LegTrade where lid in (
#         select lu.lid from LegUsed lu where olid in (
#             select olid from LegOrd where tsCreated >= (now() + '-2 hour') or tsFilled >= (now() + '-2 hour')
#         )
#     ) and tid in (
#         select tid from Trade where status != ?
#     )
#     """, Closed)
#     return toDict(getId, trades)
# end
function updateTradesOpen()
    Threads.@spawn tradesOpen(;age=Second(0))
end
function updateTradesClosed()
    Threads.@spawn tradesClosed(;age=Second(0))
end

# function loadTradesUpdated()
#     t1 = @timed trades = loadTrades("select tid from VTrade where tsfilled >= now()+'-2 hour' or tsclosed >= now()+'-2 hour'")
#     t2 = @timed for trade in trades TradesCache[getId(trade)] = trade end
#     @log debug "loadTradesUpdated" length(trades) t1.time t2.time
# end

dbdc(x) = isSomething(x) ? C(Float64(x)) : nothing
noth(x) = ismissing(x) ? nothing : x

# findTrades(states::Type{<:Status}...)::Vector{Trade} = loadTrades("select tid from Trade where status in ($(qmarks(length(states))))", states...)
# findTrades(exp::Date, states::Type{<:Status}...)::Vector{Trade} = loadTrades("select tid from Trade where targetDate = ? and status in ($(qmarks(length(states))))", exp, states...)
# function findTrades(exp::Date, states::Type{<:Status}...)::Vector{Trade}
#     strStates = join(repeat(['?'], length(states)), ',')
#     tids = selectCol("select tid from Trade where targetDate = ? and status in ($(strStates))", exp, states...)
#     loadTrade.(tids)
# end
# TODO: make this work with local timezone to date properly
# findTradeEntered(d::Date)::Vector{Trade} = loadTrade.(selectCol("select tid from Trade where cast(tsCreated as date)=?", d))

# TODO: needs timezone
# queryEntered(d::Date, states::Type{<:Status}...)::Vector{NamedTuple} =
#     isempty(states) ? select("select tid, cast(tsCreated as date) enteredDate, targetDate, status from Trade where cast(tsCreated as date)=?", d) :
#                       select("select tid, cast(tsCreated as date) enteredDate, targetDate, status from Trade where cast(tsCreated as date)=? and status in ($(join(repeat(['?'], length(states)), ',')))", d, states...)
# queryLegsEntered(d::Date)::Vector{LegTrade} =
#     loadLegTrade.(select("select * from VLegTrade where cast(tsCreated as date)=?", d)) # TODO: doesn't use timezone as it should

# # queryLegStatus(lid::Int)::T where T<:Status = strToStatus(select("select status from VLegTrade where lid=?", lid)[1].status)
# queryLegStatus(lid::Int) = strToStatus(select("select status from VLegTrade where lid=?", lid)[1].status)
getLegStatus(tid::Int, lid::Int) = findLeg(getTradeAll(tid), lid)

# queryNumLegs(tid::Int)::Union{Nothing,Int} = (res = selectCol("select count(lid) from LegTrade where tid=?", tid) ; isempty(res) ? nothing : res[1] )

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
    trade = ST.getTradeOpen(tid)
    isnothing(getPrillDirOpen(trade)) || error("Trying to delete trade that has fill ", tid)
    # print("Are you sure this trade should be deleted? (N/y)")
    # input = readline()
    # if input == "y"
        writeStr(joinpath(dirData("save/deletedTrades"), "$(tid).json"), DictUtil.jsonPretty(trade))
        res = update("delete from Trade where tid=?", tid)
        @info "Deleted." res
    # else
    #     @info "Not deleted."
    # end
    updateTradesOpen()
    # delete!(TradesCache, tid)
end
function undeleteTrade(tid::Int)
    t = loadJson(joinpath(dirData("save/deletedTrades"), "$(tid).json"), Trade)
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