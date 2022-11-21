module TradierAccount
using CollUtil
using BaseTypes, TradierConfig, TradierBase

export tradierOrders, tradierOrder, tradierPositions, tradierBalances

export ta
const ta = @__MODULE__

function tradierOrders()::TradierRespVec
    result = tradierGet("/accounts/$(getAccountId())/orders?includeTags=true", Call(nameof(var"#self#")))
    orders = get(result, "orders") do
        println("result = ", result)
        # error("No orders key in response for tradierOrders.")
        println("WARN: No orders key in response for tradierOrders.")
    end
    if !isnothing(orders) && orders != "null"
        return ensureVector(orders["order"])
    else
        return TRADIER_EMPTY
    end
end

function tradierOrder(oid::Int)::TradierResp
    return tradierGet("/accounts/$(getAccountId())/orders/$(oid)?includeTags=true", Call(nameof(var"#self#")))["order"]
end

function tradierPositions()::TradierRespVec
    result = tradierGet("/accounts/$(getAccountId())/positions", Call(nameof(var"#self#")))
    positions = get(result, "positions", nothing)
    if !isnothing(positions) && positions != "null"
        return ensureVector(positions["position"])
    else
        return TRADIER_EMPTY
    end
end

function tradierBalances()::TradierResp
    result = tradierGet("/accounts/$(getAccountId())/balances", Call(nameof(var"#self#")))
    return result["balances"]
end

using Dates
export tradierHistory
function tradierHistory(from::Date, to::Date, type::AStr="trade,option")::TradierRespVec
    strStart = Dates.format(from, TRADIER_DATE_FORMAT)
    strEnd = Dates.format(to, TRADIER_DATE_FORMAT)
    payload = "?limit=100&type=$(type)&start=$(strStart)&end=$(strEnd)"
    result = tradierGet("/accounts/$(getAccountId())/history$(payload)", Call(nameof(var"#self#")))
    positions = result["history"]
    if positions != "null"
        return ensureVector(positions["event"])
    else
        return []
    end
end
# nonTrade(hist) = filter(h-> h["type"] != "trade", hist)

# select * from ViewTradeLeg where expiration < strftime('%s', 'now') * 1000 and openRes != 'Canceled' and closeRes is null

# select strftime('%s', 'now') * 1000


# function tradierGainLoss(config::TradierConfig=cfg(), accountId::AbstractString=config.accountId)
#     strStart = Dates.format(today() - Day(1), TRADIER_DATE_FORMAT)
#     strEnd = Dates.format(today(), TRADIER_DATE_FORMAT)
#     payload = "?limit=100&sortBy=closeDate&start=$(strStart)&end=$(strEnd)"
#     result = tradierGet(config, "/accounts/$(accountId)/gainloss$(payload)")
#     gainloss = result["gainloss"]
#     if gainloss != "null"
#         return ensureVector(gainloss["closed_position"])
#     else
#         return []
#     end
# end

end