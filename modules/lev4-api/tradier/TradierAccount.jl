module TradierAccount
using CollUtil
using BaseTypes, TradierConfig, TradierBase

export tradierOrders, tradierOrder, tradierPositions

function tradierOrders()::TradierRespVec
    result = tradierGet("/accounts/$(getAccountId())/orders?includeTags=true", Call(nameof(var"#self#")))
    orders = result["orders"]
    if orders != "null"
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
    positions = result["positions"]
    if positions != "null"
        return ensureVector(positions["position"])
    else
        return TRADIER_EMPTY
    end
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

# function tradierBalances(config::TradierConfig=cfg(), accountId::AbstractString=config.accountId)
#     return tradierGet(config, "/accounts/$(accountId)/balances")["balances"]
# end

end