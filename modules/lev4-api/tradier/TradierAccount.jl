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
    resp = tradierGet("/accounts/$(getAccountId())/orders/$(oid)?includeTags=true", Call(nameof(var"#self#")))
    return get(resp, "order") do
        error("No order key in tradierOrder response ", res)
    end
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

import HTTP:WebSockets
import Sockets

const listeners = Vector{Function}()
const websocket = Ref{Union{Nothing,WebSockets.WebSocket}}(nothing)

#region AccountStreamingPublic
function ensure_listening()
    if isnothing(websocket[])
        start_listening()
    else
        check_websocket()
    end
    return
end

function start_listening()
    sid, url = start_session()
    Threads.@spawn WebSockets.open(url) do ws
        Sockets.send(ws, """{ "events": ["order"], "sessionid": "$(sid)", "excludeAccounts": [] }""")
        websocket[] = ws
        listening(receive_msg, ws)
    end
    yield()
    i = 0
    while isnothing(websocket[]) || WebSockets.isclosed(websocket[])
        sleep(0.1)
        i += 1
        i < 20 || error("Timed out waiting for websocket setup")
    end
    return
end

function stop_listening()
    if !isnothing(websocket[])
        close(websocket[])
    end
    websocket[] = nothing
    empty!(listeners)
    return
end

function register_listener(listener)
    push!(listeners, listener)
end

function unregister_listener(listener)
    ind = findfirst(x -> x === listener, listeners)
    isnothing(ind) || deleteat!(listeners, ind)
end
#endregion

#region AccountStreamLocal
import JSON3
function listening(callback, ws)
    try
        for msg in ws
            try
                # println("TradierAccount.listening msg receieved: $(msg)")
                d = JSON3.read(msg)
                typ = get(d, "event", nothing)
                if typ == "order"
                    callback(d)
                end
            catch e
                showerror(stderr, e)
            end
        end
    catch e
        if e isa EOFError
            # ignore
        else
            showerror(stderr, e)
        end
    end
    # println("TradierAccount.listening() returning")
    websocket[] = nothing
    return
end

function receive_msg(dict)
    for x in listeners
        x(dict)
    end
end

function start_session()
    resp = tradierPost("/accounts/events/session", "", Call(nameof(var"#self#")))
    stream = get(resp, "stream") do
        println("result = ", result)
        # error("No orders key in response for tradierOrders.")
        println("WARN: No orders key in response for tradierOrders.")
    end
    sid = stream["sessionid"]
    url = stream["url"]
    return (;sid, url)
end

function check_websocket()
    if WebSockets.isclosed(websocket[])
        websocket[] = nothing
        start_listening()
    end
end
#endregion

end