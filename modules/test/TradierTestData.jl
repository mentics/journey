module TradierTestData
using DateUtil, DictUtil, TradierUtil
using SH, BaseTypes, SmallTypes, OptionTypes

export mockSubmitOrder, mockOrder

mockSubmitOrder(oid) = Dict{String, Any}("order" => Dict{String, Any}("partner_id" => "3a8bbee1-5184-4ffe-8a0c-294fbad1aee9", "status" => "ok", "id" => oid))

function mockOrder(tid, act, oid, ms, status, lqs, primitDir, isMkt=false, prillDir=getBid.(lqs)) # fill(C(0.0),length(lqs))
    if length(lqs) == 1
        return mockSingle(tid, act, oid, ms, status, lqs[1], primitDir, prillDir[1], isMkt)
    end
    dt = tier.toDateStr(ms)
    ordType = isMkt ? "market" : tier.orderType(primitDir)
    return jsonToDict("""
{"orders":{"order":[{
      "last_fill_price": 0,
                  "leg": [
$(join([mockLeg(act, oid+i, dt, status, ordType, lq, primitDir, prillDir[i]) for (i, lq) in enumerate(lqs)], ','))
                         ],
                "price": $(abs(primitDir)),
          "create_date": "$(dt)",
             "duration": "day",
       "avg_fill_price": $(sum(prillDir)),
               "status": "$(status)",
             "num_legs": 4,
                   "id": $(oid),
   "remaining_quantity": 0,
             "quantity": 4,
                "class": "multileg",
               "symbol": "SPY",
             "strategy": "condor",
        "exec_quantity": 4,
                 "side": "buy",
   "last_fill_quantity": 1,
     "transaction_date": "$(dt)",
                 "type": "$(ordType)",
                 "tag": "$(tag(act, tid))"
}]}}
""")
end

function mockSingle(tid, act, oid, ms, status, lq, primitDir, prillDir, isMkt=false)
    dt = tier.toDateStr(ms)
    ordType = isMkt ? "market" : "limit"
    @assert checkDirTrade(act, getSide(lq), prillDir)
    # TODO: not sure if price is abs or dir
    return jsonToDict("""
{"order":{
    "last_fill_price": $(status == "filled" ? abs(prillDir) : 0.0),
                "price": $(abs(primitDir)),
        "create_date": "$(dt)",
            "duration": "day",
        "avg_fill_price": $(status == "filled" ? abs(prillDir) : 0.0),
                "status": "$(status)",
                    "id": $(oid),
        "option_symbol": "$(tier.optToOcc(getOption(lq)))",
    "remaining_quantity": 0,
            "quantity": 1,
                "class": "option",
                "symbol": "SPY",
        "exec_quantity": 1,
                "side": "$(tier.toTierSide(act, getSide(lq)))",
    "last_fill_quantity": 1,
    "transaction_date": "$(dt)",
                "type": "$(ordType)",
                "tag": "$(tag(act, tid))"
}}
""")
end

function mockLeg(act, olid, dt, status, ordType, lq, primitDir, prillDir)
    @assert checkDirTrade(act, getSide(lq), prillDir) "checkDirTrade($(act), $(getSide(lq)), $(prillDir))"
    return """
{
    "last_fill_price": $(status == "filled" ? abs(prillDir) : 0.0),
                "price": $(abs(primitDir)),
        "create_date": "$(dt)",
            "duration": "day",
        "avg_fill_price": $(status == "filled" ? abs(prillDir) : 0.0),
                "status": "$(status)",
                    "id": $(olid),
        "option_symbol": "$(tier.optToOcc(getOption(lq)))",
    "remaining_quantity": 0,
            "quantity": $(getQuantity(lq)),
                "class": "option",
                "symbol": "SPY",
        "exec_quantity": 1,
                "side": "$(tier.toTierSide(act, getSide(lq)))",
    "last_fill_quantity": 1,
    "transaction_date": "$(dt)",
                "type": "$(ordType)"
}
"""
end

tag(act, tid) = (act == Action.open ? "op" : "cl") * string(tid)

end