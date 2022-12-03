module SpyLoader
using BaseTypes, SmallTypes
using DateUtil, LogUtil
import SqlLoader
import HistSpy as hspy

# using CSV, Tables
# function loadSample()
#     csvPath = "C:/data/market/optionsdx/spy_sample-1.csv"
#     cnt = SqlLoader.loadCsv(hspy.db(), csvPath, tbl; checkTypeRows=10000, dropCols=[2,3,4,6])
#     hspy.run("create unique index if not exists $(tbl)_pk on $(tbl)(QUOTE_UNIXTIME, EXPIRE_UNIX, STRIKE)")
#     println("Spy hist table now has $(cnt) rows")
# end

#=
QUOTE_UNIXTIME,UNDERLYING_LAST,EXPIRE_UNIX,DTE,
C_DELTA,C_GAMMA,C_VEGA,C_THETA,C_RHO,C_IV,
C_VOLUME,C_LAST,C_SIZE,C_BID,C_ASK,
STRIKE,
P_BID,P_ASK,P_SIZE,P_LAST,
P_DELTA,P_GAMMA,P_VEGA,P_THETA,P_RHO,P_IV,
P_VOLUME,STRIKE_DISTANCE,STRIKE_DISTANCE_PCT
=#

function dropTables()
    hspy.close!()
    hspy.db()
    hspy.exec("drop table if exists Call")
    hspy.exec("drop table if exists Put")
    hspy.exec("drop table if exists Under")
end

function createTables()
    colSpec = """
        (
            ts int not null,
            expir int not null,
            strike int not null,
            bid int not null,
            ask int not null,
            last int,
            vol int not null,
            delta real,
            gamma real,
            vega real,
            theta real,
            rho real,
            iv real,
            primary key(ts, expir, strike)
        ) strict
    """
    hspy.exec("create table if not exists Call" * colSpec)
    hspy.exec("create table if not exists Put" * colSpec)
    hspy.exec("create table if not exists Under (ts int not null primary key, under int not null) strict")
end

function pragmasSpeed()
    hspy.exec("PRAGMA journal_mode = OFF")
    hspy.exec("PRAGMA synchronous = 0")
    hspy.exec("PRAGMA cache_size = 1000000")
    hspy.exec("PRAGMA locking_mode = EXCLUSIVE")
    hspy.exec("PRAGMA temp_store = MEMORY")
end

function load()
    LogUtil.resetLog(:spyloader)
    # dropTables()
    # createTables()
    timeStart = time()
    println("Started: ", nowz())
    hspy.db()
    pragmasSpeed()
    stmtCall = hspy.prep("insert or ignore into Call (ts, expir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
    stmtPut = hspy.prep("insert or ignore into Put (ts, expir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv) values (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)")
    stmtUnder = hspy.prep("insert or ignore into under values (?, ?)")
    try
        csvPaths = ["C:/data/market/optionsdx/spy_15x_20220$(month).txt" for month in 1:8]
        for csvPath in csvPaths
            println("Loading $(csvPath)...")
            line, itr = Iterators.peel(Iterators.drop(eachline(csvPath), 1))
            tsPrev = procLine(line, 0, stmtCall, stmtPut, stmtUnder)
            i = 1
            for line in itr
                tsPrev = procLine(line, tsPrev, stmtCall, stmtPut, stmtUnder)
                i += 1
                if i % 10000 == 0
                    println(i)
                    # break
                end
                # break
            end
            println("Finished loading $(csvPath)")
        end
    finally
        hspy.close!.((stmtCall, stmtPut, stmtUnder))
    end
    timeEnd = time()
    println("End: ", nowz())
    println("Elapsed seconds: ", timeEnd - timeStart)
    return first(hspy.select("select (select count(*) as cnt from call) as calls, (select count(*) as cnt from put) as puts, (select count(*) as cnt from under) as unders"))
end

function procLine(line, tsPrev, stmtCall, stmtPut, stmtUnder)
    try
        left = 1
        right = findnext(',', line, left)
        ts = parsem(Int, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        left = right+1 ; right = findnext(',', line, left)
        left = right+1 ; right = findnext(',', line, left)
        left = right+1 ; right = findnext(',', line, left)
        under = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        left = right+1 ; right = findnext(',', line, left)
        xpir = parsem(Int, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        left = right+1 ; right = findnext(',', line, left)
        cDelta = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cGamma = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cVega = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cTheta = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cRho = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cIv = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cVol = parsem(Float64, SubString(line, left:right-1), 0.0)
        left = right+1 ; right = findnext(',', line, left)
        cLast = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left) # skip size
        left = right+1 ; right = findnext(',', line, left)
        cBid = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        cAsk = parsem(Currency, SubString(line, left:right-1))

        left = right+1 ; right = findnext(',', line, left)
        strike = parsem(Currency, SubString(line, left:right-1))

        left = right+1 ; right = findnext(',', line, left)
        pBid = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pAsk = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left) # skip size
        left = right+1 ; right = findnext(',', line, left)
        pLast = parsem(Currency, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pDelta = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pGamma = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pVega = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pTheta = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pRho = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pIv = parsem(Float64, SubString(line, left:right-1))
        left = right+1 ; right = findnext(',', line, left)
        pVol = parsem(Float64, SubString(line, left:right-1), 0.0)

        if cBid > cAsk
            @log spyloader ("WARN: Swapping bid/ask because cBid > cAsk: $(cBid) > $(cAsk) for line:\n|", line, '|')
            tmp = cAsk
            cAsk = cBid
            cBid = tmp
        end

        if pBid > pAsk
            @log spyloader ("WARN: Swapping bid/ask because pBid > pAsk: $(pBid) > $(pAsk) for line:\n|", line, '|')
            tmp = pAsk
            pAsk = pBid
            pBid = tmp
        end

        if xpir < ts
            @log spyloader ("WARN: xpir < ts: $(xpir) < $(ts) for line:\n|", line, '|')
        end

        if !(1.0 < strike < 1000.0)
            @log spyloader ("WARN: invalid strike: $(strike) for line:\n|", line, '|')
        end

        if !ismissing(cBid) && !ismissing(cAsk)
            hspy.run(stmtCall, (ts, xpir, strike, cBid, cAsk, cLast, cVol, cDelta, cGamma, cVega, cTheta, cRho, cIv))
        else
            @log spyloader ("skipped call ($(strike)):\n", line)
        end
        if !ismissing(pBid) && !ismissing(pAsk)
            hspy.run(stmtPut, (ts, xpir, strike, pBid, pAsk, pLast, pVol, pDelta, pGamma, pVega, pTheta, pRho, pIv))
        else
            @log spyloader ("skipped put ($(strike)):\n", line)
        end

        if ts != tsPrev
            hspy.run(stmtUnder, (ts, under))
        end
        return ts
    catch e
        println(line)
        rethrow(e)
    end
end

parsem(type, s, mis=missing) = isempty(strip(s)) ? mis : parse(type, s)
parsem(type::Type{Currency}, s, mis=missing) = isempty(s) ? mis : Int(parse(type, s))

# function toQuotePair(nt::NamedTuple)::Union{NTuple{2,OptionQuote},Tuple{OptionQuote}}
#     xpir = Date(fromUnix(nt.EXPIRE_UNIX))
#     if !ismissing(nt.C_BID) # || ismissing(nt.C_ASK)
#         # println(nt)
#         c = OptionQuote(
#             Option(Style.call, xpir, nt.STRIKE),
#             Quote(Action.open, nt.C_BID, nt.C_ASK),
#             OptionMeta(nt.C_DELTA, nt.C_THETA, 0.0, nt.C_VEGA, nt.C_RHO, nt.C_GAMMA, nt.C_IV, nt.C_IV, nt.C_IV),
#             nothing
#         )
#     end
#     if !ismissing(nt.P_BID) # || ismissing(nt.P_ASK)
#         # println(nt)
#         p = OptionQuote(
#             Option(Style.put, xpir, nt.STRIKE),
#             Quote(Action.open, nt.P_BID, nt.P_ASK),
#             OptionMeta(nt.P_DELTA, nt.P_THETA, 0.0, nt.P_VEGA, nt.P_RHO, nt.P_GAMMA, nt.P_IV, nt.P_IV, nt.P_IV),
#             nothing
#         )
#     end
#     return (c, p)
# end

end