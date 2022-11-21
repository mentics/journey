module TestProcess
using Test
#TestSetExtensions
using Globals, TestUtil, CollUtil, DateUtil, FileUtil, StoreUtil, CalcUtil
using SH, BaseTypes, SmallTypes, OptionTypes, StratTypes, StatusTypes, TradeTypes, LegMetaTypes, ChainTypes
using Strats, Trading
using TradierConfig
using OrderWatching, Store, StoreTrade, Scoring
using ProbHist, Snapshots, Chains, Markets, Backups
using TradierTestData
using ProcOrder
using Shorthand
using RunStrats, CmdStrats, CmdTrading, CmdExplore

TEST_PATH_DB = "C:/data/test/testProcess.db"
TEST_BASE_PATH = "C:/data/test"

function setup()
    clearOverride()
    testStart()
    println(intest())
    Backups.setBasePath(TEST_BASE_PATH)
    tenv(:paper)
    OrderWatching.setEnabled(false)
    foreach(filter(fn -> isfile(fn) && endswith(fn, ".json"), readdir(joinpath(TEST_BASE_PATH, "orders"); join=true))) do fn; rm(fn) end
    # useDbTest()
    StoreUtil.resetDbTest()
    # snap(4, 8, 6, 37)
    return
end

function tearDown()
    snop()
    clearOverride()
    Backups.setBasePath()
    testStop()
    tenv(:prod)
    useDbProd()
    OrderWatching.setEnabled(true)
end

function runTests()
    # @testset "Preview" begin
    #     testStart()
    #     clearOverride()
    #     try
    #         an(2; maxRun=12000, keep=1200, scorer=scoreRand, posStrat=Vector{LegRet}())
    #         so(1; at=-18.02)

    #         lrs = sh("l428c1@1 / s447c1@1 / s447c1@1 / l448c1@1")
    #         so(lrs; at=-18.02)

    #         lrs = sh("l428c1@1 / s447c2@1 / l448c1@1")
    #         so(lrs; at=-18.02)
    #     finally
    #         testStop()
    #     end
    # end

    @testset "Process" begin
        @assert startswith(TEST_PATH_DB, 'C')

        setupTeardown(setup, tearDown) do
            @assert intest()

            tenv() != :paper && error("Test must run in paper env")
            an(1; maxRun=12000, keep=1200, noPos=true, scorer=(args...) -> rand())
            @test ar(1) isa Strat
            ph = probs()[1]
            # sortar(byProb(ph))
            # @test calc(ph, 1).prob > calc(ph, 100).prob
            # sortar(byEvr(ph))
            # @test calc(ph, 2).evr > calc(ph, 99).evr
            sa()
            strat1 = find(!isCal, CmdStrats.lastRes[])
            strat2 = find(isCal, CmdStrats.lastRes[])

            @test !StoreTrade.hasTrades()

            #### Ignorable Orders ####
            openTradeMocked(strat1, 9000, "rejected")
            @test !StoreTrade.hasTrades()
            openTradeMocked(strat1, 9010, "canceled")
            @test !StoreTrade.hasTrades()
            openTradeMocked(strat1, 9020, "open")
            @test !StoreTrade.hasTrades()

            #### Trade 1 Vert ####
            trad1id = openTradeMocked(strat1, 1000, "filled")
            trads1 = findTrades(Filled)
            @test length(trads1) == 1
            trad1 = loadTrade(getId(trads1[1]))
            lgs1 = getLegs(trad1)
            @test length(lgs1) == 4
            @test string(trads1[1]) == string(trad1)
            # TODO: verify prices

            ## Close Trade 1 ##
            trad1id2 = closeTradeMocked(trad1, 1010, "filled", PriceT(.59))
            @test isempty(findTrades(Filled))
            @test trad1id2 == trad1id
            return
            # TODO: the above close trade deleted a leg: need to be non-destructive or something
            trad1db = loadTrade(trad1id)
            @test length(getLegs(trad1)) == length(getLegs(trad1db)) == 4
            # @test trad1db == trad1
            # @info "tradFilled" length(getLegs(trad1))
            # @info "tradFilled" length(getLegs(trad1db))

            # trad, oid, tierStat, primitDir, lgsInd=nothing, isMkt=false)
            #### Trade 2 Cal ####
            trad2id = openTradeMocked(strat2, 2000, "filled")
            @test length(findTrades(Filled)) == 1
            trad2 = loadTrade(trad2id)

            ## Close Trade 2 ##
            trad2id2 = closeTradeMocked(trad2, 2010, "filled", PriceT(-1.01), 4:4, true)
            @test trad2id2 === trad2id
            @test StoreTrade.findTrades(PartialClosed)[1] == loadTrade(trad2id)
            addOrderMocked(strat2, Action.close, 2020, "filled", 1:2)
            @test StoreTrade.hasTrades(PartialClosed)
            addOrderMocked(strat2, Action.close, 2020, "open", 3:3) # ignored
            @test StoreTrade.hasTrades(PartialClosed) && !StoreTrade.hasTrades(Closing, Closed)
            addOrderMocked(rsub, Action.close, 2030, "canceled", 3:3) # ignored
            @test StoreTrade.hasTrades(PartialClosed) && !StoreTrade.hasTrades(Closing, Closed)
            addOrderMocked(rsub, Action.close, 2040, "filled", 3:3)
            @test !StoreTrade.hasTrades(Starting, Accepted, Filled, PartialClosed) && StoreTrade.hasTrades(Closed)

            # TODO: test expired options, mixed resolutions on trades (some legs fill, some expire), and market sell to close legs that don't fill

            #### Reconcile? ####
            # Verify

            # TODO: Use TradeInfo
            # println(trad1)
        end
    end
end

function openTradeMocked(rsub, oid, tierStat, lgsInd=nothing, pre=false)::Int
    tierStat == "Filled" && error("not supported")
    lqs = isnothing(lgsInd) ? tos(LegMeta, rsub) : tos(LegMeta, rsub)[lgsInd] ; primitDir = PriceT(bap(lqs))
    setOverride(:submitOrder, mockSubmitOrder(oid))
    setOverride(:tradierOrders, mockOrder("tidOpenLast", Action.open, oid, nowMs(), tierStat, lqs, primitDir))
    tid = sor(rsub, primitDir; skipConfirm=true)
    clearOverride()
    return tid
end

function closeTradeMocked(trad, oid, tierStat, primitDir, lgsInd=nothing, isMkt=false)::Int
    tid = getId(trad)
    tierStat == "Filled" && error("not supported")
    lqs = isnothing(lgsInd) ? getLegs(trad) : getLegs(trad)[lgsInd]
    setOverride(:submitOrder, mockSubmitOrder(oid))
    setOverride(:tradierOrders, mockOrder(tid, Action.close, oid, nowMs(), tierStat, lqs, primitDir, isMkt, [copysign(rand(), Int(getSide(leg))) for leg in lqs]))
    ctr(trad, primitDir; lgsInd)
    clearOverride()
    return tid
end

function addOrderMocked(rsub, act, oid, tierStat, lgsInd=nothing, isMkt=false)::Nothing
    lqs = isnothing(lgsInd) ? tos(LegMeta, rsub) : tos(LegMeta, rsub)[lgsInd] ; primitDir = bap(lqs)
    lgprs = tierStat == "filled" ? bap.(lqs) : fill(C(0.0), length(lqs))
    setOverride(:tradierOrders, mockOrder(act, oid, nowMs(), tierStat, lqs, primitDir, isMkt, lgprs))
    OrderWatching.onTimer(nothing)
    clearOverride()
    return
end

calc(ph, i) = calcMetrics(getVals(ph), combineTo(Vals, ar(i)))

#=================
Submit order payload:
Submitting order: tag=1647319044218&class=multileg&symbol=SPY&type=credit&duration=day&price=0.38&option_symbol[0]=SPY220316C00427000&side[0]=sell_to_open&quantity[0]=1&option_symbol[1]=SPY220316C00430000&side[1]=buy_to_open&quantity[1]=1&option_symbol[2]=SPY220316C00433000&side[2]=buy_to_open&quantity[2]=1&option_symbol[3]=SPY220316C00435000&side[3]=sell_to_open&quantity[3]=1

Submit order live response:
Dict{String, Any}("partner_id" => "3a8bbee1-5184-4ffe-8a0c-294fbad1aee9", "status" => "ok", "id" => 1619216)
================#

# noop() = nothing
# export test1
# function test1()
#     @testset ExtendedTestSet "testset1" begin
#         setupTeardown(identity, identity) do
#             @test 1 < 2
#             p1 = 1
#             p2 = 2
#             @test p1 < p2
#             @test p1 < p2
#         end
#     end
# end

end