module SeekingAlpha
using Dates, Tables, HTTP, JSON3
# import XLSX
using BaseTypes
using DateUtil, FileUtil, DictUtil

export sa
const sa = @__MODULE__

# Covered Calls:
# Collab: ASRT(3)-11/18, CODX(7)-alert, OPEN(5)-11/18
# TestStrat: WKHS(4)-1/20, CLOV(3)-alert, NVTA(5.5)-alert, FSR(9)-10/7, TSP(10)-10/21, NNVC(2.5)-10/21
# 401k: UAL, HLF(19), LUMN(7), AVIR(7.5)-12/16, AHT(9/30/2022,9)-10/14, CBIO(2.5)-alert

ActiveSyms = [] # ["BHC","BLUE","CLNN","CLOV","CTIC","INVZ","NKLA","NNVC","NVTA","PAYO","SENS","TSP","WVE","FSR","WKHS","WTI","DNMR"]

BadPricing = []#"BAX", "OLN"]
Ignore = ["SNDL","YANG","MUX","QD","RIOT","GOTU","TAL","ACB","HUT","IQ","JMIA","ABEV","JDST","SOXS","FAZ",
          "SQQQ","SPXU","QID","SDOW","TWM","NINE","TECS","DUST","SPXS","SRTY","ZSL","DRV","TZA","DIG","UNG",
          "URA","EDZ","EPV","BNO","IEZ","TBT","SDS","XME","TUR","DXD","ECH","TYO","VIXM","ERX","IAU","WEAT",
          "EWZ","FLBR","FCG","AMLP","BKLN","LABD","SRS","DBO","IYE","USFR","ITB","COPX","UCO","AHT","ARR",

          "AFRM","CAN","SRAD","SYF","PRPH","WB","BROS","YY","RLX","RUM","AMPL","PTLO","TLRY","HLF","SGHC","YOU","MPW", # don't like company
          "GRPH","NKTX","RGNX","ITOS","EQRX", # clinical stage
          "PCH", # something wrong with pricing
          "RKT","BBW","ACRX","REI","FRSH","GPRO","QTRX","QFIN","UWMC","ENOV","FLGT","CODX","SB","EGY","ZIM","VIR" # maybe reconsider someday
]
# Early stage clinical: "AVIR"
IgnoreTemp = []#"CORZ","CLSK"]
isGlobalIgnore(sym) = sym in vcat(BadPricing, Ignore, IgnoreTemp, ActiveSyms)

const BaseDir = mkpath(joinpath("C:/Users/joel/Downloads", "journey"))
const BaseDirData = mkpath(joinpath(BaseDir, "data"))
const BaseDirStatic = mkpath(joinpath(BaseDir, "static"))

import OutputUtil
function disp(syms)
    OutputUtil.pretyble(Iterators.map(syms) do s
        (;sym=s, rating=getRating(s), range=rangeRatio(Quotes[s]))
    end)
end

function forNeutral()
    loadWeeklys()
    loadPennys()
    syms = intersect(keys(Weeklys), keys(Pennys))
    println("weekly, penny count: ", length(syms))
    loadQuotes(syms)
    syms = filter(filterNeutral, syms)
    println("weekly, penny, quoted, filtered count: ", length(syms))
    getMetrics(syms)

    global NeutLong = filter(syms) do s
        q = Quotes[s]
        rating = getRating(s)
        return !isnothing(rating) && rating > 4.5 # && rangeRatio(q) < 0.5
    end
    global NeutShort = filter(syms) do s
        q = Quotes[s]
        rating = getRating(s)
        return !isnothing(rating) && rating < 1.5 # && rangeRatio(q) > 0.5
    end
    return (NeutLong, NeutShort)
end

NoPriceFor = String[]
function filterNeutral(s)
    haskey(Quotes, s) || ( println("No quote for ", s, ", skipping.") ; return false )
    q = Quotes[s]
    p = toPrice(q)
    if isnothing(p)
        println("No price for ", s, ", skipping.")
        push!(NoPriceFor)
        return false
    else
        return q["type"] == "stock" && q["average_volume"] > 100000
    end
end

toPrice(q) = q["last"]
function rangeRatio(q)
    hi = q["week_52_high"]
    lo = q["week_52_low"]
    return (toPrice(q) - lo) / (hi - lo)
end
function getRating(s)
    m = Data[:metrics]
    haskey(m, s) || ( println("WARN: no metrics for ", s) ; return )
    d = m[s]
    haskey(d, "quant_rating") || ( println("no quant rating for ", s) ; return )
    return d["quant_rating"]
end

function getCandidates(;maxSyms=-1)
    quoteAll()
    global syms = filter(filterCandidateQuotes, keys(Quotes))
    if maxSyms > 0
        println("Restricting ", length(syms), " to ", maxSyms)
        syms = collect(Iterators.take(syms, maxSyms))
    end
    getMetrics(syms)
    getGrades(syms)
    loadDividends(syms)
    loadEarnings(syms)

    global res = filter(syms) do s
        gs = get(Data[:grades], s, nothing)
        grade = !isnothing(gs) && (checkGrade(gs, "value_category", 5) && checkGrade(gs, "profitability_category", 5)) # && checkGrade(gs, "growth_category", 9))
        return grade && DictUtil.safeKeys(Data, 0.0, :metrics, s, "quant_rating") > 2.6
    end
    return res
end

checkGrade(gs, cat, lte) = haskey(gs, cat) ? gs[cat] <= lte : false

function quoteWeeklys()
    loadWeeklys()
    loadQuotes(keys(Weeklys))
end

function quoteAll()
    loadOptionables()
    loadQuotes(keys(Optionables))
end

# function qsyms()
#     return filter(s -> Quotes[s]["average_volume"] > 100000 && Quotes[s]["prevclose"] < 12.5, keys(Quotes))
# end

filtWeeklys(s::AStr) = haskey(Weeklys, s)
function filterCandidateQuotes(s::AStr)
    if isnothing(Quotes[s]["prevclose"])
        println("No prevclose for ", s, ", skipping.")
        return false
    else
        q = Quotes[s]
        bid = q["bid"]
        return q["type"] == "stock" &&
            !isnothing(bid) && 1.75 <= bid <= 57.0 &&
            q["average_volume"] > 100000 &&
            # q["prevclose"] < 57.0 &&
            rangeRatio(q) <= .67
    end
end



#region SaOld
# const Combined = Dict{String,Dict{Symbol,Any}}()
# const Syms = String[]
# const Cands = Dict{String,Dict{Symbol,Any}}()

# function getCandSyms(filt=filt1; up=false)
#     loadWeeklys()
#     loadSa()
#     empty!(Cands)
#     empty!(Syms)
#     merge!(Cands, filter(filt, Combined))
#     append!(Syms, keys(Cands))
#     sort!(Syms)
#     return Syms
# end

# RequirePass = (:summaryValuation, :summaryProfitability) # :Growth, :Momentum
# function filt1(kv)
#     filtWeeklys(kv) || return false
#     val = kv[2]
#     val[:summaryQuant] >= 2.5 || return false
#     for req in RequirePass
#         gradeAtAbove(val[req], "C-") || return false
#     end
#     grades = gradeAtAbove(val[:dividendsSafety], "C") &&
#                 # gradeAtAbove(nt.DivGrowth, "C") &&
#                 gradeAtAbove(val[:dividendsYield], "C-") &&
#                 gradeAtAbove(val[:dividendsConsistency], "C")
#     return grades
#     return true
# end

# const Pages = Dict{String,Dict{String,Dict{Symbol,Any}}}()
# const PageNames = ["summary", "dividends", "earnings"]
# function loadSa(;up=false)
#     !up || resetSa()

#     xlfn = sort!(filter!(x -> occursin("sa-" * PageNames[1], x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]
#     if unix2datetime(mtime(xlfn)) < (now(UTC) - Hour(8))
#         error("Sa file not downloaded today. Go to: https://seekingalpha.com/screeners/922a27ae98-To-Download")
#     end
#     isempty(Combined) || return
#     println("Loading sa")

#     for pageName in PageNames
#         xlfn = sort!(filter!(x -> occursin("sa-" * pageName, x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]

#         XLSX.openxlsx(xlfn; mode="rw") do xf
#             sheet = xf[1]
#             if !startswith(sheet[1,1], pageName)
#                 for i in 1:100
#                     x = sheet[1,i]
#                     !ismissing(x) || break
#                     sheet[1,i] = pageName * filter(!isspace, x)
#                 end
#             end
#             table = rowtable(XLSX.gettable(sheet))
#             Pages[pageName] = Dict{String,Dict{String,Any}}()
#             toDict!(Pages[pageName], table, Symbol(pageName * "Symbol"))
#         end
#     end

#     for sym in keys(Pages[PageNames[1]])
#         isnothing(findfirst(x -> !haskey(Pages[x], sym), PageNames[2:end])) || continue
#         Combined[sym] = Dict(Iterators.flatten(map(x -> x[sym], values(Pages))))
#     end
# end
#endregion

#region PenWek
const Optionables = Dict{String,AVec}()
const Weeklys = Dict{String,AVec}()
const Pennys = Dict{String,AVec}()

function loadOptionables()
    length(Optionables) < 5000 || return
    println("Loading optionables")
    fn = joinpath(BaseDirStatic, "optionables.tsv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(1))
        url = "https://marketdata.theocc.com/delo-download?prodType=ALL&downloadFields=US;OS;SN;EXCH;PL;ONN&format=txt"
        println("## downloading optionables ##\n", url)
        HTTP.download(url, fn)
    end
    rows = FileUtil.readTsv(fn, String; header=false)
    procOcc!(Optionables, rows, 2)
end

function loadWeeklys()
    isempty(Weeklys) || return
    println("Loading weeklys")
    fn = joinpath(BaseDirStatic, "weeklys.csv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(1))
        url = "https://marketdata.theocc.com/weekly-options?action=download"
        println("## downloading weeklys ##\n", url)
        HTTP.download(url, fn)
    end
    loadOccCsv!(Weeklys, fn, 1, 3)
end

const DF_PENNYS = dateformat"yyyymmdd"
function loadPennys()
    isempty(Pennys) || return
    println("Loading pennies")
    fn = joinpath(BaseDirStatic, "pennys.csv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(2))
        strDate = Dates.format(lastTradingDay(lastdayofmonth(today() - Month(1))), DF_PENNYS)
        url = "https://marketdata.theocc.com/penny-program?fileName=$(strDate)&reportType=rolling"
        println("## Downloading pennys ##\n", url)
        HTTP.download(url, fn)
    end
    loadOccCsv!(Pennys, fn, 2, 1)
end
#endregion

#region SeekingAlpha
SaSymsPerPage = 500

SaHeaders = [
    "content-type" => "application/json",
    "cookie" => """## REMOVED PRIVATE ##"""
    "accept" => "*/*",
    "accept-language" => "en-US,en;q=0.9",
    "referer" => "https://seekingalpha.com/screeners/922a27ae98-To-Download",
    "authority" => "seekingalpha.com",
    "origin" => "https://seekingalpha.com",
    "sec-ch-ua" => "\".Not/A)Brand\";v=\"99\", \"Google Chrome\";v=\"103\", \"Chromium\";v=\"103\"",
    "sec-ch-ua-mobile" => "?0",
    "sec-ch-ua-platform" => "\"Windows\"",
    "sec-fetch-dest" => "empty",
    "sec-fetch-mode" => "cors",
    "sec-fetch-site" => "same-origin",
    "user-agent" => "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36",
    "accept-encoding" => "deflate, gzip, br, zstd"
    # "accept-encoding" => "gzip"
]

Data = Dict{Symbol,Dict{String,Dict{String,Any}}}(
    :metrics => Dict{String,Dict{String,Any}}(),
    :grades => Dict{String,Dict{String,Any}}()
)
DataLookup = Dict{Symbol,Dict{String,String}}(
    :metrics => Dict{String,Dict{String,Any}}(),
    :grades => Dict{String,Dict{String,Any}}()
)
DataNotFound = Dict{Symbol,Vector{String}}(
    :metrics => String[],
    :grades => String[]
)

getMetrics(syms) = getData(:metrics, syms)
getGrades(syms) = getData(:grades, syms)
function getData(type, syms)
    dict = Data[type]
    lookup = DataLookup[type]
    notFound = DataNotFound[type]
    !isempty(dict) || loadData(type)
    filter!(!isGlobalIgnore, syms)
    global todo = String[]
    for s in syms
        !(s in notFound) || continue
        if !haskey(dict, s) || !haskey(dict[s], "tsUpdate") || DateTime(dict[s]["tsUpdate"]) < now(UTC) - Hour(12)
            push!(todo, s)
        end
    end
    if !isempty(todo)
        println("Loading $(length(todo)) $(type)")
        for s in todo
            delete!(dict, s)
        end
        for part in Iterators.partition(todo, 150)
            global procJson = loadData(type, part)
            procDict(dict, lookup, procJson)
            skipped = filter(s -> !haskey(dict, s), part)
            isempty(skipped) || println("skipped: ", skipped)
            # error("stop")
            append!(notFound, skipped)
            unique!(notFound)
            saveData(type)
        end
    end
end

MetricNames = [
    "marketcap_display","dividend_yield","quant_rating","authors_rating","sell_side_rating",
    "last_div_date","div_pay_date","div_yield_fwd","div_yield_4y","div_rate_ttm","div_rate_fwd","payout_ratio","payout_ratio_4y","div_grow_rate3","div_grow_rate5","dividend_growth",
    "earning_announce_date"]

GradeNames = ["value_category","growth_category","profitability_category","momentum_category","eps_revisions_category",
                "div_safety_category","div_growth_category","div_yield_category","div_consistency_category"]

function makeUrl(type::Symbol, syms)
    if type == :metrics
        url = "https://seekingalpha.com/api/v3/metrics?filter[fields]=" * join(MetricNames, "%2C") * "&filter[slugs]=" * join(syms, "%2C")
    elseif type == :grades
        url = "https://seekingalpha.com/api/v3/ticker_metric_grades?filter[fields]=" * join(GradeNames, "%2C") *
            "&filter[slugs]=" * join(syms, "%2C") *
            "&filter[algos][]=etf&filter[algos][]=dividends&filter[algos][]=main_quant&filter[algos][]=reit&filter[algos][]=reit_dividend"
    end
    return url
end

function loadData(type::Symbol, syms)
    @assert length(syms) <= 150
    println("Downloading $(type) start sym $(first(syms))")
    resp = HTTP.get(makeUrl(type, syms), SaHeaders; decompress=true)
    json = procResp(resp)
    return json
end

# NotFoundGrades = String[]
# NotFoundMetrics = String[]
# function getMetrics(syms)
#     !isempty(SaMetrics) || loadMetrics()
#     filter!(!isGlobalIgnore, syms)
#     todo = String[]
#     for s in syms
#         !(s in NotFoundMetrics) || continue
#         if !haskey(SaMetrics, s) || !haskey(SaMetrics[s], "tsUpdate") || DateTime(SaMetrics[s]["tsUpdate"]) < now(UTC) - Hour(12)
#             push!(todo, s)
#         end
#     end
#     if !isempty(todo)
#         println("Loading $(length(todo)) metrics")
#         for part in Iterators.partition(todo, 150)
#             println(part)
#             global procJson = loadMetrics(part)
#             procDict(SaMetrics, SaMetricToId, procJson)
#             skipped = filter(s -> !haskey(SaMetrics, s), syms)
#             append!(NotFoundMetrics, skipped)
#             saveMetrics()
#         end
#     end
# end

pathFor(type::Symbol) = joinpath(BaseDirData, string(type) * ".json")
pathForLookup(type::Symbol) = joinpath(BaseDirData, string(type) * "-lookup.json")
pathForNotFound(type::Symbol) = joinpath(BaseDirData, string(type) * "-not-found.json")

function loadData(type)
    path = pathFor(type)
    if isfile(path)
        empty!.((Data[type], DataLookup[type], DataNotFound[type]))
        merge!(Data[type], loadJson(path))
        merge!(DataLookup[type], loadJson(pathForLookup(type)))
        append!(DataNotFound[type], loadJson(pathForNotFound(type), Vector{String}))
    end
end

function saveData(type::Symbol)
    writeJson(pathFor(type), Data[type])
    writeJson(pathForLookup(type), DataLookup[type])
    writeJson(pathForNotFound(type), DataNotFound[type])
end

function procDict(store, lookup, dict)
    for d in dict["included"]
        if d["type"] == "metric_type"
            lookup[d["attributes"]["field"]] = d["id"]
            lookup[d["id"]] = d["attributes"]["field"]
        elseif d["type"] == "ticker"
            addsym(d)
        end
    end
    for d in dict["data"]
        if !haskey(d["relationships"]["ticker"]["data"], "id")
            @error "no id key" d
            error("no id key")
        end
        tickerName = SymLookup[d["relationships"]["ticker"]["data"]["id"]]["name"]
        metricName = lookup[d["relationships"]["metric_type"]["data"]["id"]]
        attrs = d["attributes"]
        value = haskey(attrs, "value") ? attrs["value"] : attrs["grade"]
        to = useKey(Dict{String,Any}, store, tickerName)
        to[metricName] = value
        to["tsUpdate"] = string(now(UTC))
    end
    return length(dict["data"])
end

const SymLookup = Dict{String,Dict{String,Any}}()
function addsym(d)
    attrs = d["attributes"]
    val = Dict("id" => d["id"], "slug" => attrs["slug"], "name" => attrs["name"])
    SymLookup[d["id"]] = val
    SymLookup[attrs["name"]] = val
end
#endregion

#region util
const Grades = Dict{String,Int}("A+"=>1, "A"=>2, "A-"=>3, "B+"=>4, "B"=>5, "B-"=>6, "C+"=>7, "C"=>8, "C-"=>9, "D+"=>10, "D"=>11, "D-"=>12, "F"=>13)
# gradePass(x) = !(occursin("D", x) || occursin("F", x)) # x in
gradeAtAbove(x, g) = haskey(Grades, x) && Grades[x] <= Grades[g]

function loadOccCsv!(dict, fn, col, skipstart)
    rows = readCsv(fn; header=true, skipstart)[1]
    procOcc!(dict, rows, col)
end

function procOcc!(dict, rows, col)
    for row in eachrow(rows)
        # @assert isValue(row[col])
        try
        crow = cleanRow(row)
        sym = crow[col]
        !isGlobalIgnore(sym) || continue
        dict[sym] = crow
        catch e
            @error "procOcc" row
            rethrow(e)
        end
    end
end

cleanRow(row) = map(row) do x
    x isa AStr ? strip(x) : x
end

toDict!(dict::Dict{String,Dict{Symbol,Any}}, table::Vector{<:NamedTuple}, keySym::Symbol) = for row in table
    dict[row[keySym]] = Dict(pairs(row))
end

isValue(::Nothing) = false
isValue(::Missing) = false
isValue(f::Float64) = true
isValue(s::AStr) = !isempty(s) && !isnothing(findfirst(!isspace, s))

using TranscodingStreams, CodecZlib
function procResp(resp)
    global raw = resp.body
    # content = transcode(CodecZlib.GzipDecompressor, raw)
    content = String(raw)
    json = JSON3.read(content, Dict)
    if !haskey(json, "data")
        @error "no data key" content
        error("no data key")
    end
    return json
end

allSlugs() = unique!(sort!(map(x -> x["slug"], values(SaSyms))))
allSyms() = unique!(sort!(map(x -> x["name"], values(SaSyms))))
#endregion

#region Tradier
using TradierData
const TradSymNotFound = Dict{String, DateTime}()

const Quotes = Dict{String,Dict{String,Any}}()
function loadQuotes(src)
    global symsAll = filter(x -> !haskey(Quotes, x) && !haskey(TradSymNotFound, x), src)
    println("Loading $(length(symsAll)) quotes")
    for syms in Iterators.partition(symsAll, 1000)
        # "DQ" in syms || continue
        # println("loading ", syms)
        # global lll = syms
        raw = tradierQuotes(syms)
        unmatched = haskey(raw, "unmatched_symbols") ? raw["unmatched_symbols"]["symbol"] : []

        if haskey(raw, "quote")
            quotes = raw["quote"]
            for q in quotes
                Quotes[q["symbol"]] = q
            end
            diff = setdiff(syms, map(x->x["symbol"], quotes))
            len = length(syms) - length(quotes)
            @assert len == length(diff) == length(unmatched) "Different lengths $(len) $(length(diff)) $(length(unmatched))"
            println("not found ", diff)
        else
            println("no quotes found, unmatched: ", unmatched)
        end
        n = now(UTC)
        for sym in unmatched
            TradSymNotFound[sym] = n
        end
    end
end

const Dividends = Dict{String,Any}()
function loadDividends(src)
    symsAll = filter(x -> !haskey(Dividends, x) && !haskey(TradSymNotFound, x), src)
    println("Loading $(length(symsAll)) dividends")
    for syms in Iterators.partition(symsAll, 100)
        res = tradierDividends(syms)
        merge!(Dividends, res)
    end
end

const Earnings = Dict{String,Any}()
function loadEarnings(src)
    symsAll = filter(x -> !haskey(Earnings, x) && !haskey(TradSymNotFound, x), src)
    println("Loading $(length(symsAll)) earnings")
    for syms in Iterators.partition(symsAll, 100)
        res = tradierCorpCal(syms)
        merge!(Earnings, res)
    end
end
#endregion

end


# const SaSyms = Dict{String,Dict{String,Any}}()
#region FixThis
# const SaSymsRaw = Dict{Int,Dict{String,Any}}()
# function loadSaSyms(page)
#     path = joinpath(BaseDirSaSymsRaw, "$(page).json")
#     isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

#     sl = 2 + 4*rand()
#     println("Downloading SaSyms page $(page) after $(sl) seconds")
#     sleep(sl)
#     url = "https://seekingalpha.com/api/v3/screener_results"
#     body = """{"filter":{"close":{"gte":0.01,"disabled":false}},"quant_rank":true,"sort":null,"type":"stock","per_page":$(SaSymsPerPage),"page":$(page)}"""
#     resp = HTTP.post(url, SaHeaders; body, verbose=2)
#     json = procResp(resp)
#     writeJson(path, json)
#     return json
# end

# function loadSaSyms()
#     # SaSymsRaw[1] = loadSaSyms(1)
#     # numCalls = div(count, SaSymsPerPage) + 1
#     proced = 0
#     for page in 1:1000
#         json = loadSaSyms(page)
#         SaSymsRaw[page] = json
#         count = SaSymsRaw[page]["meta"]["count"]
#         for d in json["data"]
#             addsym(d)
#             proced += 1
#         end
#         println("proced ", proced)
#         proced < count || break
#     end
# end
# #endregion
