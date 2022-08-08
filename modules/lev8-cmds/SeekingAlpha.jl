module SeekingAlpha
using Dates, Tables, HTTP
import XLSX
using BaseTypes
using DateUtil, FileUtil

const BaseDir = "C:/Users/joel/Downloads/journey"

# resetSa() = empty!.([Combined, Pages, Weeklys, Pennys, Cands, Syms])

filtWeeklys(x) = haskey(Weeklys, x[1])

#region SaOld
const Combined = Dict{String,Dict{Symbol,Any}}()
const Syms = String[]
const Cands = Dict{String,Dict{Symbol,Any}}()

function getCandSyms(filt=filt1; up=false)
    loadWeeklys()
    loadSa()
    empty!(Cands)
    empty!(Syms)
    merge!(Cands, filter(filt, Combined))
    append!(Syms, keys(Cands))
    sort!(Syms)
    return Syms
end

RequirePass = (:summaryValuation, :summaryProfitability) # :Growth, :Momentum
function filt1(kv)
    filtWeeklys(kv) || return false
    val = kv[2]
    val[:summaryQuant] >= 2.5 || return false
    for req in RequirePass
        gradeAtAbove(val[req], "C-") || return false
    end
    grades = gradeAtAbove(val[:dividendsSafety], "C") &&
                # gradeAtAbove(nt.DivGrowth, "C") &&
                gradeAtAbove(val[:dividendsYield], "C-") &&
                gradeAtAbove(val[:dividendsConsistency], "C")
    return grades
    return true
end

const Pages = Dict{String,Dict{String,Dict{Symbol,Any}}}()
const PageNames = ["summary", "dividends", "earnings"]
function loadSa(;up=false)
    !up || resetSa()

    xlfn = sort!(filter!(x -> occursin("sa-" * PageNames[1], x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]
    if unix2datetime(mtime(xlfn)) < (now(UTC) - Hour(8))
        error("Sa file not downloaded today. Go to: https://seekingalpha.com/screeners/922a27ae98-To-Download")
    end
    isempty(Combined) || return
    println("Loading sa")

    for pageName in PageNames
        xlfn = sort!(filter!(x -> occursin("sa-" * pageName, x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]

        XLSX.openxlsx(xlfn; mode="rw") do xf
            sheet = xf[1]
            if !startswith(sheet[1,1], pageName)
                for i in 1:100
                    x = sheet[1,i]
                    !ismissing(x) || break
                    sheet[1,i] = pageName * filter(!isspace, x)
                end
            end
            table = rowtable(XLSX.gettable(sheet))
            Pages[pageName] = Dict{String,Dict{String,Any}}()
            toDict!(Pages[pageName], table, Symbol(pageName * "Symbol"))
        end
    end

    for sym in keys(Pages[PageNames[1]])
        isnothing(findfirst(x -> !haskey(Pages[x], sym), PageNames[2:end])) || continue
        Combined[sym] = Dict(Iterators.flatten(map(x -> x[sym], values(Pages))))
    end
end
#endregion

#region PenWek
const Weeklys = Dict{String,AVec}()
const Pennys = Dict{String,AVec}()

function loadWeeklys()
    isempty(Weeklys) || return
    println("Loading weeklys")
    fn = joinpath(BaseDir, "weeklys.csv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(1))
        url = "https://marketdata.theocc.com/weekly-options?action=download"
        println("## downloading weeklys ##\n", url)
        HTTP.download(url, fn)
    end
    loadOccCsv(Weeklys, fn, 1, 3)
end

const DF_PENNYS = dateformat"yyyymmdd"
function loadPennys()
    isempty(Pennys) || return
    println("Loading pennies")
    fn = joinpath(BaseDir, "pennys.csv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(2))
        strDate = Dates.format(lastTradingDay(lastdayofmonth(today() - Month(1))), DF_PENNYS)
        url = "https://marketdata.theocc.com/penny-program?fileName=$(strDate)&reportType=rolling"
        println("## Downloading pennys ##\n", url)
        HTTP.download(url, fn)
    end
    loadOccCsv(Pennys, fn, 2, 1)
end
#endregion

#region SeekingAlpha
using HTTP, JSON3

const SaSyms = Dict{String,Dict{String,Any}}()
const SaMetrics = Dict{String,Dict{String,Any}}()
const SaGrades = Dict{String,Dict{String,Any}}()

const BaseDirSa = joinpath(BaseDir, "sa")
const BaseDirSaSymsRaw = mkpath(joinpath(BaseDir, "SaSymsRaw"))
const BaseDirSaMetricsRaw = mkpath(joinpath(BaseDir, "SaMetricsRaw"))
const BaseDirSaGradesRaw = mkpath(joinpath(BaseDir, "SaGradesRaw"))
SaSymsPerPage = 1000

SaHeaders = [
    "content-type" => "application/json",
    """cookie" => "machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; session_id=922a0694-00c9-4e0b-ac93-f0b66a5457db; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; _hjSession_65666=eyJpZCI6IjFhZmY0YjNiLWIxOTMtNDY5My1hYmFjLTFhZTY4ZDRmZWMzZiIsImNyZWF0ZWQiOjE2NTk5MzczMDcxNDIsImluU2FtcGxlIjpmYWxzZX0=; _hjAbsoluteSessionInProgress=0; user_cookie_key=v9808k; gk_user_access=1*archived*1659937712; gk_user_access_sign=b9a9f74c8724dc98d5a6a0354b9a41c6704a913a; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%2Ftrading%22%2C%22pageKey%22%3A%228d71213c-646f-4fbd-9a02-f92ffe40fa7b%22%7D; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gMYDMAFggM4zEBMADAFYAeALAiADQgBGATgHtkNfhmx4ArAE9CpBGwBebALYNqAN3SYeAV1FVsAO125ce0QGUALhGv7jp8yCoJrMcVhNmAvkA; __pvi=%7B%22id%22%3A%22v-2022-08-07-22-35-05-455-kIliAeii3xMYHqWw-b130a7b8f8ed2b551d9ed77834de6e3b%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659937781623%7D; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DEUPy8flmGBQ-FhtZwp6XQa8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-XwsLhsFO4qSUxA8KFcOBItcxy6Hf0o6sDOQWS8nvXPVcrLz8MS9Khp2ijAmxULIbqXfaegWMFM7R8Q4qHI--jHneruSFac3Mm8RTWtEd57IkVzIRtnXYpFQc9b3bh5DqUllag2yx72VFuiW9Jbbgu_SBaI7kWx6aJYRuMNLmvcwo3dxKBLEZA1Z_wbgaKY5kkFz7f0ZQ-OoM4Fj2Kv-0YWnr46alCxCYawVmzw_n1dWL_4dLBqH0dxR5Ih2TxKxCKLjtQvngU7dXY2aXvLppdd4Ecrk14jgEZwFoG0HTLD3wvi97W605_34Mn8kuqBOTBalH1bIHGYm4N2kEkXAKrPqdBuwGaGIcH10pMG428V0hdbrC7vstC5e6CHGD-3CZde0m_-yczW2tj-kA1NQBZxArAURL6R_XQ-9LG9SL3n2dFK3sX-W8tcq8B5qEiqHm; sailthru_pageviews=6; _gat_UA-142576245-4=1; _dc_gtm_UA-142576245-1=1; sailthru_content=6466d62d98c56179f988c58d69b5bdd08226088c167ecf65440e0fc4d786860e4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426340e50ea7ed4f2934b0685f4b9504786ce9dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb6c6c6ef74477fc2a27e5e8ab2a51659a3; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; _clsk=1p9prl6|1659937783011|13|0|i.clarity.ms/collect; _px2=eyJ1IjoiZTZlNGExMDAtMTZkZC0xMWVkLWI5YjUtZjcwNWI5YjI5Y2UwIiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5MzgzMjQ1MjUsImgiOiI0YzFkNmNiNzczNmQ3ZTQ1OGIwNGZhMjAxMDRlZGVkODY2NDlkNzNkZDU2OGUxZjIwN2NiNjc2ZGMyODZkOTc2In0=; _px=7gqr6jDyPBKqqd7B6TSIf7843spkV3E9X6ZK0vYEeXjAX/SL99Va6RXI/cuET2P7JJ8B6nW2PnzUOPGw/5xDsQ==:1000:auXAUtiSAkJeK8A0v0S5G4GfcDd9AorUfkrujtTq7VsHwQ7/umLkL1umnA411ylLltV1rKh7R7geqMZw0sDx2ZxPr79y75Koxh+BoI1tmfrtMXWkE4MDKyyKv488LnL+R2JTx8hr2SJJ9/zCr33XbE0Z5efUXNxRXBZzS5/ivrWwygC/8nGlTp346Gqswmrfu+pSNcS/aaUnhgOPTzTl+5/OH4+SI6BeXtZWDCDK9VlbcQkw4TOxG4NFwm21Yd315Ahgnmo0xB3lAx9yzcw6iw==; _pxde=41f57b03e3a1a532664b36ef69c6f86b2585bc66b6d2ae6b79b27696e6a23025:eyJ0aW1lc3RhbXAiOjE2NTk5Mzc4MjgxMDksImZfa2IiOjB9""",
    "accept" => "*/*",
    "accept-language" => "en-US,en;q=0.9",
    "referer" => "https://seekingalpha.com/screeners/922a27ae98-To-Download/trading",
    "authority" => "seekingalpha.com",
    "origin" => "https://seekingalpha.com",
    "sec-ch-ua" => "\".Not/A)Brand\";v=\"99\", \"Google Chrome\";v=\"103\", \"Chromium\";v=\"103\"",
    "sec-ch-ua-mobile" => "?0",
    "sec-ch-ua-platform" => "\"Windows\"",
    "sec-fetch-dest" => "empty",
    "sec-fetch-mode" => "cors",
    "sec-fetch-site" => "same-origin",
    "user-agent" => "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36",
    # "accept-encoding" => "deflate, gzip, br, zstd"
]

const SaSymsRaw = Dict{Int,Dict{String,Any}}()
function loadSaSyms(page)
    path = joinpath(BaseDirSaSymsRaw, "$(page).json")
    isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    println("Downloading SaSyms page $(page)")
    url = "https://seekingalpha.com/api/v3/screener_results"
    body = """{"filter":{},"quant_rank":false,"sort":null,"type":"stock","per_page":$(SaSymsPerPage),"page":$(page)}"""
    resp = HTTP.post(url, SaHeaders; body, decompress=true)
    json = procResp(resp)
    writeJson(path, json)
    return json
end

function loadSaSyms()
    SaSymsRaw[1] = loadSaSyms(1)
    count = SaSymsRaw[1]["meta"]["count"]
    numCalls = div(count, SaSymsPerPage) + 1

    for page in 2:numCalls
        json = loadSaSyms(page)
        SaSymsRaw[page] = json
        for d in json["data"]
            attrs = d["attributes"]
            SaSyms[d["id"]] = Dict("slug" => attrs["slug"], "name" => attrs["name"])
        end
    end
end

SaMetricNames = [
    "marketcap_display","dividend_yield","quant_rating","authors_rating","sell_side_rating",
    "last_div_date","div_pay_date","div_yield_fwd","div_yield_4y","div_rate_ttm","div_rate_fwd","payout_ratio","payout_ratio_4y","div_grow_rate3","div_grow_rate5","dividend_growth"
    # ,"dividend_yield"
]
function loadMetrics(syms)
    path = joinpath(BaseDirSaMetricsRaw, "$(syms[1]).json")
    isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    sl = 2*rand()
    println("Downloading SaMetrics start sym $(syms[1]) after $(sl) seconds")
    sleep(sl)
    url = "https://seekingalpha.com/api/v3/metrics?filter[fields]=" * join(SaMetricNames, "%2C") * "&filter[slugs]=" * join(syms, "%2C")
    resp = HTTP.get(url, SaHeaders; decompress=true)
    json = procResp(resp)
    writeJson(path, json)
    return json
end

const SaMetricToId = Dict{String,String}()
function loadMetrics()
    !isempty(SaSyms) || loadSaSyms()
    symsAll = sort!(map(x -> x["slug"], values(SaSyms)))
    for syms in Iterators.partition(symsAll, 150)
        json = loadMetrics(syms)
        procDict(SaMetrics, SaMetricToId, json)
    end
end

SaGradeNames = ["value_category","growth_category","profitability_category","momentum_category","eps_revisions_category",
                "div_safety_category","div_growth_category","div_yield_category","div_consistency_category"]
function loadGrades(syms)
    path = joinpath(BaseDirSaGradesRaw, "$(syms[1]).json")
    isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    sl = 2*rand()
    println("Downloading SaGrades start sym $(syms[1]) after $(sl) seconds")
    sleep(sl)
    url = "https://seekingalpha.com/api/v3/ticker_metric_grades?filter[fields]=" * join(SaGradeNames, "%2C") *
            "&filter[slugs]=" * join(syms, "%2C") *
            "&filter[algos][]=etf&filter[algos][]=dividends&filter[algos][]=main_quant&filter[algos][]=reit&filter[algos][]=reit_dividend"
    resp = HTTP.get(url, SaHeaders; decompress=true)
    json = procResp(resp)
    writeJson(path, json)
    return json
end

const SaGradeToId = Dict{String,String}()
function loadGrades()
    !isempty(SaSyms) || loadSaSyms()
    symsAll = sort!(map(x -> x["slug"], values(SaSyms)))
    for syms in Iterators.partition(symsAll, 150)
        json = loadGrades(syms)
        procDict(SaGrades, SaGradeToId, json)
    end
end
using DictUtil
function procDict(store, lookup, dict)
    for d in filter(x->x["type"] == "metric_type", dict["included"])
        lookup[d["attributes"]["field"]] = d["id"]
        lookup[d["id"]] = d["attributes"]["field"]
    end
    for d in dict["data"]
        if !haskey(d["relationships"]["ticker"]["data"], "id")
            @error "no id key" d
            error("no id key")
        end
        tickerName = SaSyms[d["relationships"]["ticker"]["data"]["id"]]["name"]
        metricName = lookup[d["relationships"]["metric_type"]["data"]["id"]]
        attrs = d["attributes"]
        value = haskey(attrs, "value") ? attrs["value"] : attrs["grade"]
        useKey(Dict{String,Any}, store, tickerName)[metricName] = value
    end
end
#endregion

#region util
const Grades = Dict{String,Int}("A+"=>1, "A"=>2, "A-"=>3, "B+"=>4, "B"=>5, "B-"=>6, "C+"=>7, "C"=>8, "C-"=>9, "D+"=>10, "D"=>11, "D-"=>12, "F"=>13)
# gradePass(x) = !(occursin("D", x) || occursin("F", x)) # x in
gradeAtAbove(x, g) = haskey(Grades, x) && Grades[x] <= Grades[g]

function loadOccCsv(dict, fn, col, skipstart)
    csv = readCsv(fn; header=true, skipstart)[1]
    for row in eachrow(csv)
        @assert isValue(row[col])
        dict[strip(row[col])] = cleanRow(row)
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
isValue(s::AStr) = !isempty(s) && !isnothing(findfirst(!isspace, s))

function procResp(resp)
    content = String(resp.body)
    json = JSON3.read(content, Dict)
    if !haskey(json, "data")
        @error "no data key" content
        error("no data key")
    end
    return json
end
#endregion

end
