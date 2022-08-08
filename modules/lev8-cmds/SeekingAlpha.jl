module SeekingAlpha
using Dates, Tables, HTTP
import XLSX
using BaseTypes
using DateUtil, FileUtil, DictUtil

BadPricing = ["BAX", "OLN"]
Ignore = ["SNDL"]

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
const BaseDirSaSyms = mkpath(joinpath(BaseDir, "SaSyms"))
const BaseDirSaMetrics = mkpath(joinpath(BaseDir, "SaMetrics"))
const BaseDirSaGrades = mkpath(joinpath(BaseDir, "SaGrades"))
SaSymsPerPage = 500

SaHeaders = [
    "content-type" => "application/json",
    """cookie: machine_cookie=8489826285309; _igt=1a767908-6c26-413f-a930-aa2a110c2e73; _cls_s=2bfc639e-5692-4d1c-876e-defb35c3d07f:0; _cls_v=c819e217-4440-48aa-9468-e1347d6681ec; __pat=-14400000; _gcl_au=1.1.2045947085.1658066187; _ga=GA1.2.1467132679.1658066187; _fbp=fb.1.1658066187416.1684167567; _cc_id=2676af92b467222a2b94d5312a51c22f; prism_25946650=3d5b82ac-3d0b-475c-9e2c-8ae75c5b9b12; pxcts=40ad5fc5-05d8-11ed-ac35-414c78494568; _pxvid=40a46a87-05d8-11ed-ac35-414c78494568; user_id=54607428; user_nick=taotree; user_devices=2; u_voc=; marketplace_author_slugs=; has_paid_subscription=true; user_perm=; ever_pro=1; sapu=12; user_remember_token=***REMOVED***; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _hjSessionUser_65666=eyJpZCI6IjZiYzk4MjFmLTMyZGYtNTU3ZC05MGIyLTA0ZGM2ZDc0NGVhMCIsImNyZWF0ZWQiOjE2NTkzNDc5ODczNzksImV4aXN0aW5nIjp0cnVlfQ==; ga_clientid=1467132679.1658066187; __pnahc=0; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; _ig=54607428; ubvt=eea277d5-273b-405d-8c65-cfada3212c30; _gac_UA-142576245-3=1.1659791857.Cj0KCQjworiXBhDJARIsAMuzAuyBw3OQHqwNHSiJxB_hKx8LSoz4iYamKgUzoOeosrcO273xgTBBl3MaAhZmEALw_wcB; _gcl_aw=GCL.1659791857.Cj0KCQjworiXBhDJARIsAMuzAuyBw3OQHqwNHSiJxB_hKx8LSoz4iYamKgUzoOeosrcO273xgTBBl3MaAhZmEALw_wcB; _gac_UA-142576245-1=1.1659791857.Cj0KCQjworiXBhDJARIsAMuzAuyBw3OQHqwNHSiJxB_hKx8LSoz4iYamKgUzoOeosrcO273xgTBBl3MaAhZmEALw_wcB; __tac=; __tae=1659791859924; _sapi_session_id=L5wCjf1OwQORjF3JQfDjvA6mK9avkocgdG22CdfNFRmtDNjSvS8VdYG7LzfMZJZjNrRL9KdNJdkL%2Bj1aF10k3RlSB%2B%2Bx0iskEAKmi3RR5y2vBEjLIWYMBkfcKYdj80linIghJhN3JyO%2FnZxOapoek6z%2FMJ%2F%2BRRx1GDu58Xvf%2ByqzeVx2rqG5aMpOssb4zx%2FEhNqmtCLVZTPoBuC%2Ff%2Fxn3JPt4kloSOQ27x8DaqjLvy88br0caNhhDZ3CF3T%2FWxBX6HNahfqJvUV44vC1xNn6zaU%2F3UdjDMnwI4WyQYwIPZA%3D--AnrcxY0rYtn%2BSxk5--JUv%2Bio0%2Bq10jPb2fqj%2F%2FSg%3D%3D; user_cookie_key=vcb8pq; gk_user_access=1*archived*1659956555; gk_user_access_sign=b50b25c7728e16a055ece1080a0f3cd040badb0e; _gid=GA1.2.2053506453.1659956558; panoramaId_expiry=1660561357847; panoramaId=99a0dfd70a392873461bac28f1d516d53938c5d4f08b54c3dac0836e4ed9e007; _clck=1k3iu2x|1|f3u|0; _sasource=; session_id=c08a7814-d327-4540-adac-955285f07c63; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%22%2C%22pageKey%22%3A%221b320d91-f363-41d8-a2b2-8c837b653cc2%22%7D; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gI4CsATABwCu5A9vgC7LqkAeIANCAEYBOdZAGcYfDNjz4IhPsSg8A7NRht0EPgGMuIaiL5DsAO2q5c3XaIDKjCI11GTZkEISMY4rMdMBfIA; __pvi=%7B%22id%22%3A%22v-2022-08-08-05-01-36-632-6mVk1ucDKYif6BGB-74880d2716ff21790c53cef4d5b3342c%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659961369761%7D; sailthru_pageviews=3; __tbc=%7Bkpex%7DeJTJaoGdmSxQQbmi72jolAMuT8f9QsT4y6zo5-jtcXC5p0n7J1uJ94GhsnAF-FxrDN1N10XkvCwHs4aOx5HObwnN2zcsNQpGOlM3YPnxO7ahAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DWvngWwznmhXRcIdQ9bB9eK8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-6ljfX0bEp4E7K1DH8uQQIERW73mang6ewYpBhQXOSEDeik40-XmFAwgcqqIREZVZEDEI-chm6EzK1Txp5qsupxjfKfrMCQiu9Vl3pYv0hSDKygnRmIGx9LlNFUKRmxSlYqTSysPgo4wkqA9jBLmplb96btHR1DBhNwdJYAA3dKVqNldHDcy7T6P94z_MPjaat30-h2a6v1O8FUwXQoRIcHDniD8zQqbexw8jyL2KnXnZ0QtVDffKv0-XFh9fHOaVhZqiepO88iGIbcat_m4EPODN2b-vmvX-Uh58K0HrEn7gL2vZUsnQxZeceLFR05bGMscFELxWPZvvJS3V13jFBM7X7U56Wg52ha03kvuhy_pxS1OhUM2lehzZGjYyCwcsms1Fd8GM1O-I1JOCRD-Y78nAuyQ6LrFKW6j88MQuts2mlBiuKZ5Vm6PY_YzHjF2eObmubP-BdzyvZKoFPTE4zg; _gat_UA-142576245-4=1; _dc_gtm_UA-142576245-1=1; _uetsid=9d7fc9e0170911ed9ff889f34d52169f; _uetvid=672d90705ea711ec9a26f7a3d82d27e3; sailthru_content=23322bb65919647e151d407dc665a4fed336ff2bdab2770099e746360c3b2e4ac713a55b11e43cb6ce9e3b1a05f01007b9e1a40725e124f440f2a66071cd910348b3ccffa1ecd7cdb2803b647f2eb946240832d0b93035dd5ce212edfb999c1f8f9ba8a8cf8c53c60b8ab7d48ac1ee46e4940b9ab6e01cb7c8fba64bb35bbf58157d2b67379713f37f7152f7c4a1b96a5a38fbc96c6b4e4a7683c0ebb2bad97370b4474ea119649b556a22f336c5e5c130e3833e50b83bc33558e1dd028b9f3512f2e47d85124c3d6c77f5c9bd5add6bf2bf59f907da11ef259c5d53c8a99e070e50ea7ed4f2934b0685f4b9504786ce0df6122f6a027444bc544aeb909fceb6; sailthru_visitor=51fff7ec-bfcd-415d-be1f-0cfae5576025; _clsk=cikd15|1659961371411|7|0|i.clarity.ms/collect; _px2=eyJ1IjoiZDI5M2E2YjAtMTcxNC0xMWVkLWJlMGMtZDcxOTE5MmZkZTE0IiwidiI6IjQwYTQ2YTg3LTA1ZDgtMTFlZC1hYzM1LTQxNGM3ODQ5NDU2OCIsInQiOjE2NTk5NjE4NzExMDcsImgiOiJiODgwMTNlNzMyN2YyNjM0MGM1OTAyODk2OGJjZjM4YmJlMjk4NDg2OGU4ZTYzYTVkNjI0ZmU5MzhmMGE5NzkwIn0=; _px=/2W6DxrLwQHJd25VHSD8wmcESzEDCxFIawRPp0JQBPfDTWIjV+FcutjiTXGA2O1sp84/PIYHe2PKlqGBmuw0jw==:1000:ZPRDnFgWkJwAsZrcIA1adCqTpW3Cl4K9Bf4hf47E6FA+VoaUwe5kfEYzycRj4ycdwz9PceBs27R0LYjAsP988mT5kgJh/aeLcDMRO0CDDlV70uO6JAR++iqGhqXYavUIKnYuZ15fxs34G8vobcTSZalEMlWuhqHO6dYFnmxT64VqPdZM/g7pwJ/ZHAiK17kE5mzGMLBjYjR2aMhV+KwZgpRZtvf0mOLUvgau+2dE2iggbFVVeB+UgFmmxlbmVkNv69phZpUP7GaZ5vyi4bX5Ig==; _pxde=567893822c8fcbf2430790ac30d527faf9656b63c2435764bc5f5ed0a06345c2:eyJ0aW1lc3RhbXAiOjE2NTk5NjEzNzExMDgsImZfa2IiOjB9""",
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

# -H 'authority: seekingalpha.com' \
# -H 'accept: */*' \
# -H 'accept-language: en-US,en;q=0.9' \
# -H 'content-type: application/json' \
# -H 'cookie: machine_cookie=6071666182064; _igt=eac4b6a3-4f66-497e-db45-26a99710806c; _cls_s=364f8035-f84b-4d81-b13e-0563633b4c05:0; _cls_v=6b22da5b-8cd3-4529-be8d-ea1bf8a5aa9d; __pat=-14400000; _ga=GA1.2.303343256.1658620965; _gcl_au=1.1.770029613.1658620965; _fbp=fb.1.1658620964957.187023299; _cc_id=bbdbd0ec9054d648845a516a9034d5e1; prism_25946650=7ea4c454-399e-4576-b169-279f902e84b8; _pxvid=f1e5bfaa-0ae3-11ed-96d7-5749446d4366; pxcts=f1e5cbb2-0ae3-11ed-96d7-5749446d4366; ga_clientid=303343256.1658620965; g_state={"i_l":0}; user_id=54607428; u_voc=; marketplace_author_slugs=; user_perm=; user_remember_token=d5e1f67e796f682b3e605bf4283e8c33443cdb68; _gac_UA-142576245-3=1.1658695017.Cj0KCQjw2_OWBhDqARIsAAUNTTFT5kgCIX7TbzJHIG1_swVZT2ehZO6tRDKsqB5b6_hnYylaECFxcv0aAgfNEALw_wcB; sailthru_hid=e865f74ef53b891c8276d027693f33ac60e4d2352e08d924735ce6b556c6bc3e5d4403affe742307ff74a234; __pnahc=0; has_paid_subscription=true; ever_pro=1; sapu=12; user_nick=taotree; _hjSessionUser_65666=eyJpZCI6ImM5Nzg3ZGYwLTRlODEtNWUxOS1iY2UwLTcxYjI0N2UyZDA5MCIsImNyZWF0ZWQiOjE2NTkxOTA0OTE2OTMsImV4aXN0aW5nIjp0cnVlfQ==; user_devices=2; __tae=1659754665990; __tac=; _gid=GA1.2.1902944425.1659875734; panoramaId=ef49906a77f3dcf67f1752487ebc16d53938221439d41fa5de9ff54dc80f7bdb; panoramaId_expiry=1660480534033; _gcl_aw=GCL.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _gac_UA-142576245-1=1.1659879239.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _hjCachedUserAttributes=eyJhdHRyaWJ1dGVzIjp7ImxvZ2dlZF9pbiI6dHJ1ZSwibXBfc3ViIjpmYWxzZSwicHJlbWl1bV9zdWIiOnRydWUsInByb19zdWIiOmZhbHNlfSwidXNlcklkIjoiNTQ2MDc0MjgifQ==; _gac_UA-142576245-4=1.1659879240.Cj0KCQjwxb2XBhDBARIsAOjDZ36qEUfFUGzvYR1l_j87V_kwCJhrSVi-d_St16yGuxzQuVhEadbGtZkaAp9cEALw_wcB; _ig=54607428; _clck=g8tqn|1|f3u|0; session_id=e55e6428-7812-4585-a2d8-8eca993ec86b; _sasource=; _uetsid=6e456600164d11eda6108b3e96e4d981; _uetvid=9aa4ca703e5f11ec8dfc43f19f496f0e; sailthru_visitor=108bebf1-2815-489a-8d93-bf931b716ad9; sailthru_content=4fbb613834ea8cc57feae8bd1e4aaeae4dfcd4abf2c20d3c6d398b4f5a8270b3a6868281678e0269f2e30c72a59520f86ee54343d85ccfb04f65bd847aaafeb5ed016a510cc06d5e79dc1db8608d729a0cf0526f52035a3a6aa562a10b4bae2be1980fd747e58a39e7568037500e1f926a6ebc798cc2c232c5ef8b94521c981527196a8a2587992b557d4b875d9a736c05bccff52c907303182a1f2e536426349dc8a9254a0258aae32f96507bf440950df6122f6a027444bc544aeb909fceb68779dc62b8018008a97c994a984f1063f2bf59f907da11ef259c5d53c8a99e070e50ea7ed4f2934b0685f4b9504786cec6c6ef74477fc2a27e5e8ab2a51659a3; _clsk=w2xq1y|1659954537422|6|0|i.clarity.ms/collect; _pxde=317e00236949e85f15251435a86822b62b0dd6c7d8664cfe6bde2db3292d9508:eyJ0aW1lc3RhbXAiOjE2NTk5NTQ1NzEzNzgsImZfa2IiOjB9; _px=hqOop1qkPRlTfmZleXEIa1AscSRNQgqTULokU5ReM6yd9wI9/IeabT7ULQ213Dk+kjSs4MrA76R7uWj5eJoMog==:1000:qmceoYZ5JNenu68ApKvuHG4PI8glUe+m3lzGHgq23qN4bjppPo/ZiolI9oyt/RpchpO8wHFO6g0eI7FGmaoKtb/zsGj3v6JneZ5XPUYK8ive/tbckGQgfB2dVPvu+tkLnPGwASRtGysjs8hz0NShpTs0ap8V0O1cUDKh/hF9kreNc/69aktNRXmGAlHVhQM9cXdAXQdJG0b3i99FDXnYi8GrAK/SIjbWh1iMYfIG6VsNjVYehoF1zC9vy7kXD1Dh9fdPXPMwm4y1LNcG+Ep1KQ==; _px2=eyJ1IjoiMTUxMGZkYTAtMTZmZi0xMWVkLWI3NTktMDVlY2VmMTgxMTg4IiwidiI6ImYxZTViZmFhLTBhZTMtMTFlZC05NmQ3LTU3NDk0NDZkNDM2NiIsInQiOjE2NTk5NTUwNzEzNzgsImgiOiIwMjgyNGNhNWEyZTRjZjE1MTYxYTE1ZTQzMGVkNDUwMGRjNzhiNWU5N2UxMTQzZWEzNGNhNmZmM2M3NzUyNjA2In0=; _gat_UA-142576245-4=1; LAST_VISITED_PAGE=%7B%22pathname%22%3A%22https%3A%2F%2Fseekingalpha.com%2Fscreeners%2F922a27ae98-To-Download%2Ftrading%22%2C%22pageKey%22%3A%224f3644c6-087d-4387-b05b-13f44d41cfd9%22%7D; user_cookie_key=1342hsv; gk_user_access=1*archived*1659954608; gk_user_access_sign=bb3cfeda030d4f80cef25db2ff9773762ab2d079; _pctx=%7Bu%7DN4IgDghg5gpgagSxgdwJIBMQC4QBsBsA1gLYAsArPsQHYCcAjAgEYBMAHgI4gA0ITATgHtkAZxj8M2POQCehAMwJSAL1LEADAhEA3dJl4BXMfxHZqB3LkPGAygBcIdo2YtWQIhHZiSs5ywF8gA; __pvi=%7B%22id%22%3A%22v-2022-08-08-03-27-33-202-purfcwFClVfMhaiq-5b443c5e466197036a9c2591726a2aba%22%2C%22domain%22%3A%22.seekingalpha.com%22%2C%22time%22%3A1659954608449%7D; sailthru_pageviews=3; __tbc=%7Bkpex%7DW6oShuI0nLVBtUkcsYyQNO3HbqO1F1NSDqxDOemOp1I2VzJNwbAM-L7VWV3iXEZeONhV6ZY_iID8vDMKo689yG6JVQyV6-Y5zmElMQcneQqhAoGIqkJOLfI05scsegea; xbc=%7Bkpex%7DEUPy8flmGBQ-FhtZwp6XQa8oHL1LXaI6DgepA81zoVYnB9hA2UEAmS9MNnm_cJP-XwsLhsFO4qSUxA8KFcOBItcxy6Hf0o6sDOQWS8nvXPVcrLz8MS9Khp2ijAmxULIbqXfaegWMFM7R8Q4qHI--jHneruSFac3Mm8RTWtEd57IkVzIRtnXYpFQc9b3bh5DqUllag2yx72VFuiW9Jbbgu_SBaI7kWx6aJYRuMNLmvcwo3dxKBLEZA1Z_wbgaKY5kkFz7f0ZQ-OoM4Fj2Kv-0YWnr46alCxCYawVmzw_n1dWL_4dLBqH0dxR5Ih2TxKxCKLjtQvngU7dXY2aXvLppdd4Ecrk14jgEZwFoG0HTLD3wvi97W605_34Mn8kuqBOTBalH1bIHGYm4N2kEkXAKrPqdBuwGaGIcH10pMG428V0hdbrC7vstC5e6CHGD-3CZde0m_-yczW2tj-kA1NQBZxArAURL6R_XQ-9LG9SL3n2dFK3sX-W8tcq8B5qEiqHm; _dc_gtm_UA-142576245-1=1' \
# -H 'origin: https://seekingalpha.com' \
# -H 'referer: https://seekingalpha.com/screeners/922a27ae98-To-Download/trading' \
# -H 'sec-ch-ua: ".Not/A)Brand";v="99", "Google Chrome";v="103", "Chromium";v="103"' \
# -H 'sec-ch-ua-mobile: ?0' \
# -H 'sec-ch-ua-platform: "Windows"' \
# -H 'sec-fetch-dest: empty' \
# -H 'sec-fetch-mode: cors' \
# -H 'sec-fetch-site: same-origin' \
# -H 'user-agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/103.0.0.0 Safari/537.36' \


const SaSymsRaw = Dict{Int,Dict{String,Any}}()
function loadSaSyms(page)
    path = joinpath(BaseDirSaSymsRaw, "$(page).json")
    isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    sl = 2 + 4*rand()
    println("Downloading SaSyms page $(page) after $(sl) seconds")
    sleep(sl)
    url = "https://seekingalpha.com/api/v3/screener_results"
    body = """{"filter":{"close":{"gte":0.01,"disabled":false}},"quant_rank":true,"sort":null,"type":"stock","per_page":$(SaSymsPerPage),"page":$(page)}"""
    resp = HTTP.post(url, SaHeaders; body, verbose=2)
    json = procResp(resp)
    writeJson(path, json)
    return json
end

function loadSaSyms()
    # SaSymsRaw[1] = loadSaSyms(1)
    # numCalls = div(count, SaSymsPerPage) + 1
    proced = 0
    for page in 1:1000
        json = loadSaSyms(page)
        SaSymsRaw[page] = json
        count = SaSymsRaw[page]["meta"]["count"]
        for d in json["data"]
            addsym(d)
            proced += 1
        end
        println("proced ", proced)
        proced < count || break
    end
end

SaMetricNames = [
    "marketcap_display","dividend_yield","quant_rating","authors_rating","sell_side_rating",
    "last_div_date","div_pay_date","div_yield_fwd","div_yield_4y","div_rate_ttm","div_rate_fwd","payout_ratio","payout_ratio_4y","div_grow_rate3","div_grow_rate5","dividend_growth",
    "earning_announce_date",
    # ,"dividend_yield"
]
function loadMetrics(syms)
    # path = joinpath(BaseDirSaMetricsRaw, "$(syms[1]).json")
    # isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    # sl = 2 + 4*rand()
    println("Downloading SaMetrics start sym $(first(syms))")# after $(sl) seconds")
    # sleep(sl)
    url = "https://seekingalpha.com/api/v3/metrics?filter[fields]=" * join(SaMetricNames, "%2C") * "&filter[slugs]=" * join(syms, "%2C")
    resp = HTTP.get(url, SaHeaders; decompress=true)
    json = procResp(resp)
    # writeJson(path, json)
    return json
end

const SaMetricToId = Dict{String,String}()
# function loadMetrics()
#     !isempty(SaSyms) || loadSaSyms()
#     symsAll = allSlugs()
#     for syms in Iterators.partition(symsAll, 150)
#         json = loadMetrics(syms)
#         procDict(SaMetrics, SaMetricToId, json)
#     end
# end

function getMetrics(syms)
    !isempty(SaMetrics) || loadMetrics()
    @assert length(syms) < 150
    todo = String[]
    for s in syms
        if !haskey(SaMetrics, s) || !haskey(SaMetrics[s], "tsUpdate") || SaMetrics[s]["tsUpdate"] < now(UTC) - Hour(12)
            push!(todo, s)
        end
    end
    if !isempty(todo)
        global procJson = loadMetrics(todo)
        procDict(SaMetrics, SaMetricToId, procJson)
        saveMetrics()
    end
end

function loadMetrics()
    path = joinpath(BaseDirSaMetrics, "SaMetrics.json")
    if isfile(path)
        empty!(SaMetrics)
        empty!(SaMetricToId)
        merge!(SaMetrics, loadJson(path))
        merge!(SaMetricToId, loadJson(joinpath(BaseDirSaMetrics, "SaMetricToId.json")))
    end
end

function saveMetrics()
    writeJson(joinpath(BaseDirSaMetrics, "SaMetrics.json"), SaMetrics)
    writeJson(joinpath(BaseDirSaMetrics, "SaMetricToId.json"), SaMetricToId)
end

SaGradeNames = ["value_category","growth_category","profitability_category","momentum_category","eps_revisions_category",
                "div_safety_category","div_growth_category","div_yield_category","div_consistency_category"]
function loadGrades(syms)
    path = joinpath(BaseDirSaGradesRaw, "$(syms[1]).json")
    isfile(path) && unix2datetime(mtime(path)) > (now(UTC) - Hour(12)) && return loadJson(path)

    sl = 2 + 2*rand()
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
    symsAll = allSlugs()
    for syms in Iterators.partition(symsAll, 150)
        json = loadGrades(syms)
        procDict(SaGrades, SaGradeToId, json)
    end
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
        tickerName = SaSyms[d["relationships"]["ticker"]["data"]["id"]]["name"]
        metricName = lookup[d["relationships"]["metric_type"]["data"]["id"]]
        attrs = d["attributes"]
        value = haskey(attrs, "value") ? attrs["value"] : attrs["grade"]
        to = useKey(Dict{String,Any}, store, tickerName)
        to[metricName] = value
        to["tsUpdate"] = now(UTC)
    end
end

function addsym(d)
    attrs = d["attributes"]
    val = Dict("id" => d["id"], "slug" => attrs["slug"], "name" => attrs["name"])
    SaSyms[d["id"]] = val
    SaSyms[attrs["name"]] = val
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
        sym = strip(row[col])
        !(sym in BadPricing || sym in Ignore) || continue
        dict[sym] = cleanRow(row)
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

using TranscodingStreams, CodecZlib
function procResp(resp)
    global raw = resp.body
    content = transcode(CodecZlib.GzipDecompressor, raw)
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

#region Quotes
using TradierData
const Quotes = Dict{String,Dict{String,Any}}()
const QuoteNotFound = Dict{String, DateTime}()
function loadQuotes(src)
    symsAll = filter(x -> !haskey(Quotes, x) && !haskey(QuoteNotFound, x), src)
    println("loading $(length(symsAll)) quotes")
    for syms in Iterators.partition(symsAll, 1000)
        raw = tradierQuotes(syms)
        unmatched = haskey(raw, "unmatched_symbols") ? raw["unmatched_symbols"]["symbol"] : []

        if haskey(raw, "quote")
            global quotes = raw["quote"]
            for q in quotes
                Quotes[q["symbol"]] = q
            end
            diff = setdiff(syms, map(x->x["symbol"], quotes))
            len = length(syms) - length(quotes)
            @assert len == length(diff) == length(unmatched) "Different lens $(len) $(length(diff)) $(length(unmatched))"
            println("not found ", diff)
        else
            println("no quotes found, unmatched: ", unmatched)
        end
        n = now(UTC)
        for sym in unmatched
            QuoteNotFound[sym] = n
        end
    end
end

function quoteWeeklys()
    loadWeeklys()
    loadQuotes(keys(Weeklys))
end

function qsyms()
    return filter(s -> Quotes[s]["average_volume"] > 100000 && Quotes[s]["prevclose"] < 12.5, keys(Quotes))
end
#endregion

end
