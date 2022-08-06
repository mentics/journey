module SeekingAlpha
using Dates, Tables
import XLSX
using BaseTypes
using DateUtil

const Cands = Dict{String,Dict{Symbol,Any}}()
const Syms = String[]
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

filtWeeklys(x) = haskey(Weeklys, x[1])

const Grades = Dict{String,Int}("A+"=>1, "A"=>2, "A-"=>3, "B+"=>4, "B"=>5, "B-"=>6, "C+"=>7, "C"=>8, "C-"=>9, "D+"=>10, "D"=>11, "D-"=>12, "F"=>13)
# gradePass(x) = !(occursin("D", x) || occursin("F", x)) # x in
gradeAtAbove(x, g) = haskey(Grades, x) && Grades[x] <= Grades[g]

const Combined = Dict{String,Dict{Symbol,Any}}()
const Pages = Dict{String,Dict{String,Dict{Symbol,Any}}}()
const BaseDir = "C:/Users/joel/Downloads"
const PageNames = ["summary", "dividends", "earnings"]
resetSa() = ( empty!(Combined) ; empty!(Summary) ; empty!(Dividends) ; empty!(Weeklys) )
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

using Dates, HTTP
using FileUtil
const Weeklys = Dict{String,AVec}()
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
const Pennys = Dict{String,AVec}()
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

end