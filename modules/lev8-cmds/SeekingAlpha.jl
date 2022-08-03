module SeekingAlpha
using Dates, Tables
import XLSX
using BaseTypes

const Cands = Dict{String,NamedTuple}()
const Syms = String[]
function getCands(filt=filt1; up=false)
    loadWeeklys()
    loadSa()
    empty!(Cands)
    empty!(Syms)
    merge!(Cands, filter(filt, Combined))
    append!(Syms, keys(Cands))
    return Cands
end

RequirePass = (:Valuation, :Profitability) # :Growth, :Momentum
function filt1(x)
    filtWeeklys(x) || return false
    nt = x[2]
    nt.Quant >= 2.5 || return false
    for req in RequirePass
        gradeAtAbove(nt[req], "C-") || return false
    end
    grades = gradeAtAbove(nt.DivSafety, "C") &&
                gradeAtAbove(nt.DivGrowth, "C") &&
                gradeAtAbove(nt.DivYield, "C") &&
                gradeAtAbove(nt.DivConsistency, "C")
    return grades
    return true
end

filtWeeklys(x) = haskey(Weeklys, x[1])

const Grades = Dict{String,Int}("A+"=>1, "A"=>2, "A-"=>3, "B+"=>4, "B"=>5, "B-"=>6, "C+"=>7, "C"=>8, "C-"=>9, "D+"=>10, "D"=>11, "D-"=>12, "F"=>13)
# gradePass(x) = !(occursin("D", x) || occursin("F", x)) # x in
gradeAtAbove(x, g) = haskey(Grades, x) && Grades[x] <= Grades[g]

const Combined = Dict{String,NamedTuple}()
const Summary = Dict{String,NamedTuple}()
const Dividends = Dict{String,NamedTuple}()
BaseDir = "C:/Users/joel/Downloads"
resetSa() = ( empty!(Combined) ; empty!(Summary) ; empty!(Dividends) )
function loadSa(;up=false)
    !up || resetSa()
    isempty(Combined) || return
    xlfnSum = sort!(filter!(x -> occursin("sa-summary", x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]
    xlfnDiv = sort!(filter!(x -> occursin("sa-dividends", x), readdir(BaseDir; join=true)); rev=true, by=x -> unix2datetime(mtime(x)))[1]
    xfSum = XLSX.readxlsx(xlfnSum)
    XLSX.openxlsx(xlfnDiv; mode="rw") do xfDiv
        sheetSum = xfSum[1]
        sheetDiv = xfDiv[1]
        if !startswith(sheetDiv[1,1], "Div")
            for i in 1:100
                x = sheetDiv[1,i]
                !ismissing(x) || break
                sheetDiv[1,i] = "Div" * filter(!isspace, x)
            end
        end
        tableSum = rowtable(XLSX.gettable(sheetSum))
        tableDiv = rowtable(XLSX.gettable(sheetDiv))
        toDict!(Summary, tableSum, :Symbol)
        toDict!(Dividends, tableDiv, :DivSymbol)
        for sym in keys(Summary)
            haskey(Dividends, sym) || continue
            Combined[sym] = merge(Summary[sym], Dividends[sym])
        end
    end
end

using Dates, HTTP
using FileUtil
const Weeklys = Dict{String,AVec}()
function loadWeeklys()
    isempty(Weeklys) || return
    fn = joinpath(BaseDir, "weeklys.csv")
    if !isfile(fn) || unix2datetime(mtime(fn)) < (now(UTC) - Week(1))
        println("************** downloading weeklys")
        HTTP.download("https://marketdata.theocc.com/weekly-options?action=download", fn)
    end
    matWeeklys = readCsv(fn; header=true, skipstart=3)[1]
    for row in eachrow(matWeeklys)
        @assert isValue(row[1])
        Weeklys[strip(row[1])] = strip.(row)
    end
end

toDict!(dict, table, keySym) = for row in table
    dict[row[keySym]] = row
end

isValue(::Nothing) = false
isValue(::Missing) = false
isValue(s::AStr) = !isempty(s) && !isnothing(findfirst(!isspace, s))

end