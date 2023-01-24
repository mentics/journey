module CheckDx
using Dates
using Globals, SH
using DateUtil, SnapUtil
using Snapshots
using Markets, Expirations, Chains
import HistSpy as hspy


#=
Try:
 2022-03-14T07:00:00
 2022-05-17T08:00:00
 =#

function run()
    snap(Date(2022,9,1), 1)
    num = SnapUtil.snapNum(snap()) - 1
    xpir = expir(1)
    dt = round(Snapshots.snapToTs(snap()), Minute(30))

    curpdiff = 1.0
    while abs(curpdiff) > 0.0005
        num += 1
        snap(num)
        dt = round(Snapshots.snapToTs(snap()), Minute(30))
        if minute(dt) == 30
            if hour(dt) != 13
                continue
            end
            dt = dt + Second(10)
        end
        xpir = expir(1)
        println("Snapped to $(snap()), getting dx quotes for $(formatLocal(dt))) for expir: $(xpir)")

        curp = market().curp
        curpdx = hspy.getUnder(dt)
        curpdiff = curpdx - curp
        println("curp diff: ", curpdiff, "  $(curp)    $(curpdx)")
    end

    oqs = chain(xpir).chain # ; age=Second(0)).chain
    oqsdx = hspy.getQuotes(dt, xpir)
end

check(dts::String) = check(fromLocal(dts))
function check(dt::DateTime)
    name = SnapUtil.snapName(dt)
    snap(name)
    checkCurp(dt)
    xpirs = SnapUtil.snapExpirs(name)
    xpir = xpirs[1]
    oqs = chain(xpir).chain
    oqsdx = hspy.getQuotes(dt, xpir)
    compare(oqs, oqsdx)
end

function checkCurp(dt::DateTime)
    curp = market().curp
    curpdx = hspy.getUnder(dt)
    curpdiff = curpdx - curp
    println("curp diff: ", curpdiff, "  $(curp)    $(curpdx)")
end

function compare(oqs, oqsdx)
    @assert length(oqs) == length(oqsdx)
    for i in eachindex(oqs)
        oq1 = oqs[i]
        oq2 = oqsdx[i]
        @assert getOption(oq1) == getOption(oq2)
        q1 = getQuote(oq1)
        q2 = getQuote(oq2)
        diff = q2 - q1
        diffbap = bap(q2) - bap(q1)
        # if abs(diffbap) > 0.01
            println(getStrike(oq1), ": ", diff, "  ", diffbap, "  $(q1)  $(q2)")
        # end
        if getBid(q1) == getAsk(q1)
            println("$(getStrike(oq2)) WARN Q1: bid == ask: $(q1)")
        end
        if getBid(q2) == getAsk(q2)
            println("$(getStrike(oq2)) bid == ask: $(q2)")
        end
    end
end

end