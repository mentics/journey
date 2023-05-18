module ProcOptionsDx
using Dates
using BaseTypes
using LogUtil, DateUtil
import SimpleStore

const BaseDir = "C:/data/market/optionsdx/"
function findFile(y, m)
    dateStr = Dates.format(Date(y,m,1), SimpleStore.DF_FILE)
    rx = Regex("spy.+$(dateStr).txt")
    for (root, dirs, files) in walkdir(BaseDir)
        for file in files
            m = match(rx, file)
            isnothing(m) || return joinpath(root, file)
        end
    end
end

sameDate(ts1::Integer, ts2::Integer) = Date(unix2datetime(ts1)) == Date(unix2datetime(ts2))

function proc(months; maxLines=1e9)
    LogUtil.resetLog(:hand)
    timeStart = time()
    println("Started: ", nowz())
    for my in months
        y = year(my)
        m = month(my)
        csvPath = findFile(y, m)
        println("Loading $(csvPath)...")
        pathCall, pathPut, pathUnder = SimpleStore.paths(y, m)
        open(pathCall; write=true) do ioCall
            open(pathPut; write=true) do ioPut
                open(pathUnder; write=true) do ioUnder
                    procFile(csvPath, writerOpt(ioCall), writerOpt(ioPut), writerUnder(ioUnder); maxLines)
                end
            end
        end
        println("Finished loading $(csvPath)")
    end
    timeEnd = time()
    println("End: ", nowz())
    println("Elapsed seconds: ", timeEnd - timeStart)
    # return first(hspy.sel("select (select count(*) as cnt from call) as calls, (select count(*) as cnt from put) as puts, (select count(*) as cnt from under) as unders"))
    return
end

function writerOpt(io)
    return function(ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
        write(io, ts, xpir, strike, bid, ask, last, vol, delta, gamma, vega, theta, rho, iv)
    end
end

function writerUnder(io)
    return function(ts, under, open, hi, lo)
        write(io, ts, under, open, hi, lo)
    end
end

function procFile(csvPath, fcall, fput, funder; maxLines)
    line, itr = Iterators.peel(Iterators.drop(eachline(csvPath), 1))
    _, open = procLine(line, fcall, fput)
    hi = open ; lo = open
    tsPrev = 0
    i = 1
    for line in itr
        tsNew, under = procLine(line, fcall, fput)
        if under == CZ
            println("Skipping ts $(tsNew)")
            continue
        end
        # !isnothing(res) || continue
        # tsNew, under = res
        if tsNew != tsPrev
            if !sameDate(tsPrev, tsNew)
                open = under
                hi = under
                lo = under
            else
                if under > hi
                    hi = under
                end
                if under < lo
                    lo = under
                end
            end

            funder(tsNew, under, open, hi, lo)
            # println("new ts $(tsPrev) -> $(tsNew)")
            tsPrev = tsNew
        end

        i += 1
        if i % 10000 == 0
            println(i)
            if i >= maxLines
                println("Stopping at $(i) >= maxLines $(maxLines)")
                break
            end
        end
    end
end

function procLine(line, fcall, fput)
    try
        left = 1
        right = findnext(',', line, left)
        ts = parsem(Int, SubString(line, left:right-1))
        if !isBusDay(Date(unix2datetime(ts)))
            @log hand "skipping non business day" line
            return (ts, CZ)
        end
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
        cVol = parsem(Float64, SubString(line, left:right-1))
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
        pVol = parsem(Float64, SubString(line, left:right-1))

        # if cBid > cAsk
        #     if cAsk > 30 && cBid > 1.5 * cAsk
        #         @log hand "WARN: Suspicious cBid > 1.5 * cAsk, skipping" cBid cAsk line
        #         return (ts, CZ)
        #     end
        #     tmp = cAsk
        #     cAsk = cBid
        #     cBid = tmp
        # end

        # if pBid > pAsk
        #     if pAsk > 30 && pBid > 1.5 * pAsk
        #         @log hand "WARN: Suspicious pBid > 1.5 * pAsk, skipping" pBid pAsk line
        #         return (ts, CZ)
        #     end
        #     tmp = pAsk
        #     pAsk = pBid
        #     pBid = tmp
        # end

        if xpir < ts
            @log hand "WARN: xpir < ts, skipping" xpir ts line
            return (ts, CZ)
        end

        if !(1000 < strike < 1000000)
            @log hand "WARN: invalid strike, skipping" strike line
            return (ts, CZ)
        end

        if !ismissing(cBid) # && !ismissing(cAsk)
            fcall(ts, xpir, strike, cBid, cAsk, cLast, cVol, cDelta, cGamma, cVega, cTheta, cRho, cIv)
        else
            @log hand "skipped call" strike line
        end
        if !ismissing(pBid) # && !ismissing(pAsk)
            fput(ts, xpir, strike, pBid, pAsk, pLast, pVol, pDelta, pGamma, pVega, pTheta, pRho, pIv)
        else
            @log hand "skipped put" strike line
        end

        return (ts, under)
    catch e
        println(line)
        rethrow(e)
    end
end

parsem(type::Type{Int}, s) = isempty(strip(s)) ? error("missing int") : parse(type, s)
parsem(type::Type{Float64}, s) = isempty(strip(s)) ? NaN : parse(type, s)
parsem(type::Type{Currency}, s) = isempty(s) ? error("missing currency") : Int(parse(type, s))

function test_line(line)
    fcall = (args...) -> println("Parsed call: ", args)
    fput = (args...) -> println("Parsed put: ", args)
    procLine(line, fcall, fput)
end

end