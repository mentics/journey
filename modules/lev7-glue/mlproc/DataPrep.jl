module DataPrep
using DataFiles

run(f, from::DateLike, to::DateLike; maxSeconds=1000000, max_iterations=typemax(Int)) = run(f, getTss(from, to); maxSeconds, max_iterations)
run(f, range::Interval; maxSeconds=1000000, max_iterations=typemax(Int)) = run(f, getTss(range); maxSeconds, max_iterations)
function run(f, tss; maxSeconds=1000000, max_iters=typemax(Int))
    tssall = DataFiles.tsstable().ts
    tss = Iterators.filter(tssall) do ts
        minute(ts) in (0, 30) && second(ts) != 10
    end

    y = 0
    m = 0
    for ts in tss
        yn = year(tss)
        mn = month(tss)
        if yn != y || mn != m
            calls = DataFiles.callstable(y, m)
            puts = DataFiles.putstable(y, m)
        end
        @assert calls.under === puts.under

    end
end

# DataFrame([calls1.ts, calls1.under], [:ts, :under])

#region Util
function make_xpirlokoup()
    DataFiles.xpirstable()
end
#endregion

end