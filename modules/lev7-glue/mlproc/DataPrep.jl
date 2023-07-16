module DataPrep
using DataFiles

run(f, from::DateLike, to::DateLike; maxSeconds=1000000, max_iterations=typemax(Int)) = run(f, getTss(from, to); maxSeconds, max_iterations)
run(f, range::Interval; maxSeconds=1000000, max_iterations=typemax(Int)) = run(f, getTss(range); maxSeconds, max_iterations)
function run(f, tss; maxSeconds=1000000, max_iters=typemax(Int))
    tss = DataFiles.tsstable()

end

end