module KellyTest
using Test, Statistics
using TestUtil
using Kelly

function runTests()
    # ignore for now
    return

    @testset "CalcKelly" begin
    end

    @testset "Basics" begin
        buf = Vector{Float64}(undef, outercount)
        tested = 0
        for i in 1:5
            probWin = 0.2 + rand() / 2.0
            win = .2 + rand() / 2.0
            loss = win * (1.0 + 0.4 * (rand() - 0.5))
            # @info "test" probWin win loss
            kel = Kelly.simple(probWin, win, loss)
            if isfinite(kel) && 0.1 < kel < .6
                tested += 1
                rk = simKelly(buf,probWin, win, loss, kel)
                rkr = simKelly(buf, probWin, win, loss, 1.2 * kel)
                rkl = simKelly(buf, probWin, win, loss, .8 * kel)
                @test rkl < rk
                @test rkr < rk
                (rkl > rk || rkr > rk) && (@info "ik" rk rkl rkr i tested kel probWin win loss ; break)
            end
        end
    end
end

outercount = 1
function simKelly(buf, probWin::Float64, win::Float64, loss::Float64, ratio::Float64)
    win = win/loss
    lose = -1
    wins = 0; losses = 0
    for trial in 1:outercount
        balance = 1.0
        for _ in 1:10
            isWin = rand() <= probWin
            isWin ? (wins += 1) : (losses += 1)
            prebal = balance
            balance += (isWin ? win : lose) * (balance * ratio)
            # @info "bal" prebal balance isWin win lose (balance*ratio)
            if balance <= 0.0000001
                println("balance 0")
                balance = 0.0
                break
            end
            # @info "balance" probWin isWin win -loss ratio balance
            if (1.0+ratio) * balance > 1E+20
                # println("hit max ", balance)
                break
            end
        end
        buf[trial] = balance
    end
    # @info "check prob" probWin (wins / (wins+losses)) length(buf)
    println(buf)
    return median(buf)
end

# @testset "TestCalcSimpleKelly" begin
#     # kelly = winprob + (winprob - 1)/gain%
#     @test isnan(kellySimple([.5, .5], [1.0, -1.0]))
#     @test Kelly.kellySimple([.6, .4], [1.0, -1.0]) ≅ 0.6 + (0.6 - 1.0) / 1.0
#     @test Kelly.kellyNormalize([.73, .27], [0.5, -.75]) ≅ 0.73 + (0.73 - 1.0) / (.5/.75)
#     @test Kelly.kellyNormalize([.73, .27], [0.5/.75, -1.0]) ≅ 0.73 + (0.73 - 1.0) / (.5/.75)

#     @test Kelly.kellyNormalize([.8, 0, .2], [0.5, -0.5, -0.75]) ≅ 0.8 + (0.8 - 1.0) / (.5/.75)

#     #region Dive
#     for x in 0.1:0.05:0.9 @test kellyTermDeriv(0.1, 0.0, x) ≅ 0.0 end
#     vp = [.8, .1, .1]; vr = [0.5/0.75, 0.0, -1.0];
#     ratio = kellySimple(vp, vr)
#     @test kellySimpleF(vp, vr)(ratio) ≅ 0.0
#     @test ratio ≅ .7222222222222222

#     # Two points right around the max should be a less
#     x1 = kellySimpleF(vp, vr, kellyTerm)(ratio-0.001)
#     x2 = kellySimpleF(vp, vr, kellyTerm)(ratio)
#     x3 = kellySimpleF(vp, vr, kellyTerm)(ratio+0.001)
#     @test x1 < x2 && x3 < x2
#     #endregion

#     @test Kelly.kellyNormalize([.8, .1, .1], [0.5, 0.0, -.75]) ≅ 0.722222222222 # NOT: (0.8+0.1) + ((0.8+0.1) - 1.0) / ((.8*.5/.9)/.75)
#     @test Kelly.kellyNormalize([.9, .1], [(0.8*0.5/.9), -0.75]) ≅ .73125 ≅ 0.9 + (0.9 - 1.0) / ((0.8*0.5/.9)/.75)

#     @test Kelly.kellyNormalize([.5, .25, .25], [1.0, -0.25, -0.25]) ≅ 0.5 + (0.5 - 1.0) / (1.0/.25)
#     # @test kellySimple([.5, .25, .25], [1.0, -0.4, -0.2]) ≅  # NOT: 0.5 + (0.5 - 1.0) / (1.0/.3)
#     @test Kelly.kellyNormalize([.2, .2, .2, .4], [6.0, 2, -1, -2]) ≅ 0.155587 # NOT: (0.2 + 0.2) + ((0.2 + 0.2) - 1.0) / ( (.2*6+.2*2)/.4 / ( (.2*1+.4*2)/.6   ))
# end

# @testset "TestCalcDistriKelly" begin
#     # risk = stats(Test1.di, Test1.sp, Test1.ss)
#     # @info risk
#     # kel = kelly(Test1.sp, Test1.di, -risk.maxLoss, Test1.ss)
#     # @info kel

#     # si = info = stratInfo(bapOLF, Test1.di, Test1.sp, Test1.lqs)
#     # @info si
#     # @test kelly(Test1.sp, Test1.di, Test1.ss, risk)) ≅ 1.0)

#     # function kelly(cp::Float64, distri::BinDistri, risk::Float64, ss::SegSet)::Float64
#     risk = -maxLoss(Test1.info)
#     probs = calcProbsDI(Test1.di)
#     ratio1 = calcKelly(Test1.scss, risk, probs)
#     # vp, vr = InfoStrats.kellyPR(Test1.sp, Test1.di, -Test1.info.stats.maxLoss, Test1.ss)
#     # rets = InfoStrats.calcRets(Test1.di, Test1.ss, Test1.sp) ./ risk
#     # ratio2 = InfoStrats.calcKelly2(Test1.sp, Test1.di, risk, Test1.ss)

#     x1 = InfoStrats.kellyDistriF(Test1.sp, Test1.di, risk, Test1.ss, Kelly.kellyTermR)(ratio1)
#     x2 = kellySimpleF(probs, rets, kellyTerm)(ratio2)

#     @info "Kelly: " ratio1 x1
#     @info "Kelly2: " ratio2 x2
# end


# @testset "Test impossibly high kelly" begin
#     # TODO: test a double diagonal that the lower/upper bin rets use full max risk loss and not just at that point
# end

end

# #=
# https://math.stackexchange.com/questions/662104/kelly-criterion-with-more-than-two-outcomes

# maximize: ∑ p_i log(1 + b_i x)
# for 2 cases: p1 * log (1 + b1 x) + p2 * log (1 + b2 x)
# find zero of derivative:
# 0 = p1 * b1 / (1 + b1 x) + p2 * b2 / (1 + b2 x)
# let's say b2 is the losing side, so divide it out, and p2 = 1 - p1, and multiply (1 + b2 x)
# 0 = p1 * (b1/b2) * (1 + b2 x) / (1 + b1 x) + (1 - p1)

# 0 = p*b/(1 + b*x) + (1-p)*c/(1 + c*x)
# WA solved: x = (p - 1) / b which matches Kelly's formula

# For: [0.8, 0.1, 0.1], [2/3, 0.0, -1.0]
# 0 = 0.8*(2/3)/(1 + (2/3)*x) - 0.1*1.0/(1 - 1.0*x)
# WA solved: x = 0.72222222
# but with trying to estimate and use 2 part equation: (p-1)/b:
# (0.8+0.1) + ((0.8+0.1) - 1.0) / ((.8*(2/3)/.9)/1.0) = .73125
# So, they don't match.

# For: [.2, .2, .2, .4], [6.0, 2, -1, -2], first normalize to: [3.0, 1, -0.5, -1]
# 0 = 0.2*3/(1+3*x) + .2*1/(1+1*x) - .2*.5/(1-.5*x) - .4*1/(1-1*x)
# WA solved: x = 0.155587

#  =#

#=
Maximize the log of bankroll for each case
    probability of this case * log(resulting balance)
    if stock goes to R, then we take the ratio times the return and add it to the balance:
        M + (M * ratio) * return
    but we need this in terms of the option outcomes... what is the return per dollar spent?
    if we use max loss, then M * ratio / max loss = number of contracts
        and return = M*ratio/maxloss * return
        sum( pi * log(1 + ratio * return/maxloss))
=#