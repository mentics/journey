module TestUtil
using Globals

const TU = @__MODULE__
export TU

# export ≅, within, setupTeardown, testStart, testStop

testStart() = Globals.set(:testing, true)
testStop() = Globals.set(:testing, false)

≅(x1::Tuple{Float64,Float64}, x2::Tuple{Float64,Float64}; sigdigits=6) = ≅(x1[1], x2[1]; sigdigits) && ≅(x1[2], x2[2]; sigdigits)
≅(x1::Number, x2::Number; sigdigits=6) = round(x1; sigdigits) == round(x2; sigdigits)
≅(v1::Vector, v2::Vector; sigdigits=6) = length(v1) != length(v2) ? false : !(false in Iterators.map(xs -> ≅(xs[1], xs[2]; sigdigits), zip(v1, v2)))
# ≅(x1::Float64, x2::Float64; sigdigits=6) = round(x1; sigdigits) == round(x2; sigdigits)

within(x1::Float64, x2::Float64, k::Float64) = abs(1.0 - x1 / x2) < k

function setupTeardown(f, setup, tearDown)
    res = setup()
    try
        isnothing(res) ? f() : f(res)
    finally
        tearDown()
    end
end

# using Parsers, BaseTypes
# Base.tryparse(::Type{Currency}, s::String) = Base.parse(Currency, s)
# function testparse(s)
#     len = length(s)
#     io = IOBuffer(s)
#     Parsers.xparse(Currency, io, 1, len)
# end

function testSymbols()
    s1 = :sym1
    s2 = :sym2
    setproperty!(@__MODULE__, s1, 1)
    setproperty!(@__MODULE__, s2, 2)
    getproperty(@__MODULE__, s1)
    getproperty(@__MODULE__, s2)
end

end