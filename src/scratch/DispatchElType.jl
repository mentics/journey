module Scratches

struct ElType{T} end

testET(itr, args...) = testET(ElType{eltype(itr)}, itr, args...)
(testET(::Type{ElType{E}}, itr, args...)::E) where E = error("Undefined testElType for ", E)
(testET(::Type{ElType{Float64}}, itr, args...)::Float64) = doit(itr, args[1])

(testETR(::Type{R}, itr, args...)::R) where R = testET(ElType{eltype(itr)}, itr, args...)
(testETR(::Type{R}, ::Type{ElType{E}}, itr, args...)::R) where {R,E} = error("Undefined testElType for ", E)
(testETR(::Type{Float64}, ::Type{ElType{Float64}}, itr, args...)::Float64) = doit(itr, args[1])



testElType(itr, args...) = testElType(ElType{eltype(itr)}, itr, args...)
(testElType(::Type{ElType{E}}, itr, args...)::E) where E = error("Undefined testElType for ", E)
(testElType(::Type{ElType{Float64}}, itr, args...)::Float64) = doit(itr, args[1])

testElType2(itr, args...) = runElType(eltype(itr), itr, args...)
(runElType(::Type{E}, itr, args...)::E) where T where E = error("Undefined testElType for ", E)
(runElType(::Type{Float64}, itr, args...)::Float64) where T = doit(itr, args[1])

testElType3(itr::T, args...) where T = runElType3(eltype(T), itr, args...)
(runElType3(::Type{E}, itr, args...)::E) where T where E = error("Undefined testElType for ", E)
(runElType3(::Type{Float64}, itr, args...)::Float64) where T = doit(itr, args[1])

testElType4(itr::T, arg) where T = runElType4(eltype(T), itr, arg)
(runElType4(::Type{E}, itr, arg)::E) where T where E = error("Undefined testElType for ", E)
(runElType4(::Type{Float64}, itr, arg)::Float64) where T = doit(itr, arg)

testDirect(itr::T, args...) where T = error("Undefined testDirect for ", T) # doit(itr, args[1])
testDirect(itr::Vector{Float64}, args...) = doit(itr, args[1])

testDirect2(::Type{ElType{E}}, itr, args...) where E = error("Undefined testElType for ", E)
testDirect2(itr::Vector{Float64}, args...) = testDirect2(ElType{Float64}, itr, args...)
# testDirect2(::Type{ElType{Float64}}, itr::Vector{Float64}, args...) = doit(itr, args[1])
testDirect2(::Type{ElType{Float64}}, itr, args...) = doit(itr, args[1])

# (testElType(::Type{ElType{Float64}}, itr, args...)::Float64) where T = doit(itr, args[1])
testElType(::Type{ElType{Float64}}, itr, args...)::Float64 = doit(itr, args[1])

@generated function testGen(itr::T, args...) where T
    typ = eltype(T)
    # :(testDirect2(ElType{$typ}, itr, args...))
    :(testElType(ElType{$typ}, itr, args...))
end

doit(itr, i) = sum(x -> x + i, itr)

end