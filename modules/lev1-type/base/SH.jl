module SH

# TODO: The convention is to use `function emptyfunc end` according to https://docs.julialang.org/en/v1/manual/methods/#Empty-generic-functions

export Vals

export getStyle, getExpir, getStrike, getSide, getQuantity, getAction
export getBid, getAsk
export toCode, toOther
export getIv
export getOption, getLeg, getMeta, getOptionMeta, getQuote, getGreeks
export calcQuote, calcOptQuote
export getQuantityDir, addQuantity # , withQuantity

export getVals, getVals!, valAt, getCenter

export getNetOpen, getNetClose, getLegs, getPnl
export bap
export isValid
export getDateOpen, getRisk
export getTargetDate

# export tsOpen, tsClose, getNetClose
# export getMaxClose

export to, tos, tosn, tosnn, combineTo, mapFlattenTo

export v
(v(x::Dict{K,V})::Vector{V}) where {K,V} = collect(values(x))

const Vals = Vector{Float64}

export toDraw
function toDraw end

function getBid() end
function getAsk() end

function getStyle() end
function getStrike() end
getExpir(itr) = minimum(getExpir, itr)

function getSide() end
function getQuantity() end
function getAction() end

function getOption() end
function getQuote() end
function getMeta() end
function getOptionMeta() end
function getLeg() end
function getGreeks() end

function toCode() end
function toOther() end

function getIv() end

function calcQuote() end
function calcOptQuote() end

function getQuantityDir() end

function addQuantity() end
# function withQuantity() end

# for Order
export getId, getSymbol, getClass, getOrderType, getPrimitDir, getPrillDir, tsCreated, tsFilled, tsClosed, isStatus
function getId() end
function getClass() end
function getSymbol() end
function getOrderType() end
function getPrimitDir() end
function getPrillDir() end
function tsCreated() end
function tsFilled() end
function tsClosed() end
# function isLive() end
# function isDeleted() end
function isStatus() end

# for Trade
export getPrillDirOpen, getPrillDirClose
function getPrillDirOpen() end
function getPrillDirClose() end
function getPnl() end

## not sure

function getVals() end
function getVals!() end
function valAt() end
# function getValMid() end
function getCenter() end
# function draw(::Nothing) end
# function draw!(::Nothing) end

# these are for prill's * quantity
getNetOpen(itr) = sum(getNetOpen, itr)
function getNetClose() end
function getLegs() end

function isValid() end

function getDateOpen end
function getRisk end
function getTargetDate end

## conversions

# function mapFlattenTo(gen, ::Type{T}, itr)::Base.Generator where T; Iterators.map(x -> to(T, x), Iterators.flatten(Iterators.map(gen, itr))) end # TODO: is closure here optimum?
function mapFlattenTo(gen, ::Type{T}, itr, args...)::Base.Generator where T; Iterators.map(x -> to(T, x, args...), Iterators.flatten(Iterators.map(gen, itr))) end # TODO: is closure here optimum?
# tos(::Type{T}, itr) where T = map(x -> to(T, x), itr) # TODO: is closure here optimum?
tosnn(::Type{T}, itr, args...) where T =
        filter(!isnothing, map(x -> to(Union{Nothing,T}, x, args...), itr)) # TODO: is closure here optimum?
tos(::Type{T}, itr, args...) where T = map(x -> to(T, x, args...), itr) # TODO: is closure here optimum?
tosn(::Type{T}, itr, args...) where T = mapStopNothing(x -> to(T, x, args...), itr)
mapStopNothing(f, t::Tuple{Any}) = (@inline; y = f(t[1]) ; isnothing(y) ? nothing : (y,))
function mapStopNothing(f, t::Tuple{Any,Any})
    @inline;
    y1 = f(t[1])
    !isnothing(y1) || return nothing
    y2 = f(t[2])
    return isnothing(y2) ? nothing : (y1, y2)
end
function mapStopNothing(f, t::Tuple{Any,Any,Any})
    @inline;
    y1 = f(t[1])
    !isnothing(y1) || return nothing
    y2 = f(t[2])
    !isnothing(y2) || return nothing
    y3 = f(t[3])
    return isnothing(y3) ? nothing : (y1, y2, y3)
end
function mapStopNothing(f, t::Tuple{Any,Any,Any,Any})
    @inline;
    y1 = f(t[1])
    !isnothing(y1) || return nothing
    y2 = f(t[2])
    !isnothing(y2) || return nothing
    y3 = f(t[3])
    !isnothing(y3) || return nothing
    y4 = f(t[4])
    return isnothing(y4) ? nothing : (y1, y2, y3, y4)
end

export ElType
struct ElType{T} end
(combineTo(::Type{R}, itr, args...)::R) where R = combineTo(R, ElType{eeltype(itr)}, itr, args...)
(combineTo(::Type{R}, ::Type{ElType{E}}, itr, args...)::R) where {R,E} = error("Undefined combineTo for ", R, ' ', E, ' ', args)
function eeltype(itr)
    e = eltype(itr)
    return e != Any ? e : eltype(first(itr.it))
end

# (testETR(::Type{R}, itr, args...)::R) where R = testET(ElType{eltype(itr)}, itr, args...)
# (testETR(::Type{R}, ::Type{ElType{E}}, itr, args...)::R) where {R,E} = error("Undefined testElType for ", E)
# (testETR(::Type{Float64}, ::Type{ElType{Float64}}, itr, args...)::Float64) = doit(itr, args[1])

function to() end
# function to(::Type{T}, x, args...)::T where T end
to(::Type{T}) where T = x -> to(T, x)

## Extra
# export lms
# function lms() end
##

end