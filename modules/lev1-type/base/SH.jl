module SH

export Vals

export getStyle, getExpiration, getStrike, getSide, getQuantity, getAction
export getBid, getAsk
export toCode, toOther
export getIv
export getOption, getLeg, getMeta, getQuote
export calcQuote, calcOptQuote
export getQuantityDir, addQuantity
export isCall, isPut, isLong, isShort

export getVals, getVals!, valAt, getCenter, draw, draw!

export getNetOpen, getNetClose, getLegs, getPnl
export bap
export isValid

# export tsOpen, tsClose, getNetClose
# export getMaxClose

export to, tos, combineTo, mapFlattenTo

const Vals = Vector{Float64}

function getBid() end
function getAsk() end

function getStyle() end
function getStrike() end
function getExpiration() end

function getSide() end
function getQuantity() end
function getAction() end

function getOption() end
function getQuote() end
function getMeta() end
function getLeg() end

function toCode() end
function toOther() end

function getIv() end

function calcQuote() end
function calcOptQuote() end

function getQuantityDir() end

function addQuantity() end

function isLong() end
function isShort() end
function isCall() end
function isPut() end

# for Order
export getId, getSymbol, getClass, getOrderType, getStatus, getPrimitDir, getPrillDir, tsCreated, tsFilled, tsClosed
function getId() end
function getClass() end
function getSymbol() end
function getOrderType() end
function getStatus() end
function getPrimitDir() end
function getPrillDir() end
function tsCreated() end
function tsFilled() end
function tsClosed() end

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
function draw(::Nothing) end
function draw!(::Nothing) end

# these are for prill's * quantity
function getNetOpen() end
function getNetClose() end
function getLegs() end

function bap() end

function isValid() end

## conversions

# function mapFlattenTo(gen, ::Type{T}, itr)::Base.Generator where T; Iterators.map(x -> to(T, x), Iterators.flatten(Iterators.map(gen, itr))) end # TODO: is closure here optimum?
function mapFlattenTo(gen, ::Type{T}, itr, args...)::Base.Generator where T; Iterators.map(x -> to(T, x, args...), Iterators.flatten(Iterators.map(gen, itr))) end # TODO: is closure here optimum?
# tos(::Type{T}, itr) where T = map(x -> to(T, x), itr) # TODO: is closure here optimum?
tos(::Type{T}, itr, args...) where T = map(x -> to(T, x, args...), itr) # TODO: is closure here optimum?
function combineTo() end
# function combineTo(::Type{T}, itr, args...)::T where T end
function to() end
# function to(::Type{T}, x, args...)::T where T end
to(::Type{T}) where T = x -> to(T, x)

end