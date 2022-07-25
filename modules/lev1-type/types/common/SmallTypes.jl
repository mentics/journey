module SmallTypes
using EnumX
using SH

export Style, Side, Action
export Styles, Sides
export checkDirOrder, checkDirTrade

@enumx Style call=1 put=-1
SH.toCode(s::Style.T) = s === Style.call ? 'c' : 'p'
SH.to(::Type{Style.T}, s::AbstractString) = s in ("c", "C", "call") ? Style.call : (s in ("p", "P", "put") ? Style.put : error("Invalid Style code ", s))
# SH.random(::Type{Style.T}) = rand((Style.call, Style.put))

struct Styles{T}
    call::T
    put::T
end

@enumx Side long=1 short=-1
SH.toOther(x::Side.T) = x === Side.long ? Side.short : Side.long
SH.toCode(s::Side.T) = s === Side.long ? 'l' : 's'
SH.to(::Type{Side.T}, s::AbstractString) = s in ("l", "L", "long") ? Side.long : (s in ("s", "S", "short") ? Side.short : error("Invalid Side code ", s))
# SH.random(::Type{Side.T}) = rand((Side.call, Side.put))

struct Sides{T}
    long::T
    short::T
end

@enumx Action open=1 close=-1
SH.toOther(x::Action.T) = x === Action.open ? Action.close : Action.open
SH.toCode(s::Action.T) = s === Action.open ? 'o' : 'c'
SH.to(::Type{Action.T}, s::AbstractString) = s in ("o", "O", "open") ? Action.open : (s in ("c", "C", "close") ? Action.close : error("Invalid Action code ", s))
# SH.random(::Type{Action.T}) = rand((Action.call, Action.put))

checkDirOrder(side::Side.T, n::Number)::Bool = iszero(n) || Int(side) * n < 0
checkDirTrade(action::Action.T, side::Side.T, n::Number)::Bool = iszero(n) || xor(signbit(Int(action) * Int(side)), signbit(n))

export isCall, isPut, isLong, isShort
isCall(s::Style.T) = s == Style.call
isCall(o) = getStyle(o) == Style.call

isPut(s::Style.T) = s == Style.put
isPut(o) = getStyle(o) == Style.put

isLong(o) = getSide(o) == Side.long
isShort(o) = getSide(o) == Side.short

end