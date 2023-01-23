module BackTypes
using BaseTypes, LegMetaTypes

export Strat, Account, TradeBTOpen, TradeBTClose, TradeBT, MarginSide, MarginInfo

abstract type Strat end

struct TradeBTOpen{N}
    id::Int
    lms::NTuple{N,LegMetaOpen}
    neto::Currency
    multiple::Int
end

struct TradeBTClose{N}
    lms::NTuple{N,LegMetaClose}
    netc::Currency
end

struct TradeBT
    open::TradeBTOpen
    close::TradeBTOpen
end

mutable struct Account
    bal::Currency
    open::Vector{TradeBTOpen}
    closed::Vector{TradeBT}
    nextTradeId::Int
end

struct MarginSide
    margin::Currency
    count::Int
end
marginSideZero() = MarginSide(CZ, 0)

struct MarginInfo
    margin::Currency
    marginBalRat::Float64
    call::MarginSide
    put::MarginSide
end
marginZero() = MarginInfo(CZ, 0.0, marginSideZero(), marginSideZero())

end