module BackTypes
import Dates:DateTime
using SH, BaseTypes, LegMetaTypes

# TODO: some things should be PriceT instead of Currency

export Strat, Account, TradeBTOpen, TradeBTClose, TradeBT, MarginSide, MarginInfo

abstract type Strat end

function pricingOpen end
function pricingClose end
function checkExit end

abstract type TradeBTStage end

struct TradeBTOpen{N,E} <: TradeBTStage
    id::Int
    ts::DateTime
    lms::NTuple{N,LegMetaOpen}
    neto::Currency
    multiple::Int
    label::String
    extra::E
end

struct TradeBTClose{N} <: TradeBTStage
    ts::DateTime
    lms::NTuple{N,LegMetaClose}
    netc::Currency
    label::String
end
SH.getExpir(t::TradeBTStage) = getExpir(t.lms)

struct TradeBT{N,E}
    open::TradeBTOpen{N,E}
    close::TradeBTClose{N}
end

mutable struct Account{N,E}
    bal::Currency
    open::Vector{TradeBTOpen{N,E}}
    closed::Vector{TradeBT{N,E}}
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