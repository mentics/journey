module BackTypes
import Dates:DateTime
using SH, BaseTypes, SmallTypes, LegQuoteTypes

export Strat, Account, TradeBTOpen, TradeBTClose, TradeBT, MarginSide, MarginInfo, marginZero, marginSideZero

function pricingOpen end
function pricingClose end
function checkExit end
function hasMultExpirs end

abstract type Strat end
abstract type TradeBTStage end

struct TradeBTOpen{N,E} <: TradeBTStage
    id::Int
    ts::DateTime
    lms::NTuple{N,LegQuoteOpen}
    neto::PT
    margin::Sides{PT}
    multiple::Int
    label::String
    extra::E
end

struct TradeBTClose{N} <: TradeBTStage
    ts::DateTime
    lms::NTuple{N,LegQuoteClose}
    netc::PT
    label::String
end
SH.getExpir(t::TradeBTStage) = getExpir(t.lms)

struct TradeBT{N,E}
    open::TradeBTOpen{N,E}
    close::TradeBTClose{N}
end

struct MarginSide
    margin::PT
    count::Int
end
marginSideZero() = MarginSide(CZ, 0)

struct MarginInfo
    margin::PT
    marginBalRat::Float64
    long::MarginSide
    short::MarginSide
end
marginZero() = MarginInfo(CZ, 0.0, marginSideZero(), marginSideZero())

mutable struct Account{N,E}
    bal::PT
    margin::MarginInfo
    open::Vector{TradeBTOpen{N,E}}
    closed::Vector{TradeBT{N,E}}
    nextTradeId::Int
end

end