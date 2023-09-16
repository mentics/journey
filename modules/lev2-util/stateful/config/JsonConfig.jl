module JsonConfig
using BaseTypes, SmallTypes, OptionTypes, QuoteTypes, ChainTypes, LegTypes
using TradeTypes, LegTradeTypes, StatusTypes, LegQuoteTypes, OptionMetaTypes, RetTypes
import JSON3

JSON3.StructType(::Type{<:Trade}) = JSON3.Struct()
JSON3.StructType(::Type{TradeMeta}) = JSON3.Struct()
JSON3.StructType(::Type{LegTrade}) = JSON3.Struct()
JSON3.StructType(::Type{LegTradeMeta}) = JSON3.Struct()
JSON3.StructType(::Type{<:Status}) = JSON3.StringType() # TODO: is this working/needed?
# JSON3.StructType(::Type{Type{<:Status}}) = JSON3.StringType() # TODO: is this working/needed?

JSON3.StructType(::Type{LegQuote}) = JSON3.Struct()
JSON3.StructType(::Type{OptionMeta}) = JSON3.Struct()
JSON3.StructType(::Type{Ret}) = JSON3.Struct()

JSON3.StructType(::Type{Leg}) = JSON3.Struct()
JSON3.StructType(::Type{OptionQuote}) = JSON3.Struct()
JSON3.StructType(::Type{Option}) = JSON3.Struct()
JSON3.StructType(::Type{Quote}) = JSON3.Struct()

JSON3.StructType(::Type{Style.T}) = JSON3.StringType()
JSON3.StructType(::Type{Side.T}) = JSON3.StringType()
JSON3.StructType(::Type{Action.T}) = JSON3.StringType()

JSON3.StructType(::Type{Currency}) = JSON3.CustomStruct()
JSON3.StructTypes.lower(x::Currency) = Float64(x)
JSON3.construct(::Type{Currency}, x::Number) = Currency(x)
JSON3.construct(::Type{Currency}, x::String) = parse(Currency, x)

JSON3.StructType(::Type{PriceT}) = JSON3.CustomStruct()
JSON3.StructTypes.lower(x::PriceT) = Float64(x)
JSON3.construct(::Type{PriceT}, x::Number) = PriceT(x)
JSON3.construct(::Type{PriceT}, x::String) = parse(PriceT, x)

end