module StatusTypes

export Status, Preview, Starting, Accepted, Filled, PartialClosed, Closing, Closed, Canceled, Rejected, Expired
export WithBasic, WithFilled, WithAccepted, WithDone, OrderDone, Closeable, Deleted
export symToStatus, strToStatus, toStatus

export st
const st = @__MODULE__

abstract type Status end
abstract type Preview <: Status end
abstract type Starting <: Status end
abstract type Accepted <: Status end
abstract type PartialFilled <: Status end
abstract type Filled <: Status end
abstract type PartialClosed <: Status end
abstract type Closing <: Status end
abstract type Closed <: Status end
abstract type Canceled <: Status end
abstract type Rejected <: Status end
abstract type Expired <: Status end
const WithBasic = Union{Preview,Starting,Accepted,Canceled,Rejected}
const WithFilled = Union{Filled,PartialClosed,Closing,Closed,Expired}
const WithAccepted = Union{Accepted,Canceled,Rejected,PartialFilled,WithFilled}
const WithDone = Union{Closed,Expired,Canceled,Rejected}
const OrderDone = Union{Filled,Expired,Canceled,Rejected}
const StatusPending = Union{Starting,Accepted,PartialFilled}
const Deleted = Union{Canceled,Rejected}
const Closeable = Union{Filled,PartialClosed}
# const Open = Union{Preview,Starting,Accepted,PartialFilled,Filled,PartialClosed,Closing}
const TradeLive = Union{PartialFilled,Filled,PartialClosed,Closing}
# TODO: clean up unused of above after store simplification

export StatusClosed
const StatusClosed = (Closed, Canceled, Rejected, Expired)

(Type{<:Status})(s::String) = strToStatus(s)
strToStatus(str::AbstractString) = getproperty(@__MODULE__, Symbol(str))
symToStatus(sym::Symbol) = getproperty(@__MODULE__, sym)
toStatus(str) = StrToStatus[str]

Base.show(io::IO, x::Type{<:Status}) = print(io, x.name.name)

#region Local
# TODO: add expired
const StrToStatus = Dict{String,DataType}(
    "open" => Accepted,
    "filled" => Filled,
    "canceled" => Canceled,
    "rejected" => Rejected
)
#endregion

end