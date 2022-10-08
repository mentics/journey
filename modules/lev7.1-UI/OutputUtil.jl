module OutputUtil
using Dates, JSON3, Crayons
using Tables, PrettyTables
using BaseTypes
using DateUtil

# TODO: go over all this

export confirm
export pretty, pretyble, spretyble, niceShow, sho
export PRI, SEC
export r2, r3, rd2, rd3

r2(x) = round(x; sigdigits=2)
r3(x) = round(x; sigdigits=3)
rd2(x) = round(x; digits=2)
rd3(x) = round(x; digits=3)

function confirm()
    print("Are you sure? (N/y)")
    input = readline()
    return input == "y"
end

PRI = crayon"bold green"
SEC = crayon"bold blue"
(c::Crayon)(x::Union{PriceT,Currency,Float64}) = (c::Crayon)(string(x))

#region Tables Interface
# row1 = [2, 3, 4]
# row2 = [6, 7, 8]
# vv = [row1, row2]
# pretty_table(vv; header=["asdf", "234rsdf", "sdfsdf"])

function pretty(d)::String
    io = IOBuffer()
    JSON3.pretty(io, d; allow_inf=true)
    return String(take!(io))
end

Tables.istable(::Type{<:AbstractVector{<:AbstractVector}}) = true
Tables.rowaccess(::Type{<:AbstractVector{<:AbstractVector}}) = true

Tables.getcolumn(x::AbstractVector, i::Int) = x[i]
Tables.getcolumn(v::AbstractVector, nm::Symbol) = v[parse(Int, string(nm))]
Tables.columnnames(v::AbstractVector) = collect(map(x->Symbol(x), 1:length(v)))
#endregion

function niceShow(a)
    io = IOBuffer()
    show(IOContext(io, :limit => true, :displaysize => (20, 40)), "text/plain", a)
    return String(take!(io))
end

# ppFormat(v, _, _) = v isa Float64 ? string(round(v; digits=3)) : v
# ppFormat(v, _, _) = sho(v)
function sho(v; dateYear=false, kws...)
    if eltype(v) === Float64
        return map(x -> round(x; digits=3), v)
    elseif eltype(v) === Currency
        return map(x -> round(x; digits=3), v)
    elseif eltype(v) === PriceT
        return map(x -> round(x; digits=2), v)
    elseif eltype(v) == Date
        return join(strShort.(v), ',')#map(x -> string(v), v)
    elseif v isa DateTime
        return strShort(v)
    elseif v isa Date
        return strShort(v; dateYear)
    elseif v isa AbstractString || v isa Symbol
        return v
    # elseif !(eltype(v) isa AbstractString)
    #     return join(v, ',')#map(x -> string(v), v)
    else
        return v
    end
end

pretyble(tbl; kws...) = pretyble(stdout, tbl; kws...)
spretyble(tbl; kws...)::String = (io = IOBuffer() ; pretyble(io, tbl; kws...) ; return String(take!(io)) )
function pretyble(io, tbl; header=nothing, rowcol=false, widths=0, dateYear=false, kws...)
    Tables.isrowtable(tbl) || (tbl = collect(tbl))
    if isempty(tbl)
        println("No data")
    else
        if rowcol
            tbl = addRowcol(tbl)
        end
        # TODO: consider showing title: pretyble(to.(NamedTuple, left); title="Lots of fun legs", title_crayon=crayon"blue bg:yellow bold", title_same_width_as_table=true, title_alignment=:c)
        # might want to do our own title formatting because pretty tables centered with background color crayon didn't work right
        # pretty_table(tbl; header, formatters=ppFormat, nosubheader=true, display_size=(100,1000), columns_width=[0,0,0,0,30,0,0,0,48,20], autowrap=true)
        pretty_table(io, tbl; header, formatters=(x,_,_)->sho(x; dateYear, kws...), nosubheader=true, display_size=(100,1000), maximum_columns_width=widths, kws...)
    end
end

addRowcol(vtup) = [merge((;row=i), (vtup[i])) for i in eachindex(vtup)]

end