module BaseUtil

export isSomething, coal, toEnum

isSomething(v) = !isnothing(v) && !ismissing(v)

function coal(args...)
    for x in args
        isSomething(x) && return x
    end
    return nothing
end

macro coal(args...)
    expr = :(nothing)
    for arg in reverse(args)
        expr = :((val = $arg) |> isSomething ? val : $expr)
    end
    return esc(:(let val; $expr; end))
end

toEnum(mod::Module, sym::Symbol) = getproperty(mod, sym)
toEnum(mod::Module, s::AbstractString) = toEnum(mod, Symbol(s))

end