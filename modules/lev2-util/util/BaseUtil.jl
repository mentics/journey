module BaseUtil

export isSomething, coal, toEnum

isSomething(v) = !isnothing(v) && !ismissing(v)

function coal(args...)
    for x in args
        isSomething(x) && return x
    end
    return nothing
end

toEnum(mod::Module, sym::Symbol) = getproperty(mod, sym)
toEnum(mod::Module, s::AbstractString) = toEnum(mod, Symbol(s))

end