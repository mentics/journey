module LogUtil
using Dates
#Crayons
using ThreadUtil

export nstr, @log, @logret, @logerr, resetLog, @tail, @logboth
export prout, prerr

function prout(args...)
    runSync(lockPrint) do
        println(args...)
    end
end

function prerr(e)
    runSync(lockPrint) do
        showerror(stdout, e, catch_backtrace())
    end
end

nstr(n::Real)::String = string(round(n; digits=4))

macro tail(sym, n=10, strx=".*")
    blk = Expr(:call, :tailit)
    push!(blk.args, QuoteNode(sym), n, string(strx))
    return Expr(:block, blk)
end

function tailit(typ::Symbol, n::Integer, strx::String)
    rx = Regex(strx)
    prout.(filter(x->!isnothing(match(rx, x)), last(eachline(pathOut(typ)), n)))
    return
end

macro log(exs...)
    prblk = Expr(:call, :logit)
    inner(exs, prblk)
    return Expr(:block, prblk)
end

macro logboth(exs...)
    prblk = Expr(:call, :logboth)
    inner(exs, prblk)
    return Expr(:block, prblk)
end

# function logexc(e)
#     runSync(LOCK) do
#         prout(streamInfo[], "Error: [$(now(UTC))]")
#         showerror(stderr, e, catch_backtrace())
#         prout(streamInfo[])
#         showerror(streamInfo[], e, catch_backtrace())
#         prout(streamInfo[])
#         prout(streamInfo[])
#     end
# end

macro logret(exs...)
    # prblk = Expr(:call, :logit, "$(ERROR("ERROR")): ")
    prblk = Expr(:call, :logboth, QuoteNode(:error))
    inner(exs, prblk)
    return Expr(:block, prblk, :(return))
end

macro logerr(exs...)
    # prblk = Expr(:call, :logit, "$(ERROR("ERROR")): ")
    callBlock = Expr(:call, :logerr, QuoteNode(:error))
    inner(exs, callBlock)
    return Expr(:block, callBlock)
end

#region Local
const BasePath = Ref{String}("C:/data/log")
mutable struct Outfile
    typ::Symbol
    lock::ReentrantLock
    stream::IOStream
end
const Outs = Dict{Symbol,Outfile}()

const lockPrint = ReentrantLock()

# preloading the most likely keys to avoid needing to lock for multiple threads (ie. few/no changes to the dict keys)
function init(basePath)
    BasePath[] = basePath
    foreach(init, (:debug, :info, :warn, :error, :sql, :tradier))
end

pathOut(sym::Symbol) = joinpath(BasePath[], string(sym)*".log")
ensureOpen(out::Outfile) = isopen(out.stream) || (out.stream = open(pathOut(out.typ), "a"))
ensureClosed(out::Outfile) = isopen(out.stream) && close(out.stream)
closeAll() = foreach(ensureClosed, values(Outs))
clearFile(path)::Nothing = ( open(path, "w") do file write(file, "") end; return )
deleteAll()::Nothing = ( foreach(ensureClosed, values(Outs)) ; foreach(rm, filter(endswith(".log"), readdir(BasePath[],join=true))) ; empty!(Outs) ; return )

function formatArgs(args...)
    return ("[$(now(UTC))] ", args...)
end

function inner(exs, callBlock)
    dest = exs[1]
    push!(callBlock.args, QuoteNode(dest))
    for ex in exs[2:end]
        if ex isa AbstractString || (ex isa Expr && ex.head == :string)
            push!(callBlock.args, esc(ex), " ")
        else
            # push!(prblk.args, string(ex)*"=", Expr(:call, esc(:repr), esc(ex)), " ")
            push!(callBlock.args, string(ex)*"=", esc(ex), " ")
        end
    end
end

function logerr(typ::Symbol, args...)
    logboth(typ, args...)
    error(args...)
end

function logboth(typ::Symbol, args...)
    content = formatArgs(args...)
    prout(content...)
    loglog(typ, content)
end

function logit(typ::Symbol, args...)
    loglog(typ, formatArgs(args...))
end

function loglog(typ::Symbol, content)
    haskey(Outs, typ) || init(typ)
    out = Outs[typ]
    runSync(out.lock) do
        ensureOpen(out)
        prout(out.stream, content...)
        flush(out.stream)
    end
    return
end

function init(sym::Symbol)
    eam = open(pathOut(sym), "a")
    Outs[sym] = Outfile(sym, ReentrantLock(), eam)
end

function resetLog(syms::Symbol...)
    for sym in syms
        haskey(Outs, sym) || continue
        out = Outs[sym]
        runSync(out.lock) do
            ensureClosed(out)
            clearFile(pathOut(out.typ))
            init(sym)
        end
    end
end
#endregion

end