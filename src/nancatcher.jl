# https://gist.github.com/JeffreySarnoff/db217673bc220360dc388136ab170e76

# provide context when a NaN is generated
# IMPORTANT!! define these before overloading the arithmetic functions

function nanfound(fncall)
    stk = stacktrace() # stacktrace(catch_backtrace())
    stk = relevantframes(stk)
    info = []
    for frame in stk
        push!(info, frameinfo(frame))
    end
    return((NaN_ERROR=fncall, stack=tuple(info...,)))
end

function frameinfo(frame)
   func = frame.func
   file = String(frame.file)
   line = frame.line
   result = ("$func in $file(#$line)")
   return result
end

function relevantframes(stack::Vector{Base.StackTraces.StackFrame}; framemin=3)
    framemax = nrelevantframes(stack)
    framemin = min(framemin, framemax)
    return framemax > 0 ? stack[framemin:framemax] : Vector{Base.StackTraces.StackFrame}()
end

function nrelevantframes(stack::Vector{Base.StackTraces.StackFrame})
    nframes = length(stack)
    iszero(nframes) && return 0
    framemax = 0
    for i=1:nframes
        if :eval === stack[i].func
            break
        else
            framemax = framemax + 1
        end
    end
    framemax = max(1, framemax)
    return framemax
end

# overload basic arithmetic to catch NaN generation
# IMPORTANT!! define these after the code supporting `nanfound`

function Base.:(+)(x::Float64, y::Float64)
    z = Core.Intrinsics.add_float(x, y)
    isfinite(z) ? z : error("nan found ", x, " + ", y)
    # isnan(z) ? nanfound((:+, x, y)) : z
end

function Base.:(-)(x::Float64, y::Float64)
    z = Core.Intrinsics.sub_float(x, y)
    Core.Intrinsics.sub_float(z, z) == 0.0 || error("nan found ", x, " - ", y)
    z
    # !(isinf(z) || isnan(z)) ? z : error("nan found")
    # isfinite(z) ? z : error("nan found")
    # isnan(z) ? nanfound((:-, x, y)) : z
end

function Base.:(*)(x::Float64, y::Float64)
    z = Core.Intrinsics.mul_float(x, y)
    isfinite(z) ? z : error("nan found ", x, " * ", y)
    # isnan(z) ? nanfound((:*, x, y)) : z
end

function Base.:(/)(x::Float64, y::Float64)
    z = Core.Intrinsics.div_float(x, y)
    isfinite(z) ? z : error("nan found ", x, " / ", y)
    # isnan(z) ? nanfound((:/, x, y)) : z
end

function Base.:(%)(x::Float64, y::Float64)
    z = Core.Intrinsics.rem_float(x, y)
    isfinite(z) ? z : error("nan found ", x, " % ", y)
    # isnan(z) ? nanfound((:%, x, y)) : z
end

# test it

function grandparent(op, x, y)
    xx = x
    yy = y
    parent(op, xx, yy)
end
function parent(op, x, y)
    xx = x
    child(op, xx, y)
end
child(op, x, y) = op(x, y)

x = Inf; y = -Inf; z = 0.0

grandparent(+, x, y)
# (NaN_ERROR = (:+, Inf, -Inf), stack = ("child in .\\REPL[13](#1)", "parent in .\\REPL[12](#3)", "grandparent in .\\REPL[11](#4)", "top-level scope in REPL[15](#1)"))

z / z
# (NaN_ERROR = (:/, 0.0, 0.0), stack = ("top-level scope in REPL[22](#1)",))