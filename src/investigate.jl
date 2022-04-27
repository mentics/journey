# https://gist.github.com/JeffreySarnoff/db217673bc220360dc388136ab170e76
function Base.:(/)(x::Float64, y::Float64)
    r = Core.Intrinsics.div_float(x, y)
    if !isfinite(r) # !isfinite(x) || !isfinite(y) || y == 0.0 ||
        # @error "found nan" stacktrace()
        error("found nan")#stacktrace())
    end
    return r
end

function Base.:(/)(x::Float64, y::Float64)
    z = Core.Intrinsics.div_float(x, y)
    isnan(z) ? nanfound((:/, x, y)) : z
end