module FileUtil
using JSON3, DelimitedFiles
using BaseTypes

export loadJson, loadJson!, writeJson
export readLastLines
export writeStr, writeCsv, appendCsv, readCsv

#region Json
function (loadJson(path::AStr, typ::Type{T}=Dict)::T) where T
    if isfile(path)
        return JSON3.read(read(path, String), typ)
    else
        return typ()
    end
end
loadJson!(path::AStr, o) = JSON3.read!(read(path, String), o)

writeJson(path, d::Dict) = open(path, "w") do io; JSON3.pretty(io, d; allow_inf=true) end
writeJson(path, o) = open(path, "w") do io; JSON3.pretty(io, o; allow_inf=true) end
#endregion


function writeStr(path::AStr, str::AStr)
    open(path, "w") do io
        write(io, str)
    end
end

function writeCsv(path::AbstractString, data::Vector{<:Dict}; keys=keys(data[1]))
    mat = [r[key] for r in data, key in keys]
    out = vcat(reshape(collect(keys), 1, :), mat)
    writedlm(path, out, ',')
end
function appendCsv(path::AStr, data::Vector{<:Dict}; keys=keys(data[1]))
    mat = [r[key] for r in data, key in keys]
    open(path, "a") do io
        writedlm(io, mat, ',')
    end
    nothing
end
writeCsv(path::AStr, data::Union{Vector{Tuple},Vector{NamedTuple}}) = writedlm(path, data, ',')
readCsv(path::AStr; kws...) = readdlm(path, ','; kws...)
readTsv(path::AStr; kws...) = readdlm(path, '\t'; kws...)
readTsv(path::AStr, type::Type; kws...) = readdlm(path, '\t', type; kws...)

function readLastLines(path::AStr, n::Int=1, maxLineLength::Int=1000)::String
    result = IOBuffer()
    open(path) do io
        seekend(io)
        pos = position(io)-1
        lineCount = 0
        lineLen = 0

        # Ignore the last carriage return
        seek(io, pos)
        char = read(io, Char)
        if char == '\n'
            pos -= 1
        end

        while lineLen < maxLineLength
            seek(io, pos)
            char = read(io, Char)
            # @info "$(pos) $(char)"
            if char == '\n'
                lineCount += 1
                lineLen = 0
                if lineCount == n
                    break
                end
            end
            write(result, char)
            pos -= 1
        end
    end
    return String(reverse(take!(result)))
end

end