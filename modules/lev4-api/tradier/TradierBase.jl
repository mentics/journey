module TradierBase
using HTTP
using DictUtil, LogUtil
using TradierConfig

export TradierException, HttpException
export tradierGet, tradierPost, tradierPostVector

struct TradierException <: Exception
    url::String
    result::TradierResp
end

struct HttpException <: Exception
    url::String
    response::HTTP.Response
end

function tradierGet(pathQuery::AbstractString, info::CallInfo{T})::T where T
    call(pathQuery, info) do url
        try
            resp = HTTP.get(url, TradierConfig.HEADERS_GET[]; retry=true, retries=3)
            @log tradier "tradierGet:" url resp
            return resp
        catch e
            @log error "HTTP error in tradierGet:" url info
            rethrow(e)
        end
    end
end
function tradierPost(pathQuery::AbstractString, payload::AbstractString, info::CallInfo{T}; retries=0)::T where T
    call(pathQuery, info) do url
        try
            kws = retries == 0 ? (;) : (;retry=true, retries, retry_non_idempotent=true)
            resp = HTTP.post(url, TradierConfig.HEADERS_POST[], payload; kws...)
            @log tradier "tradierPost:" url payload resp
            return resp
        catch e
            @log error "HTTP error in tradierPost:" url payload
            rethrow()
        end
    end
end
# tradierPostVector(pathQuery::AbstractString, payload::AbstractString = "", save=nothing)::TradierRespVec =
#     callVector(pathQuery, save) do url; HTTP.post(url, headers(), payload) end

# function tradierGetRaw(pathQuery::AbstractString)
#     headers = (("Authorization", "Bearer $(cfg().apiKey)"),
#                ("Accept", "application/json"))
#     url = pathQuery
#     return handleResponse(url, HTTP.get(url, headers), Vector)
# end

# function tradierPut(config::TradierConfig, pathQuery::AbstractString, payload::AbstractString = "")
#     headers = (("Authorization", "Bearer $(config.apiKey)"),
#                ("Accept", "application/json"))
#                # ("Accept-Encoding", "gzip"))
#     url = "$(config.baseUrl)$(pathQuery)"
#     return handleResponse(url, HTTP.put(url, headers, payload))
# end

# function tradierDelete(config::TradierConfig, pathQuery::AbstractString)
#     headers = (("Authorization", "Bearer $(config.apiKey)"),
#                ("Accept", "application/json"))
#                # ("Accept-Encoding", "gzip"))
#     url = "$(config.baseUrl)$(pathQuery)"
#     return handleResponse(url, HTTP.delete(url, headers))
# end

#region Local
function call(f, pathQuery::AbstractString, info::CallInfo{T})::T where T
    d = ifOverride(info)
    if isnothing(d)
        url = startswith(pathQuery, "https://") ? pathQuery : toTradierUrl(pathQuery)
        response = f(url)
        if response.status != 200
            throw(HttpException(url, response))
        end
        # @info "call response" response
        str = String(response.body)
        handleRecording(info, str)
        d = parseJson(str, T)
        if T <: Dict && isTradierError(d)
            if occursin("preview", pathQuery)
                @log error "Tradier returned error, but continuing because preview" url d
            else
                throw(TradierException(url, d))
            end
        end
    end
    return d
end

isTradierError(res) = haskey(res, "errors")
#endregion

end