module DataFrameUtil

# # Split a dataframe into the range given and everything after it
# function split_df(df, range; keycol=:ts)
#     @assert df[keycol][1] >= first(range)
#     @assert df[keycol][end] <= last(range)

# end

function in(df, range; keycol=:ts)
    # @assert df[1,keycol] <= first(range) "df[1,keycol] $(df[1,keycol]) <= $(first(range)) first(range)"
    @assert df[end,keycol] >= last(range)
    left = searchsortedfirst(df[:,keycol], first(range))
    right = searchsortedfirst(df[:,keycol], last(range))
    return df[left:right,:]
end

function after(df, ts; keycol=:ts)
    @assert df[keycol][1] <= ts
    @assert df[keycol][end] >= ts
    ind = searchsortedfirst(df[keycol], ts)
    return df[ind,:]
end

# Split a dataframe into before and after the given timestamp
function split(df, ts; keycol=:ts)
    @assert df[keycol][1] <= ts
    @assert df[keycol][end] >= ts
    ind = searchsortedfirst(df[keycol], ts)
    return df[1:ind - 1,:], df[ind,:]
end

function split_in_after(df, range; keycol=:ts)
    # @assert df[1,keycol] <= first(range)
    @assert df[end,keycol] >= last(range)
    left = searchsortedfirst(df[!,keycol], first(range))
    right = searchsortedfirst(df[!,keycol], last(range))
    return df[left:(right-1),:], df[right:end,:]
end

end