module CheckUtil

export setObj, obj

obj = nothing
function setObj(x)
    global obj = x
    return "[Set obj type $(typeof(x))]"
end

end