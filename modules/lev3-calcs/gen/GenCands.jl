module GenCands
using SH, BaseTypes, SmallTypes, LegMetaTypes
using ChainTypes

function iterSingle(f::Function, oqss::Oqss, args...)
    for oq in oqss.call.long
        f([to(LegMeta, oq, Side.long)], args...)
    end

    for oq in oqss.put.long
        f([to(LegMeta, oq, Side.long)], args...)
    end
end

function iterSpreads(f::Function, oqss::Oqss, args...)
    res = iterSpreads(f, oqss.call, args...)
    res = iterSpreads(f, oqss.put, args...)
end

#region Local
function iterSpreads(f::Function, oqs::Sides{Vector{ChainTypes.OptionQuote}}, args...)
    for oq1 in oqs.long, oq2 in oqs.short
        oq1 != oq2 || continue
        lms = [to(LegMeta, oq1, Side.long), to(LegMeta, oq2, Side.short)]
        f(lms, args...)
    end
end
#endregion

end