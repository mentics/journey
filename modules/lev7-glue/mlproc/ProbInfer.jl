module ProbInfer

#region API
struct Input

end

function make_infer()
    get_combined_model()
    get_historical_model()
    infer = function()
    end
    return infer
end
#endregion API

#region Load
get_combined_model()
get_historical_model()
#endregion Load

end