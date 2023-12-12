module ProbInfer

import HistShapeModel
import ReturnProbModel

#region API
function make_infer()
    HistShapeModel.MLTrain
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