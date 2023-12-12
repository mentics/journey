module ProbInfer

# TODO: this might not be needed because I've decided to always keep data up to date (within 15-30 minute or so)

import HistShapeModel
import ReturnProbModel

struct RetProbInfer
    hist_trainee
    hist_model
    retprob_trainee
    retprob_model
end

#region API
function make_infer()
    hist_trainee = HistShapeModel.MLTrain()
    hist_model = MLInfer.setup(hist_trainee)
    retprob_trainee = ReturnProbModel.MLTrain()
    retprob_model = MLInfer.setup(prob_trainee)
    return RetProbInfer(hist_trainee, hist_model, retprob_model, retprob_trainee)
end

function infer(inferer::RetProbInfer)
    hist_batchx =
    hist_encoded = inferer.hist_trainee.run_infer(inferer.hist_model, hist_batchx)
    retprob_batchx = # include hist_encoded
    prob = inferer.retprob_trainee.run_infer(inferer.retprob_model, retprob_batchx)
    return prob
end
#endregion API

end