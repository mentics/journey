module TrainUtil

learning_rate_const(c = 1e-3) = (_...) -> c

learning_rate_linear_decay(lr_init = 1e-3) = function(epoch, loss_prev, loss)
    epoch = max(epoch, 1)
    lr = lr_init / epoch
    println("Updating learning rate: $(lr)")
    return lr
end

end