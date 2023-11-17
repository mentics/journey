module TrainUtil

learning_rate_const(c = 1e-3) = (_...) -> c

learning_rate_linear_decay(lr_init = 1e-3) = function(epoch, loss_prev, loss)
    lr = lr_init / (1 + epoch)
    println("Updating learning rate: $(lr)")
    return lr
end

end