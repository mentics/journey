module TrainUtil

learning_rate_const(c = 1e-3) = (_...) -> c

learning_rate_linear_decay(lr_init = 1e-3) = function(epoch, loss_prev, loss)
    epoch = max(epoch, 1)
    lr = lr_init / epoch
    println("Updating learning rate: $(lr)")
    return lr
end

lr_cycle_decay(lower=1f-5, upper=1f-3, decay_rate=1f0, max_epochs=1000) = function(i)
    # offset = lower * max_epochs / (max_epochs + 10 * (epoch - 1))
    # (100/(100 + 10 * (i - 1))) + (100/(100 + i)) * abs(cos(sqrt(i) / 2 * π))
    lower * (max_epochs / (max_epochs + decay_rate * (i - 1))) + upper * (max_epochs / (max_epochs + (i - 1))) * abs(cos(sqrt(i) / 2 * π))
end

end
