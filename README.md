# hagrad

x1 = param(value, name)
x2 = param(value, name)
l = (x1 / x2) + tan(x1) * x2 + K
l = zeroGrad l
l = feedforward l
l = backpropagation l
g1 = gradient l x1
g2 = gradient l x2
