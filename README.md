# hagrad
Haskell implementation of gradient computation via backpropagation algorithm, based on Karpathy's [micrograd](https://github.com/karpathy/micrograd). This repository is intended for educational purposes.

#### Example usage

``` haskell
ghci> x1 = param 0.2 "x1"
ghci> x2 = param 3 "x2"
ghci> loss = (x1 / (cos x2)) + (exp(x1) ** x2) / 20
ghci> loss_zg = zeroGrad loss
ghci> loss_ff = feedforward loss_zg
ghci> loss_bp = backpropagation loss_ff
ghci> g1 = gradient loss_bp x1
ghci> g1 
-0.7367909
ghci> g2 = gradient loss_bp x2
4.7018692e-2
```

#### Visualization

``` haskell
ghci> x1 = param 0.2 "x1"
ghci> x2 = param 3 "x2"
ghci> loss = (x1 / (cos x2)) + (exp(x1) ** x2) / 20
ghci> loss_zg = zeroGrad loss
ghci> loss_ff = feedforward loss_zg
ghci> loss_bp = backpropagation loss_ff
ghci> loss_bp 

│
└─ ( + )  -  V: -0.11091579  G: 1.0
  │
  └─ ( / )  -  V: -0.20202173  G: 1.0
    │
    └─ ( 0.2 )  -  V: 0.2  G: -1.0101087
    │
    └─ ( cos )  -  V: -0.9899925  G: -0.2040639
      │
      └─ ( 3.0 )  -  V: 3.0  G: 2.87975e-2
  │
  └─ ( / )  -  V: 9.1105945e-2  G: 1.0
    │
    └─ ( ^ )  -  V: 1.8221189  G: 5.0e-2
      │
      └─ ( exp )  -  V: 1.2214028  G: 0.22377372
        │
        └─ ( 0.2 )  -  V: 0.2  G: 0.27331784
      │
      └─ ( 3.0 )  -  V: 3.0  G: 1.822119e-2
    │
    └─ ( 20.0 )  -  V: 20.0  G: 0.0

```
