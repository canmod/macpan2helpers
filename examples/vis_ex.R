library(macpan2)
library(macpan2helpers)
## with birth (D0)/death (D) distinct
sir1 <- Compartmental("examples/sir_open1")
visCompartmental(sir1)
## with all fake compartments distinct
sir2 <- Compartmental("examples/sir_open2")
visCompartmental(sir2)
