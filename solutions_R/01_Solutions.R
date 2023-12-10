# Assignment 1
S <- 0.3
TgS <- 0.2 # 0.8 is Tbar given S
TgSbar <- 0.6 # 0.4 is Tbar given Sbar

TandS <- S * TgS
TandSbar <- (1 - S) * TgSbar
TbarandS <- S * (1 - TgS)
TbarandSbar <- (1 - S) * (1 - TgSbar)

print(paste( "Sum: ", (TandS + TandSbar + TbarandS + TbarandSbar) ) )
