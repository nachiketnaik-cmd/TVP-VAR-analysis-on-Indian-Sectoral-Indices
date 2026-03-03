##Frequency connectedness approach
##Measuring the frequency dynamics of financial connectedness and systemic risk
##Barunik and Krehlik (2018)

data("dy2012")
data("exampleSim")
exampleSim = exampleSim[1:600,]
exampleSim = zoo(exampleSim, order.by=tail(index(dy2012),600)) # date is required so we borrow it from dy2012
##Replication of frequencyConnectedness
partition = c(pi+0.00001, pi/4, 0)
dca = ConnectednessApproach(exampleSim, 
                            nlag=2,
                            nfore=100,
                            model="VAR",
                            connectedness="Frequency",
                            Connectedness_config=list(FrequencyConnectedness=list(partition=partition, generalized=TRUE, scenario="ABS")))
## Estimating model
## Computing connectedness measures
## The VAR frequency connectedness approach is implemented according to:
##  Baruník, J., & Krehlík, T. (2018). Measuring the frequency dynamics of financial connectedness and systemic risk. Journal of Financial Econometrics, 16(2), 271-296.
kable(dca$TABLE[,,1])
kable(dca$TABLE[,,2])
##Data used are Wells Fargo Co. (WFC), U.S. Bancorp (USB), Morgan Stanley (MS),J.P. Morgan (JPM), Goldman Sachs (GS), Citibank (C), Bank of New York Mellon (BK),Bank of America (BAC), American Express (AXP), American International Group (AIG),and PNC Group (PNC)
##For the computation of volatility, we restrict the analysis to daily logarithmic realized volatility, which is computed using 5-min returns5 during the business hours.
