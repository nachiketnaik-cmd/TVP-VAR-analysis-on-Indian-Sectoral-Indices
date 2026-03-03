cclibrary("ConnectednessApproach")
library(knitr)
library("zoo")
library(kableExtra)
library(PerformanceAnalytics)

dd <- read.csv.zoo("SI'N.csv", sep = ',', header = T)
#write.csv(x = SI_N,file = "SI'N.csv",row.names = F)

Y = Yp = Yn = dd[-1,]
k = ncol(Y)
for (i in 1:k) {
  x = embed(as.numeric(unlist(dd[,i])),2)
  Y[,i] = Yp[,i] = Yn[,i] = 100*(x[,1]-x[,2])/x[,2]
  Yp[which(Y[,i]<0),i] = 0
  Yn[which(Y[,i]>0),i] = 0
}
Y_list = list(Y, Yp, Yn)
DCA = list()
spec = c("all", "positive", "negative")
for (i in 1:length(Y_list)) {
  DCA[[i]] = suppressMessages(ConnectednessApproach(Y_list[[i]], 
                                                    model="TVP-VAR",
                                                    connectedness="Time",
                                                    nlag=1,
                                                    nfore=10,
                                                    window.size=200,
                                                    VAR_config=list(TVPVAR=list(kappa1=0.99, kappa2=0.99, prior="MinnesotaPrior", gamma=0.1))))
  kable(DCA[[i]]$TABLE)
}
CT<-kbl(DCA[[1]]$TABLE)
CTp<-kbl(DCA[[2]]$TABLE)
CTn<-kbl(DCA[[3]]$TABLE)
CT;CTp;CTn

#TCI Figue 1
png(filename ="tci.png",width = 35,height = 20.95,units = "cm",res = 300)
PlotTCI(DCA[[1]],ca=list(DCA[[2]],DCA[[3]]))
dev.off()

#NET Figure 2
png(filename ="net.png",width = 35,height = 20.95,units = "cm",res = 300)
PlotNET(DCA[[1]],ca=list(DCA[[2]],DCA[[3]]))
dev.off()

#NPDC Figure 3
png(filename ="npdc.png",width = 35,height = 20.95,units = "cm",res = 300)
PlotNPDC(DCA[[1]],ca=list(DCA[[2]],DCA[[3]]))
dev.off()

#PCI Figure 4
png(filename ="pci.png",width = 35,height = 20.95,units = "cm",res = 300)
PlotPCI(DCA[[1]], ca=list(DCA[[2]], DCA[[3]]))
dev.off()

######
NAMES = colnames(dd)
k = ncol(dd)
Y = dd[-1,]
for (i in 1:k) {
  Y[,i] = 100*diff(log(as.numeric(dd[,i])))
}

# SUMMARY STATISTICS Table 1
kbl(SummaryStatistics(Y, correlation="pearson"))

###############Figure 5: Equity Lines (Code lines 70-105 to be run)

nlag = 1
nfore = 20
#####
#TOTAL RETURNS
y = Y[,1:k]
prior = BayesPrior(Y, nlag=nlag)
fit = TVPVAR(Y, configuration=list(l=c(0.99,0.99), nlag=nlag, prior=prior))
dca = TimeConnectedness(Phi=fit$B_t, Sigma=fit$Q_t, nfore=nfore, corrected=TRUE)
kbl(dca$TABLE)

#####Portfolio Performance analysis
date = index(y)
Q_t = fit$Q_t[1:k,1:k,]
R_t = ConditionalCorrelation(Q_t)
C_t = dca$PCI[1:k,1:k,]

# Minimum Variance Portfolio of TOP Panel of Table 3
mvp = MinimumConnectednessPortfolio(y, Q_t)
kbl(mvp$TABLE) 

# Minimum Correlation Portfolio of TOP Panel of Table 4
mcp = MinimumConnectednessPortfolio(y, R_t)
kbl(mcp$TABLE) 

# Minimum Connectedness Portfolio of TOP Panel of Table 5
mpc = MinimumConnectednessPortfolio(y, C_t)
kbl(mpc$TABLE) 

mvpret<- zoo(mvp$portfolio_return,order.by = date)
shrpmvp<-SharpeRatio(mvpret,Rf = 0,p = 0.95,FUN = "StdDev")
meanmvp<-mean(mvpret)
sdmvp<- sd(mvpret)
shrpmvp;meanmvp;sdmvp
#MCP
mcpret<- zoo(mcp$portfolio_return,order.by = date)
shrpmcp<-SharpeRatio(mcpret,Rf = 0,p = 0.95,FUN = "StdDev")
meanmcp<-mean(mcpret)
sdmcp<- sd(mcpret)
shrpmcp;meanmcp;sdmcp
#MCoP
mpcret<- zoo(mpc$portfolio_return,order.by = date)
shrpmpc<-SharpeRatio(mpcret,Rf = 0,p = 0.95,FUN = "StdDev")
meanmpc<-mean(mpcret)
sdmpc<- sd(mpcret)
shrpmpc;meanmpc;sdmpc

tb7= matrix(c(shrpmvp,meanmvp,sdmvp,shrpmcp,meanmcp,sdmcp,shrpmpc,meanmpc,sdmpc), ncol=3, byrow=TRUE)
colnames(tb7) = c('MVP','MCP','MCoP')
rownames(tb7) <- c('Mean','SD','SR')
finalP=as.table(tb7)
kbl(final)


#Equity Lines Weights Figure 5
par(mfrow=c(1,1))
plot(date, mcp$cumulative_portfolio_return, type="l", las=1, xaxs="i", xlab="", ylab="", col="red")
grid(NA, NULL); abline(h=0, lty=3)
lines(date, mpc$cumulative_portfolio_return, col="steelblue1")
lines(date, mvp$cumulative_portfolio_return, col="steelblue4")
legend("topleft", c("MVP","MCP","MCoP"), fill=c("steelblue4", "red", "steelblue1"), bty="n")


############Multivariate Portfolios for Positive Returns########################
y = Yp[,1:k]
prior = BayesPrior(Yp, nlag=nlag)
fit = TVPVAR(Yp, configuration=list(l=c(0.99,0.99), nlag=nlag, prior=prior))
dca = TimeConnectedness(Phi=fit$B_t, Sigma=fit$Q_t, nfore=nfore, corrected=TRUE)
kbl(dca$TABLE)


#####Portfolio Performance analysis
date = index(y)
Q_t = fit$Q_t[1:k,1:k,]
R_t = ConditionalCorrelation(Q_t)
C_t = dca$PCI[1:k,1:k,]

# Minimum Variance Portfolio of Middle Panel of Table 3
mvp = MinimumConnectednessPortfolio(y, Q_t)
kbl(mvp$TABLE) 

# Minimum Correlation Portfolio of Middle Panel of Table 4
mcp = MinimumConnectednessPortfolio(y, R_t)
kbl(mcp$TABLE) 

# Minimum Connectedness Portfolio of Middle Panel of Table 5
mpc = MinimumConnectednessPortfolio(y, C_t)
kbl(mpc$TABLE) 

#########################Table 6: Portfolio analysis############################
#########To generate the TOP Panel, again rerun codelines 70-96 then run 164-182

mvpretP<- zoo(mvp$portfolio_return,order.by = date)
shrpmvpP<-SharpeRatio(mvpretP,Rf = 0,p = 0.95,FUN = "StdDev")
meanmvpP<-mean(mvpretP)
sdmvpP<- sd(mvpretP)
shrpmvpP;meanmvpP;sdmvpP
#MCP
mcpretP<- zoo(mcp$portfolio_return,order.by = date)
shrpmcpP<-SharpeRatio(mcpretP,Rf = 0,p = 0.95,FUN = "StdDev")
meanmcpP<-mean(mcpretP)
sdmcpP<- sd(mcpretP)
shrpmcpP;meanmcpP;sdmcpP
#MCoP
mpcretP<- zoo(mpc$portfolio_return,order.by = date)
shrpmpcP<-SharpeRatio(mpcretP,Rf = 0,p = 0.95,FUN = "StdDev")
meanmpcP<-mean(mpcretP)
sdmpcP<- sd(mpcretP)
shrpmpcP;meanmpcP;sdmpcP

tb7P= matrix(c(shrpmvpP,meanmvpP,sdmvpP,shrpmcpP,meanmcpP,sdmcpP,shrpmpcP,meanmpcP,sdmpcP), ncol=3, byrow=TRUE)
colnames(tb7P) = c('MVP','MCP','MCoP')
rownames(tb7P) <- c('Mean','SD','SR')
finalP=as.table(tb7P)
kbl(finalP)


############Multivariate Portfolios for Negative Returns########################
y = Yn[,1:k]
prior = BayesPrior(Yp, nlag=nlag)
fit = TVPVAR(Yp, configuration=list(l=c(0.99,0.99), nlag=nlag, prior=prior))
dca = TimeConnectedness(Phi=fit$B_t, Sigma=fit$Q_t, nfore=nfore, corrected=TRUE)
kbl(dca$TABLE)


#####Portfolio Performance analysis
date = index(y)
Q_t = fit$Q_t[1:k,1:k,]
R_t = ConditionalCorrelation(Q_t)
C_t = dca$PCI[1:k,1:k,]

# Minimum Variance Portfolio of Bottom Panel of Table 3
mvp = MinimumConnectednessPortfolio(y, Q_t)
kbl(mvp$TABLE) 

# Minimum Correlation Portfolio of Bottom Panel of Table 4
mcp = MinimumConnectednessPortfolio(y, R_t)
kbl(mcp$TABLE) 

# Minimum Connectedness Portfolio of Bottom Panel of Table 5
mpc = MinimumConnectednessPortfolio(y, C_t)
kbl(mpc$TABLE) 

#########################Table 6: Portfolio analysis############################
#########To generate the TOP Panel, again rerun codelines 70-96 then run 164-182

mvpretN<- zoo(mvp$portfolio_return,order.by = date)
shrpmvpN<-SharpeRatio(mvpretN,Rf = 0,p = 0.95,FUN = "StdDev")
meanmvpN<-mean(mvpretN)
sdmvpN<- sd(mvpretN)
shrpmvpN;meanmvpN;sdmvpN
#MCP
mcpretN<- zoo(mcp$portfolio_return,order.by = date)
shrpmcpN<-SharpeRatio(mcpretN,Rf = 0,p = 0.95,FUN = "StdDev")
meanmcpN<-mean(mcpretN)
sdmcpN<- sd(mcpretN)
shrpmcpN;meanmcpN;sdmcpN
#MCoP
mpcretN<- zoo(mpc$portfolio_return,order.by = date)
shrpmpcN<-SharpeRatio(mpcretN,Rf = 0,p = 0.95,FUN = "StdDev")
meanmpcN<-mean(mpcretN)
sdmpcN<- sd(mpcretN)
shrpmpcN;meanmpcN;sdmpcN

tb7N= matrix(c(shrpmvpN,meanmvpN,sdmvpN,shrpmcpN,meanmcpN,sdmcpN,shrpmpcN,meanmpcN,sdmpcN), ncol=3, byrow=TRUE)
colnames(tb7N) = c('MVP','MCP','MCoP')
rownames(tb7N) <- c('Mean','SD','SR')
finalN=as.table(tb7N)
kbl(finalN)


