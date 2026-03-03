space <- 10  # adjust to your needs
date <- DATE[-c(1:space)]
total <- total[-c(1:space)]

# Make sure everything is the same length and numeric
stopifnot(length(date) == length(total))
stopifnot(all(is.finite(total)))
stopifnot(all(!is.na(date)))

t0 <- length(total)

par(mfcol=c(1,1), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1)+0.35, mgp=c(0.5,0.5,0))
plot(date, total, type="l", xaxs="i", col="blue4", las=1, 
     main="Dynamic Total Return Connectedness", ylab="", ylim=c(55,100),
     yaxs="i", xlab="", tck=-0.02)

grid(NA, NULL)
polygon(c(date, rev(date)), c(rep(0, t0), rev(total)), col="blue4", border="blue4")
box()
total_ret_var <- total / total[1] * 100