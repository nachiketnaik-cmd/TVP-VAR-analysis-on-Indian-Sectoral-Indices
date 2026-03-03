split = 2
par(mfcol=c(ceiling(k/split),split), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1)+0.5, mgp=c(0.5,0.5,0))
for (i in 1:k){
  plot(date,to[,i], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste(colnames(Y)[i],"TO all others"),ylim=c(-10,120),tck=-0.02,yaxs="i")
  grid(NA,NULL)
  polygon(c(date,rev(date)),c(c(rep(0,t0)),rev(to[,i])),col="blue4", border="blue4")
  box()
}

par(mfcol=c(ceiling(k/split),split), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1)+0.5, mgp=c(0.5,0.5,0))
for (i in 1:k){
  plot(date,net[,i], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste("NET",colnames(Y)[i]),ylim=c(-10,35),tck=-0.02,yaxs="i")
  grid(NA,NULL)
  polygon(c(date,rev(date)),c(c(rep(0,t0)),rev(net[,i])),col="blue4", border="blue4")
  box()
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS for variable 1
i=1
par(mfcol=c(6,2), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",xaxs="i",
         col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),
         tck=-0.02,yaxs="i", ylim=c(-5,7))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t0)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}


## NET PAIRWISE DIRECTIONAL CONNECTEDNESS for variable 2
i=2
par(mfcol=c(6,2), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",
         xaxs="i",col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),
         tck=-0.02,yaxs="i",ylim=c(-5,8))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t0)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS for variable 3
i=3
par(mfcol=c(6,2), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",
         xaxs="i",col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),
         tck=-0.02,yaxs="i",ylim=c(-4,4))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t0)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}

par(mfrow = c(ceiling(k/2),2), oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1)+0.5, mgp=c(0.5,0.5,0))
for (i in 1:k){
  plot(date,to[,i], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste(colnames(Y)[i],"TO all others"),ylim=c(-5,120),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(to))),rev(to[,i])),col="blue4", border="blue4")
  box()
}

### NET TOTAL DIRECTIONAL CONNECTEDNESS
par(mfrow = c(ceiling(k/2),2),oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1)+0.5, mgp=c(0.5,0.5,0))
for (i in 1:k){
  plot(date,net[,i], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste("NET",colnames(Y)[i]),ylim=c(-10,30),tck=0.01,yaxs="i")
  grid(NA,NULL,lty=1)
  polygon(c(date,rev(date)),c(c(rep(0,nrow(net))),rev(net[,i])),col="blue4", border="blue4")
  box()
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS for demandshock


i=1
par(mfcol=c(6,2), 
    oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),tck=-0.02,yaxs="i",ylim=c(-4,8))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS for supplyshock
i=2
par(mfcol=c(6,2), 
    oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),tck=-0.02,yaxs="i",ylim=c(-6,6))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}

### NET PAIRWISE DIRECTIONAL CONNECTEDNESS for riskshock
i=3
par(mfcol=c(6,2), 
    oma=c(0.5,0.5,0,0), mar=c(1.5,1,1.5,1), mgp=c(0.5,0.5,0))
for (j in 4:k) {
  if (i!=j) {
    # Remove NA rows
    valid_idx <- which(!is.na(date) & !is.na(total))
    date <- date[valid_idx]
    total <- total[valid_idx]
    
    plot(date,npso[j,i,], xlab="",ylab="",type="l",xaxs="i",col="blue4", las=1, main=paste0(NAMES[i],"-",NAMES[j]),tck=-0.02,yaxs="i",ylim=c(-5,5))
    grid(NA,NULL)
    polygon(c(date,rev(date)),c(c(rep(0,t)),rev(npso[j,i,])),col="blue4", border="blue4")
    box()
  }
}




