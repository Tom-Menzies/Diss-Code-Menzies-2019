color.gradient.red <- function(y, colors=c("white","red","darkred"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.green <- function(y, colors=c("white","green","darkgreen"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.pink <- function(y, colors=c("white","pink","pink4"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.blue <- function(y, colors=c("white","blue","darkblue"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.orange <- function(y, colors=c("white","orange","darkorange"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.magenta <- function(y, colors=c("white","magenta","magenta4"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m2m1 <- function(y, colors=c("red","white","green"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m3m1 <- function(y, colors=c("red","white","orange"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m3m2 <- function(y, colors=c("green","white","orange"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m4m1 <- function(y, colors=c("red","white","blue"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m4m2 <- function(y, colors=c("green","white","blue"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m4m3 <- function(y, colors=c("orange","white","blue"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m5m1 <- function(y, colors=c("red","white","pink"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m5m2 <- function(y, colors=c("green","white","pink"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m5m3 <- function(y, colors=c("orange","white","pink"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m5m4 <- function(y, colors=c("blue","white","pink"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m6m1 <- function(y, colors=c("red","white","magenta"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m6m2 <- function(y, colors=c("green","white","magenta"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m6m3 <- function(y, colors=c("orange","white","magenta"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m6m4 <- function(y, colors=c("blue","white","magenta"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m6m5 <- function(y, colors=c("pink","white","magenta"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}
color.gradient.m4m6 <- function(y, colors=c("magenta","white","blue"), colsteps=100) {
  x <- (1:y)
  return( colorRampPalette(colors) (colsteps) [ findInterval(x, seq(min(x),max(x), length.out=colsteps)) ] )
}

Contourplot_fnct_comp1_v2 <-function(a, b, c, diffba, diffca, diffcb, pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=18, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=18, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,6), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.red(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[u^"L"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"P"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=c[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.orange(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"ML"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m2m1(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["P"~-~"L"] ), cex=1.5, side=3, line=1)  
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffca[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m3m1(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["ML"~-~"L"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffcb[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m3m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["ML"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    
  }
  
  dev.off()
}
Contourplot_fnct_comp1_v2(res_1_u1$u20.1, res_1_u2$u20.2, res_1_u3$u20.3,
                          res_1_u2$u20.2-res_1_u1$u20.1, res_1_u3$u20.3-res_1_u1$u20.1,
                          res_1_u3$u20.3-res_1_u2$u20.2, pct="80", order="1,2", name="comp1_69")
Contourplot_fnct_comp2_v2 <-function(a, b, c, d, diffba, diffca, diffcb, pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=21, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=21, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,7), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.blue(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[u^"S"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[l^"P"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=c[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.orange(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"ML"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=d[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.magenta(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[l^"MLS"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffca[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m3(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"ML"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffcb[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m6(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"MLS"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    
  }
  
  dev.off()
}
Contourplot_fnct_comp2_v2(res_1_l1$l20.1, res_1_u2$u20.2, res_1_u3$u20.3, res_1_l3$l20.3,
                          res_1_l1$l20.1-res_1_u2$u20.2, res_1_l1$l20.1-res_1_u3$u20.3,
                          res_1_l1$l20.1-res_1_l3$l20.3, pct="80", order="1,2", name="comp2_69")
Contourplot_fnct_comp3_v2 <-function(a, b, c, d, diffba, diffca, diffcb, pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=21, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=21, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,7), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.magenta(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[l^"MLS"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.red(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"L"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=c[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"P"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=d[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.orange(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"ML"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m1(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"L"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffca[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffcb[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m3(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"ML"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    
  }
  
  dev.off()
}
Contourplot_fnct_comp3_v2(res_1_l3$l20.3, res_1_u1$u20.1, res_1_u2$u20.2, res_1_u3$u20.3,
                          res_1_l3$l20.3-res_1_u1$u20.1, res_1_l3$l20.3-res_1_u2$u20.2,
                          res_1_l3$l20.3-res_1_u3$u20.3, pct="80", order="1,2", name="comp3_69")
Contourplot_fnct_comp4_v2 <-function(a, b, c, diffba, diffca, diffcb, pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=18, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=18, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,6), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.blue(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[l^"S"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.magenta(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"MLS"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=c[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"P"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m6(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"MLS"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffca[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffcb[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    
  }
  
  dev.off()
}
Contourplot_fnct_comp4_v2(res_1_l1$l20.1, res_1_l3$l20.3, res_1_u2$u20.2, 
                          res_1_l1$l20.1-res_1_l3$l20.3, res_1_l1$l20.1-res_1_u2$u20.2,
                          res_1_l3$l20.3-res_1_u2$u20.2, pct="80", order="1,2", name="comp4_69")

Contourplot_fnct_m4m1_v2 <-function(a, b, diffba,  pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=9, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=9, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,3), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.blue(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[l^"S"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.red(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"L"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)

    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m4m1(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"L"] ), cex=1.5, side=3, line=1)  
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
  }
  
  dev.off()
}
Contourplot_fnct_m4m1_v2(res_1_l1$l20.1, res_1_u1$u20.1, res_1_l1$l20.1-res_1_u1$u20.1,
                          pct="80", order="1,2", name="compm4m1_69")
Contourplot_fnct_m5m2_v2 <-function(a, b, diffba,  pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=9, height=18, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=9, height=18, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(7,3), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (1:7)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.pink(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==1)   mtext(bquote(P(P[l^"S"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(P(P[u^"L"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diffba[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m5m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"L"] ), cex=1.5, side=3, line=1)  
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
  }
  
  dev.off()
}
Contourplot_fnct_m5m2_v2(res_1_l2$l20.2, res_1_u2$u20.2, res_1_l2$l20.2-res_1_u2$u20.2,
                         pct="80", order="1,2", name="compm5m2_69")

Contourplot_fnct_comp99_v2 <-function(a, b, c, d, e, f,
                                      diff1, diff2, diff3, diff4, diff5, diff6, diff7, diff8, diff9, diff10,
                                      pct="", order="", name="") {
  #  png(file=paste("BR_Loss2crit_Sim_P", name, "PNG", sep=""), res = 200, width=18, height=5, units = "in")
  png(file=paste("BR_Loss2crit_Sim_P", name, ".png", sep=""), res = 200, width=18, height=5, units = "in")
  
  pct2=paste(pct, "%", sep="")
  par(mfrow=c(2,6), mar=c(3, 5, 0, 2), oma = c(4, 6, 7, 2))
  for (scenario1 in (8:9)) {
    image2D(y=r2,x=b2,z=a[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.red(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    mtext("P(Risk)", cex=1.3, side=2, line=2.7)
    mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1)
    if (scenario1==8)   mtext(bquote(P(P[l^"L"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=b[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.green(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==8)   mtext(bquote(P(P[u^"P"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=c[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.orange(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==8)   mtext(bquote(P(P[u^"ML"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=d[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.blue(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==8)   mtext(bquote(P(P[l^"S"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=e[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.pink(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==8)   mtext(bquote(P(P[l^"PS"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=f[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(0,1),contour=F, col=color.gradient.magenta(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==8)   mtext(bquote(P(P[l^"MLS"]^list(.(order))>.(pct2)) ), cex=1.5, side=3, line=1)
    if (scenario1==9)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
    image2D(y=r2,x=b2,z=diff1[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff2[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff3[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff4[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"L"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff5[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff6[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["S"~-~"ML"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff7[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"L"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff8[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"P"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff9[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"ML"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    image2D(y=r2,x=b2,z=diff10[scenario1,, ], xlim=c(0,1), ylim=c(0,1), zlim=c(-1,1),contour=F, col=color.gradient.m6m2(nscen2), xlab="", ylab="")
    points(scen1[scenario1,1], scen1[scenario1,2], pch=16, col="black", cex=2)
    if (scenario1==1)   mtext(bquote(phi["MLS"~-~"S"] ), cex=1.5, side=3, line=1)
    if (scenario1==7)     mtext("P(Benefit)", cex=1.5, side=1, line=3.5)
    
  }
  
  dev.off()
}
Contourplot_fnct_comp99_v2(res_1_u1$u20.1, res_1_u2$u20.2, res_1_u3$u20.3,
                           res_1_l1$l20.1, res_1_l2$l20.2, res_1_l3$l20.3,
                         pct="80", order="1,2", name="comp99_69")
