install.packages("plot3D")
library("plot3D")
rm(list=setdiff(ls(), c()))
loss<-function(p.tox,p.eff,w1=0.5){
  y<-(1/p.eff)^w1 + 1/(1-p.tox)^(1-w1)
  return(y)}
loss2<-function(p.tox,p.eff,w1=0.5){
  y<-(1/p.eff)^(w1)*1/(1-p.tox)^(1-w1)
  return(y)}
loss3<-function(p.tox,p.eff,w1=0.5){
  y<-(1/p.eff)^((2/3)*w1)+1/(1-p.tox)^((2/3)*(1-w1))+1/((p.eff*(1-p.tox)))^((1/3)*(w1)+((1-w1)/3))
  return(y)}
utility<-function(p.tox,p.eff,w=0.5){
  y<-w*(p.eff)+(1-w)*(1-p.tox)
  y<-1-y
  return(y)
}
utility2<-function(p.tox,p.eff,w=0.5){
  y<-(p.eff)^(w)*(1-p.tox)^(1-w)
  y<-1-y
  return(y)
}
utility3<-function(p.tox,p.eff,w=0.5){
  y<-(2*w/3)*(p.eff) + (2*(1-w)/3)*(1-p.tox) + ((w/3)+((1-w)/3))*((p.eff*(1-p.tox)))
  y<-1-y
  return(y)
}
f_contour<- function(x, w, d) {
  return(c(0*x[which(x<=(d)^(-1/w))],1-(d-x[which(x>((d)^(-1/w)))]^(-w))^(-1/(1-w))))
}
der_mid<-function(w){
  y<-(w/(1-w)) * 2 ^(2*w-1)
  return(y)
}

search<-function(target){
  y<-der_mid(w=seq(0.0001,1,0.0001))
  z<-which.min(abs(y-target))
  we<-seq(0.0001,1,0.0001)[z]
  return(we)
}



P.TOX<-seq(0.01,0.99,0.01)
P.EFF<-seq(0.01,0.99,0.01)
loss.result1<-mat.or.vec(length(P.TOX),length(P.EFF))
loss.result2<-mat.or.vec(length(P.TOX),length(P.EFF))
loss.result3<-mat.or.vec(length(P.TOX),length(P.EFF))
utility.result1<-mat.or.vec(length(P.TOX),length(P.EFF))
utility.result2<-mat.or.vec(length(P.TOX),length(P.EFF))
utility.result3<-mat.or.vec(length(P.TOX),length(P.EFF))
for (i in 1:length(P.TOX)){
  for (j in 1:length(P.EFF)){
    loss.result1[i,j]<-loss(P.TOX[i],P.EFF[j],w=0.5)
    utility.result1[i,j]<-utility(P.TOX[i],P.EFF[j], w=0.5)
    loss.result2[i,j]<-loss(P.TOX[i],P.EFF[j],w=0.25)
    utility.result2[i,j]<-utility(P.TOX[i],P.EFF[j], w=0.25)
    loss.result3[i,j]<-loss(P.TOX[i],P.EFF[j],w=0.75)
    utility.result3[i,j]<-utility(P.TOX[i],P.EFF[j], w=0.75)
  }}

P.TOX2<-seq(0.01,0.99,0.01)
P.EFF2<-seq(0.01,0.99,0.01)
loss.result1.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
loss.result2.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
loss.result3.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result1.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result2.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result3.2<-mat.or.vec(length(P.TOX2),length(P.EFF2))
for (i in 1:length(P.TOX2)){
  for (j in 1:length(P.EFF2)){
    loss.result1.2[i,j]<-loss2(P.TOX2[i],P.EFF[j],w=0.5)
    utility.result1.2[i,j]<-utility2(P.TOX2[i],P.EFF2[j], w=0.5)
    loss.result2.2[i,j]<-loss2(P.TOX2[i],P.EFF2[j],w=0.25)
    utility.result2.2[i,j]<-utility2(P.TOX2[i],P.EFF2[j], w=0.25)
    loss.result3.2[i,j]<-loss2(P.TOX2[i],P.EFF2[j],w=0.75)
    utility.result3.2[i,j]<-utility2(P.TOX2[i],P.EFF2[j], w=0.75)
  }}
P.TOX2<-seq(0.01,0.99,0.01)
P.EFF2<-seq(0.01,0.99,0.01)
loss.result1.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
loss.result2.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
loss.result3.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result1.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result2.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
utility.result3.3<-mat.or.vec(length(P.TOX2),length(P.EFF2))
for (i in 1:length(P.TOX2)){
  for (j in 1:length(P.EFF2)){
    loss.result1.3[i,j]<-loss3(P.TOX2[i],P.EFF[j],w=0.5)
    utility.result1.3[i,j]<-utility3(P.TOX2[i],P.EFF2[j], w=0.5)
    loss.result2.3[i,j]<-loss3(P.TOX2[i],P.EFF2[j],w=0.25)
    utility.result2.3[i,j]<-utility3(P.TOX2[i],P.EFF2[j], w=0.25)
    loss.result3.3[i,j]<-loss3(P.TOX2[i],P.EFF2[j],w=0.75)
    utility.result3.3[i,j]<-utility3(P.TOX2[i],P.EFF2[j], w=0.75)
  }}


####### FIGURE _1 #######

#jpeg(file="Figure_1_1.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_1_1.png", res = 400, width=5, height=10, units = "in")

par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX,x=P.EFF,z=t(utility.result2),zlim=c(0,1),contour=T,
        mtext( paste("(", "scenario1", ")\n\n", sep=""), cex=2, at=-0.6, side=1),
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(a) ", w^{L} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


image2D(y=P.TOX,x=P.EFF,z=t(utility.result1),zlim=c(0,1),contour=T,
        xlab=expression(paste("Benefit value   ", u[1](theta[i1]), sep = )),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{L} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


dev.off()



#Figure 2#
#jpeg(file="Figure_2_2.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_2_2.png",res = 400, width=5, height=10, units = "in")
par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX,x=P.EFF,z=t(utility.result2.2),zlim=c(0,1),contour=T,
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),        
        main=expression(paste("(a) ", w^{P} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


image2D(y=P.TOX,x=P.EFF,z=t(utility.result1.2),zlim=c(0,1),contour=T,
        xlab=expression(paste("Benefit value   ", (theta[i1])^u[1], sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{P} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


dev.off()


#figure 3#
#jpeg(file="Figure_3_2.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_3_2.png",res = 400, width=5, height=10, units = "in")
par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX,x=P.EFF,z=t(utility.result2.3),zlim=c(0,1),contour=T,
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(a) ", w^{ML} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


image2D(y=P.TOX,x=P.EFF,z=t(utility.result1.3),zlim=c(0,1),contour=T,
        xlab=expression(paste("Benefit value   ", u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{ML} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)



dev.off()



####### FIGURE _4 #######


#jpeg(file="Figure_4.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_4.png",res = 400, width=5, height=10, units = "in")
par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX,x=P.EFF,z=t(loss.result2)-2,zlim=c(0,3),contour=T,
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(a) ", w^{S} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)

image2D(y=P.TOX,x=P.EFF,z=t(loss.result1)-2,zlim=c(0,3), contour=T, 
        xlab=expression(paste("Benefit value   ", u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{S} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


dev.off()


#experiment
#jpeg(file="Figure_5_2.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_5_2.png",res = 400, width=5, height=10, units = "in")
par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX2,x=P.EFF2,z=t(loss.result2.2),zlim=c(0,5),contour=T,
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(a) ", w^{PS} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


image2D(y=P.TOX2,x=P.EFF2,z=t(loss.result1.2),zlim=c(0,5),contour=T,
        xlab=expression(paste("Benefit value   ", u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{PS} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)


dev.off()


#figure 6#
#jpeg(file="Figure_6_2.jpg", res = 400, width=5, height=10, units = "in")
png(file="Figure_6_2.png",res = 400, width=5, height=10, units = "in")

par(mfrow=c(2,1),mai=c(1,1,1,1))

image2D(y=P.TOX,x=P.EFF,z=t(loss.result2.3)-3,zlim=c(0,3),contour=T,
        mtext( paste("(", 5, ")\n\n", sep=""), cex=2, at=-0.6, side=1),
        xlab=expression(paste("Benefit value   ",u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(a) ", w^{MLS} ,"=0.25", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)
       



image2D(y=P.TOX,x=P.EFF,z=t(loss.result1.3)-3,zlim=c(0,3),contour=T,
        mtext( paste("(", scenario1, ")\n\n", sep=""), cex=2, at=-0.6, side=1),
        xlab=expression(paste("Benefit value   ", u[1](theta[i1]), sep="")),
        ylab=expression(paste("Risk value   ", 1-u[2](theta[i2]), sep="")),
        main=expression(paste("(b) ", w^{MLS} ,"=0.50", sep="")), cex.main=1.25, cex.lab=1.25, cex.axis=1)



dev.off()

