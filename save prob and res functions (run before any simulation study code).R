Res_fnct_l1 <-function(trt1, trt2) {
  loss1=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.1=array(0, dim=c(nscen1, 9, 9))
  l80.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss1[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$loss[ct,]>trt2[[scenario2]]$loss[ct,])
      }
      l20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss1[scenario1, scenario2,]<0.2)
      l80.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss1[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.1"=l20.1, "l80.1"=l80.1))
}
Res_fnct_l2 <-function(trt1, trt2) {
  loss2=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.2=array(0, dim=c(nscen1, 9, 9))
  l80.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss2[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$loss2[ct,]>trt2[[scenario2]]$loss2[ct,])
      }
      l20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss2[scenario1, scenario2,]<0.2)
      l80.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss2[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.2"=l20.2, "l80.2"=l80.2))
}
Res_fnct_l3 <-function(trt1, trt2) {
  loss3=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.3=array(0, dim=c(nscen1, 9, 9))
  l80.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss3[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$loss3[ct,]>trt2[[scenario2]]$loss3[ct,])
      }
      l20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss3[scenario1, scenario2,]<0.2)
      l80.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss3[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.3"=l20.3, "l80.3"=l80.3))
}
Res_fnct_u1 <-function(trt1, trt2) {
  utility1=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.1=array(0, dim=c(nscen1, 9, 9))
  u80.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility1[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$utility[ct,]<trt2[[scenario2]]$utility[ct,])
      }
      u20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility1[scenario1, scenario2,]<0.2)
      u80.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility1[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.1"=u20.1, "u80.1"=u80.1))
}
Res_fnct_u2 <-function(trt1, trt2) {
  utility2=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.2=array(0, dim=c(nscen1, 9, 9))
  u80.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility2[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$utility2[ct,]<trt2[[scenario2]]$utility2[ct,])
      }
      u20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility2[scenario1, scenario2,]<0.2)
      u80.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility2[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.2"=u20.2,"u80.2"=u80.2))
}
Res_fnct_u3 <-function(trt1, trt2) {
  utility3=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.3=array(0, dim=c(nscen1, 9, 9))
  u80.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility3[scenario1, scenario2,ct] = mean(trt1[[scenario1]]$utility3[ct,]<trt2[[scenario2]]$utility3[ct,])
      }
      u20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility3[scenario1, scenario2,]<0.2)
      u80.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility3[scenario1, scenario2,]>0.8)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.3"=u20.3, "u80.3"=u80.3))
}

Saveprob_fnct_u1 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2], 
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_u1$u20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_u1$u80.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u2 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_u2$u20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_u2$u80.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u3 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_u3$u20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_u3$u80.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l1 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_l1$l20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_l1$l80.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l2 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_l2$l20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_l2$l80.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l3 <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(res_1_l3$l20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 "\nUtility:\nP[P(T2>T1)>80%]=",round(res_1_l3$l80.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}







Results_fnct_pcorr_l1 <-function(t1_pc, t2_pc) {
  loss1=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss1[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$l1[ct,]>t2_pc[[scenario2]]$l1[ct,])
      }
      l20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss1[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.1"=l20.1))
}
Results_fnct_pcorr_l2 <-function(t1_pc, t2_pc) {
  loss2=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss2[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$l2[ct,]>t2_pc[[scenario2]]$l2[ct,])
      }
      l20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss2[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.2"=l20.2))
}
Results_fnct_pcorr_l3 <-function(t1_pc, t2_pc) {
  loss3=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss3[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$l3[ct,]>t2_pc[[scenario2]]$l3[ct,])
      }
      l20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss3[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.3"=l20.3))
}
Results_fnct_pcorr_u1 <-function(t1_pc, t2_pc) {
  utility1=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility1[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$u1[ct,]<t2_pc[[scenario2]]$u1[ct,])
      }
      u20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility1[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.1"=u20.1))
}
Results_fnct_pcorr_u2 <-function(t1_pc, t2_pc) {
  utility2=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility2[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$u2[ct,]<t2_pc[[scenario2]]$u2[ct,])
      }
      u20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility2[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.2"=u20.2))
}
Results_fnct_pcorr_u3 <-function(t1_pc, t2_pc) {
  utility3=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility3[scenario1, scenario2,ct] = mean(t1_pc[[scenario1]]$u3[ct,]<t2_pc[[scenario2]]$u3[ct,])
      }
      u20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility3[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.3"=u20.3))
}

Saveprob_fnct_u1_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2], 
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_u1_2$u20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u2_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_u2_2$u20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u3_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_u3_2$u20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l1_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_l1_2$l20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l2_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_l2_2$l20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l3_corr_p <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(p_corr_results_l3_2$l20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}







Results_fnct_ncorr_l1 <-function(t1_nc, t2_nc) {
  loss1=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss1[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$l1[ct,]>t2_nc[[scenario2]]$l1[ct,])
      }
      l20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss1[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.1"=l20.1))
}
Results_fnct_ncorr_l2 <-function(t1_nc, t2_nc) {
  loss2=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss2[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$l2[ct,]>t2_nc[[scenario2]]$l2[ct,])
      }
      l20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss2[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.2"=l20.2))
}
Results_fnct_ncorr_l3 <-function(t1_nc, t2_nc) {
  loss3=array(0, dim=c(nscen1, nscen2, nsim1))
  l20.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        loss3[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$l3[ct,]>t2_nc[[scenario2]]$l3[ct,])
      }
      l20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(loss3[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("l20.3"=l20.3))
}
Results_fnct_ncorr_u1 <-function(t1_nc, t2_nc) {
  utility1=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.1=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility1[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$u1[ct,]<t2_nc[[scenario2]]$u1[ct,])
      }
      u20.1[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility1[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.1"=u20.1))
}
Results_fnct_ncorr_u2 <-function(t1_nc, t2_nc) {
  utility2=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.2=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility2[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$u2[ct,]<t2_nc[[scenario2]]$u2[ct,])
      }
      u20.2[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility2[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.2"=u20.2))
}
Results_fnct_ncorr_u3 <-function(t1_nc, t2_nc) {
  utility3=array(0, dim=c(nscen1, nscen2, nsim1))
  u20.3=array(0, dim=c(nscen1, 9, 9))
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      for (ct in (1:nsim1)) {
        utility3[scenario1, scenario2,ct] = mean(t1_nc[[scenario1]]$u3[ct,]<t2_nc[[scenario2]]$u3[ct,])
      }
      u20.3[scenario1, scen2[scenario2,1]*10, scen2[scenario2,2]*10]=mean(utility3[scenario1, scenario2,]<0.2)
      print(scenario2)
      print(paste("scenario2"))
    }
    print(scenario1)
    print(paste("scenario1"))
  }
  return(list("u20.3"=u20.3))
}

Saveprob_fnct_u1_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2], 
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_u1_2$u20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u2_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_u2_2$u20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_u3_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_u3_2$u20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l1_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_l1_2$l20.1[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l2_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_l2_2$l20.2[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
Saveprob_fnct_l3_corr_n <-function(res, name="")  {
  outFile=paste("BR_Loss2crit_Sim_probas_text", name, ".txt", sep="")
  write("Probabilities\n", file=outFile,append=FALSE)
  for (scenario1 in (1:nscen1)) {
    for (scenario2 in (1:nscen2)) {
      write(paste("\nTrt1: P(B)=",scen1[scenario1,1], ", ", "P(R)=", scen1[scenario1,2],
                  "\nTrt2: P(B)=",scen2[scenario2,1], ", ", "P(R)=", scen2[scenario2,2], sep=""),file=outFile,append=T)
      
      desc=paste("\nUtility:\nP[P(T2>T1)<20%]=",round(n_corr_results_l3_2$l20.3[scenario1,scen2[scenario2,1]*10, scen2[scenario2,2]*10],2), 
                 sep="")
      write(desc, file=outFile, append=T)
    }
  }
}
