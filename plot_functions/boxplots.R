par(mar=c(3,3,3,3))
mycol <- rgb(255, 0, 0, max = 255, alpha = 100, names = "blue50")

boxplot(FULLDATA_ENGLAND$LAMBDA_H, FULLDATA_ENGLAND$LAMBDA_A,FULLDATA_ENGLAND$LAMBDA_E,col=grey(0.6), names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,main="USE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2)
boxplot(best_100_england$LAMBDA_H, best_100_england$LAMBDA_A,best_100_england$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,boxfill = "pink")
boxplot(best_100_denmark$LAMBDA_H, best_100_denmark$LAMBDA_A,best_100_denmark$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,boxfill = "lightblue")
boxplot(best_100_senegal$LAMBDA_H, best_100_senegal$LAMBDA_A,best_100_senegal$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.1,las=2,boxfill = "lightyellow")

boxplot(FULLDATA_ENGLAND$beta_HH, FULLDATA_ENGLAND$beta_AA,FULLDATA_ENGLAND$beta_EE,FULLDATA_ENGLAND$beta_HE,
        FULLDATA_ENGLAND$beta_AH, FULLDATA_ENGLAND$beta_EH,FULLDATA_ENGLAND$beta_HA,
        FULLDATA_ENGLAND$beta_EA, FULLDATA_ENGLAND$beta_AE
        ,col=grey(0.6), names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,main="TRANSMISSION",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,
        cex.lab=2)
boxplot(best_100_england$beta_HH, best_100_england$beta_AA,best_100_england$beta_EE,best_100_england$beta_HE,
        best_100_england$beta_AH, best_100_england$beta_EH,best_100_england$beta_HA,
        best_100_england$beta_EA, best_100_england$beta_AE, boxfill = "pink",
       names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2)
boxplot(best_100_denmark$beta_HH, best_100_denmark$beta_AA,best_100_denmark$beta_EE,best_100_denmark$beta_HE,
        best_100_denmark$beta_AH, best_100_denmark$beta_EH,best_100_denmark$beta_HA,
        best_100_denmark$beta_EA, best_100_denmark$beta_AE,boxfill = "lightblue",
       names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2)
boxplot(best_100_senegal$beta_HH, best_100_senegal$beta_AA,best_100_senegal$beta_EE,best_100_senegal$beta_HE,
        best_100_senegal$beta_AH, best_100_senegal$beta_EH,best_100_senegal$beta_HA,
        best_100_senegal$beta_EA, best_100_senegal$beta_AE,boxfill = "lightyellow",
        names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.2,las=2,cex.lab=2)


boxplot(FULLDATA_ENGLAND$mu_H, FULLDATA_ENGLAND$mu_A,FULLDATA_ENGLAND$mu_E,col=grey(0.6), names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,main="DECAY",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(best_100_england$mu_H, best_100_england$mu_A,best_100_england$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2,boxfill = "pink")
boxplot(best_100_denmark$mu_H, best_100_denmark$mu_A,best_100_denmark$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2,boxfill = "lightblue")
boxplot(best_100_senegal$mu_H, best_100_senegal$mu_A,best_100_senegal$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.25,las=2,cex.lab=2,boxfill = "lightyellow")


boxplot(FULLDATA_ENGLAND$gamma,col=grey(0.6), names=c(expression(gamma)),show.names=TRUE,main="EMERGENCE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(best_100_england$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2,boxfill = "pink")
boxplot(best_100_denmark$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2,boxfill = "lightblue")
boxplot(best_100_senegal$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.2,las=2,cex.lab=2,boxfill = "lightyellow")

#ggsave does not work with boxplot function
#ggsave(paste("~/Documents/Projects/SEFASI/outputs/boxplots.png",sep=""), width = 500, height = 150, units='mm',dpi=1000)
#10X3