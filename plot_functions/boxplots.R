#### Generates boxplot figure of parameter distributions of input and fits

#par(mar=c(3,3,3,3))
tiff('plots/para_distributions.tiff', units="in", width=10, height=10, res=300, compression = 'lzw')

layout(matrix(c(1,1,2,2,1,1,2,2,3,3,4,4,3,3,4,4,5,5,5,5), nrow = 5, ncol = 4, byrow = TRUE))
mycol <- rgb(255, 0, 0, max = 255, alpha = 100, names = "blue50")

boxplot(p$LAMBDA_H, p$LAMBDA_A,p$LAMBDA_E,col=grey(0.6), names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,main="USE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2)
boxplot(best_100_para_england$LAMBDA_H, best_100_para_england$LAMBDA_A,best_100_para_england$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,boxfill = "pink")
boxplot(best_100_para_denmark$LAMBDA_H, best_100_para_denmark$LAMBDA_A,best_100_para_denmark$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,boxfill = "lightblue")
boxplot(best_100_para_senegal$LAMBDA_H, best_100_para_senegal$LAMBDA_A,best_100_para_senegal$LAMBDA_E, names=c(expression(Lambda[H]),expression(Lambda[A]),expression(Lambda[E])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.1,las=2,boxfill = "lightyellow")

boxplot(p$beta_HH, p$beta_AA,p$beta_EE,p$beta_HE,
        p$beta_AH, p$beta_EH,p$beta_HA,
        p$beta_EA, p$beta_AE
        ,col=grey(0.6), names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,main="TRANSMISSION",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,
        cex.lab=2)
boxplot(best_100_para_england$beta_HH, best_100_para_england$beta_AA,best_100_para_england$beta_EE,best_100_para_england$beta_HE,
        best_100_para_england$beta_AH, best_100_para_england$beta_EH,best_100_para_england$beta_HA,
        best_100_para_england$beta_EA, best_100_para_england$beta_AE, boxfill = "pink",
       names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2)
boxplot(best_100_para_denmark$beta_HH, best_100_para_denmark$beta_AA,best_100_para_denmark$beta_EE,best_100_para_denmark$beta_HE,
        best_100_para_denmark$beta_AH, best_100_para_denmark$beta_EH,best_100_para_denmark$beta_HA,
        best_100_para_denmark$beta_EA, best_100_para_denmark$beta_AE,boxfill = "lightblue",
       names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2)
boxplot(best_100_para_senegal$beta_HH, best_100_para_senegal$beta_AA,best_100_para_senegal$beta_EE,best_100_para_senegal$beta_HE,
        best_100_para_senegal$beta_AH, best_100_para_senegal$beta_EH,best_100_para_senegal$beta_HA,
        best_100_para_senegal$beta_EA, best_100_para_senegal$beta_AE,boxfill = "lightyellow",
        names=c(expression(beta[HH]),expression(beta[AA]),expression(beta[EE]),expression(beta[HE]),expression(beta[AH]),expression(beta[EH]),expression(beta[HA]), expression(beta[EA]),expression(beta[AE])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.2,las=2,cex.lab=2)


boxplot(p$mu_H, p$mu_A,p$mu_E,col=grey(0.6), names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,main="DECAY",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(best_100_para_england$mu_H, best_100_para_england$mu_A,best_100_para_england$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2,boxfill = "pink")
boxplot(best_100_para_denmark$mu_H, best_100_para_denmark$mu_A,best_100_para_denmark$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2,boxfill = "lightblue")
boxplot(best_100_para_senegal$mu_H, best_100_para_senegal$mu_A,best_100_para_senegal$mu_E,names=c(expression(mu[H]),expression(mu[A]),expression(mu[E])),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.25,las=2,cex.lab=2,boxfill = "lightyellow")


boxplot(p$gamma,col=grey(0.6), names=c(expression(gamma)),show.names=TRUE,main="EMERGENCE",medcol=grey(0),whiskcol=grey(0.6),staplecol=grey(0.6),boxcol=grey(0.6),outcol=grey(0.6),outbg=grey(0.6),las=2,cex.lab=2)
boxplot(best_100_para_england$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="red",whiskcol="red",staplecol="red",boxcol="red",outcol="red",outbg="red",boxwex=0.5,las=2,cex.lab=2,boxfill = "pink")
boxplot(best_100_para_denmark$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="blue",whiskcol="blue",staplecol="blue",boxcol="blue",outcol="blue",outbg="blue",boxwex=0.35,las=2,cex.lab=2,boxfill = "lightblue")
boxplot(best_100_para_senegal$gamma, names=c(expression(gamma)),show.names=TRUE,add=TRUE,medcol="yellow",whiskcol="yellow",staplecol="yellow",boxcol="yellow",outcol="yellow",outbg="yellow",boxwex=0.2,las=2,cex.lab=2,boxfill = "lightyellow")

plot(0, 0, type='n', bty='n', xaxt='n', yaxt='n')
legend("top", horiz=TRUE, c("Input", "england","denmark","senegal"), border="black", fill = c("grey", "red","blue","yellow"))


dev.off()

