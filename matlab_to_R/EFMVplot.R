#require(ggplot2)
EFMVplot <- function(tlow, thigh, tinc, alpha0, alphal, beta0, beta2)
{
xmin = 0.0;
xmax = beta0[1]  + thigh^2 * beta2[1];
ymin = alpha0 - thigh * alphal;
ymax = alpha0 + thigh * alphal;
mup = seq()
muplow = seq()
sigma2p = seq();


#length(seq(tlow+tinc,tinc,thigh));
k=0;
t=0;
while(k<=thigh)
{
mup[t]     =   alpha0+ k * alphal;
muplow[t]  =   alpha0- k * alphal;
sigma2p[t] =  beta0 + k*k*beta2;
k=k+tinc;
t=t+1;
}
#df <- data.frame(sigma2p, mup, muplow);

#plot(sigma2p,mup,xmin,xmax);

#ggplot(df,aes(x=sigma2))+geom_line(aes(y=mup), colour="red") + geom_line(aes(y=muplow),colour="green");
#plot(sigma2p,muplow)

plot(sigma2p,mup, col = 'blue',type = 'l', xlim=c(xmin, xmax), ylim=c(ymin, ymax),main="Efficient Frontier: Mean-Variance Space" ,xlab = "Portfolio Variance sigma^2", ylab = "Portfolio Mean mu_p");
par(new=TRUE);
plot(sigma2p,	muplow,col='red',type = 'l',lty = 2,  xlim=c(xmin, xmax), ylim=c(ymin, ymax),xlab = "", ylab = "");

#plot(sin,add =TRUE);

#plot(sigma2p,muplow)
}
