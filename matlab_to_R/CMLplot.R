CMLplot <- function(tlow, thigh, tinc, alpha0, alpha1, beta0, beta2, mu, Sigma, r) {
		
	n       =   length(mu);
	ell     =   matrix(c(1),nrow=n,ncol=1);
	coeff = t(mu  - r*ell) %*% solve(Sigma) %*% (mu - r*ell) 
	coeffsqrt = coeff^0.5 
	tmtemp = (t(ell) %*% solve(Sigma) %*% (mu - r*ell));
	tm  =  1/(tmtemp[1]);
	xm  =  tm * (solve(Sigma) %*% (mu - r*ell));

	axmax = EFMSDplot(tlow, thigh, tinc, alpha0, alpha1, beta0, beta2,tm);
        xmin = axmax[[1]];
        xmax = axmax[[2]];
        ymin = axmax[[3]];
        ymax = axmax[[4]];
#print("Hello")

	mup = seq();
	sigmap = seq();
	x=0;
	t=0;
	while(x<tm){
		mup[t]  =  r + x * coeff[1]; 
		sigmap[t]  =  x * coeffsqrt[1];
		x = x+tinc^2;
		t=t+1;
	} 

	plot  (sigmap, mup,col='darkkhaki',type="l",xlim = c(xmin, xmax), ylim = c(ymin, ymax),xlab = "Portfolio S.D.", ylab = "Portfolio Mean");	
	par(new=TRUE);

	mup1 = seq();
	sigmap1 = seq();
	x=tm;
	t=0;
	while(x<2*tm){
		mup1[t]  =  r + x * coeff[1]; 
		sigmap1[t]  =  x * coeffsqrt[1];
		x = x+tinc^2;
		t=t+1;
	} 
	plot(sigmap1, mup1,col='black',lty = 3, type='l',xlim = c(xmin, xmax), ylim = c(ymin, ymax),xlab = "", ylab = "");
#	par(new=TRUE);
}


