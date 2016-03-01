EFMSDplot <- function(tlow, thigh, tinc, alpha0, alpha1, beta0, beta2,tm){ 
	mup = seq();
	muplow = seq();
	stddevp = seq();
	mup1=seq();
	stddevp1=seq();
	k=0;
	t=0;
	while(k<thigh){
		mup[t]  =  alpha0  +  k  *  alpha1; 
		muplow[t]  =  alpha0 - k*alpha1; 
		stddevp[t]  =  (beta0  +  k^2  * beta2 )^0.5 ; 
		k=k+tinc;
		t=t+1;
	}

	 k=0;
        t=0;
        while(k<tm)
        {
        mup1[t]  =  alpha0  +  k  *  alpha1;
        stddevp1[t]  =  (beta0  +  k^2  * beta2 )^0.5 ;
        t=t+1;
        k=k+tinc;
        }

	xmin = 0; 
	xmax = (beta0 + thigh^2 * beta2)^(0.5) ; 
	ymin = (alpha0 - thigh * alpha1);
	ymax = (alpha0 + thigh * alpha1); 
	
	plot(stddevp, mup,type = 'l',xlim = c(xmin, xmax), ylim = c(ymin, ymax),xlab = "", ylab = "",col='yellow');
	par(new=TRUE);
	plot(stddevp, muplow,type = 'l',col = 'blue',lty = 2,xlim = c(xmin, xmax), ylim = c(ymin, ymax),xlab = "", ylab = "");
	par(new=TRUE);
	plot(stddevp1, mup1,type = 'l',col = 'red',xlim = c(xmin, xmax), ylim = c(ymin, ymax),,xlab = "", ylab = "");
   	par(new=TRUE);	
	return (list(xmin,xmax,ymin,ymax));
}
