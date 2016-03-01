EFMVcoeff <- function(mu, Sigma)
{
	#mu=matrix(c(1.1,1.2,1.3),ncol=1);
	#Sigma=matrix(c(0.1,0,0,0,0.05,0,0,0,0.07),3,3);
	n       =   length(mu);
	ell     =   matrix(c(1),nrow=n,ncol=1);
	temp1   =   solve(Sigma) %*% ell;	#n*1
	temp2   =   t(temp1)%*%ell;	#1*n * n*1 = 1*1
	h0      =   temp1/temp2[1];	#n*1
	temp3   =   solve(Sigma) %*% mu;	#n*1
	temp4   =   t(ell)%*%temp3;#1*1 * n*1 = 1*1
	h1      =   (solve(Sigma)%*%mu)-(temp4[1]*h0); #n*1
	alpha0  =   t(mu)%*%h0;
	alpha1  =   t(mu)%*%h1;
	beta0   =   t(h0)%*%Sigma%*%h0;
	beta2   =   t(h1)%*%Sigma%*%h1;
	return(list(alpha0, alpha1, beta0, beta2, h0, h1));	
}
