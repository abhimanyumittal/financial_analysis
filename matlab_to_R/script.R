#a=list();
source('validation.R');
source('EFMVcoeff.R');
source('EFMSDplot.R');
source('CMLplot.R');
files=readline();
files=as.integer(files);

icici=read.csv('ICICI.csv',header=TRUE,sep=",",dec=".")
itc=read.csv('ITC.csv',header=TRUE,sep=",",dec=".")
jayp=read.csv('JAIPP.csv',header=TRUE,sep=",",dec=".")
icici=icici$Adj.Close
itc=itc$Adj.Close
jayp=jayp$Adj.Close
min1=min(length(icici),length(itc),length(jayp))
i=1;
itc_list=seq()
icici_list=seq()
jayp_list=seq()

while(i<min1)
{
	itc_list[i]=(itc[i]-itc[i+1])/itc[i+1]
	icici_list[i]=(icici[i]-icici[i+1])/icici[i+1]
	jayp_list[i]=(jayp[i]-jayp[i+1])/jayp[i+1]
	i=i+1
}

itc_list=matrix(itc_list,nrow=min1-1,ncol=1)
icici_list=matrix(icici_list,nrow=min1-1,ncol=1)
jayp_list=matrix(jayp_list,nrow=min1-1,ncol=1)
expected_mean=matrix(c(mean(itc_list),mean(icici_list),mean(jayp_list)),nrow=3,ncol=1)
#print(expected_mean)
vcor=matrix(nrow=files,ncol=files)
#a=data.frame(itc_list,icici_list,jayp_list)
a=list()
#i=1
#while(i<=file)
a[[1]]=itc_list;
a[[2]]=icici_list;
a[[3]]=jayp_list;


i=1
j=1

while(i<=files)
{
	j=1
	while(j<=files)
	{

		vcor[i,j]=cov(a[[i]],a[[j]])
		j=j+1;	
	}

	i=i+1
}
print(expected_mean)
print(vcor)

"validate(vcor);
x1 = EFMVcoeff(expected_mean,vcor);
#print(vcor);
r  =  0.000021; 
tlow = 0.0;
thigh = 10.0;
tinc = 0.05;
alpha0=x1[[1]];
alpha1=x1[[2]];
beta0=x1[[3]];
beta2=x1[[4]];
h0=x1[[5]];
h1=x1[[6]];

CMLplot(tlow, thigh, tinc, alpha0, alpha1, beta0,  beta2, expected_mean, vcor, r) ; 
"