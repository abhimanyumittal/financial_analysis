source('validation.R');
source('EFMVcoeff.R');
source('EFMSDplot.R');
source('CMLplot.R');
files=readline(prompt="Enter The number of Stocks \n");
files=as.integer(files);

date1=readline(prompt="Enter the opening date Format YYYY-MM-DD \n")
date1=as.Date(date1)
date2=readline(prompt="Enter the closing date Format YYYY-MM-DD \n")
date2=as.Date(date2)

num=1;
try2=list()
print("Enter the stocks");
while(num<=files)
{
	xyz=readline();
	try2[[num]]=xyz;
	num=num+1
}

num=1
while(num<=files)
{
	try2[[num]]=read.csv(try2[[num]],header=TRUE,sep=",",dec=".")
	num=num+1;
}

num=1;
while(num<=files)
{
if(as.Date(try2[[num]][nrow(try2[[num]]),]$Date) > date1)
{
print("invalid Dates \n")
quit("no")
}
num=num+1
}
num=1

while(num<=files)
{
try2[[num]]=try2[[num]][as.Date(try2[[num]]$Date)<=date2 & as.Date(try2[[num]]$Date)>=date1,]$Adj.Close
num=num+1
}

min1=10000000000;
num=1;
while(num<=files)
{
	min1=min(length(try2[[num]]),min1);
	num=num+1
}

i=1;
num_list=list()
j=1

expected_mean=c()

while(j<=files)
{
	i=1
	num_list[[j]]=seq()
	while(i<min1)
	{
		num_list[[j]][i]=(try2[[j]][i]-try2[[j]][i+1])/try2[[j]][i+1]
		i=i+1
	}
	num_list[[j]]=matrix(num_list[[j]],nrow=min1-1,ncol=1)
	expected_mean=cbind(expected_mean,mean(num_list[[j]]))
	j=j+1
}

expected_mean1=matrix(expected_mean,nrow=files,ncol=1)
vcor=matrix(nrow=files,ncol=files)
#print(expected_mean)
i=1
j=1

while(i<=files)
{
	j=1
	while(j<=files)
	{

		vcor[i,j]=cov(num_list[[i]],num_list[[j]])
		j=j+1;	
	}

	i=i+1
}

#print(vcor)
validate(vcor);
x1 = EFMVcoeff(expected_mean1,vcor);
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

CMLplot(tlow, thigh, tinc, alpha0, alpha1, beta0,  beta2, expected_mean1, vcor, r) ; 
