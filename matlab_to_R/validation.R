#Matrix is symmetric or not
isSymmetric <- function(Matrix, epsilon)					# var i,j should be var j,i
{
	if(isSquare(Matrix))
	{
		if(norm(Matrix - t(Matrix))>epsilon)
		{	
			print("Not a symmetric matrix");
			return(FALSE);
		}
		else
		{
			return(TRUE);
		}
	}
	else	
	{	
		return(FALSE);
	}
}

isPosDef <- function(Matrix, epsilon)				#min should be foundable
{
	if(isSquare(Matrix))
	{
		result = eigen(Matrix, symmetric = TRUE, only.value = TRUE, EISPACK=FALSE);
		if(min(result$values) < epsilon)
		{
			print("Not a Positive Definite Matrix");
			return(FALSE);	
		}
		else
		{
			return(TRUE);
		}
	}
	else
	{
		return(FALSE);
	}
}

isSquare <- function(Matrix)
{
	M = dim(Matrix)[1]; #rows
	N = dim(Matrix)[2]; #columns
	if(M != N)
	{
		print("Matrix is not square");
		return(FALSE);
	}	
	else
	{
		return(TRUE);
	}
}


#Here Matrix is M X N
validate <- function(Matrix)
{
	isValid=FALSE;		
	epsilon = 0.000001;
	if(isSymmetric(Matrix, epsilon) && isPosDef(Matrix, epsilon))
	{
		print("Matrix is positive definite")
	}
}
