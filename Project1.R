library(jpeg);
library(lattice);
library(ggplot2);
rm(list = ls());

#________________________

#INPUT
files <- list.files("E:/project_ail/data", full.name = TRUE);
n <- length(files);

#________________________

#CALCULATE IMAGES HISTOGRAM COLOR
#clch = color channel
histogram <- function(img,u1,v1,u2,v2,clch) 
{
	h <- 0 * c(1 : 16);
	for (i in u1 : u2) 
	{
		for (j in v1 : v2) 
		{
			x <- floor(255 * img[i,j,clch]/16.0) + 1;
			h[x] <- h[x] + 1;
		}
	}
	return(h);
}

#________________________

#DATASET
Data <- rbind();
for (i in 1 : n) 
{
	img <- readJPEG(files[i]);
	m <- nrow(img[ , ,1]);
	n <- ncol(img[ , ,1]);
	m1 <- floor(m/2);
	n1 <- floor(n/2);
	x <- c();
	for (clch in 1 : 3) 
	{
		x <- c(x,histogram(img,1,1,m,n,clch));
		x <- c(x,histogram(img,1,1,m1,n1,clch));
		x <- c(x,histogram(img,1,n1+1,m1,n,clch));
		x <- c(x,histogram(img,m1+1,1,m,n1,clch));
		x <- c(x,histogram(img,m1+1,n1+1,m,n,clch));
	}
	Data <- rbind(Data,x);
}

#________________________

#K-MEAN
kmeanLabel <- function(X, C) 
{
	m <- nrow(X);
	n <- nrow(C);
	labels <- 1 : m;
	z <- 1 : n;
	for (i in 1 : m) 
	{
		for (j in 1 : n) 
		{
			z[j] <- sum((X[i, ]-C[j, ])*(X[i, ]-C[j, ]));
		labels[i] <- which.min(z);
		}
	}
	return(labels);
}

kmeanAvg <- function(X,L,k) 
{
	n <- nrow(X);
	C <- matrix(nrow=k,ncol=ncol(X),data=0);
	N <- rep(0,k);
	for (i in 1 : n) 
	{
		z <- L[i];
		C[z, ] <- C[z, ]+as.numeric(X[i, ]);
		N[z] <- N[z] + 1;
	}
	for (i in 1 : k) 
	{
		if (N[i]>0) 
		{
			C[i, ] <- C[i, ]/N[i];
		}
	}
	return(C);
}

kmeanInit <- function(X,k) 
{
	d <- ncol(X);
	C <- matrix(nrow = k, ncol = d);
	for (i in 1 : k)
		C[i, ] <- runif(d,0,1024);
	return(C);
}

k <- 8;
maxstep <- 20;
set.seed(44);
C <- kmeanInit(Data,k);
L <- kmeanLabel(Data,C);
for (i in 1 : maxstep) 
{
	C <- kmeanAvg(Data,L,k);
	L <- kmeanLabel(Data,C);
}

#________________________

#OUTPUT
n <- length(L);
sink(file = "E:/project_ail/output/output.html", type = "output");
#title
cat("<h1> PROJECT:CLUSTERING </h1>");
#k
cat("<p> k = ",k,"</p>");
#group
for (i in 1 : k) {
	cat("<strong><p>GROUP ",i,"</p></strong>");
	for (j in 1 : n) 
	{
		if (L[j]==i) 
		{
			cat("<img src = ",files[j],">");
		}
	}
	cat("<p>________________________________________________________________________________________________________________</p>");
}

sink(file = NULL,type = "output");