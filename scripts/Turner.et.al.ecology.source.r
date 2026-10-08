##########################################
##########     Turner et al.    ##########
##########     Source code      ##########
##########################################

####  Dispersion Stats Set-up 
# Create an array for all groups. THIS IS NEEDED!
# this is a 3-d matrix of n rows x 2 isotope variables x k groups
# where n is the maximum sample size

ds.prep<-function(A,g){ # A is the matrix of residual data; g is a list of group ids

	rownames(A)<-g
	n.m<-NULL # n.m stands for "new" matrix
	n.t<-length(g) # the 'total' sample size
	gp<-length(levels(g)) # the number of groups
	gn<-tapply(g,g,length) # the separate group sample sizes
	nmax<-max(gn) # the maximum group size

	s<-1  # the start point
	n<-gn[1]  # the sample size of the first group
	
	B<-NULL # This simple step orders the data by groups, irrespective of the original order
	for(i in 1:n.t){
		temp<-as.matrix(A[which(g==levels(g)[i]),])
		B<-rbind(B,temp)
		}

	for(i in 1:gp){# Create objects 'res.gp.1', 'res.gp.2', ... 'res.gp.n'

		d<-nmax-gn[i]
		blank<-as.matrix(array(NA,2*d));dim(blank)<-c(d,2)
		nam <- paste("res.gp",i, sep=".")
		temp<-B[s:n,1:2]
		temp<-rbind(temp,blank)  # this assures equal sample sizes for the matrices
		assign(nam,temp) # note: the matix has correct dimensions
		temp<-array(temp) # now the matrix is a vector
		n.m<-c(n.m,temp) # now there is a matrix of row vectors

		s<-s+gn[i]
		n<-n+gn[i+1]

	}

	C<-array(n.m,dim=c(nmax,2,gp)) # the vectors are reassembled into submatrices of a 3-d matrix
}


### Functions for dispersion stats

# Mean distance to Centroid

mdc<-function(w){ # w will be the 3-d matrix from above
	d.sum<-0
	for(i in 1:(nrow(w))){
		v<-w[i,]
		dim(v)<-c(1,length(v))
		d<-sqrt(v%*%t(v)) # this is the Euclidean distance of the residual to centroid
		d.sum<-d.sum+d # this adds that distance to the progressive sum
	}
	m<-d.sum/nrow(w) # this calculates the mean of the sum of all distances
}

# Mean nearest neighbor

mnn<-function(w){
	d<-as.matrix(dist(w)) # first create distance matrices
	n<-ncol(d) # each column provides one value; thus n is the # of columns
	d.sum<-0
	for(i in 1:n){
		y<-d[,i]
		# need to subtract the 0 from the column, which is the distance of
		# something to itself
		y<-y[-i]
		m<-min(y) # the minimum distance is the nearest neighbor
		d.sum<-d.sum+m # this adds that distance to the progressive sum
	}
	mnn<-d.sum/n # this calculates the mean of the sum of all nearest distances
}

# Eccentricity

ecc<-function(w){
	v<-var(w)
	p<-eigen(v)
	ev<-p$values
	ecc<-1-(ev[2]/ev[1])
}

# Combine dispersion stats
# Rows are groups

disp.stat<-function(w){
	result<-NULL
	rn<-NULL # rn = row name
	for(j in 1:gp){
		x<-w[,,j]
		x<-na.omit(x)
		m<-mdc(x) # Note that this function calls the functions defined above
		n<-mnn(x)
		e<-ecc(x)
		rn<-c(rn,paste("Group",j,sep=".")) # names the rows
		result<-rbind(result,c(m,n,e))
		rownames(result)<-rn
		colnames(result)<-c("mdc","mnn","ecc") # names the columns
	
	}
	result
}


# Group means
# this function will generate group means irrespective of sample size

group.means<-function(A,g){# A = data, g = list of group ids
	rownames(A)<-g
	l<-lm(A~g,x=T,model=T)
	ls<-data.frame(g=levels(g))
	ls[]<-lapply(ls,factor)
	m<-predict(l,ls)
	m
	}
	
	
# Contrast dispersion measures

# by creating a distance matrix for univariate values
# it takes the square root of the squared difference
# for every pairwise comparison
# this is a shortcut for finding the absolute value
# of the difference

ds.dif<-function(A){
	n.m<-NULL # n.m stands for "new" matrix
	c<-ncol(A)
	for(i in 1:c){
  	  temp<-as.matrix(A[,i])
 	  dis<-as.vector(dist(temp))
   	  n.m<-cbind(n.m,dis)
 	 }
  n.g<-list(mdc=n.m[,1],mnn=n.m[,2],ecc=n.m[,3])
  
  n.g
 }


# GPA FUNCTIONS

# The following are functions for setting-up a generalized Procrustes
# analysis

# First, specify the trajectories, which are matrices with 
# rows indicating the location of centroids

arrayspecs<-function(A,g,o){ # A is matrix of means (centroids) for groups by observations; g is a list of groups; o is a list of observations (e.g., time steps or geographic locations, within groups).  One must be careful to assure that the A matrix is in proper order (levels of o within levels of g).  One should check the output to make sure that vectors are correctly described, based on the means input 
	
	n.m<-NULL # n.m stands for "new" matrix
	gp<-length(levels(g))
	n.obs<-length(levels(o))
	for(i in 1:gp){
  		temp<-as.matrix(A[((1+(i-1)*n.obs):(i*n.obs)),1:2])
  		n.m<-cbind(n.m,temp)
  		}
  		
	n.g<-array(n.m,dim=c(n.obs,2,gp))
	}
	
# Second, calculate the path distance (the sum of consecutive Euclidean 
# distance between points, which is only the Euclidean distance of 
# a vector for two points)

# Pathlength distance
pathdist<-function(M) {as.matrix(dist(M))}
trajsize<-function(M){
traj.pathdist<-array(0,dim=c(gp,1))   	#loop across trajectories
for (i in 1:gp){
  temp<-pathdist(M[,,i])
  for (j in 1:(n.obs-1)){
    traj.pathdist[i]<-traj.pathdist[i]+temp[j,j+1]
  }
}

# For pairwise comparisons:
traj.size.dist<-as.matrix(dist(traj.pathdist))	
}

# Third, find the general directional differences of the trajectories
# For two points, this is an angle between corresponding vectors,
# which is the same as the angle between first PCs
# For >2 points, this is just the angle between first PCs

#trajectory direction 
orient<-function(M) {(svd(var(M))$v[,1])} 	#find orientation (PC)

trajorient<-function(M,ang.cor=TRUE){# ang.cor is an angle correction if one wishes to arbitrarily correct angles > 90 degrees.  This is useful in permutation tests when direction is assigned to random patterns.  It is also useful for correcting an observed angle that has been wrongly made obtuse or acute because of a PC vector in an opposite direction. The default is true, so one must change it to ang.cor=F to override this option.
	
traj.orient<-array(NA,dim=c(gp,2))  # create a matrix for separate PCs 		

# Unfortunately, PCs can be arbitrarily described with respect to direction
# (that is, a rotation of a PC 180 degrees produces the same PC, but with 
# opposite loadings).  One must check to see if this is the case.  If so
# one of the observed PC vectors can be multiplied by -1 to change its 
# direction 180 degrees and correct this problem.  Or one can use the ang.cor# option in
# this function

for (i in 1:gp){
  traj.orient[i,]<-orient(M[,,i]) # finds first PC, group by group
  
 }
 
options(warn=-1)		#b/c acos of 1 (e.g, on diagonal) yields warning
traj.ang.diff<-(180/pi)*acos(traj.orient%*%t(traj.orient))
diag(traj.ang.diff)<-0
traj.ang.diff
if(!is.null(ang.cor)){
	for(i in 1:length(traj.ang.diff)){
		if(traj.ang.diff[i]>90) traj.ang.diff[i]=180-traj.ang.diff[i]
		}
	}
traj.ang.diff
}

# Fourth, perform GPA to get shape differences
# GPA: following J. Claude 2008: Morphometrics in R
# Please see this reference for additional notes on functions

trans<-function(A){scale(A,scale=F)} 	## TRANSLATION

csize<-function(A){				## CENTROID SIZE
 
  size<-sqrt(sum(apply(A,2,var))*(n.obs-1))
  list("centroid_size"=size,"scaled"=A/size)}
  
mshape<-function(A){apply(A,c(1,2),mean)}	# meanshape	

pPsup<-function(M1,M2){				## OPA rotation 1-->2
  k<-ncol(M1)
  Z1<-trans(csize(M1)[[2]])
  Z2<-trans(csize(M2)[[2]])
  sv<-svd(t(Z2)%*%Z1)
  U<-sv$v; V<-sv$u; Delt<-sv$d
  sig<-sign(det(t(Z1)%*%Z2))
  Delt[k]<-sig*abs(Delt[k]); V[,k]<-sig*V[,k]
  Gam<-U%*%t(V)
  beta<-sum(Delt)
  list(Mp1=beta*Z1%*%Gam,Mp2=Z2,rotation=Gam,scale=beta,
       df=sqrt(1-beta^2))}

pgpa<-function(A)
  {p<-dim(A)[1]; k<-dim(A)[2]; n<-dim(A)[3]  
  temp2<-temp1<-array(NA,dim=c(p,k,n)); Siz<-numeric(n)#; Qm2<-numeric(n)
  for (i in 1:n){
  	Acs<-csize(A[,,i])
    Siz[i]<-Acs[[1]]
    temp1[,,i]<-trans(Acs[[2]])}
  Qm1<-dist(t(matrix(temp1,k*p,n)))
  Q<-sum(Qm1); iter<-0
  while (abs(Q)> 0.00001)
    {for(i in 1:n){
      M<-mshape(temp1[,,-i])
      temp2[,,i]<-pPsup(temp1[,,i],M)[[1]]}
    Qm2<-dist(t(matrix(temp2,k*p,n)))
    Q<-sum(Qm1)-sum(Qm2)
    Qm1<-Qm2
    iter=iter+1
    temp1<-temp2}
  list("rotated"=temp2,"it.number"=iter,"Q"=Q,"intereucl.dist"=Qm2,"mshape"=
	csize(mshape(temp2))[[2]],"cent.size"=Siz)
}

## loop for GPA and shape distances
trajshape<-function(M){
  x<-pgpa(M)
  traj.shape.dist<-as.matrix(x$intereucl.dist) 
}






