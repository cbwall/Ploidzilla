## Comparing host and symbiont groups 
## Following Turner et al. 2010   
## Turner, T.F., Collyer, M.L., & Krabbenhoft, T.J. 2010. A general hypothesis-testing 
## framework for stable isotope ratios in ecological studies. Ecology, 91, 2227-2233.
## DOI: https://doi.org/10.1890/09-1454.1
## Source code and examples: http://www.esapubs.org/archive/ecol/E091/157/suppl-1.htm
## Downloaded 17 June 2016
## Used in Conti-Jerpe et al. 2020


rm(list=ls())
graphics.off()

setwd("~/HKU/RGC Data")

source('Turner.et.al.ecology.source.r')

#headings of columns in .csv must be "group", "x", and "y"
ex.data<-read.csv("RGC old SIBER ACRO 20181012.csv",header=T)

Y<-as.matrix(ex.data[,2:3])


# Designate groups
group<-as.factor(ex.data[,1])
gp<-length(levels(group)) # number of groups
n.comp<-(gp^2-gp)/2 # number of possible comparisons
rownames(Y)<-group 

lm.gp<-lm(Y~group,x=T,model=T) # for estimating group means
#This outputs the centroid of one group and distance in x and y to the centroid of the second group
res.gp<-resid(lm.gp) # residuals of groups from group means
yhat.gp<-predict(lm.gp) # predicted values

lm.gp.red<-lm(Y~1) # this is the model reduced by the group factor: only estimates an overall mean



#PLOTS

par(mai=c(1,1,0.5,0.5))

Trophic.1<-ex.data[ex.data[,1]=="1",2:3]
Trophic.2<-ex.data[ex.data[,1]=="2",2:3]
All.pts<-rbind(Trophic.1,Trophic.2)


plot(All.pts,type='n',asp=1,xlab=expression(delta^13~C),ylab=expression(delta^15~N),cex.lab=1.7,cex.axis=1.7)
points(Trophic.1, pch=21,bg="#ACA4E2")
points(Trophic.2, pch=21,bg="#5CBD92")
Trophic.1.c<-colMeans(Trophic.1)
Trophic.2.c<-colMeans(Trophic.2)

points(Trophic.1.c[1],Trophic.1.c[2], pch=21,bg="#ACA4E2",cex=1.7,lwd=2.5)
points(Trophic.2.c[1],Trophic.2.c[2], pch=21,bg="#5CBD92",cex=1.7,lwd=2.5)



# DISPERSION MEASURES
ex1<-ds.prep(res.gp,group) # see source file
ex1.ds<-disp.stat(ex1) # see source file
ex1.ds

# GROUP MEANS
gp.m<-group.means(Y,group) # finds the group means for the raw data
gp.m

# CONTRASTS
mean.dif<-as.vector(dist(as.matrix(gp.m)))
mean.dif

disp.dif<-ds.dif(ex1.ds) # Dimension names not given by output

disp.dif 
disp.dif$mdc
disp.dif$mnn
disp.dif$ecc
#mdc = mean centroid distance
#mnn = mean nearest neighbor
#ecc = eccentricity


#### PERMUTATION PROCEDURE ####

# Describe a function that needs
# x = the full linear model
# x2 = the reduced linear model
# y = the raw data
# g = group list
# p = the number of permutations to use (keep in mind that
# the observed values count as 1 random permutation)

# SETUP
permute<-function(x,x2,y,g,p){
  p.table<-NULL
  
  yhat<-predict(x)
  res<-resid(x)
  line<-nrow(res) # this defines the number of permutable objects
  yhat.2<-predict(x2)
  res.2<-resid(x2)
  
  mean.dif<-as.vector(dist(as.matrix(group.means(Y,g))))
  
  
  for(i in 1:p){
    
    #For dispersion tests
    
    line.rand<-sample(line,replace=FALSE) # This creates a line from 1 to the number
    # of permutable objects, then randomizes the order
    res.temp<-cbind(line.rand,res) # attaches the random order to the ordered matrix
    z<-(order(line.rand)) # randomizes matrix as it reorders line
    res.temp2<-as.matrix(res.temp[z,])# removes randomized line
    res.p<-res.temp2[,-1] # Rows of residuals are now randomized
    y.rand<-yhat+res.p # random values created
    
    # The resampling procedure above is the same as randomizing original values
    # (Since the reduced model only contains the overall mean)
    # But using residuals makes it applicable to multi-factor models
    
    lm.r<-lm(y.rand~g,x=T,model=T) # new linear model
    r<-resid(lm.r)
    yhat<-predict(lm.r)
    ex1.r<-ds.prep(r,g) #thanks to the functions, prep takes one step
    ex.r.ds<-disp.stat(ex1.r) # thanks to the function, dispersion stats require one step
    disp.dif.r<-ds.dif(ex.r.ds) # thanks to function, contrasts require one step
    
    
    # For means tests
    
    # uses different linear model
    # the null hypothesis that means are equal
    # means that an intercept model (i.e., defines only the overall mean)
    # is as viable as a group means model.
    # Thus, residuals are calculated from the intercept model (see above)
    # and random means are created despite no mean differences define by the model
    # this creates random distibutions of outcomes under the null hypothesis
    
    res.2.temp<-cbind(line.rand,res.2)
    z<-(order(line.rand))
    res.2.temp2<-as.matrix(res.2.temp[z,])
    res.2.p<-res.temp2[,-1] # Rows of residuals are now randomized
    y.rand.2<-yhat.2+res.2.p
    
    
    gm.r<-group.means(y.rand.2,g)
    md.r<-as.vector(dist(as.matrix(gm.r)))
    
    result<-c(i,md.r,disp.dif.r$mdc,disp.dif.r$mnn,disp.dif.r$ecc) # bind all results together
    p.table<-rbind(p.table,result) # add them to a table, row by row
    
  }
  
  head<-NULL # create a header
  line1<-as.vector(c(0,mean.dif,disp.dif$mdc,disp.dif$mnn,disp.dif$ecc))
  
  # The following is a bunch of code simply for generating column names in the output
  
  cn<-length(mean.dif) # cn = column name
  
  test.list<-NULL
  if (cn>1) for(i in 1:cn){
    l1<-rep(i,cn)
    l2<-array(1:cn)
    l12<-cbind(l1,l2)
    test.list<-rbind(test.list,l12)
  }
  
  test.list2<-NULL
  if (cn>1) for(j in 1:nrow(test.list)){
    t<-test.list[j,]
    if(t[2]>t[1]) test.list2<-rbind(test.list2,t)
  }
  
  test.list3<-NULL
  if (cn>1) for(k in 1:nrow(test.list2)){
    t<-test.list2[k,]
    lab<-paste(t[1],t[2],sep="--")
    test.list3<-rbind(test.list3,lab)}
  
  if (cn==1) test.list3<-c("1--2")
  
  lab2<-c(rep("MD",cn),rep("MDC",cn),rep("MNN",cn),rep("ECC",cn))
  test.list4<-paste(lab2,test.list3,sep=".")
  
  head<-c("iteration",test.list4)
  p.table<-rbind(head,line1,p.table)
  
}


test1<-permute(lm.gp,lm.gp.red,Y,group,999) # run the permutation test


# results can be opened in e.g.,
# excel. Find the P-value by measuring the rank of the first value in the
# distribution of values in the same column.  
# p.table.out<-test1
# write.csv(p.table.out,"ACRO_Turner_results.csv") 
# Alternatvely, more R-code can
# be generated to calculate P-values by using a function that ranks observed values
# see below

# P-Values
h<-test1[1,];h<-h[-1]
t<-test1[-1,];t<-t[,-1]
f<-function(t){
  r<-rank(t)
  p<-r[1]
  c<-length(t)
  pv<-(c-p+1)/c}
p.value<-apply(t,2,f)
names(p.value)<-h

p.value


#ADDENDUM: HOTELLING'S T2
#Each group comparison one at a time

#Comparing groups 1 (host) and 2 (symbiont) 
gp.m.dif<-gp.m[1,]-gp.m[2,] # vector for difference between means
gn<-tapply(group,group,length) # group sizes
e<-resid(lm.gp)
E<-t(e)%*%e
n<-nrow(e)
k<-lm.gp$rank
V<-(1/(n-k))*E # This is the pooled within-group vcv

d<-gp.m.dif; dim(d)<-c(1,length(d))

D<-d%*%solve(V)%*%t(d) # Squared Mahalanobis Distance

H.T2<-(gn[1]*gn[2])/(gn[1]+gn[2])*D # Hotelling T2

F<-(gn[1]+gn[2]-2-1)/((gn[1]+gn[2])*2)*H.T2 # Convert to an F value

P<-df(F,2,(gn[1]+gn[2]-2-1)) # P-value

P

