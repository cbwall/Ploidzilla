## SIBER Script following Jackson et al. 2011 
## Jackson, A.L., Parnell, A.C., Inger R., & Bearhop, S. 2011. Comparing isotopic niche 
## widths among and within communities: SIBER - Stable Isotope Bayesian Ellipses in R. 
## Journal of Animal Ecology, 80, 595-602. 
## DOI: https://doi.org/10.1111/j.1365-2656.2011.01806.x 
## Example vignette: https://cran.r-project.org/web/packages/SIBER/vignettes/Introduction-to-SIBER.html #####
## Updated by Jackson 28 April 2017 
## Downloaded 02 October 2018
## Used in Conti-Jerpe et al. 2020


#headings of columns in .csv must be "iso1", "iso2", "group", and "community"
#all headings & columns must be present

rm(list=ls())
graphics.off()
set.seed(1)

library(SIBER)

setwd("~/HKU/RGC Data")

ACRO <- read.csv("RGC SIBER ACRO 20180917.csv", header=T)
FAV <- read.csv("RGC SIBER FF 20180917.csv", header=T)
GONI <- read.csv("RGC SIBER GONI 20180917.csv", header=T)
PAV <- read.csv("RGC SIBER PAV 20180917.csv", header=T)
PLAT <- read.csv("RGC SIBER PLAT 20180917.csv", header=T)
POR <- read.csv("RGC SIBER POR 20180917.csv", header=T)
TURB <- read.csv("RGC SIBER TURB 20181011.csv", header=T)

siber.example <- createSiberObject(ACRO)
siber.example <- createSiberObject(FAV)
siber.example <- createSiberObject(GONI)
siber.example <- createSiberObject(PAV)
siber.example <- createSiberObject(PLAT)
siber.example <- createSiberObject(POR)
siber.example <- createSiberObject(TURB)


# calculate metrics
community.hulls.args <- list(col = 1, lty = 1, lwd = 1)
group.ellipses.args  <- list(n = 100, p.interval = 0.40, lty = 1, lwd = 2)
group.hull.args      <- list(lty = 2, col = "grey20")


#Plot data with convex hulls and SEA
palette(c("#ACA4E2","#5CBD92"))


par(mfrow=c(1,1))
plotSiberObject(siber.example,
                ax.pad = 2, 
                hulls = F, community.hulls.args, 
                ellipses = T, group.ellipses.args,
                group.hulls = T, group.hull.args,
                bty = "o",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

# Calculate summary statistics for each group: TA, SEA and SEAc
group.ML <- groupMetricsML(siber.example)
print(group.ML)

# Calculate the various Layman metrics on each of the communities.
community.ML <- communityMetricsML(siber.example) 
print(community.ML)


#Fitting Bayesian models to the data
# options for running jags
parms <- list()
parms$n.iter <- 2 * 10^4   # number of iterations to run the model for
parms$n.burnin <- 1 * 10^3 # discard the first set of values
parms$n.thin <- 10     # thin the posterior by this many
parms$n.chains <- 2        # run this many chains

# define the priors
priors <- list()
priors$R <- 1 * diag(2)
priors$k <- 2
priors$tau.mu <- 1.0E-3

# fit the ellipses which uses an Inverse Wishart prior
# on the covariance matrix Sigma, and a vague normal prior on the 
# means. Fitting is via the JAGS method.
ellipses.posterior <- siberMVN(siber.example, parms, priors)


# The posterior estimates of the ellipses for each group can be used to
# calculate the SEA.B for each group.
SEA.B <- siberEllipses(ellipses.posterior)


siberDensityPlot(SEA.B, xticklabels = colnames(group.ML), 
                 xlab = c("Community | Group"),
                 ylab = expression("Standard Ellipse Area " ('\u2030' ^2) ),
                 bty = "L",
                 las = 1,
                 main = "SIBER ellipses on each group"
)

# Add red x's for the ML estimated SEA-c
points(1:ncol(SEA.B), group.ML[3,], col="red", pch = "x", lwd = 2)


# Calculate some credible intervals 
cr.p <- c(0.95, 0.99) # vector of quantiles


# call to hdrcde:hdr using lapply()
SEA.B.credibles <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$hdr},
  prob = cr.p)


# do similar to get the modes, taking care to pick up multimodal posterior
# distributions if present
SEA.B.modes <- lapply(
  as.data.frame(SEA.B), 
  function(x,...){tmp<-hdrcde::hdr(x)$mode},
  prob = cr.p, all.modes=T)





### Ellipses overlap

# The first ellipse is referenced using a character string representation where 
# in "x.y", "x" is the community, and "y" is the group within that community.
# So in this example: community 1, group 2
ellipse1 <- "1.1" 

# Ellipse two is similarly defined: community 1, group3
ellipse2 <- "1.2"

# Overlap metric (units are per mil^2)
# The overlap of the maximum likelihood fitted standard ellipses are 
# estimated using
sea.overlap <- maxLikOverlap(ellipse1, ellipse2, siber.example, 
                             p.interval = NULL, n = 100)
sea.overlap


# overlap as a proportion of group 1 area (host)
prop.sea.over.1 <- sea.overlap[3] / (sea.overlap[1])
prop.sea.over.1
