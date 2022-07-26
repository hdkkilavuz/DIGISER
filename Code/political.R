

setwd('/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/My_Exam/mcshapiro.test')
load('mcshapiro.test.RData')

library(readxl)
setwd("C:/Users/miche/Documents/UNI/Polimi/Courses/2 semester/Applied Statistics/Project/Code")
data_political <- read.csv("political_data.csv", head=T, sep = ",")
data_country<-read_excel("countries_excel_2.xlsx")

data <- merge(x = data_country, y = data_political, by = "CCode", all.x = TRUE)
data <- data[,-c(2,7,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53)]
data <- data[-c(255,238,1),]








#### TEST OF MEAN ON THE DSPVI VARIABLE (Incomplete) ####


data <- data[,35:36]

orientations <- factor(data$party_family)
data_agrar <- data[data$party_family == 'Agrarian', ]
data_chris <- data[data$party_family == 'Christian democracy', ]
data_comun <- data[data$party_family == 'Communist/Socialist', ]
data_conse <- data[data$party_family == 'Conservative', ]
data_liber <- data[data$party_family == 'Liberal', ]
data_right <- data[data$party_family == 'Right-wing', ]
data_socia <- data[data$party_family == 'Social democracy', ]

data_agrar$party_family <- NULL
data_chris$party_family <- NULL
data_comun$party_family <- NULL
data_conse$party_family <- NULL
data_liber$party_family <- NULL
data_right$party_family <- NULL
data_socia$party_family <- NULL

n_agrar <- dim(data_agrar)[1]
n_chris <- dim(data_chris)[1]
n_comun <- dim(data_comun)[1]
n_conse <- dim(data_conse)[1]
n_liber <- dim(data_liber)[1]
n_right <- dim(data_right)[1]
n_socia <- dim(data_socia)[1]
p  <- 1

data_agrar <- as.data.frame(sapply(data_agrar, as.numeric))
data_chris <- as.data.frame(sapply(data_chris, as.numeric))
data_comun <- as.data.frame(sapply(data_comun, as.numeric))
data_conse <- as.data.frame(sapply(data_conse, as.numeric))
data_liber <- as.data.frame(sapply(data_liber, as.numeric))
data_right <- as.data.frame(sapply(data_right, as.numeric))
data_socia <- as.data.frame(sapply(data_socia, as.numeric))






# we compute the sample mean, covariance matrices and the matrix Spooled
# we don't do difference between the two populations: they are independent!

data_agrar.mean <- sapply(data_agrar,mean)
data_agrar.cov  <-  cov(data_agrar)
data_chris.mean <- sapply(data_chris,mean)
data_chris.cov  <-  cov(data_chris)
data_comun.mean <- sapply(data_comun,mean)
data_comun.cov  <-  cov(data_comun)
data_conse.mean <- sapply(data_conse,mean)
data_conse.cov  <-  cov(data_conse)
data_liber.mean <- sapply(data_liber,mean)
data_liber.cov  <-  cov(data_liber)
data_right.mean <- sapply(data_right,mean)
data_right.cov  <-  cov(data_right)
data_socia.mean <- sapply(data_socia,mean)
data_socia.cov  <-  cov(data_socia)
Sp      <- ((n_agrar-1)*data_agrar.cov + (n_chris-1)*data_chris.cov + (n_comun-1)*data_comun.cov + (n_conse-1)*data_conse.cov 
            + (n_liber-1)*data_liber.cov + (n_right-1)*data_right.cov + (n_socia-1)*data_socia.cov)/(n_agrar+n_chris+n_comun+n_conse+n_liber+n_right+n_socia-7)

# we compare the matrices
#list(S1=data_agrar.cov, S2=data_chris.cov, S3=data_comun.cov, S4=data_conse.cov, S5=data_liber.cov, S6=data_right.cov, S7=data_socia.cov, Spooled=Sp)


# Test of Gaussianity of each population
mcshapiro.test(data_agrar)$pvalue
mcshapiro.test(data_chris)$pvalue
mcshapiro.test(data_comun)$pvalue
mcshapiro.test(data_conse)$pvalue
mcshapiro.test(data_liber)$pvalue
mcshapiro.test(data_right)$pvalue
mcshapiro.test(data_socia)$pvalue
# High (> 0.1): there is no statistical evidence to reject H0 (I can assume Gaussianity of data)




alpha   <- .01
delta.0 <- 0



# Test H0: mu_agr == mu2  vs  H1: mu1 != mu2



Spinv   <- solve(Sp)
# in the statistics i dont have the mean of the difference, but the difference of the means
T2 <- n1*n2/(n1+n2) * (data1.mean-data2.mean-delta.0) %*% Spinv %*% (data1.mean-data2.mean-delta.0)
# n1*n2/(n1+n2) is used to normalize
cfr.fisher <- (p*(n1+n2-2)/(n1+n2-1-p))*qf(1-alpha,p,n1+n2-1-p)


# Check if mu0 is outside the rejection region {T2>cfr.fisher}:
T2 < cfr.fisher
# True: there is no statistical evidence to reject H0 at level alpha (the statistic is outside the rejection region).
# False: mu0 is inside the rejection region so there is statistical evidence to reject H0 at level alpha


# compute pvalue:
pvalue <- 1 - pf(T2/(p*(n1+n2-2)/(n1+n2-1-p)), p, n1+n2-1-p)
pvalue
# High (> 0.1): there is no statistical evidence to reject H0 
# Very low (< 0.05): there is statistical evidence to reject H0 










#### ANOVA WITH POLITICAL ORIENTATION AS LEVELS OF TREATMENT ####



# ANOVA may be a good analysis to check if the political orientation of the cities have an impact
# on the digitization of them. We will focus on the DPSVI index, that express the final degree of digitization 
# of a city.




data <- data[,35:36]




# Setting dataset:

# labels:
factor1 <- factor(data[,2]) # we have to put the column where the labels or categorical variables are

# numerical data
x <- data[,1] # or the column there the numerical data is
x <- as.data.frame(sapply(x, as.numeric))
x <- x[,1] # or the column there the numerical data is

par(mar=c(8,8,8,8))
boxplot(x~factor1,   main='Digitalization of cities with different party families'  , 
        ylab='Digitalization Variability', xlab='Political Families', cex.lab=1.8, cex.axis=1.3, cex.main=2.5, cex.sub=1.5, col=rainbow(g))



#c('khaki','yellow','orange', 'sienna', 'tomato', 'orangered', 'red')



# Exploring the variability of the 7 population, each with a different party family, we can see that
# cities with different political orientations have different digitization variability.







p <- 1
n <- dim(data)[1]         # number of observations
ng <- table(factor1)      # number of observations in each group
treat <- levels(factor1)  # levels of the treatment
g <- length(treat)        # number of levels/groups/labels



# Model assumptions:
# - Gaussian distribution of the error;
# - Homoschedasticity, is an assumption of equal or similar variances in different groups being compared
#   so same covariance structure





# - Normality in each group:
# 
# we are in ANOVA setting, so we perform the univariate shapiro tests, one for each group:
pvalue <- NULL
for (i in 1:g) {
  pval <- shapiro.test(x[factor1==treat[i]])$p
  pvalue <- c(pvalue, pval)
}
pvalue
# p-values are large, I can accept the hypothesis of Gaussianity of data
# and each level is gaussian distributed



# - Same covariance structure

# I can perform the Bartlett test (that relies on Gaussianity assumption) to check homogeneity of variances. 
# Namely, the test I'm performing is the following:
# \[ H_0: \sigma_1 = \sigma_2 = \dots = \sigma_g \quad vs \quad H_1: \exists i,j s.t. \sigma_i \neq \sigma_j\]

bartlett.test(x, factor1)

# pvalue VERY LARGE (>alpha): we cannot reject the null hypothesis, I can assume Homoschedasticity





# After having run the ANOVA and having checked the assumption of normality and homoschedasticity of the data,
# we got that the political orientation has an effect on the variability of the digitization
# (pvalue is small: we reject H0 at level 99%, so we have evidence to say that the political orientation
# has an effect on the digitization.








# Now we want to see which treatment is responsible for this effect. 
# So we perform g*(g-1)/2 tests simultaneously, one for each couple of treatments.
# We use BONFERRONI approach.

k <- g*(g-1)/2    # +1 se chiede sia media che varianza number of comparisons
alpha <- 0.05     # overall level # ******************************************************************************

Mediag <- tapply(x, factor1, mean) 
SSres <- sum(residuals(fit)^2)
S <- SSres/(n-g)



# CONFIDENCE INTERVALS for all the differences in mean at level 1-alpha - BONFERRONI
# we are in univariate case so we use the quantile that is the t student
# also, we center the interval in the difference between the means
ICrange=NULL
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    print(paste(treat[i],"-",treat[j]))        
    print(as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
    ICrange=rbind(ICrange,as.numeric(c(Mediag[i]-Mediag[j] - qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )),
                                       Mediag[i]-Mediag[j] + qt(1-alpha/(2*k), n-g) * 
                                         sqrt( S * ( 1/ng[i] + 1/ng[j] )))))
  }}

# plot CI for all the differences in mean - BONFERRONI
par(mar=c(8,8,8,8))


h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',
     xlab='Political families pairs', ylab='Confidence interval at level 99%', main='Bonferroni intervals of political families'  ,
     cex.lab=1.8, cex.axis=1.3, cex.main=2.5, cex.sub=1.5)
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55', cex=4); 
    points(h, ICrange[ind,1], col=rainbow(g)[j], pch=16, cex=4); 
    points(h, ICrange[ind,2], col=rainbow(g)[i], pch=16, cex=4); 
    h <- h+1
  }}
abline(h=0)
# Note: if the interval do not contains 0, 
# there is a strong statistical difference between the effects of the two treatments.

# we can interpret as: if an interval contains 0, it means that we dont have evidence to reject that H0: mu1 - m2 = 0, 
# because the proposed model (0)
# actually is inside the confidence region, and this means that we didnt have an effect between those two treatment
# while if in a interval we don't have the 0, it means we had an effect (positive or negative)












# We perform g*(g-1)/2 = 21 tests simultaneously, one for each couple of political families.
# We use BONFERRONI approach to understand which political families are responsible for this effect.

# We obtain 21 intervals, one for each couple of political families. 
# If an interval contains 0, this means that we don't have an effect between those two political families
# (meaning that the mean of the two political family is the same).

# Finally We can state that Christian Democracy and Liberal orientation has an effect on the digitization,
# as well as Conservative oreintation and again Christian Democracy.



