library(readxl)


df <- read.csv("indicators_and_political.csv")[,-1]
skill <- read_xlsx("skill.xlsx")
colnames(skill) <- c("country", "skill")
df <- merge(df, skill, by="country")

hist(df$skill, breaks = 20)

low <- df$skill<40
med <- df$skill>40 & data$skill<60
high <- df$skill>60

class <- as.factor(0*low + med + 2*high)
df <- data.frame(df, class = class)

alpha <- 0.05
g <- 3 # or 3,... number of groups
n <- dim(df)[1] #Observations
p <- 1 #covariates

fit.aov <- aov(DPSVI ~class, df)
DF <- fit.aov$df.residual #degrees of freedom
Sp <- sum(fit.aov$residuals^2)/fit.aov$df.residual #Pooled variance

means <- as.vector(tapply(df$DPSVI, df$class, mean))
#group by color, mean values of alcohol
df$grouping <- as.factor(df$class) #Need to be factor if already not
names(means) <- levels(df$class)
means

k <- g + 1 # = 3 (g=2 groups and one comparison for variance)
k <- g*(g-1)/2 #If not doing variance


#quantile for bonferroni mean
cf.t <- qt(alpha/(2*k), DF) #n1+n2-2 = fit.aov$DF
ng <- table(df[,38]) # column of group
treat <- levels(df[,38]) #Column of group

#Mean differences between groups
paste(treat[1],'-',treat[2])
as.numeric(c(means[1]-means[2] - cf.t * sqrt( Sp * ( 1/ng[1] + 1/ng[2] )),
             means[1]-means[2] + cf.t * sqrt( Sp * ( 1/ng[1] + 1/ng[2] ))))
paste(treat[1],'-',treat[3])
as.numeric(c(means[1]-means[3] - cf.t * sqrt( Sp * ( 1/ng[1] + 1/ng[3] )),
             means[1]-means[3] + cf.t * sqrt( Sp * ( 1/ng[1] + 1/ng[3] ))))

paste(treat[2],'-',treat[3])
as.numeric(c(means[2]-means[3] - cf.t * sqrt( Sp * ( 1/ng[2] + 1/ng[3] )),
             means[2]-means[3] + cf.t * sqrt( Sp * ( 1/ng[2] + 1/ng[3] ))))



attach(df)
Mediag <- tapply(DPSVI, class, mean) 
S <- Sp
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
pdf("skillAnova.pdf", height = 15, width = 15)
par(mfrow=c(1,2))
plot(class, DPSVI, xlab='', ylab='DPSVI', col = rainbow(g), las=2, main="Boxplots of the 3 skillgroups", xaxt='n')

h <- 1
plot(c(1,g*(g-1)/2),range(ICrange), pch='',
     xlab='', ylab='', main = "Confidence intervals 95%", xaxt='n')
for(i in 1:(g-1)) {
  for(j in (i+1):g) {
    ind <- (i-1)*g-i*(i-1)/2+(j-i)
    lines (c(h,h), c(ICrange[ind,1],ICrange[ind,2]), col='grey55'); 
    points(h, Mediag[i]-Mediag[j], pch=16, col='grey55'); 
    points(h, ICrange[ind,1], col=rainbow(g)[j], pch=16); 
    points(h, ICrange[ind,2], col=rainbow(g)[i], pch=16); 
    h <- h+1
  }}
abline(h=0)
dev.off()
