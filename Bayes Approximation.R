library(MCMCpack)
library(mcmcplots)
lowbw	<- read.table('lowbw.txt', header = TRUE)
head(lowbw)
non_smoke<-lowbw[lowbw['smoke']==0,]
smoke<-lowbw[lowbw['smoke']==1,]
#non-smoker
y		<- non_smoke$bwt
ybar	<- mean(y)
s2		<- var(y)
n		<- length(y)
B		<- 20000

set.seed(11155)

rmu		<- ybar + (sqrt(s2/n))*rt(B, df = n-1)
rsig2	<- rinvgamma(B, (n-1)/2, ((n-1)/2)*s2)

mean(rmu)
median(rmu)
quantile(rmu, probs = c(0.025, 0.975))

mean(rsig2)
median(rsig2)
quantile(rsig2, probs = c(0.025, 0.975))


rparams	<- cbind(rmu, rsig2)
colnames(rparams)	<- c('mu', 'sigma^2')

par(mfrow = c(1,2))
plot(density(rmu), xlab = expression(mu), col = 'blue', lwd = 2)
plot(density(rsig2), xlab = expression(sigma^2), col = 'blue', lwd = 2)

#Smoker
set.seed(11155)
y		<- smoke$bwt
ybar	<- mean(y)
s2		<- var(y)
n		<- length(y)
B		<- 20000

set.seed(11155)

rmu		<- ybar + (sqrt(s2/n))*rt(B, df = n-1)
rsig2	<- rinvgamma(B, (n-1)/2, ((n-1)/2)*s2)

mean(rmu)
median(rmu)
quantile(rmu, probs = c(0.025, 0.975))

mean(rsig2)
median(rsig2)
quantile(rsig2, probs = c(0.025, 0.975))


rparams	<- cbind(rmu, rsig2)
colnames(rparams)	<- c('mu', 'sigma^2')

par(mfrow = c(1,2))
plot(density(rmu), xlab = expression(mu), col = 'blue', lwd = 2)
plot(density(rsig2), xlab = expression(sigma^2), col = 'blue', lwd = 2)

#From the posterior summaries from two groups, we can see that children whose mothers smoke
# during pregnancy appear to have lower weight.
