# Section 15.6 in Woodridge "Introductory Econometrics: A Modern Approach"

library(AER)
data(mroz, package='wooldridge')

# restrict to non-missing wage observations
oursample <- subset(mroz, !is.na(wage))

# IV regression
summary( res.2sls <- ivreg(log(wage) ~ educ+exper+I(exper^2)
                           | exper+I(exper^2)+motheduc+fatheduc,data=oursample) )

# Auxiliary regression
res.aux <-  lm(resid(res.2sls) ~ exper+I(exper^2)+motheduc+fatheduc
               , data=oursample) 

# Calculations for test
( r2 <- summary(res.aux)$r.squared )
( n <- nobs(res.aux) )
( teststat <- n*r2 )
( pval <- 1-pchisq(teststat,1) )



library(plm)
data(jtrain, package='wooldridge')

# Define panel data (for 1987 and 1988 only)
jtrain.87.88 <- subset(jtrain,year<=1988)
jtrain.p<-pdata.frame(jtrain.87.88, index=c("fcode","year"))

# IV FD regression
summary( plm(log(scrap)~hrsemp|grant, model="fd",data=jtrain.p) )
