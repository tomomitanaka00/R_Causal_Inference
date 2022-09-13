# Section 14.3 in Woodridge "Introductory Econometrics: A Modern Approach"
# There are 3 approaches to fixed effects regressions.
# approach 1: within transformation (reg.fe)
# approach 2: dummy variable approach (reg.dum)
# approach 3: CRE (correlated random effects) approach (reg.cre)

library(plm);library(stargazer)
data(wagepan, package='wooldridge')

# Generate pdata.frame:
wagepan.p <- pdata.frame(wagepan, index=c("nr","year") )

# Estimate FE parameter in 3 different ways:
wagepan.p$yr<-factor(wagepan.p$year)
reg.fe <-(plm(lwage~married+union+yr*educ,data=wagepan.p, model="within"))
reg.dum<-( lm(lwage~married+union+yr*educ+factor(nr), data=wagepan.p))
reg.re <-(plm(lwage~married+union+yr*educ,data=wagepan.p, model="random"))
reg.cre<-(plm(lwage~married+union+yr*educ+Between(married)+Between(union)
              ,data=wagepan.p, model="random"))

stargazer(reg.fe,reg.dum,reg.cre,reg.re,type="text",model.names=FALSE,
          keep=c("married","union",":educ"),keep.stat=c("n","rsq"),
          column.labels=c("Within","Dummies","CRE","RE"))
