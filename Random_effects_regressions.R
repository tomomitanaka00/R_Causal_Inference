# Example 14.4 in Woodridge "Introductory Econometrics: A Modern Approach"

library(plm);library(stargazer)
data(wagepan, package='wooldridge')

# Generate pdata.frame:
wagepan.p <- pdata.frame(wagepan, index=c("nr","year") )

pdim(wagepan.p)

# Check variation of variables within individuals
pvar(wagepan.p)

# Estimate different models
wagepan.p$yr<-factor(wagepan.p$year)

reg.ols<- (plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union+yr, 
               data=wagepan.p, model="pooling") )
reg.re <- (plm(lwage~educ+black+hisp+exper+I(exper^2)+married+union+yr, 
               data=wagepan.p, model="random") )
reg.fe <- (plm(lwage~                      I(exper^2)+married+union+yr, 
               data=wagepan.p, model="within") )

# Pretty table of selected results (not reporting year dummies)
stargazer(reg.ols,reg.re,reg.fe, type="text", 
          column.labels=c("OLS","RE","FE"),keep.stat=c("n","rsq"),
          keep=c("ed","bl","hi","exp","mar","un"))