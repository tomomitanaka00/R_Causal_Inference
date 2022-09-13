# Example 13.2 in Woodridge "Introductory Econometrics: A Modern Approach"

data(kielmc, package='wooldridge')

# Separate regressions for 1978 and 1981: report coeeficients only
coef( lm(rprice~nearinc, data=kielmc, subset=(year==1978)) )
coef( lm(rprice~nearinc, data=kielmc, subset=(year==1981)) )

# Joint regression including an interaction term 
library(lmtest)
coeftest( lm(rprice~nearinc*y81, data=kielmc) )

DiD      <- lm(log(rprice)~nearinc*y81                     , data=kielmc)
DiDcontr <- lm(log(rprice)~nearinc*y81+age+I(age^2)+log(intst)+
                 log(land)+log(area)+rooms+baths, data=kielmc)
library(stargazer)
stargazer(DiD,DiDcontr,type="text")