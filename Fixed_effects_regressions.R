# Example 14.2 in Woodridge "Introductory Econometrics: A Modern Approach"

library(plm)
data(wagepan, package='wooldridge')

# Generate pdata.frame:
wagepan.p <- pdata.frame(wagepan, index=c("nr","year") )

pdim(wagepan.p)

# Estimate FE model
summary( plm(lwage~married+union+factor(year)*educ, 
             data=wagepan.p, model="within") )
