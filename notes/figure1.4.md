
# vertical lines same as text at 19
ci <- confint(chdfit, level = 0.95)
ci_lower <- ci[2,1]
ci_upper <- ci[2,2] 

# horizontal line
# 'log Lik.' -53.67655 (df=2)  text has transposition error -53.6756
                            # should be -53.676546, see page 10
qchisq(.95, df=1) # = 3.8416  differs in 4th place 
logLik(chdfit) - (nin5/2)
# -55.59728 (df=2) # text has -55.5964

#Asymmetry

asymmetry <- function(x) {
	ci <- confint(x, level = 0.95)
	ci_lower <- ci[2,1]
	ci_upper <- ci[2,2]
	coeff <- x$coefficients[2]
	round(100 * ((ci_upper - coeff) - (coeff - ci_lower))/(ci_upper - ci_lower),1)
}
chdprof <- profilelike.glm(CHD ~ AGE, data=chdage_df, family=binomial(link="logit"), length=100, round=2)

profilelike.plot(theta=xx$theta, profile.lik.norm=xx$profile.lik.norm, round=2)

# What is the $\theta$ numerical grid?

It's a vector of log maximum likelihoods taken at different coefficients?

