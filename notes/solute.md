	library(dplyr)
	library(purrr)
	library(ggplot2)


	get_profile_glm <- function(aglm){
	  prof <- MASS:::profile.glm(aglm, del = .05, maxsteps = 52)
	  disp <- attr(prof,"summary")$dispersion
	  purrr::imap_dfr(prof, .f = ~data.frame(par = .y,
	                  deviance=.x$z^2*disp+aglm$deviance, 
	                  values = as.data.frame(.x$par.vals)[[.y]], stringsAsFactors = FALSE))
	}

	chdage_df <- readr::read_tsv("data/CHDAGE/CHDAGE.txt")
	chdfit <- glm(CHD ~ AGE, data = chdage_df, family = binomial(link = "logit"))


	pll <- get_profile_glm(chdfit) %>% filter(par == "AGE") %>% mutate(beta = values) %>% mutate(pll = deviance * -0.5) %>% select(-c(par,values, deviance))

	asymmetry <- function(x) {
	  ci <- confint(x, level = 0.95)
	  ci_lower <- ci[2,1]
	  ci_upper <- ci[2,2]
	  coeff <- x$coefficients[2]
	  round(100 * ((ci_upper - coeff) - (coeff - ci_lower))/(ci_upper - ci_lower), 2)
	}

	asym <- asymmetry(chdfit)

	ggplot(data = pll, aes(x = beta, y = pll)) + 
	  geom_line() + 
	  scale_x_continuous(breaks = c(0.06, 0.08, 0.10, 0.12, 0.14, 0.16)) + 
	  scale_y_continuous(breaks = c(-57, -56, -55, -54)) + 
	  xlab("Coefficient for age") + 
	  ylab("Profile log-likelihood function") + 
	  geom_vline(xintercept = confint(chdfit)[2,1]) + 
	  geom_vline(xintercept = confint(chdfit)[2,2]) + 
	  geom_hline(yintercept = (logLik(chdfit) - (qchisq(0.95, df = 1)/2))) + 
	  theme_classic() + 
	  ggtitle(paste("Asymmetry =", scales::percent(asym/100, accuracy = 0.1))) + theme(plot.title = element_text(hjust = 0.5))
	") %>% mutate(beta = values) %>% mutate(pll = deviance * -0.5) %>% select(-c(par,values, deviance))
