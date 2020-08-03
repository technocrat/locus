library(dplyr)
library(MASS)
library(profileModel)
library(readr)
#chdage_df <- read_tsv("../data/CHDAGE/CHDAGE.txt")
#chdage_fit <- glm(CHD ~ AGE, data = chdage_df, family = binomial(link="logit"))


raoscore <- profConfint(profileModel(chdage_fit, quantile=qchisq(0.95, 1), objective = "RaoScoreStatistic", X = model.matrix(chdage_fit)))
attr(raoscore, "dimnames")[2][[1]] <- c("2.5 %","97.5 %")
raoscore <- raoscore[,1:2]
waldscore <- confint.default(chdage_fit)
vmscore <- confint(chdage_fit)

width <- function(x){x[[2,1]] - x[[2,2]]}

print("Rao score function test")
raoscore
print("Rao score function test width")
width(raoscore)
print("Wald test")
waldscore
print("Wald test width")
width(waldscore)
print("Venzon-Moolgavkar test")
vmscore
print("Venzon-Moolgavkar test width")
width(vmscore)
