# ICU
library(tidyverse)
library(ggthemes)
icu <- read_tsv("content/post/data/ICU/ICU.txt")
fit <- glm(STA ~ AGE, data = icu, family = binomial(link = "logit"))
summary(fit)
p <- ggplot(icu, aes(AGE,STA))
p + geom_point() + theme_tufte() + theme(text=element_text(size=16, family="Fira Sans"))
plot(fit)

# Exercise 1(c)
group_icu <- icu %>%  group_by(cohort)
mean_breaks <- c(15,24,34,44,54,64,74,84,94)
mean_icu <- icu %>%  group_by(cohort) %>% summarize(mean(AGE)) %>% ungroup()
median_icu <- icu %>% group_by(cohort) %>% summarize(median(AGE)) %>% ungroup()
try1 <- icu %>% group_by(cohort, mean_icu) %>% ungroup()
try1
