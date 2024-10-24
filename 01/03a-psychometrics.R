#first stage psychometrics

library(psych)
library(lavaan)

# Function for reliability with rounded values
reliability <- function(vars) {
  suppressWarnings(
    round(c(
      alpha = psych::alpha(adf[,vars], check.keys=TRUE)$total$raw_alpha,
      omega = psych::omega(adf[,vars], plot=FALSE)$omega.tot
    ), 2)
  )
}

# Create reverse coded variables
adf <- adf %>%
  mutate(
    know3_r = 6 - know3,
    know4_r = 6 - know4,
    skep3_r = 6 - skep3,  # Added: reverse code overconfidence items
    skep4_r = 6 - skep4,  # Added: reverse code overconfidence items
    edfu3_r = 6 - edfu3,
    edfu4_r = 6 - edfu4,
    ethi3_r = 6 - ethi3,
    ethi4_r = 6 - ethi4
  )


# Analysis for each scale (reliability + one factor cfas):

# 1. Frequency of use
reliability(c("fuse1","fuse2","fuse3","fuse4","fuse5","fuse6","fuse7"))
mod <- 'fuse =~ fuse1 + fuse2 + fuse3 + fuse4 + fuse5 + fuse6 + fuse7'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
modindices(fit)[modindices(fit)$mi > 4, ][order(-modindices(fit)[modindices(fit)$mi > 4, ]$mi), ]
mod <- 'fuse =~ fuse1 + fuse2 + fuse3 + fuse4 + fuse5 + fuse6'#+ fuse7
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
modindices(fit)[modindices(fit)$mi > 4, ][order(-modindices(fit)[modindices(fit)$mi > 4, ]$mi), ]
mod <- 'fuse =~ fuse2 + fuse3 + fuse4 + fuse5 + fuse6'#+ fuse7 + fuse1
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]


# 2. Personal use
reliability(c("puse1","puse2","puse3","puse4"))
mod <- 'puse =~ puse1 + puse2 + puse3 + puse4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
modindices(fit)[modindices(fit)$mi > 4, ][order(-modindices(fit)[modindices(fit)$mi > 4, ]$mi), ]

# 3. Others' use
reliability(c("ouse1","ouse2","ouse3","ouse4"))
mod <- 'ouse =~ ouse1 + ouse2 + ouse3 + ouse4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 4. Personal benefits
reliability(c("pben1","pben2","pben3","pben4"))
mod <- 'pben =~ pben1 + pben2 + pben3 + pben4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 5. Personal risks
reliability(c("pris1","pris2","pris3","pris4"))
mod <- 'pris =~ pris1 + pris2 + pris3 + pris4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 6. Societal benefits
reliability(c("sben1","sben2","sben3","sben4"))
mod <- 'sben =~ sben1 + sben2 + sben3 + sben4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 7. Societal risks
reliability(c("sris1","sris2","sris3","sris4"))
mod <- 'sris =~ sris1 + sris2 + sris3 + sris4'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 8. Knowledge
reliability(c("know1","know2","know3_r","know4_r"))
mod <- 'know =~ know1 + know2 + know3_r + know4_r'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 9. Skepticism (modified to use reversed items)
reliability(c("skep1","skep2","skep3_r","skep4_r"))
mod <- 'skep =~ skep1 + skep2 + skep3_r + skep4_r'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 10. Education future
reliability(c("edfu1","edfu2","edfu3_r","edfu4_r"))
mod <- 'edfu =~ edfu1 + edfu2 + edfu3_r + edfu4_r'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]

# 11. Ethics
reliability(c("ethi1","ethi2","ethi3_r","ethi4_r"))
mod <- 'ethi =~ ethi1 + ethi2 + ethi3_r + ethi4_r'
fit <- cfa(mod, data = adf, ordered=TRUE)
fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea","srmr"))
standardizedsolution(fit)[standardizedsolution(fit)$op=='=~',c(1:4,7)]
