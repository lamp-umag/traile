#correlations
library(GGally)
# Create correlation plot with distributions and scatter plots
ggpairs(tdf,
        upper = list(continuous = wrap("cor", size = 3)), 
        diag = list(continuous = wrap("barDiag", bins = 30)), 
        lower = list(continuous = wrap("smooth", alpha = 0.3, size = 0.8)),
        title = "Correlations and Distributions of AI Perception Scales")

# simpler:
library(corrplot)
corrplot(cor(tdf), 
         method = "color",
         type = "upper",
         addCoef.col = "black",
         number.cex = 0.7,
         tl.col = "black",
         tl.srt = 45,
         diag = FALSE)




#now ega with composite scores


library(EGAnet)
library(dplyr)

# Create tdf as before
tdf <- adf %>%
  select(
    FrecuencyUse=cfuse,
    OwnUse=cpuse,
    OthersUse=couse,
    PeronalBenefit=cpben,
    PersonalRisk=cpris,
    SocietalBenefit=csben,
    SocietalRisk=csris,
    Knowldge=cknow,
    Skepticism=cskep,
    SupportEducationUse=cedfu,
    Ethical=cethi
    ) %>%
  na.omit()



ega_results <- EGA(data=tdf, 
                   plot.EGA=TRUE,
                   model="TMFG",
                   algorithm="walktrap")


