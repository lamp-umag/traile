library(EGAnet)
library(dplyr)

# Create tdf as before
tdf <- adf %>%
  select(
    puse1, puse2, puse3, puse4,
    ouse1, ouse2, ouse3, ouse4,
    pben1, pben2, pben3, pben4,
    pris1, pris2, pris3, pris4,
    sben1, sben2, sben3, sben4,
    sris1, sris2, sris3, sris4,
    know1, know2, diff3=know3, diff4=know4,
    skep1, skep2, hype3=skep3, hype4=skep4,
    edsu1=edfu1, edsu2=edfu2, edop3=edfu3, edop4=edfu4,
    ethi1, ethi2, unet3=ethi3, unet4=ethi4
  ) %>%
  na.omit()



ega_results <- EGA(data=tdf, 
                   plot.EGA=TRUE,
                   model="TMFG",
                   algorithm="walktrap",)
