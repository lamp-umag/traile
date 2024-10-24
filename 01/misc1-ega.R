library(EGAnet)
library(qgraph)
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

# Run EGA 
ega_results <- EGA(data=tdf, plot.EGA=FALSE)

# Conceptually-driven colors
my_colors <- c(
  "#4682B4",  # Steel blue for personal use (neutral/practical)
  "#87CEEB",  # Sky blue for others' use (similar but lighter)
  "#90EE90",  # Light green for personal benefits (positive)
  "#CD5C5C",  # Indian red for personal risks (warning)
  "#98FB98",  # Pale green for societal benefits (positive but broader)
  "#DC143C",  # Crimson for societal risks (severe warning)
  "#FFD700",  # Gold for knowledge (valuable/bright)
  "#808080",  # Gray for skepticism (doubt/uncertainty)
  "#6A5ACD",  # Slate blue for education (institutional)
  "#BA55D3"   # Medium orchid for ethics (complexity/morality)
)

# Legend labels
legend_labels <- c("Personal Use", "Others' Use", 
                   "Personal Benefits", "Personal Risks",
                   "Societal Benefits", "Societal Risks",
                   "Knowledge", "Skepticism",
                   "Education", "Ethics")

# Enhanced qgraph plot with legend
qgraph(ega_results$network,
       layout="spring",
       groups=factor(rep(1:10, each=4)),
       color=my_colors,
       vsize=6,
       edge.width=0.8,
       labels=colnames(tdf),
       label.scale=FALSE,
       label.cex=0.7,
       #edge.color="gray80",
       borders=FALSE,
       maximum=0.5,
       legend=TRUE,
       legend.cex=0.6,
       GLratio=2.5,
       groups.names=legend_labels)