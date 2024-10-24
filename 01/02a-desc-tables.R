#tables for univariate analysis

# Helper function to get stats for a set of variables
get_stats <- function(vars) {
  stats <- cbind(
    psych::describe(adf[vars]), 
    t(sapply(adf[vars], function(x) 
      table(factor(x, levels = 0:5, exclude = NULL))))
  )[,c(2,3,11,12,14:19)]
  return(stats)
}

# Characterization of sample
cat("\nCharacterization of sample:\n");get_stats(c("prol","pdep","psex","page"))[,1:6]
# Frequency of use
cat("\nFrequency of use vars:\n");get_stats(c("fuse1","fuse2","fuse3","fuse4","fuse5","fuse6","fuse7"))[,1:9]
# Personal use
cat("\nPersonal use vars:\n");get_stats(c("puse1","puse2","puse3","puse4"))[c(1:4,6:10)]
# Others' use
cat("\nOthers' use vars:\n");get_stats(c("ouse1","ouse2","ouse3","ouse4"))[c(1:4,6:10)]
# Personal benefits
cat("\nPersonal benefits vars:\n");get_stats(c("pben1","pben2","pben3","pben4"))[c(1:4,6:10)]
# Personal risks
cat("\nPersonal risks vars:\n");get_stats(c("pris1","pris2","pris3","pris4"))[c(1:4,6:10)]
# Societal benefits
cat("\nSocietal benefits vars:\n");get_stats(c("sben1","sben2","sben3","sben4"))[c(1:4,6:10)]
# Societal risks
cat("\nSocietal risks vars:\n");get_stats(c("sris1","sris2","sris3","sris4"))[c(1:4,6:10)]
# Knowledge 
cat("\nKnowledge vars:\n");get_stats(c("know1","know2","know3","know4"))[c(1:4,6:10)]
# Skepticism
cat("\nSkepticism vars:\n");get_stats(c("skep1","skep2","skep3","skep4"))[c(1:4,6:10)]
# Education future
cat("\nEducation future vars:\n");get_stats(c("edfu1","edfu2","edfu3","edfu4"))[c(1:4,6:10)]
# Ethics
cat("\nEthics vars:\n");get_stats(c("ethi1","ethi2","ethi3","ethi4"))[c(1:4,6:10)]