# Function for composite scores
composite <- function(vars) {
  rowMeans(adf[,vars], na.rm = TRUE)
}
adf$cfuse <- composite(c("fuse1","fuse2","fuse3","fuse4","fuse5","fuse6","fuse7"))
adf$cpuse <- composite(c("puse1","puse2","puse3","puse4"))
adf$couse <- composite(c("ouse1","ouse2","ouse3","ouse4"))
adf$cpben <- composite(c("pben1","pben2","pben3","pben4"))
adf$cpris <- composite(c("pris1","pris2","pris3","pris4"))
adf$csben <- composite(c("sben1","sben2","sben3","sben4"))
adf$csris <- composite(c("sris1","sris2","sris3","sris4"))
adf$cknow <- composite(c("know1","know2","know3_r","know4_r"))
adf$cskep <- composite(c("skep1","skep2","skep3_r","skep4_r"))
adf$cedfu <- composite(c("edfu1","edfu2","edfu3_r","edfu4_r"))
adf$cethi <- composite(c("ethi1","ethi2","ethi3_r","ethi4_r"))