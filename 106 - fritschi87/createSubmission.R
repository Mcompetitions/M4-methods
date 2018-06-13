files <- sort(dir("path/to/all/the/forecast/files",full.names=T))

d <- read.csv(files[fileIndex],header = T)

m <- matrix(rep(NA,49*length(files)),ncol=49)

for(i in 1:length(files)) {
  #i = 1
  f <- read.csv(files[i],header = T)
  f <- unname(as.matrix(f))
  f <- c(f,rep(NA,49-length(f)))
  m[i,] <- f
}


colnames(m) <- c("id",paste("F",1:48,sep=""))

write.csv(m,"submission.csv",quote = F,row.names = F)
