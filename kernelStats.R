# sets the directory I operate out of to the following:
setwd("D:/caltech/proj/kernelStats/")

# my text file that I read in
dTable <- read.table("resultsVerFinal.txt", header = FALSE, sep = ";",
  colClasses=c('character','numeric','numeric','numeric', 'character',
  'character', 'numeric'))

# relevant data are the version numbers, number of iterations, the average size
# of each file, and the number of days between the first and last release
# was going to use the dates, but decided to save them for later processing
ver <- dTable[1:42,1]
iter <- dTable[1:42,2]
aSize <- dTable[1:42,4]/1000000
days <- dTable[1:42,7]

# series of saving bar charts and scatter plots to my computer
bChart1=barplot(iter, names = ver, xlab='Version',ylab='Number of Iterations',
  ylim=c(0,250))
box()
dev.copy(png, file = 'verVsIter.png', width = 1680)
dev.off()

bChart2=barplot(aSize, names = ver, xlab='Version',ylab='Avg Size (MB)',
  ylim=c(0,150))
box()
dev.copy(png, file = 'verVsAvgSize.png', width = 1680)
dev.off()

bChart3=barplot(days, names = ver, xlab='Version',
  ylab='Days Between First and Last Release', ylim=c(0,3000))
box()
dev.copy(png, file = 'verVsDays.png', width = 1680)
dev.off()

coef=lm(days ~ iter)$coef
adjRSquare = summary(lm(days ~ iter))[9]
pChart1=plot(iter, days, xlab='Number of Iterations',
  ylab='Days Between First and Last Release')
abline(lm(days ~ iter))
text(50, 2000, paste("y =", round(coef[2],3), "x", round(coef[1],3)), cex=1)
text(50, 1900, substitute( paste (bar(R)^2, "=", m ),
  list(m=round(as.numeric(adjRSquare), 4) ) ), pos=1, cex=1)
dev.copy(png, file = 'iterVsDays.png')
dev.off()

resid4=residuals(lm(days ~ iter))
plot(iter,resid4,xlab='Number of Iterations', ylab='Residuals')
dev.copy(png, file = 'iterResiduals.png')
dev.off()

pChart2=plot(   days, aSize,
  xlab='Days Between First and Last Release',ylab='Avg Size (Mb)',
  col=ifelse(  grepl("^1",ver,perl=TRUE), "red",
      ifelse( grepl("^2",ver,perl=TRUE), "blue", 
      ifelse( grepl("^3",ver,perl=TRUE), "gray", "black") )  )   , pch=19)

legend(2500, 35, pch=c(19,19), col=c("red", "blue", "gray", "black"),
  c("1.x", "2.x", "3.x", "4.x"), bty="o",  box.col="darkgreen", cex=1,
  title="Version")
dev.copy(png, file = 'daysVsAvgSize.png')
dev.off()


pChart3=plot(iter, days, xlab='Number of Iterations',
  ylab='Days Between First and Last Release',
  col=ifelse(  grepl("^1",ver,perl=TRUE), "red",
      ifelse( grepl("^2",ver,perl=TRUE), "blue", 
      ifelse( grepl("^3",ver,perl=TRUE), "gray", "black") )  )   , pch=19)

legend(25, 2500, pch=c(19,19), col=c("red", "blue", "gray", "black"),
  c("1.x", "2.x", "3.x", "4.x"), bty="o",  box.col="darkgreen", cex=1,
  title="Version")

dev.copy(png, file = 'iterVsDaysVer.png')
dev.off()

pChart4=plot(   iter, aSize, xlab='Number of Iterations',ylab='Avg Size (Mb)',
  col=ifelse(  grepl("^1",ver,perl=TRUE), "red",
      ifelse( grepl("^2",ver,perl=TRUE), "blue", 
      ifelse( grepl("^3",ver,perl=TRUE), "gray", "black") )  )   , pch=19)

legend(200, 40, pch=c(19,19), col=c("red", "blue", "gray", "black"),
  c("1.x", "2.x", "3.x", "4.x"), bty="o",  box.col="darkgreen", cex=1,
  title="Version")
dev.copy(png, file = 'iterVsAvgSize.png')
dev.off()

