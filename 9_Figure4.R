plot.data <- niche_index$F0.001
head(plot.data)
colr <- palette(c("#0D2B30", "#FF3333", "#4ECDC4", "#FFE66D"))


#tiff("JBI-21-0586_Fig4.tiff",width= 9, height = 7, units = "in", res = 600, compression = "lzw")
pdf("JBI-21-0586_Fig4.pdf", width = 9, height = 7)
par(mfrow=c(2,2), mar=c(.7,.7,.7,.7), oma=c(5,5,4,1))

##plot1---------------------------------------
x <- plot.data$mrt
y <- sqrt(plot.data$expansion)
g <- as.factor(plot.data$biom)

# m <- lm(y ~ poly(x,2))
m <- lm(y ~ x)
summary(m)

pr <- predict(m , interval="confidence", level = 0.95)
ix <- sort(x,index.return=T)$ix

plot(x, y, axes=F, type="n",  xlab="", ylab="", ylim = c(0,1.16))
# abline(h=seq(0,1,0.2), v=log(seq(150,400,50)), lty="dotted", col="gray80")
abline(h=sqrt(seq(0,1,0.2)), v=seq(150,400,50), lty="dotted", col="gray80")
polygon(c(rev(x[ix]), x[ix]), c(rev(pr[ ix,3]), pr[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
points(x, y, pch=21, 
       bg=colr[spe_info$id_biom], 
       cex=1.6)
# axis(2, las=1)
axis(2, at=sqrt(seq(0,1,0.2)), labels=seq(0,1,0.2), las=1, cex.axis=1.2)
mtext("Niche expansion", side=2, line=3.5,
      outer=F, xpd=NA, cex=1.2)

lines(x[ix], pr[ix , 1], col="black", lwd=1.6 )

r <- cor(x, y, method="spearman")
p <- cor.test(x,y, method="spearman", exact = F)$p.value
p
text(155,sqrt(1.23), substitute(paste(bold("(a)"))), cex = 1.2)

box()

#plot2------------------------------------
x <- log(plot.data$area/1000000)
y <- sqrt(plot.data$expansion)
g <- as.factor(plot.data$biom)

m <- lm(y ~ x)
summary(m)

pr <- predict(m , interval="confidence", level = 0.95)
ix <- sort(x,index.return=T)$ix

plot(x, y, axes=F, type="n",  xlab="", ylab="", ylim = c(0,1.16))
abline(h=sqrt(seq(0,1,0.2)), v=log(c(0.25, 0.5, 1, 2, 3, 4)), lty="dotted", col="gray80")
polygon(c(rev(x[ix]), x[ix]), c(rev(pr[ ix,3]), pr[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
points(x, y, pch=21, 
       bg=colr[spe_info$id_biom], 
       cex=1.6)
# axis(2, las=1)
# axis(1, at=log(c(0.25, 0.5, 1, 2, 3, 4, 5, 6)), labels=c(0.25, 0.5, 1, 2, 3, 4, 5, 6))

lines(x[ix], pr[ix , 1], col="black", lwd=1.6 )

r <- cor(x, y, method="spearman")
p <- cor.test(x,y, method="spearman", exact = F)$p.value
p

text(log(0.15),sqrt(1.23), substitute(paste(bold("(b)"))), cex = 1.2)

box()

#plot3-------------------------------------------------
x <- plot.data$mrt
y <- plot.data$relgeoexp*100
g <- as.factor(plot.data$biom)

# m <- lm(y ~ poly(x,2))
m <- lm(y ~ x)
summary(m)

pr <- predict(m , interval="confidence", level = 0.95)
ix <- sort(x,index.return=T)$ix

plot(x, y, axes=F, type="n",  xlab="", ylab="", ylim = c(15,112))
# abline(h=seq(0.2,0.5,0.1), v=log(seq(150,400,50)), lty="dotted", col="gray80")
abline(h=seq(0,100,20), v=seq(150,400,50), lty="dotted", col="gray80")
polygon(c(rev(x[ix]), x[ix]), c(rev(pr[ ix,3]), pr[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
points(x, y, pch=21, 
       bg=colr[spe_info$id_biom], 
       cex=1.6)
axis(2, las=1, cex.axis=1.2)
# axis(1, at=log(seq(150,400,50)), labels=seq(150,400,50))
axis(1, cex.axis=1.2)
mtext("Residence time [years]", side=1, line=3,
      outer=F, xpd=NA, cex=1.2)
mtext("Geographical expansion [%]", side=2, line=3.5,
      outer=F, xpd=NA, cex=1.2)

lines(x[ix], pr[ix , 1], col="black", lwd=1.6 )

r <- cor(x, y, method="spearman")
p <- cor.test(x,y, method="spearman", exact = F)$p.value
p
text(155,108, substitute(paste(bold("(c)"))), cex = 1.2)

box()

#plot4----------------------------------------------------
x <- log(plot.data$area/1000000)
y <- plot.data$relgeoexp*100
g <- as.factor(plot.data$biom)

# m <- lm(y ~ poly(x,2))
m <- lm(y ~ x)
summary(m)

pr <- predict(m , interval="confidence", level = 0.95)
ix <- sort(x,index.return=T)$ix

plot(x, y, axes=F, type="n",  xlab="", ylab="", ylim = c(15,112))
abline(h=seq(0,100,20), v=log(c(0.25, 0.5, 1, 2, 3, 4)), lty="dotted", col="gray80")
polygon(c(rev(x[ix]), x[ix]), c(rev(pr[ ix,3]), pr[ ix,2]), col = rgb(0.7,0.7,0.7,0.4) , border = NA)
points(x, y, pch=21, 
       bg=colr[spe_info$id_biom], 
       cex=1.6)
# axis(2, las=1)
axis(1, at=log(c(0.25, 0.5, 1, 2, 3, 4)), labels=c(0.25, 0.5, 1, 2, 3, 4), cex.axis=1.2)
mtext("Native range size [million sq. km]", side=1, line=3.5,
      outer=F, xpd=NA, cex=1.2)

lines(x[ix], pr[ix , 1], col="black", lwd=1.6)

r <- cor(x, y, method="spearman")
p <- cor.test(x,y, method="spearman", exact = F)$p.value
p
text(log(0.17),108, substitute(paste(bold("(d)"))), cex = 1.2)

box()

par(fig = c(0, 1, 0, 1), oma = c(0, 5, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("top", pch=21, cex=1.2, pt.bg=c("#0D2B30", "#FF3333", "#FFE66D", "#4ECDC4"), legend = c("Boreal forests", "Mountain forests", "Coastal plain forests", "Deciduous forests"),
       horiz = T, pt.cex = 1.5, bty="n")

dev.off()
