F0.001 <- reshape2::melt(niche_index$F0.001[,c(1,2,4,6)], id = "biom")
F0.001$F <- "F0.001"


tiff("JBI-21-0586_Fig1.tiff",width= 7, height = 17, units = "in", res = 600, compression = "lzw")
#pdf("JBI-21-0586_Fig1.pdf", width = 7, height = 17)
par(mfrow=c(3,1), mar=c(2.5,4.5,0,.7), oma=c(7,5,1,1))


### plot 1 (D)
boxplot(value ~ biom, data = F0.001[which(F0.001$variable == "D"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#FF6B6B",
        ylim = c(0,1))
axis(2, at = seq(0,1,0.2), las = 1, cex.axis = 2.1)
mtext("Niche overlap", side = 2, line = 5, outer = F, xpd = NA, cex = 1.5)
text(0.6,0.98, substitute(paste(bold("(a)"))), cex = 2.1)

### plot 2 (expansion)
boxplot(value ~ biom, data = F0.001[which(F0.001$variable == "expansion"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#FFE66D",
        ylim = c(0,1))
axis(2, at = seq(0,1,0.2), las = 1, cex.axis = 2.1)
mtext("Niche expansion", side = 2, line = 5, outer = F, xpd = NA, cex = 1.5)
text(0.6,0.98, substitute(paste(bold("(b)"))), cex = 2.1)

### plot 3 (unfilling)
boxplot(value ~ biom, data = F0.001[which(F0.001$variable == "unfilling"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#4ECDC4",
        ylim = c(0,1))
axis(2, at = seq(0,1,0.2), las = 1, cex.axis = 2.1)
mtext("Niche unfilling", side = 2, line = 5, outer = F, xpd = NA, cex = 1.5)
axis(1, at = seq(1,4,1), labels = F, cex.axis = 2.1)
text(x = c(1:4) + 0.17, y = par("usr")[3]  - 0.08, 
     labels = c("Boreal f.", "Mountain f.", "Coastal Plain f.", "Deciduous f."),
     xpd = NA, srt = 35, adj = 0.85, cex = 2.1)
text(0.6,0.98, substitute(paste(bold("(c)"))), cex = 2.1)

dev.off()



