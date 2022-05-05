tab_boxploty_modely <- reshape2::melt(area_reclass[,c(1,2,3,4,6)], id = "biom")


#tiff("JBI-21-0586_Fig2.tiff",width= 10, height = 8, units = "in", res = 600, compression = "lzw")
pdf("JBI-21-0586_Fig2.pdf", width = 10, height = 8)

par(mfrow=c(2,2), mar=c(2.5,6,0,.7), oma=c(7,5,1,1))

### plot 1 (NAT)
boxplot(value ~ biom, data = tab_boxploty_modely[which(tab_boxploty_modely$variable == "NAT"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#FF6B6B",
        ylim = c(0,4797400))
axis(2, las=1, cex.axis = 1.5, cex.lab = 1.5, seq(0,4000000,1000000), labels = c(0,1,2,3,4))
mtext("Potential range size", side = 2, line = 4, outer = F, xpd = NA, cex = 1.2)
mtext("[million sq. km]", side = 2, line = 2.5, outer = F, xpd = NA, cex = 1.2)
text(0.62,4600000, substitute(paste(bold("(a)"))), cex = 1.5)

### plot 2 (NAT+INV)
boxplot(value ~ biom, data = tab_boxploty_modely[which(tab_boxploty_modely$variable == "NATINV"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#FFE66D",
        ylim = c(0,4797400))
axis(2, las=1, cex.axis = 1.5, cex.lab = 1.5, seq(0,4000000,1000000), labels = c(0,1,2,3,4))
mtext("Potential range size", side = 2, line = 4, outer = F, xpd = NA, cex = 1.2)
mtext("[million sq. km]", side = 2, line = 2.5, outer = F, xpd = NA, cex = 1.2)
text(0.62,4600000, substitute(paste(bold("(b)"))), cex = 1.5)

### plot 3 (abs geo exp)
boxplot(value ~ biom, data = tab_boxploty_modely[which(tab_boxploty_modely$variable == "geo_exp"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#4ECDC4",
        ylim = c(0,4797400))
axis(2, las=1, cex.axis = 1.5, cex.lab = 1.5, seq(0,4000000,1000000), labels = c(0,1,2,3,4))
mtext("Geographical expansion", side = 2, line = 4, outer = F, xpd = NA, cex = 1.2)
mtext("[million sq. km]", side = 2, line = 2.5, outer = F, xpd = NA, cex = 1.2)
axis(1, at = seq(1,4,1), labels = F, cex.axis = 1.5)
text(x = c(1:4) + 0.35, y = par("usr")[3] - 200000, 
     labels = c("Boreal f.", "Mountain f.", "Coastal Plain f.", "Deciduous f."),
     xpd = NA, srt = 35, adj = 1.1, cex = 1.5)
text(0.62,4600000, substitute(paste(bold("(c)"))), cex = 1.5)

### plot 4 (rel geo exp)
boxplot(value ~ biom, data = tab_boxploty_modely[which(tab_boxploty_modely$variable == "geo_exp_rel"),],
        xaxt = "n", yaxt = "n", xlab = "", ylab = "", col = "#28818F",
        ylim = c(0,1))
axis(2, las=1, cex.axis = 1.5, cex.lab = 1.5, seq(0,1,0.2), labels = seq(0,100,20))
mtext("Geographical expansion", side=2, line=4, las=0, cex = 1.2)
mtext("[%]", side=2, line=2.5, las=0, cex = 1.2)
axis(1, at = seq(1,4,1), labels = F, cex.axis = 1.5)
text(x = c(1:4) + 0.35, y = par("usr")[3]  - 0.05, 
     labels = c("Boreal f.", "Mountain f.", "Coastal Plain f.", "Deciduous f."),
     xpd = NA, srt = 35, adj = 1.1, cex = 1.5)
text(0.62,0.98, substitute(paste(bold("(d)"))), cex = 1.5)

dev.off()



