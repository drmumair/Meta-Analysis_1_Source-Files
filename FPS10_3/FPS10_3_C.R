
library(readxl)
FPS10_3 <- read_excel("/Users/Umair/Library/CloudStorage/OneDrive-Nexus365/Carbapenemase Metaanalysis/CMA-R-Projects/FPDataSup2.xlsx", 
                      sheet = "FPS10_3", range = "A1:W39")

FPS10_3IR <- escalc(measure = "IR", xi = FPS10_3$xi, ti = FPS10_3$ti, data = FPS10_3, replace = FALSE)

FPS10_3RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS10_3IR)

FPS10_3RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FPS10_3IR)


###########
pdf(file = "FPS10_3.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FPS10_3RMA, slab = paste(FPS10_3$Author, FPS10_3$`Pub Year`), ilab = cbind(FPS10_3$ilab2, FPS10_3$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.10, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(52:47, 41:39, 33:5), ylim = c(-3, 103), cex.lab = 0.75, top = 50)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FPS10_3RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FPS10_3RMA$k - FPS10_3RMA$p), ", ", "Q = ", .(formatC(FPS10_3RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FPS10_3RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS10_3RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS10_3RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FPS10_3RMA1$tau2, digits=4, format="f")),", df = ", .(FPS10_3RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FPS10_3RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FPS10_3RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS10_3RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS10_3RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(53, 42, 34), pos = 4, c("ESBL Producing", "ACBL Producing", "Naive Isolates"), font = 2, cex = 0.8)

text(-1.6, c( 56.25 ), pos = 4, c("Enterobacteriaceae - Clinical"), font = 4, cex = 0.9)


text(-1.6, c(55), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(56), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(55), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(55), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(55), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS10_3IR, subset = (SubGroup=="ESBL")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS10_3IR, subset = (SubGroup=="ACBL")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS10_3IR, subset = (SubGroup=="Naive")) 


addpoly(RMASG1, row = 44.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 36.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(45.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(44.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(43.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(37.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(36.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(35.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                "%)")), pos=4, cex=0.65)


dev.off()
#####################################

pdf(file = "FPS10_3inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FPS10_3RMA)

plot(inf, layout=c(8,1))

dev.off()
