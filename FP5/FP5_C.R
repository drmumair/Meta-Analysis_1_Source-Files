
library(readxl)
FP5 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP5", range = "A1:S13")

FP5IR <- escalc(measure = "IR", xi = FP5$xi, ti = FP5$ti, data = FP5, replace = FALSE)

FP5RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP5IR)

FP5RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FP5IR)


###########

pdf(file = "FP5.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP5RMA, slab = paste(FP5$Author, FP5$`Pub Year`), ilab = cbind(FP5$ilab2, FP5$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.67, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(21:13, 7:5), ylim = c(-3, 103), cex.lab = 0.75, top = 81)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP5RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP5RMA$k - FP5RMA$p), ", ", "Q = ", .(formatC(FP5RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP5RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP5RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP5RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FP5RMA1$tau2, digits=4, format="f")),", df = ", .(FP5RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FP5RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FP5RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP5RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP5RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(22, 8), pos = 4, c("Carbapenem Resistant Isolates", "Naive Isolates"), font = 2, cex = 0.8)

text(-1.6, c( 25.25 ), pos = 4, c("Metallo-β-Lactamase Production - DDST/CDDT"), font = 4, cex = 0.9)


text(-1.6, c(24), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(25), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(24), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(24), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(24), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP5IR, subset = (SubGroup=="Resistant")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP5IR, subset = (SubGroup=="Susceptible")) 


addpoly(RMASG1, row = 10.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(11.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(10.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(9.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)


text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                "%)")), pos=4, cex=0.65)


dev.off()

#####################################

pdf(file = "FP5inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FP5RMA)

plot(inf, layout=c(8,1))

dev.off()

#####################################

pdf(file = "FP5rad.pdf", family = "Times", height = 11, width = 7.5)

par(mar = c(5,5,3,3))

radial(FP5RMA)

dev.off()
