
library(readxl)
FPS3 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPDataSup2.xlsx", 
                   sheet = "FPS3", range = "A1:V52")

FPS3IR <- escalc(measure = "IR", xi = FPS3$xi, ti = FPS3$ti, data = FPS3, replace = FALSE)

FPS3RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS3IR)

FPS3RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FPS3IR)


###########

pdf(file = "FPS3.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FPS3RMA, slab = paste(FPS3$Author, FPS3$`Pub Year`), ilab = cbind(FPS3$ilab2, FPS3$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.09, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(70:43, 37:29, 23:16, 10:5), ylim = c(-3, 103), cex.lab = 0.75, top = 32)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FPS3RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FPS3RMA$k - FPS3RMA$p), ", ", "Q = ", .(formatC(FPS3RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FPS3RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS3RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS3RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FPS3RMA1$tau2, digits=4, format="f")),", df = ", .(FPS3RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FPS3RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FPS3RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS3RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS3RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(71, 38, 24, 11), pos = 4, c("Punjab", "Sindh", "Khyber Pakhtunkhwa (KPK)", "Islamabad"), font = 2, cex = 0.8)

text(-1.6, c( 74.25 ), pos = 4, c("Enterobacteriaceae"), font = 4, cex = 0.9)

text(-1.6, c(73), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(74), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(73), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(73), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(73), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS3IR, subset = (SubGroup=="SG1")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS3IR, subset = (SubGroup=="SG2")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS3IR, subset = (SubGroup=="SG3")) 
RMASG4 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS3IR, subset = (SubGroup=="SG4")) 


addpoly(RMASG1, row = 40.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 26.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 13.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG4, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(41.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(40.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(39.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(27.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(26.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(25.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(14.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(13.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(12.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG4$tau2, digits=4, format="f")), ", df = ", 
                                .(RMASG4$k - RMASG4$p), ", ", "Q = ", .(formatC(RMASG4$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG4$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                .(formatC(RMASG4$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG4$I2, digits=1, format="f")), 
                                "%)")), pos=4, cex=0.65)


dev.off()

