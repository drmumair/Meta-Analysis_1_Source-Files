
library(readxl)
FP6_2 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                    sheet = "FP6_2", range = "A1:U36")

FP6_2IR <- escalc(measure = "IR", xi = FP6_2$xi, ti = FP6_2$ti, data = FP6_2, replace = FALSE)

FP6_2RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR)

FP6_2RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FP6_2IR)


###########

pdf(file = "FP6_2.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP6_2RMA, slab = paste(FP6_2$Author, FP6_2$`Pub Year`), ilab = cbind(FP6_2$ilab2, FP6_2$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.11, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(59:54, 48:43, 37:35, 29:15, 9:5), ylim = c(-3, 103), cex.lab = 0.75, top = 43)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP6_2RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP6_2RMA$k - FP6_2RMA$p), ", ", "Q = ", .(formatC(FP6_2RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP6_2RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP6_2RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP6_2RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FP6_2RMA1$tau2, digits=4, format="f")),", df = ", .(FP6_2RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FP6_2RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FP6_2RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP6_2RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP6_2RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(60, 49, 38, 30, 10), pos = 4, c("Urinary Tract Infections", "Pediatrics", "Intensive Care Unit", "Tertiary Care Hospital",
                                             "Hospitalized/Non-Hospitalized"), font = 2, cex = 0.8)
text(-1.6, c( 63.25 ), pos = 4, c("Enterobacteriaceae - Clinical - Naive Isolates"), font = 4, cex = 0.9)

text(-1.6, c(62), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(62), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(62), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(62), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR, subset = (SubGroup=="UTI")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR, subset = (SubGroup=="PED")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR, subset = (SubGroup=="ICU")) 
RMASG4 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR, subset = (SubGroup=="TCH")) 
RMASG5 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP6_2IR, subset = (SubGroup=="HNHS")) 


addpoly(RMASG1, row = 51.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 40.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 32.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG4, row = 13.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG5, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(52.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(51.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(50.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(41.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(40.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(39.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(33.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(32.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(31.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ",H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(13.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(12.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG4$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG4$k - RMASG4$p), ", ", "Q = ", .(formatC(RMASG4$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(11.5), bquote(paste("p ", .(metafor:::.pval(RMASG4$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG4$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG4$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG5$tau2, digits=4, format="f")), ", df = ", 
                                .(RMASG5$k - RMASG5$p), ", ", "Q = ", .(formatC(RMASG5$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG5$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                .(formatC(RMASG5$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG5$I2, digits=1, format="f")), 
                                "%)")), pos=4, cex=0.65)


dev.off()

#####################################

pdf(file = "FP6_2inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FP6_2RMA)

plot(inf, layout=c(8,1))

dev.off()

#####################################

pdf(file = "FP6_2rad.pdf", family = "Times", height = 11, width = 7.5)

par(mar = c(5,5,3,3))

radial(FP6_2RMA)

dev.off()


