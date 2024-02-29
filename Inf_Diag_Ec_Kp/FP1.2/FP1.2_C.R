library(readxl)
FP1.2 <- read_excel("C:/Users/lina4064/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/Inf_Diag_Ec_Kp/FPData2.xlsx", 
                      sheet = "FP1.2", range = "A1:U50")

FP1.2IR <- escalc(measure = "IR", xi = FP1.2$xi, ti = FP1.2$ti, data = FP1.2, replace = FALSE)

FP1.2RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1.2IR)

FP1.2RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FP1.2IR)


###########

pdf(file = "FP1.2.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP1.2RMA, slab = paste(FP1.2$Author, FP1.2$`Pub Year`), ilab = cbind(FP1.2$ilab2, FP1.2$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.12, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(68:56, 50:45, 39:36, 30:5), ylim = c(-3, 103), cex.lab = 0.75, top = 34)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP1.2RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP1.2RMA$k - FP1.2RMA$p), ", ", "Q = ", .(formatC(FP1.2RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP1.2RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP1.2RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP1.2RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FP1.2RMA1$tau2, digits=4, format="f")),", df = ", .(FP1.2RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FP1.2RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FP1.2RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP1.2RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP1.2RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(69, 51, 40), pos = 4, c("Escherichia coli", "Klebsiella pneumoniae", "Salmonella enterica"), font = 4, cex = 0.8)

text(-1.6, c( 31 ), pos = 4, c("Unsorted"), font = 2, cex = 0.8)

text(-1.6, c( 72.25 ), pos = 4, c("Enterobacteriaceae"), font = 4, cex = 0.9)


text(-1.6, c(71), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(71), pos = 4, c("Trait"), font = 2, cex = 0.75)
text(1.215, c(71), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(71), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1.2IR, subset = (SubGroup=="Escherichia coli")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1.2IR, subset = (SubGroup=="Klebsiella pneumoniae")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1.2IR, subset = (SubGroup=="Salmonella")) 
RMASG4 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1.2IR, subset = (SubGroup=="Enterobacteriaceae")) 


addpoly(RMASG1, row = 53.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 41.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 33.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG4, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(54.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(53.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(52.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(43.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(42.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(41.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(34.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(33.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(32.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ",H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG4$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG4$k - RMASG4$p), ", ", "Q = ", .(formatC(RMASG4$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG4$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG4$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG4$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)


dev.off()

#####################################

pdf(file = "FP1.2inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FP1.2RMA)

plot(inf, layout=c(8,1))

dev.off()

#####################################

pdf(file = "FP1.2rad.pdf", family = "Times", height = 11, width = 7.5)

par(mar = c(5,5,3,3))

radial(FP1.2RMA)

dev.off()


