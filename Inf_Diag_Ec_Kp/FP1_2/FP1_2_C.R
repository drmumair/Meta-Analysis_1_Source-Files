
library(readxl)
FP1_2 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP1_2", range = "A1:U36")

FP1_2IR <- escalc(measure = "IR", xi = FP1_2$xi, ti = FP1_2$ti, data = FP1_2, replace = FALSE)

FP1_2RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1_2IR)

FP1_2RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FP1_2IR)


###########

pdf(file = "FP1_2.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP1_2RMA, slab = paste(FP1_2$Author, FP1_2$`Pub Year`), ilab = cbind(FP1_2$ilab2, FP1_2$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.11, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(51:48, 42:39, 33:32, 29:5), ylim = c(-3, 103), cex.lab = 0.75, top = 51)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP1_2RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP1_2RMA$k - FP1_2RMA$p), ", ", "Q = ", .(formatC(FP1_2RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP1_2RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP1_2RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP1_2RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FP1_2RMA1$tau2, digits=4, format="f")),", df = ", .(FP1_2RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FP1_2RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FP1_2RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP1_2RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP1_2RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(52, 43, 34), pos = 4, c("Escherichia coli", "Klebsiella pneumoniae", "Salmonella enterica"), font = 4, cex = 0.8)

text(-1.6, c( 30 ), pos = 4, c("Unsorted"), font = 2, cex = 0.8)

text(-1.6, c( 55.25 ), pos = 4, c("Enterobacteriaceae - Clinical - Naive Isolates"), font = 4, cex = 0.9)


text(-1.6, c(54), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(54), pos = 4, c("Trait"), font = 2, cex = 0.75)
text(1.215, c(54), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(54), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1_2IR, subset = (SubGroup=="Escherichia coli")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1_2IR, subset = (SubGroup=="Klebsiella pneumoniae")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP1_2IR, subset = (SubGroup=="Enterobacteriaceae")) 


addpoly(RMASG1, row = 45.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 36.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(46.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(45.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(44.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG1$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(37.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(36.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(35.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ",H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)


dev.off()

#####################################

pdf(file = "FP1_2inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FP1_2RMA)

plot(inf, layout=c(8,1))

dev.off()


