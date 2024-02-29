
library(readxl)
FP7 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP7", range = "A1:X26")

FP7IR <- escalc(measure = "IR", xi = FP7$xi, ti = FP7$ti, data = FP7, replace = FALSE)

FP7RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP7IR)

FP7RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FP7IR)


###########

pdf(file = "FP7.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP7RMA, slab = paste(FP7$Author, FP7$`Pub Year`), ilab = cbind(FP7$ilab2, FP7$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.24, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(34:25, 19:5), ylim = c(-3, 103), cex.lab = 0.75, top = 68)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP7RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP7RMA$k - FP7RMA$p), ", ", "Q = ", .(formatC(FP7RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP7RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP7RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP7RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FP7RMA1$tau2, digits=4, format="f")),", df = ", .(FP7RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FP7RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FP7RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP7RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP7RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(35, 20), pos = 4, c("Carbapenem Resistant Isolates", "Naive Isolates"), font = 2, cex = 0.8)

text(-1.6, c( 38.25 ), pos = 4, c("Enterobacteriaceae + Non-Enterobacteriaceae"), font = 4, cex = 0.9)


text(-1.6, c(37), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(38), pos = 4, c("NDM"), font = 2, cex = 0.75)
text(1.545, c(37), pos = 4, c("Gene(s)"), font = 2, cex = 0.75)
text(1.215, c(38), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.215, c(37), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.9274, c(37), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP7IR, subset = (SubGroup=="Resistant")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP7IR, subset = (SubGroup=="Susceptible")) 


addpoly(RMASG1, row = 22.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(23.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(22.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(21.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
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

pdf(file = "FP7inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))

inf <- influence(FP7RMA)

plot(inf, layout=c(8,1))

dev.off()

#####################################

pdf(file = "FP7rad.pdf", family = "Times", height = 11, width = 7.5)

par(mar = c(5,5,3,3))

radial(FP7RMA)

dev.off()


