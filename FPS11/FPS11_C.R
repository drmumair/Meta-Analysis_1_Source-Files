
library(readxl)
FPS11 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPDataSup2.xlsx", 
                   sheet = "FPS11", range = "A1:Y26")

FPS11IR <- escalc(measure = "IR", xi = FPS11$xi, ti = FPS11$ti, data = FPS11, replace = FALSE)

FPS11RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS11IR)

FPS11RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FPS11IR)


###########

pdf(file = "FPS11.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FPS11RMA, slab = paste(FPS11$Author, FPS11$`Pub Year`), ilab = cbind(FPS11$ilab2, FPS11$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.24, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(46, 43:39, 33:30, 24:20, 14:5), ylim = c(-3, 103), cex.lab = 0.75, top = 56)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FPS11RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FPS11RMA$k - FPS11RMA$p), ", ", "Q = ", .(formatC(FPS11RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FPS11RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS11RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS11RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FPS11RMA1$tau2, digits=4, format="f")),", df = ", .(FPS11RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FPS11RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FPS11RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS11RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS11RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(47, 44, 34, 25, 15), pos = 4, c("2011-12", "2013-14", "2015-16", "2017-18", "2019-20"), font = 2, cex = 0.8)

text(-1.6, c( 50.25 ), pos = 4, c("Enterobacteriaceae + Non-Enterobacteriaceae"), font = 4, cex = 0.9)

text(-1.6, c(49), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(50), pos = 4, c("NDM"), font = 2, cex = 0.75)
text(1.545, c(49), pos = 4, c("Gene(s)"), font = 2, cex = 0.75)
text(1.215, c(50), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.215, c(49), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.9274, c(49), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)


RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS11IR, subset = (SubGroup=="SG2")) 
RMASG3 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS11IR, subset = (SubGroup=="SG3")) 
RMASG4 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS11IR, subset = (SubGroup=="SG4")) 
RMASG5 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS11IR, subset = (SubGroup=="SG5")) 


addpoly(RMASG2, row = 36.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG3, row = 27.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG4, row = 17.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG5, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(37.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(36.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG2$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG2$k - RMASG2$p), ", ", "Q = ", .(formatC(RMASG2$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(35.5), bquote(paste("p ", .(metafor:::.pval(RMASG2$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG2$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG2$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(28.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(27.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG3$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG3$k - RMASG3$p), ", ", "Q = ", .(formatC(RMASG3$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(26.5), bquote(paste("p ", .(metafor:::.pval(RMASG3$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG3$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG3$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)

text(-1.6, c(18.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(17.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG4$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG4$k - RMASG4$p), ", ", "Q = ", .(formatC(RMASG4$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(16.5), bquote(paste("p ", .(metafor:::.pval(RMASG4$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                 .(formatC(RMASG4$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG4$I2, digits=1, format="f")), 
                                 "%)")), pos=4, cex=0.65)


text(-1.6, c(3.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(2.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG5$tau2, digits=4, format="f")), ", df = ", 
                                .(RMASG5$k - RMASG5$p), ", ", "Q = ", .(formatC(RMASG5$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(1.5), bquote(paste("p ", .(metafor:::.pval(RMASG5$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                .(formatC(RMASG5$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(RMASG5$I2, digits=1, format="f")), 
                                "%)")), pos=4, cex=0.65)


dev.off()

