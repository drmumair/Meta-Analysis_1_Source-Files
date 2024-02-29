
library(readxl)
FP2 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP2", range = "A1:U5")

FP2IR <- escalc(measure = "IR", xi = FP2$xi, ti = FP2$ti, data = FP2, replace = FALSE)

FP2RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP2IR)


###########

pdf(file = "FP2.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP2RMA, slab = paste(FP2$Author, FP2$`Pub Year`), ilab = cbind(FP2$ilab2, FP2$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.87, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(4:1), ylim = c(0, 103), cex.lab = 0.75, top = 98)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP2RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP2RMA$k - FP2RMA$p), ", ", "Q = ", .(formatC(FP2RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP2RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP2RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP2RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c( 5 ), pos = 4, c("Acinetobacter baumannii"), font = 4, cex = 0.8)

text(-1.6, c( 8.25 ), pos = 4, c("Non-Enterobacteriaceae"), font = 4, cex = 0.9)


text(-1.6, c(7), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(7), pos = 4, c("Trait"), font = 2, cex = 0.75)
text(1.215, c(7), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(7), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)


dev.off()

