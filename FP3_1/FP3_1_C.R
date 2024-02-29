
library(readxl)
FP3_1 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP3_1", range = "A1:U12")

FP3_1IR <- escalc(measure = "IR", xi = FP3_1$xi, ti = FP3_1$ti, data = FP3_1, replace = FALSE)

FP3_1RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP3_1IR)


###########

pdf(file = "FP3_1.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP3_1RMA, slab = paste(FP3_1$Author, FP3_1$`Pub Year`), ilab = cbind(FP3_1$ilab2, FP3_1$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.23, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(11:1), ylim = c(0, 103), cex.lab = 0.75, top = 91)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP3_1RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP3_1RMA$k - FP3_1RMA$p), ", ", "Q = ", .(formatC(FP3_1RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP3_1RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP3_1RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP3_1RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(12), pos = 4, c( "Gram-Negative Bacteria" ), font = 2, cex = 0.8)

text(-1.6, c(15.25), pos = 4, c("Enterobacteriaceae + Non-Enterobacteriaceae"), font = 4, cex = 0.9)


text(-1.6, c(14), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(15), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(14), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(14), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(14), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)


dev.off()

