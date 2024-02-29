
library(readxl)
FP3 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPData2.xlsx", 
                  sheet = "FP3", range = "A1:U13")

FP3IR <- escalc(measure = "IR", xi = FP3$xi, ti = FP3$ti, data = FP3, replace = FALSE)

FP3RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FP3IR)



###########

pdf(file = "FP3.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FP3RMA, slab = paste(FP3$Author, FP3$`Pub Year`), ilab = cbind(FP3$ilab2, FP3$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.23, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(12:1), ylim = c(0, 103), cex.lab = 0.75, top = 90)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FP3RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FP3RMA$k - FP3RMA$p), ", ", "Q = ", .(formatC(FP3RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FP3RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FP3RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FP3RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c( 13 ), pos = 4, c( "Unsorted" ), font = 2, cex = 0.8)

text(-1.6, c( 16.25 ), pos = 4, c("Enterobacteriaceae + Non-Enterobacteriaceae"), font = 4, cex = 0.9)

text(-1.6, c(15), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(16), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(15), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(15), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(15), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)


dev.off()

