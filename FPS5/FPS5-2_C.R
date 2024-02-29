
library(readxl)
FPS5.1 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPDataSup2.xlsx", 
                   sheet = "FPS5", range = "A22:U31")

FPS5.1IR <- escalc(measure = "IR", xi = FPS5.1$xi, ti = FPS5.1$ti, data = FPS5.1, replace = FALSE)

FPS5.1RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS5.1IR)

###########

pdf(file = "FPS5.2.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FPS5.1RMA, slab = paste(FPS5.1$Author, FPS5.1$`Pub Year`), ilab = cbind(FPS5.1$ilab2, FPS5.1$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.45, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(9:1), ylim = c(0, 103), cex.lab = 0.75, top = 92)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FPS5.1RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FPS5.1RMA$k - FPS5.1RMA$p), ", ", "Q = ", .(formatC(FPS5.1RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FPS5.1RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS5.1RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS5.1RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(11, 10), pos = 4, c("SGIII - Carbapenem Resistance in", "Naive Isolates SG2"), font = 2, cex = 0.8)


text(-1.6, c(13), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(14), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(13), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(13), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(13), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

dev.off()
