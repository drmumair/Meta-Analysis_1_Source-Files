
library(readxl)
FPS9 <- read_excel("C:/Users/drmum/OneDrive - Higher Education Commission/Carbapenemase Metaanalysis/Forest Plots/FPDataSup2.xlsx", 
                  sheet = "FPS9", range = "A1:W54")

FPS9IR <- escalc(measure = "IR", xi = FPS9$xi, ti = FPS9$ti, data = FPS9, replace = FALSE)

FPS9RMA <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS9IR)

FPS9RMA1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, mods = ~ SubGroup, data = FPS9IR)


###########

pdf(file = "FPS9.pdf", family = "Times", height = 14, width = 8.5)

par(mar = c(4,7,1,5))

## c(4,8,1,6)

forest.rma(FPS9RMA, slab = paste(FPS9$Author, FPS9$`Pub Year`), ilab = cbind(FPS9$ilab2, FPS9$ilab1), ilab.pos = 4, 
           ilab.xpos = c(1.545, 1.215), digits = c(2, 1), at = c(-0.5, 0, 0.5, 1, 1.5), mlab = "",
           refline = 0.08, xlim = c(-1.6, 2.8), showweights = TRUE, efac = 0.75, header = FALSE, 
           cex = 0.65, rows = c(62:16, 10:5), ylim = c(-3, 103), cex.lab = 0.75, top = 40)

text(-1.6, c(-1), pos = 4, c("RE Model for All Studies"), font = 2, cex = 0.65)
text(-1.6, c(-2.25), bquote(paste("(",Tau^2, " = ",.(formatC(FPS9RMA$tau2, digits=4, format="f")), ", df = ", 
                                  .(FPS9RMA$k - FPS9RMA$p), ", ", "Q = ", .(formatC(FPS9RMA$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-3.25), bquote(paste("p ", .(metafor:::.pval(FPS9RMA$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS9RMA$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS9RMA$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)

text(-1.6, c(-4.5), pos = 4, c("Test for Subgroup Differences"), font = 2, cex = 0.65)
text(-1.6, c(-5.75), bquote(paste("(", Tau^2, " = ",.(formatC(FPS9RMA1$tau2, digits=4, format="f")),", df = ", .(FPS9RMA1$p - 1),
                                  ", ", Q[M], " = ", .(formatC(FPS9RMA1$QM, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(-6.75), bquote(paste("p ", .(metafor:::.pval(FPS9RMA1$QMp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
                                  .(formatC(FPS9RMA1$H2, digits=1, format="f")), ", ", I^2, " = ", .(formatC(FPS9RMA1$I2, digits=1, format="f")), 
                                  "%)")), pos=4, cex=0.65)


text(-1.6, c(63, 11), pos = 4, c("Clinical Studies", "Veterinary Studies"), font = 2, cex = 0.8)

text(-1.6, c( 66.25 ), pos = 4, c("Enterobacteriaceae"), font = 4, cex = 0.9)


text(-1.6, c(65), pos = 4, c("Subgroups/Author(s)"), font = 2, cex = 0.75)
text(1.545, c(66), pos = 4, c("Trait-"), font = 2, cex = 0.75)
text(1.545, c(65), pos = 4, c("Species"), font = 2, cex = 0.75)
text(1.215, c(65), pos = 4, c("Sample"), font = 2, cex = 0.75)
text(1.9274, c(65), pos = 4, c("Weight% Pr[95% CI]"), font = 2, cex = 0.75)

RMASG1 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS9IR, subset = (SubGroup=="Human")) 
RMASG2 <- rma(yi = yi, vi = vi, method = "DL", knha = TRUE, data = FPS9IR, subset = (SubGroup=="Veterinary")) 


addpoly(RMASG1, row = 13.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))
addpoly(RMASG2, row = 2.5, cex = 0.7, mlab = "", border = 0, col = rgb(64, 64, 64, maxColorValue = 255))


text(-1.6, c(14.75), pos = 4, c("RE Model for Subgroup"), font = 2, cex = 0.65)
text(-1.6, c(13.5), bquote(paste("(",Tau^2, " = ",.(formatC(RMASG1$tau2, digits=4, format="f")), ", df = ", 
                                 .(RMASG1$k - RMASG1$p), ", ", "Q = ", .(formatC(RMASG1$QE, digits=2, format="f")), ",")), pos=4, cex=0.65)

text(-1.6, c(12.5), bquote(paste("p ", .(metafor:::.pval(RMASG1$QEp, digits=4, showeq=TRUE, sep=" ")), "; ", H^2, " = ", 
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

pdf(file = "FPS9inf.pdf", family = "Times", height = 11, width = 8.5)

par(mar = c(5,6,3,4))


inf <- influence(FPS9RMA)

plot(inf, layout=c(8,1))


dev.off()

#####################################

pdf(file = "FPS9rad.pdf", family = "Times", height = 11, width = 7.5)

par(mar = c(5,5,3,3))

radial(FPS9RMA)

dev.off()

