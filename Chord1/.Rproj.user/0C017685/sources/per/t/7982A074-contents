
library(readxl)
DataC <- read_excel("/Users/Umair/Library/CloudStorage/OneDrive-Nexus365/Carbapenemase Metaanalysis/Chord_Data/Data_Circos.xlsx", 
                    sheet = "DataC1_1", range = "B1:AF27")

DataC1_1 <- read_excel("/Users/Umair/Library/CloudStorage/OneDrive-Nexus365/Carbapenemase Metaanalysis/Chord_Data/Data_Circos.xlsx", 
                     sheet = "DataC1_1", range = "C1:AF27")

RowsC1_1 <- read_excel("/Users/Umair/Library/CloudStorage/OneDrive-Nexus365/Carbapenemase Metaanalysis/Chord_Data/Data_Circos.xlsx", 
                     sheet = "RowsC1_1", range = "A1:A27")

matc1 = as.matrix(DataC1_1)

matRowsC1_1 = as.matrix(RowsC1_1)

row.names(matc1) <- c(matRowsC1_1)


########## pdf ##########

pdf(file = "Circos1-1.pdf", family = "Times", height = 12, width = 12)
par(mar = c(1, 1, 1, 1))

grid.col = c('NDM-1' = "red", 'NDM-4' = "orangered3", 'NDM-5' = "red4", 'NDM-7' = "firebrick3", 'GES-1' = "darkslategrey", 
             
             'GES-5' = "darkslategray4", 'GIM-1' = "goldenrod3", 'IMP-1' = "gold", 'KPC-1' = "violetred4", 'KPC-2' = "violetred", 
             
             'SIM-1' = "chocolate", 'VIM-1' = "blue", 'VIM-2'  = "cornflowerblue", 'VIM-4' = "blue4", 'OXA-23' = "chartreuse4", 
             
             'OXA-23-like' = "chartreuse", 'OXA-25' = "deepskyblue", 'OXA-48' = "darkorchid4", 'OXA-48-like' = "orchid", 
             
             'OXA-51' = "skyblue", 'OXA-51-like' = "slateblue", 'OXA-82' = "maroon", 'OXA-117' = "deeppink", 
             
             'OXA-181' = "purple", 'OXA-181/232' = "maroon4", 'OXA-232' = "salmon4",
             
             'Non-Ent**' ="orange3", 'S. ureilytica' ="orange3", 'S. marcescens' ="orange3", 'P. rettgeri' = "orange3", 'P. putida' ="orange3", 'P. aeruginosa' ="orange3", 
             
             'P. mirabilis' ="orange3", 'M. morganii' = "orange3", 'A. faecalis' = "orange3",  'A. caviae' = "orange3", 'A. towneri' = "orange3", 
             
             'A. schindleri' = "orange3", 'A. haemolyticus' = "orange3", 'A. baumannii' = "orange3", 'Ent.*' = "violetred4", 
             
             'S. enterica' = "violetred4", 'P. faecalis' = "violetred4", 'K. georgiana' = "violetred4", 
             
             'K. quasipneumoniae' = "violetred4", 'K. pneumoniae' = "violetred4", 'K. oxytoca' = "violetred4", 'E. sakazakii' = "violetred4", 
             
             'E. cloacae' = "violetred4", 'E. asburiae' = "violetred4", 'E. aerogenes' = "violetred4", 'E. coli' = "violetred4", 
             
             'C. sedlakii' = "violetred4", 'C. novel spp.' = "violetred4", 'C. freundii' = "violetred4", 'C. braakii' = "violetred4")

circos.par(start.degree = 90, clock.wise = FALSE)


chordDiagram(matc1, annotationTrack = "grid", grid.col = grid.col, transparency = 0.20, directional = 1, diffHeight = mm_h(1.25),
             
             preAllocateTracks = list(track.height = max(strwidth(unlist(dimnames(matc1))))))

circos.track(track.index = 1, panel.fun = function(x, y) {
  
  circos.text(x = , CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index, col = "black",
              
              facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), font = 1, cex = 0.6)
  
}, bg.border = NA)

circos.clear()

dev.off()
