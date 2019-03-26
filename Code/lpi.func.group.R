setwd("C:/Users/seaperry/Dropbox/DiStefano_WRFO_project")

master.species.list <- as.data.frame(names(summ.cover[6:131]))
write.csv(master.species.list,"master.species.list.csv")
master.species.list <- read.csv("master.species.list.csv")[,c("common.name", "duration", "growth.habit", "invasive")]

# renaming point data
ART.lpi.point <- lpi.tall.layers
UT.lpi.point <- UT.lpi.tall.layers

# adding duration to all point data
ART.lpi.func.group <- merge(ART.lpi.point, master.species.list, by.x = "CommonName", by.y = "common.name", all.x = T)
UT.lpi.func.group <- merge(UT.lpi.point, master.species.list, by.x = "CommonName", by.y = "common.name", all.x = T)

# summarizing lpi data based on functional group
summ.ART.fg.cover <- pct.cover(lpi.tall = ART.lpi.func.group,
                              tall = FALSE,
                              hit = "any",
                              by.year = FALSE,
                              by.line = FALSE,
                              duration, growth.habit, invasive
)
summ.UT.fg.cover <- pct.cover(lpi.tall = UT.lpi.func.group,
                              tall = FALSE,
                              hit = "any",
                              by.year = FALSE,
                              by.line = FALSE,
                              duration, growth.habit, invasive
)

# summing values for: shrub, perennial grass, annual grass, invasive
summ.ART.fg.cover$shrub <- rowSums(summ.ART.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
summ.ART.fg.cover$PG <- rowSums(summ.ART.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
summ.ART.fg.cover$AG <- summ.ART.fg.cover$annual.graminoid.yes
summ.ART.fg.cover$invasive <- rowSums(summ.ART.fg.cover[,grepl("yes", names(summ.ART.fg.cover))])

summ.UT.fg.cover$shrub <- rowSums(summ.UT.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
summ.UT.fg.cover$PG <- rowSums(summ.UT.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
summ.UT.fg.cover$AG <- summ.UT.fg.cover$annual.graminoid.yes
summ.UT.fg.cover$invasive <- rowSums(summ.UT.fg.cover[,grepl("yes", names(summ.UT.fg.cover))])

# merging the reclamation, reference, and ART plots based on Site Name so they can be compared
summ.ART.fg.cover$SiteName <- gsub(replacement = "CATH FED 30 01 Well Pad",
                            pattern = "Cath Fed 30 01 Well Pad",
                            x = summ.ART.fg.cover$SiteName) 
func.group <- merge(summ.ART.fg.cover,summ.UT.fg.cover, by = "SiteName", all.x = T)[,c("SiteName", "PlotID.x","PlotID.y", "shrub.x", "PG.x", "AG.x", "invasive.x",
                                                                                   "shrub.y", "PG.y", "AG.y", "invasive.y")]
# selecting comparisons between ART plot and its reference    
func.group <- func.group[grep("Reference|Ref",func.group$PlotID.y),]

func.group$ref.inv.bigger <- func.group$invasive.y > func.group$invasive.x
func.group$ART.PG.bigger <- func.group$PG.x > func.group$PG.y
func.group$ART.shrub.bigger <- func.group$shrub.x > func.group$shrub.y
