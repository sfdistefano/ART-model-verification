

species.char <- as.data.frame(unique(UT.lpi.tall.layers$code))



### Still working on this, right NOW imported DIMA data doesn't have species characteristics ###

# ## summarizing lpi data based on functional group 
# # we're interested in shrubs, perennial grasses, and invasives
# summ.ART.fg.cover <- pct.cover(lpi.tall = lpi.tall.layers,
#                                tall = FALSE,
#                                hit = "any",
#                                by.year = FALSE,
#                                by.line = FALSE,
#                                duration, growth.habit, invasive
# )
# summ.UT.fg.cover <- pct.cover(lpi.tall = UT.lpi.tall.layers,
#                               tall = FALSE,
#                               hit = "any",
#                               by.year = FALSE,
#                               by.line = FALSE,
#                               duration, growth.habit, invasive
# )
# 
# ## summing cover values for: shrub, perennial grass, annual grass, invasive
# # foliar cover for ART plots
# summ.ART.fg.cover$shrub <- rowSums(summ.ART.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
# summ.ART.fg.cover$PG <- rowSums(summ.ART.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
# summ.ART.fg.cover$invasive <- rowSums(summ.ART.fg.cover[,grepl("yes", names(summ.ART.fg.cover))])
# 
# # foliar cover for reclamation and reference sites
# summ.UT.fg.cover$shrub <- rowSums(summ.UT.fg.cover[,c("perennial.shrub.no", "perennial.shrub.yes")])
# summ.UT.fg.cover$PG <- rowSums(summ.UT.fg.cover[,c("perennial.graminoid.no", "perennial.graminoid.yes")])
# summ.UT.fg.cover$invasive <- rowSums(summ.UT.fg.cover[,grepl("yes", names(summ.UT.fg.cover))])
# 
# ## merging the reclamation, reference, and ART plots based on Site Name so they can be compared
# # cleaning up names so they'll match for merge
# summ.ART.fg.cover$SiteName <- gsub(replacement = "CATH FED 30 01 Well Pad",
#                                    pattern = "Cath Fed 30 01 Well Pad",
#                                    x = summ.ART.fg.cover$SiteName) 
# # merging so that ART plot can be compared to their reference and reclamation
# # each comparison has its own row
# func.group <- merge(summ.ART.fg.cover,summ.UT.fg.cover, by = "SiteName", all.x = T)[,c("SiteName", "PlotID.x","PlotID.y", "shrub.x", "PG.x", "AG.x", "invasive.x",
#                                                                                        "shrub.y", "PG.y", "AG.y", "invasive.y")]
# # selecting comparisons between ART plot and its reference    
# func.group <- func.group[grep("Reference|Ref",func.group$PlotID.y),]