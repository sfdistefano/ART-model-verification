library(dplyr)
library(tidyverse)
library(qdapRegex)
library(magrittr)

setwd("C:/Users/sfper/Documents/R/WRFO_git/Soil.data")

art.soil.data <- read.csv("art.soil.field.csv")
UT.soil.data <- read.csv("ref.soil.field.csv")

summ.art.soil <- art.soil.data %>% group_by(plot) %>% 
  mutate(tot.rock.frag.vol = sum(horizon.rock.frag.vol), 
         soil.pedon.depth = max(Lower.Depth)) %>%
  dplyr::select(plot, tot.rock.frag.vol, soil.pedon.depth) %>% unique() %>%
  mutate(site = rm_white(gsub(pattern = "FR|Plot 1|Plot 2|Well Pad|Rd", 
                              replacement = "", x = plot))) %>%
  mutate(site = ifelse(plot == "Cath Fed P 35 3 101 Rd Plot 1" |
                         plot == "Cath Fed P 35 3 101 Rd Plot 2",
                       "Cath Fed P 35 3 101 Rd", site)) 


summ.UT.soil <- UT.soil.data %>% group_by(plot) %>% 
  mutate(tot.rock.frag.vol = sum(horizon.rock.frag.vol), 
         soil.pedon.depth = max(Lower.Depth)) %>% 
  dplyr::select(plot, tot.rock.frag.vol, soil.pedon.depth) %>% unique() %>%
  mutate(site = rm_white(gsub(pattern = "Ref", replacement = "", x = plot))) %>%
  mutate(site = rm_white(gsub(pattern = "Fed 30 16", replacement = "Federal 30-16", x = site)))

summ.soil.wide <- merge(summ.art.soil, summ.UT.soil, by = "site") %>% 
  select(-plot.y) %>% set_colnames(c("site", "ART.plot", "ART.rock.frag.vol", "ART.pedon.depth",
                                      "ref.rock.frag.col", "ref.pedon.depth")) %>%
  mutate(diff.rock.frag = abs(ART.rock.frag.vol - ref.rock.frag.col))
                                                                             
