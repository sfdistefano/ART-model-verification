library(ggplot2)
library(gridExtra)
library(tidyverse)

#### FIGURES USED FOR PUBLICATION ####

#### IMPORT DATA ####

# vegetatation analysis results between ART plots and their reference site
comparisons <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/comparison.csv") 
# slope and topographic position index values for ART plots, reference, and reclamation
slope.wide <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/Topographic.data/slope.wide.csv")
# water holding capacity of collected soil samples for ART plots and reference sites
whc.wide <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/Soil.data/whc.wide.csv") %>%
  filter(art.plot != c("Federal 30-16 Plot 2","Federal 30-16 Plot 1"))
# rock fragment % of the total volume of soil samples
rock.wide <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/Soil.data/rock.diff.wide.csv")
# depth to restricting layer (e.g. bedrock)
bedrock.wide <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/Soil.data/bedrock.diff.wide.csv")

#### FUNCTION FOR PLOTTING LINEAR REGRESSION ####
ggplotRegression <- function(fit, x.pos, y.pos, font.size = 12, reg.size = 6){
  
  require(ggplot2)
  
  lab = paste("atop(atop(","R^2 == ",signif(summary(fit)$r.squared, 2),",",
              "Intercept == ",signif(fit$coef[[1]],2),"),atop(",
              "Regression~Coefficient ==",signif(fit$coef[[2]], 2),",",
              "p-value ==",signif(summary(fit)$coef[2,4], 2),
              "))")
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) +
    geom_point() +
    stat_smooth(method = "lm", col = "black", se = F) +
    annotate("text", x = x.pos, y = y.pos, parse=T,
             label = lab, size = reg.size) +
    theme(axis.title = element_text(size = font.size),
          axis.text = element_text(size = font.size)) +
    geom_abline(linetype = "dashed")
    
  
}

##### PLOTTING LINEAR REGRESSION RELATIONSHIPS #####

## graphing the relationship between ART plots and there reference
# comparing water holding capacity aka gravimetric moisture
# these values were obtained by air drying and then oven drying samples
# for 48 hrs at 105 degrees celsius
ggplotRegression(lm(ref.grav.moist ~ art.grav.moist, data = whc.wide),
                 x.pos = 0.0075, y.pos = 0.07) +
  labs(x = expression("ART Gravimetric Moisture"~theta[g]),
       y = expression("Reference Gravimetric Moisture"~theta[g]))  

ggsave("C:/Users/sfper/Documents/R/WRFO_git/Figures/grav.moist.jpeg",
       dpi = 300, device = "jpeg")

## vegetative similarity compared to ART predicted similarity
ggplotRegression(lm(bray.sim ~ ART.value, data = comparisons),
                 x.pos = 0.91875, y.pos = 0.7) +
  xlab("ART value") +
  ylab("Vegetative Similarity (Bray-Curtis)")
  
ggsave("C:/Users/sfper/Documents/R/WRFO_git/Figures/veg.vs.ART.jpeg",
       dpi = 300, device = "jpeg")

### vegetative similarity relationship over time ###
# calculates years since reclamation started
comparisons$years.rec <- 2018 - as.numeric(comparisons$abandon_year)

ggplotRegression(lm(bray.sim ~ years.rec, data = comparisons %>% 
                      filter(SiteName != "Federal 30-16 Well Pad")),
                 x.pos = 5.5 ,y =0.6) + # remove outliers (Fed 30-16)
  xlab("Years Since Reclamation Start") +
  ylab("Similarity (Bray-Curtis)") 

ggsave("C:/Users/sfper/Documents/R/WRFO_git/Figures/sim.vs.year.rec.jpeg",
       dpi = 300, device = "jpeg")

#### FOLIAR COVER RELATIONSHIPS ####

## ART plot foliar cover vs their reference site's foliar cover
foliar <- ggplotRegression(lm(ref.foliar ~ ART.foliar, data = comparisons),
                           x.pos = 32, y = 82, font.size = 9, reg.size = 4.5) +
  xlab("ART Plot Total Foliar Cover (%)") +
  ylab("Reference Total Foliar Cover (%)") 

## Foliar cover by functional group
# ART foliar shrub cover vs reference
shrub <- ggplotRegression(lm(shrub.ref ~ shrub.ART, data = comparisons),
                          x.pos = 27.25, y = 60, font.size = 9, reg.size = 4.5) +
  xlab("ART Plot Shrub Cover (%)") +
  ylab("Reference Shrub Cover (%)") 

# invasive plant species cover
invasive <- ggplotRegression(lm(invasive.ref ~ invasive.ART, 
                                data = comparisons %>% 
                                  filter(ART.plot != "Cath Fed P 35 3 101 Rd Plot 1")), # remove outlier
                             x.pos = 1.5, y = 16.75, font.size = 10, reg.size = 4.5) +
  xlab("ART Plot Invasive Species Cover (%)") +
  ylab("Reference Invasive Species Cover (%)")

# ART foliar PG cover vs reference
PG <- ggplotRegression(lm(PG.ref ~ PG.ART, data = comparisons),
                       x.pos = 32, y = 11.25, font.size = 9, reg.size = 4.5) +
  xlab("ART Plot Perennial Grass Cover (%)") +
  ylab("Reference Perennial Grass Cover (%)")  

## arranging the previous 4 vegetative plots into one graph

# if you need to see it in the R console
# can't save this using ggsave() since grid.arrange() directly draws in device
# grid.arrange(foliar, shrub, invasive, PG, nrow = 2)

# save
g <- arrangeGrob(foliar,shrub, invasive, PG, nrow = 2) # generates ggplot object
ggsave(file = "C:/Users/sfper/Documents/R/WRFO_git/Figures/foliar.relationships.jpeg",
       g, dpi = 300)

#### PLOTTING 3 HISTOGRAMS FOR DIFFERENCES (ART VS REF) ####
### SITE PHYSICAL CHARACTERISTICS: ROCK FRAGMENT, BEDROCK, SLOPE 

# function for plotting histograms
diff.hist <- function(df, x, xlab, ylab){
  breaks <- rock.breaks <- seq(0, 60, by = 10) # creating bins for frequency
  df$bins <- cut(x, breaks, right = FALSE) 
  
  graph <- ggplot(df, aes(x = bins)) +
    geom_bar() +
    #geom_text(stat = 'count', aes(label = ..count.., vjust = -0.2), # adds the count on top of each bar 
    #show.legend = FALSE) + # cut off with grid.arrange() so I removed it
    scale_x_discrete(xlab, 
                     labels = c("0-9.99", "10-19.99", "20-29.99", # labeling bins for frequency
                                "30-39.99", "40-49.99", ">50")) + 
    labs(y=ylab) +
    theme(axis.text = element_text(size=12), axis.title.x = element_text(size =12),
          axis.title.y = element_text(size=12)) # specifying font size
  
  return(graph)
}

# rock fragment (%)
rock <- diff.hist(df = rock.wide, x = rock.wide$rock.diff, 
                  xlab = "Rock Fragment (%) Differences", ylab = "Frequency")
# depth to restricting layer (bedrock)
bedrock <- diff.hist(df = bedrock.wide, x = bedrock.wide$bedrock.diff, 
                     xlab = "Bedrock (cm) Differences", ylab = "Frequency")
# topographic slope
slope <- diff.hist(df = slope.wide, x = slope.wide$slope.diff,
                   xlab = "Slope (%) Differences", ylab = "Frequency")

# arranging previous 3 plots into 1 figure, stacked on top of eachother
grid.arrange(rock, bedrock, slope, nrow = 3)

# exporting the figure
g <- arrangeGrob(rock, bedrock, slope, nrow = 3) # generates ggplot object
ggsave(file = "C:/Users/sfper/Documents/R/WRFO_git/Figures/site.phys.char.jpeg",
       g, dpi = 300)
