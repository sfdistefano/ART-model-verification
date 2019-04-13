whc.wide <- read.csv("C:/Users/sfper/Documents/R/WRFO_git/Soil.data/whc.wide.csv")

ggplotRegression <- function(fit){
  
  require(ggplot2)
  
  wrapper <- function(x, ...) paste(strwrap(x, ...), collapse = "\n")
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", se = F) +
    annotate("text", x = 0.125, y = 0.07,
             label = stringr::str_wrap(paste("R2 =",signif(summary(fit)$r.squared, 2),",",
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Regression Coefficient =",signif(fit$coef[[2]], 2),
                       "p-value =",signif(summary(fit)$coef[2,4], 2)),
                       width = 30)) +
    theme(axis.title = element_text(size = 12),
          title = element_text(size = 12), axis.text = element_text(size = 12))
}


ggplotRegression(lm(ref.grav.moist ~ art.grav.moist, data = whc.wide)) +
  xlab("ART Gravimetric Moisture ??g") +
  ylab("Reference Gravimetric Moisture ??g") +
  geom_abline(linetype = "dashed") 

