ggplotRegression <- function(fit){
  
  require(ggplot2)
  
  ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
    geom_point() +
    stat_smooth(method = "lm", col = "red", se = F) +
    labs(title = paste("R2 = ",signif(summary(fit)$r.squared, 2),
                       "Intercept =",signif(fit$coef[[1]],2 ),
                       " Regression Coefficient =",signif(fit$coef[[2]], 2),
                       " p-value =",signif(summary(fit)$coef[2,4], 2)))
}

comparisons <- read.csv("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/comparison.csv")
# figure 2
ggplotRegression(lm(similarity ~ ART.value, data = comparisons)) +
  xlab("ART value") +
  ylab("Vegetative Similarity (Bray-Curtis)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))
  

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ARTsim.vs.braysim.jpg")
# figure 3
# calculates years since reclamation started
comparisons$years.rec <- 2018 - as.numeric(comparisons$abandon_year)

ggplotRegression(lm(bray.sim ~ years.rec, 
                    # removes outliers
                    data = comparisons[!grepl("Federal 30-16", comparisons$Plot2),])) +
  xlab("Years Since Reclamation Start") +
  ylab("Similarity (Bray-Curtis)") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/bray.vs.recdate.jpg")

# ggplotRegression(lm(ART.simp ~ ref.simp, data = diversity.wide)) +
#   xlab("Reference Diversity (D)") +
#   ylab("ART Plot Diversity (D)") 
# 
# ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.div.D.jpg")

# ggplotRegression(lm(ref.shann ~ ART.shann, data = diversity.wide)) +
#   xlab("Reference Diversity (H')") +
#   ylab("ART Plot Diversity (H')") 
# 
# ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.div.H.jpg")

# figure 3
ggplotRegression(lm(ref.foliar ~ ART.foliar, data = table2)) +
  xlab("ART Plot Total Foliar Cover (%)") +
  ylab("Reference Total Foliar Cover (%)") +
  geom_abline(linetype = "dashed") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.tot.foliar.jpg")


# ggplotRegression(lm(ART.bg ~ ref.BG, data = table2))

# figure 4
ggplotRegression(lm(shrub.y ~ shrub.x, data = func.group)) +
  xlab("ART Plot Shrub Cover (%)") +
  ylab("Reference Shrub Cover (%)") + 
  geom_abline(linetype = "dashed") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.shrub.jpg")

ggplotRegression(lm(PG.y ~ PG.x, data = func.group)) +
  xlab("ART Plot Perennial Grass Cover (%)") +
  ylab("Reference Perennial Grass Cover (%)") +
  geom_abline(linetype = "dashed") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.PG.jpg")

# ggplotRegression(lm(AG.x ~ AG.y, data = func.group)) +
#   xlab("Reference Annual Grass Cover (%)") +
#   ylab("ART Plot Annual Grass Cover (%)")
# 
# ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.AG.jpg")

ggplotRegression(lm(invasive.y ~ invasive.x, data = func.group)) +
  xlab("ART Plot Invasive Species Cover (%)") +
  ylab("Reference Invasive Species Cover (%)") +
  geom_abline(linetype = "dashed") +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 12))

ggsave("C:/Users/sfper/Dropbox/DiStefano_WRFO_project/Thesis/figures/ART.vs.ref.invasive.jpg")
