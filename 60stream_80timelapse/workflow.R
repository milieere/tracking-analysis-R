data <- loadCleanData('./', 9, 20, 60)
distr <- plotDistr(data, 9, 20)

library(dplyr)
#COunt msd only for the first three seconds of stream!
msds_filtered <- calcMSD(data %>% filter(FRAME < 60), 1)

fun_groups <- function(x) if (x <= 20) 'category1' else if (x > 20 & x <= 30) 'category2' else if (x > 30 & x <= 40) 'category3' else if (x > 40 & x <= 50) 'category4' else 'uncat'
msds_joined$size_cats <- mapply(fun_groups, msds_joined$length)


summaryMsd <- data_summary(msds_joined, varname="msd", 
                           groupnames=c('length_cat', 'timepoint'))

ribbonplot <- ggplot(summaryMsd %>% filter(timepoint < 20), aes(timepoint, mean, group = length_cat, color = length_cat)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymax = mean + sd, ymin = mean - sd),
              alpha = 0.15,
              fill = "grey70",
              colour=NA)

#Plot mean intensities for categories, filtering for spots lasting more than 10 frames
summary1 <- data %>% 
  filter(FRAME < 60) %>%
  group_by(length_category, timepoint) %>% 
  summarize(mean_intensity = mean(MEAN_INTENSITY), sd= sd(MEAN_INTENSITY))

#AUtomatic grouping
data_filtered$group <- as.numeric(cut(data_filtered$duration_sec, 20))


summary <- data %>% 
  filter(FRAME < 60) %>%
  group_by(timepoint, length_category) %>% 
  summarize(mean_intensity = mean(MEAN_INTENSITY), sd= sd(MEAN_INTENSITY))

summary_length <- data_filtered %>% 
  filter(FRAME < 60) %>%
  group_by(group) %>% 
  summarize(duration_sec = mean(duration_sec), sd= sd(duration_sec))

ribbonplot1 <- ggplot(summary %>% filter(timepoint<10), aes(timepoint, mean_intensity, group = length_category, color = length_category)) +
  geom_line() +
  geom_point() +
  geom_ribbon(aes(ymax = mean_intensity + sd, ymin = mean_intensity - sd),
              alpha = 0.1,
              fill = "grey70",
              colour=NA)


#Filter only spots that have more than 0.25 sec length
data_filtered <- data %>% filter(duration_sec>0.25)

data.pca <- prcomp(data_filtered[,c(4, 13:21)], center = TRUE,scale. = TRUE)
summary(data.pca)
library(ggbiplot)
pca <- ggbiplot(data.pca,ellipse=TRUE, groups=data_filtered$length_category)

#Better PCA package
library(factoextra)
library(FactoMineR)

#Make PCA
# PCA tutorial http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/
# CLustering http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/117-hcpc-hierarchical-clustering-on-principal-components-essentials/

#Do PCA on summaries of 10 timepoints only, with summary of TRACKS

data.active <- data_filtered[,c(4, 13:21)]
res.pca <- PCA(data.active, graph = FALSE)
fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50))
var <- get_pca_var(res.pca)

fviz_pca_var(res.pca, col.var = "black")

library("corrplot")
corrplot(var$cos2, is.corr=FALSE)
corrplot(var$contrib, is.corr=FALSE)    

set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)

fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

ind <- get_pca_ind(res.pca)
ind

fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = F # Avoid text overlapping (slow if many points)
)

res.hcpc <- HCPC(res.pca, graph = FALSE)

#Visualize
fviz_pca_ind(data.pca,
  geom.ind = "point", # show points only (nbut not "text")
  col.ind = data_filtered$length_category, # color by groups
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  addEllipses = TRUE, # Concentration ellipses
  legend.title = "Groups")

#SUmmary of TRACKS for PCA
summary_tracks <- data %>% 
  filter(timepoint < 10) %>%
  group_by(TRACK_ID, rep_no) %>% 
  summarize(mean_intensity = mean(MEAN_INTENSITY), max_intensity = mean(MAX_INTENSITY), sd = sd(normalized_mean_int), length=mean(duration_sec))

#Summary mean intensities for each group
summary_lengths <- data %>% filter(timepoint < 10) %>%
  filter(duration_sec > 0.25) %>%
  group_by(length_category) %>% 
  summarize(mean_intensity = mean(MEAN_INTENSITY), max_intensity = mean(MAX_INTENSITY), sd = sd(normalized_mean_int), length=mean(duration_sec))

library(rstatix)
length_stats <- data %>% filter(timepoint < 10, FRAME < 60) %>%
  group_by(length_category) %>%
  get_summary_stats(MEAN_INTENSITY, type = "mean_sd")

res.aov <- data %>% filter(timepoint < 10, FRAME < 60) %>% anova_test(MEAN_INTENSITY ~ length_category)

pwc_tracks <- length_stats_track %>% 
  pairwise_t_test(mean ~ length_category, p.adjust.method = "bonferroni")
pwc_tracks

library(ggpubr)
pwc <- pwc %>% add_xy_position(x = "length_category")
ggboxplot(data %>% filter(timepoint < 10), x = "length_category", y = "MEAN_INTENSITY") +
  stat_pvalue_manual(pwc, label = "p.adj", tip.length = 0, step.increase = 0.1) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

ggboxplot(data %>% filter(timepoint < 10), x = "length_category", y = "MEAN_INTENSITY") +
  stat_pvalue_manual(pwc, hide.ns = TRUE, label = "p.adj.signif") +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )
