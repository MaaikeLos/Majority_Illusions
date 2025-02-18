##### Version 18-2-2025, Maaike Venema-Los #####

library(dplyr)
library(ggplot2)
library(reshape2)

### Functions ###

# Function to make summaries with medians for plots, where 'weak' is including strict:
make_summary_median<-function(df_grouped){
  summary <- summarize(df_grouped, 
                       median_strict = median(frac_nodes_strict),
                       Q1_strict = quantile(frac_nodes_strict, 0.25),
                       Q3_strict = quantile(frac_nodes_strict, 0.75),
                       median_weak = median(frac_nodes_weak),
                       Q1_weak = quantile(frac_nodes_weak, 0.25),
                       Q3_weak = quantile(frac_nodes_weak, 0.75))
  summary_strict <- subset(summary, select = -c(median_weak, Q1_weak, Q3_weak))
  summary_strict$illusion_type <- 'strict'
  names(summary_strict)[names(summary_strict) == "median_strict"] <- "median"
  names(summary_strict)[names(summary_strict) == "Q1_strict"] <- "Q1"
  names(summary_strict)[names(summary_strict) == "Q3_strict"] <- "Q3"
  summary_weak <- subset(summary, select = -c(median_strict, Q1_strict, Q3_strict))
  summary_weak$illusion_type <- 'weak'
  names(summary_weak)[names(summary_weak) == "median_weak"] <- "median"
  names(summary_weak)[names(summary_weak) == "Q1_weak"] <- "Q1"
  names(summary_weak)[names(summary_weak) == "Q3_weak"] <- "Q3"
  summary <- rbind(summary_strict, summary_weak)
  return(summary)
}

# Function to  make summaries for plots of MSE
make_summary_MSE <- function(df_grouped){
  summary <- summarize(df_grouped, 
                       MSE = mean(MSE, na.rm = TRUE),
                       MSE_no = mean(MSE_no_illusion, na.rm = TRUE),
                       MSE_any = mean(MSE_any_illusion, na.rm = TRUE),
                       MSE_strict = mean(MSE_strict_illusion, na.rm = TRUE))
  summary <-melt(summary, measure.vars = c("MSE", "MSE_no", "MSE_any", "MSE_strict"))
  names(summary)[names(summary) == "variable"] <- "MSE_type"
  names(summary)[names(summary) == "value"] <- "MSE_value"
  return(summary)
}

# Function to  make summaries for plots of the fraction of runs with Mm/Mw/Wm/Ww illusion.
make_summary_WMwmi<- function(df_grouped){
  summary <- summarize(df_grouped, 
                       Wm = mean(WMmi),
                       Mm = mean(Mmi),
                       Ww = mean(WMwmi),
                       Mw = mean(Mwmi)) # mean will give the fraction of trues, since true is 1 and false is 0.
  summary <- melt(summary, measure.vars = c("Wm", "Mm", "Ww", "Mw"))
  # Rename molten columns appropriately:
  names(summary)[names(summary) == "variable"] <- "illusion_type"
  names(summary)[names(summary) == "value"] <- "illusion_fraction"
  return(summary)
}
#special version of above for FB
make_summary_Mwmi<- function(df_grouped){
  summary <- summarize(df_grouped, 
                       Mm = mean(Mmi),
                       Mw = mean(Mwmi)) # mean will give the fraction of trues, since true is 1 and false is 0.
  summary <- melt(summary, measure.vars = c("Mm", "Mw"))
  # Rename molten columns appropriately:
  names(summary)[names(summary) == "variable"] <- "illusion_type"
  names(summary)[names(summary) == "value"] <- "illusion_fraction"
  return(summary)
}


## Function to make summaries for plots with mean and standard deviation:
#make_summary<-function(df_grouped){
#  summary <- summarize(df_grouped, 
#                             mean_strict = mean(frac_nodes_strict),
#                             sd_strict = sd(frac_nodes_strict),
#                             mean_weak = mean(frac_nodes_only_weak),
#                             sd_weak = sd(frac_nodes_only_weak))
#  #There is probably a way to do this with melt
#  summary_strict <- subset(summary, select = -c(mean_weak, sd_weak))
#  summary_strict$illusion_type <- 'strict'
#  names(summary_strict)[names(summary_strict) == "mean_strict"] <- "mean"
#  names(summary_strict)[names(summary_strict) == "sd_strict"] <- "sd"
#  summary_weak <- subset(summary, select = -c(mean_strict, sd_strict))
#  summary_weak$illusion_type <- 'weak'
#  names(summary_weak)[names(summary_weak) == "mean_weak"] <- "mean"
#  names(summary_weak)[names(summary_weak) == "sd_weak"] <- "sd"
#  summary <- rbind(summary_strict, summary_weak)
#  return(summary)
#}
#Function to make summaries with medians for plots, where 'weak' is not including strict:
#make_summary_median_excl_weak<-function(df_grouped){
#  summary <- summarize(df_grouped, 
#                       median_strict = median(frac_nodes_strict),
#                       Q1_strict = quantile(frac_nodes_strict, 0.25),
#                       Q3_strict = quantile(frac_nodes_strict, 0.75),
#                       median_weak = median(frac_nodes_only_weak),
#                       Q1_weak = quantile(frac_nodes_only_weak, 0.25),
#                       Q3_weak = quantile(frac_nodes_only_weak, 0.75))
#  #There is probably a way to do this with melt
#  summary_strict <- subset(summary, select = -c(median_weak, Q1_weak, Q3_weak))
#  summary_strict$illusion_type <- 'strict'
#  names(summary_strict)[names(summary_strict) == "median_strict"] <- "median"
#  names(summary_strict)[names(summary_strict) == "Q1_strict"] <- "Q1"
#  names(summary_strict)[names(summary_strict) == "Q3_strict"] <- "Q3"
#  summary_weak <- subset(summary, select = -c(median_strict, Q1_strict, Q3_strict))
#  summary_weak$illusion_type <- 'weak'
#  names(summary_weak)[names(summary_weak) == "median_weak"] <- "median"
#  names(summary_weak)[names(summary_weak) == "Q1_weak"] <- "Q1"
#  names(summary_weak)[names(summary_weak) == "Q3_weak"] <- "Q3"
#  summary <- rbind(summary_strict, summary_weak)
#  return(summary)
#}
############################################################

### Correlation matrices ###

# Code for heatmap is derived from code via Sophie van Schaik from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
library("Hmisc")

draw_cor_mat <- function(df){
  cormat <- round(cor(df, use='pairwise.complete.obs'),2)
  #Only upper tri:
  cormat[lower.tri(cormat)]<- NA
  #upper_tri <- get_upper_tri(cormat)
  #melted_cormat <- melt(upper_tri, na.rm = TRUE)
  melted_cormat <- melt(cormat, na.rm = TRUE)
  
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    ## labels for ER:
    #scale_x_discrete(labels = c("n", "p_edge", "proportion blue/red globally", "degree assortativity coefficient", "avg. path length", 
    #                            "clustering coefficient", "fraction of nodes in largest comp.", "degree centrality", "eigenvector centrality", 
    #                            "closeness centrality", "betweenness centrality", "homophily", "fraction of nodes under m ill.",
    #                            "fraction of nodes under w ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w ill.",
    #                            "MSE nodes with m ill." )) +
    #scale_y_discrete(labels = c("n", "p_edge", "proportion blue/red globally", "degree assortativity coefficient", "avg. path length", 
    #                            "clustering coefficient", "fraction of nodes in largest comp.", "degree centrality", "eigenvector centrality", 
    #                            "closeness centrality", "betweenness centrality", "homophily", "fraction of nodes under m ill.",
    #                            "fraction of nodes under w ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w ill.",
    #                            "MSE nodes with m ill." )) +
  ## labels for HK:
  #scale_x_discrete(labels = c("n", "m", "p", "proportion blue/red globally", "degree assortativity coefficient", "avg. path length", 
  #                          "clustering coefficient",  "degree centrality", "eigenvector centrality", 
  #                          "closeness centrality", "betweenness centrality", "homophily", "fraction of nodes under m ill.",
  #                          "fraction of nodes under w ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w ill.",
  #                          "MSE nodes with m ill." )) +
  #scale_y_discrete(labels = c("n", "m", "p", "proportion blue/red globally", "degree assortativity coefficient", "avg. path length", 
  #                            "clustering coefficient",  "degree centrality", "eigenvector centrality", 
  #                            "closeness centrality", "betweenness centrality", "homophily", "fraction of nodes under m ill.",
  #                            "fraction of nodes under w ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w ill.",
  #                            "MSE nodes with m ill." )) +
  theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 13, hjust = 1),
          axis.text.y = element_text(size =13))+
    coord_fixed()
  
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 3.5) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal",
      legend.text = element_text(size = 12),
      legend.title = element_text(size =12))+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}

#Adjust sizes and labels for facebook
draw_cor_mat_fb <- function(df){
  cormat <- round(cor(df, use='pairwise.complete.obs'),2)
  #Only upper tri:
  cormat[lower.tri(cormat)]<- NA
  #upper_tri <- get_upper_tri(cormat)
  #melted_cormat <- melt(upper_tri, na.rm = TRUE)
  melted_cormat <- melt(cormat, na.rm = TRUE)
  
  ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "red", high = "blue", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    scale_x_discrete(labels = c("proportion blue/red globally", "homophily", "fraction of nodes under strict ill.", "fraction of nodes under weak ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w/m ill.", "MSE nodes with m ill." )) +
    scale_y_discrete(labels = c("proportion blue/red globally", "homophily", "fraction of nodes under strict ill.", "fraction of nodes under weak ill.", "MSE all nodes", "MSE nodes without ill.",  "MSE nodes with w/m ill.", "MSE nodes with m ill." )) +
    theme_minimal()+ # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 17, hjust = 1),
          axis.text.y = element_text(size =17))+
    coord_fixed()
  
  ggheatmap + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 7) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal",
      legend.text = element_text(size = 15),
      legend.title = element_text(size =17))+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}

df_ER_cormat <- subset(df_ER, select = c(n, p_edge, proportion_blue_global,  deg_assort_coef, avg_path_length, clustering_coefficient, frac_largest_comp, C_degree, C_EV, C_closeness, C_between, homophily, frac_nodes_strict, frac_nodes_weak, MSE, MSE_no_illusion, MSE_any_illusion, MSE_strict_illusion) ) 
draw_cor_mat(df_ER_cormat)
ggsave('Correlation_matrix_ER.pdf', width = 10, height = 10, dpi = 1000)

df_HK_cormat <- subset(df_HK, select = c(n, m, p, proportion_blue_global,  deg_assort_coef, avg_path_length, clustering_coefficient, frac_largest_comp, C_degree, C_EV, C_closeness, C_between, homophily, frac_nodes_strict, frac_nodes_weak, MSE, MSE_no_illusion, MSE_any_illusion, MSE_strict_illusion) ) 
draw_cor_mat(df_HK_cormat)
ggsave('Correlation_matrix_HK.pdf', width = 10, height = 10, dpi = 1000)

df_FB_cormat <- subset(df_FB, select = c(proportion_blue_global, homophily, frac_nodes_strict, frac_nodes_weak, MSE, MSE_no_illusion, MSE_any_illusion, MSE_strict_illusion) ) 
draw_cor_mat_fb(df_FB_cormat)
ggsave('Correlation_matrix_FB.pdf', width = 10, height = 10, dpi = 1000)


# Only large n:
df_HK_cormat <- subset(filter(df_HK, n ==100 | n == 101), select = c(n, m, p, proportion_blue_global,  deg_assort_coef, avg_path_length, clustering_coefficient, frac_largest_comp, C_degree, C_EV, C_closeness, C_between, homophily, frac_nodes_strict, frac_nodes_weak, MSE, MSE_no_illusion, MSE_any_illusion, MSE_strict_illusion) ) 
draw_cor_mat(df_HK_cormat)
ggsave('Correlation_matrix_HK_large_n.png', width = 10, height = 10, dpi = 400)
df_ER_cormat <- subset(filter(df_ER, n ==100 | n == 101), select = c(n, p_edge, proportion_blue_global,  deg_assort_coef, avg_path_length, clustering_coefficient, frac_largest_comp, C_degree, C_EV, C_closeness, C_between, homophily, frac_nodes_strict, frac_nodes_weak, MSE, MSE_no_illusion, MSE_any_illusion, MSE_strict_illusion) ) 
draw_cor_mat(df_ER_cormat)
ggsave('Correlation_matrix_ER_large_n.png', width = 10, height = 10, dpi = 400)

#Clean variable space:
rm(df_ER_cormat, df_HK_cormat, df_FB_cormat)
rm(draw_cor_mat, draw_cor_mat_fb)
detach(package:Hmisc) #because Hmisc has other summarize function than dplyr

######################################################################################################################################


### General results ###

## Fraction of nodes under illusion
summary_frac_ill_ER <- make_summary_median(df_ER)
summary_frac_ill_ER$graph <- "Erdös-Rényi"
summary_frac_ill_HK <- make_summary_median(df_HK)
summary_frac_ill_HK$graph <- "Holme-Kim"
summary_frac_ill_fb <- make_summary_median(df_FB)
summary_frac_ill_fb$graph <- "Facebook"
summary_frac_ill <- rbind(summary_frac_ill_ER, summary_frac_ill_HK, summary_frac_ill_fb)
ggplot(summary_frac_ill, aes(x=graph)) +
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under m/w illusion")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "illusion type", labels = c("m illusion", "w illusion"))+
  scale_x_discrete(name = "graph type") +
  scale_y_continuous(name = " fraction of nodes under illusion") +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
  )
ggsave(
  "Median_fraction_nodes_illusion.pdf",
  dpi = 600, 
  width = 8, height = 5
)

## MSE
summary_MSE_ER <- make_summary_MSE(df_ER)
summary_MSE_ER$graph <- "Erdös-Rényi"
summary_MSE_HK <- make_summary_MSE(df_HK)
summary_MSE_HK$graph <- "Holme-Kim"
summary_MSE_fb <- make_summary_MSE(df_FB)
summary_MSE_fb$graph <- "Facebook"
summary_MSE <- rbind(summary_MSE_ER, summary_MSE_HK, summary_MSE_fb)

ggplot(summary_MSE, aes(x=graph, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  labs(title = "Mean squared error of different groups of nodes")+
  scale_y_continuous(name = "mean squared error") +
  scale_x_discrete(name = "graph type")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "group", labels = c("all nodes", "nodes without illusion", "nodes with w illusion", "nodes with m illusion"))+
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold")
  )
ggsave('MSE.pdf', width = 8, height =5, dpi = 1000)

# Fraction of runs under global illusion
summary_WMwmi_ER <- make_summary_WMwmi(df_ER)
summary_WMwmi_ER$graph <- "Erdös-Rényi"
summary_WMwmi_HK <- make_summary_WMwmi(df_HK)
summary_WMwmi_HK$graph <- "Holme-Kim"
summary_Mwmi_fb <- make_summary_Mwmi(df_FB)
summary_Mwmi_fb$graph <- "Facebook"
summary_WMwmi <- rbind(summary_WMwmi_ER, summary_WMwmi_HK, summary_Mwmi_fb)

ggplot(summary_WMwmi, aes(x=graph, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "fraction of networks under global illusion") +
  scale_x_discrete(name = "graph type")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"), name = "illusion type")+
  ggtitle("Fraction of runs with (Weak-)Majority-(weak-)majority illusion") +
  theme(axis.text = element_text(size=15),
        axis.title = element_text(size = 15, face = "bold"),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 15, face = "bold"),
        plot.title = element_text(size = 15, face = "bold")
  )
ggsave('WMwmi.pdf',
       dpi = 1000, 
       width = 8, height = 5
)

# Clean variable space:
rm(summary_frac_ill, summary_frac_ill_ER, summary_frac_ill_HK, summary_frac_ill_fb, summary_MSE, summary_MSE_ER, summary_MSE_HK, summary_MSE_fb, summary_Mwmi_fb, summary_WMwmi, summary_WMwmi_ER, summary_WMwmi_HK)

##############################################################################################################################

### Relation of graph parameters to majority illusions ### 

###############################
##  All parameters together  ##
###############################

# Fraction nodes under illusion versus n, p_edge/m, and the proportion of blue/red globally, ER
df_ER_grouped_general <- group_by(df_ER, n_combined, p_edge, blue_global_bins)
summary_ER_general <- make_summary_median(df_ER_grouped_general)

ggplot(summary_ER_general, aes(x=blue_global_bins))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 0.5, linewidth = 1)+
  facet_grid(p_edge~n_combined)+
  #labs(title = "Median (and IQR) fraction of nodes under m/w illusion (ER graphs)\n n")+
  labs(title = "n")+ # Without title for paper
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type", labels = c('m illusion', 'w illusion'))+
  scale_x_discrete(name = "Proportion blue/red globally", labels = c("0-.1", ".1-.2", ".2-.3", ".3-.4", ".4-.5")) +
  scale_y_continuous(name = "Median fraction of nodes under illusion", sec.axis = sec_axis(~ . , name = "p_edge", breaks = NULL, labels = NULL)) +
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 23, face = "bold"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        title = element_text(size = 24, face ="bold")
  )
ggsave('Median_frac_illusion_ER.pdf', width = 17, height = 10, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, ER
summary_ER_WMwmi <- make_summary_WMwmi(df_ER_grouped_general)
ggplot(summary_ER_WMwmi, aes(x=blue_global_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  facet_grid(p_edge~n_combined)+
  scale_y_continuous(name = "Fraction of networks under global illusion", sec.axis = sec_axis(~ . , name = "p_edge", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))+
  ggtitle("n") 
ggsave('WMwmi_ER.pdf', width = 15, height = 9, dpi = 1000)

#MSE, ER
df_ER_grouped_general <- group_by(df_ER, n, p_edge, blue_global_bins) #Cannot use n-combined here, since the group sizes are not equal.
summary_ER_MSE <- make_summary_MSE(df_ER_grouped_general)
ggplot(summary_ER_MSE, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) +
  geom_col(position = position_dodge())+
  facet_grid(p_edge~n)+
  scale_y_continuous(name = "Mean Squared Error of all nodes", sec.axis = sec_axis(~ . , name = "p_edge", breaks = NULL, labels = NULL)) +
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"))+
  scale_x_discrete(name = "Proportion blue/red globally")+
  ggtitle("n") 
ggsave('MSE_ER.pdf', width = 15, height = 9, dpi = 1000)


# Fraction nodes under illusion versus n, p_edge/m, and the proportion of blue/red globally, HK
df_HK_grouped_general <- group_by(df_HK, n_combined, m_relative, blue_global_bins)
summary_HK_general <- make_summary_median(df_HK_grouped_general)
ggplot(summary_HK_general, aes(x=blue_global_bins))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 0.5, linewidth = 1)+
  facet_grid(m_relative~n_combined)+
  #labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)\n n")+
  labs(title = "n")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type", labels = c('m illusion', 'w illusion'))+
  scale_x_discrete(name = "Proportion blue/red globally", labels = c("0-.1", ".1-.2", ".2-.3", ".3-.4", ".4-.5")) +
  scale_y_continuous(name = "Median fraction of nodes under illusion", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 23, face = "bold"),
        strip.text.x = element_text(size = 20),
        strip.text.y = element_text(size = 20),
        title = element_text(size = 24, face ="bold")
  )
ggsave('Median_frac_illusion_HK.pdf', width = 17, height = 10, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, HK
summary_HK_WMwmi <- make_summary_WMwmi(df_HK_grouped_general)
ggplot(summary_HK_WMwmi, aes(x=blue_global_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  facet_grid(m_relative~n_combined)+
  scale_y_continuous(name = "Fraction of networks under global illusion", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "Proportion blue/red globally")+
  ggtitle("n") +
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_HK.pdf',
       dpi = 600, 
       width = 15, height = 10
)

#MSE, HK
df_HK_grouped_general <- group_by(df_HK, n, m_relative, blue_global_bins) #Cannot use n-combined here, since the group sizes are not equal.
#Since p is not informative, we left it out here. Similar plots that do take into account p can be easily generated with the same template.
summary_HK_MSE <- make_summary_MSE(df_HK_grouped_general)
ggplot(summary_HK_MSE, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) +
  geom_col(position = position_dodge())+
  facet_grid(m_relative~n)+
  scale_y_continuous(name = "Mean Squared Error", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))+
  ggtitle("n") 
ggsave('MSE_HK.pdf', width = 15, height = 9, dpi = 1000)
# with only even n: (For paper)
summary_HK_MSE_even <- filter(summary_HK_MSE, n%%2 == 0)
ggplot(summary_HK_MSE_even, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) +
  geom_col(position = position_dodge())+
  facet_grid(m_relative~n)+
  ggtitle("n")+
  scale_y_continuous(name = "Mean Squared Error", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "nodes without illusion", "nodes with w illusion", "nodes with m illusion")) +
  theme(axis.text = element_text(size=13),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 24, face = "bold"),
        legend.text = element_text(size = 17),
        legend.title = element_text(size = 20, face = "bold"),
        strip.text.x = element_text(size = 18),
        strip.text.y = element_text(size = 18),
        title = element_text(size = 24, face ="bold")
  )
ggsave('MSE_HK_even.pdf', width = 15, height = 9, dpi = 1000)

# Clean variable space:
rm(df_ER_grouped_general, summary_ER_general, df_HK_grouped_general, summary_HK_general)
rm(summary_ER_MSE, summary_ER_WMwmi, summary_HK_MSE, summary_HK_MSE_even, summary_HK_WMwmi)

# Without m and p_edge, to compare ER with HK:
#df_ER_grouped_general <- group_by(df_ER, n_combined, blue_global_bins)
#summary_ER_general <- make_summary(df_ER_grouped_general)
#ggplot(summary_ER_general, aes(x=blue_global_bins, y = mean, fill = illusion_type)) +
#  geom_col(position = position_dodge())+
#  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.3,position=position_dodge(.9)) +
#  facet_grid(~n_combined )+
#  scale_y_continuous(name = "Fraction of nodes under illusion") +
#  scale_x_discrete(name = "Proportion blue/red globally")+
#  scale_fill_manual(values = c( "tomato3", "paleturquoise3"))+
#  ggtitle("n") 
#ggsave('Mean_frac_illusion_ER_tocompare.pdf', width = 15, height = 9, dpi = 1000)

#df_HK_grouped_general <- group_by(df_HK, n_combined, blue_global_bins)
#summary_HK_general <- make_summary(df_HK_grouped_general)
#ggplot(summary_HK_general, aes(x=blue_global_bins, y = mean, fill = illusion_type)) +
#  geom_col(position = position_dodge())+
#  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.3,position=position_dodge(.9)) +
#  facet_grid(~n_combined)+
#  scale_y_continuous(name = "Fraction of nodes under illusion") +
#  scale_x_discrete(name = "Proportion blue/red globally")+
#  scale_fill_manual(values = c( "tomato3", "paleturquoise3"))+
#  ggtitle("n") 
#ggsave('Mean_frac_illusion_HK_tocompare.pdf', width = 15, height = 9, dpi = 1000)



### Per variable to see effects clearer ###

#########
##  n  ##
#########

# median fraction of nodes under illusion, ER
df_ER_grouped_n <- group_by(df_ER, n_combined)
summary_ER_n<- make_summary_median(df_ER_grouped_n)
ggplot(summary_ER_n, aes(x=n_combined))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "n") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_ER_n.pdf', width = 15, height = 9, dpi = 1000)

#ggplot(summary_ER_n, aes(x=n_combined, y = mean, fill = illusion_type)) +
#  geom_col(position = position_dodge())+
#  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.3,position=position_dodge(.9)) +
#  scale_y_continuous(name = "Fraction of nodes under illusion") +
#  scale_x_discrete(name = "n")+
#  scale_fill_manual(values = c( "tomato3", "paleturquoise3"))
#ggsave('Mean_frac_illusion_ER_n.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, ER
summary_ER_WMwmi_n <-make_summary_WMwmi(df_ER_grouped_n)
ggplot(summary_ER_WMwmi_n, aes(x=n_combined, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "n")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_n_ER.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE ER
df_ER_grouped_n <- group_by(df_ER, n) #Cannot use n-combined here, since the group sizes are not equal.
summary_ER_MSE_n <- make_summary_MSE(df_ER_grouped_n)
ggplot(summary_ER_MSE_n, aes(x=factor(n), y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error of all nodes") +
  scale_x_discrete(name = "n")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave("MSE_n_ER.pdf",
       dpi = 600, 
       width = 15, height = 10
)

# median fraction of nodes under illusion, HK
df_HK_grouped_n <- group_by(df_HK, n_combined)
summary_HK_n<- make_summary_median(df_HK_grouped_n)
ggplot(summary_HK_n, aes(x=n_combined))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "n") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_HK_n.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, HK
summary_HK_WMwmi_n <-make_summary_WMwmi(df_HK_grouped_n)
ggplot(summary_HK_WMwmi_n, aes(x=n_combined, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "n")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_n_HK.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE HK
df_HK_grouped_n <- group_by(df_HK, n) #Cannot use n-combined here, since the group sizes are not equal.
summary_HK_MSE_n <- make_summary_MSE(df_HK_grouped_n)
ggplot(summary_HK_MSE_n, aes(x=factor(n), y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "n")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_n_HK.pdf', width = 15, height = 9, dpi = 1000)

# Clean variable space:
rm(df_ER_grouped_n, summary_ER_n, summary_ER_WMwmi_n, summary_ER_MSE_n, df_HK_grouped_n, summary_HK_n, summary_HK_WMwmi_n, summary_HK_MSE_n)


#######################################
##  Proportion of blue/red globally  ##
#######################################

# median fraction of nodes under illusion, ER
df_ER_grouped_bg <- group_by(df_ER, blue_global_bins)
summary_ER_bg<- make_summary_median(df_ER_grouped_bg)
ggplot(summary_ER_bg, aes(x=blue_global_bins))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "Proportion blue/red globally") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_ER_bg.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, ER
summary_ER_WMwmi_bg <-make_summary_WMwmi(df_ER_grouped_bg)
ggplot(summary_ER_WMwmi_bg, aes(x=blue_global_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_Propglobal_ER.pdf', width = 15, height = 9, dpi = 1000)

# MSE ER
summary_ER_MSE_bg <- make_summary_MSE(df_ER_grouped_bg)
ggplot(summary_ER_MSE_bg, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_blueglobal_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_bg <- group_by(df_HK, blue_global_bins)
summary_HK_bg<- make_summary_median(df_HK_grouped_bg)
ggplot(summary_HK_bg, aes(x=blue_global_bins))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "Proportion blue/red globally") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_HK_bg.pdf', width = 15, height = 9, dpi = 1000)

# MSE HK
summary_HK_MSE_bg <- make_summary_MSE(df_HK_grouped_bg)
ggplot(summary_HK_MSE_bg, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_blueglobal_HK.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, HK
summary_HK_WMwmi_bg <-make_summary_WMwmi(df_HK_grouped_bg)
ggplot(summary_HK_WMwmi_bg, aes(x=blue_global_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_Propglobal_HK.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# median fraction of nodes under illusion, FB
df_FB_grouped <- group_by(df_FB, blue_global_bins)
summary_fb <- make_summary_median(df_FB_grouped)
ggplot(summary_fb, aes(x=blue_global_bins))+
  geom_bar(data = df_FB_grouped, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (FB)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "Proportion blue/red globally") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_FB.pdf', width = 15, height = 9, dpi = 1000)

## MSE FB
summary_fb_MSE <- make_summary_MSE(df_FB_grouped)
ggplot(summary_fb_MSE, aes(x=blue_global_bins, y = MSE_value, fill = MSE_type)) +
  geom_col(position = position_dodge()) +
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", label = c("all nodes", "with no ill.", "with weak/strict ill.", "with strict ill."))+
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_y_continuous(name = "Mean squared error")+
  ggtitle("Mean squared error per group per proportion blue/red globally (fb)") 
ggsave('MSE_fb.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, FB
summary_fb_Mwmi <- make_summary_Mwmi(df_FB_grouped)
ggplot(summary_fb_Mwmi, aes(x=blue_global_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "Proportion blue/red globally")+
  scale_fill_manual(values = c("tomato4", "tomato3", "paleturquoise3", "paleturquoise4"), name = "Illusion type")+
  ggtitle("Fraction of runs with MAJORITY-(weak-)majority illusion (FB network)") 
ggsave('Mwmi_fb.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# Clean variable space:
rm(df_ER_grouped_bg, summary_ER_bg, summary_ER_WMwmi_bg, summary_ER_MSE_bg, df_HK_grouped_bg, summary_HK_bg, summary_HK_WMwmi_bg, summary_HK_MSE_bg, df_FB_grouped, summary_fb, summary_fb_Mwmi, summary_fb_MSE)


##############
##  p_edge  ##
##############

# median fraction of nodes under illusion, ER
df_ER_grouped_pe <- group_by(df_ER, p_edge)
summary_ER_pe<- make_summary_median(df_ER_grouped_pe)
ggplot(summary_ER_pe, aes(x=factor(p_edge)))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "p_edge") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_ER_pedge.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, ER
summary_ER_WMwmi_pe <-make_summary_WMwmi(df_ER_grouped_pe)
ggplot(summary_ER_WMwmi_pe, aes(x=factor(p_edge), y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "p_edge")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_pedge_ER.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE, ER
summary_ER_MSE_pe <- make_summary_MSE(df_ER_grouped_pe)
ggplot(summary_ER_MSE_pe, aes(x=factor(p_edge), y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "p_edge")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_pedge_ER.pdf', width = 15, height = 9, dpi = 1000)

# Clean variable space:
rm(df_ER_grouped_pe, summary_ER_pe, summary_ER_WMwmi_pe, summary_ER_MSE_pe)

#########
##  m  ##
#########

# median fraction of nodes under illusion, HK
df_HK_grouped_m <- group_by(df_HK, m_relative)
summary_HK_m<- make_summary_median(df_HK_grouped_m)
ggplot(summary_HK_m, aes(x=m_relative))+
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "m") +
  scale_y_continuous(name = "Median fraction of nodes under illusion")
ggsave('Median_frac_illusion_HK_m.pdf', width = 15, height = 9, dpi = 1000)

#fraction with Mm/Mw/Wm/Ww illusion, HK
summary_HK_WMwmi_m <-make_summary_WMwmi(df_HK_grouped_m)
ggplot(summary_HK_WMwmi_m, aes(x=m_relative, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "m")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_m_HK.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE, HK
summary_HK_MSE_m <- make_summary_MSE(df_HK_grouped_m)
ggplot(summary_HK_MSE_m, aes(x=m_relative, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "m")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_m_HK.pdf', width = 15, height = 9, dpi = 1000)

# Clean variable space:
rm(df_HK_grouped_m, summary_HK_m, summary_HK_WMwmi_m, summary_HK_MSE_m)


###########################################################

### Relation of other graph properties to majority illusions ###

##############################################
##  Centrality measures: degree centrality  ##
###############################################

# median fraction of nodes under illusion, ER
df_ER_grouped_C_degree <- group_by(df_ER, C_degree_bins)
summary_ER_C_degree <- make_summary_median(df_ER_grouped_C_degree)
ggplot(summary_ER_C_degree, aes(x=C_degree_bins)) +
  geom_bar(data = df_ER_grouped_C_degree, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "degree centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-degree_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE ER
summary_ER_MSE_C_degree <- make_summary_MSE(df_ER_grouped_C_degree)
ggplot(summary_ER_MSE_C_degree, aes(x=C_degree_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Degree centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_degree_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_C_degree <- group_by(df_HK, C_degree_bins)
summary_HK_C_degree <- make_summary_median(df_HK_grouped_C_degree)
ggplot(summary_HK_C_degree, aes(x=C_degree_bins)) +
  geom_bar(data = df_HK_grouped_C_degree, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"), name = "Illusion type")+
  scale_x_discrete(name = "degree centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")+
  theme(axis.text = element_text(size=12),
        axis.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 18),
        plot.title = element_text(size = 15, face = "bold"))
ggsave(
  "Median_fraction_per_C-degree_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, HK
summary_HK_MSE_C_degree <- make_summary_MSE(df_HK_grouped_C_degree)
ggplot(summary_HK_MSE_C_degree, aes(x=C_degree_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Degree centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_degree_HK.pdf', width = 15, height = 9, dpi = 1000)

#Clean variable space:
rm(df_ER_grouped_C_degree, df_HK_grouped_C_degree, summary_ER_C_degree, summary_HK_C_degree, summary_ER_MSE_C_degree, summary_HK_MSE_C_degree)


###################################################
##  Centrality measures: betweenness centrality  ##
###################################################

# median fraction of nodes under illusion, ER
df_ER_grouped_C_between <- group_by(df_ER, C_between_bins)
summary_ER_C_between <- make_summary_median(df_ER_grouped_C_between)
ggplot(summary_ER_C_between, aes(x=C_between_bins)) +
  geom_bar(data = df_ER_grouped_C_between, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "betweenness centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-between_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, ER
summary_ER_MSE_C_between <- make_summary_MSE(df_ER_grouped_C_between)
ggplot(summary_ER_MSE_C_between, aes(x=C_between_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Betweenness centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_between_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_C_between <- group_by(df_HK, C_between_bins)
summary_HK_C_between <- make_summary_median(df_HK_grouped_C_between)
ggplot(summary_HK_C_between, aes(x=C_between_bins)) +
  geom_bar(data = df_HK_grouped_C_between, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "betweenness centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-between_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, HK:
summary_HK_MSE_C_between <- make_summary_MSE(df_HK_grouped_C_between)
ggplot(summary_HK_MSE_C_between, aes(x=C_between_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Betweenness centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_between_HK.pdf', width = 15, height = 9, dpi = 1000)


#Clean variable space:
rm(df_ER_grouped_C_between, df_HK_grouped_C_between, summary_ER_C_between, summary_HK_C_between, summary_ER_MSE_C_between, summary_HK_MSE_C_between)

#################################################
##  Centrality measures: closeness centrality  ##
#################################################

# median fraction of nodes under illusion, ER
df_ER_grouped_C_closeness <- group_by(df_ER, C_closeness_bins)
summary_ER_C_closeness <- make_summary_median(df_ER_grouped_C_closeness)
ggplot(summary_ER_C_closeness, aes(x=C_closeness_bins)) +
  geom_bar(data = df_ER_grouped_C_closeness, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "closeness centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-closeness_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, ER
summary_ER_MSE_C_closeness <- make_summary_MSE(df_ER_grouped_C_closeness)
ggplot(summary_ER_MSE_C_closeness, aes(x=C_closeness_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Closeness centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_closeness_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_C_closeness <- group_by(df_HK, C_closeness_bins)
summary_HK_C_closeness <- make_summary_median(df_HK_grouped_C_closeness)
ggplot(summary_HK_C_closeness, aes(x=C_closeness_bins)) +
  geom_bar(data = df_HK_grouped_C_closeness, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "closeness centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-closeness_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, HK:
summary_HK_MSE_C_closeness <- make_summary_MSE(df_HK_grouped_C_closeness)
ggplot(summary_HK_MSE_C_closeness, aes(x=C_closeness_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Closeness centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_closeness_HK.pdf', width = 15, height = 9, dpi = 1000)

#Clean variable space:
rm(df_ER_grouped_C_closeness, df_HK_grouped_C_closeness, summary_ER_C_closeness, summary_HK_C_closeness, summary_ER_MSE_C_closeness, summary_HK_MSE_C_closeness)


###################################################
##  Centrality measures: Eigenvector centrality  ##
###################################################

# median fraction of nodes under illusion, ER
df_ER_grouped_C_EV <- group_by(df_ER, C_EV_bins)
summary_ER_C_EV <- make_summary_median(df_ER_grouped_C_EV)
ggplot(summary_ER_C_EV, aes(x=C_EV_bins)) +
  geom_bar(data = df_ER_grouped_C_EV, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "Eigenvector centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-EV_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, ER
summary_ER_MSE_C_EV  <- make_summary_MSE(df_ER_grouped_C_EV)
ggplot(summary_ER_MSE_C_EV, aes(x=C_EV_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Eigenvector centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_EV_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_C_EV <- group_by(df_HK, C_EV_bins)
summary_HK_C_EV <- make_summary_median(df_HK_grouped_C_EV)
ggplot(summary_HK_C_EV, aes(x=C_EV_bins)) +
  geom_bar(data = df_HK_grouped_C_EV, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "Eigenvector centrality") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_C-EV_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

# MSE, HK:
summary_HK_MSE_C_EV <- make_summary_MSE(df_HK_grouped_C_EV)
ggplot(summary_HK_MSE_C_EV, aes(x=C_EV_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "Eigenvector centrality")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_C_EV_HK.pdf', width = 15, height = 9, dpi = 1000)

#Clean variable space:
rm(df_ER_grouped_C_EV, df_HK_grouped_C_EV, summary_ER_C_EV, summary_HK_C_EV, summary_ER_MSE_C_EV, summary_HK_MSE_C_EV)

#################
##  Homophily  ##
#################

# median fraction of nodes under illusion, ER
df_ER_grouped_homophily <- group_by(df_ER, homophily_bins)
summary_ER_hom <- make_summary_median(df_ER_grouped_homophily)
ggplot(summary_ER_hom, aes(x=homophily_bins)) +
  geom_bar(data = df_ER_grouped_homophily, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
#  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(name ="Illusion type", values = c( "tomato3", "paleturquoise3"), labels = c("m illusion", "w illusion"))+
  scale_x_discrete(name = "homophily") +
  scale_y_continuous(name = "median fraction of nodes under illusion")+
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave(
  "Median_fraction_per_homophily_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

#fraction with Mm/Mw/Wm/Ww illusion, ER
summary_ER_WMwmi_hom <-make_summary_WMwmi(df_ER_grouped_homophily)
ggplot(summary_ER_WMwmi_hom, aes(x=homophily_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "homophily")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_homophily_ER.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE ER
summary_ER_MSE_hom <- make_summary_MSE(df_ER_grouped_homophily)
ggplot(summary_ER_MSE_hom, aes(x=homophily_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "mean squared error") +
  scale_x_discrete(name = "homophily")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "nodes without illusion", "nodes with w illusion", "nodes with m illusion")) +
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave('MSE_homophily_ER.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, HK
df_HK_grouped_homophily <- group_by(df_HK, homophily_bins)
summary_HK_hom <- make_summary_median(df_HK_grouped_homophily)
ggplot(summary_HK_hom, aes(x=homophily_bins)) +
  geom_bar(data = df_HK_grouped_homophily, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  # labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_color_manual(name ="Illusion type" , values = c( "tomato3", "paleturquoise3"), labels = c("m illusion", "w illusion"))+
  scale_x_discrete(name = "homophily") +
  scale_y_continuous(name = "median fraction of nodes under illusion")+
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave(
  "Median_fraction_per_homophily_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

#fraction with Mm/Mw/Wm/Ww illusion, HK
summary_HK_WMwmi_hom <-make_summary_WMwmi(df_HK_grouped_homophily)
ggplot(summary_HK_WMwmi_hom, aes(x=homophily_bins, y = illusion_fraction, fill = illusion_type)) +
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Fraction of networks under global illusion") +
  scale_x_discrete(name = "homophily")+
  scale_fill_manual(values = c( "tomato3", "tomato4", "paleturquoise3", "paleturquoise4"))
ggsave('WMwmi_homophily_HK.pdf',
       dpi = 600, 
       width = 15, height = 10
)

# MSE HK
summary_HK_MSE_hom <- make_summary_MSE(df_HK_grouped_homophily)
ggplot(summary_HK_MSE_hom, aes(x=homophily_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "homophily")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "nodes without illusion", "nodes with w illusion", "nodes with m illusion")) +
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave('MSE_homophily_HK.pdf', width = 15, height = 9, dpi = 1000)

# median fraction of nodes under illusion, FB
df_FB_grouped_homophily <- group_by(df_FB, homophily_bins)
summary_fb_hom <- make_summary_median(df_FB_grouped_homophily)
ggplot(summary_fb_hom, aes(x=homophily_bins)) +
  geom_bar(data = df_FB_grouped_homophily, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (facebook network)")+
  scale_color_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "homophily") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_homophily_fb_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

#MSE FB
summary_fb_MSE_hom <- make_summary_MSE(df_FB_grouped_homophily)
ggplot(summary_fb_MSE_hom, aes(x=homophily_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "homophily")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_homophily_fb.pdf', width = 15, height = 9, dpi = 1000)


#Clean variable space:
rm(df_ER_grouped_homophily, df_HK_grouped_homophily, df_FB_grouped_homophily, summary_ER_hom, summary_HK_hom, summary_fb_hom, summary_ER_WMwmi_hom, summary_ER_MSE_hom, summary_HK_WMwmi_hom, summary_HK_MSE_hom, summary_fb_MSE_hom)


########################################
##  Degree assortativity coefficient  ##
########################################

#First check how is the distribution of datapoints (ER):
ggplot(df_ER, aes(x=deg_assort_coef))+
  geom_density()
#ggsave("Distribution_over_deg_assort_coef.png")
ggplot(df_ER, aes(x=deg_assort_coef_bins))+
  geom_bar()+
  geom_text(stat = 'count', aes(label=after_stat(count)), vjust=-1)
# Looks like a normal distribution

# median fraction of nodes under illusion, ER
df_ER_grouped_deg_assort <- group_by(df_ER, deg_assort_coef_bins)
summary_ER_deg_assort <- make_summary_median(df_ER_grouped_deg_assort)
ggplot(summary_ER_deg_assort, aes(x=deg_assort_coef_bins)) +
  geom_bar(data = df_ER_grouped_deg_assort, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
#  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (ER graphs)")+
  scale_color_manual(name = "Illusion type", values = c( "tomato3", "paleturquoise3"), labels = c("m illusion", "w illusion"))+
  scale_x_discrete(name = "degree assortativity coefficient") +
  scale_y_continuous(name = "median fraction of nodes under illusion")+
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave(
  "Median_fraction_per_deg_assort_coef_ER_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

#MSE ER
summary_ER_MSE_dac <- make_summary_MSE(df_ER_grouped_deg_assort)
ggplot(summary_ER_MSE_dac, aes(x=deg_assort_coef_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "degree assortativity coefficient")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "nodes without illusion", "nodes with w illusion", "nodes with m illusion")) +
  theme(axis.text = element_text(size=20),
        axis.text.x = element_text(angle =90, hjust = 1),
        axis.title = element_text(size = 25, face = "bold"),
        legend.text = element_text(size = 23),
        legend.title = element_text(size = 25, face = "bold")
  )
ggsave('MSE_deg_assort_ER.pdf', width = 15, height = 9, dpi = 1000)

#First check how is the distribution of datapoints (HK):
ggplot(df_HK, aes(x=deg_assort_coef))+
  geom_density()
#ggsave("Distribution_over_deg_assort_coef.png")
ggplot(df_HK, aes(x=deg_assort_coef_bins))+
  geom_bar()+
  geom_text(stat = 'count', aes(label=after_stat(count)), vjust=-1)
# This is a strange distribution, and there are hardly any positive datapoints.

# median fraction of nodes under illusion, HK
df_HK_grouped_deg_assort <- group_by(df_HK, deg_assort_coef_bins)
summary_HK_deg_assort <- make_summary_median(df_HK_grouped_deg_assort)
ggplot(summary_HK_deg_assort, aes(x=deg_assort_coef_bins)) +
  geom_bar(data = df_HK_grouped_deg_assort, aes(y = (..count..)/sum(..count..)), fill = 'gray80')+ #This is the division of the data
  geom_pointrange(aes(y=median, ymin = Q1, ymax = Q3, color = illusion_type), position = position_dodge(width = 0.5), size = 1, linewidth = 1)+
  labs(title = "Median (and IQR) fraction of nodes under strict / only weak illusion (HK graphs)")+
  scale_fill_manual(values = c( "tomato3", "paleturquoise3"))+
  scale_x_discrete(name = "degree assortativity coefficient") +
  scale_y_continuous(name = "median fraction of nodes under illusion")
ggsave(
  "Median_fraction_per_deg_assort_coef_HK_points.pdf",
  dpi = 600, 
  width = 15, height = 10
)

#MSE HK
summary_HK_MSE_dac <- make_summary_MSE(df_HK_grouped_deg_assort)
ggplot(summary_HK_MSE_dac, aes(x=deg_assort_coef_bins, y = MSE_value, fill = MSE_type)) + 
  geom_col(position = position_dodge())+
  scale_y_continuous(name = "Mean Squared Error") +
  scale_x_discrete(name = "degree assortativity coefficient")+
  scale_fill_manual(values = c("black", "grey", "paleturquoise3", "tomato3"), name = "Group", labels = c("all nodes", "no illusion", "weak or strict ill.", "strict ill."))
ggsave('MSE_deg_assort_HK.pdf', width = 15, height = 9, dpi = 1000)


#Clean variable space:
rm(df_ER_grouped_deg_assort, df_HK_grouped_deg_assort, summary_ER_deg_assort, summary_HK_deg_assort, summary_ER_MSE_dac, summary_HK_MSE_dac)

##################################################


