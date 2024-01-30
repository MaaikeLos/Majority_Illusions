library(ggplot2)
library(ggrepel) # For labeling the points in a scatter plot

####
df_ER <- read.csv("ER_data.csv", header = TRUE, sep = ';')
View(df_ER)

#Are there graphs that consist of more than one component?
any(df_ER$frac_largest_comp != 1) # Gives TRUE: yes.

df_ER$p_blue <- factor(df_ER$p_blue, labels = c('.1', '.2', '.3', '.4', '.5'))
df_ER$p_edge <- factor(df_ER$p_edge)


### Fraction of nodes under illusion: 

# First create dataframe with the data for strict and weak illusions combined:
df_ER_combined_weak <- df_ER
df_ER_combined_strict <- df_ER
df_ER_combined_weak$combined <- 'weak'
df_ER_combined_strict$combined <- 'strict'
df_ER_combined <- rbind(df_ER_combined_weak, df_ER_combined_strict)
rm(df_ER_combined_strict)
rm(df_ER_combined_weak)
df_ER_combined$nr_nodes_ill <- df_ER_combined$nr_nodes_str_ill
df_ER_combined$nr_nodes_ill <- replace(df_ER_combined$nr_nodes_ill, df_ER_combined$combined == 'weak', df_ER_combined$nr_nodes_weak_ill)

# Plot the boxplots of the fraction of nodes under strict/weak illusion, for every value of n (horizontal facets), p_edge, and p_blue.
ggplot(df_ER_combined, aes(x=p_blue, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  facet_grid(p_edge ~ n) +
  scale_y_continuous(name = "Fraction of nodes under illusion", sec.axis = sec_axis(~ . , name = "p_edge", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "p_blue") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=12))

## Separate plots for different parameters:

#All values of p_blue together, all values of p_edge together (=influence of n):
ggplot(df_ER_combined, aes(x=factor(n), y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  #facet_grid(~n) +
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "n") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))

#All values of p_blue together, all values of n together (=influence of p_edge):
ggplot(df_ER_combined, aes(x=p_edge, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  #facet_grid(~n) +
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "p_edge") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))

#All values of p_edge together, all values of n together (=influence of p_blue):
ggplot(df_ER_combined, aes(x=p_blue, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  #facet_grid(~n) +
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "p_blue") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))


### Fraction of runs with majority-majority-illusion:

# First make a new column with the mmi values we want to measure 1 and the rest 0, and create a variable with the averages:
#wmmi
df_ER$wmmi <- replace(df_ER$mi, df_ER$mi == 'wmmi' | df_ER$mi == 'mmi', 1)
df_ER$wmmi <- replace(df_ER$wmmi, df_ER$wmmi == 'no_strict', 0)
df_ER<-transform(df_ER, wmmi = as.numeric((wmmi)))
averages_ER_wmmi <- aggregate(wmmi ~ n + p_edge + p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
#mmi
df_ER$mmi <- replace(df_ER$mi, df_ER$mi == 'mmi', 1)
df_ER$mmi <- replace(df_ER$mmi, df_ER$mmi == 'wmmi' | df_ER$mmi == 'no_strict', 0)
df_ER<-transform(df_ER, mmi = as.numeric(mmi))
averages_ER_mmi <- aggregate(mmi ~ n + p_edge + p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
#wmwmi
df_ER$wmwmi <- replace(df_ER$wmi, df_ER$wmi == 'wmwmi' | df_ER$wmi == 'mwmi', 1)
df_ER$wmwmi <- replace(df_ER$wmwmi, df_ER$wmwmi == 'no_weak', 0)
df_ER<-transform(df_ER, wmwmi = as.numeric((wmwmi)))
averages_ER_wmwmi <- aggregate(wmwmi ~ n + p_edge + p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
#mwmi
df_ER$mwmi <- replace(df_ER$wmi, df_ER$wmi == 'mwmi', 1)
df_ER$mwmi <- replace(df_ER$mwmi, df_ER$wmi == 'wmwmi' | df_ER$mwmi == 'no_weak', 0)
df_ER<-transform(df_ER, mwmi = as.numeric((mwmi)))
averages_ER_mwmi <- aggregate(mwmi ~ n + p_edge + p_blue, data = df_ER, FUN = mean, na.rm = TRUE)

# Plot the fraction of networks in which there is (weak-)majority-(weak-)majority illusion ((w)m(w)mi):
ggplot(averages_ER_wmwmi, aes(x=p_blue)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi" )) + 
  geom_point(aes(y = averages_ER_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_ER_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_ER_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  facet_grid(p_edge~n) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_blue") +
  #ggtitle("Fraction of networks in which there is (weak-)majority-(weak-)majority illusion") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=12))

## Separate plots for different parameters:

# over p_blue:
averages_ER_wmwmi <- aggregate(wmwmi ~ p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mwmi <- aggregate(mwmi ~ p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_wmmi <- aggregate(wmmi ~ p_blue, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mmi <- aggregate(mmi ~ p_blue, data = df_ER, FUN = mean, na.rm = TRUE)

ggplot(averages_ER_wmwmi, aes(x=p_blue)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi" )) + 
  geom_point(aes(y = averages_ER_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_ER_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_ER_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  #facet_grid(p_edge~n) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_blue") +
  #ggtitle("Fraction of networks in which there is (weak-)majority-(weak-)majority illusion") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))

# over p_edge:
averages_ER_wmwmi <- aggregate(wmwmi ~ p_edge, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mwmi <- aggregate(mwmi ~ p_edge, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_wmmi <- aggregate(wmmi ~ p_edge, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mmi <- aggregate(mmi ~ p_edge, data = df_ER, FUN = mean, na.rm = TRUE)

ggplot(averages_ER_wmwmi, aes(x=p_edge)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi")) + 
  geom_point(aes(y = averages_ER_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_ER_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_ER_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  #facet_grid(p_edge~n) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_edge") +
  #ggtitle("Fraction of networks in which there is (weak-)majority-(weak-)majority illusion") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))

# over n:
df_ER$n <- factor(df_ER$n) # To get labels on the x-axis
averages_ER_wmwmi <- aggregate(wmwmi ~ n, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mwmi <- aggregate(mwmi ~ n, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_wmmi <- aggregate(wmmi ~ n, data = df_ER, FUN = mean, na.rm = TRUE)
averages_ER_mmi <- aggregate(mmi ~ n, data = df_ER, FUN = mean, na.rm = TRUE)

ggplot(averages_ER_wmwmi, aes(x=n)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi")) + 
  geom_point(aes(y = averages_ER_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_ER_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_ER_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  #facet_grid(p_edge~n) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "n") +
  #ggtitle("Fraction of networks in which there is (weak-)majority-(weak-)majority illusion") +
  theme(axis.text=element_text(size=16),
        axis.title=element_text(size=18,face="bold"),
        legend.text = element_text(size = 18),
        strip.text = element_text(size=16))

### Statistics:

# NB! Need the dataframe where variables are not yet made a factor. 
df_ER <- read.csv("ER_data.csv", header = TRUE, sep = ';')
# Add homophily as one variable
df_ER$homophily <- df_ER$probability_mixed_edge - df_ER$actual_fraction_mixed_edges 

# Compute fraction of nodes under illusion (instead of absolute number):
df_ER$frac_nodes_strict_ill <-df_ER$nr_nodes_str_ill/df_ER$n
df_ER$frac_nodes_weak_ill <-df_ER$nr_nodes_weak_ill/df_ER$n

# Perform an ANOVA on all variables:
#Fraction of nodes under strict illusion:
res_ER <- aov(frac_nodes_strict_ill ~ n + p_edge + p_blue + deg_assort_coef + avg_path_length + CC + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_ER)
summary(res_ER)  #ANOVA shows that all variables except degree assortativity coefficient and average degree are significantly relevant
eta_test_ER <- lsr::etaSquared(res_ER)
eta_test_ER # 0.01: small effect; 0.06: medium effect; 0.14: large effect ->  p_blue has large effect, others have no effect.

#Fraction of nodes under weak illusion:
res_ER_weak <- aov(frac_nodes_weak_ill ~ n + p_edge + p_blue + deg_assort_coef + avg_path_length + CC + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_ER)
summary(res_ER_weak)  #ANOVA shows that all variables are relevant. 
eta_test_ER_weak <- lsr::etaSquared(res_ER_weak)
eta_test_ER_weak # 0.01: small effect; 0.06: medium effect; 0.14: large effect -> Only p-blue has a large effect, the others have no effect.

### Correlation matrix
# Code for heatmap comes via Sophie van Schaik from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization
library(reshape2)
library("Hmisc")

df_clean_ER <- subset(df_ER, select = c(n, p_edge, p_blue,  deg_assort_coef, avg_path_length, CC, avg_degree, avg_EV_centr, avg_close_centr, avg_between_centr, homophily, frac_nodes_strict_ill, frac_nodes_weak_ill) ) 
#df_clean[is.na(df_clean)] <- 0 #Better to use pairwise.complete.obs like below, otherwise the values are misleading.
cormat <- round(cor(df_clean_ER, use='pairwise.complete.obs'),2)
melted_cormat <- melt(cormat)

#get significance
sig <- rcorr(as.matrix(df_clean_ER))
print(sig$P, digits = 5)

# Get lower triangle of the correlation matrix
get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
# Get upper triangle of the correlation matrix
get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
upper_tri <- get_upper_tri(cormat)

melted_cormat <- melt(upper_tri, na.rm = TRUE)
#reorder_cormat <- function(cormat){
#  # Use correlation between variables as distance
#  dd <- as.dist((1-cormat)/2)
#  hc <- hclust(dd)
#  cormat <-cormat[hc$order, hc$order]
#}

# Reorder the correlation matrix
#cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 15, hjust = 1),
        axis.text.y = element_text(size =15))+
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


### Homophily:

#Correlation between number of nodes with weak illusion and homophily: -0.02402936
cor.test(df_ER$nr_nodes_weak_ill, df_ER$homophily, method = "pearson")
#Better: between fraction of nodes with weak illusion and homophily: -0.06536299
cor.test(df_ER$nr_nodes_weak_ill/df_ER$n, df_ER$homophily, method = "pearson")
# Between fraction of nodes with strong illusion and homophily: -0.0952882 
cor.test(df_ER$nr_nodes_str_ill/df_ER$n, df_ER$homophily, method = "pearson")
# Result: very small negative correlation. 




