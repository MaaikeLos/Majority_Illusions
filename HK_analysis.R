# Code for data analysis of the paper 'On the Graph Theory of Majority Illusions: Theoretical Results and Computational Experiments'
# Authors: Maaike Venema-Los (University of Groningen, m.d.los@rug.nl), Zoé Christoff, and Davide Grossi.

library(ggplot2)
library(ggrepel) # For labeling the points in a scatter plot

####
df_HK <- read.csv("HK_data.csv", header = TRUE, sep = ';') 
View(df_HK)
names(df_HK)[names(df_HK) == 'CC'] <- 'clustering_coefficient'


#Are there graphs that consist of more than one component?
any(df_HK$frac_largest_comp != 1) # Gives FALSE: no

df_HK$p_blue <- factor(df_HK$p_blue, labels = c('.1', '.2', '.3', '.4', '.5'))
df_HK$p <- factor(df_HK$p)
# add a column that indicates m relative to n:
df_HK$m_relative <- with(df_HK, ifelse(m==0.1*n, 'm = 0.1 * n', ifelse(m == 0.5*n, 'm = 0.5 * n', 'm = 0.9 * n')))

### Fraction of nodes under illusion: 

# First plot of fraction of nodes under strict illusion: 
ggplot(df_HK, aes(x=p_blue, y=nr_nodes_str_ill / n, color=p)) + 
  geom_boxplot() + 
  facet_grid(m_relative ~ n) +
  scale_y_continuous(name = "Fraction of nodes under illusion", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "p_blue") +
  ggtitle("Fraction of nodes under strict illusion (HK)")

# p does not seem to matter a lot.
# Test this with ANOVA: 
res_test <- aov(nr_nodes_str_ill ~ n + m + p + p_blue, data = df_HK)
summary(res_test)  #ANOVA shows that all variables except for p are relevant. (both for strict and weak illusions)
# Therefore we continue with plots in which the values of p are not separately drawn, to improve readability.

# First create dataframe with the data for strict and weak illusions combined (without p specified):
df_HK_combined_weak <- df_HK
df_HK_combined_strict <- df_HK
df_HK_combined_weak$combined <- 'weak'
df_HK_combined_strict$combined <- 'strict'
df_HK_combined <- rbind(df_HK_combined_weak, df_HK_combined_strict)
rm(df_HK_combined_strict)
rm(df_HK_combined_weak)
df_HK_combined$nr_nodes_ill <- df_HK_combined$nr_nodes_str_ill
df_HK_combined$nr_nodes_ill <- replace(df_HK_combined$nr_nodes_ill, df_HK_combined$combined == 'weak', df_HK_combined$nr_nodes_weak_ill)

# Plot the boxplots of the fraction of nodes under strict/weak illusion, for every value of n (horizontal facets), m, and p_blue.
ggplot(df_HK_combined, aes(x=p_blue, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  facet_grid(m_relative ~ n) +
  scale_y_continuous(name = "Fraction of nodes under illusion", sec.axis = sec_axis(~ . , name = "m", breaks = NULL, labels = NULL)) +
  scale_x_discrete(name = "p_blue") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  ggtitle("n") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
#(Save as 12x6 inch pdf for readability)

## Separate plots for different parameters:

#All values of p_blue together, all values of m together (=influence of n):
ggplot(df_HK_combined, aes(x=factor(n), y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "n") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))
#(Save as 10x8 inch pdf for readability)

#All values of p_blue together, all values of n together (=influence of m):
ggplot(df_HK_combined, aes(x=m_relative, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "Relative m") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))

#All values of m together, all values of n together (=influence of p_blue):
ggplot(df_HK_combined, aes(x=p_blue, y = nr_nodes_ill/n, color = combined)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "p_blue") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))


### Fraction of runs with majority-majority-illusion:

# First make a new column with the mmi values we want to measure 1 and the rest 0, and create a variable with the averages:
#wmmi
df_HK$wmmi <- replace(df_HK$mi, df_HK$mi == 'wmmi' | df_HK$mi == 'mmi', 1)
df_HK$wmmi <- replace(df_HK$wmmi, df_HK$wmmi == 'no_strict', 0)
df_HK<-transform(df_HK, wmmi = as.numeric((wmmi)))
averages_HK_wmmi <- aggregate(wmmi ~ n + m_relative  + p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
#mmi
df_HK$mmi <- replace(df_HK$mi, df_HK$mi == 'mmi', 1)
df_HK$mmi <- replace(df_HK$mmi, df_HK$mmi == 'wmmi' | df_HK$mmi == 'no_strict', 0)
df_HK<-transform(df_HK, mmi = as.numeric((mmi)))
averages_HK_mmi <- aggregate(mmi ~ n + m_relative  + p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
#wmwmi
df_HK$wmwmi <- replace(df_HK$wmi, df_HK$wmi == 'wmwmi' | df_HK$wmi == 'mwmi', 1)
df_HK$wmwmi <- replace(df_HK$wmwmi, df_HK$wmwmi == 'no_weak', 0)
df_HK<-transform(df_HK, wmwmi = as.numeric((wmwmi)))
averages_HK_wmwmi <- aggregate(wmwmi ~ n + m_relative  + p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
#mwmi
df_HK$mwmi <- replace(df_HK$wmi, df_HK$wmi == 'mwmi', 1)
df_HK$mwmi <- replace(df_HK$mwmi, df_HK$mwmi == 'wmwmi' | df_HK$mwmi == 'no_weak', 0)
df_HK<-transform(df_HK, mwmi = as.numeric((mwmi)))
averages_HK_mwmi <- aggregate(mwmi ~ n + m_relative  + p_blue, data = df_HK, FUN = mean, na.rm = TRUE)

# Plot the fraction of networks in which there is (weak-)majority-(weak-)majority illusion ((w)m(w)mi):
ggplot(averages_HK_wmwmi, aes(x=p_blue)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi" )) + 
  geom_point(aes(y = averages_HK_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_HK_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_HK_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  facet_grid(m_relative~n) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_blue") +
  ggtitle("n") +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"),
        legend.text = element_text(size = 14),
        strip.text = element_text(size=12),
        plot.title = element_text(size = 14, face = "bold", hjust = 0.5))
#(Save as 12x6 inch pdf for readability)

## Separate plots for different parameters:

# over p_blue:
averages_HK_wmwmi <- aggregate(wmwmi ~ p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mwmi <- aggregate(mwmi ~ p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_wmmi <- aggregate(wmmi ~ p_blue, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mmi <- aggregate(mmi ~ p_blue, data = df_HK, FUN = mean, na.rm = TRUE)

ggplot(averages_HK_wmwmi, aes(x=p_blue)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi" )) + 
  geom_point(aes(y = averages_HK_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_HK_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_HK_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_blue") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))

# over m:
averages_HK_wmwmi <- aggregate(wmwmi ~ m_relative, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mwmi <- aggregate(mwmi ~ m_relative, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_wmmi <- aggregate(wmmi ~ m_relative, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mmi <- aggregate(mmi ~ m_relative, data = df_HK, FUN = mean, na.rm = TRUE)

ggplot(averages_HK_wmwmi, aes(x=m_relative)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi")) + 
  geom_point(aes(y = averages_HK_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_HK_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_HK_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "Relative m") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))

# over n:
df_HK$n <- factor(df_HK$n) # To get labels on the x-axis
averages_HK_wmwmi <- aggregate(wmwmi ~ n, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mwmi <- aggregate(mwmi ~ n, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_wmmi <- aggregate(wmmi ~ n, data = df_HK, FUN = mean, na.rm = TRUE)
averages_HK_mmi <- aggregate(mmi ~ n, data = df_HK, FUN = mean, na.rm = TRUE)

ggplot(averages_HK_wmwmi, aes(x=n)) +
  geom_point(aes(y=wmwmi, colour = "wmwmi")) + 
  geom_point(aes(y = averages_HK_mwmi$mwmi, colour = "mwmi" )) +
  geom_point(aes(y = averages_HK_wmmi$wmmi, colour = "wmmi" )) +
  geom_point(aes(y = averages_HK_mmi$mmi, colour = "mmi" )) +
  scale_colour_manual("", 
                      breaks = c("wmwmi", "mwmi", "wmmi", "mmi"),
                      values = c('#99C1C1', "#045D5D", "pink", "#990033")) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "n") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=22,face="bold"),
        legend.text = element_text(size = 22),
        strip.text = element_text(size=20))


### Statistics ###

# Need to use the df_HK where variables are not made into factors yet below:
df_HK <- read.csv("HK_data.csv", header = TRUE, sep = ';') 
names(df_HK)[names(df_HK) == 'CC'] <- 'clustering_coefficient'

# Add homophily as one variable
df_HK$homophily <- df_HK$probability_mixed_edge - df_HK$actual_fraction_mixed_edges 

# Compute fraction of nodes under illusion (instead of absolute number):
df_HK$frac_nodes_strict <-df_HK$nr_nodes_str_ill/df_HK$n
df_HK$frac_nodes_weak <-df_HK$nr_nodes_weak_ill/df_HK$n

## Perform an ANOVA on all variables:

# Fraction of nodes under strict illusion:
res_HK <- aov(frac_nodes_strict~ n + m + p + p_blue + deg_assort_coef + avg_path_length + clustering_coefficient + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_HK)
summary(res_HK)  #ANOVA shows that most variables are relevant: only not p, average degree and average EV centrality. 
eta_test_HK <- lsr::etaSquared(res_HK)
eta_test_HK # 0.01: small effect; 0.06: medium effect; 0.14: large effect -> Only p-blue and homophily have a large effect, the others have no effect.

# Fraction of nodes under weak illusion:
res_HK_weak <- aov(frac_nodes_weak ~ n + m + p + p_blue + deg_assort_coef + avg_path_length + clustering_coefficient + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_HK)
summary(res_HK_weak)  #ANOVA shows that all variables are relevant
eta_test_HK_weak <- lsr::etaSquared(res_HK_weak)
eta_test_HK_weak # 0.01: small effect; 0.06: medium effect; 0.14: large effect -> Only p-blue and homophily hav a large effect, the others have no effect.

# Note: if you test for the variables separately, the effect seems larger (but still no 'small effect').

# With only p_blue = 0.5, since there most illusions happen.
df_HK_05 <- df_HK[df_HK$p_blue == 0.5,]

## ANOVA on all variables for p_blue = 0.5:

# Fraction of nodes under strict illusion:
res_HK_05 <- aov(frac_nodes_strict ~ n + m + p  + deg_assort_coef + avg_path_length + clustering_coefficient + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_HK_05)
summary(res_HK_05)  #ANOVA shows that most variables are relevant: only not p and average closeness centrality. 
eta_test_HK_05 <- lsr::etaSquared(res_HK_05)
eta_test_HK_05 # 0.01: small effect; 0.06: medium effect; 0.14: large effect -> Only homophily has large effect, others have no effect.

# Fraction of nodes under weak illusion:
res_HK_weak_05 <- aov(frac_nodes_weak ~ n + m + p + deg_assort_coef + avg_path_length + clustering_coefficient + avg_degree + avg_EV_centr + avg_close_centr + avg_between_centr + homophily, data = df_HK_05)
summary(res_HK_weak_05)  #ANOVA shows that most variables are relevant: only not  p
eta_test_HK_weak_05 <- lsr::etaSquared(res_HK_weak_05)
eta_test_HK_weak_05 # 0.01: small effect; 0.06: medium effect; 0.14: large effect -> Only homophily has medium effect, others have no effect.


## Correlationmatrix
# Code for heatmap comes via Sophie van Schaik from http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

library(reshape2)
library("Hmisc")

#Choose whether you want a correlation matrix of the total dataset or only for p_blue = 0.5:
df_clean_HK <- subset(df_HK, select = c(n, m, p_blue, p,  deg_assort_coef, avg_path_length, clustering_coefficient, avg_degree, avg_EV_centr, avg_close_centr, avg_between_centr, homophily, frac_nodes_strict, frac_nodes_weak) ) # if p_blue not specified
df_clean_HK <- subset(df_HK_05, select = c(n, m, p,  deg_assort_coef, avg_path_length, clustering_coefficient, avg_degree, avg_EV_centr, avg_close_centr, avg_between_centr, homophily, frac_nodes_strict, frac_nodes_weak) ) # if p_blue = 0.5

cormat <- round(cor(df_clean_HK, use='pairwise.complete.obs'),2)

#get significance
sig <- rcorr(as.matrix(df_clean_HK))
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

# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1),
        axis.text.y = element_text(size =12))+
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
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

### Print homophily correlations separately:

#Correlation between number of nodes with weak illusion and homophily: -0.2058
cor.test(df_HK$nr_nodes_weak_ill, df_HK$homophily, method = "pearson")
#Better: between fraction of nodes with weak illusion and homophily: -0.3764
cor.test(df_HK$frac_nodes_weak, df_HK$homophily, method = "pearson")
# Between fraction of nodes with strong illusion and homophily: -0.3923
cor.test(df_HK$frac_nodes_strict, df_HK$homophily, method = "pearson")


