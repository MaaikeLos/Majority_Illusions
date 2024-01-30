library(ggplot2)
library(ggrepel) # For labeling the points in a scatter plot

df_fb <-  read.csv("FB_data.csv", header = TRUE, sep = ';')
df_fb$p_blue <- factor(df_fb$p_blue)

### Fraction of nodes under  illusion: 

# First create dataframe with the data for strict and weak illusions combined:
df_fb_combined_weak <- df_fb
df_fb_combined_strict <- df_fb
df_fb_combined_weak$combined <- 'weak'
df_fb_combined_strict$combined <- 'strict'
df_fb_combined <- rbind(df_fb_combined_weak, df_fb_combined_strict)
rm(df_fb_combined_strict)
rm(df_fb_combined_weak)
df_fb_combined$nr_nodes_ill <- df_fb_combined$nr_nodes_str_ill
df_fb_combined$nr_nodes_ill <- replace(df_fb_combined$nr_nodes_ill, df_fb_combined$combined == 'weak', df_fb_combined$nr_nodes_weak_ill)

# Plot the boxplots of the fraction of nodes under strict/weak illusion, for every value of p_blue.
ggplot(df_fb_combined, aes(x=p_blue, y = nr_nodes_ill/4039, color = combined)) + 
  geom_boxplot() + 
  scale_y_continuous(name = "Fraction of nodes under illusion") +
  scale_x_discrete(name = "p_blue") +
  scale_colour_manual("", 
                      breaks = c("strict", "weak"),
                      values = c( "#045D5D", "#99C1C1"))+
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        legend.text = element_text(size = 20))

### Fraction of runs with majority-majority-illusion:

# First make a new column with the mmi values we want to measure 1 and the rest 0:
# Note: any(df_fb=='wmmi') and any(df_fb=='wmwmi') give 'FALSE', which is because the number of nodes is odd: any weak majority-(weak)-maj illusion is necessarily a strict one.
#mmi
df_fb$mmi <- replace(df_fb$mi,  df_fb$mi == 'mmi', 1)
df_fb$mmi <- replace(df_fb$mmi, df_fb$mmi == 'no_strict', 0)
df_fb<-transform(df_fb, mmi = as.numeric((mmi)))
averages_fb_mmi <- aggregate(mmi ~   p_blue, data = df_fb, FUN = mean, na.rm = TRUE)
ggplot(averages_fb_mmi, aes(x=p_blue, y=mmi)) + 
  geom_point() + 
  geom_text_repel(aes(label = mmi)) +
  scale_y_continuous(name = "fraction of colorings with (w)mmi") +
  scale_x_discrete(name = "p_blue") 

#mwmi
df_fb$mwmi <- replace(df_fb$wmi, df_fb$wmi == 'mwmi', 1)
df_fb $mwmi <- replace(df_fb$mwmi, df_fb$mwmi == 'no_weak', 0)
df_fb<-transform(df_fb, mwmi = as.numeric((mwmi)))
averages_fb_mwmi <- aggregate(mwmi ~   p_blue, data = df_fb, FUN = mean, na.rm = TRUE)
ggplot(averages_fb_mwmi, aes(x=p_blue, y=mwmi)) + 
  geom_point() + 
  geom_text_repel(aes(label = mwmi)) +
  scale_y_continuous(name = "fraction of colorings with (w)mwmi") +
  scale_x_discrete(name = "p_blue") 

#combined plot of (w)mwmi and (w)mmi :
ggplot(averages_fb_mwmi, aes(x=p_blue)) +
  geom_point(aes(y=mwmi, colour = "mwmi" ), position= position_jitter(h=0.005,w=0.005)) + 
  geom_point(aes(y = averages_fb_mmi$mmi, colour = "mmi" ), position= position_jitter(h=0.005,w=0.005)) +
  scale_colour_manual("", 
                      breaks = c("mwmi",  "mmi"),
                      values = c('#99C1C1', "#045D5D")) +
  scale_y_continuous(name = "fraction of runs with illusion") +
  scale_x_discrete(name = "p_blue") +
  #ggtitle("Fraction of networks in which there is (weak-)majority-(weak-)majority illusion") +
  theme(axis.text=element_text(size=20),
        axis.title=element_text(size=20,face="bold"),
        legend.text = element_text(size = 20)) 

# Add a column with the number of nodes that have weak but not strict illusion (these can only be the even nodes):
df_fb<- transform(df_fb, nr_nodes_only_weak = df_fb$nr_nodes_weak_ill - df_fb$nr_nodes_str_ill)
# Add a column with the fraction of even nodes under weak illusion:
df_fb <- transform(df_fb, even_nodes_weak_illusion = df_fb$nr_nodes_only_weak/2018)

max(df_fb$nr_nodes_only_weak)
#457
mean(df_fb[df_fb$p_blue == 0.5,]$even_nodes_weak_illusion)
#0.1859163

#Count exact number of mmi for p_blue = 0.5:
sum(df_fb[df_fb$p_blue == 0.5,]$mmi)
# 74
#Count exact number of mwmi for p_blue = 0.5:
sum(df_fb[df_fb$p_blue == 0.5,]$mwmi)
# 623


