##### Version 29-1-2025, Maaike Venema-Los #####
library(rstudioapi)
library(dplyr)

# Set working directory to source file location.(Session -> Set Working Wirectory -> To Source File Location)
# Choose one of the two options below:

### 1. Read original data ###
df_HK <- read.csv("Data/HK_data.csv")
df_ER <- read.csv("Data/ER_data.csv")
df_FB <- read.csv("Data/FB_data_1.csv")

### 2. Read newly generated data ###
# Getting the path of the current open file to set working directory
df_ER_even <- read.csv("Data/ER_data_even.csv", header = TRUE, sep = ';')
df_ER_odd <- read.csv("Data/ER_data_odd.csv", header = TRUE, sep = ';')
df_ER <- rbind(df_ER_even, df_ER_odd)
rm(df_ER_even, df_ER_odd)
df_HK_even <- read.csv("Data/HK_data_even.csv", header = TRUE, sep = ';')
df_HK_odd <- read.csv("Data/HK_data_odd.csv", header = TRUE, sep = ';')
df_HK <- rbind(df_HK_even, df_HK_odd)
rm(df_HK_even, df_HK_odd)
df_FB <- read.csv("Data/FB_data.csv", header = TRUE, sep = ';')

### Clean up and preprocess data ###

#Remove some columns with per-node-properties we do not use:
df_ER <- subset(df_ER, select=-c(deg_seq, EV_centr, close_centr, between_centr))
df_HK <- subset(df_HK, select=-c(deg_seq, EV_centr, close_centr, between_centr))

# CC name:
names(df_ER)[names(df_ER) == 'CC'] <- 'clustering_coefficient'
names(df_HK)[names(df_HK) == 'CC'] <- 'clustering_coefficient'

# Replace NA appropriately
df_ER$MSE_no_illusion <- replace(df_ER$MSE_no_illusion, df_ER$MSE_no_illusion == 'NotApplicable', NA) 
df_ER$MSE_any_illusion <- replace(df_ER$MSE_any_illusion, df_ER$MSE_any_illusion == 'NotApplicable', NA) 
df_ER$MSE_strict_illusion <- replace(df_ER$MSE_strict_illusion, df_ER$MSE_strict_illusion == 'NotApplicable', NA) 
df_ER$MSE_no_illusion = as.numeric(df_ER$MSE_no_illusion)
df_ER$MSE_any_illusion = as.numeric(df_ER$MSE_any_illusion)
df_ER$MSE_strict_illusion = as.numeric(df_ER$MSE_strict_illusion)
df_HK$MSE_no_illusion <- replace(df_HK$MSE_no_illusion, df_HK$MSE_no_illusion == 'NotApplicable', NA) 
df_HK$MSE_any_illusion <- replace(df_HK$MSE_any_illusion, df_HK$MSE_any_illusion == 'NotApplicable', NA) 
df_HK$MSE_strict_illusion <- replace(df_HK$MSE_strict_illusion, df_HK$MSE_strict_illusion == 'NotApplicable', NA) 
df_HK$MSE_no_illusion = as.numeric(df_HK$MSE_no_illusion)
df_HK$MSE_any_illusion = as.numeric(df_HK$MSE_any_illusion)
df_HK$MSE_strict_illusion = as.numeric(df_HK$MSE_strict_illusion)
df_FB$MSE_any_illusion <- replace(df_FB$MSE_any_illusion, df_FB$MSE_any_illusion == 'NotApplicable', NA) 
df_FB$MSE_any_illusion = as.numeric(df_FB$MSE_any_illusion)
df_FB$MSE_strict_illusion <- replace(df_FB$MSE_strict_illusion, df_FB$MSE_strict_illusion == 'NotApplicable', NA) 
df_FB$MSE_strict_illusion = as.numeric(df_FB$MSE_strict_illusion)


# Calculate homophily
df_ER$homophily <- df_ER$probability_mixed_edge - df_ER$actual_fraction_mixed_edges 
df_HK$homophily <- df_HK$probability_mixed_edge - df_HK$actual_fraction_mixed_edges 
df_FB$homophily <- df_FB$probability_mixed_edge - df_FB$actual_fraction_mixed_edges 
# Remove old columns that are not necessary anymore:
df_ER <- subset(df_ER, select=-c(probability_mixed_edge, actual_fraction_mixed_edges))
df_HK <- subset(df_HK, select=-c(probability_mixed_edge, actual_fraction_mixed_edges))
df_FB <- subset(df_FB, select=-c(probability_mixed_edge, actual_fraction_mixed_edges))

# Calculate fraction of nodes under illusion
df_ER$frac_nodes_strict <-df_ER$nr_nodes_str_ill/df_ER$n
df_ER$frac_nodes_weak <-df_ER$nr_nodes_weak_ill/df_ER$n
df_ER$frac_nodes_only_weak <- df_ER$frac_nodes_weak - df_ER$frac_nodes_strict #Column with only weak values
df_HK$frac_nodes_strict <-df_HK$nr_nodes_str_ill/df_HK$n
df_HK$frac_nodes_weak <-df_HK$nr_nodes_weak_ill/df_HK$n
df_HK$frac_nodes_only_weak <- df_HK$frac_nodes_weak - df_HK$frac_nodes_strict 
df_FB$frac_nodes_strict <-df_FB$nr_nodes_str_ill/4039 # The facebook network has 4039 nodes
df_FB$frac_nodes_weak <-df_FB$nr_nodes_weak_ill/4039
df_FB$frac_nodes_only_weak <- df_FB$frac_nodes_weak - df_FB$frac_nodes_strict 

## Bin variables
# blue_global: Since the thing is symmetric, we can add the 0.9-1 group to the 0-0.1, the 0.8-0.9 to the 0.1-0.2, etc.:
df_ER$proportion_blue_global <- ifelse(df_ER$proportion_blue_global > 0.50000000, 1- df_ER$proportion_blue_global, df_ER$proportion_blue_global )
df_HK$proportion_blue_global <- ifelse(df_HK$proportion_blue_global > 0.50000000, 1- df_HK$proportion_blue_global, df_HK$proportion_blue_global )
df_FB$proportion_blue_global <- ifelse(df_FB$proportion_blue_global > 0.50000000, 1- df_FB$proportion_blue_global, df_FB$proportion_blue_global )
df_ER$blue_global_bins <- cut(df_ER$proportion_blue_global, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.5), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5"))
df_HK$blue_global_bins <- cut(df_HK$proportion_blue_global, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.5), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5"))
df_FB$blue_global_bins <- cut(df_FB$proportion_blue_global, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.5), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5"))
# C_degree 
df_ER$C_degree_bins <- cut(df_ER$C_degree, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
df_HK$C_degree_bins <- cut(df_HK$C_degree, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
# degree assort. coef.
#min(df_HK$deg_assort_coef) # -0.9444663
#max(df_HK$deg_assort_coef) # 0.09883871
df_HK$deg_assort_coef_bins <-cut(df_HK$deg_assort_coef, breaks=c(-1, -0.89999, -0.79999, -0.69999, -0.59999, -0.49999, -0.39999, -0.29999, -0.19999, -0.09999, 0, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("-1--0.9", "-0.9- -0.8", "-0.8--0.7", "-0.7--0.6", "-0.6--0.5", "-0.5--0.4", "-0.4--0.3", "-0.3--0.2", "-0.2--0.1", "-0.1-0", "0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
#min(df_ER$deg_assort_coef) # -0.8469945
#max(df_ER$deg_assort_coef) # 0.6458333
df_ER$deg_assort_coef_bins <-cut(df_ER$deg_assort_coef, breaks=c(-1, -0.89999, -0.79999, -0.69999, -0.59999, -0.49999, -0.39999, -0.29999, -0.19999, -0.09999, 0, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("-1--0.9", "-0.9- -0.8", "-0.8--0.7", "-0.7--0.6", "-0.6--0.5", "-0.5--0.4", "-0.4--0.3", "-0.3--0.2", "-0.2--0.1", "-0.1-0", "0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
# homophily
df_HK$homophily_bins <- cut(df_HK$homophily, breaks=c(-1, -0.89999, -0.79999, -0.69999, -0.59999, -0.49999, -0.39999, -0.29999, -0.19999, -0.09999, 0, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("-1--0.9", "-0.9- -0.8", "-0.8--0.7", "-0.7--0.6", "-0.6--0.5", "-0.5--0.4", "-0.4--0.3", "-0.3--0.2", "-0.2--0.1", "-0.1-0", "0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
df_ER$homophily_bins <- cut(df_ER$homophily, breaks=c(-1, -0.89999, -0.79999, -0.69999, -0.59999, -0.49999, -0.39999, -0.29999, -0.19999, -0.09999, 0, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("-1--0.9", "-0.9- -0.8", "-0.8--0.7", "-0.7--0.6", "-0.6--0.5", "-0.5--0.4", "-0.4--0.3", "-0.3--0.2", "-0.2--0.1", "-0.1-0", "0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
#min(df_FB$homophily) # -0.03038015
#max(df_FB$homophily) # 0.02634664
df_FB$homophily_bins <- cut(df_FB$homophily, breaks=c( -0.039999, -0.029999, -0.019999, -0.009999, 0, 0.009999, 0.019999, 0.029999), labels=c("-0.04--0.03", "-0.03--0.02", "-0.02--0.01", "-0.01-0", "0-0.01","0.01-0.02","0.02-0.03"))
# n
df_ER$n_combined <- cut(df_ER$n, breaks=c(10, 30, 50, 70, 90, 110), labels=c("20-21","40-41","60-61", "80-81", "100-101"))
df_HK$n_combined <- cut(df_HK$n, breaks=c(10, 30, 50, 70, 90, 110), labels=c("20-21","40-41","60-61", "80-81", "100-101"))
# C_between
#ggplot(df_ER, aes(x=C_between))+
#  geom_density()
df_ER$C_between_bins <- cut(df_ER$C_between, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
df_HK$C_between_bins <- cut(df_HK$C_between, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
# C_closeness
#min(df_ER$C_closeness) #0.01910408
#max(df_ER$C_closeness) #0.3472737
#min(df_HK$C_closeness) #0.07432023
#max(df_HK$C_closeness) #0.4619815
df_ER$C_closeness_bins <- cut(df_ER$C_closeness, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
df_HK$C_closeness_bins <- cut(df_HK$C_closeness, breaks=c(-1, 0.09999, 0.19999, 0.29999, 0.39999, 0.49999, 0.59999, 0.69999, 0.79999, 0.89999, 1), labels=c("0-0.1","0.1-0.2","0.2-0.3", "0.3-0.4", "0.4-0.5", "0.5-0.6","0.6-0.7", "0.7-0.8", "0.8-0.9", "0.9-1"))
# C_EV
#min(df_ER$C_EV, na.rm = TRUE) # 0.1071865
#max(df_ER$C_EV, na.rm = TRUE) # 18.90087
#ggplot(df_ER, aes(x=C_EV))+
#  geom_density()
#min(df_HK$C_EV) # 1.732615
#max(df_HK$C_EV) # 24.95322
#ggplot(df_HK, aes(x=C_EV))+
#  geom_density()
df_ER$C_EV_bins <- cut(df_ER$C_EV, breaks=c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 20), labels=c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-20"))
df_HK$C_EV_bins <- cut(df_HK$C_EV, breaks=c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 25), labels=c("0-1", "1-2", "2-3", "3-4", "4-5", "5-6", "6-7", "7-8", "8-9", "9-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17", "17-18", "18-19", "19-20", "20-25"))


# add a column that indicates m relative to n:
df_HK$m_relative <- with(df_HK, ifelse(m==0.1*n, 'm = 0.1 * n', ifelse(m == 0.5*n, 'm = 0.5 * n', 'm = 0.9 * n')))

# Make a new column with the mmi values we want to measure 1 and the rest 0:
## ER
#WMmi
df_ER$WMmi <- replace(df_ER$mi, df_ER$mi == 'WMmi' | df_ER$mi == 'Mmi', 1)
df_ER$WMmi <- replace(df_ER$WMmi, df_ER$WMmi == 'no_strict', 0)
df_ER<-transform(df_ER, WMmi = as.numeric((WMmi)))
#Mmi
df_ER$Mmi <- replace(df_ER$mi, df_ER$mi == 'Mmi', 1)
df_ER$Mmi <- replace(df_ER$Mmi, df_ER$Mmi == 'WMmi' | df_ER$Mmi == 'no_strict', 0)
df_ER<-transform(df_ER, Mmi = as.numeric(Mmi))
#WMwmi
df_ER$WMwmi <- replace(df_ER$wmi, df_ER$wmi == 'WMwmi' | df_ER$wmi == 'Mwmi', 1)
df_ER$WMwmi <- replace(df_ER$WMwmi, df_ER$WMwmi == 'no_weak', 0)
df_ER<-transform(df_ER, WMwmi = as.numeric((WMwmi)))
#Mwmi
df_ER$Mwmi <- replace(df_ER$wmi, df_ER$wmi == 'Mwmi', 1)
df_ER$Mwmi <- replace(df_ER$Mwmi, df_ER$wmi == 'WMwmi' | df_ER$Mwmi == 'no_weak', 0)
df_ER<-transform(df_ER, Mwmi = as.numeric((Mwmi)))
## HK ##
#WMmi
df_HK$WMmi <- replace(df_HK$mi, df_HK$mi == 'WMmi' | df_HK$mi == 'Mmi', 1)
df_HK$WMmi <- replace(df_HK$WMmi, df_HK$WMmi == 'no_strict', 0)
df_HK<-transform(df_HK, WMmi = as.numeric((WMmi)))
#Mmi
df_HK$Mmi <- replace(df_HK$mi, df_HK$mi == 'Mmi', 1)
df_HK$Mmi <- replace(df_HK$Mmi, df_HK$Mmi == 'WMmi' | df_HK$Mmi == 'no_strict', 0)
df_HK<-transform(df_HK, Mmi = as.numeric(Mmi))
#WMwmi
df_HK$WMwmi <- replace(df_HK$wmi, df_HK$wmi == 'WMwmi' | df_HK$wmi == 'Mwmi', 1)
df_HK$WMwmi <- replace(df_HK$WMwmi, df_HK$WMwmi == 'no_weak', 0)
df_HK<-transform(df_HK, WMwmi = as.numeric((WMwmi)))
#Mwmi
df_HK$Mwmi <- replace(df_HK$wmi, df_HK$wmi == 'Mwmi', 1)
df_HK$Mwmi <- replace(df_HK$Mwmi, df_HK$wmi == 'WMwmi' | df_HK$Mwmi == 'no_weak', 0)
df_HK<-transform(df_HK, Mwmi = as.numeric((Mwmi)))
## FB ##
# Note: any(df_FB=='wmmi') and any(df_FB=='wmwmi') give 'FALSE', which is 
#   because the number of nodes is odd: any weak majority-(weak)-maj illusion is necessarily a strict one.
#Mmi
df_FB$Mmi <- replace(df_FB$mi,  df_FB$mi == 'Mmi', 1)
df_FB$Mmi <- replace(df_FB$Mmi, df_FB$Mmi == 'no_strict', 0)
df_FB<-transform(df_FB, Mmi = as.numeric((Mmi)))
#Mwmi
df_FB$Mwmi <- replace(df_FB$wmi, df_FB$wmi == 'Mwmi', 1)
df_FB $Mwmi <- replace(df_FB$Mwmi, df_FB$Mwmi == 'no_weak', 0)
df_FB<-transform(df_FB, Mwmi = as.numeric((Mwmi)))

