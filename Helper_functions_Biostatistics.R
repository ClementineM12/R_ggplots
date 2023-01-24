## Functions to use in the project

# Set the dataset used as "data" or else need as input in the functions.
# Import necessary libraries
library(ggplot2)
library(dplyr)
library(patchwork)
library(scales)
library(moments)
library(tidyverse)
library(ggpubr)
library(ggthemes)

colors <- c("#7AC5CD", "#5F9EA0", "#96CDCD", "#79CDCD")
pal <- colorRampPalette(c("white", "#5F9EA0"))
color_pal <- c("#CD0000", "#5F9EA0", "coral3", "mediumpurple4")
color_bar_dich <- c("#5F9EA0", "lightslategrey")

# Function for statistical measures of a variable. (arg: numeric)
statistics <- function(variable) {
  
  cat(paste("\n", "Variance:", round(var(variable), 2), "\n",
            "Standard deviation:", round(sd(variable), 2), "\n",
            "Skewness:", round(skewness(variable), 2), "\n",
            "Kurtosis:", round(kurtosis(variable), 2), "\n",
            "Min:", round(as.numeric(sub('.*:', '', summary(variable)[1])), 2), "\n",
            "1st Quarter:", round(as.numeric(sub('.*:', '', summary(variable)[2])), 2), "\n",
            "Median:", round(as.numeric(sub('.*:', '', summary(variable)[3])), 2), "\n",
            "Mean:", round(as.numeric(sub('.*:', '', summary(variable)[4])), 2), "\n",
            "3rd Quarter:", round(as.numeric(sub('.*:', '', summary(variable)[5])), 2), "\n",
            "Max:", round(as.numeric(sub('.*:', '', summary(variable)[6])), 2), "\n"))
}


# Display a table of the statistical measures of the given variables. (arg: list, vector_of_names)
statistics_df <- function(data, data_names){
  
  columns <- c("Variance", "St.Deviation", "Skewness", "Kurtosis",
               "Min", "1stQuart", "Median", "Mean", "3rdQuart", "Max")
  statistics_df <- data.frame(matrix(ncol = length(columns), nrow = length(data)))
  colnames(statistics_df) <- columns
  rownames(statistics_df) <- data_names
  
  for (i in 1:length(data)){
    statistics_df[i, 1] <- round(var(as.numeric(unlist(data[i]))), 2)
    statistics_df[i, 2] <- c(round(sd(as.numeric(unlist(data[i]))), 2))
    statistics_df[i, 3] <- c(round(skewness(as.numeric(unlist(data[i]))), 2))
    statistics_df[i, 4] <- c(round(kurtosis(as.numeric(unlist(data[i]))), 2))
    statistics_df[i, 5] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[1])), 2))
    statistics_df[i, 6] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[2])), 2))
    statistics_df[i, 7] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[3])), 2))
    statistics_df[i, 8] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[4])), 2))
    statistics_df[i, 9] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[5])), 2))
    statistics_df[i, 10] <- c(round(as.numeric(sub('.*:', '', summary(as.numeric(unlist(data[i])))[6])), 2))
  }
  library(data.table)
  library(gridExtra)
  library(grid)
  
  dt_0 <- as.data.table(t(statistics_df), "")

  theme_0 <- ttheme_minimal(
    core=list(bg_params = list(fill = pal(5), col=NA),
              fg_params = list(fontface=3L, col="darkslategray")),
    colhead=list(fg_params = list(col = "darkslategray", fontface = 1L)),
    rowhead=list(fg_params = list(col = "white", fontface = 2)))
  myTable <- tableGrob(dt_0, theme = theme_0)
  table <- grid.draw(myTable)
  print(table)
  return (myTable)
}


# Plot frequency histogram-polygon. (arg: numeric, string, integer, integer)
frequency_plots <- function(variable, name, binwidth_hist, binwidth_poly){
  
  hist <- ggplot(data, aes(x = variable)) + ggtitle("Frequency Histogram") +
    geom_histogram(aes(fill = ..density..), binwidth = binwidth_hist) +
    scale_x_continuous(name = name,
                       breaks = seq(0, 150, 25)) +
    scale_y_continuous(name = "Frequency") +
    geom_vline(aes(xintercept=mean(variable)),
               color = "lightslategrey", linetype = "solid", linewidth = 1) +
    geom_density(aes(y = ..density.. * (nrow(data) * binwidth_hist)), col = "lightskyblue3") +
    theme_minimal(base_size = 8) + theme(plot.title=element_text(hjust=0.5))
  
  poly <- ggplot(data, aes(x = variable, after_stat(density))) + ggtitle("Frequency Polygon") +
    geom_freqpoly(binwidth = binwidth_poly) +
    geom_area(aes(y=..density..), bins = 30, stat = 'bin', fill='dodgerblue4', alpha = 0.6) +
    scale_x_continuous(name = name,
                       breaks = seq(0, 150, 25)) +
    scale_y_continuous(name = "") +
    geom_vline(aes(xintercept=mean(variable)),
               color = "lightslategrey", linetype = "solid") +
    theme_minimal(base_size = 8) + theme(plot.title=element_text(hjust=0.5))
  ggarrange(hist, poly, ncol = 2, nrow = 1)
}


# Boxplot and Density plot with Measurement Results. (arg: name_of_column, numeric, vector_with_categories_names)
box_density_plots <- function(fact, variable, categories){
  
  factor <- as.factor(as.numeric(unlist(data[deparse(substitute(fact))])))
  
  boxplot <- ggplot(data, aes(x=factor, y=variable, fill=factor)) +
    stat_boxplot(geom = "errorbar", width = 0.5, col="darkseagreen3") + 
    geom_boxplot(alpha = 0.6,
                 colour = "darkseagreen3", 
                 outlier.colour = 2) +
    scale_fill_manual(values = color_pal[1:length(unique(factor))],
                      labels = categories,
                      name = "") + 
    scale_x_discrete(labels = categories) +
    ylab("") + xlab("") + ggtitle("") + 
    theme_minimal(base_size = 10)
  
  cat_List <- list()
  f <- as.numeric(levels(factor)[as.integer(factor)])
  min <- min(f)
  max <- max(f)
  
  for (i in min:max){
    cat <- list(variable[factor == i])
    cat_List <- c(cat_List, cat)
  }
  cat_df <- statistics_df(cat_List, categories)

  library(ggridges)
  library(hrbrthemes)
  
  density_plot <- ggplot(data, aes(x=variable, group=factor, fill=factor)) +
    geom_density(adjust=1, alpha=0.6, col="darkseagreen3") +
    scale_fill_cyclical(values = color_pal[1:length(unique(factor))],
                        labels = categories,
                        name = "",
                        guide = "legend") +
    ylab("") + xlab("") +
    ggtitle("") + theme(plot.title=element_text(hjust=0.5)) +
    theme_minimal(base_size = 10)
  
  p1 <- ggarrange(boxplot, density_plot, ncol = 1, nrow = 2, legend="bottom")
  p2 <- ggarrange(cat_df, ncol = 1, nrow = 1)
  plot <- ggarrange(p1, p2, ncol = 2, nrow = 1, widths = c(0.06, 0.06, 4)) + 
    theme(plot.margin = margin(0.6,0.4,0.4,0.6, "cm"))
  title <- paste("Boxplot and Density plot with Measurement Results of", toupper(deparse(substitute(variable))), "by Groups")
  annotate_figure(plot, top = text_grob(title, col="coral3", size=14))
}


# Create list of grouped data based on a numeric variable. (arg: numeric, categorical)
create_list_based_on_cat <- function(variable, categorical){
  cat_List <- list()
  factor <- as.numeric(unlist(data[deparse(substitute(categorical))]))
  min <- min(as.integer(factor))
  max <- max(as.integer(factor))
  for (i in min:max){
    
    f <- variable[factor == i]
    cat <- list(variable[factor == i])
    cat_List <- c(cat_List, cat)
  }
  return (cat_List)
}


# Table and percentage plot. (arg: name1, name2, categories1, categories2, title, manual_title, x_label)
data_table_and_percentage_barplot <- function(var1, var2, names_var1, names_var2, plot_title, manual_name, x_label){
  library(stats)
  var1 <- as.factor(as.numeric(unlist(data[deparse(substitute(var1))])))
  var2 <- as.factor(as.numeric(unlist(data[deparse(substitute(var2))])))
  
  table_ <- table(var1, var2)
  names(dimnames(table_)) <- c(x_label, manual_name)
  colnames(table_) <- names_var2
  rownames(table_) <- names_var1
  print(table_)
  
  barplot <- data %>% 
    count(var2 = factor(var2), var1 = factor(var1)) %>% 
    mutate(pct = prop.table(n)) %>% 
    ggplot(aes(x = var1, y = n, fill = var2, label = scales::percent(pct), width=0.5)) + 
    geom_col(position = 'dodge') +
    geom_text(position = position_dodge(width = 0.5),
              vjust = -1, size = 3) +
    scale_fill_manual(values = color_bar_dich[1:2],
                      labels = names_var2,
                      name = manual_name) +
    scale_x_discrete(labels = names_var1) +
    ylab("Counts") + xlab(x_label) + ggtitle(plot_title) + 
    theme_wsj(base_size = 7) + theme(plot.title=element_text(hjust=0.5))
  return (barplot)
}


# Barplot for one variable. (arg: name_column, vector_of_categories_names)
barplot_ <- function(var, categories){
  var_ <- as.factor(as.numeric(unlist(data[deparse(substitute(var))])))
  barplot <- data %>% 
    count(var = factor(var_)) %>% 
    mutate(pct = prop.table(n)) %>% 
    ggplot(aes(x = var, y = n, label = scales::percent(pct))) +
    geom_bar(stat="identity", fill="#5F9EA0", width=0.4, alpha=0.95) +
    geom_text(position = position_dodge(width = 0.6),
              vjust = -0.5, size = 3.5) +
    scale_x_discrete(labels = categories) +
    ylab("") + xlab(toupper(deparse(substitute(var)))) + ggtitle("Barplot") + 
    theme_wsj(base_size = 7) + theme(plot.title=element_text(hjust=0.5))
  return (barplot)
}





