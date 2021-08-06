#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Green Leader paper
# 2021-08-06
# Figures 3A and 3B: ML performance
# Pleae note: the confusion matrices have been produced using LaTeX and are not included in this script
# Fabian Braesemann 
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%

#%#%#%#%#%#%#%#%#%#%
# Loading packages
#%#%#%#%#%#%#%#%#%#%
library(tidyverse)    # Numerous data wrangling packages
library(data.table)   # Quick data loading
library(lubridate)    # Working with dates
library(RColorBrewer) # Nice colours for plots
library(ggpubr)       # Arrange multiple plots
library(scales)       # Special scales for plots (log, %)

#%#%#%#%#%#%#%#%#%#%
# Figure 3A
#%#%#%#%#%#%#%#%#%#%

df <- fread(paste(getwd(), "/Fig3A_data.csv", sep = ""))

# Transform data from wide to long format
df <- df %>% gather(key, value, -Classifier, -Pipeline)

# Create a new column that identifies the best performing classifiers (fifth category in plot)
df <- df %>% mutate(best_classifier = ifelse(Classifier == "QDA" & Pipeline == "None_Rand_Under_None" & key == "Recall", "best classifier",
                                             ifelse(Classifier == "QDA" & Pipeline == "Robust_Scaler_SMOTE_Under_None" & key == "F2", "best classifier",
                                                    ifelse(Classifier == "Random Forest" & Pipeline == "MinMax_SMOTE_None" & key == "ROC AUC", "best classifier",Classifier))))

# Rename axis text for the plot
df <- df %>% mutate(Classifier = ifelse(Classifier == "LDA", "Linear\nDiscriminant A.",
                                        ifelse(Classifier == "QDA", "Quadratic\nDiscriminant A.",
                                               ifelse(Classifier == "Random Forest", "Random\nForest",
                                                      ifelse(Classifier == "Logistic Regression", "Logistic\nRegression",Classifier)))),
                    best_classifier = ifelse(best_classifier == "LDA", "Linear\nDiscriminant A.",
                                        ifelse(best_classifier == "QDA", "Quadratic\nDiscriminant A.",
                                               ifelse(best_classifier == "Random Forest", "Random\nForest",
                                                      ifelse(best_classifier == "Logistic Regression", "Logistic\nRegression",best_classifier)))))

# Plot
df %>%
  ggplot(aes(x = Classifier, y = value, fill = Classifier, 
             # Colour, size, and shape refer to best classifier column (five categories)
             colour = best_classifier, size = best_classifier, shape = best_classifier)) + 
  geom_point(position = position_jitter(0.25,0), alpha = 0.75) +
  facet_wrap(~key, nrow = 1, scales = "free") +
  # Adjust size, shape, and colour of points
  scale_size_manual(values = c(2.5,2.5,2.5,2.5,5)) +
  scale_shape_manual(values = c(21,21,21,21,24)) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[8],
                               brewer.pal(11,"RdBu")[4],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[8],
                               brewer.pal(11,"RdBu")[4],
                               brewer.pal(11,"RdBu")[2],
                               "black")) + 
  # Axis labels
  labs(x= "", y = "",fill = "", colour = "", alpha = "") +
  # Add vertical and horizontal axis lines
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.8)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.8) +
  # Theme and layout definition
  theme_minimal() +
  theme(text = element_text(size = 13),
        strip.text = element_text(size = 13, face = "bold"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5), 
        legend.position = "none",
        panel.grid = element_blank())

#%#%#%#%#%#%#%#%#%#%
# Fig 3 B (inset): Compare ML and random draw performance
#%#%#%#%#%#%#%#%#%#%

# Load performance of random draw data (in total 40,000 draws)
random_draw <- fread(paste(getwd(), "/Fig3B_inset_data.csv", sep = ""))

# Filter, select and rename variables of ML performance for the plot
ml_recall <- df %>% filter(key == "Recall") %>% select(key, value) %>% mutate(key2 = "Machine learning model")
ml_f2 <- df %>% filter(key == "F2") %>% select(key, value) %>% mutate(key = ifelse(key == "F2", "F2 score", NA)) %>% mutate(key2 = "Machine learning model")

# Combin ML performance and random draw data
random_draw <- rbind(random_draw, ml_recall, ml_f2)
# Re-order keys
random_draw <- random_draw %>% mutate(key2 = factor(key2, levels = c("Random draw", "Machine learning model")))

# Calculate the mean performance for the vertical lines
means <- random_draw %>% group_by(key2, key) %>% summarise(mean_val = median(value)) 
# Position of vertical lines
means <- means %>% ungroup() %>% mutate(x_pos = c(0.09,0.15,0.265,0.565))

# Plot
random_draw %>%
  ggplot(aes(x = value, fill = key2)) + 
  geom_density(aes(color = key2), bw = 0.002, alpha = 0.4) +
  # Plot median line and text
  geom_vline(data = means, aes(xintercept = mean_val, colour = key2)) + 
  geom_text(data = means, aes(x = x_pos, y = 25, colour = key2, label = paste("Median:", round(mean_val,2)))) + 
  facet_wrap(~key, nrow =2, scales = "free") +
  # Colours and axis 
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_colour_manual(values = c(brewer.pal(11,"RdBu")[10],
                                 brewer.pal(11,"RdBu")[2])) + 
  scale_x_continuous(expand = c(0,0,0,0)) +
  scale_y_continuous(expand = c(0,0,0,0)) +
  # Labels
  labs(x = "", y = "Density", fill = "", colour = "") +
  # Add vertical and horizontal axis lines
  annotate("segment", x=-Inf, xend=Inf, y=-Inf, yend=-Inf, lwd = 0.8)+
  annotate("segment", x=-Inf, xend=-Inf, y=-Inf, yend=Inf, lwd = 0.8) +
  # Theme and layout definition
  theme_minimal() +
  theme(text = element_text(size = 14),
        strip.text = element_text(size = 13, face = "bold"),
        panel.grid = element_blank(),
        legend.position = "bottom") +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

# END OF SCRIPT
#==============