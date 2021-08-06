#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%#%
# Green Leader paper
# 2021-08-06
# Figure 1: descriptive statistics
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
# Loading data
#%#%#%#%#%#%#%#%#%#%

df <- fread(paste(getwd(), "/Fig1_data.csv", sep = ""))

#%#%#%#%#%#%
# Share of hotel class (Fig 1F)
#%#%#%#%#%#%

# Preparation: values to factors and changed labels
df_hc <- df %>% filter(HotelClass %in% c(1,2,3,4,5)) %>% 
  mutate(HotelClass = factor(HotelClass, levels = c(5,4,3,2,1)),
         GreenLeaderBinary = ifelse(GreenLeaderBinary == 1, "Green Leader", "Other"))

# Labels: label name and position on y axsis
shares <- df_hc %>% group_by(GreenLeaderBinary, HotelClass) %>% summarise(count = n()) %>%
  mutate(rel = round((count / sum(count))*100)) %>% arrange(GreenLeaderBinary, desc(HotelClass)) %>% 
  mutate(y = cumsum(rel), y2 = (y + lag(y))/2)

# Add position of labels on x axis
shares$x <- c(rep(1.4,5), rep(2.4,5))

# Change position of some labels on y axis
shares <- shares %>% mutate(y = ifelse(GreenLeaderBinary == "Other" & HotelClass == 4, y - 5,y),
                            y2 = ifelse(GreenLeaderBinary == "Other" & HotelClass == 4, y - 5,
                                       ifelse(GreenLeaderBinary == "Green Leader" & HotelClass == 1, 0,
                                              ifelse(GreenLeaderBinary == "Other" & HotelClass == 1, 0, y2))))

# Plot
df_hc %>% 
  ggplot(aes(factor(GreenLeaderBinary), fill = HotelClass)) +
  # Elements of plot
  geom_bar(position = "fill", width = 0.5, colour = "black", lwd = 0.1) + 
  geom_text(data = shares, aes(x = x, y = (y2+2)/100, label = rel)) +
  geom_segment(data = shares, aes(x = x + 0.05, xend =  x -0.15, y = (y2)/100, yend = (y2)/100),
               lwd = 0.1) +
  # Scales
  scale_y_continuous(expand = c(0,0,0,0.03), labels = percent_format()) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[8],
                               brewer.pal(11,"RdBu")[6],
                               brewer.pal(11,"RdBu")[4],
                               brewer.pal(11,"RdBu")[2])) +
  # Labs and themes
  labs(x = "", y = "Share of hotels per class (%)", fill = "Hotel class\n(stars)") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank())

#%#%#%#%#%#%
# Accommodation type (Fig 1E)
#%#%#%#%#%#%

df_at <- df %>% mutate(AccType = ifelse(BBInnBinary == 0 & SpecialtBinary == 0, "Hotel",
                                        ifelse(BBInnBinary == 1, "Bnb / Inn",
                                               ifelse(SpecialtBinary == 1, "Specialty Acc.", "Other"))),
                       AccType = factor(AccType, levels = c("Hotel", "Bnb / Inn", "Specialty Acc.")),
                       GreenLeaderBinary = ifelse(GreenLeaderBinary == 1, "Green Leader", "Other"))

# Labels: label name and position on y axsis
shares <- df_at %>% group_by(GreenLeaderBinary, AccType) %>% summarise(count = n()) %>%
  mutate(rel = round((count / sum(count))*100)) %>% arrange(GreenLeaderBinary, desc(AccType)) %>% 
  mutate(y = cumsum(rel), y2 = (y + lag(y))/2)

# Add position of labels on x axis
shares$x <- c(rep(1.4,3), rep(2.4,3))

# Change position of some labels on y axis
shares <- shares %>% mutate(y2 = ifelse(GreenLeaderBinary == "Green Leader" & AccType == "Specialty Acc.", 0,
                                        ifelse(GreenLeaderBinary == "Other" & AccType == "Specialty Acc.", 0, y2)))

# Plot
df_at %>% 
  ggplot(aes(factor(GreenLeaderBinary), fill = AccType)) +
  # Elements of plot
  geom_bar(position = "fill", width = 0.5, colour = "black", lwd = 0.1) + 
  geom_text(data = shares, aes(x = x, y = (y2+2)/100, label = rel)) +
  geom_segment(data = shares, aes(x = x + 0.05, xend =  x -0.15, y = (y2)/100, yend = (y2)/100),
               lwd = 0.1) +
  # Scales
  scale_y_continuous(expand = c(0,0,0,0.03), labels = percent_format()) +
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[9],
                               brewer.pal(11,"RdBu")[6],
                               brewer.pal(11,"RdBu")[3])) +
  # Labs and themes
  labs(x = "", y = "Accommodation type (%)", fill = "") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank())

#%#%#%#%#%#%
# Number of rooms (Fig 1B)
#%#%#%#%#%#%

df_nr <- df %>% mutate(GreenLeaderBinary = ifelse(GreenLeaderBinary == 1, "Green Leader", "Other"))

df_nr2 <- df_nr %>% filter(GreenLeaderBinary == "Green Leader")

size <- nrow(df_nr2)
df_nr3 <- df_nr %>% filter(GreenLeaderBinary != "Green Leader") %>% sample_n(size)

df_nr <- rbind(df_nr2, df_nr3)

label <- df_nr %>% group_by(GreenLeaderBinary) %>% summarise(med_nr = median(NumberRooms)) %>%
                  mutate(x = c(1.4,2.4))

df_nr %>%
  ggplot(aes(x = GreenLeaderBinary, y = NumberRooms, fill = GreenLeaderBinary)) + 
  geom_point(position = position_jitter(0.2,0.2), alpha = 0.1, shape = 21, size = 2.5) + 
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_y_log10() + geom_boxplot(fill = NA, width = 0.6, outlier.alpha = 0) +
  geom_text(data = label, aes(x = x, y = med_nr, label = med_nr)) +
  labs(x = "", y = "Number of rooms", fill = "") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank()) +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

#%#%#%#%#%#%
# Languages spoken (Fig 1D)
#%#%#%#%#%#%

label <- df_nr %>% group_by(GreenLeaderBinary) %>% 
  summarise(med_ls = median(LanguagesSpoken)) %>%
  mutate(x = c(1.4,2.4))

df_nr %>% 
  ggplot(aes(x = GreenLeaderBinary, y = LanguagesSpoken, fill = GreenLeaderBinary)) + 
  geom_point(position = position_jitter(0.2,0.2), alpha = 0.1, shape = 21, size = 2.5) + 
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_y_log10() + 
  geom_boxplot(fill = NA, width = 0.6, outlier.alpha = 0) +
  geom_text(data = label, aes(x = x, y = med_ls, label = med_ls)) +
  labs(x = "", y = "Number of languages spoken by staff", fill = "") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank()) +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

#%#%#%#%#%#%
# Amenities (Fig 1G)
#%#%#%#%#%#%

df_am <- df_nr %>% dplyr::select(GreenLeaderBinary, NonSmokingRoomType, 
                                 AmenitiesBaggage, AmenitiesBar, 
                                 AmenitiesRestaurant, AmenitiesFitness,
                                 AmenitiesSauna, AmenitiesConference, AmenitiesBreakfast, AmenitiesMeetingRooms) %>% gather(key, value, -GreenLeaderBinary)

df_am <- df_am %>% mutate(key = ifelse(key == "AmenitiesBaggage", "Baggage storage",
                                       ifelse(key == "AmenitiesBar", "Bar",
                                              ifelse(key == "AmenitiesRestaurant", "Restaurant",
                                                     ifelse(key == "AmenitiesBreakfast", "Breakfast served",
                                                            ifelse(key == "AmenitiesFitness", "Fitness room",
                                                                   ifelse(key == "AmenitiesSauna", "Spa",
                                                                          ifelse(key == "NonSmokingRoomType", "Non smoking rooms",
                                                                                 ifelse(key == "AmenitiesConference", "Conference room",
                                                                                        ifelse(key == "AmenitiesMeetingRooms", "Meeting rooms","Other"))))))))))

df_am <- df_am %>% mutate(key = factor(key, levels = c("Non smoking rooms", "Breakfast served", 
                                                   "Baggage storage", "Bar" ,"Restaurant", "Meeting rooms",
                                                   "Conference room", "Fitness room", "Spa")),
                          GreenLeaderBinary = factor(GreenLeaderBinary, levels = c("Other", "Green Leader")))

df_am %>%
  ggplot(aes(y = GreenLeaderBinary, fill = factor(value))) + 
  geom_bar(position = "fill", colour = "black", lwd = 0.1) + 
  facet_wrap(~key, ncol = 1) +
  labs(y = "", x = "Share of accommodations", fill = "") +
  scale_fill_manual(values = c("white", brewer.pal(11,"RdBu")[10])) +
  scale_x_continuous(expand = c(0,0,0,0.05), labels = percent_format()) +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"), legend.position = "none",
        axis.text.y = element_text(size = 8),
        panel.grid = element_blank()) +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

#%#%#%#%#%#%
# Photos per room (Fig 1C)
#%#%#%#%#%#%

df_tp <- df_nr %>% mutate(photos_room = TotalPhotos / NumberRooms)

label <- df_tp %>% group_by(GreenLeaderBinary) %>% 
  summarise(med_tp = median(photos_room)) %>%
  mutate(x = c(1.4,2.4))

df_tp %>% 
  ggplot(aes(x = GreenLeaderBinary, y = photos_room, fill = GreenLeaderBinary)) + 
  geom_point(position = position_jitter(0.2,0.2), alpha = 0.1, shape = 21, size = 2.5) + 
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(colour = "grey", sides = "l") +
  geom_boxplot(fill = NA, width = 0.6, outlier.alpha = 0) +
  geom_text(data = label, aes(x = x, y = med_tp, label = round(med_tp))) +
  labs(x = "", y = "Total no. of photos / no. of rooms", fill = "") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank()) +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

#%#%#%#%#%#%
# Reviews per room (Fig 1A)
#%#%#%#%#%#%

df_rr <- df_nr %>% mutate(reviews_room = ReviewCount / NumberRooms)

label <- df_rr %>% group_by(GreenLeaderBinary) %>% 
  summarise(med_rr = median(reviews_room)) %>%
  mutate(x = c(1.4,2.4))

df_rr %>% filter(reviews_room > 0.01) %>%
  ggplot(aes(x = GreenLeaderBinary, y = reviews_room, fill = GreenLeaderBinary)) + 
  geom_point(position = position_jitter(0.2,0.2), alpha = 0.1, shape = 21, size = 2.5) + 
  scale_fill_manual(values = c(brewer.pal(11,"RdBu")[10],
                               brewer.pal(11,"RdBu")[2])) + 
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x), labels = trans_format("log10", math_format(10^.x))) + 
  annotation_logticks(colour = "grey", sides = "l") +
  geom_boxplot(fill = NA, width = 0.6, outlier.alpha = 0) +
  geom_text(data = label, aes(x = x, y = med_rr, label = round(med_rr))) +
  labs(x = "", y = "Total no. of user reviews / no. of rooms", fill = "") +
  theme_minimal() +
  theme(axis.line = element_line(colour = "black"), text = element_text(size = 14),
        axis.text.x = element_text(face = "bold"),
        panel.grid = element_blank()) +
  guides(fill=guide_legend(override.aes=list(alpha=1)))

# END OF SCRIPT
#==============