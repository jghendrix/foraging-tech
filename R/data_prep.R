## Data import and tidying
## Jack Hendrix

library(tidyverse)
library(magrittr)
library(viridis)
library(visreg)
library(scales)
library(conflicted)
conflicts_prefer(dplyr::select, dplyr::filter)

# Import literature review data ----
data <- read_csv("input/data.csv")
data %>%
  group_by(Included) %>%
  summarise(n = n())
# how many ended up actually getting read? 604
# 125 of 729 left behind

# omit those irrelevant papers & drop some extraneous columns
data %<>%
  filter(Included == "yes") %>%
  select(-c("Authors", "Title", "Abstract", "DOI", "Included"))

# Identifying methods per study ----
# Split Methods_subgroup column up by the commas
data %<>%
  separate(Methods_subgroup, c("method1", "method2", "method3", "method4", "method5"), ", ")

# How many different unique methods obs are there?
m1 <- data %>% select(c("method1")) %>% rename(method = method1)
m2 <- data %>% select(c("method2")) %>% rename(method = method2)
m3 <- data %>% select(c("method3")) %>% rename(method = method3)
m4 <- data %>% select(c("method4")) %>% rename(method = method4)
m5 <- data %>% select(c("method5")) %>% rename(method = method5)

ms <- rbind(m1,m2,m3,m4,m5) %>% filter(!is.na(method))
ms %<>% filter(!duplicated(method))

# 39 unique methods, but some of them are essentially the same

## Correcting some errors/summarising methods ----

# distinguish trail cams from nest cams
nests <- data %>% filter(Primary_alt_behav == "nest_attendance" | Primary_alt_behav == "offspring_care")

nests$method1 <- gsub("camera_trap", "nest_cam", nests$method1)
nests$method2 <- gsub("camera_trap", "nest_cam", nests$method2)
nests$method3 <- gsub("camera_trap", "nest_cam", nests$method3)

non_nests <- data %>% filter(!(Primary_alt_behav %in% c("nest_attendance", "offspring_care")))

data <- rbind(nests, non_nests)

## RSF/SSF/HMM/FPT/habitat_selection are statistical, not methods ----
code <- c("RSF", "SSF", "HMM", "first_passage_time", "habitat_selection")
stats <- data %>% filter(method1 %in% c(code)|
                           method2 %in% c(code)|
                           method3 %in% c(code)|
                           method4 %in% c(code)|
                           method5 %in% c(code))

# two studies where they actually matter
data %<>% mutate(method2 = ifelse(StudyID == "McKellar_2015", 
                                  "radio_triang", method2),
                 method1 = ifelse(method1 == "habitat_selection",
                                  "feeding_evidence", method1))

# make them NAs, they're redundant; none in method 1
data$method2 <- gsub("RSF", NA, data$method2)
data$method2 <- gsub("SSF", NA, data$method2)
data$method2 <- gsub("HMM", NA, data$method2)
data$method2 <- gsub("first_passage_time", NA, data$method2)
data$method3 <- gsub("RSF", NA, data$method3)
data$method3 <- gsub("SSF", NA, data$method3)
data$method3 <- gsub("HMM", NA, data$method3)

# how many unique now?
m1 <- data %>% select(c("method1")) %>% rename(method = method1)
m2 <- data %>% select(c("method2")) %>% rename(method = method2)
m3 <- data %>% select(c("method3")) %>% rename(method = method3)
m4 <- data %>% select(c("method4")) %>% rename(method = method4)
m5 <- data %>% select(c("method5")) %>% rename(method = method5)

ms <- rbind(m1,m2,m3,m4,m5) %>% filter(!is.na(method))
ms %<>% filter(!duplicated(method))

# 35 unique methods

long_dat <- data %>% 
  pivot_longer(cols = (method1:method5), names_to = "n_met") %>%
  rename(method = value) %>%
  filter(!is.na(method))
# How to categorise these methods into groups?

# k-means clustering analysis ----

# Create binary variables describing each method (details in Appendix of manuscript)
clusters <- ms %>% mutate(obs_present  = 0,
                          subj_present = 0,
                          spatial_only = 0,
                          fixed_point = 0,
                          animal_borne = 0,
                          evidence = 0)

obs <- c("all_day_follow", "choice_trial", "direct_obs", "direct_video", "ethogram", "focal_sampling", "instantaneous_scan", "trapping","caching", "diet_analysis", "fecal_analysis", "feeding_evidence", "isotope", "radio_tracking", "tracking", "thread_trailing", "veg_survey")
subj <- c("all_day_follow", "choice_trial", "direct_obs", "direct_video", "ethogram", "focal_sampling", "instantaneous_scan", "trapping","activity_logger", "audio_logger", "axy", "depth_sensor", "drone", "light_sensor", "magnetometer",  "heart_rate_logger", "posture_sensor", "temperature_logger", "video_logger")
spatial <- c("GPS", "habitat_selection","PIT_tag", "radio_triang")
fixed <- c("trapping", "audio_trap", "camera_trap", "nest_cam", "hair_snare", "PIT_tag")
borne <- c("radio_tracking", "thread_trailing", "activity_logger", "audio_logger", "axy", "depth_sensor", "light_sensor", "magnetometer",  "heart_rate_logger", "posture_sensor", "temperature_logger", "video_logger", "GPS", "PIT_tag", "radio_triang")
evi <- c("caching", "diet_analysis", "fecal_analysis", "feeding_evidence", "tracking", "veg_survey", "hair_snare")

clusters %<>% mutate(obs_present  = ifelse(method %in% c(obs), 1, 0),
                     subj_present = ifelse(method %in% c(subj), 1, 0),
                     spatial_only = ifelse(method %in% c(spatial), 1, 0),
                     fixed_point = ifelse(method %in% c(fixed), 1, 0),
                     animal_borne = ifelse(method %in% c(borne), 1, 0),
                     evidence = ifelse(method %in% c(evi), 1, 0))

# Iterate between 2 and 10 clusters to compare which grouping is best
k2 <- kmeans(clusters[, 2:7], 2, nstart = 20)
k2g <- data_frame(c(k2$cluster))
k3 <- kmeans(clusters[, 2:7], 3, nstart = 20)
k3g <- data_frame(c(k3$cluster))
k4 <- kmeans(clusters[, 2:7], 4, nstart = 20)
k4g <- data_frame(c(k4$cluster))
k5 <- kmeans(clusters[, 2:7], 5, nstart = 20)
k5g <- data_frame(c(k5$cluster))
k6 <- kmeans(clusters[, 2:7], 6, nstart = 20)
k6g <- data_frame(c(k6$cluster))
k7 <- kmeans(clusters[, 2:7], 7, nstart = 20)
k7g <- data_frame(c(k7$cluster))
k8 <- kmeans(clusters[, 2:7], 8, nstart = 20)
k8g <- data_frame(c(k8$cluster))
k9 <- kmeans(clusters[, 2:7], 9, nstart = 20)
k9g <- data_frame(c(k9$cluster))
k10 <- kmeans(clusters[, 2:7], 10, nstart = 20)
k10g <- data_frame(c(k10$cluster))

# evaluating which is the best fit to the data
# comparing within and between sum of squares
m <- c(2, 3, 4, 5, 6, 7, 8, 9, 10)
ss <- c(k2$tot.withinss, k3$tot.withinss, k4$tot.withinss, k5$tot.withinss, k6$tot.withinss, k7$tot.withinss, k8$tot.withinss, k9$tot.withinss, k10$tot.withinss)
wss <- as.data.frame(cbind(m, ss))

# Plot within-sum squares to see where the inflection point in the curve is
ggplot(wss, aes(x = m, y = ss)) +
  geom_point() +
  geom_line() +
  xlab("k") +
  ylab("WSS") +
  theme_bw() +
  theme(panel.grid = element_blank())
# around k = 5 or 6 probably is a good number of groups

# Visualize composition of clusters - where do methods get moved from one to another?
library(clustree)
ks <- cbind(ms, k2g, k3g, k4g, k5g, k6g, k7g, k8g, k9g, k10g)
names(ks) <- c("method", "k2", "k3", "k4", "k5", "k6", "k7", "k8", "k9", "k10")

png("figures/node_tree.png", width=2800, height=4200, res=500, units="px")
clustree(ks, prefix = "k", layout = "sugiyama") +
  scale_colour_viridis(discrete = TRUE, option = "A", begin = 0.25, end = 0.85)

dev.off()

# From four to 6 groups, fairly consistent assignment of methods; zoom in on five to see how they actually differ

ks_long <- ks %>% select(c(1, 4:6)) %>%
  pivot_longer(cols = c(k4,k5,k6), names_to = "k")

k6s <- ks_long %>% filter(k == "k6") %>% arrange(value)

ks_long$method <- factor(ks_long$method, levels = k6s$method)

ggplot(ks_long, aes(x = method, y = value, colour = k)) +
  geom_jitter(width = 0.2, height = 0) +
  ylab("Group") +
  xlab("") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = -90))

# Biologgers are always together
# Direct observations are always together

# From 4 to 5 groups, k4 group 2 splits into k5 group2 and k5 group5
# generic "remote" non-biologger in k4 becomes remote spatial (GPS) vs. remote audiovisual (camera traps, hair snares) with 5 groups

# k5 group2 becomes groups 5 and 6 in k6
# remote spatial splitting further into two smaller categories, not really adding anything to the schema

## k = 5 is the system I'm going to go with moving forward ----
groups <- ks %>% select(c(method, k5)) %>%
  mutate(k5 = ifelse(k5 == 1, "tracking", k5),
         k5 = ifelse(k5 == 2, "remote spatial", k5),
         k5 = ifelse(k5 == 3, "direct obs", k5),
         k5 = ifelse(k5 == 4, "biologger", k5),
         k5 = ifelse(k5 == 5, "remote AV", k5))

# but in the k = 5 schema, radio_tracking gets thrown in with spatial and not with indirect. I want to manually change that. Overwrite those.
groups %<>% mutate(k5 = ifelse(method == "radio_tracking", "tracking", k5))

data_by5 <- left_join(long_dat, groups, by = "method")

# how many studies have > 1 method in the same group?
data_dup <- data_by5 %>% group_by(StudyID, k5) %>%
  filter(n() > 1)
length(unique(data_dup$StudyID))
# 129 studies had multiple methods but fell into a single category, ie. accelerometer and audiologger both result in "biologger" but with two categories

# Want to only keep unique StudyID - category combinations, so we want to remove these 'duplicate' rows
data <- data_by5 %>% filter(!duplicated(paste0(StudyID,k5)))

# Save this k-means clustered data
write.csv(data, "output/data_categorised.csv")

# How many of each of the five categories are there across studies?
# remembering 604 unique studies, so these will sum to > 100%
data %>% group_by(k5) %>% summarise(n = n(), prop = n/604)
# 320 direct obs, 53% of the 604
# 213 remote spatial, 35.3% of the 604
# 102 tracking, 16.9% of the 604
# 68 biologger, 11.3% of the 604 papers
# 52 remote AV, 8.6% of the 604


# Cleaning up Spatial and Temporal scales for use as response variables ----

data %<>% mutate(Spatial = as.numeric(Spatial_area_sqkm),
                     Temporal = as.numeric(Temporal_range))

# correct one temporal that was misentered
data_by5 %<>% mutate(Temporal = ifelse(StudyID == "Spencer_2018", 33, Temporal))

# A number of missing values for Spatial, less so for Temporal

# Where are the missing spatial scales?
data %>% filter(is.na(Spatial)) %>% group_by(k5) %>%
  summarise(n = n())
# primarily direct obs, but a lot of them (277) are missing regardless

# What about missing temporal scale?
data %>% filter(is.na(Temporal)) %>%
  group_by(k5) %>%
  summarise(n = n())
# again, mostly direct obs,; 12/25 obs = 48%

studies <- data %>% filter(is.na(Spatial)) %>% filter(!duplicated(StudyID))
# the 277 records w/o area are from 230 studies

data %>% filter(is.na(Temporal)) %>% 
  group_by(k5) %>% summarise(n = n()) 
# 25 approaches w/o temporal range from 22 studies

# For Spatial: if the area was reported as e.g. "10 points", then replace this with 100m2 per point
# assuming 10m x 10m is the observable area of a point location
# If no area is reported at all, impute 100m2 for unknowns
# we don't want to lose a huge chunk of our data, and this feels a reasonable compromise
points <- data %>% subset(grepl("point", Spatial_area_sqkm)) %>%
  mutate(points = str_remove(Spatial_area_sqkm, "point"),
         points = str_remove(points, " "),
         points = str_remove(points, "s")) %>%
  # assume one point = 10m x 10m = 0.0001km2
  mutate(area = 0.0001*as.numeric(points)) %>%
  select(c(StudyID, area)) %>%
  filter(!duplicated(StudyID))

data <- left_join(data, points, by = "StudyID")       

data %<>% mutate(Spatial = ifelse(!is.na(area), area, Spatial),
                 Spatial = ifelse(is.na(Spatial), 0.0001, Spatial))

# Leave the unknown Temporal as is
# It's a much smaller proportion, easier to omit those from temporal analyses than try to estimate as we did for Spatial

write.csv(data, "output/data_w_scales.csv")