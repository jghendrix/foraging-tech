## Foraging-technology literature review - analyses
## Jack Hendrix

library(tidyverse)
library(magrittr)
library(viridis)
library(visreg)
library(scales)

# Data import and tidying ----
data <- read_csv("output/data_w_scales.csv") %>%
  select(-c(1, 3:9, 17:20, 26)) %>%
  mutate(k5 = as.factor(k5))

# Assign signature colours to each of the five approaches for consistency, from viridis palette
colours <- c("biologger" = "#FEBA2CFF", "direct obs" = "#E77059FF", "remote AV" = "#B7318AFF", "remote spatial" = "#6F00A8FF", "tracking" = "#0D0887FF")
# use scale_colour_manual(values = colours)

# labels including the sample size for the spatial/temporal combined
colours_n <- c("biologger n = 67" = "#FEBA2CFF", "direct obs n = 312" = "#E77059FF", "remote AV n = 53" = "#B7318AFF", "remote spatial n = 210" = "#6F00A8FF", "tracking n = 98" = "#0D0887FF")


# Main analyses ----
## 1 - scale of studies by method ----

cor.test(data$Spatial, data$Temporal)
cor.test(log(data$Spatial), log(data$Temporal))
# for untransformed scales, r = 0.048, t = 1.28, df = 728, p = 0.20
# for log: r = 0.146, t = 3.987, df = 728, p << 0.001

summary(area <- aov(log(Spatial) ~ k5, data = data))
TukeyHSD(area)
# spatial > tracking > biologger > remote AV > direct obs
#   A         B           BC           CD          D

summary(time <- aov(log(Temporal) ~ k5, data = data))
TukeyHSD(time)
# tracking > direct obs > remote spatial > biologger > remote AV
#     A         A             A               AB           B

### Temporal scale as extent vs. proportion ----
prop_time <- data %>% filter(!is.na(Temporal)) %>%
  mutate(indiv = as.numeric(n_indiv),
         indiv = ifelse(is.na(indiv), 1, indiv),
         days = as.numeric(Days_obs_per_indiv),
         fixes = as.numeric(Fixes_per_indiv))

# fix some errors in data entry for five studies with obs.time > extent
prop_time %<>%
  mutate(days = ifelse(StudyID == "Bagniewska_2013", Temporal/indiv, days),
         fixes = ifelse(StudyID == "Bagniewska_2013", fixes/5, fixes),
         fixes = ifelse(StudyID == "Camacho_2014", days, fixes),
         days = ifelse(StudyID == "Camacho_2014", NA, days),
         fixes = ifelse(StudyID == "Lundy_2012", days, fixes),
         days = ifelse(StudyID == "Lundy_2012", NA, days),
         days = ifelse(StudyID == "Pedrozo_2018", 10.625, days),
         Temporal = ifelse(StudyID == "Spencer_2018", 33, Temporal),
         days = ifelse(StudyID == "Spencer_2018", 0.0836, days))

# Lots of missing values for observation period, particularly from the remote spatial where it was recorded as a number of fixes instead of a time duration
prop_time %>% filter(!is.na(fixes)) %>%
  group_by(k5) %>%
  summarise(n = n())
# 46/67 biologger 69%
# 70/312 ethology 22%
# 17/53 remote AV 32%
# 179/210 remote spatial 85%
# 50/98 tracking 51%

length(unique(subset(prop_time, !is.na(fixes))$StudyID))

# how to synthesize studies using observation periods with those looking at fixes?
# impute one fix = one second (1/86400 of a day)
prop_time %<>% mutate(days = ifelse(is.na(days), fixes/86400, days))

# new model with one fix = one second of obs
prop_time %<>% mutate(per_capita_prop = days/Temporal,
                      total_obs = indiv*days,
                      obs_prop = total_obs/Temporal)

# Proportion can't go above 100%
prop_time %<>% mutate(obs_prop = ifelse(obs_prop > 1, 1, obs_prop))

summary(obs_time <- aov(obs_prop ~ k5, data = prop_time))
TukeyHSD(obs_time)

# For proportion of study duration spent recording data:
# remote AV > biologger > tracking > direct obs > remote spatial
#     B           B          A            A            A

## 2 - Which behavioural stage is being examined by which approach? ----

# studies using multiple approaches and looking at multiple stages... it expands out exponentially
data %<>%
  mutate(feeding = ifelse(str_detect(Stage, "A"), 1, 0),
         searching = ifelse(str_detect(Stage, "B"), 1, 0),
         hs = ifelse(str_detect(Stage, "C"), 1, 0))
stages <- data %>% group_by(k5) %>%
  summarise(feed = mean(feeding),
            search = mean(searching),
            hab = mean(hs))

### Stage 1 - feeding -----
summary(feed_m <- glm(feeding ~ k5, data = data, family = "binomial"))
visreg(feed_m)

stages$k5 <- factor(stages$k5, levels = stages$k5[order(stages$feed)])


ggplot(stages, aes(x = k5, y = feed)) +
  geom_bar(stat = "identity")
# spatial < biologger < tracking = remote AV < direct obs

### Stage 2: searching ----

summary(search_m <- glm(searching ~ k5, data = data, family = "binomial"))
visreg(search_m)

stages$k5 <- factor(stages$k5, levels = stages$k5[order(stages$search)])

ggplot(stages, aes(x = k5, y = search)) +
  geom_bar(stat = "identity")

# spatial < tracking = remote AV < biologger = direct obs


### Stage 3: habitat selection ----
summary(hs_m <- glm(hs ~ k5, data = data, family = "binomial"))
visreg(hs_m)

stages$k5 <- factor(stages$k5, levels = stages$k5[order(stages$hab)])

ggplot(stages, aes(x = k5, y = hab)) +
  geom_bar(stat = "identity")

# direct obs < remote AV < tracking < biolog < spatial


# Figures ----

## Fig 2: Scatterplot of all spatial-temporal obs ----
ggplot(data, aes(x = log(Spatial), y = log(Temporal), colour = k5)) +
  geom_jitter() +
  theme_bw() +
  xlab(expression(log(Spatial~extent~(km^{"2"})))) +
  ylab("log(Temporal extent (days))") +
  scale_colour_manual(values = colours) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank())

## Summarising data ----
scales <- data %>% group_by(k5) %>%
  summarise(n = n(), 
            area = mean(log(Spatial)), 
            area.se = sd(log(Spatial))/sqrt(n),
            time = mean(log(Temporal), na.rm = T), 
            time.se = sd(log(Temporal), na.rm = T)/sqrt(n))

# add sample sizes to the k5 values?
data %>% group_by(k5) %>% summarise(n = n())
scales$k5n <- gsub("biologger", "biologger n = 67", scales$k5)
scales$k5n <- gsub("remote AV", "remote AV n = 53", scales$k5n)
scales$k5n <- gsub("remote spatial", "remote spatial n = 210", scales$k5n)
scales$k5n <- gsub("direct obs", "direct obs n = 312", scales$k5n)
scales$k5n <- gsub("tracking", "tracking n = 98", scales$k5n)

## Statistical significance from ANOVAs above
# log(Spatial) :
# direct < AV < biologger < tracking < spatial
#   A      AB      BC          CD        E

# log(Temporal) : 
# remote AV > biologger > remote spatial > direct obs > tracking
#     A         AB             B              B           B

scales %<>% mutate(spat.sig = ifelse(k5 == "direct obs", "A", 0),
                   spat.sig = ifelse(k5 == "remote AV", "AB", spat.sig),
                   spat.sig = ifelse(k5 == "biologger", "BC", spat.sig),
                   spat.sig = ifelse(k5 == "tracking", "CD", spat.sig),
                   spat.sig = ifelse(k5 == "remote spatial", "E", spat.sig),
                   temp.sig = ifelse(k5 == "remote AV", "A", 0),
                   temp.sig = ifelse(k5 == "biologger", "AB", temp.sig),
                   temp.sig = ifelse(k5 == "remote spatial", "B", temp.sig),
                   temp.sig = ifelse(k5 == "direct obs", "B", temp.sig),
                   temp.sig = ifelse(k5 == "tracking", "B", temp.sig))

### Fig 3: area by time summarised by k5 ----
ggplot(scales, aes(colour = k5n, x = area, y = time, xmin = area - area.se, xmax = area + area.se, ymin = time - time.se, ymax = time + time.se)) +
  geom_point(size = 3) +
  geom_errorbar(width = 0.4) +
  geom_errorbarh(height = 0.08) +
  xlab(expression(ln(Spatial~extent~(km^{"2"})))) +
  ylab("ln(Temporal extent (days))") +
  scale_colour_manual(values = colours_n) + 
  theme_bw() +
  theme(legend.title = element_blank(),
        panel.grid = element_blank())


### Fig 4: Spatial extent ----
scales$k5 <- factor(scales$k5, levels = scales$k5[order(scales$area, decreasing = FALSE)])

ggplot(scales, aes(x = k5, colour= k5, y = area, ymin = area - area.se, ymax = area + area.se)) +
  geom_point(size = 3) +
  geom_errorbar(size = 0.8, width = 0.5) +
  geom_text(data = scales, aes(label = spat.sig), 
            position = position_nudge(y = 1.5),
            size = 4, colour = "black") +
  ylab(expression(log(Spatial~extent~(km^{"2"})))) +
  xlab("") +
  scale_colour_manual(values = colours) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())


### Fig 5: Temporal extent ----

scales$k5 <- factor(scales$k5, levels = scales$k5[order(scales$time, decreasing = FALSE)])

ggplot(scales, aes(x = k5, colour= k5, y = time, ymin = time - time.se, ymax = time + time.se)) +
  geom_point(size = 3) +
  geom_errorbar(size = 0.8, width = 0.4) +
  geom_text(data = scales, aes(label = temp.sig), 
            position = position_nudge(y = 0.4),
            size = 4, colour = "black") +
  #scale_y_continuous(trans = "log2") +
  ylab("log(Temporal extent (days))") +
  xlab("") +
  scale_colour_manual(values = colours) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())

### Proportion obs. time
time_obs <- prop_time %>% group_by(k5) %>%
  summarise(n = n(), 
            obs_time = mean(obs_prop, na.rm = T),
            obs_time.se = sd(obs_prop, na.rm = T)/sqrt(n))

# significance from ANOVA
TukeyHSD(obs_time)
time_obs %<>% 
  mutate(sig = ifelse(k5 == "remote AV", "B", 0),
         sig = ifelse(k5 == "biologger", "B", sig),
         sig = ifelse(k5 == "remote spatial", "A", sig),
         sig = ifelse(k5 == "direct obs", "A", sig),
         sig = ifelse(k5 == "tracking", "A", sig))


### Fig 6 Temporal coverage ----

time_obs$k5 <- factor(time_obs$k5, levels = time_obs$k5[order(time_obs$obs_time)])

ggplot(time_obs, aes(x = k5, colour = k5, y = obs_time, ymin = obs_time - obs_time.se, ymax = obs_time + obs_time.se)) +
  geom_point(size = 3) +
  geom_errorbar(size = 0.8, width = 0.4) +
  geom_text(data = time_obs, aes(label = sig), 
            position = position_nudge(y = 0.1),
            size = 4, colour = "black") +
  ylab("Proportion of study duration observed") +
  xlab("") +
  scale_y_continuous(n.breaks = 6) +
  scale_colour_manual(values = colours) +
  theme_bw() +
  theme(legend.position = "none",
        panel.grid = element_blank())


## Stages of the foraging process by approach ----
stages <- data %>%
  group_by(k5) %>%
  summarise(n_method = n(), 
            feeding = sum(feeding), 
            searching = sum(searching), 
            habitat_sel = sum(hs))
stages$k5 <- factor(stages$k5, levels = stages$k5[order(stages$n_method, decreasing = TRUE)])
stages %<>%
  pivot_longer(3:5, names_to = "Stage", values_to = "n") %>%
  group_by(Stage) %>%
  mutate(prop_of_stage = n/sum(n)) %>%
  ungroup()

stages$Stage <- gsub("_sel", " selection", stages$Stage)

stages$Stage <-relevel(as.factor(stages$Stage), ref = "habitat selection")
stages$Stage <-relevel(as.factor(stages$Stage), ref = "searching")
stages$Stage <-relevel(as.factor(stages$Stage), ref = "feeding")

### total studies of each stage done by each approach
ggplot(stages, aes(x = Stage, y = n, fill = k5)) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  theme_bw() +
  xlab("") +
  ylab("Number of studies") +
  scale_fill_manual(values = colours) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank())

### number of studies within each approach at each stage
ggplot(stages, aes(x = k5, y = n, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("") +
  ylab("Number of studies") +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.85) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank())

### Fig 7: Proportion within approach of each stage ----
stages %<>% mutate(frac = n/n_method)
stages$k5 <- factor(stages$k5, levels = c("biologger", "direct obs", "remote AV", "remote spatial", "tracking"))

ggplot(stages, aes(x = k5, y = frac, fill = Stage)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_bw() +
  xlab("") +
  ylab("Proportion of studies") +
  scale_fill_viridis(discrete = TRUE, option = "viridis", begin = 0.3, end = 0.85) +
  theme(legend.title = element_blank(),
        panel.grid = element_blank())