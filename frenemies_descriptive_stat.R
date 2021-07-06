## Install packages if haven't
#install.packages(c("tidyverse", "lubridate", "cowplot", "RColorBrewer"))

## Load packages
library(tidyverse)
library(lubridate)
library(cowplot)

## Get nice colors 
library(RColorBrewer)
colors <- brewer.pal(5, "Set1")
blue <- colors[2]
green <- colors[3]
orange <- colors[5]
yellow <- colors[6]
pink <- colors[8]
purple <- colors[4]
brown <- colors[7]

## Get personal theme
source("http://staff.washington.edu/kpleung/vis/theme/theme_cavis.R")

## Load data
Frenemies <- read_csv("C:\\Users\\owner\\iCloudDrive\\Documents\\Emily_Projects\\Frenemies\\FrenemiesCoded4.csv")


# separate OSE letters from number code
Frenemies <- separate(Frenemies, 
                      ose_claim_id, 
                      into = c("ose_letters", "ose_date"), 
                      sep = 3,
                      remove = FALSE)
# create OSE year
Frenemies <- separate(Frenemies, 
                      ose_date, 
                      into = c("ose_year", "ose_month_day"), 
                      sep = 4, 
                      remove = FALSE)
# create OSE month
Frenemies <- separate(Frenemies, 
                      ose_month_day, 
                      into = c("ose_month", "ose_day"), 
                      sep = 2, 
                      remove = FALSE)

## Create a new column of `date` class
Frenemies <- 
  Frenemies %>%
  mutate(date = make_date(year = year, month = ose_month, day = 1))

## Remove Skipped
Frenemies$Skip <- ifelse(Frenemies$Skip  == 'y', 2, 1)

## Skip the skips
FrenemiesFiltered <- filter(Frenemies, Skip == 1)
## Rename to data
data <- FrenemiesFiltered


## Visualize count over time
count_byDate <- 
  data %>%
  group_by(date) %>%
  count()

allClaims_vis <- 
  ggplot(count_byDate, aes(x = date, y = n)) +
  geom_line() +
  theme_cavis_hgrid +
  scale_y_continuous(expand = c(0, 0),
                     limits = c(0, 50)
  ) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "'%y",
               limits = c(make_date(2012, 1, 1), 
                          make_date(2019, 1, 1))
  ) +
  labs(y = "Count", x = "Year", title = "Total Frenemies Claims")



## Create Binary Variables for Frenemie Type
data <- mutate(data, 
               Merger = ifelse(type1 == "Merger" , TRUE, FALSE ), 
               Breakup = ifelse(type1 == "Breakup", TRUE, FALSE), 
               Ceasefire = ifelse(type1 == "Ceasefire", TRUE, FALSE ), 
               Collaboration = ifelse(type1 == "Collaboration", TRUE, FALSE ),
               Congratulations = ifelse(type1 == "Congratulations", TRUE, FALSE ),
               Critique = ifelse(type1 == "Critique", TRUE, FALSE ),
               Defect_Neg = ifelse(type1 == "Defect_Neg", TRUE, FALSE ),
               Defect_Pos = ifelse(type1 == "Defect_Pos", TRUE, FALSE ),
               Denial = ifelse(type1 == "Denial", TRUE, FALSE ),
               Formal_Alliance = ifelse(type1 == "Formal_Alliance", TRUE, FALSE ),
               Gen_Pos = ifelse(type1 == "Gen_Pos", TRUE, FALSE ),
               JOR = ifelse(type1 == "JOR", TRUE, FALSE ),
               Negotiations = ifelse(type1 == "Negotiations", TRUE, FALSE ), 
               Prisoner_Exchange = ifelse(type1 == "Prisoner_Exchange", TRUE, FALSE ), 
               Release = ifelse(type1 == "Release", TRUE, FALSE ), 
               Weapon_Theft = ifelse(type1 == "Weapon_Theft", TRUE, FALSE ))


## Mergers
count_Merger <- 
  data %>%
  filter(Merger == TRUE) %>%
  group_by(date) %>%
  count() 


count_Mergers_vis <- 
  ggplot(count_Merger, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Mergers") 


## Breakup
count_Breakup <- 
  data %>%
  filter(Breakup == TRUE) %>%
  group_by(date) %>%
  count() 


count_Breakup_vis <- 
  ggplot(count_Breakup, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Breakups") 


## Ceasefire
count_Ceasefire <- 
  data %>%
  filter(Ceasefire == TRUE) %>%
  group_by(date) %>%
  count() 


count_Ceasefire_vis <- 
  ggplot(count_Ceasefire, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Ceasefires") 

## Collaboration
count_Collaboration <- 
  data %>%
  filter(Collaboration == TRUE) %>%
  group_by(date) %>%
  count() 


count_Collaboration_vis <- 
  ggplot(count_Collaboration, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Collaboration") 


## Congratulations

count_Congratulations <- 
  data %>%
  filter(Congratulations == TRUE) %>%
  group_by(date) %>%
  count() 


count_Congratulations_vis <- 
  ggplot(count_Congratulations, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Congratulations") 

## General Positive
count_Gen_Pos <- 
  data %>%
  filter(Gen_Pos == TRUE) %>%
  group_by(date) %>%
  count() 


count_Gen_Pos_vis <- 
  ggplot(count_Gen_Pos , aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Positive Statements") 


# Critique
count_Critique <- 
  data %>%
  filter(Critique == TRUE) %>%
  group_by(date) %>%
  count() 


count_Critique_vis <- 
  ggplot(count_Critique, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Critiques") 

## Defect Negative
count_Defect_Neg <- 
  data %>%
  filter(Defect_Neg == TRUE) %>%
  group_by(date) %>%
  count() 


count_Defect_Neg_vis <- 
  ggplot(count_Defect_Neg, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Defections from a Group") 

## Defect Positive
count_Defect_Pos <- 
  data %>%
  filter(Defect_Pos == TRUE) %>%
  group_by(date) %>%
  count() 


count_Defect_Pos_vis <- 
  ggplot(count_Defect_Pos, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Defections to a Group") 


## Denial 

count_Denial <- 
  data %>%
  filter(Denial == TRUE) %>%
  group_by(date) %>%
  count() 


count_Denial_vis <- 
  ggplot(count_Denial, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Denials") 


## Formal Alliance
count_Formal_Alliance <- 
  data %>%
  filter(Formal_Alliance == TRUE) %>%
  group_by(date) %>%
  count() 


count_Formal_Alliance_vis <- 
  ggplot(count_Formal_Alliance, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Formal Alliances") 

## JOR
count_JOR <- 
  data %>%
  filter(JOR == TRUE) %>%
  group_by(date) %>%
  count() 


count_JOR_vis <- 
  ggplot(count_JOR, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Joint Operation Room Forming/Joining") 


## Prisoner Exchange
count_Prisoner_Exchange <- 
  data %>%
  filter(Prisoner_Exchange == TRUE) %>%
  group_by(date) %>%
  count() 


count_Prisoner_Exchange_vis <- 
  ggplot(count_Prisoner_Exchange, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Prisoner Exchanges") 


## Negotiations
count_Negotiations <- 
  data %>%
  filter(Negotiations == TRUE) %>%
  group_by(date) %>%
  count() 


count_Negotiations_vis <- 
  ggplot(count_Negotiations, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Negotiations") 



## Release
count_Release <- 
  data %>%
  filter(Release  == TRUE) %>%
  group_by(date) %>%
  count() 


count_Release_vis <- 
  ggplot(count_Release , aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Prisoner Releases") 


## Weapons Theft
count_Weapon_Theft <- 
  data %>%
  filter(Weapon_Theft  == TRUE) %>%
  group_by(date) %>%
  count() 


count_Weapon_Theft_vis <- 
  ggplot(count_Weapon_Theft, aes(x = date, y = n)) +
  geom_line() +
  scale_y_continuous(expand = c(0, 0)) +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1)
               )
  ) +
  theme_cavis_hgrid +
  labs(y = "Count", x = "Year", title = "Weapon Theft") 



#############################################################
# Corrections and edits for the codes above:
# Some rows are removed and some are coded incorrectly ie. "coll" for collaboration
# in places where myself or Willa thought we had chosen one from the drop down
# This is only for type1. Add type2, type3
# not yet coded by group
# some codes primarily defect_neg and breakup are clearly coded differently by each of us
# Willa erred with breakup and I chose defect_neg. Should review to resolve coding issues. 
# prisoner release and weapons theft only have 1 or 2 observations. 
# should graft the negative valence and positive valence ones together. 
# for example, "congratulations" + " gen positive" and maybe + "negotiations"
##########################################################


## Cowplot them
claims_vis <- plot_grid(allClaims_vis,count_Mergers_vis, count_Breakup_vis, 
                        count_Ceasefire_vis, count_Collaboration_vis, count_Gen_Pos_vis, 
                        count_Critique_vis, count_Defect_Neg_vis, count_Release_vis, 
                        count_Weapon_Theft_vis, count_Negotiations_vis, 
                        count_Formal_Alliance_vis, count_JOR_vis, count_Denial_vis,                         
                        nrow = 4)


## Plot relative proportions
prop_tbl <- 
  data %>%
  group_by(date) %>%
  summarize(meanMerg = mean(Merger),
            meanCollab = mean(Collaboration),
            meanCeasefire = mean(Ceasefire), 
            meanBreakup = mean(Breakup),
            meanGenPos = mean(Gen_Pos), 
            meanCong = mean(Congratulations), 
            meanCrit = mean(Critique), 
            meanDefNeg = mean(Defect_Neg)) %>%
  pivot_longer(c(meanMerg, meanCollab, meanCeasefire, meanBreakup, meanGenPos,
                 meanCong, meanCrit, meanDefNeg), # This is important
               names_to = "type", values_to = "prop") %>%
  mutate(type = case_when(type == "meanMerg" ~ "Mergers",
                          type == "meanCollab" ~ "Collaboration",
                          type == "meanCeasefire" ~ "Ceasefire", 
                          type == "meanBreakup" ~ "Breakup",
                          type == "meangGenPos" ~ "Positive Statements", 
                          type == "meanCrit" ~ "Critiques", 
                          type == "meanDefNeg" ~ "Defect Negative"
                          ), #case_when an ifelse for multiple. Check
  type = factor(type, levels = c("Mergers", 
                                 "Collaborations", 
                                 "Ceasefire", 
                                 "Breakups", 
                                 "Positive Statements", 
                                 "Critiques", 
                                 "Defect Negative" )))

prop_vis <-          
  prop_tbl %>%
  ggplot(aes(x = date, y = prop, color = type, fill = type)) +
  geom_smooth(alpha = 0.15, size = 0.5) +
  scale_color_manual(values = c(blue, orange, green, brown, purple, yellow, pink)) +
  scale_fill_manual(values = c(blue, orange, green, brown, purple, yellow, pink)) +
  theme_cavis_hgrid +
  scale_x_date(date_breaks = "1 year", date_labels = "'%y",
               limits = c(make_date(2012, 1, 1),
                          make_date(2019, 1, 1))) +
  theme(legend.position = c(0.9, 1)) +
  labs(y = "Proportion", x = "Year", title = "Relative proportions of frenemies types")



## cowplot again
all_vis <- plot_grid(claims_vis, prop_vis, 
                     nrow = 2, 
                     rel_heights = c(1, 2))


## save the final output
save_plot(all_vis, 
          filename = "visClaims4.pdf", 
          base_height = 9, 
          base_asp = 1.618)













