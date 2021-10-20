library(tidyverse)
library(janitor)
library(skimr)

library(tidymodels)
library(gridExtra)
library(lubridate)
library(ggiraph)
library(heatmaply)
library(RColorBrewer)
library(scales)

# load household data created in 02_data_cleaning.R ----------------------------------------------------------------------

hhfile <- read_csv("data/hh_merged_allyears.csv")

# training set
training <- 
  hhfile %>% 
  filter(year == 2018)

# income -----------------------------------------------------------------------------------------------------
## histogram ---------------------------------------------------------------------------------

hist_ipcm <- 
  ggplot(training, aes(ipcm)) +   
  geom_histogram(binwidth = 500000, col = "#D53E4F", fill = "#D53E4F", alpha = .5) +  
  xlab("monthly income per capita") +
  ylab("Frequency") +
  ggtitle("(a)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )

## boxplot ---------------------------------------------------------------------------------------------------

box_ipcm <-
  ggplot(training, aes(x = "", y = ipcm)) +
  geom_boxplot(alpha = 0.2) +
  ylab("monthly income per capita") +
  ggtitle("(b)") +
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip()

## violin ------------------------------------------------------------------------

violin_ipcm <-
  ggplot(training, aes(x = "", y = ipcm)) +
  geom_violin(alpha = 0.2) +
  ylab("monthly income per capita") +
  ggtitle("(c)") +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  coord_flip() 

## plot together -------------------------------------------------------------------------

grid.arrange(hist_ipcm, box_ipcm, violin_ipcm, nrow = 3, ncol = 1, heights = c(2, 1, 1))


# predictors -------------------------------------------------------------------------
## dptorep ------------------------------------------------------------------------

ggplot(training, mapping = aes(x = reorder(as.factor(dptorep), ipcm, median), 
                               y = log(ipcm))) + 
  geom_boxplot() +
  coord_flip() +
  labs(y = "log(ipcm)", x = "dptorep")

##totpers -------------------------------------------------------------------------

ggplot(training, aes(x = hh_totpers, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")

ggplot(training, aes(x = vivi_dormitorios, y = log(ipcm))) +
  geom_point(alpha = 0.2) +
  geom_smooth(method="loess")