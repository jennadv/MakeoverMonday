#Add a callout under the first time debt overcame GDP - 
#e.g. hasn't happened since X, also what are the actual data points


library(tidyverse)
library(lubridate)
library(httr)
library(readxl)
GET("https://query.data.world/s/lagxqybl432j3msqh3rjgt33wkmlhv", write_disk(tf <- tempfile(fileext = ".xlsx")))
df <- read_excel(tf)

# Data for plot -----------------------------------------------------------

df <- df %>%
  rename("Q" = Quarter, 
         "GDP" = `Gross Domestic Product ($mil)`, 
         "Debt" = `Total Public Debt ($mil)`) %>%
  filter(!is.na(Debt)) %>% #Only grab those with both data points (debt and GDP)
  mutate(GDPTR = round(GDP / 1000, 1), #In Trillions
         DebtTR = round(Debt / 1000000, 1), 
         GDPLab = paste0("$", GDPTR, "T"), #Labels
         DebtLab = paste0("$", DebtTR, "T"),
         Q = as.Date(Q)) 

overall <- df %>%
  filter(year(Q) >= 2000)

# covid <- df %>%
#   filter(year(Q) >= 2019)

# Basic Plot --------------------------------------------------------------------

overall %>%
  ggplot() +
  geom_area(aes(Q, DebtTR),
            fill = "dark red", 
            alpha = .8) +
  geom_area(aes(Q, GDPTR),
            fill = "dark grey",
            alpha = .6) +
  geom_line(aes(Q, DebtTR),
            color = "dark red", 
            alpha = .9) +
  geom_line(aes(Q, GDPTR),
            color = "dark grey",
            alpha = .9) +
#Scales  
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") +
  scale_y_continuous(name = "$ in Trillions",
                     breaks = c(5, 10, 15, 20, 25)) +
  theme_jdv() +

# Annotations -------------------------------------------------------------
#COVID -- PROJECTION
  geom_vline(xintercept = mdy("01-01-20"), 
             linetype = "dotted",
             size = .8,
             alpha = .7) + 
  annotate("text", 
           x = mdy("01-01-20"),
           y = 27,
           label = "COVID-19  
(projection)  ",
           size = 3,
           hjust = 1) + 
  
#RECESSION
  geom_vline(xintercept = mdy("01-01-08"),
             linetype = "dotted",
             size = .8,
             alpha = .7) + #Recession
  annotate("text", 
           x = mdy("01-01-08"),
           y = 21,
           label = "  Great Recession",
           size = 3,
           hjust = 0) +
  
#When Debt Overtook GDP
  geom_vline(xintercept = mdy("10-01-2012"),
             linetype = "dotted",
             size = .8,
             alpha = .7) +

  #Y Axis Label
  xlab("Year") +
  
#Caption / Sources
  labs(caption = "\nSource: Federal Reserve Bank of St. Louis
       Design: Jenna DeVries | Twitter: @jennaldevries
       Created for #MakeoverMonday") +
  
#Title and Summary of Plot
  annotate("rect", 
           xmin = mdy("02-01-2000"), xmax = mdy("11-01-2006"), 
           ymin = 18, ymax = 26.5, 
           fill = "Grey", 
           #color = "Dark Grey",
           alpha = .3) +
  annotate("text", 
           x = mdy("05-01-2000"), 
           y = 25, 
           label = "America's Rising Debt",
           color = "Black", 
           size = 5, 
           hjust = .0,
           family = "Helvetica Bold") +
  annotate("text", 
           x = mdy("05-01-2000"), 
           y = 21.3, 
           label = "U.S. debt has slowly overcome GDP 
since the financial crisis of 2008 - and 
2020 projections reveal a sharp increase
following COVID-19 relief efforts.",
           color = "Black", 
           size = 3, 
           hjust = .0,
           family = "Helvetica") +
  
#Subtitle / When debt overtook GDP
  annotate("rect", 
           xmin = mdy("02-01-2013"), xmax = mdy("10-01-2016"), 
           ymin = 5.5, ymax = 12, 
           fill = "Dark Red",
           #color = "Dark Grey",
           alpha = .15) +
  annotate("text", 
           x = mdy("05-01-2013"), 
           y = 8.8, 
           label = "In October of 2012, debt 
overtook GDP - an event 
which has not occurred 
in the U.S. since the end 
of World War II.",
           color = "Black", 
           size = 2.5, 
           hjust = .0,
           family = "Helvetica") +
  
#Legend Labels
  annotate("text", 
           x = mdy("05-01-2000"), 
           y = 8.1, 
           label = "GDP",
           color = "Dark Grey", 
           size = 10, 
           hjust = .0,
           family = "Helvetica Bold") +
  annotate("text", 
           x = mdy("05-01-2000"), 
           y = 2.8, 
           label = "Debt",
           color = "Dark Red", 
           size = 10, 
           hjust = .0,
           family = "Helvetica Bold")
  

#ggsave("U.S. Debt.png", height = 4.4, width = 12.6, units = "in")
