
# Libraries and Loading ---------------------------------------------------

library(tidyverse)
library(janitor)
library(lubridate)

sf_evictions <- read.csv("https://query.data.world/s/gzyncjukjb6kpzebkb6bidhqhflflu", header = TRUE, stringsAsFactors = FALSE)

# Theme -------------------------------------------------------------------
theme_jdv_dark <- function(base_size = 11, 
                           base_family = "Arial Narrow") {
  theme_minimal(base_size = base_size,
                base_family = base_family) %+replace%
    theme(axis.title = element_text(face = 'bold', 
                                    color = "#D3D3D3"),
          axis.title.x = element_blank(),
          axis.title.y = element_text(vjust = 4, 
                                      angle = 90, 
                                      color = "#D3D3D3"),
          axis.text = element_text(color = "#D3D3D3"),
          axis.text.y = element_text(size = 9, 
                                     hjust = 0, 
                                     vjust = 2, 
                                     color = "#D3D3D3"),
          legend.title = element_text(face = 'bold', 
                                      color = "#FFFFFF"),
          legend.text = element_text(size = 10,
                                     color = "#FFFFFF"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "grey20"), 
          plot.title = element_text(size = 19, 
                                    face = 'bold', 
                                    hjust = 0, 
                                    vjust = 3, 
                                    color = "#D3D3D3"), 
          plot.subtitle = element_text(size = 10, 
                                       hjust = 0, 
                                       vjust = 2, 
                                       color = "#C0C0C0"),
          plot.caption = element_text(size = 8, 
                                      color = "#D3D3D3", 
                                      hjust = 1),
          plot.margin = unit(c(.8, .5, .5, 1), "cm"),
          strip.text = element_text(size = 11,
                                    color = "#FFFFFF",
                                    face = "bold"),
          strip.background = element_rect(size = 8),
          complete = FALSE,
          validate = TRUE)
}


# Data for plot -----------------------------------------------------------

#SF Ellis Act evictions from 2010 to most recent
sf_evictions_plot <- sf_evictions %>%
  clean_names() %>%
  mutate(file_date = mdy(file_date),
         month_year = format(as.Date(file_date), "%Y-%m")) %>%
  filter(year(file_date) >= 2010,
         ellis_act_with_drawal == "true") %>%
  group_by(month_year) %>% #Get a monthly tally of evictions
  tally() %>%
  mutate(month_year = parse_date(month_year, "%Y-%m")) %>%
  complete(month_year = seq.Date(min(month_year), #Fill in any missing months - no Ellis evictions
                                 max(month_year), 
                                 by = "month")) %>%
  mutate(n = ifelse(is.na(n), 0, n)) #Fill new month evictions with 0's


# Plot --------------------------------------------------------------------

sf_evictions_plot %>%
  ggplot(aes(month_year, n)) +
  geom_line(stat = "identity", 
            size = 1, 
            color = "dark grey") +
  geom_area(stat = "identity", 
            fill = "dark grey", 
            alpha = .3) +
  scale_x_date(date_breaks = "1 year", 
               date_labels = "%Y") +
#Add basic labels
  labs(title = "Ellis Act Evictions in San Francisco (2010 - 2019)",
       subtitle = "From 2010-2019, the fourth most common reason for eviction notices in San Francisco was an 'Ellis Act Withdrawal'.
What does this mean? The Ellis Act is a state law which gives landlords the unconditional right to evict all of their tenants in order to 'go out of business'.
This approach is commonly used to convert rental buildings into luxury condos, or multiple units into a single-family mansion.\n\n",
       caption = "\nSources: SF Open Data | SF Business Times | SFTU
       Design: Jenna DeVries | Twitter: @jennaldevries
       Created for #MakeoverMonday") +
  ylab("Monthly Ellis Evictions") +
#Add theme + overlay
  theme_jdv_dark() +
  annotate("rect", 
           xmin = mdy("01-01-2010"), xmax = mdy("12-01-2013"), 
           ymin = 0, ymax = 48.3, 
           fill = "red", 
           alpha = .15) +
  annotate("rect", 
           xmin = mdy("12-01-2013"), xmax = mdy("10-01-2014"), 
           ymin = 0, ymax = 48.3, 
           fill = "turquoise", 
           alpha = .15) +
  annotate("rect", 
           xmin = mdy("12-05-2015"), xmax = mdy("05-01-2019"),
           ymin = 37.5, ymax = 46.5, 
           fill = "grey", 
           alpha = .05) +
#Add text annotations
  annotate("text", 
           x = mdy("02-01-2010"), 
           y = 35, 
           label = "From 2010 to 2014, Ellis Act evictions were steadily rising to a peak of 231 in 2013. 
As tenant activists protested and put pressure on elected officials, City Hall 
took steps to reform the act, and dramatically cut the 
number of evictions between 2014 and 2015.",
           color = "white", 
           alpha = .5, 
           size = 2.7, 
           hjust = .0) +
  annotate("text", 
           x = mdy("01-01-2016"), 
           y = 42, 
           label = "Unfortunately, attempts to limit the use of the Ellis Act have not lasted. 
Ordinances put in place were struck down, and Ellis evictions have persisted.",
           color = "white", 
           alpha = .5, 
           size = 2.7, 
           hjust = 0)


ggsave("Ellis Evictions.png", height = 4.4, width = 12.6, units = "in")
