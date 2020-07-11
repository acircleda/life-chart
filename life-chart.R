#life chart designed by Isabella Benabye
# https://isabella-b.com/blog/my-life-in-months/

library(tidyverse)
library(lubridate)
library(waffle)
library(hrbrthemes)
library(extrafont)
loadfonts(device = "win", quiet = TRUE)

# Create the data-----
life_data <-
  tibble(months = factor(rep(month.abb[1:12], 101), levels=month.abb[1:12])) %>%   ## make months
  tibble(
    age = rep(0:100, each = 12) ## age range: 1-70
  ) %>%
  slice(6:(n()-7)) %>%
  rowid_to_column("row_name") %>%
  mutate(year = age + 1983)## add column for row number

## add the "eras" to be colored in the waffle chart
life_data_events <- life_data %>%
  mutate(era = fct_inorder(case_when(
  row_name == 1 ~ "Born - NY",
  row_name > 1 & row_name < 61 ~ "Growing Up",
  row_name == 61 ~ "Moved from NY to FL",
  row_name > 61 & row_name < 217 ~ "Growing Up 2",
  row_name == 217 ~ "Graduated High School",
  row_name > 217 & row_name < 275 ~ "College",
   row_name == 275 ~ "Graduated with a BA",
  row_name > 275 & row_name < 291 ~ 'College 2',
   row_name == 291 ~ "Moved to Korea",
  row_name > 291 & row_name < 323 ~ "Life in Korea 1",
  row_name == 323 ~ "Molly",
  row_name > 323 & row_name < 350 ~ "Life in Korea 2",
  row_name == 350 ~ "Lilly",
  row_name > 350 & row_name < 375 ~ "Life in Korea 3",
  row_name == 375 ~ "Moved back to US",
  row_name > 375 & row_name < 416 ~ "Life in the US",
  row_name == 416 ~ "Began PhD",
  row_name > 416 & row_name < 435 ~ "Life in the US 2",
  row_name == 435 ~ "Changed Careers",
   row_name > 435 & row_name < ((year(Sys.Date()) - 1983)*12) + (month(Sys.Date()) - 3) ~ "Life in the US 3", ## months into "US" based on current month
  TRUE ~ "Time left")))

annotations <- tribble(
  ~x, ~y, ~label, ~hjust,
  # major events
  6, -5, "Born: June, 1983", .5,
  18, 6, "Moved from NY to FL", 0,
  18, 16, "2001: Graduated HS", 0,
  18, 20, "2006: Graduated University", 0,
  18, 24, "2007: Moved to Korea", 0,
  18, 28, "Children Born", 0,
  18, 35, "Moved back to US", 0,
  18, 40, "Began PhD", 0,
  18, 48, "Changed Careers", 0,
  # eras
  -5, 11, "Growing Up", 1,
  -5, 23.5, "College Years", 1,
  -5, 28, "Life in Korea", 1,
  -5, 35, "Life in the US", 1,
  -5, 45, "Time left", 1
)

segments <- tribble(
  ~x, ~y, ~xend, ~yend, ~curve,
  -2, -3, 0, 1, -.4
)

# Waffle chart-----
life_data_events %>%
  count(era) %>% ## the count of each era is the number of months in that era
  ggplot() +
  geom_waffle(aes(fill = era, values = n), color = "white", n_rows = 12, size = .5, flip = TRUE) + ## make each row a year/12 months
  scale_fill_manual(name = "", values = c("black", "#EF476F","black","#EF476F","black", "#FCA311","black", "#FFD166","black", "#FFD166","black","#0EAD69", "black","#0EAD69","black","#4ECDC4","black", "#4ECDC4", "black", "#4ECDC4", "#118AB2")) +  ## assign colors to the eras
  coord_equal() +
  #age labels
  geom_text(data=data.frame(y=c(1,10,20,30,40,50,60,70,80,90,100),
                            label=c(0,10,20,30,40,50,60,70,80,90,100)),
            aes(x=-2, y=y, label=label, family="Ink Free"), size=2, nudge_y = .5)+
  geom_text(data=annotations,
            aes(x=x, y=y, label=label, family="Ink Free", size=.5), hjust=annotations$hjust)+
  geom_curve(data=segments,
               aes(x=x, y=y, xend=xend, yend=yend),
               curvature=segments$curve, arrow=arrow(length = unit(.15, "cm"), type="closed"))+
  theme(plot.background = element_rect(fill="white"),
        panel.background = element_rect(fill="white"),
        legend.position = "none",
        axis.ticks = element_blank()) +
 theme_enhance_waffle()+
  expand_limits(x=c(-20,45))

#export and touch up in Illustrator

