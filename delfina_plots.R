library(tidyverse)

setwd("C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one")

#--------------------------------PLOT SETUP--------------------------------------------------------#

# define color palette
woodland_palette_dark <- c(
  "#231f20",
           "#1c3c4a",
           "#344a2f",
           "#886ea9",
           "#e02c2d",
           "#ffb400"
)


theme_woodland <- function() {
  theme_classic() %+replace% # replace elements we want to change
    
    theme(
      
      # grid elements
      axis.line = element_line(linewidth = 1, lineend = "round"),
      axis.ticks = element_blank(), # strip axis ticks
      
      # since theme_minimal() already strips axis lines,
      # we don't need to do that again
      
      # text elements
      plot.title = element_text( # title
        size = 20, # set font size
        hjust = 0.5, # left align
        vjust = 1
      ), # raise slightly
      
      plot.subtitle = element_text( # subtitle
        size = 14, # font size
        vjust = -0.5
      ), # lower slightly
      
      plot.caption = element_text( # caption
        size = 11, # font size
        hjust = 1
      ), # right align
      
      axis.title = element_text( # axis titles
        size = 12
      ), # font size
      
      axis.text = element_text( # axis text
        size = 12
      ), # font size
      
      axis.text.x = element_text(
        hjust = 0.5, # center text
        angle = 45
      ), # margin for axis text
      
      axis.text.y = element_text(
        hjust = -0.5
      ),
      aspect.ratio = 1 # make plot square
    )
}

#--------------------------------HOUSEHOLD BY GENDER--------------------------------------------------------#

hhs_by_gender <- read.csv("./single_HHers_gender.csv")

tidy_hhs_by_gender <- hhs_by_gender %>%
  pivot_longer(c("Males", "Females"), names_to = "gender", values_to = "count_or_pct") |>
  rename(hh_type = X) |>
  filter(!grepl("Woodland", hh_type)) |>
  mutate(
    pct = str_remove_all(count_or_pct, "%"),
    pct = as.numeric(pct),
    raw_count = as.integer(case_when(
      gender == "Males" ~ pct * 822 / 100,
      TRUE ~ pct * 1527 / 100
    ))
  ) |>
  select(-count_or_pct)


# generate plot
hhs_plot <- ggplot(tidy_hhs_by_gender, aes(x = hh_type, y = raw_count, fill = gender)) +
  geom_col(position = "dodge") +
  labs(
    title = "Single Parent Households by Type",
    fill = "Gender of Household Head"
  ) +
  scale_fill_manual(values = c(
    "#344a2f",
             "#ffb400"
  )) +
  theme_woodland() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    aspect.ratio = 1
  )


#--------------------------------LANGUAGE--------------------------------------------------------#

other_langs <- read.csv("./other_lang_2011-2021.csv")

# make tidy version of other_langs df
tidy_langs <- other_langs |>
  pivot_longer(c("X2011", "X2021"), names_to = "year", values_to = "count_or_pct") |>
  rename(lang = X) |>
  filter(!grepl("5", lang)) |>
  mutate(
    year = str_remove_all(year, "X"),
    year = as.numeric(year),
    pct = str_remove_all(count_or_pct, "%"),
    pct = as.numeric(pct),
    raw_count = as.integer(case_when(
      year == 2011 ~ pct * 1803 / 100,
      TRUE ~ pct * 1488 / 100
    ))
  ) |>
  select(-count_or_pct)



# generate plot
langs_plot <- ggplot(tidy_langs, aes(x = year, y = raw_count, color = lang)) +
  geom_line(linewidth = 2) +
  labs(
    title = "Non-English Languages Spoken at Home",
    subtitle = "Total Individuals 5 and Over",
    color = "Language"
  ) +
  scale_color_manual(values = c(
    "#886ea9",
             "#231f20",
             "#e02c2d",
             "#344a2f",
             "#ffb400"
  )) +
  scale_x_continuous(breaks = c(2011, 2021)) +
  theme_woodland() +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    aspect.ratio = 1.5
  )


#------------Export Data-----------------#

# this chunk creates a variable for today's date and formats it properly for the file name
today <- lubridate::ymd(Sys.Date()) |>
  str_replace_all("-", "_")

# vectorize plots, concatenate names, loop through ggsave
plots <- list(hhs_plot, langs_plot)

plot_names <- paste0(today, c("_hhs_plot.pdf", "_langs_plot.pdf"))

# this line saves the plot as a pdf with the proper formatting based on today's date
purrr::map2(plots, plot_names, ~ ggsave(.y, .x, path = "C:/Users/Nissim/Desktop/Spring 2023/Studio/studio_one", device = "pdf", bg = "transparent"))