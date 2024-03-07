library(imager)    # image loading and processing
library(dplyr)     # data manipulation
library(ggplot2)   # data visualization
library(tidyr)     # data wrangling
remotes::install_github("garretrc/ggvoronoi", dependencies = TRUE, build_opts = c("--no-resave-data"))
library(ggvoronoi) # visualization
library(kableExtra)

img <- load.image(file = "img/Uses1.png")

# Print the image object out
plot(img)

img_df <- as.data.frame(img)

img_df %>% 
  arrange(x, y, cc) %>% # sort by columns for viewing
  filter(row_number() < 10) %>% # Select top 10 columns
  kable("html") %>%  # Display table in R Markdown
  kable_styling(full_width = F) 

img_df <- img_df %>% 
  mutate(channel = case_when(
    cc == 1 ~ "Red",
    cc == 2 ~ "Green", 
    cc == 3 ~ "Blue"
  ))

img_wide <- img_df %>%
  select(x, y, channel, value) %>%
  spread(key = channel, value = value) %>%
  mutate(
    color = rgb(Red, Green, Blue)
  )

sample_size <- 10000
img_sample <- img_wide[sample(nrow(img_wide), sample_size), ]

img_sample$size <- runif(min = 0, max = 1, sample_size)

aRt_map <- ggplot(img_sample) +
  geom_point(mapping = aes(x = x, y = y, color = color, size = size),
             alpha = 0.05, shape = 16) +
  guides(size = FALSE) + # don't show the legend
  scale_color_identity() + # use the actual value in the `color` column
  scale_y_reverse() + # Orient the image properly (it's upside down!)
  theme_void() # Remove axes, background

aRt_map

ggsave("img/aRt_map.png")
