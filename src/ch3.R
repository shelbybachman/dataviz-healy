# recreating plots from chapter 3
# of Kieran Healy's "Data Visualization: A Practical Introduction"


# setup -------------------------------------------------------------------

library(here)
library(gapminder)
library(ggplot2)


# plot life expectancy vs. GDP --------------------------------------------

p_le_vs_gdp <- ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp)) +
  geom_point(alpha = 0.3) +
  geom_smooth(colour = 'gold',
              se = FALSE,
              size = 2,
              method = 'lm') +
  scale_x_log10(labels = scales::dollar) +
  labs(x = 'GDP Per Capita',
       y = 'Life Expectancy in Years',
       title = 'Economic Growth and Life Expectancy',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.') +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_le_vs_gdp.png'),
       plot = p_le_vs_gdp,
       height = 5, width = 8, dpi = 400)


# plot life expectancy vs. GDP, by continent ------------------------------

p_le_vs_gdp_cont <- ggplot(data = gapminder,
       mapping = aes(x = gdpPercap,
                     y = lifeExp,
                     colour = continent,
                     fill = continent)) +
  geom_point(alpha = 0.4) +
  geom_smooth(se = TRUE,
              size = 1,
              method = 'loess') +
  scale_x_log10(labels = scales::dollar) +
  labs(x = 'GDP Per Capita',
       y = 'Life Expectancy in Years',
       title = 'Economic Growth and Life Expectancy by Continent',
       subtitle = 'Data points are country-years',
       caption = 'Source: Gapminder.') +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_le_vs_gdp_cont.png'),
       plot = p_le_vs_gdp_cont,
       height = 5, width = 8, dpi = 400)

