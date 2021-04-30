# recreating plots from chapter 4
# of Kieran Healy's "Data Visualization: A Practical Introduction"


# setup -------------------------------------------------------------------

library(here)
library(gapminder)
library(dplyr)
library(ggplot2)
library(socviz)


# plot GDP per capita over time -------------------------------------------

p_gdp_vs_year <- ggplot(data = gapminder,
       mapping = aes(x = year,
                     y = gdpPercap)) +
  geom_line(mapping = aes(group = country),
            colour = 'gray70') +
  geom_smooth(size = 1.1, 
              method = 'loess',
              se = FALSE,
              colour = 'violet') +
  scale_y_log10(labels = scales::dollar) +
  scale_x_continuous(breaks = seq(from = 1952, to = 2007, by = 20)) +
  facet_wrap(~ continent,
             nrow = 1, ncol = 5) +
  labs(x = 'Year', y = 'GDP per capita',
       title = 'GDP on Five Continents',
       caption = 'Source: Gapminder.') +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))
  
ggsave(filename = here('fig', 'fig_gdp_vs_year.png'),
       plot = p_gdp_vs_year,
       height = 5, width = 8, dpi = 400)


# plot county areas by state ----------------------------------------------

p_county_areas <- ggplot(data = midwest,
       mapping = aes(x = area, 
                     colour = state,
                     fill = state)) +
  geom_density(alpha = 0.3) +
  labs(x = 'Area', y = 'Density', 
       colour = 'State', fill = 'State',
       title = 'Distribution of County Areas in Midwest States',
       caption = 'Source: `midwest` from ggplot2 package.') +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'),
        axis.text.y = element_blank())

ggsave(filename = here('fig', 'fig_county_areas.png'),
       plot = p_county_areas,
       height = 5, width = 8, dpi = 400)


# plot US life expectancy gap ---------------------------------------------

p_US_le_gap <- ggplot(data = oecd_sum, 
       mapping = aes(x = year, y = diff, fill = hi_lo)) +
  geom_col() +
  guides(fill = FALSE) +
  scale_x_continuous(breaks = seq(from = 1960, to = 2016, by = 8)) +
  scale_fill_brewer(palette = 'Accent') +
  labs(x = '', y = 'Difference in Years', 
       title = 'The US Life Expectancy Gap',
       subtitle = 'Difference between US and OECD average life expectancies, 1960-2015',
       caption = 'Source: OECD. After a chart by Christopher Ingraham,
                  Washington Post, 2017-12-27') +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_US_le_gap.png'),
       plot = p_US_le_gap,
       height = 3, width = 8, dpi = 400)

