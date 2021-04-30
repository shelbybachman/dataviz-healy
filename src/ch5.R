# recreating plots from chapter 5
# of Kieran Healy's "Data Visualization: A Practical Introduction"


# setup -------------------------------------------------------------------

library(here)
library(gapminder)
library(dplyr)
library(ggplot2)
library(socviz)
library(ggrepel)


# plot religious preference by region -------------------------------------

rel_by_region <- gss_sm %>%
  filter(!is.na(religion)) %>%
  group_by(bigregion, religion) %>%
  summarize(N = n()) %>%
  mutate(freq = N / sum(N),
         pct = round(freq*100))

p_rel_by_region <- ggplot(data = rel_by_region,
                          mapping = aes(x = religion,
                                        y = pct,
                                        fill = religion)) +
  geom_col(position = 'dodge2') +
  scale_fill_brewer(type = 'qual', palette = 'Set2') +
  labs(x = '',
       y = 'Percent',
       title = 'Religious Preference by Census Region',
       caption = 'Source: 2016 General Social Survey.') +
  guides(fill = FALSE) +
  coord_flip() +
  facet_grid(~ bigregion) +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_rel_by_region.png'),
       plot = p_rel_by_region,
       height = 4, width = 8, dpi = 400)


# plot organ donations by country -----------------------------------------

organdata_by_country <- organdata %>%
  group_by(consent_law, country) %>%
  summarize_if(is.numeric, list(mean = mean, sd = sd), na.rm = TRUE) %>%
  ungroup()

p_donations_by_country <- ggplot(data = organdata_by_country,
       mapping = aes(x = reorder(country, donors_mean), 
                     y = donors_mean)) +
  geom_pointrange(mapping = aes(ymin = donors_mean - donors_sd,
                                ymax = donors_mean + donors_sd)) +
  labs(x = 'Donor Procurement Rate', y = '',
       colour = 'Consent Law',
       title = 'Organ Donor Procurement Rates',
       subtitle = 'For countries with informed & presumed consent laws',
       caption = 'Source: OECD & Kieran Hiely.') +
  coord_flip() +
  facet_wrap(~ consent_law, scales = 'free_y', ncol = 2) +
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_donations_by_country.png'),
       plot = p_donations_by_country,
       height = 4, width = 8, dpi = 400)


# plot popular vs. electoral vote margins ---------------------------------

p_pop_vs_electoral <- ggplot(data = elections_historic,
       mapping = aes(x = popular_pct, y = ec_pct,
                     label = winner_label)) +
  geom_hline(yintercept = 0.5, size = 1.4, colour = 'gray80') +
  geom_vline(xintercept = 0.5, size = 1.4, colour = 'gray80') +
  geom_point() +
  geom_text_repel(family = 'Fira Sans', size = 2.5) +
  scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent) +
  labs(x = "Winner's Share of Popular Vote",
       y = "Winner's Share of Electoral College Votes",
       title = 'Presidential Elections: Popular & Electoral College Margins',
       subtitle = '1824-2016',
       caption = 'Data for 2016 are provisional. Source: `socviz` package.') + 
  theme_minimal() +
  theme(text = element_text(family = 'Fira Sans'))

ggsave(filename = here('fig', 'fig_pop_vs_electoral.png'),
       plot = p_pop_vs_electoral,
       height = 6, width = 8, dpi = 400)


