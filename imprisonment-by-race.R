library(ojo)

sr_disparity <- state_pop %>%
  select(-perc) %>%
  left_join(doc_race %>%
              select(-perc)) %>%
  mutate(rate = round(n/total*100000, 0))

r_disp <- sr_disparity %>%
  group_by(race) %>%
  summarize(across(c(n, total), sum)) %>%
  mutate(rate = round(n/total*100000, 0))

r_disp %>%
  mutate(race = if_else(race == "NATIVE\nAMERICAN",
                        "AMERICAN INDIAN/\nALASKA NATIVE",
                        race)) %>%
  mutate(race = fct_relevel(race,
                        c("WHITE",
                          "HISPANIC",
                          "BLACK",
                          "AMERICAN INDIAN/\nALASKA NATIVE"))) %>%
  ggplot(aes(race, rate)) +
  geom_col(fill = "black") +
  geom_text(aes(label = scales::comma(rate)),
            vjust = 0,
            nudge_y = 30,
            family = "Menlo",
            fontface = "bold",
            size = 6) +
  scale_y_continuous(limits = c(0, 3500),
                     labels = scales::comma) +
  theme_ojo() +
  theme(axis.title = element_blank(),
        title = element_text(face = "bold",
                             size = 14),
        axis.text.x = element_text(face = "bold",
                                   size = 14)) +
  labs(title = "OKLAHOMA INCARCERATION RATES BY RACE/ENTHNICITY",
       subtitle = "As of June 30, 2020")

