##############################################
# Evaluation of White Religious Conservative #
# Support for Foreign Aid                    #
##############################################

# Author:  Miles Wiliams
# Updated: 1/20/2021


# attach libraries --------------------------------------------------------

library(tidyverse)
library(interplot)


# load data ---------------------------------------------------------------

ussamp <- 
  haven::read_dta(
    "01_data/U.S. representative sample data.dta"
  )

# make figures ------------------------------------------------------------

# Figure 1:
ussamp %>%
  mutate(
    relimp = ifelse(relimp>=3,"Religious","Unreligious"),
    race3 = as_factor(race3),
    con = ifelse(lib==0,"Conservative","Liberal")
  ) %>%
  group_by(
    race3, con, relimp
  ) %>%
  summarize(
    pr = mean(aid,na.rm=T),
    se = sd(aid,na.rm=T)/sqrt(n())
  ) %>%
  na.omit %>%
  ggplot() +
  aes(
    x = con,
    y = relimp,
    fill = pr,
    label = paste0(round(pr*100,2),"%")
  ) +
  geom_tile() +
  geom_text() +
  facet_wrap(~race3) +
  scale_fill_gradient(
    low = 'red', high = 'blue'
  ) +
  labs(
    x="",
    y="",
    title = "% Support for Foreign Aid"
  ) +
  ggridges::theme_ridges(
    font_size = 10
  ) +
  theme(
    legend.position = 'none',
    panel.grid = element_blank(),
    axis.ticks = element_blank()
  ) +
  ggsave(
    "03_figures/plot1.png",
    height = 3,
    width = 9
  )

# Figure 2:
ussamp %>%
  group_by(evan) %>%
  summarize(
    pr = mean(aid,na.rm=T),
    se = sd(aid,na.rm=T)/sqrt(n())
  ) %>%
  ggplot() +
  aes(
    x = evan,
    y = pr,
    ymin = pr - 1.96*se,
    ymax = pr + 1.96*se
  ) +
  geom_col(width = .5) +
  geom_errorbar(width = .15) +
  scale_x_continuous(
    breaks = c(0,1),
    labels = c("Everyone Else","White, Conservative,\n&Religious")
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    x = "",
    y = "Support for Foreign Aid",
    subtitle = "White, Conservative, & Religious Respondents\nAre Less Supportive of Foreign Aid",
    caption = "95% CIs Shown"
  ) +
  ggridges::theme_ridges(
    font_size = 10
  ) +
  ggsave(
    "03_figures/plot2.png",
    height = 3,
    width = 4
  )

# Crosstab of white-con-rel vs. everyone else:
ussamp %>%
  mutate(
    evan = ifelse(
      relimp>=3&race3==1&lib==0,
      1,0
    )
  ) -> ussamp
ussamp %>%
  group_by(evan,ties3) %>%
  count() %>% 
  na.omit %>%
  group_by(evan) %>%
  mutate(
    pct = n/sum(n)*100
  )

# Figure 3:
ussamp %>%
  mutate(
    evan = ifelse(
      race3==1 & relimp >= 3 & lib == 0,
      "White, Conservative,\n& Religious",
      "Everyone Else"
    )
  ) %>%
  na.omit %>%
  group_by(ties3,evan) %>%
  summarize(
    pr = mean(aid,na.rm=T),
    se = sd(aid,na.rm=T)/sqrt(n())
  ) %>%
  ggplot() +
  aes(
    x = ties3,
    y = pr,
    ymin = pr - 1.96*se,
    ymax = pr + 1.96*se
  ) +
  facet_wrap(~evan) +
  geom_col(width = .5) +
  geom_errorbar(width = .15) +
  scale_x_continuous(
    n.breaks = 3,
    labels = c("No Ties","Weak Ties","Strong Ties")
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    x = "",
    y = "Support for Foreign Aid",
    caption = "95% CIs Shown"
  ) +
  ggridges::theme_ridges(
    font_size = 10
  ) +
  theme(
    axis.text.x = element_text(
      angle = 45, hjust = 1, vjust = 1
    )
  ) +
  ggsave(
    "03_figures/plot3.png",
    height = 3, width = 4.5
  )

# Figure 4:
fit <- 
  lm(
    aid ~ ties3*evan + 
      gender + educ + loinc + age +
      fullemploy,
    data = ussamp
  )
interplot(
  fit,
  var1 = "evan",
  var2 = "ties3"
) +
  geom_hline(
    yintercept = 0, lty = 2
  ) +
  scale_x_continuous(
    n.breaks = 3,
    labels = c("No Ties","Weak Ties","Strong Ties")
  ) +
  scale_y_continuous(
    labels = scales::percent
  ) +
  labs(
    y = "Difference in Support",
    caption = "95% CIs reported",
    subtitle = "Religious White Conservatives Less Likely to Support Aid\n(and support worsens as ties abroad strengthen)"
  ) +
  ggridges::theme_ridges(
    font_size = 10
  ) +
  ggsave(
    "03_figures/plot4.png",
    height = 3, width = 4.5
  )