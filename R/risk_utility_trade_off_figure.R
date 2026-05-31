# Risk Utility map with
# mu_I for the risk assessment 
# a range of E(|Z| | rho) as loss information assessment 

library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(latex2exp)
library(patchwork)
library(knitr)

source("R/functions.R")
source("R/theme_ggplot.R")

# ---- Parameters -------------------------------------------------------------
beta_I <- 0.2
dominance_theshold <- 1 - beta_I
sg_nu_vals <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
n_vals <- c(3, 6, 9, 12)

# sigma_epsilon chosen at the  step 1 (see risk_figures.R).
# Modifier cette valeur si une autre a ete selectionnee.
sg_eps_fixed <- 0.022

thresholds  <- c(0.5, 0.8)

# ---- Range of the loss function (en %) ----------------------

loss_min_pct <- function(sg_eps) {
  # reached for rho -> 0.
  sqrt(2 * sg_eps^2 / pi) * 100
}

loss_dom_pct <- function(n, sg_nu, sg_eps) {
  # reached for rho = dominance_theshold
  sqrt(2 * (sg_nu^(2*n)*dominance_theshold + sg_eps^2) / pi) * 100
}

loss_max_pct <- function(sg_nu, sg_eps) {
  # reached for rho = 1
  sqrt(2 * (sg_nu^2 + sg_eps^2) / pi) * 100
}

# ---- Data  ---------------------------------------------
data_ru <- expand_grid(sg_nu = sg_nu_vals, n = n_vals) %>%
  mutate(
    risk    = map2_dbl(sg_nu, n, ~ worst_case_risk_I(
      n = .y, sg_nu = .x, sg_eps = sg_eps_fixed, beta = beta_I)),
    u_min   = loss_min_pct(sg_eps_fixed),   # meme valeur pour toutes les lignes
    u_dom   = loss_dom_pct(n, sg_nu, sg_eps_fixed),
    u_max   = loss_max_pct(sg_nu, sg_eps_fixed),
    n_label = factor(n, levels = n_vals,
                     labels = paste0("n = ", n_vals))
  )

# Verification : la plage d'utilite ne depend pas de n (propriete analytique).
# Les 4 lignes de meme sg_nu doivent avoir le meme u_max.
stopifnot(
  data_ru %>%
    group_by(sg_nu) %>%
    summarise(range_umax = diff(range(u_max))) %>%
    pull(range_umax) %>%
    max() < 1e-10
)


# ---- RU map  -----------------------------------------------------

pal_nu <- scale_color_viridis_d(
  name   = TeX("$\\sigma_\\nu$"),
  labels = as.character(sg_nu_vals),
  end    = 0.88
)

ru_map <- ggplot(data_ru,
                     aes(color = factor(sg_nu))) +
  # line of the lower bound
  geom_vline(xintercept = loss_min_pct(sg_eps_fixed),
             linetype = "dotted", color = "grey50", linewidth = 0.6) +
  # thresholds
  geom_hline(yintercept = thresholds,
             linetype = "dashed", color = "grey60", linewidth = 0.5) +
  # Range of Losses
  geom_segment(aes(x = u_min, xend = u_max,
                   y = risk, yend = risk),
               linewidth = 0.9) +
  # Lower bound of the loss
  geom_point(aes(x = u_min, y = risk),
             shape = 21, fill = "white", size = 2.2, stroke = 0.8) +
  # # Intermediate point : diamond (at the dominance threshold)
  # geom_point(aes(x = u_dom, y = risk),
  #            shape = 23, fill = "grey", size = 2.2, stroke = 0.8) +
  # Upper bound of the loss
  geom_point(aes(x = u_max, y = risk),
             shape = 19, size = 2.5) +
  facet_wrap(~ n_label, nrow = 1) +
  pal_nu +
  scale_x_continuous(limits = c(0, 44),
                     breaks = seq(0,40,10),
                     expand = c(0,0)
  ) +
  scale_y_continuous(limits = c(0, 1.02),
                     breaks = c(0,0.25,0.5,0.8,1),
                     expand = c(0,0)
                     ) +
  labs(
    x = TeX(paste0(
      "Range of the loss ",
      "$E(|Z| | P = \\rho)$ (%)"
    )),
    y = TeX("$\\mu_I^{0.2}$  (worst case in $\\rho$)")
  ) +
  theme(
    strip.text = element_text(face = "bold"),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 10.5),
  )

ggsave("figures/ru_map.png", ru_map, width = 11, height = 5.5, dpi = 300)

print(ru_map)

# ---- Data -------------
data_ru %>%
  mutate(
    across(c(u_min, u_max), ~ round(.x, 2)),
    risk = round(risk, 3)
  ) %>%
  select(n, sg_nu, risk, u_min, u_max) %>%
  arrange(n, sg_nu) %>%
  print(n = Inf) %>% 
  kable(format = "latex", digits=3)


