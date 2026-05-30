library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(latex2exp)
library(patchwork)

source("R/functions.R")
source("R/theme_ggplot.R")

# ---- Parametres -------------------------------------------------------------
beta_inf  <- 0.2     # scenarios I et II (regle de dominance / p% a 80%)
beta_diff <- 0.05     # scenario DIFF
n_values  <- c(3, 6, 9, 12)

tau_mid   <- 0.5     # seuils de risque de reference
tau_high  <- 0.8
tau_diff  <- 0.9
thresholds <- c(tau_mid, tau_high)

# grilles continues (axes des abscisses)
sg_nu_grid  <- seq(0, 0.5, by = 0.005)
sg_eps_grid <- seq(0, 0.1, by = 0.001)

# valeurs caracteristiques
sg_nu_marks  <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
sg_eps_marks <- c(0.01, 0.025, 0.05, 0.1)

# ---- Etape 1 : scenario DIFF et choix de sigma_epsilon ----------------------
data_diff <- tibble(sg_eps = sg_eps_grid) %>%
  mutate(risk = assess_risk_DIFF(sg_eps = sg_eps, beta = beta_diff))

marks_diff <- tibble(sg_eps = sg_eps_marks) %>%
  mutate(risk = assess_risk_DIFF(sg_eps = sg_eps, beta = beta_diff))

# sigma_epsilon retenu : plus petite valeur de la grille telle que
# mu_DIFF <= tau_high. C'est le resultat de l'etape 1 de la calibration.
sg_eps_fixed <- data_diff %>%
  filter(risk <= tau_diff) %>%
  slice_min(sg_eps, n = 1) %>%
  pull(sg_eps)

# ---- Etapes 2 et 3 : scenarios I et II --------------------------------------
# Table (sigma_nu x n) -> risque (pire cas en rho), a sigma_epsilon fixe.
build_curve_data <- function(worst_fun, sg_nu_values, n_values, sg_eps, beta) {
  expand_grid(sg_nu = sg_nu_values, n = n_values) %>%
    mutate(
      risk = map2_dbl(
        sg_nu, n,
        \(s, n){
          worst_fun(n = n, sg_nu = s, sg_eps = sg_eps, beta = beta)
        }),
      n = factor(n, levels = sort(unique(n_values)))
    )
}

data_I   <- build_curve_data(worst_case_risk_I,  sg_nu_grid,  n_values, sg_eps_fixed, beta_inf)
marks_I  <- build_curve_data(worst_case_risk_I,  sg_nu_marks, n_values, sg_eps_fixed, beta_inf)
data_II  <- build_curve_data(worst_case_risk_II, sg_nu_grid,  n_values, sg_eps_fixed, beta_inf)
marks_II <- build_curve_data(worst_case_risk_II, sg_nu_marks, n_values, sg_eps_fixed, beta_inf)

pal_n <- scale_color_viridis_d(name = "n", end = 0.85)

# ---- (a) Scenario DIFF ------------------------------------------------------
p_diff <- ggplot(data_diff, aes(sg_eps, risk)) +
  geom_hline(yintercept = 0.9, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9, color = "#3b528b") +
  geom_point(data = marks_diff, size = 1.8, color = "#3b528b") +
  geom_vline(xintercept = sg_eps_fixed, linetype = "dotted", color = "grey30") +
  annotate("text", x = sg_eps_fixed, y = 0.06,
           label = sprintf("\u03c3_\u03b5 = %.3f", sg_eps_fixed),
           hjust = -0.08, size = 3, color = "grey30") +
  scale_x_continuous(breaks = c(seq(0,0.1,0.025)), limits = c(0, 0.101), expand=c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.25),0.9), limits = c(0, 1.01), expand=c(0,0)) +
  labs(title = TeX("(a) Scenario DIFF - differenciation"),
       x = TeX("$\\sigma_\\epsilon$"),
       y = TeX("$\\mu_{DIFF}^{0.05}$")) 

# ---- (b) Scenario I ---------------------------------------------------------
p_I <- ggplot(data_I, aes(sg_nu, risk, color = n)) +
  geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(data = marks_I, size = 1.6) +
  scale_x_continuous(limits = c(0, 0.51), expand = c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.25),0.8), limits = c(0, 1.01), expand = c(0,0)) +
  pal_n +
  labs(title = "(b) Scenario I \u2014 attaque externe",
       x = TeX("$\\sigma_\\nu$"),
       y = TeX("$\\mu_I^{0.2}$  (pire cas en $\\rho$)")) +
  theme(legend.position = "bottom")

# ---- (c) Scenario II --------------------------------------------------------
p_II <- ggplot(data_II, aes(sg_nu, risk, color = n)) +
  # geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(data = marks_II, size = 1.6) +
  scale_x_continuous(limits = c(0, 0.51), expand = c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.25),0.8), limits = c(0, 1.01), expand = c(0,0)) +
  pal_n +
  labs(title = "(c) Scenario II \u2014 attaque interne",
       x = TeX("$\\sigma_\\nu$"),
       y = TeX("$\\mu_{II}^{0.2}$  (pire cas en $\\rho$)")) +
  theme(legend.position = "bottom")

# ---- Assemblage des trois panneaux ------------------------------------------
fig_risques <- (p_diff | p_I | p_II) +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = sprintf(
      paste0("Seuils de reference a 0.5 et 0.8. ",
             "Panneaux (b)-(c) : sigma_epsilon fixe a %.3f (issu de l'etape 1)."),
      sg_eps_fixed)
  ) &
  theme(legend.position = "bottom")

# ---- Sauvegarde -------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)
# ggsave("figures/maitrise_risques.pdf", fig_risques, width = 11, height = 3.8)
ggsave("figures/maitrise_risques.png", fig_risques, width = 11, height = 5, dpi = 300)

print(fig_risques)

# =============================================================================
# (Optionnel) Figure de profil : mu_I(rho) et mu_II(rho) pour une configuration
# unique, afin de justifier l'usage du "pire cas en rho" (forme en cloche,
# maximum atteint a un rho intermediaire).
# =============================================================================
rho_seq    <- seq(0.001, 1, by = 0.001)
sg_nu_demo <- 0.2
n_demo     <- 3

rho_seq_II <- rho_seq[rho_seq >= 0.5]
data_shape <- bind_rows(
  tibble(rho = rho_seq, scenario = "I",
         risk = assess_risk_I(rho_seq, n = n_demo, sg_nu = sg_nu_demo,
                              sg_eps = sg_eps_fixed, beta = beta_inf)),
  tibble(rho = rho_seq_II, scenario = "II",
         risk = assess_risk_II(rho_seq_II, 1 - rho_seq_II, n = n_demo,
                               sg_nu = sg_nu_demo, sg_eps = sg_eps_fixed,
                               beta = beta_inf))
)

p_shape <- ggplot(data_shape, aes(rho, risk, color = scenario)) +
  geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  scale_color_viridis_d(name = "Scenario", end = 0.7) +
  scale_x_continuous(limits = c(0, 1.01), expand = c(0,0)) +
  scale_y_continuous(limits = c(-0.005, 1.01), expand = c(0,0)) +
  labs(title = sprintf("Profil en rho (n = %d, sigma_nu = %.2f)", n_demo, sg_nu_demo),
       x = TeX("$\\rho$"), y = "Risque") 

ggsave("figures/profil_risques_rho.pdf", p_shape, width = 5, height = 3.6)