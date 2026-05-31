library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(latex2exp)
library(patchwork)

source("R/functions.R")
source("R/theme_ggplot.R")

# ---- Parameters -------------------------------------------------------------
beta_inf_I  <- 0.2 # scenarios I & II (dominance rule to 80%)
beta_inf_II  <- 0.1 # scenarios I & II (rp% to 90%)
beta_inf_diff <- 0.05     # scenario DIFF to 5%
n_values  <- c(3, 6, 9, 12)

# Trhesholds of inference risks
tau_mid   <- 0.5     
tau_high  <- 0.8
tau_diff  <- 0.9
thresholds <- c(tau_mid, tau_high)

# grilles continues (axes des abscisses)
sg_nu_grid  <- seq(0, 0.5, by = 0.005)
sg_eps_grid <- seq(0, 0.1, by = 0.001)

# valeurs caracteristiques
sg_nu_marks  <- c(0.05, 0.1, 0.2, 0.3, 0.4, 0.5)
sg_eps_marks <- c(0.01, 0.025, 0.05, 0.1)

# ---- Step 1 : scenario DIFF and choice of sigma_epsilon ------------------
data_diff <- tibble(sg_eps = sg_eps_grid) %>%
  mutate(risk = assess_risk_DIFF(sg_eps = sg_eps, beta = beta_inf_diff))

marks_diff <- tibble(sg_eps = sg_eps_marks) %>%
  mutate(risk = assess_risk_DIFF(sg_eps = sg_eps, beta = beta_inf_diff))

# sigma_epsilon chosen : smallest value such that risk <= tau_diff
sg_eps_fixed <- data_diff %>%
  filter(risk <= tau_diff) %>%
  slice_min(sg_eps, n = 1) %>%
  pull(sg_eps)

# ---- Steps 2 & 3 : scenarios I & II --------------------------------------
# Search of worst case risk in rho for a grid values of (sigma_nu x n) 
# sigma_epsilon set to the optimal value chosen at step 1
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

data_I   <- build_curve_data(worst_case_risk_I,  sg_nu_grid,  n_values, sg_eps_fixed, beta_inf_I)
marks_I  <- build_curve_data(worst_case_risk_I,  sg_nu_marks, n_values, sg_eps_fixed, beta_inf_I)
data_II  <- build_curve_data(worst_case_risk_II, sg_nu_grid,  n_values, sg_eps_fixed, beta_inf_II)
marks_II <- build_curve_data(worst_case_risk_II, sg_nu_marks, n_values, sg_eps_fixed, beta_inf_II)

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
  labs(title = "(a) Scenario DIFF - differenciation",
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
  labs(title = "(b) Scenario I - external attack",
       x = TeX("$\\sigma_\\nu$"),
       y = TeX("$\\mu_I^{0.2}$  (worst case in $\\rho$)")) +
  theme(legend.position = "bottom")

# ---- (c) Scenario II --------------------------------------------------------
p_II <- ggplot(data_II, aes(sg_nu, risk, color = n)) +
  geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  geom_point(data = marks_II, size = 1.6) +
  scale_x_continuous(limits = c(0, 0.51), expand = c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.25),0.8), limits = c(0, 1.01), expand = c(0,0)) +
  pal_n +
  labs(title = "(c) Scenario II - internal attack",
       x = TeX("$\\sigma_\\nu$"),
       y = TeX("$\\mu_{II}^{0.1}$  (worst case in $\\rho$ and $\\rho_2$)")) +
  theme(legend.position = "bottom")

# ---- Panels together ------------------------------------------
fig_risques <- (p_diff | p_I | p_II ) +
  plot_layout(guides = "collect") +
  plot_annotation(
    caption = sprintf(
             "Panels (b) & (c) : \u03c3_\u03b5 set to %.3f.",
      sg_eps_fixed)
  ) &
  theme(legend.position = "bottom")

ggsave("figures/monitoring_risks.png", fig_risques, width = 11, height = 5, dpi = 300)

print(fig_risques)


# ---- Profile of the risks I and II depending on rho -------------------------
rho_seq    <- seq(0.001, 1, by = 0.001)
sg_nu_demo <- 0.3
n_demo     <- 6

rho_seq_II <- rho_seq[rho_seq >= 0.5]

data_shape <- bind_rows(
  tibble(rho = rho_seq, scenario = "I",
         risk = assess_risk_I(rho_seq, n = n_demo, sg_nu = sg_nu_demo,
                              sg_eps = sg_eps_fixed, beta = beta_inf_I)),
  tibble(rho = rho_seq_II, scenario = "II",
         risk = assess_risk_II(rho_seq_II, 1 - rho_seq_II, n = n_demo,
                               sg_nu = sg_nu_demo, sg_eps = sg_eps_fixed,
                               beta = beta_inf_II))
)

p_shape <- ggplot(data_shape, aes(rho, risk, color = scenario)) +
  geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  scale_color_viridis_d(name = "Scenario", end = 0.7) +
  scale_x_continuous(breaks = c(seq(0,1,0.25)), limits = c(0, 1.01), expand = c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.2)), limits = c(-0.005, 1.01), expand = c(0,0)) +
  labs(title = sprintf("Risk profile in \u03c1 (n = %d, \u03c3_\u03bd = %.2f, \u03c3_\u03b5 = %.3f)", n_demo, sg_nu_demo, sg_eps_fixed),
       x = TeX("$\\rho$"), y = "Risk") +
  theme(legend.position = "inside", legend.position.inside = c(0.25,0.25))

ggsave("figures/profile_risks_rho.png", p_shape, width = 5, height = 3.6)
