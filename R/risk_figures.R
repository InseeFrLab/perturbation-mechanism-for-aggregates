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

data_IIa  <- build_curve_data(worst_case_risk_II, sg_nu_grid,  n_values, sg_eps_fixed, beta_inf_II)
marks_IIa <- build_curve_data(worst_case_risk_II, sg_nu_marks, n_values, sg_eps_fixed, beta_inf_II)
data_IIb  <- build_curve_data(worst_case_risk_IIb, sg_nu_grid,  n_values, sg_eps_fixed, beta_inf_II)
marks_IIb <- build_curve_data(worst_case_risk_IIb, sg_nu_marks, n_values, sg_eps_fixed, beta_inf_II)

data_II <- bind_rows(data_IIa |> mutate(scenario="IIa"),
                    data_IIb |> mutate(scenario="IIb"))
marks_II <- bind_rows(marks_IIa |> mutate(scenario="IIa"),
                      marks_IIb |> mutate(scenario="IIb"))

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
  geom_line(aes(linetype=scenario), linewidth = 0.9) +
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
    caption = TeX(
      sprintf(
        "Panels (b) & (c) : $\\sigma_\\epsilon=%.3f$", 
        sg_eps_fixed
      )
    )
      # sprintf(
      #        "Panels (b) & (c) : \u03c3_\u03b5 set to %.3f.",
      # sg_eps_fixed)
  ) &
  theme(
    legend.position = "bottom",
    legend.title.position = "top"
    )

ggsave("figures/monitoring_risks.png", fig_risques, width = 11, height = 5, dpi = 300)

print(fig_risques)



