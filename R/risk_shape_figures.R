library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(latex2exp)
library(patchwork)
library(knitr)

source("R/functions.R")
source("R/theme_ggplot.R")

# ---- Profile of the risks I and II depending on rho -------------------------
sigma_epsilons <- c(0,0.005,0.01,0.025,0.05,0.1)
sigma_nus <- c(0,0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5)
rhos <- seq(0,1,0.01)
n <- 1:12
beta_inf_I  <- 0.2 # scenarios I (dominance rule to 80%)
beta_inf_II  <- 0.1 # scenarios II (p% to 90%)
sg_nu_demo <- 0.2
sg_eps_demo <- 0.025

params <- expand.grid(
  rho = rhos,
  n = n,
  sg_nu = sigma_nus,
  sg_eps = sigma_epsilons,
  KEEP.OUT.ATTRS = FALSE
) |>
  as.data.frame()

data_shape <- params |> 
  mutate(
    risk = pmap(
      params, 
      assess_risk_I,
      beta = beta_inf_I,
      .progress = TRUE
    ) |> 
      list_c(),
    scenario = "Scenario: I"
  )

paramsIIa <- params |> 
  filter(rho >= 0.5 & rho < 1) |> 
  mutate(rho2 = 1-rho) |> 
  relocate(rho2, .after = rho)

data_shape <- data_shape |> bind_rows(
  paramsIIa |> 
  mutate(
    risk = pmap(
      paramsIIa, 
      assess_risk_II,
      beta = beta_inf_II,
      .progress = TRUE
    ) |> 
      list_c(),
    scenario = "Scenario: IIa"
  ))

paramsIIb <- params |> 
  filter(rho >= 0.5 & rho < 0.95) |> 
  mutate(rho2 = 0.95-rho) |> 
  relocate(rho2, .after = rho)

data_shape <- data_shape |> bind_rows(
  paramsIIb |> 
    mutate(
      risk = pmap(
        paramsIIb, 
        assess_risk_II,
        beta = beta_inf_II,
        .progress = TRUE
      ) |> 
        list_c(),
      scenario = "Scenario: IIb"
    ))

p_shape <- data_shape |> 
  filter(n %in% c(3,6,9,12), sg_nu == sg_nu_demo, sg_eps == sg_eps_demo) |>
  ggplot(aes(rho, risk, color = as.factor(n))) +
  geom_hline(yintercept = thresholds, linetype = "dashed", color = "grey60") +
  geom_line(linewidth = 0.9) +
  facet_wrap(~scenario) +
  scale_color_viridis_d(TeX("$n$")) +
  scale_x_continuous(breaks = c(seq(0,1,0.25)), labels = c("0", seq(0.25,0.75,0.25), "1"), limits = c(0, 1.01), expand = c(0,0)) +
  scale_y_continuous(breaks = c(seq(0,1,0.2)), limits = c(-0.005, 1.01), expand = c(0,0)) +
  labs(x = TeX("$\\rho$"), y = "Risk") +
  ggtitle(
    label = NULL,
    subtitle = TeX(
      sprintf(
        "$\\sigma_\\nu=%.2f$, $\\sigma_\\epsilon=%.3f$, IIa: $\\rho + \\rho_2=1$, IIb: $\\rho + \\rho_2=0.95$", 
        sg_nu_demo, sg_eps_demo
      )
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.5,0.15),
    legend.direction = "horizontal"
  )

ggsave("figures/profile_risks_rho.png", p_shape, width = 8, height = 3.6)

print(p_shape)
