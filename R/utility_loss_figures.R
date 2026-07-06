library(dplyr)
library(purrr)
library(ggplot2)
library(latex2exp)

source("R/theme_ggplot.R")
source("R/functions.R")


# Parameters -----------------
sigma_epsilons <- c(0,0.005,0.01,0.025,0.05,0.1)
sigma_nus <- c(0,0.01,0.025,0.05,0.1,0.2,0.3,0.4,0.5)
rhos <- seq(0,1,0.01)
n <- 1:12
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


# Confidence Interval of the relative Loss -------------------

IC_Z_loss <- pmap(
  params, 
  IC_relative_loss,
  .progress = TRUE
) |> 
  map(\(ic) data.frame(lb = ic[1]*100, ub = ic[2]*100)) |>
  list_rbind()


bind_cols(params, IC_Z_loss) |> 
  filter(n %in% c(3,6,9,12), sg_nu == sg_nu_demo, sg_eps == sg_eps_demo) |>
  ggplot() +
  geom_line(aes(x = rho, y = lb, color = as.factor(n))) +
  geom_line(aes(x = rho, y = ub, color = as.factor(n))) +
  scale_color_viridis_d(TeX("$n$")) +
  scale_y_continuous("Z loss %", breaks = sort(rep(c(0,5,seq(10,40,10)),2))*c(-1,1), expand = c(0,0), limits = c(-40,40)) +
  scale_x_continuous(
    TeX("$\\rho$"), 
    breaks = seq(0,1,0.25), 
    labels = c("0", seq(0.25,0.75,0.25), "1"),
    expand = c(0,0)) +
  # ggtitle(
  #   label = NULL,
  #   subtitle = sprintf("\u03c3_\u03bd = %.2f, \u03c3_\u03b5 = %.3f", sg_nu_demo, sg_eps_demo)) +
  # 
  ggtitle(
    label = NULL,
    subtitle = TeX(
      sprintf(
        "$\\sigma_\\nu=%.2f$, $\\sigma_\\epsilon=%.3f", 
        sg_nu_demo, sg_eps_demo
      )
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.35, 0.8),
    legend.direction = "horizontal"
  )

ggsave(filename = "figures/perte_relative_IC_cas_unique.png", width = 5, height = 4, dpi=300)

# Absolute expectation of the relative Loss -------------------

abs_expect_Z_loss <- pmap(
  params, 
  abs_expect_relative_loss,
  .progress = TRUE
) |> 
  list_c()


bind_cols(params, loss = abs_expect_Z_loss*100) |> 
  filter(n %in% c(3,6,9,12), sg_nu == sg_nu_demo, sg_eps == sg_eps_demo) |>
  ggplot() +
  geom_line(aes(x = rho, y = loss, color = as.factor(n))) +
  scale_color_viridis_d(TeX("$n$")) +
  scale_y_continuous(
    # TeX("$\\mathbb{E}(|Z|)$ (%)"), 
    expression(bold(E)("|Z|") ~ "(%)"),
    breaks = c(0,2,seq(5,20,5)), expand = c(0,0)) +
  scale_x_continuous(
    TeX("$\\rho$"), 
    breaks = seq(0,1,0.25), 
    labels = c("0", seq(0.25,0.75,0.25), "1"),
    expand = c(0,0)) +
  ggtitle(
    label = NULL,
    subtitle = TeX(
      sprintf(
        "$\\sigma_\\nu=%.2f$, $\\sigma_\\epsilon=%.3f", 
        sg_nu_demo, sg_eps_demo
      )
    )
  ) +
  theme(
    legend.position = "inside",
    legend.position.inside = c(0.35, 0.8),
    legend.direction = "horizontal"
  )

ggsave(filename = "figures/perte_relative_esperance_cas_unique.png", width = 5, height = 4, dpi=300)

