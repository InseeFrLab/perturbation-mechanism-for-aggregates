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
  filter(n %in% c(3,6,9,12), sg_nu == 0.2, sg_eps == 0.025) |>
  ggplot() +
  geom_line(aes(x = rho, y = lb, color = as.factor(n)), linewidth = 0.5) +
  geom_line(aes(x = rho, y = ub, color = as.factor(n)), linewidth = 0.5) +
  scale_color_viridis_d(TeX("$n$")) +
  scale_y_continuous("Z loss %", breaks = sort(rep(c(0,5,seq(10,40,10)),2))*c(-1,1), expand = c(0,0), limits = c(-40,40)) +
  scale_x_continuous(TeX("$\\rho$"), expand = c(0,0)) +
  ggtitle(
    label = NULL,
    # TeX("Perturbation relative moyenne (E(|Z|) en fonction de $\\rho$"),
    subtitle = TeX("$\\sigma_\\nu=0.2$, $\\sigma_\\epsilon=0.025$")) +
  theme(
    legend.position = "bottom"
    # legend.position.inside = c(0.1, 0.25)
  )

ggsave(filename = "figures/perte_relative_IC_cas_unique.png", width = 4, height = 5)

# Absolute expectation of the relative Loss -------------------

abs_expect_Z_loss <- pmap(
  params, 
  abs_expect_relative_loss,
  .progress = TRUE
) |> 
  list_c()


bind_cols(params, loss = abs_expect_Z_loss*100) |> 
  filter(n %in% c(3,6,9,12), sg_nu == 0.2, sg_eps == 0.025) |>
  ggplot() +
  geom_line(aes(x = rho, y = loss, color = as.factor(n)), linewidth = 0.5) +
  scale_color_viridis_d(TeX("$n$")) +
  scale_y_continuous(
    # TeX("$\\mathbb{E}(|Z|)$ (%)"), 
    expression(bold(E)("|Z|") ~ "(%)"),
    breaks = c(0,2,seq(5,20,5)), expand = c(0,0), limits = c(0,20)) +
  scale_x_continuous(TeX("$\\rho$"), expand = c(0,0)) +
  ggtitle(
    label = NULL,
    # TeX("Perturbation relative moyenne (E(|Z|) en fonction de $\\rho$"),
    subtitle = TeX("$\\sigma_\\nu=0.2$, $\\sigma_\\epsilon=0.025$")) +
  theme(
    legend.position = "bottom"
    # legend.position.inside = c(0.1, 0.5)
  )

ggsave(filename = "figures/perte_relative_esperance_cas_unique.png", width = 4, height = 5)

