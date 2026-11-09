# Scratch example: emulator = "replace" against running the model directly, as
# the simulator's Monte Carlo noise grows.
#
# The model is y1 = x1 + x2, y2 = x1 * x2 with Gaussian noise, so the answer is
# known (x1 = 0.5, x2 = 1) and two targets identify two parameters exactly.
# `noise` is in bound half-widths: 0.01 is a near-deterministic simulator, 1 is
# one whose single run lands a whole half-width from its own expectation, which
# is the regime CRC-SPIN is in (median SEER noise is ~0.52 half-widths).
#
# The four runs are verbose and sequential, so the terminal also shows what a
# user sees in each mode. The visible difference is that an emulator run has a
# simulation progress bar in iteration 1 only, followed by the training message,
# and none afterwards, because every later iteration is a forward pass.
#
# Run from the package root: Rscript dev/neural_imabc.R

suppressMessages({
  devtools::load_all(".", quiet = TRUE)
  # attached here, and before dplyr so dplyr wins the name clashes: the foreach
  # backend loads it lazily otherwise, which prints its startup message in the
  # middle of the first iteration
  library(data.table)
  # select() is qualified as dplyr::select() throughout, because imabc imports
  # MASS and MASS::select(obj) takes a single argument: in a session where MASS
  # ends up attached after dplyr, a bare select() fails with "unused arguments"

  library(dplyr)
  library(tidyr)
  library(purrr)
  library(ggplot2)
})

# small net: this is a 2 -> 2 problem and the script should finish in a minute
options(imabc.neural_config = list(
  hidden = 64L, depth = 2L, members = 2L, max_epochs = 150L, workers = 2L
))

truth <- tibble(parm = c("x1", "x2"), truth = c(0.5, 1))
half_width <- c(Y1 = 0.03, Y2 = 0.01)
noise_levels <- c(0.01, 1)

# the counter is how the script reports model evaluations: it ticks once per
# call, which is once per draw the model actually simulated
toy_model <- function(noise, counter) {
  function(x1, x2) {
    counter$n <- counter$n + 1L
    c(
      Y1 = x1 + x2 + rnorm(1, 0, noise * half_width[["Y1"]]),
      Y2 = x1 * x2 + rnorm(1, 0, noise * half_width[["Y2"]])
    )
  }
}

priors <- define_priors(
  x1 = add_prior(parameter_name = "x1", dist_base_name = "unif", min = 0.4, max = 0.6),
  x2 = add_prior(parameter_name = "x2", dist_base_name = "unif", min = 0.8, max = 1.2)
)

targets <- define_targets(G1 = group_targets(
  Y1 = add_target(
    scale = 1, target = 1.5,
    starting_range = 1.5 + c(-20, 20) * half_width[["Y1"]],
    stopping_range = 1.5 + c(-1, 1) * half_width[["Y1"]]
  ),
  Y2 = add_target(
    scale = 1, target = 0.5,
    starting_range = 0.5 + c(-20, 20) * half_width[["Y2"]],
    stopping_range = 0.5 + c(-1, 1) * half_width[["Y2"]]
  )
))

run_one <- function(noise, evaluated_by) {
  # the runs are sequential, so this line is what attributes the imabc output
  # below it to a run; in an emulator run it is followed by the training message
  cat(sprintf("\nnoise = %g, evaluated by the %s\n", noise, evaluated_by))
  counter <- new.env(parent = emptyenv())
  counter$n <- 0L
  res <- imabc(
    target_fun = define_target_function(
      targets, priors,
      FUN = toy_model(noise, counter), use_seed = TRUE
    ),
    priors = priors, targets = targets,
    emulator = if (evaluated_by == "model") "none" else "replace",
    seed = 20261109, N_start = 2000, max_iter = 100, N_centers = 5, Center_n = 500,
    N_cov_points = 50, N_post = 500, improve_method = "percentile", verbose = TRUE
  )
  meta <- function(field) as.numeric(res$metaddata$value[res$metaddata$info == field])

  list(
    # the accepted draws are a weighted sample, so every summary and every
    # density below has to carry sample_wt
    draws = res$good_parm_draws %>%
      as_tibble() %>%
      dplyr::select(all_of(truth$parm), sample_wt) %>%
      mutate(noise = noise, evaluated_by = evaluated_by, w = sample_wt / sum(sample_wt)) %>%
      pivot_longer(all_of(truth$parm), names_to = "parm", values_to = "value"),
    cost = tibble(
      noise = noise, evaluated_by = evaluated_by,
      iterations = meta("current_iteration"),
      draws_evaluated = meta("last_draw"),
      model_evals = counter$n
    )
  )
}

as_approach <- function(x) factor(x, c("model", "emulator"))

runs <- expand_grid(noise = noise_levels, evaluated_by = c("model", "emulator")) %>%
  pmap(run_one)

draws <- runs %>%
  map("draws") %>%
  bind_rows() %>%
  mutate(evaluated_by = as_approach(evaluated_by))

cost <- runs %>%
  map("cost") %>%
  bind_rows() %>%
  mutate(
    evaluated_by = as_approach(evaluated_by),
    model_evals_per_draw = round(model_evals / draws_evaluated, 3)
  )

print(cost)

posteriors <- draws %>%
  group_by(noise, evaluated_by, parm) %>%
  summarise(
    accepted = n(),
    ess = 1 / sum(w^2),
    post_mean = weighted.mean(value, w),
    post_sd = sqrt(sum(w * (value - weighted.mean(value, w))^2)),
    .groups = "drop"
  ) %>%
  left_join(truth, by = "parm") %>%
  mutate(bias_in_post_sd = abs(post_mean - truth) / post_sd) %>%
  dplyr::select(noise, evaluated_by, parm, truth, post_mean, post_sd, bias_in_post_sd, accepted, ess)

posteriors %>%
  mutate(
    across(c(post_mean, post_sd), \(x) round(x, 4)),
    across(c(bias_in_post_sd, ess), \(x) round(x, 2))
  ) %>%
  print(n = Inf)

# ratio of posterior widths: 1 means the emulator reproduced the model's
# uncertainty, below 1 means it understated it
widths <- posteriors %>%
  dplyr::select(noise, evaluated_by, parm, post_sd) %>%
  pivot_wider(names_from = evaluated_by, values_from = post_sd) %>%
  mutate(sd_ratio = round(emulator / model, 2))

print(widths)

panel_label <- function(noise) {
  if_else(
    noise < 0.1,
    sprintf("noise = %g\nnear-deterministic", noise),
    sprintf("noise = %g\nCRC-SPIN regime", noise)
  )
}

# the model-evaluation counts go in the row strip, where they have their own
# space, rather than into the panels where the mean lines already are
panels <- cost %>%
  group_by(noise) %>%
  summarise(
    panel = sprintf(
      "%s\n%s model evals (emulator %s)",
      panel_label(noise[1]),
      format(model_evals[evaluated_by == "model"], big.mark = ","),
      format(model_evals[evaluated_by == "emulator"], big.mark = ",")
    ),
    .groups = "drop"
  )

# the two densities coincide exactly at low noise, so the model is drawn wide
# and the emulator narrow on top of it: one visible line means they agree
posterior_plot <- ggplot(left_join(draws, panels, by = "noise")) +
  geom_density(
    aes(value, weight = w, colour = evaluated_by, fill = evaluated_by, linewidth = evaluated_by),
    alpha = 0.15
  ) +
  geom_vline(
    aes(xintercept = post_mean, colour = evaluated_by),
    data = left_join(posteriors, panels, by = "noise"),
    linewidth = 0.4, show.legend = FALSE
  ) +
  geom_vline(aes(xintercept = truth), data = truth, linetype = "dashed", linewidth = 0.5) +
  facet_grid(panel ~ parm, scales = "free") +
  scale_colour_manual(values = c(model = "#1b6ca8", emulator = "#d9641e")) +
  scale_fill_manual(values = c(model = "#1b6ca8", emulator = "#d9641e")) +
  scale_linewidth_manual(values = c(model = 1.3, emulator = 0.5), guide = "none") +
  scale_y_continuous(expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title = "Posteriors against the truth, with the model and with the emulator",
    subtitle = paste(
      "Dashed line is the true value, thin solid lines are the posterior means.",
      "Noise is in target bound half-widths."
    ),
    x = NULL, y = "weighted density", colour = "evaluated by", fill = "evaluated by"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "top",
    panel.grid.minor = element_blank(),
    strip.text.y.right = element_text(angle = 0)
  )

ggsave("dev/neural_imabc_posteriors.png", posterior_plot, width = 9, height = 5.5, dpi = 150)

# What it prints, at seed 20261109:
#
#   noise evaluated_by iterations draws_evaluated model_evals model_evals_per_draw
#    0.01        model          2            4500        4505                1.00
#    0.01     emulator          2            4500        2000                0.444
#    1           model          3            7000        7010                1.00
#    1        emulator          3            7000        2000                0.286
#
#   noise evaluated_by parm truth post_mean post_sd bias_in_post_sd accepted  ess
#    0.01        model   x1   0.5    0.5018  0.0194            0.10     1126 1119
#    0.01        model   x2   1.0    0.9984  0.0329            0.05     1126 1119
#    0.01     emulator   x1   0.5    0.5020  0.0195            0.10     1128 1121
#    0.01     emulator   x2   1.0    0.9981  0.0330            0.06     1128 1121
#    1.00        model   x1   0.5    0.5068  0.0318            0.21      974  964
#    1.00        model   x2   1.0    0.9905  0.0537            0.18      974  964
#    1.00     emulator   x1   0.5    0.5015  0.0202            0.07     2412 2395
#    1.00     emulator   x2   1.0    0.9977  0.0344            0.07     2412 2395
#
#   sd_ratio: 1.01, 1.00 at noise 0.01; 0.64, 0.64 at noise 1.00
#
# Cost. The emulator runs the model for N_start = 2,000 draws and never again,
# so its model evaluations do not depend on how long the run takes, while the
# direct run's grow with every iteration. This toy is easy -- it converges in 2
# or 3 iterations -- so the saving here is only 2.3x and 3.5x. The point is the
# direction: the harder (noisier) case needs 3 iterations instead of 2, and the
# direct run pays for it (4,505 -> 7,010 evaluations) where the emulator does
# not (2,000 either way). On CRC-SPIN a production run is 42 iterations and
# 2.17M simulations against one hypercube, which is the same argument at ~20x.
#
# The 5 extra evaluations in the direct runs are the re-simulated centers, which
# total_draws excludes and the counter does not.
#
# Accuracy. Both recover the answer in both regimes: the posterior mean is
# within 0.21 posterior SD of the truth everywhere, and the emulator is in fact
# the closer of the two at noise = 1.
#
# Uncertainty. At noise = 0.01 the two posteriors are the same to the third
# decimal. At noise = 1 the model's posterior widens (0.0194 -> 0.0318 and
# 0.0329 -> 0.0537) while the emulator's barely moves (0.0195 -> 0.0202 and
# 0.0330 -> 0.0344), so it reports about two thirds of the real uncertainty.
#
# That is the stated property of the mode rather than a defect of the fit: the
# ensemble mean estimates E[x | theta], so Monte Carlo noise is averaged away
# instead of being propagated into the posterior. The accepted counts show the
# same thing from the other side -- with a deterministic emulator, whether a
# draw is accepted is a function of theta alone, so 2,412 draws qualify against
# the model's 974.
#
# Practical reading: the saving is in model evaluations, and the cost is that
# posterior spread is understated by roughly the simulator's own noise. Point
# estimates are safe; credible intervals from a "replace" run are not, unless
# the model's noise is small against the target bounds.
