# Neural IMABC: requirements and implementation plan

## 1. Goal

Let IMABC calibrate an expensive simulator through a neural surrogate, as an opt-in mode of the existing `imabc` package. The user adds one argument; everything else about their calibration stays as it is.

## 2. Purpose

IMABC narrows tolerance intervals by simulating batches of parameter draws, so its cost is iterations times batch size. Production CRC-SPIN took 42 iterations and about 2.17 million model runs (`RunMetadata_20260908_1342PDT.csv`).

A validated prototype (`neuralsbi-crcspin/R/neural_imabc.R`) replaced CRC-SPIN with a four-member MLP ensemble trained on a single Latin hypercube and ran the same production IMABC configuration against it. Results from `output/tables/nimabc_lhs_size.csv`:

| LHS rows | Iterations | 5-seed mean score (of 101) | Inside every seed |
|---|---|---|---|
| 10,000 | 28 | 82 | 77 |
| 25,000 | 23 | 96.0 | 90 |
| 50,000 | 21 | 97.2 | 89 |
| 100,000 | 21 | 99.4 | 95 |
| 139,825 (all) | 26 | 98.8 | 92 |
| IMABC on real CRC-SPIN | 42 | 99.8 | 95 |

So one iteration's worth of model runs, plus about 2.4 minutes of training and 7.5 minutes of IMABC, reproduces a calibration that cost 2.17 million simulations. That is the result this work is making available inside the package.

## 3. Scope

v1 delivers one mode: **the emulator replaces the model** after the first iteration. Later phases, each with its own section below:

| Later phase | Why it waits |
|---|---|
| Center refinement | It changes where IMABC looks, so it must be measured against a working replace mode as the baseline. |
| Iteratively-trained mode (`"guide"`) | It is only worth its complexity if refitting beats a single fit, which needs the replace mode's numbers to compare against. |
| Continuing a run (`previous_results_dir`) | The emulator is not part of IMABC's saved run state; the mode rejects the combination for now. |
| Emulator noise | Adding the measured per-target noise back stopped IMABC converging at all (0 of 40 SEER bounds in 100 iterations). A deterministic emulator means IMABC calibrates `E[x | theta]`, which is a property of the mode, not a defect. |
| Posterior checking inside `imabc()` | Judging a calibration belongs to whoever is evaluating one. `neuralsbi-crcspin` already does it (`R/score_targets.R`, `R/eval_posterior_means.R`, `tmp_test_confirm.R`). |

## 4. Design constraints

1. All logic lives in new `R/neural_*.R` files, one concern per file.
2. `R/imabc.R` gains the user-facing argument and its dispatch, nothing more.
3. `torch` is a `Suggests` dependency, checked with `requireNamespace()` and never attached, so every torch call is qualified.
4. The existing output keeps its exact shape: same elements, same columns. One additive element is allowed.
5. Work starts from `CurrentDev`.
6. Terse, well-designed code; comments carry the *why*.

## 5. Where the extension attaches

Verified on `CurrentDev` at `cce477a`.

| Thing | Location | Note |
|---|---|---|
| `imabc()` signature | `R/imabc.R:148` | `backend_fun = NULL` is already a public argument |
| Only `target_fun` call site | `R/imabc.R:498` | one `run_handler()` call, passing `custom_function = backend_fun` |
| Custom backend contract | `R/run_handler.R:21` | `custom_function(parms_to_run, all_parm_names, target_fun, other_inputs)` — the whole batch, in the calling process |
| Result reorder | `R/imabc.R:504` | `res[, match(sim_target_names, colnames(res))]` |
| Iteration number on the batch | `R/imabc.R:1132` | `iter_parm_draws$iter <- main_loop_iter + 1` |
| Re-simulated centers | `step == N_centers + 1` | present in `parms_to_run` from iteration 2 |
| Return value | `R/imabc.R:1597` | `good_parm_draws`, `good_sim_target`, `good_target_dist`, `mean_cov`, `priors`, `targets`, `metaddata`, plus `all_iter_*` when `validate_run` |
| Center selection | `R/imabc.R:1089-1124` | `center_next` at 1124: the single hook center refinement needs |
| RNG | `R/imabc.R:281`, `R/seed_stream.R` | L'Ecuyer-CMRG for the whole call |

Because the backend receives a whole batch in the calling process, a closure over one environment can hold the emulator across iterations, and the main loop needs no emulator-specific code at all. The prototype proved this seam: it substituted CRC-SPIN entirely through `backend_fun` without patching the package.

The difference in the package is who builds that closure. The prototype asked its caller to. Here `imabc()` does it: when `emulator != "none"` it replaces `backend_fun` with a neural backend that holds the user's model backend inside it.

Two traps in this seam:

- **The backend must return a `data.frame`, not a `data.table`.** `res[, match(...)]` on a `data.table` returns a bare integer vector, which silently empties the results.
- `imabc()` already uses the word `validate` for `validate_run`, which controls saving intermediate results. New code must not reuse it.

## 6. User API

```r
imabc(target_fun, priors, targets, ..., emulator = c("none", "replace"))
```

`emulator` is the only addition.

| `emulator` | `target_fun` runs for | The emulator |
|---|---|---|
| `"none"` (default) | every draw | is not built; classic IMABC, unchanged |
| `"replace"` | the first iteration only | is trained on those results and evaluates every later iteration |

Every other argument keeps its meaning, `backend_fun` included: it still only says how the real model runs (`NULL` is the default `foreach` path; a function is an EMEWS or cluster backend). The neural wrapping of it is internal, so a user running on a cluster changes nothing but `emulator`.

There is no `starting_targets` argument. The emulator learns from whatever the first iteration simulated, so the Latin hypercube stays IMABC's own first iteration.

The result gains one element, `neural`:

```r
res$neural$emulator   # the fitted ensemble; predict(emulator, theta) works
res$neural$val_mse    # per-member holdout MSE, in standardised units
res$neural$n_train    # draws trained on
res$neural$file       # where it was saved, when output_directory was given
```

## 7. Internal configuration

`neural_config()` holds every setting. None is a user argument yet; `options(imabc.neural_config = list(...))` is the undocumented development override the experiments and the fast test path use. Explicit arguments beat the option, which beats the defaults.

### 7.1 Training

| Setting | Default | Why |
|---|---|---|
| Training rows | every (theta, x) the model returned in iteration 1 | Rejected draws say where the bounds fail. Accepted draws alone are a few hundred rows. |
| Network | 256 x 3 hidden, SiLU, inputs and outputs standardised | The validated recipe. Holdout MSE about 0.01 in standardised units at 100k rows. |
| Members | 4, each with its own seed and holdout split | The mean is the predictor. Disagreement among members is where the emulator is least trustworthy: it is the guard for center refinement and the per-iteration warning sign. At 10k rows the worst member scored 71 of 101 where real CRC-SPIN scored 82. |
| Optimiser | Adam 1e-3, batch 1024, halve the LR on a 5-epoch plateau, stop after 20 epochs without gain, at most 200 epochs; holdout `min(10k, n/10)` | Validated. The full-LHS members hit the 200-epoch cap at holdout MSE 0.009-0.013, so the cap binds: it is a time budget, not convergence. |
| Scalers | per member, from that member's training split | Zero-variance columns are left on their own scale, so a fixed parameter or a constant target does not produce an infinite standardised column. |
| Missing targets | `na = "mask"` | See 7.2. |
| Parallelism | one PSOCK worker per member, 1 torch thread each | A 256-wide MLP at batch 1024 is bound by per-op overhead, not flops: 9.1 to 3.1 minutes on the full LHS, identical members. |
| Seeds | drawn from IMABC's own RNG stream | One `seed` argument reproduces the emulator along with the rest of the run. |

### 7.2 Missing targets

An earlier version of this plan argued for a masked loss on the grounds that CRC-SPIN's early exit returns NA for every target group after the first failure, so dropping incomplete rows would discard most of the Latin hypercube. **That rationale was wrong**, and the correction matters because it changes what the right default is:

- The stored LHS ran with `quick_exit = FALSE` (`output/tables/quick_exit_check.csv`: 0 all-NA rows out of 150,000), so early exit never fired and `range_check` never gated anything.
- The NAs that are there mean something else. `neuralsbi-crcspin/R/paths.R::apply_na_policy()` reads a non-Pickhardt NA as a missing numerator over an existing denominator, i.e. an **exact 0**, and a Pickhardt NA as true missingness, i.e. **drop the row**. That keeps 139,825 of 150,000 rows where complete-case would keep 53,363.
- The prototype therefore trains with a plain MSE and asserts `!anyNA(x)`. Masking those entries would have *discarded* information, not rescued it.

So NA handling is a domain decision, and the package cannot make it for a user. It is a setting:

| `cfg$na` | Behaviour | When it is right |
|---|---|---|
| `"mask"` (default) | An NA entry contributes nothing to the loss; the rest of its row still trains | The only domain-agnostic choice: a gap means "not reported", nothing more |
| `"zero"` | NA is read as an exact 0 | A gap means a missing numerator over a real denominator, as in CRC-SPIN |
| `"drop"` | Incomplete rows are dropped | A gap means the row is not trustworthy |

With no missing values the masked loss reduces exactly to the plain mean squared error, so the default costs nothing on complete data. CRC-SPIN work applies its own policy upstream of the emulator and never relies on this setting.

### 7.3 Evaluation in replace mode

| Setting | Default | Why |
|---|---|---|
| Predictor | ensemble mean, deterministic | With measured noise added, IMABC did not converge in 100 iterations. Deterministic means it calibrates `E[x | theta]`, a stated property of the mode. |
| Iteration 1 | accepted on the real results, unchanged | There is no emulator yet, and it barely matters: the best LHS row scores 66 of 101, so no iteration-1 draw survives to the stopping bounds. |
| Center re-simulation | unchanged | On a deterministic emulator the re-simulated center returns the same values, so no special path is needed. |
| Minimum training rows | 500 | A 150-row first iteration cannot train this network, and the failure should name `N_start` rather than appear as a bad calibration. |

## 8. Milestones

### Milestone 1 — branch and scaffolding (done)

**Requirements**

1. Branch `neural-imabc` from `origin/CurrentDev`.
2. `torch` added to `Suggests`.

**Acceptance:** the existing suite passes unchanged on the branch, as the baseline for "`emulator = \"none\"` is untouched".

### Milestone 2 — the emulator, standalone (done)

`R/neural_emulator.R`, usable and testable with no reference to IMABC. A port of `nimabc_make_net` / `nimabc_fit_member` / `nimabc_surrogate`, keeping the recipe identical so milestone 4's numbers stay comparable.

**Requirements**

1. `neural_config(...)` returns the settings of section 7, with explicit arguments overriding `options(imabc.neural_config)` overriding the defaults.
2. `neural_fit(X, Y, cfg, seeds)` returns a `neural_emulator`: members, parameter and target names, per-member holdout MSE and epochs, training size, settings.
3. The loss is masked by observation, and `cfg$na` selects between masking, reading zero, and dropping rows.
4. A target no draw ever reported is an error naming the target, not a NaN fit.
5. A parameter or target that never varies does not produce an infinite standardised column.
6. Members train in parallel, one worker each, and a member is plain R data so it can cross a worker boundary and be saved.
7. `predict()` returns a `data.frame` of named targets for the ensemble mean, or the per-member predictions on request. Parameter columns are matched by name.
8. `neural_write()` / `neural_read()` round-trip an emulator without changing a prediction.

**Acceptance:** `tests/testthat/test-neural-emulator.R` recovers `y1 = x1 + x2`, `y2 = x1 * x2` from 2,000 draws to within 1%, and covers requirements 3-8. The package checks clean with `torch` absent, every test skipping.

### Milestone 3 — `emulator = "replace"` (done)

**Requirements**

1. `imabc()` takes `emulator = c("none", "replace")`; the diff to `R/imabc.R` is the signature, its documentation, and dispatch.
2. The model runs for the first iteration and never again; later iterations are forward passes.
3. The user's `backend_fun` runs the model iteration when given, and `NULL` still means the default `foreach` path.
4. `emulator = "none"` behaves exactly as before.
5. The result keeps every element and column it had, plus `neural`.
6. The emulator is written to `output_directory` when one is given, tagged like the other outputs.
7. Too few draws to train on, and `previous_results_dir`, are each refused with a message that says what to do.
8. `verbose` reports the training size and the resulting holdout MSE.

**Acceptance:** the existing suite passes untouched. `tests/testthat/test-neural-replace.R` converges on the toy problem with the model called exactly `N_start` times, and the real toy model, re-run 5 times at the posterior mean, puts every target inside its stopping bounds every time. `R CMD check` is clean.

**Outcome.** `R/imabc.R` gained 6 lines of code and one documentation block. On the toy problem: 2,000 model calls, all tolerance intervals reached their stopping bounds, effective sample size 425 against a target of 100, and the posterior mean `(x1, x2) = (0.5042, 0.9935)` gives `Y1 = 1.4977` and `Y2 = 0.5009`, both inside the stopping bounds.

### Milestone 4 — CRC-SPIN replay

Lives in `neuralsbi-crcspin`, not in the package: a variant of `run_neural_imabc()` that uses the package mode instead of pre-training outside IMABC.

**Requirements**

1. `starting_draws` is the first `n_lhs` rows of `output/subsets.csv` and `N_start = n_lhs`, so iteration 1 *is* the stored LHS subset and looking its results up is legitimate. Confirm first that `imabc()` passes `starting_draws` through unscaled and unreordered.
2. `backend_fun` returns the stored `SimulatedTargets_fullLHS.csv` rows for those draws, already through `apply_na_policy()`, as a `data.frame`.
3. Everything else is the production configuration: `N_centers = 20`, `Center_n = 2500`, `N_cov_points = 250`, `N_post = 1000`, `sample_inflate = 150`, `tiebraker_dist_fn = "chisquare"`, `tiebraker_dist_sum_avgs = TRUE`, `improve_method = "percentile"`, `seed = 12345`, `validate_run = FALSE`, and `options(imabc.target_eval_distance = "zscore", imabc.min_sample_var_pct = 0.005)`.
4. Scoring stays in `R/score_targets.R`, and the real-CRC-SPIN check in `tmp_test_confirm.R` with `quick_exit = FALSE`.

**Acceptance:** at 100k rows, about 21 iterations and a 5-seed mean of at least 98 of 101, against the prototype's 21 and 99.4. Re-run at 50k and 25k to confirm the size curve of section 2 survives the port.

### Milestone 5 — center refinement

The first phase that changes where IMABC looks rather than how it evaluates.

Take the centers IMABC already chose, `c_1..c_K`, and move each by gradient descent on the emulator:

    minimise  L(theta) = sum_j relu( max_m d_j(theta; m) - 0.7 )^2   over theta in the prior box

`d_j` is the distance from target j to its **stopping** bounds, scaled per side so that it is 1 at the bound.

| Requirement | Why |
|---|---|
| Start from IMABC's own centers | Selection is untouched, so zero steps reproduces classic IMABC exactly: a clean ablation. |
| Stopping bounds, not current | The centers are accepted draws, so they already satisfy the current bounds; with current bounds `L = 0` and nothing moves. |
| This `d`, not IMABC's z-score tie-breaker | The z-score measures distance to the target point. `d` measures distance to the acceptance region, respects asymmetric bounds, and is 0 exactly where the run has to end. |
| Hinge | The gradient vanishes once every target is within 0.7 of its half-width, so each center stops at the first acceptable point on its path rather than being pulled toward one global optimum. |
| Worst member (`max_m`) | Members disagree most where they have no data, so requiring all four to agree keeps the optimiser out of places where one net is wrong. |
| Margin 0.7, flat | Room for emulator error and model noise. Flat beat noise-aware margins in the optimisation track. |
| Prior box by clamping each step | Proposals are truncated to the prior support, so a center outside it would waste draws. |
| Optimise in standardised inputs, Adam 0.02, up to 1000 steps, stop at `L = 0` | Stepping in raw units makes one learning rate mean half the range of a 0.04-wide parameter and nothing for a 300-wide one. |
| Only while some bound is above stopping | Narrowing is the whole cost. After the last bound reaches stopping, centers are IMABC's high-weight points, whose job is effective sample size, not location. |
| Keep `c_k` if `L` did not fall | Never worse than classic IMABC. |
| A refined center is a new draw | It has no simulation yet, so it is simulated with the batch rather than through the re-simulated-center path. |

**No trust region initially.** Of the two risks, drifting to where the emulator is wrong is already blocked by the worst-member term and the hinge. The real risk is the K centers collapsing onto one point, which shrinks the mixture; its symptom is a low effective sample size once the bounds reach stopping, and IMABC already answers that with its high-weight-center branch. Collapse would move work from narrowing to growing the sample, not break the posterior. If a constraint does turn out to be needed, the right name is a trust region, and the better form bounds ensemble disagreement, which measures reliability directly, rather than raw distance. `tmp_test_opt.py` and `tmp_test_opt.R` already implement this objective including a `--trust` displacement penalty, so the port is mostly translation.

**Hook:** `R/imabc.R:1124` (`center_next`), implemented in `R/neural_centers.R`. Test inside replace mode through the development option first, where it is cheap.

**Acceptance:** report iterations to all-bounds-at-stopping; iterations from there to the target effective sample size; the 5-seed score; how far centers moved, in prior-SD units; and the minimum pairwise distance between refined centers. Run at 100k, 50k and 25k rows, where replace alone scored 99.4, 97.2 and 96.0. Make refinement the default inside replace only if it cuts iterations without costing score.

### Milestone 6 — `emulator = "guide"`, the iteratively-trained mode

The model runs every iteration, as in classic IMABC, and the emulator is refit after each batch and used only to move the centers. It trades the replace mode's saving for keeping every accepted draw a real simulation.

| Requirement | Default | Why |
|---|---|---|
| Refit when | after every `target_fun` batch | One refit (about a minute) is negligible beside a real batch of about 50,000 simulations. |
| Refit how | warm start, 20 epochs on the whole store, **scalers frozen** | Cheaper than retraining, and training on the whole store avoids forgetting. Moving scalers would invalidate the warm-started weights, which is the one place "frozen" is a real requirement. |
| Holdout | 10% of the newest batch, error reported in bound half-widths | The newest batch is where the next centers go, so that is the accuracy that matters. |
| Oversampling the newest batch | none | Batches are about 50,000 rows; the heavy upweighting in the optimisation track was for 35 points. |
| Store cap | 500k rows: newest batches whole, older rows subsampled | A production-length run is 2.1M rows, about 2.2 GB at 101 targets. |

**Implementation:** add `"guide"` to the `match.arg`; the backend closure gains a refit branch and a capped store in `R/neural_store.R`. No new hook beyond milestone 5's.

**Acceptance:** `target_fun` calls to convergence, `"none"` against `"guide"`, plus effective sample size and agreement of the posterior means. Use the frozen full-LHS ensemble plus the measured per-target noise (`output/tables/noise_floor.csv`) as a cheap stochastic stand-in model, so the guide emulator sees only what IMABC simulated. Run real CRC-SPIN only if the saving is large: one real iteration is about 50,000 simulations.

### Milestone 7 — exposure review

Decide, from milestones 4-6, which `neural_config()` settings become documented `imabc()` arguments and which stay internal.

## 9. Code map

| File | Responsibility | Status |
|---|---|---|
| `R/neural_emulator.R` | settings, the network, training a member, training the ensemble in parallel, prediction, save and load | done |
| `R/neural_backend.R` | run state, the backend closure that wraps the model backend, training on the first batch, what the run reports | done |
| `R/imabc.R` | `emulator` argument, its documentation, and 6 lines of dispatch | done |
| `tests/testthat/test-neural-emulator.R` | the emulator against a known function, including every NA setting and the `data.frame` trap | done |
| `tests/testthat/test-neural-replace.R` | the mode end to end: convergence, model-call count, backend composition, output shape, saving, both refusals | done |
| `R/neural_centers.R` | center refinement | milestone 5 |
| `R/neural_store.R` | the training store and refits for guide mode | milestone 6 |

The dispatch in `R/imabc.R` in full:

```r
emulator <- match.arg(emulator)
if (emulator != "none") {
  neural <- neural_init(emulator, backend_fun, previous_results_dir, verbose)
  backend_fun <- neural_backend(neural)
}
...
if (emulator != "none") { out_list$neural <- neural_result(neural, output_directory, output_tag) }
```

## 10. Open questions

1. Is the development-only `options(imabc.neural_config = ...)` override acceptable, or should the experiments edit `neural_config()` directly?
2. The emulator inside `imabc` with `torch` in `Suggests` (what is built), or a companion package?
3. Upstream path: pull requests into `c-rutter/imabc` CurrentDev, or a fork until the mode is proven on CRC-SPIN?
4. In replace mode the emulator can only be as good as the first iteration's targets. Should the documentation advise users with an early-exit model to switch it off for the first iteration, or is the `na` setting enough? Milestone 4 measures this.
