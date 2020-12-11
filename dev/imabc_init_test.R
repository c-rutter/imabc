rm(list = ls(all = TRUE))
gc()

# Load required libraries
devtools::document()
devtools::load_all()

library(data.table)
#.datatable.aware = TRUE
#assignInNamespace("cedta.pkgEvalsUserCode", c(data.table:::cedta.pkgEvalsUserCode,"imabc"), "data.table")

library(jsonlite)
library(lhs)
library(imabc)

library(doParallel)
library(truncnorm)

b_fun <- NULL

algo.params <- list(
    extras = list(priors.path = ""),
    imabc.args = list(
        targets = define_targets(
            d = add_target(
                target = 0.00001,
                starting_range = c(0, 2000),
                stopping_range = c(0, 4)
            ),
            h = add_target(
                target = 0.00001,
                starting_range = c(0, 2000),
                stopping_range = c(0, 6)
            )
        ),
        previous_results_dir = 'dev/outputs/out',
        N_start = 350,
        seed = 12345,
        latinHypercube = TRUE,
        N_centers = 6,
        Center_n = 25,
        N_post = 1000,
        max_iter = 12,
        N_cov_points = 50,
        sample_inflate = 2,
        verbose = TRUE,
        output_tag = "timestamp"
    )
)


# use modifyList to override items in algo.params$imabc.args
# imabc.args <- modifyList(algo.params$imabc.args, list(output_directory = turbine_output, priors = priors, backend_fun = b_fun))
imabc.args <- modifyList(algo.params$imabc.args, list(output_directory = "output/out2", backend_fun = b_fun))

# if ('previous_results' %in% names(imabc.args)) {
#   last_run <- read_previous_results(path = imabc.args$previous_results)
#   imabc.args$previous_results = last_run$previous_results
#   # imabc.args$targets = last_run$new_targets
#   # imabc.args$priors = last_run$new_priors
# }

#a <- imabc(priors=imabc.args$priors, output_directory=turbine_output, backend_fun=b_fun,
#  N_start=imabc.args$N_start, targets=imabc.args$targets, seed=imabc.args$seed, latinHypercube=imabc.args$latinHypercube,
#  N_centers=imabc.args$N_centers, Center_n=imabc.args$Center_n, N_post=imabc.args$N_post, max_iter=imabc.args$max_iter,
#  N_cov_points=imabc.args$N_cov_points, sample_inflate=imabc.args$sample_inflate, recalc_centers=imabc.args$recalc_centers,
#  verbose=imabc.args$verbose, output_tag=imabc.args$output_tag)

print(imabc.args$priors)
print(imabc.args$targets)

a <- do.call(imabc, imabc.args)

targets = imabc.args$targets
priors = imabc.args$priors
backend_fun = imabc.args$backend_fun
previous_results_dir = imabc.args$previous_results_dir
N_start = imabc.args$N_start
seed = imabc.args$seed
latinHypercube = imabc.args$latinHypercube
N_centers = imabc.args$N_centers
Center_n = imabc.args$Center_n
N_post = imabc.args$N_post
max_iter = imabc.args$max_iter
N_cov_points = imabc.args$N_cov_points
sample_inflate = imabc.args$sample_inflate
verbose = imabc.args$verbose
output_directory = imabc.args$output_directory
output_tag = imabc.args$output_tag

imabc(
    targets = imabc.args$targets,
    backend_fun = imabc.args$backend_fun,
    previous_results_dir = imabc.args$previous_results_dir,
    N_start = imabc.args$N_start,
    seed = imabc.args$seed,
    latinHypercube = imabc.args$latinHypercube,
    N_centers = imabc.args$N_centers,
    Center_n = imabc.args$Center_n,
    N_post = imabc.args$N_post,
    max_iter = imabc.args$max_iter,
    N_cov_points = imabc.args$N_cov_points,
    sample_inflate = imabc.args$sample_inflate,
    verbose = imabc.args$verbose,
    output_directory = imabc.args$output_directory,
    output_tag = imabc.args$output_tag
)



