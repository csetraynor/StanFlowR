## Example usage of generate_nextflow() and launch_nextflow()
## from the StanFlowR package.

library(StanFlowR)



# ---------------------------------------------------------------------------
# Example 1: PopPK run with custom cluster options and overwrite enabled
# ---------------------------------------------------------------------------

generate_nextflow(
  outfile      = "Scripts_PKPD/run_pkpd_t1.nf",
  queue        = "high",
  memory       = "16 GB",
  time_limit   = "48h",
  label        = "PKPD",
  ncores       = 64L,
  module_load  = "OpenMPI/3.1.3-GCC-8.2.0-2.31.1:R/3.6.3-foss-2019a",
  n_chains     = 4L,
  methods      = "sample",
  initfile     = "Scripts/inits.R",
  model_names  = "T1_1cpt_fo",
  data_file    = "DerivedData/theophylline_stan_data_T1.json",
  tag_name     = "FINAL",
  iter         = 2000L,
  warmup       = 1000L,
  compile      = FALSE,
  source_pkg   = "StanFlowR/R",
  init_call    = "make_init(standatafile = datafile)",
  process_name = "pkpdRun",
  overwrite    = TRUE,
  additional_cluster_options = "--mem-per-cpu=250M"
)


# ---------------------------------------------------------------------------
# Example 3: Dry-run the Nextflow launch command (no actual execution)
# ---------------------------------------------------------------------------

launch_nextflow(
  nf_file  = "Scripts_SZ/run_mod27.nf",
  work_dir = "work/mod27",
  resume   = FALSE,
  dry_run  = TRUE
)

# To actually run (on a system with Nextflow installed):
# launch_nextflow(
#   nf_file  = "Scripts_SZ/run_mod27.nf",
#   work_dir = "work/mod27",
#   resume   = TRUE
# )
