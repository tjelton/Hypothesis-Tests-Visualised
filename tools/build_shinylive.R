#!/usr/bin/env Rscript
# Reproducible shinylive build for Hypothesis Tests Visualised.
#
# Automates the deterministic R-side of the export so the site (and therefore
# the cold-start load-time measurement) can be rebuilt with one command:
#
#     Rscript tools/build_shinylive.R
#
# Run it from the repository root. It:
#   1. Rebuilds the staging folder inst/app/R as a fresh copy of R/
#      (Shiny auto-sources <appdir>/R/*.R at startup, which is how the
#      shinylive build picks up every module + helper — NOT the package
#      namespace; DESCRIPTION/run_HTVP_app() are only for the package-install
#      distribution).
#   2. Runs shinylive::export(appdir = "inst/app", destdir = "site").
#
# The git / gh-pages publish step is intentionally NOT automated (branch
# switching is easy to get wrong by hand and risky to script). See CLAUDE.md
# "Deployment / migration mechanics" for the manual publish steps.
#
# NOTE: `site/` is rebuilt wholesale by shinylive::export (app.json,
# index.html, shinylive/, shinylive-sw.js). Never put hand-authored files
# (e.g. rewritten JS module folders) inside inst/app or site — inst/app/R is
# overwritten from R/, and site/ is regenerated. Commit such folders directly
# to the gh-pages branch in their own top-level dirs.

if (!requireNamespace("shinylive", quietly = TRUE)) {
  stop("The 'shinylive' package is required. Install it with install.packages('shinylive').",
       call. = FALSE)
}

app_dir    <- "inst/app"
staging    <- file.path(app_dir, "R")
source_dir <- "R"
dest_dir   <- "site"

if (!dir.exists(source_dir) || !file.exists(file.path(app_dir, "app.R"))) {
  stop("Run this script from the repository root (couldn't find R/ and inst/app/app.R).",
       call. = FALSE)
}

# 1. Fresh staging copy of R/ -> inst/app/R
if (dir.exists(staging)) unlink(staging, recursive = TRUE)
dir.create(staging, recursive = TRUE, showWarnings = FALSE)
copied <- file.copy(
  list.files(source_dir, full.names = TRUE, recursive = TRUE),
  staging, overwrite = TRUE, recursive = TRUE
)
message(sprintf("Staged %d R file(s) into %s", sum(copied), staging))

# 2. Export
message(sprintf("Exporting shinylive site to '%s' ...", dest_dir))
shinylive::export(appdir = app_dir, destdir = dest_dir)
message("Done. Next: follow the gh-pages publish steps in CLAUDE.md.")
