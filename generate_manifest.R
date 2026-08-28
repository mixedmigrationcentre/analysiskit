# Generate manifest.json for Posit Cloud / Connect / shinyapps.io
#
#   source("generate_manifest.R")
#
# Run this on the machine that holds your real library: the manifest records
# your R version and your installed package versions, so one generated
# elsewhere will not reproduce. Regenerate it whenever you add a package or
# change which files the app needs, and commit the result.
#
# If it stops with a list of missing packages, that list is the fix - see the
# deployment section of README.md for why the survey packages are involved.

source("R/setup_packages.R")
source("R/manifest.R")

ak_write_manifest()
