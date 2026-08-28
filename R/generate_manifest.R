# Moved.
#
# The runnable script is now generate_manifest.R in the PROJECT ROOT:
#
#   source("generate_manifest.R")
#
# It could not stay here. shiny::runApp() auto-sources every file in R/ when it
# starts an app directory, so this file ran on every launch and stopped the app
# before the interface appeared. The functions it used are in R/manifest.R,
# which is safe to auto-source because it only defines things.
#
# This file does nothing and can be deleted.
