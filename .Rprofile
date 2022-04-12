# options(renv.settings.ignored.packages(c("codetools","rmarkdown","tinytex")) )
renv::settings$ignored.packages(c("codetools","rmarkdown","tinytex"), persist = TRUE)

# options(renv.settings.use.cache(FALSE) )
# renv::settings$use.cache(FALSE, persist = T)

# Allow absolute module imports (relative to the app root).
if (file.exists("renv")) {
  source("renv/activate.R")
} else {
  # The `renv` directory is automatically skipped when deploying with rsconnect.
  message("No 'renv' directory found; renv won't be activated.")
}

options(box.path = getwd())
