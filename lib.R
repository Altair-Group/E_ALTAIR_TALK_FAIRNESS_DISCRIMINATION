options(install.packages.ask = FALSE)

if(.Platform[["OS.type"]]=="windows"){
  Sys.setenv(RENV_DOWNLOAD_MEHOD="wininet")
} else if(.Platform[["OS.type"]]=="unix"){
  Sys.setenv(RENV_DOWNLOAD_MEHOD="libcurl")
}


# R Repo
primary_repo <- "https://cloud.r-project.org/"
secondary_repo <- "https://cran.rstudio.com"
tertiary_repo <- "https://biol.univ-lyon1.fr/CRAN/"

if (interactive()) {
  restore_renv <- askYesNo("Do you want to restore Renv? (Suggestion: Yes if it's your first time)")
}
restore_renv=F
if (restore_renv == TRUE) {
  tryCatch({
    # Primary repo attempt
    renv::restore(repos = primary_repo, prompt = FALSE, clean = TRUE, rebuild = TRUE)
    cat("Renv successfully restored the library for the project using the renv.lock JSON file.")
  }, error = function(e) {
    cat("Error in downloading from primary repo. Switching to secondary repo.")
    tryCatch({
      # Secondary repo attempt
      renv::restore(repos = secondary_repo, prompt = FALSE, clean = TRUE, rebuild = TRUE)
      cat("Renv successfully restored the library for the project using the renv.lock JSON file.")
    }, error = function(e) {
      cat("Error in downloading from secondary repo. Switching to tertiary repo.")
      tryCatch({
        renv::restore(repos = tertiary_repo, prompt = FALSE, clean = TRUE, rebuild = TRUE)
        cat("Renv successfully restored the library for the project using the renv.lock JSON file.")
      }, error = function(e) {
        cat("All restoration attempts failed.")
      })
    })
  })
}



# R Libraries
lib <- c(
  "gbm", "xts", "sp", "zoo", "lattice", "caret", "ggplot2", "mgcv",
  "dplyr", "ggthemes", "locfit", "parallel", "sqldf", "ineq"
)

for (libr in lib) {
  suppressWarnings({
    if (!requireNamespace(libr, quietly = TRUE)) {
      renv::install(libr,prompt = FALSE, ask = FALSE)
    }
    library(libr, character.only = TRUE)  # Load library
  })
}