install_and_load <- function(packages) {
  # Identify which packages are not yet installed
  missing_packages <- packages[!packages %in% installed.packages()[, "Package"]]
  
  # Install missing packages if any
  if (length(missing_packages) > 0) {
    install.packages(missing_packages, dependencies = TRUE)
  }
  
  # Load all requested packages (returns logical vector of success)
  invisible(sapply(packages, require, character.only = TRUE))
}
