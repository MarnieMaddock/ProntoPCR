#function to check for updates on github
check_for_updates <- function(repo = "MarnieMaddock/ProntoPCR") {
  # Get local version
  local_version <- as.character(utils::packageVersion("ProntoPCR"))
  
  # Get remote version from GitHub
  remote_version <- tryCatch(
    {
      desc_url <- paste0("https://raw.githubusercontent.com/", repo, "/main/DESCRIPTION")
      desc <- readLines(desc_url, warn = FALSE)
      remote_version <- gsub("Version: ", "", desc[grep("Version:", desc)])
      remote_version
    },
    error = function(e) NA
  )
  
  # Compare versions
  if (!is.na(remote_version) && remote_version != local_version) {
    message("A new version of ProntoPCR is available on GitHub.\n",
            "Run devtools::install_github('", repo, "') to update.")
  } else {
    message("You have the latest version of ProntoPCR (", local_version, ").")
  }
}
