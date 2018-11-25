unzipEddy <- function(site, path) {
  files <- list.files(pattern = "*zip")
  for (file in files) {
    date <- strsplit(file, "[.]")[[1]][7]
    unzip(zipfile = file, exdir = date)
    unlink(file)
    incProgress(1/length(files))
  }
  setwd('../../CyVerse-NEON-Browser')
}
