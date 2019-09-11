unique_folderpath <- function(pathname, zip = F) {
  if (zip == F) {
    if (!file.exists(paste0("../NEON_Downloads/", pathname))) {
      return(pathname)
    }
    i <- 2
    repeat {
      path = paste0(pathname, "(", i, ")")
      if (!file.exists(paste0("../NEON_Downloads/", path))) {
        return(path)
      }
      i=i+1
    }
  } else if (zip == T) {
    if (!file.exists(paste0(pathname, ".zip"))) {
      return(pathname)
    }
    i <- 2
    repeat {
      path = paste0(pathname, "(", i, ")")
      if (!file.exists(paste0(path, ".zip"))) {
        return(path)
      }
      i=i+1
    } 
  }
}
