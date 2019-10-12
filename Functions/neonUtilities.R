# stackByTable function with support for progress bars. This is no longer used as it stopped working once
# new versions of neonUtilities came out
stackByTable <- function (filepath, savepath = filepath, folder = FALSE, saveUnzippedFiles = FALSE, 
          dpID = NA) 
{
  if (folder == FALSE) {
    files <- neonUtilities:::listFilesInZip(filepath)
    files <- files$Name[grep(files$Name, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")]
    if (length(files) == 0) {
      stop("Data files are not present in specified filepath.")
    }
  }
  if (folder == TRUE) {
    files <- list.files(filepath, pattern = "NEON.D[[:digit:]]{2}.[[:alpha:]]{4}.")
    if (length(files) == 0) {
      stop("Data files are not present in specified filepath.")
    }
  }
  dpID <- substr(files[1], 15, 27)
  package <- substr(files[1], nchar(files[1]) - 25, nchar(files[1]) - 
                      21)
  if (package == "anded") {
    package <- "expanded"
  }
  if (regexpr("DP[1-4]{1}.[0-9]{5}.001", dpID) != 1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001, where the first placeholder must be between 1 and 4.", 
               sep = " "))
  }
  if (substr(dpID, 5, 5) == "3") {
    stop("This is an AOP data product, files cannot be stacked. Use byFileAOP() if you would like to download data.")
  }
  if (dpID == "DP4.00200.001") {
    stop("This eddy covariance data product is in HDF5 format and cannot be stacked.")
  }
  if (dpID == "DP1.10017.001" && package != "basic") {
    saveUnzippedFiles = TRUE
    writeLines("Note: Digital hemispheric photos (in NEF format) cannot be stacked; only the CSV metadata files will be stacked.")
  }
  if (folder == FALSE) {
    savepath <- substr(filepath, 1, nchar(filepath) - 4)
    if (length(grep(files, pattern = ".zip")) > 0) {
      incProgress(amount = 0, detail = "Unpacking zip files")
      neonUtilities:::unzipZipfile(zippath = filepath, outpath = savepath, 
                   level = "all")
      incProgress(amount = 0.2)
    }
  }
  if (folder == TRUE) {
    if (is.na(savepath)) {
      savepath <- filepath
    }
    if (length(grep(files, pattern = ".zip")) > 0) {
      incProgress(amount = 0, detail = "Unpacking zip files")
      neonUtilities:::unzipZipfile(zippath = filepath, outpath = savepath, 
                   level = "in")
      incProgress(amount = 0.2)
    }
    else {
      if (length(grep(files, pattern = ".csv")) > 0) {
        for (i in files) {
          file.copy(paste(filepath, i, sep = "/"), savepath, 
                    recursive = T)
        }
      }
    }
  }
  stackDataFiles(savepath)
  if (saveUnzippedFiles == FALSE) {
    neonUtilities:::cleanUp(savepath)
  }
}

stackDataFiles <- function (folder) {
  starttime <- Sys.time()
  table_types <- neonUtilities:::table_types
  ttypes <- table_types
  filenames <- neonUtilities:::findDatatables(folder = folder, fnames = F)
  filepaths <- neonUtilities:::findDatatables(folder = folder, fnames = T)
  filelist <- stats::setNames(as.list(filepaths), filenames)
  datafls <- filelist
  if (length(datafls) == 0) {
    stop("No data files are present in specified file path.")
  }
  if (length(datafls) == 1) {
    if (dir.exists(paste0(folder, "/stackedFiles")) == F) {
      dir.create(paste0(folder, "/stackedFiles"))
    }
    file.copy(from = datafls[1][[1]], to = "/stackedFiles")
  }
  if (length(datafls) > 1) {
    if (dir.exists(paste0(folder, "/stackedFiles")) == F) {
      dir.create(paste0(folder, "/stackedFiles"))
    }
    tables <- neonUtilities:::findTablesUnique(names(datafls), ttypes)
    messages <- character()
    labTables <- tables[which(tables %in% table_types$tableName[which(table_types$tableType %in% 
                                                                        c("lab-current", "lab-all"))])]
    if (length(labTables) > 0) {
      labFilePaths <- character()
      labFiles <- character()
      for (m in 1:length(labTables)) {
        labFilePaths <- c(labFilePaths, filepaths[grep(labTables[m], 
                                                       filepaths)])
        labFiles <- c(labFiles, unique(filenames[grep(labTables[m], 
                                                      filenames)]))
      }
      if (length(labFiles) > 0) {
        for (l in 1:length(labFiles)) {
          file.copy(from = labFilePaths[grep(labFiles[l], 
                                             labFilePaths)][1], to = paste0(folder, "/stackedFiles/"))
          messages <- c(messages, paste("Copied the first available", 
                                        labFiles[l], "to /stackedFiles"))
        }
      }
    }
    varpath <- filepaths[grep("variables.20", filepaths)[1]]
    if (is.na(varpath)) {
      varpath <- filepaths[grep("variables", filepaths)[1]]
    }
    if (!is.na(varpath)) {
      variables <- neonUtilities:::getVariables(varpath)
      file.copy(from = varpath, to = paste0(folder, "/stackedFiles/variables.csv"))
      messages <- c(messages, "Copied the first available variable definition file to /stackedFiles and renamed as variables.csv")
    }
    valpath <- filepaths[grep("validation", filepaths)[1]]
    if (!is.na(valpath)) {
      file.copy(from = valpath, to = paste0(folder, "/stackedFiles/validation.csv"))
      messages <- c(messages, "Copied the first available validation file to /stackedFiles and renamed as validation.csv")
    }
    if (!is.na(varpath)) {
      tables <- c(tables, "variables")
    }
    if (!is.na(valpath)) {
      tables <- c(tables, "validation")
    }
    n <- 0
    for (i in 1:length(tables)) {
      tbltype <- ttypes$tableType[which(ttypes$tableName == 
                                          gsub(tables[i], pattern = "_pub", replacement = ""))]
      variables <- neonUtilities:::getVariables(varpath)
      if ((length(tbltype) == 0 && !(tables[i] %in% c("variables", 
                                                      "validation"))) || (length(tbltype) > 0 && tbltype == 
                                                                          "site-all")) {
        incProgress(amount = 0, detail = paste0("Stacking table ", tables[i]))
        writeLines(paste0("Stacking table ", tables[i]))
        pb <- utils::txtProgressBar(style = 3)
        utils::setTxtProgressBar(pb, 0)
        tblfls <- filepaths[grep(paste(".", tables[i], 
                                       sep = ""), filepaths, fixed = T)]
        tblnames <- filenames[grep(paste(".", tables[i], 
                                         sep = ""), filenames, fixed = T)]
        sites <- unique(substr(tblnames, 10, 13))
        sites <- sites[order(sites)]
        d <- suppressWarnings(data.table::fread(tblfls[grep(sites[1], 
                                                            tblfls)][1], header = T))
        d <- neonUtilities:::assignClasses(d, variables)
        d <- neonUtilities:::makePosColumns(d, tblnames[1])
        numRows <- nrow(d)
        utils::setTxtProgressBar(pb, 1/length(tblfls))
        if (length(sites) > 1) {
          for (j in 2:length(sites)) {
            sitefls <- tblfls[grep(sites[j], tblfls)]
            sitenames <- tblnames[grep(sites[j], tblnames)]
            d.next <- suppressWarnings(data.table::fread(sitefls[1], 
                                                         header = T))
            d.next <- neonUtilities:::assignClasses(d.next, variables)
            d.next <- neonUtilities:::makePosColumns(d.next, sitenames[1])
            numRows <- sum(numRows, nrow(d.next))
            d <- rbind(d, d.next, fill = TRUE)
            utils::setTxtProgressBar(pb, (i * j)/length(tblfls))
            incProgress(amount = 0.8/(length(tables)*(length(sites) - 1)))
          }
        }
        utils::write.csv(d, paste0(folder, "/stackedFiles/", 
                                   tables[i], ".csv"), row.names = F)
        messages <- c(messages, paste0("Stacked ", tables[i], 
                                       " which has ", numRows, " out of the expected ", 
                                       nrow(d), " rows (", (numRows/nrow(d)) * 100, 
                                       "%)."))
        n <- n + 1
        utils::setTxtProgressBar(pb, 1)
        close(pb)
      }
      if ((length(tbltype) == 0 && !(tables[i] %in% c("variables", 
                                                      "validation"))) || (length(tbltype) > 0 && tbltype == 
                                                                          "site-date")) {
        writeLines(paste0("Stacking table ", tables[i]))
        incProgress(amount = 0, detail = paste0("Stacking table ", tables[i]))
        pb <- utils::txtProgressBar(style = 3)
        utils::setTxtProgressBar(pb, 0)
        tblfls <- filepaths[grep(paste(".", tables[i], 
                                       sep = ""), filepaths, fixed = T)]
        tblnames <- filenames[grep(paste(".", tables[i], 
                                         sep = ""), filenames, fixed = T)]
        d <- suppressWarnings(data.table::fread(tblfls[1], 
                                                header = T))
        d <- neonUtilities:::assignClasses(d, variables)
        d <- neonUtilities:::makePosColumns(d, tblnames[1])
        numRows <- nrow(d)
        utils::setTxtProgressBar(pb, 1/length(tblfls))
        if (length(tblfls) > 1) {
          for (j in 2:length(tblfls)) {
            d.next <- suppressWarnings(data.table::fread(tblfls[j], 
                                                         header = T))
            d.next <- neonUtilities:::assignClasses(d.next, variables)
            d.next <- neonUtilities:::makePosColumns(d.next, tblnames[j])
            numRows <- sum(numRows, nrow(d.next))
            d <- rbind(d, d.next, fill = TRUE)
            utils::setTxtProgressBar(pb, (i * j)/length(tblfls))
            incProgress(amount = 0.8/(length(tables)*(length(tblfls) - 1)))
          }
        }
        utils::write.csv(d, paste0(folder, "/stackedFiles/", 
                                   tables[i], ".csv"), row.names = F)
        messages <- c(messages, paste0("Stacked ", tables[i], 
                                       " which has ", numRows, " out of the expected ", 
                                       nrow(d), " rows (", (numRows/nrow(d)) * 100, 
                                       "%)."))
        n <- n + 1
        utils::setTxtProgressBar(pb, 1)
        close(pb)
      }
    }
  }
  writeLines(paste("Finished: All of the data are stacked into", 
                   n, "tables!"))
  writeLines(paste0(messages, collapse = "\n"))
  endtime <- Sys.time()
  writeLines(paste0("Stacking took ", format((endtime - starttime), 
                                             units = "auto")))
}

byFileAOP <- function (dpID, site = "SJER", year = "2017", check.size = TRUE, 
                       savepath = NA) 
{
  if (regexpr("DP[1-4]{1}.[0-9]{5}.001", dpID) != 1) {
    stop(paste(dpID, "is not a properly formatted data product ID. The correct format is DP#.#####.001", 
               sep = " "))
  }
  productUrl <- paste0("http://data.neonscience.org/api/v0/products/", 
                       dpID)
  req <- httr::GET(productUrl)
  avail <- jsonlite::fromJSON(httr::content(req, as = "text"), 
                              simplifyDataFrame = TRUE, flatten = TRUE)
  if (!is.null(avail$error$status)) {
    stop(paste("No data found for product", dpID, sep = " "))
  }
  if (avail$data$productScienceTeamAbbr != "AOP") {
    stop(paste(dpID, "is not a remote sensing product. Use zipsByProduct()"))
  }
  month.urls <- unlist(avail$data$siteCodes$availableDataUrls)
  month.urls <- month.urls[grep(paste(site, year, sep = "/"), 
                                month.urls)]
  if (length(month.urls) == 0) {
    stop("There are no data at the selected site and year.")
  }
  incProgress(amount = 0, detail = "Querying NEON API")
  getFileUrls <- function(m.urls) {
    url.messages <- character()
    file.urls <- c(NA, NA, NA)
    for (i in 1:length(m.urls)) {
      tmp <- httr::GET(m.urls[i])
      tmp.files <- jsonlite::fromJSON(httr::content(tmp, 
                                                    as = "text"), simplifyDataFrame = T, flatten = T)
      if (length(tmp.files$data$files) == 0) {
        url.messages <- c(url.messages, paste("No files found for site", 
                                              tmp.files$data$siteCode, "and year", tmp.files$data$month, 
                                              sep = " "))
        next
      }
      file.urls <- rbind(file.urls, cbind(tmp.files$data$files$name, 
                                          tmp.files$data$files$url, tmp.files$data$files$size))
      file.urls <- data.frame(file.urls, row.names = NULL)
      colnames(file.urls) <- c("name", "URL", "size")
      file.urls$URL <- as.character(file.urls$URL)
      file.urls$name <- as.character(file.urls$name)
      if (length(url.messages) > 0) {
        writeLines(url.messages)
      }
      file.urls <- file.urls[-1, ]
      return(file.urls)
    }
  }
  file.urls.current <- getFileUrls(month.urls)
  downld.size <- sum(as.numeric(as.character(file.urls.current$size)), 
                     na.rm = T)
  downld.size.read <- gdata::humanReadable(downld.size, units = "auto", 
                                           standard = "SI")
  if (check.size == TRUE) {
    resp <- readline(paste("Continuing will download", nrow(file.urls.current), 
                           "files totaling approximately", downld.size.read, 
                           ". Do you want to proceed y/n: ", sep = " "))
    if (!(resp %in% c("y", "Y"))) {
      stop("Download halted.")
    }
  }
  if (is.na(savepath)) {
    filepath <- paste(getwd(), "/", dpID, sep = "")
  }
  else {
    filepath <- paste(savepath, "/", dpID, sep = "")
  }
  if (dir.exists(filepath) == F) 
    dir.create(filepath, showWarnings = F)
  j <- 1
  messages <- list()
  incProgress(amount = 0.1)
  while (j <= nrow(file.urls.current)) {
    incProgress(amount = 0, detail = paste0("Downloading file ", j, " of ", nrow(file.urls.current)))
    path1 <- strsplit(file.urls.current$URL[j], "\\?")[[1]][1]
    pathparts <- strsplit(path1, "\\/")
    path2 <- paste(pathparts[[1]][4:(length(pathparts[[1]]) - 
                                       1)], collapse = "/")
    newpath <- paste0(filepath, "/", path2)
    if (dir.exists(newpath) == F) 
      dir.create(newpath, recursive = T)
    t <- try(downloader::download(file.urls.current$URL[j], 
                                  paste(newpath, file.urls.current$name[j], sep = "/"), 
                                  mode = "wb"), silent = T)
    if (class(t) == "try-error") {
      writeLines("File could not be downloaded. URLs may have expired. Getting new URLs.")
      file.urls.new <- getFileUrls(month.urls)
      file.urls.current <- file.urls.new
      writeLines("Continuing downloads.")
    }
    if (class(t) != "try-error") {
      messages[j] <- paste(file.urls.current$name[j], "downloaded to", 
                           newpath, sep = " ")
      incProgress(amount = 1/nrow(file.urls.current))
      j = j + 1
    }
  }
  writeLines(paste("Successfully downloaded ", length(messages), 
                   " files."))
  writeLines(paste0(messages, collapse = "\n"))
}
