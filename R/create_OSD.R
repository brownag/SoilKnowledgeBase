# Create OSD data structures in inst/extdata

#' Create OSD Dataset
#'
#' @param ... not used
#' @param download _logical_. Download OSD Snapshot from OSDRegistry? Default: `TRUE`
#'
#' @return TRUE if successful
#' @export
create_OSD <- function(..., download = TRUE) {

  output_dir <- "inst/extdata/OSD"
  logfile <- file.path(output_dir, "OSD.log")

  logmsg(logfile, "Processing OSD data snapshot from OSDRegistry...")

  attempt <- try({

        if (download) {
          logmsg(logfile, "Downloading snapshot...")
          # Download OSDRegistry snapshot
          download_OSD(url = 'https://github.com/ncss-tech/OSDRegistry/releases/download/main/OSD-data-snapshot.zip')
        }

        # Do JSON parsing of sections
        res <- osd_to_json(logfile = logfile,
                           output_dir = output_dir)

        if (!all(res))
          logmsg(logfile, "ERROR: One or more OSDs failed to parse to JSON!")

        unlink("OSD", recursive = TRUE)

        # Optional: special scripts can be called from inst/scripts/OSD
        #  rpath <- list.files("inst/scripts/OSD/", ".*.R", full.names = TRUE)
        #
        # # Find each .R file (one or more for each part) and source them
        # lapply(rpath, function(filepath) {
        #   if (file.exists(filepath))
        #     source(filepath)
        # })

      })

  if (inherits(attempt, 'try-error'))
    return(FALSE)

  logmsg(logfile, "Done!")
  return(TRUE)
}

#' Internal method for downloading OSDRegistry snapshot
#'
#' @description This is an internal method for downloading the latest snapshot (or sample ZIP) containing Official Series Description (OSD) text files in an alphabetical folder structure. These files are un-zipped and processed into JSON which is stored in \code{"inst/extdata/OSD"}
#'
#' @details Default path to OSD data snapshot is an "artifact" created by the \code{refresh-osd} workflow on the \href{https://github.com/ncss-tech/OSDRegistry}{OSDRegistry} GitHub Repository.
#'
#' The latest version of the snapshot can be downloaded as a ZIP file from this URL: \url{https://github.com/ncss-tech/OSDRegistry/releases/download/main/OSD-data-snapshot.zip}.
#'
#' Note: This is a ZIP file within a ZIP file where the internal ZIP file has a unique (weekly) date stamp.
#'
#' @param url Path to OSD Data Snapshot
#' @param ... Additional arguments to `curl::curl_download()`
#' @return TRUE if successful, try-error if download or parsing fails
#' @importFrom utils unzip
#' @importFrom curl curl_download
download_OSD <- function(url = NULL, ...) {
  if (is.null(url))
    url <- 'https://github.com/ncss-tech/OSDRegistry/releases/download/main/OSD-data-snapshot.zip'
  curl::curl_download(url, "OSD.zip", handle = .SKB_curl_handle(), ...)
  unzip('OSD.zip')
  file.remove('OSD.zip')
  wkzip <- list.files(pattern = "OSD_.*zip")
  unzip(wkzip)
  file.remove(wkzip)
  return(TRUE)
}

#' Validate OSD using NSSH standards
#'
#' @param logfile Path to log file
#' @param filepath Path to a single plain text file containing OSD narrative
#'
#' @return A nested list containing OSD structure for specified file path
#' @export
#' @importFrom stringi stri_trans_general
validateOSD <- function(logfile, filepath) {

  if (!file.exists(filepath))
    return(FALSE)

  raw <- scan(file = filepath,
              what = character(),
              sep = "\n", quiet = TRUE)

  raw <- stringi::stri_trans_general(raw, "Latin-ASCII")

  raw <- trimws(raw[trimws(raw) != ""])

  ser.raw.idx <- grep("SERIES$", raw)[1]
  tax.raw.idx <- grep("TAXONOMIC CLASS[:]", raw)[1]
  brief.desc.idx <- which(1:length(raw) > ser.raw.idx &
                            1:length(raw) < tax.raw.idx)

  # capture any info between ALPHA SERIES and TAXONOMIC CLASS:
  if (length(brief.desc.idx) > 0) {
    brief.desc <- raw[brief.desc.idx]
    raw <- raw[-brief.desc.idx]
  } else {
    brief.desc <- NA
  }

  # this should be the last line in OSD
  raw.max.idx <- grep("^U\\. ?S\\. ?A\\.$", raw)

  if (length(raw.max.idx) == 0) {
    # if not present, take last line
    raw.max.idx <- length(raw)
  } else if (length(raw.max.idx) > 1) {
    # this shouldnt happen (but it does) -- duplicated OSD contents e.g. HEDVILLE
    logmsg(logfile, "DUPLICATE 'U.S.A.' END OF FILE MARKER: %s", filepath)
  }

  # handle only first instance where OSD is duplicated, remove NCSS/U.S.A lines
  raw <- raw[1:(raw.max.idx[1] - 1)]
  if (raw[length(raw)] == "National Cooperative Soil Survey") {
    raw <- raw[1:(length(raw) - 1)]
  }

  # TODO: abstract and generalize these into rules
  x <- trimws(raw[-grep("[A-Z '`][A-Z\\.'`]{2}[A-Z `']+.*|Ty[pic]+(al|fying)? ?[Pp]edon ?[:;\\-] ?.*|[A-Z]{3,}[:].*|\\(Colors are for", raw, invert = TRUE)])

  if (length(x) != length(unique(x))) {
    # x is all sorts of "headers" based on what the above pattern is allowed to match
    # filter to just things that look like headers that would confuse the OSD parser
    # sometimes this includes stuff in the RIC/REMARKS/ADDITIONAL DATA and may be "ok"
    # though even OSDCheck/formatting is confused by such things (see HTML)
    x.sub <- x[grepl("^[A-Z][A-Z \\(\\)]+:", x)]
    if (length(x.sub) > 0 && length(x.sub) != length(unique(x.sub))) {
      dh <- table(x.sub)
      logmsg(logfile, "CHECK DUPLICATION OF HEADERS: %s [%s]", filepath,
             paste0(names(dh)[which(dh > 1)], collapse = ","))
    }
  }

  loc.idx <- grep("^LOCATION", x)[1]
  ser.idx <- grep("SERIES$", x)[1]
  lst.idx <- grep("SERIES ESTABLISHED[:]|SERIES PROPOSED[:]|ESTABLISHED SERIES[:]|PROPOSED SERIES[:]", x)
  lst.idx <- lst.idx[length(lst.idx)]

  # allow the last section to be remarks, additional data or diagnostic horizons and other features recognized
  rem.idx <- grep("REMARKS[:]|ADDITIONAL DATA[:]|DIAGNOSTIC HORIZONS AND OTHER FEATURES RECOGNIZED[:]|TABULAR SERIES DATA[:]", x)
  rem.idx <- rem.idx[length(rem.idx)]

  if (length(rem.idx) == 0) {
    rem.idx <- lst.idx
  }

  # unable to locate location and series
  if (is.na(loc.idx) | is.na(ser.idx) | length(x) == 0) {
    logmsg(logfile, "CHECK LOCATION AND/OR SERIES: %s", filepath)
    return(FALSE)
  }

  if (length(rem.idx) == 0) {
    # these have some sort of malformed series status
    logmsg(logfile, "CHECK SECTION HEADINGS %s", filepath)
    rem.idx <- length(x)
  }

  # TODO: abstract and generalize these into rules
  markers <- trimws(gsub("^([A-Z`']{2}[A-Z ().`']+[A-Za-z)`']{2}) ?[:;] ?.*|(USE): .*|(TY[PIC]+AL +PEDON)[ \\-]+.*|^(Ty[pic]+(al|fying) +[Pp]edon) ?[;:\\-]+.*",
                         "\\1\\2\\3\\4",
                         x[(ser.idx + 1):rem.idx]))
  marker_self1 <- trimws(unlist(strsplit(gsub("LOCATION +([A-Z .`']+) {2,}\\d?([A-Z\\+]+)", "\\1;\\2",
                                              x[loc.idx]), ";")))
  marker_self2 <- trimws(gsub("([A-Z .`']) SERIES", "\\1", x[ser.idx]))

  if (marker_self1[1] != marker_self2) {
    logmsg(logfile, "CHECK LINE 1 LOCATION: %s", filepath)

    # TODO: abstract and generalize these into rules
    # three series in California have established dates before the state
    marker_self1[1] <- trimws(gsub("[0-9]|/", "", marker_self1[1]))
    marker_self1 <- trimws(unlist(strsplit(gsub("LOCATION +([A-Z .`']+) {2,}\\d?([A-Z\\+]+)", "\\1;\\2", marker_self1[1]), ";")))

    # inconsistent location and series
    if (marker_self1[1] != marker_self2)
      marker_self1[1] <- marker_self2
  }

  # TODO: abstract and generalize these into rules
  # for now, just check that at least one valid state code is used (dput(datasets::state.abb)); check these against metadata
  all_states <- c("AS","PB","PR","VI","HT","PW","FM", c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "FL", "GA",
                                                        "HI", "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA",
                                                        "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY",
                                                        "NC", "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX",
                                                        "UT", "VT", "VA", "WA", "WV", "WI", "WY"))
  n_states <- sum(unlist(lapply(all_states, function(x) sum(grepl(x, marker_self1[2])))))
  what_states <- paste0(unlist(sapply(all_states, function(x) x[grep(x, marker_self1[2])])), collapse = ",")

  if (n_states < 1) {
    # This is not invoked, given list of state and territory codes
    logmsg(logfile, "CHECK UNKNOWN STATE: %s", marker_self1[2])
  }

  # remove whole series name if used in header
  markheaders <- trimws(gsub(paste0("\\b", marker_self2, "\\b"), "", markers))

  # all section headers begin with capitals, and contain capitals up to the colon
  bad.idx <- grep("[a-z1-9]", markheaders)

  # has typical pedon
  typ.idx <- grep("ty[pic]+(al|fying) pedon", markheaders, ignore.case = TRUE)
  if (length(typ.idx) > 0) {
    if (any(typ.idx %in% bad.idx))
      bad.idx <- bad.idx[!(bad.idx %in% typ.idx)]
  }

  # TODO: abstract and generalize these into rules

  # these are non-canonical headers (with colons) that should be collapsed within RIC, REMARKS, etc
  bad.idx <- unique(c(bad.idx, grep("SAR|SLOPE|NAD83|MLRA[s(:]|NSTH 17|NOTES|NOTE", markheaders)))

  if (length(bad.idx) > 0) {
    nu <- markheaders[-bad.idx]
    tst <- unique(nu)

    if (length(tst) != length(nu)) {
      logmsg(logfile, "CHECK DUPLICATE STANDARD SECTIONS: %s [%s]", filepath,
             paste0(names(table(nu))[table(nu) > 1], collapse = ","))
    }

    markheaders <- tst
  }

  # dput(markheaders[markheaders != ""])

  # TODO: abstract and generalize these into rules

  headerpatterns <- c("TAXONOMIC CLASS",
                      "TY[PIC]+(AL|FYING)? ?PEDON|SOIL PROFILE|Soil Profile|Ty[pic]+(al|fying)? ?[Pp]edon|REFERENCE PEDON",
                      "TYPE LOCATION",
                      "RANGE IN CHARACTERISTICS|RANGE OF CHARACTERISTICS|RANGE OF INDIVIDUAL HORIZONS",
                      "COMPETING SERIES",
                      "GEOGRAPHICA?L? SETTINGS?|SETTING",
                      "ASSOCIATED SOILS",
                      "DRAINAGE AND (PERMEABILITY|SATURATED HYDRAULIC CONDUCTIVITY)|PERMEABILITY|DRAINAGE CLASS|DRAINAGE",
                      "USE AND VEGETATION|VEGETATION|USE",
                      "DISTRIBUTION AND EXTENT|DISTRIBUTION|EXTENT",
                      "SOIL SURVEY REGIONAL OFFICE",
                      "(SERIES )?(ESTABLISHED|PROPOSED)",
                      "REMARKS|DIAGNOSTIC HORIZONS AND OTHER FEATURES RECOGNIZED",
                      "ADDITIONAL DATA|TABULAR SERIES DATA")

  names(headerpatterns) <- c("TAXONOMIC CLASS",
                             "TYPICAL PEDON",
                             "TYPE LOCATION",
                             "RANGE IN CHARACTERISTICS",
                             "COMPETING SERIES",
                             "GEOGRAPHIC SETTING",
                             "GEOGRAPHICALLY ASSOCIATED SOILS",
                             "DRAINAGE AND PERMEABILITY",
                             "USE AND VEGETATION",
                             "DISTRIBUTION AND EXTENT",
                             "REGIONAL OFFICE",
                             "ORIGIN",
                             "REMARKS",
                             "ADDITIONAL DATA")

  headerorders <- sapply(1:length(headerpatterns), function(i) {
    j <- grep(headerpatterns[i], markheaders)
    if (length(j) == 0)
      return(NA)
    return(j)
  })

  rez <- do.call('list', lapply(1:length(headerorders), function(i) {

    parts <- headerorders[[i]]

    if (!all(grepl(headerpatterns[i], markheaders[parts]))) {
      return(list(section = names(headerpatterns)[i],
                  content = NA))
    }

    if (length(parts) > 0) {
      lpart <- lapply(seq_along(parts), function(pii) {
        p <- parts[pii]
        idx_start <- grep(pattern = markheaders[p], x = raw, fixed = TRUE)

        if (length(idx_start) > 1) {
          idx_start <- idx_start[pii]
        }

        if (length(idx_start) == 0 || is.na(idx_start)) {
          idx_start <- grep(pattern = markheaders[p - 1], x = raw, fixed = TRUE)
        }

        idx_next <- grep(markheaders[p + 1], raw, fixed = TRUE)
        tag_next <- sort(as.numeric(sapply(headerpatterns[i:length(headerpatterns)], function(xx) {
            y <- grep(xx, raw[idx_start:length(raw)], fixed = TRUE) + idx_start - 1
            return(y[length(y)])
          })))

        idx_new <- pmin(ifelse(tag_next[1] > idx_start, tag_next[1], length(raw)),
                        idx_next[which(idx_next > idx_start)[1]] - 1,
                        length(raw), na.rm = TRUE)[1]

        idx_stop <- ifelse(test = (i == length(headerpatterns)),
                           yes = length(raw),
                           no =  idx_new)

        # # : when and why do these get invoked?
        if (length(idx_start) == 0 | length(idx_stop) == 0) {
          logmsg(logfile, "DEBUG: idx_start/idx_stop have length 0")
          return(list(section = markheaders[p],
                      content = NA))
        }

        if (is.na(idx_start) | is.na(idx_stop)) {
          logmsg(logfile, "DEBUG: idx_start/idx_stop are NA")
          return(list(section = markheaders[p],
                      content = NA))
        }

        if (names(headerpatterns)[i] == "TAXONOMIC CLASS" &&
            idx_stop < idx_start) {
          idx_stop <- idx_start
        }

        return(list(section = markheaders[p],
                    content = paste0(raw[unique(idx_start:idx_stop)], collapse = "\n")))
      })

      # TODO: resolve duplication with unique() to hide exact duplicates (common c/p error)
      lpartc <- Map('c', lpart)

      if (length(lpartc) > 0)
        return(as.list(apply(do.call(rbind, lpartc), 2, paste, collapse = " & ")))
    }
  }))

  present_idx <- apply(sapply(headerpatterns, function(y) {
    grepl(y, sapply(rez, function(x) x$section))
  }), 2, which)


  names(rez) <- names(headerpatterns)
  rez2 <- c(list(SERIES = marker_self2,
                 STATUS = raw[loc.idx + 1],
                 BYREV = raw[loc.idx + 2],
                 REVDATE = raw[loc.idx + 3],
                 STATES = what_states,
                 OVERVIEW = paste0(brief.desc, collapse = "\n")),
            rez)

  rez2[is.na(names(rez2))] <- NULL

  return(rez2)
}

#' Convert OSD plaintext to JSON using NSSH structural elements
#'
#' @param logfile Path to log file; default: \code{file.path(output_dir, "OSD/OSD.log")}
#' @param input_dir Default: \code{'OSD'}; files matching pattern are listed recursively
#' @param pattern Argument passed to \code{list.files} when \code{osd_files} is not specified
#' @param output_dir Default: \code{'inst/extdata/OSD'}; folder to create alphabetical folder structure with JSON files
#' @param osd_files Default \code{NULL}; Optional over-ride vector of file names for testing
#'
#' @return A logical vector equal in length to the number of input files.
#' @export
#' @importFrom jsonlite toJSON
osd_to_json <- function(logfile = file.path(output_dir, "OSD.log"),
                        input_dir = 'OSD',
                        pattern = "txt",
                        output_dir = "inst/extdata/OSD",
                        osd_files = NULL) {

  if (!is.null(osd_files)) {
    all_osds <- osd_files
  } else {
    all_osds <- list.files(input_dir, pattern = pattern,
                           full.names = TRUE, recursive = TRUE)
  }

  res <- sapply(1:length(all_osds), function(i) {
    filepath <- all_osds[[i]]
    logmsg(logfile, " - %s", filepath)

    x <- validateOSD(logfile, filepath)

    parsed.OSD <- .doParseOSD(x, logfile, filepath)

    # SPC-style components from parseOSD returned as nested data.frames in JSON
    x$SITE <- I(list(parsed.OSD$`site-data`))
    x$HORIZONS <- I(list(parsed.OSD$`hz-data`))
    x$RIC <- parsed.OSD$`ric-data`

    if (is.logical(x))
      if (!x) return(FALSE)

    fld <- file.path(output_dir, substr(x$SERIES, 1, 1))

    if (!dir.exists(fld))
      dir.create(fld, showWarnings = FALSE, recursive = TRUE)

    fn <- gsub("\\.txt", "\\.json", basename(all_osds[[i]]))

    write(jsonlite::toJSON(x, pretty = TRUE, auto_unbox = TRUE, na = 'string'), file = file.path(fld, fn))
    return(TRUE)
  })
  names(res) <- all_osds
  return(res)
}
