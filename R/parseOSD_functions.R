# parseOSD functions by dylan beaudette

### moved all OSD HTML|TXT|JSON file getting to soilDB

######## OSD parsing ########

## Note: results do not contain the series name as a column, as in the old parseOSD

#' Prepare Site and Horizon _data.frame_ from a `validateOSD()` result
#' @param x a _list_ result of SoilKnowledgeBase::validateOSD()
#' @keywords internal
#' @noRd
.doParseOSD <- function(x, logfile = "OSD.log", filename = "FOO.txt") {
  # # get data
  # res <- soilDB:::.getLocalOSD(x, path)
  #
  # # init section REGEX: critical for locating brief narrative
  # .setSectionREGEX(x)
  #
  # # extract sections
  # l[['sections']] <- .extractSections(res)
  # l[['section-indices']] <- .findSectionIndices(res)

  message(filename)
  l <- list()
  l[['site-data']] <- .extractSiteData(x, logfile, filename)
  tp <- strsplit(as.character(x$`TYPICAL PEDON`$content), "\n")
  if (length(tp) > 0)
    l[['hz-data']] <- .extractHzData(tp[[1]], logfile, filename)
  else
    l[['hz-data']] <- data.frame(name = NA)

  # RIC extraction
  ric_content <- x$`RANGE IN CHARACTERISTICS`$content
  if (!is.null(ric_content) && is.character(ric_content) && nzchar(ric_content)) {
    tp_header <- if (length(tp) > 0) tp[[1]][1] else ""
    default_cs <- .extractRICDefaultColorState(tp_header)
    l[['ric-data']] <- try(.extractRICData(ric_content, default_cs), silent = TRUE)
    if (inherits(l[['ric-data']], 'try-error')) l[['ric-data']] <- list()
  } else {
    l[['ric-data']] <- list()
  }

  return(l)
}


## safely (efficiently?) find specific classes within a vector of narratives
# needle: class labels
# haystack: narrative by horizon
#' @importFrom stringi stri_detect_fixed
.findClass <- function(needle, haystack) {
  # iterate over vector of horizon narratives, searching for exact matches
  test.by.hz <- lapply(haystack, stringi::stri_detect_fixed, pattern = needle, opts_fixed = list(case_insensitive = TRUE))

  # iterate over search results by horizon, keeping names of matching classes
  matches <- lapply(test.by.hz, function(i) {
    needle[i]
  })

  # compute number of characters: longer matches are the most specific / correct
  res <- sapply(matches, function(i) {

    # find the longest matching string
    idx <- which.max(nchar(i))

    # extract it
    m <- i[idx]

    # convert _nothing_ into NA
    if(length(m) > 0) {
      return(m)
    } else {
      return(NA)
    }

  })

  return(res)
}


# vectorized parsing of texture class from OSD
.parse_texture <- function(text) {
  # mineral texture classes, sorted from coarse -> fine
  textures <- c('coarse sand', 'sand', 'fine sand', 'very fine sand', 'loamy coarse sand', 'loamy sand', 'loamy fine sand', 'loamy very fine sand', 'coarse sandy loam', 'sandy loam', 'fine sandy loam', 'very fine sandy loam', 'loam', 'silt loam', 'silt', 'sandy clay loam', 'clay loam', 'silty clay loam', 'sandy clay', 'silty clay', 'clay')

  ## 2019-05-29: generalized for all non-greedy, exact matching, longest string
  m <- .findClass(needle = textures, haystack = text)
  m <- tolower(m)

  # convert to ordered factor
  #
  # m <- factor(m, levels = textures, ordered = TRUE)
  #
  # factors cannot be preserved in JSON output, and wont work for multiple classes/ranges of classes

  return(m)
}


# vectorized parsing of horizon boundary
#' @importFrom stringi stri_match
.parse_hz_boundary <- function(text) {

  distinctness <- c('very abrupt', 'abrupt', 'clear', 'gradual', 'diffuse')
  topography <- c('smooth', 'wavy', 'irregular', 'broken')
  bdy <- apply(expand.grid(distinctness, topography), 1, paste, collapse = ' ')


  ## TODO: this is too greedy ?
  # https://github.com/dylanbeaudette/parse-osd/issues/10

  # combine into capturing REGEX
  bdy.regex <- paste0('(', paste(bdy, collapse='|'), ') boundary')

  # get matches
  m <- stringi::stri_match(text, regex = bdy.regex, mode = 'first', opts_regex = list(case_insensitive = TRUE))

  # fail gracefully in the case of no section data or no matches
  if(nrow(m) < 1)
    return(NA)

  # keep only matches and convert to lower case
  m <- tolower(m[, 2])

  # split into pieces
  res <- data.frame(
    distinctness = .findClass(needle = distinctness, haystack = m),
    topography = .findClass(needle = topography, haystack = m),
    stringsAsFactors = FALSE
  )

  return(res)
}


# vectorized parsing of coarse fraction qty+class from OSD
#' @importFrom stringi stri_match
.parse_CF <- function(text) {
  cf.type <- c('gravelly', 'cobbly', 'stony', 'bouldery', 'channery', 'flaggy')
  cf.qty <- c('', 'very ', 'extremely ')
  cf <- apply(expand.grid(cf.qty, cf.type), 1, paste, collapse = '')


  ## TODO: this is too greedy as 'fine sand' will be found _within_ 'fine sandy loam'
  # https://github.com/dylanbeaudette/parse-osd/issues/10

  # combine into capturing REGEX
  cf.regex <- paste0('(', paste(cf, collapse = '|'), ')')

  # get matches
  m <- stringi::stri_match(text, regex = cf.regex, mode = 'first', opts_regex = list(case_insensitive = TRUE))

  # fail gracefully in the case of no section data or no matches
  if(nrow(m) < 1)
    return(NA)

  # keep only matches and convert to lower case
  m <- tolower(m[, 2])

  return(m)
}

# vectorized parsing of pH
#' @importFrom stringi stri_match
.parse_pH <- function(text) {

  # combine into capturing REGEX
  ph.regex <- '\\(ph\\s?([0-9]\\.?[0-9]?)\\)'

  # get matches
  m <- stringi::stri_match(text, regex = ph.regex, mode = 'first', opts_regex = list(case_insensitive = TRUE))

  # fail gracefully in the case of no section data or no matches
  if(nrow(m) < 1)
    return(NA)

  # keep only matches
  m <- as.numeric(m[, 2])

  return(m)
}


# vectorized parsing of pH class
#' @importFrom stringi stri_match
.parse_pH_class <- function(text) {

  # reaction classes
  pH_classes <- c('ultra acid', 'extremely acid', 'very strongly acid', 'strongly acid', 'moderately acid', 'slightly acid', 'neutral', 'slightly alkaline', 'mildly alkaline', 'moderately alkaline', 'strongly alkaline', 'very strongly alkaline')

  ## 2019-05-29: generalized for all non-greedy, exact matching
  m <- .findClass(needle = pH_classes, haystack = text)
  m <- tolower(m)

  # return as an ordered factor acidic -> basic
  # m <- factor(m, levels = pH_classes, ordered = TRUE)
  # factors cannot be preserved in JSON output, and wont work for multiple classes/ranges of classes

  return(m)
}

# vectorized parsing of effervescence class
.parse_eff_class <- function(x) {
  .zerochar_to_na(gsub("^.*[;,]? ?\\b(very [a-z]+ effervescen[tce]+ (to|and|or) [a-z ]+ ?effervescen[tce]+).*$|^.*[;,]? ?\\b([a-z]+ ?effervescen[tce]+ (to|and|or) [a-z]+ ?effervescen[tce]+).*$|^.*[;,] ?\\b(very [a-z]+ effervescen[tce]+).*$|^.*[;,] ?\\b([a-z]+ ?effervescen[tce]+).*$|^.*[;,]? ?\\b(very [a-z]+ effervescen[tce]+).*$|^.*[;,]? ?\\b([a-z]+ ?effervescen[tce]+).*$|.*",
                                           "\\1\\3\\5\\6\\7\\8", x, ignore.case = TRUE))
  # factors cannot be preserved in JSON output, and wont work for multiple classes/ranges of classes
}

# vectorized parsing of drainage class
#' @importFrom stringi stri_match
.parse_drainage_class <- function(text) {

  # drainage classes, in order, lower case
  classes <- c("excessively", "somewhat excessively", "well", "moderately well",
               "somewhat poorly", "poorly", "very poorly", "subaqueous")
  class_hyphen <- gsub(" ", "[ \\-]", classes)

  # combine into capturing REGEX
  classes.regex <- paste0('(', paste(class_hyphen, collapse = '|'), ')', "([ \\-]drained)?( (to|or|and) )?",
                          paste0('(', paste(class_hyphen, collapse = '|'), ')'),
                          "?[ \\-]drained|subaqueous|Drainage[ class]*[:\\-]+ ",
                          '(', paste(class_hyphen, collapse = '|'), ')', "([ \\-]drained)?( (to|or|and) )?",
                          paste0('(', paste(class_hyphen, collapse = '|'), ')?'))

  # get matches
  m <- stringi::stri_match(text, regex = classes.regex, mode = 'first', opts_regex = list(case_insensitive = TRUE))
  m <- gsub("Drainage[ Cclass]*[:\\-]+ ", "", m, ignore.case = TRUE)

  # fail gracefully in the case of no section data or no matches
  if (nrow(m) < 1) {
    return(NA)
  }

  # keep full match and convert to lower case, remove the word "drained"
  m <- trimws(gsub("  ", " ", gsub("-", " ", gsub("drained", "", tolower(m[, 1])))))

  # put classes in order from excessively->subaqueous
  # interpolate ranges across more than 2 classes, and concatenate with comma
  m2 <- strsplit(m, "\\b(and|or|to)\\b")
  m3 <- lapply(m2, function(x) {
    x <- trimws(x)
    y <- as.integer(factor(unique(classes[match(x, classes)]),
                           levels = classes, ordered = TRUE))
    if (length(y) > 1) {
      y <- seq(from = min(y, na.rm = TRUE), to = max(y, na.rm = TRUE))
    }
    ifelse(is.na(classes[y]), "", classes[y]) # TODO: use zero chars or NA?
  })

  return(sapply(m3, paste0, collapse = ", "))
}

.zerochar_to_na <- function(x) {
  x <- trimws(x)
  if (length(x) == 0) {
    return(NA)
  }
  x[nchar(x) == 0] <- NA
  x
}

.parse_structure <- function(x) {
  .zerochar_to_na(gsub(".*(weak|moderate|strong) (very fine|very thin|fine|thin|medium|coarse|thick|very coarse|very thick|extremely coarse) (.*) structure.*|.*(massive).*|.*(single grain).*|.*", "\\1 \\2 \\3\\4\\5", x, ignore.case = TRUE))
}

.parse_rupture_dry <- function(x) {
  .zerochar_to_na(gsub(".*(loose|soft|slightly hard|moderately hard|hard|very hard|extremely hard|rigid|very rigid).*|.*", "\\1", x, ignore.case = TRUE))
}

.parse_rupture_moist <- function(x) {
  .zerochar_to_na(gsub(".*(loose|very friable|friable|firm|very firm|extremely firm|slightly rigid|rigid|very rigid).*|.*", "\\1", x, ignore.case = TRUE))
}

.parse_rupture_cem <- function(x) {
  .zerochar_to_na(gsub(".*(non|extremely weakly|very weakly|weakly|moderately|strongly|very strongly) (cemented|coherent).*|.*(indurated).*|.*", "\\1 \\2\\3", x, ignore.case = TRUE))
}

######## extract SPC-style data.frames ########

# parse important pieces from sections
# x: list of section chunks
.extractSiteData <- function(x, logfile = "OSD.log", filename = "FOO.txt") {

  ## drainage class

  # the standard place to report drainage class is in drainage and permeability
  drainage.class1 <- .parse_drainage_class(x[['DRAINAGE AND PERMEABILITY']]$content)
  if (is.na(drainage.class1)) {
    drainage.class1 <- ""
  }

  # use OVERVIEW for SSR1 updated OSD format (that removes drainage from drainage and permeability???)
  #       https://casoilresource.lawr.ucdavis.edu/sde/?series=bordengulch

  # several OSDs specify different drainage classes in OVERVIEW v.s. DRAINAGE AND PERMEABILITY sections
  #       also some OSDs specify a range of drainage classes in one or both sections
  drainage.class2 <- .parse_drainage_class(x[['OVERVIEW']])
  if (is.na(drainage.class2)) {
    drainage.class2 <- ""
  }

  ## TODO: other things?

  # - drainage is the standard value derived from DRAINAGE AND PERMEABILITY
  # - drainage_overview is parsed from the brief description at top of OSD (non-standard)

  # if both present, they should match; flag mismatches for update
  r <- data.frame(drainage = drainage.class1, drainage_overview = drainage.class2)
  return(r)
}



#' @importFrom stringi stri_match_all
.extractHzData <- function(tp, logfile = "OSD.log", filename = "FOO.txt") {

  # detect horizons with both top and bottom depths
  hz.rule <- "([\\^\\'\\\"`\\/a-zA-Z0-9]+(?: and [\\^\\'\\\"`\\/a-zA-Z0-9]+)?)\\s*[-=\u2014]+\\s*([Ol0-9.]+)\\s*?([toTO0\\-]+)?\\s+?([Ol0-9.]+)\\s*?(inche?s?|in|cm|centimeters?)"

  # detect horizons with no bottom depth
  hz.rule.no.bottom <- "([\\^\\'\\\"`\\/a-zA-Z0-9]+(?: and [\\^\\'\\\"`\\/a-zA-Z0-9]+)?)\\s*[-=\u2014]+?\\s*([Ol0-9./ ]+)\\s*(inche?s?|in|cm|centimeters?)?\\s*([toTO0\\-]+)?\\s*([Ol0-9./ ]+)?\\s*(inche?s?|in|cm|centimeters?)?"

  ## default encoding of colors: Toggle dry/moist assumption
  ##
  ## Profile-level statement: Colors are for dry soil unless otherwise stated | Colors are for moist soil unless otherwise stated
  ##
  ## Examples:
  ## moist:
  ##   E1--7 to 12 inches; very dark gray (10YR 3/1) silt loam, 50 percent gray (10YR 5/1) and 50 percent gray (10YR 6/1) dry; moderate thin platy structure parting to weak thin platy; friable, soft; common fine and medium roots throughout; common fine tubular pores; few fine distinct dark yellowish brown (10YR 4/6) friable masses of iron accumulations with sharp boundaries on faces of peds; strongly acid; clear wavy boundary.
  ##
  ## dry:
  ##   A--0 to 6 inches; light gray (10YR 7/2) loam, dark grayish brown (10YR 4/2) moist; moderate coarse subangular blocky structure; slightly hard, friable, slightly sticky and slightly plastic; many very fine roots; many very fine and few fine tubular and many very fine interstitial pores; 10 percent pebbles; strongly acid (pH 5.1); clear wavy boundary. (1 to 8 inches thick)
  ##
  dry.is.default <- length(grep('for[ athe]+(?:air-* *)?dr[yied]+[ \\n,]+(colors|soil|conditions)', tp, ignore.case = TRUE)) > 0
  moist.is.default <- length(grep('for[ athe]+(wet|moi*st)[ \\n,]+(rubbed|crushed|broken|interior|soil|conditions)', tp, ignore.case = TRUE)) > 0

  if (dry.is.default)
    default.moisture.state <- 'dry'
  if (moist.is.default)
    default.moisture.state <- 'moist'

  # if neither are specified assume moist conditions
  if ((!dry.is.default & !moist.is.default))
    default.moisture.state <- 'moist'

  # if both are specified (?)
  if (dry.is.default & moist.is.default)
    default.moisture.state <- 'unknown'

  ## TODO: account for l,O style OCR errors
  # https://github.com/ncss-tech/SoilKnowledgeBase/issues/53

  # single rule, with dry/moist state
  # note that dry/moist may not always be present
  color.rule <- "\\(([Ol0-9]?[\\.]?[Ol0-9]?[B|G|Y|R|N]+) *([Ol0-9\\.]+) */([Ol0-9]*) *\\)\\s?(dry|moist|)"

  # eliminate empty lines within typical pedon
  tp <- tp[nzchar(trimws(tp))]

  # ID starting lines of horizon information
  hz.idx <- sort(unique(c(grep(hz.rule, tp), grep(hz.rule.no.bottom, tp))))

  # the first line of the TYPICAL PEDON section should not appear in this index
  first.line.flag <- which(hz.idx == 1)
  if (length(first.line.flag) > 0) {
    hz.idx <- hz.idx[-first.line.flag]
  }

  check.multiline <- diff(hz.idx)
  if (any(check.multiline > 1)) {
    # multiline typical pedon horizon formatting (needs fix)
    logmsg(logfile, paste0("CHECK MULTILINE TYPICAL PEDON: ", filename, " [number of multilines=", sum(check.multiline > 1), "]"))
  }

  if (length(hz.idx) > 0 ) {
    # concatenate multiline entries in typical pedon
    multiline.rle <- list(lengths = c(check.multiline, 1),
                          values = hz.idx)
    class(multiline.rle) <- "rle"
    grp <- inverse.rle(multiline.rle)
    tpclean <- sapply(split(tp[min(grp):max(grp)], grp), paste, collapse = " ")
  } else {
    tpclean <- tp[0]
  }

  # init empty lists to store hz data and colors
  hz.data <- list()
  dry.colors <- list()
  moist.colors <- list()
  narrative.data <- list()

  # iterate over identified horizons, extracting hz parts
  for (i in seq_along(tpclean)) {
    this.chunk <- tpclean[i]

    # parse hz designations and depths, keep first match
    # first try to find horizons with top AND bottom depths
    h <- stringi::stri_match(this.chunk, regex = hz.rule)

    # if none, then try searching for only top depths
    if (all(is.na(h))) {
      # this won't have the correct number of elements, adjust manually
      h <- trimws(stringi::stri_match(this.chunk, regex = hz.rule.no.bottom))
      h[2] <- gsub("0", "O", h[2], fixed=TRUE)
      h[6] <- gsub("l", "1", h[6], fixed=TRUE)
      h <- gsub(" *3/4", ".75", h)
      h <- gsub(" *[1l]/2", ".5", h)
      h <- gsub(" *[1l]/[48]", ".25", h) # NB: fudging 1/8 inch -> 1 cm
      h <- gsub("^\\.", "0.", h)
      h <- gsub("l", "1", h)
      i_num <- grep("^\\d+\\.*\\d*$", h)
      # fill missing depth with NA
      if (length(i_num) == 1) {
        i_num <- c(i_num, NA)
      }
      h_num <- h[i_num]
      l_alp <- grepl("[A-Za-z]", h)
      h_alp <- h[l_alp & h != "to" & h != "-"][2:3]
      h <- c(h_alp[1], h_num, h_alp[2])

    } else {
      h[2] <- gsub("0", "O", h[2], fixed=TRUE)
      h[c(3,5)] <- gsub("l", "1", h[c(3,5)], fixed=TRUE)
      h <- h[c(2:3,5:6)]
    }

    # apply a filter so horizon data with no horizon designation skip
    if (!is.na(h[1]) && grepl("[OABCDELMRVWbcxw]", h[1])) {
      # save hz data to list
      hz.data[[i]] <- h

      # save narrative to list
      narrative.data[[i]] <- this.chunk

      ## TODO: test this!
      # parse ALL colors, result is a multi-row matrix, 5th column is moisture state
      colors <- stringi::stri_match_all(this.chunk, regex = color.rule)[[1]]

      # replace missing moisture state with (parsed) default value
      colors[, 5][which(colors[, 5] == '')] <- default.moisture.state

      # extract dry|moist colors, note that there may be >1 color per state
      dc <- colors[which(colors[, 5] == 'dry'), 1:4, drop = FALSE]
      mc <- colors[which(colors[, 5] == 'moist'), 1:4, drop = FALSE]

      # there there was at least 1 match, keep the first 1
      if (nrow(dc) > 0) {
        dry.colors[[i]] <- dc[1, ]
      } else dry.colors[[i]] <- matrix(rep(NA, times = 4), nrow = 1)

      if (nrow(mc) > 0)
        moist.colors[[i]] <- mc[1, ]
      else moist.colors[[i]] <- matrix(rep(NA, times = 4), nrow = 1)
    } else {
      hz.data[[i]] <- NULL
      narrative.data[[i]] <- NULL
      dry.colors[[i]] <- NULL
      moist.colors[[i]] <- NULL
    }

  }

  # test for no parsed data, must be some funky formatting...
  if (length(hz.data) == 0)
    return(NULL)

  # convert to DF
  hz.data <- as.data.frame(do.call('rbind', hz.data))
  if (ncol(hz.data) != 4)
    return(NULL)
  dry.colors <- as.data.frame(do.call('rbind', dry.colors))[2:4]
  moist.colors <- as.data.frame(do.call('rbind', moist.colors))[2:4]
  narrative.data <- as.data.frame(do.call('rbind', narrative.data))

  names(hz.data) <- c('name', 'top', 'bottom', 'units')
  names(dry.colors) <- c('dry_hue', 'dry_value', 'dry_chroma')
  names(moist.colors) <- c('moist_hue', 'moist_value', 'moist_chroma')
  names(narrative.data) <- c('narrative')

  suppressWarnings({
    # cast to proper data types
    hz.data$top <- as.numeric(hz.data$top)
    hz.data$bottom <- as.numeric(hz.data$bottom)

    dry.colors$dry_value <- as.numeric(dry.colors$dry_value)
    dry.colors$dry_chroma <- as.numeric(dry.colors$dry_chroma)

    moist.colors$moist_value <- as.numeric(moist.colors$moist_value)
    moist.colors$moist_chroma <- as.numeric(moist.colors$moist_chroma)
  })

  # convert in -> cm using the first horizon
  if (!is.na(hz.data$units[1]) &&
      startsWith(tolower(hz.data$units[1]), "in")) {
    hz.data$top <- round(hz.data$top * 2.54)
    hz.data$bottom <- round(hz.data$bottom * 2.54)
  }

  # remove units column
  hz.data$units <- NULL

  # combine into single DF
  res <- cbind(hz.data, dry.colors, moist.colors)

  # parse out other elements from the narrative
  res$texture_class <- .parse_texture(narrative.data$narrative)
  res$structure <- .parse_structure(narrative.data$narrative)
  res$dry_rupture <- .parse_rupture_dry(narrative.data$narrative)
  res$moist_rupture <- .parse_rupture_moist(narrative.data$narrative)
  res$coherence <- .parse_rupture_cem(narrative.data$narrative)
  res$cf_class <- .parse_CF(narrative.data$narrative)
  res$pH <- .parse_pH(narrative.data$narrative)
  res$pH_class <- .parse_pH_class(narrative.data$narrative)
  res$eff_class <- .parse_eff_class(narrative.data$narrative)

  bdy <- .parse_hz_boundary(narrative.data$narrative)
  res$distinctness <- bdy$distinctness
  res$topography <- bdy$topography

  # add narrative
  res <- cbind(res, narrative.data)

  return(res)
}


