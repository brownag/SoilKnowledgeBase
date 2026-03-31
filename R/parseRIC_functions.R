# parseRIC_functions.R — Range in Characteristics (RIC) parsing for OSD data
#
# Extracts structured property data from RIC sections of Official Series
# Descriptions. Produces nested list output with horizon-level and series-level
# properties including colors, textures, clay content, coarse fragments,
# structure, boundaries, reaction, pH, effervescence, consistence, and PSCS.
#
# Integration: Called from .doParseOSD() via .extractRICData()

# NULL-coalesce operator (safe for R < 4.1.0)
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# CONSTANTS
# ============================================================================

.RIC_HUE_ORDER <- c("R", "YR", "Y", "GY", "G", "BG", "B", "PB", "P", "RP")

.RIC_VOCABULARY <- list(
  consistence_dry = c("loose", "soft", "slightly hard", "hard", "very hard", "extremely hard"),
  consistence_moist = c("loose", "friable", "firm", "very firm", "extremely firm"),
  cementation = c("weakly cemented", "strongly cemented", "indurated", "rigid"),
  effervescence = c("violently effervescent", "strongly effervescent", "slightly effervescent", "noneffervescent"),
  fragment_composition = c("granite", "limestone", "basalt", "sandstone", "quartzite", "schist", "gneiss",
                           "ironstone", "gravel", "cobbles", "stones", "boulders")
)

.RIC_REACTION_CLASSES <- c(
  "ultra acid", "extremely acid", "very strongly acid", "strongly acid",
  "moderately acid", "medium acid", "slightly acid", "neutral",
  "slightly alkaline", "moderately alkaline", "strongly alkaline",
  "very strongly alkaline"
)

.RIC_EFFERVESCENCE_CLASSES <- c(
  "violently effervescent", "strongly effervescent",
  "slightly effervescent", "very slightly effervescent",
  "noneffervescent"
)

# Texture modifiers from NASIS texmod domain (current only; excludes obsolete terms
# and family particle-size class modifiers e.g. vitric, skeletal)
.RIC_TEXTURE_MODIFIERS <- c(
  # Rock fragment modifiers (ordered by intensity variant)
  "Artifactual", "Very artifactual", "Extremely artifactual",
  "Bouldery", "Very bouldery", "Extremely bouldery",
  "Parabouldery", "Very parabouldery", "Extremely parabouldery",
  "Channery", "Very channery", "Extremely channery",
  "Parachannery", "Very parachannery", "Extremely parachannery",
  "Cobbly", "Very cobbly", "Extremely cobbly",
  "Paracobbly", "Very paracobbly", "Extremely paracobbly",
  "Flaggy", "Very flaggy", "Extremely flaggy",
  "Paraflaggy", "Very paraflaggy", "Extremely paraflaggy",
  "Gravelly", "Very gravelly", "Extremely gravelly",
  "Coarse gravelly", "Fine gravelly", "Medium gravelly",
  "Paragravelly", "Very paragravelly", "Extremely paragravelly",
  "Parastony", "Very parastony", "Extremely parastony",
  "Shelly", "Very shelly", "Extremely shelly",
  "Stony", "Very stony", "Extremely stony",
  # Combined fragment + artifact modifiers
  "Bouldery-artifactual", "Very bouldery-artifactual", "Extremely bouldery-artifactual",
  "Channery-artifactual", "Very channery-artifactual", "Extremely channery-artifactual",
  "Cobbly-artifactual", "Very cobbly-artifactual", "Extremely cobbly-artifactual",
  "Flaggy-artifactual", "Very flaggy-artifactual", "Extremely flaggy-artifactual",
  "Gravelly-artifactual", "Very gravelly-artifactual", "Extremely gravelly-artifactual",
  "Shelly-artifactual", "Very shelly-artifactual", "Extremely shelly-artifactual",
  "Stony-artifactual", "Very stony-artifactual", "Extremely stony-artifactual",
  # Volcanic and mineralogy modifiers (from texmod domain)
  "Ashy", "Medial", "Diatomaceous",
  # Cementation/consolidation
  "Cemented", "Hydrous", "Permanently frozen",
  # Organic matter content
  "Highly organic", "Peaty", "Mucky",
  # Biological/botanical
  "Coprogenous", "Grassy", "Herbaceous", "Mossy", "Woody",
  # Chemical/mineral composition
  "Gypsiferous", "Marly"
)

.RIC_TEXTURE_MODIFIERS_LC <- tolower(.RIC_TEXTURE_MODIFIERS)

# Rock fragment size class subset of .RIC_TEXTURE_MODIFIERS
.RIC_FRAGMENT_CLASSES <- tolower(c(
  "Artifactual", "Very artifactual", "Extremely artifactual",
  "Bouldery", "Very bouldery", "Extremely bouldery",
  "Parabouldery", "Very parabouldery", "Extremely parabouldery",
  "Channery", "Very channery", "Extremely channery",
  "Parachannery", "Very parachannery", "Extremely parachannery",
  "Cobbly", "Very cobbly", "Extremely cobbly",
  "Paracobbly", "Very paracobbly", "Extremely paracobbly",
  "Flaggy", "Very flaggy", "Extremely flaggy",
  "Paraflaggy", "Very paraflaggy", "Extremely paraflaggy",
  "Gravelly", "Very gravelly", "Extremely gravelly",
  "Coarse gravelly", "Fine gravelly", "Medium gravelly",
  "Paragravelly", "Very paragravelly", "Extremely paragravelly",
  "Parastony", "Very parastony", "Extremely parastony",
  "Shelly", "Very shelly", "Extremely shelly",
  "Stony", "Very stony", "Extremely stony",
  "Bouldery-artifactual", "Very bouldery-artifactual", "Extremely bouldery-artifactual",
  "Channery-artifactual", "Very channery-artifactual", "Extremely channery-artifactual",
  "Cobbly-artifactual", "Very cobbly-artifactual", "Extremely cobbly-artifactual",
  "Flaggy-artifactual", "Very flaggy-artifactual", "Extremely flaggy-artifactual",
  "Gravelly-artifactual", "Very gravelly-artifactual", "Extremely gravelly-artifactual",
  "Shelly-artifactual", "Very shelly-artifactual", "Extremely shelly-artifactual",
  "Stony-artifactual", "Very stony-artifactual", "Extremely stony-artifactual"
))

# Texture class hierarchy (coarse to fine, aqp::SoilTextureLevels() ordering)
.RIC_TEXTURE_ABBREV_MAP <- c(
  "cos" = "coarse sand", "s" = "sand", "fs" = "fine sand", "vfs" = "very fine sand",
  "lcos" = "loamy coarse sand", "ls" = "loamy sand", "lfs" = "loamy fine sand",
  "lvfs" = "loamy very fine sand", "cosl" = "coarse sandy loam", "sl" = "sandy loam",
  "fsl" = "fine sandy loam", "vfsl" = "very fine sandy loam", "l" = "loam",
  "sil" = "silt loam", "si" = "silt", "scl" = "sandy clay loam", "cl" = "clay loam",
  "sicl" = "silty clay loam", "sc" = "sandy clay", "sic" = "silty clay", "c" = "clay"
)

.RIC_TEXTURE_HIERARCHY <- unname(.RIC_TEXTURE_ABBREV_MAP)

# Longest-first to prevent partial matches
.RIC_TEXTURE_PATTERNS <- c(
  "silty clay loam", "sandy clay loam", "silty clay", "sandy clay",
  "loamy coarse sand", "loamy very fine sand", "loamy fine sand",
  "coarse sandy loam", "very fine sandy loam", "fine sandy loam",
  "clay loam", "silt loam", "sandy loam", "loamy sand",
  "loam", "clay", "sand", "silt",
  "coarse sand", "fine sand", "very fine sand"
)

# Diagnostic feature names to filter from horizon detection
.RIC_DIAGNOSTIC_FEATURES <- c(
  "argillic", "ochric", "cambic", "mollic", "umbric", "spodic",
  "albic", "calcic", "petrocalcic", "natric", "kandic", "oxic",
  "glossic", "duripan", "fragipan", "placic", "petroferric",
  "histic", "folistic", "melanic", "plaggen", "anthropic",
  "agric", "sombric", "gypsic", "petrogypsic", "salic"
)

# ============================================================================
# LOW-LEVEL HELPERS
# ============================================================================

# Match property value against vocabulary list (longest match first)
.ricVocabMatch <- function(text, vocabulary, case_insensitive = TRUE) {
  if (is.null(text) || !is.character(text) || text == "") return(NULL)
  text_lower <- if (case_insensitive) tolower(text) else text
  vocab_sorted <- vocabulary[order(-nchar(vocabulary))]
  for (term in vocab_sorted) {
    term_lower <- if (case_insensitive) tolower(term) else term
    if (stri_detect_fixed(text_lower, term_lower)) return(term)
  }
  return(NULL)
}

# Extract numeric range from text using a two-capture-group pattern
#' @importFrom stringi stri_match_first_regex
.ricExtractRange <- function(text, pattern) {
  if (is.null(text)) return(list(low = NULL, high = NULL))
  match <- stri_match_first_regex(text, pattern)
  if (is.na(match[1, 2])) return(list(low = NULL, high = NULL))
  list(low = as.numeric(match[1, 2]), high = as.numeric(match[1, 3]))
}

# ============================================================================
# HUE HELPERS
# ============================================================================

.ricParseCommaHues <- function(hue_str) {
  if (is.null(hue_str) || nchar(hue_str) == 0) return(character(0))
  hues <- stri_split_regex(hue_str, "\\s*,\\s*")[[1]]
  hues <- stri_trim_both(hues)
  hues[nchar(hues) > 0]
}

.ricExpandHueRange <- function(hue_low, hue_high) {
  parse_hue <- function(h) {
    match <- stri_match(h, regex = "^([0-9.]+)([A-Z]+)$")
    if (is.na(match[1, 2])) return(NULL)
    list(num = as.numeric(match[1, 2]), suffix = match[1, 3])
  }
  h_low_parsed <- parse_hue(hue_low)
  h_high_parsed <- parse_hue(hue_high)
  if (is.null(h_low_parsed) || is.null(h_high_parsed)) return(c(hue_low, hue_high))

  low_suffix_idx <- which(.RIC_HUE_ORDER == h_low_parsed$suffix)
  high_suffix_idx <- which(.RIC_HUE_ORDER == h_high_parsed$suffix)
  if (length(low_suffix_idx) == 0 || length(high_suffix_idx) == 0) return(c(hue_low, hue_high))

  result <- character(0)
  if (low_suffix_idx <= high_suffix_idx) {
    for (suffix_idx in low_suffix_idx:high_suffix_idx) {
      suffix <- .RIC_HUE_ORDER[suffix_idx]
      if (suffix_idx == low_suffix_idx) {
        result <- c(result, paste0(h_low_parsed$num, suffix))
        next_values <- c(2.5, 5, 7.5, 10)[c(2.5, 5, 7.5, 10) > h_low_parsed$num]
        result <- c(result, paste0(next_values, suffix))
      } else if (suffix_idx == high_suffix_idx) {
        prev_values <- c(2.5, 5, 7.5, 10)[c(2.5, 5, 7.5, 10) < h_high_parsed$num]
        result <- c(result, paste0(prev_values, suffix))
        result <- c(result, paste0(h_high_parsed$num, suffix))
      } else {
        result <- c(result, paste0(c(2.5, 5, 7.5, 10), suffix))
      }
    }
  }
  result
}

# ============================================================================
# SERIES-LEVEL HUE EXTRACTION
# ============================================================================

#' @importFrom stringi stri_match_first_regex stri_trim_both stri_detect_regex stri_split_regex stri_replace_all_regex
.extractRICSeriesHue <- function(preamble_text) {
  if (is.null(preamble_text) || !is.character(preamble_text) || nchar(preamble_text) == 0) {
    return(NULL)
  }

  # Pattern 1: "hue of X or Y" or "hues of X to Y"
  # Matches: "The soil has hue of 5YR or redder"
  #          "The soils have hues of 7.5YR to 5Y"
  hue_match <- stri_match_first_regex(preamble_text, 
    "(?i)\\b(?:The\\s+soils?(?:\\s+material)?\\s+(?:has|have)\\s+)?hues?\\s+(?:is|are|of)?\\s+([\\w\\.]+?)\\s+(?:to|or|and|through)\\s+([^\\n,\\.]+?)(?:\\.| |,|$)")

  # Pattern 2: "Hue ranges from X to/through Y" or "Hue ranges from X or Y"
  if (is.na(hue_match[1, 1])) {
    hue_match <- stri_match_first_regex(preamble_text,
      "(?i)\\bHues?\\s+ranges?\\s+from\\s+([[:alnum:]\\.]+?)\\s+(?:to|through|or)\\s+([[:alnum:]\\.]+)(?:\\s|,|\\.|$)")
  }

  if (is.na(hue_match[1, 1])) {
    return(NULL)
  }

  matched_text <- stri_trim_both(hue_match[1, 1])
  hue_low <- stri_trim_both(hue_match[1, 2])
  hue_high <- stri_trim_both(hue_match[1, 3])

  # Clean up hue_high: extract only the hue value itself (numbers, letters, dots), strip trailing periods
  hue_high <- stri_extract_first_regex(hue_high, "^[[:alnum:]\\.]+")
  hue_high <- stri_replace_all_regex(hue_high, "\\.$", "")
  hue_high <- stri_trim_both(hue_high)

  # Normalize "redder" → "R", "yellower" → "Y", etc.
  hue_map <- list(
    "redder" = "R",
    "yellower" = "Y",
    "greener" = "G",
    "bluer" = "B",
    "purpler" = "P",
    "grayer" = "N"  # neutral/gray
  )

  for (key in names(hue_map)) {
    if (stri_detect_regex(tolower(hue_high), paste0("\\b", key, "\\b"))) {
      hue_high <- hue_map[[key]]
      break
    }
  }

  return(list(
    matched_text = matched_text,
    low = hue_low,
    high = hue_high
  ))
}

#' @importFrom stringi stri_match_all_regex
.ricExtractAllHues <- function(hue_text) {
  if (is.null(hue_text) || nchar(hue_text) == 0) return(character(0))
  hue_text <- stri_trim_both(hue_text)
  hues <- character(0)

  if (stri_detect_regex(hue_text, "(?i)neutral|^N$")) {
    hues <- c(hues, "N")
  }

  all_hue_matches <- stri_match_all(hue_text, regex = "\\b([0-9.]+(?:R|YR|Y|GY|G|BG|B|PB|P|RP))\\b")[[1]]

  if (!is.na(all_hue_matches[1, 1])) {
    extracted_hues <- all_hue_matches[, 2]
    extracted_hues <- extracted_hues[!is.na(extracted_hues)]

    if (stri_detect_regex(hue_text, "(?i)through")) {
      through_matches <- stri_match_all(hue_text, regex = "([0-9.]+[A-Z]+)\\s+through\\s+([0-9.]+[A-Z]+)")[[1]]
      if (!is.na(through_matches[1, 1])) {
        expanded <- character(0)
        for (i in seq_len(nrow(through_matches))) {
          hue_low <- through_matches[i, 2]
          hue_high <- through_matches[i, 3]
          if (!is.na(hue_low) && !is.na(hue_high)) {
            expanded <- c(expanded, .ricExpandHueRange(hue_low, hue_high))
          }
        }
        hues <- c(hues, expanded)
      } else {
        hues <- c(hues, extracted_hues)
      }
    } else {
      hues <- c(hues, extracted_hues)
    }
  }

  hues <- unique(hues)
  hues[order(hues)]
}

# ============================================================================
# HORIZON BLOCK DETECTION
# ============================================================================

#' @importFrom stringi stri_locate_all stri_match_all stri_sub stri_split_fixed stri_extract_first_regex stri_trim_both
#' @importFrom stringi stri_replace_all_fixed
.extractRICHorizonBlocks <- function(ric_text) {
  if (!is.character(ric_text) || length(ric_text) == 0 || nchar(ric_text) == 0) {
    return(list())
  }

  all_matches <- data.frame(
    position = integer(0),
    designation = character(0),
    pattern = character(0),
    stringsAsFactors = FALSE
  )

  HZ_SUFFIX <- "(?:horizons?|layers?|tier)(?:\\(s\\)|s)?"
  HZ_DESIG <- "[\\^\\'\\\"`\\/a-zA-Z0-9]+"

  # Pattern 0: non-multiline (single-line RIC text)
  pattern0_simple <- paste0("(?i)(?<!\\.)\\bThe\\s+(", HZ_DESIG, ")\\s+", HZ_SUFFIX)
  pos0 <- stri_locate_all(ric_text, regex = pattern0_simple)[[1]]
  matches0 <- stri_match_all(ric_text, regex = pattern0_simple)[[1]]
  if (!is.na(pos0[1, 1]) && nrow(pos0) > 0) {
    for (i in seq_len(nrow(pos0))) {
      if (!is.na(matches0[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos0[i, 1], designation = stri_trim_both(matches0[i, 2]),
          pattern = "pattern0", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 0b: "The X and Y horizons" compound, non-multiline
  pattern0b <- paste0("(?i)(?<!\\.)\\bThe\\s+((", HZ_DESIG, ")(?:\\s+and\\s+", HZ_DESIG, ")?)\\s+", HZ_SUFFIX)
  pos0b <- stri_locate_all(ric_text, regex = pattern0b)[[1]]
  matches0b <- stri_match_all(ric_text, regex = pattern0b)[[1]]
  if (!is.na(pos0b[1, 1]) && nrow(pos0b) > 0) {
    for (i in seq_len(nrow(pos0b))) {
      if (!is.na(matches0b[i, 2])) {
        combined <- stri_trim_both(matches0b[i, 2])
        combined_normalized <- stri_replace_all_fixed(combined, " and ", ", ")
        designations <- stri_split_fixed(combined_normalized, ",")[[1]]
        designations <- stri_trim_both(designations[nchar(designations) > 0])
        for (desig in designations) {
          if (nchar(desig) > 0 && !tolower(desig) %in% c("and")) {
            all_matches <- rbind(all_matches, data.frame(
              position = pos0b[i, 1], designation = desig,
              pattern = "pattern0b", stringsAsFactors = FALSE))
          }
        }
      }
    }
  }

  # Pattern 1: "The X horizon" blocks (simple single designations, line-start only)
  pattern1 <- paste0("(?i)^\\s*The\\s+(", HZ_DESIG, ")(?:\\s+or\\s+", HZ_DESIG, ")?\\s+", HZ_SUFFIX, "(?!\\s*,)")
  pos1 <- stri_locate_all(ric_text, regex = pattern1, opts_regex = list(multiline = TRUE))[[1]]
  matches1 <- stri_match_all(ric_text, regex = pattern1, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos1[1, 1]) && nrow(pos1) > 0) {
    for (i in seq_len(nrow(pos1))) {
      if (!is.na(matches1[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos1[i, 1], designation = stri_trim_both(matches1[i, 2]),
          pattern = "pattern1", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2: Simple "X horizon:" at line start
  pattern2 <- paste0("(?i)^\\s*(", HZ_DESIG, ")\\s+", HZ_SUFFIX, "(?:\\s|:|$)")
  pos2 <- stri_locate_all(ric_text, regex = pattern2, opts_regex = list(multiline = TRUE))[[1]]
  matches2 <- stri_match_all(ric_text, regex = pattern2, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2[1, 1]) && nrow(pos2) > 0) {
    for (i in seq_len(nrow(pos2))) {
      if (!is.na(matches2[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos2[i, 1], designation = stri_trim_both(matches2[i, 2]),
          pattern = "pattern2", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2b: Compound "Ap or A horizon:" or "Bt1 and Bt2 horizon:"
  pattern2b <- paste0("(?i)^\\s*((", HZ_DESIG, ")(?:\\s+(?:or|and|and/or)\\s+", HZ_DESIG, ")?)\\s+", HZ_SUFFIX, "(?:\\s|:|$)")
  pos2b <- stri_locate_all(ric_text, regex = pattern2b, opts_regex = list(multiline = TRUE))[[1]]
  matches2b <- stri_match_all(ric_text, regex = pattern2b, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2b[1, 1]) && nrow(pos2b) > 0) {
    for (i in seq_len(nrow(pos2b))) {
      if (!is.na(matches2b[i, 2])) {
        combined <- stri_trim_both(matches2b[i, 2])
        designation <- stri_extract_first_regex(combined, HZ_DESIG)
        if (is.na(designation) || nchar(designation) == 0) designation <- combined
        all_matches <- rbind(all_matches, data.frame(
          position = pos2b[i, 1], designation = stri_trim_both(designation),
          pattern = "pattern2b", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2c: Comma-and-separated "The Apg, Ag and Ap horizons"
  pattern2c <- paste0("(?i)^\\s*The\\s+((", HZ_DESIG, ")(?:\\s*,\\s*", HZ_DESIG, ")*(?:\\s+and\\s+", HZ_DESIG, ")?)\\s+", HZ_SUFFIX)
  pos2c <- stri_locate_all(ric_text, regex = pattern2c, opts_regex = list(multiline = TRUE))[[1]]
  matches2c <- stri_match_all(ric_text, regex = pattern2c, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2c[1, 1]) && nrow(pos2c) > 0) {
    for (i in seq_len(nrow(pos2c))) {
      if (!is.na(matches2c[i, 2])) {
        combined <- stri_trim_both(matches2c[i, 2])
        combined_normalized <- stri_replace_all_fixed(combined, " and ", ", ")
        designations <- stri_split_fixed(combined_normalized, ",")[[1]]
        designations <- stri_trim_both(designations[nchar(designations) > 0])
        for (desig in designations) {
          if (nchar(desig) > 0) {
            all_matches <- rbind(all_matches, data.frame(
              position = pos2c[i, 1], designation = desig,
              pattern = "pattern2c", stringsAsFactors = FALSE))
          }
        }
      }
    }
  }

  # Pattern 2d: "Upper/Lower/Middle X horizon" qualifiers
  pattern2d <- paste0("(?i)^\\s*((?:Upper|Lower|Middle)\\s+", HZ_DESIG, ")\\s+", HZ_SUFFIX, "(?:\\s|:|$)")
  pos2d <- stri_locate_all(ric_text, regex = pattern2d, opts_regex = list(multiline = TRUE))[[1]]
  matches2d <- stri_match_all(ric_text, regex = pattern2d, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2d[1, 1]) && nrow(pos2d) > 0) {
    for (i in seq_len(nrow(pos2d))) {
      if (!is.na(matches2d[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos2d[i, 1], designation = stri_trim_both(matches2d[i, 2]),
          pattern = "pattern2d", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2e: Organic soil tier patterns
  pattern2e <- paste0("(?i)^\\s*((?:Surface|Subsurface|Bottom)\\s+tier)(?:\\s+of\\s+the\\s+", HZ_DESIG, "\\s+horizon)?\\s*[:)]?")
  pos2e <- stri_locate_all(ric_text, regex = pattern2e, opts_regex = list(multiline = TRUE))[[1]]
  matches2e <- stri_match_all(ric_text, regex = pattern2e, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2e[1, 1]) && nrow(pos2e) > 0) {
    for (i in seq_len(nrow(pos2e))) {
      if (!is.na(matches2e[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos2e[i, 1], designation = stri_trim_both(matches2e[i, 2]),
          pattern = "pattern2e", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2f: Dash-delimited "A horizon--" (double dash)
  pattern2f <- paste0("(?i)^\\s*((", HZ_DESIG, ")(?:/", HZ_DESIG, ")?(?:\\s+(?:or|and|and/or)\\s+", HZ_DESIG, "(?:/", HZ_DESIG, ")?)?)\\s+", HZ_SUFFIX, "\\s*--")
  pos2f <- stri_locate_all(ric_text, regex = pattern2f, opts_regex = list(multiline = TRUE))[[1]]
  matches2f <- stri_match_all(ric_text, regex = pattern2f, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2f[1, 1]) && nrow(pos2f) > 0) {
    for (i in seq_len(nrow(pos2f))) {
      if (!is.na(matches2f[i, 2])) {
        combined <- stri_trim_both(matches2f[i, 2])
        designation <- stri_extract_first_regex(combined, HZ_DESIG)
        if (is.na(designation) || nchar(designation) == 0) designation <- combined
        all_matches <- rbind(all_matches, data.frame(
          position = pos2f[i, 1], designation = stri_trim_both(designation),
          pattern = "pattern2f", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 2g: Comma-and-separated without "The" prefix
  pattern2g <- paste0("(?i)^\\s*((", HZ_DESIG, ")(?:\\s*,\\s*", HZ_DESIG, ")+(?:\\s*,?\\s*and\\s+", HZ_DESIG, ")?)\\s+", HZ_SUFFIX)
  pos2g <- stri_locate_all(ric_text, regex = pattern2g, opts_regex = list(multiline = TRUE))[[1]]
  matches2g <- stri_match_all(ric_text, regex = pattern2g, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2g[1, 1]) && nrow(pos2g) > 0) {
    for (i in seq_len(nrow(pos2g))) {
      if (!is.na(matches2g[i, 2])) {
        combined <- stri_trim_both(matches2g[i, 2])
        combined_normalized <- stri_replace_all_fixed(combined, " and ", ", ")
        designations <- stri_split_fixed(combined_normalized, ",")[[1]]
        designations <- stri_trim_both(designations[nchar(stri_trim_both(designations)) > 0])
        for (desig in designations) {
          if (nchar(desig) > 0) {
            all_matches <- rbind(all_matches, data.frame(
              position = pos2g[i, 1], designation = desig,
              pattern = "pattern2g", stringsAsFactors = FALSE))
          }
        }
      }
    }
  }

  # Pattern 2h: Slash-designation horizons "C1/A horizon"
  pattern2h <- paste0("(?i)^\\s*((", HZ_DESIG, ")/", HZ_DESIG, ")\\s+", HZ_SUFFIX, "(?:\\s|:|$)")
  pos2h <- stri_locate_all(ric_text, regex = pattern2h, opts_regex = list(multiline = TRUE))[[1]]
  matches2h <- stri_match_all(ric_text, regex = pattern2h, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos2h[1, 1]) && nrow(pos2h) > 0) {
    for (i in seq_len(nrow(pos2h))) {
      if (!is.na(matches2h[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos2h[i, 1], designation = stri_trim_both(matches2h[i, 2]),
          pattern = "pattern2h", stringsAsFactors = FALSE))
      }
    }
  }

  # Pattern 3: "Some pedons have a X horizon:"
  pattern3 <- paste0("(?i)^\\s*Some\\s+pedons?\\s+have\\s+a(?:n)?\\s+(", HZ_DESIG, ")\\s+", HZ_SUFFIX)
  pos3 <- stri_locate_all(ric_text, regex = pattern3, opts_regex = list(multiline = TRUE))[[1]]
  matches3 <- stri_match_all(ric_text, regex = pattern3, opts_regex = list(multiline = TRUE))[[1]]
  if (!is.na(pos3[1, 1]) && nrow(pos3) > 0) {
    for (i in seq_len(nrow(pos3))) {
      if (!is.na(matches3[i, 2])) {
        all_matches <- rbind(all_matches, data.frame(
          position = pos3[i, 1], designation = stri_trim_both(matches3[i, 2]),
          pattern = "pattern3", stringsAsFactors = FALSE))
      }
    }
  }

  if (nrow(all_matches) == 0) return(list())

  # Filter diagnostic feature names
  all_matches <- all_matches[!tolower(all_matches$designation) %in% .RIC_DIAGNOSTIC_FEATURES, , drop = FALSE]
  if (nrow(all_matches) == 0) return(list())

  # Remove duplicates: keep only the FIRST occurrence of each designation
  all_matches <- all_matches[!duplicated(tolower(all_matches$designation)), , drop = FALSE]
  all_matches <- all_matches[order(all_matches$position), , drop = FALSE]

  # Map each unique position to its end boundary
  unique_positions <- unique(all_matches$position)
  position_to_end <- list()
  for (j in seq_along(unique_positions)) {
    current_pos <- unique_positions[j]
    if (j < length(unique_positions)) {
      end_pos <- unique_positions[j + 1] - 1
    } else {
      end_pos <- nchar(ric_text)
    }
    position_to_end[[as.character(current_pos)]] <- end_pos
  }

  # Consolidate compound horizons
  horizons <- list()
  position_processed <- character(0)
  for (i in seq_len(nrow(all_matches))) {
    start <- all_matches$position[i]
    str_start <- as.character(start)
    if (str_start %in% position_processed) next
    position_processed <- c(position_processed, str_start)

    designations_at_pos <- all_matches$designation[all_matches$position == start]
    end <- position_to_end[[str_start]]
    block_text <- stri_trim_both(stri_sub(ric_text, start, end))

    # Create an entry for EACH designation at this position, not just the first
    for (designation in designations_at_pos) {
      if (nchar(designation) > 0 && nchar(block_text) > 0) {
        horizons[[designation]] <- block_text
      }
    }
  }

  horizons
}

# ============================================================================
# HORIZON PATTERN AND DRY/MOIST HELPERS
# ============================================================================

.ricExtractHorizonPattern <- function(narrative, primary_designation) {
  if (is.null(narrative) || nchar(narrative) == 0) return(primary_designation)

  pattern1 <- "^\\s*The\\s+([A-Za-z0-9]+)(?:\\s+or\\s+([A-Za-z0-9]+))?(?:\\s+or\\s+([A-Za-z0-9]+))?\\s+horizons?"
  match1 <- stri_match_first_regex(narrative, pattern1)
  if (!is.na(match1[1, 2])) {
    designations <- c()
    for (i in 2:ncol(match1)) {
      if (!is.na(match1[1, i]) && nchar(match1[1, i]) > 0) {
        designations <- c(designations, stri_trim_both(match1[1, i]))
      }
    }
    if (length(designations) > 0) return(paste(designations, collapse = "|"))
  }

  pattern2 <- "^\\s*The\\s+([A-Za-z0-9]+)(?:,?\\s+(?:and\\s+)?([A-Za-z0-9]+))?(?:,?\\s+(?:and\\s+)?([A-Za-z0-9]+))?\\s+horizons?"
  match2 <- stri_match_first_regex(narrative, pattern2)
  if (!is.na(match2[1, 2])) {
    designations <- c()
    for (i in 2:ncol(match2)) {
      if (!is.na(match2[1, i]) && nchar(match2[1, i]) > 0) {
        des <- stri_trim_both(match2[1, i])
        if (!tolower(des) %in% c("and", "or", "the", "buried", "soils", "with", "dark", "shades")) {
          designations <- c(designations, des)
        }
      }
    }
    if (length(designations) > 0) return(paste(designations, collapse = "|"))
  }

  primary_designation
}

.ricParseDryMoistValues <- function(value_str) {
  if (is.null(value_str) || nchar(value_str) == 0) {
    return(list(dry = list(low = NULL, high = NULL), moist = list(low = NULL, high = NULL)))
  }
  result <- list(dry = list(low = NULL, high = NULL), moist = list(low = NULL, high = NULL))
  parts <- stri_split_regex(value_str, "[;,]")[[1]]
  for (part in parts) {
    part_trimmed <- stri_trim_both(part)
    part_lower <- tolower(part_trimmed)
    range_match <- stri_match(part_trimmed, regex = "(\\d+)\\s*(?:to|or|-|through)\\s*(\\d+)")
    if (!is.na(range_match[1, 2])) {
      low <- as.numeric(range_match[1, 2])
      high <- as.numeric(range_match[1, 3])
      if (stri_detect_fixed(part_lower, "dry")) {
        result$dry <- list(low = low, high = high)
      } else if (stri_detect_fixed(part_lower, "moist")) {
        result$moist <- list(low = low, high = high)
      }
    }
  }
  result
}

# ============================================================================
# DIRECT MUNSELL NOTATION PARSER
# ============================================================================

#' @importFrom stringi stri_detect_regex stri_match_all_regex stri_extract_all_regex stri_trim_both stri_replace_all_regex
.ricParseDirectMunsellNotation <- function(horizon_block, default_color_state = "moist") {
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(list())
  
  result_list <- list()
  
  dry_match <- stri_match_first_regex(horizon_block, "(?i)dry\\s+color\\s+(?:is|are)\\s+(.+?)(?=\\bMoist|\\bmoist|[.;]|$)")
  if (!is.na(dry_match[1, 2])) {
    dry_text <- stri_trim_both(dry_match[1, 2])
    if (nchar(dry_text) > 0 && stri_detect_regex(dry_text, "\\d+/\\d+")) {
      dry_pairs <- .ricExtractMunsellPairsFromText(dry_text)
      if (length(dry_pairs) > 0) {
        hue <- .ricExtractMunsellHue(dry_text)
        if (!is.null(hue)) {
          values <- sapply(dry_pairs, function(x) x$value, USE.NAMES = FALSE)
          chromas <- sapply(dry_pairs, function(x) x$chroma, USE.NAMES = FALSE)
          result_list$dry <- list(
            state = "dry",
            hue = hue,
            munsell_pairs = dry_pairs,
            value_min = min(values, na.rm = TRUE),
            value_max = max(values, na.rm = TRUE),
            chroma_min = min(chromas, na.rm = TRUE),
            chroma_max = max(chromas, na.rm = TRUE)
          )
        }
      }
    }
  }
  
  moist_match <- stri_match_first_regex(horizon_block, "(?i)moist\\s+color\\s+(?:is|are)\\s+(.+?)(?=[.;]|$)")
  if (!is.na(moist_match[1, 2])) {
    moist_text <- stri_trim_both(moist_match[1, 2])
    if (nchar(moist_text) > 0 && stri_detect_regex(moist_text, "\\d+/\\d+")) {
      moist_pairs <- .ricExtractMunsellPairsFromText(moist_text)
      if (length(moist_pairs) > 0) {
        hue <- .ricExtractMunsellHue(moist_text)
        if (!is.null(hue)) {
          values <- sapply(moist_pairs, function(x) x$value, USE.NAMES = FALSE)
          chromas <- sapply(moist_pairs, function(x) x$chroma, USE.NAMES = FALSE)
          result_list$moist <- list(
            state = "moist",
            hue = hue,
            munsell_pairs = moist_pairs,
            value_min = min(values, na.rm = TRUE),
            value_max = max(values, na.rm = TRUE),
            chroma_min = min(chromas, na.rm = TRUE),
            chroma_max = max(chromas, na.rm = TRUE)
          )
        }
      }
    }
  }
  
  result_list
}

# Extract Munsell hue from text (e.g., "10YR 5/2, 4/2, 4/3" -> "10YR")
.ricExtractMunsellHue <- function(text) {
  match <- stri_match_first_regex(text, "([0-9.]*(?:YR|GY|Y|G|B|BG|RP|P|R))", case_insensitive = TRUE)
  if (!is.na(match[1, 1])) {
    return(stri_trim_both(match[1, 1]))
  }
  NULL
}

# Extract all Munsell value/chroma pairs from text
# Examples: "10YR 5/2, 4/2, 4/3" -> list(list(value=5, chroma=2), list(value=4, chroma=2), ...)
.ricExtractMunsellPairsFromText <- function(text) {
  pairs <- list()
  matches <- stri_match_all_regex(text, "(\\d+)/(\\d+)")[[1]]
  
  if (is.na(matches[1, 1])) return(pairs)
  
  for (i in seq_len(nrow(matches))) {
    value_str <- matches[i, 2]
    chroma_str <- matches[i, 3]
    
    if (!is.na(value_str) && !is.na(chroma_str)) {
      value_num <- as.numeric(value_str)
      chroma_num <- as.numeric(chroma_str)
      
      # Validate: value 0-10, chroma 0-8
      if (value_num >= 0 && value_num <= 10 && chroma_num >= 0 && chroma_num <= 8) {
        pairs[[length(pairs) + 1]] <- list(value = value_num, chroma = chroma_num)
      }
    }
  }
  
  pairs
}

# ============================================================================
# COLOR EXTRACTION
# ============================================================================

.extractRICColors <- function(horizon_block, default_color_state = "moist") {
  result <- list(
    dry = list(hues = NULL, value = list(low = NULL, high = NULL), chroma = list(low = NULL, high = NULL), munsell_pairs = NULL),
    moist = list(hues = NULL, value = list(low = NULL, high = NULL), chroma = list(low = NULL, high = NULL), munsell_pairs = NULL),
    unknown = list(hues = NULL, value = list(low = NULL, high = NULL), chroma = list(low = NULL, high = NULL), munsell_pairs = NULL),
    state = NULL
  )
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(result)

  # **Priority 1: Direct Munsell notation** (e.g., "10YR 7/2, 6/2, 6/3, 6/4" or "Dry: ... Moist: ...")
  munsell_result <- .ricParseDirectMunsellNotation(horizon_block, default_color_state)
  
  # Process dry munsell if found
  if (!is.null(munsell_result$dry)) {
    dry_data <- munsell_result$dry
    result$dry$hues <- dry_data$hue
    result$dry$value <- list(low = dry_data$value_min, high = dry_data$value_max)
    result$dry$chroma <- list(low = dry_data$chroma_min, high = dry_data$chroma_max)
    result$dry$munsell_pairs <- dry_data$munsell_pairs
    result$state <- "dry_moist"  # Will be updated if only one state exists
  }
  
  # Process moist munsell if found
  if (!is.null(munsell_result$moist)) {
    moist_data <- munsell_result$moist
    result$moist$hues <- moist_data$hue
    result$moist$value <- list(low = moist_data$value_min, high = moist_data$value_max)
    result$moist$chroma <- list(low = moist_data$chroma_min, high = moist_data$chroma_max)
    result$moist$munsell_pairs <- moist_data$munsell_pairs
    if (result$state != "dry_moist") result$state <- "moist"
  }
  
  # If both dry and moist were found, state should be "dry_moist"
  if (!is.null(munsell_result$dry) && !is.null(munsell_result$moist)) {
    result$state <- "dry_moist"
  } else if (!is.null(munsell_result$dry) && is.null(munsell_result$moist) && !is.null(munsell_result$dry$munsell_pairs)) {
    result$state <- "dry"
  } else if (is.null(munsell_result$dry) && !is.null(munsell_result$moist) && !is.null(munsell_result$moist$munsell_pairs)) {
    result$state <- "moist"
  }
  
  # If we found Munsell data in either state, return early
  if (!is.null(result$state) && result$state %in% c("dry", "moist", "dry_moist")) {
    return(result)
  }

  # **Priority 2: Structured format** (e.g., "Hue of 10YR, Value of 7, Chroma of 2")
  # Hue extraction
  hues <- NULL
  hue_match <- stri_match(horizon_block, regex = "(?i)hue\\s+of\\s+(.+?)(?=,\\s*value|;|\\n[A-Z]|$)", opts_regex = list(multiline = TRUE))
  if (!is.na(hue_match[1, 2])) {
    hues <- .ricExtractAllHues(stri_trim_both(hue_match[1, 2]))
  } else {
    hue_match <- stri_match(horizon_block, regex = "(?i)Hue:?\\s*(.+?)(?=Value|Chroma|\\n[A-Z]|$)", opts_regex = list(multiline = TRUE))
    if (!is.na(hue_match[1, 2])) {
      hues <- .ricExtractAllHues(stri_trim_both(hue_match[1, 2]))
    }
  }

  has_dry_data <- FALSE
  has_moist_data <- FALSE

  # Value extraction
  value_match <- stri_match(horizon_block, regex = "(?i)value\\s+of\\s+(.+?)(?=Chroma|and\\s+chroma|\\n[A-Z]|$)", opts_regex = list(multiline = TRUE))
  if (!is.na(value_match[1, 2])) {
    value_str <- stri_trim_both(value_match[1, 2])
    if (stri_detect_fixed(tolower(value_str), "dry") || stri_detect_fixed(tolower(value_str), "moist")) {
      if (stri_detect_fixed(tolower(value_str), "dry or moist")) {
        states <- .ricParseDryMoistValues(value_str)
        if (!is.null(states$dry$low)) {
          result$dry$value <- list(low = states$dry$low, high = states$dry$high)
          result$moist$value <- list(low = states$dry$low, high = states$dry$high)
          has_dry_data <- TRUE; has_moist_data <- TRUE
        }
      } else {
        states <- .ricParseDryMoistValues(value_str)
        if (!is.null(states$dry$low)) { result$dry$value <- list(low = states$dry$low, high = states$dry$high); has_dry_data <- TRUE }
        if (!is.null(states$moist$low)) { result$moist$value <- list(low = states$moist$low, high = states$moist$high); has_moist_data <- TRUE }
      }
    } else {
      range_match <- stri_match(value_str, regex = "(\\d+)\\s*(?:to|or|through)\\s*(\\d+)")
      if (!is.na(range_match[1, 2])) {
        result$unknown$value <- list(low = as.numeric(range_match[1, 2]), high = as.numeric(range_match[1, 3]))
      }
    }
  } else {
    value_match <- stri_match(horizon_block, regex = "(?i)Value:\\s*(.+?)(?=Chroma|\\n[A-Z]|$)", opts_regex = list(multiline = TRUE))
    if (!is.na(value_match[1, 2])) {
      value_str <- stri_trim_both(value_match[1, 2])
      if (stri_detect_fixed(tolower(value_str), "dry") || stri_detect_fixed(tolower(value_str), "moist")) {
        if (stri_detect_fixed(tolower(value_str), "dry or moist")) {
          states <- .ricParseDryMoistValues(value_str)
          if (!is.null(states$dry$low)) {
            result$dry$value <- list(low = states$dry$low, high = states$dry$high)
            result$moist$value <- list(low = states$dry$low, high = states$dry$high)
            has_dry_data <- TRUE; has_moist_data <- TRUE
          }
        } else {
          states <- .ricParseDryMoistValues(value_str)
          if (!is.null(states$dry$low)) { result$dry$value <- list(low = states$dry$low, high = states$dry$high); has_dry_data <- TRUE }
          if (!is.null(states$moist$low)) { result$moist$value <- list(low = states$moist$low, high = states$moist$high); has_moist_data <- TRUE }
        }
      } else {
        range_match <- stri_match(value_str, regex = "(\\d+)\\s*(?:to|or|through)\\s*(\\d+)")
        if (!is.na(range_match[1, 2])) {
          result$unknown$value <- list(low = as.numeric(range_match[1, 2]), high = as.numeric(range_match[1, 3]))
        }
      }
    }
  }

  # Chroma extraction
  chroma_match <- stri_match(horizon_block, regex = "(?i)chroma\\s+of\\s+(.+?)(?=\\n[A-Z]|$|It\\s+is|It\\s+has)", opts_regex = list(multiline = TRUE))
  if (!is.na(chroma_match[1, 2])) {
    chroma_str <- stri_trim_both(chroma_match[1, 2])
    if (stri_detect_fixed(tolower(chroma_str), "dry") || stri_detect_fixed(tolower(chroma_str), "moist")) {
      if (stri_detect_fixed(tolower(chroma_str), "dry or moist")) {
        states <- .ricParseDryMoistValues(chroma_str)
        if (!is.null(states$dry$low) && states$dry$low <= 8 && states$dry$high <= 8 && states$dry$low <= states$dry$high) {
          result$dry$chroma <- list(low = states$dry$low, high = states$dry$high)
          result$moist$chroma <- list(low = states$dry$low, high = states$dry$high)
          has_dry_data <- TRUE; has_moist_data <- TRUE
        }
      } else {
        states <- .ricParseDryMoistValues(chroma_str)
        if (!is.null(states$dry$low) && !is.null(states$dry$high) && states$dry$low <= 8 && states$dry$high <= 8 && states$dry$low <= states$dry$high) {
          result$dry$chroma <- list(low = states$dry$low, high = states$dry$high); has_dry_data <- TRUE
        }
        if (!is.null(states$moist$low) && !is.null(states$moist$high) && states$moist$low <= 8 && states$moist$high <= 8 && states$moist$low <= states$moist$high) {
          result$moist$chroma <- list(low = states$moist$low, high = states$moist$high); has_moist_data <- TRUE
        }
      }
    } else {
      range_match <- stri_match(chroma_str, regex = "(\\d+)\\s*(?:to|or|through)\\s*(\\d+)")
      if (!is.na(range_match[1, 2])) {
        cl <- as.numeric(range_match[1, 2]); ch <- as.numeric(range_match[1, 3])
        if (cl <= 8 && ch <= 8 && cl <= ch) result$unknown$chroma <- list(low = cl, high = ch)
      }
    }
  } else {
    chroma_match <- stri_match(horizon_block, regex = "(?i)Chroma:\\s*(.+?)(?=\\n[A-Z]|$)", opts_regex = list(multiline = TRUE))
    if (!is.na(chroma_match[1, 2])) {
      chroma_str <- stri_trim_both(chroma_match[1, 2])
      if (stri_detect_fixed(tolower(chroma_str), "dry") || stri_detect_fixed(tolower(chroma_str), "moist")) {
        if (stri_detect_fixed(tolower(chroma_str), "dry or moist")) {
          states <- .ricParseDryMoistValues(chroma_str)
          if (!is.null(states$dry$low) && states$dry$low <= 8 && states$dry$high <= 8 && states$dry$low <= states$dry$high) {
            result$dry$chroma <- list(low = states$dry$low, high = states$dry$high)
            result$moist$chroma <- list(low = states$dry$low, high = states$dry$high)
            has_dry_data <- TRUE; has_moist_data <- TRUE
          }
        } else {
          states <- .ricParseDryMoistValues(chroma_str)
          if (!is.null(states$dry$low) && !is.null(states$dry$high) && states$dry$low <= 8 && states$dry$high <= 8 && states$dry$low <= states$dry$high) {
            result$dry$chroma <- list(low = states$dry$low, high = states$dry$high); has_dry_data <- TRUE
          }
          if (!is.null(states$moist$low) && !is.null(states$moist$high) && states$moist$low <= 8 && states$moist$high <= 8 && states$moist$low <= states$moist$high) {
            result$moist$chroma <- list(low = states$moist$low, high = states$moist$high); has_moist_data <- TRUE
          }
        }
      } else {
        range_match <- stri_match(chroma_str, regex = "(\\d+)\\s*(?:to|or|through)\\s*(\\d+)")
        if (!is.na(range_match[1, 2])) {
          cl <- as.numeric(range_match[1, 2]); ch <- as.numeric(range_match[1, 3])
          if (cl <= 8 && ch <= 8 && cl <= ch) result$unknown$chroma <- list(low = cl, high = ch)
        }
      }
    }
  }

  # Determine color state
  if (has_dry_data && has_moist_data) {
    result$state <- "both"
  } else if (has_dry_data) {
    result$state <- "dry"
  } else if (has_moist_data) {
    result$state <- "moist"
  } else {
    has_color_data <- !is.null(result$unknown$value$low) || !is.null(result$unknown$value$high) ||
                      !is.null(result$unknown$chroma$low) || !is.null(result$unknown$chroma$high) ||
                      length(result$unknown$hues) > 0
    if (has_color_data && !is.null(default_color_state)) {
      result$state <- default_color_state
      if (default_color_state %in% c("dry", "moist")) {
        result[[default_color_state]] <- result$unknown
        result$unknown <- list(hues = NULL, value = list(low = NULL, high = NULL), chroma = list(low = NULL, high = NULL))
      }
    } else if (has_color_data) {
      result$state <- "unknown"
    }
  }

  # Cleanup: build final result with only non-empty states
  has_dry_content <- !is.null(result$dry$value$low) || !is.null(result$dry$chroma$low) || !is.null(result$dry$munsell_pairs)
  has_moist_content <- !is.null(result$moist$value$low) || !is.null(result$moist$chroma$low) || !is.null(result$moist$munsell_pairs)
  has_unknown_content <- !is.null(result$unknown$value$low) || !is.null(result$unknown$chroma$low) || !is.null(result$unknown$munsell_pairs)

  final_result <- list(state = result$state)
  if (has_dry_content) {
    final_result$dry <- list(hues = if (length(hues) > 0) hues else NULL, value = result$dry$value, chroma = result$dry$chroma, munsell_pairs = result$dry$munsell_pairs)
  }
  if (has_moist_content) {
    final_result$moist <- list(hues = if (length(hues) > 0) hues else NULL, value = result$moist$value, chroma = result$moist$chroma, munsell_pairs = result$moist$munsell_pairs)
  }
  if (has_unknown_content) {
    final_result$unknown <- list(hues = if (length(hues) > 0) hues else NULL, value = result$unknown$value, chroma = result$unknown$chroma, munsell_pairs = result$unknown$munsell_pairs)
  }
  final_result
}

# ============================================================================
# CLAY EXTRACTION
# ============================================================================

.extractRICClay <- function(horizon_block) {
  result <- list(clay_low_pct = NULL, clay_high_pct = NULL)
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(result)

  clay_match <- stri_match(horizon_block, regex = "(?i)\\bclay\\s*(?:content\\s*)?(?::|--?|--)?\\s*(\\d+\\.?\\d*)\\s*(?:to|-|or)\\s*(\\d+\\.?\\d*)\\s*(?:percent|%)")
  if (!is.na(clay_match[1, 2])) {
    result$clay_low_pct <- as.numeric(clay_match[1, 2])
    result$clay_high_pct <- as.numeric(clay_match[1, 3])
    return(result)
  }

  clay_match <- stri_match(horizon_block, regex = "(?i)\\bClay\\s*(?:content)?\\s*(?::|--?|--)?\\s*(.+?)(?=\\n[A-Z]|$)")
  if (!is.na(clay_match[1, 2])) {
    clay_str <- stri_trim_both(clay_match[1, 2])
    range_match <- stri_match(clay_str, regex = "(\\d+)\\s*(?:to|or)\\s*(\\d+)")
    if (!is.na(range_match[1, 2])) {
      result$clay_low_pct <- as.numeric(range_match[1, 2])
      result$clay_high_pct <- as.numeric(range_match[1, 3])
    }
  }
  result
}

# ============================================================================
# TEXTURE HELPERS (extracted from closures)
# ============================================================================

#' @importFrom stringi stri_replace_all_regex stri_replace_first_regex
.ricExtractModifiers <- function(text_str) {
  text_lower <- tolower(text_str)
  found_modifiers <- character(0)
  # longer tokens first so "very gravelly" is consumed before "gravelly"
  modifier_order <- .RIC_TEXTURE_MODIFIERS_LC[order(nchar(.RIC_TEXTURE_MODIFIERS_LC), decreasing = TRUE)]
  for (mod in modifier_order) {
    if (stri_detect_fixed(text_lower, mod)) {
      found_modifiers <- c(found_modifiers, mod)
      text_lower <- stri_replace_all_fixed(text_lower, mod, " ")
    }
  }
  found_modifiers
}

.ricRemoveModifiers <- function(text_str) {
  text_clean <- tolower(stri_trim_both(text_str))
  text_clean <- stri_replace_all_regex(text_clean, "their\\s+([a-z\\s]+)\\s+analogues?", "$1")
  text_clean <- stri_replace_all_regex(text_clean, "or\\s+their\\s+([a-z\\s]+)\\s+analogues?", "or $1")
  for (mod in .RIC_TEXTURE_MODIFIERS_LC) {
    text_clean <- stri_replace_all_fixed(text_clean, mod, " ")
  }
  text_clean <- stri_replace_all_regex(text_clean, "\\s+", " ")
  stri_trim_both(text_clean)
}

.ricNormalizeTexture <- function(tx_name) {
  tx_lower <- tolower(stri_trim_both(tx_name))
  for (h_tx in .RIC_TEXTURE_HIERARCHY) {
    if (identical(tx_lower, tolower(h_tx))) return(h_tx)
  }
  for (p_tx in .RIC_TEXTURE_PATTERNS) {
    if (identical(tx_lower, tolower(p_tx))) return(p_tx)
  }
  NULL
}

.ricParseTextureWithMods <- function(raw_texture_str) {
  raw_str <- stri_trim_both(raw_texture_str)
  modifiers <- .ricExtractModifiers(raw_str)
  texture_clean <- .ricRemoveModifiers(raw_str)
  base_texture <- .ricNormalizeTexture(texture_clean)
  if (is.null(base_texture)) return(NULL)
  list(class = base_texture, modifiers = if (length(modifiers) > 0) modifiers else NULL, full_name = raw_str)
}

.ricInterpolateTextureRange <- function(start_raw, end_raw) {
  start_obj <- .ricParseTextureWithMods(start_raw)
  end_obj <- .ricParseTextureWithMods(end_raw)

  if (is.null(start_obj) || is.null(end_obj)) {
    result_textures <- list()
    result_interp <- c()
    if (!is.null(start_obj)) { result_textures[[1]] <- start_obj; result_interp <- c(result_interp, FALSE) }
    if (!is.null(end_obj) && is.null(start_obj)) { result_textures[[1]] <- end_obj; result_interp <- c(result_interp, FALSE) }
    else if (!is.null(end_obj)) { result_textures[[length(result_textures) + 1]] <- end_obj; result_interp <- c(result_interp, FALSE) }
    if (length(result_textures) > 0) return(list(textures = result_textures, interpolated = result_interp))
    return(NULL)
  }

  start_idx <- match(tolower(start_obj$class), tolower(.RIC_TEXTURE_HIERARCHY))
  end_idx <- match(tolower(end_obj$class), tolower(.RIC_TEXTURE_HIERARCHY))
  min_idx <- min(start_idx, end_idx)
  max_idx <- max(start_idx, end_idx)
  range_textures <- .RIC_TEXTURE_HIERARCHY[min_idx:max_idx]

  result_textures <- list()
  for (i in seq_along(range_textures)) {
    tx_class <- range_textures[i]
    if (i == 1) { mods <- start_obj$modifiers }
    else if (i == length(range_textures)) { mods <- end_obj$modifiers }
    else { mods <- NULL }
    result_textures[[i]] <- list(
      class = tx_class, modifiers = mods,
      full_name = if (!is.null(mods)) paste0(paste(mods, collapse = " "), " ", tx_class) else tx_class
    )
  }

  n <- length(result_textures)
  interpolated_flags <- rep(TRUE, n)
  interpolated_flags[1] <- FALSE
  interpolated_flags[n] <- FALSE
  list(textures = result_textures, interpolated = interpolated_flags)
}

.ricExtractTexturesFromText <- function(texture_str) {
  texture_str <- stri_replace_all_regex(texture_str, "^(?:from|ranging|ranges?)\\s+", "")
  texture_str <- stri_replace_all_regex(texture_str, "\\s+(?:ranges?|ranging)\\s+from\\s+", " ")
  texture_str <- stri_trim_both(texture_str)
  texture_str <- stri_replace_first_regex(texture_str, "\\s+(?:to|and|commonly|with)\\s+(?:coarser|finer|loamy|strata).*$", "")
  texture_str <- stri_replace_first_regex(texture_str, ",\\s*(?:commonly|with|and).*$", "")

  texture_items <- stri_split_regex(texture_str, ",\\s*|\\s+or\\s+|\\s+and\\s+")[[1]]
  result_textures <- list()
  for (item in texture_items) {
    item_clean <- stri_trim_both(item)
    item_lower <- tolower(item_clean)
    if (stri_detect_regex(item_lower, "^(?:a|the|with|commonly|which|fine-earth|fine earth|fraction|from|to)$")) next
    tx_obj <- .ricParseTextureWithMods(item_clean)
    if (!is.null(tx_obj)) result_textures[[length(result_textures) + 1]] <- tx_obj
  }
  if (length(result_textures) > 0) {
    return(list(textures = result_textures, interpolated = rep(FALSE, length(result_textures))))
  }
  NULL
}

# ============================================================================
# TEXTURE EXTRACTION
# ============================================================================

.extractRICTextures <- function(horizon_block) {
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(NULL)

  # Pattern 0: Simple "Textures are X or Y throughout" format
  simple_match <- stri_match_first_regex(horizon_block, "(?i)Textures?\\s+(?:are|can\\s+be)\\s+(.+?)(?:\\s+throughout|[,;.]|$)")
  if (!is.na(simple_match[1, 2])) {
    texture_str <- stri_trim_both(simple_match[1, 2])
    result <- .ricExtractTexturesFromText(texture_str)
    if (!is.null(result)) return(c(result, list(method = "simple_are_format")))
  }

  # Pattern 1: Range "ranges from X to Y" with interpolation
  range_match <- stri_match_first_regex(horizon_block, "(?i)ranges?\\s+from\\s+([^:,;.\\n]+?)\\s+to\\s+([^:,;.\\n]+?)(?:$|\\s+(?:in\\s+)?texture|\\s+material|[,;.])")
  if (!is.na(range_match[1, 2])) {
    start_raw <- stri_trim_both(range_match[1, 2])
    end_raw <- stri_trim_both(range_match[1, 3])
    result <- .ricInterpolateTextureRange(start_raw, end_raw)
    if (!is.null(result) && length(result$textures) > 0) return(c(result, list(method = "range_interpolation")))
  }

  # Pattern 2: Structured "Texture(s): X, Y, Z" or "Texture(s)--X"
  texture_line_match <- stri_match_first_regex(horizon_block, "(?i)(?:Fine-earth\\s+)?Textures?(?:\\s*\\([^)]*\\))?\\s*(?::|--?|--)\\s*(.+?)(?=\n[A-Z]|\\nRock|$)")
  if (!is.na(texture_line_match[1, 2])) {
    texture_str <- stri_trim_both(texture_line_match[1, 2])
    result <- .ricExtractTexturesFromText(texture_str)
    if (!is.null(result)) return(c(result, list(method = "explicit_texture_header")))
  }

  # Pattern 3: Narrative "The X horizon is [texture]" or "It is [texture]"
  narrative_patterns <- c(
    "(?i)The\\s+[A-Z]{1,3}\\s+horizon(?:\\s+or\\s+[A-Z]{1,3})?(?:\\s+is|\\s+are|\\s+can\\s+be)\\s+([^.!?]+)",
    "(?i)It\\s+(?:is|are|can\\s+be)\\s+([^.!?]+)"
  )
  for (np in narrative_patterns) {
    narrative_match <- stri_match_first_regex(horizon_block, np)
    if (!is.na(narrative_match[1, 2])) {
      texture_str <- stri_trim_both(narrative_match[1, 2])
      result <- .ricExtractTexturesFromText(texture_str)
      if (!is.null(result)) return(c(result, list(method = "narrative_format")))
    }
  }

  # Pattern 4: "Texture of the fine-earth fraction is/ranges [X to Y]"
  fea_match <- stri_match_first_regex(horizon_block, "(?i)Texture\\s+of\\s+the\\s+fine-earth\\s+fraction\\s+(?:is|ranges)\\s+([^.]+)")
  if (!is.na(fea_match[1, 2])) {
    texture_str <- stri_trim_both(fea_match[1, 2])
    result <- .ricExtractTexturesFromText(texture_str)
    if (!is.null(result)) return(c(result, list(method = "fine_earth_fraction")))
  }

  NULL
}

# ============================================================================
# FRAGMENT HELPERS AND EXTRACTION
# ============================================================================

.ricPrioritizeFragmentClass <- function(classes) {
  if (length(classes) == 0) return(NULL)
  if (length(classes) == 1) return(classes[1])
  priority_order <- .RIC_FRAGMENT_CLASSES[order(nchar(.RIC_FRAGMENT_CLASSES), decreasing = TRUE)]
  for (frag_class in priority_order) {
    if (frag_class %in% tolower(classes)) {
      matching_idx <- which(tolower(classes) == frag_class)[1]
      return(classes[matching_idx])
    }
  }
  classes[1]
}

.ricExtractFragmentKinds <- function(horizon_block) {
  fragment_kinds <- c("granite", "limestone", "basalt", "sandstone", "quartzite", "schist", "gneiss",
                      "ironstone", "gravel", "gravels", "cobbles", "stones", "boulders")
  text_lower <- tolower(horizon_block)
  found_kinds <- c()
  for (fkind in fragment_kinds) {
    if (stri_detect_fixed(text_lower, fkind)) found_kinds <- c(found_kinds, fkind)
  }
  unique(found_kinds)
}

.ricExtractFragmentPct <- function(horizon_block) {
  result <- list(low_pct = NULL, high_pct = NULL)

  pct_match <- stri_match(horizon_block, regex = "(?i)(?:rock\\s+)?fragment(?:\\s+content)?s?\\s*(?::|--?|--)\\s*(\\d+)\\s*(?:to|-|through)\\s*(\\d+)\\s*(?:percent|%)")
  if (!is.na(pct_match[1, 2])) { result$low_pct <- as.numeric(pct_match[1, 2]); result$high_pct <- as.numeric(pct_match[1, 3]); return(result) }

  pct_match <- stri_match(horizon_block, regex = "(?i)(?:volume\\s+of\\s+)?rock\\s+fragments?\\s+(?:ranges?|vary|varies)\\s+from\\s+(\\d+)\\s+(?:to|-|through)\\s+(\\d+)\\s*(?:percent|%)")
  if (!is.na(pct_match[1, 2])) { result$low_pct <- as.numeric(pct_match[1, 2]); result$high_pct <- as.numeric(pct_match[1, 3]); return(result) }

  pct_match <- stri_match(horizon_block, regex = "(?i)(?:coarse|rock)\\s+fragment(?:\\s+content)?s?\\s*(?::|--?|--)\\s*(\\d+)\\s*(?:to|-|through)\\s*(\\d+)\\s*(?:percent|%)")
  if (!is.na(pct_match[1, 2])) { result$low_pct <- as.numeric(pct_match[1, 2]); result$high_pct <- as.numeric(pct_match[1, 3]); return(result) }

  pct_match <- stri_match(horizon_block, regex = "(\\d+)\\s+(?:to|-|through)\\s+(\\d+)\\s*(?:percent|%)\\s+(?:gravel|cobbles|stones|boulders)")
  if (!is.na(pct_match[1, 2])) { result$low_pct <- as.numeric(pct_match[1, 2]); result$high_pct <- as.numeric(pct_match[1, 3]); return(result) }

  desc_match <- stri_match(horizon_block, regex = "(?i)(?:as\\s+much\\s+as|up\\s+to|may\\s+be|approximately)\\s+(\\d+)\\s*(?:percent|%)")
  if (!is.na(desc_match[1, 2])) { result$low_pct <- 0; result$high_pct <- as.numeric(desc_match[1, 2]); return(result) }

  result
}

.extractRICFragments <- function(horizon_block) {
  result <- list(class = NULL, kind = NULL, low_pct = NULL, high_pct = NULL)
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(result)

  text_lower <- tolower(horizon_block)
  # Use NASIS-authoritative fragment class list sorted by specificity (longest first)
  fragment_classes <- .RIC_FRAGMENT_CLASSES[order(nchar(.RIC_FRAGMENT_CLASSES), decreasing = TRUE)]
  found_classes <- c()
  for (fclass in fragment_classes) {
    if (stri_detect_fixed(text_lower, fclass)) found_classes <- c(found_classes, fclass)
  }
  if (length(found_classes) > 0) result$class <- .ricPrioritizeFragmentClass(found_classes)

  all_kinds <- .ricExtractFragmentKinds(horizon_block)
  if (length(all_kinds) > 0) {
    if ("gravels" %in% all_kinds && "gravel" %in% all_kinds) all_kinds <- all_kinds[all_kinds != "gravels"]
    result$kind <- ifelse(length(all_kinds) == 1, all_kinds[1], paste(all_kinds, collapse = ", "))
  }

  pct_data <- .ricExtractFragmentPct(horizon_block)
  result$low_pct <- pct_data$low_pct
  result$high_pct <- pct_data$high_pct
  result
}

# ============================================================================
# STRUCTURE EXTRACTION
# ============================================================================

.extractRICStructure <- function(horizon_block) {
  result <- list(structure_grade = NULL, structure_size = NULL, structure_type = NULL)
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(result)

  grades <- c("weak", "moderate", "strong")
  sizes <- c("very fine", "fine", "medium", "coarse", "very coarse")
  types <- c("subangular blocky", "angular blocky", "blocky", "platy", "prismatic", "columnar", "granular")
  special_types <- c("massive", "single grain")

  grade_rx <- paste(grades, collapse = "|")
  size_rx <- paste(sizes, collapse = "|")
  type_rx <- paste(c(types, special_types), collapse = "|")
  all_keywords_rx <- paste(c(grades, sizes, types, special_types), collapse = "|")

  structure_text <- NULL

  # Pattern 1: "Structure is/has/:/- CONTENT"
  p1 <- paste0("(?i)(?:structure\\s+(?:is|has|was|ranges?)\\s+|structure\\s*[-:]\\s*)(.{3,80})")
  m1 <- stri_match(horizon_block, regex = p1)
  if (!is.na(m1[1, 2])) {
    candidate <- tolower(stri_trim_both(m1[1, 2]))
    candidate <- stri_replace_first_regex(candidate, "\\.\\s.*$", "")
    candidate <- stri_replace_first_regex(candidate, "\\.$", "")
    if (stri_detect_regex(candidate, paste0("(?i)", all_keywords_rx))) structure_text <- candidate
  }

  # Pattern 2: "CONTENT structure" (postfix)
  if (is.null(structure_text)) {
    p2 <- paste0("(?i)((?:", grade_rx, "|", type_rx, ").{0,50}?)\\s+structure\\b")
    m2 <- stri_match(horizon_block, regex = p2)
    if (!is.na(m2[1, 2])) structure_text <- tolower(stri_trim_both(m2[1, 2]))
  }

  # Pattern 3: Standalone special types
  if (is.null(structure_text)) {
    if (stri_detect_regex(horizon_block, "(?i)\\bmassive\\b")) structure_text <- "massive"
    else if (stri_detect_regex(horizon_block, "(?i)\\bsingle\\s+grain\\b")) structure_text <- "single grain"
  }

  if (!is.null(structure_text)) {
    for (grade in grades) {
      if (stri_detect_fixed(structure_text, grade)) { result$structure_grade <- grade; break }
    }
    for (size in rev(sizes)) {
      if (stri_detect_fixed(structure_text, size)) { result$structure_size <- size; break }
    }
    for (type in types) {
      if (stri_detect_fixed(structure_text, type)) { result$structure_type <- type; break }
    }
    if (is.null(result$structure_type)) {
      if (stri_detect_regex(structure_text, "(?i)\\bmassive\\b")) result$structure_type <- "massive"
      else if (stri_detect_regex(structure_text, "(?i)\\bsingle\\s+grain\\b")) result$structure_type <- "single grain"
    }
  }
  result
}

# ============================================================================
# BOUNDARY EXTRACTION
# ============================================================================

.extractRICBoundaries <- function(horizon_block) {
  result <- list(boundary_distinctness = NULL, boundary_topography = NULL)
  if (!is.character(horizon_block) || nchar(horizon_block) == 0) return(result)

  distinctness_classes <- c("abrupt", "clear", "gradual", "diffuse")
  topography_classes <- c("smooth", "wavy", "irregular", "broken")

  boundary_pattern <- paste0(
    "(?i)(?:boundary|contact).*?(", paste(distinctness_classes, collapse = "|"),
    ")\\s+(?:", paste(topography_classes, collapse = "|"), ")")

  boundary_match <- stri_match(horizon_block, regex = boundary_pattern)
  if (!is.na(boundary_match[1, 1])) {
    bt <- tolower(boundary_match[1, 1])
    for (dist in distinctness_classes) {
      if (stri_detect_fixed(bt, dist)) { result$boundary_distinctness <- dist; break }
    }
    for (topo in topography_classes) {
      if (stri_detect_fixed(bt, topo)) { result$boundary_topography <- topo; break }
    }
  }
  result
}

# ============================================================================
# PREAMBLE AND GENERAL TEXT
# ============================================================================

#' @importFrom stringi stri_locate_first_regex
.extractRICPreamble <- function(ric_text) {
  if (!is.character(ric_text) || nchar(ric_text) == 0) return("")

  patterns <- c(
    "(?i)(?:^|\n)(\\s*The\\s+[0-9]*[A-Z]+[a-z0-9]*(?:\\s*,\\s*[0-9]*[A-Z]+[a-z0-9]*)*(?:\\s+and\\s+[0-9]*[A-Z]+[a-z0-9]*)?\\s+horizons?)",
    "(?i)(?:^|\n)(\\s*The\\s+[0-9]*[A-Z]+[a-z0-9]*(?:\\s+or\\s+[0-9]*[A-Z]+[a-z0-9]*)?\\s+horizon)",
    "(?i)(?:^|\n)(\\s*[A-Z]{1,3}(?:\\s+or\\s+[A-Z]{1,3})?\\s+horizon(?:\\s|:|,|$))",
    "(?i)(?:^|\n)(\\s*where\\s+present)",
    "(?i)(?:^|\n)(\\s*\\d+[A-Z])",
    "(?i)^[A-Z]{1,3}(?:\\s+or\\s+[A-Z]{1,3})? horizon"
  )

  first_hz_match_pos <- NA
  for (pattern in patterns) {
    match_result <- stri_locate_first_regex(ric_text, pattern, opts_regex = list(multiline = TRUE))
    if (!is.na(match_result[1, 1])) {
      if (is.na(first_hz_match_pos) || match_result[1, 1] < first_hz_match_pos) {
        first_hz_match_pos <- match_result[1, 1]
      }
    }
  }

  if (is.na(first_hz_match_pos)) return(ric_text)
  stri_sub(ric_text, 1, first_hz_match_pos - 1)
}

.extractRICGeneralText <- function(ric_text) {
  if (!is.character(ric_text) || nchar(ric_text) == 0) return(character(0))

  pattern_first_hz <- "(?i)(?:^|\n)(\\s*The\\s+[0-9]*[A-Z]+[a-z0-9]*(?:\\s*,\\s*[0-9]*[A-Z]+[a-z0-9]*)*(?:\\s+and\\s+[0-9]*[A-Z]+[a-z0-9]*)?\\s+horizons?|^\\s*[A-Z]{1,3}\\s+horizon)"
  first_hz_match <- stri_locate_first_regex(ric_text, pattern_first_hz, opts_regex = list(multiline = TRUE))

  if (is.na(first_hz_match[1, 1])) {
    sentences <- stri_split_regex(ric_text, "(?<=[.!?])\\s+")[[1]]
    return(sentences[nchar(sentences) > 0])
  }

  preamble <- stri_sub(ric_text, 1, first_hz_match[1, 1] - 1)
  preamble <- stri_trim_both(preamble)
  if (nchar(preamble) == 0) return(character(0))

  sentences <- stri_split_regex(preamble, "(?<=[.!?])\\s+")[[1]]
  sentences <- stri_trim_both(sentences)
  sentences[nchar(sentences) > 0]
}

# ============================================================================
# DEFAULT COLOR STATE
# ============================================================================

.extractRICDefaultColorState <- function(typical_pedon_header) {
  if (is.null(typical_pedon_header) || !is.character(typical_pedon_header) ||
      length(typical_pedon_header) == 0 || is.na(typical_pedon_header[1]) ||
      nchar(typical_pedon_header) == 0) return("moist")

  tp_lower <- tolower(typical_pedon_header)
  if (stri_detect_regex(tp_lower, "for[ \\t]+(air-*\\s*)?dry[ied]*[ \\t,\\n]+(colors|soil|conditions)")) return("dry")
  if (stri_detect_regex(tp_lower, "for[ \\t]+(wet|moi*st)[ \\t,\\n]+(rubbed|crushed|broken|interior|soil|conditions)")) return("moist")
  "moist"
}

# ============================================================================
# PSCS EXTRACTION — Phase 1 (Clay + Rock Fragments)
# ============================================================================

.extractRICPSCS1 <- function(ric_text) {
  pscs <- list()
  if (!is.character(ric_text) || length(ric_text) == 0 || nchar(ric_text) == 0) return(pscs)

  # Clay content
  clay_patterns <- c(
    "(?i)clay\\s+content.*?--\\s*([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)clay\\s+content\\s*:\\s*(?:averages?\\s+)?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)clay\\s+content\\s+(?:in|of)\\s+the.*?control\\s+section.*?([0-9]+)\\s+(?:percent|%).*?(?:to|or)\\s+(?:about\\s+)?([0-9]+)\\s*(?:percent|%)",
    "(?i)weighted\\s+average\\s+clay\\s+content.*?([0-9]+)\\s+(?:to|or)\\s+(?:about\\s+)?([0-9]+)\\s*(?:percent|%)"
  )
  for (pattern in clay_patterns) {
    clay_match <- stri_match_first_regex(ric_text, pattern)
    if (!is.na(clay_match[1, 2]) && !is.na(clay_match[1, 3])) {
      cl <- as.numeric(clay_match[1, 2]); ch <- as.numeric(clay_match[1, 3])
      if (cl >= 0 && ch <= 100 && cl <= ch) {
        pscs$clay_low_pct <- cl; pscs$clay_high_pct <- ch; break
      }
    }
  }

  # Rock fragments
  rf_patterns <- c(
    "(?i)rock\\s+fragments?\\s+-\\s+([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)rock\\s+fragments?\\s+(?:in\\s+)?(?:the\\s+)?(?:particle(?:\\s+size)?\\s+)?control\\s+section.*?range\\s+from\\s+([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)rock\\s+fragments?.*?(?:particle(?:\\s+size)?\\s+)?control\\s+section.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)weighted\\s+average\\s+rock\\s+fragments?.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)(?:particle-size|particle\\s+size)\\s+control\\s+section\\s+contains.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)\\s+(?:rock\\s+fragments?|coarse\\s+fragments?)",
    "(?i)coarse\\s+fragments?.*?(?:particle(?:\\s+size)?\\s+)?control\\s+section.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)(?:gravel|coarse\\s+fragments?)\\s+(?:or\\s+channers?\\s+)?(?:in\\s+)?(?:the\\s+)?control\\s+section.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)"
  )
  for (pattern in rf_patterns) {
    rf_match <- stri_match_first_regex(ric_text, pattern)
    if (!is.na(rf_match[1, 1]) && !is.na(rf_match[1, 2]) && !is.na(rf_match[1, 3])) {
      pscs$rock_fragments_low_pct <- as.numeric(rf_match[1, 2])
      pscs$rock_fragments_high_pct <- as.numeric(rf_match[1, 3])
      break
    }
  }

  if (is.null(pscs$rock_fragments_low_pct)) {
    rf_onegroup <- "(?i)weighted\\s+average.*?rock\\s+fragments?.*?(?:less\\s+than|is)\\s+([0-9]+)\\s*(?:percent|%)"
    rf_match_one <- stri_match_first_regex(ric_text, rf_onegroup)
    if (!is.na(rf_match_one[1, 1]) && !is.na(rf_match_one[1, 2])) {
      pscs$rock_fragments_low_pct <- 0
      pscs$rock_fragments_high_pct <- as.numeric(rf_match_one[1, 2])
    }
  }

  if (!is.null(pscs$rock_fragments_low_pct) && !is.null(pscs$rock_fragments_high_pct)) {
    kind_pattern <- "(?i)(?:gravel|cobbles?|stones?|pumiceous|pumice|coarse\\s+fragments?)(?:\\s+and\\s+(?:cobbles?|stones?))?"
    kind_match <- stri_extract_first_regex(ric_text, kind_pattern)
    if (!is.na(kind_match) && length(kind_match) > 0) {
      pscs$rock_fragments_kind <- tolower(stri_trim_both(kind_match))
    }
  }
  pscs
}

# PSCS Phase 2 (Sand + Silt+VFS)
.extractRICPSCS2 <- function(ric_text) {
  pscs <- list()
  if (!is.character(ric_text) || nchar(ric_text) == 0) return(pscs)

  sand_patterns <- c(
    "(?i)sand\\s+content\\s+.*?(?:particle[-\\s]*size)?\\s*control\\s+section.*?(?:weighted\\s+average)?\\s*(?:--|:|\\s)\\s*([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)",
    "(?i)sand\\s+content\\s+.*?control\\s+section.*?([0-9]+)\\s+(?:to|or)\\s+([0-9]+)\\s*(?:percent|%)"
  )
  for (pattern in sand_patterns) {
    sand_match <- stri_match_first_regex(ric_text, pattern)
    if (!is.na(sand_match[1, 1]) && !is.na(sand_match[1, 2]) && !is.na(sand_match[1, 3])) {
      sl <- as.numeric(sand_match[1, 2]); sh <- as.numeric(sand_match[1, 3])
      if (sl >= 0 && sh <= 100 && sl <= sh) { pscs$sand_low_pct <- sl; pscs$sand_high_pct <- sh; break }
    }
  }

  if (is.null(pscs$sand_low_pct)) {
    sand_onegroup <- "(?i)sand\\s+content\\s+.*?control\\s+section.*?(?:less\\s+than|is)\\s+([0-9]+)\\s*(?:percent|%)"
    sand_match_one <- stri_match_first_regex(ric_text, sand_onegroup)
    if (!is.na(sand_match_one[1, 1]) && !is.na(sand_match_one[1, 2])) {
      sh <- as.numeric(sand_match_one[1, 2])
      if (sh <= 100) { pscs$sand_low_pct <- 0; pscs$sand_high_pct <- sh }
    }
  }

  svfs_patterns <- c("(?i)silt\\s+plus\\s+very\\s+fine\\s+sand.*?([0-9]+)\\s+(?:to|or|through)\\s+([0-9]+)\\s*(?:percent|%)")
  for (pattern in svfs_patterns) {
    svfs_match <- stri_match_first_regex(ric_text, pattern)
    if (!is.na(svfs_match[1, 1]) && !is.na(svfs_match[1, 2]) && !is.na(svfs_match[1, 3])) {
      sl <- as.numeric(svfs_match[1, 2]); sh <- as.numeric(svfs_match[1, 3])
      if (sl >= 0 && sh <= 100 && sl <= sh) { pscs$silt_vfs_low_pct <- sl; pscs$silt_vfs_high_pct <- sh; break }
    }
  }

  if (is.null(pscs$silt_vfs_low_pct)) {
    svfs_morethan <- "(?i)(?:more\\s+than|contains?.*?more\\s+than|averages?.*?)\\s+([0-9]+)\\s*(?:percent|%)\\s+silt\\s+plus\\s+very\\s+fine\\s+sand"
    svfs_match_mt <- stri_match_first_regex(ric_text, svfs_morethan)
    if (!is.na(svfs_match_mt[1, 1]) && !is.na(svfs_match_mt[1, 2])) {
      val <- as.numeric(svfs_match_mt[1, 2])
      if (val >= 0 && val <= 100) { pscs$silt_vfs_low_pct <- val; pscs$silt_vfs_high_pct <- 100 }
    }
  }
  pscs
}

# ============================================================================
# PSCS CHEMICAL PROPERTIES
# ============================================================================

.extractRICPSCSChemical <- function(preamble_text) {
  if (!is.character(preamble_text) || nchar(preamble_text) == 0) return(list())
  chemical <- list()

  cec_patterns <- c(
    "(?i)CEC\\s*(?:of the control section)?\\s*(?:is|:)\\s*([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)\\s*meg/100g",
    "(?i)Cation\\s+exchange\\s+capacity.*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)",
    "(?i)weighted\\s+average\\s+CEC\\s*(?:is|:)\\s*([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)"
  )
  for (pattern in cec_patterns) {
    cec_match <- stri_match_first_regex(preamble_text, pattern)
    if (!is.na(cec_match[1, 1]) && ncol(cec_match) >= 3 && !is.na(cec_match[1, 2]) && !is.na(cec_match[1, 3])) {
      chemical$cec_low_meq_100g <- as.numeric(cec_match[1, 2])
      chemical$cec_high_meq_100g <- as.numeric(cec_match[1, 3])
      break
    }
  }

  cole_patterns <- c(
    "(?i)Linear\\s+extensibility.*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)\\s*cm",
    "(?i)COLE\\s*(?:is|:)?\\s*([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)"
  )
  for (pattern in cole_patterns) {
    cole_match <- stri_match_first_regex(preamble_text, pattern)
    if (!is.na(cole_match[1, 1]) && ncol(cole_match) >= 3 && !is.na(cole_match[1, 2]) && !is.na(cole_match[1, 3])) {
      chemical$cole_low <- as.numeric(cole_match[1, 2])
      chemical$cole_high <- as.numeric(cole_match[1, 3])
      break
    }
  }

  oc_patterns <- c(
    "(?i)Organic\\s+(?:carbon|matter)\\s+content.*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)\\s*(?:percent|%)",
    "(?i)Organic\\s+carbon.*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)"
  )
  for (pattern in oc_patterns) {
    oc_match <- stri_match_first_regex(preamble_text, pattern)
    if (!is.na(oc_match[1, 1]) && ncol(oc_match) >= 3 && !is.na(oc_match[1, 2]) && !is.na(oc_match[1, 3])) {
      chemical$organic_carbon_low_pct <- as.numeric(oc_match[1, 2])
      chemical$organic_carbon_high_pct <- as.numeric(oc_match[1, 3])
      break
    }
  }

  glau_patterns <- c(
    "(?i)Glauconite\\s+content.*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+)\\s*(?:percent|%)",
    "(?i)(?:contains|has).*?([0-9.]+)\\s+(?:to|\u2013)\\s+([0-9.]+).*?glauconite"
  )
  for (pattern in glau_patterns) {
    glau_match <- stri_match_first_regex(preamble_text, pattern)
    if (!is.na(glau_match[1, 1]) && ncol(glau_match) >= 3 && !is.na(glau_match[1, 2]) && !is.na(glau_match[1, 3])) {
      chemical$glauconite_low_pct <- as.numeric(glau_match[1, 2])
      chemical$glauconite_high_pct <- as.numeric(glau_match[1, 3])
      break
    }
  }
  chemical
}

# ============================================================================
# PROPERTY NORMALIZATION HELPERS
# ============================================================================

.ricPropertyPatterns <- function() {
  list(
    list(patterns = c("depth to bedrock", "depth bedrock", "depth.*?bedrock"), key = "depth_to_bedrock"),
    list(patterns = c("depth to water table", "depth.*?water table"), key = "depth_to_water_table"),
    list(patterns = c("depth to carbonates?", "depth.*?carbonate"), key = "depth_to_carbonates"),
    list(patterns = c("depth to densic", "depth.*?densic"), key = "depth_to_densic_contact"),
    list(patterns = c("depth to paralithic", "depth.*?paralithic"), key = "depth_to_paralithic"),
    list(patterns = c("depth to lithic", "depth.*?lithic"), key = "depth_to_lithic"),
    list(patterns = c("solum thickness", "solum.*?thickness"), key = "solum_thickness"),
    list(patterns = c("clay content", "clay.*?content"), key = "clay_content_pscs"),
    list(patterns = c("sand content", "sand.*?content"), key = "sand_content_pscs"),
    list(patterns = c("silt content", "silt.*?content"), key = "silt_content_pscs"),
    list(patterns = c("rock fragment", "coarse fragment", "gravel content"), key = "rock_fragment_content"),
    list(patterns = c("cation exchange capacity", "cec", "ammonium acetate"), key = "cec_nh4oa"),
    list(patterns = c("cole", "linear extensibility coefficient"), key = "cole"),
    list(patterns = c("organic matter", "organic carbon"), key = "organic_matter"),
    list(patterns = c("calcium carbonate", "caco3", "lime requirement"), key = "calcium_carbonate"),
    list(patterns = c("gypsum", "soluble salts"), key = "gypsum_content"),
    list(patterns = c("mean annual soil temperature", "mast"), key = "mean_annual_soil_temperature"),
    list(patterns = c("soil moisture", "moisture regime"), key = "soil_moisture_regime"),
    list(patterns = c("bulk density"), key = "bulk_density"),
    list(patterns = c("vertic crack"), key = "vertic_cracks"),
    list(patterns = c("slickenside"), key = "slickensides"),
    list(patterns = c("redoximorphic", "redox features", "mottles"), key = "redox_features"),
    list(patterns = c("dominant hue", "hue range"), key = "dominant_hue"),
    list(patterns = c("mica content", "mica mineral"), key = "mica_content"),
    list(patterns = c("glauconite"), key = "glauconite_content"),
    list(patterns = c("albic"), key = "albic_properties")
  )
}

.ricNormalizePropName <- function(raw_name) {
  raw_lower <- tolower(stri_trim_both(raw_name))
  # First try pattern matching for predefined properties
  for (pattern_set in .ricPropertyPatterns()) {
    for (pattern in pattern_set$patterns) {
      if (stri_detect_regex(raw_lower, pattern, opts_regex = list(case_insensitive = TRUE))) {
        return(pattern_set$key)
      }
    }
  }
  # Aggressive normalization for unknown properties
  normalized <- tolower(raw_name)
  # Remove units and measurement qualifiers at the end
  normalized <- stri_replace_all_regex(normalized, "\\s*\\(?(?:degrees?\\s*[FC]|°[CF]|percent|%|inches?|cm|feet|meters?|kg)\\)?\\s*$", "")
  # Normalize "average" to "mean"
  normalized <- stri_replace_all_regex(normalized, "\\baverage\\b", "mean")
  # Remove leading articles and connector words
  normalized <- stri_replace_all_regex(normalized, "^(?:and\\s+the\\s+|and\\s+|the\\s+)", "")
  # Remove trailing location/context qualifiers
  normalized <- stri_replace_all_regex(normalized, "\\s+(?:at\\s+the|in\\s+the|for\\s+the|in\\s+).+$", "")
  # Convert to snake_case
  normalized <- stri_replace_all_regex(normalized, "[^a-z0-9\\s]", "")
  normalized <- stri_trim_both(normalized)
  normalized <- stri_replace_all_regex(normalized, "\\s+", "_")
  normalized
}

.ricClassifyValueType <- function(value_text) {
  value_lower <- tolower(value_text)
  if (stri_detect_regex(value_text, "(?i)present|absent|yes|no|true|false")) return("boolean")
  if (stri_detect_regex(value_text, "(?i)greater than|more than|less than|<|>|\u2013|-")) return("open_ended_numeric")
  if (stri_detect_regex(value_text, "\\d+.*?(?:to|\u2013|-|and).*?\\d+")) return("numeric_range")
  if (stri_detect_regex(value_text, "^\\d+(\\.\\d+)?\\s*(?:percent|%|inches?|cm|\u00b0[CF]|mmhos|ppm|meq|db|kg|m)")) return("numeric_single")
  if (stri_detect_regex(value_text, "(?i)percent|%")) return("percentage")
  if (stri_detect_regex(value_text, "(?i)degree|\u00b0[CF]")) return("temperature")
  if (stri_detect_regex(value_text, "(?i)feet|inches|cm|meter")) return("depth")
  "qualitative"
}

.ricExtractNumericRange <- function(value_text) {
  result <- list(low = NULL, high = NULL, unit = NULL, qualifier = NULL)

  temp_match <- stri_match_first_regex(value_text, "(?i)degrees?\\s*([FC])|\u00b0([CF])|\\b([FC])\\b(?=\\s|$|\\.)")
  matched_temp <- temp_match[1, 1]
  if (!is.na(matched_temp)) {
    temp_unit <- stri_trim_both(temp_match[1, 2])
    if (is.na(temp_unit) || nchar(temp_unit) == 0) temp_unit <- stri_trim_both(temp_match[1, 3])
    if (is.na(temp_unit) || nchar(temp_unit) == 0) temp_unit <- stri_trim_both(temp_match[1, 4])
    if (!is.na(temp_unit) && nchar(temp_unit) > 0) result$unit <- paste0(temp_unit, "\u00b0")
  } else {
    unit_match <- stri_match_first_regex(value_text, "(?i)(percent|%|inches?|cm|mmhos|ppm|meq|kg|db|grams?|g/cc)")
    if (!is.na(unit_match[1, 2])) result$unit <- unit_match[1, 2]
  }

  qualifier_match <- stri_match_first_regex(value_text, "(?i)(greater than|more than|less than|<|>|about|approximately|commonly)")
  if (!is.na(qualifier_match[1, 2])) result$qualifier <- qualifier_match[1, 2]

  range_match <- stri_match_first_regex(value_text, "(\\d+(?:\\.\\d+)?)[^0-9]+(\\d+(?:\\.\\d+)?)")
  if (!is.na(range_match[1, 2])) {
    result$low <- as.numeric(range_match[1, 2])
    result$high <- as.numeric(range_match[1, 3])
  } else {
    single_match <- stri_match_first_regex(value_text, "(\\d+(?:\\.\\d+)?)")
    if (!is.na(single_match[1, 2])) {
      value <- as.numeric(single_match[1, 2])
      if (!is.null(result$qualifier) && stri_detect_regex(result$qualifier, "(?i)greater|more|>")) {
        result$low <- value
      } else if (!is.null(result$qualifier) && stri_detect_regex(result$qualifier, "(?i)less|<")) {
        result$high <- value
      } else {
        result$low <- value
      }
    }
  }
  result
}

# ============================================================================
# NARRATIVE PROPERTIES
# ============================================================================

#' @importFrom stringi stri_match_all_regex
.extractRICNarrativeProps <- function(preamble_text) {
  if (!is.character(preamble_text) || nchar(preamble_text) == 0) return(list())
  properties <- list()

  # Pattern 1: "Property ranges from X to Y"
  ranges_from_pattern <- "([A-Za-z\\s&()]+?)\\s+ranges?\\s+from\\s+(.+?)\\s+to\\s+(.+?)(?:\\.|$)"
  matches_ranges <- stri_match_all_regex(preamble_text, ranges_from_pattern,
                                         opts_regex = list(case_insensitive = TRUE))[[1]]
  if (!is.na(matches_ranges[1, 1])) {
    for (i in 1:nrow(matches_ranges)) {
      prop_name <- stri_trim_both(matches_ranges[i, 2])
      range_start <- stri_trim_both(matches_ranges[i, 3])
      range_end <- stri_trim_both(matches_ranges[i, 4])
      if (prop_name %in% names(properties)) next
      if (nchar(prop_name) < 3 || nchar(prop_name) > 80) next
      if (stri_detect_regex(prop_name, "^[A-Z\\s]+$")) next
      if (stri_detect_regex(tolower(prop_name), "^(inches?|feet|foot|cm|centimeters?|mm|percent)$")) next

      numeric_data <- list(low = NA, high = NA, unit = NA)
      start_match <- stri_match_first_regex(range_start, "(\\d+(?:\\.\\d+)?)")
      if (!is.na(start_match[1, 2])) numeric_data$low <- as.numeric(start_match[1, 2])

      end_match <- stri_match_first_regex(range_end, "(\\d+(?:\\.\\d+)?)")
      if (!is.na(end_match[1, 2])) numeric_data$high <- as.numeric(end_match[1, 2])

      temp_match <- stri_match_first_regex(range_end, "(?i)degrees?\\s+([FC])|\u00b0([CF])|\\b([FC])\\b")
      if (!is.na(temp_match[1, 1])) {
        temp_unit <- stri_trim_both(temp_match[1, 2])
        if (is.na(temp_unit) || nchar(temp_unit) == 0) temp_unit <- stri_trim_both(temp_match[1, 3])
        if (is.na(temp_unit) || nchar(temp_unit) == 0) temp_unit <- stri_trim_both(temp_match[1, 4])
        if (!is.na(temp_unit) && nchar(temp_unit) > 0) numeric_data$unit <- paste0(temp_unit, "\u00b0")
      } else {
        unit_match <- stri_match_first_regex(range_end, "((?:inches?|centimeters?|cm|feet|mm|percent|%)\\b)")
        if (!is.na(unit_match[1, 2])) {
          unit_raw <- stri_trim_both(unit_match[1, 2])
          numeric_data$unit <- if (stri_detect_regex(unit_raw, "(?i)cm|centimeter")) "cm"
            else if (stri_detect_regex(unit_raw, "(?i)inch")) "inches"
            else if (stri_detect_regex(unit_raw, "(?i)feet|foot")) "feet"
            else if (stri_detect_regex(unit_raw, "(?i)percent|%")) "percent"
            else unit_raw
        }
      }

      if (!is.na(numeric_data$low) || !is.na(numeric_data$high)) {
        prop_name_normalized <- .ricNormalizePropName(prop_name)
        properties[[prop_name]] <- list(
          label_original = prop_name, label_normalized = prop_name_normalized,
          value_type = "numeric_range",
          value_raw = paste(range_start, "to", range_end),
          source_text = paste(prop_name, "ranges from", range_start, "to", range_end),
          low = numeric_data$low, high = numeric_data$high, unit = numeric_data$unit,
          extraction_method = "narrative_ranges_from"
        )
      }
    }
  }

  # Pattern 2: "Property is X to Y" with support for temperature units
  is_pattern <- "([A-Za-z\\s&()]+?)\\s+is\\s+([0-9.,\\s]+\\s+(?:to|through)\\s+[0-9.,\\s]+(?:degrees?\\s*[FC]|percent|%|inches?|cm|feet)?)"
  matches_is <- stri_match_all_regex(preamble_text, is_pattern,
                                     opts_regex = list(case_insensitive = TRUE))[[1]]
  if (!is.na(matches_is[1, 1])) {
    for (i in 1:nrow(matches_is)) {
      prop_name <- stri_trim_both(matches_is[i, 2])
      range_value <- stri_trim_both(matches_is[i, 3])
      if (prop_name %in% names(properties)) next
      if (nchar(prop_name) < 3 || nchar(prop_name) > 80) next

      numeric_data <- list(low = NA, high = NA, unit = NA)
      split_range <- stri_split_regex(range_value, "\\s+(?:to|through)\\s+")[[1]]
      if (length(split_range) >= 2) {
        start_match <- stri_match_first_regex(split_range[1], "(\\d+(?:\\.\\d+)?)")
        if (!is.na(start_match[1, 2])) numeric_data$low <- as.numeric(start_match[1, 2])
        # Extract number AND any trailing unit/text
        end_match <- stri_match_first_regex(split_range[2], "(\\d+(?:\\.\\d+)?)(?:\\s+(.+?))?$")
        if (!is.na(end_match[1, 2])) numeric_data$high <- as.numeric(end_match[1, 2])
        if (!is.na(end_match[1, 3])) {
          unit_raw <- stri_trim_both(end_match[1, 3])
          # Check for temperature units
          temp_match <- stri_match_first_regex(unit_raw, "(?i)degrees?\\s*([FC])|°([CF])")
          if (!is.na(temp_match[1, 1])) {
            temp_letter <- stri_trim_both(temp_match[1, 2])
            if (is.na(temp_letter) || nchar(temp_letter) == 0) temp_letter <- stri_trim_both(temp_match[1, 3])
            if (!is.na(temp_letter) && nchar(temp_letter) > 0) numeric_data$unit <- paste0(temp_letter, "°")
          } else if (stri_detect_regex(unit_raw, "(?i)percent|%")) {
            numeric_data$unit <- "percent"
          } else if (stri_detect_regex(unit_raw, "(?i)inch")) {
            numeric_data$unit <- "inches"
          } else if (stri_detect_regex(unit_raw, "(?i)cm|centimeter")) {
            numeric_data$unit <- "cm"
          }
        }
      }

      if (!is.na(numeric_data$low) || !is.na(numeric_data$high)) {
        prop_name_normalized <- .ricNormalizePropName(prop_name)
        properties[[prop_name]] <- list(
          label_original = prop_name, label_normalized = prop_name_normalized,
          value_type = "numeric_range", value_raw = range_value,
          source_text = paste(prop_name, "is", range_value),
          low = numeric_data$low, high = numeric_data$high, unit = numeric_data$unit,
          extraction_method = "narrative_is_value"
        )
      }
    }
  }
  properties
}

# ============================================================================
# SERIES PROPERTIES (flexible preamble extraction)
# ============================================================================

#' @importFrom stringi stri_replace_first_fixed
.extractRICSeriesProps <- function(preamble_text) {
  if (!is.character(preamble_text) || nchar(preamble_text) == 0) return(list())

  properties <- list()
  lines <- stri_split_regex(preamble_text, "\n")[[1]]
  current_property <- NULL
  current_value <- NULL

  for (line in lines) {
    line <- stri_trim_both(line)
    if (nchar(line) == 0) {
      if (!is.null(current_property) && !is.null(current_value)) {
        properties[[current_property]] <- current_value
        current_property <- NULL; current_value <- NULL
      }
      next
    }

    colon_match <- stri_match_first_regex(line, "^([^:;]+?):\\s*(.*)$")
    dash_double_match <- stri_match_first_regex(line, "^([^-]+?)--\\s*(.*)$")
    dash_single_match <- stri_match_first_regex(line, "^([^-]+?)\\s+-\\s+(.*)$")

    if (!is.na(colon_match[1, 2]) && nchar(colon_match[1, 2]) > 0) {
      if (!is.null(current_property) && !is.null(current_value)) properties[[current_property]] <- current_value
      current_property <- stri_trim_both(colon_match[1, 2])
      current_value <- stri_trim_both(colon_match[1, 3])
    } else if (!is.na(dash_double_match[1, 2]) && nchar(dash_double_match[1, 2]) > 0) {
      if (!is.null(current_property) && !is.null(current_value)) properties[[current_property]] <- current_value
      current_property <- stri_trim_both(dash_double_match[1, 2])
      current_value <- stri_trim_both(dash_double_match[1, 3])
    } else if (!is.na(dash_single_match[1, 2]) && nchar(dash_single_match[1, 2]) > 0) {
      if (!is.null(current_property) && !is.null(current_value)) properties[[current_property]] <- current_value
      current_property <- stri_trim_both(dash_single_match[1, 2])
      current_value <- stri_trim_both(dash_single_match[1, 3])
    } else if (!is.null(current_property)) {
      if (nchar(current_value) > 0) current_value <- paste(current_value, line, sep = " ")
      else current_value <- line
    }
  }
  if (!is.null(current_property) && !is.null(current_value)) properties[[current_property]] <- current_value

  # Transform into structured format
  structured_properties <- list()
  for (prop_name_raw in names(properties)) {
    prop_value_raw <- properties[[prop_name_raw]]
    if (nchar(prop_value_raw) < 3) next
    if (stri_detect_regex(prop_value_raw, "^[A-Z\\s&]+$")) next
    if (stri_detect_regex(prop_name_raw, "(?i)^RANGE IN CHARACTERISTICS|INDIVIDUAL HORIZONS|GEOGRAPHIC SETTING|GEOGRAPHICALLY")) next
    if (stri_detect_regex(prop_name_raw, "(?i)Other Features")) next

    prop_name_normalized <- .ricNormalizePropName(prop_name_raw)
    value_type <- .ricClassifyValueType(prop_value_raw)

    prop_entry <- list(
      label_original = prop_name_raw, label_normalized = prop_name_normalized,
      value_type = value_type, value_raw = prop_value_raw,
      source_text = paste(prop_name_raw, ":", prop_value_raw)
    )

    if (value_type %in% c("numeric_range", "open_ended_numeric", "percentage", "depth", "temperature")) {
      numeric_data <- .ricExtractNumericRange(prop_value_raw)
      prop_entry$low <- numeric_data$low
      prop_entry$high <- numeric_data$high
      prop_entry$unit <- numeric_data$unit
      prop_entry$qualifier <- numeric_data$qualifier
    }
    structured_properties[[prop_name_raw]] <- prop_entry
  }

  # Fall back to narrative extraction if no delimiter-based properties found
  if (length(structured_properties) == 0) {
    narrative_props <- .extractRICNarrativeProps(preamble_text)
    if (length(narrative_props) > 0) {
      for (prop_name_raw in names(narrative_props)) {
        prop_data <- narrative_props[[prop_name_raw]]
        prop_entry <- list(
          label_original = prop_data$label_original, label_normalized = prop_data$label_normalized,
          value_type = prop_data$value_type, value_raw = prop_data$value_raw,
          source_text = prop_data$source_text
        )
        if (!is.null(prop_data$low)) prop_entry$low <- prop_data$low
        if (!is.null(prop_data$high)) prop_entry$high <- prop_data$high
        if (!is.null(prop_data$unit)) prop_entry$unit <- prop_data$unit
        if (!is.null(prop_data$extraction_method)) prop_entry$extraction_method <- prop_data$extraction_method
        structured_properties[[prop_name_raw]] <- prop_entry
      }
    }
  }
  structured_properties
}

# ============================================================================
# REACTION / pH / EFFERVESCENCE
# ============================================================================

.ricNormalizeReaction <- function(val, other_val = NULL) {
  if (is.null(val) || is.na(val) || nchar(stri_trim_both(val)) == 0) return(NULL)
  v <- tolower(stri_trim_both(val))
  v <- stri_replace_all_regex(v, "\\.$", "")
  v <- stri_replace_all_regex(v, ",.*$", "")
  v <- stri_trim_both(v)
  v <- stri_replace_all_regex(v, "\\s+(in\\s+the\\s+|and\\s+non|and\\s+is\\s+|through\\s+the\\s+|and\\s+most\\s+|throughout).*$", "")
  v <- stri_replace_all_regex(v, "^(?:ranges?\\s+from\\s+|typically\\s+is\\s+|is\\s+)", "")
  v <- stri_trim_both(v)
  v <- stri_replace_all_fixed(v, "stongly", "strongly")
  v <- stri_replace_all_regex(v, "acideb\\b", "acid")

  if (stri_detect_regex(v, "\\s+through\\s+")) {
    parts <- stri_split_regex(v, "\\s+through\\s+")[[1]]
    v <- stri_trim_both(parts[1])
  }

  v <- stri_replace_all_regex(v, "^medium\\s+acid$", "moderately acid")

  if (!is.null(other_val) && !stri_detect_regex(v, "\\b(acid|alkaline|neutral)\\b")) {
    other_lower <- tolower(stri_trim_both(other_val))
    for (kw in c("acid", "alkaline")) {
      if (stri_detect_regex(other_lower, paste0("\\b", kw, "\\b"))) {
        v <- paste0(v, " ", kw)
        break
      }
    }
  }

  if (!(v %in% .RIC_REACTION_CLASSES)) return(NULL)
  v
}

.ricExtractPHRange <- function(text_block) {
  low <- NULL; high <- NULL
  if (!is.character(text_block) || nchar(text_block) == 0) return(list(low = low, high = high))

  m <- stri_match_first_regex(text_block,
    "(?i)\\bpH\\b[^0-9]{0,40}?([0-9]+\\.?[0-9]*)\\s*(?:to|through)\\s*([0-9]+\\.?[0-9]*)")
  if (!is.na(m[1, 2])) {
    low <- as.numeric(m[1, 2]); high <- as.numeric(m[1, 3])
    if (!is.na(low) && !is.na(high) && low >= 1 && high <= 14 && low <= high) return(list(low = low, high = high))
  }

  m2 <- stri_match_first_regex(text_block,
    "(?i)\\bpH\\b[^0-9]{0,40}?([0-9]+\\.?[0-9]*)\\s*-\\s*([0-9]+\\.?[0-9]*)")
  if (!is.na(m2[1, 2])) {
    low <- as.numeric(m2[1, 2]); high <- as.numeric(m2[1, 3])
    if (!is.na(low) && !is.na(high) && low >= 1 && high <= 14 && low <= high) return(list(low = low, high = high))
  }

  m3 <- stri_match_first_regex(text_block,
    "(?i)\\bpH\\b[^0-9]{0,20}?([0-9]+\\.?[0-9]*)")
  if (!is.na(m3[1, 2])) {
    val <- as.numeric(m3[1, 2])
    if (!is.na(val) && val >= 1 && val <= 14) return(list(low = val, high = val))
  }
  list(low = NULL, high = NULL)
}

#' @importFrom stringi stri_locate_first_fixed
.ricExtractEffervescence <- function(text_block) {
  low <- NULL; high <- NULL
  if (!is.character(text_block) || nchar(text_block) == 0) return(list(low = low, high = high))

  text_lower <- tolower(text_block)
  found <- c(); found_positions <- c()
  for (cls in .RIC_EFFERVESCENCE_CLASSES) {
    pos <- stri_locate_first_fixed(text_lower, cls)
    if (!is.na(pos[1, 1])) {
      found <- c(found, cls)
      found_positions <- c(found_positions, pos[1, 1])
    }
  }
  if (length(found) == 0) return(list(low = low, high = high))

  if (length(found) == 1) {
    low <- found[1]; high <- found[1]
  } else {
    indices <- match(found, .RIC_EFFERVESCENCE_CLASSES)
    low <- .RIC_EFFERVESCENCE_CLASSES[max(indices)]
    high <- .RIC_EFFERVESCENCE_CLASSES[min(indices)]
  }
  list(low = low, high = high)
}

.ricExtractReactionClass <- function(text_block) {
  low <- NULL; high <- NULL
  if (!is.character(text_block) || nchar(text_block) == 0) return(list(low = low, high = high))

  color_words <- c("brown", "gray", "grey", "red", "yellow", "orange", "white", "black",
                    "tan", "cream", "buff", "pink", "purple", "green", "blue", "pale", "dark")

  # Pattern 1: Explicit "Reaction: X to Y"
  reaction_match <- stri_match_first_regex(text_block, "(?i)Reaction:\\s*([^\\n]+)")
  if (!is.na(reaction_match[1, 2])) {
    reaction_text <- stri_trim_both(reaction_match[1, 2])
    if (stri_detect_regex(reaction_text, "^pH\\s+[0-9]")) return(list(low = low, high = high))
    reaction_text <- stri_replace_all_regex(reaction_text, "\\s*\\([^)]*\\)", "")
    reaction_text <- stri_trim_both(reaction_text)
    text_lower <- tolower(reaction_text)
    is_color_range <- any(sapply(color_words, function(cw) stri_detect_fixed(text_lower, cw)))
    if (is_color_range) return(list(low = low, high = high))
    has_keyword <- any(stri_detect_fixed(tolower(reaction_text), tolower(.RIC_REACTION_CLASSES)))
    if (!has_keyword && !stri_detect_regex(tolower(reaction_text), "\\b(acid|alkaline|neutral|calcareous)\\b")) {
      return(list(low = low, high = high))
    }
    reaction_split <- stri_split_regex(reaction_text, "\\s+(?:to|or|through)\\s+")[[1]]
    if (length(reaction_split) >= 2) { low <- stri_trim_both(reaction_split[1]); high <- stri_trim_both(reaction_split[2]) }
    else if (length(reaction_split) == 1) { low <- stri_trim_both(reaction_split[1]); high <- low }
  } else {
    # Pattern 2: "ranges from X to Y"
    ranges_match <- stri_match_first_regex(text_block,
      "(?i)ranges?\\s+from\\s+([a-z\\s]+?)\\s+(?:to|or|through)\\s+([a-z\\s]+?)(?:\\.|,|;|$)")
    if (!is.na(ranges_match[1, 2]) && !is.na(ranges_match[1, 3])) {
      potential_low <- stri_trim_both(ranges_match[1, 2])
      potential_high <- stri_trim_both(ranges_match[1, 3])
      is_low_color <- any(sapply(color_words, function(cw) stri_detect_fixed(tolower(potential_low), cw)))
      is_high_color <- any(sapply(color_words, function(cw) stri_detect_fixed(tolower(potential_high), cw)))
      has_low_r <- stri_detect_regex(tolower(potential_low), "\\b(acid|alkaline|neutral|calcareous)\\b")
      has_high_r <- stri_detect_regex(tolower(potential_high), "\\b(acid|alkaline|neutral|calcareous)\\b")
      if ((has_low_r || has_high_r) && !is_low_color && !is_high_color) {
        low <- potential_low; high <- potential_high
      }
    }

    # Pattern 3: "It is X to Y"
    if (is.null(low)) {
      sentences <- stri_split_regex(text_block, "(?<=[.!?])\\s+")[[1]]
      for (sent in sentences) {
        sent_lower <- tolower(sent)
        if (stri_detect_regex(sent_lower, "neutral\\s*,\\s*value\\s+of")) next
        if (stri_detect_regex(sent_lower, "\\bhue\\s+of\\b|\\bvalue\\s+of\\b|\\bchroma\\s+of\\b")) next
        if (stri_detect_regex(sent_lower, "\\bit\\s+is\\b") &&
            stri_detect_regex(sent_lower, "\\b(acid|alkaline|neutral|calcareous)\\b")) {
          it_is_match <- stri_match_first_regex(sent, "(?i)It\\s+is\\s+([^.]+)")
          if (!is.na(it_is_match[1, 2])) {
            reaction_text <- stri_trim_both(it_is_match[1, 2])
            reaction_text <- stri_replace_all_regex(reaction_text, "\\s*\\([^)]*\\)", "")
            reaction_split <- stri_split_regex(reaction_text, "\\s+(?:to|or|through)\\s+")[[1]]
            if (length(reaction_split) >= 2) { low <- stri_trim_both(reaction_split[1]); high <- stri_trim_both(reaction_split[2]) }
            else if (length(reaction_split) == 1) { low <- stri_trim_both(reaction_split[1]); high <- low }
            break
          }
        }
      }
    }
  }

  # Normalize both values
  if (!is.null(low) || !is.null(high)) {
    norm_low <- .ricNormalizeReaction(low, high)
    norm_high <- .ricNormalizeReaction(high, low)
    low <- norm_low; high <- norm_high
  }
  list(low = low, high = high)
}

# ============================================================================
# TAXONOMIC CLASS EXTRACTION
# ============================================================================

.extractRICTaxClass <- function(taxclass_text) {
  result <- list(
    text = NA_character_, order = NA_character_, suborder = NA_character_,
    great_group = NA_character_, subgroup = NA_character_, family_characteristics = NA_character_
  )
  if (!is.character(taxclass_text) || length(taxclass_text) == 0 || nchar(taxclass_text) == 0) return(result)

  taxclass_clean <- stri_trim_both(taxclass_text)
  result$text <- taxclass_clean
  if (nchar(taxclass_clean) < 5) return(result)

  soil_orders <- c("Alfisols", "Andisols", "Aridisols", "Entisols", "Gelisols",
                   "Histosols", "Inceptisols", "Mollisols", "Oxisols", "Spodosols",
                   "Ultisols", "Vertisols")
  for (order in soil_orders) {
    if (stri_detect_fixed(taxclass_clean, order, case_insensitive = TRUE)) {
      result$order <- order; break
    }
  }

  if (is.na(result$order)) {
    if (stri_detect_regex(taxclass_clean, "(?i)aquepts|andiepts|dystrepts|fragiaquepts|glossaquepts")) result$order <- "Inceptisols"
    else if (stri_detect_regex(taxclass_clean, "(?i)aquents|xerofluvents|torrifluvents")) result$order <- "Entisols"
    else if (stri_detect_regex(taxclass_clean, "(?i)aquults|paleudults|hapludults")) result$order <- "Ultisols"
    else if (stri_detect_regex(taxclass_clean, "(?i)aquerts|pellerts|haplerts")) result$order <- "Vertisols"
  }

  words <- stri_split_fixed(taxclass_clean, " ")[[1]]
  family_chars <- c()
  for (i in seq_along(words)) {
    word <- words[i]
    if (stri_detect_regex(word, "^[A-Z][a-z]+(?:ic|us|ent|aquepts|epts|erts|ults)")) break
    if (tolower(word) %in% c("fine", "sandy", "silty", "ashy", "medial", "euic", "dysic",
                              "calcareous", "smectitic", "illitic", "kaolinitic", "mesic",
                              "thermic", "hyperthermic", "lytic", "nonlithic", "superactive")) {
      family_chars <- c(family_chars, word)
    } else if (stri_detect_regex(word, ",")) {
      family_chars <- c(family_chars, stri_replace_first_fixed(word, ",", ""))
    }
  }
  if (length(family_chars) > 0) result$family_characteristics <- paste(family_chars, collapse = ", ")

  remaining_text <- stri_trim_both(stri_replace_first_fixed(taxclass_clean, result$family_characteristics %||% "", ""))
  remaining_words <- stri_split_fixed(remaining_text, " ")[[1]]
  remaining_words <- remaining_words[remaining_words != "" & !is.na(remaining_words)]

  if (length(remaining_words) > 0) {
    last_word <- remaining_words[length(remaining_words)]
    if (stri_detect_regex(last_word, "(?i)aquepts$")) result$suborder <- stri_replace_first_regex(last_word, "(?i)aquepts$", "")
    else if (stri_detect_regex(last_word, "(?i)aquerts$")) result$subgroup <- stri_replace_first_regex(last_word, "(?i)aquerts$", "")
    else if (stri_detect_regex(last_word, "(?i)epts$")) result$suborder <- stri_replace_first_regex(last_word, "(?i)epts$", "")

    if (length(remaining_words) > 1) {
      potential_gg <- remaining_words[length(remaining_words) - 1]
      if (stri_detect_regex(potential_gg, "^[A-Z]")) result$great_group <- potential_gg
    }
  }
  result
}

# ============================================================================
# MASTER ORCHESTRATOR
# ============================================================================

#' Extract structured data from a Range in Characteristics section
#'
#' Parses RIC text into horizon-level and series-level properties including
#' colors, textures, clay content, coarse fragments, structure, boundaries,
#' reaction, pH, effervescence, consistence, and PSCS data.
#'
#' @param ric_text Character string containing the RIC section text.
#' @param default_color_state Default color state ("moist" or "dry") from
#'   the TYPICAL PEDON header.
#' @return A named list with components: \code{general}, \code{horizons},
#'   \code{series_properties}, and \code{pscs}.
#' @keywords internal
#' @noRd
.extractRICData <- function(ric_text, default_color_state = "moist") {

  ric <- list(
    general = list(),
    horizons = list(horizons = list())
  )

  # Series-level general text
  general_sentences <- .extractRICGeneralText(ric_text)
  if (length(general_sentences) > 0) ric$general <- general_sentences

  # Series properties from preamble
  preamble_text <- .extractRICPreamble(ric_text)
  series_properties <- NULL

  if (!is.null(preamble_text) && nchar(preamble_text) > 0) {
    extracted_properties <- .extractRICSeriesProps(preamble_text)
    if (length(extracted_properties) > 0) {
      series_properties <- list()
      for (prop_original in names(extracted_properties)) {
        prop_data <- extracted_properties[[prop_original]]
        prop_entry <- list(
          label_normalized = prop_data$label_normalized,
          value_type = prop_data$value_type,
          value_raw = prop_data$value_raw,
          source_text = prop_data$source_text
        )
        if (!is.null(prop_data$low)) prop_entry$low <- prop_data$low
        if (!is.null(prop_data$high)) prop_entry$high <- prop_data$high
        if (!is.null(prop_data$unit)) prop_entry$unit <- prop_data$unit
        if (!is.null(prop_data$qualifier)) prop_entry$qualifier <- prop_data$qualifier
        series_properties[[prop_original]] <- prop_entry
      }
      if (length(series_properties) > 0) ric$series_properties <- series_properties
    }
  }

  # Series-level reaction class from preamble
  if (!is.null(preamble_text) && nchar(preamble_text) > 0) {
    reaction_match <- stri_match_first_regex(preamble_text, "(?i)(Reaction\\s+is\\s+[^\\n]{1,400})")
    if (is.na(reaction_match[1, 1])) {
      reaction_match <- stri_match_first_regex(preamble_text, "(?i)(The soils?\\s+(?:is|are)\\s+[^\\n]{1,200}(?:acid|alkaline|neutral))")
    }
    if (is.na(reaction_match[1, 1])) {
      reaction_match <- stri_match_first_regex(preamble_text, "(?i)(This\\s+section\\s+is\\s+[^\\n]{1,300}(?:acid|alkaline|neutral))")
    }

    if (!is.na(reaction_match[1, 1])) {
      matched_text <- stri_trim_both(reaction_match[1, 1])
      reaction_text <- matched_text
      reaction_text <- stri_replace_all_regex(reaction_text, "(?i)^Reaction\\s+is\\s+", "")
      reaction_text <- stri_replace_all_regex(reaction_text, "(?i)^The soils?\\s+(?:is|are)\\s+(?:usually\\s+)?", "")
      reaction_text <- stri_replace_all_regex(reaction_text, "(?i)^This\\s+section\\s+is\\s+", "")
      reaction_text <- stri_trim_both(stri_replace_all_regex(reaction_text, "(?i)\\s+(throughout|in all horizons|in the layer|but|and\\s+(?:low|high)\\s+pH|however).*$", ""))
      reaction_text <- stri_replace_all_regex(reaction_text, "\\s*\\([^)]*\\)", "")
      reaction_text <- stri_trim_both(reaction_text)

      ranges_match <- stri_match_first_regex(reaction_text, "(?i)and\\s+ranges?\\s+from\\s+([^,]+?)\\s+(?:to|or)\\s+(.+?)(?:\\s+in|\\s+throughout|\\.|$)")
      if (!is.na(ranges_match[1, 1])) {
        low <- stri_trim_both(ranges_match[1, 2])
        high <- stri_trim_both(ranges_match[1, 3])
      } else {
        if (nchar(reaction_text) > 0 && stri_detect_regex(tolower(reaction_text), "acid|alkaline|neutral")) {
          reaction_split <- stri_split_regex(reaction_text, "\\s+(?:to|or)\\s+")[[1]]
          low <- stri_trim_both(reaction_split[1])
          high <- if (length(reaction_split) >= 2) stri_trim_both(reaction_split[2]) else low
        } else {
          low <- NULL; high <- NULL
        }
      }

      if (!is.null(low) && nchar(low) > 0 &&
          (stri_detect_regex(tolower(low), "acid|alkaline|neutral") ||
           stri_detect_regex(tolower(high), "acid|alkaline|neutral"))) {
        for (kw in c("acid", "alkaline")) {
          if (stri_detect_regex(tolower(high), paste0("\\b", kw, "\\b")) &&
              !stri_detect_regex(tolower(low), paste0("\\b", kw, "\\b"))) {
            low <- paste0(low, " ", kw)
            break
          }
        }
        if (is.null(series_properties)) series_properties <- list()
        series_properties[["reaction_class"]] <- list(
          label_normalized = "reaction_class",
          value_type = "categorical_range",
          value_raw = matched_text,
          low = low, high = high
        )
        ric$series_properties <- series_properties
      }
    }
  }

  # Series-level hue from preamble
  if (!is.null(preamble_text) && nchar(preamble_text) > 0) {
    hue_data <- .extractRICSeriesHue(preamble_text)
    if (!is.null(hue_data)) {
      if (is.null(series_properties)) series_properties <- list()
      series_properties[["hue"]] <- list(
        label_normalized = "hue",
        value_type = "categorical_range",
        value_raw = hue_data$matched_text,
        low = hue_data$low,
        high = hue_data$high
      )
      ric$series_properties <- series_properties
    }
  }

  # Horizon detection
  horizon_blocks <- .extractRICHorizonBlocks(ric_text)
  if (length(horizon_blocks) == 0) {
    ric$horizons$horizons <- list()
    ric$horizons$metadata <- list(total_horizons = 0)
    return(ric)
  }

  # Process each horizon block
  hz_list <- list()
  hz_order <- 1

  for (designation in names(horizon_blocks)) {
    block_text <- horizon_blocks[[designation]]

    horizon <- list(
      designation = designation,
      pattern = .ricExtractHorizonPattern(block_text, designation),
      vertical_order = hz_order,
      raw_text = stri_trim_both(block_text)
    )

    # Colors
    horizon$colors <- .extractRICColors(block_text, default_color_state)

    # Texture
    texture_data <- .extractRICTextures(block_text)
    if (!is.null(texture_data)) {
      texture_classes <- vapply(texture_data$textures, function(tx) tx$class, character(1))
      texture_modifiers <- lapply(texture_data$textures, function(tx) tx$modifiers %||% NA_character_)
      horizon$texture <- list(
        texture_classes = as.character(texture_classes),
        texture_modifiers = texture_modifiers,
        texture_interpolated = texture_data$interpolated,
        texture_method = texture_data$method %||% "unknown"
      )
    } else {
      horizon$texture <- list(
        texture_classes = NA_character_, texture_modifiers = NA_character_,
        texture_interpolated = NA, texture_method = NA_character_
      )
    }

    # Reaction
    reaction_data <- .ricExtractReactionClass(block_text)
    horizon$reaction <- list(
      reaction_class_low = reaction_data$low %||% NA_character_,
      reaction_class_high = reaction_data$high %||% NA_character_
    )

    # Effervescence
    eff_data <- .ricExtractEffervescence(block_text)
    horizon$effervescence <- list(
      effervescence_low = eff_data$low %||% NA_character_,
      effervescence_high = eff_data$high %||% NA_character_
    )

    # Structure
    structure_data <- .extractRICStructure(block_text)
    horizon$structure <- list(
      structure_grade = structure_data$structure_grade %||% NA_character_,
      structure_size = structure_data$structure_size %||% NA_character_,
      structure_type = structure_data$structure_type %||% NA_character_
    )

    # Consistence
    horizon$consistence <- list(
      consistence_dry = .ricVocabMatch(block_text, .RIC_VOCABULARY$consistence_dry) %||% NA_character_,
      consistence_moist = .ricVocabMatch(block_text, .RIC_VOCABULARY$consistence_moist) %||% NA_character_,
      consistence_cementation = .ricVocabMatch(block_text, .RIC_VOCABULARY$cementation) %||% NA_character_
    )

    # pH
    ph_range <- .ricExtractPHRange(block_text)
    if (!is.null(ph_range$low)) {
      horizon$ph_water_low <- ph_range$low
      horizon$ph_water_high <- ph_range$high
    }

    # Clay
    clay_data <- .extractRICClay(block_text)
    if (!is.null(clay_data$clay_low_pct)) horizon$clay_low_pct <- clay_data$clay_low_pct
    if (!is.null(clay_data$clay_high_pct)) horizon$clay_high_pct <- clay_data$clay_high_pct

    # Boundaries
    boundary_data <- .extractRICBoundaries(block_text)
    if (!is.null(boundary_data$boundary_distinctness)) horizon$boundary_distinctness <- boundary_data$boundary_distinctness
    if (!is.null(boundary_data$boundary_topography)) horizon$boundary_topography <- boundary_data$boundary_topography

    # Coarse fragments
    frag_data <- .extractRICFragments(block_text)
    if (!is.null(frag_data$class) || !is.null(frag_data$kind) ||
        !is.null(frag_data$low_pct) || !is.null(frag_data$high_pct)) {
      horizon$coarse_fragments <- list(
        class = frag_data$class, kind = frag_data$kind,
        low_pct = frag_data$low_pct, high_pct = frag_data$high_pct
      )
    }

    hz_list[[hz_order]] <- horizon
    hz_order <- hz_order + 1
  }

  ric$horizons$horizons <- hz_list
  ric$horizons$metadata <- list(
    total_horizons = length(hz_list),
    raw_preamble = stri_trim_both(preamble_text %||% "")
  )

  # PSCS extraction (from preamble only)
  ric_preamble <- .extractRICPreamble(ric_text)
  pscs_data <- .extractRICPSCS1(ric_preamble)
  pscs_phase2 <- .extractRICPSCS2(ric_preamble)
  if (length(pscs_phase2) > 0) pscs_data <- c(pscs_data, pscs_phase2)
  if (length(pscs_data) > 0) ric$pscs <- pscs_data

  ric
}
