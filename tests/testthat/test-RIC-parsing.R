context("RIC Parsing")


# ---- Horizon detection ----

test_that("colon-delimited horizon blocks (Miami format)", {
  ric_text <- "Ap or A horizon:
Hue: 10YR
Value: 3 to 5 moist, 6 dry
Chroma: 1 to 4 moist, 2 or 3 dry
Texture: loam or silt loam
Rock fragment content: 0 to 5 percent
Reaction: moderately acid to neutral

Bt or 2Bt horizon:
Hue: 7.5YR to 2.5Y
Value: 4 to 6
Chroma: 3 to 6
Texture: silt loam, silty clay loam, loam, or clay loam
Clay content: averages 27 to 35 percent
Rock fragment content: 1 to 10 percent
Reaction: strongly acid to slightly acid"

  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  expect_true(length(blocks) >= 2)
  expect_true("Ap" %in% names(blocks) || "A" %in% names(blocks))
  expect_true("Bt" %in% names(blocks) || "2Bt" %in% names(blocks))
})


test_that("narrative-format horizon blocks (Congaree format)", {
  ric_text <- "The A or Ap horizon has hue of 5YR to 10YR, value of 3 to 5, and chroma of 2 to 6.
The C horizon, to a depth of 20 inches or more, has hue of 5YR to 10YR, value of 3 to 5, and chroma of 3 to 6."

  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  expect_true(length(blocks) >= 2)
  expect_true("A" %in% names(blocks) || "Ap" %in% names(blocks))
  expect_true("C" %in% names(blocks))
})


test_that("diagnostic features are excluded from horizon blocks", {
  ric_text <- "The argillic horizon extends from 10 to 30 inches.
A horizon:
Hue: 10YR
Value: 3 to 5
Chroma: 2 to 4"

  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  expect_false("argillic" %in% names(blocks))
  expect_true("A" %in% names(blocks))
})


test_that("dash-delimited horizon blocks", {
  ric_text <- "A horizon--Hue of 10YR, value of 3 to 5, chroma of 2 to 4.
Bt horizon--Hue of 7.5YR, value of 4 to 6, chroma of 4 to 6."

  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  expect_true(length(blocks) >= 2)
  expect_true("A" %in% names(blocks))
  expect_true("Bt" %in% names(blocks))
})


test_that("compound 'or' horizon designations", {
  ric_text <- "Ap or A horizon:
Hue: 10YR, value of 3 to 5, chroma of 2 to 4."

  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  expect_true(length(blocks) >= 1)
  expect_true("Ap" %in% names(blocks) || "A" %in% names(blocks))
})


# ---- Color extraction ----

test_that("color extraction with dry/moist states", {
  block <- "Hue: 10YR
Value: 3 to 5 moist, 6 dry
Chroma: 1 to 4 moist, 2 or 3 dry"

  colors <- SoilKnowledgeBase:::.extractRICColors(block, "moist")
  expect_equal(colors$state, "both")
  expect_true(!is.null(colors$moist$value$low))
  expect_equal(colors$moist$value$low, 3)
  expect_equal(colors$moist$value$high, 5)
  expect_equal(colors$moist$chroma$low, 1)
  expect_equal(colors$moist$chroma$high, 4)
  expect_equal(colors$dry$chroma$low, 2)
  expect_equal(colors$dry$chroma$high, 3)
  expect_equal(colors$moist$hues, "10YR")
  expect_equal(colors$dry$hues, "10YR")
})


test_that("color extraction with narrative format", {
  block <- "The A horizon has hue of 5YR to 10YR, value of 3 to 5, and chroma of 2 to 6."

  colors <- SoilKnowledgeBase:::.extractRICColors(block, "moist")
  expect_true(length(colors) > 0)
  # Hues should include 5YR and 10YR
  all_hues <- c(colors$moist$hues, colors$dry$hues, colors$unknown$hues)
  if (length(all_hues) > 0) {
    expect_true("5YR" %in% all_hues || "10YR" %in% all_hues)
  }
})


test_that("neutral hue detection in colors", {
  block <- "The Oa horizon has hue of 10YR, 7.5YR, or it is neutral, value of 2 or 3, chroma of 0 to 2."

  colors <- SoilKnowledgeBase:::.extractRICColors(block, "moist")
  all_hues <- c(colors$moist$hues, colors$dry$hues, colors$unknown$hues)
  if (length(all_hues) > 0) {
    expect_true("N" %in% all_hues)
  }
})


test_that("default color state assignment when no explicit dry/moist", {
  block <- "Hue of 10YR, value of 3 to 5, chroma of 2 to 4"

  colors_moist <- SoilKnowledgeBase:::.extractRICColors(block, "moist")
  colors_dry <- SoilKnowledgeBase:::.extractRICColors(block, "dry")

  if (!is.null(colors_moist$state)) {
    expect_true(colors_moist$state %in% c("moist", "unknown"))
  }
  if (!is.null(colors_dry$state)) {
    expect_true(colors_dry$state %in% c("dry", "unknown"))
  }
})


# ---- Default color state from typical pedon header ----

test_that("default color state detection", {
  expect_equal(
    SoilKnowledgeBase:::.extractRICDefaultColorState("Colors are for moist soil"),
    "moist"
  )
  expect_equal(
    SoilKnowledgeBase:::.extractRICDefaultColorState("Colors are for dry soil unless otherwise noted"),
    "dry"
  )
  expect_equal(
    SoilKnowledgeBase:::.extractRICDefaultColorState(""),
    "moist"
  )
})


# ---- Clay extraction ----

test_that("clay content extraction", {
  block <- "Clay content: 27 to 35 percent"
  clay <- SoilKnowledgeBase:::.extractRICClay(block)
  expect_equal(clay$clay_low_pct, 27)
  expect_equal(clay$clay_high_pct, 35)
})

test_that("clay extraction with dash delimiter", {
  block <- "Clay content--15 to 25 percent"
  clay <- SoilKnowledgeBase:::.extractRICClay(block)
  expect_equal(clay$clay_low_pct, 15)
  expect_equal(clay$clay_high_pct, 25)
})


# ---- Texture extraction ----

test_that("explicit texture header extraction", {
  block <- "Texture: loam or silt loam or silty clay loam"
  textures <- SoilKnowledgeBase:::.extractRICTextures(block)
  expect_true(!is.null(textures))
  classes <- vapply(textures$textures, function(tx) tx$class, character(1))
  expect_true("loam" %in% classes)
  expect_true("silt loam" %in% classes)
})

test_that("texture range interpolation", {
  block <- "Texture ranges from sandy loam to clay loam"
  textures <- SoilKnowledgeBase:::.extractRICTextures(block)
  expect_true(!is.null(textures))
  classes <- vapply(textures$textures, function(tx) tx$class, character(1))
  expect_true("sandy loam" %in% classes)
  expect_true("clay loam" %in% classes)
  expect_true(length(classes) > 2)
})

test_that("texture list extraction", {
  block <- "Texture: loam, silt loam, or clay loam"
  textures <- SoilKnowledgeBase:::.extractRICTextures(block)
  expect_true(!is.null(textures))
  classes <- vapply(textures$textures, function(tx) tx$class, character(1))
  expect_true("loam" %in% classes)
  expect_true("silt loam" %in% classes || "clay loam" %in% classes)
})


# ---- Coarse fragments ----

test_that("rock fragment content extraction", {
  block <- "Rock fragment content: 1 to 10 percent"
  frags <- SoilKnowledgeBase:::.extractRICFragments(block)
  expect_equal(frags$low_pct, 1)
  expect_equal(frags$high_pct, 10)
})

test_that("fragment class detection", {
  block <- "Texture: very gravelly loam. Rock fragments: 35 to 60 percent gravel."
  frags <- SoilKnowledgeBase:::.extractRICFragments(block)
  expect_equal(frags$class, "very gravelly")
  expect_equal(frags$low_pct, 35)
  expect_equal(frags$high_pct, 60)
})


# ---- Structure ----

test_that("structure extraction", {
  block <- "Structure is moderate medium subangular blocky."
  structure <- SoilKnowledgeBase:::.extractRICStructure(block)
  expect_equal(structure$structure_grade, "moderate")
  expect_equal(structure$structure_size, "medium")
  expect_equal(structure$structure_type, "subangular blocky")
})

test_that("massive structure", {
  block <- "It is massive throughout."
  structure <- SoilKnowledgeBase:::.extractRICStructure(block)
  expect_equal(structure$structure_type, "massive")
})


# ---- Boundaries ----

test_that("boundary extraction", {
  block <- "The boundary is clear wavy."
  boundary <- SoilKnowledgeBase:::.extractRICBoundaries(block)
  expect_equal(boundary$boundary_distinctness, "clear")
  expect_equal(boundary$boundary_topography, "wavy")
})


# ---- Reaction class ----

test_that("explicit reaction extraction", {
  block <- "Reaction: moderately acid to neutral"
  reaction <- SoilKnowledgeBase:::.ricExtractReactionClass(block)
  expect_equal(reaction$low, "moderately acid")
  expect_equal(reaction$high, "neutral")
})

test_that("narrative reaction extraction", {
  block <- "It is very strongly acid to moderately acid."
  reaction <- SoilKnowledgeBase:::.ricExtractReactionClass(block)
  expect_equal(reaction$low, "very strongly acid")
  expect_equal(reaction$high, "moderately acid")
})

test_that("reaction rejects color descriptions (Pamlico bug)", {
  # "or it is neutral, value of 2 or 3" should NOT be detected as reaction
  block <- "The Oa horizon has hue of 10YR, 7.5YR, or it is neutral, value of 2 or 3, chroma of 0 to 2."
  reaction <- SoilKnowledgeBase:::.ricExtractReactionClass(block)
  expect_true(is.null(reaction$low) || is.na(reaction$low))
})


# ---- pH extraction ----

test_that("pH range extraction", {
  block <- "Reaction: strongly acid to slightly acid (pH 5.1 to 6.5)"
  ph <- SoilKnowledgeBase:::.ricExtractPHRange(block)
  expect_equal(ph$low, 5.1)
  expect_equal(ph$high, 6.5)
})

test_that("pH dash range", {
  block <- "pH 4.5-6.0"
  ph <- SoilKnowledgeBase:::.ricExtractPHRange(block)
  expect_equal(ph$low, 4.5)
  expect_equal(ph$high, 6.0)
})


# ---- Effervescence ----

test_that("effervescence detection", {
  block <- "It is slightly effervescent to strongly effervescent."
  eff <- SoilKnowledgeBase:::.ricExtractEffervescence(block)
  expect_true(!is.null(eff$low))
  expect_true(!is.null(eff$high))
})


# ---- Preamble extraction ----

test_that("preamble extraction before horizon blocks", {
  ric_text <- "Solum thickness ranges from 40 to 60 inches.
Depth to bedrock: Greater than 152 centimeters.
Rock Fragment Content: 0 to 35 percent.
A horizon:
Hue: 10YR, value of 3 to 5, chroma of 2 to 8."

  preamble <- SoilKnowledgeBase:::.extractRICPreamble(ric_text)
  expect_true(nchar(preamble) > 0)
  expect_true(grepl("Solum thickness", preamble))
  expect_false(grepl("A horizon:", preamble))
})


# ---- Series properties ----

test_that("colon-delimited series properties", {
  preamble <- "Solum thickness: 102 to 152 centimeters
Depth to bedrock: Greater than 152 centimeters
Depth to carbonates: 51 to 102 cm"

  props <- SoilKnowledgeBase:::.extractRICSeriesProps(preamble)
  expect_true(length(props) >= 2)
  norm_names <- vapply(props, function(p) p$label_normalized, character(1))
  expect_true("solum_thickness" %in% norm_names || any(grepl("solum", norm_names)))
})

test_that("narrative series properties fallback", {
  preamble <- "Solum thickness ranges from 40 to 60 inches. Depth to bedrock ranges from 60 to 80 inches."

  props <- SoilKnowledgeBase:::.extractRICSeriesProps(preamble)
  expect_true(length(props) >= 1)
})


# ---- PSCS extraction ----

test_that("PSCS extraction from preamble", {
  # Clay content with colon-delimited format (directly after 'Clay content:')
  preamble_clay <- "Clay content: 27 to 35 percent"
  pscs_clay <- SoilKnowledgeBase:::.extractRICPSCS1(preamble_clay)
  expect_equal(pscs_clay$clay_low_pct, 27)
  expect_equal(pscs_clay$clay_high_pct, 35)

  # Rock fragments in PSCS
  preamble_frag <- "Rock fragments in the particle-size control section: 0 to 10 percent."
  pscs_frag <- SoilKnowledgeBase:::.extractRICPSCS1(preamble_frag)
  expect_equal(pscs_frag$rock_fragments_low_pct, 0)
  expect_equal(pscs_frag$rock_fragments_high_pct, 10)
})


# ---- Full orchestrator (.extractRICData) ----

test_that("full RIC extraction on colon-delimited format", {
  ric_text <- "Solum thickness: 61 to 102 cm
Depth to densic contact: 61 to 102 cm

Ap horizon:
Hue: 10YR
Value: 3 to 5
Chroma: 1 to 4
Texture: loam or silt loam
Clay content: 12 to 20 percent
Reaction: moderately acid to neutral

Bt horizon:
Hue: 7.5YR to 2.5Y
Value: 4 to 6
Chroma: 3 to 6
Texture: silty clay loam or clay loam
Clay content: 27 to 35 percent
Rock fragment content: 1 to 10 percent
Reaction: strongly acid to slightly acid"

  result <- SoilKnowledgeBase:::.extractRICData(ric_text, "moist")

  # Basic structure
  expect_true(is.list(result))
  expect_true("horizons" %in% names(result))
  expect_true("general" %in% names(result))

  # Horizons detected
  expect_equal(result$horizons$metadata$total_horizons, 2)

  hz1 <- result$horizons$horizons[[1]]
  hz2 <- result$horizons$horizons[[2]]

  # Designations
  expect_equal(hz1$designation, "Ap")
  expect_equal(hz2$designation, "Bt")

  # Clay
  expect_equal(hz1$clay_low_pct, 12)
  expect_equal(hz1$clay_high_pct, 20)
  expect_equal(hz2$clay_low_pct, 27)
  expect_equal(hz2$clay_high_pct, 35)

  # Textures extracted
  expect_true(!is.null(hz1$texture))
  expect_true(!all(is.na(hz1$texture$texture_classes)))

  # Reaction
  expect_equal(hz1$reaction$reaction_class_low, "moderately acid")
  expect_equal(hz1$reaction$reaction_class_high, "neutral")

  # Series properties present
  expect_true("series_properties" %in% names(result))
})


test_that("full RIC extraction on narrative format", {
  ric_text <- "Depth to bedrock commonly is more than 10 feet. The soil is very strongly acid to neutral throughout.
The A or Ap horizon has hue of 5YR to 10YR, value of 3 to 5, and chroma of 2 to 6. The A horizon is loam, silt loam, sandy loam, or fine sandy loam.
The C horizon has hue of 5YR to 10YR, value of 3 to 5, and chroma of 3 to 6."

  result <- SoilKnowledgeBase:::.extractRICData(ric_text, "moist")
  expect_true(result$horizons$metadata$total_horizons >= 2)

  # General text captured
  expect_true(length(result$general) >= 1)
})


test_that("empty/null RIC text returns empty structure", {
  result_empty <- SoilKnowledgeBase:::.extractRICData("", "moist")
  expect_true(is.list(result_empty))
  expect_equal(result_empty$horizons$metadata$total_horizons, 0)

  result_null <- SoilKnowledgeBase:::.extractRICData(NULL, "moist")
  expect_true(is.list(result_null))
})


test_that("RIC extraction does not crash on pathological input", {
  # Very long text with no horizon patterns
  noise <- paste(rep("This is a sentence with some words.", 100), collapse = " ")
  result <- SoilKnowledgeBase:::.extractRICData(noise, "moist")
  expect_true(is.list(result))
  expect_equal(result$horizons$metadata$total_horizons, 0)
})


# ---- Integration: .doParseOSD includes ric-data ----

test_that(".doParseOSD returns ric-data key", {
  mock_osd <- list()
  mock_osd$`TYPICAL PEDON` <- list(
    content = "Cecil fine sandy loam, on a 4 percent slope (colors are for moist soil)\nAp--0 to 8 inches; brown (7.5YR 4/4) fine sandy loam; weak fine granular structure; friable; slightly acid; abrupt smooth boundary."
  )
  mock_osd$`RANGE IN CHARACTERISTICS` <- list(
    content = "Ap horizon:\nHue of 5YR to 10YR, value of 3 to 5, and chroma of 2 to 8.\nTexture: sandy loam or loam.\nReaction: very strongly acid to slightly acid."
  )

  result <- SoilKnowledgeBase:::.doParseOSD(mock_osd, logfile = tempfile(), filename = "TEST.txt")
  expect_true("ric-data" %in% names(result))
  expect_true(is.list(result$`ric-data`))
  expect_true(result$`ric-data`$horizons$metadata$total_horizons >= 1)
})


test_that(".doParseOSD handles missing RIC gracefully", {
  mock_osd <- list()
  mock_osd$`TYPICAL PEDON` <- list(
    content = "Test pedon\nAp--0 to 8 inches; brown loam."
  )
  mock_osd$`RANGE IN CHARACTERISTICS` <- list(content = NULL)

  result <- SoilKnowledgeBase:::.doParseOSD(mock_osd, logfile = tempfile(), filename = "TEST.txt")
  expect_true("ric-data" %in% names(result))
  expect_true(is.list(result$`ric-data`))
  expect_equal(length(result$`ric-data`), 0)
})


# ---- Hue helpers ----

test_that("hue expansion through range", {
  hues <- SoilKnowledgeBase:::.ricExpandHueRange("5YR", "10YR")
  expect_true("5YR" %in% hues)
  expect_true("10YR" %in% hues)
  expect_true("7.5YR" %in% hues)
})

test_that("neutral hue detection", {
  hues <- SoilKnowledgeBase:::.ricExtractAllHues("10YR, 7.5YR, or neutral")
  expect_true("N" %in% hues)
  expect_true("10YR" %in% hues)
})


# ---- Series-level hue extraction ----

test_that("series hue extraction with 'hue of' pattern", {
  preamble <- "The soil has hue of 5YR or redder."
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_false(is.null(hue))
  expect_equal(hue$low, "5YR")
  expect_equal(hue$high, "R")
})

test_that("series hue extraction with 'ranges from' pattern", {
  preamble <- "Hue ranges from 5Y through 7.5YR in most pedons."
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_false(is.null(hue))
  expect_equal(hue$low, "5Y")
  expect_equal(hue$high, "7.5YR")
})

test_that("series hue extraction with 'ranges from' and color word", {
  preamble <- "Hue ranges from 5YR or redder in some parts."
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_false(is.null(hue))
  expect_equal(hue$low, "5YR")
  expect_equal(hue$high, "R")
})

test_that("series hue extraction with bidirectional colors ('yellower')", {
  preamble <- "The soil has hue of 10YR or yellower"
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_false(is.null(hue))
  expect_equal(hue$low, "10YR")
  expect_equal(hue$high, "Y")
})

test_that("series hue extraction with 'to' connector", {
  preamble <- "The soils have hue of 7.5YR to 5Y."
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_false(is.null(hue))
  expect_equal(hue$low, "7.5YR")
  expect_equal(hue$high, "5Y")
})

test_that("series hue returns null when no match", {
  preamble <- "Depth to bedrock ranges from 40 to 80 inches."
  hue <- SoilKnowledgeBase:::.extractRICSeriesHue(preamble)
  expect_true(is.null(hue))
})


# ---- Horizon pattern enhancements ----

test_that("complex horizon designations with lithologic prefix", {
  ric_text <- "The 2Bkk horizon has clay films. The 3C1 layer is sandy. The 2C2 and 2C3 contain stones."
  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  designations <- names(blocks)
  expect_true("2Bkk" %in% designations)
  expect_true("3C1" %in% designations)
  expect_true("2C2" %in% designations)
  expect_true("2C3" %in% designations)
})

test_that("horizon designations with prime notation (alternative material)", {
  ric_text <- "The B't horizon has slickensides. The 3B'qk layer is calcareous."
  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  designations <- names(blocks)
  expect_true("B't" %in% designations)
  expect_true("3B'qk" %in% designations)
})

test_that("horizon designations with caret notation (anthropogenic)", {
  ric_text <- "The ^A horizon contains artifacts. The ^C and ^Cu1 layers are transported."
  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  designations <- names(blocks)
  expect_true("^A" %in% designations)
  expect_true("^C" %in% designations)
  expect_true("^Cu1" %in% designations)
})

test_that("horizon designations with slash notation (transitional)", {
  ric_text <- "The Bt1/B layer transitions. The 2BC/3C and 3C/2C horizons are complex."
  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  designations <- names(blocks)
  expect_true("Bt1/B" %in% designations)
  expect_true("2BC/3C" %in% designations)
  expect_true("3C/2C" %in% designations)
})

test_that("mixed complex horizon designations together", {
  ric_text <- "The 2Bkk, B't and ^A horizons occur together. The Bt1/B, 3C and ^Cu1 layers are distinct."
  blocks <- SoilKnowledgeBase:::.extractRICHorizonBlocks(ric_text)
  designations <- names(blocks)
  expect_true("2Bkk" %in% designations)
  expect_true("B't" %in% designations)
  expect_true("^A" %in% designations)
  expect_true("Bt1/B" %in% designations)
  expect_true("3C" %in% designations)
  expect_true("^Cu1" %in% designations)
})


# ---- Reaction normalization ----

test_that("reaction class normalization", {
  expect_equal(
    SoilKnowledgeBase:::.ricNormalizeReaction("strongly acid"),
    "strongly acid"
  )
  expect_equal(
    SoilKnowledgeBase:::.ricNormalizeReaction("stongly acid"),
    "strongly acid"
  )
  expect_equal(
    SoilKnowledgeBase:::.ricNormalizeReaction("moderately", "strongly acid"),
    "moderately acid"
  )
  expect_null(SoilKnowledgeBase:::.ricNormalizeReaction("some random text"))
})

# ---- Munsell Color Parsing ----

test_that("Basic Munsell notation parsing", {
  result <- SoilKnowledgeBase:::.extractRICColors("10YR 3/2")
  expect_equal(result$state, "moist")
  expect_length(result$moist$munsell_pairs, 1)
  expect_equal(result$moist$munsell_pairs[[1]]$hue, "10YR")
  expect_equal(result$moist$munsell_pairs[[1]]$value, 3)
  expect_equal(result$moist$munsell_pairs[[1]]$chroma, 2)
})

test_that("Multiple Munsell notations in list", {
  result <- SoilKnowledgeBase:::.extractRICColors("10YR 3/2 to 10YR 4/3")
  expect_equal(result$state, "list")
  expect_true(length(result$moist$munsell_pairs) >= 2)
  # First pair
  expect_equal(result$moist$munsell_pairs[[1]]$hue, "10YR")
  expect_equal(result$moist$munsell_pairs[[1]]$value, 3)
})

test_that("Neutral hue Munsell notation", {
  result <- SoilKnowledgeBase:::.extractRICColors("N 2/0")
  expect_equal(result$state, "moist")
  expect_equal(result$moist$munsell_pairs[[1]]$hue, "N")
  expect_equal(result$moist$munsell_pairs[[1]]$value, 2)
  expect_equal(result$moist$munsell_pairs[[1]]$chroma, 0)
})

test_that("Dry and moist Munsell colors", {
  result <- SoilKnowledgeBase:::.extractRICColors("Dry color: 10YR 5/3; Moist color: 10YR 3/2")
  expect_true(!is.null(result$dry$munsell_pairs))
  expect_true(!is.null(result$moist$munsell_pairs))
  expect_equal(result$dry$munsell_pairs[[1]]$value, 5)
  expect_equal(result$moist$munsell_pairs[[1]]$value, 3)
})

test_that("Fractional value and chroma", {
  result <- SoilKnowledgeBase:::.extractRICColors("7.5YR 4.5/4")
  expect_equal(result$moist$munsell_pairs[[1]]$hue, "7.5YR")
  expect_equal(result$moist$munsell_pairs[[1]]$value, 4.5)
  expect_equal(result$moist$munsell_pairs[[1]]$chroma, 4)
})

test_that("Munsell extraction in full RIC context", {
  ric_text <- "A horizon:
Color - Dry: 10YR 5/3 to 10YR 6/3; Moist: 10YR 3/2 to 10YR 4/3"
  
  result <- SoilKnowledgeBase:::.extractRICHorizons(ric_text)
  expect_gt(length(result), 0)
  hz <- result[[1]]$colors
  expect_true(!is.null(hz$dry$munsell_pairs))
  expect_true(!is.null(hz$moist$munsell_pairs))
})


# ---- Raw text capture ----

test_that("raw text capture at horizon level", {
  ric_text <- "Ap horizon:
Hue: 10YR
Value: 3 to 5
Chroma: 1 to 4
Texture: loam

Bt horizon:
Hue: 7.5YR
Value: 4 to 6
Chroma: 3 to 6"

  result <- SoilKnowledgeBase:::.extractRICData(ric_text, "moist")

  # Each horizon should have raw_text
  expect_equal(result$horizons$metadata$total_horizons, 2)
  
  hz1 <- result$horizons$horizons[[1]]
  hz2 <- result$horizons$horizons[[2]]
  
  expect_true("raw_text" %in% names(hz1))
  expect_true("raw_text" %in% names(hz2))
  expect_true(nchar(hz1$raw_text) > 0)
  expect_true(nchar(hz2$raw_text) > 0)
  
  # Content verification
  expect_true(grepl("Hue", hz1$raw_text))
  expect_true(grepl("10YR", hz1$raw_text))
  expect_true(grepl("7.5YR", hz2$raw_text))
  expect_true(grepl("Texture", hz1$raw_text))
})

test_that("raw preamble capture in metadata", {
  ric_text <- "Solum thickness: 61 to 102 cm
Depth to bedrock: 40 to 60 inches
Mean annual temperature: 47 degrees F

Ap horizon:
Hue: 10YR
Value: 3 to 5
Chroma: 2 to 4"

  result <- SoilKnowledgeBase:::.extractRICData(ric_text, "moist")

  # Check raw_preamble exists
  expect_true("raw_preamble" %in% names(result$horizons$metadata))
  expect_true(nchar(result$horizons$metadata$raw_preamble) > 0)
  
  # Preamble should contain series-level properties
  expect_true(grepl("Solum thickness", result$horizons$metadata$raw_preamble))
  expect_true(grepl("Depth to bedrock", result$horizons$metadata$raw_preamble))
  
  # Preamble should NOT contain horizon text
  expect_false(grepl("Ap horizon", result$horizons$metadata$raw_preamble))
})

test_that("raw text with complex horizon patterns", {
  ric_text <- "The 2Bkk horizon has clay films. The 3C1 layer is sandy."

  result <- SoilKnowledgeBase:::.extractRICData(ric_text, "moist")

  expect_equal(result$horizons$metadata$total_horizons, 2)
  
  hz1 <- result$horizons$horizons[[1]]
  hz2 <- result$horizons$horizons[[2]]
  
  expect_equal(hz1$designation, "2Bkk")
  expect_equal(hz2$designation, "3C1")
  
  expect_true(grepl("clay", hz1$raw_text, ignore.case = TRUE))
  expect_true(grepl("sandy", hz2$raw_text, ignore.case = TRUE))
})
