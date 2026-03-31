test_that("osd_to_json works", {
  # setwd("~/workspace/SoilKnowledgeBase/tests/testthat")

  # set pseudorandom seed for consistently random results
  set.seed(123)

  # list OSD .txt files
  testfiles <- na.omit(list.files("OSD",
                             recursive = TRUE,
                             full.names = TRUE))

  # skip if files do not exist
  skip_if_not(length(testfiles) > 0)

  # testfiles <- sort(sample(testfiles, size = 1000))

  osd_result <- osd_to_json(logfile = "test.log",
                            osd_files = testfiles)

  # expect they all run without error (does not validate contents)
  expect_true(all(unlist(osd_result)))
})


test_that("osd_to_json output includes RIC key", {
  # Create a minimal OSD .txt fixture in a temp directory
  tmpdir <- tempdir()
  osd_dir <- file.path(tmpdir, "OSD_RIC_TEST")
  dir.create(osd_dir, showWarnings = FALSE, recursive = TRUE)

  osd_txt <- "LOCATION TESTSOIL              XX

Established Series
Rev. TEST/2024

TESTSOIL SERIES

The TESTSOIL series consists of very deep, well drained soils on uplands.

TAXONOMIC CLASS: Fine-loamy, mixed, active, mesic Typic Hapludalfs

TYPICAL PEDON: TESTSOIL silt loam, on a 4 percent slope (colors are for moist soil).

 Ap--0 to 25 cm; brown (10YR 4/3) silt loam; moderate fine granular structure; friable; slightly acid; clear smooth boundary.

 Bt--25 to 76 cm; yellowish brown (10YR 5/6) silty clay loam; moderate medium subangular blocky structure; firm; moderately acid; gradual smooth boundary.

TYPE LOCATION: Test County, XX; about 1 mile north of town.

RANGE IN CHARACTERISTICS:
Solum thickness: 76 to 127 cm
Depth to bedrock: greater than 152 cm

Ap horizon:
Hue: 10YR
Value: 3 to 5
Chroma: 2 to 4
Texture: silt loam or loam
Reaction: moderately acid to neutral

Bt horizon:
Hue: 7.5YR to 10YR
Value: 4 to 6
Chroma: 4 to 8
Texture: silty clay loam or clay loam
Clay content: 27 to 35 percent
Reaction: strongly acid to moderately acid

COMPETING SERIES: None.

GEOGRAPHIC SETTING: Uplands.

GEOGRAPHICALLY ASSOCIATED SOILS: None documented.

DRAINAGE AND PERMEABILITY: Well drained; moderate permeability.

USE AND VEGETATION: Cropland.

DISTRIBUTION AND EXTENT: Small extent.

SERIES ESTABLISHED: 2024.

REMARKS: Diagnostic horizons recognized in this pedon include:
Ochric epipedon: 0 to 25 cm (Ap horizon)
Argillic horizon: 25 to 76 cm (Bt horizon)
"

  osd_file <- file.path(osd_dir, "TESTSOIL.txt")
  writeLines(osd_txt, osd_file)

  logfile <- file.path(tmpdir, "test_ric.log")
  json_dir <- file.path(tmpdir, "OSD_RIC_JSON")
  dir.create(json_dir, showWarnings = FALSE, recursive = TRUE)

  result <- osd_to_json(logfile = logfile,
                        osd_files = osd_file,
                        output_dir = json_dir)

  expect_true(all(unlist(result)))

  # Read back the JSON and verify RIC key exists
  json_file <- file.path(json_dir, "T", "TESTSOIL.json")
  expect_true(file.exists(json_file))

  parsed <- jsonlite::fromJSON(json_file, simplifyVector = FALSE)
  expect_true("RIC" %in% names(parsed))
  expect_true(is.list(parsed$RIC))

  # Verify RIC has horizons
  expect_true("horizons" %in% names(parsed$RIC))
  hz <- parsed$RIC$horizons
  expect_true(hz$metadata$total_horizons >= 2)

  # Clean up
  unlink(osd_dir, recursive = TRUE)
  unlink(json_dir, recursive = TRUE)
  unlink(logfile)
})
