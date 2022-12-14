library("tidyverse")

camaras_trampa <- read_csv("../data/subset_revision_campo.csv", show_col_types = FALSE)

is_id_consistent_with_zone <- function() {
  zones_from_id <- strtoi(str_match(camaras_trampa$ID_camara_trampa, "-0([1-8])-")[, 2])
  is_each_id_consistent_with_zone <- zones_from_id == camaras_trampa$Zona
  return(all(is_each_id_consistent_with_zone))
}


test_that("ID is consisitent with zone", {
  expect_true(is_id_consistent_with_zone())
})
test_that("Expect obsolet cameras before date", {
  expect_equal(is_obsoleta_in_vigentes(), 0)
})
