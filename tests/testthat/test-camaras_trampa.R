library("tidyverse")

camaras_trampa <- read_csv("../camaras_trampa_gatos_isla_guadalupe_revision_campo.csv", show_col_types = FALSE)

is_id_consistent_with_zone <- function() {
  zones_from_id <- strtoi(str_match(camaras_trampa$ID_camara_trampa, "-0([1-8])-")[,2])
  is_each_id_consistent_with_zone <- zones_from_id == camaras_trampa$Zona
  return(all(is_each_id_consistent_with_zone))
}
get_id_from_outdated_cameras <- function(){
  obsolete_cameras <- read_csv("../camaras_trampas_guadalupe.csv", show_col_types = FALSE) %>% filter(Vigencia == "obsoleto")
  obsolete_id <- unique(obsolete_cameras$ID_camara_trampa)
  return(obsolete_id)
}

get_cameras_id_after_date <- function(){
  camaras_trampa$fecha <- lubridate::dmy(camaras_trampa$Fecha_envio_datos)
  vigentes <- camaras_trampa %>% filter(fecha > lubridate::dmy("16/Jun/2020"))
  id_vigentes <- unique(vigentes$ID_camara_trampa)
  return(id_vigentes)
}

is_obsoleta_in_vigentes <- function(){
  vigentes <- get_cameras_id_after_date()
  obsoletas <- get_id_from_outdated_cameras()
  return(sum(obsoletas %in% vigentes))
}


test_that("ID is consisitent with zone", {
  expect_true(is_id_consistent_with_zone())
})
test_that("Expect obsolet cameras before date", {
  expect_equal(is_obsoleta_in_vigentes(), 0)
})
