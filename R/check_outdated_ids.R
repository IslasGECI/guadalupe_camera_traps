camaras_trampa_campo <- read.csv("tests/data/subset_revision_campo.csv")

DEFAULT_PATH = "../data/list_of_obsolet_camera.csv"
get_id_from_outdated_cameras <- function(path = DEFAULT_PATH) {
  obsolete_cameras <- read_csv(path, show_col_types = FALSE) %>% filter(Vigencia == "obsoleto")
  obsolete_id <- unique(obsolete_cameras$ID_camara_trampa)
  return(obsolete_id)
}

get_cameras_id_after_date <- function() {
  camaras_trampa_campo$fecha <- lubridate::dmy(camaras_trampa_campo$Fecha_envio_datos)
  vigentes <- camaras_trampa_campo %>% filter(fecha > lubridate::dmy("16/Jun/2020"))
  id_vigentes <- unique(vigentes$ID_camara_trampa)
  return(id_vigentes)
}

is_obsoleta_in_vigentes <- function() {
  vigentes <- get_cameras_id_after_date()
  obsoletas <- get_id_from_outdated_cameras()
  return(sum(obsoletas %in% vigentes))
}
