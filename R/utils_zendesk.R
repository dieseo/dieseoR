# ==============================================================================
# SCRIPT: R/utils_zendesk.R
# PACKAGE: dieseoR
# ==============================================================================

#' @title Tickets ueber die Zendesk Incremental Export API holen (intern)
#'
#' @description Gemeinsamer Unterbau fuer [update_zendesk_data()] und
#' [get_zendesk_tickets()]. Laeuft die cursorbasierte Incremental Export API
#' (`/api/v2/incremental/tickets/cursor.json`) ab `start_time` durch, bis
#' `end_of_stream` gesetzt ist.
#'
#' Zwei Eigenschaften unterscheiden diesen Endpunkt von der normalen List-API
#' (`/api/v2/tickets.json`):
#'
#' * Er liefert **archivierte Tickets mit**. Zendesk nimmt geschlossene Tickets
#'   nach rund 120 Tagen aus der List-API heraus; ueber die Incremental API
#'   bleiben sie abrufbar. Genau daran fehlten im Bestand 194.253 Tickets
#'   (alle `closed`, alle erstellt vor dem 02.11.2025, belegt am 13.10.2026).
#' * Er gibt 1.000 statt 100 Tickets pro Request zurueck.
#'
#' Gefiltert wird nach `updated_at`, nicht nach `created_at`. Ein `start_time`
#' in der Vergangenheit holt daher alles, was seitdem angefasst wurde -- fuer
#' einen Vollabzug also schlicht einen Startpunkt vor dem ersten Ticket waehlen.
#'
#' @param subdomain Character. Zendesk-Subdomain (z.B. "pummys").
#' @param email Character. Zendesk-Login-E-Mail (fuer Token-Auth).
#' @param api_token Character. Zendesk API-Token.
#' @param start_time POSIXct oder numerischer Unix-Timestamp. Startpunkt.
#' @param max_pages Integer. Obergrenze an Seiten (Notbremse gegen
#'   Endlosschleifen). Default 2000 (= bis zu 2 Mio. Tickets).
#' @param drop_fields Logical. Wenn TRUE (Default), wird die Spalte `fields`
#'   verworfen. Sie ist ein Duplikat von `custom_fields` und existiert im
#'   historischen Bestand nicht -- weglassen haelt das Schema stabil.
#' @param page_pause Numeric. Pause in Sekunden zwischen zwei Seiten. Zendesk
#'   drosselt die Incremental-Endpunkte strenger als die normale API.
#' @param quiet Logical. Wenn TRUE, keine Fortschrittsmeldungen.
#'
#' @return Ein Data Frame mit den geladenen Tickets oder `NULL`, wenn der
#'   Zeitraum leer war. Duplikate aus der Cursor-Ueberlappung sind **nicht**
#'   entfernt -- das macht die aufrufende Funktion.
#'
#' @importFrom httr RETRY authenticate content status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows
#' @keywords internal
#' @noRd
.fetch_zendesk_incremental <- function(subdomain,
                                       email,
                                       api_token,
                                       start_time,
                                       max_pages = 2000,
                                       drop_fields = TRUE,
                                       page_pause = 1,
                                       quiet = FALSE) {
  start_unix <- as.integer(start_time)

  auth <- httr::authenticate(paste0(email, "/token"), api_token)
  url <- sprintf(
    "https://%s.zendesk.com/api/v2/incremental/tickets/cursor.json?start_time=%d",
    subdomain, start_unix
  )

  pages <- list()
  i <- 0L
  n_seen <- 0L

  repeat {
    i <- i + 1L
    if (i > max_pages) {
      warning(sprintf(
        "max_pages (%d) erreicht -- Abbruch. Bestand ist evtl. unvollstaendig.",
        max_pages
      ))
      break
    }

    response <- httr::RETRY(
      verb = "GET", url = url, config = auth,
      times = 5, pause_base = 5, quiet = TRUE, httr::timeout(180)
    )

    if (httr::status_code(response) != 200) {
      stop(sprintf(
        "Zendesk-Fehler auf Seite %d: Status %d",
        i, httr::status_code(response)
      ))
    }

    parsed <- jsonlite::fromJSON(
      httr::content(response, "text", encoding = "UTF-8"),
      flatten = TRUE
    )

    if (!is.null(parsed$tickets) && NROW(parsed$tickets) > 0) {
      tickets <- parsed$tickets
      if (drop_fields) {
        tickets <- tickets[, setdiff(names(tickets), "fields"), drop = FALSE]
      }
      pages[[length(pages) + 1L]] <- tickets
      n_seen <- n_seen + nrow(tickets)
      if (!quiet) {
        message(sprintf(
          "  Seite %d: %d Tickets (kumuliert %s)",
          i, nrow(tickets), format(n_seen, big.mark = ".", decimal.mark = ",")
        ))
      }
    }

    if (isTRUE(parsed$end_of_stream) || is.null(parsed$after_cursor)) break

    url <- sprintf(
      "https://%s.zendesk.com/api/v2/incremental/tickets/cursor.json?cursor=%s",
      subdomain, utils::URLencode(parsed$after_cursor, reserved = TRUE)
    )

    Sys.sleep(page_pause)
  }

  if (length(pages) == 0) {
    return(NULL)
  }
  dplyr::bind_rows(pages)
}
