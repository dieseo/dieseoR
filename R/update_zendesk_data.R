# ==============================================================================
# SCRIPT: R/update_zendesk_data.R
# PACKAGE: dieseoR
# ==============================================================================

#' @title Inkrementelles Update der Zendesk-Tickets (Incremental Export API)
#'
#' @description Laedt per Zendesk *Incremental Export API* nur die Tickets, die
#' seit dem letzten Sync **geaendert** wurden, und fuehrt sie per Upsert
#' (Schluessel: `id`) mit dem lokalen Bestand zusammen.
#'
#' Der entscheidende Unterschied zur normalen List-API (`/api/v2/tickets.json`):
#' gefiltert wird nach `updated_at`, **nicht** nach `created_at`. Ein Ticket vom
#' Januar, das heute erneut angefasst wird, kommt daher heute wieder mit und
#' ueberschreibt die alte Zeile. Late-Arriving Updates gehen so nicht verloren --
#' genau der Grund, warum bisher jedes Mal alles neu geladen wurde.
#'
#' Zusaetzlich liefert die Incremental API 1.000 statt 100 Tickets pro Request.
#'
#' @param subdomain Character. Zendesk-Subdomain (z.B. "pummys").
#' @param email Character. Zendesk-Login-E-Mail (fuer Token-Auth).
#' @param api_token Character. Zendesk API-Token.
#' @param datadir Character. Zentrales Datenverzeichnis. Default `"~/data"`.
#' @param filename Character. Dateiname des Masterdatensatzes. Default `"all_tickets.rds"`.
#' @param buffer_hours Integer. Sicherheitspuffer in Stunden, der vom letzten
#'   bekannten `updated_at` abgezogen wird (faengt Uhr-Drift und Grenzfaelle ab).
#'   Default 24.
#' @param start_time POSIXct oder NULL. Erzwingt einen expliziten Startpunkt und
#'   ignoriert den lokalen Bestand. `NULL` (Default) leitet ihn automatisch ab.
#' @param max_pages Integer. Obergrenze an Seiten pro Lauf (Notbremse gegen
#'   Endlosschleifen). Default 500 (= bis zu 500.000 Tickets).
#'
#' @return Invisible List mit `n_delta` (geaenderte Tickets), `n_total`
#'   (Bestand nach Merge) und `start_time` (genutzter Startpunkt).
#'
#' @importFrom httr RETRY authenticate content status_code timeout
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @export
#'
#' @examples
#' \dontrun{
#' update_zendesk_data(
#'   subdomain = "pummys",
#'   email     = Sys.getenv("ZENDESK_EMAIL"),
#'   api_token = Sys.getenv("ZENDESK_TOKEN")
#' )
#' }
update_zendesk_data <- function(subdomain,
                                email,
                                api_token,
                                datadir = "~/data",
                                filename = "all_tickets.rds",
                                buffer_hours = 24,
                                start_time = NULL,
                                max_pages = 500) {
  if (missing(subdomain) || missing(email) || missing(api_token)) {
    stop("subdomain, email und api_token muessen uebergeben werden.")
  }

  zendesk_dir <- file.path(datadir, "zendesk")
  if (!dir.exists(zendesk_dir)) dir.create(zendesk_dir, recursive = TRUE)
  file_path <- file.path(zendesk_dir, filename)

  old_tickets <- NULL
  if (file.exists(file_path)) old_tickets <- readRDS(file_path)

  # --- 1. Startpunkt bestimmen ---------------------------------------------
  if (is.null(start_time)) {
    if (is.null(old_tickets) || !"updated_at" %in% names(old_tickets) ||
      nrow(old_tickets) == 0) {
      stop(
        "Kein lokaler Bestand gefunden. Fuer den Erstbefuellung bitte ",
        "start_time explizit setzen (z.B. as.POSIXct('2024-01-01'))."
      )
    }
    last_seen <- max(as.POSIXct(old_tickets$updated_at,
      format = "%Y-%m-%dT%H:%M:%SZ",
      tz = "UTC"
    ), na.rm = TRUE)
    start_time <- last_seen - buffer_hours * 3600
  }
  start_unix <- as.integer(start_time)

  message(sprintf(
    "⏳ Zendesk-Delta ab %s UTC (inkl. %d h Puffer)...",
    format(start_time, "%Y-%m-%d %H:%M:%S"), buffer_hours
  ))

  # --- 2. Cursor-Schleife ueber die Incremental Export API -----------------
  # Der Ablauf steckt in .fetch_zendesk_incremental(); get_zendesk_tickets()
  # nutzt denselben Unterbau, damit es nur eine Implementierung gibt.
  delta <- .fetch_zendesk_incremental(
    subdomain  = subdomain,
    email      = email,
    api_token  = api_token,
    start_time = start_unix,
    max_pages  = max_pages
  )

  if (is.null(delta) || nrow(delta) == 0) {
    message("✅ Keine geaenderten Tickets seit dem letzten Sync.")
    return(invisible(list(
      n_delta = 0L,
      n_total = if (is.null(old_tickets)) 0L else nrow(old_tickets),
      start_time = start_time
    )))
  }

  # Die Incremental API kann denselben Ticket-Stand mehrfach ausliefern
  # (Cursor-Ueberlappung) -> pro id nur den juengsten Stand behalten.
  delta <- delta[order(delta$updated_at, decreasing = TRUE), , drop = FALSE] |>
    dplyr::distinct(id, .keep_all = TRUE)

  message(sprintf(
    "🔄 %s geaenderte Tickets geladen.",
    format(nrow(delta), big.mark = ".", decimal.mark = ",")
  ))

  # --- 3. Upsert: neue Zeile gewinnt, geloeschte Historie bleibt erhalten ---
  final <- if (!is.null(old_tickets)) {
    dplyr::bind_rows(delta, old_tickets) |> dplyr::distinct(id, .keep_all = TRUE)
  } else {
    delta
  }

  saveRDS(final, file = file_path)
  message(sprintf(
    "💾 Gespeichert: %s (%s Tickets gesamt).", file_path,
    format(nrow(final), big.mark = ".", decimal.mark = ",")
  ))

  invisible(list(n_delta = nrow(delta), n_total = nrow(final), start_time = start_time))
}
