# schmierblatt

#' Lade alle Tickets aus Zendesk herunter und speichere sie lokal
#'
#' Diese Funktion verbindet sich mit der Zendesk API und lädt mittels
#' Cursor-Pagination alle verfügbaren Tickets herunter. Wenn bereits eine
#' Datei existiert, werden alte (gelöschte) Tickets behalten und bestehende aktualisiert.
#'
#' @details
#' `mode = "incremental"` (Default) nutzt die *Incremental Export API*
#' (`/api/v2/incremental/tickets/cursor.json`). Nur sie liefert **archivierte
#' Tickets** mit: Zendesk nimmt geschlossene Tickets nach rund 120 Tagen aus
#' der List-API heraus. Mit `mode = "list"` (dem alten Verhalten, Endpunkt
#' `/api/v2/tickets.json`) fehlte dadurch im Bestand alles, was aelter als
#' rund vier Monate und geschlossen war -- am 13.10.2026 waren das 194.253
#' Tickets, ausnahmslos `closed` und alle erstellt vor dem 02.11.2025.
#'
#' `mode = "list"` ist nur noch fuer den Vergleich da. Fuer den taeglichen
#' Betrieb ist [update_zendesk_data()] die richtige Funktion -- sie leitet den
#' Startpunkt aus dem lokalen Bestand ab, statt jedes Mal alles zu laden.
#'
#' @param subdomain Character. Die Subdomain deines Zendesk-Kontos (z. B. "feew").
#' @param email Character. Die E-Mail-Adresse, mit der du dich bei Zendesk anmeldest.
#' @param api_token Character. Dein Zendesk API-Token.
#' @param save Logical. Gibt an, ob die Daten als .rds-Datei gespeichert werden sollen. Standard ist TRUE.
#' @param filename Character. Der Name der Datei (inkl. .rds Endung). Standard ist "all_tickets.rds".
#' @param merge_existing Logical. Wenn TRUE, werden neu geladene Daten mit der bestehenden lokalen Datei gemischt (Upsert), um in Zendesk gelöschte Tickets zu behalten.
#' @param mode Character. `"incremental"` (Default) laedt ueber die Incremental
#'   Export API inklusive archivierter Tickets, `"list"` ueber die alte
#'   List-API ohne sie. Siehe Details.
#' @param start_time POSIXct oder Character (`"YYYY-MM-DD"`). Nur bei
#'   `mode = "incremental"`: Startpunkt des Abzugs. Default `"2023-09-01"` --
#'   vor dem ersten Ticket des Kontos, also ein Vollabzug.
#' @param max_pages Integer. Nur bei `mode = "incremental"`: Obergrenze an
#'   Seiten a 1.000 Tickets. Default 2000.
#'
#' @return Ein Data Frame (Tibble) mit allen Zendesk-Tickets.
#' @export
#'
#' @importFrom httr GET authenticate status_code content
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#'
#' @examples
#' \dontrun{
#' tickets <- get_zendesk_tickets(
#'   subdomain = "feew",
#'   email = "muster@ffe3.de",
#'   api_token = "dein_geheimer_token"
#' )
#' }
get_zendesk_tickets <- function(subdomain, email, api_token, save = TRUE,
                                filename = "all_tickets.rds",
                                merge_existing = TRUE,
                                mode = c("incremental", "list"),
                                start_time = "2023-09-01",
                                max_pages = 2000) {
  mode <- match.arg(mode)
  # 1. Lokale Pfade laden
  if (file.exists("~/workspace/local.R")) {
    source("~/workspace/local.R", local = TRUE)
  } else {
    stop("Die Datei '~/workspace/local.R' wurde nicht gefunden. Bitte anlegen, damit 'datadir' definiert ist.")
  }

  # 2. Daten laden -- je nach Modus ueber die Incremental- oder die List-API
  if (mode == "incremental") {
    if (is.character(start_time)) {
      start_time <- as.POSIXct(start_time, tz = "UTC")
    }
    message(sprintf(
      "Incremental Export ab %s UTC (inkl. archivierter Tickets) ...",
      format(start_time, "%Y-%m-%d %H:%M:%S")
    ))

    new_tickets_df <- .fetch_zendesk_incremental(
      subdomain  = subdomain,
      email      = email,
      api_token  = api_token,
      start_time = start_time,
      max_pages  = max_pages
    )

    if (is.null(new_tickets_df)) {
      new_tickets_df <- data.frame()
    } else {
      # Cursor-Ueberlappung: pro id nur den juengsten Stand behalten.
      new_tickets_df <- new_tickets_df[
        order(new_tickets_df$updated_at, decreasing = TRUE), ,
        drop = FALSE
      ] |>
        dplyr::distinct(id, .keep_all = TRUE)
    }
  } else {
    user_auth <- paste0(email, "/token")
    url <- paste0("https://", subdomain, ".zendesk.com/api/v2/tickets.json?page[size]=100")

    all_tickets_list <- list()
    page_counter <- 1
    has_more <- TRUE

    while (has_more) {
      message("Lade Cursor-Seite ", page_counter, " ...")

      response <- httr::RETRY(
        verb = "GET",
        url = url,
        config = httr::authenticate(user_auth, api_token),
        times = 5,
        pause_base = 5,
        quiet = FALSE
      )

      if (httr::status_code(response) == 200) {
        raw_content <- httr::content(response, "text", encoding = "UTF-8")
        parsed_data <- jsonlite::fromJSON(raw_content, flatten = TRUE)

        all_tickets_list[[page_counter]] <- parsed_data$tickets
        has_more <- parsed_data$meta$has_more
        url <- parsed_data$links[["next"]]

        page_counter <- page_counter + 1
        Sys.sleep(1)
      } else {
        stop("Fehler auf Seite ", page_counter, " - Statuscode: ", httr::status_code(response))
      }
    }

    new_tickets_df <- dplyr::bind_rows(all_tickets_list)
  }

  # 5. Zwischenstand melden
  message("Download abgeschlossen. Neue/Aktuelle Tickets geladen: ", nrow(new_tickets_df))

  # 6. Speichern und Zusammenführen (Merge/Upsert)
  if (save) {
    if (!exists("datadir")) {
      stop("Die Variable 'datadir' wurde in ~/workspace/local.R nicht gefunden.")
    }

    zendesk_dir <- file.path(datadir, "zendesk")
    if (!dir.exists(zendesk_dir)) {
      dir.create(zendesk_dir, recursive = TRUE)
    }

    file_path <- file.path(zendesk_dir, filename)

    # --- DER MAGISCHE MERGE-PART ---
    if (merge_existing && file.exists(file_path)) {
      message("Bestehende Datei gefunden. Führe Update durch und bewahre gelöschte Tickets...")

      old_tickets_df <- readRDS(file_path)

      # Neue Daten ZUERST, alte Daten danach. Dann Duplikate anhand der ID löschen.
      # Dadurch gewinnt immer die neue Zeile, aber alte/gelöschte IDs bleiben am Ende hängen.
      final_tickets_df <- dplyr::bind_rows(new_tickets_df, old_tickets_df) |>
        dplyr::distinct(id, .keep_all = TRUE)

      message("Merge erfolgreich. Gesamtanzahl Tickets jetzt: ", nrow(final_tickets_df))
    } else {
      # Falls noch keine alte Datei da ist oder merge_existing = FALSE
      final_tickets_df <- new_tickets_df
    }
    # -------------------------------

    saveRDS(final_tickets_df, file = file_path)
    message("Daten erfolgreich gespeichert unter: ", file_path)

    final_tickets_df
  } else {
    # Wenn nicht gespeichert werden soll, gib einfach die neu geladenen zurück
    new_tickets_df
  }
}
