# ==============================================================================
# SCRIPT: R/update_retouren_data.R
# PACKAGE: dieseoR
# ==============================================================================

#' @title Update der Retourendaten (Rate-Limit-bewusst, wiederaufnehmbar)
#'
#' @description Laedt Retouren aus dem Retourenportal und fuehrt sie per Upsert
#' (Schluessel: `id`) mit dem lokalen Bestand zusammen.
#'
#' **Warum hier kein echtes Delta moeglich ist:** die API kennt (Stand 10.08.2026)
#' weder einen `updated_since`-Filter noch Sortier-Parameter, deckelt `per_page`
#' auf 100 und limitiert auf 60 Requests/Minute. Sie liefert immer nach `id`
#' aufsteigend. Ein Datensatz vom Januar, dessen Refund heute gebucht wird,
#' bleibt daher an seiner alten Position -- man findet ihn nur, wenn man alle
#' Seiten durchgeht (`mode = "full"`).
#'
#' Zwei Modi:
#' \itemize{
#'   \item `"full"` -- alle Seiten. Faengt auch nachtraegliche Aenderungen an
#'     alten Retouren ein. Untergrenze durch das Rate-Limit: ca. 75-80 Minuten.
#'   \item `"tail"` -- nur die Seiten ab der hoechsten lokal bekannten `id`.
#'     Die noetige Seitenzahl wird selbst ermittelt (die API liefert `id`
#'     aufsteigend), es gibt also keine fixe Fenstergroesse, die zu klein
#'     geraten kann. Holt alle Neuzugaenge, aber **keine** Aenderungen an
#'     alten Datensaetzen.
#' }
#' Empfehlung: `"full"` einmal taeglich im Nachtlauf, `"tail"` fuer schnelle
#' Zwischenupdates tagsueber.
#'
#' Die Seiten werden waehrend des Laufs in Chunks zwischengespeichert. Bricht
#' der Lauf ab, setzt der naechste mit `resume = TRUE` dort fort, statt wieder
#' bei Seite 1 zu beginnen.
#'
#' @param api_key Character. API-Key fuer den Header `N8N-API-KEY`.
#' @param base_url Character. Basis-URL des Endpunkts (ohne Query-Parameter).
#' @param datadir Character. Zentrales Datenverzeichnis. Default `"~/data"`.
#' @param filename Character. Masterdatensatz. Default `"all_returns.rds"`.
#' @param mode Character. `"full"` oder `"tail"`. Default `"full"`.
#' @param tail_min_pages Integer. Mindestanzahl Seiten am Ende, die `"tail"`
#'   immer mitnimmt -- auch wenn rechnerisch weniger noetig waeren. Default 5.
#' @param requests_per_minute Integer. Rate-Limit-Budget. Default 55 (das Limit
#'   liegt bei 60; der Puffer faengt Parallelzugriffe anderer Clients ab).
#' @param resume Logical. Bei `TRUE` bereits geladene Chunks dieses Laufs
#'   wiederverwenden. Default `TRUE`.
#' @param try_since_filter Logical. Prueft zu Beginn, ob die API inzwischen
#'   einen serverseitigen updated_at-Filter unterstuetzt (erkannt daran, dass
#'   `total` kleiner wird). Falls ja, wird nur noch das Delta geladen und der
#'   Full-Sweep entfaellt -- ohne dass hier etwas geaendert werden muss.
#'   Default `TRUE`.
#' @param buffer_hours Integer. Sicherheitspuffer in Stunden fuer den
#'   erkannten Filter. Default 24.
#' @param max_pages Integer oder NULL. Obergrenze fuer Tests. Default `NULL`.
#'
#' @return Invisible List mit `n_fetched`, `n_new`, `n_total` und `mode`.
#'
#' @importFrom httr RETRY GET add_headers content status_code timeout headers
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows distinct
#' @export
#'
#' @examples
#' \dontrun{
#' # Nachtlauf: vollstaendig, faengt auch Late-Arriving-Refunds
#' update_retouren_data(
#'   api_key = Sys.getenv("RETOUREN_API_KEY"),
#'   base_url = "https://retoure-api.pammys.com/api/all-returns"
#' )
#'
#' # Schnelles Zwischenupdate: nur Neuzugaenge
#' update_retouren_data(
#'   api_key = Sys.getenv("RETOUREN_API_KEY"),
#'   base_url = "https://retoure-api.pammys.com/api/all-returns",
#'   mode = "tail"
#' )
#' }
update_retouren_data <- function(api_key,
                                 base_url,
                                 datadir = "~/data",
                                 filename = "all_returns.rds",
                                 mode = c("full", "tail"),
                                 tail_min_pages = 5,
                                 requests_per_minute = 55,
                                 resume = TRUE,
                                 try_since_filter = TRUE,
                                 buffer_hours = 24,
                                 max_pages = NULL) {
  mode <- match.arg(mode)
  if (missing(api_key) || api_key == "") stop("api_key muss uebergeben werden.")
  if (missing(base_url) || base_url == "") stop("base_url muss uebergeben werden.")

  returns_dir <- file.path(datadir, "returns")
  chunk_dir <- file.path(returns_dir, "raw_chunks")
  if (!dir.exists(chunk_dir)) dir.create(chunk_dir, recursive = TRUE)
  file_path <- file.path(returns_dir, filename)

  hdr <- httr::add_headers(`N8N-API-KEY` = api_key)
  min_gap <- 60 / requests_per_minute # Mindestabstand zwischen Request-STARTS

  # Zusatz-Query, die jedem Request angehaengt wird (z.B. ein erkannter
  # updated_at-Filter). Leer, solange die API keinen unterstuetzt.
  extra_query <- list()

  # Die Antwortzeit zaehlt gegen das Rate-Limit-Budget: nur die Restzeit bis zum
  # naechsten erlaubten Start abwarten, statt pauschal eine Sekunde zu schlafen.
  # Das allein spart bei ~4.500 Seiten rund eine Stunde.
  fetch_query <- function(q) {
    t_start <- Sys.time()
    resp <- httr::RETRY(
      verb = "GET", url = base_url, query = q,
      config = hdr, times = 5, pause_base = 3, quiet = TRUE, httr::timeout(180)
    )
    if (httr::status_code(resp) != 200) {
      stop(sprintf(
        "API-Fehler (Query %s): Status %d",
        paste(names(q), unlist(q), sep = "=", collapse = "&"),
        httr::status_code(resp)
      ))
    }
    parsed <- jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"),
      flatten = TRUE
    )
    elapsed <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
    if (elapsed < min_gap) Sys.sleep(min_gap - elapsed)
    parsed
  }

  fetch_page <- function(page) {
    fetch_query(c(list(per_page = 100, page = page), extra_query))
  }

  # --- 1. Erste Seite: Gesamtumfang ermitteln ------------------------------
  head_page <- fetch_page(1)
  last_page <- head_page$data$last_page
  total_api <- head_page$data$total
  if (is.null(last_page)) stop("Antwort ohne 'last_page' -- API-Format geaendert?")

  # --- 1b. Serverseitigen updated_at-Filter erkennen -----------------------
  # Stand 10.08.2026 ignoriert die API jeden Filter-Parameter. Sobald sie einen
  # unterstuetzt, ist der Full-Sweep ueberfluessig: dann liefert sie direkt nur
  # die seit `since` geaenderten Datensaetze -- inklusive der Januar-Retoure,
  # deren Refund heute gebucht wurde.
  # Erkennung ohne Doku: greift der Filter, MUSS `total` kleiner werden.
  # Faellt der Test negativ aus, bleibt es beim bisherigen Verhalten.
  detect_since_filter <- function(since_date) {
    for (nm in c("updated_since", "updated_at_min", "updated_from", "since")) {
      q <- list(per_page = 1)
      q[[nm]] <- since_date
      probe <- try(fetch_query(q), silent = TRUE)
      if (inherits(probe, "try-error")) next
      tot <- probe$data$total
      if (!is.null(tot) && !is.na(tot) && tot < total_api) {
        return(nm)
      }
    }
    NULL
  }

  if (mode == "full" && isTRUE(try_since_filter) && file.exists(file_path)) {
    loc <- readRDS(file_path)
    if ("updated_at" %in% names(loc) && nrow(loc) > 0) {
      last_upd <- suppressWarnings(max(as.POSIXct(loc$updated_at,
        format = "%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"
      ), na.rm = TRUE))
      if (is.finite(last_upd)) {
        since_date <- format(last_upd - buffer_hours * 3600, "%Y-%m-%d %H:%M:%S")
        since_param <- detect_since_filter(since_date)
        if (!is.null(since_param)) {
          message(sprintf(
            "🚀 API unterstuetzt '%s' -- lade nur Aenderungen seit %s.",
            since_param, since_date
          ))
          extra_query <- stats::setNames(list(since_date), since_param)
          head_page <- fetch_query(c(list(per_page = 100, page = 1), extra_query))
          last_page <- head_page$data$last_page
          total_api <- head_page$data$total
        } else {
          message("   (API kennt keinen updated_at-Filter -> vollstaendiger Sweep)")
        }
      }
    }
  }

  # --- Startseite bestimmen -------------------------------------------------
  # Die API liefert nach `id` aufsteigend. Im tail-Modus reicht es daher, ab der
  # Seite zu laden, auf der die hoechste lokal bekannte id liegt. Diese Seite
  # wird per Binaersuche gefunden (~12 Requests statt Raten), damit ein grosser
  # Rueckstand nicht stillschweigend durch ein zu kleines Fenster faellt.
  first_page <- 1L
  if (mode == "tail") {
    max_local <- NA_real_
    if (file.exists(file_path)) {
      loc <- readRDS(file_path)
      if ("id" %in% names(loc) && nrow(loc) > 0) max_local <- max(loc$id, na.rm = TRUE)
    }
    if (is.na(max_local)) {
      stop("mode = 'tail' braucht einen lokalen Bestand mit 'id'. Erstlauf bitte mit mode = 'full'.")
    }

    page_min_id <- function(p) {
      d <- fetch_page(p)$data$data
      if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) {
        return(Inf)
      }
      min(d$id, na.rm = TRUE)
    }
    lo <- 1L
    hi <- last_page
    while (lo < hi) { # kleinste Seite mit min(id) > max_local
      mid <- (lo + hi) %/% 2L
      if (page_min_id(mid) > max_local) hi <- mid else lo <- mid + 1L
    }
    first_page <- max(1L, lo - tail_min_pages)
    message(sprintf(
      "   Hoechste lokale id: %s -> starte bei Seite %s von %s.",
      format(max_local, big.mark = ".", decimal.mark = ","),
      format(first_page, big.mark = ".", decimal.mark = ","),
      format(last_page, big.mark = ".", decimal.mark = ",")
    ))
  }

  pages <- seq.int(first_page, last_page)
  if (!is.null(max_pages)) pages <- utils::head(pages, max_pages)

  message(sprintf(
    "📥 Retouren-Sync [%s]: %s Datensaetze in %s Seiten, hole %s Seiten.",
    mode, format(total_api, big.mark = ".", decimal.mark = ","), format(last_page, big.mark = ".", decimal.mark = ","),
    format(length(pages), big.mark = ".", decimal.mark = ",")
  ))
  est_min <- length(pages) * min_gap / 60
  message(sprintf(
    "   Geschaetzte Dauer bei %d Req/Min: ~%.0f Minuten.",
    requests_per_minute, est_min
  ))

  # Chunk-Ablage pro Lauf: erlaubt Resume nach Abbruch.
  run_tag <- format(Sys.Date(), "%Y%m%d")
  chunk_file <- function(p) file.path(chunk_dir, sprintf("returns_%s_%s_p%05d.rds", run_tag, mode, p))

  collected <- list()
  n_cached <- 0L
  t0 <- Sys.time()

  for (k in seq_along(pages)) {
    p <- pages[k]
    cf <- chunk_file(p)

    if (resume && file.exists(cf)) {
      collected[[length(collected) + 1L]] <- readRDS(cf)
      n_cached <- n_cached + 1L
      next
    }

    parsed <- if (p == 1L && k == 1L) head_page else fetch_page(p)
    d <- parsed$data$data
    if (is.null(d) || !is.data.frame(d) || nrow(d) == 0) next

    # Verschachtelte Positionsliste sofort auf flache Spalten eindampfen --
    # vor dem Cachen, damit weder die Chunks noch der Merge die 474k
    # verschachtelten Data-Frames mitschleppen. Siehe
    # summarise_requested_items() fuer die Begruendung.
    d <- summarise_requested_items(d)

    saveRDS(d, cf)
    collected[[length(collected) + 1L]] <- d

    if (k %% 250 == 0 || k == length(pages)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "mins"))
      message(sprintf(
        "   Seite %s/%s (%.0f%%) | %.1f Min gelaufen", format(k, big.mark = ".", decimal.mark = ","),
        format(length(pages), big.mark = ".", decimal.mark = ","), 100 * k / length(pages), el
      ))
    }
  }

  if (n_cached > 0) message(sprintf("   (%d Seiten aus Chunk-Cache wiederverwendet)", n_cached))

  if (length(collected) == 0) {
    message("⚠️ Keine Daten geladen -- Bestand bleibt unveraendert.")
    return(invisible(list(
      n_fetched = 0L, n_new = 0L, mode = mode,
      n_total = if (file.exists(file_path)) nrow(readRDS(file_path)) else 0L
    )))
  }

  fetched <- dplyr::bind_rows(collected) |> dplyr::distinct(id, .keep_all = TRUE)
  message(sprintf("🔄 %s Datensaetze geladen.", format(nrow(fetched), big.mark = ".", decimal.mark = ",")))

  # --- 2. Upsert: frisch geladene Zeile gewinnt ----------------------------
  n_before <- 0L
  final <- fetched
  if (file.exists(file_path)) {
    old <- readRDS(file_path)
    n_before <- nrow(old)
    # Typen der ID-Spalten angleichen, sonst bricht bind_rows() ab und der
    # gesamte Abruf ist verloren -- siehe .align_id_types() am Dateiende.
    aligned <- .align_id_types(fetched, old)
    final <- dplyr::bind_rows(aligned$a, aligned$b) |> dplyr::distinct(id, .keep_all = TRUE)
  }

  saveRDS(final, file = file_path)
  message(sprintf(
    "💾 Gespeichert: %s (%s gesamt, %s neu).", file_path,
    format(nrow(final), big.mark = ".", decimal.mark = ","),
    format(nrow(final) - n_before, big.mark = ".", decimal.mark = ",")
  ))

  # Chunks des erfolgreichen Laufs aufraeumen -- sie haben ihren Zweck erfuellt.
  unlink(list.files(chunk_dir, pattern = sprintf("^returns_%s_%s_", run_tag, mode), full.names = TRUE))

  invisible(list(
    n_fetched = nrow(fetched), n_new = nrow(final) - n_before,
    n_total = nrow(final), mode = mode
  ))
}

# ==============================================================================
# Interner Helfer -- fuehrender Punkt haelt ihn aus dem Namespace heraus
# (NAMESPACE exportiert per exportPattern alles, was mit einem Buchstaben
# beginnt).
# ==============================================================================

#' Gleicht die Typen der ID-Spalten an, bevor zwei Retouren-Staende gebunden
#' werden.
#'
#' WARUM ES DAS BRAUCHT: Die Retouren-API liefert `shopify_order_id` mal als
#' JSON-Zahl, mal als String -- abhaengig davon, was in der jeweils geholten
#' Seite vorkam. `bind_rows()` verweigert dann die Arbeit
#' ("Can't combine `..1$shopify_order_id` <double> and `..2$...` <character>"),
#' der komplette Abruf wird verworfen, und es faellt niemandem auf: Schritt 3
#' der Nacht-Pipeline meldet den Fehler nur per `warning()`. Am 17.09.2026
#' hat das 6.998 bereits geladene Datensaetze gekostet.
#'
#' STRATEGIE: Nur Spalten anfassen, deren Typen sich tatsaechlich
#' unterscheiden -- ohne Konflikt bleibt alles unveraendert.
#'   * ID-artige Spalten (`id`, `*_id`) werden beidseitig auf Character
#'     normalisiert. Das ist der verlustfreie gemeinsame Nenner: IDs sind
#'     Bezeichner, keine Rechengroessen.
#'   * Ist eine Seite eine logical-Spalte aus lauter NA, ist das kein
#'     Typwechsel: jsonlite macht aus einem Feld, das in ALLEN geholten
#'     Datensaetzen null ist, genau so eine Spalte. Sie wird verlustfrei in den
#'     Typ der Gegenseite gebracht. Ausloeser 01./02.11.2026: `deleted_at`
#'     (Bestand character) kam in zwei kleinen Abrufen ohne geloeschten
#'     Vorgang, weil der naechtliche DSGVO-Purge des Portals am 01.11.
#'     ausblieb.
#'   * Alle anderen Typkonflikte werden NICHT stillschweigend zusammengebogen,
#'     sondern mit Spaltennamen gemeldet. Dort waere eine automatische
#'     Umwandlung womoeglich ein echter Datenverlust (Betragsspalten!).
#'
#' `sprintf("%.0f", ...)` statt `as.character()`: letzteres kann bei grossen
#' Zahlen Exponentialschreibweise liefern ("5.12e+12") und wuerde die ID
#' damit zerstoeren. NA bleibt NA und wird nicht zum Text "NA".
#'
#' @param a,b Data Frames, die gebunden werden sollen.
#' @return Liste mit den angeglichenen Data Frames als `$a` und `$b`.
#' @keywords internal
.align_id_types <- function(a, b) {
  to_chr <- function(x) {
    if (is.character(x)) {
      return(x)
    }
    out <- rep(NA_character_, length(x))
    ok <- !is.na(x)
    out[ok] <- if (is.double(x)) sprintf("%.0f", x[ok]) else as.character(x[ok])
    out
  }
  # NA-Vektor der Laenge n im Typ von `vorlage`; `[` erhaelt Klasse und
  # Attribute (POSIXct samt tzone, Date, factor, list).
  nur_na <- function(x) is.logical(x) && all(is.na(x))
  na_wie <- function(vorlage, n) vorlage[rep(NA_integer_, n)]

  konflikt <- character(0)
  for (nm in intersect(names(a), names(b))) {
    ca <- class(a[[nm]])[1]
    cb <- class(b[[nm]])[1]
    if (identical(ca, cb)) next

    if (grepl("(^|_)id$", nm)) {
      a[[nm]] <- to_chr(a[[nm]])
      b[[nm]] <- to_chr(b[[nm]])
      message(sprintf("  \u2194 Typangleich '%s': %s / %s -> character", nm, ca, cb))
    } else if (nur_na(a[[nm]])) {
      a[[nm]] <- na_wie(b[[nm]], length(a[[nm]]))
      message(sprintf("  \u2194 Typangleich '%s': neu nur NA -> %s", nm, cb))
    } else if (nur_na(b[[nm]])) {
      b[[nm]] <- na_wie(a[[nm]], length(b[[nm]]))
      message(sprintf("  \u2194 Typangleich '%s': Bestand nur NA -> %s", nm, ca))
    } else {
      konflikt <- c(konflikt, sprintf("%s (neu: %s / Bestand: %s)", nm, ca, cb))
    }
  }

  if (length(konflikt)) {
    stop("Typkonflikt in Nicht-ID-Spalten, nicht automatisch angeglichen:\n  ",
      paste(konflikt, collapse = "\n  "),
      "\n  -> Erst pruefen, was die API dort geaendert hat. Eine blinde",
      "\n     Umwandlung koennte Werte verfaelschen.",
      call. = FALSE
    )
  }

  list(a = a, b = b)
}
