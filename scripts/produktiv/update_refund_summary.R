# ============================================================================
# update_refund_summary.R
# ============================================================================
# Schneidet die Tages-Erstattungsstatistik des Retourenportals mit.
#
# WARUM MITSCHNEIDEN STATT ABFRAGEN:
# Der Endpunkt /api/refunds-daily-summary-csv ist KEIN historisches Register.
# Zweimal dieselben Datumswerte abgefragt, zwei Tage auseinander, ergibt beim
# zweiten Mal deutlich kleinere Zahlen -- und zwar umso kleiner, je aelter der
# Tag ist (der aelteste noch verfuegbare Tag lag beim Nachmessen bei rund einem
# Zehntel seines urspruenglichen Werts). Die PayPal-Spalten leeren sich zuerst,
# die Klarna-Spalten halten laenger durch. Aelter als rund zwei Wochen liefert
# der Endpunkt ueberhaupt nichts mehr -- ein Backfill ist ausgeschlossen.
#
# Woran das liegt, ist beim IT-Team des Portals angefragt (Stand 14.10.2026
# offen): unklar ist, was die zugrundeliegende Tabelle verlaesst und welcher
# der beiden Werte "alle an Tag X gebuchten Erstattungen" meint.
#
# Die Messreihe dazu steht nicht hier, sondern im Bestand selbst: Zeilen mit
# source = "api" stammen aus diesem Skript, alle anderen sind nachtraeglich
# eingespielte, aeltere Beobachtungen. Ein Vergleich beider ueber dasselbe
# refund_date zeigt den Verfall. (Dieses Repo ist oeffentlich -- konkrete
# Geschaeftszahlen gehoeren nicht in den Quelltext.)
#
# KONSEQUENZ FUER DIESES SKRIPT: Es wird NICHTS ueberschrieben. Jeder Abruf
# haengt seine Beobachtungen an, Schluessel ist (refund_date, fetched_at).
# Ein Upsert auf refund_date wuerde den frueheren, hoeheren Wert aktiv durch den
# spaeteren, kleineren ersetzen -- genau der stille Datenverlust, den dieses
# Skript verhindern soll. Solange
# jeder Abruf mit Zeitstempel erhalten bleibt, laesst sich die am Ende
# richtige Definition rueckwirkend rekonstruieren.
#
# Aufruf: aus update_dashboard.R gesourct (Schritt 3b), oder standalone per
#   Rscript ~/git/dieseoR/scripts/produktiv/update_refund_summary.R
# ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(httr)
})

RS_BASE_URL <- "https://retoure-api.pammys.com/api/refunds-daily-summary-csv"
RS_STORE <- path.expand("~/data/returns/refund_summary.rds")

# Fenstergroesse: der Endpunkt haelt rund 14 Tage vor. 21 Tage geben Luft,
# falls ein Lauf ausfaellt, und kosten nichts -- es ist eine Anfrage.
RS_WINDOW_DAYS <- as.integer(Sys.getenv("REFUND_SUMMARY_WINDOW_DAYS", unset = "21"))

# Die Datumswerte gehen als wiederholter Query-Parameter `dates[]` raus. Bei
# sehr grossen Fenstern wird die URL sonst zu lang -- deshalb in Haeppchen.
RS_CHUNK <- 60L

# Erwartete Spalten. Explizit, damit eine Schema-Aenderung am Portal laut
# auffaellt, statt still eine Spalte mit NA zu fuellen.
RS_COLS <- c(
  "refund_date", "total_orders", "total_refund",
  "automatic_count", "automatic_refund", "manual_count", "manual_refund",
  "klarna_total", "klarna_refund", "klarna_automatic", "klarna_automatic_refund",
  "klarna_manual", "klarna_manual_refund",
  "paypal_total", "paypal_refund", "paypal_automatic", "paypal_automatic_refund",
  "paypal_manual", "paypal_manual_refund"
)

rs_api_key <- Sys.getenv("RETOUREN_API_KEY")
if (!nzchar(rs_api_key)) {
  stop("RETOUREN_API_KEY ist nicht gesetzt -- Erstattungsstatistik nicht abrufbar.")
}

# --- 1. Abruf ---------------------------------------------------------------

rs_fetch_chunk <- function(dates) {
  query <- stats::setNames(as.list(dates), rep("dates[]", length(dates)))
  resp <- httr::RETRY(
    verb = "GET", url = RS_BASE_URL, query = query,
    config = httr::add_headers(`N8N-API-KEY` = rs_api_key),
    times = 4, pause_base = 3, quiet = TRUE, httr::timeout(120)
  )
  if (httr::status_code(resp) != 200) {
    stop(sprintf(
      "refunds-daily-summary-csv antwortete mit HTTP %d (%d Datumswerte angefragt).",
      httr::status_code(resp), length(dates)
    ))
  }
  body <- httr::content(resp, "text", encoding = "UTF-8")

  # Leere Antwort: der Endpunkt laesst Tage ohne Daten weg und liefert dann
  # unter Umstaenden nur die Kopfzeile. Das ist kein Fehler.
  parsed <- utils::read.csv(
    text = body, stringsAsFactors = FALSE,
    colClasses = "character"
  )
  if (nrow(parsed) == 0L) {
    return(parsed[, intersect(RS_COLS, names(parsed)), drop = FALSE])
  }

  fehlend <- setdiff(RS_COLS, names(parsed))
  neu <- setdiff(names(parsed), RS_COLS)
  if (length(fehlend)) {
    stop(
      "Erstattungsstatistik: erwartete Spalten fehlen in der Antwort: ",
      paste(fehlend, collapse = ", ")
    )
  }
  if (length(neu)) {
    message(
      "   ⚠️  Portal liefert NEUE Spalten (werden mitgeschrieben): ",
      paste(neu, collapse = ", ")
    )
  }
  parsed
}

alle_tage <- format(seq(Sys.Date() - (RS_WINDOW_DAYS - 1L), Sys.Date(), by = "day"))
message(
  "[", Sys.time(), "] Erstattungsstatistik: frage ", length(alle_tage),
  " Tage ab (", alle_tage[1], " bis ", alle_tage[length(alle_tage)], ") ..."
)

haeppchen <- split(alle_tage, ceiling(seq_along(alle_tage) / RS_CHUNK))
antwort <- haeppchen |>
  lapply(rs_fetch_chunk) |>
  dplyr::bind_rows()

fetched_at <- Sys.time()

# --- 2. Beobachtung zusammenbauen -------------------------------------------
# Eine Zeile je ANGEFRAGTEM Tag, nicht je gelieferter Zeile. `in_response`
# haelt fest, ob das Portal fuer diesen Tag etwas hatte -- ein Tag ohne
# Antwort ist eine Information (Wochenende, oder aus dem Fenster gefallen)
# und darf nicht als Luecke verschwinden.
num_cols <- setdiff(RS_COLS, "refund_date")

beobachtung <- data.frame(refund_date = as.Date(alle_tage), stringsAsFactors = FALSE) |>
  dplyr::left_join(
    if (nrow(antwort)) {
      antwort |>
        dplyr::mutate(
          refund_date = as.Date(.data$refund_date),
          dplyr::across(dplyr::all_of(num_cols), as.numeric)
        ) |>
        dplyr::mutate(in_response = TRUE)
    } else {
      data.frame(refund_date = as.Date(character(0)), in_response = logical(0))
    },
    by = "refund_date"
  ) |>
  dplyr::mutate(
    in_response = dplyr::coalesce(.data$in_response, FALSE),
    fetched_at = fetched_at,
    # Herkunft mitschreiben: der Bestand ist Beweismaterial. "api" = von diesem
    # Skript geholt, alles andere ist nachtraeglich eingespielt (z. B. eine
    # frueher von Hand gezogene CSV) und traegt einen ungenaueren Zeitstempel.
    source = "api"
  ) |>
  dplyr::relocate(dplyr::all_of(c("refund_date", "fetched_at", "source", "in_response")))

message(
  "   ", sum(beobachtung$in_response), " von ", nrow(beobachtung),
  " Tagen mit Daten geliefert."
)

# --- 3. Veraenderungen sichtbar machen ---------------------------------------
# Der Wertverfall ist der eigentliche Grund fuer dieses Skript. Er gehoert ins
# Log, nicht in die Stille -- sonst faellt eine Aenderung des Portal-Verhaltens
# (z. B. weil das IT-Team nachbessert) niemandem auf.

bestand <- if (file.exists(RS_STORE)) readRDS(RS_STORE) else NULL
if (!is.null(bestand) && nrow(bestand) && !"source" %in% names(bestand)) {
  bestand$source <- "api" # Bestand aus der Zeit vor der Herkunftsspalte
}

if (!is.null(bestand) && nrow(bestand)) {
  letzter_stand <- bestand |>
    dplyr::filter(.data$in_response) |>
    dplyr::group_by(.data$refund_date) |>
    dplyr::slice_max(.data$fetched_at, n = 1L, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::select(refund_date, alt_orders = "total_orders", alt_refund = "total_refund")

  vergleich <- beobachtung |>
    dplyr::filter(.data$in_response) |>
    dplyr::inner_join(letzter_stand, by = "refund_date") |>
    dplyr::mutate(diff_orders = .data$total_orders - .data$alt_orders) |>
    dplyr::filter(.data$diff_orders != 0) |>
    dplyr::arrange(.data$refund_date)

  if (nrow(vergleich)) {
    gefallen <- sum(vergleich$diff_orders < 0)
    gestiegen <- sum(vergleich$diff_orders > 0)
    message(
      "   Veraenderung gegenueber dem letzten Abruf: ", gestiegen,
      " Tage gestiegen, ", gefallen, " Tage GEFALLEN."
    )
    for (i in seq_len(nrow(vergleich))) {
      z <- vergleich[i, ]
      message(sprintf(
        "     %s: %d -> %d Bestellungen (%+d), %.2f -> %.2f EUR",
        format(z$refund_date), as.integer(z$alt_orders), as.integer(z$total_orders),
        as.integer(z$diff_orders), z$alt_refund, z$total_refund
      ))
    }
  } else {
    message("   Keine Veraenderung gegenueber dem letzten Abruf.")
  }
}

# --- 4. Anhaengen und atomar speichern ---------------------------------------
# Reines Append. Einziger Dedup: exakt derselbe Schluessel (refund_date,
# fetched_at) -- das passiert nur, wenn ein Lauf zweimal in derselben Sekunde
# startet. Alles andere bleibt erhalten, auch widerspruechliche Werte: genau
# die sind hier das Beweismaterial.
neu <- dplyr::bind_rows(bestand, beobachtung) |>
  dplyr::distinct(.data$refund_date, .data$fetched_at, .keep_all = TRUE) |>
  dplyr::arrange(.data$refund_date, .data$fetched_at)

dir.create(dirname(RS_STORE), recursive = TRUE, showWarnings = FALSE)
tmp <- paste0(RS_STORE, ".tmp")
saveRDS(neu, tmp)
if (!file.rename(tmp, RS_STORE)) {
  stop("Konnte ", tmp, " nicht nach ", RS_STORE, " umbenennen.")
}

message(
  "✅ Erstattungsstatistik mitgeschnitten: ", nrow(beobachtung),
  " Beobachtungen angehaengt, Bestand jetzt ", nrow(neu), " Zeilen (",
  dplyr::n_distinct(neu$refund_date), " verschiedene Tage, aeltester ",
  format(min(neu$refund_date)), ")."
)

# --- 5. Serving-Schnitt fuers Dashboard --------------------------------------
# Das Dashboard zeigt den TAGESSTAND, wie ihn das Portal ausliefert -- also
# genau die CSV, die man beim Abruf von heute bekaeme. Deshalb nur die Zeilen
# des juengsten API-Abrufs, nicht etwa je Tag der hoechste je gesehene Wert:
# "die Tabelle, die das Portal taeglich zur Verfuegung stellt" ist eine
# definierte Groesse, "der wahre Wert" waere eine erfundene.
#
# Der vollstaendige Bestand bleibt in RS_STORE und wird hier nicht angefasst.
# Wer spaeter den Verlauf braucht (etwa wenn das IT-Team den Verfall erklaert
# hat), liest ihn von dort -- diese Datei ist bewusst nur der Tagesstand.
DASH_DATA <- path.expand("~/git/dashboard/data")

serving <- neu |>
  dplyr::filter(.data$source == "api", .data$in_response) |>
  dplyr::filter(.data$fetched_at == max(.data$fetched_at)) |>
  dplyr::select(-dplyr::all_of(c("source", "in_response"))) |>
  dplyr::arrange(dplyr::desc(.data$refund_date))

if (nrow(serving) == 0L) {
  message(
    "⚠️  Kein Tagesstand zum Ausliefern (Abruf lieferte keine Zeile) -- ",
    "data/refund_summary.rds bleibt unveraendert."
  )
} else if (!dir.exists(DASH_DATA)) {
  message("⚠️  ", DASH_DATA, " existiert nicht -- Serving-Schnitt uebersprungen.")
} else {
  serving_path <- file.path(DASH_DATA, "refund_summary.rds")
  tmp_serving <- paste0(serving_path, ".tmp")
  saveRDS(serving, tmp_serving)
  if (!file.rename(tmp_serving, serving_path)) {
    stop("Konnte ", tmp_serving, " nicht nach ", serving_path, " umbenennen.")
  }
  message(
    "   Serving-Schnitt geschrieben: ", nrow(serving), " Tage (",
    format(min(serving$refund_date)), " bis ", format(max(serving$refund_date)), ")."
  )
}
