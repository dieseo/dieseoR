#' Zendesk-Tickets bereinigen und formatieren
#'
#' Diese Funktion nimmt einen rohen Zendesk-Datensatz (als Dateipfad oder
#' Data Frame), wandelt Datumsfelder um, extrahiert die Bestellnummer aus
#' den Textfeldern, übersetzt Agenten- und Gruppen-IDs in lesbare Namen
#' und strukturiert verschachtelte Listen (via, follower, collaborators) um.
#'
#' Seit dem 15.10.2026 gibt die Funktion garantiert **eine Zeile je Ticket**
#' zurück. Vorher erzeugte `unnest_longer(collaborator_ids)` je Collaborator
#' eine eigene Zeile, sodass `nrow()` nicht mehr der Ticketzahl entsprach.
#'
#' @param input Entweder ein Dateipfad (Character) zur .rds-Datei oder direkt der unbereinigte Data Frame.
#'
#' @return Ein bereinigter und strukturierter Data Frame (Tibble) mit genau
#'   einer Zeile je Ticket-ID. Die Collaborators stecken in zwei Spalten:
#'   `collaborator_id` (der erste, sonst `NA` -- gedacht für
#'   `is_alone = is.na(collaborator_id) & is.na(follower_ids_1)`) und
#'   `n_collaborators` (Anzahl, 0 wenn keiner).
#' @export
#'
#' @importFrom dplyr mutate select starts_with coalesce if_else
#' @importFrom tidyr unnest_wider unnest_longer
#' @importFrom stringr str_extract str_remove str_detect
#' @importFrom lubridate ymd_hms as_datetime
#' @importFrom purrr map
#' @importFrom base difftime
#' @importFrom tidyr nest
#' @examples
#' \dontrun{
#' # Aus einer Datei laden und bereinigen
#' clean_df <- clean_up_zendesk("data/zendesk/all_tickets.rds")
#'
#' # Oder einen bereits geladenen Data Frame übergeben
#' clean_df <- clean_up_zendesk(raw_tickets_df)
#' }
clean_up_zendesk <- function(input) {
  # 1. Daten einlesen (nutzt deine clean_master Funktion, falls Pfad übergeben wird)
  if (is.character(input)) {
    df <- clean_master(input)
  } else if (is.data.frame(input)) {
    df <- input
  } else {
    stop("Input muss entweder ein Dateipfad (Character) oder ein Data Frame sein.")
  }

  # 2. Datumsformate und erste Spaltenauswahl
  df <- df |>
    dplyr::mutate(
      created_at = lubridate::ymd_hms(created_at, tz = "UTC"),
      updated_at = lubridate::ymd_hms(updated_at, tz = "UTC"),
      generated_timestamp = lubridate::as_datetime(generated_timestamp, tz = "UTC")
    ) |>
    dplyr::select(
      -tidyselect::any_of(c(
        "url", "external_id", "organization_id", "forum_topic_id",
        "problem_id", "has_incidents", "due_at", "sharing_agreement_ids",
        "ticket_form_id", "brand_id", "allow_channelback",
        "satisfaction_rating_score", "is_public", "type"
      ))
    ) |>
    tidyr::unnest_wider(custom_fields, names_sep = "_")

  # 3. Bestellnummer extrahieren
  # Alle custom_fields zu einem String zusammenfassen (zeilenweise)
  custom_fields_text <- df |>
    dplyr::select(dplyr::starts_with("custom_fields_")) |>
    apply(1, paste, collapse = " ")

  df <- df |>
    dplyr::mutate(
      # Alles in eine Spalte packen für die Regex-Suche
      combined_text = paste(custom_fields_text, description, subject, raw_subject, sep = " "),

      # Primary Suche
      #
      # ⚠️ FIX 14.08.2026: Die alte Variante lautete
      #   "(?<=#)\\d{8}|\\b12\\d{6}\\b|\\b\\d{8}\\b"
      # und fing damit in JEDER Alternative exakt 8 Ziffern. Shopify-
      # Bestellnummern haben aber inzwischen 9 Stellen (#122583030), sodass
      # `(?<=#)\\d{8}` die letzte Ziffer abgeschnitten hat: aus 122583030
      # wurde 12258303. Ergebnis: die Spalte liess sich faktisch nicht mehr
      # gegen Shopify oder das Retourenportal joinen — von 438.661
      # Retouren matchten ganze 354. Gegenprobe gegen die strukturiert aus
      # `custom_fields` gelesenen Werte: in 239.022 von 239.166 Fällen
      # (99,9 %) war der alte Wert exakt substr(<echte Nummer>, 1, 8).
      #
      # Die Alternativen stehen jetzt 9-stellig zuerst: `str_extract` wählt
      # an einer Fundstelle die erstgenannte passende Alternative, nicht die
      # längste. Die 8-stelligen Varianten bleiben als Fallback für die
      # Alt-Bestellungen von vor dem Nummernwechsel erhalten.
      bestellnummer_primary = stringr::str_extract(
        combined_text,
        "(?<=#)\\d{9}|\\b12\\d{7}\\b|(?<=#)\\d{8}|\\b12\\d{6}\\b|\\b\\d{8}\\b"
      ),

      # Secondary Suche
      bestellnummer_secondary = stringr::str_extract(combined_text, "#\\d{9}|#\\d{8}") |>
        stringr::str_remove("^#")

      # HINWEIS: Die 13-stellige numerische Shopify-`order_id` steht zwar
      # ebenfalls als Custom-Field in den Rohdaten, lässt sich hier aber
      # NICHT per Regex herausziehen: `combined_text` enthält neben den
      # Feld-*Werten* auch die Feld-*IDs*, und die sind selbst 13-stellig
      # (z. B. 5832500351517). Ein Muster wie \\b[5-9]\\d{12}\\b trifft
      # deshalb zu 100 % die Feld-ID statt der Bestellung — geprüft am
      # 14.08.2026 gegen die strukturiert gelesenen Werte. Wer die order_id
      # braucht, muss `custom_fields` vor dem Zusammenkleben auswerten
      # (Feld-ID 27312698537501). Die 9-stellige Bestellnummer reicht als
      # Join-Schlüssel gegen Shopify und das Retourenportal aus.
    ) |>
    dplyr::mutate(
      bestellnummer_primary = dplyr::if_else(bestellnummer_primary == "12000000", NA_character_, bestellnummer_primary),
      bestellnummer_secondary = dplyr::if_else(bestellnummer_secondary == "12000000", NA_character_, bestellnummer_secondary),
      bestellnummer = dplyr::coalesce(bestellnummer_primary, bestellnummer_secondary)
    ) |>
    # Hilfsspalten wieder entfernen
    dplyr::select(-combined_text, -bestellnummer_primary, -bestellnummer_secondary)

  # 4. Mappings für IDs definieren
  id_name_map <- c(
    "8340805378845" = "stephanie",
    "8343002140829" = "laura",
    "17074770479005" = "anja",
    "18604712913053" = "erleta",
    "23356015505821" = "jessica",
    "26311427127837" = "altin",
    "27416907139357" = "AI",
    "30531051544093" = "anastasia",
    "22859146419357" = "abulena",
    "13506489916829" = "johanna",
    "12207192040477" = "nina",
    "5832429026205" = "pammys_support"
  )

  group_id_name_map <- c(
    "22911784253981" = "albulena_lena",
    "26311494547357" = "altin_alina",
    "30531277961245" = "anastasia_kyla",
    "17119690258461" = "anja_anna",
    "23777047107485" = "e_mail",
    "18604992762525" = "erleta_emily",
    "23356326562589" = "jessica_diana",
    "13531713501725" = "johanna_lina",
    "8342692463389"  = "laura_maya",
    "30282063790237" = "lea2",
    "8343306565533"  = "pammys_support2",
    "8342691778717"  = "stephanie_sophia",
    "5832422206621"  = "support",
    "32264914166173" = "support_leitung",
    "29004950157597" = "n8n_automation",
    "21323973965213" = "0smmak",
    "21449217355933" = "bestselling_lieferadresse",
    "21453222774173" = "oos",
    "21479307025565" = "schadensanzeige_nachforschungsantrag",
    "21479331372061" = "stornierung",
    "21453418350749" = "retourenportal",
    "21449287544349" = "falschlieferung",
    "21449298415901" = "reklamation",
    "21449322420381" = "umtausch",
    "21449278255901" = "rueckgabe_rechnung",
    "21453546452893" = "sonstiges",
    "21453280444701" = "hohe_prio_feedback",
    "21452898440605" = "meta",
    "21594391517981" = "paypal_klarna"
  )

  # Interne Helfer-Funktionen
  replace_ids_partial <- function(x) {
    x_chr <- as.character(x)
    for (pat in names(id_name_map)) {
      x_chr <- dplyr::if_else(
        !is.na(x_chr) & stringr::str_detect(x_chr, paste0("^", pat)),
        id_name_map[[pat]],
        x_chr
      )
    }
    x_chr[is.na(x_chr)] <- NA_character_
    return(x_chr)
  }

  replace_groups <- function(x) {
    x_chr <- as.character(x)
    x_chr <- dplyr::if_else(
      !is.na(x_chr) & x_chr %in% names(group_id_name_map),
      group_id_name_map[x_chr],
      x_chr
    )
    x_chr[is.na(x_chr)] <- NA_character_
    return(x_chr)
  }

  # 5. Mappings anwenden & finaler Feinschliff
  df_final <- df |>
    dplyr::mutate(
      # Sichere Abfrage für leere Listen eingebaut!
      collaborator_ids = purrr::map(collaborator_ids, ~ {
        if (length(.x) == 0 || all(is.na(.x))) {
          return(NA_character_)
        }
        replace_ids_partial(.x)
      }),
      follower_ids = purrr::map(follower_ids, ~ {
        if (length(.x) == 0 || all(is.na(.x))) {
          return(NA_character_)
        }
        replace_ids_partial(.x)
      }),
      assignee_id = replace_ids_partial(assignee_id),
      group_id = replace_groups(group_id)
    ) |>
    # ⚠️ FIX 15.10.2026: Hier stand `unnest_longer(collaborator_ids)`. Das machte
    # aus EINEM Ticket mit n Collaborators n ZEILEN -- im Bestand zuletzt 3.514
    # Zeilen auf 374 Ticket-IDs, eine davon 48-fach. Jede Auswertung, die
    # Tickets über Zeilen zählt, zählte diese mehrfach; im Dashboard betraf das
    # `total_tickets`/`geschlossen` (app.R), wodurch die Quote "alleine
    # bearbeitet" zu niedrig auswies.
    #
    # Gebraucht wird die Spalte nur für `is_alone = is.na(collaborator_id) &
    # is.na(follower_ids_1)`. Dafür genügt ein Skalar je Ticket: der erste
    # Collaborator, oder NA wenn es keinen gibt. `is.na(collaborator_id)` liefert
    # damit exakt dasselbe wie vorher -- nur eben einmal je Ticket.
    # `n_collaborators` hält die Information fest, die sonst mit den Zeilen
    # verloren ginge.
    dplyr::mutate(
      n_collaborators = vapply(collaborator_ids, function(x) {
        if (length(x) == 0 || all(is.na(x))) 0L else length(x)
      }, integer(1)),
      collaborator_id = vapply(collaborator_ids, function(x) {
        if (length(x) == 0 || all(is.na(x))) NA_character_ else as.character(x)[1]
      }, character(1))
    ) |>
    dplyr::select(
      -collaborator_ids,
      -email_cc_ids, -subject, -raw_subject,
      -custom_fields_id, -custom_fields_value, -custom_status_id,
      -followup_ids, -allow_attachments, -from_messaging_channel,
      -generated_timestamp
    ) |>
    dplyr::mutate(
      ticket_duration_min = as.numeric(difftime(updated_at, created_at, units = "mins")),
      ticket_duration_day = as.numeric(difftime(updated_at, created_at, units = "days"))
    ) |>
    tidyr::nest(via = dplyr::starts_with("via")) |>
    tidyr::unnest_wider(follower_ids, names_sep = "_")

  df_final
}
