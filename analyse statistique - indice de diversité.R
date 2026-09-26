# ==============================================================================
# Script    : indices_diversite_herpetofaune.R
# Objet     : Calculer les indices de diversité de l'herpétofaune
#             1. Nettoyage des noms de taxons (fusion des doublons)
#             2. Calcul des indices : Richesse, Shannon, Simpson, Piélou
#             3. Export d'un rapport lisible + tableau CSV
# Auteur    : RANAIVOJAONA Tsirimanjaka Tsinjoary
# Version   : 1.0
# ------------------------------------------------------------------------------
# Entrées   : Fichier Excel (feuille "data", colonnes `type` et `Taxon`).
# Sorties   : Un fichier .log lisible + un fichier .csv récapitulatif.
# ==============================================================================


# ==============================================================================
# 0. DÉPENDANCES
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(vegan)
  library(stringr)
})


# ==============================================================================
# 1. CONSTANTES GLOBALES
# ==============================================================================

ALPHA           <- 0.05
SHEET_NAME      <- "data"
COL_TYPE        <- "type"
COL_TAXON       <- "Taxon"
GROUP_AMPHIBIAN <- "amphibien"
GROUP_REPTILE   <- "reptile"
GROUPE_HERPETO  <- "Herpétofaune"
GROUPE_AMPH     <- "Amphibiens"
GROUPE_REPT     <- "Reptiles"


# ==============================================================================
# 2. DIALOGUES UTILISATEUR
# ==============================================================================

#' Demande à l'utilisateur de choisir le fichier Excel à analyser
#'
#' @return Chemin absolu du fichier Excel choisi.
#' @export
demander_fichier_excel <- function() {
  cat("Veuillez sélectionner le fichier Excel contenant vos données...\n")
  chemin <- tryCatch(
    file.choose(new = FALSE),
    error = function(e) NULL
  )
  if (is.null(chemin)) {
    stop("Aucun fichier sélectionné. Le script est interrompu.")
  }
  cat(sprintf("Fichier sélectionné : %s\n", chemin))
  return(chemin)
}


#' Demande à l'utilisateur de choisir le dossier de sauvegarde des résultats
#'
#' @return Chemin absolu du dossier choisi.
#' @export
demander_dossier_sortie <- function() {
  cat("Veuillez sélectionner le dossier où enregistrer les résultats...\n")
  dossier <- tryCatch(
    utils::choose.dir(caption = "Choisissez le dossier de sauvegarde"),
    error = function(e) NA_character_
  )
  if (is.na(dossier) || !nzchar(dossier)) {
    dossier <- getwd()
    cat(sprintf("Aucun dossier choisi. Utilisation du dossier courant : %s\n", dossier))
  } else {
    cat(sprintf("Dossier de sauvegarde : %s\n", dossier))
  }
  return(dossier)
}


# ==============================================================================
# 3. LOGGER PÉDAGOGIQUE (destiné à l'utilisateur)
# ==============================================================================

#' Initialise le fichier de log avec un en-tête lisible
#'
#' @param log_path Chemin complet du fichier log.
#' @return Une connexion fichier ouverte.
#' @export
init_logger <- function(log_path) {
  con <- file(log_path, open = "wt", encoding = "UTF-8")
  
  cat("================================================================\n", file = con)
  cat("        RAPPORT D'ANALYSE DES INDICES DE DIVERSITÉ\n", file = con)
  cat("              Herpétofaune de Maromizaha\n", file = con)
  cat("================================================================\n\n", file = con)
  cat(sprintf("Auteur : RANAIVOJAONA Tsirimanjaka Tsinjoary\n"), file = con)
  cat(sprintf("Version : 1.0\n"), file = con)
  cat(sprintf("Date de l'analyse : %s\n", format(Sys.time(), "%d/%m/%Y à %H:%M:%S")), file = con)
  cat("\n", file = con)
  
  return(con)
}


#' Écrit un message pédagogique dans le log
#'
#' @param con Connexion fichier.
#' @param msg Texte à écrire.
#' @export
log_msg <- function(con, msg = "") {
  cat(msg, "\n", file = con, sep = "")
}


#' Écrit un séparateur de section dans le log
#'
#' @param con Connexion fichier.
#' @param titre Titre de la section.
#' @export
log_section <- function(con, titre) {
  cat("\n", file = con)
  cat("----------------------------------------------------------------\n", file = con)
  cat(sprintf("  %s\n", titre), file = con)
  cat("----------------------------------------------------------------\n", file = con)
}


#' Ferme proprement le log avec un pied de page
#'
#' @param con Connexion fichier.
#' @export
close_logger <- function(con) {
  if (inherits(con, "connection") && isOpen(con)) {
    cat("\n================================================================\n", file = con)
    cat("                     FIN DU RAPPORT\n", file = con)
    cat("================================================================\n", file = con)
    close(con)
  }
}


# ==============================================================================
# 4. CHARGEMENT ET NETTOYAGE DES DONNÉES
# ==============================================================================

#' Charge, nettoie et prépare les données herpétofauniques
#'
#' @param chemin Chemin du fichier Excel.
#' @param con Connexion log.
#' @return Data.frame filtré (amphibiens + reptiles, taxon non nul).
#' @export
charger_donnees <- function(chemin, con) {
  donnees <- read_excel(chemin, sheet = SHEET_NAME)
  
  # 4.1 Supprimer les espaces en trop dans les noms de taxons
  donnees[[COL_TAXON]] <- str_trim(donnees[[COL_TAXON]])
  
  # 4.2 Fusionner les doublons (fautes de frappe et synonymes)
  donnees <- donnees %>%
    mutate(Taxon = case_when(
      # --- Correction Mantidactilus -> Mantidactylus ---
      Taxon == "Mantidactilus femoralis"    ~ "Mantidactylus femoralis",
      Taxon == "Mantidactilus grandidieri"  ~ "Mantidactylus grandidieri",
      Taxon == "Mantidactilus opiparis"     ~ "Mantidactylus opiparis",
      
      # --- Correction Eroplatus -> Uroplatus ---
      Taxon == "Eroplatus fontastictus"     ~ "Uroplatus fontastictus",
      
      # --- Correction Brokesia -> Brookesia ---
      Taxon == "Brokesia superciliaris"     ~ "Brookesia superciliaris",
      
      # --- Correction Pareudora -> Paroedura ---
      Taxon == "Pareudora gracilis"         ~ "Paroedura gracilis",
      
      TRUE ~ Taxon
    ))
  
  # 4.3 Filtrer les données valides
  donnees_herp <- donnees %>%
    filter(.data[[COL_TYPE]] %in% c(GROUP_AMPHIBIAN, GROUP_REPTILE)) %>%
    filter(!is.na(.data[[COL_TAXON]]), .data[[COL_TAXON]] != "")
  
  log_msg(con, sprintf("Fichier analysé : %s", basename(chemin)))
  log_msg(con, sprintf("Nombre total d'observations retenues : %d", nrow(donnees_herp)))
  
  n_amph <- sum(donnees_herp[[COL_TYPE]] == GROUP_AMPHIBIAN)
  n_rept <- sum(donnees_herp[[COL_TYPE]] == GROUP_REPTILE)
  log_msg(con, sprintf("  - Amphibiens : %d observations", n_amph))
  log_msg(con, sprintf("  - Reptiles   : %d observations", n_rept))
  log_msg(con, sprintf("  - Nombre de taxons uniques après nettoyage : %d",
                       n_distinct(donnees_herp[[COL_TAXON]])))
  
  return(donnees_herp)
}


# ==============================================================================
# 5. CONSTRUCTION DES MATRICES D'ABONDANCE
# ==============================================================================

#' Construit une matrice d'abondance (1 ligne x S colonnes)
#'
#' @param donnees Data.frame filtré.
#' @param filtre_type Type à filtrer (NULL pour tous).
#' @return Matrice numérique 1 x S.
#' @export
construire_matrice <- function(donnees, filtre_type = NULL) {
  if (!is.null(filtre_type)) {
    donnees <- donnees %>% filter(.data[[COL_TYPE]] == filtre_type)
  }
  
  matrice <- donnees %>%
    group_by(Taxon) %>%
    summarise(Abondance = n(), .groups = "drop") %>%
    tibble::column_to_rownames("Taxon") %>%
    t()
  
  return(matrice)
}


# ==============================================================================
# 6. CALCUL DES INDICES DE DIVERSITÉ
# ==============================================================================

#' Calcule les indices de diversité pour une matrice d'abondance
#'
#' @param matrice Matrice d'abondance (1 x S).
#' @return Liste : N, Richesse, Shannon, Simpson, Pielou.
#' @export
calculer_indices <- function(matrice) {
  
  N        <- sum(matrice)
  S        <- specnumber(matrice)
  H        <- diversity(matrice, "shannon")
  Simpson  <- diversity(matrice, "simpson")
  J        <- H / log(S)
  
  list(
    N       = N,
    Richesse = S,
    Shannon = H,
    Simpson = Simpson,
    Pielou  = J
  )
}


#' Calcule les indices pour les trois groupes (herpétofaune, amphibiens, reptiles)
#'
#' @param donnees Data.frame filtré.
#' @param con Connexion log.
#' @return Data.frame récapitulatif des indices.
#' @export
calculer_tous_indices <- function(donnees, con) {
  
  log_section(con, "2. CALCUL DES INDICES DE DIVERSITÉ")
  log_msg(con, "Objectif : quantifier la diversité biologique de chaque groupe.")
  log_msg(con)
  log_msg(con, "Indices calculés :")
  log_msg(con, "  - Richesse spécifique (S) : nombre total d'espèces")
  log_msg(con, "  - Shannon (H')           : diversité tenant compte des abondances")
  log_msg(con, "  - Simpson (1-D)          : probabilité que 2 individus soient d'espèces différentes")
  log_msg(con, "  - Piélou (J)             : équitabilité (H' / log S)")
  log_msg(con)
  
  # --- A. HERPÉTOFAUNE ENTIÈRE ---
  matrice_herpeto <- construire_matrice(donnees)
  indices_herpeto <- calculer_indices(matrice_herpeto)
  
  log_msg(con, "--- HERPÉTOFAUNE ENTIÈRE ---")
  log_msg(con, sprintf("  Richesse spécifique (S) : %d", indices_herpeto$Richesse))
  log_msg(con, sprintf("  Indice de Shannon (H')  : %.3f", indices_herpeto$Shannon))
  log_msg(con, sprintf("  Indice de Simpson (1-D) : %.3f", indices_herpeto$Simpson))
  log_msg(con, sprintf("  Équitabilité de Piélou (J) : %.3f", indices_herpeto$Pielou))
  log_msg(con)
  
  # --- B. AMPHIBIENS ---
  matrice_amph <- construire_matrice(donnees, GROUP_AMPHIBIAN)
  indices_amph <- calculer_indices(matrice_amph)
  
  log_msg(con, "--- AMPHIBIENS ---")
  log_msg(con, sprintf("  Richesse spécifique (S) : %d", indices_amph$Richesse))
  log_msg(con, sprintf("  Indice de Shannon (H')  : %.3f", indices_amph$Shannon))
  log_msg(con, sprintf("  Indice de Simpson (1-D) : %.3f", indices_amph$Simpson))
  log_msg(con, sprintf("  Équitabilité de Piélou (J) : %.3f", indices_amph$Pielou))
  log_msg(con)
  
  # --- C. REPTILES ---
  matrice_rept <- construire_matrice(donnees, GROUP_REPTILE)
  indices_rept <- calculer_indices(matrice_rept)
  
  log_msg(con, "--- REPTILES ---")
  log_msg(con, sprintf("  Richesse spécifique (S) : %d", indices_rept$Richesse))
  log_msg(con, sprintf("  Indice de Shannon (H')  : %.3f", indices_rept$Shannon))
  log_msg(con, sprintf("  Indice de Simpson (1-D) : %.3f", indices_rept$Simpson))
  log_msg(con, sprintf("  Équitabilité de Piélou (J) : %.3f", indices_rept$Pielou))
  log_msg(con)
  
  # --- D. TABLEAU RÉCAPITULATIF ---
  resultats <- data.frame(
    Groupe   = c(GROUPE_HERPETO, GROUPE_AMPH, GROUPE_REPT),
    N        = c(indices_herpeto$N, indices_amph$N, indices_rept$N),
    Richesse = c(indices_herpeto$Richesse, indices_amph$Richesse, indices_rept$Richesse),
    Shannon  = round(c(indices_herpeto$Shannon, indices_amph$Shannon, indices_rept$Shannon), 3),
    Simpson  = round(c(indices_herpeto$Simpson, indices_amph$Simpson, indices_rept$Simpson), 3),
    Pielou   = round(c(indices_herpeto$Pielou, indices_amph$Pielou, indices_rept$Pielou), 3),
    stringsAsFactors = FALSE
  )
  
  return(resultats)
}


# ==============================================================================
# 7. CONSTRUCTION DU RÉSUMÉ CSV
# ==============================================================================

#' Construit le data.frame récapitulatif des résultats
#'
#' @param resultats Data.frame des indices.
#' @return Data.frame exportable en CSV.
#' @export
construire_resume <- function(resultats) {
  
  resume <- data.frame(
    Etape = c("Nombre total d'individus (Herpétofaune)",
              "Richesse spécifique (Herpétofaune)",
              "Indice de Shannon (Herpétofaune)",
              "Indice de Simpson (Herpétofaune)",
              "Équitabilité de Piélou (Herpétofaune)",
              "Nombre total d'individus (Amphibiens)",
              "Richesse spécifique (Amphibiens)",
              "Indice de Shannon (Amphibiens)",
              "Indice de Simpson (Amphibiens)",
              "Équitabilité de Piélou (Amphibiens)",
              "Nombre total d'individus (Reptiles)",
              "Richesse spécifique (Reptiles)",
              "Indice de Shannon (Reptiles)",
              "Indice de Simpson (Reptiles)",
              "Équitabilité de Piélou (Reptiles)"),
    Valeur = c(
      sprintf("%d", resultats$N[1]),
      sprintf("%d", resultats$Richesse[1]),
      sprintf("%.3f", resultats$Shannon[1]),
      sprintf("%.3f", resultats$Simpson[1]),
      sprintf("%.3f", resultats$Pielou[1]),
      sprintf("%d", resultats$N[2]),
      sprintf("%d", resultats$Richesse[2]),
      sprintf("%.3f", resultats$Shannon[2]),
      sprintf("%.3f", resultats$Simpson[2]),
      sprintf("%.3f", resultats$Pielou[2]),
      sprintf("%d", resultats$N[3]),
      sprintf("%d", resultats$Richesse[3]),
      sprintf("%.3f", resultats$Shannon[3]),
      sprintf("%.3f", resultats$Simpson[3]),
      sprintf("%.3f", resultats$Pielou[3])
    ),
    stringsAsFactors = FALSE
  )
  
  return(resume)
}


# ==============================================================================
# 8. PIPELINE PRINCIPAL
# ==============================================================================

#' Exécute le pipeline complet d'analyse
#'
#' @export
main <- function() {
  
  # ---- 8.1 Dialogue utilisateur -------------------------------------------
  chemin_donnees <- demander_fichier_excel()
  dossier_sortie <- demander_dossier_sortie()
  
  log_path <- file.path(dossier_sortie, "rapport_indices_diversite.log")
  csv_path <- file.path(dossier_sortie, "resultats_indices_diversite.csv")
  
  # ---- 8.2 Initialisation du log ------------------------------------------
  con <- init_logger(log_path)
  on.exit(close_logger(con), add = TRUE)
  
  # ---- 8.3 Chargement des données -----------------------------------------
  log_section(con, "1. CHARGEMENT DES DONNÉES")
  donnees_herp <- charger_donnees(chemin_donnees, con)
  
  # ---- 8.4 Calcul des indices ---------------------------------------------
  resultats <- calculer_tous_indices(donnees_herp, con)
  
  # ---- 8.5 Affichage du tableau final dans le log --------------------------
  log_section(con, "3. TABLEAU RÉCAPITULATIF")
  log_msg(con, "Voici le tableau des indices de diversité :")
  log_msg(con)
  
  # Écriture manuelle du tableau dans le log
  log_msg(con, sprintf("%-15s | %6s | %8s | %8s | %8s | %7s",
                       "Groupe", "N", "Richesse", "Shannon", "Simpson", "Pielou"))
  log_msg(con, paste(rep("-", 70), collapse = ""))
  for (i in seq_len(nrow(resultats))) {
    log_msg(con, sprintf("%-15s | %6d | %8d | %8.3f | %8.3f | %7.3f",
                         resultats$Groupe[i],
                         resultats$N[i],
                         resultats$Richesse[i],
                         resultats$Shannon[i],
                         resultats$Simpson[i],
                         resultats$Pielou[i]))
  }
  log_msg(con)
  
  # ---- 8.6 Conclusion -----------------------------------------------------
  log_section(con, "4. INTERPRÉTATION")
  log_msg(con, sprintf("L'herpétofaune de Maromizaha est représentée par %d individus",
                       resultats$N[1]))
  log_msg(con, sprintf("répartis en %d taxons, avec un indice de Shannon de %.3f",
                       resultats$Richesse[1], resultats$Shannon[1]))
  log_msg(con, sprintf("et une équitabilité de Piélou de %.3f.", resultats$Pielou[1]))
  log_msg(con)
  log_msg(con, sprintf("Les amphibiens dominent largement (%d individus, %d taxons),",
                       resultats$N[2], resultats$Richesse[2]))
  log_msg(con, sprintf("tandis que les reptiles sont moins représentés (%d individus, %d taxons).",
                       resultats$N[3], resultats$Richesse[3]))
  log_msg(con)
  log_msg(con, "La diversité observée est élevée, mais marquée par la dominance")
  log_msg(con, "de quelques taxons abondants (Gephyromantis luteus, Boophis boehmei).")
  
  # ---- 8.7 Fichiers générés -----------------------------------------------
  log_section(con, "5. FICHIERS GÉNÉRÉS")
  log_msg(con, sprintf("  - Rapport texte : %s", log_path))
  log_msg(con, sprintf("  - Tableau CSV   : %s", csv_path))
  
  # ---- 8.8 Export CSV -----------------------------------------------------
  resume <- construire_resume(resultats)
  write.csv(resume, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat(sprintf("\nAnalyse terminée. Résultats enregistrés dans :\n  - %s\n  - %s\n",
              log_path, csv_path))
  
  invisible(list(resultats = resultats, log = log_path, csv = csv_path))
}


# ==============================================================================
# 9. POINT D'ENTRÉE
# ==============================================================================

if (interactive()) {
  resultats <- main()
}