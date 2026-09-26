# ==============================================================================
# Script    : analyse_herpetofaune.R
# Objet     : Comparer l'altitude entre amphibiens et reptiles
#             1. Test de normalité (Shapiro-Wilk)
#             2. Test de comparaison (Student ou Mann-Whitney)
#             3. Corrélation bisérielle de rang (taille d'effet)
# Auteur    : RANAIVOJAONA Tsirimanjaka Tsinjoary
# Version   : 1.0
# ------------------------------------------------------------------------------
# Entrées   : Fichier Excel (feuille "data", colonnes `altitude` et `type`).
# Sorties   : Un fichier .log lisible + un fichier .csv récapitulatif.
# ==============================================================================


# ==============================================================================
# 0. DÉPENDANCES
# ==============================================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
})


# ==============================================================================
# 1. CONSTANTES GLOBALES
# ==============================================================================

ALPHA           <- 0.05
SHEET_NAME      <- "data"
COL_TYPE        <- "type"
COL_ALTITUDE    <- "altitude"
GROUP_AMPHIBIAN <- "amphibien"
GROUP_REPTILE   <- "reptile"


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
  cat("           RAPPORT D'ANALYSE STATISTIQUE\n", file = con)
  cat("     Comparaison de l'altitude : amphibiens vs reptiles\n", file = con)
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
# 4. CHARGEMENT DES DONNÉES
# ==============================================================================

#' Charge et prépare les données herpétofauniques
#'
#' @param chemin Chemin du fichier Excel.
#' @param con Connexion log.
#' @return Data.frame filtré (amphibiens + reptiles, altitude non nulle).
#' @export
charger_donnees <- function(chemin, con) {
  donnees <- read_excel(chemin, sheet = SHEET_NAME)
  
  donnees[[COL_ALTITUDE]] <- suppressWarnings(as.numeric(donnees[[COL_ALTITUDE]]))
  
  donnees_herp <- donnees %>%
    filter(.data[[COL_TYPE]] %in% c(GROUP_AMPHIBIAN, GROUP_REPTILE)) %>%
    filter(!is.na(.data[[COL_ALTITUDE]]))
  
  log_msg(con, sprintf("Fichier analysé : %s", basename(chemin)))
  log_msg(con, sprintf("Nombre total d'observations retenues : %d", nrow(donnees_herp)))
  
  n_amph <- sum(donnees_herp[[COL_TYPE]] == GROUP_AMPHIBIAN)
  n_rept <- sum(donnees_herp[[COL_TYPE]] == GROUP_REPTILE)
  log_msg(con, sprintf("  - Amphibiens : %d observations", n_amph))
  log_msg(con, sprintf("  - Reptiles   : %d observations", n_rept))
  
  return(donnees_herp)
}


# ==============================================================================
# 5. TESTS STATISTIQUES
# ==============================================================================

#' Test de normalité (Shapiro-Wilk) avec explication pédagogique
#'
#' @param x Vecteur numérique.
#' @param nom_groupe Nom du groupe affiché à l'utilisateur.
#' @param con Connexion log.
#' @return Liste : statistic, p.value, normal.
#' @export
tester_normalite <- function(x, nom_groupe, con) {
  test   <- shapiro.test(x)
  normal <- test$p.value > ALPHA
  
  log_msg(con, sprintf("Groupe : %s", nom_groupe))
  log_msg(con, sprintf("  Statistique W = %.4f", test$statistic))
  log_msg(con, sprintf("  p-value       = %.4f", test$p.value))
  
  if (normal) {
    log_msg(con, sprintf("  p > %.2f : les données suivent une loi normale.", ALPHA))
  } else {
    log_msg(con, sprintf("  p < %.2f : les données NE suivent PAS une loi normale.", ALPHA))
  }
  
  list(statistic = unname(test$statistic),
       p.value   = test$p.value,
       normal    = normal)
}


#' Choisit et exécute le test de comparaison adapté
#'
#' @param data Data.frame filtré.
#' @param normal_amph Booléen normalité amphibiens.
#' @param normal_rept Booléen normalité reptiles.
#' @param con Connexion log.
#' @return Liste : nom_test, statistic, p.value.
#' @export
test_comparaison <- function(data, normal_amph, normal_rept, con) {
  formule <- as.formula(paste(COL_ALTITUDE, "~", COL_TYPE))
  
  if (normal_amph && normal_rept) {
    log_msg(con, "Les deux groupes suivent une loi normale.")
    log_msg(con, "Test paramétrique choisi : test t de Student.")
    res <- t.test(formule, data = data)
    nom <- "Test t de Student"
  } else {
    log_msg(con, "Au moins un groupe ne suit pas une loi normale.")
    log_msg(con, "Test non-paramétrique choisi : test de Mann-Whitney.")
    res <- wilcox.test(formule, data = data)
    nom <- "Test de Mann-Whitney"
  }
  
  log_msg(con, sprintf("Statistique du test = %.4f", res$statistic))
  log_msg(con, sprintf("p-value            = %.4f", res$p.value))
  
  list(nom_test  = nom,
       statistic = unname(res$statistic),
       p.value   = res$p.value)
}


#' Calcule les médianes et effectifs par groupe
#'
#' @param data Data.frame filtré.
#' @return Tibble : type, n, mediane_altitude.
#' @export
calculer_medianes <- function(data) {
  data %>%
    group_by(type = .data[[COL_TYPE]]) %>%
    summarise(
      n                = dplyr::n(),
      mediane_altitude = median(.data[[COL_ALTITUDE]], na.rm = TRUE),
      .groups = "drop"
    )
}


#' Corrélation bisérielle de rang (Rank-Biserial Correlation)
#'
#' Mesure la taille d'effet associée au test de Mann-Whitney.
#' Formule : r_rb = 1 - (2 * U) / (n1 * n2)
#'
#' @param donnees Data.frame filtré (amphibiens + reptiles).
#' @param con Connexion log.
#' @return Liste : U, n_amph, n_rept, r_rb, force, sens.
#' @export
correlation_biserielle_rang <- function(donnees, con) {
  
  log_section(con, "4. TAILLE D'EFFET : CORRÉLATION BISÉRIELLE DE RANG")
  log_msg(con, "Objectif : quantifier l'intensité de la différence observée.")
  log_msg(con, "Cette mesure complète le test de Mann-Whitney en indiquant")
  log_msg(con, "si l'effet est négligeable, faible, modéré ou fort.")
  log_msg(con)
  
  # --- Test de Mann-Whitney -------------------------------------------------
  test_mw <- wilcox.test(
    as.formula(paste(COL_ALTITUDE, "~", COL_TYPE)),
    data = donnees
  )
  U <- unname(test_mw$statistic)
  
  # --- Effectifs ------------------------------------------------------------
  n_amph <- sum(donnees[[COL_TYPE]] == GROUP_AMPHIBIAN)
  n_rept <- sum(donnees[[COL_TYPE]] == GROUP_REPTILE)
  
  # --- Calcul du r_rb -------------------------------------------------------
  r_rb <- 1 - (2 * U) / (n_amph * n_rept)
  
  log_msg(con, sprintf("Statistique U de Mann-Whitney : %.1f", U))
  log_msg(con, sprintf("Effectif amphibiens (n1)     : %d", n_amph))
  log_msg(con, sprintf("Effectif reptiles (n2)       : %d", n_rept))
  log_msg(con, sprintf("Corrélation bisérielle r_rb  : %.4f", r_rb))
  log_msg(con)
  
  # --- Interprétation de la force -------------------------------------------
  r_abs <- abs(r_rb)
  
  force <- dplyr::case_when(
    r_abs < 0.10 ~ "négligeable",
    r_abs < 0.30 ~ "faible",
    r_abs < 0.50 ~ "modéré",
    TRUE         ~ "fort"
  )
  
  log_msg(con, sprintf("|r_rb| = %.4f : effet %s", r_abs, force))
  log_msg(con)
  
  # --- Interprétation du sens -----------------------------------------------
  if (r_rb < 0) {
    sens <- "Les reptiles ont tendance à être observés à des altitudes plus élevées."
  } else if (r_rb > 0) {
    sens <- "Les amphibiens ont tendance à être observés à des altitudes plus élevées."
  } else {
    sens <- "Aucune tendance observée (r_rb = 0)."
  }
  log_msg(con, sprintf("Signe : %s", sens))
  
  list(U      = U,
       n_amph = n_amph,
       n_rept = n_rept,
       r_rb   = r_rb,
       force  = force,
       sens   = sens)
}


# ==============================================================================
# 6. CONSTRUCTION DU RÉSUMÉ CSV
# ==============================================================================

#' Construit le data.frame récapitulatif des résultats
#'
#' @param norm_amph Résultat Shapiro amphibiens.
#' @param norm_rept Résultat Shapiro reptiles.
#' @param test_cmp Résultat du test de comparaison.
#' @param medianes Tibble des médianes.
#' @param corr_bis Résultat de la corrélation bisérielle.
#' @return Data.frame exportable en CSV.
#' @export
construire_resume <- function(norm_amph, norm_rept, test_cmp, medianes, corr_bis) {
  
  conclusion <- if (test_cmp$p.value < ALPHA) {
    "Différence SIGNIFICATIVE d'altitude entre amphibiens et reptiles"
  } else {
    "Pas de différence significative d'altitude entre amphibiens et reptiles"
  }
  
  med_amph <- medianes$mediane_altitude[medianes$type == GROUP_AMPHIBIAN]
  med_rept <- medianes$mediane_altitude[medianes$type == GROUP_REPTILE]
  n_amph   <- medianes$n[medianes$type == GROUP_AMPHIBIAN]
  n_rept   <- medianes$n[medianes$type == GROUP_REPTILE]
  
  data.frame(
    Etape  = c("Normalité amphibiens",
               "Normalité reptiles",
               "Test de comparaison",
               "Statistique du test",
               "p-value du test",
               "Médiane altitude amphibiens",
               "Médiane altitude reptiles",
               "Nombre d'amphibiens",
               "Nombre de reptiles",
               "Corrélation bisérielle (r_rb)",
               "Force de l'effet",
               "Sens de l'effet",
               "Conclusion"),
    Valeur = c(
      sprintf("W=%.4f, p=%.4f (%s)", norm_amph$statistic, norm_amph$p.value,
              ifelse(norm_amph$normal, "normale", "non normale")),
      sprintf("W=%.4f, p=%.4f (%s)", norm_rept$statistic, norm_rept$p.value,
              ifelse(norm_rept$normal, "normale", "non normale")),
      test_cmp$nom_test,
      sprintf("%.4f", test_cmp$statistic),
      sprintf("%.4f", test_cmp$p.value),
      sprintf("%.2f", med_amph),
      sprintf("%.2f", med_rept),
      sprintf("%d", n_amph),
      sprintf("%d", n_rept),
      sprintf("%.4f", corr_bis$r_rb),
      corr_bis$force,
      corr_bis$sens,
      conclusion
    ),
    stringsAsFactors = FALSE
  )
}


# ==============================================================================
# 7. PIPELINE PRINCIPAL
# ==============================================================================

#' Exécute le pipeline complet d'analyse
#'
#' @export
main <- function() {
  
  # ---- 7.1 Dialogue utilisateur -------------------------------------------
  chemin_donnees <- demander_fichier_excel()
  dossier_sortie <- demander_dossier_sortie()
  
  log_path <- file.path(dossier_sortie, "rapport_analyse_herpetofaune.log")
  csv_path <- file.path(dossier_sortie, "resultats_analyse_herpetofaune.csv")
  
  # ---- 7.2 Initialisation du log ------------------------------------------
  con <- init_logger(log_path)
  on.exit(close_logger(con), add = TRUE)
  
  # ---- 7.3 Chargement des données -----------------------------------------
  log_section(con, "1. CHARGEMENT DES DONNÉES")
  donnees_herp <- charger_donnees(chemin_donnees, con)
  
  alt_amph <- donnees_herp[[COL_ALTITUDE]][donnees_herp[[COL_TYPE]] == GROUP_AMPHIBIAN]
  alt_rept <- donnees_herp[[COL_ALTITUDE]][donnees_herp[[COL_TYPE]] == GROUP_REPTILE]
  
  # ---- 7.4 Tests de normalité ---------------------------------------------
  log_section(con, "2. VÉRIFICATION DE LA NORMALITÉ")
  log_msg(con, "Objectif : vérifier si les altitudes suivent une loi normale,")
  log_msg(con, "afin de choisir le test de comparaison le plus adapté.")
  log_msg(con)
  log_msg(con, "Test utilisé : Shapiro-Wilk (H0 = les données sont normales)")
  log_msg(con)
  
  norm_amph <- tester_normalite(alt_amph, "Amphibiens", con)
  log_msg(con)
  norm_rept <- tester_normalite(alt_rept, "Reptiles", con)
  
  # ---- 7.5 Test de comparaison --------------------------------------------
  log_section(con, "3. COMPARAISON DES DEUX GROUPES")
  log_msg(con, "Objectif : déterminer si l'altitude moyenne (ou médiane)")
  log_msg(con, "diffère significativement entre amphibiens et reptiles.")
  log_msg(con)
  
  test_cmp <- test_comparaison(donnees_herp, norm_amph$normal, norm_rept$normal, con)
  
  # ---- 7.6 Corrélation bisérielle de rang ---------------------------------
  corr_bis <- correlation_biserielle_rang(donnees_herp, con)
  
  # ---- 7.7 Conclusion générale --------------------------------------------
  log_section(con, "5. CONCLUSION GÉNÉRALE")
  
  if (test_cmp$p.value < ALPHA) {
    log_msg(con, sprintf("p-value = %.4f < %.2f", test_cmp$p.value, ALPHA))
    log_msg(con, "Il existe une différence SIGNIFICATIVE d'altitude")
    log_msg(con, "entre les amphibiens et les reptiles.")
  } else {
    log_msg(con, sprintf("p-value = %.4f >= %.2f", test_cmp$p.value, ALPHA))
    log_msg(con, "Il n'y a PAS de différence significative d'altitude")
    log_msg(con, "entre les amphibiens et les reptiles.")
  }
  log_msg(con)
  log_msg(con, sprintf("Taille d'effet (r_rb = %.4f) : effet %s.",
                       corr_bis$r_rb, corr_bis$force))
  log_msg(con, corr_bis$sens)
  
  # ---- 7.8 Médianes --------------------------------------------------------
  medianes <- calculer_medianes(donnees_herp)
  
  log_section(con, "6. MÉDIANES D'ALTITUDE PAR GROUPE")
  for (i in seq_len(nrow(medianes))) {
    log_msg(con, sprintf("  - %s : médiane = %.2f m (n = %d)",
                         medianes$type[i],
                         medianes$mediane_altitude[i],
                         medianes$n[i]))
  }
  
  # ---- 7.9 Fichiers générés -----------------------------------------------
  log_section(con, "7. FICHIERS GÉNÉRÉS")
  log_msg(con, sprintf("  - Rapport texte : %s", log_path))
  log_msg(con, sprintf("  - Tableau CSV   : %s", csv_path))
  
  # ---- 7.10 Export CSV -----------------------------------------------------
  resume <- construire_resume(norm_amph, norm_rept, test_cmp, medianes, corr_bis)
  write.csv(resume, csv_path, row.names = FALSE, fileEncoding = "UTF-8")
  
  cat(sprintf("\nAnalyse terminée. Résultats enregistrés dans :\n  - %s\n  - %s\n",
              log_path, csv_path))
  
  invisible(list(resume = resume, log = log_path, csv = csv_path,
                 corr_bis = corr_bis))
}


# ==============================================================================
# 8. POINT D'ENTRÉE
# ==============================================================================

if (interactive()) {
  resultats <- main()
}