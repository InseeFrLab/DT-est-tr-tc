Ce dossier rassemble tous les programmes relatifs aux analyses sur les données simulées (sauf ceux où l'on fait varier localement le ratio I/C qui sont dans le dossier `R_local_ic`).

- `1A_data_creation.R` : création des données simulées
- `1B_plot_data.R` : graphique des données simulées pour le document de travail
- `2A_est_lp.R`, `2B_est_rkhs.R`, `2C_est_fst.R`, `2D_est_arima.R` : estimation des tendance_cycle à chaque date pour les différentes méthodes du document de travail et détection des points de retournement. Les résultats sont stockés dans un dossier `results_simul` 
- `3A_summary_of_tp.R` : calcul du déphasage sur toutes les méthodes 
- `3B_summary_of_tp_norev.R` : calcul du déphasage sur toutes les méthodes mais en utilisant une définition différente : nombre de mois nécessaires pour détecter un point de retournement **sans révision future**
- `3C_summary_of_rev.R` : analyse des statistiques sur les révisions
- `4_utils.R` :  fonctions utilisées dans les programmes suivants pour résumer toutes les sorties et effectuer des graphiques
- `4A_Select_fst_filter.R` : sélection du filtre FST optimal
- `4B_Analyse_tp.R` : analyse des statistiques sur les points de retournement (utilise aussi les résultats sur la paramétrisation locale des filtres polynomiaux à partir des programmes du dossier `R_local_ic`)
- `4C_Analyse_tp_by_kernel.R` : analyse des statistiques sur les points de retournement en utilisant différents noyaux pour l'estimation polynomiale
- `5_analyse_revisions.R` : analyse des révisions (utilise aussi les résultats sur la paramétrisation locale des filtres polynomiaux à partir des programmes du dossier `R_local_ic`)
- `9_cross_validation_test.R` : tests sur la validation croisée pour la sélection de la longueur du filtre, non mentionné dans le DT


