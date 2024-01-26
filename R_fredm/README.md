Ce dossier rassemble tous les programmes relatifs aux analyses sur les données simulées (sauf ceux où l'on fait varier localement le ratio I/C qui sont dans le dossier `R_local_ic`).


- `0_download_files.R` : pour télécharger les séries et ne sélectionner que celles utilisées
- `1_infobyseries.R` : on calcule des statistiques sur toutes les séries (ratio I/C, longueur "optimale")
- `2A_est_lp.R`, `2B_est_rkhs.R`, `2C_est_fst.R`, `2D_est_arima.R` : estimation des tendance_cycle à chaque date pour les différentes méthodes du document de travail et détection des points de retournement. Les résultats sont stockés dans un dossier `results_fredm`
- `3B_summary_of_tp_norev.R` : calcul du déphasage sur toutes les méthodes mais en utilisant une définition différente : nombre de mois nécessaires pour détecter un point de retournement **sans révision future**
- `4_utils.R` :  fonctions utilisées dans les programmes suivants pour résumer toutes les sorties et effectuer des graphiques
- `4A_Analyse_est.R` : analyse des estimations successives de la tendance_cycle autour de certains points de retournement (utilise aussi les résultats sur la paramétrisation locale des filtres polynomiaux à partir des programmes du dossier `R_local_ic`)
- `4B_Analyse_implicit_forecast_dt.R` : analyse des prévisions implicites (utilise aussi les résultats sur la paramétrisation locale des filtres polynomiaux à partir des programmes du dossier `R_local_ic`)
