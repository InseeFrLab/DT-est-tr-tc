Ce dossier rassemble tous les programmes relatifs aux analyses à la paramétrisation locale des méthodes polynomiales en faisant varier l'I/C ratio.
Il utilise les données simulées et les données réelles : il faut avoir lancer les programmes `R_fredm/0_download_files.R` et `R_simul/1A_data_creation.R`.
S'ils n'ont pas été lancés, ils le seront dans les premiers programmes.


- `0_Construction_mm.R` : construit les différentes moyennes mobiles testées pour la paramétrisation locale
- `1A_Est_pente_finale.R` : estime les paramètres locaux lorsque les données futures sont connues (filtre symétrique utilisé)
- `1B_Est_pente_daf.R` : estime les paramètres locaux en temps réel (ensemble de filtres symétrique et asymétriques) 
- `2A_Est_final.R` : estimation avec les différentes méthodes en utilisant les paaramètres locaux finaux
- `2B_Est_dafic.R` : estimation avec les différentes méthodes en utilisant les paaramètres locaux estimés en temps réel
- `2C_Est_dafic_trunc.R` : estimation avec les différentes méthodes en utilisant les paaramètres locaux estimés en temps réel mais dont les valeurs trop élevées sont tronquées
- `3_Summary_tp.R` : calcul des statistiques sur le déphasage
- `4A_Analyse_simul.R` : analyse des résultats sur les données simulées
- `4B_Analyse_fred.R` : analyse des résultats sur les données réelles