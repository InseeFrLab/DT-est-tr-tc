Ce dossier rassemble les programmes pour générer les filtres FST et RKHS utilisés dans le document de travail :

- `generate_fst.R` : génère l'ensemble des filtres FST étudiés dans le document de travail en faisant un quadrillage de [0,1]^3^. 
Des fichiers temporaires sont créés dans un dossier `fst` contenant les moyennes mobiles pour différentes longueurs (paramètre h) et les fichiers `fst_pdegree0.RDS` à `fst_pdegree4.RDS` rassemble toutes les moyennes mobiles.  

- `generate_rkhs.R` : génère les filtres RKHS en utilisant les valeurs de Bianconcini, Dagum (2015), *A new set of filters for tracking the short-term trend in real time*

- `plot_coef.R` : trace les graphiques de toutes les moyennes mobiles retenues dans le DT pour les comparaisons graphiques