PRO use_metric_for_K2

;;; on recupère la métrique faite sur les données Kepler
restore, file='/Users/lbugnet/DATA/TABLES/metric_all_stars.sav', /verbose;, xx, yy, res, slope_fit, output_a2zp, output_resize,  out
output_Kepler=output_a2zp
;;; on récupere les données K2
restore, file='/Users/lbugnet/DATA/METRIC/K2/C4/C4RESULTS_A2Zp_variab_metrics_Lor_K2_C4.sav', /verbose ;OUTPUT_A2ZP
output_K2=output_a2zp





















END