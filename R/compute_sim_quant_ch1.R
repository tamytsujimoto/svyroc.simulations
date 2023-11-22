#' Compute simulation quantities of interest
#'
#' @param sim: simulated data
#' @param roc_th: theoretical ROC curve
#'
#' @return

compute_sim_quant_ch1 <- function(sim, roc_th) {

  sim %>%
    dplyr::filter(fpr > 0, fpr < 1) %>%
    dplyr::mutate(fpr = as.character(fpr)) %>%
    dplyr::left_join(roc_th, by = 'fpr') %>%
    dplyr::mutate(bias_fin_comp = tpr_comp - tpr_pop,
                  bias_sup_comp = tpr_comp - tpr_th,
                  rel_bias_fin_comp = bias_fin_comp/tpr_pop,
                  rel_bias_sup_comp = bias_sup_comp/tpr_th,
                  ci_cov_fin = ifelse(tpr_pop >= ci_ll_fin & tpr_pop <= ci_ul_fin, 1, 0),
                  ci_cov_sup = ifelse(tpr_th >= ci_ll_sup & tpr_th <= ci_ul_sup, 1, 0)) %>%
    dplyr::group_by(scenario, fpr) %>%
    dplyr::summarise(rel_bias_fin_comp = mean(rel_bias_fin_comp, na.rm = TRUE)*100,
                     rel_bias_sup_comp = mean(rel_bias_sup_comp, na.rm = TRUE)*100,
                     emp_sd_comp = sd(tpr_comp, na.rm = TRUE),
                     asy_sd_roc_sup = mean(sqrt(vsvy_sup), na.rm = TRUE),
                     asy_sd_roc_fin = mean(sqrt(vsvy_fin), na.rm = TRUE),
                     ci_cov_fin = mean(ci_cov_fin, na.rm = TRUE)*100,
                     ci_cov_sup = mean(ci_cov_sup, na.rm = TRUE)*100) %>%
    dplyr::mutate(fpr = as.numeric(fpr))

}
