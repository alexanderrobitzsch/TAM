//// File Name: init.c
//// File Version: 3.000002
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

extern SEXP _TAM_calcfx(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_exp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_exp_redefine_vector_na(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_prob_subtract_max(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_prob(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_suff_stat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_ctt2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_ctt3(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_fit_simul(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_theta_sq(SEXP);
extern SEXP _TAM_tam_rcpp_interval_index(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_irt_likelihood_cfa(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_2pl_mstep_item_slopes_suffstat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_3pl_calc_Fdes(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_3pl_slca_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_3pl_calcexp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_3pl_compute_B(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_3pl_nonzero_entries(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_mfr_gresp_extend(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_mfr_gresp_na_facets(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_mfr_a_matrix_cumsum(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_mml_mfr_colsums_gresp(SEXP);
extern SEXP _TAM_tam_rcpp_mml_maxcat(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_modelfit_q3(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_modelfit_counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_modelfit_ccov(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_modelfit_residuals(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_msq_itemfit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_prior_normal_density_unequal_means(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_prior_normal_density_equal_means(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_pv_mcmc_likelihood(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_pv_mcmc_calc_probs_irf_3pl(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_pv_sample_theta_multidim(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_rowCumsums(SEXP);
extern SEXP _TAM_tam_rcpp_tam_np_posterior(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_wle_suffstat(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_wle_errinv(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_TAM_calcfx", (DL_FUNC) &_TAM_calcfx, 4},
    {"_TAM_tam_rcpp_calc_exp", (DL_FUNC) &_TAM_tam_rcpp_calc_exp, 10},
    {"_TAM_tam_rcpp_calc_exp_redefine_vector_na", (DL_FUNC) &_TAM_tam_rcpp_calc_exp_redefine_vector_na, 2},
    {"_TAM_tam_rcpp_calc_prob_subtract_max", (DL_FUNC) &_TAM_tam_rcpp_calc_prob_subtract_max, 4},
    {"_TAM_tam_rcpp_calc_prob", (DL_FUNC) &_TAM_tam_rcpp_calc_prob, 8},
    {"_TAM_tam_rcpp_calc_suff_stat", (DL_FUNC) &_TAM_tam_rcpp_calc_suff_stat, 6},
    {"_TAM_tam_rcpp_ctt2", (DL_FUNC) &_TAM_tam_rcpp_ctt2, 5},
    {"_TAM_tam_rcpp_ctt3", (DL_FUNC) &_TAM_tam_rcpp_ctt3, 5},
    {"_TAM_tam_rcpp_fit_simul", (DL_FUNC) &_TAM_tam_rcpp_fit_simul, 9},
    {"_TAM_tam_rcpp_theta_sq", (DL_FUNC) &_TAM_tam_rcpp_theta_sq, 1},
    {"_TAM_tam_rcpp_interval_index", (DL_FUNC) &_TAM_tam_rcpp_interval_index, 2},
    {"_TAM_tam_rcpp_irt_likelihood_cfa", (DL_FUNC) &_TAM_tam_rcpp_irt_likelihood_cfa, 5},
    {"_TAM_tam_rcpp_mml_2pl_mstep_item_slopes_suffstat", (DL_FUNC) &_TAM_tam_rcpp_mml_2pl_mstep_item_slopes_suffstat, 16},
    {"_TAM_tam_rcpp_mml_3pl_calc_Fdes", (DL_FUNC) &_TAM_tam_rcpp_mml_3pl_calc_Fdes, 2},
    {"_TAM_tam_rcpp_mml_3pl_slca_deriv", (DL_FUNC) &_TAM_tam_rcpp_mml_3pl_slca_deriv, 8},
    {"_TAM_tam_rcpp_mml_3pl_calcexp", (DL_FUNC) &_TAM_tam_rcpp_mml_3pl_calcexp, 12},
    {"_TAM_tam_rcpp_mml_3pl_compute_B", (DL_FUNC) &_TAM_tam_rcpp_mml_3pl_compute_B, 3},
    {"_TAM_tam_rcpp_mml_3pl_nonzero_entries", (DL_FUNC) &_TAM_tam_rcpp_mml_3pl_nonzero_entries, 2},
    {"_TAM_tam_rcpp_mml_mfr_gresp_extend", (DL_FUNC) &_TAM_tam_rcpp_mml_mfr_gresp_extend, 2},
    {"_TAM_tam_rcpp_mml_mfr_gresp_na_facets", (DL_FUNC) &_TAM_tam_rcpp_mml_mfr_gresp_na_facets, 3},
    {"_TAM_tam_rcpp_mml_mfr_a_matrix_cumsum", (DL_FUNC) &_TAM_tam_rcpp_mml_mfr_a_matrix_cumsum, 3},
    {"_TAM_tam_rcpp_mml_mfr_colsums_gresp", (DL_FUNC) &_TAM_tam_rcpp_mml_mfr_colsums_gresp, 1},
    {"_TAM_tam_rcpp_mml_maxcat", (DL_FUNC) &_TAM_tam_rcpp_mml_maxcat, 2},
    {"_TAM_tam_rcpp_modelfit_q3", (DL_FUNC) &_TAM_tam_rcpp_modelfit_q3, 2},
    {"_TAM_tam_rcpp_modelfit_counts", (DL_FUNC) &_TAM_tam_rcpp_modelfit_counts, 6},
    {"_TAM_tam_rcpp_modelfit_ccov", (DL_FUNC) &_TAM_tam_rcpp_modelfit_ccov, 3},
    {"_TAM_tam_rcpp_modelfit_residuals", (DL_FUNC) &_TAM_tam_rcpp_modelfit_residuals, 8},
    {"_TAM_tam_rcpp_msq_itemfit", (DL_FUNC) &_TAM_tam_rcpp_msq_itemfit, 8},
    {"_TAM_tam_rcpp_prior_normal_density_unequal_means", (DL_FUNC) &_TAM_tam_rcpp_prior_normal_density_unequal_means, 4},
    {"_TAM_tam_rcpp_prior_normal_density_equal_means", (DL_FUNC) &_TAM_tam_rcpp_prior_normal_density_equal_means, 4},
    {"_TAM_tam_rcpp_pv_mcmc_likelihood", (DL_FUNC) &_TAM_tam_rcpp_pv_mcmc_likelihood, 6},
    {"_TAM_tam_rcpp_pv_mcmc_calc_probs_irf_3pl", (DL_FUNC) &_TAM_tam_rcpp_pv_mcmc_calc_probs_irf_3pl, 6},
    {"_TAM_tam_rcpp_pv_sample_theta_multidim", (DL_FUNC) &_TAM_tam_rcpp_pv_sample_theta_multidim, 2},
    {"_TAM_tam_rcpp_rowCumsums", (DL_FUNC) &_TAM_tam_rcpp_rowCumsums, 1},
    {"_TAM_tam_rcpp_tam_np_posterior", (DL_FUNC) &_TAM_tam_rcpp_tam_np_posterior, 6},
    {"_TAM_tam_rcpp_wle_suffstat", (DL_FUNC) &_TAM_tam_rcpp_wle_suffstat, 9},
    {"_TAM_tam_rcpp_wle_errinv", (DL_FUNC) &_TAM_tam_rcpp_wle_errinv, 3},
    {NULL, NULL, 0}
};

void R_init_TAM(DllInfo *dll) {
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
