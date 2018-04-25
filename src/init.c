//// File Name: init.c
//// File Version: 0.8005
#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP _TAM_a_matrix_cumsum(SEXP, SEXP, SEXP);
extern SEXP _TAM_calcfx(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_colsums_gresp(SEXP);
extern SEXP _TAM_gresp_extend(SEXP, SEXP);
extern SEXP _TAM_gresp_na_facets(SEXP, SEXP, SEXP);
extern SEXP _TAM_msq_itemfit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_msq_itemfit2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_calccov(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_mml_3pl_calc_Fdes(SEXP, SEXP);
extern SEXP _TAM_tam_mml_3pl_calcexp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_mml_3pl_compute_B_rcpp(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_mml_3pl_nonzero_entries(SEXP, SEXP);
extern SEXP _TAM_tam_mml_3pl_slca_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_mvrnorm_rcpp(SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_pv_mcmc_likelihood_Rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_pv_sample_theta_multidim(SEXP, SEXP);
extern SEXP _TAM_tam_pv_weighted_cov(SEXP, SEXP);
extern SEXP _TAM_tam_pv_weighted_mean(SEXP, SEXP);
extern SEXP _TAM_tam_q3_calc_residM(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_q3_calc_V2counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_q3_calc_V2q3jack(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_exp_redefine_vector_na(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calc_prob_subtract_max(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calcexp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_calcexp2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_ctt2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_ctt3(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_fit_simul(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_interval_index(SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_irt_likelihood_cfa(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_prior_normal_density_equal_means(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_prior_normal_density_unequal_means(SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_rowCumsums(SEXP);
extern SEXP _TAM_tam_rcpp_theta_sq(SEXP);
extern SEXP _TAM_tam_rcpp_wle_Bs(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP _TAM_tam_rcpp_wle_errinv(SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"_TAM_a_matrix_cumsum", (DL_FUNC) &_TAM_a_matrix_cumsum, 3},
    {"_TAM_calcfx", (DL_FUNC) &_TAM_calcfx, 4},
    {"_TAM_colsums_gresp", (DL_FUNC) &_TAM_colsums_gresp, 1},
    {"_TAM_gresp_extend", (DL_FUNC) &_TAM_gresp_extend, 2},
    {"_TAM_gresp_na_facets", (DL_FUNC) &_TAM_gresp_na_facets, 3},
    {"_TAM_msq_itemfit", (DL_FUNC) &_TAM_msq_itemfit, 7},
    {"_TAM_msq_itemfit2", (DL_FUNC) &_TAM_msq_itemfit2, 7},
    {"_TAM_tam_calccov", (DL_FUNC) &_TAM_tam_calccov, 3},
    {"_TAM_tam_mml_3pl_calc_Fdes", (DL_FUNC) &_TAM_tam_mml_3pl_calc_Fdes, 2},
    {"_TAM_tam_mml_3pl_calcexp", (DL_FUNC) &_TAM_tam_mml_3pl_calcexp, 12},
    {"_TAM_tam_mml_3pl_compute_B_rcpp", (DL_FUNC) &_TAM_tam_mml_3pl_compute_B_rcpp, 3},
    {"_TAM_tam_mml_3pl_nonzero_entries", (DL_FUNC) &_TAM_tam_mml_3pl_nonzero_entries, 2},
    {"_TAM_tam_mml_3pl_slca_deriv", (DL_FUNC) &_TAM_tam_mml_3pl_slca_deriv, 8},
    {"_TAM_tam_mvrnorm_rcpp", (DL_FUNC) &_TAM_tam_mvrnorm_rcpp, 3},
    {"_TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp", (DL_FUNC) &_TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp, 6},
    {"_TAM_tam_pv_mcmc_likelihood_Rcpp", (DL_FUNC) &_TAM_tam_pv_mcmc_likelihood_Rcpp, 6},
    {"_TAM_tam_pv_sample_theta_multidim", (DL_FUNC) &_TAM_tam_pv_sample_theta_multidim, 2},
    {"_TAM_tam_pv_weighted_cov", (DL_FUNC) &_TAM_tam_pv_weighted_cov, 2},
    {"_TAM_tam_pv_weighted_mean", (DL_FUNC) &_TAM_tam_pv_weighted_mean, 2},
    {"_TAM_tam_q3_calc_residM", (DL_FUNC) &_TAM_tam_q3_calc_residM, 7},
    {"_TAM_tam_q3_calc_V2counts", (DL_FUNC) &_TAM_tam_q3_calc_V2counts, 6},
    {"_TAM_tam_q3_calc_V2q3jack", (DL_FUNC) &_TAM_tam_q3_calc_V2q3jack, 2},
    {"_TAM_tam_rcpp_calc_exp_redefine_vector_na", (DL_FUNC) &_TAM_tam_rcpp_calc_exp_redefine_vector_na, 2},
    {"_TAM_tam_rcpp_calc_prob_subtract_max", (DL_FUNC) &_TAM_tam_rcpp_calc_prob_subtract_max, 4},
    {"_TAM_tam_rcpp_calcexp", (DL_FUNC) &_TAM_tam_rcpp_calcexp, 8},
    {"_TAM_tam_rcpp_calcexp2", (DL_FUNC) &_TAM_tam_rcpp_calcexp2, 10},
    {"_TAM_tam_rcpp_ctt2", (DL_FUNC) &_TAM_tam_rcpp_ctt2, 5},
    {"_TAM_tam_rcpp_ctt3", (DL_FUNC) &_TAM_tam_rcpp_ctt3, 5},
    {"_TAM_tam_rcpp_fit_simul", (DL_FUNC) &_TAM_tam_rcpp_fit_simul, 9},
    {"_TAM_tam_rcpp_interval_index", (DL_FUNC) &_TAM_tam_rcpp_interval_index, 2},
    {"_TAM_tam_rcpp_irt_likelihood_cfa", (DL_FUNC) &_TAM_tam_rcpp_irt_likelihood_cfa, 5},
    {"_TAM_tam_rcpp_prior_normal_density_equal_means", (DL_FUNC) &_TAM_tam_rcpp_prior_normal_density_equal_means, 4},
    {"_TAM_tam_rcpp_prior_normal_density_unequal_means", (DL_FUNC) &_TAM_tam_rcpp_prior_normal_density_unequal_means, 4},
    {"_TAM_tam_rcpp_rowCumsums", (DL_FUNC) &_TAM_tam_rcpp_rowCumsums, 1},
    {"_TAM_tam_rcpp_theta_sq", (DL_FUNC) &_TAM_tam_rcpp_theta_sq, 1},
    {"_TAM_tam_rcpp_wle_Bs", (DL_FUNC) &_TAM_tam_rcpp_wle_Bs, 9},
    {"_TAM_tam_rcpp_wle_errinv", (DL_FUNC) &_TAM_tam_rcpp_wle_errinv, 3},
    {NULL, NULL, 0}
};

void R_init_TAM(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
