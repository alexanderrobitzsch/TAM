#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP calcfx(SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_a_matrix_cumsum(SEXP, SEXP, SEXP);
extern SEXP TAM_calc_prob_subtract_max(SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_colsums_gresp(SEXP);
extern SEXP TAM_gresp_extend(SEXP, SEXP);
extern SEXP TAM_gresp_na_facets(SEXP, SEXP, SEXP);
extern SEXP TAM_interval_index_C(SEXP, SEXP);
extern SEXP TAM_irt_likelihood_cfa2(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_msq_itemfit(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_msq_itemfit2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_prior_normal_density_C(SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_prior_normal_densityALL_C(SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_redefine_vector_na(SEXP, SEXP);
extern SEXP TAM_rowCumsums2_source(SEXP);
extern SEXP TAM_tam_calccov(SEXP, SEXP, SEXP);
extern SEXP TAM_TAM_CALCEXP(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_TAM_CALCEXP2(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_ctt_C(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_fit_simul(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_mml_3pl_calc_Fdes(SEXP, SEXP);
extern SEXP TAM_tam_mml_3pl_calcexp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_mml_3pl_compute_B_rcpp(SEXP, SEXP, SEXP);
extern SEXP TAM_tam_mml_3pl_nonzero_entries(SEXP, SEXP);
extern SEXP TAM_tam_mml_3pl_slca_deriv(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_mvrnorm_rcpp(SEXP, SEXP, SEXP);
extern SEXP TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_pv_mcmc_likelihood_Rcpp(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_pv_sample_theta_multidim(SEXP, SEXP);
extern SEXP TAM_tam_pv_weighted_cov(SEXP, SEXP);
extern SEXP TAM_tam_pv_weighted_mean(SEXP, SEXP);
extern SEXP TAM_tam_q3_calc_residM(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_q3_calc_V2counts(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_q3_calc_V2q3jack(SEXP, SEXP);
extern SEXP TAM_tam_wle_Bs(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_tam_wle_errinv(SEXP, SEXP, SEXP);
extern SEXP TAM_tamctt3csource(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP TAM_theta_sq_cpp(SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"calcfx",                                  (DL_FUNC) &calcfx,                                   4},
    {"TAM_a_matrix_cumsum",                     (DL_FUNC) &TAM_a_matrix_cumsum,                      3},
    {"TAM_calc_prob_subtract_max",              (DL_FUNC) &TAM_calc_prob_subtract_max,               4},
    {"TAM_colsums_gresp",                       (DL_FUNC) &TAM_colsums_gresp,                        1},
    {"TAM_gresp_extend",                        (DL_FUNC) &TAM_gresp_extend,                         2},
    {"TAM_gresp_na_facets",                     (DL_FUNC) &TAM_gresp_na_facets,                      3},
    {"TAM_interval_index_C",                    (DL_FUNC) &TAM_interval_index_C,                     2},
    {"TAM_irt_likelihood_cfa2",                 (DL_FUNC) &TAM_irt_likelihood_cfa2,                  5},
    {"TAM_msq_itemfit",                         (DL_FUNC) &TAM_msq_itemfit,                          7},
    {"TAM_msq_itemfit2",                        (DL_FUNC) &TAM_msq_itemfit2,                         7},
    {"TAM_prior_normal_density_C",              (DL_FUNC) &TAM_prior_normal_density_C,               4},
    {"TAM_prior_normal_densityALL_C",           (DL_FUNC) &TAM_prior_normal_densityALL_C,            4},
    {"TAM_redefine_vector_na",                  (DL_FUNC) &TAM_redefine_vector_na,                   2},
    {"TAM_rowCumsums2_source",                  (DL_FUNC) &TAM_rowCumsums2_source,                   1},
    {"TAM_tam_calccov",                         (DL_FUNC) &TAM_tam_calccov,                          3},
    {"TAM_TAM_CALCEXP",                         (DL_FUNC) &TAM_TAM_CALCEXP,                          8},
    {"TAM_TAM_CALCEXP2",                        (DL_FUNC) &TAM_TAM_CALCEXP2,                        10},
    {"TAM_tam_ctt_C",                           (DL_FUNC) &TAM_tam_ctt_C,                            5},
    {"TAM_tam_fit_simul",                       (DL_FUNC) &TAM_tam_fit_simul,                        9},
    {"TAM_tam_mml_3pl_calc_Fdes",               (DL_FUNC) &TAM_tam_mml_3pl_calc_Fdes,                2},
    {"TAM_tam_mml_3pl_calcexp",                 (DL_FUNC) &TAM_tam_mml_3pl_calcexp,                 12},
    {"TAM_tam_mml_3pl_compute_B_rcpp",          (DL_FUNC) &TAM_tam_mml_3pl_compute_B_rcpp,           3},
    {"TAM_tam_mml_3pl_nonzero_entries",         (DL_FUNC) &TAM_tam_mml_3pl_nonzero_entries,          2},
    {"TAM_tam_mml_3pl_slca_deriv",              (DL_FUNC) &TAM_tam_mml_3pl_slca_deriv,               8},
    {"TAM_tam_mvrnorm_rcpp",                    (DL_FUNC) &TAM_tam_mvrnorm_rcpp,                     3},
    {"TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp", (DL_FUNC) &TAM_tam_pv_mcmc_calc_probs_irf_3pl_rcpp,  6},
    {"TAM_tam_pv_mcmc_likelihood_Rcpp",         (DL_FUNC) &TAM_tam_pv_mcmc_likelihood_Rcpp,          6},
    {"TAM_tam_pv_sample_theta_multidim",        (DL_FUNC) &TAM_tam_pv_sample_theta_multidim,         2},
    {"TAM_tam_pv_weighted_cov",                 (DL_FUNC) &TAM_tam_pv_weighted_cov,                  2},
    {"TAM_tam_pv_weighted_mean",                (DL_FUNC) &TAM_tam_pv_weighted_mean,                 2},
    {"TAM_tam_q3_calc_residM",                  (DL_FUNC) &TAM_tam_q3_calc_residM,                   7},
    {"TAM_tam_q3_calc_V2counts",                (DL_FUNC) &TAM_tam_q3_calc_V2counts,                 6},
    {"TAM_tam_q3_calc_V2q3jack",                (DL_FUNC) &TAM_tam_q3_calc_V2q3jack,                 2},
    {"TAM_tam_wle_Bs",                          (DL_FUNC) &TAM_tam_wle_Bs,                           9},
    {"TAM_tam_wle_errinv",                      (DL_FUNC) &TAM_tam_wle_errinv,                       3},
    {"TAM_tamctt3csource",                      (DL_FUNC) &TAM_tamctt3csource,                       5},
    {"TAM_theta_sq_cpp",                        (DL_FUNC) &TAM_theta_sq_cpp,                         1},
    {NULL, NULL, 0}
};

void R_init_TAM(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
