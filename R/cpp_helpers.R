#' @useDynLib DIDmultiplegtDYNpolars, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

#' Propagate treatment change flag within groups (C++ optimized)
#'
#' Replaces the R loop that propagates ever_change_d_XX forward within groups
#' @param df data.frame with ever_change_d_XX, group_XX, time_XX columns
#' @param T_XX Maximum time value
#' @return Modified data.frame with propagated ever_change_d_XX
#' @noRd
propagate_ever_change_cpp_wrapper <- function(df, T_XX) {
  df <- as.data.frame(df)
  df <- df[order(df$group_XX, df$time_XX), ]
  df$ever_change_d_XX <- propagate_treatment_change_cpp(
    as.numeric(df$ever_change_d_XX),
    as.integer(df$group_XX),
    as.integer(df$time_XX),
    as.integer(T_XX)
  )
  return(df)
}

#' Compute variance-covariance matrix for effects (C++ optimized)
#'
#' Replaces nested R loops for variance-covariance computation
#' @param df data.frame containing U_Gg_var columns
#' @param l_XX Number of effects
#' @param G_XX Number of groups
#' @param normalized Whether normalized estimates are used
#' @param delta_D_global Vector of delta_D values for normalization
#' @return Variance-covariance matrix
#' @noRd
compute_vcov_effects_cpp_wrapper <- function(df, l_XX, G_XX, normalized = FALSE, delta_D_global = NULL) {
  cols <- paste0("U_Gg_var_glob_", 1:l_XX, "_XX")
  df <- as.data.frame(df)
  U_Gg_mat <- as.matrix(df[, cols, drop = FALSE])
  first_obs <- as.integer(df$first_obs_by_gp_XX)

  vcov <- compute_var_covar_matrix_cpp(U_Gg_mat, first_obs, l_XX, G_XX)

  if (normalized && !is.null(delta_D_global)) {
    for (i in 1:l_XX) {
      vcov[i, ] <- vcov[i, ] / delta_D_global[i]
      vcov[, i] <- vcov[, i] / delta_D_global[i]
    }
  }

  return(vcov)
}

#' Compute clustered variance (C++ optimized)
#'
#' @param U_Gg_var Vector of U_Gg_var values
#' @param first_obs_gp first_obs_by_gp_XX indicator
#' @param first_obs_clust first_obs_by_clust_XX indicator
#' @param cluster Cluster variable
#' @param G_XX Number of groups
#' @return List with cluster sums and variance
#' @noRd
compute_clustered_var_wrapper <- function(U_Gg_var, first_obs_gp, first_obs_clust, cluster, G_XX) {
  result <- compute_clustered_variance_cpp(
    as.numeric(U_Gg_var),
    as.integer(first_obs_gp),
    as.integer(first_obs_clust),
    as.integer(cluster),
    as.numeric(G_XX)
  )
  return(result)
}

#' Compute weighted U_Gg global values (C++ optimized)
#'
#' @param U_Gg_plus U_Gg values for switchers in
#' @param U_Gg_minus U_Gg values for switchers out
#' @param N1 Weight for switchers in
#' @param N0 Weight for switchers out
#' @return Weighted combination vector
#' @noRd
compute_U_Gg_global_wrapper <- function(U_Gg_plus, U_Gg_minus, N1, N0) {
  compute_U_Gg_global_cpp(
    as.numeric(U_Gg_plus),
    as.numeric(U_Gg_minus),
    as.numeric(N1),
    as.numeric(N0)
  )
}

# ============================================================================
# HOT LOOP OPTIMIZATIONS - New C++ wrappers for core function
# ============================================================================

#' Compute lagged difference within groups using C++
#' @param x numeric vector
#' @param group integer vector of group IDs
#' @param lag_periods number of periods to lag
#' @return numeric vector of x - lag(x, lag_periods) by group
#' @noRd
cpp_lag_diff <- function(x, group, lag_periods) {
  lag_diff_by_group_cpp(as.numeric(x), as.integer(group), as.integer(lag_periods))
}

#' Compute shift within groups using C++
#' @param x numeric vector
#' @param group integer vector of group IDs
#' @param periods number of periods to shift (positive = lag, negative = lead)
#' @return numeric vector
#' @noRd
cpp_shift <- function(x, group, periods) {
  shift_by_group_cpp(as.numeric(x), as.integer(group), as.integer(periods))
}

#' Compute conditional sum by groups using C++
#' @param x numeric vector to sum
#' @param condition integer vector (1 = include, 0 = exclude)
#' @param group1 first grouping variable
#' @param group2 second grouping variable
#' @param group3 optional third grouping variable
#' @return numeric vector with group sums
#' @noRd
cpp_conditional_sum <- function(x, condition, group1, group2, group3 = NULL) {
  conditional_sum_by_group_cpp(
    as.numeric(x),
    as.integer(condition),
    as.integer(group1),
    as.integer(group2),
    if (!is.null(group3)) as.integer(group3) else NULL
  )
}

#' Compute sum by single group using C++
#' @param x numeric vector to sum
#' @param group grouping variable
#' @return numeric vector with group sums
#' @noRd
cpp_sum_by_group <- function(x, group) {
  sum_by_group_cpp(as.numeric(x), as.integer(group))
}

#' Compute mean by single group using C++
#' @param x numeric vector
#' @param group grouping variable
#' @return numeric vector with group means
#' @noRd
cpp_mean_by_group <- function(x, group) {
  mean_by_group_cpp(as.numeric(x), as.integer(group))
}

#' Compute U_Gg core values using C++
#' @param df data.table with required columns
#' @param i effect number
#' @param G_XX number of groups
#' @param N_inc N_increase value
#' @param t_min minimum time
#' @param T_max maximum time
#' @param increase_XX 1 for switchers in, 0 for switchers out
#' @return list with U_Gg_temp, U_Gg, count_core
#' @noRd
cpp_compute_U_Gg <- function(df, i, G_XX, N_inc, t_min, T_max, increase_XX = 1) {
  diff_y_col <- paste0("diff_y_", i, "_XX")
  dist_col <- paste0("distance_to_switch_", i, "_XX")
  N_t_g_col <- paste0("N", increase_XX, "_t_", i, "_g_XX")
  N_gt_ctrl_col <- paste0("N_gt_control_", i, "_XX")
  never_col <- paste0("never_change_d_", i, "_XX")

  compute_U_Gg_core_cpp(
    diff_y = as.numeric(df[[diff_y_col]]),
    distance_to_switch = as.numeric(df[[dist_col]]),
    N_t_g = as.numeric(df[[N_t_g_col]]),
    N_gt_control = as.numeric(df[[N_gt_ctrl_col]]),
    never_change = as.numeric(df[[never_col]]),
    N_gt = as.numeric(df$N_gt_XX),
    time_XX = as.integer(df$time_XX),
    T_g = as.numeric(df$T_g_XX),
    group = as.integer(df$group_XX),
    first_obs = as.integer(df$first_obs_by_gp_XX),
    G_XX = G_XX,
    N_inc = N_inc,
    i = as.integer(i),
    t_min = as.integer(t_min),
    T_max = as.integer(T_max)
  )
}

#' Compute U_Gg variance temp using C++
#' @param df data.table with required columns
#' @param i effect number
#' @param G_XX number of groups
#' @param N_inc N_increase value
#' @param increase_XX 1 for switchers in, 0 for switchers out
#' @return numeric vector of U_Gg_var_temp
#' @noRd
cpp_compute_U_Gg_var_temp <- function(df, i, G_XX, N_inc, increase_XX = 1) {
  diff_y_col <- paste0("diff_y_", i, "_XX")
  E_hat_col <- paste0("E_hat_gt_", i, "_XX")
  DOF_col <- paste0("DOF_gt_", i, "_XX")
  dist_col <- paste0("distance_to_switch_", i, "_XX")
  N_t_g_col <- paste0("N", increase_XX, "_t_", i, "_g_XX")
  N_gt_ctrl_col <- paste0("N_gt_control_", i, "_XX")
  never_col <- paste0("never_change_d_", i, "_XX")

  compute_U_Gg_var_temp_cpp(
    diff_y = as.numeric(df[[diff_y_col]]),
    E_hat_gt = as.numeric(df[[E_hat_col]]),
    DOF_gt = as.numeric(df[[DOF_col]]),
    distance_to_switch = as.numeric(df[[dist_col]]),
    N_t_g = as.numeric(df[[N_t_g_col]]),
    N_gt_control = as.numeric(df[[N_gt_ctrl_col]]),
    never_change = as.numeric(df[[never_col]]),
    N_gt = as.numeric(df$N_gt_XX),
    time_XX = as.integer(df$time_XX),
    T_g = as.numeric(df$T_g_XX),
    G_XX = G_XX,
    N_inc = N_inc,
    i = as.integer(i)
  )
}

#' Compute same_switchers loop using C++
#' @param df data.table with required columns
#' @param effects number of effects
#' @param T_max maximum time
#' @param only_never_switchers logical
#' @return list with N_g_control_check
#' @noRd
cpp_same_switchers_loop <- function(df, effects, T_max, only_never_switchers) {
  same_switchers_loop_cpp(
    outcome = as.numeric(df$outcome_XX),
    group = as.integer(df$group_XX),
    time = as.integer(df$time_XX),
    F_g = as.numeric(df$F_g_XX),
    N_gt = as.numeric(df$N_gt_XX),
    d_sq = as.integer(df$d_sq_int_XX),
    effects = as.integer(effects),
    T_max = as.integer(T_max),
    only_never_switchers = as.logical(only_never_switchers)
  )
}
