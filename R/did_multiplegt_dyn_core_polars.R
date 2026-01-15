#' Internal function of did_multiplegt_dyn that computes U_Gg_plus_XX, U_Gg_minus_XX,
#' U_Gg_var_plus_XX, and U_Gg_var_minus_XX using pure Polars operations.
#' @param df polars DataFrame
#' @param outcome outcome
#' @param group group
#' @param time time
#' @param treatment treatment
#' @param effects effects
#' @param placebo placebo
#' @param switchers_core switchers_core
#' @param trends_nonparam trends_nonparam
#' @param controls controls
#' @param same_switchers same_switchers
#' @param same_switchers_pl same_switchers_pl
#' @param only_never_switchers only_never_switchers
#' @param normalized normalized
#' @param globals globals
#' @param const constants
#' @param trends_lin trends_lin
#' @param controls_globals controls_globals
#' @param less_conservative_se less_conservative_se
#' @param continuous continuous
#' @import polars
#' @importFrom stats na.omit predict setNames
#' @importFrom MASS ginv
#' @importFrom fixest feols
#' @importFrom dplyr n_distinct
#' @returns A list containing the polars DataFrame and constants.
#' @noRd
did_multiplegt_dyn_core_polars <- function(
    df,
    outcome,
    group,
    time,
    treatment,
    effects,
    placebo,
    cluster,
    switchers_core = NULL,
    trends_nonparam,
    controls,
    same_switchers,
    same_switchers_pl,
    only_never_switchers,
    normalized,
    globals,
    const,
    trends_lin,
    controls_globals,
    less_conservative_se,
    continuous
) {

  # Inherited Globals
  L_u_XX <- globals$L_u_XX
  L_placebo_u_XX <- globals$L_placebo_u_XX
  L_placebo_a_XX <- globals$L_placebo_a_XX
  L_a_XX <- globals$L_a_XX
  t_min_XX <- globals$t_min_XX
  T_max_XX <- globals$T_max_XX
  G_XX <- globals$G_XX

  for (e in names(const)) {
    assign(e, const[[e]])
  }

  if (!is.null(controls)) {
    for (e in names(controls_globals)) {
      assign(e, controls_globals[[e]])
    }
  }

  # Ensure df is a polars DataFrame
  if (!inherits(df, "RPolarsDataFrame")) {
    df <- as_polars_df(as.data.frame(df))
  }

  suppressWarnings({

    ####### 1. Scalars initialization

    ## Initializing the number of effects and placebos to estimate
    if (switchers_core == "in") {
      l_u_a_XX <- min(L_u_XX, effects, na.rm = TRUE)
      if (placebo != 0) {
        l_placebo_u_a_XX <- min(placebo, L_placebo_u_XX)
      }
      increase_XX <- 1
    }

    if (switchers_core == "out") {
      l_u_a_XX <- min(L_a_XX, effects, na.rm = TRUE)
      if (placebo != 0) {
        l_placebo_u_a_XX <- min(placebo, L_placebo_a_XX)
      }
      increase_XX <- 0
    }

    ## Initializing values of baseline treatment
    levels_d_sq_XX <- pl_unique(df, "d_sq_int_XX")
    levels_d_sq_XX <- levels_d_sq_XX[!is.na(levels_d_sq_XX)]
    levels_d_sq_XX <- sort(unique(as.character(levels_d_sq_XX)))

    # Drop columns if they exist
    df <- pl_safe_drop(df, c("num_g_paths_0_XX", "cohort_fullpath_0_XX"))

    # Sort by group and time
    df <- df$sort(c("group_XX", "time_XX"))

    ## 2. Data preparation steps to generate variables necessary for computation of event-study effects
    ## Loop over the number of dynamic effects

    for (i in 1:l_u_a_XX) {

      # Column names for this iteration
      dist_to_switch_col <- paste0("distance_to_switch_", i, "_XX")
      never_change_col <- paste0("never_change_d_", i, "_XX")
      N_inc_t_col <- paste0("N", increase_XX, "_t_", i, "_XX")
      N_inc_t_g_col <- paste0("N", increase_XX, "_t_", i, "_g_XX")
      N_gt_control_col <- paste0("N_gt_control_", i, "_XX")
      diff_y_col <- paste0("diff_y_", i, "_XX")
      dummy_U_col <- paste0("dummy_U_Gg", i, "_XX")
      U_temp_col <- paste0("U_Gg", i, "_temp_XX")
      U_col <- paste0("U_Gg", i, "_XX")
      U_temp_var_col <- paste0("U_Gg", i, "_temp_var_XX")
      U_var_col <- paste0("U_Gg", i, "_var_XX")

      # Drop existing columns if they exist
      cols_to_drop <- c(
        dist_to_switch_col, never_change_col, N_inc_t_col, N_inc_t_g_col,
        N_gt_control_col, diff_y_col, paste0(diff_y_col, "_temp"), dummy_U_col,
        U_temp_col, U_col, paste0("count", i, "_core_XX"),
        paste0("mean_diff_y_", i, "_nd_sq_t_XX"), paste0("mean_diff_y_", i, "_d_sq_t_XX"),
        U_temp_var_col, U_var_col, paste0("U_Gg", i, "_var_2_XX"),
        paste0("count_var_", i, "_ntreat_XX_temp"), paste0("count_var_", i, "_ntreat_XX"),
        paste0("count_var_", i, "_treat_XX_temp"), paste0("count_var_", i, "_treat_XX"),
        paste0("avg_diff_y_", i, "_tnp_XX"), paste0("count_diff_y_", i, "_nd_sq_t_XX"),
        paste0("count_diff_y_", i, "_d_sq_t_XX"), paste0("never_change_d_", i, "_wXX"),
        paste0("distance_to_switch_", i, "_wXX"),
        paste0("dof_cohort_", i, "_ns_t_XX"), paste0("dof_cohort_", i, "_s_t_XX"),
        paste0("dof_cohort_", i, "_s0_t_XX"), paste0("dof_cohort_", i, "_s1_t_XX"),
        paste0("dof_cohort_", i, "_s2_t_XX"),
        paste0("count_cohort_", i, "_ns_t_XX"), paste0("count_cohort_", i, "_s_t_XX"),
        paste0("count_cohort_", i, "_s0_t_XX"), paste0("count_cohort_", i, "_s1_t_XX"),
        paste0("count_cohort_", i, "_s2_t_XX"),
        paste0("total_cohort_", i, "_ns_t_XX"), paste0("total_cohort_", i, "_s_t_XX"),
        paste0("total_cohort_", i, "_s0_t_XX"), paste0("total_cohort_", i, "_s1_t_XX"),
        paste0("total_cohort_", i, "_s2_t_XX"),
        paste0("mean_cohort_", i, "_ns_t_XX"), paste0("mean_cohort_", i, "_s_t_XX"),
        paste0("mean_cohort_", i, "_s0_t_XX"), paste0("mean_cohort_", i, "_s1_t_XX"),
        paste0("mean_cohort_", i, "_s2_t_XX")
      )
      df <- pl_safe_drop(df, cols_to_drop)

      ## Creating long difference of outcome: outcome_XX - shift(outcome_XX, i)
      df <- df$sort(c("group_XX", "time_XX"))
      df <- df$with_columns(
        (pl$col("outcome_XX") - pl$col("outcome_XX")$shift(i)$over("group_XX"))$alias(diff_y_col)
      )

      ## Creating treatment paths if less_conservative_se option specified
      if (isTRUE(less_conservative_se)) {

        # Create d_fg_XX_temp: treatment at F_g + i - 1
        df <- df$with_columns(
          pl$when(pl$col("time_XX") == pl$col("F_g_XX") + i - 1)
            $then(pl$col("treatment_XX"))
            $otherwise(pl$lit(NA_real_))
            $alias("d_fg_XX_temp")
        )

        # Create d_fg{i}_XX as group mean of d_fg_XX_temp
        d_fg_col <- paste0("d_fg", i, "_XX")
        df <- pl_agg_merge(df, "group_XX", "d_fg_XX_temp", d_fg_col, "mean")

        # Initialize d_fg0_XX and path_0_XX when i == 1
        if (i == 1) {
          df <- df$with_columns(pl$col("d_sq_XX")$alias("d_fg0_XX"))
          df <- pl_group_id(df, c("d_fg0_XX", "F_g_XX"), "path_0_XX")
        }

        # Fill missing d_fg with previous value
        d_fg_prev_col <- paste0("d_fg", i - 1, "_XX")
        df <- df$with_columns(
          pl$when(pl$col(d_fg_col)$is_null())
            $then(pl$col(d_fg_prev_col))
            $otherwise(pl$col(d_fg_col))
            $alias(d_fg_col)
        )

        # Create path_i_XX
        path_col <- paste0("path_", i, "_XX")
        path_prev_col <- paste0("path_", i - 1, "_XX")
        df <- pl_group_id(df, c(path_prev_col, d_fg_col), path_col)

        df <- pl_safe_drop(df, "d_fg_XX_temp")

        # Count groups in each path
        if (i == 1) {
          df <- pl_n_unique_over(df, "group_XX", "path_0_XX", "num_g_paths_0_XX")
          df <- df$with_columns(
            pl$when(pl$col("num_g_paths_0_XX") > 1)
              $then(pl$lit(1))
              $otherwise(pl$lit(0))
              $cast(pl$Float64)
              $alias("cohort_fullpath_0_XX")
          )
        }

        num_g_paths_col <- paste0("num_g_paths_", i, "_XX")
        cohort_fullpath_col <- paste0("cohort_fullpath_", i, "_XX")
        df <- pl_n_unique_over(df, "group_XX", path_col, num_g_paths_col)
        df <- df$with_columns(
          pl$when(pl$col(num_g_paths_col) > 1)
            $then(pl$lit(1))
            $otherwise(pl$lit(0))
            $cast(pl$Float64)
            $alias(cohort_fullpath_col)
        )
      }

      ## Identifying the control (g,t)s in the estimation of dynamic effect i
      # never_change_d_i_XX = 1 if F_g > time
      df <- df$with_columns(
        pl$when(pl$col("F_g_XX") > pl$col("time_XX"))
          $then(pl$lit(1))
          $otherwise(pl$lit(0))
          $cast(pl$Float64)
          $alias(never_change_col)
      )

      # Set to NA if diff_y is NA
      df <- df$with_columns(
        pl$when(pl$col(diff_y_col)$is_null())
          $then(pl$lit(NA_real_))
          $otherwise(pl$col(never_change_col))
          $alias(never_change_col)
      )

      # Handle only_never_switchers option
      if (isTRUE(only_never_switchers)) {
        df <- df$with_columns(
          pl$when(
            (pl$col("F_g_XX") > pl$col("time_XX")) &
            (pl$col("F_g_XX") < T_max_XX + 1) &
            pl$col(diff_y_col)$is_not_null()
          )
            $then(pl$lit(0))
            $otherwise(pl$col(never_change_col))
            $alias(never_change_col)
        )
      }

      ## Creating N^g_t: number of control groups for g at t
      never_change_w_col <- paste0("never_change_d_", i, "_wXX")
      df <- df$with_columns(
        (pl$col(never_change_col) * pl$col("N_gt_XX"))$alias(never_change_w_col)
      )

      # Sum by time, d_sq, and trends_nonparam
      by_cols_control <- c("time_XX", "d_sq_XX")
      if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
        by_cols_control <- c(by_cols_control, trends_nonparam)
      }
      df <- pl_agg_merge(df, by_cols_control, never_change_w_col, N_gt_control_col, "sum")

      ## Creating binary variable indicating whether g is l periods away from switch
      if (same_switchers == TRUE) {
        # Pure Polars implementation of same_switchers logic
        df <- df$sort(c("group_XX", "time_XX"))
        df <- df$with_columns(pl$lit(0.0)$alias("N_g_control_check_XX"))

        by_cols_ctrl <- c("time_XX", "d_sq_XX")
        if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
          by_cols_ctrl <- c(by_cols_ctrl, trends_nonparam)
        }

        for (q in 1:effects) {
          # Compute lagged difference of outcome
          df <- df$with_columns(
            (pl$col("outcome_XX") - pl$col("outcome_XX")$shift(q)$over("group_XX"))$alias("diff_y_last_XX")
          )

          # never_change_d_last_XX = 1 if non-missing diff_y and F_g > time
          df <- df$with_columns(
            pl$when(pl$col("diff_y_last_XX")$is_not_null() & (pl$col("F_g_XX") > pl$col("time_XX")))
              $then(pl$lit(1.0))
              $otherwise(pl$lit(NA_real_))
              $alias("never_change_d_last_XX")
          )

          if (isTRUE(only_never_switchers)) {
            df <- df$with_columns(
              pl$when(
                (pl$col("F_g_XX") > pl$col("time_XX")) &
                (pl$col("F_g_XX") < (T_max_XX + 1)) &
                pl$col("diff_y_last_XX")$is_not_null()
              )$then(pl$lit(0.0))$otherwise(pl$col("never_change_d_last_XX"))$alias("never_change_d_last_XX")
            )
          }

          # Sum of (never_change_d_last * N_gt) by control groups
          df <- df$with_columns(
            (pl$col("never_change_d_last_XX") * pl$col("N_gt_XX"))$alias("_ctrl_prod_")
          )
          df <- df$with_columns(
            pl_over_cols(pl$col("_ctrl_prod_")$sum(), by_cols_ctrl)$alias("N_gt_control_last_XX")
          )

          # Mean of N_gt_control_last where time == F_g - 1 + q, by group
          df <- df$with_columns(
            pl$when(pl$col("time_XX") == pl$col("F_g_XX") - 1 + q)
              $then(pl$col("N_gt_control_last_XX"))
              $otherwise(pl$lit(NA_real_))
              $alias("_ctrl_temp_")
          )
          df <- df$with_columns(
            pl$col("_ctrl_temp_")$mean()$over("group_XX")$alias("N_g_control_last_m_XX")
          )

          # Mean of diff_y_last where time == F_g - 1 + q, by group
          df <- df$with_columns(
            pl$when(pl$col("time_XX") == pl$col("F_g_XX") - 1 + q)
              $then(pl$col("diff_y_last_XX"))
              $otherwise(pl$lit(NA_real_))
              $alias("_diff_temp_")
          )
          df <- df$with_columns(
            pl$col("_diff_temp_")$mean()$over("group_XX")$alias("diff_y_relev_XX")
          )

          # Update control check counter
          df <- df$with_columns(
            (pl$col("N_g_control_check_XX") +
              pl$when((pl$col("N_g_control_last_m_XX") > 0) & pl$col("diff_y_relev_XX")$is_not_null())
                $then(pl$lit(1.0))
                $otherwise(pl$lit(0.0)))$alias("N_g_control_check_XX")
          )
        }

        # Clean temp columns
        df <- pl_safe_drop(df, c("_ctrl_prod_", "_ctrl_temp_", "_diff_temp_",
                                  "diff_y_last_XX", "never_change_d_last_XX",
                                  "N_gt_control_last_XX", "N_g_control_last_m_XX", "diff_y_relev_XX"))

        if (same_switchers_pl == TRUE) {
          df <- df$with_columns(pl$lit(0.0)$alias("N_g_control_check_pl_XX"))

          for (q in 1:placebo) {
            # Lead difference for placebos
            df <- df$with_columns(
              (pl$col("outcome_XX") - pl$col("outcome_XX")$shift(-q)$over("group_XX"))$alias("diff_y_last_XX")
            )

            df <- df$with_columns(
              pl$when(pl$col("diff_y_last_XX")$is_not_null() & (pl$col("F_g_XX") > pl$col("time_XX")))
                $then(pl$lit(1.0))
                $otherwise(pl$lit(NA_real_))
                $alias("never_change_d_last_XX")
            )

            if (isTRUE(only_never_switchers)) {
              df <- df$with_columns(
                pl$when(
                  (pl$col("F_g_XX") > pl$col("time_XX")) &
                  (pl$col("F_g_XX") < (T_max_XX + 1)) &
                  pl$col("diff_y_last_XX")$is_not_null()
                )$then(pl$lit(0.0))$otherwise(pl$col("never_change_d_last_XX"))$alias("never_change_d_last_XX")
              )
            }

            df <- df$with_columns(
              (pl$col("never_change_d_last_XX") * pl$col("N_gt_XX"))$alias("_ctrl_prod_")
            )
            df <- df$with_columns(
              pl_over_cols(pl$col("_ctrl_prod_")$sum(), by_cols_ctrl)$alias("N_gt_control_last_XX")
            )

            df <- df$with_columns(
              pl$when(pl$col("time_XX") == pl$col("F_g_XX") - 1 - q)
                $then(pl$col("N_gt_control_last_XX"))
                $otherwise(pl$lit(NA_real_))
                $alias("_ctrl_temp_")
            )
            df <- df$with_columns(
              pl$col("_ctrl_temp_")$mean()$over("group_XX")$alias("N_g_control_last_m_XX")
            )

            df <- df$with_columns(
              pl$when(pl$col("time_XX") == pl$col("F_g_XX") - 1 - q)
                $then(pl$col("diff_y_last_XX"))
                $otherwise(pl$lit(NA_real_))
                $alias("_diff_temp_")
            )
            df <- df$with_columns(
              pl$col("_diff_temp_")$mean()$over("group_XX")$alias("diff_y_relev_XX")
            )

            df <- df$with_columns(
              (pl$col("N_g_control_check_pl_XX") +
                pl$when((pl$col("N_g_control_last_m_XX") > 0) & pl$col("diff_y_relev_XX")$is_not_null())
                  $then(pl$lit(1.0))
                  $otherwise(pl$lit(0.0)))$alias("N_g_control_check_pl_XX")
            )
          }

          # Clean temp columns
          df <- pl_safe_drop(df, c("_ctrl_prod_", "_ctrl_temp_", "_diff_temp_",
                                    "diff_y_last_XX", "never_change_d_last_XX",
                                    "N_gt_control_last_XX", "N_g_control_last_m_XX", "diff_y_relev_XX"))

          # Generate relevant_y_missing_XX
          df <- df$with_columns(
            (pl$col("outcome_XX")$is_null() &
             (pl$col("time_XX") >= pl$col("F_g_XX") - 1 - placebo) &
             (pl$col("time_XX") <= pl$col("F_g_XX") - 1 + effects))$cast(pl$Float64)$alias("relevant_y_missing_XX")
          )

          if (!is.null(controls)) {
            df <- df$with_columns(
              pl$when(
                (pl$col("fd_X_all_non_missing_XX") == 0) &
                (pl$col("time_XX") >= pl$col("F_g_XX") - 1 - placebo) &
                (pl$col("time_XX") <= pl$col("F_g_XX") - 1 + effects)
              )$then(pl$lit(1.0))$otherwise(pl$col("relevant_y_missing_XX"))$alias("relevant_y_missing_XX")
            )
          }

          df <- df$with_columns(
            (pl$col("N_g_control_check_pl_XX") == placebo)$alias("fillin_g_pl_XX")
          )

          # Tag switchers
          still_switcher_col <- paste0("still_switcher_", i, "_XX")
          df <- df$with_columns(
            ((pl$col("F_g_XX") - 1 + effects <= pl$col("T_g_XX")) &
             (pl$col("N_g_control_check_XX") == effects))$alias(still_switcher_col)
          )

          df <- df$with_columns(
            pl$when(pl$col(diff_y_col)$is_not_null())
              $then(
                pl$when(
                  (pl$col(still_switcher_col) == TRUE) &
                  (pl$col("time_XX") == pl$col("F_g_XX") - 1 + i) &
                  (i <= pl$col("L_g_XX")) &
                  (pl$col("S_g_XX") == increase_XX) &
                  (pl$col(N_gt_control_col) > 0) &
                  pl$col(N_gt_control_col)$is_not_null()
                )$then(pl$lit(1.0))$otherwise(pl$lit(0.0))
              )
              $otherwise(pl$lit(NA_real_))
              $alias(dist_to_switch_col)
          )
        } else {
          # same_switchers_pl not specified
          df <- df$with_columns(
            (pl$col("outcome_XX")$is_null() &
             (pl$col("time_XX") >= pl$col("F_g_XX") - 1) &
             (pl$col("time_XX") <= pl$col("F_g_XX") - 1 + effects))$cast(pl$Float64)$alias("relevant_y_missing_XX")
          )

          if (!is.null(controls)) {
            df <- df$with_columns(
              pl$when(
                (pl$col("fd_X_all_non_missing_XX") == 0) &
                (pl$col("time_XX") >= pl$col("F_g_XX")) &
                (pl$col("time_XX") <= pl$col("F_g_XX") - 1 + effects)
              )$then(pl$lit(1.0))$otherwise(pl$col("relevant_y_missing_XX"))$alias("relevant_y_missing_XX")
            )
          }

          still_switcher_col <- paste0("still_switcher_", i, "_XX")
          df <- df$with_columns(
            ((pl$col("F_g_XX") - 1 + effects <= pl$col("T_g_XX")) &
             (pl$col("N_g_control_check_XX") == effects))$alias(still_switcher_col)
          )

          df <- df$with_columns(
            pl$when(pl$col(diff_y_col)$is_not_null())
              $then(
                pl$when(
                  (pl$col(still_switcher_col) == TRUE) &
                  (pl$col("time_XX") == pl$col("F_g_XX") - 1 + i) &
                  (i <= pl$col("L_g_XX")) &
                  (pl$col("S_g_XX") == increase_XX) &
                  (pl$col(N_gt_control_col) > 0) &
                  pl$col(N_gt_control_col)$is_not_null()
                )$then(pl$lit(1.0))$otherwise(pl$lit(0.0))
              )
              $otherwise(pl$lit(NA_real_))
              $alias(dist_to_switch_col)
          )
        }

      } else {
        ## same_switchers option not specified - use pure Polars
        df <- df$with_columns(pl$lit(NA_integer_)$alias(dist_to_switch_col))

        # Set distance_to_switch where diff_y is not NA
        df <- df$with_columns(
          pl$when(pl$col(diff_y_col)$is_not_null())
            $then(
              pl$when(
                (pl$col("time_XX") == pl$col("F_g_XX") - 1 + i) &
                (pl$lit(i) <= pl$col("L_g_XX")) &
                (pl$col("S_g_XX") == increase_XX) &
                (pl$col(N_gt_control_col) > 0) &
                (pl$col(N_gt_control_col)$is_not_null())
              )$then(pl$lit(1L))$otherwise(pl$lit(0L))
            )
            $otherwise(pl$lit(NA_integer_))
            $alias(dist_to_switch_col)
        )
      }

      ## Creating a variable counting the number of groups l periods away from switch at t
      dist_to_switch_w_col <- paste0("distance_to_switch_", i, "_wXX")
      df <- df$with_columns(
        (pl$col(dist_to_switch_col)$cast(pl$Float64) * pl$col("N_gt_XX"))$alias(dist_to_switch_w_col)
      )

      df <- pl_agg_merge(df, "time_XX", dist_to_switch_w_col, N_inc_t_col, "sum")

      # Also count without weights
      N_dw_col <- paste0("N_dw", increase_XX, "_t_", i, "_XX")
      df <- pl_agg_merge(df, "time_XX", dist_to_switch_col, N_dw_col, "sum")

      ## Computing N^1_l/N^0_l
      # Filter for valid time range and compute
      N_val_df <- df$filter(
        (pl$col("time_XX") >= t_min_XX) & (pl$col("time_XX") <= T_max_XX)
      )$group_by("time_XX")$agg(
        pl$col(N_inc_t_col)$mean()$alias("m")
      )$select(pl$col("m")$sum())

      N_val <- as.data.frame(N_val_df)[[1, 1]]
      if (is.null(N_val) || is.na(N_val)) N_val <- 0
      assign(paste0("N", increase_XX, "_", i, "_XX"), N_val)

      N_dw_val_df <- df$filter(
        (pl$col("time_XX") >= t_min_XX) & (pl$col("time_XX") <= T_max_XX)
      )$group_by("time_XX")$agg(
        pl$col(N_dw_col)$mean()$alias("m")
      )$select(pl$col("m")$sum())

      N_dw_val <- as.data.frame(N_dw_val_df)[[1, 1]]
      if (is.null(N_dw_val) || is.na(N_dw_val)) N_dw_val <- 0
      assign(paste0("N", increase_XX, "_dw_", i, "_XX"), N_dw_val)

      ## Creating N^1_{t,l,g}/N^0_{t,l,g}
      by_cols_Ntg <- c("time_XX", "d_sq_XX")
      if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
        by_cols_Ntg <- c(by_cols_Ntg, trends_nonparam)
      }
      df <- pl_agg_merge(df, by_cols_Ntg, dist_to_switch_w_col, N_inc_t_g_col, "sum")

      ## Creating all the adjustment terms for controls - Pure Polars implementation
      if (!is.null(controls)) {
        df <- df$with_columns(pl$lit(0.0)$alias(paste0("part2_switch", increase_XX, "_", i, "_XX")))

        # T_d_XX = max(F_g_XX) by d_sq_int_XX - 1
        df <- df$with_columns(
          (pl$col("F_g_XX")$max()$over("d_sq_int_XX") - 1)$alias("T_d_XX")
        )

        count_controls <- 0
        for (var in controls) {
          count_controls <- count_controls + 1
          diff_Xi_col <- paste0("diff_X", count_controls, "_", i, "_XX")
          diff_Xi_N_col <- paste0("diff_X", count_controls, "_", i, "_N_XX")

          # Lagged difference of control
          df <- df$with_columns(
            (pl$col(var) - pl$col(var)$shift(i)$over("group_XX"))$alias(diff_Xi_col)
          )
          df <- df$with_columns(
            (pl$col("N_gt_XX") * pl$col(diff_Xi_col))$alias(diff_Xi_N_col)
          )

          for (l in levels_d_sq_XX) {
            m_g_col <- paste0("m", increase_XX, "_g_", l, "_", count_controls, "_", i, "_XX")
            m_col <- paste0("m", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")
            M_col <- paste0("M", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")
            E_hat_denom_col <- paste0("E_hat_denom_", count_controls, "_", l, "_XX")
            E_y_hat_col <- paste0("E_y_hat_gt_", l, "_XX")
            E_y_hat_int_col <- paste0("E_y_hat_gt_int_", l, "_XX")
            N_c_temp_col <- paste0("N_c_", l, "_temp_XX")
            N_c_col <- paste0("N_c_", l, "_XX")
            in_sum_temp_col <- paste0("in_sum_temp_", count_controls, "_", l, "_XX")
            in_sum_col <- paste0("in_sum_", count_controls, "_", l, "_XX")
            prod_col <- paste0("prod_X", count_controls, "_Ngt_XX")

            N_inc_val <- get(paste0("N", increase_XX, "_", i, "_XX"))

            # Compute m_g coefficient
            df <- df$with_columns(
              (
                ((i <= pl$col("T_g_XX") - 2) & (pl$col("d_sq_int_XX") == l))$cast(pl$Float64) *
                (G_XX / N_inc_val) *
                (pl$col(dist_to_switch_col) - (pl$col(N_inc_t_g_col) / pl$col(N_gt_control_col)) * pl$col(never_change_col)) *
                ((pl$col("time_XX") >= i + 1) & (pl$col("time_XX") <= pl$col("T_g_XX")))$cast(pl$Float64) *
                pl$col(diff_Xi_N_col)
              )$alias(m_g_col)
            )

            # Sum by group
            df <- df$with_columns(
              pl$col(m_g_col)$sum()$over("group_XX")$alias(m_col)
            )

            # Keep only first obs by group
            df <- df$with_columns(
              pl$when(pl$col("first_obs_by_gp_XX") == 1)
                $then(pl$col(m_col))
                $otherwise(pl$lit(NA_real_))
                $alias(m_col)
            )

            # Global M coefficient
            M_val <- pl_sum(df, m_col) / G_XX
            df <- df$with_columns(pl$lit(M_val)$alias(M_col))

            # E_hat_denom: count of valid obs for this level by time
            df <- df$with_columns(
              pl$when(
                (pl$col("F_g_XX") > pl$col("time_XX")) &
                (pl$col("d_sq_int_XX") == l) &
                pl$col("diff_y_XX")$is_not_null()
              )$then(pl$lit(1.0))$otherwise(pl$lit(0.0))$alias("_dummy_XX")
            )

            df <- df$with_columns(
              pl$when(pl$col("d_sq_int_XX") == l)
                $then(pl$col("_dummy_XX")$sum()$over(c("time_XX", "d_sq_int_XX")))
                $otherwise(pl$lit(NA_real_))
                $alias(E_hat_denom_col)
            )

            # E_y_hat_gt
            if (E_y_hat_int_col %in% df$columns) {
              df <- df$with_columns(
                (pl$col(E_y_hat_int_col) * (pl$col(E_hat_denom_col) >= 2)$cast(pl$Float64))$alias(E_y_hat_col)
              )
            } else {
              df <- df$with_columns(pl$lit(0.0)$alias(E_y_hat_col))
            }

            # N_c computation
            df <- df$with_columns(
              (pl$col("N_gt_XX") *
                ((pl$col("d_sq_int_XX") == l) &
                 (pl$col("time_XX") >= 2) &
                 (pl$col("time_XX") <= pl$col("T_d_XX")) &
                 (pl$col("time_XX") < pl$col("F_g_XX")) &
                 pl$col("diff_y_XX")$is_not_null())$cast(pl$Float64))$alias(N_c_temp_col)
            )
            N_c_val <- pl_sum(df, N_c_temp_col)
            df <- df$with_columns(pl$lit(N_c_val)$alias(N_c_col))

            # in_sum computation
            if (N_c_val > 0 && prod_col %in% df$columns) {
              df <- df$with_columns(
                (
                  pl$col(prod_col) *
                  (pl$lit(1.0) +
                    (pl$col(E_hat_denom_col) >= 2)$cast(pl$Float64) *
                    ((pl$col(E_hat_denom_col) / (pl$col(E_hat_denom_col) - 1))$sqrt() - 1)) *
                  (pl$col("diff_y_XX") - pl$col(E_y_hat_col)) *
                  ((pl$col("time_XX") >= 2) & (pl$col("time_XX") <= pl$col("F_g_XX") - 1))$cast(pl$Float64) /
                  N_c_val
                )$alias(in_sum_temp_col)
              )
            } else {
              df <- df$with_columns(pl$lit(0.0)$alias(in_sum_temp_col))
            }

            df <- df$with_columns(
              pl$col(in_sum_temp_col)$sum()$over("group_XX")$alias(in_sum_col)
            )

            # Residualize outcome
            if (exists(paste0("useful_res_", l, "_XX")) && get(paste0("useful_res_", l, "_XX")) > 1) {
              coef_val <- get(paste0("coefs_sq_", l, "_XX"))[count_controls, 1]
              df <- df$with_columns(
                pl$when(pl$col("d_sq_int_XX") == l)
                  $then(pl$col(diff_y_col) - coef_val * pl$col(diff_Xi_col))
                  $otherwise(pl$col(diff_y_col))
                  $alias(diff_y_col)
              )
              df <- df$with_columns(pl$lit(0.0)$alias(paste0("in_brackets_", l, "_", count_controls, "_XX")))
            }
          }
        }

        # Clean up temp columns
        df <- pl_safe_drop(df, "_dummy_XX")
      }

      ## Computing variables for demeaning and DOF
      if (is.null(trends_nonparam)) trends_nonparam_use <- character(0) else trends_nonparam_use <- trends_nonparam
      if (is.null(cluster)) cluster_use <- "" else cluster_use <- cluster

      # diff_y_i_N_gt_XX = N_gt_XX * diff_y_i_XX
      diff_y_N_col <- paste0("diff_y_", i, "_N_gt_XX")
      df <- df$with_columns(
        (pl$col("N_gt_XX") * pl$col(diff_y_col))$alias(diff_y_N_col)
      )

      # dof_ns_i_XX and dof_s_i_XX - indicator columns
      dof_ns_col <- paste0("dof_ns_", i, "_XX")
      dof_s_col <- paste0("dof_s_", i, "_XX")

      df <- df$with_columns(
        pl$when(
          pl$col("N_gt_XX")$is_not_null() & (pl$col("N_gt_XX") != 0) &
            pl$col(diff_y_col)$is_not_null() &
            pl$col(never_change_col)$is_not_null() & (pl$col(never_change_col) == 1) &
            pl$col(N_inc_t_col)$is_not_null() & (pl$col(N_inc_t_col) > 0)
        )
          $then(pl$lit(1L))
          $otherwise(pl$lit(0L))
          $alias(dof_ns_col)
      )

      df <- df$with_columns(
        pl$when(
          pl$col("N_gt_XX")$is_not_null() & (pl$col("N_gt_XX") != 0) &
            pl$col(dist_to_switch_col)$is_not_null() & (pl$col(dist_to_switch_col) == 1)
        )
          $then(pl$lit(1L))
          $otherwise(pl$lit(0L))
          $alias(dof_s_col)
      )

      # Grouped totals for ns-cohort
      by_cols_dof <- c("d_sq_XX")
      if (length(trends_nonparam_use) > 0) by_cols_dof <- c(by_cols_dof, trends_nonparam_use)
      by_cols_dof <- c(by_cols_dof, "time_XX")

      count_ns_col <- paste0("count_cohort_", i, "_ns_t_XX")
      total_ns_col <- paste0("total_cohort_", i, "_ns_t_XX")
      mean_ns_col <- paste0("mean_cohort_", i, "_ns_t_XX")
      dof_cohort_ns_col <- paste0("dof_cohort_", i, "_ns_t_XX")

      # Convert to data.table for complex conditional aggregations
      dt <- pl_to_dt(df)

      dt[get(dof_ns_col) == 1, (count_ns_col) := sum(N_gt_XX, na.rm = TRUE), by = by_cols_dof]
      dt[get(dof_ns_col) == 1, (total_ns_col) := sum(get(diff_y_N_col), na.rm = TRUE), by = by_cols_dof]
      dt[, (mean_ns_col) := get(total_ns_col) / get(count_ns_col)]

      if (identical(cluster_use, "") || is.na(cluster_use)) {
        dt[get(dof_ns_col) == 1, (dof_cohort_ns_col) := sum(get(dof_ns_col), na.rm = TRUE), by = by_cols_dof]
      } else {
        cluster_dof_col <- paste0("cluster_dof_", i, "_ns_XX")
        dt[, (cluster_dof_col) := data.table::fifelse(get(dof_ns_col) == 1, get(cluster_use), NA)]
        dt[!is.na(get(cluster_dof_col)), (dof_cohort_ns_col) := data.table::uniqueN(get(cluster_dof_col)), by = by_cols_dof]
      }

      # dof_y_i_N_gt_XX
      dt[[paste0("dof_y_", i, "_N_gt_XX")]] <- as.numeric(dt$N_gt_XX != 0 & !is.na(dt[[diff_y_col]]))

      ## Switchers cohort (C_k) demeaning
      if (isFALSE(less_conservative_se)) {
        by_cols_sw <- c("d_sq_XX", "F_g_XX", "d_fg_XX")
        if (length(trends_nonparam_use) > 0) by_cols_sw <- c(by_cols_sw, trends_nonparam_use)

        count_sw_col <- paste0("count_cohort_", i, "_s_t_XX")
        total_sw_col <- paste0("total_cohort_", i, "_s_t_XX")
        mean_s_col <- paste0("mean_cohort_", i, "_s_t_XX")
        dof_cohort_s_col <- paste0("dof_cohort_", i, "_s_t_XX")

        dt[get(dof_s_col) == 1, (count_sw_col) := sum(N_gt_XX, na.rm = TRUE), by = by_cols_sw]
        dt[get(dof_s_col) == 1, (total_sw_col) := sum(get(diff_y_N_col), na.rm = TRUE), by = by_cols_sw]
        dt[, (mean_s_col) := get(total_sw_col) / get(count_sw_col)]

        if (identical(cluster_use, "") || is.na(cluster_use)) {
          dt[get(dof_s_col) == 1, (dof_cohort_s_col) := sum(get(dof_s_col), na.rm = TRUE), by = by_cols_sw]
        } else {
          cluster_dof_s_col <- paste0("cluster_dof_", i, "_s_XX")
          dt[, (cluster_dof_s_col) := data.table::fifelse(get(dof_s_col) == 1, get(cluster_use), NA)]
          dt[!is.na(get(cluster_dof_s_col)), (dof_cohort_s_col) := data.table::uniqueN(get(cluster_dof_s_col)), by = by_cols_sw]
        }
      } else {
        # less_conservative_se logic (simplified)
        path_0_col <- "path_0_XX"
        path_1_col <- "path_1_XX"
        path_i_col <- paste0("path_", i, "_XX")

        # s0 cohort (by D_{g,1}, F_g)
        dt[, paste0("count_cohort_", i, "_s0_t_XX") := sum(N_gt_XX[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_0_col, trends_nonparam_use)]
        dt[[paste0("count_cohort_", i, "_s0_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("count_cohort_", i, "_s0_t_XX")]], NA)

        dt[, paste0("total_cohort_", i, "_s0_t_XX") := sum(get(diff_y_N_col)[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_0_col, trends_nonparam_use)]
        dt[[paste0("total_cohort_", i, "_s0_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("total_cohort_", i, "_s0_t_XX")]], NA)

        dt[, paste0("dof_cohort_", i, "_s0_t_XX") := sum(get(paste0("dof_y_", i, "_N_gt_XX"))[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_0_col, trends_nonparam_use)]
        dt[[paste0("dof_cohort_", i, "_s0_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("dof_cohort_", i, "_s0_t_XX")]], NA)

        # s1 cohort (by D_{g,1}, F_g, D_{g,F_g})
        dt[, paste0("count_cohort_", i, "_s1_t_XX") := sum(N_gt_XX[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_1_col, trends_nonparam_use)]
        dt[[paste0("count_cohort_", i, "_s1_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("count_cohort_", i, "_s1_t_XX")]], NA)

        dt[, paste0("total_cohort_", i, "_s1_t_XX") := sum(get(diff_y_N_col)[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_1_col, trends_nonparam_use)]
        dt[[paste0("total_cohort_", i, "_s1_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("total_cohort_", i, "_s1_t_XX")]], NA)

        dt[, paste0("dof_cohort_", i, "_s1_t_XX") := sum(get(paste0("dof_y_", i, "_N_gt_XX"))[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_1_col, trends_nonparam_use)]
        dt[[paste0("dof_cohort_", i, "_s1_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("dof_cohort_", i, "_s1_t_XX")]], NA)

        # s2 cohort (full path)
        dt[, paste0("count_cohort_", i, "_s2_t_XX") := sum(N_gt_XX[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_i_col, trends_nonparam_use)]
        dt[[paste0("count_cohort_", i, "_s2_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("count_cohort_", i, "_s2_t_XX")]], NA)

        dt[, paste0("total_cohort_", i, "_s2_t_XX") := sum(get(diff_y_N_col)[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_i_col, trends_nonparam_use)]
        dt[[paste0("total_cohort_", i, "_s2_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("total_cohort_", i, "_s2_t_XX")]], NA)

        dt[, paste0("dof_cohort_", i, "_s2_t_XX") := sum(get(paste0("dof_y_", i, "_N_gt_XX"))[get(dist_to_switch_col) == 1], na.rm = TRUE),
           by = c(path_i_col, trends_nonparam_use)]
        dt[[paste0("dof_cohort_", i, "_s2_t_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1, dt[[paste0("dof_cohort_", i, "_s2_t_XX")]], NA)

        # Mean computations
        cohort_fullpath_i_col <- paste0("cohort_fullpath_", i, "_XX")
        dt[[paste0("mean_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt[[cohort_fullpath_i_col]] == 1,
          dt[[paste0("total_cohort_", i, "_s2_t_XX")]] / dt[[paste0("count_cohort_", i, "_s2_t_XX")]],
          NA
        )
        dt[[paste0("mean_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt[[cohort_fullpath_i_col]] == 0 & dt$cohort_fullpath_1_XX == 1,
          dt[[paste0("total_cohort_", i, "_s1_t_XX")]] / dt[[paste0("count_cohort_", i, "_s1_t_XX")]],
          dt[[paste0("mean_cohort_", i, "_s_t_XX")]]
        )
        dt[[paste0("mean_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt$cohort_fullpath_1_XX == 0,
          dt[[paste0("total_cohort_", i, "_s0_t_XX")]] / dt[[paste0("count_cohort_", i, "_s0_t_XX")]],
          dt[[paste0("mean_cohort_", i, "_s_t_XX")]]
        )

        # DOF computations
        dt[[paste0("dof_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt[[cohort_fullpath_i_col]] == 1,
          dt[[paste0("dof_cohort_", i, "_s2_t_XX")]],
          NA
        )
        dt[[paste0("dof_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt[[cohort_fullpath_i_col]] == 0 & dt$cohort_fullpath_1_XX == 1,
          dt[[paste0("dof_cohort_", i, "_s1_t_XX")]],
          dt[[paste0("dof_cohort_", i, "_s_t_XX")]]
        )
        dt[[paste0("dof_cohort_", i, "_s_t_XX")]] <- data.table::fifelse(
          dt$cohort_fullpath_1_XX == 0,
          dt[[paste0("dof_cohort_", i, "_s0_t_XX")]],
          dt[[paste0("dof_cohort_", i, "_s_t_XX")]]
        )
      }

      ## Union of switchers and not-yet-switchers (ns_s)
      dof_ns_s_col <- paste0("dof_ns_s_", i, "_XX")
      count_ns_s_col <- paste0("count_cohort_", i, "_ns_s_t_XX")
      total_ns_s_col <- paste0("total_cohort_", i, "_ns_s_t_XX")
      mean_ns_s_col <- paste0("mean_cohort_", i, "_ns_s_t_XX")
      dof_cohort_ns_s_col <- paste0("dof_cohort_", i, "_ns_s_t_XX")

      dt[, (dof_ns_s_col) := as.integer((get(dof_s_col) == 1) | (get(dof_ns_col) == 1))]

      dt[get(dof_ns_s_col) == 1, (count_ns_s_col) := sum(N_gt_XX, na.rm = TRUE), by = by_cols_dof]
      dt[get(dof_ns_s_col) == 1, (total_ns_s_col) := sum(get(diff_y_N_col), na.rm = TRUE), by = by_cols_dof]
      dt[, (mean_ns_s_col) := get(total_ns_s_col) / get(count_ns_s_col)]

      if (identical(cluster_use, "") || is.na(cluster_use)) {
        dt[get(dof_ns_s_col) == 1, (dof_cohort_ns_s_col) := sum(get(dof_ns_s_col), na.rm = TRUE), by = by_cols_dof]
      } else {
        cluster_ns_s_col <- paste0("cluster_dof_", i, "_ns_s_XX")
        dt[, (cluster_ns_s_col) := data.table::fifelse(get(dof_ns_s_col) == 1, get(cluster_use), NA)]
        dt[!is.na(get(cluster_ns_s_col)), (dof_cohort_ns_s_col) := data.table::uniqueN(get(cluster_ns_s_col)), by = by_cols_dof]
      }

      # E_hat and DOF_gt computations using the helper functions (inline here)
      E_hat_col <- paste0("E_hat_gt_", i, "_XX")
      DOF_col <- paste0("DOF_gt_", i, "_XX")
      dof_s_t_col <- paste0("dof_cohort_", i, "_s_t_XX")
      dof_ns_t_col <- paste0("dof_cohort_", i, "_ns_t_XX")
      dof_ns_s_t_col <- paste0("dof_cohort_", i, "_ns_s_t_XX")
      mean_s_col <- paste0("mean_cohort_", i, "_s_t_XX")
      mean_ns_col <- paste0("mean_cohort_", i, "_ns_t_XX")
      mean_ns_s_col <- paste0("mean_cohort_", i, "_ns_s_t_XX")

      # Initialize E_hat
      dt[, (E_hat_col) := NA_real_]
      dt[(time_XX < F_g_XX) | (F_g_XX - 1 + i == time_XX), (E_hat_col) := 0]
      dt[(time_XX < F_g_XX) & (is.na(get(dof_ns_t_col)) | get(dof_ns_t_col) >= 2), (E_hat_col) := get(mean_ns_col)]
      dt[is.na(get(mean_ns_col)) | is.nan(get(mean_ns_col)), (E_hat_col) := NA_real_]
      dt[(F_g_XX - 1 + i == time_XX) & (is.na(get(dof_s_t_col)) | get(dof_s_t_col) >= 2), (E_hat_col) := get(mean_s_col)]
      dt[(!is.na(get(dof_ns_s_t_col)) & get(dof_ns_s_t_col) >= 2) &
           (((F_g_XX - 1 + i == time_XX) & !is.na(get(dof_s_t_col)) & get(dof_s_t_col) == 1) |
              ((time_XX < F_g_XX) & !is.na(get(dof_ns_t_col)) & get(dof_ns_t_col) == 1)),
         (E_hat_col) := get(mean_ns_s_col)]
      dt[is.na(get(mean_ns_s_col)) | is.nan(get(mean_ns_s_col)), (E_hat_col) := NA_real_]

      # Initialize DOF_gt
      dt[, (DOF_col) := NA_real_]
      dt[(time_XX < F_g_XX) | (F_g_XX - 1 + i == time_XX), (DOF_col) := 1]
      dt[(F_g_XX - 1 + i == time_XX) & (get(dof_s_t_col) > 1),
         (DOF_col) := sqrt(get(dof_s_t_col) / (get(dof_s_t_col) - 1))]
      dt[(time_XX < F_g_XX) & (get(dof_ns_t_col) > 1),
         (DOF_col) := sqrt(get(dof_ns_t_col) / (get(dof_ns_t_col) - 1))]
      dt[(get(dof_ns_s_t_col) >= 2) &
           (((F_g_XX - 1 + i == time_XX) & (get(dof_s_t_col) == 1)) |
              ((time_XX < F_g_XX) & (get(dof_ns_t_col) == 1))),
         (DOF_col) := sqrt(get(dof_ns_s_t_col) / (get(dof_ns_s_t_col) - 1))]
      dt[is.na(get(dof_s_t_col)) & is.na(get(dof_ns_t_col)) & is.na(get(dof_ns_s_t_col)), (DOF_col) := NA_real_]

      ## Computing U_Gg_l variables
      N_inc_val <- get(paste0("N", increase_XX, "_", i, "_XX"))

      if (!is.null(N_inc_val) && N_inc_val != 0) {

        # dummy_U_Gg
        dt[[dummy_U_col]] <- as.numeric(i <= dt$T_g_XX - 1)

        # U_Gg_temp
        dt[[U_temp_col]] <- dt[[dummy_U_col]] * (G_XX / N_inc_val) *
          as.numeric(dt$time_XX >= i + 1 & dt$time_XX <= dt$T_g_XX) * dt$N_gt_XX *
          (dt[[dist_to_switch_col]] - (dt[[N_inc_t_g_col]] / dt[[N_gt_control_col]]) * dt[[never_change_col]]) *
          dt[[diff_y_col]]

        # Sum by group
        dt[, (U_col) := sum(get(U_temp_col), na.rm = TRUE), by = group_XX]
        dt[[U_col]] <- dt[[U_col]] * dt$first_obs_by_gp_XX

        # Count for N display
        count_col <- paste0("count", i, "_core_XX")
        dt[, (count_col) := 0]
        dt[(get(U_temp_col) != 0 & !is.na(get(U_temp_col))) |
             (get(U_temp_col) == 0 & get(diff_y_col) == 0 &
                (get(dist_to_switch_col) != 0 |
                   (get(N_inc_t_g_col) != 0 & get(never_change_col) != 0))),
           (count_col) := N_gt_XX]
        dt[[count_col]] <- as.numeric(dt[[count_col]])

        # U_Gg_var_temp
        dt[[U_temp_var_col]] <- 0
        dt[[U_temp_var_col]] <- dt[[dummy_U_col]] * (G_XX / N_inc_val) *
          (dt[[dist_to_switch_col]] - (dt[[N_inc_t_g_col]] / dt[[N_gt_control_col]]) * dt[[never_change_col]]) *
          (dt$time_XX >= i + 1 & dt$time_XX <= dt$T_g_XX) * dt$N_gt_XX *
          dt[[DOF_col]] * (dt[[diff_y_col]] - dt[[E_hat_col]])

        # Controls adjustment
        if (!is.null(controls)) {
          for (l in levels_d_sq_XX) {
            dt[[paste0("combined", increase_XX, "_temp_", l, "_", i, "_XX")]] <- 0
            for (j in 1:count_controls) {
              for (k in 1:count_controls) {
                dt[[paste0("in_brackets_", l, "_", j, "_XX")]] <- dt[[paste0("in_brackets_", l, "_", j, "_XX")]] +
                  get(paste0("inv_Denom_", l, "_XX"))[j, k] * dt[[paste0("in_sum_", k, "_", l, "_XX")]] *
                  (dt$d_sq_int_XX == l & dt$F_g_XX >= 3)
              }
              dt[[paste0("in_brackets_", l, "_", j, "_XX")]] <- dt[[paste0("in_brackets_", l, "_", j, "_XX")]] -
                get(paste0("coefs_sq_", l, "_XX"))[j, 1]
              dt[[paste0("combined", increase_XX, "_temp_", l, "_", i, "_XX")]] <-
                dt[[paste0("combined", increase_XX, "_temp_", l, "_", i, "_XX")]] +
                dt[[paste0("M", increase_XX, "_", l, "_", j, "_", i, "_XX")]] *
                dt[[paste0("in_brackets_", l, "_", j, "_XX")]]
            }
            dt[[paste0("part2_switch", increase_XX, "_", i, "_XX")]] <-
              as.numeric(dt[[paste0("part2_switch", increase_XX, "_", i, "_XX")]] +
                           dt[[paste0("combined", increase_XX, "_temp_", l, "_", i, "_XX")]])
          }
        }

        # Sum variance term by group
        dt[[U_temp_var_col]] <- as.numeric(dt[[U_temp_var_col]])
        dt[, (U_var_col) := sum(get(U_temp_var_col), na.rm = TRUE), by = group_XX]

        if (!is.null(controls)) {
          if (increase_XX == 1) {
            dt[[U_var_col]] <- dt[[U_var_col]] - dt[[paste0("part2_switch1_", i, "_XX")]]
          } else {
            dt[[U_var_col]] <- dt[[U_var_col]] - dt[[paste0("part2_switch0_", i, "_XX")]]
          }
        }
      }

      ## Normalized option adjustments
      if (normalized == TRUE) {
        if (is.null(continuous)) {
          dt$sum_temp_XX <- data.table::fifelse(
            dt$time_XX >= dt$F_g_XX & dt$time_XX <= dt$F_g_XX - 1 + i & dt$S_g_XX == increase_XX,
            dt$treatment_XX - dt$d_sq_XX, NA
          )
        } else {
          dt$sum_temp_XX <- data.table::fifelse(
            dt$time_XX >= dt$F_g_XX & dt$time_XX <= dt$F_g_XX - 1 + i & dt$S_g_XX == increase_XX,
            dt$treatment_XX_orig - dt$d_sq_XX_orig, NA
          )
        }
        dt[, paste0("sum_treat_until_", i, "_XX") := sum(sum_temp_XX, na.rm = TRUE), by = group_XX]
        dt$sum_temp_XX <- NULL

        dt[[paste0("delta_D_", i, "_cum_temp_XX")]] <- data.table::fifelse(
          dt[[dist_to_switch_col]] == 1,
          (dt$N_gt_XX / N_inc_val) * (
            dt$S_g_XX * dt[[paste0("sum_treat_until_", i, "_XX")]] +
              (1 - dt$S_g_XX) * (-dt[[paste0("sum_treat_until_", i, "_XX")]])
          ), NA
        )
        assign(paste0("delta_norm_", i, "_XX"), sum(dt[[paste0("delta_D_", i, "_cum_temp_XX")]], na.rm = TRUE))
      }

      # Convert back to polars
      df <- dt_to_pl(dt)
    }

    ## trends_lin adjustment
    Ntrendslin <- 1
    for (i in 1:l_u_a_XX) {
      Ntrendslin <- min(Ntrendslin, get(paste0("N", increase_XX, "_", i, "_XX")), na.rm = TRUE)
    }

    if (isTRUE(trends_lin) && Ntrendslin != 0) {
      dt <- pl_to_dt(df)

      lu <- as.integer(l_u_a_XX)
      col_TL <- sprintf("U_Gg%d_TL", lu)
      col_var_TL <- sprintf("U_Gg%d_var_TL", lu)
      col_XX <- sprintf("U_Gg%d_XX", lu)
      col_var_XX <- sprintf("U_Gg%d_var_XX", lu)

      dt[, c(col_TL, col_var_TL) := .(0.0, 0.0)]

      for (i in seq_len(lu)) {
        dt[, (col_TL) := get(col_TL) + get(sprintf("U_Gg%d_XX", i))]
        dt[, (col_var_TL) := get(col_var_TL) + get(sprintf("U_Gg%d_var_XX", i))]
      }

      dt[, (col_XX) := get(col_TL)]
      dt[, (col_var_XX) := get(col_var_TL)]

      df <- dt_to_pl(dt)
    }

    ####### 5. Placebo computations (simplified - using data.table for complex operations)
    if (placebo != 0 && l_placebo_u_a_XX >= 1) {
      dt <- pl_to_dt(df)

      for (i in 1:l_placebo_u_a_XX) {
        # Column names
        diff_y_pl_col <- paste0("diff_y_pl_", i, "_XX")
        dist_to_switch_pl_col <- paste0("dist_to_switch_pl_", i, "_XX")
        never_change_pl_col <- paste0("never_change_d_pl_", i, "_XX")
        N_gt_control_pl_col <- paste0("N_gt_control_placebo_", i, "_XX")
        N_t_placebo_col <- paste0("N", increase_XX, "_t_placebo_", i, "_XX")
        N_t_placebo_g_col <- paste0("N", increase_XX, "_t_placebo_", i, "_g_XX")

        # Compute long differences for placebos
        dt[, (diff_y_pl_col) := data.table::shift(outcome_XX, 2 * i) - data.table::shift(outcome_XX, i), by = group_XX]

        # Identify controls for placebos
        never_change_effect_col <- paste0("never_change_d_", i, "_XX")
        dt[[never_change_pl_col]] <- dt[[never_change_effect_col]] * (!is.na(dt[[diff_y_pl_col]]))

        never_change_pl_w_col <- paste0("never_change_d_pl_", i, "_wXX")
        dt[[never_change_pl_w_col]] <- dt[[never_change_pl_col]] * dt$N_gt_XX

        by_cols_pl <- c("time_XX", "d_sq_XX")
        if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
          by_cols_pl <- c(by_cols_pl, trends_nonparam)
        }

        dt[, (N_gt_control_pl_col) := sum(get(never_change_pl_w_col), na.rm = TRUE), by = by_cols_pl]

        # Distance to switch for placebos
        dist_effect_col <- paste0("distance_to_switch_", i, "_XX")
        dt[, (dist_to_switch_pl_col) := NA_real_]
        dt[!is.na(get(dist_effect_col)),
           (dist_to_switch_pl_col) := get(dist_effect_col) *
             as.integer(!is.na(get(diff_y_pl_col))) *
             as.integer(get(N_gt_control_pl_col) > 0 & !is.na(get(N_gt_control_pl_col)))]

        if (isTRUE(same_switchers_pl)) {
          dt[[dist_to_switch_pl_col]] <- dt[[dist_to_switch_pl_col]] * dt$fillin_g_pl_XX
        }

        dist_to_switch_pl_w_col <- paste0("dist_to_switch_pl_", i, "_wXX")
        dt[[dist_to_switch_pl_w_col]] <- dt[[dist_to_switch_pl_col]] * dt$N_gt_XX

        dt[, (N_t_placebo_col) := sum(get(dist_to_switch_pl_w_col), na.rm = TRUE), by = time_XX]

        # Compute N_placebo
        N_pl_val <- dt[time_XX >= t_min_XX & time_XX <= T_max_XX,
                       .(m = mean(get(N_t_placebo_col), na.rm = TRUE)), by = time_XX][, sum(m, na.rm = TRUE)]
        if (is.null(N_pl_val) || is.na(N_pl_val)) N_pl_val <- 0
        assign(paste0("N", increase_XX, "_placebo_", i, "_XX"), N_pl_val)

        dt[, (N_t_placebo_g_col) := sum(get(dist_to_switch_pl_w_col), na.rm = TRUE), by = by_cols_pl]

        ## Controls for placebos (simplified)
        if (!is.null(controls) && N_pl_val != 0) {
          dt[[paste0("part2_pl_switch", increase_XX, "_", i, "_XX")]] <- 0

          count_controls <- 0
          for (var in controls) {
            count_controls <- count_controls + 1
            dt[, paste0("diff_X", count_controls, "_placebo_", i, "_XX") :=
                 data.table::shift(get(var), 2 * i) - data.table::shift(get(var), i), by = group_XX]

            dt[[paste0("diff_X", count_controls, "_pl_", i, "_N_XX")]] <- dt$N_gt_XX *
              dt[[paste0("diff_X", count_controls, "_placebo_", i, "_XX")]]

            for (l in levels_d_sq_XX) {
              dt[[paste0("m", increase_XX, "_pl_g_", l, "_", count_controls, "_", i, "_XX")]] <-
                (i <= dt$T_g_XX - 2 & dt$d_sq_int_XX == l) *
                (G_XX / N_pl_val) *
                ((dt[[dist_to_switch_pl_col]] - (dt[[N_t_placebo_g_col]] / dt[[N_gt_control_pl_col]]) * dt[[never_change_pl_col]]) *
                   (dt$time_XX >= i + 1 & dt$time_XX <= dt$T_g_XX) *
                   (dt[[paste0("diff_X", count_controls, "_pl_", i, "_N_XX")]]))

              dt[, paste0("m_pl", increase_XX, "_", l, "_", count_controls, "_", i, "_XX") :=
                   sum(get(paste0("m", increase_XX, "_pl_g_", l, "_", count_controls, "_", i, "_XX")), na.rm = TRUE),
                 by = group_XX]
              dt[[paste0("m_pl", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")]] <-
                data.table::fifelse(dt$first_obs_by_gp_XX == 1,
                                    dt[[paste0("m_pl", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")]], NA)

              dt[[paste0("M_pl", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")]] <-
                sum(dt[[paste0("m_pl", increase_XX, "_", l, "_", count_controls, "_", i, "_XX")]], na.rm = TRUE) / G_XX

              if (exists(paste0("useful_res_", l, "_XX")) && get(paste0("useful_res_", l, "_XX")) > 1) {
                dt[[diff_y_pl_col]] <- data.table::fifelse(
                  dt$d_sq_int_XX == l,
                  dt[[diff_y_pl_col]] - get(paste0("coefs_sq_", l, "_XX"))[count_controls, 1] *
                    dt[[paste0("diff_X", count_controls, "_placebo_", i, "_XX")]],
                  dt[[diff_y_pl_col]]
                )
                dt[[paste0("in_brackets_pl_", l, "_", count_controls, "_XX")]] <- 0
              }
            }
          }
        }

        ## DOF computations for placebos
        diff_y_pl_N_col <- paste0("diff_y_pl_", i, "_N_gt_XX")
        dof_ns_pl_col <- paste0("dof_ns_pl_", i, "_XX")
        dof_s_pl_col <- paste0("dof_s_pl_", i, "_XX")

        dt[, (diff_y_pl_N_col) := get(diff_y_pl_col) * N_gt_XX]

        dt[, (dof_ns_pl_col) := as.integer(
          N_gt_XX != 0 &
            !is.na(get(diff_y_pl_col)) &
            get(never_change_pl_col) == 1 &
            get(N_t_placebo_col) > 0 &
            !is.na(get(N_t_placebo_col))
        )]

        dt[, (dof_s_pl_col) := as.integer(N_gt_XX != 0 & get(dist_to_switch_pl_col) == 1)]
        dt[is.na(get(dof_s_pl_col)), (dof_s_pl_col) := 0L]

        # Cohort means for placebos
        by_cols_pl_dof <- c("d_sq_XX", "time_XX")
        if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
          by_cols_pl_dof <- c("d_sq_XX", "time_XX", trends_nonparam)
        }

        count_ns_pl_col <- paste0("count_cohort_pl_", i, "_ns_t_XX")
        total_ns_pl_col <- paste0("total_cohort_pl_", i, "_ns_t_XX")
        mean_ns_pl_col <- paste0("mean_cohort_pl_", i, "_ns_t_XX")
        dof_cohort_ns_pl_col <- paste0("dof_cohort_pl_", i, "_ns_t_XX")

        dt[get(dof_ns_pl_col) == 1L, (count_ns_pl_col) := sum(N_gt_XX), by = by_cols_pl_dof]
        dt[get(dof_ns_pl_col) == 1L, (total_ns_pl_col) := sum(get(diff_y_pl_N_col)), by = by_cols_pl_dof]
        dt[, (mean_ns_pl_col) := get(total_ns_pl_col) / get(count_ns_pl_col)]

        if (is.null(cluster) || cluster == "") {
          dt[get(dof_ns_pl_col) == 1L, (dof_cohort_ns_pl_col) := as.numeric(sum(get(dof_ns_pl_col))), by = by_cols_pl_dof]
        } else {
          clust_dof_ns_pl <- paste0("cluster_dof_pl_", i, "_ns_XX")
          dt[, (clust_dof_ns_pl) := data.table::fifelse(get(dof_ns_pl_col) == 1L, get(cluster), NA)]
          dt[!is.na(get(clust_dof_ns_pl)), (dof_cohort_ns_pl_col) := as.numeric(data.table::uniqueN(get(clust_dof_ns_pl))), by = by_cols_pl_dof]
        }

        # Switchers cohort for placebos
        by_cols_pl_sw <- c("d_sq_XX", "F_g_XX", "d_fg_XX")
        if (!is.null(trends_nonparam) && length(trends_nonparam) > 0) {
          by_cols_pl_sw <- c(by_cols_pl_sw, trends_nonparam)
        }

        count_s_pl_col <- paste0("count_cohort_pl_", i, "_s_t_XX")
        total_s_pl_col <- paste0("total_cohort_pl_", i, "_s_t_XX")
        mean_s_pl_col <- paste0("mean_cohort_pl_", i, "_s_t_XX")
        dof_cohort_s_pl_col <- paste0("dof_cohort_pl_", i, "_s_t_XX")

        dt[get(dof_s_pl_col) == 1L, (count_s_pl_col) := sum(N_gt_XX), by = by_cols_pl_sw]
        dt[get(dof_s_pl_col) == 1L, (total_s_pl_col) := sum(get(diff_y_pl_N_col)), by = by_cols_pl_sw]
        dt[, (mean_s_pl_col) := get(total_s_pl_col) / get(count_s_pl_col)]

        if (is.null(cluster) || cluster == "") {
          dt[get(dof_s_pl_col) == 1L, (dof_cohort_s_pl_col) := as.numeric(sum(get(dof_s_pl_col))), by = by_cols_pl_sw]
        } else {
          clust_dof_s_pl <- paste0("cluster_dof_pl_", i, "_s_XX")
          dt[, (clust_dof_s_pl) := data.table::fifelse(get(dof_s_pl_col) == 1L, get(cluster), NA)]
          dt[!is.na(get(clust_dof_s_pl)), (dof_cohort_s_pl_col) := as.numeric(data.table::uniqueN(get(clust_dof_s_pl))), by = by_cols_pl_sw]
        }

        # Union for placebos
        dof_ns_s_pl_col <- paste0("dof_ns_s_pl_", i, "_XX")
        count_ns_s_pl_col <- paste0("count_cohort_pl_", i, "_ns_s_t_XX")
        total_ns_s_pl_col <- paste0("total_cohort_pl_", i, "_ns_s_t_XX")
        mean_ns_s_pl_col <- paste0("mean_cohort_pl_", i, "_ns_s_t_XX")
        dof_cohort_ns_s_pl_col <- paste0("dof_cohort_pl_", i, "_ns_s_t_XX")

        dt[, (dof_ns_s_pl_col) := as.numeric((get(dof_s_pl_col) == 1L) | (get(dof_ns_pl_col) == 1L))]
        dt[is.na(get(dof_s_pl_col)) | is.na(get(dof_ns_pl_col)), (dof_ns_s_pl_col) := NA_real_]

        dt[get(dof_ns_s_pl_col) == 1, (count_ns_s_pl_col) := sum(N_gt_XX), by = by_cols_pl_dof]
        dt[get(dof_ns_s_pl_col) == 1, (total_ns_s_pl_col) := sum(get(diff_y_pl_N_col)), by = by_cols_pl_dof]
        dt[, (mean_ns_s_pl_col) := get(total_ns_s_pl_col) / get(count_ns_s_pl_col)]

        if (is.null(cluster) || cluster == "") {
          dt[get(dof_ns_s_pl_col) == 1, (dof_cohort_ns_s_pl_col) := as.numeric(sum(get(dof_ns_s_pl_col))), by = by_cols_pl_dof]
        } else {
          clust_dof_ns_s_pl <- paste0("cluster_dof_pl_", i, "_ns_s_XX")
          dt[, (clust_dof_ns_s_pl) := data.table::fifelse(get(dof_ns_s_pl_col) == 1, get(cluster), NA)]
          dt[!is.na(get(clust_dof_ns_s_pl)), (dof_cohort_ns_s_pl_col) := as.numeric(data.table::uniqueN(get(clust_dof_ns_s_pl))), by = by_cols_pl_dof]
        }

        # E_hat and DOF for placebos
        E_hat_pl_col <- paste0("E_hat_gt_pl_", i, "_XX")
        DOF_pl_col <- paste0("DOF_gt_pl_", i, "_XX")

        dt[, (E_hat_pl_col) := NA_real_]
        dt[(time_XX < F_g_XX) | (F_g_XX - 1 + i == time_XX), (E_hat_pl_col) := 0]
        dt[(time_XX < F_g_XX) & (is.na(get(dof_cohort_ns_pl_col)) | get(dof_cohort_ns_pl_col) >= 2), (E_hat_pl_col) := get(mean_ns_pl_col)]
        dt[is.na(get(mean_ns_pl_col)) | is.nan(get(mean_ns_pl_col)), (E_hat_pl_col) := NA_real_]
        dt[(F_g_XX - 1 + i == time_XX) & (is.na(get(dof_cohort_s_pl_col)) | get(dof_cohort_s_pl_col) >= 2), (E_hat_pl_col) := get(mean_s_pl_col)]
        dt[(!is.na(get(dof_cohort_ns_s_pl_col)) & get(dof_cohort_ns_s_pl_col) >= 2) &
             (((F_g_XX - 1 + i == time_XX) & !is.na(get(dof_cohort_s_pl_col)) & get(dof_cohort_s_pl_col) == 1) |
                ((time_XX < F_g_XX) & !is.na(get(dof_cohort_ns_pl_col)) & get(dof_cohort_ns_pl_col) == 1)),
           (E_hat_pl_col) := get(mean_ns_s_pl_col)]
        dt[is.na(get(mean_ns_s_pl_col)) | is.nan(get(mean_ns_s_pl_col)), (E_hat_pl_col) := NA_real_]

        dt[, (DOF_pl_col) := NA_real_]
        dt[(time_XX < F_g_XX) | (F_g_XX - 1 + i == time_XX), (DOF_pl_col) := 1]
        dt[(F_g_XX - 1 + i == time_XX) & (get(dof_cohort_s_pl_col) > 1),
           (DOF_pl_col) := sqrt(get(dof_cohort_s_pl_col) / (get(dof_cohort_s_pl_col) - 1))]
        dt[(time_XX < F_g_XX) & (get(dof_cohort_ns_pl_col) > 1),
           (DOF_pl_col) := sqrt(get(dof_cohort_ns_pl_col) / (get(dof_cohort_ns_pl_col) - 1))]
        dt[(get(dof_cohort_ns_s_pl_col) >= 2) &
             (((F_g_XX - 1 + i == time_XX) & (get(dof_cohort_s_pl_col) == 1)) |
                ((time_XX < F_g_XX) & (get(dof_cohort_ns_pl_col) == 1))),
           (DOF_pl_col) := sqrt(get(dof_cohort_ns_s_pl_col) / (get(dof_cohort_ns_s_pl_col) - 1))]
        dt[is.na(get(dof_cohort_s_pl_col)) & is.na(get(dof_cohort_ns_pl_col)) & is.na(get(dof_cohort_ns_s_pl_col)), (DOF_pl_col) := NA_real_]

        ## U_Gg for placebos
        dummy_U_pl_col <- paste0("dummy_U_Gg_pl_", i, "_XX")
        U_pl_temp_col <- paste0("U_Gg_pl_", i, "_temp_XX")
        U_placebo_col <- paste0("U_Gg_placebo_", i, "_XX")
        U_pl_temp_var_col <- paste0("U_Gg_pl_", i, "_temp_var_XX")
        U_pl_var_col <- paste0("U_Gg_pl_", i, "_var_XX")

        dt[[dummy_U_pl_col]] <- i <= dt$T_g_XX - 1

        N_placebo_val <- get(paste0("N", increase_XX, "_placebo_", i, "_XX"))

        if (!is.null(N_placebo_val) && N_placebo_val != 0) {

          dt[[U_pl_temp_col]] <- dt[[dummy_U_pl_col]] *
            (G_XX / N_placebo_val) * dt$N_gt_XX *
            (dt[[dist_to_switch_pl_col]] - (dt[[N_t_placebo_g_col]] / dt[[N_gt_control_pl_col]]) * dt[[never_change_pl_col]]) *
            dt[[diff_y_pl_col]] * (dt$time_XX >= i + 1 & dt$time_XX <= dt$T_g_XX)

          dt[, (U_placebo_col) := sum(get(U_pl_temp_col), na.rm = TRUE), by = group_XX]
          dt[[U_placebo_col]] <- dt[[U_placebo_col]] * dt$first_obs_by_gp_XX

          count_pl_col <- paste0("count", i, "_pl_core_XX")
          dt[[count_pl_col]] <- data.table::fifelse(
            !is.na(dt[[U_pl_temp_col]]) & dt[[U_pl_temp_col]] != 0 |
              dt[[U_pl_temp_col]] == 0 & dt[[diff_y_pl_col]] == 0 &
              (dt[[dist_to_switch_pl_col]] != 0 | dt[[N_t_placebo_g_col]] != 0 & dt[[never_change_pl_col]] != 0),
            dt$N_gt_XX, 0
          )
          dt[[count_pl_col]] <- as.numeric(dt[[count_pl_col]])

          dt[[U_pl_temp_var_col]] <- 0
          dt[[U_pl_temp_var_col]] <- dt[[dummy_U_pl_col]] * (G_XX / N_placebo_val) *
            (dt[[dist_to_switch_pl_col]] - (dt[[N_t_placebo_g_col]] / dt[[N_gt_control_pl_col]]) * dt[[never_change_pl_col]]) *
            (dt$time_XX >= i + 1 & dt$time_XX <= dt$T_g_XX) * dt$N_gt_XX *
            dt[[DOF_pl_col]] * (dt[[diff_y_pl_col]] - dt[[E_hat_pl_col]])

          if (!is.null(controls)) {
            for (l in levels_d_sq_XX) {
              dt[[paste0("combined_pl", increase_XX, "_temp_", l, "_", i, "_XX")]] <- 0
              for (j in 1:count_controls) {
                for (k in 1:count_controls) {
                  dt[[paste0("in_brackets_pl_", l, "_", j, "_XX")]] <- dt[[paste0("in_brackets_pl_", l, "_", j, "_XX")]] +
                    get(paste0("inv_Denom_", l, "_XX"))[j, k] * dt[[paste0("in_sum_", k, "_", l, "_XX")]] *
                    (dt$d_sq_int_XX == l & dt$F_g_XX >= 3)
                }
                dt[[paste0("in_brackets_pl_", l, "_", j, "_XX")]] <- dt[[paste0("in_brackets_pl_", l, "_", j, "_XX")]] -
                  get(paste0("coefs_sq_", l, "_XX"))[j, 1]
                dt[[paste0("combined_pl", increase_XX, "_temp_", l, "_", i, "_XX")]] <-
                  dt[[paste0("combined_pl", increase_XX, "_temp_", l, "_", i, "_XX")]] +
                  dt[[paste0("M_pl", increase_XX, "_", l, "_", j, "_", i, "_XX")]] *
                  dt[[paste0("in_brackets_pl_", l, "_", j, "_XX")]]
              }
              dt[[paste0("part2_pl_switch", increase_XX, "_", i, "_XX")]] <-
                as.numeric(dt[[paste0("part2_pl_switch", increase_XX, "_", i, "_XX")]]) +
                dt[[paste0("combined_pl", increase_XX, "_temp_", l, "_", i, "_XX")]]
            }
          }

          dt[, (U_pl_var_col) := sum(get(U_pl_temp_var_col), na.rm = TRUE), by = group_XX]

          if (!is.null(controls)) {
            if (increase_XX == 1) {
              dt[[U_pl_var_col]] <- dt[[U_pl_var_col]] - dt[[paste0("part2_pl_switch1_", i, "_XX")]]
            } else {
              dt[[U_pl_var_col]] <- dt[[U_pl_var_col]] - dt[[paste0("part2_pl_switch0_", i, "_XX")]]
            }
          }
        }

        ## Normalized for placebos
        if (normalized == TRUE) {
          if (is.null(continuous)) {
            dt$sum_temp_pl_XX <- data.table::fifelse(
              dt$time_XX >= dt$F_g_XX & dt$time_XX <= dt$F_g_XX - 1 + i & dt$S_g_XX == increase_XX,
              dt$treatment_XX - dt$d_sq_XX, NA
            )
          } else {
            dt$sum_temp_pl_XX <- data.table::fifelse(
              dt$time_XX >= dt$F_g_XX & dt$time_XX <= dt$F_g_XX - 1 + i & dt$S_g_XX == increase_XX,
              dt$treatment_XX_orig - dt$d_sq_XX_orig, NA
            )
          }
          dt[, paste0("sum_treat_until_", i, "_pl_XX") := sum(sum_temp_pl_XX, na.rm = TRUE), by = group_XX]
          dt$sum_temp_pl_XX <- NULL

          dt[[paste0("delta_D_pl_", i, "_cum_temp_XX")]] <- data.table::fifelse(
            dt[[dist_to_switch_pl_col]] == 1,
            (dt$N_gt_XX / N_placebo_val) * (
              dt$S_g_XX * dt[[paste0("sum_treat_until_", i, "_pl_XX")]] +
                (1 - dt$S_g_XX) * (-dt[[paste0("sum_treat_until_", i, "_pl_XX")]])
            ), NA
          )
          assign(paste0("delta_norm_pl_", i, "_XX"), sum(dt[[paste0("delta_D_pl_", i, "_cum_temp_XX")]], na.rm = TRUE))
        }
      }

      # trends_lin for placebos
      Ntrendslin_pl <- 1
      if (isTRUE(trends_lin)) {
        for (i in 1:l_placebo_u_a_XX) {
          Ntrendslin_pl <- min(Ntrendslin_pl, get(paste0("N", increase_XX, "_placebo_", i, "_XX")), na.rm = TRUE)
        }
      }

      if (trends_lin && Ntrendslin_pl != 0) {
        lp <- as.integer(l_placebo_u_a_XX)
        col_TL <- sprintf("U_Gg_pl_%d_TL", lp)
        col_var_TL <- sprintf("U_Gg_pl_%d_var_TL", lp)
        col_placebo <- sprintf("U_Gg_placebo_%d_XX", lp)
        col_pl_var_XX <- sprintf("U_Gg_pl_%d_var_XX", lp)

        dt[, c(col_TL, col_var_TL) := .(0.0, 0.0)]

        for (i in seq_len(lp)) {
          dt[, (col_TL) := get(col_TL) + get(sprintf("U_Gg_placebo_%d_XX", i))]
          dt[, (col_var_TL) := get(col_var_TL) + get(sprintf("U_Gg_pl_%d_var_XX", i))]
        }

        dt[, (col_placebo) := get(col_TL)]
        dt[, (col_pl_var_XX) := get(col_var_TL)]
      }

      df <- dt_to_pl(dt)
    }

    ####### 8. Computing Average Total Effect estimator
    if (!trends_lin) {
      dt <- pl_to_dt(df)

      total_key <- sprintf("sum_N%s_l_XX", increase_XX)
      sum_N <- sum(vapply(
        seq_len(as.integer(l_u_a_XX)),
        function(j) {
          get(sprintf("N%s_%s_XX", increase_XX, j))
        },
        numeric(1)
      ))
      assign(total_key, sum_N)

      # Initialize columns
      dt[, c("U_Gg_XX", "U_Gg_num_XX", "U_Gg_den_XX", "U_Gg_num_var_XX", "U_Gg_var_XX") := 0]

      for (i in seq_len(as.integer(l_u_a_XX))) {
        N_name <- sprintf("N%s_%s_XX", increase_XX, i)
        N_increase <- get(N_name)
        sum_N_increase <- get(total_key)

        delta_temp <- sprintf("delta_D_%s_temp_XX", i)
        delta <- sprintf("delta_D_%s_XX", i)
        delta_g <- sprintf("delta_D_g_%s_XX", i)
        dist_to_switch <- sprintf("distance_to_switch_%s_XX", i)

        if (!is.null(N_increase) && N_increase != 0) {
          w_i <- N_increase / sum_N_increase
          assign(sprintf("w_%s_XX", i), w_i)

          if (is.null(continuous)) {
            dt[, (delta_temp) := 0]
            dt[get(dist_to_switch) == 1,
               (delta_temp) := (N_gt_XX / N_increase) *
                 ((treatment_XX - d_sq_XX) * S_g_XX +
                    (1 - S_g_XX) * (d_sq_XX - treatment_XX))]
          } else {
            dt[, (delta_temp) := 0]
            dt[get(dist_to_switch) == 1,
               (delta_temp) := (N_gt_XX / get(N_name)) *
                 ((treatment_XX_orig - d_sq_XX_orig) * S_g_XX +
                    (1 - S_g_XX) * (d_sq_XX_orig - treatment_XX_orig))]
          }

          total_delta <- dt[, sum(get(delta_temp), na.rm = TRUE)]
          dt[, (delta) := total_delta]
          dt[, (delta_g) := get(delta_temp) * (N_increase / N_gt_XX)]
          dt[, (delta_temp) := NULL]

          U_col_i <- sprintf("U_Gg%s_XX", i)
          U_var_col_i <- sprintf("U_Gg%s_var_XX", i)

          dt[,
             `:=`(
               U_Gg_num_XX = U_Gg_num_XX + w_i * get(U_col_i),
               U_Gg_num_var_XX = U_Gg_num_var_XX + w_i * get(U_var_col_i),
               U_Gg_den_XX = U_Gg_den_XX + w_i * get(delta)
             ),
             by = "group_XX"]
        }
      }

      dt[, U_Gg_XX := U_Gg_num_XX / U_Gg_den_XX]
      dt[, U_Gg_var_XX := U_Gg_num_var_XX / U_Gg_den_XX]

      df <- dt_to_pl(dt)
    }

    ## Update passthrough constants
    for (e in names(const)) {
      const[[e]] <- get(e)
    }
    const[[paste0("sum_N", increase_XX, "_l_XX")]] <- get(paste0("sum_N", increase_XX, "_l_XX"))

    for (i in 1:l_u_a_XX) {
      if (isTRUE(normalized)) {
        const[[paste0("delta_norm_", i, "_XX")]] <- get(paste0("delta_norm_", i, "_XX"))
      }
    }
    if (placebo != 0) {
      if (l_placebo_u_a_XX >= 1) {
        for (i in 1:l_placebo_u_a_XX) {
          if (isTRUE(normalized)) {
            const[[paste0("delta_norm_pl_", i, "_XX")]] <- get(paste0("delta_norm_pl_", i, "_XX"))
          }
        }
      }
    }

  })

  # Convert Polars DataFrame back to data.frame for compatibility with main function
  if (inherits(df, "RPolarsDataFrame")) {
    df <- as.data.frame(df)
  }

  data <- list(
    df = df,
    const = const
  )

  return(data)
}
