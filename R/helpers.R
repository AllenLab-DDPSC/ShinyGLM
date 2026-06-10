# For GLM fitting module

# Input here should be imputed, time centered, log transformed (based on user selection)
# that is already in the long format with reference level set

### Helper for Failed lm Fit ###
make_na_row <- function(x, cond_levels) {
  out <- data.frame(ID = x,
                    category = NA_character_,
                    rmsd = NA_real_,
                    adj_r2 = NA_real_,
                    weighted_rmsd = NA_real_,
                    beta0 = NA_real_,
                    beta1 = NA_real_,
                    beta2 = NA_real_,
                    beta3 = NA_real_,
                    beta4 = NA_real_,
                    beta5 = NA_real_)

  colnames(out)[6:11] <- c(paste0("intercept_", cond_levels[1]),
                           paste0("slope_", cond_levels[1]),
                           paste0("curvature_", cond_levels[1]),
                           paste0("intercept_", cond_levels[2]),
                           paste0("slope_", cond_levels[2]),
                           paste0("curvature_", cond_levels[2]))
  out
}

### GLM Fitting and Trajectory Classification ###
GLM_Fit_and_Classification <- function(df){
  cond_levels <- levels(df$Condition)
  do.call(rbind,
          lapply(unique(df$ID), function(x){
            tryCatch({
            subdata <- df[df$ID == x,]
            # Fit the model
            model <- lm(Expression ~ Indicator + Time + I(Time^2) + Indicator:Time + Indicator:I(Time^2), data = subdata)
            # Get classification of the current feature
            # Use linearHypothesis to test linear combinations
            lh_quad_cat2 <- linearHypothesis(model, "I(Time^2) + Indicator:I(Time^2) = 0", singular.ok = TRUE)
            lh_linear_cat2 <- linearHypothesis(model, "Time + Indicator:Time = 0", singular.ok = TRUE)
            # Get p-values
            p_quad_cat2 <- lh_quad_cat2$`Pr(>F)`[2]
            p_linear_cat2 <- lh_linear_cat2$`Pr(>F)`[2]
            # Get cat1 (reference level) p-values from broom
            tidy_model <- tidy(model)
            p_quad_cat1 <- tidy_model$p.value[tidy_model$term == "I(Time^2)"]
            p_linear_cat1 <- tidy_model$p.value[tidy_model$term == "Time"]
            # Get beta signs
            beta1 <- tidy_model$estimate[tidy_model$term == "Time"]
            beta2 <- tidy_model$estimate[tidy_model$term == "I(Time^2)"]
            beta4 <- tidy_model$estimate[tidy_model$term == "Indicator:Time"]
            beta5 <- tidy_model$estimate[tidy_model$term == "Indicator:I(Time^2)"]
            linear_cat2 <- beta1 + beta4
            quad_cat2 <- beta2 + beta5
            # Also get other betas together for 3D plot
            beta0 <- tidy_model$estimate[tidy_model$term == "(Intercept)"]
            beta3 <- tidy_model$estimate[tidy_model$term == "Indicator"]
            # Classification
            category <- case_when(
              p_quad_cat1 < 0.05 & p_quad_cat2 < 0.05 & sign(beta2) == sign(quad_cat2) ~ "Polynomial Concordance",
              p_quad_cat1 < 0.05 & p_quad_cat2 < 0.05 & sign(beta2) != sign(quad_cat2) ~ "Polynomial Discordance",
              p_quad_cat1 >= 0.05 & p_quad_cat2 >= 0.05 & p_linear_cat1 < 0.05 & p_linear_cat2 < 0.05 &
                sign(beta1) == sign(linear_cat2) ~ "Linear Concordance",
              p_quad_cat1 >= 0.05 & p_quad_cat2 >= 0.05 & p_linear_cat1 < 0.05 & p_linear_cat2 < 0.05 &
                sign(beta1) != sign(linear_cat2) ~ "Linear Discordance",
              TRUE ~ "Cross-Model Discordance"
            )

            # RMSD Calculation
            adj_r2 <- summary(model)$adj.r.squared
            vcov_mat <- vcov(model)
            # 100 grid points (make this an argument for the function?)
            time_grid <- seq(min(subdata$Time), max(subdata$Time), length.out = 100)
            new_data <- data.frame(Time = rep(time_grid, 2),
                                   Indicator = rep(c(0,1), each = 100))
            # Prediction
            new_data$pred <- predict(model, new_data)
            rmsd <- new_data %>%
              group_by(Time) %>%
              summarise(
                diff = diff(pred),
                se_diff = {
                  x <- unique(Time)
                  X0 <- c(1, 0, x, x^2, 0*x, 0*x^2)
                  X1 <- c(1, 1, x, x^2, 1*x, 1*x^2)
                  diff_vec <- X1 - X0
                  sqrt(as.numeric(t(diff_vec) %*% vcov_mat %*% diff_vec))
                },
                .groups = "drop"
              )

            rmsd <- sqrt(mean((rmsd$diff / rmsd$se_diff)^2))
            weighted_rmsd <- rmsd * adj_r2

            res_df <- data.frame(ID = x, category = category,
                                 rmsd = rmsd, adj_r2 = adj_r2,
                                 weighted_rmsd = weighted_rmsd,
                                 beta0, beta1, beta2,
                                 beta0 + beta3, linear_cat2, quad_cat2)
            colnames(res_df)[6:11] <- c(paste0("intercept_", cond_levels[1]),
                                        paste0("slope_", cond_levels[1]),
                                        paste0("curvature_", cond_levels[1]),
                                        paste0("intercept_", cond_levels[2]),
                                        paste0("slope_", cond_levels[2]),
                                        paste0("curvature_", cond_levels[2]))
            return(res_df)
          }, error = function(e){
            make_na_row(x, cond_levels)
          }) #trycatch
          }) #lapply
  ) #do.call
}

### Permutation Test ###
PreparePermutationData <- function(df, len_x = 100) {
  df <- df[order(df$Time), ]
  time_points <- seq(min(df$Time), max(df$Time), length.out = len_x)
  X0 <- cbind(
    1,
    0,
    time_points,
    time_points^2,
    0,
    0
  )

  X1 <- cbind(
    1,
    1,
    time_points,
    time_points^2,
    time_points,
    time_points^2
  )

  list(
    y = df$Expression,
    Indicator = df$Indicator,
    Time = df$Time,
    time_groups = split(seq_len(nrow(df)), df$Time),
    X0 = X0,
    X1 = X1,
    D = X1 - X0,
    n = nrow(df),
    p = 6
  )
}

PermuteIndicatorWithinTime <- function(indicator, time_groups) {
  out <- indicator
  for (idx in time_groups) {
    out[idx] <- sample(out[idx])
  }
  out
}

PermutationTestFast <- function(prep) {
  Indicator_perm <- PermuteIndicatorWithinTime(
    prep$Indicator,
    prep$time_groups
  )
  Time <- prep$Time
  y <- prep$y
  X <- cbind(
    1,
    Indicator_perm,
    Time,
    Time^2,
    Indicator_perm * Time,
    Indicator_perm * Time^2
  )
  fit <- lm.fit(X, y)

  rss <- sum(fit$residuals^2)
  tss <- sum((y - mean(y))^2)

  if (tss == 0) return(NA_real_)

  sigma2 <- rss / (prep$n - prep$p)

  XtX_inv <- tryCatch(
    solve(crossprod(X)),
    error = function(e) NULL
  )

  if (is.null(XtX_inv)) return(NA_real_)

  vcov_mat <- sigma2 * XtX_inv

  r2 <- 1 - rss / tss

  adj_r2 <- 1 - (1 - r2) * (prep$n - 1) / (prep$n - prep$p)

  coef_est <- fit$coefficients

  pred0 <- prep$X0 %*% coef_est
  pred1 <- prep$X1 %*% coef_est

  diff_pred <- as.vector(pred1 - pred0)

  se_diff <- sqrt(rowSums((prep$D %*% vcov_mat) * prep$D))

  valid <- is.finite(diff_pred) &
    is.finite(se_diff) &
    se_diff > 0

  if (!any(valid)) return(NA_real_)

  rmse <- sqrt(mean((diff_pred[valid] / se_diff[valid])^2))
  weighted_rmse <- rmse * adj_r2

  return(weighted_rmse)
}


### Permutation Test Wrapper ###
PermuteWrapperFast <- function(df, n_perm = 500,
                               parallel = TRUE,
                               workers = 2,
                               seed = 123,
                               len_x = 100) {

  df_split <- split(df, df$ID)

  run_one_id <- function(one_df, id_name) {
    prep <- PreparePermutationData(one_df, len_x = len_x)

    vals <- replicate(n_perm, PermutationTestFast(prep))

    tibble::tibble(
      ID = id_name,
      perm_id = seq_len(n_perm),
      perm_value = vals
    )
  }

  if (parallel) {
    old_plan <- future::plan()

    on.exit(
      future::plan(old_plan),
      add = TRUE
    )

    future::plan(
      future::multisession,
      workers = workers
    )

    out <- furrr::future_imap_dfr(
      df_split,
      run_one_id,
      .options = furrr::furrr_options(seed = seed)
    )
  } else {
    out <- purrr::imap_dfr(
      df_split,
      run_one_id
    )
  }
  out
}
