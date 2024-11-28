IDR17 <- function(input_data, prefix = "sem") {
  # Prima pipeline
  rs <- input_data |>
    inner_join(input_data, by = "paper") %>%
    rename(i = i.x, p_i = p.x,
           j = i.y, p_j = p.y) |>
    left_join(
      relationship = "many-to-many",
      readxl::read_xlsx("Scopus_cosine_similarity.xlsx")) |>
    mutate(
      .by = c(paper,i),
      wsum_z = sum(p_j*z),
    ) |>
    summarise(
      .by = paper, 
      rs = sum(p_i*p_j*(1-z)), 
      X_RSLC_k_RR_zeta = 1 / (1 - rs), 
      X_RSLC_k_0_zeta = sum(p_i/wsum_z), 
      sum_zetaLWB = sum(1 - z)
    ) -> rs
  
  result <- input_data |>
    summarise(
      X_Both_k_0_0 = n(),
      X_RSLC_k_RR_0 = 1-sum(p^2),
      max_Balance = 1-(X_Both_k_0_0 * ((1/X_Both_k_0_0)^2)), 
      X_RSLC_0_RR_0 = X_RSLC_k_RR_0 / max_Balance,
      X_LWB_0_Gini_0 = 1- ineq::Gini(p,corr=T),
      .by = paper,
    ) |>
    left_join(
      relationship = "many-to-many",
      rs) |>
    mutate(
      X_RSLC_k_0_zeta = X_RSLC_k_0_zeta / X_Both_k_0_0,
      X_RSLC_k_RR_zeta,
      X_RSLC_0_RR_0,
      X_RSLC_0_RR_zeta = X_RSLC_k_RR_zeta / X_Both_k_0_0,
      X_RSLC_0_0_zeta = X_RSLC_k_0_zeta / X_Both_k_0_0,
      X_LWB_0_Gini_0,
      X_LWB_0_0_zetaLWB = sum_zetaLWB / (X_Both_k_0_0 * (X_Both_k_0_0-1)),
      X_LWB_k_Gini_0 = X_Both_k_0_0 * X_LWB_0_Gini_0,
      X_Both_0_Gini_zeta = X_LWB_0_Gini_0 * X_RSLC_0_0_zeta,
      X_LWB_0_Gini_zetaLWB = X_LWB_0_Gini_0 * X_LWB_0_0_zetaLWB,
      X_Both_0_RR_zetaLWB = X_RSLC_0_RR_0 * X_LWB_0_0_zetaLWB,
      X_LWB_k_Gini_0 = X_Both_k_0_0 * X_LWB_0_Gini_0,
      X_Both_k_0_zetaLWB = X_Both_k_0_0 * X_LWB_0_0_zetaLWB,
      X_Both_k_Gini_zeta = X_Both_k_0_0 * X_Both_0_Gini_zeta,
      X_LWB_k_Gini_zetaLWB = X_LWB_k_Gini_0 * X_LWB_0_0_zetaLWB,
      X_Both_k_RR_zetaLWB = X_Both_k_0_0 * X_Both_0_RR_zetaLWB
    ) |>
    select(-c(sum_zetaLWB,
              rs,
              max_Balance)) |>
    rename_with(~ gsub("^X_",
                       paste0(prefix,"_"),
                       .),
                starts_with("X_")) |>
    mutate(across(where(is.numeric),
                  ~ replace(., is.nan(.),
                            0)))
}
