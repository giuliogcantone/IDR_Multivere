pacman::p_load(tidyverse,broom,readxl)

readxl::read_xlsx(
  "Big_Five_Econ/Econ_papers.xlsx"
) -> db

db |>
  dplyr::select(
    y_5,
    sem_Both_k_0_0,
    org_Both_k_0_0,
    cog_Both_k_0_0
  ) |>
  cor(use = "complete.obs")

db |>
  dplyr::select(
    y_5,
    sem_RSLC_0_RR_0,
    org_RSLC_0_RR_0,
    cog_RSLC_0_RR_0
  ) |>
  cor(use = "complete.obs")

db |>
  dplyr::select(
    y_5,
    sem_RSLC_k_RR_zeta,
    org_RSLC_k_RR_zeta,
    cog_RSLC_k_RR_zeta
  ) |>
  cor(use = "complete.obs")

db |>
  dplyr::select(
    y_5,
    sem_LWB_k_Gini_zetaLWB,
    org_LWB_k_Gini_zetaLWB,
    cog_LWB_k_Gini_zetaLWB
  ) |>
  cor(use = "complete.obs")

###

db |>
  mutate(y_3 = ifelse(y_3 |> is.na(),0,y_3),
         y_5 = ifelse(y_5 |> is.na(),0,y_5)) %>%
  dplyr::mutate(dplyr::across(
    .cols = dplyr::matches("^sem_|^cog_|^org_"),
    .fns = ~ as.numeric(scale(.))
  )) %>%
specr::setup(
  data = .,
  y = c("y_3","y_5","y_10"),
  x = db %>%
    dplyr::select(starts_with("sem_") | starts_with("cog_") | starts_with("org_")) %>%
    names(),
  model = c("lm")
) |>
  specr::specr() |>
  as.tibble() -> multiverse

multiverse$estimate

multiverse |>
  mutate(
    signi =
      case_when(
        p.value < .05 & estimate > 0 ~ "green",
        p.value < .05 & estimate < 0 ~ "red",
        p.value > .05 ~ "grey"
      )) |>
  ggplot() +
  geom_density(aes(x = estimate,
                   color = y)) +
  geom_vline(color = "purple", xintercept = 0,)

multiverse |>
  mutate(
    paradigm =
      case_when(
        formula |> str_detect("sem_") ~ "Semantic",
        formula |> str_detect("org_") ~ "Organic",
        formula |> str_detect("cog_") ~ "Cogni"
      )) |>
  ggplot() +
  geom_density(aes(x = estimate,
                   color = paradigm)) +
  geom_vline(color = "purple", xintercept = 0,)

multiverse$formula |> str_match("sem_")

