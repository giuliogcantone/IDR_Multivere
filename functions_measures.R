pacman::p_load(
  tidyverse, openalexR,ineq
)


a %>%
  unnest(topics) |>
  filter(name == "field") |>
  select(-id,-i,-name) |>
  rename(i = display_name) |>
  mutate(
    p = score/sum(score),
    .by = paper
  ) |>
  select(paper,i,p) |>
  summarise(
    .by = c(paper,i),
    p = sum(p) |> round(5)
  ) |>
  abbreviations(i) -> b

b |> inner_join(b, by = "paper") %>%
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
    sem_rs = sum(p_i*p_j*(1-z)),
    sem_RSLC_k_RR_zeta = 1 / (1 - sem_rs),
    sem_RSLC_k_0_zeta = sum(p_i/wsum_z),
    sum_zetaLWB = sum(1 - z)
  ) -> rs
  
b |>
  summarise(
    sem_Both_k_0_0 = n(),
    sem_RSLC_k_RR_0 = 1-sum(p^2),
    max_Balance = 1-(sem_Both_k_0_0 * (1/sem_Both_k_0_0)^2), 
    sem_RSLC_0_RR_0 = sem_RSLC_k_RR_0 / max_Balance,
    sem_LWB_0_Gini_0 = 1- ineq::Gini(p,corr=T),
    .by = paper,
  ) |>
  left_join(
    relationship = "many-to-many",
    rs) |>
  mutate(
    sem_RSLC_k_0_zeta = sem_RSLC_k_0_zeta/sem_Both_k_0_0,
    sem_RSLC_k_RR_zeta,
    sem_RSLC_0_RR_0,
    sem_RSLC_0_RR_zeta = sem_RSLC_k_RR_zeta / sem_Both_k_0_0,
    sem_RSLC_0_0_zeta = sem_RSLC_k_0_zeta/sem_Both_k_0_0,
    sem_LWB_0_Gini_0,
    sem_LWB_0_0_zetaLWB = sum_zetaLWB / (sem_Both_k_0_0 *
                                           (sem_Both_k_0_0-1)),
    sem_LWB_k_Gini_0 = sem_Both_k_0_0 * sem_LWB_0_Gini_0,
    sem_Both_0_Gini_zeta = sem_LWB_0_Gini_0 * sem_RSLC_0_0_zeta,
    sem_LWB_0_Gini_zetaLWB = sem_LWB_0_Gini_0 * sem_LWB_0_0_zetaLWB,
    sem_Both_0_RR_zetaLWB = sem_RSLC_0_RR_0 * sem_LWB_0_0_zetaLWB,
    sem_LWB_k_Gini_0 = sem_Both_k_0_0 * sem_LWB_0_Gini_0,
    sem_Both_k_0_zetaLWB= sem_Both_k_0_0 * sem_LWB_0_0_zetaLWB,
    sem_Both_k_Gini_zeta = sem_Both_k_0_0 * sem_Both_0_Gini_zeta,
    sem_LWB_k_Gini_zetaLWB = sem_LWB_k_Gini_0 * sem_LWB_0_0_zetaLWB,
    sem_Both_k_RR_zetaLWB = sem_Both_k_0_0 * sem_Both_0_RR_zetaLWB
  ) |>
  select(-c(sum_zetaLWB,
            sem_rs,
            max_Balance)) |>
  View()


authorlist[1:100,] -> a

a |>
  transmute(author_id=id,topics) |>
  unnest(topics) |>
  filter(name=="field") |>
  transmute(author_id,i=display_name,count) |>
  abbreviations(i) |>
  summarise(.by = c(author_id,i),
            count = sum(count)) |>
  mutate(p_i = count/sum(count),
         .by = author_id) |> View()

