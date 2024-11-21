pacman::p_load(
  tidyverse, openalexR
)

papers[1:2,] -> a

a %>%
  unnest(concepts) %>%
  filter(level == 0,
         score>0) |>
  select(-id,-wikidata,-level) |>
  rename_disc("i") |>
  mutate(
    p = score/sum(score),
    .by = paper
  ) |>
  select(paper,i,p) -> b

b |> inner_join(b, by = "paper") %>%
  rename(i = i.x, p_i = p.x,
         j = i.y, p_j = p.y) |>
  left_join(
    readxl::read_xlsx("Ochiai_similarity.xlsx")) |>
  summarise(
    .by = paper,
    sem_q2 = 1 / (1 - (sum(p_i*p_j*(1-z_Ochiai)))),
    sem_kzeta = sum(p_i/sum(p_j*z_Ochiai)),
    sum_zetaLWB = sum(1 - z_Ochiai)
  ) -> rs
  
b |>
  summarise(
    sem_k = n(),
    sem_RR = 1-sum(p^2),
    sem_Balance = 1/(k*(1-RR)),
    sem_compleGini = 1- ineq::Gini(p),
    .by = paper,
  ) |>
  left_join(rs) |>
  mutate(
    sem_zeta = sem_kzeta/sem_k,
    sem_delta = sem_q2 / sem_k,
    sem_zetaLWB = sum_zetaLWB / (sem_k * (sem_k-1)),
  ) |>
  select(-c(sum_zetaLWB)) |>
  View()
