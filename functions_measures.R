pacman::p_load(
  tidyverse, openalexR,ineq
)
source("IDR_17_measures.R")
source("short_names.R")

### Topics Semantic IDR
papers %>%
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
  abbreviations(i) |>
  IDR17("Topics_sem") %>%
  left_join(
    papers,.
  ) -> papers


papers_og %>%
  transmute(paper = id, concepts) |>
  unnest(concepts) |>
  filter(level == 0) |> View()
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
  abbreviations(i) |>
  IDR17("Concepts_sem") %>%
  left_join(
    papers,.
  ) -> papers


### Organisational
authorlist |>
  transmute(author_id=id,topics) |>
  unnest(topics) |>
  filter(name=="field") |>
  transmute(author_id,i=display_name,count) |>
  abbreviations(i) |>
  summarise(.by = c(author_id,i),
            count = sum(count)) |>
  mutate(p = count/sum(count),
         .by = author_id) %>%
  select(-count) %>%
  left_join(
    relationship = "many-to-many",
    papers |>
      unnest(author) |>
      transmute(
        paper,author_id = au_id
      ),
    .
  ) |>
  summarise(
    .by = c(paper,i),
    p = sum(p, na.rm = T),
  ) |>
  mutate(
    p = p/sum(p, na.rm = T),
    .by = paper
  ) |>
    filter(
      !is.na(i)
    ) |>
  IDR17("org") %>%
  left_join(
    papers,.
  ) -> papers

### Cognitive
refs |>
  transmute(refs=id,topics) |>
  unnest(topics) |>
  filter(name == "field") |>
  select(-id,-i,-name) |>
  rename(i = display_name) |>
  mutate(
    p = score/sum(score),
    .by = refs
  ) |>
  transmute(refs,i,p) |>
  summarise(
    .by = c(refs,i),
    p = sum(p) |> round(5)
  ) |>
  abbreviations(i) %>%
  left_join(
    papers |>
    select(paper,refs) |>
      unnest(refs),.
  ) |>
  summarise(
    .by = c(paper,i),
    p = sum(p,na.rm = T)
  ) |>
  mutate(
    p = p/sum(p,na.rm = T),
    .by = paper
  )  %>%
  filter(
    !is.na(i)
  ) %>%
  IDR17("cog") %>%
  left_join(
    papers,.
  ) -> papers


papers %>%
  mutate(across(where(is.numeric),
                ~ replace(., is.nan(.),
                          0))) |>
  select(
    -c(counts_by_year,topics,refs)
  ) |>
  write_xlsx("Big_Five_Econ/Econ_papers_2.xlsx")
