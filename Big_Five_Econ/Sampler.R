pacman::p_load(
  tidyverse, openalexR
)

rename_disc <- function(x, l) {
  x |>
    mutate(
      !!sym(l) := recode(display_name,
                         `Computer science` = "Comp. Sci.",
                         `Computer Science` = "Comp. Sci.",
                         `Political science` = "Law & Pol.",
                         `Law & Political Science` = "Law & Pol.",
                         Mathematics = "Math.",
                         `Materials science` = "Materials Sci.",
                         `Environmental science` = "Environ. Sci.",
                         Philosophy = "Phil.",
                         Psychology = "Psych.",
                         Sociology = "Sociol.",
                         Economics = "Econ.",
                         Geography = "Geogr.",
                         Chemistry = "Chem."
                         
      )
    ) |> 
    filter(
      ! (!!sym(l) %in% c("Engineering", "Art", "Business",
                         "Environ. Sci.", "Materials Sci.",
                         "Geogr.","Geography"))
    )
}

big_five <- c("0002-8282", "0033-5533", "0022-3808",
              "0012-9682", "0034-6527")

business <- c("0022-1082", "0148-2963", "0048-7333",
              "0047-2506", "0276-7783")

organisation <- c("0143-2095", "0025-1909", "1047-7039",
                  "0272-6963", "0361-3682")

openalexR::oa_fetch(
  entity = "sources",
  issn = big_five
) -> big_five

openalexR::oa_fetch(
  entity = "sources",
  issn = business
) -> business

openalexR::oa_fetch(
  entity = "sources",
  issn = organisation
) -> organisation

openalexR::oa_fetch(
  entity = "works",
  primary_location.source.id = big_five$id,
  publication_year = c("2013","2018")
) |>
  mutate(
    group = "Big Five"
  ) |>
  add_row(
    openalexR::oa_fetch(
      entity = "works",
      primary_location.source.id = business$id,
      publication_year = c("2013","2018")
    ) |>
      mutate(
        group = "Business"
      )
  ) |>
  add_row(
    openalexR::oa_fetch(
      entity = "works",
      primary_location.source.id = organisation$id,
      publication_year = c("2013","2018")
    ) |>
      mutate(
        group = "Organisation"
      )
  ) -> papers

papers |>
filter(!author %>% is.na(),
           !publication_date %>% is.na(),
           !referenced_works %>% is.na(),
           !doi %>% is.na()) %>%
  transmute(paper = id,
            title = display_name %>% tolower(),
            author,
            journal = so,
            group,
            publication_date = publication_date %>% as.Date(),
            pub_year = publication_year,
            counts_by_year,
            concepts,
            refs = referenced_works
  ) %>%
  arrange(publication_date) %>%
  filter(title %>% str_detect("erratum", negate = T),
         title %>% str_detect("editor", negate = T),
         title %>% str_detect("corrige", negate = T),
         title %>% str_detect("publisher correction", negate = T),
         title %>% str_detect("book review", negate = T),
         title %>% str_detect("isbn", negate = T)
  ) %>%
  distinct(title, .keep_all = T) -> papers

papers %>%
  unnest(author) %>%
  rename(author = au_display_name) %>%
  filter(!author %>% is.na()) %>%
  count(title) %>%
  rename(n_authors = n) %>%
  right_join(papers) %>%
  mutate(n_authors_class =
           case_when(n_authors <= 3 ~ n_authors %>% as.character(),
                     n_authors > 3 ~ "4+") %>%
           factor(levels = c("1","2","3","4+")),
         n_authors_ordinal = 
           case_when(n_authors < 4 ~ n_authors %>% as.integer(),
                     n_authors >= 4 ~ 4)
  ) -> papers

papers |>
  select(paper,counts_by_year,pub_year) |>
  unnest(counts_by_year) |>
  reframe(
    y_3 = case_when(
      pub_year == 2013 ~ sum(cited_by_count[year <= 2016]),
      pub_year == 2018 ~ sum(cited_by_count[year <= 2021])
    ),
    y_5 = case_when(
      pub_year == 2013 ~ sum(cited_by_count[year <= 2018]),
      pub_year == 2018 ~ sum(cited_by_count[year <= 2023])
    ),
    y_10 = case_when(
      pub_year == 2013 ~ sum(cited_by_count[year <= 2023]),
      TRUE ~ NA
    ),
    .by = paper
  ) |>
  distinct() |>
  right_join(papers) -> papers

papers %>%
  unnest(concepts) %>%
  filter(level == 0,
         score>0) |>
  select(-id,-wikidata,-level) |>
  rename_disc("i") |>
  select(paper) |>
  distinct() |>
  left_join(papers) -> papers

### Sampling references

papers |>
  select(refs) |>
  unnest(refs) |>
  distinct() -> refs_id

papers |>
  select(paper,refs) |>
  unnest(refs) |>
  count(paper) |> View()

oa_fetch(
  entity = "works",
  id = refs_id$refs
)

###
  View()
