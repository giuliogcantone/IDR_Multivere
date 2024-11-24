pacman::p_load(
  tidyverse,
  fastverse,
  readxl,
  writexl
)
source("short_names.R", local = T)

###

read_xlsx(
  "Scopus_essential.xlsx"
) |>
  mutate(
    ISSN = coalesce(ISSN_print,ISSN_e),
  ) %>%
  select(ISSN, everything(),-c(ISSN_print,ISSN_e)) %>%
  pivot_longer(cols = 2:ncol(.), 
               names_to = "Field_1", 
               values_to = "Scopus_Field") |>
  select(-Field_1) |>
  filter(!Scopus_Field |> is.na()) %>%
  mutate(ISSN = str_sub(ISSN,
                        1, 4) %>% 
           paste0("-",
                  str_sub(ISSN, 5))
         ) -> Scopus_fields

### Structure

citations_classified |>
  head(1000) |>
  transmute(
    doi_citer,doi_cited,
    ISSN_citer = coalesce(ISSN_print_citer,ISSN_e_citer),
    ISSN_cited = coalesce(ISSN_print_cited,ISSN_e_cited)
  ) |>
  left_join(
    Scopus_fields |>
      rename(
        ISSN_citer = ISSN,
        Field_citer = Scopus_Field
      )
    ) |>
  left_join(
    Scopus_fields |>
      rename(
        ISSN_cited = ISSN,
        Field_cited = Scopus_Field
      )
  ) |>
  mutate(
    score = 1/n(),
    .by = c("doi_citer","doi_cited")
  ) |> View()


### FASTVERSE

setDT(citations_classified)
setDT(Scopus_fields)

citations_classified <- citations_classified[,.(doi_citer,
                                                doi_cited, 
                                           ISSN_citer = coalesce(
                                             ISSN_print_citer,
                                             ISSN_e_citer), 
                                           ISSN_cited = coalesce(
                                             ISSN_print_cited,
                                             ISSN_e_cited))]

# Convert Scopus_fields to data.tables for joining
scopus_citer <- as.data.table(Scopus_fields)[,
                                             .(ISSN,
                                               Field_citer = Scopus_Field)]
scopus_cited <- as.data.table(Scopus_fields)[,
                                             .(ISSN,
                                               Field_cited = Scopus_Field)]

# Perform many-to-many joins
setkey(citations_classified, ISSN_citer)
setkey(scopus_citer, ISSN)
citations_classified <- scopus_citer[citations_classified,
                                     allow.cartesian = T]

setnames(dt, "ISSN", "ISSN_citer")

setkey(citations_classified, ISSN_cited)
setkey(scopus_cited, ISSN)
citations_classified <- scopus_cited[citations_classified,
                                     allow.cartesian = T]

setnames(dt, "ISSN", "ISSN_cited")

citations_classified <- citations_classified[!is.na(Field_citer)]
citations_classified <- citations_classified[!is.na(Field_cited)]


# Add score calculation
citations_classified[, score := 1/.N,
       by = .(doi_citer, doi_cited)] -> citations_classified

citations_classified |>
  fgroup_by(Field_citer,Field_cited) |>
  fsummarise(
    score = fsum(score)
  ) -> Field_cross_citations

Field_cross_citations |>
  as_tibble()|>
  filter(
    !Field_citer |> is.na(),
    !Field_cited |> is.na()
  ) |>
  rename(
    i = Field_citer,
    j = Field_cited,
    c = score
  ) |>
  pivot_wider(names_from = j,
              values_from = c,
                values_fill = 0
  ) %>%
  write_xlsx("ScopusFields_citations_2023.xlsx")

###

readxl::read_xlsx("ScopusFields_citations_2023.xlsx") |>
  column_to_rownames(var = "i") %>% as.matrix() -> Z_matrix

Z_matrix |>
  proxy::simil("cosine") |>
  as.matrix() |>
  as.data.frame() %>%
  rownames_to_column(var = "Row") %>%
  pivot_longer(-Row, names_to = "Column", values_to = "z") %>%
  rename(i = Row, j = Column) |>
  abbreviations(i) |>
  abbreviations(j) |>
  mutate(z = ifelse(i==j,1,z)) |>
  write_xlsx("Scopus_cosine_similarity.xlsx")
  