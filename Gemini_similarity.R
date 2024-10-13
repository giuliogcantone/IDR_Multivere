pacman::p_load(
  tidyverse
)

pak::pak("jhk0530/gemini.R")

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

###

prompt = 
read_file(
  "Prompt.txt")

responses_Gemini = list()
symmetric_list_Gemini = c(1:100)

for (i in symmetric_list_Gemini) {

  v_matrix <- gemini.R::gemini(
    prompt = prompt,
    model = "1.5-pro",
    temperature = 0,
    maxOutputTokens = 5000
  )
  
  v_matrix -> responses_Gemini[[i]]
  
  v_matrix %>%
    str_remove_all("```r") |>
    str_remove_all("```R") |>
    str_remove_all("\\r") |>
    str_remove_all("```") %>%
    parse(text = .) %>%
    eval() |>
    isSymmetric() -> symmetric
  
  if (symmetric == T) {
    setdiff(
      symmetric_list_Gemini,i
    ) -> symmetric_list_Gemini}
  
  rm(symmetric)
  print(i)
}

Gemini = tibble(
  i = character(0),
  j = character(0),
  c = numeric(0)
)

for (t in 1:100){
  
  responses_Gemini[[t]] %>%
    str_remove_all("```r") |>
    str_remove_all("```R") |>
    str_remove_all("\\r") |>
    str_remove_all("```") %>%
    parse(text = .) %>%
    eval() %>%
    as.tibble(rownames = "i") %>%
    pivot_longer(-i, names_to = "j", values_to = "c") %>%
  add_row(Gemini,.) -> Gemini
  
rm(t)
}

Gemini |>
  summarise(
    .by = c(i,j),
    c = mean(c)
  ) |>
  mutate(display_name = i) |>
  rename_disc("i") |>
  mutate(display_name = j) |>
  rename_disc("j") |>
  select(-display_name) -> Gemini

Gemini |>
  writexl::write_xlsx("Gemini_similarity.xlsx")

Gemini |>
  arrange(i,c) |>
  pivot_wider(names_from = j,
              values_from = c,
              values_fill = 0
  ) %>%
  column_to_rownames(var = "i") %>%
  select(sort(names(.))) %>%
  as.matrix() |>
  pheatmap(clustering_distance_rows = "euclidean",  # Metodo di distanza per righe
           clustering_distance_cols = "euclidean",  # Metodo di distanza per colonne (stesso metodo)
           clustering_method = "complete",          # Metodo di clustering
           treeheight_row = 0,                     # Altezza del dendrogramma delle righe
           treeheight_col = 10,                     # Altezza del dendrogramma delle colonne
           show_rownames = TRUE,                    # Mostra nomi righe
           show_colnames = TRUE,                    # Mostra nomi colonne,
           legend = FALSE,
           angle_col = 0,
           display_numbers = T,
           number_color = "black",
           color = colorRampPalette(c("azure",
                                      "darkorange"))(100)
  )
