pacman::p_load(
  tidyverse,openalexR,ggheatmap,
  pheatmap
)

rename_disc <- function(x, l) {
  x |>
    mutate(
      !!sym(l) := recode(display_name,
                         `Computer science` = "Comp. Sci.",
                         `Political science` = "Law & Pol.",
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

oa_fetch(
  entity = "concepts",
  level = 0
) |>
  rename_disc("i") -> concepts

concepts %>%
  select(i,related_concepts) %>%
  unnest(related_concepts,
         names_sep = "_") %>%
  filter(related_concepts_level == 0) |>
  rename(display_name = related_concepts_display_name) |>
  rename_disc("j") |>
  select(i,j, c = related_concepts_score) -> Z

crossing(i = concepts$i,
         j = concepts$i) %>% as_tibble() %>%
  arrange(i,j) %>%
  left_join(Z, by = c("i", "j"))|>
  mutate(c = ifelse(is.na(c),0,c)) |>
  select(i,j,c) |>
  pivot_wider(names_from = j,
              values_from = c,
              values_fill = 0
  ) %>%
  column_to_rownames(var = "i") %>%
  select(sort(names(.))) %>%
  as.matrix() -> Z_matrix

Ochiai = Z_matrix

for (i in 1:nrow(Z_matrix)) {
  for (j in 1:ncol(Z_matrix)) {
    nom <- Z_matrix[i, j] + Z_matrix[j, i]  # Somma della cella (i,j) e (j,i)
    
    # Denominatore come specificato
    denom <- sqrt(
      sum(Z_matrix[i, ]) + sum(Z_matrix[, i]) *
        sum(Z_matrix[j, ]) + sum(Z_matrix[, j])
    )
    
    # Popola la nuova matrice con il risultato
    Ochiai[i, j] <- nom / denom
  }
}

diag(Ochiai) <- 1

pheatmap(Ochiai,
         clustering_distance_rows = "euclidean",  # Metodo di distanza per righe
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
         color = colorRampPalette(c
                                  ("azure",
                                    "darkorange"))(100)
         )

Ochiai |> as.data.frame() %>%
  rownames_to_column(var = "i") %>%
  pivot_longer(-i, names_to = "j", values_to = "Valore") |>
  writexl::write_xlsx("Ochiai_similarity.xlsx")
