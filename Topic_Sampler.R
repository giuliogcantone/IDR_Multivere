pacman::p_load(
  tidyverse, openalexR
)
source("short_names.R", local = T)

###

topics =
  tibble(i = character(0),
         j = character(0))

for (t in 11:36) {

  httr::content(httr::GET(
    paste0("https://api.openalex.org/fields/",t))
    ) -> data
    
    tibble(
      i = data$display_name,
      j = sapply(data$siblings,
                 function(x) x$display_name)
    ) %>%
      add_row(topics,.) -> topics
}

topics |>
  mutate(
    simil = 1
  ) |>
  abbreviations(i) |>
  abbreviations(j) -> topics

expand_grid(i = unique(topics$i),
            j = unique(topics$j)) |>
  left_join(topics, by = c("i", "j")) %>%
  mutate(simil = ifelse(is.na(simil), 0, simil)) -> topics

topics |>
  arrange(i,j) |>
  pivot_wider(names_from = j,
              values_from = simil) %>%
  column_to_rownames(var = "i") %>%
  as.matrix() %>%
  { 
    diag(.) <- 1
    .
  } -> Z_matrix

### Ochiai

proxy::simil(Z_matrix, method = "cosine") |>
  as.matrix() |>
  View()

Z_matrix["ArtHum",]
Z_matrix["BioChemGen",]

proxyC::simil(Z_matrix,
              method = "dice") |>
  as.matrix() |>
  View()

proxy::pr_DB |>
  summary()

Ochiai = Z_matrix


matrix(
  c(0,0,0,1,1,0),
  nrow = 2
) |>
  proxy::simil("jaccard")


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
  