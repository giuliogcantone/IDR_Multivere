model_IDR <- function(formula, data) {
  library(fixest)
  fixest::fenegbin(
    fml = as.formula(paste0(formula, "+ offset(log(n_authors))")),
    data = data
  )
}



library(dplyr)

# Esempio di tibble
my_tibble <- tibble(
  sem_example1 = 1:3,
  cog_example2 = 4:6,
  org_example3 = 7:9,
  other_example = 10:12
)

# Standardizzare le variabili con prefissi specifici usando scale
standardized_tibble <- my_tibble %>%
  mutate(across(
    .cols = matches("^sem_|^cog_|^org_"),  # Colonne da selezionare
    .fns = ~ as.numeric(scale(.))         # Standardizzazione
  ))

# Mostrare il risultato
print(standardized_tibble)