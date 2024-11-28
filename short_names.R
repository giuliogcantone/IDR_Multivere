pacman::p_load(
  tidyverse,
  fastverse,
  readxl,
  writexl
)

abbreviations <- function(x,i) {
  column_sym <- ensym(i)
  
  x |>
    mutate(
      !!column_sym :=
        case_match(
          !!column_sym,
          "Agricultural and Biological Sciences" ~ "AgriBio",
          "Arts and Humanities" ~ "ArtHum",
          "Biochemistry, Genetics and Molecular Biology" ~ "BioChemGen",
          "Business, Management and Accounting" ~ "Business",
          "Chemical Engineering" ~ "ChemEng" ,
          "Chemistry" ~ "Chem.",
          "Computer Science" ~ "CompSci",
          "Decision Sciences" ~ "DecisionSci",
          "Earth and Planetary Sciences" ~ "EarthSci",
          "Economics, Econometrics and Finance" ~ "Econom.",
          "Energy" ~ "PhysEnergy",
          "Engineering" ~ "Engineer.",
          "Environmental Science" ~ "EnvirSci",
          "Immunology and Microbiology" ~ "MicroBio",
          "Materials Science" ~ "MaterialSci",
          "Mathematics" ~ "Math.",
          "Medicine" ~ "Medic.",
          "Neuroscience" ~ "NeuroSci",
          "Nursing" ~ "Nurs.",
          "Pharmacology, Toxicology and Pharmaceutics" ~ "Pharma.",
          "Physics and Astronomy" ~ "PhysAstro",
          "Psychology" ~ "Psych.",
          "Social Sciences" ~ "SocialSci",
          "Veterinary" ~ "Veterin.",
          "Dentistry" ~ "Dentist.",
          "Health Professions" ~ "HealthPro"
        )
    )
}