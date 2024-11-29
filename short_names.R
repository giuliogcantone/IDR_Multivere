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


abbreviations_concepts <- function(x,i) {
  column_sym <- ensym(i)
  
  x |>
    mutate(
      !!column_sym :=
        case_match(
          !!column_sym,
          "Art" = "Art",
          "Biology" = "Biol.",
          "Business" = "Busin.",
          "Chemistry" ~ "Chem.",
          "Computer science" ~ "CompSci.",
          "Economics" ~ "Econom.",
          "Geography" ~ "Geogr.",
          "Geology" = "Geolog.",
          "History" = "Hist.",
          "Mathematics" ~ "Math.",
          "Materials science" ~ "MaterialSci.",
          "Medicine" = "Medic.",
          "Engineering" = "Eng",
          "Environmental science" ~ "EnvirSci.",
          "Philosophy" ~ "Phil.",
          "Physics" = "Phys.",
          "Political science" ~ "LawPol",
          "Psychology" ~ "Psych.",
          "Sociology" ~ "Sociol.",
        )
    )
}

openalexR::oa_fetch(
  entity = "concepts",
  level = 0
) -> a

a$display_name
