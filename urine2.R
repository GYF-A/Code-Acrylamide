#############################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
setwd(desktop_path)  # Set the working directory to the desktop.

urine1 <- read_excel("DATA0105.xlsx", sheet = "urine_AAMA_GAMA")
urine2 <- read_excel("DATA0105.xlsx", sheet = "urine_AAMA_only")


#############################################################################
{
  # Define constant parameters
  MW_AA <- 71.0
  MW_AAMA <- 234.1
  MW_GAMA <- 246.0    
  ##Adult
  adultp <- list(
    V24h = 1.65, 
    Cre24h = 1.48,
    BW = 66.5  
  )
  
  ## Children
  childp <- list(
    V24h = 0.66,
    Cre24h = 0.50,
    BW = 33.0
  )
}

# Define EDI calculation function
## Calculate EDI (Equation 2 - 3) based on AAMA + GAMA
edi_AG <- function(data, FUE = 0.5) {
  # Combine AAMA and GAMA into a single row.
  combined_data <- data %>%
    group_by(study_id, year, country, subgroup) %>%
    mutate(
      has_AAMA = any(biomarker == "AAMA"),
      has_GAMA = any(biomarker == "GAMA"),
      n = first(n)  
    ) %>%
    pivot_wider(
      id_cols = c(study_id, year, country, subgroup, n),
      names_from = biomarker,
      values_from = c(value, unit),
      names_sep = "_"
    ) %>%
    rename(
      AAMA_value = value_AAMA,
      GAMA_value = value_GAMA,
      AAMA_unit = unit_AAMA,
      GAMA_unit = unit_GAMA
    ) %>%
    ungroup()
  
  results <- combined_data %>%
    mutate(
      
      is_child = grepl("child|children", tolower(subgroup), ignore.case = TRUE),
      
      # Get parameters
      V24h = ifelse(is_child, childp$V24h, adultp$V24h),
      Cre24h = ifelse(is_child, childp$Cre24h, adultp$Cre24h),
      BW = ifelse(is_child, childp$BW, adultp$BW),
      
      # Calculate∑UAAM
      UAAM = AAMA_value / MW_AAMA + GAMA_value / MW_GAMA,
      
      
      # Calculate EDI
      EDI = case_when(
        AAMA_unit == "μg/g" & GAMA_unit == "μg/g" ~ 
          (UAAM * Cre24h * MW_AA) / (FUE * BW),
        
        AAMA_unit == "ng/mL" & GAMA_unit == "ng/mL" ~ 
          (UAAM * V24h * MW_AA) / (FUE * BW)
      )
    )
  return(results)
}

# Calculate EDI based on AAMA (Equation 4)
edi_single <- function(data, FUE = 0.5) {
  results <- data %>%
    mutate(
      
      is_child = grepl("child|children", tolower(subgroup), ignore.case = TRUE),
      
      Cre24h = ifelse(is_child, childp$Cre24h, adultp$Cre24h),
      V24h = ifelse(is_child, childp$V24h, adultp$V24h),
      BW = ifelse(is_child, childp$BW, adultp$BW),
      
      AAMA_cre_numeric = as.numeric(AAMA_cre),
      
      # Select the formula according to the unit.
      EDI = case_when(
        # When the unit is μg/g, use Cre24h.
        unit == "μg/g" ~ 
          (AAMA_cre_numeric * Cre24h * MW_AA) / (MW_AAMA * FUE * BW),
        
        # When the unit is ng/mL, replace Cre24h with V24h.
        unit == "ng/mL" ~ 
          (AAMA_cre_numeric * V24h * MW_AA) / (MW_AAMA * FUE * BW)
      )
    )
  
  return(results)
}


#############################################################################
# Define the FUE value
FUE <- 0.5

# result
results <- list()
edi1_all   <- list()
edi2_all   <- list()

# Calculate the EDI of the two types.
edi1 <- edi_AG(urine1, FUE)
edi2 <- edi_single(urine2, FUE)

# Extraction and combination
combined <- edi1 %>%
  # select(country, subgroup, EDI, n) %>%
  select(study_id,year,country,subgroup,EDI, n) %>%
  filter(!is.na(EDI))

single <- edi2 %>%
  # select(country, subgroup, EDI, n) %>%
  select(study_id,year,country,subgroup,EDI, n) %>%
  filter(!is.na(EDI))

edi1_all[[as.character(FUE)]] <- combined
edi2_all[[as.character(FUE)]] <- single

all_data <- bind_rows(combined, single)

edi_c<-all_data %>% 
  group_by(country,subgroup) %>% 
  summarise(EDIG=sum(EDI*n)/sum(n))
write.csv(edi_c,"edi3_all .csv",fileEncoding = "GBK")
