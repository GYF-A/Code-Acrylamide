#############################################################################
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(openxlsx)

desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
setwd(desktop_path)  # Set the working directory to the desktop.

urine1 <- read_excel("DATA0114.xlsx", sheet = "urine_AAMA_GAMA")
urine2 <- read_excel("DATA0114.xlsx", sheet = "urine_AAMA_only")


#############################################################################
{
  # Define constant parameters
  MW_AA <- 71.0
  MW_AAMA <- 234.1
  MW_GAMA <- 246.0   
  
  ## Adult
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
## Calculate EDI  based on AAMA + GAMA (Equation 2 - 3)
edi_AG <- function(data, FUE = 0.5) {
  # Merge AAMA and GAMA into one line.
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
      
      # Calculate ∑UAAM
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
# Define FUE value
FUEv <- c(0.6)

# Result
results <- list()
edi1_all   <- list()
edi2_all   <- list()

# Calculate each FUE value
# Under different FUE conditions → By study → By country → Calculation of wEDI weighted by sample size
for (FUE in FUEv) {
  # Calculate the EDI of the two types.
  edi1 <- edi_AG(urine1, FUE)
  edi2 <- edi_single(urine2, FUE)
  
  # Extraction and combination
  combined <- edi1 %>%
    select(country, EDI, n) %>%
    filter(!is.na(EDI))
  
  single <- edi2 %>%
    select(country, EDI, n) %>%
    filter(!is.na(EDI))
  
  edi1_all[[as.character(FUE)]] <- combined
  edi2_all[[as.character(FUE)]] <- single
  
  all_data <- bind_rows(combined, single)
  
  # Calculate the weighted average EDI by country (the weight is the sample size n)
  country_edi <- all_data %>%
    group_by(country) %>%
    summarize(
      weighted_edi = weighted.mean(EDI, w = n, na.rm = TRUE),
      n_total = sum(n, na.rm = TRUE),
      n_studies = n(),
      .groups = 'drop'
    )
  
  # Result
  results[[as.character(FUE)]] <- country_edi
  
  cat(sprintf("\n FUE = %.1f \n", FUE))
}

results
edi1_all
edi2_all


#############################################################################
# After sorting
plot_data <- read_excel("Sensitivity analysis.xlsx", sheet = 1)
RfD <- 2.0 

# 10.2 Bar chart
windowsFonts(Times = windowsFont("Times New Roman"))

p1 <- ggplot(plot_data, aes(x = reorder(country, weighted_edi), 
                            y = weighted_edi, fill = factor(FUE))) +
  geom_bar(stat = "identity",  position = position_dodge(width = 0.7), width = 0.6) +
  geom_hline(yintercept = RfD, color = "red", linetype = "dashed", size = 0.8) +
  labs(
    x = "Country",
    y = "EDI (μg/kg bw/day)",
    fill = "FUE"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    text = element_text(family = "Times"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    axis.text.x = element_text(angle = 0, hjust = 1, vjust = 0.5, size = 12),
    axis.text.y = element_text(size = 12, face = "bold"), 
    axis.title = element_text(size = 12, face = "bold"),
    legend.position = "right",
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "gray50", fill = NA, size = 0.5)
  ) +
  scale_fill_manual(values = c("0.4" = "#67a9cf", 
                               "0.5" = "#1c9099", 
                               "0.6" = "#016c59")) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05)))

p2 <- p1 + annotate("text", 
                    x = length(unique(plot_data$country)) * 0.5,
                    y = RfD * 0.95,
                    label = paste("Global estimated daily intake of ", round(RfD, 3), "μg/kg bw/day"),
                    color = "red", size = 2.8, family = "times",
                    angle = 90,
                    hjust = 0.5,
                    vjust = 0.5,
                    lineheight = 0.9)

print(p2)
tiff("plot.tiff", width = 2000, height = 1500, res = 300)  
print(p2)
dev.off()

