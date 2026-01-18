# Custom function
cal_EDI<-function(AA_Hb,Ek=0.15,MWaa=71,Vd=0.38,K=4.4e-6,el=120){
  # AA_Hb (pmol/g Hb)
  # Ek (0.15/h)
  # MWaa (71g/mol)
  # Vd (0.38 L/kg)
  # K (4.4×10-6 L/g Hb/h)
  # el (erythrocyte lifespan, 120 days)
  EDI=((AA_Hb*Ek*MWaa*Vd)/(K*el*0.5))*1E-6 # Unit conversion
  return(EDI)
}
cal_EDIG<-function(EDI,N){
  EDIG<-sum(EDI*N)/sum(N)
}

# R package loading
if(!require(readxl)){install.packages("readxl")}
if(!require(dplyr)){install.packages("dplyr")}
library(readxl)
library(dplyr)


# Construct desktop path (automatic system recognition）
desktop_path <- file.path(Sys.getenv("USERPROFILE"), "Desktop")
# Read files
df <- read_excel(file.path(desktop_path, "DATA0118.xlsx"), sheet = 1)

# Calculate EDI
df$EDI<-cal_EDI(df$`AA_Hb`)
write.csv(df, "Result1.csv",fileEncoding="GBK")

# Group calculation EDIG
EDIG<-df %>% 
  filter(`subgroup`!="adult(General population)") %>% 
  group_by(`subgroup`,country) %>% 
  summarise(
    total_studies=n(),
    total_samples=sum(`n`),
    EDIG=cal_EDIG(EDI,`n`),
    .groups = "drop"
  )
write.csv(EDIG, "Result2.csv",fileEncoding="GBK")

# General population
# only_country<-df[-c(1,17,21,38,65),] %>% 
only_country<-df[-c(2,3,18,19,22,23,39,40,66,67),] %>% 
  group_by(country) %>% 
  summarise(
    total_studies=n(),
    total_samples=sum(`n`),
    EDIG=cal_EDIG(EDI,`n`),
    .groups = "drop"
  ) %>% 
  arrange(desc(EDIG)) 

write.csv(only_country, "Result3.csv",fileEncoding="GBK")
