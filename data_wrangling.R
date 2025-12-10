# Run setup script
source("setup.R")

# Load RGI7 dataframes
 RGI13 <- read.csv("C:/Users/hnbor/Desktop/Research/RGI7_data/RGI2000-v7.0-G-13_central_asia/RGI2000-v7.0-G-13_central_asia-attributes.csv")
 RGI14 <- read.csv("C:/Users/hnbor/Desktop/Research/RGI7_data/RGI2000-v7.0-G-14_south_asia_west/RGI2000-v7.0-G-14_south_asia_west-attributes.csv")
 RGI15 <- read.csv("C:/Users/hnbor/Desktop/Research/RGI7_data/RGI2000-v7.0-G-15_south_asia_east/RGI2000-v7.0-G-15_south_asia_east-attributes.csv")
# 
# # Join tables
 RGIcombo <- rbind(RGI13, RGI14, RGI15)
 
# # Clean data
 sub <- c("glims_id", "subm_id", "src_date", "primeclass", "subm_id", "src_date")
# 
 FullRGI_cleaned <- RGIcombo %>% select(-sub)

#write.csv(FullRGI_cleaned, "C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523-Final-Project/data/RGIcleaned.csv")
#RGIdf <- read.csv("C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523A_FinalProject2/HMA_surge_glaciers/data/RGIcleaned.csv")
RGIdfcomplete <- FullRGI_cleaned %>%
  mutate(
    glac_name = case_when(
      # Condition 1: Check for NA (explicit missing value)
      is.na(glac_name) ~ "Unnamed",
      # Condition 2: Check for empty string ""
      glac_name == "" ~ "Unnamed",
      # Default: Keep the original value if neither condition is met
      TRUE ~ glac_name
    )
  )

#write.csv(RGIdfcomplete, "C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523A_FinalProject2/HMA_surge_glaciers/data/RGIcleaned.csv")
df<-read.csv("C:/Users/hnbor/Desktop/Environmental Data Science Applications/GitProjects/523A_FinalProject2/HMA_surge_glaciers/data/RGIcleaned.csv")
