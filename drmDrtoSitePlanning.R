setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()
if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr", "purrr", "stringr")

#DRM Site list that Jenni sent
drmSites=read.csv("C:/Users/Lexie.Sturm/Documents/GitHub/drmDrtoSitePlanning/Data/DRM_DRTO_2024_CoralTeamList_SheddCompleted_grid.csv")

#All NCRMP fish sites in DRTO
ncrmpFishSites=read.csv("C:/Users/Lexie.Sturm/Documents/GitHub/drmDrtoSitePlanning/Data/dt24_fish_samplist.csv")

#All NCRMP benthic sites in DRTO
ncrmpBenthicSites=read.csv("C:/Users/Lexie.Sturm/Documents/GitHub/drmDrtoSitePlanning/Data/dt24_benthic_samplist.csv")

#Name the sites "Benthic"
ncrmpBenthicSites <- ncrmpBenthicSites %>%
  mutate(Source = "Benthic")

#Name the sites "Fish"
ncrmpFishSites <- ncrmpFishSites %>%
  mutate(Source = "Fish")

# Get the column names excluding "Source"
common_columns <- intersect(colnames(ncrmpBenthicSites), colnames(ncrmpFishSites))
common_columns <- setdiff(common_columns, "Source")

# Perform the full join on the common columns, except for source
ncrmpAllSites <- ncrmpBenthicSites %>%
  full_join(ncrmpFishSites, by = common_columns)

# View the first few rows of the result to check
head(ncrmpAllSites)

#We will call the sites by if it is only a fish site, benthic site, or both
ncrmpAllSites <- ncrmpAllSites %>%
  mutate(survey_type = case_when(
    is.na(Source.x) & !is.na(Source.y) ~ "Fish",
    !is.na(Source.x) & is.na(Source.y) ~ "Benthic",
    !is.na(Source.x) & !is.na(Source.y) ~ "Fish & Benthic",
    TRUE ~ NA_character_  # This handles any unexpected cases
  )) %>%
  # Remove the Source.x and Source.y columns
  dplyr::select(-Source.x, -Source.y)

# Rename the columns of the DRM df to match NCRMP
drmSites <- drmSites %>%
  rename(
    mapgrid_nr = grid_id_50,
    lat_degrees = Latitude,
    lon_degrees = Longitude,
    habitat_cd = habclass,
    depth_class = depth_clas,
    rugosity_class = rugosity_c,
    subregion_nr = subreg_nr 
  )

# Join the dataframes by mapgrid_nr
allDrtoSites <- drmSites %>%
  full_join(ncrmpAllSites, by = c("mapgrid_nr"))
#Not many sites share a mapgrid_nr...

#"lat_degrees", "lon_degrees", "mpa_nr", "subregion_nr", "habitat_cd", "psu_depth"

# Get column names with suffixes
x_columns <- grep("\\.x$", colnames(allDrtoSites), value = TRUE)
y_columns <- grep("\\.y$", colnames(allDrtoSites), value = TRUE)

# Reorder columns so all the same columns from the parent dfs are next to each other so we can make sure all the info matches up
# For each `.x` column, find the corresponding `.y` column and rearrange them
new_order <- c()
for (x_col in x_columns) {
  # Append `.x` column
  new_order <- c(new_order, x_col)
  
  # Find the corresponding `.y` column
  y_col <- sub("\\.x$", ".y", x_col)
  if (y_col %in% y_columns) {
    # Append the `.y` column
    new_order <- c(new_order, y_col)
  }
}

# Append any remaining columns not included in the reordering
remaining_columns <- setdiff(colnames(allDrtoSites), new_order)
new_order <- c(new_order, remaining_columns)

# Apply the new order to the dataframe and perform additional reordering
allDrtoSites <- allDrtoSites %>%
  select(all_of(new_order)) %>%
  select(DRM_ID, psu_id, everything()) %>%
  relocate(psu_id, .after = DRM_ID)

# View the first few rows to check the result
head(allDrtoSites)

write.csv(allDrtoSites, file = "./Data/allDrtoSites.csv", row.names = FALSE)

# Filter rows where both DRM_ID and psu_id have non-missing values, these are sites with the same mapgrid_nr in common
drtoCommonSites <- allDrtoSites %>%
  filter(!is.na(DRM_ID) & !is.na(psu_id))

# Filter rows where Status is "Not Surveyed," these are new sites
newDrtoCommonSites <- drtoCommonSites %>%
  filter(Status == "Not surveyed")

# Clean up the df, Remove columns with .y suffix
newDrtoCommonSites <- newDrtoCommonSites %>%
  select(-ends_with(".y"))

# Clean up the df, Rename columns with .x suffix by removing the suffix
newDrtoCommonSites <- newDrtoCommonSites %>%
  rename_with(~ str_remove(., "\\.x$")) %>%
  select(-lat_deg, -lon_deg, -psu_vrel, -Shape_Leng, -Shape_Area) #Remove extraneous columns

# View the first few rows to check the result
head(newDrtoCommonSites)

write.csv(newDrtoCommonSites, file = "./Data/newDrtoCommonSites.csv", row.names = FALSE)

################################################################################
#All DRM Sites
# Filter rows where both DRM_ID and psu_id have non-missing values, these are sites with the same mapgrid_nr in common
drmDrtoSites <- allDrtoSites %>%
  filter(!is.na(DRM_ID))

# Filter rows where Status is "Not Surveyed," these are new sites
drmDrtoSites <- drmDrtoSites %>%
  filter(Status == "Not surveyed")

# Clean up the df, Remove columns with .y suffix
drmDrtoSites <- drmDrtoSites %>%
  select(-ends_with(".y"))

# Clean up the df, Rename columns with .x suffix by removing the suffix
drmDrtoSites <- drmDrtoSites %>%
  rename_with(~ str_remove(., "\\.x$")) %>%
  select(-lat_deg, -lon_deg, -psu_vrel, -Shape_Leng, -Shape_Area) #Remove extraneous columns

# View the first few rows to check the result
head(drmDrtoSites)

write.csv(drmDrtoSites, file = "./Data/drmDrtoSites.csv", row.names = FALSE)

