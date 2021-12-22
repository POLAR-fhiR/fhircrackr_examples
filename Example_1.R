library(fhircrackr)
library(tidyr)
library(dplyr)

############################################
###   Step A: Compute BMI for Patients   ###
############################################

### 1 Define FHIR search request ###
request <- fhir_url(url = "https://mii-agiop-3p.life.uni-leipzig.de/fhir",
                                resource = "Observation",
                                parameters = c(
                                  "_include" = "Observation:patient",
                                  "code" = "http://loinc.org|3142-7, http://loinc.org|8302-2")
                    )



### 2 Download bundles ###
bundles <- fhir_search(request = request)

### 3 Define table descriptions ###

# Without specifying columns
observations <- fhir_table_description(resource = "Observation")

# With explizit column specification
patients <- fhir_table_description(
  resource = "Patient",
  cols = c(id = "id",
           gender = "gender",
           birthdate = "birthDate")
)

### 4 Flatten bundles ###
obs_tables <- fhir_crack(bundles = bundles, design = observations)
pat_tables <- fhir_crack(bundles = bundles, design = patients)

### 5 preprocess data ###

#Adjust data types
obs_tables$valueQuantity.value <- as.numeric(obs_tables$valueQuantity.value)

# Keep only highest measurement for each patient's body weight/body height 
obs_tables <- obs_tables %>% 
  group_by(subject.reference, code.coding.code) %>% 
  arrange(valueQuantity.value, .by_group = TRUE) %>%
  slice_tail() %>% 
  ungroup()

### 6 Compute BMI ###

# Only keep relevant variables
result <- obs_tables[, c("code.coding.code", "subject.reference", "valueQuantity.value")]

# spread body weight / body height across columns
result <- pivot_wider(result,  names_from = "code.coding.code", values_from = "valueQuantity.value")

#compute BMI
result$BMI <- result$`3142-7`/(result$`8302-2`/100)^2

#remove missing and unrealistically high values
result <- result[!is.na(result$BMI) & result$BMI < 150,]


##############################################################
### Step B: Find out who has hypertension as a comorbidity ###
##############################################################

### 1 Create request url and body for search via POST ###

# Extract Patient resource ids
pat_ids <- pat_tables$id

# Create body with search params for patient and inclusion of diagnoses 
body <- fhir_body(content = list("patient" = paste(pat_ids, collapse = ","),
                                 "_include" = "Encounter:diagnosis"))

# Create request URL
request <- fhir_url(url = "https://mii-agiop-3p.life.uni-leipzig.de/fhir",
                   resource = "Encounter")

### 2 download Encounters and Conditions via POST ###
encounter_bundles <- fhir_search(request = request, body = body)

### 3 Flatten bundles ###

# Create table descriptions
encounters <- fhir_table_description(resource = "Encounter",
                       cols = c(
                         patient = "subject/reference",
                         diagnosis = "diagnosis/condition/reference",
                         diagnosis.use = "diagnosis/use/coding/code")
                       )

conditions <- fhir_table_description(resource = "Condition",
                       cols = c(
                         id = "id",
                         code = "code/coding/code",
                         patient = "subject/reference")
                       )

# Combine table descriptions in one design
design <- fhir_design(encounters, conditions)

# Flatten
tables <- fhir_crack(encounter_bundles, design = design, brackets = c("[", "]"))

# Extract Encounters and Conditions into standalone tables 
encounters <- tables$encounters
diagnoses <- tables$conditions

### 4 Filter out hypertension comorbidities ###

# Melt diagnosis elements in encounter table
molten_encounters <- fhir_melt(indexed_data_frame = encounters, 
                               columns = c("diagnosis", "diagnosis.use"),
                               brackets = c("[", "]"),
                               all_columns = T)

# Remove indices after melting
molten_encounters <- fhir_rm_indices(molten_encounters, brackets = c("[", "]"))

#only keep comorbidity diagnoses
CM_diagnosis <- molten_encounters[molten_encounters$diagnosis.use=="CM",]

CM_diagnosis_ids <- sub("Condition/", "", CM_diagnosis$diagnosis)

# Remove indices
diagnoses <- fhir_rm_indices(diagnoses, brackets = c("[", "]"))

# Filter diagnosis tables for hypertension as a comorbidity
diagnoses <- diagnoses[diagnoses$id %in% CM_diagnosis_ids & grepl("I10|I11|I12|I13|I14|I15", diagnoses$code),]

# Add hypertension info to result
result$Hypertension<- "No"
result[result$subject.reference %in% diagnoses$patient,]$Hypertension <- "Yes"

### 5 Plot results ###

# Plot relationship of BMI and hypertension as a comorbidity
library(ggplot2)
ggplot(data = result, aes(x = Hypertension, y = BMI, fill = Hypertension )) +
  geom_boxplot() +
  geom_jitter(color="black", size=2, alpha=0.7, width = 0.25, height = 0) +
  theme(legend.position = "none") + 
  scale_fill_brewer(palette = "Set2") 

  
