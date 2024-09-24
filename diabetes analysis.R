library(dplyr)
library(readxl)
library(xmlparsedata)
library(splitstackshape)
library(lubridate)
library(tidyr)
library(reshape2)
library(xlsx)

https://www.healthnetworksolutions.net/index.php/understanding-the-icd-10-code-structure 

###
#Get Data#
###

setwd("C:/Users/zachary.lippert/OneDrive - Blue Cross and Blue Shield of Nebraska/Documents/diabeetus")


x<-proc.time()

vision <- read_excel('vision.xlsx')
names(vision) <- gsub(' ', '_', names(vision))
names(vision) <- tolower(names(vision))
vision$year <- year(vision$servicefromdt)
vision <- vision[vision$service_age <= 75,]

diabetes_patients <- read_excel('diabetes patients.xlsx')
names(diabetes_patients) <- gsub(' ', '_', names(diabetes_patients))
names(diabetes_patients) <- tolower(names(diabetes_patients))
diabetes_patients$year <- year(diabetes_patients$servicefromdt)
diabetes_patients <- diabetes_patients[diabetes_patients$service_age <= 75,]

zip_map <- read_excel('zip_code_database.xlsx')
names(zip_map) <- gsub(' ', '_', names(zip_map))
names(zip_map) <- tolower(names(zip_map))
zip_map <- zip_map[zip_map$state == 'NE',]
zip_map <- zip_map[c('zip','county','latitude','longitude','irs_estimated_population_2015')]

county_pop <- zip_map %>%
  group_by_at('county') %>%
  summarize(c_pop = sum(irs_estimated_population_2015))

provider_encounters <- read_excel('patient encounters.xlsx')
names(provider_encounters) <- gsub(' ', '_', names(provider_encounters))
names(provider_encounters) <- tolower(names(provider_encounters))
provider_encounters$year <- year(provider_encounters$servicefromdt)
provider_encounters<- provider_encounters[provider_encounters$service_age <= 75,]

zip <- read_excel('ruca_codes.xlsx', sheet = 'Data')
names(zip) <- gsub(' ', '_', names(zip))
names(zip) <- tolower(names(zip))
zip <- zip[zip$state == 'NE',]

print(proc.time()-x)

yellow_pages <- merge(diabetes_patients, zip,  by.x = 'patientpostalcd', by.y = 'zip_code')
yellow_pages <- yellow_pages[c('heartbeatid','patientpostalcd','ruca1','zip_class')]
yellow_pages$rural_urban <- ifelse(yellow_pages$ruca1 >= 7 , 'rural','urban')

provider_encounters <- merge(provider_encounters, zip, by.x = 'patientpostalcd', by.y = 'zip_code')

diabetes_patients$servicefromdt <- as.Date(diabetes_patients$servicefromdt)
diabetes_patients <- diabetes_patients[order(diabetes_patients$servicefromdt),]


diagnosis <- diabetes_patients[match(unique(diabetes_patients$heartbeatid), diabetes_patients$heartbeatid),]

additional_encounters <- anti_join(diabetes_patients, diagnosis)
additional_encounters <- merge(additional_encounters, zip, by.x = 'patientpostalcd', by.y = 'zip_code')

colnames(provider_encounters) = colnames(additional_encounters)
all_encounters <- rbind(provider_encounters, additional_encounters)
all_encounters$counter <- 1

mergeme = c('heartbeatid','year')

# yr_encounter <- all_encounters %>%
#   group_by(year,patientaccountid) %>%
#   summarise(pcp_visit = sum(all_encounters$counter))

yr_encounter <- all_encounters %>%
  group_by_at(vars(one_of(mergeme))) %>%
  summarize(visits = sum(counter))

master <- data.frame(master_year = c(2014,2015,2016,2017,2018,2019)) 
master$dummymerge <- 1
diagnosis$dummymerge <- 1
master_roster <- merge(diagnosis, master, by = 'dummymerge')

treatment <- merge(master_roster, yr_encounter, by.x = c('heartbeatid','master_year'), by.y = c('heartbeatid','year'), all = T)
treatment$treat_year <- treatment$master_year
treatment <- rename(treatment, onset_year = year)
treatment$visits[is.na(treatment$visits)] <- 0
treatment$maint_opp <- ifelse(treatment$treat_year >= treatment$onset_year,1,0)

#change number of visits to change how well they adhere
treatment$maint_visit <- ifelse(treatment$maint_opp == 1 & treatment$visits > 0, 1,0)

adherence <- treatment %>%
  group_by_at('heartbeatid') %>%
  summarize(pcp = sum(maint_visit)
            ,pcp_opps = sum(maint_opp))
adherence <- na.omit(adherence)
adherence$ad_rate <- adherence$pcp/adherence$pcp_opps
adherence <- merge(adherence, yellow_pages, by = 'heartbeatid', all.x = T)
adherence$counter <- 1

adherence <- merge(adherence, zip_map, by.x = 'patientpostalcd', by.y = 'zip', all.x = T)
adherence <- merge(adherence, county_pop, by = 'county', all.x = T)

rural <- adherence[adherence$rural_urban =='rural',]
urban <- adherence[adherence$rural_urban =='urban',]

final_ru <- adherence %>%
  group_by_at('rural_urban') %>%
  summarize(group_rate = mean(ad_rate)
            ,patient_count = sum(counter))

final_county <- adherence %>%
  group_by_at('county') %>%
  summarize(group_rate = mean(ad_rate)
            ,patient_count = sum(counter)
            ,population = mean(c_pop)
            ,avg_r_u_class = mean(ruca1))
plot(final_county$avg_r_u_class, final_county$group_rate)
plot(final_county$population, final_county$group_rate)

write.csv(final_county, file = 'final county.csv')


t.test(rural$ad_rate,urban$ad_rate)



#Vision Claims###
vision$counter <- 1

vision_encounters <- vision %>%
  group_by_at(vars(one_of(mergeme))) %>%
  summarize(visits = sum(counter))

vision_master <- merge(master_roster, vision_encounters, by.x = c('heartbeatid','master_year'), by.y = c('heartbeatid','year'), all = T)

vision_master$treat_year <- vision_master$master_year
vision_master <- rename(vision_master, onset_year = year)
vision_master$visits[is.na(vision_master$visits)] <- 0
vision_master$maint_opp <- ifelse(vision_master$treat_year >= vision_master$onset_year,1,0)

vision_master$maint_visit <- ifelse(vision_master$maint_opp == 1 & vision_master$visits > 0, 1,0)

vision_adh <- vision_master %>%
  group_by_at('heartbeatid') %>%
  summarize(vx = sum(maint_visit)
            ,vx_opps = sum(maint_opp))
vision_adh <- na.omit(vision_adh)
vision_adh$ad_rate <- vision_adh$vx/vision_adh$vx_opps
vision_adh <- merge(vision_adh, yellow_pages, by = 'heartbeatid', all.x = T)
vision_adh$counter <- 1

vision_adh <- merge(vision_adh, zip_map, by.x = 'patientpostalcd', by.y = 'zip', all.x = T)
vision_adh <- merge(vision_adh, county_pop, by = 'county', all.x = T)


rural_vx <- vision_adh[vision_adh$rural_urban =='rural',]
urban_vx <- vision_adh[vision_adh$rural_urban =='urban',]

t.test(rural_vx$ad_rate,urban_vx$ad_rate)

#other method with printing and reloading pivot table

# encounter_table <- read_excel('yr_encounter_p.xlsx', sheet = 'Sheet1', skip = 3)
# names(encounter_table) <- gsub(' ', '_', names(encounter_table))
# names(encounter_table) <- tolower(names(encounter_table))
# encounter_table <- rename(encounter_table, heartbeatid = row_labels)
# encounter_table <- rename(encounter_table, total_encounters = grand_total)
# encounter_table[is.na(encounter_table)] <-0
#  
# diagnosis <- merge(diagnosis, zip, by.x = 'patientpostalcd', by.y = 'zip_code')
# diagnosis$heartbeatid
# 
# final_table <- diagnosis[c('heartbeatid', 'year', 'service_age', 'patientpostalcd', 'ruca1', 'zip_class')]
# final_table <- rename(final_table, onset_year= year)
# final_table <- rename(final_table, onset_age = service_age)
# 
# maintenance_visits <- merge(final_table, encounter_table, by = 'heartbeatid')
# maintenance_visits$years_of_maint <- 2020-maintenance_visits$onset_year
# maintenance_visits$treatment_start_column_id <- 13-maintenance_visits$years_of_maint
# maintenance_visits$compliant_years <- rowSums(maintenance_visits[treatment_start_column_id:12]>0)
