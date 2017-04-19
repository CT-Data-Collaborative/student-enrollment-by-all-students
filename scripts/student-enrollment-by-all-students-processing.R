library(dplyr)
library(datapkg)

##################################################################
#
# Processing Script for Student-Enrollment-by-All-Students
# Created by Jenna Daly
# On 03/22/2017
#
##################################################################

#Setup environment
sub_folders <- list.files()
data_location <- grep("All-Students", sub_folders, value=T)
path_to_top_level <- (paste0(getwd(), "/", data_location))
path_to_raw_data <- (paste0(getwd(), "/", data_location, "/", "raw"))
all_csvs <- dir(path_to_raw_data, recursive=T, pattern = ".csv") 
all_state_csvs <- dir(path_to_raw_data, recursive=T, pattern = "ct.csv") 
all_dist_csvs <- all_csvs[!all_csvs %in% all_state_csvs]

student_enrollment_dist <- data.frame(stringsAsFactors = F)
student_enrollment_dist_noTrend <- grep("trend", all_dist_csvs, value=T, invert=T)
for (i in 1:length(student_enrollment_dist_noTrend)) {
  current_file <- read.csv(paste0(path_to_raw_data, "/", student_enrollment_dist_noTrend[i]), stringsAsFactors=F, header=F )
  #remove first 3 rows
  current_file <- current_file[-c(1:3),]
  colnames(current_file) = current_file[1, ]
  current_file = current_file[-1, ] 
  current_file <- current_file[, !(names(current_file) == "District Code")]
  get_year <- as.numeric(substr(unique(unlist(gsub("[^0-9]", "", unlist(student_enrollment_dist_noTrend[i])), "")), 1, 4))
  get_year <- paste0(get_year, "-", get_year + 1) 
  current_file$Year <- get_year
  student_enrollment_dist <- rbind(student_enrollment_dist, current_file)
}

#Rename statewide data...
student_enrollment_dist[["District"]][student_enrollment_dist$"District" == "Total"]<- "Connecticut"

#backfill Districts
district_dp_URL <- 'https://raw.githubusercontent.com/CT-Data-Collaborative/ct-school-district-list/master/datapackage.json'
district_dp <- datapkg_read(path = district_dp_URL)
districts <- (district_dp$data[[1]])

student_enrollment_fips <- merge(student_enrollment_dist, districts, by.x = "District", by.y = "District", all=T)

student_enrollment_fips$District <- NULL

student_enrollment_fips<-student_enrollment_fips[!duplicated(student_enrollment_fips), ]

#backfill year
years <- c("2007-2008",
           "2008-2009",
           "2009-2010",
           "2010-2011",
           "2011-2012",
           "2012-2013",
           "2013-2014",
           "2014-2015",
           "2015-2016", 
           "2016-2017")

backfill_years <- expand.grid(
  `FixedDistrict` = unique(districts$`FixedDistrict`),
  `Year` = years
)

backfill_years$FixedDistrict <- as.character(backfill_years$FixedDistrict)
backfill_years$Year <- as.character(backfill_years$Year)

backfill_years <- arrange(backfill_years, FixedDistrict)

complete_student_enrollment <- merge(student_enrollment_fips, backfill_years, all=T)

#remove duplicated Year rows
complete_student_enrollment <- complete_student_enrollment[!with(complete_student_enrollment, is.na(complete_student_enrollment$Year)),]

#return blank in FIPS if not reported
complete_student_enrollment$FIPS <- as.character(complete_student_enrollment$FIPS)
complete_student_enrollment[["FIPS"]][is.na(complete_student_enrollment[["FIPS"]])] <- ""

#recode missing data with -6666
complete_student_enrollment$Count[is.na(complete_student_enrollment$Count)] <- -6666

#recode suppressed data with -9999
complete_student_enrollment$Count[complete_student_enrollment$Count == "*"]<- -9999

#Rename FixedDistrict to District
names(complete_student_enrollment)[names(complete_student_enrollment) == 'FixedDistrict'] <- 'District'

#Rename Count column
names(complete_student_enrollment)[names(complete_student_enrollment) == 'Count'] <- 'Value'

#setup Measure Type 
complete_student_enrollment$"Measure Type" <- "Number"

#set Variable
complete_student_enrollment$"Variable" <- "Student Enrollment"

#Order columns
complete_student_enrollment <- complete_student_enrollment %>% 
  select(`District`, `FIPS`, `Year`, `Variable`, `Measure Type`, `Value`)

#Use this to find if there are any duplicate entries for a given district
# test <- complete_student_enrollment[,c("District", "Year")]
# test2<-test[duplicated(test), ]

#Write CSV
write.table(
  complete_student_enrollment,
  file.path(path_to_top_level, "data", "student_enrollment_all_students_2008-2016.csv"),
  sep = ",",
  row.names = F
)
