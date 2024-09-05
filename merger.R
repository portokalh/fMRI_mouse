# Set working directory
setwd("/Users/alex/AlexBadea_MyCodes/fMRI_mouse/behavior/to_merge")



# List all CSV files in the directory
file_list <- list.files(pattern = "*.csv")

# Function to read and preprocess each file
process_file <- function(file) {
  # Read the CSV file
  data <- read.csv(file)
  
  # Rename columns
  colnames(data) <- gsub("Distance..m.", "Distance", colnames(data))
  colnames(data) <- gsub("SW...distance..m.", "SW_Distance", colnames(data))
  data$Animal = gsub("-","_",data$Animal )
  
  return(data)
}

# Read and preprocess all files
data_list <- lapply(file_list, process_file)

# Find common columns
common_columns <- Reduce(intersect, lapply(data_list, colnames))

# Select only common columns and combine the data frames
combined_data <- do.call(rbind, lapply(data_list, function(x) x[, common_columns]))

# View the first few rows of the combined data frame
head(combined_data)



# write.csv(combined_data, "/Users/alex/AlexBadea_MyCodes/fMRI_mouse/behavior/combined_MWM_data.csv", row.names = FALSE)


combined_data$ARunno = NA
combined_data$Lifestyle = NA

master_path = '/Users/alex/AlexBadea_MyCodes/fMRI_mouse/MasterSheet_Experiments2021.xlsx'
master_df = read_xlsx(master_path, sheet = "18ABB11_readable02.22.22_BJ_Cor") 


combined_data$Animal = gsub("-","_",combined_data$Animal)
master_df$CIVMID = gsub("-","_",master_df$CIVMID)
master_df$BadeaID = gsub("-","_",master_df$BadeaID )

for (i in 1:dim(combined_data)[1]) {
  index_master = which(combined_data$Animal [i] ==  master_df$BadeaID )
  if(length(index_master)==0) {  index_master = which(combined_data$Animal [i] ==  master_df$CIVMID ) }
  if(length(index_master)>0) {
    combined_data$ARunno[i] =  master_df$ARunno[index_master]
    combined_data$Genotype[i] = master_df$Genotype[index_master]
    combined_data$Sex[i]  =  master_df$Sex[index_master]
    combined_data$Diet[i]  = master_df$Diet[index_master]
    combined_data$Lifestyle[i]  = master_df$Lifestyle[index_master]
    combined_data$Age [i] = master_df$Age_Months[index_master]
    
  }
  
}

combined_data = combined_data[!is.na(combined_data$ARunno),]


combined_data = combined_data[combined_data$Stage=="Probe_D8",]

unique_combined_data <- combined_data %>% distinct(Animal, .keep_all = TRUE)


path_connec="/Users/alex/AlexBadea_MyCodes/fMRI2cardiac/fMRI2cardiac_082724/time_ser/"
file_list=list.files(path_connec)
plain_index = grep("FC", file_list)
file_list = file_list [plain_index]




temp_conn= read.csv( paste0(path_connec,file_list[1]) , header = F )
# connectivity=array( NA ,dim=c(length(including),length(including),dim(cardiac)[1]))
connectivity=array( NA ,dim=c(dim(temp_conn)[1],dim(temp_conn)[2],dim(unique_combined_data)[1]))
dim(connectivity)
notfound = 0 

for (i in 1:dim(unique_combined_data)[1] ) {
  index= which(unique_combined_data$ARunno[i] == substr(file_list,4,12) )
  if (length(index) > 0 ) {
    #print(i)
    temp = read.csv( paste0(path_connec,file_list[index]) , header = F )
    temp[is.na(temp)] = 0
    #temp = temp[,1:90]
    # temp = as.matrix(temp[index_include,index_include])
    temp = as.matrix(temp)
    
    temp=(temp - mean(temp)) /sd(temp)
    connectivity[,,i]=as.matrix(temp)
    
    
  }
  else
  { notfound = c(notfound, i) }
}

# connectivity = connectivity[index_include, index_include, ]

connectivity = connectivity [ , ,-notfound[2:length(notfound)] ]
unique_combined_data = unique_combined_data[-notfound[2:length(notfound)],]


response=unique_combined_data
save(response, file="response.rda")
save(connectivity, file="connectivity.rda")




# Optionally, save the combined data frame to a new CSV file
# write.csv(combined_data, "combined_MWM_data.csv", row.names = FALSE)