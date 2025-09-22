# PURPOSE: Munge and Analysis of Salesforce OC data
# AUTHOR: Ben Kasdan | CT OTG
# LICENSE: MIT
# DATE: 2025-09-18
# NOTES: 

# LOCALS & SETUP ============================================================================

  # Libraries
    library(tidyverse)
    library(sf)
    
    library(tidytext)
    library(gt)
    library(tidytext)
    library(patchwork)
    library(ggtext)
library(readxl)
library(janitor)
library(stringr)
    
    
    
  
  # Set paths to pull data 
    #proj_paths pending check in with BITS
   
    #si_paths pending check in with bits
    
  # # Functions  
  #   
  # clean_me_up<-function(df){
  #   df<-read_xlsx(df)%>%
  #     clean_names()
  #   
  #   
  #   
  #   
  # }

# Munge ============================================================================  
# DSS================================
 #initial actions 
  df_DSS_initial <- read_excel("Data/DSS Segment Summary  Initial Actions-2025-09-18-10-06-59.xlsx")
 
  # #playing with metadata to make it easier to add a column for code 
  # list.files("Data/DSS Segment Summary  Initial Actions-2025-09-18-10-06-59.xlsx")
  #   filenames <- list.files("Data/DSS Segment Summary  Initial Actions-2025-09-18-10-06-59.xlsx")  
  # first_letters <- substr(filenames, 1, 1)  
  # table(first_letters)

  #Make names lowercase
  df_DSS_initial<-df_DSS_initial%>%
    clean_names()
  
  df_DSS_initial<-df_DSS_initial%>%
    separate(col=initial_actions_taken_with_client,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )
  
  
 
  
  
  df_DSS_initial<-df_DSS_initial%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
  mutate(actions = str_squish(actions))
   
  
  df_DSS_initial<-df_DSS_initial%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="initial actions")%>%
    mutate(agency="DSS")%>%
    na.omit()
  
 
 #  #Playing with counts
 #  # Count occurrences of each unique value, similar to table()
 # df<-df_DSS_initial
 #   df %>%
 #    count(actions)
 #  
 #  # Count occurrences and sort by frequency
 #  df1<-df %>%
 #    count(actions, sort = TRUE)
 #  
 #  # Add a count column to the existing data frame
 #  df1<-df %>%
 #    na.omit()%>%
 #    add_count(actions, name = "value_frequency")%>%
 #    distinct(actions,value_frequency )
  
  
  #Clean DSS Referrals
  df_dss_ref <- read_excel("Data/DSS Segment Summary  Referrals Provided-2025-09-18-10-06-50.xlsx")%>%
    clean_names()
  df_dss_ref<-df_dss_ref%>%
    separate(col=referral_provided,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_dss_ref<-df_dss_ref%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="referral")%>%
    mutate(agency="DSS")%>%
    na.omit()

  #clean next steps

  
  df_dss_ns <- read_excel("Data/DSS Segment Summary  Next Steps-2025-09-18-10-06-30.xlsx")%>%
    clean_names()
  df_dss_ns<-df_dss_ns%>%
    separate(col=next_steps,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_dss_ns<-df_dss_ns%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="next steps")%>%
    mutate(agency="DSS")%>%
    na.omit()
  
  
#Working with OEC=========================
  
  #Clean OEC Referrals
  df_oec_ref <- read_excel("Data/OEC Segment Summary  Referrals Provided-2025-09-18-15-56-22.xlsx")%>%
    clean_names()
  df_oec_ref<-df_oec_ref%>%
    separate(col=referral_provided,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_oec_ref<-df_oec_ref%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="referral")%>%
    mutate(agency="OEC")%>%
    na.omit()

  #next steps
  
  df_oec_ns <- read_excel("Data/OEC Segment Summary  Next Steps-2025-09-18-15-56-33.xlsx")%>%
    clean_names()
  df_oec_ns<-df_oec_ns%>%
    separate(col=next_steps,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_oec_ns<-df_oec_ns%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="next steps")%>%
    mutate(agency="OEC")%>%
    na.omit()
  
  
  #initial actions 
  df_oec_initial <- read_excel("Data/OEC Segment Summary  Initial Actions-2025-09-18-15-56-44.xlsx")%>%
    clean_names()
  
  df_oec_initial<-df_oec_initial%>%
    separate(col=initial_actions_taken_with_client,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )
  
  
  
  
  
  df_oec_initial<-df_oec_initial%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_oec_initial<-df_oec_initial%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="initial actions")%>%
    mutate(agency="OEC")%>%
    na.omit()
  
# DOH Data========
  
  #Actions and referrals
  
 
  
  df_doh_in <- read_excel("Data/DOH Segment Summary  Actions & Referrals-2025-09-18-15-56-10.xlsx")%>%
    clean_names()
  df_doh_in<-df_doh_in%>%
    separate(col=initial_actions_taken_referrals_provided,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))%>%
     select  (-"initial actions")%>% #drop uneeded columns
    dplyr::mutate(data_stream="initial actions")%>%
    mutate(agency="DOH")%>%
    na.omit()
  
  #next steps
  
  
  df_doh_ns <- read_excel("Data/DOH Segment Summary  Next Steps-2025-09-18-15-56-00.xlsx")%>%
    clean_names()
  df_doh_ns<-df_doh_ns%>%
    separate(col=next_steps,
             into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
             , sep = ";",
             extra = "merge",                       # Handle extra parts (merge or drop)
             fill = "right"                         # Fill missing parts with NA
    )%>%
    group_by(account_account_name,appointment_number,status,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "initial actions taken with client",
      values_to="actions"
    )%>% ungroup()%>%
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    mutate(actions = str_squish(actions))
  
  
  df_doh_ns<-df_doh_ns%>%
    select  (-"initial actions taken with client")%>% #drop uneeded columns
    dplyr::mutate(data_stream="next steps")%>%
    mutate(agency="DOH")%>%
    na.omit()
  
  
#Adding in instructions of each entry=====
  df_inst<- read_excel("Data/OC data scripts.xlsx")%>%
    clean_names()%>%
    mutate(actions = str_squish(actions))%>%
    mutate(data_stream = str_squish(data_stream))%>%
    mutate(agency = str_squish(agency))
    
  
#left join test
  




#combine all data sets and write to CSV

df_master_ds<-bind_rows(df_DSS_initial,df_dss_ref,df_dss_ns,df_oec_ref,df_oec_ns,df_oec_initial,df_doh_in,df_doh_ns)
  df_master_ds<-df_master_ds%>%
    left_join(y=df_inst, by=c("actions","data_stream","agency"))
  
  
  
  
  
  
  #Munge===============
  #Munge of customer experience data
  
  df_cs<- read_excel("Data/Feedback Survey  Other Agencies-2025-09-19-15-12-59.xlsx")%>%
    clean_names()%>%
  separate(col=other_agencies_services_to_learn_about,
           into =c("Part 1", "part_2", "part_3", "part_4","part_5","part_6","part_7")
           , sep = ";",
           extra = "merge",                       # Handle extra parts (merge or drop)
           fill = "right"                         # Fill missing parts with NA
  )%>%
    group_by(account_account_name,appointment_number,survey_name,service_territory, appointment_type)%>%
    pivot_longer(
      cols = `Part 1`:part_7,
      names_to = "other_agencies",
      values_to="actions"
    )%>% ungroup()%>%
    mutate(actions = str_squish(actions))
    #Add in function to clean titles to ensure all words are the same and have no extra spaces
    # mutate("other_agencies" = str_squish("other_agencies"))
  
  
  df_cs<-df_cs%>%
    select  (-"other_agencies")%>% #drop uneeded columns
    dplyr::mutate(data_stream="survey")%>%
    dplyr::mutate(script_language=paste0("I want to learn more about:  ", actions))%>%
    mutate(agency="N/A")%>%
    na.omit()
  
 
  
 
  

  # SPINDOWN ============================================================================
  #Export as CSV
  
  df_master_ds<-bind_rows(df_master_ds1,df_cs)
  
  write.csv(df_master_ds,"Master Outcomes_1.csv")
  
  
  