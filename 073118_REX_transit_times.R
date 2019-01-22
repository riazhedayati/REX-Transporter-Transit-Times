library(RODBC)
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(stringr)
library(tidygraph)
library(ggraph)
library(datapasta)

##### read in data ----------------------------------------------------------------------

### live call to database to get most up to date historical transit data
odbcCloseAll()
netezzaDev <- odbcConnect("DEV_CDWHEADSUSER", believeNRows=FALSE)
transit_data_raw <- sqlQuery(netezzaDev, "
  SELECT TRANSPORT_REQUEST_ID, TRANSPORT_TYPE, 
    TRANSPORT_FROM_LOCATION, TRANSPORT_FROM_LOCATION_ID,
    TRANSPORT_TO_LOCATION,TRANSPORT_TO_LOCATION_ID,
    ROUND_TRIP_YN, TRANSPORT_MODE, REQUEST_SOURCE, REQUEST_DEPARTMENT,
    CANCEL_EVENT_DTTM, REGION_ID, REGION_NAME, SECTOR_ID, SECTOR_NAME, 
    TRANSPORT_PRIORITY, TRANSPORT_FROM_TYPE, TRANSPORT_TO_TYPE,
    CURRENT_STATUS, ADDITIONAL_REQUIREMENTS, ASSIGNMENT_STATUS,
    EVENT_INSTANT_LOCAL_DTTM, TRANSPORTER_ID
  FROM QA_CDWHEADS.YANANGUAN.TXPORT_TAT_DTL
  WHERE REGION_NAME = 'REX Hospital Region'
    and TRANSPORT_DATE >= '2017-07-01 00:00:00'
    and TRANSPORT_DATE < '2018-07-01 00:00:00' ;
")
odbcClose(netezzaDev)

#read in dept to unit data from Ernie
units <- read_excel("REX depts to units complete.xlsx", sheet = 'Depts')

#read in REX location to dept mapping
netezzaProd <- odbcConnect("CDWHEADSUSER", believeNRows=FALSE)
loc_dept <- sqlQuery(netezzaProd, "
    SELECT LOCATION_ID
       , LOCATION_DESCRIPTION 
       , DEPARTMENT_ID
       , DEPARTMENT_DESCRIPTION
  FROM NZ_EDWD_ADM.DIM_REX_DEPARTMENT ;
") %>% 
  rename(loc = LOCATION_ID,
         loc_desc = LOCATION_DESCRIPTION,
         dept = DEPARTMENT_ID,
         dept_desc = DEPARTMENT_DESCRIPTION) %>% 
  left_join(units, by = "dept") %>% 
  select(-dept_name)
odbcClose(netezzaProd)

#read in final destination table that Luis made
final_dest <- read_excel("REX depts to units complete.xlsx", sheet = 'Unit to Unit', range = "A2:M14") %>% 
  rename(from_unit = Origin) %>% 
  gather(-from_unit, key = "to_unit", value = "key") %>% 
  left_join(tribble(
    ~key,   ~final_dest,
    "A",   "from_dept",
    "B",     "to_dept",
    "C",   "from_dept",
    "D", "IMG MRI REX",
    "E",   "from_dept",
    "F",     "to_dept",
    "G",     "to_dept",
    "H",     "to_dept"
  )) %>% 
  select(-key)


##### merge and clean datsets  ----------------------------------------------------------------------

#clean primary dataset
transit_data_cleaned <- transit_data_raw %>% 
  rename_all(. %>% tolower) %>% #rename variables to lowercase
  left_join(loc_dept, by = c("transport_from_location_id" = "loc")) %>% #find from_dept
  rename(from_dept = dept,
         from_loc_desc = loc_desc,
         from_dept_desc = dept_desc, 
         from_unit = unit) %>% 
  left_join(loc_dept, by = c("transport_to_location_id" = "loc")) %>% #find to_dept
  rename(to_dept = dept,
         to_loc_desc = loc_desc,
         to_dept_desc = dept_desc, 
         to_unit = unit) %>% 
  distinct() %>% #drop duplicate rows
  rename(order_id = transport_request_id,
         datetime = event_instant_local_dttm) %>% 
  left_join(final_dest, by = c("from_unit", "to_unit")) #join final destination data that Luis created

#filter out all delays, cancelled trips, etc
transit_data_filtered <- transit_data_cleaned %>% 
  #filter(transport_type == "Patient") %>% #keep only patient trips for now
  #filter(is.na(round_trip_yn)) %>% #keep only one way trips
  filter(current_status == "Completed") %>% #remove cancelled trips
  filter(!is.na(from_dept)) %>% 
  filter(!is.na(to_dept)) %>% #remove trips for which there is no matching dept
  filter(!is.na(from_unit)) %>% 
  filter(!is.na(to_unit)) %>% #remove trips for which there is no matching unit
  filter(assignment_status != "Pending")

#reshape data wide and calculate trip duration (minutes) from acknowledged to complete
transit_data_spread <- transit_data_filtered %>% 
  arrange(order_id, desc(datetime)) %>% 
  group_by(order_id) %>% 
  distinct(assignment_status, .keep_all = TRUE) %>% 
  spread(key = assignment_status, value = datetime) %>% 
  ungroup() %>% 
  filter(is.na(Postponed) | is.na(Delayed) | is.na(Future)) %>% #remove all postponed, delayed, future trips
  filter(!is.na(Completed) | !is.na(Assigned) | !is.na(Acknowledged)) %>%  #remove all incomplete, unassigned, unacknowledged trips
  mutate(ack_to_ip = as.integer(`In Progress` - Acknowledged)/60) %>% 
  mutate(ip_to_completed = as.integer(Completed - `In Progress`)/60) %>% 
  mutate(duration = ack_to_ip + ip_to_completed) %>% 
  filter(duration > 0) %>% 
  filter(!is.na(Assigned)) %>% 
  mutate(additional_requirements_flag = if_else(is.na(additional_requirements), 0, 1),
         hour_assigned = hour(Assigned),
         day_assigned = wday(Assigned), 
         transporter_id = as.factor(transporter_id), 
         transport_mode = as.factor(if_else(is.na(transport_mode), "Missing", as.character(transport_mode)))) %>%
  select(duration, transport_type, transport_mode, 
         request_source, transport_from_type, transport_to_type, additional_requirements, additional_requirements_flag, 
         from_dept, to_dept, hour_assigned, day_assigned)


##### calculate avg time and shortest path distances between each dept pair----------------------------

#calculate summary stats about transit data
trip_duration <- transit_data_spread %>%
  group_by(from_dept, to_dept) %>% 
  summarize(count = n(),
            avg_time = (mean(duration))) %>% 
  ungroup() %>% 
  filter(count >= 5) %>% #use shortest path instead of historical avg for pairs with <5 observations in last year (>99% of trips)
  arrange(desc(count))

### calcualte shortest path between depts
#calculate dept node list
to_nodelist <- trip_duration %>% 
  group_by(to_dept) %>% 
  summarize() %>% 
  rename(dept = to_dept)
from_nodelist <- trip_duration %>% 
  group_by(from_dept) %>% 
  summarize() %>% 
  rename(dept = from_dept)
nodes_dept <- from_nodelist %>%
  bind_rows(to_nodelist) %>%
  distinct(dept) %>% 
  arrange(dept) %>% 
  mutate(id = row_number())

#create dept edge list
edges_dept <- trip_duration %>% 
  left_join(nodes_dept, by = c("from_dept" = "dept"))%>% 
  rename(from = id) %>% 
  left_join(nodes_dept, by = c("to_dept" = "dept")) %>% 
  rename(to = id) %>% 
  rename(weight = avg_time) %>% 
  select(from, to, weight)

#calculate shortest path for dept and make resulting dataset tidy
shortest_path_tidy <- tbl_graph(nodes = nodes_dept, edges = edges_dept, directed = TRUE) %>% 
  as.igraph() %>% 
  igraph::distances() %>% 
  as_tibble() %>% 
  set_colnames(nodes_dept$dept) %>% 
  mutate(from_dept = nodes_dept$dept) %>% 
  gather(key = "to_dept", value = "shortest_path_dept", -from_dept) %>% 
  distinct() %>% 
  mutate(to_dept = as.numeric(to_dept))


##### generate best prediction of avg transit times ------------------------------------------------

#predict transit times using series of various methods
transit_times <- shortest_path_tidy %>% 
  left_join(trip_duration, by = c("from_dept", "to_dept")) %>% 
  select(from_dept, to_dept, avg_time, shortest_path_dept)

#calculate transit times based on rules for NAs based on lowest APEs
final_output <- transit_times %>% 
  mutate(transit_time_pred = avg_time) %>% 
  mutate(transit_time_pred = ifelse(is.na(transit_time_pred), shortest_path_dept, transit_time_pred)) %>% 
  mutate(transit_time_pred = ifelse(transit_time_pred == 0, 22.5, transit_time_pred)) %>% #if you're going from/to the same dept replace 0 with historical avg of 22.5
  mutate(transit_time_pred = round(transit_time_pred, 1)) %>% 
  select(from_dept, to_dept, transit_time_pred) %>% 
  left_join(units, by = c("from_dept" = "dept")) %>% 
  rename(from_dept_name = dept_name, 
         from_unit = unit) %>% 
  left_join(units, by = c("to_dept" = "dept")) %>% 
  rename(to_dept_name = dept_name, 
         to_unit = unit) %>% 
  left_join(final_dest, by = c("from_unit", "to_unit")) %>% 
  mutate(end_dept = case_when(final_dest == "from_dept" ~ from_dept,
                       final_dest == "to_dept" ~ to_dept, 
                       final_dest == "IMG MRI REX" ~ 1060128360002)) %>% #this is the MRI dept code
  rename(avg_ip_to_complete = transit_time_pred)

#write flat file of predictions to csv 
final_output %>% 
  write_csv(paste0("predicted_transit_times ", Sys.Date(), ".csv"))



### create linear regression using point to point estimations as an input
library(tidymodels)
library(rpart)
library(randomForest)

#create training and test data
set.seed(22)
transit_data_spread_model <- transit_data_spread %>% 
  left_join(final_output, by = c("from_dept", "to_dept")) %>% 
  select(duration, transport_type, transport_mode, request_source, additional_requirements_flag, avg_ip_to_complete) %>% 
  mutate(transport_mode = relevel(transport_mode, ref = "Missing")) %>% 
  mutate(request_source = relevel(request_source, ref = "Scheduling")) 

data_split <- initial_split(transit_data_spread_model)
transit_train <- training(data_split)
transit_test <- testing(data_split)
map_dbl(transit_train, ~ mean(!is.na(.x)))

transit_rec <- recipe(duration ~ ., data = transit_train) %>% 
  step_nzv(all_predictors()) %>% 
  step_meanimpute(avg_ip_to_complete) %>% 
  step_other(transport_mode, threshold = 0.1) %>% 
  step_other(request_source, threshold = 0.1) %>% 
  step_dummy(all_nominal())

transit_rec_prepped <- prep(transit_rec, training = transit_train, retain = TRUE, verbose = TRUE)
transit_train_dm <- juice(transit_rec_prepped) %>% 
  select(avg_ip_to_complete, duration, transport_mode_Stretcher, transport_mode_Wheelchair,
         request_source_Discharge, additional_requirements_flag)  
transit_test_dm <- bake(transit_rec_prepped, newdata = transit_test) %>% 
  select(avg_ip_to_complete, duration, transport_mode_Stretcher, transport_mode_Wheelchair,
         request_source_Discharge, additional_requirements_flag)


#create linear model
transit_lm <- lm(duration ~ ., data = transit_train_dm)
summary(transit_lm)

#create simple tree model
transit_rpart <- rpart(duration ~ ., data = transit_train_dm)
summary(transit_rpart)

#predict using models
transit_test_dm$preds_rpart <- predict(transit_rpart, transit_test_dm)
transit_test_dm$preds_lm <- predict(transit_lm, transit_test_dm)

#calculate error rates
test_set_final <- transit_test_dm %>% 
  select(duration, preds_rpart, preds_lm) %>% 
  mutate(error_tree = abs(preds_rpart - duration),
         error_lm = abs(preds_lm - duration),
         pct_error_tree = error_tree/duration,
         pct_error_lm = error_lm/duration)

mean(test_set_final$error_lm)
mean(test_set_final$error_tree)
#error rate using point to point flat file is 4.91 min
