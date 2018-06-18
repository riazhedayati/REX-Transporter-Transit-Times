library(RODBC)
library(tidyverse)
library(lubridate)
library(readxl)
library(magrittr)
library(stringr)
library(tidygraph)
library(ggraph)

# ### live call to database to get most up to date info
# odbcCloseAll()
# netezza <- odbcConnect("DEV_CDWHeadsUSER", believeNRows=FALSE)
# transit_data_raw <- sqlQuery(netezza, "
#   SELECT TRANSPORT_REQUEST_ID, PAT_ID, PAT_MRN_ID,
#     TRANSPORT_TYPE, TRANSPORT_FROM_LOCATION, TRANSPORT_TO_LOCATION,
#     ROUND_TRIP_YN, TRANSPORT_MODE, CANCEL_EVENT_DTTM,
#     TRANSPORT_PRIORITY, TRANSPORT_FROM_TYPE, TRANSPORT_TO_TYPE,
#     CURRENT_STATUS, ADDITIONAL_REQUIREMENTS, ASSIGNMENT_STATUS,
#     EVENT_INSTANT_LOCAL_DTTM, TRANSPORTER_ID, TRANSPORT_DELAY_REASON
#   FROM QA_CDWHEADS.YANANGUAN.TXPORT_TAT_DTL
#   WHERE REGION_NAME = 'REX Hospital Region'
#     and TRANSPORT_DATE >= '2017-07-01 00:00:00'
#     and TRANSPORT_DATE < '2018-03-01 00:00:00'
# ;
# ")
# odbcClose(netezza)

#read in primary dataset (snapshot of database saved to excel)
transit_data_raw <- read_excel("TransitTimes/Txport_tat_extract.xlsx") #comment this out if pulling straight from DB

#read in location & dept data
location <- read_csv("Location_Heirarchy.csv")

#match location to department
loc_dept <- location %>% 
  mutate(dept = ifelse(DEPT_DESCR == "NAIS DEPARTMENT", LOC_DESCR, DEPT_DESCR)) %>% 
  rename(loc = LOC_DESCR) %>% 
  select(loc, dept) %>% 
  mutate(dept = str_to_upper(dept)) %>% 
  mutate(dept = gsub("\\s*REX.?\\s*", "", dept)) %>% #remove word REX
  distinct()

#clean primary dataset
transit_data_cleaned <- transit_data_raw %>% 
  rename_all(. %>% tolower) %>% #rename variables to lowercase
  mutate(transport_from_location = str_to_upper(transport_from_location)) %>% 
  mutate(transport_to_location = str_to_upper(transport_to_location)) %>% 
  mutate(transport_from_location = gsub("\\s*REX.?\\s*", "", transport_from_location)) %>% 
  mutate(transport_to_location = gsub("\\s*REX.?\\s*", "", transport_to_location)) %>% 
  left_join(loc_dept, by = c("transport_from_location" = "loc")) %>% #find from_dept
  rename(from_dept = dept) %>% 
  left_join(loc_dept, by = c("transport_to_location" = "loc")) %>% #find to_dept
  rename(to_dept = dept) %>% 
  distinct() %>% #drop duplicate rows
  rename(order_id = transport_request_id,
         datetime = event_instant_local_dttm)

#filter out all delays, cancelled trips, etc
transit_data_filtered <- transit_data_cleaned %>% 
  filter(is.na(cancel_event_dttm)) %>% 
  filter(is.na(transport_delay_reason)) %>% 
  filter(current_status == "Completed") %>% 
  filter(assignment_status == "In Progress" | assignment_status == "Completed" | assignment_status == "Acknowledged") %>% 
  filter(transport_type == "Patient") %>% #keep only patient trips for now
  filter(is.na(round_trip_yn)) %>% #keep only one way trips; nothing is lost here for now
  mutate(addl_requirements_flag = ifelse(is.na(additional_requirements), 0, 1))

#spread data and calculate distance
transit_data_spread <- transit_data_filtered %>% 
  select(order_id, transport_from_location, transport_to_location, from_dept, to_dept, 
         assignment_status, datetime, transporter_id) %>% 
  arrange(order_id, desc(datetime)) %>% 
  group_by(order_id) %>% 
  distinct(assignment_status, .keep_all = TRUE) %>% 
  spread(key = assignment_status, value = datetime) %>% 
  select(everything(), Acknowledged, `In Progress`, Completed) %>% 
  mutate(ack_to_ip = as.integer(`In Progress` - Acknowledged)) %>% 
  mutate(ip_to_completed = as.integer(Completed - `In Progress`)) %>% 
  mutate(duration = ack_to_ip + ip_to_completed) %>% 
  filter(!is.na(ack_to_ip)) %>% 
  filter(!is.na(ip_to_completed)) %>% 
  ungroup() %>% 
  filter(!is.na(from_dept)) %>% 
  filter(!is.na(to_dept)) %>% 
  filter(duration > 0)

# #check to see if there are any overlaps between acknowledged and previous completed
# acknowledged_vs_completed <- transit_data_spread %>% 
#   select(-`In Progress`) %>% 
#   group_by(transporter_id) %>%
#   #filter(ip_duration + ack_duration > 0) %>% #remove this comment and you will see there are no obs
#   arrange(transporter_id, Acknowledged) %>% 
#   mutate(acknowledged_lead = lead(Acknowledged)) %>% 
#   mutate(preassign_flag = ifelse(Completed > acknowledged_lead, 1, 0)) %>% 
#   mutate(preassign_flag_lag = lag(preassign_flag)) %>% 
#   mutate(preassign_flag_lag2 = lag(preassign_flag_lag)) %>% 
#   mutate(preassign_flag_lead = lead(preassign_flag)) %>% 
#   filter(preassign_flag == 1 | preassign_flag_lag ==1 | preassign_flag_lead == 1 | preassign_flag_lag2 ==1)
# ### only overlaps are data errors...no overlaps between acknowledged and completed...this is great news

#plot distribution of minutes from dept a to dept b
deptA <- "4 WEST"
deptB <- "PATIENT TOWER LOBBY"
transit_data_spread %>%
  filter(from_dept == deptA & to_dept == deptB) %>%
  ggplot(aes(x = duration)) +
  geom_histogram(binwidth = 1) +
  #geom_vline(aes(xintercept = mean(duration)), col = "black", size = 1)+
  labs(x = "Minutes (Acknowledged to Completed)", 
       y = "Number of Trips",
       title = paste("Historical Transit Times from", str_to_title(deptA), "to", str_to_title(deptB)))

#calculate summary stats about transit data
trip_duration <- transit_data_spread %>%
  group_by(from_dept, to_dept) %>% 
  summarize(count = n(),
            avg_time = (mean(duration))) %>% 
  filter(count >= 5) %>% #set minimum number of trips needed to count as part of average
  arrange(desc(count)) %>% 
  ungroup()

#count percentage of trips that are captured within our historical estimates
location_pairs <- transit_data_spread %>% 
  group_by(from_dept, to_dept) %>% 
  count() %>% 
  arrange(desc(n))

location_pairs %>% filter(n >= 5) %$% sum(n) / sum(location_pairs$n)
rm(location_pairs)  

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

#use tidygraph/ggraph to create network plot
transit_tidygraph_dept <- tbl_graph(nodes = nodes_dept, edges = edges_dept, directed = TRUE)

#graph with various layouts
# ggraph(transit_tidygraph_dept, layout = "kk") +
#   geom_edge_link() +
#   geom_node_point() +
#   geom_node_text(aes(label = dept), repel = TRUE) +
#   theme_graph()

#calculate shortest path for dept and make resulting dataset tidy
shortest_path_tidy <- transit_tidygraph_dept %>% 
  as.igraph() %>% 
  igraph::distances() %>% 
  as_tibble() %>% 
  set_colnames(nodes_dept$dept) %>% 
  mutate(from_dept = nodes_dept$dept) %>% 
  gather(key = "to_dept", value = "shortest_path_dept", -from_dept) %>% 
  distinct()


### generate best prediction of transit times ###

#predict transit times using series of various methods
transit_times <- shortest_path_tidy %>% 
  left_join(trip_duration, by = c("from_dept", "to_dept")) %>% 
  select(from_dept, to_dept, avg_time, shortest_path_dept)

#calcualte APES to determine what model to use
mapes <- transit_times %>% 
  filter(!is.na(avg_time)) %>% 
  mutate(APE_shortestpath = abs(shortest_path_dept - avg_time)/avg_time) %>% 
  filter(APE_shortestpath > 0) %>% 
  filter(APE_shortestpath < 1)

mean(mapes$APE_shortestpath, na.rm = T) #mean absolute percentage error



#calculate transit times based on rules for NAs based on lowest APEs
final_output <- transit_times %>% 
  mutate(transit_time_pred = avg_time) %>% 
  mutate(transit_time_pred = ifelse(is.na(transit_time_pred), shortest_path_dept, transit_time_pred)) %>% 
  mutate(transit_time_pred = ifelse(transit_time_pred == 0, 8, transit_time_pred)) %>% #if you're going to the same dept replace 0 with 8 minutes to account for patient time and arriving from previous destination
  mutate(transit_time_pred = round(transit_time_pred, 1)) %>% 
  select(from_dept, to_dept, transit_time_pred) 

final_output %>% 
  write_csv(paste0("predicted_transit_times ", Sys.Date(), ".csv"))
