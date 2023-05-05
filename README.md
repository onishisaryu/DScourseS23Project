# DScourseS23Project
## Exploring Strava's Suffer Score
The goal of this project is to explore how simple activity sumamry metrics may be related to Strava's suffer score metric. 

### Data Collection
The data for this project is activity log data from Strava, scraped using their API. The specific user from which the data is collelcted from is specified in the app_client_id object of the code. This object contains the ID number of the user. Data collection is initiated by the get_activity_list(stoken) command, and requires authorization from the user specified earlier. 

Data is then filtered to only include fields necessary for the project using the filter() and select() functions. 

The code for this can be found in /Rcode/data_cleaning.R

### Data Analysis

Update from local machine
