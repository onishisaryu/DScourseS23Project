# DScourseS23Project
## Exploring Strava's Suffer Score
The goal of this project is to explore how simple activity sumamry metrics may be related to Strava's suffer score metric. 

### Data Collection
The data for this project is activity log data from Strava, scraped using their API. The specific user from which the data is collelcted from is specified in the app_client_id object of the code. This object contains the ID number of the user. 

The process can be summarized in the following steps:
1. Data collection is initiated by the get_activity_list(stoken) command.
  - Requires authorization from the user specified earlier. 
2. Data is filtered to only include fields necessary for the project.
  - Use the filter() and select() functions. 
3. Store data in "run_mod" data frame for use in data analysis section.

The code for this can be found in the Rcode directory (/Rcode/data_cleaning.R)

### Data Analysis
The main method of data analysis used in this project is backwards stepwise multiple regression. This process follows these steps:
1. Generate an initial model with all variables
2. Initiate a stepwise variable selection to keep only the important variables. 

This was repeated three times, with each iteration adding a new component to the initial model. 
- The first model is a basic multiple regression, with no manipulation of variables. 
- The next includes quadratic transformations of the variables.
- The final model includes interaction terms. Only significant interactions are kept in the source code (interaction terms with elevation variable excluded).

Plots are contained in objects p1, p2 and p3. Execute the object name as a command to print plots.

The code for this section can be found in Rcode directory (/Rcode/data_analysis.R)

