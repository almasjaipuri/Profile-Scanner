#Location for Code Files
dir = "home/Profile Scanner/Working/"
dir_code = "home/Profile Scanner/Working/Code_Files/"
dir_files = "home/Profile Scanner/Working/Data_Files/"

#Location for Candidate's profile folder
dir_loc <- dir_files

gl_auth(paste0(dir,"My First Project-d94c8a0521d7.json"))
source(paste0(dir_code,"functions.R"))
source(paste0(dir_code,"model_code.R"))
#Location for Google API auth file


#To run and display the dashboard
runApp(appDir = dir_code, launch.browser = TRUE)
