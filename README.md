# eri_global food systems faculty directory
-This app was built to host all faculty involved in food systems research in a simple database.
-If you run the app and it doesn't fully load, check the code in the server for the food tab, click on the red dot(s), then click on the "stop" button at the top of the console and then re-run the app and it will load properly. 

# File structure
-The main list of faculty (faculty_list) is stored in the data folder. All faculty information should be edited in that csv file either in google sheets (its original form) or in R. This includes: faculty image and website URLs, keywords, etc.
-If you update the faculty_list.csv file, be sure to save it as faculty_list.csv and replace the existing file. That way the code for the list and the specializations will still run properly. If you name it something else, the code won't work.  

# Site images
All site images (non-faculty images) have to be stored in the www folder in the directory or they can't be read into the app. 

# Styling
The site is styled to the default "darkly" theme but any styling can be updated to that in the css file in the data/style folder. 


