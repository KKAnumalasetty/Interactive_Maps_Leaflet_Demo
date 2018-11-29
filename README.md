# Interactive_Maps_Leaflet_Demo
This visualization will help you to understand vendor details available near each consolidator. 
To run this file, download app.R and dummy_data.csv files and place them in the same directory to run it.


Once you run this shiny app, you should be able to see two input options 
1) Select a consolidator from the dropdown
2) Radius slider - scan for vendors within this radius of the consolidator

You will see three outputs including leaflet map with radius circle indicator. 

Outputs include
1) Leaflet with both vendors and consolidators.
2) Count of vendors nearby this consolidator.
3) Data table that lists down vendors with distance in miles.
4) Download button on top of leaflet map to download data table in the form of an excel.

Screenshot that shows Inputs such as selecting vendors and radius from the consolidator.
![leaflet demo image-1](https://user-images.githubusercontent.com/45413346/49183855-8f658480-f323-11e8-8c77-a6d99b9f76fb.JPG)


Screenshot that shows number of vendors available at this radius and datatable.
![leaflet demo image-2](https://user-images.githubusercontent.com/45413346/49183871-98565600-f323-11e8-9cd4-4654906f5e11.JPG)
