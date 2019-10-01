# ParaPhase
The aim of this application is to extract phenological and precipitation data on parcelles. The user can set the culture crop for one year and the dates of potential erosion events.

## Process

### Setup
The application needs rasters from the PHASE model and raster of precipitation. You should provide the path to this data in the Database.R file.

Launch PhenoErosion.R to launch the application.

### Database
*graph of the database*

The culture table contains a  declaration attribut. It indicate a day when the culture is prensent.

## Usage
The application have 4 tabs: Map, Graph, Save and Help.
### Map
In this tab, the user can interact with the spatial polygons. It is possible to draw the polygons directly on the map or to import GeoJSON files with the corresponding button. The user can also reload a project previously saved.

After a click on a polygon, it is possible to edit the data of the field. It is possible to create or delete an erosion event and to create or delete a culture declaration.


### Graph

### Save and Help
