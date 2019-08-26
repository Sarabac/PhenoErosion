PRAGMA foreign_keys=ON;

create table if not exists Zone(
  -- define an empty raster that cover the studyed Zone
  Zone_ID INTEGER PRIMARY KEY,
  Zone_Name VARCHAR,
  -- the extent of the raster --
  xmin REAL NOT NULL,
  xmax REAL NOT NULL,
  ymin REAL NOT NULL,
  ymax REAL NOT NULL,
  -- -- --
  nrow INTEGER NOT NULL,
  ncol INTEGER NOT NULL,
  CRS VARCHAR NOT NULL); -- raster projection

create table if not exists Field(
  -- field shapefiles
  Zone_ID INTEGER,
  Field_ID INTEGER PRIMARY KEY,
  Name VARCHAR,
  geometry GEOMETRY,
  selected BOOLEAN,
  FOREIGN KEY (Zone_ID) REFERENCES Zone(Zone_ID) ON DELETE CASCADE
);
create table if not exists Culture(
  -- culture on a field
  Field_ID INTEGER,
  Culture_ID INTEGER PRIMARY KEY,
  Declaration VARCHAR, -- date of crop declaration
  Crop INTEGER,
  UNIQUE(Field_ID, Declaration) ON CONFLICT IGNORE,
  FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE
);
create table if not exists Phase(
  -- Phse of a culture
  Culture_ID INTEGER,
  Phase_ID INTEGER PRIMARY KEY,
  Transition VARCHAR, -- date of Phase transition
  P INTEGER, -- Phase code
  FOREIGN KEY (Culture_ID) REFERENCES Culture(Culture_ID) ON DELETE CASCADE
);

create table if not exists Position(
  -- link the cell number and the corresponding Zone
  Zone_ID INTEGER,
  Position_ID INTEGER PRIMARY KEY,
  Coord INTEGER NOT NULL,-- the cell number in the Zone raster
  UNIQUE(Zone_ID, Coord) ON CONFLICT IGNORE,
  FOREIGN KEY (Zone_ID) REFERENCES Zone(Zone_ID) ON DELETE CASCADE);

  create table if not exists ErosionEvent(
    -- Erosion happening on a specific field
    Event_ID INTEGER PRIMARY KEY,
    Field_ID INTEGER, -- field where the erosion occure
    Event_Date VARCHAR,
    UNIQUE(Field_ID, Event_Date) ON CONFLICT IGNORE,
    FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE);

create table if not exists Weighting(
  -- link the cell position and the Field
  W_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Field_ID INTEGER,
  weight FLOAT,  -- indicate the proportion of the cell covered by the Field
  UNIQUE(Position_ID, Field_ID) ON CONFLICT IGNORE,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE,
  FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE);

create table if not exists Measure(
  -- NDVI value in a given cell in a given Date
  M_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Date Date,
  Variable VARCHAR,
  Value FLOAT NOT NULL,
  UNIQUE(Position_ID, Variable, Date) ON CONFLICT IGNORE,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

Drop view IF EXISTS CultureDate;
CREATE VIEW IF NOT EXISTS CultureDate
AS
Select distinct c.*,
(Select Transition from Phase p -- first phase
  where p.P=10 and p.Culture_ID=c.Culture_ID LIMIT 1) as Beginning,
  (Select Transition from Phase p -- last phase
    where p.P=24 and p.Culture_ID=c.Culture_ID LIMIT 1) as Ending
from Culture c;

Drop view IF EXISTS CulturePosition;
CREATE VIEW IF NOT EXISTS CulturePosition
AS
Select distinct c.*, w.Position_ID, w.W_ID
from CultureDate c
INNER JOIN Weighting w
on w.Field_ID = c.Field_ID;

Drop view IF EXISTS editField;
CREATE VIEW IF NOT EXISTS editField
AS -- all the required information to edit a field
Select *
from Zone z
INNER JOIN Field f
on z.Zone_ID = f.Zone_ID
LEFT JOIN Culture c
on f.Field_ID = c.Field_ID
LEFT JOIN ErosionEvent e
on f.Field_ID = e.Field_ID;
