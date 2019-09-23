-- init spatialite
--SELECT load_extension('mod_spatialite')
--SELECT InitSpatialMetaData()

PRAGMA foreign_keys=ON;

create table if not exists DataSource(
  -- define an empty raster that cover the studyed Zone
  Source_ID INTEGER PRIMARY KEY,
  SourceName VARCHAR,
  -- the extent of the raster --
  xmin REAL NOT NULL,
  xmax REAL NOT NULL,
  ymin REAL NOT NULL,
  ymax REAL NOT NULL,
  -- -- --
  nrow INTEGER NOT NULL,
  ncol INTEGER NOT NULL,
  CRS VARCHAR NOT NULL,-- raster projection
  -- -- --
  UNIQUE(SourceName) ON CONFLICT IGNORE);

create table if not exists Variable(
  -- name of the variables
  Var_ID INTEGER PRIMARY KEY,
  Source_ID INTEGER,
  VarName VARCHAR,
  UNIQUE(VarName, Source_ID) ON CONFLICT IGNORE,
  FOREIGN KEY (Source_ID) REFERENCES DataSource(Source_ID) ON DELETE CASCADE
);

create table if not exists Field(
  -- field shapefiles
  Field_ID INTEGER PRIMARY KEY,
  GroupName VARCHAR,
  Name VARCHAR,
  geometry POLYGON,
  selected BOOLEAN default 0
);



create table if not exists Culture(
  -- culture on a field
  Field_ID INTEGER,
  Culture_ID INTEGER PRIMARY KEY,
  Declaration VARCHAR NOT NULL ON CONFLICT IGNORE, -- date of crop declaration
  Crop INTEGER NOT NULL ON CONFLICT IGNORE,
  UNIQUE(Field_ID, Declaration) ON CONFLICT IGNORE,
  FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE
);


create table if not exists Position(
  -- link the cell number and the corresponding Zone
  Source_ID INTEGER,
  Position_ID INTEGER PRIMARY KEY,
  Coord INTEGER NOT NULL,-- the cell number in the Zone raster
  UNIQUE(Source_ID, Coord) ON CONFLICT IGNORE,
  FOREIGN KEY (Source_ID) REFERENCES DataSource(Source_ID) ON DELETE CASCADE);

  create table if not exists ErosionEvent(
    -- Erosion happening on a specific field
    Event_ID INTEGER PRIMARY KEY,
    Field_ID INTEGER NOT NULL ON CONFLICT IGNORE, -- field where the erosion occure
    Event_Date VARCHAR NOT NULL ON CONFLICT IGNORE,
    UNIQUE(Field_ID, Event_Date) ON CONFLICT IGNORE,
    FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE);

create table if not exists Weighting(
  -- link the cell position and the Field
  W_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Field_ID INTEGER,
  Fcover FLOAT NOT NULL,  -- indicate the proportion of the field covered by the cell
  Pcover FLOAT NOT NULL,  -- indicate the proportion of the cell covered by the Field
  UNIQUE(Position_ID, Field_ID) ON CONFLICT IGNORE,
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE,
  FOREIGN KEY (Field_ID) REFERENCES Field(Field_ID) ON DELETE CASCADE);

create table if not exists Measure(
  -- NDVI value in a given cell in a given Date
  M_ID INTEGER PRIMARY KEY,
  Position_ID INTEGER,
  Var_ID INTEGER,
  Date VARCHAR NOT NULL,
  Value FLOAT NOT NULL,
  UNIQUE(Position_ID, Var_ID, Date) ON CONFLICT IGNORE,
  FOREIGN KEY (Var_ID) REFERENCES Variable(Var_ID) ON DELETE CASCADE
  FOREIGN KEY (Position_ID) REFERENCES Position(Position_ID) ON DELETE CASCADE);

Drop view IF EXISTS variableONfield;
CREATE VIEW IF NOT EXISTS variableONfield
AS
SELECT Field_ID, Var_ID, VarName, d.Source_ID, SourceName, p.Position_ID, Coord
FROM DataSource d
INNER JOIN Variable v on d.Source_ID=v.Source_ID
INNER JOIN Position p on d.Source_ID=p.Source_ID
INNER JOIN Weighting w on p.Position_ID=w.Position_ID;

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
from  Field f
LEFT JOIN Culture c
on f.Field_ID = c.Field_ID
LEFT JOIN ErosionEvent e
on f.Field_ID = e.Field_ID;

Drop view IF EXISTS WeightMeasure;
CREATE VIEW IF NOT EXISTS WeightMeasure
AS
SELECT M_ID, m.Position_ID, Field_ID, VarName, Date,
  Value, Fcover, Pcover
from Variable v
inner join Measure m ON v.Var_ID=m.Var_ID
inner join Weighting w ON m.Position_ID=w.Position_ID;

Drop view IF EXISTS Beginning;
CREATE VIEW IF NOT EXISTS Beginning
AS
Select c.Culture_ID, w.M_ID
FROM
  (Select w.Field_ID, Culture_ID, MIN(w.DATE) AS Date, w.VarName, Declaration
  FROM
    (Select w.Field_ID, Culture_ID, w.VarName, MAX(DATE) AS Date, Declaration
    FROM WeightMeasure w
    INNER JOIN Culture c ON w.Field_ID=c.Field_ID
    WHERE w.VarName == 'Crop_' || c.Crop
    AND w.Value == 10
    AND julianday(Date) < julianday(Declaration)
    GROUP BY Culture_ID) c
  INNER JOIN WeightMeasure w ON w.Field_ID=c.Field_ID
  WHERE w.VarName == c.VarName
  AND w.Value == 10 -- first phase
  --- remove 30 days to the last first phase to select all the other
  AND julianday(w.Date) > (julianday(c.Date) - 30)
  GROUP BY Culture_ID) c
INNER JOIN WeightMeasure w ON w.Field_ID=c.Field_ID
WHERE w.VarName=c.VarName
AND julianday(w.Date) >= julianday(c.Date)
AND julianday(w.Date) <= julianday(Declaration);

Drop view IF EXISTS Ending;
CREATE VIEW IF NOT EXISTS Ending
AS
Select c.Culture_ID, w.M_ID
FROM
  (Select w.Field_ID, Culture_ID, MAX(w.DATE) AS Date, w.VarName, Declaration
  FROM
    (Select w.Field_ID, Culture_ID, w.VarName, MIN(DATE) AS Date, Declaration
    FROM WeightMeasure w
    INNER JOIN Culture c ON w.Field_ID=c.Field_ID
    WHERE w.VarName == 'Crop_' || c.Crop
    AND w.Value == 24 -- last phase
    AND julianday(Date) > julianday(Declaration)
    GROUP BY Culture_ID) c
  INNER JOIN WeightMeasure w ON w.Field_ID=c.Field_ID
  WHERE w.VarName == c.VarName
  AND w.Value == 24-- last
  --- add 30 days to the first last phase to select all the other
  AND julianday(w.Date) < (julianday(c.Date) + 30)
  GROUP BY Culture_ID) c
INNER JOIN WeightMeasure w ON w.Field_ID=c.Field_ID
WHERE w.VarName=c.VarName
AND julianday(w.Date) <= julianday(c.Date)
AND julianday(w.Date) >= julianday(Declaration);

Drop view IF EXISTS Phase;
CREATE VIEW IF NOT EXISTS Phase
AS
Select c.Culture_ID, w.M_ID, Position_ID, Field_ID, Date AS Transition,
Value AS P, Fcover, Pcover
FROM (
  Select Culture_ID, M_ID
  FROM Beginning
  UNION
  Select Culture_ID, M_ID FROM Ending) c
INNER JOIN (
  Select * FROM WeightMeasure a INNER JOIN Culture b
  ON a.Field_ID=b.Field_ID) w
ON c.M_ID = w.M_ID AND c.Culture_ID = w.Culture_ID;
