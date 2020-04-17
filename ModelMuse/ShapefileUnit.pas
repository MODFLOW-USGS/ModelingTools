{@abstract(@name defines @link(TShapefileGeometryReader)
  and @link(TShapefileGeometryWriter)
  which provide methods of reading and writing respectively
  the geometry part
  of ESRI Shapefiles.  Each shape in the Shapefile is stored in
  a @link(TShapeObject) which can be accessed through the
  TShapefileGeometryReader.@link(TShapefileGeometryReader.Items)
  and the TShapefileGeometryWriter.@link(TShapefileGeometryWriter.Items)
  properties.)

  See http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf for a
  description of the Shapefile format.}
unit ShapefileUnit;

interface

uses SysUtils, Classes, Types, System.Generics.Collections;

type
  EInvalidShapeFile = class(Exception);
  EMismatchHeader = class(Exception);

 { @abstract(@name represents the bounding box of a shape
   or of all the shapes in a Shapefile.)
 @longcode(#
  TBoundingBox = packed record
  case integer of
    0: (XMin: double;
        YMin: double;
        XMax: double;
        YMax: double);
    1: (Box: array[0..3] of double);
  end;
 #)
 }
  TBoundingBox = packed record
  case integer of
    0: (
        // @name is the minimum X coordinate.
        XMin: double;
        // @name is the minimum Y coordinate.
        YMin: double;
        // @name is the maximum X coordinate.
        XMax: double;
        // @name is the maximum Y coordinate.
        YMax: double);
    1: (
        // @name represents the bounding box in an array
        // in the order XMin, YMin, XMax, YMax.
        Box: array[0..3] of double);
  end;

  {@abstract(@name appears at the beginning of a Shapefile)
  @longcode(#
  TShapefileHeader = packed record
    FileCode: longint;
    Unused1: longint;
    Unused2: longint;
    Unused3: longint;
    Unused4: longint;
    Unused5: longint;
    FileLength: longint;
    Version: longint;
    ShapeType: longint;
    BoundingBoxXMin: double;
    BoundingBoxYMin: double;
    BoundingBoxXMax: double;
    BoundingBoxYMax: double;
    BoundingBoxZMin: double;
    BoundingBoxZMax: double;
    BoundingBoxMMin: double;
    BoundingBoxMMax: double;
  end;
  #)

  }
  TShapefileHeader = packed record
    // @name is a BigEndian value. It should be 9994.
    FileCode: longint;
    // @name is an unused value in BigEndian format.
    Unused1: longint;
    // @name is an unused value in BigEndian format.
    Unused2: longint;
    // @name is an unused value in BigEndian format.
    Unused3: longint;
    // @name is an unused value in BigEndian format.
    Unused4: longint;
    // @name is an unused value in BigEndian format.
    Unused5: longint;
    // @name is the file length measured in 16 bit words
    // @name is in BigEndian format.
    FileLength: longint;
    // @name is the Shapefile version number. It should be 1000.
    Version: longint;
    // @name indicates the type of shape stored in the Shapefile.
    // It can be any of the following
    // @link(stNull) = 0;
    // @link(stPoint) = 1;
    // @link(stPolyLine) = 3;
    // @link(stPolygon) = 5;
    // @link(stMultiPoint) = 8;
    // @link(stPointZ) = 11;
    // @link(stPolyLineZ) = 13;
    // @link(stPolygonZ) = 15;
    // @link(stMultiPointZ) = 18;
    // @link(stPointM) = 21;
    // @link(stPolyLineM) = 23;
    // @link(stPolygonM) = 25;
    // @link(stMultipointM) = 28;
    // @link(stMultiPatch) = 31;
    ShapeType: longint;
    // @name is the minimum X value for the bounding box.
    BoundingBoxXMin: double;
    // @name is the minimum Y value for the bounding box.
    BoundingBoxYMin: double;
    // @name is the maximum X value for the bounding box.
    BoundingBoxXMax: double;
    // @name is the maximum Y value for the bounding box.
    BoundingBoxYMax: double;
    // @name is the minimum Z value for the bounding box.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxZMin: double;
    // @name is the maximum Z value for the bounding box.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxZMax: double;
    // @name is the minimum M value.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxMMin: double;
    // @name is the maximum M value.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxMMax: double;
  end;

  // @abstract(@name is the type of
  // TShapefileGeometryReader.@link(TShapefileGeometryReader.OnProgress)
  // It can be used to show a progress bar to the user while
  // reading a Shapefile.)
  TProgressProcedure = procedure(Sender: TObject;
    FractionDone: double) of object;

const
  // Null Shape
  stNull = 0;
  // Point Shape
  stPoint = 1;
  // Polyline Shape
  stPolyLine = 3;
  // Polygon Shape
  stPolygon = 5;
  // MultiPoint Shape
  stMultiPoint = 8;
  // PointZ Shape
  stPointZ = 11;
  // PolyLineZ Shape
  stPolyLineZ = 13;
  // PolygonZ Shape
  stPolygonZ = 15;
  // MultiPointZ Shape
  stMultiPointZ = 18;
  // PointM Shape
  stPointM = 21;
  // PolyLineM Shape
  stPolyLineM = 23;
  // PolygonM Shape
  stPolygonM = 25;
  // MultiPointM Shape
  stMultipointM = 28;
  // MultiPatch Shape
  stMultiPatch = 31;

type
  { @abstract(@name appears at the beginning of each Shape record.)
  @longcode(#
  TShapeRecordHeader = packed record
    RecordNumber: longint;
    ContentLength: longint;
  end;
  #)
  }
  TShapeRecordHeader = packed record
    // @name is the record number in BigEndian format.
    RecordNumber: longint;
    // @name is the length of the shape content measured in 16 bit words
    // @name is in BigEndian format.
    ContentLength: longint;
  end;

  { @abstract(@name is the constant-length part of a null-shape.)
  @longcode(#
  TNullShapeRecord = packed record
    ShapeType: longint;
  end;
  #)
  }
  TNullShapeRecord = packed record
    // The value of ShapeType should be @link(stNull) = 0.
    ShapeType: longint;
  end;

  { @abstract(@name is the constant-length part of a point-shape.)
  @longcode(#
  TPointShapeRecord = packed record
    ShapeType: longint;
    X: double;
    Y: double;
  end;
  #)
  }
  TPointShapeRecord = packed record
    // The value of ShapeType should be @link(stPoint) = 1.
    ShapeType: longint;
    // The X coordinate of the point.
    X: double;
    // The Y coordinate of the point.
    Y: double;
  end;

  { @abstract(@name defines a point in a Shapefile.)
  @longcode(#
  TShapePoint = packed record
    X: double;
    Y: double;
  end;
  #)
  }
  TShapePoint = packed record
    // The X coordinate of the point.
    X: double;
    // The Y coordinate of the point.
    Y: double;
  end;

  { @abstract(@name is the constant-length part of a multi-point shape.)
  @longcode(#
  TMultiPointShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumPoints: longint;
  end;
  #)
  }
  TMultiPointShapeRecord = packed record
    // The value of ShapeType should be @link(stMultiPoint) = 8.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a polyline shape.)
  @longcode(#
  TPolyLineShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolyLineShapeRecord = packed record
    // The value of ShapeType should be @link(stPolyLine) = 3.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a polygon shape.)
  @longcode(#
  TPolygonShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolygonShapeRecord = packed record
    // The value of ShapeType should be @link(stPolygon) = 5.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a PointM shape.)
  @longcode(#
  TPointMShapeRecord = packed record
    ShapeType: longint;
    X: double;
    Y: double;
    M: double;
  end;
  #)
  }
  TPointMShapeRecord = packed record
    // The value of ShapeType should be @link(stPointM) = 21.
    ShapeType: longint;
    // @name is the X coordinate of the point.
    X: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name is the M value of the point.
    M: double;
  end;

  { @abstract(@name defines a PointM.)
  @longcode(#
  TShapePointM = packed record
    X: double;
    Y: double;
    M: double;
  end;
  #)
  }
  TShapePointM = packed record
    // @name is the X coordinate of the point.
    X: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name is the M value of the point.
    M: double;
  end;

  { @abstract(@name is the constant-length part of a MultiPointPointM shape.)
  @longcode(#
  TMultiPointMShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumPoints: longint;
  end;
  #)
  }
  TMultiPointMShapeRecord = packed record
    // The value of ShapeType should be @link(stMultipointM) = 28.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a PolyLineM shape.)
  @longcode(#
  TPolyLineMShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolyLineMShapeRecord = packed record
    // The value of ShapeType should be @link(stPolyLineM) = 23.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a PolygonM shape.)
  @longcode(#
  TPolygonMShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolygonMShapeRecord = packed record
    // The value of ShapeType should be @link(stPolygonM) = 25.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name defines a PointZ shape in a Shapefile.)
  @longcode(#
  TPointZShapeRecord = packed record
    ShapeType: longint;
    X: double;
    Y: double;
    Z: double;
    M: double;
  end;
  #)
  }
  TPointZShapeRecord = packed record
    // The value of ShapeType should be @link(stPointZ) = 11.
    ShapeType: longint;
    // @name is the X coordinate of the point.
    X: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name is the Z coordinate of the point.
    Z: double;
    // @name is the M value of the point.
    M: double;
  end;

  { @abstract(@name is the constant-length part of a MultiPointZ shape.)
  @longcode(#
  TMultiPointZShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumPoints: longint;
  end;
  #)
  }
  TMultiPointZShapeRecord = packed record
    // The value of ShapeType should be @link(stMultiPointZ) = 18.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a PolyLineZ shape.)
  @longcode(#
  TPolyLineZShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolyLineZShapeRecord = packed record
    // The value of ShapeType should be @link(stPolyLineZ) = 13.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

  { @abstract(@name is the constant-length part of a PolygonZ shape.)
  @longcode(#
  TPolygonZShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TPolygonZShapeRecord = packed record
    // The value of ShapeType should be @link(stPolygonZ) = 15.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
  end;

const
  // @name represents a triangle strip.
  // A linked strip of triangles, where every vertex (after the first two)
  // completes a new triangle. A new triangle is always formed by
  // connecting the new vertex with its two immediate predecessors.
  ptTriangleStrip = 0;
  // @name represents a triangle fan.
  // A linked fan of triangles, where every vertex (after the first two)
  // completes a new triangle. A new triangle is always formed by
  // connecting the new vertex with its immediate predecessor and the
  // first vertex of the part.
  ptTriangleFan = 1;
  // @name represents the outer ring of a polygon.
  ptOuterRing = 2;
  // @name represents a hole of a polygon.
  ptInnerRing = 3;
  // @name represents the first ring of a polygon of an unspecified type.
  ptFirstRing = 4;
  // @name represents a ring of a polygon of an unspecified type.
  ptRing = 5;

type
  { @abstract(@name defines a MultiPatch shape in a Shapefile.)
  @longcode(#
  TMultiPatchShapeRecord = packed record
    ShapeType: longint;
    BoundingBox: TBoundingBox;
    NumParts: longint;
    NumPoints: longint;
  end;
  #)
  }
  TMultiPatchShapeRecord = packed record
    // The value of ShapeType should be @link(stMultiPatch) = 31.
    ShapeType: longint;
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
    {  @name: An array of length @link(NumParts). Stores, for each MultiPatch,
        the index of its first point in the points array.
        Array indexes are with respect to 0.    }
//    Parts: array of integer;
    // @name is an array of length NumParts. @name stores the type of each part.
    // Each part can be one of the following
    //
    // @link(ptTriangleStrip) = 0;
    //
    // @link(ptTriangleFan) = 1;
    //
    // @link(ptOuterRing) = 2;
    //
    // @link(ptInnerRing) = 3;
    //
    // @link(ptFirstRing) = 4;
    //
    // @link(ptRing) = 5;
//    PartTypes: array of integer;
    {
    @name is an array of length @link(NumPoints).
    The points for each part in the MultiPatch
    are stored end to end. The points for
    Part 2 follow the points for Part 1, and
    so on. The @link(Parts) array holds
    the array index of the starting point for each
    part. There is no delimiter in the points array between parts.    }
//    Points: array of TShapePoint;
    // The minimum and maximum Z values for the arc
    // stored in the order ZMin, ZMax.
//    ZMin: double;
    // The minimum and maximum Z values for the arc
    // stored in the order ZMin, ZMax.
//    ZMax: double;
    {@name is an array of length @link(NumPoints).
    The Z values for each part in the MultiPatch
    are stored end to end. The Z values
    for Part 2 follow the Z values for Part 1,
    and so on. The @link(Parts) array
    holds the array index of the starting Z value for
    each part. There is no delimiter in the Z value array between parts.}
//    ZArray: array of double;
    // @name is the minimum measure for the MultiPatch.
    // @name is optional
//    MMin: double;
    // @name is the maximum measure for the MultiPatch.
    // @name is optional
//    MMax: double;
    {@name is an array of length @link(NumPoints).
    The measures for each part in the
    MultiPatch are stored end to end. The measures for Part 2 follow the
    measures for Part 1, and so on.
    The @link(Parts) array holds the array index of the
    starting measure for each part. There is no delimiter in the measure array
    between parts.
    @name is optional}
//    MArray: array of double;
  end;

  { @abstract(@name defines a Shape Index record.)
  @longcode(#
  TShapeIndexRecord = packed record
    Offset: longint;
    ContentLength: longint;
  end;
  #)
  }
  TShapeIndexRecord = packed record
    // @name is the starting point of the shape record header
    // measured in 16 bit words
    // @name is in BigEndian format.
    Offset: longint;
    // @name is the length of the shape content
    // measured in 16 bit words
    // @name is in BigEndian format.
    ContentLength: longint;
  end;

  {@abstract(@name represents a shape in an ESRI Shapefile.
  See TShapefileGeometryReader.@link(TShapefileGeometryReader.Items).)}
  TShapeObject = class(TObject)
  private
    // @name represents the bounding box of the shape.
    FBoundingBox: TBoundingBox;
    // The maximum measure for the shape.
    // @name is optional
    FMMax: double;
    // The minimum measure for the shape.
    // @name is optional
    FMMin: double;
    // The maximum Z value for the arc.
    FZMax: double;
    // The minimum Z value for the arc.
    FZMin: double;
    procedure GetMinMax(var MaxValue, MinValue: Double;
      const AnArray: TDoubleDynArray);
    procedure UpdateBoundingBox;
    procedure UpdateZMinMax;
    procedure UpdateMMinMax;
  public
    {@name is an array of length @link(FNumPoints).
    The measures for each part in the
    MultiPatch are stored end to end. The measures for Part 2 follow the
    measures for Part 1, and so on.
    The parts array holds the array index of the
    starting measure for each part. There is no delimiter in the measure array
    between parts.
    @name is optional}
    FMArray: TDoubleDynArray;
    // @name is the number of parts in the Shape.
    FNumParts: integer;
    // @name is the number of points in the Shape.
    FNumPoints: integer;
    {@name is an array of length @link(FNumParts). It stores
        the index of its first point in the points array for each Polyline.
        Array indexes are with respect to 0.    }
    FParts: array of integer;
    // @name is an array of length @link(FNumParts).
    // It stores the type of each part.
    // Each part can be one of the following
    //
    // @link(ptTriangleStrip) = 0;
    //
    // @link(ptTriangleFan) = 1;
    //
    // @link(ptOuterRing) = 2;
    //
    // @link(ptInnerRing) = 3;
    //
    // @link(ptFirstRing) = 4;
    //
    // @link(ptRing) = 5;
    FPartTypes: array of integer;
    {@name is an array of length @link(FNumPoints). The points for
    each part in the MultiPatch
    are stored end to end. The points for Part 2
    follow the points for Part 1, and
    so on. The @link(FParts) array
    holds the array index of the starting point for each
    part. There is no delimiter in the points array between parts.    }
    FPoints: array of TShapePoint;
    // @name indicates the type of shape stored in the Shapefile.
    // It can be any of the following
    // @link(stNull) = 0;
    // @link(stPoint) = 1;
    // @link(stPolyLine) = 3;
    // @link(stPolygon) = 5;
    // @link(stMultiPoint) = 8;
    // @link(stPointZ) = 11;
    // @link(stPolyLineZ) = 13;
    // @link(stPolygonZ) = 15;
    // @link(stMultiPointZ) = 18;
    // @link(stPointM) = 21;
    // @link(stPolyLineM) = 23;
    // @link(stPolygonM) = 25;
    // @link(stMultipointM) = 28;
    // @link(stMultiPatch) = 31;
    FShapeType: longint;
    {@name is an array of length @link(FNumPoints). The Z
    values for each point in the shape
    are stored end to end. The Z values for
    Part 2 follow the Z values for Part 1,
    and so on. The parts array holds the array
    index of the starting Z value for
    each part. There is no delimiter in the Z value array between parts.}
    FZArray: TDoubleDynArray;
    // @name creates an instance of @classname.
    constructor Create;
    property BoundingBox: TBoundingBox read FBoundingBox;
    property ZMin: Double read FZMin;
    property ZMax: Double read FZMax;
    function ShapeLength: double;
  end;

  {@abstract(@name provides a method of reading the geometry part
  of ESRI Shapefiles.  Each shape in the Shapefile is stored in
  a @link(TShapeObject) which can be accessed through the
  @link(TShapefileGeometryReader.Items) property.)}
  TShapefileGeometryReader = class(TObject)
  private
    // See @link(FileHeader).
    FFileHeader: TShapefileHeader;
    // @name stores the @link(TShapeObject)s accessed through @link(Items).
    // @name is instantiated as a TObjectList.
    FShapeObjects: TObjectList<TShapeObject>;
    // See @link(OnProgress).
    FOnProgress: TProgressProcedure;
    FNumberOfPoints: Integer;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItems(const Index: integer): TShapeObject;
  public
    // Count is the number of TShapeObjects that have been read.
    // See @link(ReadFromFile).
    property Count: integer read GetCount;
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    { FileHeader is the @link(TShapefileHeader) at the beginning of the file.
     The most useful thing about FileHeader is ShapeType which should be
     one of:

      @link(stNull) = 0;

      @link(stPoint) = 1;

      @link(stPolyLine) = 3;

      @link(stPolygon) = 5;

      @link(stMultipoint) = 8;

      @link(stPointZ) = 11;

      @link(stPolyLineZ) = 13;

      @link(stPolygonZ) = 15;

      @link(stMultipointZ) = 18;

      @link(stPointM) = 21;

      @link(stPolyLineM) = 23;

      @link(stPolygonM) = 25;

      @link(stMultipointM) = 28;

      @link(stMultiPatch) = 31;
      }
    property FileHeader: TShapefileHeader read FFileHeader;
    // @name provides access to the @link(TShapeObject)s read from
    // the Shapefile with @link(ReadFromFile).
    property Items[const Index: integer]: TShapeObject read GetItems; default;
    // @name reads the geometry from a file named FileName.
    // @SeeAlso(OnProgress)
    // @param(MainFileName is the name of the main geometry file.
    // It has the extension .shp.)
    // @param(IndexFileName is the name of the index file.
    // It has the extension .shx.)
    procedure ReadFromFile(const MainFileName, IndexFileName: string);
    {
      @name is called each time a shape is read from the disk during
      @link(ReadFromFile).
      FractionDone will be the fraction of the file that has been read.
    }
    property OnProgress: TProgressProcedure read FOnProgress write FOnProgress;
    // @name is only valid after @link(ReadFromFile).
    property NumberOfPoints: Integer read FNumberOfPoints;
  end;

  {@abstract(@name provides a method of writing the geometry part
  of ESRI Shapefiles.  Each shape in the Shapefile is stored in
  a @link(TShapeObject) which can be added through the
  @link(AddShape) method.)}
  TShapefileGeometryWriter = class(TObject)
  private
    {@name indicates the type of @link(TShapeObject) that can be stored in
    @classname.  @seealso(TShapeObject.FShapeType TShapeObject.FShapeType)}
    FShapeType: longint;
    // @name stores the @link(TShapeObject)s in @name.
    FShapes: TObjectList<TShapeObject>;
    // See @link(OnProgress).
    FOnProgress: TProgressProcedure;
    // See @link(Capacity).
    function GetCapacity: integer;
    // See @link(Count).
    function GetCount: integer;
    // See @link(Items).
    function GetItem(Index: integer): TShapeObject;
    // See @link(Capacity).
    procedure SetCapacity(const Value: integer);
  public
    {@name creates an instance of @classname.  ShapeType indicates the
    type of @link(TShapeObject) that can be added to @classname through
    @link(AddShape).
    If OwnsShapes is @true, any @link(TShapeObject)s added to the @classname
    through @link(AddShape) will be destroyed when the @classname is destroyed.}
    Constructor Create(ShapeType: longint; OwnsShapes: boolean);
    // @name destroys the current instance of @classname.  Don't call @name.
    // Call Free instead.
    Destructor Destroy; override;
    // @name indicates how many @link(TShapeObject)s can be stored in the
    // @classname without needing to reallocate memory.
    property Capacity: integer read GetCapacity write SetCapacity;
    // @name indicates how many @link(TShapeObject)s are stored in the
    // @classname.
    property Count: integer read GetCount;
    // @name provides access to the @link(TShapeObject)s stored in the
    // @classname.
    property Items[Index: integer]: TShapeObject read GetItem; default;
    // @name adds a @link(TShapeObject) to those stored in the
    // @classname. The @link(TShapeObject.FShapeType TShapeObject.FShapeType)
    // must match the ShapeType in @link(Create).
    procedure AddShape(Shape: TShapeObject);
    {@name writes the Shapes to a Shapefile and Shape index file.  The file
     names of these to files are passed into the procedure.  According to
     the specification, these to names must be identical except for their
     extensions (.shp and .shx). @name does not enforce this or any other
     restrictions on the file names.}
    procedure WriteToFile(const MainFileName, IndexFileName: string);
    {
      @name is called each time a shape is written to the disk during
      @link(WriteToFile).
      FractionDone will be the fraction of the shapes that have been written.
    }
    property OnProgress: TProgressProcedure read FOnProgress write FOnProgress;
  end;

  //enumeration used in variant record
  TBytePos = (bpEndVal, bpByteVal);

  // @name is a pointer to a @link(TDoubleEndianCnvRec).
  PDoubleEndianCnvRec = ^TDoubleEndianCnvRec;

  { @abstract(@name is used in @link(ConvertDouble) to convert
   a double to or from the BigEndian format.)
   @longcode(#
  TDoubleEndianCnvRec = packed record
    case BytePos of
      EndVal: (EndianVal: double);
      ByteVal: (Bytes: array[0..SizeOf(double) - 1] of byte);
  end;
   #)
   }
  TDoubleEndianCnvRec = packed record
    case TBytePos of
      //The value we are trying to convert
      bpEndVal: (EndianVal: double);
      //Overlapping bytes of the double
      bpByteVal: (Bytes: array[0..SizeOf(double) - 1] of byte);
  end;

  // @name is a pointer to a @link(TLongintEndianCnvRec).
  PLongintEndianCnvRec = ^TLongintEndianCnvRec;

  { @abstract(@name is used in @link(ConvertInteger) to convert
   a longint to or from the BigEndian format.)
   @longcode(#
  TDoubleEndianCnvRec = packed record
    case BytePos of
      EndVal: (EndianVal: double);
      ByteVal: (Bytes: array[0..SizeOf(double) - 1] of byte);
  end;
   #)}
  TLongintEndianCnvRec = packed record
    case TBytePos of
      //The value we are trying to convert
      bpEndVal: (EndianVal: longint);
      //Overlapping bytes of the longint
      bpByteVal: (Bytes: array[0..SizeOf(longint) - 1] of byte);
  end;

// @abstract(@name copies @link(TDoubleEndianCnvRec.Bytes)
// in reverse order from Source^ to Dest^.)
// @name is used in @link(ConvertDouble).
procedure SwapDoubleBytes(Dest, Source: PDoubleEndianCnvRec);
// @abstract(@name copies @link(TLongintEndianCnvRec.Bytes)
// in reverse order from Source^ to Dest^.)
// @name is used in @link(ConvertInteger).
procedure SwapLongIntBytes(Dest, Source: PLongintEndianCnvRec);

// @abstract(@name converts Value to or from the BigEndian format.)
// @param(Value is the value to be converted.)
// @returns(Value after being converted to or from the BigEndian format.)
function ConvertDouble(const Value: double): double;
// @abstract(@name converts Value to or from the BigEndian format.)
// @param(Value is the value to be converted.)
// @returns(Value after being converted to or from the BigEndian format.)
function ConvertInteger(const Value: longint): longint;

{ @abstract(@name reads a @link(TShapefileHeader) and returns the type of
   shape in the file.)
 The ShapeType which should be one of:

    @link(stNull) = 0;

    @link(stPoint) = 1;

    @link(stPolyLine) = 3;

    @link(stPolygon) = 5;

    @link(stMultipoint) = 8;

    @link(stPointZ) = 11;

    @link(stPolyLineZ) = 13;

    @link(stPolygonZ) = 15;

    @link(stMultipointZ) = 18;

    @link(stPointM) = 21;

    @link(stPolyLineM) = 23;

    @link(stPolygonM) = 25;

    @link(stMultipointM) = 28;

    @link(stMultiPatch) = 31;
  }
function FileShapeType(const FileName: string): integer;

implementation

uses Contnrs;

resourcestring
  StrTheShapefileMainF = 'The Shapefile main file and index file headers do ' +
  'not match. The Shapefile may be corrupt.';
  StrThereWasAnErrorA = 'There was an error attemtping to read a Shape Geome' +
  'try file. Please check that the Shapefile is valid.';

// http://community.borland.com/article/0,1410,28964,00.html
//A gets B's values swapped

// Dest and Source should not be the same.
procedure SwapDoubleBytes(Dest, Source: PDoubleEndianCnvRec);
var
  i: integer;
begin
  for i := high(Dest.Bytes) downto low(Dest.Bytes) do
    Dest.Bytes[i] := Source.Bytes[High(Dest.Bytes) - i];
end;

// Dest and Source should not be the same.
procedure SwapLongIntBytes(Dest, Source: PLongintEndianCnvRec);
var
  i: integer;
begin
  for i := high(Dest.Bytes) downto low(Dest.Bytes) do
    Dest.Bytes[i] := Source.Bytes[High(Dest.Bytes) - i];
end;

// This converts a double to or from the BigEndian format.
function ConvertDouble(const Value: double): double;
var
  Source, Dest: TDoubleEndianCnvRec;
begin
  Source.EndianVal := Value;
  SwapDoubleBytes(@Dest, @Source);
  result := Dest.EndianVal;
end;

// This converts a longint to or from the BigEndian format.
function ConvertInteger(const Value: longint): longint;
var
  Source, Dest: TLongintEndianCnvRec;
begin
  Source.EndianVal := Value;
  SwapLongIntBytes(@Dest, @Source);
  result := Dest.EndianVal;
end;

{ TShapefileGeometryReader }

constructor TShapefileGeometryReader.Create;
begin
  inherited;
  FShapeObjects := TObjectList<TShapeObject>.Create;
  FNumberOfPoints := 0;
end;

destructor TShapefileGeometryReader.Destroy;
begin
  FShapeObjects.Free;
  inherited;
end;

function TShapefileGeometryReader.GetCount: integer;
begin
  result := FShapeObjects.Count;
end;

function TShapefileGeometryReader.GetItems(const Index: integer): TShapeObject;
begin
  result := FShapeObjects[Index];
end;

procedure TShapefileGeometryReader.ReadFromFile(const MainFileName, IndexFileName: string);
var
  // FileStream is used to read the shape file.
  FileStream: TFileStream;
  IndexFileStream: TFileStream;
  // ShapeType tells what kinds of shape is in the shape file.
  ShapeType: integer;
  // Shape Files start with a TShapeRecordHeader.
  ShapeRecordHeader: TShapeRecordHeader;
  IndexRecordHeader: TShapeIndexRecord;
  // ContentLength is the length of the content of the shape geometry
  // file in bytes.
  ContentLength, Offset: integer;
  // CurrentPosition is the position in FileStream.
  CurrentPosition: integer;
  // ContentEnd indicates where the shapes in the shape file end.
  ContentEnd: integer;
  // NumPoints is the number of points in the current shape.
  NumPoints: Integer;
  // NumParts is the number of parts in the current shape.
  NumParts: integer;
  DummyFileHeader: TShapefileHeader;
  SizeRead: Longint;
  procedure CreateNullShape;
  var
    NullShapeObject: TShapeObject;
  begin
    NullShapeObject := TShapeObject.Create;
    FShapeObjects.Add(NullShapeObject);
    NullShapeObject.FShapeType := stNull;
    SetLength(NullShapeObject.FPoints, 0);
    NullShapeObject.FNumPoints := 0;
    NullShapeObject.FNumParts := 0;
  end;
  procedure ReadNullShape;
  var
    NullShapeObject: TShapeObject;
    Data: TNullShapeRecord;
  begin
    NullShapeObject := TShapeObject.Create;
    FShapeObjects.Add(NullShapeObject);
    FileStream.Read(Data, SizeOf(TNullShapeRecord));
    NullShapeObject.FShapeType := Data.ShapeType;
    SetLength(NullShapeObject.FPoints, 0);
    NullShapeObject.FNumPoints := 0;
    NullShapeObject.FNumParts := 0;
  end;
  procedure ReadPointShape;
  var
    PointShapeObject: TShapeObject;
    Data: TPointShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPointShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PointShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointShapeObject);
    PointShapeObject.FShapeType := Data.ShapeType;
    SetLength(PointShapeObject.FPoints, 1);
    PointShapeObject.FPoints[0].X := Data.X;
    PointShapeObject.FPoints[0].Y := Data.Y;
    PointShapeObject.FNumPoints := 1;
    PointShapeObject.FNumParts := 1;
    Inc(FNumberOfPoints);
  end;
  procedure ReadPolyLine;
  var
    PolyLineShapeObject: TShapeObject;
    Data: TPolyLineShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolyLineShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolyLineShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineShapeObject);
    Move(Data.BoundingBox, PolyLineShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    PolyLineShapeObject.FShapeType := Data.ShapeType;
    NumPoints := Data.NumPoints;
    NumParts := Data.NumParts;
    PolyLineShapeObject.FNumPoints := NumPoints;
    PolyLineShapeObject.FNumParts := NumParts;
    SetLength(PolyLineShapeObject.FParts, NumParts);
    SetLength(PolyLineShapeObject.FPoints, NumPoints);
    if NumParts > 0 then
    begin
      FileStream.Read(PolyLineShapeObject.FParts[0],
        NumParts * SizeOf(Integer));
    end;
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolyLineShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadPolygon;
  var
    PolygonShapeObject: TShapeObject;
    Data: TPolygonShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolygonShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolygonShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonShapeObject);
    Move(Data.BoundingBox, PolygonShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    NumPoints := Data.NumPoints;
    Assert(NumPoints >= 0);
    NumParts := Data.NumParts;
    PolygonShapeObject.FShapeType := Data.ShapeType;
    PolygonShapeObject.FNumPoints := Data.NumPoints;
    PolygonShapeObject.FNumParts := Data.NumParts;
    SetLength(PolygonShapeObject.FParts, NumParts);
    SetLength(PolygonShapeObject.FPoints, NumPoints);
    if NumParts > 0 then
    begin
      FileStream.Read(PolygonShapeObject.FParts[0],
        NumParts * SizeOf(longint));
    end;
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolygonShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadMultiPoint;
  var
    MultiPointShapeObject: TShapeObject;
    Data: TMultiPointShapeRecord;
    Index: Integer;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TMultiPointShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    MultiPointShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointShapeObject);
    Move(Data.BoundingBox, MultiPointShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    NumPoints := Data.NumPoints;
    MultiPointShapeObject.FNumPoints := NumPoints;
    MultiPointShapeObject.FNumParts := NumPoints;
    SetLength(MultiPointShapeObject.FParts, NumPoints);
    for Index := 0 to NumPoints - 1 do
    begin
      MultiPointShapeObject.FParts[Index] := Index;
    end;
    MultiPointShapeObject.FShapeType := Data.ShapeType;
    SetLength(MultiPointShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(MultiPointShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadPointZ;
  var
    PointZShapeObject: TShapeObject;
    Data: TPointZShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPointZShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PointZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointZShapeObject);
    PointZShapeObject.FShapeType := Data.ShapeType;
    SetLength(PointZShapeObject.FPoints, 1);
    PointZShapeObject.FPoints[0].X := Data.X;
    PointZShapeObject.FPoints[0].Y := Data.Y;
    SetLength(PointZShapeObject.FMArray, 1);
    PointZShapeObject.FMArray[0] := Data.M;
    PointZShapeObject.FMMin := Data.M;
    PointZShapeObject.FMMax := Data.M;
    SetLength(PointZShapeObject.FZArray, 1);
    PointZShapeObject.FZArray[0] := Data.Z;
    PointZShapeObject.FZMin := Data.Z;
    PointZShapeObject.FZMax := Data.Z;
    PointZShapeObject.FNumPoints := 1;
    PointZShapeObject.FNumParts := 0;
    Inc(FNumberOfPoints);
  end;
  procedure ReadPolyLineZ;
  var
    PolyLineZShapeObject: TShapeObject;
    Data: TPolyLineZShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolyLineZShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolyLineZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineZShapeObject);
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolyLineZShapeObject.FNumPoints := NumPoints;
    PolyLineZShapeObject.FNumParts := NumParts;
    PolyLineZShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, PolyLineZShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));

    SetLength(PolyLineZShapeObject.FParts, NumParts);
    if NumParts > 0 then
    begin
      FileStream.Read(PolyLineZShapeObject.FParts[0],
        NumParts * SizeOf(longint));
    end;
    SetLength(PolyLineZShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolyLineZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    FileStream.Read(PolyLineZShapeObject.FZMin, SizeOf(double));
    FileStream.Read(PolyLineZShapeObject.FZMax, SizeOf(double));
    SetLength(PolyLineZShapeObject.FZArray, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(PolyLineZShapeObject.FZArray[0],
        NumPoints * SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolyLineZShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolyLineZShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(PolyLineZShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(PolyLineZShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadPolygonZ;
  var
    PolygonZShapeObject: TShapeObject;
    Data: TPolygonZShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolygonZShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolygonZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonZShapeObject);
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolygonZShapeObject.FShapeType := Data.ShapeType;
    PolygonZShapeObject.FNumParts := NumParts;
    PolygonZShapeObject.FNumPoints := NumPoints;
    Move(Data.BoundingBox, PolygonZShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(PolygonZShapeObject.FParts, NumParts);
    if NumParts > 0 then
    begin
      FileStream.Read(PolygonZShapeObject.FParts[0],
        NumParts * SizeOf(longint));
    end;
    SetLength(PolygonZShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolygonZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    FileStream.Read(PolygonZShapeObject.FZMin, SizeOf(double));
    FileStream.Read(PolygonZShapeObject.FZMax, SizeOf(double));
    SetLength(PolygonZShapeObject.FZArray, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(PolygonZShapeObject.FZArray[0],
        NumPoints * SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolygonZShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolygonZShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(PolygonZShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(PolygonZShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadMultipPointZ;
  var
    MultiPointZShapeObject: TShapeObject;
    Data: TMultiPointZShapeRecord;
    Index: Integer;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TMultiPointZShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    MultiPointZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointZShapeObject);
    NumPoints := Data.NumPoints;
    NumParts := 0;
    MultiPointZShapeObject.FNumPoints := NumPoints;
    MultiPointZShapeObject.FNumParts := NumPoints;
    SetLength(MultiPointZShapeObject.FParts, NumPoints);
    for Index := 0 to NumPoints - 1 do
    begin
      MultiPointZShapeObject.FParts[Index] := Index;
    end;
    MultiPointZShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, MultiPointZShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(MultiPointZShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(MultiPointZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    FileStream.Read(MultiPointZShapeObject.FZMin, SizeOf(double));
    FileStream.Read(MultiPointZShapeObject.FZMax, SizeOf(double));
    SetLength(MultiPointZShapeObject.FZArray, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(MultiPointZShapeObject.FZArray[0],
        NumPoints * SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPointZShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPointZShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(MultiPointZShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(MultiPointZShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadPointM;
  var
    PointMShapeObject: TShapeObject;
    Data: TPointMShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPointMShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PointMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointMShapeObject);
    PointMShapeObject.FNumPoints := 1;
    SetLength(PointMShapeObject.FPoints, 1);
    PointMShapeObject.FPoints[0].X := Data.X;
    PointMShapeObject.FPoints[0].Y := Data.Y;
    PointMShapeObject.FShapeType := ShapeType;
    SetLength(PointMShapeObject.FMArray, 1);
    PointMShapeObject.FMArray[0] := Data.M;
    PointMShapeObject.FMMin := Data.M;
    PointMShapeObject.FMMax := Data.M;
    SetLength(PointMShapeObject.FParts, 0);
    PointMShapeObject.FNumParts := 0;
    SetLength(PointMShapeObject.FZArray, 0);
    Inc(FNumberOfPoints);
  end;
  procedure ReadPolyLineM;
  var
    PolyLineMShapeObject: TShapeObject;
    Data: TPolyLineMShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolyLineMShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolyLineMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineMShapeObject);
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolyLineMShapeObject.FNumPoints := NumPoints;
    PolyLineMShapeObject.FNumParts := NumParts;
    PolyLineMShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, PolyLineMShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(PolyLineMShapeObject.FParts, NumParts);
    if NumParts > 0 then
    begin
      FileStream.Read(PolyLineMShapeObject.FParts[0],
        NumParts * SizeOf(longint));
    end;
    SetLength(PolyLineMShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolyLineMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolyLineMShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolyLineMShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(PolyLineMShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(PolyLineMShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    SetLength(PolyLineMShapeObject.FZArray, 0);
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadPolygonM;
  var
    PolygonMShapeObject: TShapeObject;
    Data: TPolygonMShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TPolygonMShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    PolygonMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonMShapeObject);
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolygonMShapeObject.FNumPoints := NumPoints;
    PolygonMShapeObject.FNumParts := NumParts;
    PolygonMShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, PolygonMShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(PolygonMShapeObject.FParts, NumParts);
    if NumParts > 0 then
    begin
      FileStream.Read(PolygonMShapeObject.FParts[0],
        NumParts * SizeOf(longint));
    end;
    SetLength(PolygonMShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(PolygonMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolygonMShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(PolygonMShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(PolygonMShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(PolygonMShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    SetLength(PolygonMShapeObject.FZArray, 0);
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadMultiPointM;
  var
    MultiPointMShapeObject: TShapeObject;
    Data: TMultiPointMShapeRecord;
    Index: Integer;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TMultiPointMShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    MultiPointMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointMShapeObject);
    NumPoints := Data.NumPoints;
    NumParts := 0;
    MultiPointMShapeObject.FNumPoints := NumPoints;
    MultiPointMShapeObject.FNumParts := NumPoints;
    SetLength(MultiPointMShapeObject.FParts, NumPoints);
    for Index := 0 to NumPoints - 1 do
    begin
      MultiPointMShapeObject.FParts[Index] := Index;
    end;
    MultiPointMShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, MultiPointMShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(MultiPointMShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(MultiPointMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPointMShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPointMShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(MultiPointMShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(MultiPointMShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    SetLength(MultiPointMShapeObject.FZArray, 0);
    SetLength(MultiPointMShapeObject.FParts, 0);
    Inc(FNumberOfPoints, NumPoints);
  end;
  procedure ReadMultiPatch;
  var
    MultiPatchShapeObject: TShapeObject;
    Data: TMultiPatchShapeRecord;
    CurrentPostion: Int64;
  begin
    CurrentPostion := FileStream.Position;
    FileStream.Read(Data, SizeOf(TMultiPatchShapeRecord));
    if Data.ShapeType = stNull then
    begin
      FileStream.Position := CurrentPostion;
      ReadNullShape;
      Exit;
    end;
    MultiPatchShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPatchShapeObject);
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    MultiPatchShapeObject.FNumPoints := NumPoints;
    MultiPatchShapeObject.FNumParts := NumParts;
    MultiPatchShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, MultiPatchShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(MultiPatchShapeObject.FParts, NumParts);
    SetLength(MultiPatchShapeObject.FPartTypes, NumParts);
    if NumParts > 0 then
    begin
      FileStream.Read(MultiPatchShapeObject.FParts[0],
        NumParts * SizeOf(longint));
      FileStream.Read(MultiPatchShapeObject.FPartTypes[0],
        NumParts * SizeOf(longint));
    end;
    SetLength(MultiPatchShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      SizeRead := FileStream.Read(MultiPatchShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
      if (NumPoints * SizeOf(TShapePoint) <> SizeRead) then
      begin
        raise EInvalidShapeFile.Create('Invalid Shapefile');
      end;
    end;
    FileStream.Read(MultiPatchShapeObject.FZMin, SizeOf(double));
    FileStream.Read(MultiPatchShapeObject.FZMax, SizeOf(double));
    SetLength(MultiPatchShapeObject.FZArray, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(MultiPatchShapeObject.FZArray[0],
        NumPoints * SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPatchShapeObject.FMMin, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      FileStream.Read(MultiPatchShapeObject.FMMax, SizeOf(double));
    end;
    if FileStream.Position < ContentEnd then
    begin
      SetLength(MultiPatchShapeObject.FMArray, NumPoints);
      if NumPoints > 0 then
      begin
        FileStream.Read(MultiPatchShapeObject.FMArray[0],
          NumPoints * SizeOf(double));
      end;
    end;
    Inc(FNumberOfPoints, NumPoints);
  end;
begin
  FileStream := TFileStream.Create(MainFileName, fmOpenRead or fmShareDenyWrite);
  try
    IndexFileStream := TFileStream.Create(IndexFileName, fmOpenRead or fmShareDenyWrite);
    try
      FileStream.Read(FFileHeader, SizeOf(TShapefileHeader));
      IndexFileStream.Read(DummyFileHeader, SizeOf(TShapefileHeader));

      if FFileHeader.FileCode <> DummyFileHeader.FileCode then
      begin
        EMismatchHeader.Create(StrTheShapefileMainF);
      end;

      ShapeType := FFileHeader.ShapeType;
      while IndexFileStream.Position < IndexFileStream.Size do
      begin
        IndexFileStream.Read(IndexRecordHeader, SizeOf(TShapeIndexRecord));

        Offset := ConvertInteger(IndexRecordHeader.Offset) *2;
        FileStream.Position := Offset;

        FileStream.Read(ShapeRecordHeader, SizeOf(TShapeRecordHeader));
        CurrentPosition := FileStream.Position;
        if Assigned(OnProgress) then
        begin
          OnProgress(self, CurrentPosition / FileStream.Size);
        end;

        // ShapeRecordHeader.ContentLength is measured in 16 bit words = 2 bytes.
        ContentLength := ConvertInteger(ShapeRecordHeader.ContentLength) * 2;
        ContentEnd := CurrentPosition + ContentLength;
        if (ContentEnd > FileStream.Size) then
        begin
          CreateNullShape;
        end
        else
        begin
          if FileStream.Position < FileStream.Size then
          begin
            case ShapeType of
              stNull:
                begin
                  ReadNullShape;
                end;
              stPoint:
                begin
                  ReadPointShape;
                end;
              stPolyLine:
                begin
                  ReadPolyLine;
                end;
              stPolygon:
                begin
                  ReadPolygon;
                end;
              stMultipoint:
                begin
                  ReadMultiPoint;
                end;
              stPointZ:
                begin
                  ReadPointZ;
                end;
              stPolyLineZ:
                begin
                  ReadPolyLineZ;
                end;
              stPolygonZ:
                begin
                  ReadPolygonZ;
                end;
              stMultipointZ:
                begin
                  ReadMultipPointZ;
                end;
              stPointM:
                begin
                  ReadPointM;
                end;
              stPolyLineM:
                begin
                  ReadPolyLineM;
                end;
              stPolygonM:
                begin
                  ReadPolygonM;
                end;
              stMultipointM:
                begin
                  ReadMultiPointM;
                end;
              stMultiPatch:
                begin
                  ReadMultiPatch;
                end;
            else
              raise EInvalidShapeFile.Create(StrThereWasAnErrorA);
            end;
          end;
        end;
      end;
    finally
      IndexFileStream.Free;
    end;
  finally
    FileStream.Free;
  end;
end;

{ TShapeObject }

constructor TShapeObject.Create;
begin
  inherited;
  SetLength(FPoints, 0);
  SetLength(FParts, 0);
  SetLength(FMArray, 0);
  SetLength(FZArray, 0);
  SetLength(FPartTypes, 0);
  FNumPoints := 0;
  FNumParts := 0;
end;

function FileShapeType(const FileName: string): integer;
var
  // FileStream is used to read the shape file.
  FileStream: TFileStream;
  FileHeader: TShapefileHeader;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FileStream.Read(FileHeader, SizeOf(TShapefileHeader));
    result := FileHeader.ShapeType;
  finally
    FileStream.Free;
  end;
end;

procedure TShapeObject.UpdateZMinMax;
begin
  GetMinMax(FZMax, FZMin, FZArray);
end;

procedure TShapeObject.UpdateMMinMax;
begin
  GetMinMax(FMMax, FMMin, FMArray);
end;

procedure TShapeObject.UpdateBoundingBox;
var
  Index: Integer;
begin
  Assert(Length(FPoints) = FNumPoints);
  if FNumPoints = 0 then
  begin
    FShapeType := stNull;
  end;
  if FShapeType = stNull then
  begin
    FBoundingBox.XMin := 0;
    FBoundingBox.XMax := 0;
    FBoundingBox.YMin := 0;
    FBoundingBox.YMax := 0;
    Exit;
  end;
  Assert(FNumPoints > 0);
  FBoundingBox.XMin := FPoints[0].X;
  FBoundingBox.XMax := FPoints[0].X;
  FBoundingBox.YMin := FPoints[0].Y;
  FBoundingBox.YMax := FPoints[0].Y;
  for Index := 1 to FNumPoints - 1 do
  begin
    if FBoundingBox.XMin > FPoints[Index].X then
    begin
      FBoundingBox.XMin := FPoints[Index].X;
    end
    else if FBoundingBox.XMax < FPoints[Index].X then
    begin
      FBoundingBox.XMax := FPoints[Index].X;
    end;
    if FBoundingBox.YMin > FPoints[Index].Y then
    begin
      FBoundingBox.YMin := FPoints[Index].Y;
    end
    else if FBoundingBox.YMax < FPoints[Index].Y then
    begin
      FBoundingBox.YMax := FPoints[Index].Y;
    end;
  end;
end;

procedure TShapeObject.GetMinMax(var MaxValue, MinValue: Double;
  const AnArray: TDoubleDynArray);
var
  Index: Integer;
begin
  if Length(AnArray) = 0 then
  begin
    MinValue := 0;
    MaxValue := 0;
  end
  else
  begin
    MinValue := AnArray[0];
    MaxValue := MinValue;
    for Index := 1 to Length(AnArray) - 1 do
    begin
      if MinValue > AnArray[Index] then
      begin
        MinValue := AnArray[Index];
      end
      else if MaxValue < AnArray[Index] then
      begin
        MaxValue := AnArray[Index];
      end;
    end;
  end;
end;

function TShapeObject.ShapeLength: double;
var
  PartIndex: Integer;
  StartIndex: Integer;
  StopIndex: Integer;
  StartPoint: TShapePoint;
  StopPoint: TShapePoint;
  PointIndex: Integer;
begin
  result := 0;
  for PartIndex := 0 to FNumParts - 1 do
  begin
    StartIndex := FParts[PartIndex];
    if PartIndex = FNumParts - 1 then
    begin
      StopIndex := FNumPoints-1;
    end
    else
    begin
      StopIndex := FParts[PartIndex+1];
    end;
    for PointIndex := StartIndex+1 to StopIndex do
    begin
      StartPoint := FPoints[PointIndex-1];
      StopPoint := FPoints[PointIndex];
      result := result + Sqrt(Sqr(StopPoint.X - StartPoint.X)
        + Sqr(StopPoint.Y - StartPoint.Y));
    end;
  end;
end;

{ TShapefileGeometryWriter }

procedure TShapefileGeometryWriter.AddShape(Shape: TShapeObject);
begin
  FShapes.Add(Shape);
  Assert((Shape.FShapeType = FShapeType) or (Shape.FShapeType = stNull));
end;

constructor TShapefileGeometryWriter.Create(ShapeType: integer;
  OwnsShapes: boolean);
begin
  Assert(ShapeType in [stNull, stPoint, stPolyLine, stPolygon,
    stMultiPoint, stPointZ, stPolyLineZ, stPolygonZ, stMultiPointZ,
    stPointM, stPolyLineM, stPolygonM, stMultipointM, stMultiPatch]);
  FShapeType := ShapeType;
  FShapes := TObjectList<TShapeObject>.Create(OwnsShapes);
end;

destructor TShapefileGeometryWriter.Destroy;
begin
  FShapes.Free;
  inherited;
end;

function TShapefileGeometryWriter.GetCapacity: integer;
begin
  result := FShapes.Capacity;
end;

function TShapefileGeometryWriter.GetCount: integer;
begin
  result := FShapes.Count;
end;

function TShapefileGeometryWriter.GetItem(Index: integer): TShapeObject;
begin
  result := FShapes[Index];
end;

procedure TShapefileGeometryWriter.SetCapacity(const Value: integer);
begin
  FShapes.Capacity := Value;
end;

procedure TShapefileGeometryWriter.WriteToFile(const MainFileName,
  IndexFileName: string);
var
  ShapeMemoryStream: TMemoryStream;
  IndexMemoryStream: TMemoryStream;
  Index: Integer;
  Shape: TShapeObject;
  ShapeStream: TMemoryStream;
  procedure WriteNullShape;
  var
    ARecord: TNullShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ShapeStream.Write(ARecord, SizeOf(TNullShapeRecord));
  end;
  procedure WritePointShape;
  var
    ARecord: TPointShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    Assert(Shape.FNumPoints >= 1);
    ARecord.X := Shape.FPoints[0].X;
    ARecord.Y := Shape.FPoints[0].Y;
    ShapeStream.Write(ARecord, SizeOf(TPointShapeRecord));
  end;
  procedure WritePolyLineShape;
  var
    ARecord: TPolyLineShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolyLineShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure WritePolygonShape;
  var
    ARecord: TPolygonShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolygonShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure WriteMultiPointShape;
  var
    ARecord: TMultiPointShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TMultiPointShapeRecord));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure WritePointZShape;
  var
    ARecord: TPointZShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    Assert(Shape.FNumPoints >= 1);
    ARecord.X := Shape.FPoints[0].X;
    ARecord.Y := Shape.FPoints[0].Y;
    ARecord.Z := Shape.FZArray[0];
    ARecord.M := Shape.FMArray[0];
    ShapeStream.Write(ARecord, SizeOf(TPointZShapeRecord));
  end;
  procedure WritePolylineZShape;
  var
    ARecord: TPolyLineZShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolyLineZShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;

    ShapeStream.Write(Shape.FZMin, SizeOf(double));
    ShapeStream.Write(Shape.FZMax, SizeOf(double));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FZArray[0],
        ARecord.NumPoints * SizeOf(double));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WritePolygonZShape;
  var
    ARecord: TPolygonZShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolygonZShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    ShapeStream.Write(Shape.FZMin, SizeOf(double));
    ShapeStream.Write(Shape.FZMax, SizeOf(double));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FZArray[0],
        ARecord.NumPoints * SizeOf(double));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WriteMultipPointZ;
  var
    ARecord: TMultiPointZShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TMultiPointZShapeRecord));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    ShapeStream.Write(Shape.FZMin, SizeOf(double));
    ShapeStream.Write(Shape.FZMax, SizeOf(double));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FZArray[0],
        ARecord.NumPoints * SizeOf(double));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WritePointMShape;
  var
    ARecord: TPointMShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    Assert(Shape.FNumPoints >= 1);
    ARecord.X := Shape.FPoints[0].X;
    ARecord.Y := Shape.FPoints[0].Y;
    ARecord.M := Shape.FMArray[0];
    ShapeStream.Write(ARecord, SizeOf(TPointMShapeRecord));
  end;
  procedure WritePolylineMShape;
  var
    ARecord: TPolyLineMShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolyLineMShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WritePolygonMShape;
  var
    ARecord: TPolygonMShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TPolygonMShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(Integer));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WriteMultiPointMShape;
  var
    ARecord: TMultiPointMShapeRecord;
  begin
    ARecord.ShapeType := FShapeType;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ARecord.NumPoints := Shape.FNumPoints;
    ShapeStream.Write(ARecord, SizeOf(TMultiPointMShapeRecord));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
  procedure WriteMultiPatchShape;
  var
    ARecord: TMultiPatchShapeRecord;
  begin
    ARecord.ShapeType := Shape.FShapeType;
    ARecord.NumPoints := Shape.FNumPoints;
    ARecord.NumParts := Shape.FNumParts;
    ARecord.BoundingBox := Shape.FBoundingBox;
    ShapeStream.Write(ARecord, SizeOf(TMultiPatchShapeRecord));
    if ARecord.NumParts > 0 then
    begin
      ShapeStream.Write(Shape.FParts[0],
        ARecord.NumParts * SizeOf(longint));
      ShapeStream.Write(Shape.FPartTypes[0],
        ARecord.NumParts * SizeOf(longint));
    end;
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FPoints[0],
        ARecord.NumPoints * SizeOf(TShapePoint));
    end;
    ShapeStream.Write(Shape.FZMin, SizeOf(double));
    ShapeStream.Write(Shape.FZMax, SizeOf(double));
    if ARecord.NumPoints > 0 then
    begin
      ShapeStream.Write(Shape.FZArray[0],
        ARecord.NumPoints * SizeOf(double));
    end;
    if Length(Shape.FMArray) > 0 then
    begin
      ShapeStream.Write(Shape.FMMin, SizeOf(double));
      ShapeStream.Write(Shape.FMMax, SizeOf(double));
      if ARecord.NumPoints > 0 then
      begin
        ShapeStream.Write(Shape.FMArray[0],
          ARecord.NumPoints * SizeOf(double));
      end;
    end;
  end;
var
  ShapeRecordHeader: TShapeRecordHeader;
  IndexRecordHeader: TShapeIndexRecord;
  ShapeFileHeader: TShapefileHeader;
  IndexFileHeader: TShapefileHeader;
  Offset: integer;
  MainFileStream : TFileStream;
  IndexFileStream : TFileStream;
  FirstFound: Boolean;
begin
  ShapeMemoryStream := TMemoryStream.Create;
  IndexMemoryStream := TMemoryStream.Create;
  ShapeStream := TMemoryStream.Create;
  try
    ShapeFileHeader.Unused1 := 0;
    ShapeFileHeader.Unused2 := 0;
    ShapeFileHeader.Unused3 := 0;
    ShapeFileHeader.Unused4 := 0;
    ShapeFileHeader.Unused5 := 0;

    Offset := SizeOf(TShapefileHeader);
    ShapeMemoryStream.Position := 0;
    FirstFound := False;
    for Index := 0 to Count - 1 do
    begin
      if Assigned(FOnProgress) then
      begin
        FOnProgress(self, Index/Count);
      end;
      Shape := Items[Index];
      Shape.UpdateBoundingBox;
      Shape.UpdateZMinMax;
      Shape.UpdateMMinMax;

      if Shape.FShapeType <> stNull then
      begin
        if not FirstFound then
        begin
          ShapeFileHeader.BoundingBoxXMin := Shape.FBoundingBox.XMin;
          ShapeFileHeader.BoundingBoxYMin := Shape.FBoundingBox.YMin;
          ShapeFileHeader.BoundingBoxXMax := Shape.FBoundingBox.XMax;
          ShapeFileHeader.BoundingBoxYMax := Shape.FBoundingBox.YMax;
          ShapeFileHeader.BoundingBoxZMin := Shape.FZMin;
          ShapeFileHeader.BoundingBoxZMax := Shape.FZMax;
          ShapeFileHeader.BoundingBoxMMin := Shape.FMMin;
          ShapeFileHeader.BoundingBoxMMax := Shape.FMMax;
          FirstFound := True;
        end
        else
        begin
          if ShapeFileHeader.BoundingBoxXMin > Shape.FBoundingBox.XMin then
          begin
            ShapeFileHeader.BoundingBoxXMin := Shape.FBoundingBox.XMin;
          end;
          if ShapeFileHeader.BoundingBoxYMin > Shape.FBoundingBox.YMin then
          begin
            ShapeFileHeader.BoundingBoxYMin := Shape.FBoundingBox.YMin;
          end;
          if ShapeFileHeader.BoundingBoxXMax < Shape.FBoundingBox.XMax then
          begin
            ShapeFileHeader.BoundingBoxXMax := Shape.FBoundingBox.XMax;
          end;
          if ShapeFileHeader.BoundingBoxYMax < Shape.FBoundingBox.YMax then
          begin
            ShapeFileHeader.BoundingBoxYMax := Shape.FBoundingBox.YMax;
          end;
          if ShapeFileHeader.BoundingBoxZMin > Shape.FZMin then
          begin
            ShapeFileHeader.BoundingBoxZMin := Shape.FZMin;
          end;
          if ShapeFileHeader.BoundingBoxZMax < Shape.FZMax then
          begin
            ShapeFileHeader.BoundingBoxZMax := Shape.FZMax;
          end;
          if ShapeFileHeader.BoundingBoxMMin > Shape.FMMin then
          begin
            ShapeFileHeader.BoundingBoxMMin := Shape.FMMin;
          end;
          if ShapeFileHeader.BoundingBoxMMax < Shape.FMMax then
          begin
            ShapeFileHeader.BoundingBoxMMax := Shape.FMMax;
          end;
        end;
      end;

      ShapeStream.Clear;
      case FShapeType of
        stNull:
          begin
            WriteNullShape;
          end;
        stPoint:
          begin
            WritePointShape;
          end;
        stPolyLine:
          begin
            WritePolyLineShape;
          end;
        stPolygon:
          begin
            WritePolygonShape;
          end;
        stMultiPoint:
          begin
            WriteMultiPointShape;
          end;
        stPointZ:
          begin
            WritePointZShape;
          end;
        stPolyLineZ:
          begin
            WritePolylineZShape;
          end;
        stPolygonZ:
          begin
            WritePolygonZShape;
          end;
        stMultiPointZ:
          begin
            WriteMultipPointZ;
          end;
        stPointM:
          begin
            WritePointMShape;
          end;
        stPolyLineM:
          begin
            WritePolylineMShape;
          end;
        stPolygonM:
          begin
            WritePolygonMShape;
          end;
        stMultipointM:
          begin
            WriteMultiPointMShape;
          end;
        stMultiPatch:
          begin
            WriteMultiPatchShape;
          end;
        else Assert(False);
      end;
      ShapeRecordHeader.RecordNumber := ConvertInteger(Index+1);
      ShapeRecordHeader.ContentLength := ConvertInteger(ShapeStream.Size div 2);

      IndexRecordHeader.Offset := ConvertInteger(Offset div 2);
      IndexRecordHeader.ContentLength := ConvertInteger(ShapeStream.Size div 2);
      Offset := Offset + ShapeStream.Size + SizeOf(TShapeRecordHeader);

      ShapeStream.Position := 0;

      ShapeMemoryStream.Write(ShapeRecordHeader, SizeOf(TShapeRecordHeader));
      IndexMemoryStream.Write(IndexRecordHeader, SizeOf(TShapeIndexRecord));

      ShapeStream.SaveToStream(ShapeMemoryStream);
    end;
    ShapeFileHeader.FileCode := ConvertInteger(9994);
    ShapeFileHeader.FileLength := ConvertInteger((SizeOf(TShapefileHeader)
      + ShapeMemoryStream.Size) div 2);
    ShapeFileHeader.Version := 1000;
    ShapeFileHeader.ShapeType := FShapeType;

    IndexFileHeader := ShapeFileHeader;
    IndexFileHeader.FileLength := ConvertInteger((SizeOf(TShapefileHeader)
      + IndexMemoryStream.Size) div 2);

    MainFileStream := TFileStream.Create(MainFileName,
      fmCreate or fmShareDenyWrite);
    try
      MainFileStream.Write(ShapeFileHeader, SizeOf(TShapefileHeader));
      ShapeMemoryStream.SaveToStream(MainFileStream);
    finally
      MainFileStream.Free;
    end;

    IndexFileStream := TFileStream.Create(IndexFileName,
      fmCreate or fmShareDenyWrite);
    try
      IndexFileStream.Write(IndexFileHeader, SizeOf(TShapefileHeader));
      IndexMemoryStream.SaveToStream(IndexFileStream);
    finally
      IndexFileStream.Free;
    end;
    if Assigned(FOnProgress) then
    begin
      FOnProgress(self, 1);
    end;
  finally
    ShapeStream.Free;
    ShapeMemoryStream.Free;
    IndexMemoryStream.Free;
  end;
end;

end.

