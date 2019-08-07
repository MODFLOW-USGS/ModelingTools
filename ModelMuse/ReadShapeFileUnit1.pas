{@abstract(@name defines @link(TShapeGeometryFile)
  which provides a method of reading the geometry part
  of ESRI Shapefiles.  Each shape in the Shapefile is stored in
  a @link(TShapeObject) which can be accessed through the
  TShapeGeometryFile.@link(TShapeGeometryFile.Items) property.)

  See http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf for a
  description of the Shapefile format.}
unit ReadShapeFileUnit1;

interface

uses SysUtils, Classes;

type
 { @abstract(@name represents the bounding box or of all the shapes
   in a ShapeFile.)
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
  TShapeFileHeader = packed record
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
  TShapeFileHeader = packed record
    // @name is a BigEndian value. It should be 9994.
    FileCode: longint;
    // @name: longint;
    // @name is an unused value in BigEndian format.
    Unused1: longint;
    // @name: longint;
    // @name is an unused value in BigEndian format.
    Unused2: longint;
    // @name: longint;
    // @name is an unused value in BigEndian format.
    Unused3: longint;
    // @name: longint;
    // @name is an unused value in BigEndian format.
    Unused4: longint;
    // @name: longint;
    // @name is an unused value in BigEndian format.
    Unused5: longint;
    // @name: longint;
    // @name is the file length measured in 16 bit words
    // @name is in BigEndian format.
    FileLength: longint;
    // @name: longint;
    // @name is the Shapefile version number. It should be 1000.
    Version: longint;
    // @name: longint;
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
    // @name: double;
    // @name is the minimum X value for the bounding box.
    BoundingBoxXMin: double;
    // @name: double;
    // @name is the minimum Y value for the bounding box.
    BoundingBoxYMin: double;
    // @name: double;
    // @name is the maximum X value for the bounding box.
    BoundingBoxXMax: double;
    // @name: double;
    // @name is the maximum Y value for the bounding box.
    BoundingBoxYMax: double;
    // @name: double;
    // @name is the minimum Z value for the bounding box.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxZMin: double;
    // @name: double;
    // @name is the maximum Z value for the bounding box.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxZMax: double;
    // @name: double;
    // @name is the minimum M value.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxMMin: double;
    // @name: double;
    // @name is the maximum M value.
    // @name is unused, with value 0.0, if the shape is not Measured or Z type
    BoundingBoxMMax: double;
  end;

  // @abstract(@name is the type of
  // TShapeGeometryFile.@link(TShapeGeometryFile.OnProgress)
  // It can be used to show a progress bar to the user while
  // reading a ShapeFile.)
  TProgressProcedure = procedure(Sender: TObject;
    FractionDone: double) of object;

const
  // Null Shape
  stNull = 0;
  // Point Shape
  stPoint = 1;
  // PolyLine Shape
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
    // @name: longint;
    // @name is the record number in BigEndian format.
    RecordNumber: longint;
    // @name: longint;
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
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPoint) = 1.
    ShapeType: longint;
    // @name: double;
    // The X coordinate of the point.
    X: double;
    // @name: double;
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
    // @name: double;
    // The X coordinate of the point.
    X: double;
    // @name: double;
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
    // @name: longint;
    // The value of ShapeType should be @link(stMultiPoint) = 8.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolyLine) = 3.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolygon) = 5.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPointM) = 21.
    ShapeType: longint;
    // @name: double;
    // @name is the X coordinate of the point.
    X: double;
    // @name: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name: double;
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
    // @name: double;
    // @name is the X coordinate of the point.
    X: double;
    // @name: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name: double;
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
    // @name: longint;
    // The value of ShapeType should be @link(stMultipointM) = 28.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolyLineM) = 23.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolygonM) = 25.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPointZ) = 11.
    ShapeType: longint;
    // @name: double;
    // @name is the X coordinate of the point.
    X: double;
    // @name: double;
    // @name is the Y coordinate of the point.
    Y: double;
    // @name: double;
    // @name is the Z coordinate of the point.
    Z: double;
    // @name: double;
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
    // @name: longint;
    // The value of ShapeType should be @link(stMultiPointZ) = 18.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolyLineZ) = 13.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    // @name: longint;
    // The value of ShapeType should be @link(stPolygonZ) = 15.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
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
    Parts: array of integer;
    PartTypes: array of integer;
    Points: array of TShapePoint;
    ZMin: double;
    ZMax: double;
    ZArray: array of double;
    MMin: double;
    MMax: double;
    MArray: array of double; 
  end;
  #)
  }
  TMultiPatchShapeRecord = packed record
    // @name: longint;
    // The value of ShapeType should be @link(stMultiPatch) = 31.
    ShapeType: longint;
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    BoundingBox: TBoundingBox;
    // @name: longint;
    // @name is the number of parts in the Shape.
    NumParts: longint;
    // @name: longint;
    // @name is the number of points in the Shape.
    NumPoints: longint;
    {  @name: An array of length @link(NumParts). Stores, for each multipatch,
        the index of its first point in the points array.
        Array indexes are with respect to 0.    }
    Parts: array of integer;
    // @name: array of integer;
    // An array of length NumParts. Stores for each part its type.
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
    PartTypes: array of integer;
    {
    An array of length @link(NumPoints).
    The points for each part in the MultiPatch
    are stored end to end. The points for
    Part 2 follow the points for Part 1, and
    so on. The parts array holds the array index of the starting point for each
    part. There is no delimiter in the points array between parts.    }
    Points: array of TShapePoint;
    // @name: double;
    // The minimum and maximum Z values for the arc
    // stored in the order ZMin, ZMax.
    ZMin: double;
    // @name: double;
    // The minimum and maximum Z values for the arc
    // stored in the order ZMin, ZMax.
    ZMax: double;
    {@name: array of double;
    An array of length @link(NumPoints).
    The Z values for each part in the MultiPatch
    are stored end to end. The Z values
    for Part 2 follow the Z values for Part 1,
    and so on. The parts array holds the array index of the starting Z value for
    each part. There is no delimiter in the Z value array between parts.}
    ZArray: array of double;
    // @name: double;
    // @name is the minimum measure for the MultiPatch.
    // @name is optional
    MMin: double;
    // @name: double;
    // @name is the maximum measure for the MultiPatch.
    // @name is optional
    MMax: double;
    {@name: array of double;
    An array of length @link(NumPoints). The measures for each part in the
    MultiPatch are stored end to end. The measures for Part 2 follow the
    measures for Part 1, and so on. The parts array holds the array index of the
    starting measure for each part. There is no delimiter in the measure array
    between parts.
    @name is optional}
    MArray: array of double; 
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
    // @name: longint;
    // @name is the starting point of the shape record header
    // measured in 16 bit words
    // @name is in BigEndian format.
    Offset: longint;
    // @name: longint;
    // @name is the length of the shape content
    // measured in 16 bit words
    // @name is in BigEndian format.
    ContentLength: longint;
  end;

  {@abstract(@name represents a shape in an ESRI Shapefile.
  See TShapeGeometryFile.@link(TShapeGeometryFile.Items).)}
  TShapeObject = class(TObject)
  public
    // @name: @link(TBoundingBox);
    // @name represents the bounding box of the shape.
    FBoundingBox: TBoundingBox;
    {@name: array of double;
    An array of length @link(FNumPoints). The measures for each part in the
    MultiPatch are stored end to end. The measures for Part 2 follow the
    measures for Part 1, and so on.
    The parts array holds the array index of the
    starting measure for each part. There is no delimiter in the measure array
    between parts.
    @name is optional}
    FMArray: array of double;
    // @name: double;
    // The maximum measure for the shape.
    // @name is optional
    FMMax: double;
    // @name: double;
    // The minimum measure for the shape.
    // @name is optional
    FMMin: double;
    // @name: longint;
    // @name is the number of parts in the Shape.
    FNumParts: integer;
    // @name: longint;
    // @name is the number of points in the Shape.
    FNumPoints: integer;
    {@name: An array of length @link(FNumParts). Stores, for each PolyLine,
        the index of its first point in the points array.
        Array indexes are with respect to 0.    }
    FParts: array of integer;
    // @name: array of integer;
    // An array of length NumParts. Stores for each part its type.
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
    {@name: array of @link(TShapePoint);
    An array of length @link(FNumPoints). The points for
    each part in the MultiPatch
    are stored end to end. The points for Part 2
    follow the points for Part 1, and
    so on. The parts array holds the array index of the starting point for each
    part. There is no delimiter in the points array between parts.    }
    FPoints: array of TShapePoint;
    // @name: longint;
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
    {@name: array of double;
    An array of length @link(FNumPoints). The Z
    values for each point in the shape
    are stored end to end. The Z values for
    Part 2 follow the Z values for Part 1,
    and so on. The parts array holds the array
    index of the starting Z value for
    each part. There is no delimiter in the Z value array between parts.}
    FZArray: array of double;
    // @name: double;
    // The maximum Z value for the arc.
    FZMax: double;
    // @name: double;
    // The minimum Z value for the arc.
    FZMin: double;
    // @name creates an instance of @classname.
    constructor Create;
  end;

type
  {@abstract(@name provides a method of reading the geometry part
  of ESRI Shapefiles.  Each shape in the Shapefile is stored in
  a @link(TShapeObject) which can be accessed through the
  @link(TShapeGeometryFile.Items) property.)}
  TShapeGeometryFile = class(TObject)
  private
    // @name: @link(TShapeFileHeader);
    // See @link(FileHeader).
    FFileHeader: TShapeFileHeader;
    // @name: TList;
    // @name stores the @link(TShapeObject)s accessed through @link(Items).
    // @name is instantiated as a TObjectList.
    FShapeObjects: TList;
    // @name: @link(TProgressProcedure);
    // See @link(OnProgress).
    FOnProgress: TProgressProcedure;
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
    { FileHeader is the @link(TShapeFileHeader) at the beginning of the file.
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
    property FileHeader: TShapeFileHeader read FFileHeader;
    // @name provides access to the @link(TShapeObject)s read from
    // the Shapefile with @link(ReadFromFile).
    property Items[const Index: integer]: TShapeObject read GetItems; default;
    // @name reads the geometry from a file named FileName.
    // See also @link(OnProgress).
    // @param(MainFileName is the name of the main geometry file.
    // It has the extension .shp.)
    // @param(IndexFileName is the name of the index file.
    // It has the extension .shx.)
    procedure ReadFromFile(const MainFileName, IndexFileName: string);
    {
      @name is called each time a shape is read from the disk during
      ReadFromFile.
      FractionDone will be the fraction of the file that has been read.
    }
    property OnProgress: TProgressProcedure read FOnProgress write FOnProgress;
  end;

type
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

{ @abstract(@name reads a @link(TShapeFileHeader) and returns the type of
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

{ TShapeGeometryFile }

constructor TShapeGeometryFile.Create;
begin
  inherited;
  FShapeObjects := TObjectList.Create;
end;

destructor TShapeGeometryFile.Destroy;
begin
  FShapeObjects.Free;
  inherited;
end;

function TShapeGeometryFile.GetCount: integer;
begin
  result := FShapeObjects.Count;
end;

function TShapeGeometryFile.GetItems(const Index: integer): TShapeObject;
begin
  result := FShapeObjects[Index];
end;

procedure TShapeGeometryFile.ReadFromFile(const MainFileName, IndexFileName: string);
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
  NumPoints: integer;
  // NumParts is the number of parts in the current shape.
  NumParts: integer;
  DummyFileHeader: TShapeFileHeader;
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
  begin
    PointShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointShapeObject);
    FileStream.Read(Data, SizeOf(TPointShapeRecord));
    PointShapeObject.FShapeType := Data.ShapeType;
    SetLength(PointShapeObject.FPoints, 1);
    PointShapeObject.FPoints[0].X := Data.X;
    PointShapeObject.FPoints[0].Y := Data.Y;
    PointShapeObject.FNumPoints := 1;
    PointShapeObject.FNumParts := 1;
  end;
  procedure ReadPolyLine;
  var
    PolyLineShapeObject: TShapeObject;
    Data: TPolyLineShapeRecord;
  begin
    PolyLineShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineShapeObject);
    FileStream.Read(Data, SizeOf(TPolyLineShapeRecord));
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
      FileStream.Read(PolyLineShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure ReadPolygon;
  var
    PolygonShapeObject: TShapeObject;
    Data: TPolygonShapeRecord;
  begin
    PolygonShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonShapeObject);
    FileStream.Read(Data, SizeOf(TPolygonShapeRecord));
    Move(Data.BoundingBox, PolygonShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    NumPoints := Data.NumPoints;
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
      FileStream.Read(PolygonShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure ReadMultiPoint;
  var
    MultiPointShapeObject: TShapeObject;
    Data: TMultiPointShapeRecord;
  begin
    MultiPointShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointShapeObject);
    FileStream.Read(Data, SizeOf(TMultiPointShapeRecord));
    Move(Data.BoundingBox, MultiPointShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    NumPoints := Data.NumPoints;
    MultiPointShapeObject.FNumPoints := NumPoints;
    MultiPointShapeObject.FNumParts := 0;
    MultiPointShapeObject.FShapeType := Data.ShapeType;
    SetLength(MultiPointShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(MultiPointShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
    end;
  end;
  procedure ReadPointZ;
  var
    PointZShapeObject: TShapeObject;
    Data: TPointZShapeRecord;
  begin
    PointZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointZShapeObject);
    FileStream.Read(Data, SizeOf(TPointZShapeRecord));
    PointZShapeObject.FShapeType := Data.ShapeType;
    SetLength(PointZShapeObject.FPoints, 1);
    PointZShapeObject.FPoints[0].X := Data.X;
    PointZShapeObject.FPoints[0].Y := Data.Y;
    SetLength(PointZShapeObject.FMArray, 1);
    PointZShapeObject.FMArray[0] := Data.M;
    PointZShapeObject.FMMin := Data.M;
    PointZShapeObject.FMMax := Data.M;
    PointZShapeObject.FZArray[0] := Data.Z;
    PointZShapeObject.FZMin := Data.Z;
    PointZShapeObject.FZMax := Data.Z;
  end;
  procedure ReadPolyLineZ;
  var
    PolyLineZShapeObject: TShapeObject;
    Data: TPolyLineZShapeRecord;
  begin
    PolyLineZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineZShapeObject);
    FileStream.Read(Data, SizeOf(TPolyLineZShapeRecord));
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
      FileStream.Read(PolyLineZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadPolygonZ;
  var
    PolygonZShapeObject: TShapeObject;
    Data: TPolygonZShapeRecord;
  begin
    PolygonZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonZShapeObject);
    FileStream.Read(Data, SizeOf(TPolygonZShapeRecord));
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
      FileStream.Read(PolygonZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadMultipPointZ;
  var
    MultiPointZShapeObject: TShapeObject;
    Data: TMultiPointZShapeRecord;
  begin
    MultiPointZShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointZShapeObject);
    FileStream.Read(Data, SizeOf(TMultiPointZShapeRecord));
    NumPoints := Data.NumPoints;
    NumParts := 0;
    MultiPointZShapeObject.FNumPoints := NumPoints;
    MultiPointZShapeObject.FNumParts := NumParts;
    MultiPointZShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, MultiPointZShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(MultiPointZShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(MultiPointZShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadPointM;
  var
    PointMShapeObject: TShapeObject;
    Data: TPointMShapeRecord;
  begin
    PointMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PointMShapeObject);
    FileStream.Read(Data, SizeOf(TPointMShapeRecord));
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
    PointMShapeObject.FNumPoints := 1;
    PointMShapeObject.FNumParts := 0;
    SetLength(PointMShapeObject.FZArray, 0);
  end;
  procedure ReadPolyLineM;
  var
    PolyLineMShapeObject: TShapeObject;
    Data: TPolyLineMShapeRecord;
  begin
    PolyLineMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolyLineMShapeObject);
    FileStream.Read(Data, SizeOf(TPolyLineMShapeRecord));
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolyLineMShapeObject.FNumPoints := NumParts;
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
      FileStream.Read(PolyLineMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadPolygonM;
  var
    PolygonMShapeObject: TShapeObject;
    Data: TPolygonMShapeRecord;
  begin
    PolygonMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(PolygonMShapeObject);
    FileStream.Read(Data, SizeOf(TPolygonMShapeRecord));
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    PolygonMShapeObject.FNumPoints := NumParts;
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
      FileStream.Read(PolygonMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadMultiPointM;
  var
    MultiPointMShapeObject: TShapeObject;
    Data: TMultiPointMShapeRecord;
  begin
    MultiPointMShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPointMShapeObject);
    FileStream.Read(Data, SizeOf(TMultiPointMShapeRecord));
    NumPoints := Data.NumPoints;
    NumParts := 0;
    MultiPointMShapeObject.FNumPoints := NumParts;
    MultiPointMShapeObject.FNumParts := NumParts;
    MultiPointMShapeObject.FShapeType := Data.ShapeType;
    Move(Data.BoundingBox, MultiPointMShapeObject.FBoundingBox,
      sizeOf(TBoundingBox));
    SetLength(MultiPointMShapeObject.FPoints, NumPoints);
    if NumPoints > 0 then
    begin
      FileStream.Read(MultiPointMShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
  procedure ReadMultiPatch;
  var
    MultiPatchShapeObject: TShapeObject;
    Data: TMultiPatchShapeRecord;
  begin
    MultiPatchShapeObject := TShapeObject.Create;
    FShapeObjects.Add(MultiPatchShapeObject);
    FileStream.Read(Data, SizeOf(TMultiPatchShapeRecord));
    NumParts := Data.NumParts;
    NumPoints := Data.NumPoints;
    MultiPatchShapeObject.FNumPoints := NumParts;
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
      FileStream.Read(MultiPatchShapeObject.FPoints[0],
        NumPoints * SizeOf(TShapePoint));
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
  end;
begin
  FileStream := TFileStream.Create(MainFileName, fmOpenRead or fmShareDenyWrite);
  IndexFileStream := TFileStream.Create(IndexFileName, fmOpenRead or fmShareDenyWrite);
  try
    FileStream.Read(FFileHeader, SizeOf(TShapeFileHeader));
    IndexFileStream.Read(DummyFileHeader, SizeOf(TShapeFileHeader));

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
      Assert(ContentEnd <= FileStream.Size);
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
          Assert(False);
        end;
      end;
    end;
  finally
    FileStream.Free;
    IndexFileStream.Free;
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
  FileHeader: TShapeFileHeader;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    FileStream.Read(FileHeader, SizeOf(TShapeFileHeader));
    result := FileHeader.ShapeType;
  finally
    FileStream.Free;
  end;
end;

end.

