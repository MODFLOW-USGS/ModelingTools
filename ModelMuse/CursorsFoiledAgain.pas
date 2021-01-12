{@abstract(@name defines a series of cursor constants.
  It creates the associated cursors in the initialization section and
  destroys them in the finalization section.)
  The bitmaps for the cursors are stored in ModelMuseCursors.res.}
unit CursorsFoiledAgain;

interface

uses Windows;

{User defined cursors are assigned unique, arbitrary, positive values.}
const
  // @name represents a cursor shaped like a hand.
  // See @link(crHandGrab).
  crHandFlat = 34;
  // @name represents a cursor shaped like a magnifying glass with
  // a "+" symbol in the middle.
  // See @link(crZoomOut) and @link(crZoom).
  crZoomIn = 2;
  // @name represents a cursor shaped like a magnifying glass with
  // a "-" symbol in the middle.
  // See @link(crZoomIn) and @link(crZoom).
  crZoomOut = 3;
  // @name represents a cursor shaped like a magnifying glass.
  // See @link(crZoomIn) and @link(crZoomOut).
  crZoom = 4;
  // @name represents a cursor shaped like an "X".
  crDelete = 5;
  // @name represents a cursor shaped like a horizontal line.
  crHorizontal = 6;
  // @name represents a cursor shaped like a vertical line.
  crVertical = 7;
  // @name represents a cursor shaped like two vertical lines close together
  // with arrows point away from them horizontally.
  crMoveColumn = 8;
  // @name represents a cursor shaped like two horizontal lines close together
  // with arrows point away from them vertically.
  crMoveRow = 9;
  // @name represents a cursor shaped like three vertical
  // lines close together crossed by three horizontal lines.
  crSubdivide = 10;
  // @name represents a cursor shaped like a large arrowhead.
  crSelectPoint = 11;
  // @name represents a cursor shaped like a hand
  // with bent fingers as if holding something.
  // See @link(crHandGrab).
  crHandGrab = 12;
  // @name represents a cursor shaped like
  // A large "+" with a dot in the middle
  // with a smaller dot and line in the upper left.
  crInsertPoint = 13;
  // @name represents a cursor shaped like
  // A large "X" with a smaller broken line in the upper left.
  crDeleteSegment = 14;
  // @name represents a cursor shaped like
  // A large "+" with a dot in the middle
  // with a smaller dot and line in the upper left.
  // the "+" is drawn with a dotted line.
  crDisabledInsertPoint = 15;
  // @name represents a cursor shaped like
  // A large "X" with a smaller broken line in the upper left.
  // the "X" is drawn with a dotted line.
  crDisabledDeleteSegment = 16;
  // @name represents a cursor shaped like two widely spaced vertical
  // lines close together crossed by two widely spaced horizontal lines.
  crSetWidth = 17;
  // @name represents a cursor shaped like a curved, double-headed
  // arrow with a dot at the center of the circle defined by the curve.
  crRotate = 18;
  // @name represents a cursor shaped like an arrow with a large point
  // in the upper right.
  crPointArrow = 19;
  // @name represents a cursor shaped like an arrow with a poly line
  // on the right.
  crLineArrow = 20;
  // @name represents a cursor shaped like an arrow with a polygon
  // on the right.
  crPolygonArrow = 21;
  // @name represents a cursor shaped like an arrow with a stair-step pattern
  // on the right.
  crStraightLineArrow = 22;
  // @name represents a cursor shaped like an arrow with a rectangle
  // on the right.
  crRectangleArrow = 23;

  // @name represents a cursor with multiple polygons. The cursor is
  // used when drawing @link(TScreenObject)s with multiple polygons.
  crMultiPartPolygon = 24;
  // @name represents a cursor with multiple lines. The cursor is
  // used when drawing @link(TScreenObject)s with multiple lines.
  crMultiPartLine = 25;
  // @name represents a cursor with multiple points. The cursor is
  // used when drawing @link(TScreenObject)s with multiple points.
  crMultiPartPoint = 26;

  crSnapPointArrow = 27;
  crSnapLineArrow = 28;
  crSnapPolygonArrow = 29;
  crSnapStraightLineArrow = 30;
  crSnapRectangleArrow = 31;

  crSnapMultiPartPolygon = 32;
  crSnapMultiPartPoint = 35;
  crSnapMultiPartLine = 36;

  // @name represents a cursor shaped like a magnifying glass with
  // a plus sign above it and a minus sign below it.
  crZoomByY = 33;

  // @name represents a cursor shaped like a large arrowhead.
  crSnapSelectPoint = 37;

  crVertexValue = 38;

  crMeasure = 39;
  crMoveCrossSection = 40;
  crFishnet = 50;
  crMoveNode = 51;
  crAddPilotPoint = 52;
  crDeletePilotPoint = 53 ;

  // cursor numbers 41-46 are defined dynamically in frmGoPhast.

implementation

// Force GExperts' "Backup Project" to backup ModelMuseCursors.res.
{#BACKUP ModelMuseCursors.res}

// ModelMuseCursors.res contains the monochrome bitmaps that will define
// the new cursors. The bitmaps are 32 x 32 pixels in size.
{$R ModelMuseCursors.res}

uses Forms, Graphics;

var
  LastCursor: integer = 0;
  MyCursors: array of integer;

Procedure LoadACursor(const CursorName: string; CursorNumber: integer);
var
  Index: integer;
begin
  // Make sure the cursor number is not duplicated.
  for Index := 0 to LastCursor - 1 do
  begin
    Assert(MyCursors[Index] <> CursorNumber);
  end;
  // store the cursor number in MyCursors for use in DestroyCursors.
  Inc(LastCursor);
  SetLength(MyCursors, LastCursor);
  MyCursors[LastCursor - 1] := CursorNumber;

  Screen.Cursors[CursorNumber] := LoadCursor(HInstance, PChar(CursorName));
end;

{procedure DestroyCursors;
var
  Index: integer;
begin
  // Screen destroys the cursors by calling QCursor_destroy.
  for Index := 0 to LastCursor - 1 do
  begin
    Screen.Cursors[MyCursors[Index]] := nil;
  end;
end;   }

initialization
  LoadACursor('CRHANDFLAT', crHandFlat);
  LoadACursor('CRZOOMIN', crZoomIn);
  LoadACursor('CRZOOMOUT', crZoomOut);
  LoadACursor('CRZOOM', crZoom);
  LoadACursor('CRDELETE', crDelete);
  LoadACursor('CRHORIZONTALLINE', crHorizontal);
  LoadACursor('CRVERTICALLINE', crVertical);
  LoadACursor('CRMOVECOLUMN', crMoveColumn);
  LoadACursor('CRMOVEROW', crMoveRow);
  LoadACursor('CRSUBDIVIDE', crSubdivide);
  LoadACursor('CRSELECTPOINT', crSelectPoint);
  LoadACursor('CRHANDGRAB', crHandGrab);
  LoadACursor('CRINSERTPOINT', crInsertPoint);
  LoadACursor('CRDELETESEGMENT', crDeleteSegment);
  LoadACursor('CRDISABLEDINSERTPOINT', crDisabledInsertPoint);
  LoadACursor('CRDISABLEDDELETESEGMENT', crDisabledDeleteSegment);
  LoadACursor('CRSETWIDTH', crSetWidth);
  LoadACursor('CRROTATE', crRotate);
  LoadACursor('CRPOINTARROW', crPointArrow);
  LoadACursor('CRLINEARROW', crLineArrow);
  LoadACursor('CRPOLYGONARROW', crPolygonArrow);
  LoadACursor('CRSTRAIGHTLINEARROW', crStraightLineArrow);
  LoadACursor('CRRECTANGLEARROW', crRectangleArrow);
  LoadACursor('CRMULTIPARTPOLYGON', crMultiPartPolygon);
  LoadACursor('CRMULTIPARTLINE', crMultiPartLine);
  LoadACursor('CRMULTIPARTPOINT', crMultiPartPoint);
  LoadACursor('CRSNAPPOINTARROW', crSnapPointArrow);
  LoadACursor('CRSNAPLINEARROW', crSnapLineArrow);
  LoadACursor('CRSNAPPOLYGONARROW', crSnapPolygonArrow);
  LoadACursor('CRSNAPSTRAIGHTLINEARROW', crSnapStraightLineArrow);
  LoadACursor('CRSNAPRECTANGLEARROW', crSnapRectangleArrow);
  LoadACursor('CRSNAPMULTIPARTPOINT', crSnapMultiPartPoint);
  LoadACursor('CRSNAPMULTIPARTLINE', crSnapMultiPartLine);
  LoadACursor('CRSNAPMULTIPARTPOLYGON', crSnapMultiPartPolygon);
  LoadACursor('CRZOOMBYY', crZoomByY);
  LoadACursor('CRSNAPSELECTPOINT', crSnapSelectPoint);
  LoadACursor('CRVERTEXVALUE', crVertexValue);
  LoadACursor('CRMEASURE', crMeasure);
  LoadACursor('CRMOVECROSSSECTION', crMoveCrossSection);
  LoadACursor('CRFISHNET', crFishnet);
  LoadACursor('CRMOVENODE', crMoveNode);
  LoadACursor('CRNEWPILOTPOINT', crAddPilotPoint);
  LoadACursor('CRDELETEPILOTPOINT', crDeletePilotPoint);

//finalization
//  DestroyCursors;

end.

