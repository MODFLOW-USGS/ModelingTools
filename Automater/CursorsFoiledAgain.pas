{@abstract(@name defines a series of cursor constants.
  It creates the associated cursors in the initialization section and
  destroys them in the finalization section.)
  The bitmaps for the cursors are stored in CustomCursor.res.}
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

(*procedure CreateMaskedCursor(const BitmapName, MaskName: string;
  const CursorNumber, HotPointX, HotPointY: integer);
var
  QBitmap, QMask: QBitmapH;
  QCursor: QCursorH;
  bmCursor, bmMask: TBitmap;
  Index: integer;
begin
  // ReferenceS:
  // p. 588 of
  // Teixeira, Steve and  Pacheco, Xavier. 2002. Borland®
  // Delphi TM 6 Developer's Guide. Sams Publishing,
  // 201 West 103rd St., Indianapolis, Indiana, 46290 USA,  1169 p.
  //
  // P. 442 of
  // Shemitz, Jon, 2002. Kylix: The Prefessional Developer's Guide
  // and Reference. Apress, 901 Grayson Street, Suite 204,
  // Berkeley, CA 94710, 943 p.

  // Make sure the cursor number is not duplicated.
  for Index := 0 to LastCursor - 1 do
  begin
    Assert(MyCursors[Index] <> CursorNumber);
  end;
  // store the cursor number in MyCursors for use in DestroyCursors.
  Inc(LastCursor);
  SetLength(MyCursors, LastCursor);
  MyCursors[LastCursor - 1] := CursorNumber;

  {
  from http://doc.trolltech.com/3.0/qcursor.html#QCursor-3

  The cursor bitmap (B) and mask (M) bits are combined like this:

      B=1 and M=1 gives black.
      B=0 and M=1 gives white.
      B=0 and M=0 gives transparent.
      B=1 and M=0 gives an undefined result.
  }

  // Now create the cursor.

  // First create two bitmaps.
  // The first represents the cursor;
  // the second represents the mask.
  bmCursor := TBitmap.Create;
  bmMask := TBitmap.Create;
  try
    // load the appropriate contents into the two bitmaps.
    bmCursor.LoadFromResourceName(HInstance, BitmapName);
    bmMask.LoadFromResourceName(HInstance, MaskName);

    // Create two QBitmapH.
    // The first represents the cursor;
    // the second represents the mask.
    QBitmap := QBitmap_create;
    QMask := QBitmap_create;
    try
      // load the appropriate contents into the two QBitmapHs.
      QBitmap_from_QPixmap(QBitmap, bmCursor.Handle);
      QBitmap_from_QPixmap(QMask, bmMask.Handle);
      // make a cursor.
      QCursor := QCursor_create(QBitmap, QMask, HotPointX, HotPointY);
    finally
      // clean up.
      QBitmap_destroy(QBitmap);
      QBitmap_destroy(QMask);
    end;
  finally
    // clean up.
    bmCursor.Free;
    bmMask.Free;
  end;

  // store the cursor in Screen.Cursors.
  Screen.Cursors[CursorNumber] := QCursor;
end;
*)

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

{
  CreateMaskedCursor('CRHANDFLAT', 'MASKHANDFLAT', crHandFlat, 14, 11);
  CreateMaskedCursor('CRZOOMIN', 'MASKZOOMIN', crZoomIn, 11, 11);
  CreateMaskedCursor('CRZOOMOUT', 'MASKZOOMOUT', crZoomOut, 11, 11);
  CreateMaskedCursor('CRZOOM', 'MASKZOOM', crZoom, 11, 11);
  CreateMaskedCursor('CRZOOMNODOT', 'MASKZOOM', crZoomNoDot, 11, 11);
  CreateMaskedCursor('CRDELETE', 'MASKDELETE', crDelete, 15, 15);
  CreateMaskedCursor('CRHORIZONTALLINE', 'MASKHORIZONTALLINE',
    crHorizontal, 16, 16);
  CreateMaskedCursor('CRVERTICALLINE', 'MASKVERTICALLINE', crVertical, 16, 16);
  CreateMaskedCursor('CRMOVECOLUMN', 'MASKMOVECOLUMN', crMoveColumn, 16, 16);
  CreateMaskedCursor('CRMOVEROW', 'MASKMOVEROW', crMoveRow, 16, 16);
  CreateMaskedCursor('CRSUBDIVIDE', 'MASKSUBDIVIDE', crSubdivide, 16, 16);
  CreateMaskedCursor('CRSELECTPOINT', 'MASKSELECTPOINT', crSelectPoint, 7, 7);
  CreateMaskedCursor('CRHANDGRAB', 'MASKHANDGRAB', crHandGrab, 16, 11);
  CreateMaskedCursor('CRINSERTPOINT', 'MASKINSERTPOINT', crInsertPoint, 15, 15);
  CreateMaskedCursor('CRDELETESEGMENT', 'MASKDELETESEGMENT',
    crDeleteSegment, 21, 21);
  CreateMaskedCursor('CRDISABLEDINSERTPOINT', 'MASKINSERTPOINT',
    crDisabledInsertPoint, 15, 15);
  CreateMaskedCursor('CRDISABLEDDELETESEGMENT', 'MASKDELETESEGMENT',
    crDisabledDeleteSegment, 21, 21);
  CreateMaskedCursor('CRSETWIDTH', 'MASKSETWIDTH', crSetWidth, 16, 16);
  CreateMaskedCursor('CRROTATE', 'MASKROTATE', crRotate, 16, 16);
  CreateMaskedCursor('CRPOINTARROW', 'MASKPOINTARROW', crPointArrow, 2, 2);
  CreateMaskedCursor('CRLINEARROW', 'MASKLINEARROW', crLineArrow, 2, 2);
  CreateMaskedCursor('CRPOLYGONARROW', 'MASKPOLYGONARROW',
    crPolygonArrow, 2, 2);
  CreateMaskedCursor('CRSTRAIGHTLINEARROW', 'MASKSTRAIGHTLINEARROW',
    crStraightLineArrow, 2, 2);
  CreateMaskedCursor('CRRECTANGLEARROW', 'MASKRECTANGLEARROW',
    crRectangleArrow, 2, 2);
  CreateMaskedCursor('CR3DPOINTARROW', 'MASK3DPOINTARROW',
    cr3dPointArrow, 2, 2);
  CreateMaskedCursor('CR3DLINEARROW', 'MASK3DLINEARROW', cr3dLineArrow, 2, 2);
  CreateMaskedCursor('CR3DSHEETARROW', 'MASK3DSHEETARROW',
    cr3dSheetArrow, 2, 2);
  CreateMaskedCursor('CR3DSOLIDARROW', 'MASK3DSOLIDARROW',
    cr3dSolidArrow, 2, 2);
  CreateMaskedCursor('CR3DSTRAIGHTLINEARROW', 'MASK3DSTRAIGHTLINEARROW',
    cr3dStraightLineArrow, 2, 2);
  CreateMaskedCursor('CR3DCUBEARROW', 'MASK3DCUBEARROW', crCubeArrow, 2, 2);
  CreateMaskedCursor('CRDOMAINLINE', 'MASKDOMAINLINE', crDomainLine, 2, 2);
  CreateMaskedCursor('CRDOMAINPOLYGON', 'MASKDOMAINPOLYGON',
    crDomainPolygon, 2, 2);
//  CreateMaskedCursor('CRZOOMBYY', 'MASKZOOMBYY',
//    crZoomByY, 15, 15);
  CreateMaskedCursor('3DZOOM', '3DZOOMMASK',
    crZoomByY, 15, 32);  }

//finalization
//  DestroyCursors;

end.

