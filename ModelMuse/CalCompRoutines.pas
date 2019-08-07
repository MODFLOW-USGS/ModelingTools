{
TRICP was designed to call plotting routines in the (obsolete) Calcomp plotter.
These routines replace the Calcomp plotter routines.

@author(Richard B. Winston <rbwinst@usgs.gov>)
This file is in the public domain.
}

unit CalCompRoutines;

interface

uses TriPackRoutines, GoPhastTypes, SubPolygonUnit;

type
  // @name is a type of procedure used to display a contour plot.
  // @seealso(TShowPlotObject)
  // @seealso(ShowPlot)
  TShowPlot = procedure;
  // @name is a type of procedure used to display a contour plot.
  // @seealso(TShowPlot).
  // @seealso(ShowPlotObject)
  TShowPlotObject = procedure of object;

// INITIALIZE PLOTTING
//procedure PLOTS; stdcall;

// As used here, simply plot a dot at the specified location.
//procedure SYMBOL(var XD: Real; var YD: Real; var Scale: Real;
//  var Dummy1: longint; var Dummy2: Real; var Dummy3: longint); stdcall;

// calling with IPEN = 3 starts a new line.
// calling with IPEN = 2 continues a line.
// calling with IPEN = 999 closes the plot by setting @link(CurrentLineList)
// to nil and then calls @link(ShowPlot) or @link(ShowPlotObject) if
// either has been assigned.
procedure PLOT(var X, Y: Real; var IPEN: integer); stdcall;

// @name adds X0, Y0 and
// NumberToPlot to @link(CurrentLineList).Numbers.
//procedure NUMBER (var X0,Y0,Scale: Real; var NumberToPlot: Real;
//  var Dummy2: Real; var Dummy3: longint); stdcall;

{
  @name sets @link(TLine.TriangleNumber CurrentLine.ContourLevel) and
  @link(TLine.TriangleNumber CurrentLine.TriangleNumber) of @link(CurrentLine).
}
procedure SpecifyContourLevel(const ContourLevel: Real;
  const TriangleNumber, TriangleSide: longint);

{
  @name sets @link(TLineList.XD CurrentLineList.XD),
  @link(TLineList.YD CurrentLineList.YD), and
  @link(TLineList.Triangles CurrentLineList.Triangles) of @link(CurrentLineList).
}
//procedure StoreArrays(X, Y: TRealArray; Triangles: TIntArray);

var
  // If assigned, @name is called in @link(PLOT) when all the contour
  // lines have been created. If @name will be used it
  // should be assigned before calling @link(TRICP_Pascal).
  // @seealso(ShowPlotObject)
  // @seealso(TMultipleContourCreator.CreateAndDrawContours)
  // @seealso(TCustomContourCreator.PerformAlg626)
  ShowPlot: TShowPlot = nil;
  // If assigned, @name is called in @link(PLOT) when all the contour
  // lines have been created. If @name will be used it
  // should be assigned before calling @link(TRICP_Pascal).
  // @seealso(ShowPlot)
  // @seealso(TMultipleContourCreator.CreateAndDrawContours)
  // @seealso(TCustomContourCreator.PerformAlg626)
  ShowPlotObject: TShowPlotObject = nil;

  MeshOutline: TOutline = nil;

implementation

uses LineStorage;

var
  GlobalContourLevel: Real = 0;
  GlobalTriangleNumber: Integer = 0;

//procedure PLOTS; stdcall;
//begin
//end;

{procedure SYMBOL(var XD: Real; var YD: Real; var Scale: Real;
  var Dummy1: longint; var Dummy2: Real; var Dummy3: longint); stdcall;
begin
  if CurrentLineList = nil then
  begin
    CurrentLineList := TLineList.Create;
    PlotList.Add(CurrentLineList);
  end;
  CurrentLineList.Symbols.Add(XD, YD);
end; }

procedure SpecifyContourLevel(const ContourLevel: Real;
  const TriangleNumber, TriangleSide: longint);
begin
  GlobalContourLevel := ContourLevel;
  GlobalTriangleNumber := TriangleNumber;
  if (CurrentLine <> nil) then
  begin
    CurrentLine.ContourLevel := ContourLevel;
    CurrentLine.TriangleNumber := TriangleNumber;
  end;
//  CurrentLine.StartingSide := TriangleSide;
end;

{procedure StoreArrays(X, Y: TRealArray; Triangles: TIntArray);
begin
  Assert(CurrentLineList <> nil);
  CurrentLineList.XD := X;
  CurrentLineList.YD := Y;
  CurrentLineList.Triangles := Triangles;
end; }

procedure PLOT(var X, Y: Real; var IPEN: longint); stdcall;
begin
  if IPEN = 999 then
  begin
    CurrentLineList := nil;
    CurrentLine := nil;
    if  Assigned(ShowPlot) then
    begin
      ShowPlot;
    end;
    if Assigned(ShowPlotObject) then
    begin
      ShowPlotObject;
    end;
    Exit;
  end;

  if Assigned(MeshOutline) then
  begin
    if not MeshOutline.PointInside(X,Y) then
    begin
      CurrentLine := nil;
      Exit;
    end;
  end;

  if (IPEN = 3) or (CurrentLine = nil) then
  begin
    if CurrentLineList = nil then
    begin
      CurrentLineList := TLineList.Create;
      PlotList.Add(CurrentLineList);
    end;
    CurrentLine := TLine.Create;
    CurrentLineList.Add(CurrentLine);
    CurrentLine.ContourLevel := GlobalContourLevel;
    CurrentLine.TriangleNumber := GlobalTriangleNumber;
  end;
  CurrentLine.Add(X,Y);
end;

{procedure NUMBER (var X0,Y0,Scale: Real; var NumberToPlot: Real;
  var Dummy2: Real; var Dummy3: longint); stdcall;
begin
  if CurrentLineList = nil then
  begin
    CurrentLineList := TLineList.Create;
    PlotList.Add(CurrentLineList);
  end;
  CurrentLineList.Numbers.Add(X0, Y0, NumberToPlot);
end;  }

end.

