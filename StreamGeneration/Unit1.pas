unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Grids, Vcl.ComCtrls,
  AlternateAlgorithm;

type
  TForm1 = class(TForm)
    btn1: TButton;
    dlgSave1: TSaveDialog;
    pgc1: TPageControl;
    tabElevations: TTabSheet;
    sgElevations: TStringGrid;
    tabFlowDirections: TTabSheet;
    sgFlowDirections: TStringGrid;
    tabNeighbors: TTabSheet;
    sgNeighbors: TStringGrid;
    tabEndpoints: TTabSheet;
    sgEndPoints: TStringGrid;
    tabEdgePoints: TTabSheet;
    sgEdgePointsBoolean: TStringGrid;
    tabSpillPoints: TTabSheet;
    sgSpillPoints: TStringGrid;
    tabNewFlowDir: TTabSheet;
    sgNewFlowDir: TStringGrid;
    tabNewEndPoints: TTabSheet;
    sgNewEndPoints: TStringGrid;
    edRandSeed: TEdit;
    tabPitless: TTabSheet;
    sgPitless: TStringGrid;
    tabFinalFlowDir: TTabSheet;
    sgFinalFlowdir: TStringGrid;
    tabFinalSpillPoints: TTabSheet;
    sgFinalSpillPoints: TStringGrid;
    procedure btn1Click(Sender: TObject);
  private
    function SaveFile(var FileName: string): Boolean;
    procedure DisplayElevations(ElevationFileName: string);
    procedure DisplayFlowDirections(FlowDirectionsFileName: string);
    procedure DisplayValues(Grid: TStringGrid; SurferGirdFileName: String);
    procedure DisplayNeighbors(NeighborsFileName: string);
    procedure DisplayEndPoints(EndPointFileName: string);
    procedure DisplaySpillPoints(SpillPoints: TSpillPointList;
      RasterName: string);
    procedure DisplayFD(FlowDirectionsFileName: string; Grid: TStringGrid);
    procedure DisplayNewFlowDir(FlowDirectionsFileName: string);
    procedure DisplayNewEndPoints(EndPointFileName: string);
    procedure CheckFlowDirections(const FlowDirectionsFileName, EndPointFileName: string);
    procedure SaveElevationFile(var FileName: string);
    procedure DisplayEdgePoints(EdgePointBooleanFileName: string);
    procedure DisplayPitless(PitlessElevationFileName: string);
    procedure DisplayFinalFillDir(FlowDirectionsFileName: string);
    procedure DisplaySpillPointsOnGrid(Grid: TStringGrid;
      SpillPoints: TSpillPointList; RasterName: string);
    procedure CheckFlowPitless(const FlowDirectionsFileName, PitlessFileName: string);

    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  SurferGridFileReaderUnit, GenericRasterUnit,
  System.IOUtils, System.Generics.Collections;

{$R *.dfm}

procedure TForm1.btn1Click(Sender: TObject);
var
  ElevationFileName: string;
  FlowDirectionsFileName: string;
  NeighborsFileName: string;
  PitTranslationList: TPitTranslationList;
  EndPointFileName: string;
  EdgePointListFileNames: TStringList;
  EdgePointBooleanFileName: String;
  SpillPoints: TSpillPointList;
//  Index: Integer;
  SpillPits: TArray<TSpillPoint>;
  PitList: TPitList;
  PitlessElevationFileName: string;
begin
  if not SaveFile(ElevationFileName) then
  begin
    Exit;
  end;
  RandSeed := -1945481447;
//  for Index := 1 to 1000 do
  begin
    edRandSeed.Text := IntToStr(RandSeed);
    SaveElevationFile(ElevationFileName);
    DisplayElevations(ElevationFileName);

    PitList := TPitList.Create;
    try
      FlowDirectionsFileName := ExtractFileDir(ElevationFileName);
      FlowDirectionsFileName := IncludeTrailingPathDelimiter(FlowDirectionsFileName)
        + 'FlowDirections.grd';
      AssignFlowDirections(ElevationFileName, FlowDirectionsFileName, PitList);
      DisplayFlowDirections(FlowDirectionsFileName);

      NeighborsFileName := ExtractFileDir(ElevationFileName);
      NeighborsFileName := IncludeTrailingPathDelimiter(NeighborsFileName)
        + 'Neighbors.List';

      AssignNeighbors(FlowDirectionsFileName, NeighborsFileName);
      DisplayNeighbors(NeighborsFileName);

      PitTranslationList := TPitTranslationList.Create;
      try
        EndPointFileName := ExtractFileDir(ElevationFileName);
        EndPointFileName := IncludeTrailingPathDelimiter(EndPointFileName)
          + 'EndPoints.grd';

        AssignEndPoints(FlowDirectionsFileName, NeighborsFileName,
          EndPointFileName, PitTranslationList);
        DisplayEndPoints(EndPointFileName);

        EdgePointListFileNames := TStringList.Create;
        try
          EdgePointBooleanFileName := ExtractFileDir(ElevationFileName);
          EdgePointBooleanFileName := IncludeTrailingPathDelimiter(EdgePointBooleanFileName)
            + 'EdgePointBooleanFileName.grd';
          GetEdgePoints(EndPointFileName, EdgePointBooleanFileName,
            EdgePointListFileNames);
          DisplayEdgePoints(EdgePointBooleanFileName);

          SpillPoints := TSpillPointList.Create;
          try
            GetSpillPoints(ElevationFileName, EndPointFileName,
              EdgePointBooleanFileName, EdgePointListFileNames,
              SpillPoints);

            DisplaySpillPoints(SpillPoints, ElevationFileName);

            MergePits(SpillPoints, ElevationFileName,
              FlowDirectionsFileName, EndPointFileName,
              EdgePointListFileNames, PitTranslationList, SpillPits, PitList);

            SpillPoints.Clear;
            SpillPoints.AddRange(SpillPits);
            DisplaySpillPointsOnGrid(sgFinalSpillPoints, SpillPoints, ElevationFileName);



            DisplayNewFlowDir(FlowDirectionsFileName);
            DisplayNewEndPoints(EndPointFileName);

            CheckFlowDirections(FlowDirectionsFileName, EndPointFileName);

            PitlessElevationFileName := ExtractFileDir(ElevationFileName);
            PitlessElevationFileName := IncludeTrailingPathDelimiter(PitlessElevationFileName)
              + 'PitlessElevations.grd';

            HandlePits(ElevationFileName, FlowDirectionsFileName,
              EndPointFileName, PitlessElevationFileName, SpillPits, PitList);
            DisplayPitless(PitlessElevationFileName);
            DisplayFinalFillDir(FlowDirectionsFileName);

            CheckFlowDirections(FlowDirectionsFileName, EndPointFileName);
            CheckFlowPitless(FlowDirectionsFileName, PitlessElevationFileName);

          finally
            SpillPoints.Free;
          end;

        finally
          EdgePointListFileNames.Free;
        end;


      finally
        PitTranslationList.Free;
      end;
    finally
      PitList.Free;
    end;
  end;

end;

function TForm1.SaveFile(var FileName: string): Boolean;
begin
  result := dlgSave1.Execute;
end;

procedure TForm1.CheckFlowDirections(const FlowDirectionsFileName,
  EndPointFileName: string);
type
  TFlowDirection = record
  private
    FDeltaX: integer;
    FDeltaY: Integer;
  end;
const
  FlowValues: array[TFlowOrdinal] of TFlowDirection
    = (
       (FDeltaX : -1; FDeltaY : +1), (FDeltaX :  0; FDeltaY : +1), (FDeltaX : +1; FDeltaY : +1),
       (FDeltaX : -1; FDeltaY :  0), (FDeltaX :  0; FDeltaY :  0), (FDeltaX : +1; FDeltaY :  0),
       (FDeltaX : -1; FDeltaY : -1), (FDeltaX :  0; FDeltaY : -1), (FDeltaX : +1; FDeltaY : -1)
      );
var
  YIndex: Integer;
  Header: TGrid7Header;
  AFlowOrdinal: TFlowOrdinal;
  XIndex: Integer;
  FlowDirections: TSurferRaster7File2;
  SurferGridFile: TSurferRaster7File2;
  FlowDirectionsArray: array of array of TFlowDirection;
  Zones: array of array of Integer;
  Used: array of array of Boolean;
  AFlowDir: TFlowDirection;
  AZone: Integer;
  X: Integer;
  Y: Integer;
  procedure InitializeUsed;
  var
    XIndex: Integer;
    YIndex: Integer;
  begin
    for XIndex := 0 to Length(Used) - 1 do
    begin
      for YIndex := 0 to Length(Used[0]) - 1 do
      begin
        Used[XIndex,YIndex] := False;
      end;
    end;
  end;
begin
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  try
    Header := FlowDirections.Header;
    SetLength(FlowDirectionsArray, Header.nCol, Header.nRow);
    SetLength(Zones, Header.nCol, Header.nRow);
    SetLength(Used, Header.nCol, Header.nRow);
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        AFlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[XIndex, YIndex]));
        FlowDirectionsArray[XIndex,YIndex] := FlowValues[AFlowOrdinal];
      end;
    end;
  finally
    FlowDirections.Free;
  end;

  SurferGridFile := TSurferRaster7File2.Create(EndPointFileName);
  try
    Header := SurferGridFile.Header;
//    Grid.ColCount := Header.nCol;
//    Grid.RowCount := Header.nRow;
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        Zones[XIndex, YIndex] := Round(SurferGridFile.Z[XIndex, YIndex]);
      end;
    end;
  finally
    SurferGridFile.Free;
  end;

  for XIndex := 0 to Length(Used) - 1 do
  begin
    for YIndex := 0 to Length(Used[0]) - 1 do
    begin
      InitializeUsed;
      AFlowDir := FlowDirectionsArray[XIndex,YIndex];
      if (AFlowDir.FDeltaX <> 0) or (AFlowDir.FDeltaY <> 0) then
      begin
        AZone := Zones[XIndex,YIndex];
        Used[XIndex,YIndex] := True;
        X := XIndex;
        Y := YIndex;
        while (AFlowDir.FDeltaX <> 0) or (AFlowDir.FDeltaY <> 0) do
        begin
          X := X + AFlowDir.FDeltaX;
          Y := Y + AFlowDir.FDeltaY;
          Assert(not Used[X,Y]);
          Used[X,Y] := True;
          Assert(Zones[X,Y] = AZone);
          AFlowDir := FlowDirectionsArray[X,Y];
        end;
      end;
    end;
  end;

//  ShowMessage('success');

end;

procedure TForm1.CheckFlowPitless(const FlowDirectionsFileName,
  PitlessFileName: string);
type
  TFlowDirection = record
  private
    FDeltaX: integer;
    FDeltaY: Integer;
  end;
const
  FlowValues: array[TFlowOrdinal] of TFlowDirection
    = (
       (FDeltaX : -1; FDeltaY : +1), (FDeltaX :  0; FDeltaY : +1), (FDeltaX : +1; FDeltaY : +1),
       (FDeltaX : -1; FDeltaY :  0), (FDeltaX :  0; FDeltaY :  0), (FDeltaX : +1; FDeltaY :  0),
       (FDeltaX : -1; FDeltaY : -1), (FDeltaX :  0; FDeltaY : -1), (FDeltaX : +1; FDeltaY : -1)
      );
var
  FlowDirections: TSurferRaster7File2;
  Pitless: TSurferRaster7File2;
  FlowDir: TFlowDirection;
  AFlowOrdinal: TFlowOrdinal;
  Z1: Double;
  Header: TGrid7Header;
  XIndex: Integer;
  YIndex: Integer;
  Z2: Double;
begin
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  Pitless := TSurferRaster7File2.Create(PitlessFileName);
  try
    Header := FlowDirections.Header;
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        if not Pitless.Ignore[XIndex, YIndex] then
        begin
          AFlowOrdinal := TFlowOrdinal(Round(FlowDirections.Z[XIndex, YIndex]));
          if AFlowOrdinal <> foMiddle then
          begin
            FlowDir := FlowValues[AFlowOrdinal];
            Z1 := Pitless.Z[XIndex,YIndex];
            Z2 := Pitless.Z[XIndex+FlowDir.FDeltaX,YIndex+FlowDir.FDeltaY];
            if Z1 < Z2 then
            begin
              ShowMessage(IntToStr(XIndex) + ', ' + IntToStr(YIndex));
            end;
            Assert(Z1 >= Z2);
          end;
        end;
      end;
    end;
  finally
    FlowDirections.Free;
    Pitless.Free;
  end;
end;

procedure TForm1.SaveElevationFile(var FileName: string);
var
  YIndex: Integer;
  Header: TGrid7Header;
  SurferFile: TSurferRaster7;
  XIndex: Integer;
begin
  FileName := dlgSave1.FileName;
  SurferFile := TSurferRaster7.Create;
  try
    Header.nRow := 10;
    Header.nCol := 10;
    Header.BlankValue := 1000000;
    Header.xSize := 1;
    Header.ySize := 1;
    Header.zMin := 0;
    Header.zMax := 100;
    SurferFile.Header := Header;
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        //          Angle := XIndex/5*Pi;
        //          SurferFile.Z[XIndex, YIndex] := (Cos(Angle) + 1) * (YIndex + 1) + Random;
        SurferFile.Z[XIndex, YIndex] := {(Cos(Angle) + 1) * (YIndex + 1) +} Random(10);
      end;
    end;
    SurferFile.Z[5, 5] := 17;
    SurferFile.SaveToFile(dlgSave1.FileName);
  finally
    SurferFile.Free;
  end;
end;

procedure TForm1.DisplayEdgePoints(EdgePointBooleanFileName: string);
begin
  DisplayValues(sgEdgePointsBoolean, EdgePointBooleanFileName);
end;

procedure TForm1.DisplayPitless(PitlessElevationFileName: string);
begin
  DisplayValues(sgPitless, PitlessElevationFileName);
end;

procedure TForm1.DisplayFinalFillDir(FlowDirectionsFileName: string);
begin
  DisplayFD(FlowDirectionsFileName, sgFinalFlowdir);
end;

procedure TForm1.DisplaySpillPointsOnGrid(Grid: TStringGrid;
  SpillPoints: TSpillPointList; RasterName: string);
var
  APoint: TPoint;
  ASpillPoint: TSpillPoint;
  Raster: TSurferRaster7File2;
  SpillIndex: Integer;
begin
  Raster := TSurferRaster7File2.Create(RasterName);
  try
    Grid.RowCount := SpillPoints.Count;
    for SpillIndex := 0 to SpillPoints.Count - 1 do
    begin
      ASpillPoint := SpillPoints[SpillIndex];
      Grid.Cells[0, SpillIndex] := IntToStr(ASpillPoint.PitID);
      Grid.Cells[1, SpillIndex] := IntToStr(ASpillPoint.Location);
      APoint := IdToLocation(ASpillPoint.Location, Raster);
      Grid.Cells[2, SpillIndex] := IntToStr(APoint.X);
      Grid.Cells[3, SpillIndex] := IntToStr(APoint.Y);
      Grid.Cells[4, SpillIndex] := FloatToStr(ASpillPoint.Z);
    end;
  finally
    Raster.Free;
  end;
end;

procedure TForm1.DisplayElevations(ElevationFileName: string);
begin
  DisplayValues(sgElevations, ElevationFileName);
end;

procedure TForm1.DisplayFlowDirections(FlowDirectionsFileName: string);
begin
  DisplayFD(FlowDirectionsFileName, sgFlowDirections);
end;

procedure TForm1.DisplayValues(Grid: TStringGrid; SurferGirdFileName: String);
var
  XIndex: Integer;
  Header: TGrid7Header;
  SurferGridFile: TSurferRaster7File2;
  YIndex: Integer;
  RowIndex: Integer;
begin
  SurferGridFile := TSurferRaster7File2.Create(SurferGirdFileName);
  try
    Header := SurferGridFile.Header;
    Grid.ColCount := Header.nCol;
    Grid.RowCount := Header.nRow;
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        RowIndex := Header.nRow - 1 - YIndex;
        Grid.Cells[XIndex, RowIndex] := FloatToStr(SurferGridFile.Z[XIndex, YIndex]);
      end;
    end;
  finally
    SurferGridFile.Free;
  end;
end;

procedure TForm1.DisplayNeighbors(NeighborsFileName: string);
var
  NeighborFile: TFileStream;
  Neighbors: array of TNeighbors;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  NeighborFile := TFile.Open(NeighborsFileName, TFileMode.fmOpen);
  try
    SetLength(Neighbors, NeighborFile.Size div SizeOf(TNeighbors));
    NeighborFile.Read(Neighbors[0], NeighborFile.Size);
    sgNeighbors.RowCount := Length(Neighbors);
    for RowIndex := 0 to Length(Neighbors)-1 do
    begin
      for ColIndex := 0 to Neighbors[RowIndex].Count - 1 do
      begin
        sgNeighbors.Cells[ColIndex,RowIndex] :=
          IntToStr(Neighbors[RowIndex].Neighbors[ColIndex]);
      end;
    end;
  finally
    NeighborFile.Free;
  end;
end;

procedure TForm1.DisplayEndPoints(EndPointFileName: string);
begin
  DisplayValues(sgEndPoints, EndPointFileName);
end;

procedure TForm1.DisplaySpillPoints(SpillPoints: TSpillPointList;
  RasterName: string);
var
  Grid: TStringGrid;
begin
  Grid := sgSpillPoints;
  DisplaySpillPointsOnGrid(Grid, SpillPoints, RasterName);
end;

procedure TForm1.DisplayFD(FlowDirectionsFileName: string; Grid: TStringGrid);
var
  YIndex: Integer;
  Header: TGrid7Header;
  AFlowDirection: TFlowOrdinal;
  XIndex: Integer;
  FlowDirections: TSurferRaster7File2;
  RowIndex: Integer;
begin
  FlowDirections := TSurferRaster7File2.Create(FlowDirectionsFileName);
  try
    Header := FlowDirections.Header;
    Grid.ColCount := Header.nCol;
    Grid.RowCount := Header.nRow;
    for XIndex := 0 to Header.nCol - 1 do
    begin
      for YIndex := 0 to Header.nRow - 1 do
      begin
        RowIndex := Header.nRow - 1 - YIndex;
        AFlowDirection := TFlowOrdinal(Round(FlowDirections.Z[XIndex, YIndex]));
        case AFlowDirection of
          foUpperLeft:
            Grid.Cells[XIndex, RowIndex] := 'foUpperLeft';
          foUpper:
            Grid.Cells[XIndex, RowIndex] := 'foUpper';
          foUpperRight:
            Grid.Cells[XIndex, RowIndex] := 'foUpperRight';
          foLeft:
            Grid.Cells[XIndex, RowIndex] := 'foLeft';
          foMiddle:
            Grid.Cells[XIndex, RowIndex] := 'foMiddle';
          foRight:
            Grid.Cells[XIndex, RowIndex] := 'foRight';
          foLowerLeft:
            Grid.Cells[XIndex, RowIndex] := 'foLowerLeft';
          foLower:
            Grid.Cells[XIndex, RowIndex] := 'foLower';
          foLowerRight:
            Grid.Cells[XIndex, RowIndex] := 'foLowerRight';
        end;
      end;
    end;
  finally
    FlowDirections.Free;
  end;
end;

procedure TForm1.DisplayNewFlowDir(FlowDirectionsFileName: string);
begin
  DisplayFD(FlowDirectionsFileName, sgNewFlowDir);
end;

procedure TForm1.DisplayNewEndPoints(EndPointFileName: string);
begin
  DisplayValues(sgNewEndPoints, EndPointFileName);
end;

end.
