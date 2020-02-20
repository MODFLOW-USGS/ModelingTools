unit Unit6;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, PriorityQueueUnit,
  System.Generics.Collections, Vcl.Mask, JvExMask, JvToolEdit, Vcl.ComCtrls,
  System.IOUtils, JvSpin, Vcl.Grids, RbwDataGrid4, System.Generics.Defaults,
  SubPolygonUnit;

type
  TIntegerPriorityQueue = class(TRbwPriorityQueue<Integer>)
    constructor Create;
  end;

  TTestOutline = class(TOutline)
    procedure Add(Item: TSubPolygon);
  end;

  TForm6 = class(TForm)
    btnTestPriorityQueue: TButton;
    memo2: TMemo;
    btnSimplePitRemoval: TButton;
    btnTestCompoundPitRemoval: TButton;
    btnTestMultipleSimplePits: TButton;
    btnAnotherSimpleTest: TButton;
    btnTransformSurferGridFile: TButton;
    fedInput: TJvFilenameEdit;
    fedOutput: TJvFilenameEdit;
    btnCalculateAccumulation: TButton;
    pb1: TProgressBar;
    btnExportStreams: TButton;
    btnTestSimpleAccumulation: TButton;
    btnExportStreamsRunoffStreamGrid: TButton;
    btn1: TButton;
    btnAssignMValuesFromRaster: TButton;
    fedInputShapefile: TJvFilenameEdit;
    fedOutputShapeFile: TJvFilenameEdit;
    lbl1: TLabel;
    rdgValues: TRbwDataGrid4;
    seRow: TJvSpinEdit;
    seColumn: TJvSpinEdit;
    rdgStartingPoints: TRbwDataGrid4;
    rdgPits: TRbwDataGrid4;
    rdgPitless: TRbwDataGrid4;
    btnTestPitFilling: TButton;
    seX: TJvSpinEdit;
    seY: TJvSpinEdit;
    btnExtract: TButton;
    fedClipShapeFile: TJvFilenameEdit;
    btnRandomGrids: TButton;
    seRandSeed: TJvSpinEdit;
    btnTestRndGrd: TButton;
    btnTestOutline: TButton;
    btnTestFileQueue: TButton;
    sdFileQueueFile: TSaveDialog;
    btnTestFileQueue2: TButton;
    procedure btnTestPriorityQueueClick(Sender: TObject);
    procedure btnSimplePitRemovalClick(Sender: TObject);
    procedure btnTestCompoundPitRemovalClick(Sender: TObject);
    procedure btnTestMultipleSimplePitsClick(Sender: TObject);
    procedure btnAnotherSimpleTestClick(Sender: TObject);
    procedure fedInputChange(Sender: TObject);
    procedure btnTransformSurferGridFileClick(Sender: TObject);
    procedure btn2Click(Sender: TObject);
    procedure btnCalculateAccumulationClick(Sender: TObject);
    procedure btnExportStreamsClick(Sender: TObject);
    procedure btnTestSimpleAccumulationClick(Sender: TObject);
    procedure btnExportStreamsRunoffStreamGridClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure btnAssignMValuesFromRasterClick(Sender: TObject);
    procedure seRowChange(Sender: TObject);
    procedure seColumnChange(Sender: TObject);
    procedure btnTestPitFillingClick(Sender: TObject);
    procedure btnExtractClick(Sender: TObject);
    procedure btnRandomGridsClick(Sender: TObject);
    procedure btnTestRndGrdClick(Sender: TObject);
    procedure btnTestOutlineClick(Sender: TObject);
    procedure btnTestFileQueueClick(Sender: TObject);
    procedure btnTestFileQueue2Click(Sender: TObject);
  private
    procedure DoOnProgress(Sender: TObject; Position, MaxPosition: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

uses
  TestRasterUnit, PitRemovalUnit, RasterValuesAlongSegmentsUnit,
  SurferGridFileReaderUnit, System.Math, ExtractStreamUnit, GenericRasterUnit,
  StreamExporterUnit, ShapefileUnit, FastGEO, ClippedSurferRasterUnit,
  FileQueueUnit;

{$R *.dfm}

{ TIntegerPriorityQueue }

//function TIntegerPriorityQueue.Compare(Left, Right: Integer): Integer;
//begin
//  result := Right - Left;
//end;

procedure TForm6.btnTestPitFillingClick(Sender: TObject);
var
  RastInt: IRaster;
  RowIndex: Integer;
  ColIndex: Integer;
  Rast: TTestRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  PointIndex: integer;
  PitRemover: TPitRemover;
  PitlessRaster: IRaster;
  FlowDirections: TFlowDirections;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  Rast :=  TTestRaster.Create(seColumn.AsInteger, seRow.AsInteger);
  RastInt := Rast;
  for RowIndex := 0 to RastInt.YCount - 1 do
  begin
    for ColIndex := 0 to RastInt.XCount - 1 do
    begin
      Rast.Ignore[ColIndex, RowIndex] := False;
      Rast.Z[ColIndex, RowIndex] := rdgValues.RealValue[ColIndex, RowIndex];
    end;
  end;

  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for PointIndex := 0 to rdgStartingPoints.RowCount - 1 do
    begin
      if TryStrToInt(rdgStartingPoints.Cells[0,PointIndex], APoint.X)
        and TryStrToInt(rdgStartingPoints.Cells[1,PointIndex], APoint.Y) then
      begin
        EdgeStartPoints.Add(APoint);
      end;
    end;

    Assert(EdgeStartPoints.Count > 0);

    for PointIndex := 0 to rdgPits.RowCount - 1 do
    begin
      if TryStrToInt(rdgPits.Cells[0,PointIndex], APoint.X)
        and TryStrToInt(rdgPits.Cells[1,PointIndex], APoint.Y) then
      begin
        Pits.Add(APoint);
      end;
    end;

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(RastInt, EdgeStartPoints, Pits, PitlessRaster,
        FlowDirections, CutCriteria, Changed);
    finally
      PitRemover.Free;
    end;

    rdgPitless.RowCount := PitlessRaster.YCount;
    rdgPitless.ColCount := PitlessRaster.XCount;
    for RowIndex := 0 to PitlessRaster.YCount - 1 do
    begin
      for ColIndex := 0 to PitlessRaster.XCount - 1 do
      begin
        rdgPitless.RealValue[ColIndex,RowIndex] := PitlessRaster.Z[ColIndex,RowIndex];
      end;
    end;

  finally
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTestPriorityQueueClick(Sender: TObject);
var
  Queue: TIntegerPriorityQueue;
  AValue: Integer;
  index: Integer;
  List: TList<Integer>;
begin
  Queue := TIntegerPriorityQueue.Create;
  List := TList<Integer>.Create;
  try
    memo2.Lines.Clear;
    for index := 1 to 100 do
    begin
      AValue := Random(500);
      List.Add(AValue);
      Queue.Enqueue(AValue);
    end;
    Assert(Queue.Count = 100);
    List.Sort;
    for index := 0 to 99 do
    begin
      AValue := Queue.Dequeue;
      Assert(AValue = List[index]);
      memo2.Lines.Add(IntToStr(AValue) + ' ' + IntToStr(List[index]));
    end;
  finally
    Queue.Free;
    List.Free;
  end;
end;

procedure TForm6.btnTestRndGrdClick(Sender: TObject);
var
  ARaster: TTestRaster;
  ARasterI: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  OuterIndex: Integer;
  InnerIndex: Integer;
  RandIndex: Integer;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  RandSeed := 0;

  ARaster := TTestRaster.Create(10, 10);
  ARasterI := ARaster;
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  rdgValues.RowCount := 10;
  rdgValues.ColCount := 10;
  rdgPitless.RowCount := 10;
  rdgPitless.ColCount := 10;


  try
    memo2.Lines.Clear;
    RandIndex := seRandSeed.AsInteger;
//    for RandIndex := 1 to 1000 do
    begin
      RandSeed := RandIndex;
      for OuterIndex := 0 to ARaster.XCount - 1 do
      begin
        for InnerIndex := 0 to ARaster.YCount - 1 do
        begin
          ARaster.Ignore[OuterIndex,InnerIndex] := False;
          ARaster.Z[OuterIndex,InnerIndex] := Random;
          rdgValues.RealValue[OuterIndex,InnerIndex] := ARaster.Z[OuterIndex,InnerIndex];
        end;
      end;
      EdgeStartPoints.Clear;
      Pits.Clear;
      IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, nil, DoOnProgress);


      PitRemover := TPitRemover.Create;
      try
        Changed := False;
        PitRemover.RemovePits(ARasterI, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
        FlowDirections.Free;
      finally
        PitRemover.Free;
      end;

      for OuterIndex := 0 to ARaster.XCount - 1 do
      begin
        for InnerIndex := 0 to ARaster.YCount - 1 do
        begin
          ARaster.Ignore[OuterIndex,InnerIndex] := False;
          ARaster.Z[OuterIndex,InnerIndex] := NewRaster.Z[OuterIndex,InnerIndex];
          rdgPitless.RealValue[OuterIndex,InnerIndex] := ARaster.Z[OuterIndex,InnerIndex];
        end;
      end;
      EdgeStartPoints.Clear;
      Pits.Clear;
      IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, nil, DoOnProgress);

      PitRemover := TPitRemover.Create;
      try
        Changed := False;
        PitRemover.RemovePits(ARasterI, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
        FlowDirections.Free;
      finally
        PitRemover.Free;
      end;

      if Changed then
      begin
        memo2.Lines.Add(IntToStr(RandIndex));
      end;
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTestSimpleAccumulationClick(Sender: TObject);
var
  ARaster: TTestRaster;
  XIndex: Integer;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  PitlessRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  YIndex: Integer;
//  NumberOfInflowingStreams: T2DIntArray;
  MinArea: Integer;
  Streams: TStreamObjectList;
  AccumulationRaster: IRaster;
  AStream: TGeoStream;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  ARaster := TTestRaster.Create(10, 10);
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for XIndex := 0 to ARaster.XCount - 1 do
    begin
      for YIndex := 0 to ARaster.YCount-1 do
      begin
        ARaster.Ignore[XIndex,0] := False;
        if XIndex < 5 then
        begin
          ARaster.Z[XIndex,YIndex] := 20 - XIndex
        end
        else
        begin
          ARaster.Z[XIndex,YIndex] := 10 + XIndex
        end;
        ARaster.Z[XIndex,YIndex] := ARaster.Z[XIndex,YIndex] + YIndex * 2 + Random;
      end;
    end;

    APoint.X := 5;
    APoint.Y := 0;
    EdgeStartPoints.Add(APoint);

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(ARaster, EdgeStartPoints, Pits, PitlessRaster, FlowDirections, CutCriteria, Changed);

      MinArea := 4;
//        MinArea := 200000;
      ComputeAccumulation(PitlessRaster, FlowDirections, AccumulationRaster,
        MinArea, Streams, DoOnProgress);

      FlowDirections.Free;
    finally
      PitRemover.Free;
    end;

    Memo2.Lines.BeginUpdate;
    try
      Memo2.Lines.Clear;
      for AStream in Streams do
      begin
        for APoint in AStream do
        begin
          Memo2.Lines.Add(Format('%d %d', [APoint.x, APoint.y]));
        end;
        Memo2.Lines.Add('');
      end;
    finally
      Memo2.Lines.EndUpdate;
    end;

    Streams.Free;


  finally
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTransformSurferGridFileClick(Sender: TObject);
var
  Raster: TSurferRaster7;
  RastInterface: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemover;
  FlowDirections: TFlowDirections;
  NewRaster: IRaster;
  RowIndex: Integer;
  ColIndex: Integer;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  Assert(FileExists(fedInput.FileName));
  Raster := TSurferRaster7.Create;
  Screen.Cursor := crHourGlass;
  try
    RastInterface := Raster ;
    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    try
      IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      PitRemover := TPitRemover.Create;
      try
        PitRemover.OnProgress := DoOnProgress;
        Changed := False;
        PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, NewRaster,
          FlowDirections, CutCriteria, Changed);

        FlowDirections.Free;
        for RowIndex := 0 to NewRaster.YCount - 1 do
        begin
          for ColIndex := 0 to NewRaster.XCount - 1 do
          begin
            Raster.Z[ColIndex,RowIndex] := NewRaster.Z[ColIndex,RowIndex];
          end;
        end;
        Raster.SaveToFile(fedOutput.FileName);
      finally
        PitRemover.Free;
      end;
    finally
      EdgeStartPoints.Free;
      Pits.Free;
    end;
  finally
    RastInterface := nil;
    Screen.Cursor := crDefault;
  end;

end;

procedure TForm6.DoOnProgress(Sender: TObject; Position, MaxPosition: Integer);
begin
  pb1.Position := Position;
end;

procedure TForm6.fedInputChange(Sender: TObject);
begin
  if fedOutput.FileName = '' then
  begin
    fedOutput.FileName := ChangeFileExt(fedInput.FileName, '') + 'Output.grd'
  end;
end;

procedure TForm6.seColumnChange(Sender: TObject);
begin
  rdgValues.ColCount  := seColumn.AsInteger;
end;

procedure TForm6.seRowChange(Sender: TObject);
begin
  rdgValues.RowCount := seRow.AsInteger;
end;

procedure TForm6.btnTestMultipleSimplePitsClick(Sender: TObject);
var
  ARaster: TTestRaster;
  Index: Integer;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  ARaster := TTestRaster.Create(26, 1);
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for Index := 0 to ARaster.XCount - 1 do
    begin
      ARaster.Ignore[Index,0] := False;
    end;
    ARaster.Z[0,0] := 0;
    ARaster.Z[1,0] := 1;
    ARaster.Z[2,0] := 1;
    ARaster.Z[3,0] := 2;
    ARaster.Z[4,0] := 6;
    ARaster.Z[5,0] := 6;
    ARaster.Z[6,0] := 6;
    ARaster.Z[7,0] := 3;
    ARaster.Z[8,0] := 1;
    ARaster.Z[9,0] := 2;
    ARaster.Z[10,0] := 3;
    ARaster.Z[11,0] := 3;
    ARaster.Z[12,0] := 4;
    ARaster.Z[13,0] := 5;
    ARaster.Z[14,0] := 7;
    ARaster.Z[15,0] := 8;
    ARaster.Z[16,0] := 8;
    ARaster.Z[17,0] := 8;
    ARaster.Z[18,0] := 8;
    ARaster.Z[19,0] := 8;
    ARaster.Z[20,0] := 8;
    ARaster.Z[21,0] := 8;
    ARaster.Z[22,0] := 8;
    ARaster.Z[23,0] := 8;
    ARaster.Z[24,0] := 4;
    ARaster.Z[25,0] := 8;

    APoint.X := 0;
    APoint.Y := 0;
    EdgeStartPoints.Add(APoint);
    APoint.X := 8;
    Pits.Add(APoint);
    APoint.X := 24;
    Pits.Add(APoint);

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(ARaster, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
    finally
      FlowDirections.Free;
      PitRemover.Free;
    end;

    memo2.Lines.Clear;

    for Index := 0 to NewRaster.XCount - 1 do
    begin
      memo2.Lines.Add(FloatToStr(NewRaster.Z[Index,0]))
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTestOutlineClick(Sender: TObject);
var
  ARaster: TTestRaster;
  ARasterI: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PointIndex: Integer;
  APoint: TPoint;
  OuterIndex: Integer;
  InnerIndex: Integer;
  FoundSpecialPoint: Boolean;
  SpecialPointIndex: integer;
  Points: TRealPointArray;
  ASubPolygon: TSubPolygon;
  AnOutline: TTestOutline;
begin
  ARaster := TTestRaster.Create(10, 10);
  ARasterI := ARaster;
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  rdgValues.RowCount := 10;
  rdgValues.ColCount := 10;
  rdgPitless.RowCount := 10;
  rdgPitless.ColCount := 4;
  Randomize;

  try
      for OuterIndex := 0 to ARaster.XCount - 1 do
      begin
        for InnerIndex := 0 to ARaster.YCount - 1 do
        begin
          ARaster.Ignore[OuterIndex,InnerIndex] := False;
          ARaster.Z[OuterIndex,InnerIndex] := Random;
          rdgValues.RealValue[OuterIndex,InnerIndex] := ARaster.Z[OuterIndex,InnerIndex];
        end;
      end;
      SpecialPointIndex := 5;
      ARaster.Z[SpecialPointIndex,SpecialPointIndex] := -1;

      EdgeStartPoints.Clear;
      Pits.Clear;
      IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, nil, DoOnProgress);

      Assert(Pits.Count > 0);
      FoundSpecialPoint := False;
      for PointIndex := 0 to Pits.Count - 1 do
      begin
        APoint := Pits[PointIndex];
        if (APoint.X = SpecialPointIndex) and (APoint.Y = SpecialPointIndex) then
        begin
          FoundSpecialPoint := True;
          break;
        end;
      end;

      if not FoundSpecialPoint then
      begin
        Beep;
        ShowMessage('Failed');
        Exit;
      end;

      FoundSpecialPoint := False;
      for PointIndex := 0 to EdgeStartPoints.Count - 1 do
      begin
        APoint := EdgeStartPoints[PointIndex];
        if (APoint.X = SpecialPointIndex) and (APoint.Y = SpecialPointIndex) then
        begin
          FoundSpecialPoint := True;
          break;
        end;
      end;

      if FoundSpecialPoint then
      begin
        Beep;
        ShowMessage('Failed');
        Exit;
      end;

      AnOutline := TTestOutline.Create;
      try
        SetLength(Points, 5);
        Points[0].x := SpecialPointIndex -1;
        Points[0].Y := SpecialPointIndex -1;

        Points[1].x := SpecialPointIndex +1;
        Points[1].Y := SpecialPointIndex -1;

        Points[2].x := SpecialPointIndex +1;
        Points[2].Y := SpecialPointIndex +1;

        Points[3].x := SpecialPointIndex -1;
        Points[3].Y := SpecialPointIndex +1;

        Points[4] := Points[0];

        ASubPolygon := TSubPolygon.Create(Points, 5, 0, 0);

        AnOutline.Add(ASubPolygon);

        EdgeStartPoints.Clear;
        Pits.Clear;
        IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, AnOutline, DoOnProgress);

      finally
        AnOutline.Free;
      end;

      FoundSpecialPoint := False;
      for PointIndex := 0 to Pits.Count - 1 do
      begin
        APoint := Pits[PointIndex];
        if (APoint.X = SpecialPointIndex) and (APoint.Y = SpecialPointIndex) then
        begin
          FoundSpecialPoint := True;
          break;
        end;
      end;

      if FoundSpecialPoint then
      begin
        Beep;
        ShowMessage('Failed');
        Exit;
      end;

      Assert(EdgeStartPoints.Count > 0);
      FoundSpecialPoint := False;
      for PointIndex := 0 to EdgeStartPoints.Count - 1 do
      begin
        APoint := EdgeStartPoints[PointIndex];
        if (APoint.X = SpecialPointIndex) and (APoint.Y = SpecialPointIndex) then
        begin
          FoundSpecialPoint := True;
          break;
        end;
      end;

      if not FoundSpecialPoint then
      begin
        Beep;
        ShowMessage('Failed');
        Exit;
      end;

      ShowMessage('Success');
  finally
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnAnotherSimpleTestClick(Sender: TObject);
var
  ARaster: TTestRaster;
  Index: Integer;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  ARaster := TTestRaster.Create(17, 1);
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for Index := 0 to ARaster.XCount - 1 do
    begin
      ARaster.Ignore[Index,0] := False;
    end;
    ARaster.Z[0,0] := 0;
    ARaster.Z[1,0] := 3;
    ARaster.Z[2,0] := 3;
    ARaster.Z[3,0] := 3;
    ARaster.Z[4,0] := 3;
    ARaster.Z[5,0] := 3;
    ARaster.Z[6,0] := 2;
    ARaster.Z[7,0] := 3;
    ARaster.Z[8,0] := 1;
    ARaster.Z[9,0] := 2;
    ARaster.Z[10,0] := 7;
    ARaster.Z[11,0] := 7;
    ARaster.Z[12,0] := 7;
    ARaster.Z[13,0] := 7;
    ARaster.Z[14,0] := 7;
    ARaster.Z[15,0] := 9;
    ARaster.Z[16,0] := 9;

    APoint.X := 0;
    APoint.Y := 0;
    EdgeStartPoints.Add(APoint);
    APoint.X := 6;
    Pits.Add(APoint);
    APoint.X := 8;
    Pits.Add(APoint);

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(ARaster, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
    finally
      FlowDirections.Free;
      PitRemover.Free;
    end;

    memo2.Lines.Clear;

    for Index := 0 to NewRaster.XCount - 1 do
    begin
      memo2.Lines.Add(FloatToStr(NewRaster.Z[Index,0]))
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

function Compareshapes(Left, Right: Pointer): integer;
var
  LeftShape: TShapeObject;
  RightShape: TShapeObject;
begin
  LeftShape := Left;
  RightShape := Right;
  result := Sign(LeftShape.FPoints[0].Y - RightShape.FPoints[0].Y);
end;

procedure TForm6.btnAssignMValuesFromRasterClick(Sender: TObject);
var
  Raster: IRasterFile;
  ShapefileReader: TShapefileGeometryReader;
  ShapefileWriter: TShapefileGeometryWriter;
  Shapetype: integer;
  ShapeIndex: Integer;
  AShape: TShapeObject;
  PointIndex: Integer;
  APoint: TShapePoint;
  LowerLeft: TPoint2D;
  XSpacing: Double;
  YSpacing: Double;
  SRaster: TSurferRaster7File2;
  IgnoreValue: Double;
  XIndex: integer;
  XCount: Integer;
  YCount: Integer;
  AValue: Double;
  YIndex: integer;
  ShapeList: TList;
  MaxDeltaY: double;
  DeltaY: Double;
  CachedRowCount: integer;
//  Shapetype: integer;
begin
  Assert(fedInput.FileName <> '');
  Assert(TFile.Exists(fedInput.FileName));
  Assert(fedInputShapefile.FileName <> '');
  Assert(TFile.Exists(fedInputShapefile.FileName));
  Assert(fedOutputShapefile.FileName <> '');


  Screen.Cursor := crHourGlass;
  ShapeList := TList.Create;
  SRaster := TSurferRaster7File2.Create(fedInput.FileName);
  Raster := SRaster;
  ShapefileReader := TShapefileGeometryReader.Create;
  try
    ShapefileReader.ReadFromFile(fedInputShapefile.FileName,
      ChangeFileExt(fedInputShapefile.FileName, '.shx'));

    LowerLeft := Raster.LowerLeft;
    XSpacing := Raster.XSpacing;
    YSpacing := Raster.YSpacing;
    IgnoreValue := SRaster.IgnoreValue;
    XCount := Raster.XCount;
    YCount := Raster.YCount;

    Shapetype := ShapefileReader.FileHeader.ShapeType;
    if Shapetype in [stPoint, stPolyLine, stPolygon, stMultiPoint] then
    begin
      Shapetype := Shapetype + 20;
    end;
    Assert(Shapetype in [stPointM, stPolyLineM, stPolygonM, stMultiPointM,
      stPointM, stPolyLineM, stPolygonM, stMultiPointM]);


    ShapefileWriter := TShapefileGeometryWriter.Create(Shapetype, False);
    try
      MaxDeltaY := 0.;
      for ShapeIndex := 0 to ShapefileReader.Count - 1 do
      begin
  //      pb1.Position := ShapeIndex;
        AShape := ShapefileReader[ShapeIndex];
        ShapeList.Add(AShape);
        SetLength(AShape.FMArray, AShape.FNumPoints);
        AShape.FShapeType := Shapetype;
        ShapefileWriter.AddShape(AShape);
        DeltaY := Abs(AShape.FPoints[0].Y - AShape.FPoints[AShape.FNumPoints-1].Y);
        if DeltaY > MaxDeltaY then
        begin
          MaxDeltaY := DeltaY;
        end;
      end;
      ShapeList.Sort(Compareshapes);

      CachedRowCount := Trunc(MaxDeltaY/YSpacing)+1;
      if CachedRowCount > 1000 then
      begin
        CachedRowCount := 1000;
      end;
      SRaster.BufferedRowCount := CachedRowCount;

      pb1.Max := ShapefileReader.Count;
      for ShapeIndex := 0 to ShapeList.Count - 1 do
      begin
        pb1.Position := ShapeIndex;
        AShape := ShapeList[ShapeIndex];

        for PointIndex := 0 to AShape.FNumPoints - 1 do
        begin
          APoint := AShape.FPoints[PointIndex];

          XIndex := Trunc((APoint.X - LowerLeft.x)/XSpacing);
          YIndex := Trunc((APoint.y - LowerLeft.y)/YSpacing);
          if (XIndex >= 0) and (YIndex >= 0)
            and (XIndex < XCount) and (YIndex < YCount) then
          begin
            try
              AValue := Raster.Z[XIndex,YIndex];
            except
              begin
                ShowMessage(IntToStr(ShapeIndex));
                raise
              end;
            end;
          end
          else
          begin
            AValue := IgnoreValue;
          end;

          AShape.FMArray[PointIndex] := AValue;
        end;

      end;

      ShapefileWriter.WriteToFile(fedOutputShapeFile.FileName,
        ChangeFileExt(fedOutputShapeFile.FileName, '.shx'));
    finally
      ShapefileWriter.Free;
    end;
  finally
    ShapefileReader.Free;
    Screen.Cursor := crDefault;
    ShapeList.Free;
  end;
end;

procedure TForm6.btnExportStreamsClick(Sender: TObject);
var
  Raster: TSurferRaster7;
  RastInterface: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemover;
  FlowDirections: TFlowDirections;
  PitlessRaster: IRaster;
//  RowIndex: Integer;
//  ColIndex: Integer;
  AccumulationRaster: IRaster;
//  MinZ: Double;
//  MaxZ: Double;
//  Z: Double;
//  Header: TGrid7Header;
//  NumberOfInflowingStreams: T2DIntArray;
  MinArea: Integer;
  Streams: TStreamObjectList;
//  AStream: TGeoStream;
//  APoint: TPoint;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  Assert(FileExists(fedInput.FileName));
  Screen.Cursor := crHourGlass;
  Raster := TSurferRaster7.Create;
  try
    RastInterface := Raster ;
    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    try
      IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      PitRemover := TPitRemover.Create;
      try
        PitRemover.OnProgress := DoOnProgress;
        Changed := False;
        PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitlessRaster,
          FlowDirections, CutCriteria, Changed);

//        MinArea := 4;
        MinArea := 2000;
        ComputeAccumulation(PitlessRaster, FlowDirections, AccumulationRaster,
          MinArea, Streams, DoOnProgress);

        FlowDirections.Free;
//        StreamStartPoints.Free;

        ExportStreams(Streams, PitlessRaster, AccumulationRaster,
          ChangeFileExt(fedInput.FileName, '.shp'));

        Streams.Free;

      finally
        PitRemover.Free;
      end;
    finally
      EdgeStartPoints.Free;
      Pits.Free;
    end;
  finally
    RastInterface := nil;
    Screen.Cursor := crDefault;
  end;

end;

procedure TForm6.btnExportStreamsRunoffStreamGridClick(Sender: TObject);
var
  Raster: TClippedSurferRaster;
  RastInterface: IRasterFile;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemoverSurfGrid7;
  FlowDirections: TFlowDirections;
  PitlessRaster: IRasterFile;
//  PitlessRaster2: IRasterFile;
  AccumulationRaster: IRasterFile;
//  NumberOfInflowingStreams: T2DIntArray;
  MinArea: Integer;
  Streams: TStreamObjectList;
  RunoffRaster: IRasterFile;
  StartTime: TDateTime;
//  NewFileName: string;
  Changed: Boolean;
  RastI: IRaster;
  PitI: IRaster;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  StartTime := Now;
  Assert(FileExists(fedInput.FileName));
  Screen.Cursor := crHourGlass;
  Raster := TClippedSurferRaster.Create(fedInput.FileName);
  try
    Raster.IncludeShapeFileName := fedClipShapeFile.FileName;

    RastInterface := Raster ;
    RastI := Raster;
//    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    try
      lbl1.Caption := 'Identifying start points';
      Application.ProcessMessages;
      IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      PitRemover := TPitRemoverSurfGrid7.Create;
      try
        PitRemover.OnProgress := DoOnProgress;
      lbl1.Caption := 'removing pits';
      Application.ProcessMessages;
      PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitI,
          FlowDirections, CutCriteria, Changed);
      PitlessRaster := PitI as IRasterFile;
      while Changed do
      begin
        RastInterface := PitlessRaster;
        EdgeStartPoints.Free;
        Pits.Free;
        PitlessRaster := nil;
        lbl1.Caption := 'Identifying start points';
        Application.ProcessMessages;
        IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      lbl1.Caption := 'removing pits';
      Application.ProcessMessages;
        PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitI,
          FlowDirections, CutCriteria, Changed);
        PitlessRaster := PitI as IRasterFile;
      end;
//      NewFileName := ChangeFileExt(fedInput.FileName, '');
//      NewFileName := NewFileName + '_Pitless.grd';
//      if TFile.Exists(NewFileName) then
//      begin
//        TFile.Delete(NewFileName);
//      end;
//      TFile.Copy(PitlessRaster.FileName, NewFileName);


        MinArea := 20000;
      lbl1.Caption := 'extracting streams';
      Application.ProcessMessages;
        ComputeAccumulationSurfModified(PitlessRaster, FlowDirections, AccumulationRaster,
          MinArea, Streams, DoOnProgress);

      lbl1.Caption := 'identifying contributing areas';
      Application.ProcessMessages;
        ExtractRunoffLocationsSurf(Streams, FlowDirections, PitlessRaster,
          RunoffRaster, DoOnProgress);

//        for XIndex := 0 to RunoffRaster.XCount - 1 do
//        begin
//          for YIndex := 0 to RunoffRaster.YCount - 1 do
//          begin
//            Raster.Z[XIndex, YIndex] := RunoffRaster.Z[XIndex, YIndex];
//          end;
//        end;

//        Raster.SaveToFile(fedOutput.FileName);

      lbl1.Caption := 'copying contributing areas';
      Application.ProcessMessages;
        RunoffRaster.Flush;
        if TFile.Exists(fedOutput.FileName) then
        begin
          TFile.Delete(fedOutput.FileName);
        end;
        TFile.Copy(RunoffRaster.FileName, fedOutput.FileName);

        FlowDirections.Free;

      lbl1.Caption := 'writng streams';
      Application.ProcessMessages;
        ExportStreams(Streams, PitlessRaster, AccumulationRaster,
          ChangeFileExt(fedInput.FileName, '.shp'));

        Streams.Free;

      finally
        PitRemover.Free;
      end;
    finally
      EdgeStartPoints.Free;
      Pits.Free;
    end;
  finally
    RastInterface := nil;
    Screen.Cursor := crDefault;
    memo2.Lines.Add(FloatToStr((Now - StartTime)*3600*24));
    ShowMessage('Done');
  end;

end;

procedure TForm6.btnExtractClick(Sender: TObject);
var
  AGrid: TSurferRaster7File2;
  XStart: integer;
  XEnd: Integer;
  YStart: integer;
  YEnd: Integer;
  YIndex: Integer;
  XIndex: Integer;
  AValue: Double;
  RasterInt: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemover;
  PitlessRaster: IRaster;
  FlowDirections: TFlowDirections;
  Raster: TTestRaster;
//  RowIndex: Integer;
  MinArea: Integer;
  AccumulationRaster: IRaster;
//  NumberOfInflowingStreams: T2DIntArray;
  Streams: TStreamObjectList;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  if TFile.Exists(fedInput.FileName) then
  begin
    Raster := TTestRaster.Create(seColumn.AsInteger, seRow.AsInteger);
    RasterInt := Raster;
    AGrid := TSurferRaster7File2.Create(fedInput.FileName);
    try
      XStart := Max(0, seX.AsInteger - seColumn.AsInteger div 2);
      XEnd := XStart + seColumn.AsInteger-1;
      if XEnd >= AGrid.XCount then
      begin
        XEnd := AGrid.XCount-1;
        XStart := XEnd - seColumn.AsInteger+1;
        Assert(XStart >= 0);
      end;

      YStart := Max(0, seY.AsInteger - seRow.AsInteger div 2);
      YEnd := YStart + seRow.AsInteger-1;
      if YEnd >= AGrid.YCount then
      begin
        YEnd := AGrid.YCount-1;
        YStart := YEnd - seRow.AsInteger+1;
        Assert(YStart >= 0);
      end;

      for YIndex := YStart to YEnd do
      begin
        for XIndex := XStart to XEnd do
        begin
          AValue := AGrid.Z[XIndex,YIndex];
          rdgValues.RealValue[XIndex-XStart,YIndex-YStart] := AValue;
          Raster.Z[XIndex-XStart,YIndex-YStart] := AValue;
          Raster.Ignore[XIndex-XStart,YIndex-YStart]
            := AGrid.Ignore[XIndex,YIndex];
        end;
      end;
      try
        IdentifyStartPointsAndPits(RasterInt, EdgeStartPoints, Pits, nil, DoOnProgress);
        PitRemover := TPitRemover.Create;
        try
//          PitRemover.OnProgress := DoOnProgress;
          Changed := False;
          PitRemover.RemovePits(RasterInt, EdgeStartPoints, Pits, PitlessRaster,
            FlowDirections, CutCriteria, Changed);

          rdgPitless.BeginUpdate;
          rdgPitless.RowCount := PitlessRaster.YCount;
          rdgPitless.ColCount := PitlessRaster.XCount;

          for YIndex := 0 to PitlessRaster.YCount - 1 do
          begin
            for XIndex := 0 to PitlessRaster.XCount - 1 do
            begin
              rdgPitless.RealValue[XIndex,YIndex] := PitlessRaster.Z[XIndex,YIndex];
            end;
          end;
          rdgPitless.EndUpdate;


        MinArea := 4;
      lbl1.Caption := 'extracting streams';
      Application.ProcessMessages;
        ComputeAccumulation(PitlessRaster, FlowDirections, AccumulationRaster,
           MinArea, Streams, nil);

        finally
          PitRemover.Free;
        end;

      finally
        EdgeStartPoints.Free;
        Pits.Free;
      end;

    finally
      AGrid.Free;
    end;
  end;
end;

procedure TForm6.btnRandomGridsClick(Sender: TObject);
var
  ARaster: TTestRaster;
  ARasterI: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  OuterIndex: Integer;
  InnerIndex: Integer;
  RandIndex: Integer;
  XIndex: Integer;
  XStart: Integer;
  XEnd: Integer;
  YStart: Integer;
  YEnd: Integer;
  ZMin: Double;
  YIndex: Integer;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  RandSeed := 0;

  ARaster := TTestRaster.Create(10, 10);
  ARasterI := ARaster;
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
//  Randomize;

  pb1.Max := 1000000;
  try
    memo2.Lines.Clear;
    for RandIndex := 1 to pb1.Max do
    begin
      pb1.StepIt;
      RandSeed := RandIndex;
      for OuterIndex := 0 to ARaster.XCount - 1 do
      begin
        for InnerIndex := 0 to ARaster.YCount - 1 do
        begin
          ARaster.Ignore[OuterIndex,InnerIndex] := False;
          ARaster.Z[OuterIndex,InnerIndex] := Random;
        end;
      end;
      EdgeStartPoints.Clear;
      Pits.Clear;
      IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, nil, nil);


      PitRemover := TPitRemover.Create;
      try
        Changed := False;
        PitRemover.RemovePits(ARasterI, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
        FlowDirections.Free;
      finally
        PitRemover.Free;
      end;

      for OuterIndex := 0 to ARaster.XCount - 1 do
      begin
        for InnerIndex := 0 to ARaster.YCount - 1 do
        begin
          ARaster.Ignore[OuterIndex,InnerIndex] := False;
          ARaster.Z[OuterIndex,InnerIndex] := NewRaster.Z[OuterIndex,InnerIndex]
        end;
      end;
      EdgeStartPoints.Clear;
      Pits.Clear;
      IdentifyStartPointsAndPits(ARasterI, EdgeStartPoints, Pits, nil, nil);

      PitRemover := TPitRemover.Create;
      try
        Changed := False;
        PitRemover.RemovePits(ARasterI, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
        FlowDirections.Free;
      finally
        PitRemover.Free;
      end;

      if Changed then
      begin
        Changed := False;
        // test starting raster
        for OuterIndex := 1 to ARasterI.XCount - 2 do
        begin
          XStart := OuterIndex-1;
          XEnd := OuterIndex+1;
          for InnerIndex := 1 to ARasterI.YCount - 2 do
          begin
            YStart := InnerIndex-1;
            YEnd := InnerIndex+1;
            ZMin := ARasterI.Z[OuterIndex,InnerIndex];
            for XIndex := XStart to XEnd do
            begin
              for YIndex := YStart to YEnd do
              begin
                if (XIndex = OuterIndex) and (YIndex = InnerIndex) then
                begin
                  Continue;
                end;
                if ARasterI.Z[XIndex,YIndex] < ZMin then
                begin
                  ZMin := ARasterI.Z[XIndex,YIndex];
                end;
              end;
            end;
            if ARasterI.Z[OuterIndex,InnerIndex] < ZMin then
            begin
              Changed := True;
            end;
          end;
        end;
      end;

      if Changed then
      begin
        memo2.Lines.Add(IntToStr(RandIndex));
      end;
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
  ShowMessage('Done');
end;

procedure TForm6.btn1Click(Sender: TObject);
var
  Raster: TSurferRaster7;
  RastInterface: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemover;
  FlowDirections: TFlowDirections;
  PitlessRaster: IRaster;
  AccumulationRaster: IRaster;
//  NumberOfInflowingStreams: T2DIntArray;
  MinArea: Integer;
  Streams: TStreamObjectList;
  RunoffRaster: IRaster;
  XIndex: Integer;
  YIndex: Integer;
  StartTime: TDateTime;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  StartTime := Now;
  Assert(FileExists(fedInput.FileName));
  Screen.Cursor := crHourGlass;
  Raster := TSurferRaster7.Create;
  try
    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    RastInterface := Raster ;
    try
      IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      PitRemover := TPitRemover.Create;
      try
        PitRemover.OnProgress := DoOnProgress;
        Changed := False;
        PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitlessRaster,
          FlowDirections, CutCriteria, Changed);

        MinArea := 20000;
        ComputeAccumulation(PitlessRaster, FlowDirections, AccumulationRaster,
           MinArea, Streams, DoOnProgress);

        ExtractRunoffLocations(Streams, FlowDirections, PitlessRaster,
          RunoffRaster, DoOnProgress);

        for YIndex := 0 to RunoffRaster.YCount - 1 do
        begin
          for XIndex := 0 to RunoffRaster.XCount - 1 do
          begin
            Raster.Z[XIndex, YIndex] := RunoffRaster.Z[XIndex, YIndex];
          end;
        end;

        Raster.SaveToFile(fedOutput.FileName);

//        RunoffRaster.Flush;
//        if TFile.Exists(fedOutput.FileName) then
//        begin
//          TFile.Delete(fedOutput.FileName);
//        end;
//        TFile.Copy(RunoffRaster.FileName, fedOutput.FileName);

        FlowDirections.Free;

        ExportStreams(Streams, PitlessRaster, AccumulationRaster,
          ChangeFileExt(fedInput.FileName, '.shp'));

        Streams.Free;

      finally
        PitRemover.Free;
      end;
    finally
      EdgeStartPoints.Free;
      Pits.Free;
    end;
  finally
    RastInterface := nil;
    Screen.Cursor := crDefault;
    memo2.Lines.Add(FloatToStr((Now - StartTime)*3600*24));
    ShowMessage('Done');
  end;

end;

procedure TForm6.btn2Click(Sender: TObject);
var
  Raster: TSurferRaster7;
  XIndex: Integer;
  YIndex: Integer;
  StartX: Integer;
  StartY: Integer;
  EndX: Integer;
  EndY: Integer;
  InnerYIndex: Integer;
  InnerXIndex: Integer;
  IsBoundary: boolean;
begin
  Raster := TSurferRaster7.Create;
  try
    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    for XIndex := 0 to Raster.XCount - 1 do
    begin
      for YIndex := 0 to Raster.YCount - 1 do
      begin
        if Raster.Z[XIndex,YIndex] = 0 then
        begin
          StartX := Max(0, XIndex-1);
          StartY := Max(0, YIndex-1);
          EndX := Min(Raster.XCount - 1, XIndex+1);
          EndY := Min(Raster.YCount - 1, YIndex+1);
          IsBoundary := True;
          for InnerXIndex := StartX to EndX do
          begin
            for InnerYIndex := StartY to EndY do
            begin
              if Raster.Z[InnerXIndex,InnerYIndex] <> 0 then
              begin
                IsBoundary := false;
                break;
              end;
            end;
            if not IsBoundary then
            begin
              Break;
            end;
          end;
          if IsBoundary then
          begin
            Raster.Z[XIndex,YIndex] := Raster.Header.BlankValue;
          end;
        end;
      end;
    end;
    Raster.SaveToFile(fedOutput.FileName);
  finally
    Raster.Free;
  end;
end;

procedure TForm6.btnCalculateAccumulationClick(Sender: TObject);
var
  Raster: TSurferRaster7;
  RastInterface: IRaster;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  PitRemover: TPitRemover;
  FlowDirections: TFlowDirections;
  PitlessRaster: IRaster;
  RowIndex: Integer;
  ColIndex: Integer;
  AccumulationRaster: IRaster;
  MinZ: Double;
  MaxZ: Double;
  Z: Double;
  Header: TGrid7Header;
//  NumberOfInflowingStreams: T2DIntArray;
  MinArea: Integer;
  Streams: TStreamObjectList;
  AStream: TGeoStream;
  APoint: TPoint;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  Assert(FileExists(fedInput.FileName));
  Screen.Cursor := crHourGlass;
  Raster := TSurferRaster7.Create;
  try
    RastInterface := Raster ;
    ReadSurfer7GrdFile(fedInput.FileName, Raster);
    try
      IdentifyStartPointsAndPits(RastInterface, EdgeStartPoints, Pits, nil, DoOnProgress);
      PitRemover := TPitRemover.Create;
      try
        PitRemover.OnProgress := DoOnProgress;
        Changed := False;
        PitRemover.RemovePits(RastInterface, EdgeStartPoints, Pits, PitlessRaster,
          FlowDirections, CutCriteria, Changed);

//        MinArea := 4;
        MinArea := 20000;
        ComputeAccumulation(PitlessRaster, FlowDirections, AccumulationRaster,
           MinArea, Streams, DoOnProgress);

        FlowDirections.Free;
//        StreamStartPoints.Free;

        MinZ := AccumulationRaster.Z[0,0];
        MaxZ := MinZ;
        for RowIndex := 0 to Raster.YCount - 1 do
        begin
          for ColIndex := 0 to Raster.XCount - 1 do
          begin
            if not Raster.Ignore[ColIndex,RowIndex] then
            begin
              Z := AccumulationRaster.Z[ColIndex,RowIndex];
              Raster.Z[ColIndex,RowIndex] := Z;
              if Z < MinZ then
              begin
                MinZ := Z;
              end
              else if Z > MaxZ then
              begin
                MaxZ := Z;
              end;
            end;
          end;
        end;
        Header := Raster.Header;
        Header.zMin := MinZ;
        Header.zMax := MaxZ;
        Raster.Header := Header;
        Raster.SaveToFile(fedOutput.FileName);

        Memo2.Lines.BeginUpdate;
        try
          Memo2.Lines.Clear;
          for AStream in Streams do
          begin
            for APoint in AStream do
            begin
              Memo2.Lines.Add(Format('%d %d', [APoint.x, APoint.y]));
            end;
            Memo2.Lines.Add('');
          end;
        finally
          Memo2.Lines.EndUpdate;
        end;

        Streams.Free;

      finally
        PitRemover.Free;
      end;
    finally
      EdgeStartPoints.Free;
      Pits.Free;
    end;
  finally
    RastInterface := nil;
    Screen.Cursor := crDefault;
  end;

end;

procedure TForm6.btnSimplePitRemovalClick(Sender: TObject);
var
  ARaster: TTestRaster;
  Index: Integer;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  ARaster := TTestRaster.Create(17, 1);
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for Index := 0 to ARaster.XCount - 1 do
    begin
      ARaster.Ignore[Index,0] := False;
    end;
    ARaster.Z[0,0] := 0;
    ARaster.Z[1,0] := 1;
    ARaster.Z[2,0] := 1;
    ARaster.Z[3,0] := 2;
    ARaster.Z[4,0] := 6;
    ARaster.Z[5,0] := 6;
    ARaster.Z[6,0] := 6;
    ARaster.Z[7,0] := 3;
    ARaster.Z[8,0] := 1;
    ARaster.Z[9,0] := 2;
    ARaster.Z[10,0] := 3;
    ARaster.Z[11,0] := 3;
    ARaster.Z[12,0] := 4;
    ARaster.Z[13,0] := 5;
    ARaster.Z[14,0] := 7;
    ARaster.Z[15,0] := 8;
    ARaster.Z[16,0] := 8;

    APoint.X := 0;
    APoint.Y := 0;
    EdgeStartPoints.Add(APoint);
    APoint.X := 8;
    Pits.Add(APoint);

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(ARaster, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
      FlowDirections.Free;
    finally
      PitRemover.Free;
    end;

    memo2.Lines.Clear;

    for Index := 0 to NewRaster.XCount - 1 do
    begin
      memo2.Lines.Add(FloatToStr(NewRaster.Z[Index,0]))
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTestCompoundPitRemovalClick(Sender: TObject);
var
  ARaster: TTestRaster;
  Index: Integer;
  EdgeStartPoints: TPointList;
  Pits: TPointList;
  APoint: TPoint;
  NewRaster: IRaster;
  FlowDirections: TFlowDirections;
  PitRemover: TPitRemover;
  Changed: Boolean;
  CutCriteria: TCutCriteria;
begin
  CutCriteria.LimitLength := False;
  CutCriteria.LimitCutDepth := False;
  ARaster := TTestRaster.Create(17, 1);
  EdgeStartPoints := TPointList.Create;
  Pits := TPointList.Create;
  try
    for Index := 0 to ARaster.XCount - 1 do
    begin
      ARaster.Ignore[Index,0] := False;
    end;
    ARaster.Z[0,0] := 0;
    ARaster.Z[1,0] := 8;
    ARaster.Z[2,0] := 8;
    ARaster.Z[3,0] := 8;
    ARaster.Z[4,0] := 8;
    ARaster.Z[5,0] := 8;
    ARaster.Z[6,0] := 2;
    ARaster.Z[7,0] := 3;
    ARaster.Z[8,0] := 1;
    ARaster.Z[9,0] := 2;
    ARaster.Z[10,0] := 7;
    ARaster.Z[11,0] := 7;
    ARaster.Z[12,0] := 7;
    ARaster.Z[13,0] := 7;
    ARaster.Z[14,0] := 7;
    ARaster.Z[15,0] := 9;
    ARaster.Z[16,0] := 9;

    APoint.X := 0;
    APoint.Y := 0;
    EdgeStartPoints.Add(APoint);
    APoint.X := 6;
    Pits.Add(APoint);
    APoint.X := 8;
    Pits.Add(APoint);

    PitRemover := TPitRemover.Create;
    try
      Changed := False;
      PitRemover.RemovePits(ARaster, EdgeStartPoints, Pits, NewRaster, FlowDirections, CutCriteria, Changed);
    finally
      FlowDirections.Free;
      PitRemover.Free;
    end;

    memo2.Lines.Clear;

    for Index := 0 to NewRaster.XCount - 1 do
    begin
      memo2.Lines.Add(FloatToStr(NewRaster.Z[Index,0]))
    end;

  finally
//    ARaster := nil;
//    ARaster.Free;
    EdgeStartPoints.Free;
    Pits.Free;
  end;
end;

procedure TForm6.btnTestFileQueue2Click(Sender: TObject);
var
  AFileQueue: TFileQueue<Integer>;
  AValue: Integer;
  FileStream: TFileStream;
  index: Integer;
begin
  if sdFileQueueFile.Execute then
  begin
    FileStream := TFile.Create(sdFileQueueFile.FileName);
    FileStream.Free;
    AFileQueue := TFileQueue<Integer>.Create(sdFileQueueFile.FileName, 5);
    try
      for index := 0 to 100 do
      begin
        AFileQueue.EnQueue(index);
      end;
      Assert(AFileQueue.Count = 101);

      for index := 0 to 100 do
      begin
        AValue := AFileQueue.DeQueue;
        Assert(AValue = index)
      end;
      Assert(AFileQueue.Count = 0);

    finally
      AFileQueue.Free;
      DeleteFile(sdFileQueueFile.FileName);
    end;
  end;
end;

procedure TForm6.btnTestFileQueueClick(Sender: TObject);
var
  AFileQueue: TFileQueue<Integer>;
  AValue: Integer;
  FileStream: TFileStream;
begin
  if sdFileQueueFile.Execute then
  begin
    FileStream := TFile.Create(sdFileQueueFile.FileName);
    FileStream.Free;
    AFileQueue := TFileQueue<Integer>.Create(sdFileQueueFile.FileName, 5);
    try
      Assert(AFileQueue.Count = 0);

      AFileQueue.EnQueue(1);
      Assert(AFileQueue.Count = 1);

      AFileQueue.EnQueue(2);
      Assert(AFileQueue.Count = 2);

      AFileQueue.EnQueue(3);
      Assert(AFileQueue.Count = 3);

      AFileQueue.EnQueue(4);
      Assert(AFileQueue.Count = 4);

      AFileQueue.EnQueue(5);
      Assert(AFileQueue.Count = 5);

      AFileQueue.EnQueue(6);
      Assert(AFileQueue.Count = 6);

      AFileQueue.EnQueue(7);
      Assert(AFileQueue.Count = 7);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 1);
      Assert(AFileQueue.Count = 6);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 2);
      Assert(AFileQueue.Count = 5);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 3);
      Assert(AFileQueue.Count = 4);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 4);
      Assert(AFileQueue.Count = 3);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 5);
      Assert(AFileQueue.Count = 2);


      AFileQueue.EnQueue(8);
      Assert(AFileQueue.Count = 3);



      AValue := AFileQueue.DeQueue;
      Assert(AValue = 6);
      Assert(AFileQueue.Count = 2);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 7);
      Assert(AFileQueue.Count = 1);

      AValue := AFileQueue.DeQueue;
      Assert(AValue = 8);
      Assert(AFileQueue.Count = 0);
    finally
      AFileQueue.Free;
      DeleteFile(sdFileQueueFile.FileName);
    end;
  end;
end;

{ TIntegerPriorityQueue }

constructor TIntegerPriorityQueue.Create;
begin
  inherited Create(TComparer<Integer>.Construct(
    function(const Left, Right: Integer): Integer
    begin
      result := Right - Left;
    end));
end;

{ TTestOutline }

procedure TTestOutline.Add(Item: TSubPolygon);
begin
  FPolygons.Add(Item);
end;

end.
