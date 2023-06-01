unit ContourExport;

interface

uses System.UITypes, System.Types, Classes, FastGeo, QuadtreeClass, RealListUnit, AbstractGridUnit,
  DataSetUnit, SysUtils, GoPhastTypes, ContourUnit, ValueArrayStorageUnit,
  PhastModelUnit, Vcl.Dialogs, System.Generics.Collections, XBase1,
  ShapefileUnit;

type
  TPointList = class(TObject)
  private
    FPoints: TPoint2DArray;
    function GetFirst: TPoint2D;
    function GetLast: TPoint2D;
  strict private
    FCount: integer;
    procedure Grow;
    function GetCapacity: integer;
    function GetPoint(Index: integer): TPoint2D;
    procedure SetCapacity(const Value: integer);
    procedure SetPoint(Index: integer; const Value: TPoint2D);
  public
    function Add(Point: TPoint2D): integer;
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read FCount;
    property Points[Index: integer]: TPoint2D read GetPoint write SetPoint; default;
    procedure Insert(Position: integer; Point: TPoint2D);
    property First: TPoint2D read GetFirst;
    property Last: TPoint2D read GetLast;
    procedure Reverse;
  end;

  TListOfPointLists = TObjectList<TPointList>;

  TContourExtractor = class(TCustomContourCreator)
  private
    FModel: TCustomModel;
    FQuadTree: TRbwQuadTree;
    // @name contains instances of @link(TPointList).
    // @name is instantiated as a TObjectList.
    // @name is filled in @link(ImportSegments).
    FPointLists: TListOfPointLists;
    FEpsilon: Real;
    FModelGrid: TCustomModelGrid;
    FAlgorithm: TContourAlg;
    FShapeDataBase: TXBase;
    FShapeFileWriter: TShapefileGeometryWriter;
    FFileName: string;
    FFieldName: AnsiString;
    FLayerFieldName: AnsiString;
    // @name is the event handler for TContourCreator.OnExtractSegments
    procedure ImportSegments(Sender: TObject; const Segments: TLine2DArray);
    procedure InitializeQuadTree;
    procedure InitializeEpsilon;
    procedure MergePointLists;
  public
    // If contours for multiple layers will be created, specify the file name,
    // the first time @name is called. For subsequent calls, the file name
    // should be an empty string.
    procedure CreateShapes(ValueList: TValueArrayStorage; DataArray: TDataArray;
      FileName: string; LabelSpacing: integer; LayerNumber: Integer = -1);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
  end;

// PointLists is filled with @link(TPointList)s.
procedure GlobalImportSegments(Sender: TObject;
  const Segments: TLine2DArray; Epsilon: Real;
  QuadTree: TRbwQuadTree; PointLists: TObjectList<TPointList>);

procedure GlobalInitializeQuadTree(var QuadTree: TRbwQuadTree;
  ModelGrid: TCustomModelGrid);

procedure GlobalInitializeEpsilon(var Epsilon: Real; ModelGrid: TCustomModelGrid);

implementation

uses
  frmGoPhastUnit, Math,
  frmExportShapefileUnit, RbwParser, ModelMuseUtilities,
  LineStorage, SutraMeshUnit, DataSetNamesUnit;

procedure GlobalImportSegments(Sender: TObject;
  const Segments: TLine2DArray; Epsilon: Real;
  QuadTree: TRbwQuadTree; PointLists: TObjectList<TPointList>);
var
  Index: Integer;
  APoint: TPoint2D;
  PointIndex: Integer;
  Exaggeration: double;
  LineIndex: Integer;
  ALine: TPointList;
  NewLine: TPointList;
begin
  for Index := 0 to Length(Segments) - 1 do
  begin
    NewLine := TPointList.Create;
    NewLine.Add(Segments[Index][1]);
    NewLine.Add(Segments[Index][2]);
    PointLists.Add(NewLine);
  end;
  Exaggeration := 1;
  if (frmGoPhast.ModelSelection in [msSutra22, msSutra30, msSutra40])
    and (frmGoPhast.SutraMesh.MeshType = mtProfile) then
  begin
    Exaggeration := frmGoPhast.PhastModel.Exaggeration;
    if Exaggeration = 0 then
    begin
      Exaggeration := 1;
    end;
  end;
  if Exaggeration <> 1 then
  begin
    for LineIndex := 0 to PointLists.Count - 1 do
    begin
      ALine := PointLists[LineIndex];
      for PointIndex := 0 to ALine.Count - 1 do
      begin
        APoint := ALine.Points[PointIndex];
        APoint.y := APoint.y/Exaggeration;
        ALine.Points[PointIndex] := APoint;
      end;
    end;
  end;
end;

procedure GlobalInitializeQuadTree(var QuadTree: TRbwQuadTree;
  ModelGrid: TCustomModelGrid);
var
  CornerPoint: TPoint2d;
  XMin, XMax, YMin, YMax: double;
begin
  if QuadTree = nil then
  begin
    QuadTree := TRbwQuadTree.Create(nil);

    CornerPoint := ModelGrid.TwoDElementCorner(0,0);
    XMin := CornerPoint.x;
    XMax := XMin;
    YMin := CornerPoint.y;
    YMax := YMin;

    CornerPoint := ModelGrid.TwoDElementCorner(ModelGrid.ColumnCount,0);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := ModelGrid.TwoDElementCorner(ModelGrid.ColumnCount,ModelGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    CornerPoint := ModelGrid.TwoDElementCorner(0,ModelGrid.RowCount);
    XMin := Min(XMin, CornerPoint.x);
    XMax := Max(XMax, CornerPoint.x);
    YMin := Min(YMin, CornerPoint.y);
    YMax := Max(YMax, CornerPoint.y);

    QuadTree.XMin := XMin;
    QuadTree.XMax := XMax;
    QuadTree.YMin := YMin;
    QuadTree.YMax := YMax;
  end
  else
  begin
    QuadTree.Clear;
  end;
end;


{ TPointList }

function TPointList.Add(Point: TPoint2D): integer;
begin
  if FCount = Capacity then
  begin
    Grow;
  end;
  FPoints[FCount] := Point;
  result := FCount;
  Inc(FCount);
end;

function TPointList.GetCapacity: integer;
begin
  result := Length(FPoints);
end;

function TPointList.GetFirst: TPoint2D;
begin
  result := FPoints[0];
end;

function TPointList.GetLast: TPoint2D;
begin
  result := FPoints[Count-1];
end;

function TPointList.GetPoint(Index: integer): TPoint2D;
begin
  result := FPoints[Index];
end;

procedure TPointList.Grow;
var
  Delta: Integer;
  LocalCapacity: integer;
begin
  LocalCapacity := Capacity;
  if LocalCapacity < 16 then
  begin
    Delta := 4;
  end
  else
  begin
    Delta := LocalCapacity  div 4;
  end;
  Capacity := LocalCapacity + Delta;
end;

procedure TPointList.Insert(Position: integer; Point: TPoint2D);
var
  Index: Integer;
begin
  if Count = Capacity then
  begin
    Grow;
  end;
  if Position = Count then
  begin
    Add(Point);
  end
  else
  begin
    for Index := FCount - 1 downto Position do
    begin
      FPoints[Index+1] := FPoints[Index];
    end;
    FPoints[Position] := Point;
    Inc(FCount);
  end;
end;

procedure TPointList.Reverse;
var
  NewPoints: TPoint2DArray;
  Index: Integer;
begin
  SetLength(NewPoints, Count);
  for Index := 0 to Count - 1 do
  begin
    NewPoints[Index] := FPoints[Count-1-Index];
  end;
  FPoints := NewPoints;
end;

procedure TPointList.SetCapacity(const Value: integer);
begin
  SetLength(FPoints, Value);
end;

procedure TPointList.SetPoint(Index: integer; const Value: TPoint2D);
begin
  FPoints[Index] := Value;
end;

{ TContourExtractor }

constructor TContourExtractor.Create(Model: TBaseModel);
begin
  FFileName := '';
  FModel := (Model as TCustomModel);
  FModelGrid := FModel.Grid;
  Mesh := FModel.DrawMesh;
  FShapeDataBase := TXBase.Create(nil);
  FShapeFileWriter := TShapefileGeometryWriter.Create(stPolyLine, True);
end;

procedure TContourExtractor.MergePointLists;
var
  index: Integer;
  PointList: TPointList;
  APoint: TPoint2D;
  Data: TQuadPointArray;
  Merged: Boolean;
  OtherPointList: TPointList;
  APoint1: TPoint2D;
  APoint2: TPoint2D;
  PointerArray: TPointerArray;
  Closed: Boolean;
  CloseDist: double;
  CandidateLines: TList;
  MergeCount: Integer;
  procedure MergeLines(PointsToMerge: Integer);
  var
    DataIndex: Integer;
    PointIndex: Integer;
    ArrayIndex: Integer;
    MinIndex: Integer;
    CandiateList: TPointList;
    MinDistance: double;
    CandidateIndex: Integer;
    TestDistance: Double;
    PIndex: Integer;
  begin
    CandidateLines.Clear;
    APoint := PointList.Last;
    FQuadTree.FindNearestPoints(APoint.x, APoint.y, PointsToMerge, Data);
    OtherPointList := nil;
    for ArrayIndex := 0 to Length(Data) - 1 do
    begin
      PointerArray := Data[ArrayIndex].Data;
      for DataIndex := 0 to Length(PointerArray) - 1 do
      begin
        OtherPointList := PointerArray[DataIndex];
        if OtherPointList = PointList then
        begin
          OtherPointList := nil;
        end;
        if OtherPointList <> nil then
        begin
          CandidateLines.Add(OtherPointList);
        end;
      end;
    end;
    if CandidateLines.Count = 0 then
    begin
      Exit;
    end
    else if CandidateLines.Count = 1 then
    begin
      OtherPointList := CandidateLines[0];
    end
    else
    begin
      MinIndex := 0;
      OtherPointList := CandidateLines[0];
      MinDistance := Min( Distance(OtherPointList.First, APoint),
        Distance(OtherPointList.Last, APoint));
      for CandidateIndex := 1 to CandidateLines.Count - 1 do
      begin
        CandiateList := CandidateLines[CandidateIndex];
        TestDistance := Min( Distance(CandiateList.First, APoint),
          Distance(CandiateList.Last, APoint));
        if MinDistance > TestDistance then
        begin
          OtherPointList := CandiateList;
          MinDistance := TestDistance;
        end;
      end;
    end;

    if OtherPointList <> nil then
    begin
      if Distance(APoint, OtherPointList.First) > Distance(APoint, OtherPointList.Last) then
      begin
        OtherPointList.Reverse;
      end;
      if Distance(APoint, OtherPointList.First) < FEpsilon then
      begin
        APoint2 := PointList.Last;
        FQuadTree.RemovePoint(APoint2.x, APoint2.y, PointList);
        for PointIndex := 1 to OtherPointList.Count - 1 do
        begin
          PointList.Add(OtherPointList[PointIndex]);
        end;
        APoint1 := OtherPointList.First;
        APoint2 := OtherPointList.Last;
        FQuadTree.RemovePoint(APoint1.x, APoint1.y, OtherPointList);
        FQuadTree.RemovePoint(APoint2.x, APoint2.y, OtherPointList);
        APoint2 := PointList.Last;
        FQuadTree.AddPoint(APoint2.x, APoint2.y, PointList);
        PIndex := FPointLists.IndexOf(OtherPointList);
        Assert(PIndex >= 0);
        FPointLists[PIndex] := nil;
        Merged := True;
      end;
    end
  end;
begin
  CandidateLines := TList.Create;
  try
  for MergeCount := 1 to 2 do
  begin
    Merged := True;
    while Merged do
    begin
      Merged := False;
      InitializeQuadTree;
      for index := 0 to FPointLists.Count - 1 do
      begin
        PointList := FPointLists[index];
        APoint1 := PointList.First;
        FQuadTree.AddPoint(APoint1.x, APoint1.y, PointList);
        APoint2 := PointList.Last;
        FQuadTree.AddPoint(APoint2.x, APoint2.y, PointList);
      end;

      for index := FPointLists.Count - 1 downto 0 do
      begin
        PointList := FPointLists[index];
        if PointList <> nil then
        begin
          MergeLines(MergeCount);
        end;
      end;
      FPointLists.Pack;
      for index := FPointLists.Count - 1 downto 0 do
      begin
        PointList := FPointLists[index];
        if PointList <> nil then
        begin
          PointList.Reverse;
          MergeLines(MergeCount);
        end;
      end;
      FPointLists.Pack;
    end;
  end;
  finally
    CandidateLines.Free;
  end;
  for index := 0 to FPointLists.Count - 1 do
  begin
    PointList := FPointLists[index];
    APoint1 := PointList.First;
    APoint2 := PointList.Last;
    CloseDist := Distance(PointList.First, PointList.Last);
    if CloseDist < FEpsilon then
    begin
      Closed := CloseDist = 0;
      if not Closed then
      begin
        PointList.Add(PointList.First);
      end;
    end;
  end;
end;

procedure TContourExtractor.CreateShapes(ValueList: TValueArrayStorage;
  DataArray: TDataArray; FileName: string; LabelSpacing: integer; LayerNumber: Integer = -1);
var
  ContourCreator: TContourCreator;
  ValueIndex: Integer;
  PointListIndex: Integer;
  PointList: TPointList;
  PointIndex: Integer;
  PointsForShape: TList;
  Shape: TShapeObject;
  PPoint: TPoint2DPtr;
  Fields: TStringList;
//  FieldName: AnsiString;
  FieldDescription: AnsiString;
  MinValue, MaxValue: double;
  DSValues: TStringList;
  FieldFormat: AnsiString;
  Contours: TContours;
  C: TRealArray;
  APlot: TLineList;
  ContVals: TRealList;
  AContourLine: TLine;
  Location: TLocation;
  FieldNames: TStringList;
  MinPositive: Double;
  RowIndex: Integer;
  MinRowWidth: double;
  TestValue: double;
  MinColWidth: double;
  ColIndex: Integer;
//  LayerFieldName: AnsiString;
  procedure MakeShapesFromContourLines;
  var
    ContourIndex: integer;
    PointIndex: integer;
    Exaggeration: Double;
  begin
    if PlotList.Count = 0 then
    begin
      Exit;
    end;
    Exaggeration := 1;
    if frmGoPhast.ModelSelection in [msSutra22, msSutra30, msSutra40] then
    begin
      if (Mesh as TSutraMesh3D).MeshType = mtProfile then
      begin
        Exaggeration := frmGoPhast.PhastModel.Exaggeration;
        if Exaggeration = 0 then
        begin
          Exaggeration := 1;
        end;
      end;
    end;
    Assert(PlotList.Count = 1);
    APlot := PlotList[0];
    APlot.MergeLines;
    FShapeFileWriter.Capacity := FShapeFileWriter.Count + APlot.Count;
    for ContourIndex := 0 to APlot.Count - 1 do
    begin
      AContourLine := APlot[ContourIndex];
      if AContourLine.Count = 0 then
      begin
        Continue;
      end;
      ValueIndex := ContVals.IndexOfClosest(
        AContourLine.ContourLevel);

      Shape := TShapeObject.Create;
      try
        Shape.FNumPoints := AContourLine.Count;
        Shape.FShapeType := stPolyLine;
        SetLength(Shape.FPoints, AContourLine.Count);
        Shape.FNumParts := 1;
        SetLength(Shape.FParts, 1);
        Shape.FParts[0] := 0;
        SetLength(Shape.FPartTypes, 0);
        for PointIndex := 0 to AContourLine.Count - 1 do
        begin
          Location := AContourLine[PointIndex];
          Shape.FPoints[PointIndex].X := Location.x;
          Shape.FPoints[PointIndex].Y := Location.y/Exaggeration;
        end;
        FShapeFileWriter.AddShape(Shape);

        FShapeDataBase.AppendBlank;
        case DataArray.DataType of
          rdtDouble:
            FShapeDataBase.UpdFieldNum(FFieldName,
              ValueList.RealValues[ValueIndex]);
          rdtInteger:
            FShapeDataBase.UpdFieldInt(FFieldName,
              ValueList.IntValues[ValueIndex]);
          rdtBoolean:
            if ValueList.BooleanValues[ValueIndex] then
            begin
              FShapeDataBase.UpdFieldInt(FFieldName, 1);
            end
            else
            begin
              FShapeDataBase.UpdFieldInt(FFieldName, 0);
            end;
          rdtString:
            FShapeDataBase.UpdFieldStr(FFieldName,
              AnsiString(ValueList.StringValues[ValueIndex]));
          else
            Assert(False);
        end;
        if LayerNumber > 0 then
        begin
          FShapeDataBase.UpdFieldInt(FLayerFieldName, LayerNumber);
        end;

        FShapeDataBase.PostChanges;
      except
        Shape.Free;
        raise;
      end
    end
  end;
begin
  DataSet := DataArray;
  FAlgorithm := DataSet.ContourAlg;

  if Assigned(FModelGrid) then
  begin
    Grid := FModelGrid.ContourGrid(DataArray.EvaluatedAt,
      frmGoPhast.PhastModel.ModelSelection, vdTop, FModelGrid.SelectedLayer);
  end
  else
  begin
    Assert(Assigned(Mesh));
  end;

  if Assigned(Grid) then
  begin
    ActiveDataSet := FModel.DataArrayManager.
      GetDataSetByName(rsActive);
    Assert(Assigned(ActiveDataSet));
  end
  else
  begin
    Assert(Assigned(Mesh));
  end;

  ViewDirection := vdTop;
//  Assert(Grid <> nil);
  Assert(ValueList.Count > 0);

  PlotList := TPlotList.Create;
  Contours :=  DataSet.Contours;
  DSValues := TStringList.Create;
  try
    case FAlgorithm of
      caSimple:
        begin
          if Assigned(Grid) then
          begin
            AssignGridValues(MinValue, MaxValue, MinPositive, FModel.SelectedLayer,
              DSValues, vdTop);
          end
          else
          begin
            AssignTriangulationValuesFromMesh(MinValue, MaxValue, MinPositive,
              FModel.SelectedLayer, DSValues, vdTop);
          end;
        end;
      caACM626:
        begin
          if Assigned(Grid) then
          begin
            AssignTriangulationValuesFromGrid(MinValue, MaxValue, MinPositive,
              FModel.SelectedLayer, DSValues, vdTop);
          end
          else
          begin
            AssignTriangulationValuesFromMesh(MinValue, MaxValue, MinPositive,
              FModel.SelectedLayer, DSValues, vdTop);
          end;
        end;
      else Assert(False);
    end;

    if FileName <> '' then
    begin
      FFileName := FileName;
      Fields := TStringList.Create;
      FieldNames := TStringList.Create;
      try
        FFieldName := AnsiString(UpperCase(Copy(DataArray.Name, 1, 10)));
        FFieldName := FixShapeFileFieldName(FFieldName, FieldNames);
        FieldNames.Add(string(FFieldName));
        case DataArray.DataType of
          rdtDouble:
            FieldFormat := 'N18,10';
          rdtInteger:
            FieldFormat := 'N';
          rdtBoolean:
            FieldFormat := 'N';
          rdtString:
            FieldFormat := 'C18';
          else
            Assert(False);
        end;
        FieldDescription := FFieldName + '=' + FieldFormat;
        Fields.Add(string(FieldDescription));
        if LayerNumber > 0 then
        begin
          FLayerFieldName := 'LAYER';
          FieldFormat := 'N';
          FieldDescription := FLayerFieldName + '=' + FieldFormat;
          Fields.Add(string(FieldDescription));
        end;
        try
          InitializeDataBase(FileName, FShapeDataBase, Fields);
        except
          on E: EFOpenError do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
          on E: EXBaseException do
          begin
            MessageDlg(E.Message, mtError, [mbOK], 0);
            Exit;
          end;
      end;
      finally
        Fields.Free;
        FieldNames.Free;
      end;
    end;

    case FAlgorithm of
      caSimple:
        begin
          if Assigned(Grid) then
          begin
            FPointLists:= TListOfPointLists.Create;
            ContourCreator:= TContourCreator.Create(LabelSpacing);
            try
              InitializeEpsilon;
              ContourCreator.EvaluatedAt := DataArray.EvaluatedAt;
              ContourCreator.Grid := Grid;

              MinRowWidth := FModel.Grid.RowWidth[0];
//                  MinRowWidth := Grid.
              for RowIndex := 1 to FModel.Grid.RowCount - 1 do
              begin
                TestValue := FModel.Grid.RowWidth[RowIndex];
                if TestValue < MinRowWidth then
                begin
                  MinRowWidth := TestValue;
                end;
              end;
              MinColWidth := FModel.Grid.ColumnWidth[0];
//                  MinColWidth := Grid.
              for ColIndex := 1 to FModel.Grid.ColumnCount - 1 do
              begin
                TestValue := FModel.Grid.ColumnWidth[ColIndex];
                if TestValue < MinColWidth then
                begin
                  MinColWidth := TestValue;
                end;
              end;
              FEpsilon := Sqrt(Sqr(MinRowWidth) + Sqr(MinColWidth))/4;

              ContourCreator.OnExtractSegments := ImportSegments;

              Assert(ValueList.Count = Length(Contours.ContourValues));
              for ValueIndex := 0 to ValueList.Count - 1 do
              begin
                ContourCreator.Value := Contours.ContourValues[ValueIndex];

                InitializeQuadTree;
                FPointLists.Clear;
                ContourCreator.ExtractContour;

                if (FPointLists.Count> 0) then
                begin
                  try
                    MergePointLists;
                  except on E: Exception do
                  begin
                    ShowMessage(ValueIndex.ToString);
                    raise;
                  end;
                  end;
                  FShapeFileWriter.Capacity := FShapeFileWriter.Capacity
                    + FPointLists.Count;
                  for PointListIndex := 0 to FPointLists.Count - 1 do
                  begin
                    PointsForShape := TList.Create;
                    try
                      PointList := FPointLists[PointListIndex];
                      for PointIndex := 0 to PointList.Count - 1 do
                      begin
                        if (PointIndex > 0) and (PointIndex < PointList.Count - 1) then
                        begin
                          if not Collinear(PointList.Points[PointIndex-1],
                            PointList.Points[PointIndex],
                            PointList.Points[PointIndex+1]) then
                          begin
                            PointsForShape.Add(@PointList.FPoints[PointIndex])
                          end;
                        end
                        else
                        begin
                          try
                          PointsForShape.Add(@PointList.FPoints[PointIndex])
                          except
                            ShowMessage(ValueIndex.ToString + ' ' + PointListIndex.ToString + ' ' + PointIndex.ToString);
                            raise;
                          end
                        end;
                      end;
                      if PointsForShape.Count > 0 then
                      begin
                        Shape := TShapeObject.Create;
                        try
                          Shape.FNumPoints := PointsForShape.Count;
                          Shape.FShapeType := stPolyLine;
                          SetLength(Shape.FPoints, PointsForShape.Count);
                          Shape.FNumParts := 1;
                          SetLength(Shape.FParts, 1);
                          Shape.FParts[0] := 0;
                          SetLength(Shape.FPartTypes, 0);
                          for PointIndex := 0 to PointsForShape.Count - 1 do
                          begin
                            PPoint := PointsForShape[PointIndex];
                            Shape.FPoints[PointIndex].X := PPoint.x;
                            Shape.FPoints[PointIndex].Y := PPoint.y;
                          end;
                          FShapeFileWriter.AddShape(Shape);

                          FShapeDataBase.AppendBlank;
                          case DataArray.DataType of
                            rdtDouble:
                              FShapeDataBase.UpdFieldNum(FFieldName, ValueList.RealValues[ValueIndex]);
                            rdtInteger:
                              FShapeDataBase.UpdFieldInt(FFieldName, ValueList.IntValues[ValueIndex]);
                            rdtBoolean:
                              if ValueList.BooleanValues[ValueIndex] then
                              begin
                                FShapeDataBase.UpdFieldInt(FFieldName, 1);
                              end
                              else
                              begin
                                FShapeDataBase.UpdFieldInt(FFieldName, 0);
                              end;
                            rdtString:
                              FShapeDataBase.UpdFieldStr(FFieldName, AnsiString(ValueList.StringValues[ValueIndex]));
                            else
                              Assert(False);
                          end;
                          if LayerNumber > 0 then
                          begin
                            FShapeDataBase.UpdFieldInt(FLayerFieldName, LayerNumber);
                          end;

                          FShapeDataBase.PostChanges;
                        except
                          Shape.Free;
                          raise;
                        end;
                      end;
                    finally
                      PointsForShape.Free;
                    end;
                  end;
                end;
              end;
//                ShapeFileWriter.WriteToFile(FileName, ChangeFileExt(FileName, '.shx'));
            finally
              ContourCreator.Free;
              FPointLists.Free;
            end;
          end
          else
          begin
            ContVals := TRealList.Create;
            try
              ContVals.Capacity := ValueList.Count;
              for ValueIndex := 0 to ValueList.Count - 1 do
              begin
                ContVals.Add(Contours.ContourValues[ValueIndex]);
              end;
              CreateSimpleContoursFromMesh(Contours.ContourValues);
              MakeShapesFromContourLines;
            finally
              ContVals.Free;
            end;
          end;
        end;
      caACM626:
        begin
          ContVals := TRealList.Create;
          try
            ContVals.Capacity := ValueList.Count;
            SetLength(C, ValueList.Count);
            for ValueIndex := 0 to ValueList.Count - 1 do
            begin
              C[ValueIndex] := Contours.ContourValues[ValueIndex];
              ContVals.Add(Contours.ContourValues[ValueIndex]);
            end;
            PerformAlg626(C);
            MakeShapesFromContourLines;
          finally
            ContVals.Free;
          end;

        end
      else Assert(False);
    end;
//  finally
//  end;
//    end;
  finally
    DSValues.Free;
    FreeAndNil(PlotList);
  end;
end;

destructor TContourExtractor.Destroy;
begin
  FShapeDataBase.Active := False;
  FShapeDataBase.Free;

  if FFileName <> '' then
  begin
    FShapeFileWriter.WriteToFile(FFileName, ChangeFileExt(FFileName, '.shx'));
  end;
  FShapeFileWriter.Free;

  FQuadTree.Free;
  inherited;
end;

procedure TContourExtractor.ImportSegments(Sender: TObject;
  const Segments: TLine2DArray);
begin
  GlobalImportSegments(Sender, Segments, FEpsilon, FQuadTree, FPointLists);
end;

procedure TContourExtractor.InitializeEpsilon;
begin
  GlobalInitializeEpsilon(FEpsilon, FModelGrid);
end;

procedure TContourExtractor.InitializeQuadTree;
begin
  GlobalInitializeQuadTree(FQuadTree, FModelGrid);
end;

procedure GlobalInitializeEpsilon(var Epsilon: Real; ModelGrid: TCustomModelGrid);
var
  Index: Integer;
begin
  Epsilon := ModelGrid.ColumnWidth[0];
  for Index := 1 to ModelGrid.ColumnCount - 1 do
  begin
    Epsilon := Min(Epsilon, ModelGrid.ColumnWidth[Index]);
  end;
  for Index := 0 to ModelGrid.RowCount - 1 do
  begin
    Epsilon := Min(Epsilon, ModelGrid.RowWidth[Index]);
  end;
  Epsilon := Epsilon/4;
  if Epsilon > 1 then
  begin
    Epsilon := Sqrt(Epsilon);
  end
  else
  begin
    Epsilon := Sqr(Epsilon);
  end;
end;


end.
