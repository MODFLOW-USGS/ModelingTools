unit frmImportSurferGridFilesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons, GoPhastTypes, SurferGridFileReaderUnit,
  frmImportShapefileUnit, frmCustomImportSimpleFileUnit, PhastModelUnit,
  FastGEO, AbstractGridUnit, MeshRenumberingTypes, ScreenObjectUnit,
  ValueArrayStorageUnit;

type
  TUndoImportGrdFiles = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;


  TfrmImportSurferGridFiles = class(TfrmCustomGoPhast)
    lblInterpolator: TLabel;
    OpenDialogFile: TOpenDialog;
    cbIntersectedCells: TCheckBox;
    cbInterpolation: TCheckBox;
    rgEvaluatedAt: TRadioGroup;
    rgFilterMethod: TRadioGroup;
    comboInterpolators: TComboBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure cbIntersectedCellsClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
    procedure FormCreate(Sender: TObject); override;
  private
  FGrid: TCustomModelGrid;
    FGridTypes: TSurferFileTypes;
    FNodeElemString: string;
    LocalModel: TCustomModel;
    Values: array of array of double;
    Counts: array of array of integer;
    CenterPoints: array of array of TPoint2D;
    Distances: array of array of double;
    MinX: Real;
    MaxX: Real;
    MinY: Real;
    MaxY: Real;
    ImportMethod: TImportMethod;
    EvalAt: TEvaluatedAt;
    FMesh: IMesh3D;
    { Private declarations }
    // @name updates the contents of rgEvaluatedAt to the appropriate
    // values depending on what model (PHAST, MODFLOW, or SUTRA) is selected.
    procedure UpdateEvalAt;
    procedure SetCheckBoxCaptions;
    procedure GetInterpolators;
    procedure SetData;
    function MakeNewDataSet(NewDataSets: TList; Suffix, Classification: string;
      NewDataSetNeeded: Boolean; FileName: string): string;
    function InitializeArrays(ImportMethod: TImportMethod): Boolean;
    procedure GetDiscretizationMinMax;
    procedure HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
    procedure HandleAPoint(APoint3D: TPoint3D; ImportMethod: TImportMethod;
      EvalAt: TEvaluatedAt; Grid: TCustomModelGrid; Mesh: IMesh3D);
    procedure ComputeAverage(ImportMethod: TImportMethod;
      ImportProgress: TImportProgress = nil);
    procedure AssignPointsAndValues(Grid: TCustomModelGrid;
      AScreenObject: TScreenObject; Item: TValueArrayItem;
      ComparePoints: Boolean = False);
  public
    function GetData: boolean;
    property NodeElemString: string read FNodeElemString;
    { Public declarations }
  end;

var
  frmImportSurferGridFiles: TfrmImportSurferGridFiles;

implementation

uses
  frmGoPhastUnit, frmDataSetsUnits, DataSetUnit,
  RbwParser,
  ModelMuseUtilities, UndoItems, GIS_Functions;

{$R *.dfm}

const
  StrGrdZ = '_Grd_Z';

var
  FilterIndex : integer = 0;

resourcestring
  StrImportedFromSurfer = 'Imported from Surfer Grid files';
  CommentStr = 'Minimum X %0g' + sLineBreak
    + 'Maximum X %1g' + sLineBreak
    + 'Minimum Y %2g' + sLineBreak
    + 'Maximum Y %3g' + sLineBreak
    + 'Minimum Z %4g' + sLineBreak
    + 'Maximum Z %5g' + sLineBreak;
  StrImportSurferGridFs = 'import Surfer grid files';
  StrYouMustHaveAGrid = 'You must have a grid or mesh defined before you can' +
  ' import the data from your file.';

function ConvertPoint(const SurferPoint: TSurferPoint): TPoint2D;
begin
  result.X := SurferPoint.X;
  result.Y := SurferPoint.Y;
end;


procedure TfrmImportSurferGridFiles.AssignPointsAndValues(
  Grid: TCustomModelGrid; AScreenObject: TScreenObject; Item: TValueArrayItem;
  ComparePoints: Boolean);
var
  ValueIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  GridPoint2D: TPoint2D;
  FMissingCount: Integer;
begin
  FMissingCount := 0;
  ValueIndex := 0;
  for RowIndex := 0 to Length(Values) - 1 do
  begin
    for ColIndex := 0 to Length(Values[0]) - 1 do
    begin
      if Counts[RowIndex, ColIndex] > 0 then
      begin
        GridPoint2D := CenterPoints[RowIndex, ColIndex];
        if Grid <> nil then
        begin
          GridPoint2D :=
            Grid.RotateFromGridCoordinatesToRealWorldCoordinates(GridPoint2D);
        end;
        if ComparePoints then
        begin
          if not PointsEqual(AScreenObject.Points[ValueIndex], GridPoint2D) then
          begin
            raise EDifferentPointsError.Create(StrPointsDontMatch);
          end;
        end
        else
        begin
          AScreenObject.AddPoint(GridPoint2D, True);
        end;
        Item.Values.RealValues[ValueIndex] := Values[RowIndex, ColIndex];
        Inc(ValueIndex);
      end
      else
      begin
        Inc(FMissingCount);
      end;
    end;
  end;
  Item.Values.Count := ValueIndex;
  Item.CacheData;
end;

procedure TfrmImportSurferGridFiles.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmImportSurferGridFiles.cbIntersectedCellsClick(Sender: TObject);
begin
  EmphasizeCheckBoxes([cbIntersectedCells, cbInterpolation]);
  btnOK.Enabled := cbIntersectedCells.Checked or
    cbInterpolation.Checked;
end;

procedure TfrmImportSurferGridFiles.ComputeAverage(ImportMethod: TImportMethod;
  ImportProgress: TImportProgress);
var
  ColIndex: Integer;
  RowIndex: Integer;
begin
  if ImportMethod = imAverage then
  begin
    if Length(Values) > 0 then
    begin
      for ColIndex := 0 to Length(Values[0]) - 1 do
      begin
        if Assigned(ImportProgress) then
        begin
          ImportProgress(self, ColIndex / Length(Values[0]));
        end;
        for RowIndex := 0 to Length(Values) - 1 do
        begin
          if Counts[RowIndex, ColIndex] > 1 then
          begin
            Values[RowIndex, ColIndex] := Values[RowIndex, ColIndex]
              / Counts[RowIndex, ColIndex];
          end;
        end;
      end;
    end;
//    for RowIndex := 0 to Length(Values) - 1 do
//    begin
//      if Assigned(ImportProgress) then
//      begin
//        ImportProgress(self, RowIndex / Length(Values));
//      end;
//      for ColIndex := 0 to Length(Values[0]) - 1 do
//      begin
//        if Counts[RowIndex, ColIndex] > 1 then
//        begin
//          Values[RowIndex, ColIndex] := Values[RowIndex, ColIndex]
//            / Counts[RowIndex, ColIndex];
//        end;
//      end;
//    end;
  end;
end;

procedure TfrmImportSurferGridFiles.FormDestroy(Sender: TObject);
begin
  inherited;
  FGridTypes.Free;
end;

procedure TfrmImportSurferGridFiles.FormCreate(Sender: TObject);
begin
  inherited;
  cbIntersectedCellsClick(nil);
  SetCheckBoxCaptions;
  GetInterpolators;
  FGridTypes := TSurferFileTypes.Create;
end;

{ TfrmImportSurferGridFiles }

function TfrmImportSurferGridFiles.GetData: boolean;
var
  FileIndex: Integer;
  FileType: TSurferFileType;
begin
  UpdateEvalAt;
  OpenDialogFile.FilterIndex := FilterIndex;

  comboInterpolators.ItemIndex := 1;

  result := OpenDialogFile.Execute;
  if result then
  begin
    for FileIndex := 0 to OpenDialogFile.Files.Count - 1 do
    begin
      try
        FileType := SurferFileType(OpenDialogFile.Files[FileIndex]);
      except
        on E: EGrdReadError do
        begin
          result := False;
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
        on E: EFOpenError do
        begin
          result := False;
          Beep;
          MessageDlg(E.message, mtError, [mbOK], 0);
          Exit;
        end;
      end;
      FGridTypes.Add(FileType)

    end;
    FilterIndex := OpenDialogFile.FilterIndex;
  end;

end;

procedure TfrmImportSurferGridFiles.GetDiscretizationMinMax;
var
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  MeshLimits: TGridLimit;
  Mesh: IMesh3D;
  procedure EnsureMinMax(var MinValue, MaxValue: Real);
  var
    Temp: Real;
  begin
    if MinValue > MaxValue then
    begin
      Temp := MinValue;
      MinValue := MaxValue;
      MaxValue := Temp;
    end;
  end;
begin
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
  Grid := LocalModel.Grid;
  Mesh := LocalModel.Mesh3D;

  if Grid <> nil then
  begin
    MinX := Grid.ColumnPosition[0];
    MaxX := Grid.ColumnPosition[Grid.ColumnCount];
    EnsureMinMax(MinX, MaxX);
    MinY := Grid.RowPosition[0];
    MaxY := Grid.RowPosition[Grid.RowCount];
    EnsureMinMax(MinY, MaxY);
  end
  else
  begin
    Assert(Mesh <> nil);
    MeshLimits := Mesh.MeshLimits(vdTop, 0);
    MinX := MeshLimits.MinX;
    MaxX := MeshLimits.MaxX;
    MinY := MeshLimits.MinY;
    MaxY := MeshLimits.MaxY;
  end;

end;

procedure TfrmImportSurferGridFiles.GetInterpolators;
var
  List: TList;
  Index: integer;
  AType: TInterpolatorType;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  List := TList.Create;
  try
    AddInterpolatorsToList(List);
    comboInterpolators.Items.Add('none');
    for Index := 0 to List.Count - 1 do
    begin
      AType := List[Index];
      comboInterpolators.Items.AddObject(AType.InterpolatorName,
        TObject(AType));
    end;
  finally
    List.Free;
  end;
end;

procedure TfrmImportSurferGridFiles.HandleAPoint(APoint3D: TPoint3D;
  ImportMethod: TImportMethod; EvalAt: TEvaluatedAt; Grid: TCustomModelGrid;
  Mesh: IMesh3D);
var
  ADistance: TFloat;
  ARow: Integer;
  ACol: Integer;
  APoint2D: TPoint2D;
  MeshLocation: T2DTopCell;
begin
  if (APoint3D.x >= MinX)
    and (APoint3D.x <= MaxX)
    and (APoint3D.y >= MinY)
    and (APoint3D.y <= MaxY) then
  begin
    ACol := -1;
    ARow := -1;
    if Grid <> nil then
    begin
      case EvalAt of
        eaBlocks:
          begin
            ACol := Grid.GetContainingColumn(APoint3D.x);
            ARow := Grid.GetContainingRow(APoint3D.y);
          end;
        eaNodes:
          begin
            ACol := Grid.NearestColumnPosition(APoint3D.x);
            ARow := Grid.NearestRowPosition(APoint3D.y);
          end;
      else
        Assert(False);
      end;
    end
    else
    begin
      Assert(Mesh <> nil);
      ARow := 0;

      APoint2D.x := APoint3D.x;
      APoint2D.y := APoint3D.y;
      MeshLocation := Mesh.Mesh2DI.TopContainingCellOrElement(APoint2D, EvalAt);
      ACol := MeshLocation.Col;
      if ACol < 0 then
      begin
        Exit;
      end;

    end;

    if Counts[ARow, ACol] = 0 then
    begin
      Values[ARow, ACol] := APoint3D.z;
      if ImportMethod = imClosest then
      begin
        APoint2D.x := APoint3D.x;
        APoint2D.y := APoint3D.y;
        Distances[ARow, ACol] := Distance(APoint2D, CenterPoints[ARow, ACol]);
      end;
    end;
    Counts[ARow, ACol] := Counts[ARow, ACol] + 1;
    case ImportMethod of
      imLowest:
        begin
          if Values[ARow, ACol] > APoint3D.z then
          begin
            Values[ARow, ACol] := APoint3D.z;
          end;
        end;
      imHighest:
        begin
          if Values[ARow, ACol] < APoint3D.z then
          begin
            Values[ARow, ACol] := APoint3D.z;
          end;
        end;
      imAverage:
        begin
          if Counts[ARow, ACol] <> 1 then
          begin
            // The first value has already been set if Counts[ARow, ACol] = 1.
            Values[ARow, ACol] := Values[ARow, ACol] + APoint3D.z;
          end;
        end;
      imClosest:
        begin
          APoint2D.x := APoint3D.x;
          APoint2D.y := APoint3D.y;
          ADistance := Distance(APoint2D, CenterPoints[ARow, ACol]);
          if Distances[ARow, ACol] > ADistance then
          begin
            Values[ARow, ACol] := APoint3D.z;
            Distances[ARow, ACol] := ADistance;
          end;
        end;
    end;


  end;
end;

procedure TfrmImportSurferGridFiles.HandleARasterPoint(Sender: TObject;
  APoint: TPoint3D);
var
  Point2D: TPoint2D;
begin
  if (FGrid <> nil) and (FGrid.GridAngle <> 0) then
  begin
    Point2D.x := APoint.X;
    Point2D.y := APoint.Y;
    Point2D := FGrid.
      RotateFromRealWorldCoordinatesToGridCoordinates(Point2D);
    APoint.x := Point2D.x;
    APoint.y := Point2D.y;
  end;
  HandleAPoint(APoint, ImportMethod, EvalAt, FGrid, FMesh);
end;

function TfrmImportSurferGridFiles.InitializeArrays(
  ImportMethod: TImportMethod): Boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
  Grid: TCustomModelGrid;
//  Mesh: TSutraMesh3D;
  ColumnCount: Integer;
  RowCount: Integer;
  Mesh: IMesh3D;
begin
  result := True;
  if LocalModel = nil then
  begin
    LocalModel := frmGoPhast.PhastModel;
  end;
  Grid := LocalModel.Grid;
  Mesh := LocalModel.Mesh3D;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  ColumnCount := -1;
  RowCount := -1;
  if Grid <> nil then
  begin
    case EvalAt of
      eaBlocks:
        begin
          ColumnCount := Grid.ColumnCount;
          RowCount := Grid.RowCount;
        end;
      eaNodes:
        begin
          ColumnCount := Grid.ColumnCount+1;
          RowCount := Grid.RowCount+1;
        end;
    end;
  end
  else if Mesh <> nil then
  begin
    RowCount := 1;
    case EvalAt of
      eaBlocks:
        begin
          ColumnCount := Mesh.Mesh2DI.ElementCount;
        end;
      eaNodes:
        begin
          ColumnCount := Mesh.Mesh2DI.NodeCount;
        end;
    end;
  end
  else
  begin
    Assert(False);
  end;

  if (ColumnCount < 0) or (RowCount < 0) then
  begin
    Beep;
    MessageDlg(StrYouMustHaveAGrid, mtError, [mbOK], 0);
    result := false;
    Exit;
  end;

  SetLength(Values, RowCount, ColumnCount);
  SetLength(Counts, RowCount, ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      Counts[RowIndex, ColIndex] := 0;
      Values[RowIndex, ColIndex] := 0;
    end;
  end;
  if ImportMethod = imClosest then
  begin
    SetLength(Distances, RowCount, ColumnCount);
  end;
  SetLength(CenterPoints, RowCount, ColumnCount);
  if Grid <> nil then
  begin
    case EvalAt of
      eaBlocks:
        begin
          for RowIndex := 0 to RowCount - 1 do
          begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              CenterPoints[RowIndex, ColIndex] :=
                Grid.UnrotatedTwoDElementCenter(ColIndex, RowIndex);
            end;
          end;
        end;
      eaNodes:
        begin
          for RowIndex := 0 to RowCount do
          begin
            for ColIndex := 0 to ColumnCount do
            begin
              CenterPoints[RowIndex, ColIndex] :=
                Grid.UnrotatedTwoDElementCorner(ColIndex, RowIndex);
            end;
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    Assert(Mesh <> nil);
    RowIndex := 0;
    case EvalAt of
      eaBlocks:
        begin
            for ColIndex := 0 to ColumnCount - 1 do
            begin
              CenterPoints[RowIndex, ColIndex] :=
                Mesh.Mesh2DI.ElementsI2D[ColIndex].Center;
            end;
        end;
      eaNodes:
        begin
            for ColIndex := 0 to ColumnCount-1 do
            begin
              CenterPoints[RowIndex, ColIndex] :=
                Mesh.Mesh2DI.Nodes[ColIndex].Location;
            end;
        end;
      else Assert(False);
    end;
  end;
end;

function TfrmImportSurferGridFiles.MakeNewDataSet(NewDataSets: TList; Suffix,
  Classification: string; NewDataSetNeeded: Boolean; FileName: string): string;
var
  DataSet: TDataArray;
  AType: TInterpolatorType;
  Interpolator: TCustom2DInterpolater;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataSet: TDataArray;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
  if NewDataSetNeeded then
  begin
    Assert(FileName <> '');
    result := ExtractFileName(FileName);
    result := ChangeFileExt(result, '');
    result := GenerateNewName(result + Suffix);

    DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(TDataArray,
      result, '0.', result, [], rdtDouble,
      TEvaluatedAt(rgEvaluatedAt.ItemIndex), dsoTop, Classification);

    DataSet.OnDataSetUsed := frmGoPhast.PhastModel.ModelResultsRequired;
    DataSet.Units := '';

    if comboInterpolators.ItemIndex > 0 then
    begin
      AType := TInterpolatorType(comboInterpolators.Items.
        Objects[comboInterpolators.ItemIndex]);
      Interpolator := AType.Create(nil);
      try
        DataSet.TwoDInterpolator := Interpolator
      finally
        Interpolator.Free;
      end;
    end;

    frmGoPhast.PhastModel.UpdateDataArrayDimensions(DataSet);

    if frmGoPhast.PhastModel.LgrUsed then
    begin
      for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
        if ChildModel <> nil then
        begin
          ChildDataSet := ChildModel.DataArrayManager.GetDataSetByName(DataSet.Name);
          ChildModel.UpdateDataArrayDimensions(ChildDataSet);
        end;
      end;
    end;
//    DataSet.UpdateDimensions(frmGoPhast.Grid.LayerCount,
//      frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);
    NewDataSets.add(DataSet);
  end;
end;

procedure TfrmImportSurferGridFiles.SetCheckBoxCaptions;
begin
  FNodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
    frmGoPhast.ModelSelection, True, False);
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
end;

procedure TfrmImportSurferGridFiles.SetData;
const
  BlankValue = 1.70141e+38;
  Epsilon = 1e-8;
  function ConvertPoint2(ASurferPoint: TSurferPoint): TPoint3D;
  begin
    Result.x := ASurferPoint.X;
    Result.Y := ASurferPoint.Y;
    Result.z := ASurferPoint.Z;
  end;
var
  NewDataSets: TList;
  FileIndex: Integer;
  FileName: string;
  DataSetName: string;
  DataSet: TDataArray;
  FGrd6: TSurfer6Grid;
  FGrd7: TSurferRaster7File2;
  ScreenObjectList: TList;
  Undo: TUndoImportGrdFiles;
  Root: string;
  ExistingObjectCount: Integer;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  Position: Integer;
  PointIndex: Integer;
  APoint3D: TSurferPoint;
  APoint2D: TPoint2D;
  Item: TValueArrayItem;
  UsedIndex: Integer;
  Z: Double;
  Count: Integer;
  YIndex: Integer;
  XIndex: Integer;
  PointCount: Integer;
  Formula: string;
begin

  LocalModel := frmGoPhast.PhastModel;
  frmGoPhast.PhastModel.BeginScreenObjectUpdate;
  frmGoPhast.CanDraw := False;

  try
    NewDataSets := TList.Create;
    try
      ScreenObjectList := TList.Create;
      //MultipleParts := false;
      try
        Undo := TUndoImportGrdFiles.Create;
        try
          try
            ScreenObjectList.Capacity := OpenDialogFile.Files.Count;
            for FileIndex := 0 to OpenDialogFile.Files.Count - 1 do
            begin
              FileName := OpenDialogFile.Files[FileIndex];
              DataSetName := MakeNewDataSet(NewDataSets, StrGrdZ, strDefaultClassification + '|' + StrImportedFromSurfer,
                True, FileName);
              DataSet := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(DataSetName);
              Assert(DataSet <> nil);
              FGrd7 := nil;

              try
                case FGridTypes[FileIndex] of
                  sft6:
                    begin
                      ReadSurfer6GrdFile(FileName, FGrd6);
                      DataSet.Comment := Format(CommentStr,
                        [FGrd6.Header.Xlo, FGrd6.Header.Xhi,
                        FGrd6.Header.Ylo, FGrd6.Header.Yhi,
                        FGrd6.Header.Zlo, FGrd6.Header.Zhi]);
                    end;
                  sftAscii:
                    begin
                      ReadSurferAsciiFile(FileName, FGrd6);
                      DataSet.Comment := Format(CommentStr,
                        [FGrd6.Header.Xlo, FGrd6.Header.Xhi,
                        FGrd6.Header.Ylo, FGrd6.Header.Yhi,
                        FGrd6.Header.Zlo, FGrd6.Header.Zhi]);
                    end;
                  sft7:
                    begin
                      FGrd7 := TSurferRaster7File2.Create(FileName);
                      DataSet.Comment := Format(CommentStr,
                        [FGrd7.Header.xLL,
                        FGrd7.Header.xLL + FGrd7.Header.xSize* FGrd7.Header.nCol,
                        FGrd7.Header.yLL,
                        FGrd7.Header.yLL + FGrd7.Header.ySize* FGrd7.Header.nRow,
                        FGrd7.Header.zMin, FGrd7.Header.zMax]);
                    end
                  else Assert(False);
                end;

                if rgFilterMethod.ItemIndex <> 4 then
                begin
                  ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
                  FGrid := LocalModel.Grid;
                  FMesh := LocalModel.Mesh3D;
                  if not InitializeArrays(ImportMethod) then
                  begin
                    Exit;
                  end;
                  GetDiscretizationMinMax
                end;


                Root := TScreenObject.ValidName(
                  ExtractFileRoot(FileName));
                ExistingObjectCount :=
                  frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

                AScreenObject :=
                  TScreenObject.CreateWithViewDirection(
                  frmGoPhast.PhastModel, vdTop,
                  UndoCreateScreenObject, False);
                AScreenObject.Comment := 'Imported from ' + FileName +' on ' + DateTimeToStr(Now);
                if ExistingObjectCount > 0 then
                begin
                  AScreenObject.Name := Root + '_'+ IntToStr(ExistingObjectCount);
                end
                else
                begin
                  AScreenObject.Name := Root;
                end;
                AScreenObject.SetValuesOfIntersectedCells
                  := cbIntersectedCells.Checked;
                AScreenObject.SetValuesByInterpolation
                  := cbInterpolation.Checked;
                AScreenObject.ElevationCount := ecZero;
                AScreenObject.EvaluatedAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
                AScreenObject.Visible := False;


                Position := -1;
                case FGridTypes[FileIndex] of
                  sft6, sftAscii:
                    begin
                      if rgFilterMethod.ItemIndex = 4 then
                      begin
                        AScreenObject.Capacity := FGrd6.Header.nx * FGrd6.Header.ny;
                        for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                        begin
                          APoint3D := FGrd6.Points[PointIndex];
                          if Abs(APoint3D.Z - BlankValue)/BlankValue >= Epsilon then
                          begin
                            APoint2D := ConvertPoint(FGrd6.Points[PointIndex]);
                            AScreenObject.AddPoint(APoint2D, True);
                          end;
                        end;
                        ScreenObjectList.Add(AScreenObject);
                        Position := AScreenObject.AddDataSet(DataSet);
                        Assert(Position >= 0);

                        Item := AScreenObject.
                          ImportedValues.Add as TValueArrayItem;
                        Item.Name := DataSet.Name;
                        Item.Values.DataType := DataSet.DataType;
                        Item.Values.Count := FGrd6.Header.nx * FGrd6.Header.ny;

                        UsedIndex := 0;
                        for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                        begin
                          Z := FGrd6.Points[PointIndex].Z;
                          if Abs(Z - BlankValue)/BlankValue >= Epsilon then
                          begin
                            Item.Values.RealValues[UsedIndex] := Z;
                            Inc(UsedIndex);
                          end;
                        end;
                        Item.Values.Count := UsedIndex;
                        Item.CacheData;
                      end
                      else
                      begin
                        ScreenObjectList.Add(AScreenObject);
                        Position := AScreenObject.AddDataSet(DataSet);
                        Assert(Position >= 0);
                        for PointIndex := 0 to Length(FGrd6.Points) - 1 do
                        begin
                          APoint3D := FGrd6.Points[PointIndex];
                          if Abs(APoint3D.Z - BlankValue)/BlankValue >= Epsilon then
                          begin
                            HandleARasterPoint(nil, ConvertPoint2(APoint3D));
                          end;
                        end;
                      end;
                    end;
                  sft7:
                    begin
                      if rgFilterMethod.ItemIndex = 4 then
                      begin
                        Count := 0;
                        for YIndex := 0 to FGrd7.YCount - 1 do
                        begin
                          for XIndex := 0 to FGrd7.XCount - 1 do
                          begin
                            if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                            begin
                              Inc(Count);
                            end;
                          end;
                        end;
                        AScreenObject.Capacity := Count;
                        for YIndex := 0 to FGrd7.YCount - 1 do
                        begin
                          for XIndex := 0 to FGrd7.XCount - 1 do
                          begin
                            if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                            begin
                              APoint2D := ConvertPoint(FGrd7.Points[XIndex,YIndex]);
                              AScreenObject.AddPoint(APoint2D, True);
                            end;
                          end;
                        end;
                        ScreenObjectList.Add(AScreenObject);
                        Position := AScreenObject.AddDataSet(DataSet);
                        Assert(Position >= 0);

                        Item := AScreenObject.
                          ImportedValues.Add as TValueArrayItem;
                        Item.Name := DataSet.Name;
                        Item.Values.DataType := DataSet.DataType;
                        Item.Values.Count := Count;

                        PointCount := 0;
                        for YIndex := 0 to FGrd7.YCount - 1 do
                        begin
                          for XIndex := 0 to FGrd7.XCount - 1 do
                          begin
                            if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                            begin
                              Item.Values.RealValues[PointCount] :=
                                FGrd7.Z[XIndex, YIndex];
                              Inc(PointCount);
                            end;
                          end;
                        end;
                        Item.CacheData;
                      end
                      else
                      begin
                        ScreenObjectList.Add(AScreenObject);
                        Position := AScreenObject.AddDataSet(DataSet);
                        Assert(Position >= 0);

                        for YIndex := 0 to FGrd7.YCount - 1 do
                        begin
                          for XIndex := 0 to FGrd7.XCount - 1 do
                          begin
                            if FGrd7.Z[XIndex,YIndex] < FGrd7.Header.BlankValue then
                            begin
                              HandleARasterPoint(nil, ConvertPoint2(FGrd7.Points[XIndex, YIndex]));
                            end;
                          end;
                        end;
                      end;
                    end;

                  else Assert(False);
                end;

                if rgFilterMethod.ItemIndex <> 4 then
                begin
                  ComputeAverage(ImportMethod);

                  Item := AScreenObject.
                    ImportedValues.Add as TValueArrayItem;
                  Item.Name := DataSet.Name;
                  Item.Values.DataType := DataSet.DataType;
                  if LocalModel.Grid <> nil then
                  begin
                    case EvalAt of
                      eaBlocks:
                        begin
                          AScreenObject.Capacity := FGrid.ColumnCount
                            * FGrid.RowCount;
                        end;
                      eaNodes:
                        begin
                          AScreenObject.Capacity :=
                            (FGrid.ColumnCount+1)
                            * (FGrid.RowCount + 1);
                        end;
                      else
                        Assert(False);
                    end;
                  end
                  else
                  begin
                    case EvalAt of
                      eaBlocks:
                        begin
                          AScreenObject.Capacity := FMesh.Mesh2DI.ElementCount;
                        end;
                      eaNodes:
                        begin
                          AScreenObject.Capacity := FMesh.Mesh2DI.NodeCount;
                        end;
                      else
                        Assert(False);
                    end;
                  end;

                  Item.Values.Count := AScreenObject.Capacity;
                  AssignPointsAndValues(FGrid, AScreenObject, Item);
                end;

                Formula := rsObjectImportedValuesR + '("' + DataSet.Name + '")';
                AScreenObject.DataSetFormulas[Position] := Formula;
              finally
                FGrd7.Free;
              end;


            end;

            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
            Undo := nil;
            frmGoPhast.PhastModel.AddFileToArchive(FileName);
          finally
            Undo.Free;
          end;
        except
          raise;
        end;
      finally
        ScreenObjectList.Free;
      end;

    finally
      NewDataSets.Free
    end;
  finally
    frmGoPhast.CanDraw := True;
    frmGoPhast.PhastModel.EndScreenObjectUpdate;
  end
end;

procedure TfrmImportSurferGridFiles.UpdateEvalAt;
begin
  rgEvaluatedAt.Items[Ord(eaBlocks)] :=
    EvalAtToString(eaBlocks, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] :=
    EvalAtToString(eaNodes, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled :=
    frmGoPhast.PhastModel.ModelSelection in [msPhast, msSutra22, msSutra30, msSutra40];

end;

{ TUndoImportGrdFiles }

function TUndoImportGrdFiles.Description: string;
begin
  result := StrImportSurferGridFs;
end;

end.
