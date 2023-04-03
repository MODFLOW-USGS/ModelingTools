{@abstract(The main purpose of @name is to define
  @link(TfrmCustomImportSimpleFile)
  which is used as a base class for importing
  DXF and Surfer grid files into ModelMuse. )}
unit frmCustomImportSimpleFileUnit;


interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls,
  ScreenObjectUnit,
  FastGEO, AbstractGridUnit, GoPhastTypes,
  ValueArrayStorageUnit, PhastModelUnit, MeshRenumberingTypes;

type
  TImportMethod = (imLowest, imHighest, imAverage, imClosest);
  TImportProgress = procedure (Sender: TObject; FractionDone: double) of object;

  EDifferentPointsError = class(Exception);

  {@abstract(@name is a base class used to import DXF and Surfer
    grid files into ModelMuse.)
    See @link(TfrmGoPhast.miImportDXFFileClick).}
  TfrmCustomImportSimpleFile = class(TfrmCustomGoPhast)
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // Clicking @name displays help about @classname.
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of enclosed cells or elements in the related
    // @link(TDataArray).
    cbEnclosedCells: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of cells or elements in the related
    // @link(TDataArray) by interpolation.
    cbInterpolation: TCheckBox;
    // @name indicates that the value of the imported @link(TScreenObject)s
    // should set the value of intersected cells or elements in the related
    // @link(TDataArray).
    cbIntersectedCells: TCheckBox;
    // @name is used to select the name of the @link(TDataArray) to be
    // affected by the imported @link(TScreenObject)s.
    comboDataSets: TComboBox;
    // @name is the name of the @link(TCustom2DInterpolater) that will
    // be used with a new @link(TDataArray).
    comboInterpolators: TComboBox;
    // @name displays "Data Set".
    lblDataSet: TLabel;
    // @name displays "Interpolator".
    lblInterpolator: TLabel;
    // @name is used to select the DXF file.
    OpenDialogFile: TOpenDialog;
    // @name indicates whether a new data set will be evaluated at
    // elements or cells.
    rgEvaluatedAt: TRadioGroup;
    // @name makes sure that at least one of the following checkboxes is
    // checked: @link(cbEnclosedCells), @link(cbIntersectedCells), and
    // @link(cbInterpolation).  If not, their fonts are changed to emphasize
    // them and @link(btnOK) is disabled.
    procedure cbEnclosedCellsClick(Sender: TObject);
    // @name enables or disables @link(comboInterpolators) depending
    // on whether a new @link(TDataArray) is to be created.
    // @link(comboInterpolators) will be enabled if
    // a new @link(TDataArray) is to be created.
    procedure comboDataSetsChange(Sender: TObject);
    // @name enables @link(cbInterpolation) if an interpolator
    // is specified.
    procedure comboInterpolatorsChange(Sender: TObject);
    // @name calls @link(GetInterpolators).
    procedure FormCreate(Sender: TObject); override;
    // @name changes the captions of @link(cbEnclosedCells),
    // @link(cbIntersectedCells), and @link(cbInterpolation).
    procedure rgEvaluatedAtClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject); override;
  private
    FMissingCount: Integer;
    FNodeElemString: string;
//    FQuadTree: TRbwQuadTree;
//    FOutline: TSubPolygon;
  protected
    Values: array of array of double;
    Counts: array of array of integer;
    CenterPoints: array of array of TPoint2D;
    Distances: array of array of double;
    MinX: Real;
    MaxX: Real;
    MinY: Real;
    MaxY: Real;
    LocalModel: TCustomModel;
    procedure GetDiscretizationMinMax;
    procedure HandleAPoint(APoint3D: TPoint3D; ImportMethod: TImportMethod;
      EvalAt: TEvaluatedAt; Grid: TCustomModelGrid; Mesh: IMesh3D);
    // @name updates the contents of rgEvaluatedAt to the appropriate
    // values depending on what model (PHAST or MODFLOW) is selected.
    procedure UpdateEvalAt;
    // @name fills @link(comboDataSets) with the names of
    // @link(TDataArray)s that can be used by the imported
    // @link(TScreenObject)s.
    procedure GetDataSets; virtual;
    // @name fills @link(comboInterpolators) with a list of
    // @link(TCustom2DInterpolater)s.
    procedure GetInterpolators;
    // If @link(comboDataSets).ItemIndex = 0,
    // @name creates a new @link(TDataArray) with
    // an @link(TDataArray.Orientation) of dsoTop
    // and adds it to NewDataSets.
    // Suffix is a string that will be added to the end of the file name
    // when generating the data set name.
    // Classification is the @link(TDataArray.Classification)
    // of the @link(TDataArray).
    // If @name does create a new @link(TDataArray), @link(comboDataSets).Text
    // is set to the name of the @link(TDataArray).
    procedure MakeNewDataSet(NewDataSets: TList; Suffix, Classification: string;
      NewDataSetNeeded: Boolean; FileName: string = '');
    { Set the captions of @link(cbEnclosedCells), @link(cbIntersectedCells),
      and @link(cbInterpolation) based on @link(rgEvaluatedAt).ItemIndex
      depending on what model (PHAST or MODFLOW) is selected.}
    procedure SetCheckBoxCaptions; virtual;
    function InitializeArrays(ImportMethod: TImportMethod): Boolean;
    procedure AssignPointsAndValues(Grid: TCustomModelGrid;
      AScreenObject: TScreenObject; Item: TValueArrayItem;
      ComparePoints: Boolean = False);
    procedure ComputeAverage(ImportMethod: TImportMethod;
      ImportProgress: TImportProgress = nil);
    { Protected declarations }
  public
    property MissingCount: Integer read FMissingCount;
    property NodeElemString: string read FNodeElemString;
    { Public declarations }
  end;

function PointsEqual(Point1, Point2: TPoint2D): Boolean;

resourcestring
  StrPointsDontMatch = 'Points don''t match';

implementation

uses frmGoPhastUnit, DataSetUnit,
  RbwParser, frmDataSetsUnits;

resourcestring
  StrYouMustHaveAGrid = 'You must have a grid or mesh defined before you can' +
  ' import the data from your file.';

{$R *.dfm}

procedure TfrmCustomImportSimpleFile.FormCreate(Sender: TObject);
begin
  inherited;
  cbEnclosedCellsClick(nil);
  SetCheckBoxCaptions;
  GetInterpolators;
end;

procedure TfrmCustomImportSimpleFile.FormDestroy(Sender: TObject);
begin
  inherited;
//  FOutline.Free;
end;

procedure TfrmCustomImportSimpleFile.MakeNewDataSet(NewDataSets: TList;
  Suffix, Classification: string; NewDataSetNeeded: Boolean; FileName: string = '');
var
  NewDataSetName: string;
  DataSet: TDataArray;
  AType: TInterpolatorType;
  Interpolator: TCustom2DInterpolater;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  ChildDataSet: TDataArray;
begin
  Assert(SizeOf(TObject) = SizeOf(TInterpolatorType));
//  if comboDataSets.ItemIndex = 0 then
  if NewDataSetNeeded then
  begin
    if FileName = '' then
    begin
      FileName := OpenDialogFile.FileName;
    end;
    NewDataSetName := ExtractFileName(FileName);
    NewDataSetName := ChangeFileExt(NewDataSetName, '');
    NewDataSetName := GenerateNewName(NewDataSetName + Suffix);

    DataSet := frmGoPhast.PhastModel.DataArrayManager.CreateNewDataArray(TDataArray,
      NewDataSetName, '0.', NewDataSetName, [], rdtDouble,
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

    comboDataSets.Items[0] := NewDataSetName;
    comboDataSets.Text := NewDataSetName;
    comboDataSets.ItemIndex := 0;
  end;
end;

procedure TfrmCustomImportSimpleFile.GetInterpolators;
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

procedure TfrmCustomImportSimpleFile.HandleAPoint(APoint3D: TPoint3D;
  ImportMethod: TImportMethod; EvalAt: TEvaluatedAt; Grid: TCustomModelGrid;
  Mesh: IMesh3D);
//const
  // These two constants were determined empirically.
  // In a test case, the desired element was always found
  // among those retrieved from the mesh using these constants.
  // In a test case, the desired node was either always found
  // among those retrieved from the mesh using these constants or
  // was the first node among the additional nodes tested.
//  PointCountForElements = 6;
//  PointCountForNodes = 6;
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

function TfrmCustomImportSimpleFile.InitializeArrays(
  ImportMethod: TImportMethod): Boolean;
var
  RowIndex: Integer;
  ColIndex: Integer;
  Grid: TCustomModelGrid;
  EvalAt: TEvaluatedAt;
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

procedure TfrmCustomImportSimpleFile.GetDataSets;
var
  EvalAt: TEvaluatedAt;
  DataSet: TDataArray;
  Index: integer;
  DataArrayManager: TDataArrayManager;
begin
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  with comboDataSets.Items do
  begin
    Clear;
    AddObject(rsNewDataSet, nil);
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if (DataSet.EvaluatedAt = EvalAt)
        and (DataSet.Orientation = dsoTop)
        and (DataSet.DataType = rdtDouble) then
      begin
        AddObject(DataSet.Name, DataSet);
      end;
    end;
  end;
  comboDataSets.ItemIndex := 0;
  comboDataSetsChange(nil);
end;

procedure TfrmCustomImportSimpleFile.SetCheckBoxCaptions;
begin
  FNodeElemString := EvalAtToString(TEvaluatedAt(rgEvaluatedAt.ItemIndex),
    frmGoPhast.ModelSelection, True, False);
  cbEnclosedCells.Caption := rsSetValueOfEnclosed + NodeElemString;
  cbIntersectedCells.Caption := rsSetValueOfIntersected + NodeElemString;
  cbInterpolation.Caption := rsSetValueOf + NodeElemString + rsByInterpolation;
end;

procedure TfrmCustomImportSimpleFile.UpdateEvalAt;
begin
  rgEvaluatedAt.Items[Ord(eaBlocks)] :=
    EvalAtToString(eaBlocks, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Items[Ord(eaNodes)] :=
    EvalAtToString(eaNodes, frmGoPhast.PhastModel.ModelSelection, True, True);
  rgEvaluatedAt.Enabled :=
    frmGoPhast.PhastModel.ModelSelection in [msPhast, msSutra22, msSutra30, msSutra40];
end;

procedure TfrmCustomImportSimpleFile.rgEvaluatedAtClick(Sender: TObject);
begin
  inherited;
  SetCheckBoxCaptions;
  GetDataSets;
end;

procedure TfrmCustomImportSimpleFile.comboDataSetsChange(Sender: TObject);
begin
  inherited;
  comboInterpolators.Enabled := comboDataSets.Text = rsNewDataSet;
end;

procedure TfrmCustomImportSimpleFile.comboInterpolatorsChange(Sender: TObject);
begin
  inherited;
  cbInterpolation.Enabled := comboInterpolators.ItemIndex <> 0;
  if not cbInterpolation.Enabled then
  begin
    cbInterpolation.Checked := False;
  end;
end;

procedure TfrmCustomImportSimpleFile.ComputeAverage(
  ImportMethod: TImportMethod; ImportProgress: TImportProgress = nil);
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

procedure TfrmCustomImportSimpleFile.GetDiscretizationMinMax;
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

function PointsEqual(Point1, Point2: TPoint2D): Boolean;
begin
  result := (Point1.x = Point2.x) and (Point1.y = Point2.y);
end;

procedure TfrmCustomImportSimpleFile.AssignPointsAndValues(Grid: TCustomModelGrid;
  AScreenObject: TScreenObject; Item: TValueArrayItem; ComparePoints: Boolean = False);
var
  ValueIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  GridPoint2D: TPoint2D;
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

procedure TfrmCustomImportSimpleFile.cbEnclosedCellsClick(Sender: TObject);
begin
  inherited;
  EmphasizeCheckBoxes([cbEnclosedCells, cbIntersectedCells, cbInterpolation]);
  btnOK.Enabled := cbEnclosedCells.Checked or
    cbIntersectedCells.Checked or
    cbInterpolation.Checked;
end;

end.



