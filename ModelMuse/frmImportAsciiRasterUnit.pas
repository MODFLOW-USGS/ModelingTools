unit frmImportAsciiRasterUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls, FastGEO, 
  frmImportShapefileUnit, AbstractGridUnit, GoPhastTypes, Grids, RbwDataGrid4,
  PhastModelUnit, MeshRenumberingTypes;

type

  {@abstract(@name is the command used to import
    the ASCII Raster file or reverse the import.)}
  TUndoImportAsciiRasterFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportAsciiRaster = class(TfrmCustomImportSimpleFile)
    rgFilterMethod: TRadioGroup;
    rdgFilesAndDataSets: TRbwDataGrid4;
    comboModel: TComboBox;
    lblConvert: TLabel;
    comboFromUnits: TComboBox;
    lblTo: TLabel;
    comboToUnits: TComboBox;
    lblWarning: TLabel;
    procedure btnOKClick(Sender: TObject);
    procedure rdgFilesAndDataSetsButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormCreate(Sender: TObject); override;
  private
    FAsciiRasterFileName: string;
    FValues: TPoint3DArray;
    FGrid: TCustomModelGrid;
    ImportMethod: TImportMethod;
    FEvalAt: TEvaluatedAt;
    FMesh: IMesh3D;
    procedure SetData;
    procedure HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
    { Private declarations }
  protected
    procedure SetCheckBoxCaptions; override;
    procedure GetDataSets; override;
  public
    function GetData: boolean;
    { Public declarations }
  end;

var
  frmImportAsciiRaster: TfrmImportAsciiRaster;

resourcestring
  StrElement = 'element';
  StrElementCenter = 'element center';
  StrCell = 'cell';
  StrNode = 'node';
  StrCellCenter = 'cell center';
  StrLowestPointInS = 'Lowest point in %s';
  StrHighestPointInS = 'Highest point in %s';
  StrAverageOfPointsIn = 'Average of points in %s';
  StrPointClosestToS = 'Point closest to %s';

implementation

uses
  frmProgressUnit, AsciiRasterReaderUnit, frmGoPhastUnit, DataSetUnit,
  ScreenObjectUnit, UndoItems, ModelMuseUtilities,
  ValueArrayStorageUnit, GIS_Functions;

resourcestring
  StrYouMustHaveAVali = 'You must have a valid grid before attempting ' +
    'to sample a raster file.';
  StrYouMustHaveAMesh = 'You must have a valid mesh before attempting ' +
    'to sample a raster file.';
  StrTheLocationsInS = 'The locations in %s that have data are different fro' +
  'm the locations in %s that have data.';
  StrImportASCIIRaster = 'import ASCII raster file';
  StrFileName = 'File name';
  StrDataSet = 'Data set';
  StrTheFileSDoesN = 'The file "%s" does not exist.';
  StrTheFileSIsNot = 'The file "%s" either is not an ASCII raster file or it could not be read.';
  StrNoneImportAllS = 'None (import all %d points)';
  StrMultipleFiles = ' - multiple files';
  StrImportedFromAnAS = 'Imported from an ASCII Raster file';
  StrSampledFromAnASCI = 'Sampled from an ASCII Raster file';
  StrProgress = 'Progress';
  StrNoDataPointsWere = 'No data points were inside the model area';
  StrNoPointsWereInclu = 'No points were included in %0:d %1:s so they have ' +
  'been skipped.';

{$R *.dfm}

type
  TAsciiRasterGridColumns = (gcNone, gcFileName, gcDataSet);

{ TfrmImportAsciiRaster }

procedure TfrmImportAsciiRaster.btnOKClick(Sender: TObject);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    SetData;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmImportAsciiRaster.FormCreate(Sender: TObject);
begin
  inherited;
  rdgFilesAndDataSets.Cells[Ord(gcFileName), 0] := StrFileName;
  rdgFilesAndDataSets.Cells[Ord(gcDataSet), 0] := StrDataSet;
end;

function TfrmImportAsciiRaster.GetData: boolean;
var
  AsciiReader: TAsciiRasterReader;
  FileHeader: TRasterHeader;
  NumberOfPoints: Int64;
  FileIndex: Integer;
  Model: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
//  Number: string;
  ComponentIndex: integer;
  AControl: TControl;
begin
  UpdateEvalAt;

  result := OpenDialogFile.Execute;
  if result then
  begin
    if OpenDialogFile.Files.Count > 1 then
    begin
      rdgFilesAndDataSets.RowCount := OpenDialogFile.Files.Count + 1;
    end;
    rdgFilesAndDataSets.BeginUpdate;
    try
      for FileIndex := 0 to OpenDialogFile.Files.Count - 1 do
      begin

        FAsciiRasterFileName := OpenDialogFile.Files[FileIndex];
        if not FileExists(FAsciiRasterFileName) then
        begin
          result := False;
          Beep;
          MessageDlg(Format(StrTheFileSDoesN, [FAsciiRasterFileName]),
            mtError, [mbOK], 0);
          Exit;
        end;

        AsciiReader := TAsciiRasterReader.Create;
        try
          AsciiReader.FileName := FAsciiRasterFileName;
          result := AsciiReader.ValidFileHeader;
          if not result then
          begin
            Beep;
            MessageDlg(Format(StrTheFileSIsNot, [FAsciiRasterFileName]),
              mtError, [mbOK], 0);
            Exit;
          end
          else if FileIndex = 0 then
          begin
            FileHeader := AsciiReader.FileHeader;
            // avoid integer overflow by first converting to Int64.
            NumberOfPoints := FileHeader.NumberOfColumns;
            NumberOfPoints := NumberOfPoints * FileHeader.NumberOfRows;
//            Number := FloatToStrF(NumberOfPoints, ffNumber, 15, 0);
            rgFilterMethod.Items[4] := Format(StrNoneImportAllS, [NumberOfPoints]);
          end;
        finally
          AsciiReader.Free;
        end;
        if OpenDialogFile.Files.Count > 1 then
        begin
          rdgFilesAndDataSets.Cells[Ord(gcFileName), FileIndex+1]
            := FAsciiRasterFileName;
          rdgFilesAndDataSets.Cells[Ord(gcDataSet), FileIndex+1]
            := rsNewDataSet;
        end;
      end;
    finally
      rdgFilesAndDataSets.EndUpdate;
    end;

    if OpenDialogFile.Files.Count = 1 then
    begin
      Caption := Caption + ' - ' + FAsciiRasterFileName;
    end
    else
    begin
      Caption := Caption + StrMultipleFiles;
      rdgFilesAndDataSets.Visible := True;
      lblDataSet.Visible := False;
      comboDataSets.Visible := False;
      Height := Height + 150;
      rdgFilesAndDataSets.Height := 200;
      for ComponentIndex := 0 to ControlCount - 1 do
      begin
        AControl := Controls[ComponentIndex];
        if AControl <> rdgFilesAndDataSets then
        begin
          AControl.Top := AControl.Top + 150;
        end;
      end;
//      rgFilterMethod.Controls[4].Enabled := False;
    end;
    GetDataSets;
    comboInterpolators.ItemIndex := 1;

    Model := frmGoPhast.PhastModel;
    comboModel.Items.AddObject(Model.DisplayName, Model);
    if Model.LgrUsed then
    begin
      for ChildIndex := 0 to Model.ChildModels.Count - 1 do
      begin
        ChildModel := Model.ChildModels[ChildIndex].ChildModel;
        comboModel.Items.AddObject(ChildModel.DisplayName, ChildModel);
      end;
    end
    else
    begin
      comboModel.Enabled := False;
    end;
    comboModel.ItemIndex := 0;
  end;
end;

procedure TfrmImportAsciiRaster.GetDataSets;
begin
  inherited;
  rdgFilesAndDataSets.Columns[Ord(gcDataSet)].PickList := comboDataSets.Items;
end;

procedure TfrmImportAsciiRaster.HandleARasterPoint(Sender: TObject; APoint: TPoint3D);
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
  HandleAPoint(APoint, ImportMethod, FEvalAt, FGrid, FMesh);
end;

procedure TfrmImportAsciiRaster.rdgFilesAndDataSetsButtonClick(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  OpenDialogFile.Options := OpenDialogFile.Options - [ofAllowMultiSelect];
  if OpenDialogFile.Execute then
  begin

  end;
end;

procedure TfrmImportAsciiRaster.SetCheckBoxCaptions;
var
  NodeElemString: string;
  EvalAt: TEvaluatedAt;
  CenterString: string;
begin
  inherited;
  EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
  case frmGoPhast.PhastModel.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast, msSutra22, msSutra30, msSutra40:
      begin
        case EvalAt of
          eaBlocks:
            begin
              NodeElemString := StrElement;
              CenterString := StrElementCenter
            end;
          eaNodes:
            begin
              NodeElemString := StrCell;
              CenterString := StrNode;
            end;
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        NodeElemString := StrCell;
        CenterString := StrCellCenter
      end;
    else Assert(False);
  end;
  rgFilterMethod.Items[Ord(imLowest)] :=
    Format(StrLowestPointInS, [NodeElemString]);
  rgFilterMethod.Items[Ord(imHighest)] :=
    Format(StrHighestPointInS, [NodeElemString]);
  rgFilterMethod.Items[Ord(imAverage)] :=
    Format(StrAverageOfPointsIn, [NodeElemString]);
  rgFilterMethod.Items[Ord(imClosest)] :=
    Format(StrPointClosestToS, [CenterString]);
end;

procedure TfrmImportAsciiRaster.SetData;
var
  NewDataSets: TList;
  DataArrayName: string;
  DataArray: TDataArray;
  ScreenObjectList: TList;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  PointIndex: integer;
  Root: string;
  ExistingObjectCount: integer;
  Item: TValueArrayItem;
  DA_Position: integer;
  Undo: TUndoImportAsciiRasterFile;
  AsciiReader: TAsciiRasterReader;
  DataSetCount: Integer;
  DataSetIndex: Integer;
  APoint: TPoint2D;
  ConvertUnits: boolean;
  FromUnits: TSupportedLengthConv;
  ToUnits: TSupportedLengthConv;
  FileNames: TStrings;
  ShouldCreateScreenObject: Boolean;
//  LocalModel: TCustomModel;
  function ConvertPoint(const A3D_Point: TPoint3D): TPoint2D;
  begin
    result.X := A3D_Point.X;
    result.Y := A3D_Point.Y;
  end;
begin
//  OutputDebugString('SAMPLING ON');
  ConvertUnits := (comboFromUnits.ItemIndex > 0) and (comboToUnits.ItemIndex > 0);
  if ConvertUnits then
  begin
    FromUnits := TSupportedLengthConv(comboFromUnits.ItemIndex-1);
    ToUnits := TSupportedLengthConv(comboToUnits.ItemIndex-1);
  end
  else
  begin
    FromUnits := slcMeter;
    ToUnits := slcMeter;
  end;

  FileNames := OpenDialogFile.Files;

  LocalModel := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  AScreenObject := nil;
  try
    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    frmGoPhast.CanDraw := False;
    try
      NewDataSets := TList.Create;
      try
        if rdgFilesAndDataSets.Visible then
        begin
          DataSetCount := rdgFilesAndDataSets.RowCount -1;
        end
        else
        begin
          DataSetCount := 1;
        end;

        ScreenObjectList := TList.Create;
        try
          for DataSetIndex := 0 to DataSetCount - 1 do
          begin
            ShouldCreateScreenObject := (DataSetIndex = 0);
//              or (rgFilterMethod.ItemIndex = 4);
            if ShouldCreateScreenObject then
            begin
              Root := TScreenObject.ValidName(
                ExtractFileRoot(FileNames[DataSetIndex])+ '_');
              ExistingObjectCount :=
                frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

              AScreenObject :=
                TScreenObject.CreateWithViewDirection(
                frmGoPhast.PhastModel, vdTop,
                UndoCreateScreenObject, False);
              AScreenObject.Comment := 'Imported from ' + FileNames[DataSetIndex]
                +' on ' + DateTimeToStr(Now);


              AScreenObject.Name := Root + IntToStr(ExistingObjectCount+1);
              AScreenObject.SetValuesOfEnclosedCells
                := False;
              AScreenObject.SetValuesOfIntersectedCells
                := cbIntersectedCells.Checked;
              AScreenObject.SetValuesByInterpolation
                := cbInterpolation.Checked;
              AScreenObject.ElevationCount := ecZero;

              AScreenObject.EvaluatedAt :=
                TEvaluatedAt(rgEvaluatedAt.ItemIndex);
              AScreenObject.Visible := False;
            end
            else
            begin
              AScreenObject.Comment := AScreenObject.Comment
                + sLineBreak + 'and ' + FileNames[DataSetIndex];
            end;

            if ShouldCreateScreenObject then
            begin
              if LocalModel.Grid <> nil then
              begin
                case TEvaluatedAt(rgEvaluatedAt.ItemIndex) of
                  eaBlocks:
                    begin
                      AScreenObject.Capacity := LocalModel.Grid.ColumnCount
                        * LocalModel.Grid.RowCount;
                    end;
                  eaNodes:
                    begin
                      AScreenObject.Capacity :=
                        (LocalModel.Grid.ColumnCount+1)
                        * (LocalModel.Grid.RowCount + 1);
                    end;
                  else
                    Assert(False);
                end;
              end
              else
              begin
                case TEvaluatedAt(rgEvaluatedAt.ItemIndex) of
                  eaBlocks:
                    begin
                      AScreenObject.Capacity := LocalModel.Mesh3D.Mesh2DI.ElementCount;
                    end;
                  eaNodes:
                    begin
                      AScreenObject.Capacity := LocalModel.Mesh3D.Mesh2DI.NodeCount;
                    end;
                  else
                    Assert(False);
                end;
              end;
            end;

            if rgFilterMethod.ItemIndex = 4 then
            begin
              if DataSetCount = 1 then
              begin
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|' + StrImportedFromAnAS,
                  comboDataSets.ItemIndex = 0);
              end
              else
              begin
                FAsciiRasterFileName := rdgFilesAndDataSets.Cells[
                  Ord(gcFileName), DataSetIndex+1];
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|' + StrImportedFromAnAS,
                  rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0,
                  FAsciiRasterFileName);
              end;
            end
            else
            begin
              if DataSetCount = 1 then
              begin
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|'+ StrSampledFromAnASCI,
                  comboDataSets.ItemIndex = 0);
              end
              else
              begin
                FAsciiRasterFileName := rdgFilesAndDataSets.Cells[Ord(gcFileName), DataSetIndex+1];
                MakeNewDataSet(NewDataSets, '',
                  strDefaultClassification + '|'+ StrSampledFromAnASCI,
                  rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0,
                  FAsciiRasterFileName);
              end;
            end;

            if (DataSetCount = 1) or
              (rdgFilesAndDataSets.ItemIndex[Ord(gcDataSet), DataSetIndex+1] = 0)  then
            begin
              DataArrayName := comboDataSets.Text;
            end
            else
            begin
              DataArrayName := rdgFilesAndDataSets.Cells[Ord(gcDataSet), DataSetIndex+1];
            end;

            DataArray := frmGoPhast.PhastModel.DataArrayManager.
              GetDataSetByName(DataArrayName);
            Assert(DataArray <> nil);

            AsciiReader := TAsciiRasterReader.Create;
            try
              AsciiReader.FileName := FAsciiRasterFileName;
              if rgFilterMethod.ItemIndex = 4 then
              begin
                try
                  frmProgressMM.PopupParent := self;
                  frmProgressMM.Caption := StrProgress;
                  frmProgressMM.Show;
                  try
                    AsciiReader.ReadAsciiRaster(FValues, frmProgressMM.pbProgress);
                  finally
                    frmProgressMM.Hide
                  end;
                except on E: EOutOfMemory do
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(E.Message, mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                AScreenObject.Capacity := Length(FValues);
                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataArray.Name;
                Item.Values.DataType := DataArray.DataType;
                Item.Values.Count := AScreenObject.Capacity;
                for PointIndex := 0 to Length(FValues) - 1 do
                begin
//                  Assert(DataSetCount = 1);
                  APoint := ConvertPoint(FValues[PointIndex]);
                  if ShouldCreateScreenObject then
                  begin
                    if ConvertUnits then
                    begin
                      AScreenObject.AddPoint(ConvertPoint2D(APoint, FromUnits, ToUnits), True);
                    end
                    else
                    begin
                      AScreenObject.AddPoint(APoint, True);
                    end;
                  end
                  else
                  begin
                    if ConvertUnits then
                    begin
                      APoint := ConvertPoint2D(APoint, FromUnits, ToUnits);
                    end;
                    if not PointsEqual(AScreenObject.Points[PointIndex], APoint) then
                    begin
                      AScreenObject.Free;
                      Beep;
                      MessageDlg(Format('Point %0:d in %1:s don''t match.',
                        [PointIndex, FAsciiRasterFileName]), mtError, [mbOK], 0);
                      Exit;
//                      raise EDifferentPointsError.Create(
//                        Format('Point %0:d in %1:s don''t match.', [PointIndex, FAsciiRasterFileName]));
                    end;
                  end;
                  Item.Values.RealValues[PointIndex] := FValues[PointIndex].z;
                end;
                Item.CacheData;
              end
              else
              begin

                if (LocalModel.Grid <> nil) then
                begin
                  if (LocalModel.Grid.ColumnCount <= 0)
                    or (LocalModel.Grid.RowCount <= 0) then
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(StrYouMustHaveAVali , mtError, [mbOK], 0);
                    Exit;
                  end;
                end
                else
                begin
                  if LocalModel.Mesh3D.Mesh2DI.ElementCount = 0 then
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(StrYouMustHaveAMesh , mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                try
                  GetDiscretizationMinMax;
                except on EInvalidGrid do
                  begin
                    AScreenObject.Free;
                    Beep;
                    MessageDlg(StrYouMustHaveAVali , mtError, [mbOK], 0);
                    Exit;
                  end;
                end;
                AsciiReader.OnReadPoint := HandleARasterPoint;
                FEvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
                ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
                FGrid := LocalModel.Grid;
                FMesh := LocalModel.Mesh3D;
                InitializeArrays(ImportMethod);

                frmProgressMM.PopupParent := self;
                frmProgressMM.Caption := StrProgress;
                frmProgressMM.Show;
                try
                  AsciiReader.ReadAsciiRaster(frmProgressMM.pbProgress);
                finally
                  frmProgressMM.Hide
                end;

                ComputeAverage(ImportMethod);

                Item := AScreenObject.
                  ImportedValues.Add as TValueArrayItem;
                Item.Name := DataArray.Name;
                Item.Values.DataType := DataArray.DataType;
                if LocalModel.Grid <> nil then
                begin
                  case FEvalAt of
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
                  case FEvalAt of
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
//                case EvalAt of
//                  eaBlocks: AScreenObject.Capacity
//                    := FGrid.ColumnCount * FGrid.RowCount;
//                  eaNodes: AScreenObject.Capacity
//                    := (FGrid.ColumnCount+1) * (FGrid.RowCount+1);
//                  else Assert(False);
//                end;
                Item.Values.Count := AScreenObject.Capacity;
                AssignPointsAndValues(FGrid, AScreenObject, Item, DataSetIndex <> 0);

                if MissingCount > 0 then
                begin
                  Beep;
                  MessageDlg(Format(StrNoPointsWereInclu,
                    [MissingCount, NodeElemString]), mtWarning, [mbOK], 0);
                end;
              end;
            finally
              AsciiReader.Free;
            end;

            DA_Position := AScreenObject.AddDataSet(DataArray);
            AScreenObject.DataSetFormulas[DA_Position] :=
              rsObjectImportedValuesR + '("' + DataArray.Name + '")';

            if LocalModel <> frmGoPhast.PhastModel then
            begin
              AScreenObject.UsedModels.AddModel(LocalModel);
              AScreenObject.UsedModels.UsedWithAllModels := False;
            end;

            if AScreenObject.Count > 0 then
            begin
              if ShouldCreateScreenObject then
              begin
                ScreenObjectList.Add(AScreenObject);
              end;
            end
            else
            begin
              AScreenObject.Free;
              AScreenObject := nil;
              Beep;
              MessageDlg(StrNoDataPointsWere , mtError, [mbOK], 0);
              Exit;
            end;
          end;

          Undo := TUndoImportAsciiRasterFile.Create;
          try
            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
          except
            AScreenObject.Free;
            AScreenObject := nil;
            Undo.Free;
            raise;
          end;

          frmGoPhast.PhastModel.AddFileToArchive(OpenDialogFile.FileName);
        finally
          ScreenObjectList.Free;
        end;
      finally
        NewDataSets.Free;
      end;
    finally
      frmGoPhast.CanDraw := True;
      frmGoPhast.PhastModel.EndScreenObjectUpdate;
    end;
  except on EDifferentPointsError do
    begin
      Beep;
      MessageDlg(Format(StrTheLocationsInS, [FAsciiRasterFileName,
        rdgFilesAndDataSets.Cells[Ord(gcFileName), 1]]), mtError, [mbOK], 0);
      AScreenObject.Free;
    end;
  end;
//  OutputDebugString('SAMPLING OFF');
end;

{ TUndoImportAsciiRasterFile }

function TUndoImportAsciiRasterFile.Description: string;
begin
  result := StrImportASCIIRaster;
end;

end.
