unit frmImportDEMUnit;

interface

uses System.UITypes, System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomImportSimpleFileUnit, StdCtrls, Buttons, ExtCtrls,
  frmImportShapefileUnit, ArgusDataEntry, FastGEO, AbstractGridUnit,
  GoPhastTypes, ScreenObjectUnit, ValueArrayStorageUnit;

type
  {@abstract(@name is the command used to import
    DEM files or reverse the import.)}
  TUndoImportDemFile = class(TUndoImportShapefile)
  protected
    // @name describes what @classname does.
    function Description: string; override;
  end;

  TfrmImportDEM = class(TfrmCustomImportSimpleFile)
    rgFilterMethod: TRadioGroup;
    memoCorners: TMemo;
    Label1: TLabel;
    cbIgnore: TCheckBox;
    rdeIgnore: TRbwDataEntry;
    procedure btnOKClick(Sender: TObject);
    procedure cbIgnoreClick(Sender: TObject);
  protected
    procedure SetCheckBoxCaptions; override;
  private
    FDemIndex: Integer;
    procedure SetData;
    procedure DemProgress(Sender: TObject; FractionDone: double);
    procedure ImportProgress(Sender: TObject; FractionDone: double);
    function DisplayCornerCoordinates: boolean;
    procedure InvalidDem;
    { Private declarations }
  public
    function GetData: boolean;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, DemReaderUnit, 
  ModelMuseUtilities, DataSetUnit, frmProgressUnit, UndoItems,
  GIS_Functions, CoordinateConversionUnit, SutraMeshUnit;

resourcestring
  StrSampleDigitalEleva = 'sample Digital Elevation Model';
  StrReadingS = 'Reading %s';
  StrThereWasAnErrorR = 'There was an error reading the DEM file.  Please ch' +
  'eck that the format of the DEM is a format that ModelMuse supports.  Chec' +
  'k the ModelMuse help to see what formats ModelMuse supports.  For further' +
  ' assistance contact rbwinst@usgs.gov.';
  StrImportingData = 'Importing data';
  StrYouMustCreateThe = 'You must create the grid before importing a Digital' +
  ' Elevation Model.';
  StrYouMustCreateTheMesh = 'You must create the mesh before importing a Digital' +
  ' Elevation Model.';
  StrElement = 'element';
  StrElementCenter = 'element center';
  StrCell = 'cell';
  StrNode = 'node';
  StrCellCenter = 'cell center';
  StrLowestPointInS = 'Lowest point in %s';
  StrHighestPointInS = 'Highest point in %s';
  StrAverageOfPointsIn = 'Average of points in %s';
  StrPointClosestToS = 'Point closest to %s';
  StrProgress = 'Progress';
  StrSampledFromDEMFil = 'Sampled from DEM files using ';
  StrNoneOfThePointsI = 'None of the points in the DEM are inside the grid.';
  StrMeshNoneOfThePoints = 'None of the points in the DEM are inside the mesh.';
  Str«ornerCoordinatesN = '«orner coordinates not listed in DEM file. Check ' +
  'that the file is of the correct type.';

{$R *.dfm}


{ TfrmCustomImportSimpleFile2 }

procedure TfrmImportDEM.btnOKClick(Sender: TObject);
begin
  inherited;
  Screen.Cursor := crHourGlass;
  try
    SetData;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmImportDEM.cbIgnoreClick(Sender: TObject);
begin
  inherited;
  rdeIgnore.Enabled := cbIgnore.Checked;
end;

procedure TfrmImportDEM.DemProgress(Sender: TObject; FractionDone: double);
begin
  FractionDone := (FDemIndex + FractionDone)/(OpenDialogFile.Files.Count+1);
  frmProgressMM.pbProgress.Position
    := Round(frmProgressMM.pbProgress.Max * FractionDone);
  frmProgressMM.ProgressLabelCaption :=
    Format(StrReadingS, [OpenDialogFile.Files[FDemIndex]]);
  Application.ProcessMessages;
end;

function TfrmImportDEM.DisplayCornerCoordinates: boolean;
const
  FormatString = '(%0:g, %1:g)';
var
  DemIndex: Integer;
  DemReader: TDemReader;
  CornerIndex: Integer;
  Corner: TCornerPoint;
  Line: string;
  CentralMeridian: double;
  X: Double;
  Y: Double;
//  OldDecSep: Char;
begin
  result := OpenDialogFile.Files.Count > 0;
  CentralMeridian := 0.0;
  for DemIndex := 0 to OpenDialogFile.Files.Count - 1 do
  begin
    DemReader := TDemReader.Create;
    try
      DemReader.ReadHeader(OpenDialogFile.Files[DemIndex]);
      if DemIndex = 0 then
      begin
//        OldDecSep := FormatSettings.DecimalSeparator;
//        try
          CentralMeridian := DemReader.CentralMeridianRadians;
//        finally
//          FormatSettings.DecimalSeparator := OldDecSep;
//        end;
      end;
      memoCorners.Lines.Add(OpenDialogFile.Files[DemIndex]);
      if DemReader.CornerCount = 4 then
      begin
        for CornerIndex := 0 to 3 do
        begin
          Corner := DemReader.Corners[CornerIndex];
          if DemReader.CoordInSec then
          begin
            ConvertToUTM(Corner.Y/60/60/180*PI, Corner.X/60/60/180*PI, CentralMeridian,
              X, Y);
            Line := Format(FormatString, [Corner.X/3600, Corner.Y/3600])
              + '; ' + Format(FormatString,[X,Y]);
          end
          else
          begin
            Line := Format(FormatString, [Corner.X, Corner.Y]);
          end;
          memoCorners.Lines.Add(Line);
        end;
      end
      else
      begin
        memoCorners.Lines.Add(Str«ornerCoordinatesN);
        result := False;
      end;
    finally
      DemReader.Free;
    end;
  end;
end;

procedure TfrmImportDEM.ImportProgress(Sender: TObject; FractionDone: double);
begin
  FractionDone := (OpenDialogFile.Files.Count + FractionDone)/
    (OpenDialogFile.Files.Count+1);
  frmProgressMM.pbProgress.Position
    := Round(frmProgressMM.pbProgress.Max * FractionDone);
  frmProgressMM.ProgressLabelCaption := StrImportingData;
  Application.ProcessMessages;
end;

procedure TfrmImportDEM.InvalidDem;
begin
  Beep;
  MessageDlg(StrThereWasAnErrorR, mtError, [mbOK], 0);
end;

function TfrmImportDEM.GetData: boolean;
var
  Grid: TCustomModelGrid;
  Mesh: TSutraMesh3D;
begin
  result := False;
  case frmGoPhast.ModelSelection of
    msSutra22, msSutra30:
      begin
        Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;
        result := (Mesh <> nil) and (Mesh.Mesh2D.Nodes.Count > 0);
        if result then
        begin
          result := OpenDialogFile.Execute;
        end
        else
        begin
          MessageDlg(StrYouMustCreateTheMesh, mtInformation, [mbOK], 0);
        end;
      end;
    msPhast, msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
      begin
        Grid := frmGoPhast.PhastModel.Grid;
        result := (Grid <> nil) and (Grid.ColumnCount > 0)
          and (Grid.RowCount > 0);
        if result then
        begin
          result := OpenDialogFile.Execute;
        end
        else
        begin
          MessageDlg(StrYouMustCreateThe, mtInformation, [mbOK], 0);
        end;
      end;
    else Assert(False);
  end;
  if result then
  begin
    GetDataSets;
    GetInterpolators;
    UpdateEvalAt;
    SetCheckBoxCaptions;
    try
      try
        Result := DisplayCornerCoordinates;
      except
        on E: EInOutError do
        begin
          result := False;
          Beep;
          MessageDlg(E.Message, mtError, [mbOK], 0);
        end;
      else
        begin
          result := False;
        end;
      end;
    finally
      if not result then
      begin
        InvalidDem;
      end;
    end;
  end;
end;

procedure TfrmImportDEM.SetCheckBoxCaptions;
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
    msPhast, msSutra22, msSutra30:
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

procedure TfrmImportDEM.SetData;
var
  Grid: TCustomModelGrid;
  DemReader: TDemReader;
  FirstFile: string;
  CentralMeridian: Double;
  DemIndex: Integer;
  PointIndex: Integer;
  APoint: TElevationPoint;
  Point2D: TPoint2D;
  EvalAt: TEvaluatedAt;
  ImportMethod: TImportMethod;
  NewDataSets: TList;
  Undo: TUndoImportDemFile;
  Root: string;
  DataSetName: string;
  DataSet: TDataArray;
  ScreenObjectList: TList;
  ExistingObjectCount: Integer;
  AScreenObject: TScreenObject;
  UndoCreateScreenObject: TCustomUndo;
  Item: TValueArrayItem;
  DS_Position: Integer;
  IgnoreValue: Integer;
  APoint3D: TPoint3D;
  Mesh: TSutraMesh3D;
  DivAmount: Integer;
  Fraction: Extended;
begin
  frmProgressMM.Caption := StrProgress;
  frmProgressMM.Show;
  try
    EvalAt := TEvaluatedAt(rgEvaluatedAt.ItemIndex);
    ImportMethod := TImportMethod(rgFilterMethod.ItemIndex);
    if not InitializeArrays(ImportMethod) then
    begin
      Exit;
    end;

    FirstFile := OpenDialogFile.Files[0];
    try
      DemReader := TDemReader.Create;
      try
        DemReader.ReadHeader(FirstFile);
        CentralMeridian := DemReader.CentralMeridianRadians;
      finally
        DemReader.Free;
      end;
    except on EConvertError do
      begin
        InvalidDEM;
        Exit;
      end;
    end;
    GetDiscretizationMinMax;

    Grid := frmGoPhast.PhastModel.Grid;
    Mesh := frmGoPhast.PhastModel.Mesh as TSutraMesh3D;

    IgnoreValue := StrToInt(rdeIgnore.Text);
    try
      for DemIndex := 0 to OpenDialogFile.Files.Count - 1 do
      begin
        FDemIndex := DemIndex;
        DemReader := TDemReader.Create;
        try
          DemReader.OnProgress := DemProgress;
          DemReader.CentralMeridianRadians := CentralMeridian;
          DemReader.ReadFile(OpenDialogFile.Files[DemIndex], False);
          DivAmount := DemReader.PointCount div 1000;
          frmProgressMM.ProgressLabelCaption := StrImportingData;
          for PointIndex := 0 to DemReader.PointCount - 1 do
          begin
            APoint := DemReader.Points[PointIndex];
            if cbIgnore.Checked and (IgnoreValue = APoint.Value) then
            begin
              Continue
            end;

            if PointIndex mod DivAmount = 0 then
            begin
              Fraction := PointIndex/DemReader.PointCount/2 + 0.5;
              frmProgressMM.pbProgress.Position
                := Round(frmProgressMM.pbProgress.Max * Fraction);
//              frmProgressMM.ProgressLabelCaption := StrImportingData;
              Application.ProcessMessages;
            end;

            if Grid <> nil then
            begin
              Point2D.x := APoint.X;
              Point2D.y := APoint.Y;
              Point2D := Grid.
                RotateFromRealWorldCoordinatesToGridCoordinates(Point2D);
              APoint3D.x := Point2D.x;
              APoint3D.y := Point2D.y;
            end
            else
            begin
              APoint3D.x := APoint.x;
              APoint3D.y := APoint.y;
            end;
            APoint3D.z := APoint.Elevation;
            HandleAPoint(APoint3D, ImportMethod, EvalAt, Grid, Mesh);
          end;
        finally
          DemReader.Free;
        end;
      end;
    except on EConvertError do
      begin
        InvalidDEM;
        Exit;
      end;
    end;
    ComputeAverage(ImportMethod, ImportProgress);
    frmGoPhast.PhastModel.BeginScreenObjectUpdate;
    frmGoPhast.CanDraw := False;
    try
      NewDataSets := TList.Create;
      try
        MakeNewDataSet(NewDataSets, '_DEM_Elevation',
          strDefaultClassification + '|' + StrSampledFromDEMFil
          + LowerCase(rgFilterMethod.Items[rgFilterMethod.ItemIndex]),
          comboDataSets.ItemIndex = 0);
        DataSetName := comboDataSets.Text;
        DataSet := frmGoPhast.PhastModel.DataArrayManager.
          GetDataSetByName(DataSetName);
        Assert(DataSet <> nil);
        ScreenObjectList := TList.Create;
        try
          Root := TScreenObject.ValidName(
            ExtractFileRoot(OpenDialogFile.FileName)+ '_');
          ScreenObjectList.Capacity := 1;
          ExistingObjectCount :=
            frmGoPhast.PhastModel.NumberOfLargestScreenObjectsStartingWith(Root);

          AScreenObject :=
            TScreenObject.CreateWithViewDirection(
            frmGoPhast.PhastModel, vdTop,
            UndoCreateScreenObject, False);
          AScreenObject.Comment := 'Imported from ' + OpenDialogFile.FileName +' on ' + DateTimeToStr(Now);
          Inc(ExistingObjectCount);
          AScreenObject.Name := Root + IntToStr(ExistingObjectCount);
          AScreenObject.SetValuesOfEnclosedCells
            := cbEnclosedCells.Checked;
          AScreenObject.SetValuesOfIntersectedCells
            := cbIntersectedCells.Checked;
          AScreenObject.SetValuesByInterpolation
            := cbInterpolation.Checked;
          AScreenObject.ColorLine := False;
          AScreenObject.FillScreenObject := False;
          AScreenObject.ElevationCount := ecZero;
          AScreenObject.Capacity := Length(Values) * Length(Values[0]);
          AScreenObject.EvaluatedAt := EvalAt;
          AScreenObject.Visible := False;

          Item := AScreenObject.
            ImportedValues.Add as TValueArrayItem;
          Item.Name := DataSet.Name;
          Item.Values.DataType := DataSet.DataType;
          Item.Values.Count := AScreenObject.Capacity;
          AssignPointsAndValues(Grid, AScreenObject, Item);


          DS_Position := AScreenObject.AddDataSet(DataSet);
          AScreenObject.DataSetFormulas[DS_Position] :=
            rsObjectImportedValuesR + '("' + DataSet.Name + '")';
          if AScreenObject.Count = 0 then
          begin
            Beep;
            if Grid <> nil then
            begin
              MessageDlg(StrNoneOfThePointsI, mtError, [mbOK], 0);
            end
            else
            begin
              MessageDlg(StrMeshNoneOfThePoints, mtError, [mbOK], 0);
            end;
            AScreenObject.Free;
          end
          else
          begin
            ScreenObjectList.Add(AScreenObject);
          end;

          Undo := TUndoImportDemFile.Create;
          try
            Undo.StoreNewScreenObjects(ScreenObjectList);
            Undo.StoreNewDataSets(NewDataSets);
            frmGoPhast.UndoStack.Submit(Undo);
            if ScreenObjectList.Count = 0 then
            begin
              frmGoPhast.UndoStack.Undo(1);
              frmGoPhast.UndoStack.Remove(Undo);
              Undo.Free;
              frmGoPhast.tbRedo.Enabled := False;
            end;
          except
            Undo.Free;
            raise;
          end;
          for DemIndex := 0 to OpenDialogFile.Files.Count-1 do
          begin
            frmGoPhast.PhastModel.AddFileToArchive
              (OpenDialogFile.Files[DemIndex]);
          end;
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
  finally
    frmProgressMM.Hide;
  end;
end;

{ TUndoImportDemFile }

function TUndoImportDemFile.Description: string;
begin
  result := StrSampleDigitalEleva;
end;

end.
