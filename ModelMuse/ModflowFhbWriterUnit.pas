unit ModflowFhbWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, CustomModflowWriterUnit, ScreenObjectUnit, PhastModelUnit, GoPhastTypes,
  RealListUnit, SysUtils, Forms, ModflowPackageSelectionUnit,
  Classes, ModflowBoundaryDisplayUnit, Vcl.Dialogs;

type
  TModflowFhbWriter = class(TCustomTransientWriter)
  private
    FHeadScreenObjects: TScreenObjectList;
    FFlowScreenObjects: TScreenObjectList;
    FTimes: TRealList;
    NoAssignmentErrorRoot: string;
    FNameOfFile: string;
    FFlowValues: TList;
    FUnitNumber: integer;
    FPrintInputCellLists: Boolean;
    NFLW: Integer;
    NHED: integer;
    procedure FillScreenObjectList;
    procedure GetModelTimes;
    procedure EvaluateHeadBoundaries;
    procedure EvaluateFlowBoundaries;
    procedure WriteDataSet1;
    procedure WriteDataSets2and3;
    procedure WriteDataSet4a;
    procedure WriteDataSet4b;
    procedure WriteDataSet5;
    procedure WriteDataSet6;
    procedure WriteDataSet7;
    procedure WriteDataSet8;
    procedure WriteDataSetHeader(const Comment: string);
    procedure WriteCellValues(const HeaderComment, LineComment: string;
      ValuesToExport: TList);
    procedure WriteAuxiliaryValues(const HeaderComment, LineComment: string;
      ValuesToExport: TList);
    procedure ClearTimeLists(AModel: TBaseModel);
    function TimeToPlot: double;
  protected
    procedure Evaluate; override;
    function Package: TModflowPackageSelection; override;
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  frmProgressUnit, ModflowTimeUnit, ModflowFhbUnit, ModflowBoundaryUnit,
  frmErrorsAndWarningsUnit, ModflowUnitNumbers, Contnrs, frmGoPhastUnit,
  DataSetUnit, System.Math;

resourcestring
  StrInvalidFHBSpecHead = 'Invalid FHB Specified Head times will be ignored';
  StrInvalidFHBSpecFlow = 'Invalid FHB Specified Flow times will be ignored';
  StrOneOrMoreOfTheSpecHead = 'One or more of the specified head times in ob' +
  'ject %0:s, was after the last time in the model';
  StrOneOrMoreOfTheSpecFlow = 'One or more of the specified flow times in ob' +
  'ject %0:s, was after the last time in the model';

{ TModflowFhbWriter }

procedure TModflowFhbWriter.ClearTimeLists(AModel: TBaseModel);
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TModflowBoundary;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    Boundary := ScreenObject.ModflowFhbHeadBoundary;
    if Boundary <> nil then
    begin
      Boundary.ClearTimeLists(AModel);
    end;
    Boundary := ScreenObject.ModflowFhbFlowBoundary;
    if Boundary <> nil then
    begin
      Boundary.ClearTimeLists(AModel);
    end;
  end;
end;

constructor TModflowFhbWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FHeadScreenObjects := TScreenObjectList.Create;
  FFlowScreenObjects := TScreenObjectList.Create;
  FTimes := TRealList.Create;
  FTimes.Sorted := True;
  FFlowValues := TObjectList.Create;
end;

destructor TModflowFhbWriter.Destroy;
begin
  FFlowValues.Free;
  FTimes.Free;
  FFlowScreenObjects.Free;
  FHeadScreenObjects.Free;
  inherited;
end;

procedure TModflowFhbWriter.Evaluate;
begin
  frmErrorsAndWarnings.BeginUpdate;
  try
    NoAssignmentErrorRoot := Format(StrNoBoundaryConditio, [Package.PackageIdentifier]);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);

    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidFHBSpecHead);
    frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidFHBSpecFlow);

    frmGoPhast.PhastModel.FixFhbItems;
    FillScreenObjectList;
    GetModelTimes;
    EvaluateHeadBoundaries;
    EvaluateFlowBoundaries;

    FPrintInputCellLists := Model.ModflowOutputControl.PrintInputCellLists;
    if (FTimes.Count > 1) and (FTimes.Last = FTimes[FTimes.Count-2]) then
    begin
      FTimes.Delete(FTimes.Count-1);
    end;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowFhbWriter.EvaluateFlowBoundaries;
var
  ScreenObjectIndex: Integer;
  Boundary: TFhbFlowBoundary;
  ScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to FFlowScreenObjects.Count - 1 do
  begin
    ScreenObject := FFlowScreenObjects[ScreenObjectIndex];
    Boundary := ScreenObject.ModflowFhbFlowBoundary;
    Assert(Boundary <> nil);
    if not ScreenObject.SetValuesOfEnclosedCells
      and not ScreenObject.SetValuesOfIntersectedCells then
    begin
      frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
        ScreenObject.Name, ScreenObject);
    end;
    frmProgressMM.AddMessage(Format(StrEvaluatingS,
      [ScreenObject.Name]));
    Boundary.GetCellValues(FFlowValues, nil, Model);
  end;
end;

procedure TModflowFhbWriter.EvaluateHeadBoundaries;
var
  ScreenObjectIndex: Integer;
  Boundary: TFhbHeadBoundary;
  ScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to FHeadScreenObjects.Count - 1 do
  begin
    ScreenObject := FHeadScreenObjects[ScreenObjectIndex];
    Boundary := ScreenObject.ModflowFhbHeadBoundary;
    Assert(Boundary <> nil);
    if not ScreenObject.SetValuesOfEnclosedCells
      and not ScreenObject.SetValuesOfIntersectedCells then
    begin
      frmErrorsAndWarnings.AddError(Model, NoAssignmentErrorRoot,
        ScreenObject.Name, ScreenObject);
    end;
    frmProgressMM.AddMessage(Format(StrEvaluatingS,
      [ScreenObject.Name]));
    Boundary.GetCellValues(Values, nil, Model);
  end;
end;

class function TModflowFhbWriter.Extension: string;
begin
  result := '.fhb';
end;

procedure TModflowFhbWriter.FillScreenObjectList;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
begin
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if not ScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;

    if ScreenObject.ModflowFhbHeadBoundary <> nil then
    begin
      FHeadScreenObjects.Add(ScreenObject);
    end;
    if ScreenObject.ModflowFhbFlowBoundary <> nil then
    begin
      FFlowScreenObjects.Add(ScreenObject);
    end;
  end;
end;

function TModflowFhbWriter.TimeToPlot: double;
var
  LocalModel: TCustomModel;
  StressPeriods: TModflowStressPeriods;
  StartTime: Double;
  EndTime: Double;
begin
  LocalModel := Model as TCustomModel;
  result := LocalModel.ThreeDDisplayTime;
  StressPeriods := LocalModel.ModflowFullStressPeriods;
  StartTime := StressPeriods.First.StartTime;
  if result < StartTime then
  begin
    result := StartTime;
  end;
  EndTime := StressPeriods.Last.EndTime;
  if result > EndTime then
  begin
    result := EndTime;
  end;
end;

procedure TModflowFhbWriter.GetModelTimes;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Values: TFhbHeadCollection;
  ItemIndex: Integer;
  AnItem: TCustomBoundaryItem;
  DuplicateTimes: TRealList;
  PriorItem: TCustomBoundaryItem;
  TimeIndex: Integer;
  Cells: TFhbCellList;
  EndTime: Double;
  ThreeDDisplayTime: Double;
  ClosestIndex: Integer;
  ClosestTime: Double;
  HighTime: Double;
//  InnerTimeIndex: integer;
  LowTime: Double;
begin
  EndTime := Model.ModflowFullStressPeriods.Last.EndTime;
  DuplicateTimes:= TRealList.Create;
  try
    DuplicateTimes.Sorted := True;
    for ScreenObjectIndex := 0 to FHeadScreenObjects.Count - 1 do
    begin
      ScreenObject := FHeadScreenObjects[ScreenObjectIndex];
      Values := ScreenObject.ModflowFhbHeadBoundary.Values as TFhbHeadCollection;
      PriorItem := nil;
      for ItemIndex := 0 to Values.Count - 1 do
      begin
        AnItem := Values.Items[ItemIndex];
        if AnItem.StartTime > EndTime then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrInvalidFHBSpecHead,
            Format(StrOneOrMoreOfTheSpecHead, [ScreenObject.Name]), ScreenObject);
          break;
        end;
        FTimes.AddUnique(AnItem.StartTime);
        if (PriorItem <> nil) and (PriorItem.StartTime = AnItem.StartTime) then
        begin
          DuplicateTimes.AddUnique(AnItem.StartTime);
        end;
        PriorItem := AnItem;
      end;
    end;
    for ScreenObjectIndex := 0 to FFlowScreenObjects.Count - 1 do
    begin
      ScreenObject := FFlowScreenObjects[ScreenObjectIndex];
      Values := ScreenObject.ModflowFhbFlowBoundary.Values as TFhbFlowCollection;
      PriorItem := nil;
      for ItemIndex := 0 to Values.Count - 1 do
      begin
        AnItem := Values.Items[ItemIndex];
        if AnItem.StartTime > EndTime then
        begin
          frmErrorsAndWarnings.AddWarning(Model, StrInvalidFHBSpecFlow,
            Format(StrOneOrMoreOfTheSpecFlow, [ScreenObject.Name]), ScreenObject);
          break;
        end;
        FTimes.AddUnique(AnItem.StartTime);
        if (PriorItem <> nil) and (PriorItem.StartTime = AnItem.StartTime) then
        begin
          DuplicateTimes.AddUnique(AnItem.StartTime);
        end;
        PriorItem := AnItem;
      end;
    end;
    if FEvaluationType = etDisplay then
    begin
      FTimes.AddUnique(TimeToPlot);
    end;
    for TimeIndex := 0 to DuplicateTimes.Count - 1 do
    begin
      FTimes.Add(DuplicateTimes[TimeIndex]);
    end;
  finally
    DuplicateTimes.Free
  end;

  FTimes.Add(EndTime);

  if FEvaluationType = etDisplay then
  begin
    ThreeDDisplayTime := TimeToPlot;
    ClosestIndex := FTimes.IndexOfClosest(ThreeDDisplayTime);
    ClosestTime := FTimes[ClosestIndex];
    HighTime := ClosestTime;
    for TimeIndex := ClosestIndex to FTimes.Count - 1 do
    begin
      HighTime := FTimes[TimeIndex];
      if HighTime > ThreeDDisplayTime then
      begin
        break;
      end;
    end;
    LowTime := ClosestTime;
    for TimeIndex := ClosestIndex downto 0 do
    begin
      LowTime := FTimes[TimeIndex];
      if LowTime < ThreeDDisplayTime then
      begin
        break;
      end;
    end;
    for TimeIndex := FTimes.Count - 1 downto 0 do
    begin
      if (FTimes[TimeIndex] > HighTime) or (FTimes[TimeIndex] < LowTime) then
      begin
        FTimes.Delete(TimeIndex);
      end;
    end;

    if FTimes.IndexOf(ClosestTime) > 0 then
    begin
      FTimes.Delete(0);
    end;
  end;

  for TimeIndex := 1 to FTimes.Count - 1 do
  begin
    Cells := TFhbCellList.Create(TFhb_Cell);
    Cells.StartTime := FTimes[TimeIndex-1];
    Cells.EndTime := FTimes[TimeIndex];
    Self.Values.Add(Cells);

    Cells := TFhbCellList.Create(TFhb_Cell);
    Cells.StartTime := FTimes[TimeIndex-1];
    Cells.EndTime := FTimes[TimeIndex];
    FFlowValues.Add(Cells);
  end;
end;

function TModflowFhbWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.FhbPackage;
end;

procedure TModflowFhbWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  DataArrayList: TList;
  TimeIndex: Integer;
  CellList: TFhbCellList;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  LocalTimeToPlot: Double;
  InnerTimeIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  try
    DataArrayList := TList.Create;
    try
      Evaluate;

      ClearTimeLists(Model);

      LocalTimeToPlot := TimeToPlot;


      // For each stress period, transfer values from
      // the cells lists to the data arrays.
      TimeListIndex := 0;
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        CellList := Values[TimeIndex];
        if LocalTimeToPlot < CellList.StartTime then
        begin
          continue
        end
        else
        begin
          if LocalTimeToPlot > CellList.EndTime then
          begin
            break;
          end;
        end;
  //      if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
  //        for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];

            for InnerTimeIndex := 0 to DisplayTimeList.Count - 1 do
            begin
              DataArray := DisplayTimeList[InnerTimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.UpToDate := True;
            end;

            DataArray := DisplayTimeList[0]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;


          UpdateCellDisplay(CellList, DataArrayList, []);
          DataArray.UpToDate := True;
          DisplayTimeList.SetUpToDate(True);
        end;
  //      break;
      end;

      TimeListIndex := 1;
      for TimeIndex := 0 to FFlowValues.Count - 1 do
      begin
        CellList := FFlowValues[TimeIndex];
        if LocalTimeToPlot < CellList.StartTime then
        begin
          continue
        end
        else
        begin
          if LocalTimeToPlot > CellList.EndTime then
          begin
            break;
          end;
        end;
  //      if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
  //        for TimeListIndex := 0 to TimeLists.Count - 1 do
          begin
            DisplayTimeList := TimeLists[TimeListIndex];

            for InnerTimeIndex := 0 to DisplayTimeList.Count - 1 do
            begin
              DataArray := DisplayTimeList[InnerTimeIndex]
                as TModflowBoundaryDisplayDataArray;
              DataArray.UpToDate := True;
            end;

            DataArray := DisplayTimeList[0]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
          end;
          UpdateCellDisplay(CellList, DataArrayList, []);
          DataArray.UpToDate := True;
          DisplayTimeList.SetUpToDate(True);
        end;
  //      break;
      end;

    finally
      DataArrayList.Free;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

  if frmErrorsAndWarnings.HasMessages then
  begin
    frmErrorsAndWarnings.Show;
  end;

end;

procedure TModflowFhbWriter.WriteDataSet1;
const
  IFHBSS = 0;
  NFHBX1 = 1;
  NFHBX2 = 1;
var
  NBDTIM: integer;
  Cells: TFhbCellList;
  IFHBCB: Integer;
  Index: Integer;
  StartTime: Double;
  TimeIndex: Integer;
begin
//  NBDTIM := FTimes.Count-1;
  NBDTIM := 0;
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
//  for TimeIndex := 0 to FTimes.Count - 2 do
  for TimeIndex := 0 to FTimes.Count - 1 do
  begin
    if FTimes[TimeIndex]-StartTime >= 0 then
    begin
      Inc(NBDTIM);
    end;
  end;

  if FFlowValues.Count > 0 then
  begin
    Cells := FFlowValues[0];
    NFLW := Cells.Count;
    for Index := 0 to FFlowValues.Count - 1 do
    begin
      Cells := FFlowValues[Index];
      NFLW := Max(NFLW, Cells.Count);
    end;
  end
  else
  begin
    NFLW := 0;
  end;

  if Values.Count > 0 then
  begin
    Cells := Values[0];
    NHED := Cells.Count;
    for Index := 0 to Values.Count - 1 do
    begin
      Cells := Values[Index];
      NHED := Max(NHED, Cells.Count);
    end;
  end
  else
  begin
    NHED := 0;
  end;

  GetFlowUnitNumber(IFHBCB);

  WriteInteger(NBDTIM);
  WriteInteger(NFLW);
  WriteInteger(NHED);
  WriteInteger(IFHBSS);
  WriteInteger(IFHBCB);
  WriteInteger(NFHBX1);
  WriteInteger(NFHBX2);
  WriteString(' # Data Set 1: NBDTIM NFLW NHED IFHBSS  IFHBCB NFHBX1 NFHBX2');
  NewLine;
end;

procedure TModflowFhbWriter.WriteDataSetHeader(const Comment: string);
const
  CNSTM = 1;
var
  IFHBUN: integer;
  IFHBPT: integer;
begin
  IFHBUN := FUnitNumber;
  if FPrintInputCellLists then
  begin
    IFHBPT := 1;
  end
  else
  begin
    IFHBPT := 0;
  end;
  WriteInteger(IFHBUN);
  WriteFloat(CNSTM);
  WriteInteger(IFHBPT);
  WriteString(Comment);
  NewLine;
end;

procedure TModflowFhbWriter.WriteDataSet4a;
begin
  WriteDataSetHeader(' # Data Set 4a: IFHBUN, CNSTM, IFHBPT');
end;

procedure TModflowFhbWriter.WriteDataSet4b;
var
  TimeIndex: Integer;
  StartTime: Double;
  TimeCount: Integer;
begin
  StartTime := Model.ModflowFullStressPeriods.First.StartTime;
//  for TimeIndex := 0 to FTimes.Count - 2 do
  TimeCount := 0;
  for TimeIndex := 0 to FTimes.Count - 1 do
  begin
    if FTimes[TimeIndex]-StartTime >= 0 then
    begin
      WriteFloat(FTimes[TimeIndex]-StartTime);
      if TimeIndex = FTimes.Count - 1 then
      begin
        WriteString(' # Data Set 4b: BDTIM');
      end;
      Inc(TimeCount);
      if (TimeCount mod 10) = 0 then
      begin
        NewLine;
      end;
    end;
  end;
  if (TimeCount mod 10) <> 0 then
  begin
    NewLine;
  end;
end;

procedure TModflowFhbWriter.WriteAuxiliaryValues(const HeaderComment,
  LineComment: string; ValuesToExport: TList);
var
  Cells: TFhbCellList;
  CellIndex: Integer;
  ACell: TFhb_Cell;
  TimeIndex: Integer;
  CellListIndex: Integer;
  CellStartIndex: Integer;
begin
  CellStartIndex := 0;
  Cells := nil;
  if ValuesToExport.Count > 0 then
  begin
    WriteDataSetHeader(HeaderComment);

    // Data Set 6b or 8b
    for CellListIndex := 0 to ValuesToExport.Count -1 do
    begin
      Cells := ValuesToExport[CellListIndex];
      if Cells.Count > 0 then
      begin
        CellStartIndex := CellListIndex;
        break;
      end;
    end;
    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TFhb_Cell;
      WriteIface(ACell.IFace);
      for TimeIndex := CellStartIndex+1 to FTimes.Count - 1 do
      begin
        WriteIface(ACell.IFace);
        if (((TimeIndex+1) mod 10) = 0)
          and (TimeIndex <> FTimes.Count - 1) then
        begin
          NewLine;
        end;
      end;
      WriteString(LineComment);
      NewLine;
    end;
  end
end;

procedure TModflowFhbWriter.WriteCellValues(const HeaderComment, LineComment: string;
  ValuesToExport: TList);
var
  Cells: TFhbCellList;
  TimeCells: TFhbCellList;
  CellIndex: Integer;
  ACell: TFhb_Cell;
  TimeIndex: Integer;
  TimeCell: TFhb_Cell;
  EndTime: Double;
  CellListIndex: Integer;
  CellStartIndex: Integer;
  PriorTimeCells: TFhbCellList;
begin
  CellStartIndex := 0;
  Cells := nil;
  if ValuesToExport.Count > 0 then
  begin
    EndTime := Model.ModflowFullStressPeriods.Last.EndTime;
    WriteDataSetHeader(HeaderComment);

    // Data Set 5b or 7b
    for CellListIndex := 0 to ValuesToExport.Count -1 do
    begin
      Cells := ValuesToExport[CellListIndex];
      if Cells.Count > 0 then
      begin
        CellStartIndex := CellListIndex;
        break;
      end;
    end;
    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACell := Cells[CellIndex] as TFhb_Cell;
      WriteInteger(ACell.Layer+1);
      WriteInteger(ACell.Row+1);
      WriteInteger(ACell.Column+1);
      WriteIface(ACell.IFace);
      WriteFloat(ACell.BoundaryValue);
      TimeCells := Cells;
      PriorTimeCells := Cells;
      for TimeIndex := CellStartIndex+1 to ValuesToExport.Count - 1 do
      begin
        TimeCells := ValuesToExport[TimeIndex];
        if (TimeCells.Count = 0) then
        begin
          TimeCells := PriorTimeCells;
        end;
        if (TimeIndex = ValuesToExport.Count - 1) and (TimeCells.Count = 0) then
        begin
          TimeCells := PriorTimeCells;
        end;
        PriorTimeCells := TimeCells;
//        if TimeCells.Count <> Cells.Count then
//        begin
//          ShowMessage('TimeCells.Count = ' + TimeCells.Count.ToString
//            + '; Cells.Count = ' + Cells.Count.ToString
//            + '; TimeIndex = ' + TimeIndex.ToString);
//        end;
        Assert(TimeCells.Count = Cells.Count);
        TimeCell := TimeCells[CellIndex] as TFhb_Cell;
        Assert(ACell.Layer = TimeCell.Layer);
        Assert(ACell.Row = TimeCell.Row);
        Assert(ACell.Column = TimeCell.Column);
        WriteFloat(TimeCell.BoundaryValue);
        if (((TimeIndex+1) mod 10) = 0)
          and (TimeIndex <> ValuesToExport.Count - 1) then
        begin
          NewLine;
        end;
      end;
      if TimeCells.StartTime <> EndTime then
      begin
        TimeCell := TimeCells[CellIndex] as TFhb_Cell;
        Assert(ACell.Layer = TimeCell.Layer);
        Assert(ACell.Row = TimeCell.Row);
        Assert(ACell.Column = TimeCell.Column);
        WriteFloat(TimeCell.BoundaryValue);
      end;
      WriteString(LineComment);
      NewLine;
    end;
  end
end;

procedure TModflowFhbWriter.WriteDataSet5;
begin
  WriteCellValues(' # Data Set 5a: IFHBUN, CNSTM, IFHBPT',
    ' # Data Set 5b: Layer Row Column IAUX  FLWRAT(NBDTIM)', FFlowValues);
end;

procedure TModflowFhbWriter.WriteDataSet6;
begin
  WriteAuxiliaryValues(' # Data Set 6a: IFHBUN, CNSTM, IFHBPT',
    ' # Data Set 6b: AuxVar(NBDTIM)', FFlowValues);
end;

procedure TModflowFhbWriter.WriteDataSet7;
begin
  WriteCellValues(' # Data Set 7a: IFHBUN, CNSTM, IFHBPT',
    ' # Data Set 7b: Layer Row Column IAUX  SBHED(NBDTIM)', Values);
end;

procedure TModflowFhbWriter.WriteDataSet8;
begin
  WriteAuxiliaryValues(' # Data Set 8a: IFHBUN, CNSTM, IFHBPT',
    ' # Data Set 8b: AuxVar(NBDTIM)', Values);
end;

procedure TModflowFhbWriter.WriteDataSets2and3;
begin
  WriteString('IFACE 0 # Data Set 2: VarName Weight');
  NewLine;

  WriteString('IFACE 0 # Data Set 3: VarName Weight');
  NewLine;
end;

procedure TModflowFhbWriter.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrFHB) then
  begin
    Exit;
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  FUnitNumber := Model.UnitNumbers.UnitNumber(StrFHB);
  WriteToNameFile(StrFHB, FUnitNumber, FNameOfFile, foInput, Model);

  Evaluate;

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage('Writing FHB Package input.');
    frmProgressMM.AddMessage('  Writing Data Set 1.');
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Sets 2 and 3.');
    WriteDataSets2and3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 4a.');
    WriteDataSet4a;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage('  Writing Data Set 4b.');
    WriteDataSet4b;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    if NFLW > 0 then
    begin
      frmProgressMM.AddMessage('  Writing Data Set 5.');
      WriteDataSet5;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage('  Writing Data Set 6.');
      WriteDataSet6;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

    if NHED > 0 then
    begin
      frmProgressMM.AddMessage('  Writing Data Set 7.');
      WriteDataSet7;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage('  Writing Data Set 8.');
      WriteDataSet8;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    end;

  finally
    CloseFile;
  end;

end;

end.
