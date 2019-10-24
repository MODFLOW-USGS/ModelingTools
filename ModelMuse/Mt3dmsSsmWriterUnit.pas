unit Mt3dmsSsmWriterUnit;

interface

uses System.UITypes,
  Winapi.Windows, CustomModflowWriterUnit, SysUtils, ScreenObjectUnit,
  ModflowPackageSelectionUnit, Forms, PhastModelUnit, Classes, IntListUnit,
  ModflowBoundaryDisplayUnit, ModflowCellUnit, Vcl.Dialogs;

type
  TMt3dmsSsmWriter = class(TCustomTransientWriter)
  private
    MXSS: Integer;
    FBoundaryCellsPerStressPeriod: TIntegerList;
    // After @link(Evaluate) is called,
    // @name contains a series of @link(TValueCellList)s;
    // one for each stress period.
    // Each such list contains series of @link(TValueCell)s. Each
    // @link(TValueCell) defines one boundary cell for one stress period.
    // @name is a TObjectList.
    FRechConValues: TList;
    FSeepConcValues: TList;
    procedure WriteBoundaryArrays(const FormatString: string;
      BoundaryID: integer; List: TValueCellList);
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    // recharge
    procedure WriteDataSets3and4(StressPeriod: Integer; List: TValueCellList);
    // Evapotranspiration via EVT or ETS packages
    procedure WriteDataSets5and6(StressPeriod: Integer; List: TValueCellList);
    procedure WriteUztDataSets7Through10(StressPeriod: Integer);
    // point sources, Data set 11 and 12 in MT3D-USGS
    procedure WriteDataSets7and8(StressPeriod: integer; List: TValueCellList);
    procedure WriteStressPeriods; reintroduce;
    Procedure WriteU2DREL_ConstantHeader;
    procedure EvaluateUzfRechStressData;
    procedure WriteUzfArrays(const FormatString: string; List: TValueCellList);
    procedure EvaluateUzfSeepageStressData;
  protected
    procedure CountCells(var MaximumNumberOfCells: Integer); override;
    procedure Evaluate; override;
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  GoPhastTypes, Mt3dmsChemUnit, frmProgressUnit, frmErrorsAndWarningsUnit,
  ModflowUnitNumbers, ModflowGridUnit, Mt3dmsChemSpeciesUnit,
  DataSetUnit, RbwParser, ModflowPackagesUnit, System.Contnrs, Mt3dUztRchUnit,
  Mt3dUzfSeepageUnit;

resourcestring
  StrUnspecifiedSSMData = 'Unspecified SSM data';
  StrRechageConc = 'The recharge or evapotranspiration concentration in the ' +
  'SSM package was not defined in any stress periods.';
  StrDataSet4CRCHSp = 'Data set 4: CRCH; Species: %s';
  StrDataSet6CEVTSp = 'Data set 4: CEVT; Species: %s';
  StrSSMPackageDeactiva = 'SSM package deactivated';
  StrTheSinkSourceMi = 'The Sink & Source Mixing Package (SSM) has not been ' +
  'included because the number of sources or sinks for the package is zero. ' +
  'You need to specify the source or sink concentration for the sources and ' +
  'sinks to be included.';
  StrEvaluatingSSMPacka = 'Evaluating SSM Package data.';
  StrEvaluatingS = '    Evaluating %s.';
  StrWritingRechar = '      Writing Recharge Concentration';
  StrWritingEvapot = '      Writing Evapotranspiration Concentration';
  StrWritingPoint = '      Writing Point Source Concentrations';
  StrWritingSSMPackage = 'Writing SSM Package input.';
//  StrWritingDataSet1 = '  Writing Data Set 1.';
//  StrWritingDataSet2 = '  Writing Data Set 2.';
  StrWritingStressP = '    Writing Stress Period %d';
  StrTransferringDat = '    Transferring Data for %s';
  StrBothEVTAndETSPac = 'Both EVT and ETS packages selected.';
  StrBothTheEvapotransp = 'Both the Evapotranspiration (EVT) and the Evapotr' +
  'anspirtion Segments (ETS) packages selected. You can only use one or the ' +
  'other of them with MT3DMS.';

{ TMt3dmsSsmWriter }

procedure TMt3dmsSsmWriter.CountCells(var MaximumNumberOfCells: Integer);
var
  StressPeriodIndex: Integer;
  List: TValueCellList;
  StressPeriodCellCount: Integer;
  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  ModflowLayerCount: integer;
  Grid: TModflowGrid;
  ColumnCount: integer;
  RowCount: integer;
  LayerCount: integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
  LakeID: TDataArray;
  LayerIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  FBoundaryCellsPerStressPeriod.Clear;
  FBoundaryCellsPerStressPeriod.Capacity := Values.Count;
  ModflowLayerCount := Model.ModflowLayerCount;
  Grid := Model.ModflowGrid;
  ColumnCount := Grid.ColumnCount;
  RowCount := Grid.RowCount;
  LayerCount := Grid.LayerCount;
  MaximumNumberOfCells := 0;
  for StressPeriodIndex := 0 to Values.Count - 1 do
  begin
    StressPeriodCellCount := 0;
    List := Values[StressPeriodIndex];
    for CellIndex := 0 to List.Count - 1 do
    begin
      ACell := List[CellIndex] as TMt3dmsConc_Cell;
      StressPeriodCellCount := StressPeriodCellCount
        + ACell.PointSinkCount(ColumnCount, RowCount, LayerCount,
        ModflowLayerCount, Model);
    end;
    FBoundaryCellsPerStressPeriod.Add(StressPeriodCellCount);
    List.Cache;
    if StressPeriodCellCount > MaximumNumberOfCells then
    begin
      MaximumNumberOfCells := StressPeriodCellCount;
    end;
  end;

  CellList := TCellAssignmentList.Create;
  try

    for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
    begin
      ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if not ScreenObject.UsedModels.UsesModel(Model) then
      begin
        Continue;
      end;
      if (ScreenObject.ModflowChdBoundary <> nil)
        and ScreenObject.ModflowChdBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (ScreenObject.ModflowGhbBoundary <> nil)
        and ScreenObject.ModflowGhbBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (ScreenObject.ModflowWellBoundary <> nil)
        and ScreenObject.ModflowWellBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (ScreenObject.ModflowRivBoundary <> nil)
        and ScreenObject.ModflowRivBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (ScreenObject.ModflowDrnBoundary <> nil)
        and ScreenObject.ModflowDrnBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowDrtBoundary <> nil)
        and ScreenObject.ModflowDrtBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowResBoundary <> nil)
        and ScreenObject.ModflowResBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowStrBoundary <> nil)
        and ScreenObject.ModflowStrBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowFhbHeadBoundary <> nil)
        and ScreenObject.ModflowFhbHeadBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowFhbFlowBoundary <> nil)
        and ScreenObject.ModflowFhbFlowBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowLakBoundary <> nil)
        and ScreenObject.ModflowLakBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count*5);
      end;
      if (Model.ModelSelection <> msModflow2015) and
        (ScreenObject.ModflowSfrBoundary <> nil)
        and ScreenObject.ModflowSfrBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign({Model.Grid,} '0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection = msModflow2015) and
        (ScreenObject.ModflowRchBoundary <> nil)
        and ScreenObject.ModflowRchBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
      if (Model.ModelSelection = msModflow2015) and
        (ScreenObject.ModflowETSBoundary <> nil)
        and ScreenObject.ModflowETSBoundary.Used then
      begin
        CellList.Clear;
        ScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        Inc(MaximumNumberOfCells, CellList.Count);
      end;
    end;
  finally
    CellList.Free;
  end;

  if (Model.ModelSelection = msModflow2015) and
    Model.ModflowPackages.LakPackage.IsSelected then
  begin
    LakeID := Model.DataArrayManager.GetDataSetByName(rsLakeID);
    for LayerIndex := 0 to LakeID.LayerCount - 1 do
    begin
      for RowIndex := 0 to LakeID.RowCount - 1 do
      begin
        for ColIndex := 0 to LakeID.ColumnCount - 1 do
        begin
          if LakeID.IntegerData[LayerIndex, RowIndex, ColIndex] > 0 then
          begin
            if (LayerIndex < LakeID.LayerCount - 1)
              and (LakeID.IntegerData[LayerIndex+1, RowIndex, ColIndex] > 0)  then
            begin
              Inc(MaximumNumberOfCells);
            end;
            if (RowIndex > 0)
              and (LakeID.IntegerData[LayerIndex, RowIndex-1, ColIndex] = 0) then
            begin
              Inc(MaximumNumberOfCells);
            end;
            if (RowIndex < LakeID.RowCount - 1)
              and (LakeID.IntegerData[LayerIndex, RowIndex+1, ColIndex] = 0) then
            begin
              Inc(MaximumNumberOfCells);
            end;
            if (ColIndex > 0)
              and (LakeID.IntegerData[LayerIndex, RowIndex, ColIndex-1] = 0) then
            begin
              Inc(MaximumNumberOfCells);
            end;
            if (ColIndex < LakeID.ColumnCount - 1)
              and (LakeID.IntegerData[LayerIndex, RowIndex, ColIndex+1] = 0) then
            begin
              Inc(MaximumNumberOfCells);
            end;
          end;
        end;
      end;
    end;

  end;

  if (Model is TChildModel) and (Model.ModelSelection = msModflowLGR) then
  begin
    Inc(MaximumNumberOfCells, Grid.ColumnCount * Grid.LayerCount * 2);
    Inc(MaximumNumberOfCells, (Grid.RowCount -2) * Grid.LayerCount * 2);
    Inc(MaximumNumberOfCells, (Grid.RowCount -2) * (Grid.ColumnCount -2));
  end;

end;

constructor TMt3dmsSsmWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FArrayWritingFormat := awfMt3dms;
  FBoundaryCellsPerStressPeriod := TIntegerList.Create;
  FRechConValues := TObjectList.Create;
  FSeepConcValues := TObjectList.Create;
end;

destructor TMt3dmsSsmWriter.Destroy;
begin
  FSeepConcValues.Free;
  FRechConValues.Free;
  FBoundaryCellsPerStressPeriod.Free;
  inherited;
end;

procedure TMt3dmsSsmWriter.Evaluate;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  Boundary: TMt3dmsConcBoundary;
  NoAssignmentErrorRoot: string;
  BoundaryList: TList;
  BoundaryIndex: Integer;
  ValueIndex: Integer;
  Cells: TValueCellList;
begin
  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio,
    [Package.PackageIdentifier]);
  frmProgressMM.AddMessage(StrEvaluatingSSMPacka);
  frmErrorsAndWarnings.BeginUpdate;
  try
    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrBothEVTAndETSPac);

    BoundaryList := TList.Create;
    try
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        Boundary := ScreenObject.Mt3dmsConcBoundary;
        if Boundary <> nil then
        begin
          frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
          Application.ProcessMessages;
          if not ScreenObject.SetValuesOfEnclosedCells
            and not ScreenObject.SetValuesOfIntersectedCells then
          begin
            frmErrorsAndWarnings.AddError(Model,
              NoAssignmentErrorRoot, ScreenObject.Name, ScreenObject);
          end;
          Boundary.GetCellValues(Values, nil, Model);
          BoundaryList.Add(Boundary);
        end;
      end;
      Application.ProcessMessages;
      for BoundaryIndex := 0 to BoundaryList.Count - 1 do
      begin
        Boundary := BoundaryList[BoundaryIndex];
        ScreenObject := Boundary.ScreenObject as TScreenObject;
        frmProgressMM.AddMessage(Format(StrTransferringDat, [ScreenObject.Name]));
        Boundary.BoundaryAssignCells(Model,Values);
      end;
      for ValueIndex := 0 to Values.Count - 1 do
      begin
        Cells := Values[ValueIndex];
        Cells.Cache;
      end;
    finally
      BoundaryList.Free;
    end;
    CountCells(MXSS);
    EvaluateUzfRechStressData;
    EvaluateUzfSeepageStressData;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TMt3dmsSsmWriter.EvaluateUzfRechStressData;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
//  Boundary: TMt3dmsConcBoundary;
  NoAssignmentErrorRoot: string;
  RechBoundaryList: TList;
  BoundaryIndex: Integer;
  ValueIndex: Integer;
  Cells: TValueCellList;
  Boundary: TMt3dUztRchConcBoundary;
begin
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUSGS then
  begin
    Exit;
  end;
  if not Model.ModflowPackages.UzfPackage.IsSelected then
  begin
    Exit;
  end;

//  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio,
//    [Package.PackageIdentifier]);
//  frmProgressMM.AddMessage(StrEvaluatingSSMPacka);
//  frmErrorsAndWarnings.BeginUpdate;
//  try
//    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
//    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrBothEVTAndETSPac);

    RechBoundaryList := TList.Create;
    try
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        Boundary := ScreenObject.Mt3dUzfRechConc;
        if Boundary <> nil then
        begin
          frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
          Application.ProcessMessages;
          if not ScreenObject.SetValuesOfEnclosedCells
            and not ScreenObject.SetValuesOfIntersectedCells then
          begin
            frmErrorsAndWarnings.AddError(Model,
              NoAssignmentErrorRoot, ScreenObject.Name, ScreenObject);
          end;
          Boundary.GetCellValues(FRechConValues, nil, Model);
          RechBoundaryList.Add(Boundary);
        end;
      end;
      Application.ProcessMessages;
      for BoundaryIndex := 0 to RechBoundaryList.Count - 1 do
      begin
        Boundary := RechBoundaryList[BoundaryIndex];
        ScreenObject := Boundary.ScreenObject as TScreenObject;
        frmProgressMM.AddMessage(Format(StrTransferringDat, [ScreenObject.Name]));
        Boundary.BoundaryAssignCells(Model,FRechConValues);
      end;
      for ValueIndex := 0 to FRechConValues.Count - 1 do
      begin
        Cells := FRechConValues[ValueIndex];
        Cells.Cache;
      end;
    finally
      RechBoundaryList.Free;
    end;
//    CountCells(MXSS);
//  finally
//    frmErrorsAndWarnings.EndUpdate;
//  end;
end;

procedure TMt3dmsSsmWriter.EvaluateUzfSeepageStressData;
var
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  NoAssignmentErrorRoot: string;
  RechBoundaryList: TList;
  BoundaryIndex: Integer;
  ValueIndex: Integer;
  Cells: TValueCellList;
  Boundary: TMt3dUzSsmSinkConcBoundary;
//  Boundary: TMt3dUztRchConcBoundary;
begin
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUSGS then
  begin
    Exit;
  end;
  if not Model.ModflowPackages.UzfPackage.IsSelected then
  begin
    Exit;
  end;

//  NoAssignmentErrorRoot := Format(StrNoBoundaryConditio,
//    [Package.PackageIdentifier]);
//  frmProgressMM.AddMessage(StrEvaluatingSSMPacka);
//  frmErrorsAndWarnings.BeginUpdate;
//  try
//    frmErrorsAndWarnings.RemoveErrorGroup(Model, NoAssignmentErrorRoot);
//    frmErrorsAndWarnings.RemoveErrorGroup(Model, StrBothEVTAndETSPac);

    RechBoundaryList := TList.Create;
    try
      for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
      begin
        ScreenObject := Model.ScreenObjects[ScreenObjectIndex];
        if ScreenObject.Deleted then
        begin
          Continue;
        end;
        if not ScreenObject.UsedModels.UsesModel(Model) then
        begin
          Continue;
        end;
        Boundary := ScreenObject.Mt3dUzSsmSinkConcBoundary;
        if Boundary <> nil then
        begin
          frmProgressMM.AddMessage(Format(StrEvaluatingS, [ScreenObject.Name]));
          Application.ProcessMessages;
          if not ScreenObject.SetValuesOfEnclosedCells
            and not ScreenObject.SetValuesOfIntersectedCells then
          begin
            frmErrorsAndWarnings.AddError(Model,
              NoAssignmentErrorRoot, ScreenObject.Name, ScreenObject);
          end;
          Boundary.GetCellValues(FSeepConcValues, nil, Model);
          RechBoundaryList.Add(Boundary);
        end;
      end;
      Application.ProcessMessages;
      for BoundaryIndex := 0 to RechBoundaryList.Count - 1 do
      begin
        Boundary := RechBoundaryList[BoundaryIndex];
        ScreenObject := Boundary.ScreenObject as TScreenObject;
        frmProgressMM.AddMessage(Format(StrTransferringDat, [ScreenObject.Name]));
        Boundary.BoundaryAssignCells(Model,FSeepConcValues);
      end;
      for ValueIndex := 0 to FSeepConcValues.Count - 1 do
      begin
        Cells := FSeepConcValues[ValueIndex];
        Cells.Cache;
      end;
    finally
      RechBoundaryList.Free;
    end;
//    CountCells(MXSS);
//  finally
//    frmErrorsAndWarnings.EndUpdate;
//  end;
end;

class function TMt3dmsSsmWriter.Extension: string;
begin
  result := '.ssm';
end;

function TMt3dmsSsmWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mt3dmsSourceSink;
end;

procedure TMt3dmsSsmWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  ConcentrationsTimes: TModflowBoundaryDisplayTimeList;
  RechargeTimes: TModflowBoundaryDisplayTimeList;
  SinkTimes: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  ConcentrationArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
  Index: Integer;
  ATimeList: TModflowBoundaryDisplayTimeList;
  DataArrayList: TList;
  ParameterIndicies: TByteSet;
  DataArray: TModflowBoundaryDisplayDataArray;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  ConcentrationsTimes := TimeLists[0];
  RechargeTimes := TimeLists[1];
  SinkTimes := TimeLists[2];

  if (Values.Count = 0)
    and (FRechConValues.Count = 0)
    and (FSeepConcValues.Count = 0) then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;

  try
    ParameterIndicies := [0];
    DataArrayList := TList.Create;
    try
      for TimeIndex := 0 to Values.Count - 1 do
      begin
        ConcentrationArray := ConcentrationsTimes[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        CellList := Values[TimeIndex];

        if CellList.Count > 0 then
        begin
          DataArrayList.Clear;
          DataArray := ConcentrationsTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          DataArrayList.Add(DataArray);
          UpdateCellDisplay(CellList, DataArrayList, ParameterIndicies);
        end;

        ConcentrationArray.UpToDate := True;
        ConcentrationArray.CacheData;
      end;

      if RechargeTimes <> nil then
      begin
        for TimeIndex := 0 to FRechConValues.Count - 1 do
        begin
          ConcentrationArray := RechargeTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          CellList := FRechConValues[TimeIndex];

          if CellList.Count > 0 then
          begin
            DataArrayList.Clear;
            DataArray := RechargeTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
            UpdateCellDisplay(CellList, DataArrayList, ParameterIndicies);
          end;

          ConcentrationArray.UpToDate := True;
          ConcentrationArray.CacheData;
        end;
      end;

      if SinkTimes <> nil then
      begin
        for TimeIndex := 0 to FSeepConcValues.Count - 1 do
        begin
          ConcentrationArray := SinkTimes[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          CellList := FSeepConcValues[TimeIndex];

          if CellList.Count > 0 then
          begin
            DataArrayList.Clear;
            DataArray := SinkTimes[TimeIndex]
              as TModflowBoundaryDisplayDataArray;
            DataArrayList.Add(DataArray);
            UpdateCellDisplay(CellList, DataArrayList, ParameterIndicies);
          end;

          ConcentrationArray.UpToDate := True;
          ConcentrationArray.CacheData;
        end;
      end;

    finally
      DataArrayList.Free;
    end;

    for Index := 0 to TimeLists.Count - 1 do
    begin
      ATimeList := TimeLists[Index];
      if ATimeList <> nil then
      begin
        ATimeList.SetUpToDate(True);
      end;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;
end;

procedure TMt3dmsSsmWriter.WriteDataSet2;
var
  ISSGOUT: Integer;
begin
  // MNW wells are not current supported so ISSGOUT is set to zero.
  ISSGOUT := 0;
  WriteI10Integer(MXSS, 'SSM package, MXSS');
  WriteI10Integer(ISSGOUT, 'SSM package, ISSGOUT');
  WriteString(' # Data Set 2: MXSS ISSGOUT');
  NewLine;
end;

procedure TMt3dmsSsmWriter.WriteUzfArrays(const FormatString: string;
  List: TValueCellList);
var
  Grid: TModflowGrid;
  ComponentCount: Integer;
  ComponentList: TList;
  ComponentIndex: Integer;
  Concentration: array of array of array of double;

  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  ChemItem: TChemSpeciesItem;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLineNeeded: Boolean;
//  AllZero: Boolean;
//  ComponentList: TList;
begin
  Grid := Model.ModflowGrid;
  ComponentCount := Model.NumberOfMt3dChemComponents;

  // Get concentration
  SetLength(Concentration, ComponentCount, Grid.RowCount,
    Grid.ColumnCount);

  for ComponentIndex := 0 to ComponentCount - 1 do
  begin
    for RowIndex := 0 to Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Grid.ColumnCount - 1 do
      begin
        Concentration[ComponentIndex, RowIndex, ColIndex] := 0;
      end;
    end;
  end;

  if List <> nil then
  begin
    for CellIndex := 0 to List.Count - 1 do
    begin
      ACell := List[CellIndex] as TMt3dmsConc_Cell;
  //    if ACell.BoundaryTypes.IndexOf(BoundaryID) >= 0 then
      begin
        for ComponentIndex := 0 to ComponentCount - 1 do
        begin
          Concentration[ComponentIndex, ACell.Row, ACell.Column]
            := ACell.Concentration[ComponentIndex];
        end;
      end;
    end;
  end;
//  List.Cache;

  ComponentList := TList.Create;
  try
    ComponentList.Capacity := ComponentCount;
    for ComponentIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ComponentList.Add(Model.MobileComponents[ComponentIndex])
    end;
    for ComponentIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      ComponentList.Add(Model.ImmobileComponents[ComponentIndex])
    end;
    // Data set 4
    for ComponentIndex := 0 to ComponentCount - 1 do
    begin
      ChemItem := ComponentList[ComponentIndex];
      WriteU2DRELHeader(Format(FormatString, [ChemItem.Name]), matStructured,
        ChemItem.Name);
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        NewLineNeeded := True;
        for ColIndex := 0 to Grid.ColumnCount - 1 do
        begin
          WriteFloat(Concentration[ComponentIndex, RowIndex, ColIndex]);
          if ((ColIndex+1) mod 10) = 0 then
          begin
            NewLine;
            NewLineNeeded := False;
          end
          else
          begin
            NewLineNeeded := True;
          end;
        end;
        if NewLineNeeded then
        begin
          NewLine
        end;
      end;
    end
  finally
    ComponentList.Free;
  end;
end;

procedure TMt3dmsSsmWriter.WriteBoundaryArrays(const FormatString: string;
  BoundaryID: integer; List: TValueCellList);
var
  Grid: TModflowGrid;
  ComponentCount: Integer;
  ComponentList: TList;
  ComponentIndex: Integer;
  Concentration: array of array of array of double;

  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  ChemItem: TChemSpeciesItem;
  RowIndex: Integer;
  ColIndex: Integer;
  NewLineNeeded: Boolean;
//  AllZero: Boolean;
//  ComponentList: TList;
begin
  Grid := Model.ModflowGrid;
  ComponentCount := Model.NumberOfMt3dChemComponents;

  // Get concentration
  SetLength(Concentration, ComponentCount, Grid.RowCount,
    Grid.ColumnCount);

  for ComponentIndex := 0 to ComponentCount - 1 do
  begin
    for RowIndex := 0 to Grid.RowCount - 1 do
    begin
      for ColIndex := 0 to Grid.ColumnCount - 1 do
      begin
        Concentration[ComponentIndex, RowIndex, ColIndex] := 0;
      end;
    end;
  end;

  for CellIndex := 0 to List.Count - 1 do
  begin
    ACell := List[CellIndex] as TMt3dmsConc_Cell;
    if ACell.BoundaryTypes.IndexOf(BoundaryID) >= 0 then
    begin
      for ComponentIndex := 0 to ComponentCount - 1 do
      begin
        Concentration[ComponentIndex, ACell.Row, ACell.Column]
          := ACell.Concentration[ComponentIndex];
      end;
    end;
  end;
//  List.Cache;

  ComponentList := TList.Create;
  try
    ComponentList.Capacity := ComponentCount;
    for ComponentIndex := 0 to Model.MobileComponents.Count - 1 do
    begin
      ComponentList.Add(Model.MobileComponents[ComponentIndex])
    end;
    for ComponentIndex := 0 to Model.ImmobileComponents.Count - 1 do
    begin
      ComponentList.Add(Model.ImmobileComponents[ComponentIndex])
    end;
    // Data set 4
    for ComponentIndex := 0 to ComponentCount - 1 do
    begin
      ChemItem := ComponentList[ComponentIndex];
      WriteU2DRELHeader(Format(FormatString, [ChemItem.Name]), matStructured,
        ChemItem.Name);
      for RowIndex := 0 to Grid.RowCount - 1 do
      begin
        NewLineNeeded := True;
        for ColIndex := 0 to Grid.ColumnCount - 1 do
        begin
          WriteFloat(Concentration[ComponentIndex, RowIndex, ColIndex]);
          if ((ColIndex+1) mod 10) = 0 then
          begin
            NewLine;
            NewLineNeeded := False;
          end
          else
          begin
            NewLineNeeded := True;
          end;
        end;
        if NewLineNeeded then
        begin
          NewLine
        end;
      end;
    end
  finally
    ComponentList.Free;
  end;
end;

procedure TMt3dmsSsmWriter.WriteDataSets3and4(StressPeriod: Integer; List: TValueCellList);
var
  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  ComponentIndex: Integer;
  AllZero: Boolean;
  ComponentCount: Integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    // In MODFLOW 6, recharge is treated as a point source
    Exit;
  end;
  if Model.ModflowPackages.RchPackage.IsSelected then
  begin
    AllZero := True;
    ComponentCount := Model.NumberOfMt3dChemComponents;
    for CellIndex := 0 to List.Count - 1 do
    begin
      ACell := List[CellIndex] as TMt3dmsConc_Cell;
      if ACell.BoundaryTypes.IndexOf(ISSTYPE_RCH) >= 0 then
      begin
        for ComponentIndex := 0 to ComponentCount - 1 do
        begin
          if ACell.Concentration[ComponentIndex] <> 0 then
          begin
            AllZero := False;
            Break;
          end;
        end;
      end;
    end;

    frmProgressMM.AddMessage(StrWritingRechar);
    // Data set 3
    WriteI10Integer(1, 'SSM package, INCRCH');
    WriteString(' # Data set 3: INCRCH; Stress Period ' + IntToStr(StressPeriod+1));
    NewLine;
    if AllZero then
    begin
      for ComponentIndex := 0 to ComponentCount - 1 do
      begin
        WriteU2DREL_ConstantHeader;
      end;
    end
    else
    begin
      // data set 4.
      WriteBoundaryArrays(StrDataSet4CRCHSp, ISSTYPE_RCH, List);
    end;
  end;
end;

procedure TMt3dmsSsmWriter.WriteDataSets5and6(StressPeriod: Integer; List: TValueCellList);
var
  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  ComponentIndex: Integer;
  AllZero: Boolean;
  ComponentCount: Integer;
  ISSTYPE: integer;
begin
  if Model.ModelSelection = msModflow2015 then
  begin
    // In MODFLOW 6, evapotranspiration is treated as a point source
    Exit;
  end;
  if Model.ModflowPackages.EvtPackage.IsSelected
    and Model.ModflowPackages.EtsPackage.IsSelected then
  begin
    frmErrorsAndWarnings.AddError(Model, StrBothEVTAndETSPac,
      StrBothTheEvapotransp );
  end;
  ISSTYPE := 0;
  if Model.ModflowPackages.EvtPackage.IsSelected then
  begin
    ISSTYPE := ISSTYPE_EVT;
  end
  else if Model.ModflowPackages.EtsPackage.IsSelected then
  begin
    ISSTYPE := ISSTYPE_ETS;
  end;
  if ISSTYPE = 0 then
  begin
    Exit;
  end
  else
  begin
    AllZero := True;
    ComponentCount := Model.NumberOfMt3dChemComponents;
    for CellIndex := 0 to List.Count - 1 do
    begin
      ACell := List[CellIndex] as TMt3dmsConc_Cell;
      if ACell.BoundaryTypes.IndexOf(ISSTYPE) >= 0 then
      begin
        for ComponentIndex := 0 to ComponentCount - 1 do
        begin
          if ACell.Concentration[ComponentIndex] <> 0 then
          begin
            AllZero := False;
            Break;
          end;
        end;
      end;
    end;

    frmProgressMM.AddMessage(StrWritingEvapot);
    // Data set 5
    WriteI10Integer(1, 'SSM package, INCEVT');
    WriteString(' # Data set 5: INCEVT; Stress Period ' + IntToStr(StressPeriod+1));
    NewLine;
    if AllZero then
    begin
      for ComponentIndex := 0 to ComponentCount - 1 do
      begin
        WriteU2DREL_ConstantHeader;
      end;
    end
    else
    begin
      // data set 6.
      WriteBoundaryArrays(StrDataSet6CEVTSp, ISSTYPE, List);
    end;
  end;
end;

procedure TMt3dmsSsmWriter.WriteDataSets7and8(StressPeriod: integer; List: TValueCellList);
const
  DS8 = ' # Data Set 8: KSS, ISS, JSS, CSS, ISSTYPE, [CSSMS(n), n=1, NCOMP]';
  DS12 = ' # Data Set 12: KSS, ISS, JSS, CSS, ISSTYPE, [CSSMS(n), n=1, NCOMP]';
var
  NSS: Integer;
  ComponentCount: Integer;
  CellIndex: Integer;
  ACell: TMt3dmsConc_Cell;
  LayerCount: Integer;
  BoundaryIndex: Integer;
  BoundaryID: Integer;
  LayerIndex: Integer;
  SpeciesIndex: Integer;
  LocalLayer: Integer;
  Grid: TModflowGrid;
  ActiveDataArray: TDataArray;
  TestLayer: Integer;
  ScreenIndex: Integer;
  Mt3dVersion: TMt3dVersion;
begin
  frmProgressMM.AddMessage(StrWritingPoint);
  Application.ProcessMessages;
  // Data set 7 in MT3DMS, Data set 11 in MT3D-USGS
  Mt3dVersion := Model.ModflowPackages.Mt3dBasic.Mt3dVersion;
  NSS := FBoundaryCellsPerStressPeriod[StressPeriod];
  WriteI10Integer(NSS, 'SSM package, NSS');
  case Mt3dVersion of
    mvUSGS:
      begin
        WriteString(' # Data set 11: NSS; Stress Period ' + IntToStr(StressPeriod+1));
      end;
    mvMS:
      begin
        WriteString(' # Data set 7: NSS; Stress Period ' + IntToStr(StressPeriod+1));
      end;
    else
      Assert(False);
  end;
  NewLine;
  if NSS > 0 then
  begin
    // data set 8 in MT3DMS, data set 12 in MT3D-USGS
    Grid := Model.ModflowGrid;
    LayerCount := Model.ModflowLayerCount;
    ComponentCount := Model.NumberOfMt3dChemComponents;
    List := Values[StressPeriod];
    for CellIndex := 0 to List.Count - 1 do
    begin
      ACell := List[CellIndex] as TMt3dmsConc_Cell;
      for BoundaryIndex := 0 to ACell.BoundaryTypes.Count - 1 do
      begin
        BoundaryID := ACell.BoundaryTypes[BoundaryIndex];
        {$IFNDEF Mt3dUSGS}
        if BoundaryID in [ISSTYPE_RCH, ISSTYPE_EVT, ISSTYPE_ETS] then
        begin
          Continue;
        end;
        {$ENDIF}
        if (BoundaryID = ISSTYPE_MNW) and (ACell.Mnw2Layers.Count > 0) then
        begin
          for ScreenIndex := 0 to ACell.Mnw2Layers.Count-1 do
          begin
            LocalLayer := ACell.Mnw2Layers[ScreenIndex];
            WriteI10Integer(LocalLayer, 'SSM package, KSS');
            WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
            WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
          Continue;
        end;
        if BoundaryID = ISSTYPE_ETS then
        begin
          for LayerIndex := 1 to LayerCount do
          begin
            WriteI10Integer(LayerIndex, 'SSM package, KSS');
            WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
            WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
        end
        else if BoundaryID = ISSTYPE_LAK then
        begin
          ActiveDataArray := Model.DataArrayManager.GetDataSetByName(rsActive);
          ActiveDataArray.Initialize;
          LocalLayer := Model.
            DataSetLayerToModflowLayer(ACell.Layer);
          if LocalLayer < LayerCount then
          begin
            TestLayer := ACell.Layer+1;
            if not Model.IsLayerSimulated(TestLayer) then
            begin
              Inc(TestLayer);
            end;
            if ActiveDataArray.BooleanData[TestLayer, ACell.Row, ACell.Column] then
            begin
              WriteI10Integer(LocalLayer+1, 'SSM package, KSS');
              WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
              WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
              WriteF10Float(ACell.Concentration[0]);
              WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
              for SpeciesIndex := 0 to ComponentCount - 1 do
              begin
                WriteF10Float(ACell.Concentration[SpeciesIndex]);
              end;
              case Mt3dVersion of
                mvUSGS:
                  begin
                    WriteString(DS12);
                  end;
                mvMS:
                  begin
                    WriteString(DS8);
                  end;
                else
                  Assert(False);
              end;
              NewLine;
            end;
          end;
          if (ACell.Column > 0)
            and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, ACell.Column-1] then
          begin
            WriteI10Integer(LocalLayer, 'SSM package, KSS');
            WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
            WriteI10Integer(ACell.Column, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
          if (ACell.Column+1 < Grid.ColumnCount)
            and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row, ACell.Column+1] then
          begin
            WriteI10Integer(LocalLayer, 'SSM package, KSS');
            WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
            WriteI10Integer(ACell.Column+2, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
          if (ACell.Row > 0)
            and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row-1, ACell.Column] then
          begin
            WriteI10Integer(LocalLayer, 'SSM package, KSS');
            WriteI10Integer(ACell.Row, 'SSM package, ISS');
            WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
          if (ACell.Row+1 < Grid.RowCount)
            and ActiveDataArray.BooleanData[ACell.Layer, ACell.Row+1, ACell.Column] then
          begin
            WriteI10Integer(LocalLayer, 'SSM package, KSS');
            WriteI10Integer(ACell.Row+2, 'SSM package, ISS');
            WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
            WriteF10Float(ACell.Concentration[0]);
            WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
            for SpeciesIndex := 0 to ComponentCount - 1 do
            begin
              WriteF10Float(ACell.Concentration[SpeciesIndex]);
            end;
            case Mt3dVersion of
              mvUSGS:
                begin
                  WriteString(DS12);
                end;
              mvMS:
                begin
                  WriteString(DS8);
                end;
              else
                Assert(False);
            end;
            NewLine;
          end;
        end
        else
        begin
          LocalLayer := Model.
            DataSetLayerToModflowLayer(ACell.Layer);
          WriteI10Integer(LocalLayer, 'SSM package, KSS');
          WriteI10Integer(ACell.Row+1, 'SSM package, ISS');
          WriteI10Integer(ACell.Column+1, 'SSM package, JSS');
          WriteF10Float(ACell.Concentration[0]);
          WriteI10Integer(BoundaryID, 'SSM package, ITYPE');
          for SpeciesIndex := 0 to ComponentCount - 1 do
          begin
            WriteF10Float(ACell.Concentration[SpeciesIndex]);
          end;
          case Mt3dVersion of
            mvUSGS:
              begin
                WriteString(DS12);
              end;
            mvMS:
              begin
                WriteString(DS8);
              end;
            else
              Assert(False);
          end;
          NewLine;
        end;
      end;
    end;
//    List.Cache;
  end;
end;

procedure TMt3dmsSsmWriter.WriteDataSet1;
var
  SsmPkg: TMt3dmsSourceSinkMixing;
  ALine: string;
begin
  SsmPkg := Model.ModflowPackages.Mt3dmsSourceSink;
  if SsmPkg.Comments.Count > 0 then
  begin
    ALine := SsmPkg.Comments[0];
    WriteString(ALine);
    NewLine;
  end
  else
  begin
    WriteCommentLine(PackageID_Comment(SsmPkg));
  end;
end;

procedure TMt3dmsSsmWriter.WriteFile(const AFileName: string);
var
  NameOfFile: string;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrUnspecifiedSSMData);

  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrSSMPackageDeactiva);
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(StrSSM) then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingSSMPackage);
  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  if MXSS = 0 then
  begin
    frmErrorsAndWarnings.AddWarning(Model, StrSSMPackageDeactiva,
      StrTheSinkSourceMi);
  end;

  NameOfFile := FileName(AFileName);
  WriteToMt3dMsNameFile(StrSSM, Mt3dSSM,
    NameOfFile, foInput, Model);
//  WriteToNameFile(StrUZF, Model.UnitNumbers.UnitNumber(StrUZF), NameOfFile, foInput);
  OpenFile(NameOfFile);
  try

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteStressPeriods
  finally
    CloseFile;
  end;


end;

procedure TMt3dmsSsmWriter.WriteStressPeriods;
var
  TimeIndex: Integer;
  NSS: Integer;
  Packages: TModflowPackages;
  List: TValueCellList;
begin
  if Values.Count = 0 then
  begin
    Packages := Model.ModflowPackages;

    if Packages.RchPackage.IsSelected or Packages.EvtPackage.IsSelected then
    begin
      frmErrorsAndWarnings.AddError(Model, StrUnspecifiedSSMData,
        StrRechageConc);
    end;
    for TimeIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
    begin
      WriteUztDataSets7Through10(TimeIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      NSS := 0;
      WriteI10Integer(NSS, 'SSM package, NSS');
      WriteString(' # Data set 7: NSS');
      NewLine;

    end;
  end;

  for TimeIndex := 0 to Values.Count - 1 do
  begin
    frmProgressMM.AddMessage(Format(StrWritingStressP, [TimeIndex+1]));

    List := Values[TimeIndex];
    try
      WriteDataSets3and4(TimeIndex, List);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteDataSets5and6(TimeIndex, List);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteUztDataSets7Through10(TimeIndex);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      WriteDataSets7and8(TimeIndex, List);
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    finally
      List.Cache;
    end;
  end;
end;

procedure TMt3dmsSsmWriter.WriteU2DREL_ConstantHeader;
begin
  WriteString('         0        0.                    ');
  WriteString(FixedFormattedInteger(IPRN_Real, 10));
  NewLine;
end;

procedure TMt3dmsSsmWriter.WriteUztDataSets7Through10(StressPeriod: Integer);
var
  INCUZF: Integer;
  INCGWET: Integer;
  List: TValueCellList;
begin
  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUSGS then
  begin
    Exit;
  end;
  if not Model.ModflowPackages.UzfPackage.IsSelected then
  begin
    Exit;
  end;

  if Model.ModflowPackages.Mt3dUnsatTransport.IsSelected then
  begin
    Exit;
  end;

  if FRechConValues.Count <> 0 then
  begin
    List := FRechConValues[StressPeriod];
    if List.Count > 0 then
    begin
      INCUZF := 1;
    end
    else
    begin
      INCUZF := -1;
    end;
    WriteI10Integer(INCUZF, 'SSM package, INCUZF');
    WriteString(' # INCUZF Stress period ');
    WriteInteger(StressPeriod+1);
    NewLine;

    if INCUZF >= 0 then
    begin
      WriteUzfArrays('Data set 8: CRCH; Species: %s', List)
    end;
  end
  else
  begin
    if StressPeriod = 0 then
    begin
      INCUZF := 1;
    end
    else
    begin
      INCUZF := -1;
    end;
    WriteI10Integer(INCUZF, 'SSM package, INCUZF');
    WriteString(' # INCUZF Stress period ');
    WriteInteger(StressPeriod+1);
    NewLine;

    if INCUZF >= 0 then
    begin
      WriteUzfArrays('Data set 8: CRCH; Species: %s', nil)
    end;
  end;

  if FSeepConcValues.Count <> 0 then
  begin
    List := FSeepConcValues[StressPeriod];
    if List.Count > 0 then
    begin
      INCGWET := 1;
    end
    else
    begin
      INCGWET := -1;
    end;
    WriteI10Integer(INCGWET, 'SSM package, INCGWET');
    WriteString(' # INCGWET Stress period ');
    WriteInteger(StressPeriod+1);
    NewLine;

    if INCGWET >= 0 then
    begin
      WriteUzfArrays('Data set 10: CGWET; Species: %s', List)
    end;
  end
  else
  begin
    if StressPeriod = 0 then
    begin
      INCGWET := 1;
    end
    else
    begin
      INCGWET := -1;
    end;
    WriteI10Integer(INCGWET, 'SSM package, INCGWET');
    WriteString(' # INCGWET Stress period ');
    WriteInteger(StressPeriod+1);
    NewLine;

    if INCGWET >= 0 then
    begin
      WriteUzfArrays('Data set 10: CGWET; Species: %s', nil)
    end;
  end;

end;

end.
