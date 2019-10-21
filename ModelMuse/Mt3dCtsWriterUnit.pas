unit Mt3dCtsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, Mt3dCtsSystemUnit, System.Classes,
  ModflowPackageSelectionUnit, System.Generics.Collections, ScreenObjectUnit,
  RbwParser;

type
  TCstSystemList = TList<TCtsSystem>;

  TMt3dCtsWriter = class(TCustomModflowWriter)
  private
    FCtsSystems: TCtsSystemCollection;
    FExtractionWells: TListOfObjects;
    FInjectionWells: TListOfObjects;
    FAllWells: TListOfObjects;
    FWellCellCounts: TList<Integer>;
    FUsedWellCounts: TList<Integer>;
    FMt3dCts: TMt3dCtsPackageSelection;
    FWellCountDictionary: TDictionary<TScreenObject, Integer>;
    FUsedWellCountDictionary: TDictionary<TScreenObject, Integer>;
    FUsedWelStartsDictionary: TDictionary<TScreenObject, Integer>;
    FParser: TRbwParser;
    procedure WriteDataSet1;
    procedure WriteStressPeriods;
    procedure WriteASystem(ACstSystem: TCtsSystem; StartTime: Double;
      SystemIndex, StressPeriodIndex: integer);
    procedure WriteDataSet4(CtsObjects: TCtsObjectItem);
    procedure WriteDataSet5(ACstSystem: TCtsSystem; StartTime: Double;
      StressPeriodIndex: Integer);
    procedure WriteDataSet6(InjectionTimeOption: TCtsInjectionTimeItem);

    procedure GetUsedWells;
    procedure Evaluate;
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel;
      EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
  end;

implementation

uses
  ModflowUnitNumbers, GoPhastTypes, ModflowTimeUnit, ModflowMnw2Unit,
  ModflowWellUnit, ModflowBoundaryUnit, GIS_Functions, frmFormulaErrorsUnit,
  System.SysUtils;

resourcestring
  StrErrorEvaluatingInf = 'Error evaluating Inflow in Contaminanent treatmen' +
  't system %0:s in stress period %1:d at time %2:g';
  StrErrorEvaluatingInfConc = 'Error evaluating Inflow Concentration %0:d in' +
  ' Contaminanent treatment system %1:s in stress period %2:d at time %3:g';

{ TMt3dCtsWriter }

constructor TMt3dCtsWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FMt3dCts := Model.ModflowPackages.Mt3dCts;
  FCtsSystems := Model.CtsSystems;

  FExtractionWells := TListOfObjects.Create;
  FInjectionWells:= TListOfObjects.Create;
  FAllWells:= TListOfObjects.Create;
  FWellCellCounts := TList<Integer>.Create;
  FWellCountDictionary := TDictionary<TScreenObject, Integer>.Create;
  FUsedWellCounts := TList<Integer>.Create;
  FUsedWellCountDictionary := TDictionary<TScreenObject, Integer>.Create;
  FUsedWelStartsDictionary := TDictionary<TScreenObject, Integer>.Create;

  FParser := Model.rpThreeDFormulaCompiler;
end;

destructor TMt3dCtsWriter.Destroy;
begin
  FUsedWelStartsDictionary.Free;
  FUsedWellCountDictionary.Free;
  FUsedWellCounts.Free;
  FWellCountDictionary.Free;
  FWellCellCounts.Free;
  FAllWells.Free;
  FInjectionWells.Free;
  FExtractionWells.Free;
  inherited;
end;

procedure TMt3dCtsWriter.Evaluate;
begin
  GetUsedWells;
end;

class function TMt3dCtsWriter.Extension: string;
begin
  result := '.cts';
end;

procedure TMt3dCtsWriter.WriteASystem(ACstSystem: TCtsSystem;
  StartTime: Double; SystemIndex, StressPeriodIndex: integer);
var
  CstObjects: TCtsObjectItem;
  ICTS: Integer;
  NEXT: Integer;
  NINJ: Integer;
  ITRTINJ: Integer;
  WellIndex: Integer;
  AWell: TScreenObject;
  ACount: Integer;
  WellItem: TIndividualWellInjectionItem;
  InjectionOptions: TCtsInjectionTimeCollection;
  InjectionTimeOption: TCtsInjectionTimeItem;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  NCOMP: Integer;
  CompIndex: Integer;
  ValueFormula: string;
  ExtractionWells: TCellAssignmentList;
  ExternalFlowsItem: TCtsExternalFlowsItem;
begin
  NCOMP := Model.NumberOfMt3dChemComponents;
  CstObjects  := ACstSystem.CtsObjects.GetItemByStartTime(StartTime);
  ICTS := SystemIndex+1;
  NEXT := 0;
  for WellIndex := 0 to CstObjects.ExtractionWellCount - 1 do
  begin
    AWell := CstObjects.ExtractionWells[WellIndex];
    if FUsedWellCountDictionary.TryGetValue(AWell, ACount) then
    begin
      Inc(NEXT, ACount);
    end
    else
    begin
      Assert(False);
    end;
  end;
  NINJ := 0;
  for WellIndex := 0 to CstObjects.InjectionWellCount - 1 do
  begin
    AWell := CstObjects.InjectionWells[WellIndex];
    if FUsedWellCountDictionary.TryGetValue(AWell, ACount) then
    begin
      if ACstSystem.TreatmentDistribution = tlIndividual then
      begin
        WellItem := ACstSystem.Injections.GetItemByObjectName(AWell.Name);
        if WellItem.UseDefaultInjectionOptions then
        begin
          InjectionOptions := ACstSystem.DefaultInjectionOptions;
        end
        else
        begin
          InjectionOptions := WellItem.Injections;
        end;
      end
      else
      begin
        InjectionOptions := ACstSystem.DefaultInjectionOptions;
      end;
      InjectionTimeOption := InjectionOptions.GetItemByStartTime(StartTime);

      if InjectionTimeOption <> nil then
      begin
        Inc(NINJ, ACount);

        CellList := TCellAssignmentList.Create;
        try
          AWell.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          if ACstSystem.TreatmentDistribution = tlIndividual then
          begin
            for CellIndex := 0 to CellList.Count - 1 do
            begin
              ACell := CellList[CellIndex];
              for CompIndex := 0 to NCOMP - 1 do
              begin
                ValueFormula := InjectionTimeOption.InjectionOptions[CompIndex].Value;
                // Get values for data set 8 here
              end;
            end;
          end;


        finally
          CellList.Free;
        end;
      end;
    end
    else
    begin
      Assert(False);
    end;
  end;
  ITRTINJ := Ord(ACstSystem.TreatmentDistribution);

  // Data Set 3
  WriteInteger(ICTS);
  WriteInteger(NEXT);
  WriteInteger(NINJ);
  WriteInteger(ITRTINJ);
  NewLine;

  WriteDataSet4(CstObjects);

  WriteDataSet5(ACstSystem, StartTime, StressPeriodIndex);

  if ITRTINJ = 1 then
  begin
    InjectionOptions := ACstSystem.DefaultInjectionOptions;
    InjectionTimeOption := InjectionOptions.GetItemByStartTime(StartTime);
    WriteDataSet6(InjectionTimeOption);
  end;

  if FMt3dCts.ForceOption = ctsDontForce then
  begin

  end;
  // Write data sets 4, 5, and 6.

end;

procedure TMt3dCtsWriter.WriteDataSet1;
var
  MXCTS: Integer;
  ICTSOUT: Integer;
  MXEXT: Integer;
  MXINJ: Integer;
  MXWEL: Integer;
  IFORCE: Integer;
  ICTSPKG: Integer;
  WellIndex: Integer;
  ACount: Integer;
  function CountWells(AList: TListOfObjects): integer;
  var
    WellIndex: Integer;
    AWell: TScreenObject;
  begin
    result := 0;
    for WellIndex := 0 to AList.Count - 1 do
    begin
      AWell := AList[WellIndex];
      if FWellCountDictionary.TryGetValue(AWell, ACount) then
      begin
        Inc(result, ACount);
      end
      else
      begin
        Assert(False);
      end;
    end;
  end;
begin
  MXCTS := FCtsSystems.Count;
  ICTSOUT := Mt3dCtsOut;
  MXEXT := CountWells(FExtractionWells);
  MXINJ := CountWells(FInjectionWells);
  MXWEL := 0;
  for WellIndex := 0 to FWellCellCounts.Count - 1 do
  begin
    Inc(MXWEL, FWellCellCounts[WellIndex]);
  end;
  IFORCE := Ord(FMt3dCts.ForceOption);
  ICTSPKG := Ord(FMt3dCts.WellPackageChoice);
  WriteInteger(MXCTS);
  WriteInteger(ICTSOUT);
  WriteInteger(MXEXT);
  WriteInteger(MXINJ);
  WriteInteger(MXWEL);
  WriteInteger(IFORCE);
  WriteInteger(ICTSPKG);
  NewLine;
end;

procedure TMt3dCtsWriter.WriteDataSet4(CtsObjects: TCtsObjectItem);
var
  WellIndex: Integer;
  AWell: TScreenObject;
  ACount: Integer;
  CellList: TCellAssignmentList;
  index: Integer;
  KEXT: Integer;
  IEXT: Integer;
  JEXT: Integer;
  IWEXT: Integer;
  ACell: TCellAssignment;
  StartIndex: Integer;
begin
  for WellIndex := 0 to CtsObjects.ExtractionWellCount - 1 do
  begin
    AWell := CtsObjects.ExtractionWells[WellIndex];
    if FUsedWellCountDictionary.TryGetValue(AWell, ACount) then
    begin
      Assert(FUsedWelStartsDictionary.TryGetValue(AWell, StartIndex));
      CellList := TCellAssignmentList.Create;
      try
        AWell.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        for index := 0 to CellList.Count - 1 do
        begin
          ACell := CellList[index];

          KEXT := ACell.Layer + 1;
          IEXT := ACell.Row + 1;
          JEXT := ACell.Column + 1;
          IWEXT := StartIndex + index + 1;

          WriteInteger(KEXT);
          WriteInteger(IEXT);
          WriteInteger(JEXT);
          WriteInteger(IWEXT);
        end;
      finally
        CellList.Free;
      end;
    end;
  end;
end;

procedure TMt3dCtsWriter.WriteDataSet5(ACstSystem: TCtsSystem;StartTime: Double; StressPeriodIndex: Integer);
var
  NCOMP: Integer;
  CompIndex: Integer;
  InflowFormula: string;
  ExternalFlowsItem: TCtsExternalFlowsItem;
  Expression: TExpression;
  QINCTS: Double;
  CINCTS: Array Of Double;
  ConcentrationFormula: string;
begin
  ExternalFlowsItem  := ACstSystem.ExternalFlows.GetItemByStartTime(StartTime);

  NCOMP := Model.NumberOfMt3dChemComponents;
  if ExternalFlowsItem = nil then
  begin
    WriteFloat(0);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      WriteFloat(0);
    end;
  end
  else
  begin
    UpdateCurrentScreenObject(nil);
    UpdateGlobalLocations(-1, -1, -1, eaBlocks, Model);

    InflowFormula := ExternalFlowsItem.Inflow;
    try
      FParser.Compile(InflowFormula);
    Except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingInf,
          [ACstSystem.Name, StressPeriodIndex, StartTime]),
          ExternalFlowsItem.Inflow, E.Message);
        InflowFormula := '0';
        FParser.Compile(InflowFormula);
      end;
    end;

    Expression := FParser.CurrentExpression;
    Expression.Evaluate;
    QINCTS := Expression.doubleResult;

    SetLength(CINCTS, NCOMP);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      ConcentrationFormula := ExternalFlowsItem.InflowConcentrations[CompIndex].Value;

      try
        FParser.Compile(ConcentrationFormula);
      Except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingInfConc,
            [CompIndex+1, ACstSystem.Name, StressPeriodIndex, StartTime]),
            ExternalFlowsItem.Inflow, E.Message);
          ConcentrationFormula := '0';
          FParser.Compile(ConcentrationFormula);
        end;
      end;

      Expression := FParser.CurrentExpression;
      Expression.Evaluate;
      CINCTS[CompIndex] := Expression.doubleResult;
    end;

    WriteFloat(QINCTS);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      WriteFloat(CINCTS[CompIndex]);
    end;
    NewLine;
  end;
end;

procedure TMt3dCtsWriter.WriteDataSet6(
  InjectionTimeOption: TCtsInjectionTimeItem);
var
  NCOMP: Integer;
  CompIndex: Integer;
  InjectItem: TInjectionOptionItem;
  TreatmentFormula: string;
  IOPTINJ: Integer;
  Expression: TExpression;
  CMCHGINJ: Double;
begin
  NCOMP := Model.NumberOfMt3dChemComponents;
  if InjectionTimeOption = nil then
  begin
    for CompIndex := 0 to NCOMP - 1 do
    begin
      WriteInteger(1);
      WriteFloat(0);
    end;
  end
  else
  begin
    for CompIndex := 0 to NCOMP - 1 do
    begin
      InjectItem := InjectionTimeOption.InjectionOptions[CompIndex];
      IOPTINJ := Ord(InjectItem.TreatmentOption)+1;
      TreatmentFormula := InjectItem.Value;

      try
        FParser.Compile(TreatmentFormula);
      Except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', '',
            TreatmentFormula, E.Message);
          TreatmentFormula := '0';
          FParser.Compile(TreatmentFormula);
        end;
      end;

      Expression := FParser.CurrentExpression;
      Expression.Evaluate;
      CMCHGINJ := Expression.doubleResult;

      WriteInteger(IOPTINJ);
      WriteFloat(CMCHGINJ);
    end;
  end;
  NewLine;
end;

procedure TMt3dCtsWriter.WriteFile(const AFileName: string);
begin

end;

procedure TMt3dCtsWriter.WriteStressPeriods;
var
  StressPeriods: TModflowStressPeriods;
  StressPeriodIndex: Integer;
  UsedSystems: TCstSystemList;
  SystemIndex: Integer;
  ACstSystem: TCtsSystem;
  StartTime: Double;
  NCTS: Integer;
  WellIndex: Integer;
  AScreenObject: TScreenObject;
  MNW2_Well: TMnw2Boundary;
  Well: TMfWellBoundary;
  TimeItem: TCustomModflowBoundaryItem;
  StartIndex: Integer;
begin
  UsedSystems := TCstSystemList.Create;
  try
    StressPeriods := Model.ModflowFullStressPeriods;
    for StressPeriodIndex := 0 to StressPeriods.Count - 1 do
    begin
      StartTime := StressPeriods[StressPeriodIndex].StartTime;
      UsedSystems.Clear;
      for SystemIndex := 0 to FCtsSystems.Count - 1 do
      begin
        ACstSystem := FCtsSystems[SystemIndex].CtsSystem;
        if ACstSystem.CtsObjects.GetItemByStartTime(StartTime) <> nil then
        begin
          UsedSystems.Add(ACstSystem);
        end;
      end;

      // Data Set 2;
      NCTS := UsedSystems.Count;
      WriteInteger(NCTS);
      NewLine;

      FUsedWellCounts.Clear;
      FUsedWellCountDictionary.Clear;
      FUsedWelStartsDictionary.Clear;
      StartIndex := 0;
      for WellIndex := 0 to FAllWells.Count - 1 do
      begin
        AScreenObject := FAllWells[WellIndex];
        TimeItem := nil;
        case FMt3dCts.WellPackageChoice of
          cwpcMnw2:
            begin
              MNW2_Well := AScreenObject.ModflowMnw2Boundary;
              TimeItem := MNW2_Well.Values.GetItemByStartTime(StartTime);
            end;
          cwpcWel:
            begin
              Well := AScreenObject.ModflowWellBoundary;
              TimeItem := Well.Values.GetItemByStartTime(StartTime);
            end;
          else
            Assert(False);
        end;
        if TimeItem = nil then
        begin
          FUsedWellCounts.Add(0);
          FUsedWellCountDictionary.Add(AScreenObject, 0);
        end
        else
        begin
          FUsedWellCounts.Add(FWellCellCounts[WellIndex]);
          FUsedWellCountDictionary.Add(AScreenObject, FWellCellCounts[WellIndex]);
        end;
        FUsedWelStartsDictionary.Add(AScreenObject, StartIndex);
        StartIndex := StartIndex + FWellCellCounts[WellIndex];
      end;

      for SystemIndex := 0 to UsedSystems.Count - 1 do
      begin
        ACstSystem := UsedSystems[SystemIndex];
        WriteASystem(ACstSystem, StartTime, SystemIndex, StressPeriodIndex);
      end;
    end;
  finally
    UsedSystems.Free;
  end;
end;

procedure TMt3dCtsWriter.GetUsedWells;
var
  InjectionWellNames: TStringList;
  ExtractionWellNames: TStringList;
  SystemIndex: Integer;
  ASystem: TCtsSystem;
  TimeIndex: Integer;
  ATimeItem: TCtsObjectItem;
  WellIndex: Integer;
  AWell: TScreenObject;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  CellList: TCellAssignmentList;
begin
  InjectionWellNames := TStringList.Create;
  ExtractionWellNames := TStringList.Create;
  try
    InjectionWellNames.Sorted := True;
    ExtractionWellNames.Sorted := True;
    InjectionWellNames.Duplicates := dupIgnore;
    ExtractionWellNames.Duplicates := dupIgnore;
    for SystemIndex := 0 to FCtsSystems.Count - 1 do
    begin
      ASystem := FCtsSystems[SystemIndex].CtsSystem;
      for TimeIndex := 0 to ASystem.CtsObjects.Count - 1 do
      begin
        ATimeItem := ASystem.CtsObjects[TimeIndex];
        for WellIndex := 0 to ATimeItem.InjectionWellCount - 1 do
        begin
          AWell := ATimeItem.InjectionWells[WellIndex];
          if AWell.Deleted then
          begin
            Continue;
          end;
          if AWell.UsedModels.UsesModel(Model) then
          begin
            Continue;
          end;
          InjectionWellNames.AddObject(AWell.Name, AWell);
        end;
        for WellIndex := 0 to ATimeItem.ExtractionWellCount - 1 do
        begin
          AWell := ATimeItem.ExtractionWells[WellIndex];
          if AWell.Deleted then
          begin
            Continue;
          end;
          if AWell.UsedModels.UsesModel(Model) then
          begin
            Continue;
          end;
          ExtractionWellNames.AddObject(AWell.Name, AWell);
        end;
      end;
    end;
    for WellIndex := 0 to InjectionWellNames.Count - 1 do
    begin
      FInjectionWells.Add(InjectionWellNames.Objects[WellIndex] as TScreenObject);
    end;
    for WellIndex := 0 to ExtractionWellNames.Count - 1 do
    begin
      FExtractionWells.Add(ExtractionWellNames.Objects[WellIndex] as TScreenObject);
    end;
  finally
    InjectionWellNames.Free;
    ExtractionWellNames.Free;
  end;
  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if AScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;
    case FMt3dCts.WellPackageChoice of
      cwpcMnw2:
        begin
          if (AScreenObject.ModflowMnw2Boundary <> nil)
            and AScreenObject.ModflowMnw2Boundary.Used then
          begin
            FAllWells.Add(AScreenObject);
          end;
        end;
      cwpcWel:
        begin
          if (AScreenObject.ModflowWellBoundary <> nil)
            and AScreenObject.ModflowWellBoundary.Used then
          begin
            FAllWells.Add(AScreenObject);
          end;
        end;
      else
        Assert(False);
    end;
  end;
  for ScreenObjectIndex := 0 to FAllWells.Count - 1 do
  begin
    AScreenObject := FAllWells[ScreenObjectIndex];
    CellList := TCellAssignmentList.Create;
    try
      AScreenObject.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
      FWellCountDictionary.Add(AScreenObject, CellList.Count);
      FWellCellCounts.Add(CellList.Count);
    finally
      CellList.Free;
    end;
  end;
end;

end.
