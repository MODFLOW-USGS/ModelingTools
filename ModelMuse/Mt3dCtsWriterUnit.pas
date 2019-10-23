unit Mt3dCtsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, Mt3dCtsSystemUnit, System.Classes,
  ModflowPackageSelectionUnit, System.Generics.Collections, ScreenObjectUnit,
  RbwParser, Vcl.Forms;

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
    FNameOfFile: string;
    procedure WriteDataSet1;
    procedure WriteStressPeriods;
    procedure WriteASystem(ACstSystem: TCtsSystem; StartTime: Double;
      SystemIndex, StressPeriodIndex: integer);
    procedure WriteDataSet4(CtsObjects: TCtsObjectItem);
    procedure WriteDataSet5(ACstSystem: TCtsSystem; StartTime: Double;
      StressPeriodIndex: Integer);
    procedure WriteDataSet6(InjectionTimeOption: TCtsInjectionTimeItem);
    procedure WriteDataSet7(ACstSystem: TCtsSystem; StartTime: Double;
      StressPeriodIndex: Integer);
    procedure WriteDataSet8(ACstSystem: TCtsSystem; StartTime: Double;
      StressPeriodIndex: Integer);
    procedure WriteDataSet9(ACstSystem: TCtsSystem; StartTime: Double;
      StressPeriodIndex: Integer);

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
  System.SysUtils, frmProgressUnit;

resourcestring
  StrErrorEvaluatingInf = 'Error evaluating Inflow in Contaminanent treatmen' +
  't system %0:s in stress period %1:d at time %2:g';
  StrErrorEvaluatingInfConc = 'Error evaluating Inflow Concentration %0:d in' +
  ' Contaminanent treatment system %1:s in stress period %2:d at time %3:g';
  StrErrorEvaluatingMax = 'Error evaluating Maximum Allowed Concentration %0' +
  ':d in Contaminanent treatment system %1:s in stress period %2:d at time %' +
  '3:g';
  StrErrorEvaluatingOut = 'Error evaluating Outflow in Contaminanent treatme' +
  'nt system %0:s in stress period %1:d at time %2:g';
  StrWritingMT3DUSGSCT = 'Writing MT3D-USGS CTS Package input.';

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
begin
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  CstObjects  := ACstSystem.CtsObjects.GetItemByStartTime(StartTime);
  ICTS := SystemIndex+1;
  NEXT := 0;
  for WellIndex := 0 to CstObjects.ExtractionWellCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
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
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
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
      end;
    end
    else
    begin
      Assert(False);
    end;
  end;
  ITRTINJ := Ord(ACstSystem.TreatmentDistribution);

  frmProgressMM.AddMessage(StrWritingDataSet3);
  // Data Set 3
  WriteInteger(ICTS);
  WriteInteger(NEXT);
  WriteInteger(NINJ);
  WriteInteger(ITRTINJ);
  WriteString(' # Data Set 3 for ');
  WriteString(ACstSystem.Name);
  WriteString(', ICTS, NEXT, NINJ, ITRTINJ');
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
    WriteDataSet7(ACstSystem, StartTime, StressPeriodIndex);
  end;

  WriteDataSet8(ACstSystem, StartTime, StressPeriodIndex);
  WriteDataSet9(ACstSystem, StartTime, StressPeriodIndex);

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
  WriteString(' # Data Set 1, MXCTS, ICTSOUT, MXEXT, MXINJ, MXWEL, IFORCE, ICTSPKG');
  NewLine;

  WriteToMt3dMsNameFile(StrData, Mt3dCtsOut,
    ChangeFileExt(FNameOfFile, '.cto'), foOutput, Model);

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
  frmProgressMM.AddMessage(StrWritingDataSet4);
  for WellIndex := 0 to CtsObjects.ExtractionWellCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    AWell := CtsObjects.ExtractionWells[WellIndex];
    if FUsedWellCountDictionary.TryGetValue(AWell, ACount) then
    begin
      Assert(FUsedWelStartsDictionary.TryGetValue(AWell, StartIndex));
      CellList := TCellAssignmentList.Create;
      try
        AWell.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
        for index := 0 to CellList.Count - 1 do
        begin
          Application.ProcessMessages;
          if not frmProgressMM.ShouldContinue then
          begin
            Exit;
          end;
          ACell := CellList[index];

          KEXT := ACell.Layer + 1;
          IEXT := ACell.Row + 1;
          JEXT := ACell.Column + 1;
          IWEXT := StartIndex + index + 1;

          WriteInteger(KEXT);
          WriteInteger(IEXT);
          WriteInteger(JEXT);
          WriteInteger(IWEXT);
          WriteString(' # Data Set 4, KEXT, IEXT, JEXT, IWEXT');
          NewLine;
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
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet5);
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
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    SetLength(CINCTS, NCOMP);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ConcentrationFormula := ExternalFlowsItem.InflowConcentrations[CompIndex].Value;

      try
        FParser.Compile(ConcentrationFormula);
      Except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingInfConc,
            [CompIndex+1, ACstSystem.Name, StressPeriodIndex, StartTime]),
            ExternalFlowsItem.InflowConcentrations[CompIndex].Value, E.Message);
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
  end;
  WriteString(' # Data Set 5, QINCTS, (CINCTS(n), n=1,NCOMP)');
  NewLine;
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
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet6);
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
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
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
  WriteString(' # Data Set 6, IOPTINJ(n), CMCHGINJ(n), n=1,NCOMP');
  NewLine;
end;

procedure TMt3dCtsWriter.WriteDataSet7(ACstSystem: TCtsSystem;
  StartTime: Double; StressPeriodIndex: Integer);
var
  NCOMP: Integer;
  CompIndex: Integer;
//  InflowFormula: string;
  MaxConcItem: TCtsMaxConcItem;
  Expression: TExpression;
//  QINCTS: Double;
  CNTE: Array Of Double;
  ConcentrationFormula: string;
begin
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet7);
  MaxConcItem  := ACstSystem.MaximumAllowedConc.GetItemByStartTime(StartTime);

  NCOMP := Model.NumberOfMt3dChemComponents;
  if MaxConcItem = nil then
  begin
    for CompIndex := 0 to NCOMP - 1 do
    begin
      WriteFloat(0);
    end;
  end
  else
  begin
    UpdateCurrentScreenObject(nil);
    UpdateGlobalLocations(-1, -1, -1, eaBlocks, Model);

    SetLength(CNTE, NCOMP);
    for CompIndex := 0 to NCOMP - 1 do
    begin
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      ConcentrationFormula := MaxConcItem.MaxConcentrations[CompIndex].Value;

      try
        FParser.Compile(ConcentrationFormula);
      Except on E: ERbwParserError do
        begin
          frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingMax,
            [CompIndex+1, ACstSystem.Name, StressPeriodIndex, StartTime]),
            MaxConcItem.MaxConcentrations[CompIndex].Value, E.Message);
          ConcentrationFormula := '0';
          FParser.Compile(ConcentrationFormula);
        end;
      end;

      Expression := FParser.CurrentExpression;
      Expression.Evaluate;
      CNTE[CompIndex] := Expression.doubleResult;
    end;

    for CompIndex := 0 to NCOMP - 1 do
    begin
      WriteFloat(CNTE[CompIndex]);
    end;
  end;
  WriteString(' # Data Set 7, CNTE(n), n=1,NCOMP');
  NewLine;
end;

procedure TMt3dCtsWriter.WriteDataSet8(ACstSystem: TCtsSystem;
  StartTime: Double; StressPeriodIndex: Integer);
var
  CstObjects: TCtsObjectItem;
  WellIndex: Integer;
  AWell: TScreenObject;
  ACount: Integer;
  WellItem: TIndividualWellInjectionItem;
  InjectionOptions: TCtsInjectionTimeCollection;
  InjectionTimeOption: TCtsInjectionTimeItem;
  CellList: TCellAssignmentList;
  CellIndex: Integer;
  ACell: TCellAssignment;
  CompIndex: Integer;
  NCOMP: Integer;
  ValueFormula: string;
  KINJ: Integer;
  IINJ: Integer;
  JINJ: Integer;
  IWINJ: Integer;
  StartIndex: Integer;
  Expression: TExpression;
  OptionItem: TInjectionOptionItem;
  IOPTINJ: Integer;
  CMCHGINJ: double;
begin
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet8);
  NCOMP := Model.NumberOfMt3dChemComponents;
  CstObjects  := ACstSystem.CtsObjects.GetItemByStartTime(StartTime);
  for WellIndex := 0 to CstObjects.InjectionWellCount - 1 do
  begin
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    AWell := CstObjects.InjectionWells[WellIndex];
    if FUsedWellCountDictionary.TryGetValue(AWell, ACount) then
    begin
      Assert(FUsedWelStartsDictionary.TryGetValue(AWell, StartIndex));
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
        CellList := TCellAssignmentList.Create;
        try
          AWell.GetCellsToAssign('0', nil, nil, CellList, alAll, Model);
          for CellIndex := 0 to CellList.Count - 1 do
          begin
            Application.ProcessMessages;
            if not frmProgressMM.ShouldContinue then
            begin
              Exit;
            end;
            ACell := CellList[CellIndex];
            KINJ := ACell.Layer + 1;
            IINJ := ACell.Row + 1;
            JINJ := ACell.Column + 1;
            IWINJ := StartIndex + CellIndex + 1;
            WriteInteger(KINJ);
            WriteInteger(IINJ);
            WriteInteger(JINJ);
            WriteInteger(IWINJ);
            if ACstSystem.TreatmentDistribution = tlIndividual then
            begin
              for CompIndex := 0 to NCOMP - 1 do
              begin                        ;
                Application.ProcessMessages;
                if not frmProgressMM.ShouldContinue then
                begin
                  Exit;
                end;
                OptionItem := InjectionTimeOption.InjectionOptions[CompIndex];
                IOPTINJ := Ord(OptionItem.TreatmentOption)+1;

                ValueFormula := OptionItem.Value;
                // Get values for data set 8 here
                try
                  FParser.Compile(ValueFormula);
                Except on E: ERbwParserError do
                  begin
                    frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingMax,
                      [CompIndex+1, ACstSystem.Name, StressPeriodIndex, StartTime]),
                      InjectionTimeOption.InjectionOptions[CompIndex].Value, E.Message);
                    ValueFormula := '0';
                    FParser.Compile(ValueFormula);
                  end;
                end;

                Expression := FParser.CurrentExpression;
                Expression.Evaluate;
                CMCHGINJ := Expression.doubleResult;

                WriteInteger(IOPTINJ);
                WriteFloat(CMCHGINJ);
              end;
            end;
            WriteString(' # Data Set 8 KINJ, IINJ, JINJ, IWINJ');
            if ACstSystem.TreatmentDistribution = tlIndividual then
            begin
              WriteString(', IOPTINJ(n),CMCHGINJ(n), n=1,NCOMP');
            end;

            NewLine;
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
end;

procedure TMt3dCtsWriter.WriteDataSet9(ACstSystem: TCtsSystem;
  StartTime: Double; StressPeriodIndex: Integer);
var
  OutflowFormula: string;
  ExternalFlowsItem: TCtsExternalFlowsItem;
  Expression: TExpression;
  QOUTCTS: Double;
begin
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;
  frmProgressMM.AddMessage(StrWritingDataSet9);
  ExternalFlowsItem  := ACstSystem.ExternalFlows.GetItemByStartTime(StartTime);

  if ExternalFlowsItem = nil then
  begin
    WriteFloat(0);
  end
  else
  begin
    UpdateCurrentScreenObject(nil);
    UpdateGlobalLocations(-1, -1, -1, eaBlocks, Model);

    OutflowFormula := ExternalFlowsItem.Outflow;
    try
      FParser.Compile(OutflowFormula);
    Except on E: ERbwParserError do
      begin
        frmFormulaErrors.AddFormulaError('', Format(StrErrorEvaluatingOut,
          [ACstSystem.Name, StressPeriodIndex, StartTime]),
          ExternalFlowsItem.Inflow, E.Message);
        OutflowFormula := '0';
        FParser.Compile(OutflowFormula);
      end;
    end;

    Expression := FParser.CurrentExpression;
    Expression.Evaluate;
    QOUTCTS := Expression.doubleResult;
    WriteFloat(QOUTCTS);
  end;
  WriteString(' # Data Set 9, QOUTCTS');
  NewLine;
end;

procedure TMt3dCtsWriter.WriteFile(const AFileName: string);
begin
  if not Model.ModflowPackages.Mt3dCts.IsSelected then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  // PackageGeneratedExternally needs to be updated for MT3DMS
  if Model.PackageGeneratedExternally(StrCTS) then
  begin
    Exit;
  end;

  Evaluate;
  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  WriteToMt3dMsNameFile(StrCTS, mt3dCTS,
    FNameOfFile, foInput, Model);

  OpenFile(FNameOfFile);
  try
    frmProgressMM.AddMessage(StrWritingMT3DUSGSCT);
    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteStressPeriods;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
  finally
    CloseFile;
  end;


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
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      frmProgressMM.AddMessage(Format(StrWritingStressP, [StressPeriodIndex+1]));


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
      WriteString(' # Data Set 2, NCTS, Stress Period');
      WriteFreeInteger(StressPeriodIndex+1);
      NewLine;

      FUsedWellCounts.Clear;
      FUsedWellCountDictionary.Clear;
      FUsedWelStartsDictionary.Clear;
      StartIndex := 0;
      for WellIndex := 0 to FAllWells.Count - 1 do
      begin
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;

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
        Application.ProcessMessages;
        if not frmProgressMM.ShouldContinue then
        begin
          Exit;
        end;
        ACstSystem := UsedSystems[SystemIndex];
        frmProgressMM.AddMessage(Format('Writing System %s', [ACstSystem.Name]));
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
          if not AWell.UsedModels.UsesModel(Model) then
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
          if not AWell.UsedModels.UsesModel(Model) then
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
    if not AScreenObject.UsedModels.UsesModel(Model) then
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
