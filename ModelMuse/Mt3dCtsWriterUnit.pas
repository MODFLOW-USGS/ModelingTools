unit Mt3dCtsWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, Mt3dCtsSystemUnit, System.Classes,
  ModflowPackageSelectionUnit, System.Generics.Collections, ScreenObjectUnit;

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
    procedure WriteDataSet1;
    procedure WriteStressPeriods;
    procedure WriteASystem(ACstSystem: TCtsSystem; StartTime: Double;
      SystemIndex: integer);

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
  ModflowWellUnit, ModflowBoundaryUnit;

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
end;

destructor TMt3dCtsWriter.Destroy;
begin
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
  StartTime: Double; SystemIndex: integer);
var
  CstObjects: TCtsObjectItem;
  ICTS: Integer;
  NEXT: Integer;
  NINJ: Integer;
  ITRTINJ: Integer;
  WellIndex: Integer;
  AWell: TScreenObject;
  ACount: Integer;
begin
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
      Inc(NINJ, ACount);
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
      end;

      for SystemIndex := 0 to UsedSystems.Count - 1 do
      begin
        ACstSystem := UsedSystems[SystemIndex];
        WriteASystem(ACstSystem, StartTime, SystemIndex);
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
