unit ModflowMvrWriterUnit;

interface

uses
  CustomModflowWriterUnit, ModflowMvrUnit, GoPhastTypes,
  System.Generics.Collections, PhastModelUnit, ModflowCellUnit, System.Classes,
  ModflowPackageSelectionUnit, ScreenObjectUnit, ModflowBoundaryUnit,
  OrderedCollectionUnit,
  {$IF CompilerVersion > 28}
  System.Hash,
  {$ENDIF}
  System.Generics.Defaults,
  ModflowBoundaryDisplayUnit;

type
  TMvrSource = record
    Key: TMvrRegisterKey;
    SourcePackage: TSourcePackageChoice;
    Receivers: TReceiverCollection;
    MvrTypes: TMvrTypeArray;
    MvrRates: TOneDRealArray;
    // @name is only used when the receiver is a stream and
    // @link(TSfrReceiverChoice) is @link(TSfrReceiverChoice.srcNearest).
    Cell: TCellLocation;
  end;

  TMvrSources = TList<TMvrSource>;

  TMvrReceiverKey = record
    StressPeriod: Integer;
    ScreenObject: TObject;
    ReceiverPackage: TReceiverPackageChoice;
  end;

  TMvrReceiverValues = record
    // @name is the number of the receiver in its respective package and stress
    // period starting with 1. For the stream package, it is ithe number of
    // the first reach defined by the object.
    Index: Integer;
    // @name is only used for streams.
    StreamCells: TCellLocationArray;
    // @name is only used for streams.
    StreamReachNumbers: TOneDIntegerArray;
    // @name is only used for UZF boundaries.
    UzfCells: TOneDIntegerArray;
    // @name is used with SFR and UZF boundaries.
    SectionIndices: TOneDIntegerArray;
    Section: Integer;
  end;

  TMvrReceiver = record
    ReceiverValues: TMvrReceiverValues;
    ReceiverKey: TMvrReceiverKey;
  end;

  TMvrReceiverList = TList<TMvrReceiver>;
  TMvrReceiverLists = TObjectList<TMvrReceiverList>;

  TMvrSourceKeyComparer = class(TEqualityComparer<TMvrSourceKey>)
    function Equals(const Left, Right: TMvrSourceKey): Boolean; override;
    function GetHashCode(const Value: TMvrSourceKey): Integer; override;
  end;

  TMvrSourceCellDictionary = TDictionary<TMvrSourceKey, TMvrSourceCell>;

  TMvrReceiverKeyComparer = class(TEqualityComparer<TMvrReceiverKey>)
    function Equals(const Left, Right: TMvrReceiverKey): Boolean; override;
    function GetHashCode(const Value: TMvrReceiverKey): Integer; override;
  end;

  TModflowMvrWriter = class(TCustomParameterTransientWriter)
  private
  const
    Abbreviation = 'MVR6';
  var
    FReceiverDictionary: TDictionary<TMvrReceiverKey, TMvrReceiverValues>;
    FSourceLists: TObjectList<TMvrSources>;
    FSourceCellDictionaries: TObjectList<TMvrSourceCellDictionary>;
    FSfrReceivers: TMvrReceiverLists;
    FUsedPackages: TSourcePackageChoices;
    FSpeciesIndex: Integer;
    function ShouldEvaluate: Boolean;
    procedure WriteOptions;
    procedure WriteDimensions;
    procedure WritePackages;
    procedure WriteMvrStressPeriods;
    function GetMaxMvr: Integer;
    procedure WriteGwtOptions;
    procedure WriteGwtFileInternal;
  protected
    class function Extension: string; override;
    function Package: TModflowPackageSelection; override;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    procedure WriteStressPeriods(const VariableIdentifiers, DataSetIdentifier,
      DS5, D7PNameIname, D7PName: string); override;
    function ParameterType: TParameterType; override;
    procedure WriteParameterCells(CellList: TValueCellList; NLST: Integer;
      const VariableIdentifiers, DataSetIdentifier: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
    procedure WriteParameterDefinitions(const DS3, DS3Instances, DS4A,
      DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
      AssignmentMethod: TUpdateMethod;
      MultiplierArrayNames: TTransientMultCollection;
      ZoneArrayNames: TTransientZoneCollection); override;
  public
    Constructor Create(AModel: TCustomModel; EvaluationType: TEvaluationType); override;
    destructor Destroy; override;
    procedure AddMvrSource(RegisterKey: TMvrRegisterKey);
    procedure AddMvrReceiver(MvrReceiver: TMvrReceiver);
    procedure Evaluate; override;
    procedure WriteFile(const AFileName: string);
    procedure WriteMvtFile(const AFileName: string; SpeciesIndex: Integer);
    property UsedPackages: TSourcePackageChoices read FUsedPackages;
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

const
  StrMvrbudget = '.mvr_budget';

implementation

uses
  frmProgressUnit, System.SysUtils, frmErrorsAndWarningsUnit, Vcl.Forms,
  FastGEO, ModflowIrregularMeshUnit, AbstractGridUnit, System.Math,
  ModflowLakMf6Unit, RbwParser, frmFormulaErrorsUnit,
  ModflowPackagesUnit, Mt3dmsChemSpeciesUnit, QuadTreeClass;

resourcestring
  StrWritingMVROptions = 'Writing MVR Options';
  StrWritingMVRDimensio = 'Writing MVR Dimensions';
  StrWritingMVRPackages = 'Writing MVR Packages';
  StrWritingMVRStressP = 'Writing MVR Stress Periods';
  StrReceiverNotFound = 'Receiver not found';
  StrSourceObject0s = 'Source object: %0:s.';
  StrTheFormulaDoesNot = 'The formula does not result in a real number.';
  StrMVRValue = 'MVR Value';
  StrErrorInMVRSource = 'Error in MVR source. Check to make sure that the ob' +
  'ject defines both the MVR boundary and the type of boundary specified in ' +
  'the source package for the MVR boundary.';
  StrThereWasAnErrorS = 'There was an error specifying the MVR source in %s.';
  StrInvalidMVRSourceP = 'Invalid MVR source package';
  StrThe0sPackageCan = 'The %0:s package can not be used as a source package' +
  ' in %1:s for MVR because the %0:s package is not seleccted';
  StrInvalidMVRReceiver = 'Invalid MVR receiver package';
  StrThe0sPackageCanRec = 'The %0:s package can not be used as a receiver pa' +
  'ckage in %1:s for MVR because the %0:s package is not seleccted';
  StrInvalidMVRMapName = 'Invalid MVR Map name';
  StrS0InStressPerio = '%s:0 in stress period %1:d is not a MVR Map defined ' +
  'in %s:s.';

type
  TSourceReceiverMap = TDictionary<Integer, TReceiverSectionValue>;
  TSfrQuadTreeDictionary = TDictionary<TObject, TRbwQuadTree>;

{ TModflowMvrWriter }

procedure TModflowMvrWriter.AddMvrReceiver(MvrReceiver: TMvrReceiver);
begin
  if not ShouldEvaluate then
  begin
    Exit
  end;
  if not FReceiverDictionary.ContainsKey(MvrReceiver.ReceiverKey) then
  begin
    FReceiverDictionary.Add(MvrReceiver.ReceiverKey, MvrReceiver.ReceiverValues);

    if MvrReceiver.ReceiverKey.ReceiverPackage = rpcSfr then
    begin
      FSfrReceivers[MvrReceiver.ReceiverKey.StressPeriod].Add(MvrReceiver);
    end;
  end;
end;

procedure TModflowMvrWriter.AddMvrSource(RegisterKey: TMvrRegisterKey);
var
  ScreenObject: TScreenObject;
  MvrSourceCell: TMvrSourceCell;
  MvrSource: TMvrSource;
  MvrItems: TMvrItems;
  StartTime: Double;
  EndTime: Double;
  ItemIndex: Integer;
  AnItem: TMvrItem;
  MvrDefined: Boolean;
begin
  if not ShouldEvaluate then
  begin
    Exit
  end;
  ScreenObject := RegisterKey.SourceKey.ScreenObject as TScreenObject;

  MvrSource.Key := RegisterKey;
  MvrSource.SourcePackage := ScreenObject.ModflowMvr.SourcePackageChoice;
  MvrSource.Receivers := ScreenObject.ModflowMvr.Receivers;
  Assert(RegisterKey.StressPeriod >= 0);
  Assert(RegisterKey.StressPeriod < FSourceCellDictionaries.Count);

  StartTime := Model.ModflowFullStressPeriods[RegisterKey.StressPeriod].StartTime;
  EndTime := Model.ModflowFullStressPeriods[RegisterKey.StressPeriod].EndTime;

  Assert(ScreenObject.ModflowMvr <> nil);
  MvrItems := ScreenObject.ModflowMvr.Values as TMvrItems;
  MvrDefined := False;
  for ItemIndex := 0 to MvrItems.Count - 1 do
  begin
    AnItem := MvrItems[ItemIndex];
    if (AnItem.StartTime <= EndTime) and (AnItem.EndTime > StartTime) then
    begin
      MvrDefined := True;
      break;
    end;
  end;

  if not MvrDefined then
  begin
    Exit;
  end;

  try
    MvrSourceCell := FSourceCellDictionaries[RegisterKey.StressPeriod][RegisterKey.SourceKey];
  except on E: EListError do
    begin
      frmErrorsAndWarnings.AddError(Model, StrErrorInMVRSource,
        Format(StrThereWasAnErrorS, [ScreenObject.Name]), ScreenObject);
      Exit;
    end;
  end;
  MvrSource.MvrTypes := MvrSourceCell.Values.MvrTypes;
  MvrSource.MvrRates := MvrSourceCell.Values.Values   ;
  MvrSource.Cell := MvrSourceCell.Values.Cell;

  Assert(MvrSource.Receivers.Count = Length(MvrSource.MvrTypes));
  Assert(MvrSource.Receivers.Count = Length(MvrSource.MvrRates));
  Assert(RegisterKey.StressPeriod < FSourceLists.Count);
  FSourceLists[RegisterKey.StressPeriod].Add(MvrSource);
end;

function TModflowMvrWriter.CellType: TValueCellType;
begin
  result := TMvrSourceCell;
end;

constructor TModflowMvrWriter.Create(AModel: TCustomModel;
  EvaluationType: TEvaluationType);
var
  StressPeriodIndex: Integer;
  ReceiverComparer: IEqualityComparer<TMvrReceiverKey>;
  SourceComparer: IEqualityComparer<TMvrSourceKey>;
begin
  inherited;
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrErrorInMVRSource);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrReceiverNotFound);

  ReceiverComparer := TMvrReceiverKeyComparer.Create;
  SourceComparer := TMvrSourceKeyComparer.Create;

  FReceiverDictionary := TDictionary<TMvrReceiverKey, TMvrReceiverValues>.
    Create(ReceiverComparer);
  FSfrReceivers := TMvrReceiverLists.Create;

  FSourceLists := TObjectList<TMvrSources>.Create;
  FSourceLists.Capacity := AModel.ModflowFullStressPeriods.Count;

  FSourceCellDictionaries := TObjectList<TMvrSourceCellDictionary>.Create;
  FSourceCellDictionaries.Capacity := AModel.ModflowFullStressPeriods.Count;
  for StressPeriodIndex := 0 to FSourceLists.Capacity - 1 do
  begin
    FSourceLists.Add(TMvrSources.Create);
    FSourceCellDictionaries.Add(TMvrSourceCellDictionary.Create(SourceComparer));
    FSfrReceivers.Add(TMvrReceiverList.Create);
  end;
end;

destructor TModflowMvrWriter.Destroy;
begin
  FSfrReceivers.Free;
  FReceiverDictionary.Free;
  FSourceLists.Free;
  FSourceCellDictionaries.Free;
  inherited;
end;

procedure TModflowMvrWriter.Evaluate;
var
  StressPeriodIndex: Integer;
  AList: TValueCellList;
  MvrSourceDictionary: TMvrSourceCellDictionary;
  SourceKey: TMvrSourceKey;
  CellIndex: Integer;
  ACell: TMvrSourceCell;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  MvrBound: TMvrBoundary;
  ReceiverIndex: Integer;
  ReceiverItem: TReceiverItem;
  ModflowLak6: TLakeMf6;
  LkValues: TMvrRecord;
  TimeIndex: Integer;
  MvrItem: TMvrItem;
  StartStressPeriod: Integer;
  EndStressPeriod: Integer;
  MvrIndex: Integer;
  IndItem: TIndividualMvrItem;
  Formula: string;
  Compiler: TRbwParser;
  Expression: TExpression;
  AReceiver: TReceiverItem;
  Packages: TModflowPackages;
begin
  if not ShouldEvaluate then
  begin
    Exit
  end;
  inherited;
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidMVRSourceP);
  frmErrorsAndWarnings.RemoveWarningGroup(Model, StrInvalidMVRReceiver);
  frmErrorsAndWarnings.RemoveErrorGroup(Model, StrInvalidMVRMapName);

  Compiler := Model.rpThreeDFormulaCompilerNodes;

  for ScreenObjectIndex := 0 to Model.ScreenObjectCount - 1 do
  begin
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    AScreenObject := Model.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    if not AScreenObject.UsedModels.UsesModel(Model) then
    begin
      Continue;
    end;

    MvrBound := AScreenObject.ModflowMvr;
    if (MvrBound = nil) or not MvrBound.Used then
    begin
      Continue;
    end;
    if (MvrBound.SourcePackageChoice = spcLak)  then
    begin
      ModflowLak6 := AScreenObject.ModflowLak6;
      if (ModflowLak6 <> nil) and ModflowLak6.Used then
      begin
        for TimeIndex := 0 to MvrBound.Values.Count - 1 do
        begin
          MvrItem := MvrBound.Values[TimeIndex] as TMvrItem;
          StartStressPeriod := Model.ModflowFullStressPeriods.FindStressPeriod(MvrItem.StartTime);
          EndStressPeriod := Model.ModflowFullStressPeriods.FindEndStressPeriod(MvrItem.EndTime);
          for StressPeriodIndex := StartStressPeriod to EndStressPeriod do
          begin
            AList := Values[StressPeriodIndex];
            if AList.Count > 0 then
            begin
              // Ensure cached list is restored.
              AList.First;
            end;

            ACell := TMvrSourceCell.Create;
            ACell.ScreenObject := AScreenObject;
            ACell.StressPeriod := StressPeriodIndex;
            LkValues.Cell.Layer := 0;
            LkValues.Cell.Row := 0;
            LkValues.Cell.Column := 0;
            LkValues.Cell.Section := 0;
            LkValues.StartingTime := MvrItem.StartTime;
            LkValues.EndingTime := MvrItem.EndTime;

            SetLength(LkValues.Values, MvrItem.Items.Count);
            SetLength(LkValues.MvrTypes, MvrItem.Items.Count);
            SetLength(LkValues.ValueAnnotations, MvrItem.Items.Count);
            Assert(MvrItem.Items.Count = MvrBound.Receivers.Count);
            for MvrIndex := 0 to MvrItem.Items.Count - 1 do
            begin
              AReceiver := MvrBound.Receivers[MvrIndex];
              LkValues.MvrIndex := AReceiver.LakeOutlet-1;

              IndItem := MvrItem.Items[MvrIndex];
              LkValues.MvrTypes[MvrIndex] := IndItem.MvrType;
              Formula := IndItem.Value;
              try
                Compiler.Compile(Formula);
              except on E: ERbwParserError do
                begin
                  frmFormulaErrors.AddFormulaError(AScreenObject.Name,
                    StrMVRValue, Formula, E.Message);
                  Formula := '0';
                  Compiler.Compile(Formula);
                end;
              end;
              Expression := Compiler.CurrentExpression;
              if not (Expression.ResultType in [rdtDouble, rdtInteger]) then
              begin
                frmFormulaErrors.AddFormulaError(AScreenObject.Name,
                  StrMVRValue, Formula, StrTheFormulaDoesNot);
                Formula := '0';
                Compiler.Compile(Formula);
                Expression := Compiler.CurrentExpression;
              end;
              Expression.Evaluate;
              LkValues.Values[MvrIndex] := Expression.DoubleResult;
              LkValues.ValueAnnotations[MvrIndex] := Formula;
            end;

            ACell.Values := LkValues;
            AList.Add(ACell);
          end;
        end;
      end;
    end;
  end;

  for StressPeriodIndex := 0 to Values.Count - 1 do
  begin
    AList := Values[StressPeriodIndex];
    MvrSourceDictionary := FSourceCellDictionaries[StressPeriodIndex];
    for CellIndex := 0 to AList.Count - 1 do
    begin
      ACell := AList[CellIndex] as TMvrSourceCell;
      Assert(ACell.ScreenObject <> nil);
      SourceKey.MvrIndex := ACell.MvrIndex;
      SourceKey.ScreenObject := ACell.ScreenObject as TScreenObject;
      if not MvrSourceDictionary.ContainsKey(SourceKey) then
      begin
        MvrSourceDictionary.Add(SourceKey, ACell);
      end;
    end;
  end;

  FUsedPackages := [];
  Packages := Model.ModflowPackages;
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

    MvrBound := AScreenObject.ModflowMvr;
    if (MvrBound = nil) or not MvrBound.Used then
    begin
      Continue;
    end;

    case MvrBound.SourcePackageChoice of
      spcWel:
        begin
          if not Packages.WelPackage.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['Well', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcDrn:
        begin
          if not Packages.DrnPackage.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['Drain', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcRiv:
        begin
          if not Packages.RivPackage.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['River', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcGhb:
        begin
          if not Packages.GhbBoundary.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['GHB', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcLak:
        begin
          if not Packages.LakMf6Package.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['Lake', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcMaw:
        begin
          if not Packages.MawPackage.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['MAW', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcSfr:
        begin
          if not Packages.SfrModflow6Package.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['SFR', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      spcUzf:
        begin
          if not Packages.UzfMf6Package.IsSelected then
          begin
            frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRSourceP,
              Format(StrThe0sPackageCan, ['UZF', AScreenObject.Name]),
              AScreenObject);
            Continue;
          end;
        end;
      else
        begin
          Assert(False);
        end;
    end;

    Include(FUsedPackages, MvrBound.SourcePackageChoice);

    for ReceiverIndex := 0 to MvrBound.Receivers.Count - 1 do
    begin
      ReceiverItem := MvrBound.Receivers[ReceiverIndex];
      case ReceiverItem.ReceiverPackage of
        rpcLak:
          begin
            if not Packages.LakMf6Package.IsSelected then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRReceiver,
                Format(StrThe0sPackageCanRec, ['Lake', AScreenObject.Name]),
                AScreenObject);
              Continue;
            end;
            Include(FUsedPackages, spcLak);
          end;
        rpcMaw:
          begin
            if not Packages.MawPackage.IsSelected then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRReceiver,
                Format(StrThe0sPackageCanRec, ['MAW', AScreenObject.Name]),
                AScreenObject);
              Continue;
            end;
            Include(FUsedPackages, spcMaw);
          end;
        rpcSfr:
          begin
            if not Packages.SfrModflow6Package.IsSelected then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRReceiver,
                Format(StrThe0sPackageCanRec, ['SFR', AScreenObject.Name]),
                AScreenObject);
              Continue;
            end;
            Include(FUsedPackages, spcSfr);
          end;
        rpcUzf:
          begin
            if not Packages.UzfMf6Package.IsSelected then
            begin
              frmErrorsAndWarnings.AddWarning(Model, StrInvalidMVRReceiver,
                Format(StrThe0sPackageCanRec, ['UZF', AScreenObject.Name]),
                AScreenObject);
              Continue;
            end;
            Include(FUsedPackages, spcUzf);
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

class function TModflowMvrWriter.Extension: string;
begin
  result := '.mvr';
end;

function TModflowMvrWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  result := ScreenObject.ModflowMvr;
end;

function TModflowMvrWriter.GetMaxMvr: Integer;
var
  StressPeriodIndex: Integer;
  SourceList: TMvrSources;
  SourceIndex: Integer;
  ASource: TMvrSource;
  MaxForStressPeriod: Integer;
  ReceiverIndex: Integer;
  ReceiverItem: TReceiverItem;
  ReceiverKey: TMvrReceiverKey;
  ReceiverValues: TMvrReceiverValues;
  SourceScreenObject: TScreenObject;
begin
  result := 0;
  for StressPeriodIndex := 0 to FSourceLists.Count - 1 do
  begin
    ReceiverKey.StressPeriod := StressPeriodIndex;
    MaxForStressPeriod := 0;
    SourceList := FSourceLists[StressPeriodIndex];
    for SourceIndex := 0 to SourceList.Count - 1 do
    begin
      ASource := SourceList[SourceIndex];

      for ReceiverIndex := 0 to ASource.Receivers.Count - 1 do
      begin
        ReceiverItem := ASource.Receivers[ReceiverIndex];
        ReceiverKey.ReceiverPackage := ReceiverItem.ReceiverPackage;
        ReceiverKey.ScreenObject := ReceiverItem.ReceiverObject as TScreenObject;

        if FReceiverDictionary.ContainsKey(ReceiverKey) then
        begin
          ReceiverValues := FReceiverDictionary[ReceiverKey];
        end
        else
        begin
          SourceScreenObject := ASource.Key.SourceKey.ScreenObject as TScreenObject;
          frmErrorsAndWarnings.AddError(Model, StrReceiverNotFound,
           Format(StrSourceObject0s, [SourceScreenObject.Name]),
           SourceScreenObject);
          Continue;
        end;

        if ReceiverItem.ReceiverPackage = rpcUzf then
        begin
          Inc(MaxForStressPeriod, Length(ReceiverValues.UzfCells));
        end
        else
        begin
          Inc(MaxForStressPeriod);
        end;
      end;

    end;
    result := Max(result, MaxForStressPeriod);
  end;
end;

function TModflowMvrWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.MvrPackage;
end;

function TModflowMvrWriter.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

function TModflowMvrWriter.ShouldEvaluate: Boolean;
begin
  result := False;
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  result := True;
end;

procedure TModflowMvrWriter.UpdateDisplay(
  TimeLists: TModflowBoundListOfTimeLists);
var
  StressPeriodIndex: Integer;
  MrvCell: TMvrSourceCell;
  ReceiverIndex: Integer;
  MvrTimes: TModflowBoundaryDisplayTimeList;
  MvrArray: TModflowBoundaryDisplayDataArray;
  TimeListIndex: Integer;
  DisplayTimeList: TModflowBoundaryDisplayTimeList;
  TimeIndex: Integer;
  DataArray: TModflowBoundaryDisplayDataArray;
  AList: TValueCellList;
  CellIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;

  Evaluate;

  MvrTimes := TimeLists[0];

  for StressPeriodIndex := 0 to Values.Count - 1 do
  begin
    AList := Values[StressPeriodIndex];
    MvrArray := MvrTimes[StressPeriodIndex]
      as TModflowBoundaryDisplayDataArray;
    for CellIndex := 0 to AList.Count - 1 do
    begin
      MrvCell := AList[CellIndex] as TMvrSourceCell;
      for ReceiverIndex := 0 to MrvCell.MvrValueCount - 1 do
      begin
        MvrArray.AddDataValue(MrvCell.ValueAnnotations[ReceiverIndex],
          MrvCell.MvrValues[ReceiverIndex],
          MrvCell.Column, MrvCell.Row, MrvCell.Layer);
      end;
    end;
  end;

  for TimeListIndex := 0 to TimeLists.Count - 1 do
  begin
    DisplayTimeList := TimeLists[TimeListIndex];
    for TimeIndex := 0 to DisplayTimeList.Count - 1 do
    begin
      DataArray := DisplayTimeList[TimeIndex]
        as TModflowBoundaryDisplayDataArray;
      DataArray.UpToDate := True;
    end;
    DisplayTimeList.SetUpToDate(True);
  end;

end;

procedure TModflowMvrWriter.WriteDimensions;
var
  PackageChoice: TSourcePackageChoice;
  maxpackages: Integer;
begin
  WriteBeginDimensions;

  WriteString('    MAXMVR ');
  WriteInteger(GetMaxMvr);
  NewLine;

  maxpackages := 0;
  for PackageChoice in FUsedPackages do
  begin
    Inc(maxpackages);
  end;
  WriteString('    MAXPACKAGES ');
  WriteInteger(maxpackages);
  NewLine;

  WriteEndDimensions
end;

procedure TModflowMvrWriter.WriteFile(const AFileName: string);
begin
  if not ShouldEvaluate then
  begin
    Exit
  end;

  FNameOfFile := FileName(AFileName);
  FInputFileName := FNameOfFile;
  WriteToNameFile(Abbreviation, -1, FNameOfFile, foInput, Model, False, 'MVR-1');
  frmErrorsAndWarnings.BeginUpdate;
  try
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;
    OpenFile(FileName(FNameOfFile));
    try
      WriteDataSet0;

      frmProgressMM.AddMessage(StrWritingMVROptions);
      WriteOptions;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingMVRDimensio);
      WriteDimensions;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingMVRPackages);
      WritePackages;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;

      frmProgressMM.AddMessage(StrWritingMVRStressP);
      WriteMvrStressPeriods;
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
    finally
      CloseFile
    end;

  finally
    frmErrorsAndWarnings.EndUpdate;
  end;
end;

procedure TModflowMvrWriter.WriteGwtFileInternal;
begin
  OpenFile(FNameOfFile);
  try
    WriteDataSet0;

    WriteGwtOptions;
  finally
    CloseFile;
  end;
end;

procedure TModflowMvrWriter.WriteGwtOptions;
var
  ASpecies: TMobileChemSpeciesItem;
  budgetfile: string;
  BaseFileName: string;
  budgetCsvFile: string;
  MvrPackage: TMvrPackage;
begin
  WriteBeginOptions;
  try
    Assert(FSpeciesIndex >= 0);
    Assert(FSpeciesIndex < Model.MobileComponents.Count);
    ASpecies := Model.MobileComponents[FSpeciesIndex];

    PrintListInputOption;
    PrintFlowsOption;
    WriteSaveFlowsOption;

    MvrPackage := Model.ModflowPackages.MvrPackage;
    BaseFileName := ChangeFileExt(FNameOfFile, '');
    BaseFileName := ChangeFileExt(BaseFileName, '') + '.' + ASpecies.Name;

    if MvrPackage.SaveBudgetFile then
    begin
      WriteString('    BUDGET FILEOUT ');
      budgetfile := BaseFileName + '.mvt_budget';
      Model.AddModelOutputFile(budgetfile);
      budgetfile := ExtractFileName(budgetfile);
      WriteString(budgetfile);
      NewLine;
    end;

    if MvrPackage.SaveCsvBudgetFile then
    begin
      WriteString('    BUDGETCSV FILEOUT ');
      budgetCsvFile := BaseFileName + '.mvt_budget.csv';
      Model.AddModelOutputFile(budgetCsvFile);
      budgetCsvFile := ExtractFileName(budgetCsvFile);
      WriteString(budgetCsvFile);
      NewLine;
    end;
  finally
    WriteEndOptions
  end;
end;

procedure TModflowMvrWriter.WriteMvrStressPeriods;
var
  StressPeriodIndex: Integer;
  SourceList: TMvrSources;
  SourceIndex: Integer;
  ASource: TMvrSource;
  ReceiverKey: TMvrReceiverKey;
  ReceiverItem: TReceiverItem;
  ReceiverValues: TMvrReceiverValues;
  MvrSourceDictionary: TMvrSourceCellDictionary;
  MvrCell: TMvrSourceCell;
  ReceiverIndex: Integer;
  AColumn: Integer;
  ARow: Integer;
  DisvGrid: TModflowDisvGrid;
  Grid: TCustomModelGrid;
  MinIndex: Integer;
  SourceLocation: TPoint2D;
  ReceiverLocation: TPoint2D;
  SfrCellIndex: Integer;
  SourceScreenObject: TScreenObject;
  InnerReceiverIndex: Integer;
  ReceiverCount: Integer;
  PotentialReceivers: TMvrReceiverList;
  SfrReciverIndex: Integer;
  AReceiver: TMvrReceiver;
  AStreamCell: TCellLocation;
  DummyValue: Integer;
  UseSfrNearestMultipleSegment: Boolean;
  AllStreamReaches: TRbwQuadTree;
  EnclosedReaches: TRbwQuadTree;
  CurrentObjectReaches: TRbwQuadTree;
  GridLimit: TGridLimit;
  PriorSourceObject: TObject;
  PriorReceiverObject: TObject;
  SourceReceiverIndexMap: TSourceReceiverMap;
  MapName: string;
  ModflowMvr: TMvrBoundary;
  MapIndex: Integer;
  AMap: TSectionMapItemCollection;
  MapItemIndex: Integer;
  MapItem: TSectionMapItem;
  Divisor: Integer;
  ReceiverSection: Integer;
  SectionMaps: TDictionary<string,TSourceReceiverMap>;
  SectionMapsList: TObjectList<TSourceReceiverMap>;
  QuadTreeList: TObjectList<TRbwQuadTree>;
  SfrQuadTreeDictionary: TSfrQuadTreeDictionary;
  ReceiverSectionValue: TReceiverSectionValue;
  function GetLocation(ACol, ARow: Integer): TPoint2D;
  begin
    if Grid <> nil then
    begin
      result := Grid.TwoDElementCenter(ACol, ARow)
    end
    else
    begin
      Assert(DisvGrid <> nil);
      result := DisvGrid.TwoDGrid.Cells[ACol].Location;
    end;
  end;
  procedure AssignGridLimits(QuadTree: TRbwQuadTree);
  begin
    QuadTree.XMax := GridLimit.MaxX;
    QuadTree.XMin := GridLimit.MinX;
    QuadTree.YMax := GridLimit.MaxY;
    QuadTree.YMin := GridLimit.MinY;
  end;
begin
  if Model.DisvUsed then
  begin
    DisvGrid := Model.DisvGrid;
    Grid := nil;
  end
  else
  begin
    Grid := Model.Grid;
    DisvGrid := nil;
  end;

  SectionMaps := TDictionary<string,TSourceReceiverMap>.Create;
  SectionMapsList := TObjectList<TSourceReceiverMap>.Create;
  AllStreamReaches := TRbwQuadTree.Create(nil);
  EnclosedReaches := TRbwQuadTree.Create(nil);
  QuadTreeList := TObjectList<TRbwQuadTree>.Create;
  SfrQuadTreeDictionary := TSfrQuadTreeDictionary.Create;
  try
    if Model.ModflowPackages.SfrModflow6Package.IsSelected then
    begin
      AllStreamReaches.MaxPoints := 5;
      EnclosedReaches.MaxPoints := 5;
      GridLimit := Model.DiscretizationLimits(vdTop);
      AssignGridLimits(AllStreamReaches);
      AssignGridLimits(EnclosedReaches);
    end;
    for StressPeriodIndex := 0 to FSourceLists.Count - 1 do
    begin
      frmProgressMM.AddMessage(Format('    Writing MVR stress period %d', [StressPeriodIndex+1]));
      Application.ProcessMessages;
      if not frmProgressMM.ShouldContinue then
      begin
        Exit;
      end;
      SourceList := FSourceLists[StressPeriodIndex];
      if SourceList.Count > 0 then
      begin
        AllStreamReaches.Clear;
        PriorSourceObject := nil;
        PriorReceiverObject := nil;
        MvrSourceDictionary := FSourceCellDictionaries[StressPeriodIndex];

        ReceiverKey.StressPeriod := StressPeriodIndex;
        WriteBeginPeriod(StressPeriodIndex);

        for SourceIndex := 0 to SourceList.Count - 1 do
        begin
          // ASource.Key.SourceKey.ScreenObject has the maps.
          // ASource.SourcePackage indicates the source package
          ASource := SourceList[SourceIndex];
          if PriorSourceObject <> ASource.Key.SourceKey.ScreenObject then
          begin
            PriorSourceObject := ASource.Key.SourceKey.ScreenObject;
            EnclosedReaches.Clear;
            SourceScreenObject := ASource.Key.SourceKey.ScreenObject as TScreenObject;
            SectionMaps.Clear;
            SectionMapsList.Clear;

            ModflowMvr := SourceScreenObject.ModflowMvr;
            Assert(ModflowMvr <> nil);
            for MapIndex := 0 to ModflowMvr.MvrMaps.Count - 1 do
            begin
              AMap := ModflowMvr.MvrMaps[MapIndex].MvrMap;

              SourceReceiverIndexMap := TSourceReceiverMap.Create;
              SectionMapsList.Add(SourceReceiverIndexMap);
              SectionMaps.Add(UpperCase(AMap.MapName), SourceReceiverIndexMap);
              for MapItemIndex := 0 to AMap.Count - 1 do
              begin
                MapItem := AMap[MapItemIndex];
                for ReceiverIndex := 0 to MapItem.ReceiverSectionsValues.Count - 1 do
                begin
                  SourceReceiverIndexMap.Add(MapItem.SourceSection-1, MapItem.ReceiverSectionsValues[ReceiverIndex]);
                end;
              end;
            end;

          end;
          // MrvCell.FValues.MvrMapNames has the map name
          // MrvCell.Values.Cell.Section is the object section
          MvrCell := MvrSourceDictionary[ASource.Key.SourceKey];


          for ReceiverIndex := 0 to ASource.Receivers.Count - 1 do
          begin
            SourceReceiverIndexMap := nil;
            ReceiverItem := ASource.Receivers[ReceiverIndex];
            if (ReceiverItem.ReceiverPackage in [rpcSfr, rpcUzf])
              and (ASource.SourcePackage in [spcWel, spcDrn, spcRiv, spcGhb, spcSfr, spcUzf]) then
            begin
              MapName := MvrCell.Values.MvrMapNames[ReceiverIndex];
              if MapName <> '' then
              begin
                if not SectionMaps.TryGetValue(UpperCase(MapName), SourceReceiverIndexMap) then
                begin
                  SourceReceiverIndexMap := nil;
                  frmErrorsAndWarnings.AddError(Model, StrInvalidMVRMapName,
                    Format(StrS0InStressPerio,
                    [MapName, StressPeriodIndex+1, MvrCell.ScreenObject.Name]),
                    MvrCell.ScreenObject as TObject);
                end;
              end;
            end;

            ReceiverKey.ReceiverPackage := ReceiverItem.ReceiverPackage;
            ReceiverKey.ScreenObject := ReceiverItem.ReceiverObject;

            UseSfrNearestMultipleSegment :=
              (ReceiverItem.ReceiverPackage = rpcSfr)
              and (ReceiverItem.SfrReceiverChoice in
              [srcNearestEnclosed, srcNearestAnySegment]);
            ReceiverCount := 1;
            Divisor := 1;

            if not UseSfrNearestMultipleSegment then
            begin
              if FReceiverDictionary.ContainsKey(ReceiverKey) then
              begin
                ReceiverValues := FReceiverDictionary[ReceiverKey];
              end
              else
              begin
                SourceScreenObject := ASource.Key.SourceKey.ScreenObject as TScreenObject;
                frmErrorsAndWarnings.AddError(Model, StrReceiverNotFound,
                  Format(StrSourceObject0s, [SourceScreenObject.Name]),
                  SourceScreenObject);
                Continue;
              end;
            end;

            ReceiverSectionValue := nil;
            ReceiverSection := -1;
            if ReceiverItem.ReceiverPackage = rpcUzf then
            begin
              ReceiverCount := Length(ReceiverValues.UzfCells);
              Divisor := ReceiverCount;
              if (ASource.SourcePackage in [spcWel, spcDrn, spcRiv, spcGhb, spcSfr, spcUzf]) then
              begin
                MapName := MvrCell.Values.MvrMapNames[ReceiverIndex];
                // If a map is used, ReceiverCount can not be used as the divisor.
                if (MapName <> '') and (SourceReceiverIndexMap <> nil) then
                begin
                  if not SourceReceiverIndexMap.TryGetValue(MvrCell.Section, ReceiverSectionValue) then
                  begin
                    Continue
                  end;
                  ReceiverSection := ReceiverSectionValue.SectionNumber-1;
                  Divisor := 1;
                end;
              end;
            end;

            if ReceiverItem.ReceiverPackage = rpcSfr then
            begin
              ReceiverCount := Length(ReceiverValues.StreamCells);
              if (ASource.SourcePackage in [spcWel, spcDrn, spcRiv, spcGhb, spcSfr, spcUzf]) then
              begin
                MapName := MvrCell.Values.MvrMapNames[ReceiverIndex];
                if (MapName <> '') and (SourceReceiverIndexMap <> nil) then
                begin
                  if not SourceReceiverIndexMap.TryGetValue(MvrCell.Section, ReceiverSectionValue) then
                  begin
                    Continue
                  end;
                  ReceiverSection := ReceiverSectionValue.SectionNumber-1;
                end
                else
                begin
                  ReceiverCount := 1;
                end
              end
              else
              begin
                ReceiverCount := 1;
              end
            end;

            if (ReceiverKey.ScreenObject <> PriorReceiverObject)
              and (ReceiverItem.ReceiverPackage = rpcSfr) then
            begin
              case ReceiverItem.SfrReceiverChoice of
                srcFirst:
                  begin
                  end;
                srcNearest:
                  begin
                    if not SfrQuadTreeDictionary.TryGetValue(ReceiverKey.ScreenObject,
                      CurrentObjectReaches) then
                    begin
                      CurrentObjectReaches := TRbwQuadTree.Create(nil);
                      QuadTreeList.Add(CurrentObjectReaches);
                      CurrentObjectReaches.MaxPoints := 5;
                      AssignGridLimits(CurrentObjectReaches);
                      SfrQuadTreeDictionary.Add(ReceiverKey.ScreenObject, CurrentObjectReaches);
                      for SfrCellIndex := 0 to Length(ReceiverValues.StreamCells) - 1 do
                      begin
                        if SourceReceiverIndexMap <> nil then
                        begin
                          if not SourceReceiverIndexMap.TryGetValue(MvrCell.Section, ReceiverSectionValue) then
                          begin
                            Continue;
                          end;
                          ReceiverSection := ReceiverSectionValue.SectionNumber -1;
                          if ReceiverSection <> ReceiverValues.SectionIndices[SfrCellIndex] then
                          begin
                            Continue;
                          end;
                        end;
                        ReceiverLocation := GetLocation(
                          ReceiverValues.StreamCells[SfrCellIndex].Column,
                          ReceiverValues.StreamCells[SfrCellIndex].Row);
                        CurrentObjectReaches.AddPoint(ReceiverLocation.x,
                          ReceiverLocation.y,
                          Addr(ReceiverValues.StreamReachNumbers[SfrCellIndex]))
                      end;
                    end;
                  end;
                srcNearestEnclosed:
                  begin
                  end;
                srcNearestAnySegment:
                  begin
                  end;
                else
                  Assert(False);
              end;
              PriorReceiverObject := ReceiverKey.ScreenObject;
            end;

            if (ReceiverItem.ReceiverPackage = rpcSfr)
              and (ReceiverItem.SfrReceiverChoice = srcNearestAnySegment)
              and (AllStreamReaches.Count = 0) then
            begin
              PotentialReceivers := FSfrReceivers[StressPeriodIndex];
              for SfrReciverIndex := 0 to PotentialReceivers.Count - 1 do
              begin
                AReceiver := PotentialReceivers[SfrReciverIndex];
                for SfrCellIndex := 0 to Length(
                  AReceiver.ReceiverValues.StreamCells) - 1 do
                begin
                  if SourceReceiverIndexMap <> nil then
                  begin
                    if not SourceReceiverIndexMap.TryGetValue(MvrCell.Section, ReceiverSectionValue) then
                    begin
                      Continue;
                    end;
                    ReceiverSection := ReceiverSectionValue.SectionNumber -1;
                    if ReceiverSection <> ReceiverValues.SectionIndices[SfrCellIndex] then
                    begin
                      Continue;
                    end;
                  end;

                  AStreamCell := AReceiver.ReceiverValues.
                    StreamCells[SfrCellIndex];
                  ReceiverLocation := GetLocation( AStreamCell.Column,
                    AStreamCell.Row);
                  AllStreamReaches.AddPoint(ReceiverLocation.x,
                    ReceiverLocation.y,
                    Addr(AReceiver.ReceiverValues.StreamReachNumbers[SfrCellIndex]));
                end;
              end
            end;

            if (ReceiverItem.ReceiverPackage = rpcSfr)
              and (ReceiverItem.SfrReceiverChoice = srcNearestEnclosed)
              and (EnclosedReaches.Count = 0) then
            begin
              PotentialReceivers := FSfrReceivers[StressPeriodIndex];
              SourceScreenObject := ASource.Key.SourceKey.ScreenObject as TScreenObject;
              for SfrReciverIndex := 0 to PotentialReceivers.Count - 1 do
              begin
                AReceiver := PotentialReceivers[SfrReciverIndex];
                for SfrCellIndex := 0 to Length(
                  AReceiver.ReceiverValues.StreamCells) - 1 do
                begin
                  if SourceReceiverIndexMap <> nil then
                  begin
                    if not SourceReceiverIndexMap.TryGetValue(MvrCell.Section, ReceiverSectionValue) then
                    begin
                      Continue;
                    end;
                    ReceiverSection := ReceiverSectionValue.SectionNumber - 1;
                    if ReceiverSection <> AReceiver.ReceiverValues.SectionIndices[SfrCellIndex] then
                    begin
                      Continue;
                    end;
                  end;
                  AStreamCell := AReceiver.ReceiverValues.
                    StreamCells[SfrCellIndex];
                  ReceiverLocation := GetLocation( AStreamCell.Column,
                    AStreamCell.Row);
                  if SourceScreenObject.IsPointInside(ReceiverLocation, DummyValue) then
                  begin
                    EnclosedReaches.AddPoint(ReceiverLocation.x,
                      ReceiverLocation.y,
                      Addr(AReceiver.ReceiverValues.StreamReachNumbers[SfrCellIndex]));
                  end;
                end;
              end
            end;

            for InnerReceiverIndex := 0 to ReceiverCount-1 do
            begin
              if SourceReceiverIndexMap <> nil then
              begin
                if (ReceiverItem.ReceiverPackage = rpcUzf) then
                begin
                  if (ReceiverSection <> ReceiverValues.SectionIndices[InnerReceiverIndex]) then
                  begin
                    Continue;
                  end;
                end
                else if (ReceiverItem.ReceiverPackage = rpcSfr) then
                begin
                  if (ReceiverSection <> ReceiverValues.SectionIndices[InnerReceiverIndex]) then
                  begin
                    Continue;
                  end
                  else
                  begin
                    ReceiverSection := -1;
                  end;
                end;
              end;

              case ASource.SourcePackage of
                spcWel:
                  begin
                    WriteString('  WEL-1');
                  end;
                spcDrn:
                  begin
                    WriteString('  DRN-1');
                  end;
                spcRiv:
                  begin
                    WriteString('  RIV-1');
                  end;
                spcGhb:
                  begin
                    WriteString('  GHB-1');
                  end;
                spcLak:
                  begin
                    WriteString('  LAK-1');
                  end;
                spcMaw:
                  begin
                    WriteString('  MAW-1');
                  end;
                spcSfr:
                  begin
                    WriteString('  SFR-1');
                  end;
                spcUzf:
                  begin
                    WriteString('  UZF-1');
                  end;
                else Assert(False);
              end;

              WriteInteger(ASource.Key.Index);

              case ReceiverItem.ReceiverPackage of
                rpcLak:
                  begin
                    WriteString(' LAK-1');
                  end;
                rpcMaw:
                  begin
                    WriteString(' MAW-1');
                  end;
                rpcSfr:
                  begin
                    WriteString(' SFR-1');
                  end;
                rpcUZF:
                  begin
                    WriteString(' UZF-1');
                  end;
              end;

              if (ReceiverItem.ReceiverPackage = rpcSfr)
                and (ReceiverItem.SfrReceiverChoice = srcNearest) then
              begin
                AColumn := MvrCell.Column;
                ARow := MvrCell.Row;
                SourceLocation := GetLocation(AColumn, ARow);

                Assert(Length(ReceiverValues.StreamCells) > 0);
                Assert(Length(ReceiverValues.StreamReachNumbers)
                  = Length(ReceiverValues.StreamCells));

                if CurrentObjectReaches.Count = 0 then
                begin
                  MinIndex := -1;
                end
                else
                begin
                  MinIndex := PInteger(CurrentObjectReaches.NearestPointsFirstData(
                    SourceLocation.x, SourceLocation.y))^;
                end;
                WriteInteger(MinIndex);
              end
              else if (ReceiverItem.ReceiverPackage = rpcSfr)
                and (ReceiverItem.SfrReceiverChoice = srcNearestAnySegment) then
              begin
                AColumn := MvrCell.Column;
                ARow := MvrCell.Row;
                SourceLocation := GetLocation(AColumn, ARow);

                if AllStreamReaches.Count = 0 then
                begin
                  MinIndex := -1;
                end
                else
                begin
                  MinIndex := PInteger(AllStreamReaches.NearestPointsFirstData(
                    SourceLocation.x, SourceLocation.y))^;
                end;
                WriteInteger(MinIndex);
              end
              else if (ReceiverItem.ReceiverPackage = rpcSfr)
                and (ReceiverItem.SfrReceiverChoice = srcNearestEnclosed) then
              begin
                AColumn := MvrCell.Column;
                ARow := MvrCell.Row;
                SourceLocation := GetLocation(AColumn, ARow);

                if EnclosedReaches.Count = 0 then
                begin
                  MinIndex := -1;
                end
                else
                begin
                  MinIndex := PInteger(EnclosedReaches.NearestPointsFirstData(
                    SourceLocation.x, SourceLocation.y))^;
                end;
                WriteInteger(MinIndex);
              end
              else if ReceiverItem.ReceiverPackage = rpcUzf then
              begin
                WriteInteger(ReceiverValues.UzfCells[InnerReceiverIndex]);
              end
              else
              begin
                WriteInteger(ReceiverValues.Index);
              end;

              case MvrCell.MvrTypes[ReceiverIndex] of
                mtFactor:
                  begin
                    WriteString(' FACTOR   ');
                  end;
                mtExcess:
                  begin
                    WriteString(' EXCESS   ');
                  end;
                mtThreshold:
                  begin
                    WriteString(' THRESHOLD');
                  end;
                mtUpTo:
                  begin
                    WriteString(' UPTO     ');
                  end;
                else Assert(False);
              end;

              if ReceiverSectionValue = nil then
              begin
                WriteFloat(MvrCell.MvrValues[ReceiverIndex]/Divisor);
              end
              else
              begin
                WriteFloat(ReceiverSectionValue.Value);
              end;

              NewLine;
            end;
          end;
        end;

        WriteEndPeriod;
      end;
    end;
  finally
    AllStreamReaches.Free;
    EnclosedReaches.Free;
    SectionMaps.Free;
    SectionMapsList.Free;
    SfrQuadTreeDictionary.Free;
    QuadTreeList.Free;
  end;
end;

procedure TModflowMvrWriter.WriteMvtFile(const AFileName: string;
  SpeciesIndex: Integer);
var
  SpeciesName: string;
  Abbreviation: string;
begin
  if not Package.IsSelected then
  begin
    Exit
  end;
  if Model.ModelSelection = msModflow2015 then
  begin
    Abbreviation := 'MVT6';
  end
  else
  begin
    Exit;
  end;
  if Model.PackageGeneratedExternally(Abbreviation) then
  begin
    Exit;
  end;
  if not Model.MobileComponents[SpeciesIndex].UsedForGWT then
  begin
    Exit;
  end;
  FPestParamUsed := False;
  FSpeciesIndex :=  SpeciesIndex;
  SpeciesName := Model.MobileComponents[SpeciesIndex].Name;
  FNameOfFile := ChangeFileExt(AFileName, '') + '.' + SpeciesName + '.mvt';
  FInputFileName := FNameOfFile;

  WriteToGwtNameFile(Abbreviation, FNameOfFile, SpeciesIndex);

  FPestParamUsed := False;
  WritingTemplate := False;

  frmErrorsAndWarnings.BeginUpdate;
  try
    WriteGwtFileInternal;
  finally
    frmErrorsAndWarnings.EndUpdate;
  end;

end;

procedure TModflowMvrWriter.WriteOptions;
var
  MvrPackage: TMvrPackage;
  budgetfile: string;
  BaseFile: string;
  budgetCsvFile: string;
begin
  WriteBeginOptions;

  PrintListInputOption;
  PrintFlowsOption;

  BaseFile := ChangeFileExt(FNameOfFile, '');
  MvrPackage := Model.ModflowPackages.MvrPackage;
  if MvrPackage.SaveBudgetFile or Model.SeparateGwtUsed then
  begin
    WriteString('    BUDGET FILEOUT ');
    budgetfile := ChangeFileExt(BaseFile, StrMvrbudget);
    Model.AddModelOutputFile(budgetfile);
    budgetfile := ExtractFileName(budgetfile);
    WriteString(budgetfile);
    NewLine;
  end;

  if MvrPackage.SaveCsvBudgetFile then
  begin
    WriteString('    BUDGETCSV FILEOUT ');
    budgetCsvFile := ChangeFileExt(BaseFile, '.mvr_budget.csv');
    Model.AddModelOutputFile(budgetCsvFile);
    budgetCsvFile := ExtractFileName(budgetCsvFile);
    WriteString(budgetCsvFile);
    NewLine;
  end;

  WriteEndOptions
end;

procedure TModflowMvrWriter.WritePackages;
var
  PackageChoice: TSourcePackageChoice;
begin
  WriteString('BEGIN PACKAGES');
  NewLine;

  for PackageChoice in FUsedPackages do
  begin
    case PackageChoice of
      spcWel:
        begin
          WriteString('  WEL-1');
          NewLine
        end;
      spcDrn:
        begin
          WriteString('  DRN-1');
          NewLine
        end;
      spcRiv:
        begin
          WriteString('  RIV-1');
          NewLine
        end;
      spcGhb:
        begin
          WriteString('  GHB-1');
          NewLine
        end;
      spcLak:
        begin
          WriteString('  LAK-1');
          NewLine
        end;
      spcMaw:
        begin
          WriteString('  MAW-1');
          NewLine
        end;
      spcSfr:
        begin
          WriteString('  SFR-1');
          NewLine
        end;
      spcUzf:
        begin
          WriteString('  UZF-1');
          NewLine
        end;
      else Assert(False);
    end;
  end;

  WriteString('END PACKAGES');
  NewLine;
  NewLine;
end;

procedure TModflowMvrWriter.WriteParameterCells(CellList: TValueCellList;
  NLST: Integer; const VariableIdentifiers, DataSetIdentifier: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
  inherited;
end;

procedure TModflowMvrWriter.WriteParameterDefinitions(const DS3, DS3Instances,
  DS4A, DataSetIdentifier, VariableIdentifiers, ErrorRoot: string;
  AssignmentMethod: TUpdateMethod;
  MultiplierArrayNames: TTransientMultCollection;
  ZoneArrayNames: TTransientZoneCollection);
begin
  Assert(False);
  inherited;
end;

procedure TModflowMvrWriter.WriteStressPeriods(const VariableIdentifiers,
  DataSetIdentifier, DS5, D7PNameIname, D7PName: string);
begin
  Assert(False);
  inherited;
end;

{ TMvrReceiverKeyComparer }

function TMvrReceiverKeyComparer.Equals(const Left,
  Right: TMvrReceiverKey): Boolean;
begin
  Result := (Left.StressPeriod = Right.StressPeriod)
    and (Left.ScreenObject = Right.ScreenObject)
    and (Left.ReceiverPackage = Right.ReceiverPackage);
end;

function TMvrReceiverKeyComparer.GetHashCode(
  const Value: TMvrReceiverKey): Integer;
begin
  {$IF CompilerVersion > 28}
  Result := THashBobJenkins.GetHashValue(Value.StressPeriod, SizeOf(Value.StressPeriod), 0);
  Result := THashBobJenkins.GetHashValue(Value.ScreenObject, SizeOf(Value.ScreenObject), Result);
  Result := THashBobJenkins.GetHashValue(Value.ReceiverPackage, SizeOf(Value.ReceiverPackage), Result);
  {$ELSE}
  Result := BobJenkinsHash(Value.StressPeriod, SizeOf(Value.StressPeriod), 0);
  Result := BobJenkinsHash(Value.ScreenObject, SizeOf(Value.ScreenObject), Result);
  Result := BobJenkinsHash(Value.ReceiverPackage, SizeOf(Value.ReceiverPackage), Result);
  {$ENDIF}
end;

{ TMvrSourceKeyComparer }

function TMvrSourceKeyComparer.Equals(const Left,
  Right: TMvrSourceKey): Boolean;
begin
  Result := (Left.MvrIndex = Right.MvrIndex)
    and (Left.ScreenObject = Right.ScreenObject)
end;

function TMvrSourceKeyComparer.GetHashCode(const Value: TMvrSourceKey): Integer;
begin
  {$IF CompilerVersion > 28}
  Result := THashBobJenkins.GetHashValue(Value.MvrIndex, SizeOf(Value.MvrIndex), 0);
  Result := THashBobJenkins.GetHashValue(Value.ScreenObject, SizeOf(Value.ScreenObject), Result);
  {$ELSE}
  Result := BobJenkinsHash(Value.MvrIndex, SizeOf(Value.MvrIndex), 0);
  Result := BobJenkinsHash(Value.ScreenObject, SizeOf(Value.ScreenObject), Result);
  {$ENDIF}
end;

end.
