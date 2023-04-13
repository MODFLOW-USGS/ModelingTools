unit ModflowSwrStructureUnit;

interface

uses
  Classes, GoPhastTypes, OrderedCollectionUnit, SysUtils;

type
  TSwrStructureType = (sstUncontrolledZeroDepth, sstNone, sstSpecifiedElevation,
    sstUncontrolledCriticalDepth, sstPump, sstStageDishargeTable,
    sstCulvert, sstFixedWeir, sstFixedSpillway, sstMoveableWeir,
    sstGatedSpillway, sstSpillEquation, sstSfrInflow, sstManning,
    sstOverbankFlow);

  TCulvertType = (ctCircular, ctRectangular);

  TControlType = (ctStage, ctFlow);

  TControlOperated = (coGreaterEqual, coLessThan);

  TSpecificationMethod = (smValue, smTabFile);

  TStructureRestrictions = (srRestrictFromConnected, srBidirectional,
    srRestrictToConnected);

  TSmoothingMethod = (smLinear, smSigmoid);

  TStructureDischargeItem = class(TOrderedItem)
  private
    FStoredElevation: TRealStorage;
    FStoredDischarge: TRealStorage;
    function GetElev: Double;
    function GetDischarge: Double;
    procedure SetElev(const Value: Double);
    procedure SetStoredElevation(const Value: TRealStorage);
    procedure SetStoredDischarge(const Value: TRealStorage);
    procedure SetDischarge(const Value: Double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    // STRQ
    property Discharge: Double read GetDischarge write SetDischarge;
    // STRELEV
    property Elev: Double read GetElev write SetElev;
  published
    property StoredDischarge: TRealStorage read FStoredDischarge
      write SetStoredDischarge;
    property StoredElevation: TRealStorage read FStoredElevation
      write SetStoredElevation;
  end;

  TStructureDischargeCollection = class(TOrderedCollection)
  private
    function GetItems(Index: Integer): TStructureDischargeItem;
    procedure SetItems(Index: Integer; const Value: TStructureDischargeItem);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: Integer]: TStructureDischargeItem read GetItems
      write SetItems; default;
    function Add: TStructureDischargeItem;
  end;

  TStructureTimeItem = class(TOrderedItem)
  private
    FStoredStartTime: TRealStorage;
    FStoredEndTime: TRealStorage;
    FUsed: boolean;
    procedure SetStoredEndTime(const Value: TRealStorage);
    procedure SetStoredStartTime(const Value: TRealStorage);
    function GetEndTime: double;
    function GetStartTime: double;
    procedure SetEndTime(const Value: double);
    procedure SetStartTime(const Value: double);
    procedure SetUsed(const Value: boolean);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property StartTime: double read GetStartTime write SetStartTime;
    property EndTime: double read GetEndTime write SetEndTime;
  published
    property StoredStartTime: TRealStorage read FStoredStartTime
      write SetStoredStartTime;
    property StoredEndTime: TRealStorage read FStoredEndTime
      write SetStoredEndTime;
    property Used: boolean read FUsed write SetUsed;
  end;

  TStructureTimes = class(TOrderedCollection)
  private
    function GetItem(Index: Integer): TStructureTimeItem;
    procedure SetItem(Index: Integer; const Value: TStructureTimeItem);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: Integer]: TStructureTimeItem read GetItem
      write SetItem; default;
    function Add: TStructureTimeItem;
    function Last: TStructureTimeItem;
  end;

  TStructure = class(TOrderedItem)
  private
    FStructureType: TSwrStructureType;
    FStoredOrificeDischargeCoefficient: TRealStorage;
    FTable: TStructureDischargeCollection;
    FName: string;
    FStoredWeirDischargeCoefficient: TRealStorage;
    FStoredCulvertRise: TRealStorage;
    FStoredCulvertLength: TRealStorage;
    FStoredDownstreamInvertElevation: TRealStorage;
    FStoredSubmergenceExponent: TRealStorage;
    FStoredWidth: TRealStorage;
    FStoredInitialFlowRateOrGateOpening: TRealStorage;
    FStoredInvertElevation: TRealStorage;
    FStoredCulvertRoughness: TRealStorage;
    FCulvertType: TCulvertType;
    FStoredDownstreamCulvertLength: TRealStorage;
    FSpecifyCulvertLengths: Boolean;
    FStoredControlOffsetCriterion: TRealStorage;
    FControlType: TControlType;
    FControlReach: integer;
    FFullCriticalTabFileName: string;
    FStoredStartingControlRate: TRealStorage;
    FStoredMaximumControlRate: TRealStorage;
    FControlOperated: TControlOperated;
    FStoredCriticalValue: TRealStorage;
    FConnectedConnectedReach: Integer;
    FFullDischargeTabFile: string;
    FCriticalMethod: TSpecificationMethod;
    FReach: integer;
    FConnectedReach: integer;
    FStructureRestrictions: TStructureRestrictions;
    FSfrSegment: integer;
    FSfrReach: integer;
    FTimes: TStructureTimes;
    FInitialFlowRateMethod: TSpecificationMethod;
    FFullInitialFlowRateTabFile: string;
    FStoredSmoothingValue: TRealStorage;
    FSmoothingMethod: TSmoothingMethod;
    procedure SetStructureType(const Value: TSwrStructureType);
    procedure SetName(const Value: string);
    procedure SetStoredCulvertLength(const Value: TRealStorage);
    procedure SetStoredCulvertRise(const Value: TRealStorage);
    procedure SetStoredCulvertRoughness(const Value: TRealStorage);
    procedure SetStoredDownstreamInvertElevation(const Value: TRealStorage);
    procedure SetStoredInitialFlowRateOrGateOpening(const Value: TRealStorage);
    procedure SetStoredInvertElevation(const Value: TRealStorage);
    procedure SetStoredOrificeDischargeCoefficient(const Value: TRealStorage);
    procedure SetStoredSubmergenceExponent(const Value: TRealStorage);
    procedure SetStoredWeirDischargeCoefficient(const Value: TRealStorage);
    procedure SetStoredWidth(const Value: TRealStorage);
    procedure SetTable(const Value: TStructureDischargeCollection);
    function GetCulvertLength: double;
    function GetCulvertRise: double;
    function GetCulvertRoughness: double;
    function GetDownstreamInvertElevation: double;
    function GetInitialFlowRateOrGateOpening: double;
    function GetInvertElevation: double;
    function GetOrificeDischargeCoefficient: double;
    function GetSubmergenceExponent: double;
    function GetWeirDischargeCoefficient: double;
    function GetWidth: double;
    procedure SetCulvertLength(const Value: double);
    procedure SetCulvertRise(const Value: double);
    procedure SetCulvertRoughness(const Value: double);
    procedure SetDownstreamInvertElevation(const Value: double);
    procedure SetInitialFlowRateOrGateOpening(const Value: double);
    procedure SetInvertElevation(const Value: double);
    procedure SetOrificeDischargeCoefficient(const Value: double);
    procedure SetSubmergenceExponent(const Value: double);
    procedure SetWeirDischargeCoefficient(const Value: double);
    procedure SetWidth(const Value: double);
    procedure SetCulvertType(const Value: TCulvertType);
    procedure SetStoredDownstreamCulvertLength(const Value: TRealStorage);
    function GetDownstreamCulvertLength: double;
    procedure SetDownstreamCulvertLength(const Value: double);
    procedure SetSpecifyCulvertLengths(const Value: Boolean);
    procedure SetConnectedConnectedReach(const Value: Integer);
    procedure SetControlOperated(const Value: TControlOperated);
    procedure SetControlReach(const Value: integer);
    procedure SetControlType(const Value: TControlType);
    procedure SetCriticalMethod(const Value: TSpecificationMethod);
    procedure SetCriticalTabFileName(const Value: string);
    procedure SetDischargeTabFile(const Value: string);
    procedure SetStoredControlOffsetCriterion(const Value: TRealStorage);
    procedure SetStoredCriticalValue(const Value: TRealStorage);
    procedure SetStoredMaximumControlRate(const Value: TRealStorage);
    procedure SetStoredStartingControlRate(const Value: TRealStorage);
    function GetControlOffsetCriterion: double;
    function GetCriticalValue: double;
    function GetMaximumControlRate: double;
    function GetStartingControlRate: double;
    procedure SetControlOffsetCriterion(const Value: double);
    procedure SetCriticalValue(const Value: double);
    procedure SetMaximumControlRate(const Value: double);
    procedure SetStartingControlRate(const Value: double);
    procedure SetReach(const Value: integer);
    procedure SetConnectedReach(const Value: integer);
    procedure SetStructureRestrictions(const Value: TStructureRestrictions);
    procedure SetSfrReach(const Value: integer);
    procedure SetSfrSegment(const Value: integer);
    procedure SetTimes(const Value: TStructureTimes);
    procedure SetInitialFlowRateMethod(const Value: TSpecificationMethod);
    procedure SetInitialFlowRateTabFile(const Value: string);
    function GetDischargeTabFile: string;
    function GetFullDischargeTabFile: string;
    procedure SetFullDischargeTabFile(const Value: string);
    function GetInitialFlowRateTabFile: string;
    function GetFullInitialFlowRateTabFile: string;
    procedure SetFullInitialFlowRateTabFile(const Value: string);
    function GetCriticalTabFileName: string;
    function GetFullCriticalTabFileName: string;
    procedure SetFullCriticalTabFileName(const Value: string);
    function GetSmoothingValue: Double;
    procedure SetSmoothingMethod(const Value: TSmoothingMethod);
    procedure SetSmoothingValue(const Value: Double);
    procedure SetStoredSmoothingValue(const Value: TRealStorage);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function UsedInPeriod(StartTime, EndTime: Double): boolean;
    property WeirDischargeCoefficient: double read GetWeirDischargeCoefficient
      write SetWeirDischargeCoefficient;
    property OrificeDischargeCoefficient: double
      read GetOrificeDischargeCoefficient write SetOrificeDischargeCoefficient;
    property SubmergenceExponent: double read GetSubmergenceExponent
      write SetSubmergenceExponent;
    property InvertElevation: double read GetInvertElevation
      write SetInvertElevation;
    property DownstreamInvertElevation: double read GetDownstreamInvertElevation
      write SetDownstreamInvertElevation;
    property Width: double read GetWidth write SetWidth;
    // STRWID2
    property CulvertRise: double read GetCulvertRise write SetCulvertRise;
    property CulvertLength: double read GetCulvertLength write SetCulvertLength;
    property DownstreamCulvertLength: double read GetDownstreamCulvertLength
      write SetDownstreamCulvertLength;
    property CulvertRoughness: double read GetCulvertRoughness
      write SetCulvertRoughness;
    // STRVAL
    property InitialFlowRateOrGateOpening: double
      read GetInitialFlowRateOrGateOpening write SetInitialFlowRateOrGateOpening;
    // CSTRCRIT
    property CriticalValue: double read GetCriticalValue write SetCriticalValue;
    // STRCRITC
    property ControlOffsetCriterion: double read GetControlOffsetCriterion
      write SetControlOffsetCriterion;
    // STRRT
    property StartingControlRate: double read GetStartingControlRate
      write SetStartingControlRate;
    // STRMAX
    property MaximumControlRate: double read GetMaximumControlRate
      write SetMaximumControlRate;
    // CSTRVAL
    property FullDischargeTabFile: string read GetFullDischargeTabFile
      write SetFullDischargeTabFile;
    // STRVAL
    property FullInitialFlowRateTabFile: string read GetFullInitialFlowRateTabFile
      write SetFullInitialFlowRateTabFile;
    // CSTRCRIT
    property FullCriticalTabFileName: string read GetFullCriticalTabFileName
      write SetFullCriticalTabFileName;
    property SmoothingValue: Double read GetSmoothingValue
      write SetSmoothingValue;
  published
    // ISTRNUM
    property Name: string read FName write SetName;
    // ISTRRCH
    // @name starts at 1.
    property Reach: integer read FReach write SetReach;
    // ISTRCONN
    // @name starts at 1.
    property ConnectedReach: integer read FConnectedReach
      write SetConnectedReach;
    // ISTRDIR
    property StructureRestrictions: TStructureRestrictions
       read FStructureRestrictions write SetStructureRestrictions;
    // ISTRTYPE
    property StructureType: TSwrStructureType read FStructureType
      write SetStructureType Stored True;
    // STRCD
    property StoredWeirDischargeCoefficient: TRealStorage
      read FStoredWeirDischargeCoefficient
      write SetStoredWeirDischargeCoefficient;
    // STRCD2
    property StoredOrificeDischargeCoefficient: TRealStorage
      read FStoredOrificeDischargeCoefficient
      write SetStoredOrificeDischargeCoefficient;
    // STRCD3
    property StoredSubmergenceExponent: TRealStorage
      read FStoredSubmergenceExponent write SetStoredSubmergenceExponent;
    // STRINV
    property StoredInvertElevation: TRealStorage read FStoredInvertElevation
      write SetStoredInvertElevation;
    // STRINV2
    property StoredDownstreamInvertElevation: TRealStorage
      read FStoredDownstreamInvertElevation
      write SetStoredDownstreamInvertElevation;
    // STRWID
    property StoredWidth: TRealStorage read FStoredWidth write SetStoredWidth;
    // STRWID
    property CulvertType: TCulvertType read FCulvertType write SetCulvertType;
    // STRWID2
    property StoredCulvertRise: TRealStorage read FStoredCulvertRise
      write SetStoredCulvertRise;
    // STRLEN and STRLEN2 with ISTRTYPE = 2 or -2
    property SpecifyCulvertLengths: Boolean read FSpecifyCulvertLengths
      write SetSpecifyCulvertLengths;
    // STRLEN
    property StoredCulvertLength: TRealStorage read FStoredCulvertLength
      write SetStoredCulvertLength;
    // STRLEN2
    property StoredDownstreamCulvertLength: TRealStorage
      read FStoredDownstreamCulvertLength
      write SetStoredDownstreamCulvertLength;
    // STRMAN
    property StoredCulvertRoughness: TRealStorage read FStoredCulvertRoughness
      write SetStoredCulvertRoughness;
    // STRVAL
    property InitialFlowRateMethod: TSpecificationMethod read FInitialFlowRateMethod write SetInitialFlowRateMethod;
    // STRVAL
    property StoredInitialFlowRateOrGateOpening: TRealStorage
      read FStoredInitialFlowRateOrGateOpening
      write SetStoredInitialFlowRateOrGateOpening;
    // STRVAL
    property InitialFlowRateTabFile: string read GetInitialFlowRateTabFile
      write SetInitialFlowRateTabFile;
    // ISFRSEG
    property SfrSegment: integer read FSfrSegment write SetSfrSegment;
    // ISFRRCH
    property SfrReach: integer read FSfrReach write SetSfrReach;
    // CSTROTYP
    property ControlType: TControlType read FControlType write SetControlType;
    // ISTRORCH
    property ControlReach: integer read FControlReach write SetControlReach;
    // ISTROQCON
    property ConnectedControlReach: Integer read FConnectedConnectedReach
      write SetConnectedConnectedReach;
    // CSTROLO
    property ControlOperated: TControlOperated read FControlOperated
      write SetControlOperated;
    // CSTRCRIT
    property CriticalMethod: TSpecificationMethod read FCriticalMethod
      write SetCriticalMethod;
    // CSTRCRIT
    property CriticalTabFileName: string read GetCriticalTabFileName
      write SetCriticalTabFileName;
    // CSTRCRIT
    property StoredCriticalValue: TRealStorage read FStoredCriticalValue
      write SetStoredCriticalValue;
    // STRCRITC
    property StoredControlOffsetCriterion: TRealStorage
      read FStoredControlOffsetCriterion write SetStoredControlOffsetCriterion;
    // STRRT
    property StoredStartingControlRate: TRealStorage
      read FStoredStartingControlRate write SetStoredStartingControlRate;
    // STRMAX
    property StoredMaximumControlRate: TRealStorage
      read FStoredMaximumControlRate write SetStoredMaximumControlRate;
    // CSTRVAL @name is a relative file name
    property DischargeTabFile: string read GetDischargeTabFile
      write SetDischargeTabFile;
    // data set 13c
    property Table: TStructureDischargeCollection read FTable write SetTable;
    property Times: TStructureTimes read FTimes write SetTimes;
    // STRWSMO
    property SmoothingMethod: TSmoothingMethod read FSmoothingMethod
      write SetSmoothingMethod default smSigmoid;
    property StoredSmoothingValue: TRealStorage read FStoredSmoothingValue
      write SetStoredSmoothingValue;
  end;

  TStructureCollection = class(TOrderedCollection)
  private
    function GetItems(Index: Integer): TStructure;
    procedure SetItems(Index: Integer; const Value: TStructure);
  public
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: Integer]: TStructure read GetItems
      write SetItems; default;
    function GetStructureByName(AName: string): TStructure;
    function Add: TStructure;
  end;


implementation

uses
  PhastModelUnit;

{ TStructure }

procedure TStructure.Assign(Source: TPersistent);
var
  SourceItem: TStructure;
begin
  if Source is TStructure then
  begin
    SourceItem := TStructure(Source);
    Name := SourceItem.Name;
    Reach := SourceItem.Reach;
    ConnectedReach := SourceItem.ConnectedReach;
    StructureRestrictions := SourceItem.StructureRestrictions;
    StructureType := SourceItem.StructureType;
    WeirDischargeCoefficient := SourceItem.WeirDischargeCoefficient;
    OrificeDischargeCoefficient := SourceItem.OrificeDischargeCoefficient;
    SubmergenceExponent := SourceItem.SubmergenceExponent;
    InvertElevation := SourceItem.InvertElevation;
    DownstreamInvertElevation := SourceItem.DownstreamInvertElevation;
    Width := SourceItem.Width;
    CulvertType := SourceItem.CulvertType;
    CulvertRise := SourceItem.CulvertRise;
    SpecifyCulvertLengths := SourceItem.SpecifyCulvertLengths;
    CulvertLength := SourceItem.CulvertLength;
    DownstreamCulvertLength := SourceItem.DownstreamCulvertLength;
    CulvertRoughness := SourceItem.CulvertRoughness;
    InitialFlowRateOrGateOpening := SourceItem.InitialFlowRateOrGateOpening;
    SfrSegment := SourceItem.SfrSegment;
    SfrReach := SourceItem.SfrReach;

    ControlType := SourceItem.ControlType;
    ControlReach := SourceItem.ControlReach;
    ConnectedControlReach := SourceItem.ConnectedControlReach;
    ControlOperated := SourceItem.ControlOperated;
    CriticalMethod := SourceItem.CriticalMethod;
    FullCriticalTabFileName := SourceItem.FullCriticalTabFileName;
    CriticalValue := SourceItem.CriticalValue;
    ControlOffsetCriterion := SourceItem.ControlOffsetCriterion;
    StartingControlRate := SourceItem.StartingControlRate;
    MaximumControlRate := SourceItem.MaximumControlRate;
    FullDischargeTabFile := SourceItem.FullDischargeTabFile;
    InitialFlowRateMethod := SourceItem.InitialFlowRateMethod;
    FullInitialFlowRateTabFile := SourceItem.FullInitialFlowRateTabFile;
    Table := SourceItem.Table;
    Times := SourceItem.Times;

    SmoothingValue := SourceItem.SmoothingValue;
    SmoothingMethod := SourceItem.SmoothingMethod;
  end
  else
  begin
    inherited;
  end;
end;

constructor TStructure.Create(Collection: TCollection);
begin
  inherited;
  FSmoothingMethod := smSigmoid;

  FStoredOrificeDischargeCoefficient := TRealStorage.Create;
  FStoredOrificeDischargeCoefficient.OnChange := OnInvalidateModelEvent;

  FStoredWeirDischargeCoefficient := TRealStorage.Create;
  FStoredWeirDischargeCoefficient.OnChange := OnInvalidateModelEvent;

  FStoredCulvertRise := TRealStorage.Create;
  FStoredCulvertRise.OnChange := OnInvalidateModelEvent;

  FStoredCulvertLength := TRealStorage.Create;
  FStoredCulvertLength.OnChange := OnInvalidateModelEvent;

  FStoredDownstreamCulvertLength := TRealStorage.Create;
  FStoredDownstreamCulvertLength.OnChange := OnInvalidateModelEvent;

  FStoredDownstreamInvertElevation := TRealStorage.Create;
  FStoredDownstreamInvertElevation.OnChange := OnInvalidateModelEvent;

  FStoredSubmergenceExponent := TRealStorage.Create;
  FStoredSubmergenceExponent.OnChange := OnInvalidateModelEvent;

  FStoredWidth := TRealStorage.Create;
  FStoredWidth.OnChange := OnInvalidateModelEvent;

  FStoredInitialFlowRateOrGateOpening := TRealStorage.Create;
  FStoredInitialFlowRateOrGateOpening.OnChange := OnInvalidateModelEvent;

  FStoredInvertElevation := TRealStorage.Create;
  FStoredInvertElevation.OnChange := OnInvalidateModelEvent;

  FStoredCulvertRoughness := TRealStorage.Create;
  FStoredCulvertRoughness.OnChange := OnInvalidateModelEvent;

  FStoredCriticalValue := TRealStorage.Create;
  FStoredCriticalValue.OnChange := OnInvalidateModelEvent;

  FStoredControlOffsetCriterion := TRealStorage.Create;
  FStoredControlOffsetCriterion.OnChange := OnInvalidateModelEvent;

  FStoredStartingControlRate := TRealStorage.Create;
  FStoredStartingControlRate.OnChange := OnInvalidateModelEvent;

  FStoredMaximumControlRate := TRealStorage.Create;
  FStoredMaximumControlRate.OnChange := OnInvalidateModelEvent;

  FStoredSmoothingValue := TRealStorage.Create;
  FStoredSmoothingValue.OnChange := OnInvalidateModelEvent;

  FTable := TStructureDischargeCollection.Create(Model as TCustomModel);

  FTimes := TStructureTimes.Create(Model as TCustomModel);

end;

destructor TStructure.Destroy;
begin
  FTimes.Free;
  FStoredOrificeDischargeCoefficient.Free;
  FStoredWeirDischargeCoefficient.Free;
  FStoredCulvertRise.Free;
  FStoredDownstreamCulvertLength.Free;
  FStoredCulvertLength.Free;
  FStoredDownstreamInvertElevation.Free;
  FStoredSubmergenceExponent.Free;
  FStoredWidth.Free;
  FStoredInitialFlowRateOrGateOpening.Free;
  FStoredInvertElevation.Free;
  FStoredCulvertRoughness.Free;
  FStoredCriticalValue.Free;
  FStoredControlOffsetCriterion.Free;
  FStoredStartingControlRate.Free;
  FStoredMaximumControlRate.Free;
  FStoredSmoothingValue.Free;
  FTable.Free;
  inherited;
end;

function TStructure.GetControlOffsetCriterion: double;
begin
  result := StoredControlOffsetCriterion.Value;
end;

function TStructure.GetCriticalTabFileName: string;
var
  LocalModel: TCustomModel;
begin
  Result := FullCriticalTabFileName;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    result := ExtractRelativePath(LocalModel.ModelFileName, Result);
  end;
end;

function TStructure.GetCriticalValue: double;
begin
  result := StoredCriticalValue.Value;
end;

function TStructure.GetCulvertLength: double;
begin
  result := StoredCulvertLength.Value
end;

function TStructure.GetCulvertRise: double;
begin
  result := StoredCulvertRise.Value
end;

function TStructure.GetCulvertRoughness: double;
begin
  result := StoredCulvertRoughness.Value
end;

function TStructure.GetDischargeTabFile: string;
var
  LocalModel: TCustomModel;
  ModelFileName: string;
begin
  Result := FullDischargeTabFile;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ModelFileName := LocalModel.ModelFileName;
    result := ExtractRelativePath(ModelFileName, Result);
  end;
end;

function TStructure.GetDownstreamCulvertLength: double;
begin
  result := FStoredDownstreamCulvertLength.Value;
end;

function TStructure.GetDownstreamInvertElevation: double;
begin
  result := StoredDownstreamInvertElevation.Value
end;

function TStructure.GetFullCriticalTabFileName: string;
begin
  result := FFullCriticalTabFileName;
end;

function TStructure.GetFullDischargeTabFile: string;
begin
  result := FFullDischargeTabFile;
end;

function TStructure.GetFullInitialFlowRateTabFile: string;
begin
  result := FFullInitialFlowRateTabFile;
end;

function TStructure.GetInitialFlowRateOrGateOpening: double;
begin
  result := StoredInitialFlowRateOrGateOpening.Value
end;

function TStructure.GetInitialFlowRateTabFile: string;
var
  LocalModel: TCustomModel;
  ModelFileName: string;
begin
  Result := FullInitialFlowRateTabFile;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    ModelFileName := LocalModel.ModelFileName;
    result := ExtractRelativePath(ModelFileName, Result);
  end;
end;

function TStructure.GetInvertElevation: double;
begin
  result := StoredInvertElevation.Value
end;

function TStructure.GetMaximumControlRate: double;
begin
  result := StoredMaximumControlRate.Value;
end;

function TStructure.GetOrificeDischargeCoefficient: double;
begin
  result := StoredOrificeDischargeCoefficient.Value
end;

function TStructure.GetSmoothingValue: Double;
begin
  result := StoredSmoothingValue.Value;
end;

function TStructure.GetStartingControlRate: double;
begin
  result := StoredStartingControlRate.Value;
end;

function TStructure.GetSubmergenceExponent: double;
begin
  result := StoredSubmergenceExponent.Value
end;

function TStructure.GetWeirDischargeCoefficient: double;
begin
  result := StoredWeirDischargeCoefficient.Value
end;

function TStructure.GetWidth: double;
begin
  result := StoredWidth.Value
end;

function TStructure.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherStructure: TStructure;
begin
  result := (AnotherItem is TStructure);
  if result then
  begin
    OtherStructure := TStructure(AnotherItem);
    result := (Name = OtherStructure.Name)
      and (Reach = OtherStructure.Reach)
      and (ConnectedReach = OtherStructure.ConnectedReach)
      and (StructureRestrictions = OtherStructure.StructureRestrictions)
      and (StructureType = OtherStructure.StructureType)
      and (WeirDischargeCoefficient = OtherStructure.WeirDischargeCoefficient)
      and (OrificeDischargeCoefficient = OtherStructure.OrificeDischargeCoefficient)
      and (SubmergenceExponent = OtherStructure.SubmergenceExponent)
      and (InvertElevation = OtherStructure.InvertElevation)
      and (DownstreamInvertElevation = OtherStructure.DownstreamInvertElevation)
      and (Width = OtherStructure.Width)
      and (CulvertType = OtherStructure.CulvertType)
      and (CulvertRise = OtherStructure.CulvertRise)
      and (SpecifyCulvertLengths = OtherStructure.SpecifyCulvertLengths)
      and (CulvertLength = OtherStructure.CulvertLength)
      and (DownstreamCulvertLength = OtherStructure.DownstreamCulvertLength)
      and (CulvertRoughness = OtherStructure.CulvertRoughness)
      and (InitialFlowRateOrGateOpening = OtherStructure.InitialFlowRateOrGateOpening)
      and (SfrSegment = OtherStructure.SfrSegment)
      and (SfrReach = OtherStructure.SfrReach)
      and (ControlType = OtherStructure.ControlType)
      and (ControlReach = OtherStructure.ControlReach)
      and (ConnectedControlReach = OtherStructure.ConnectedControlReach)
      and (ControlOperated = OtherStructure.ControlOperated)
      and (CriticalMethod = OtherStructure.CriticalMethod)
      and (FullCriticalTabFileName = OtherStructure.FullCriticalTabFileName)
      and (CriticalValue = OtherStructure.CriticalValue)
      and (ControlOffsetCriterion = OtherStructure.ControlOffsetCriterion)
      and (StartingControlRate = OtherStructure.StartingControlRate)
      and (MaximumControlRate = OtherStructure.MaximumControlRate)
      and (FullDischargeTabFile = OtherStructure.DischargeTabFile)
      and (InitialFlowRateMethod = OtherStructure.InitialFlowRateMethod)
      and (FullInitialFlowRateTabFile = OtherStructure.FullInitialFlowRateTabFile)
      and Table.IsSame(OtherStructure.Table)
      and Times.IsSame(OtherStructure.Times)
  end;
end;

procedure TStructure.SetConnectedConnectedReach(const Value: Integer);
begin
  SetIntegerProperty(FConnectedConnectedReach, Value);
end;

procedure TStructure.SetControlOffsetCriterion(const Value: double);
begin
  StoredControlOffsetCriterion.Value := Value;
end;

procedure TStructure.SetControlOperated(const Value: TControlOperated);
begin
  if FControlOperated <> Value then
  begin
    FControlOperated := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetControlReach(const Value: integer);
begin
  SetIntegerProperty(FControlReach, Value);
end;

procedure TStructure.SetControlType(const Value: TControlType);
begin
  if FControlType <> Value then
  begin
    FControlType := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetCriticalMethod(const Value: TSpecificationMethod);
begin
  if FCriticalMethod <> Value then
  begin
    FCriticalMethod := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetCriticalTabFileName(const Value: string);
begin
  FullCriticalTabFileName := ExpandFileName(Value);
end;

procedure TStructure.SetCriticalValue(const Value: double);
begin
  StoredCriticalValue.Value := Value;
end;

procedure TStructure.SetCulvertLength(const Value: double);
begin
  StoredCulvertLength.Value := Value;
end;

procedure TStructure.SetCulvertRise(const Value: double);
begin
  StoredCulvertRise.Value := Value;
end;

procedure TStructure.SetCulvertRoughness(const Value: double);
begin
  StoredCulvertRoughness.Value := Value;
end;

procedure TStructure.SetCulvertType(const Value: TCulvertType);
begin
  if FCulvertType <> Value then
  begin
    FCulvertType := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetDischargeTabFile(const Value: string);
begin
  FullDischargeTabFile := ExpandFileName(Value);
end;

procedure TStructure.SetDownstreamCulvertLength(const Value: double);
begin
  FStoredDownstreamCulvertLength.Value := Value;
end;

procedure TStructure.SetDownstreamInvertElevation(const Value: double);
begin
  StoredDownstreamInvertElevation.Value := Value;
end;

procedure TStructure.SetFullCriticalTabFileName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FFullCriticalTabFileName, Value);
end;

procedure TStructure.SetFullDischargeTabFile(const Value: string);
begin
  SetCaseSensitiveStringProperty(FFullDischargeTabFile, Value);
end;

procedure TStructure.SetFullInitialFlowRateTabFile(const Value: string);
begin
  SetCaseSensitiveStringProperty(FFullInitialFlowRateTabFile, Value);
end;

procedure TStructure.SetInitialFlowRateMethod(
  const Value: TSpecificationMethod);
begin
  if FInitialFlowRateMethod <> Value then
  begin
    FInitialFlowRateMethod := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetInitialFlowRateOrGateOpening(const Value: double);
begin
  StoredInitialFlowRateOrGateOpening.Value := Value;
end;

procedure TStructure.SetInitialFlowRateTabFile(const Value: string);
begin
  FullInitialFlowRateTabFile := ExpandFileName(Value)
end;

procedure TStructure.SetInvertElevation(const Value: double);
begin
  StoredInvertElevation.Value := Value;
end;

procedure TStructure.SetMaximumControlRate(const Value: double);
begin
  StoredMaximumControlRate.Value := Value;
end;

procedure TStructure.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TStructure.SetOrificeDischargeCoefficient(const Value: double);
begin
  StoredOrificeDischargeCoefficient.Value := Value;
end;

procedure TStructure.SetReach(const Value: integer);
begin
  SetIntegerProperty(FReach, Value);
end;

procedure TStructure.SetSfrReach(const Value: integer);
begin
  SetIntegerProperty(FSfrReach, Value);
end;

procedure TStructure.SetSfrSegment(const Value: integer);
begin
  SetIntegerProperty(FSfrSegment, Value);
end;

procedure TStructure.SetSmoothingMethod(const Value: TSmoothingMethod);
begin
  if FSmoothingMethod <> Value then
  begin
    FSmoothingMethod := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetSmoothingValue(const Value: Double);
begin
  StoredSmoothingValue.Value := Value;
end;

procedure TStructure.SetSpecifyCulvertLengths(const Value: Boolean);
begin
  SetBooleanProperty(FSpecifyCulvertLengths, Value);
end;

procedure TStructure.SetStartingControlRate(const Value: double);
begin
  StoredStartingControlRate.Value := Value;
end;

procedure TStructure.SetStoredControlOffsetCriterion(const Value: TRealStorage);
begin
  FStoredControlOffsetCriterion.Assign(Value);
end;

procedure TStructure.SetStoredCriticalValue(const Value: TRealStorage);
begin
  FStoredCriticalValue.Assign(Value);
end;

procedure TStructure.SetStoredCulvertLength(const Value: TRealStorage);
begin
  FStoredCulvertLength.Assign(Value);
end;

procedure TStructure.SetStoredCulvertRise(const Value: TRealStorage);
begin
  FStoredCulvertRise.Assign(Value);
end;

procedure TStructure.SetStoredCulvertRoughness(const Value: TRealStorage);
begin
  FStoredCulvertRoughness.Assign(Value);
end;

procedure TStructure.SetStoredDownstreamCulvertLength(
  const Value: TRealStorage);
begin
  FStoredDownstreamCulvertLength.Assign(Value);
end;

procedure TStructure.SetStoredDownstreamInvertElevation(
  const Value: TRealStorage);
begin
  FStoredDownstreamInvertElevation.Assign(Value);
end;

procedure TStructure.SetStoredInitialFlowRateOrGateOpening(
  const Value: TRealStorage);
begin
  FStoredInitialFlowRateOrGateOpening.Assign(Value);
end;

procedure TStructure.SetStoredInvertElevation(const Value: TRealStorage);
begin
  FStoredInvertElevation.Assign(Value);
end;

procedure TStructure.SetStoredMaximumControlRate(const Value: TRealStorage);
begin
  FStoredMaximumControlRate.Assign(Value);
end;

procedure TStructure.SetStoredOrificeDischargeCoefficient(
  const Value: TRealStorage);
begin
  FStoredOrificeDischargeCoefficient.Assign(Value);
end;

procedure TStructure.SetStoredSmoothingValue(const Value: TRealStorage);
begin
  FStoredSmoothingValue.Assign(Value);
end;

procedure TStructure.SetStoredStartingControlRate(const Value: TRealStorage);
begin
  FStoredStartingControlRate.Assign(Value);
end;

procedure TStructure.SetStoredSubmergenceExponent(const Value: TRealStorage);
begin
  FStoredSubmergenceExponent.Assign(Value);
end;

procedure TStructure.SetStoredWeirDischargeCoefficient(
  const Value: TRealStorage);
begin
  FStoredWeirDischargeCoefficient.Assign(Value);
end;

procedure TStructure.SetStoredWidth(const Value: TRealStorage);
begin
  FStoredWidth.Assign(Value);
end;

procedure TStructure.SetStructureRestrictions(
  const Value: TStructureRestrictions);
begin
  if FStructureRestrictions <> Value then
  begin
    FStructureRestrictions := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetStructureType(const Value: TSwrStructureType);
begin
  if FStructureType <> Value then
  begin
    FStructureType := Value;
    InvalidateModel;
  end;
end;

procedure TStructure.SetSubmergenceExponent(const Value: double);
begin
  StoredSubmergenceExponent.Value := Value;
end;

procedure TStructure.SetTable(const Value: TStructureDischargeCollection);
begin
  FTable.Assign(Value);
end;

procedure TStructure.SetTimes(const Value: TStructureTimes);
begin
  FTimes.Assign(Value);
end;

procedure TStructure.SetConnectedReach(const Value: integer);
begin
  SetIntegerProperty(FConnectedReach, Value);
end;

procedure TStructure.SetWeirDischargeCoefficient(const Value: double);
begin
  StoredWeirDischargeCoefficient.Value := Value;
end;

procedure TStructure.SetWidth(const Value: double);
begin
  StoredWidth.Value := Value;
end;

function TStructure.UsedInPeriod(StartTime, EndTime: Double): boolean;
var
  TimeIndex: Integer;
  StructureTime: TStructureTimeItem;
begin
  result := False;
  for TimeIndex := 0 to Times.Count - 1 do
  begin
    StructureTime := Times[TimeIndex];
    if StartTime >= StructureTime.EndTime then
    begin
      continue;
    end;
    if (StructureTime.StartTime <= StartTime)
      and (StructureTime.EndTime > StartTime)
      and (StructureTime.EndTime >= EndTime) then
    begin
      result := StructureTime.Used;
      Exit;
    end;
  end;
end;

{ TStructureDischargeItem }

procedure TStructureDischargeItem.Assign(Source: TPersistent);
var
  SourceItem: TStructureDischargeItem;
begin
  if Source is TStructureDischargeItem then
  begin
    SourceItem := TStructureDischargeItem(Source);
    Discharge := SourceItem.Discharge;
    Elev := SourceItem.Elev;
  end
  else
  begin
    inherited;
  end;
end;

constructor TStructureDischargeItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredElevation := TRealStorage.Create;
  FStoredDischarge := TRealStorage.Create;
  FStoredElevation.OnChange := OnInvalidateModelEvent;
  FStoredDischarge.OnChange := OnInvalidateModelEvent;

end;

destructor TStructureDischargeItem.Destroy;
begin
  FStoredElevation.Free;
  FStoredDischarge.Free;
  inherited;
end;

function TStructureDischargeItem.GetDischarge: Double;
begin
  result := FStoredDischarge.Value;
end;

function TStructureDischargeItem.GetElev: Double;
begin
  result := FStoredElevation.Value;
end;

function TStructureDischargeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherDischItem: TStructureDischargeItem;
begin
  result := (AnotherItem is TStructureDischargeItem);
  if result then
  begin
    OtherDischItem := TStructureDischargeItem(AnotherItem);
    result := (Discharge = OtherDischItem.Discharge)
      and (Elev = OtherDischItem.Elev);
  end;
end;

procedure TStructureDischargeItem.SetDischarge(const Value: Double);
begin
  FStoredDischarge.Value := Value;
end;

procedure TStructureDischargeItem.SetElev(const Value: Double);
begin
  FStoredElevation.Value := Value;
end;

procedure TStructureDischargeItem.SetStoredDischarge(const Value: TRealStorage);
begin
  FStoredDischarge.Assign(Value);
end;

procedure TStructureDischargeItem.SetStoredElevation(const Value: TRealStorage);
begin
  FStoredElevation.Assign(Value);
end;

{ TStructureDischargeCollection }

function TStructureDischargeCollection.Add: TStructureDischargeItem;
begin
  result := inherited Add as TStructureDischargeItem;
end;

constructor TStructureDischargeCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TStructureDischargeItem, Model);
end;

function TStructureDischargeCollection.GetItems(
  Index: Integer): TStructureDischargeItem;
begin
  result := inherited Items[Index] as TStructureDischargeItem;
end;

procedure TStructureDischargeCollection.SetItems(Index: Integer;
  const Value: TStructureDischargeItem);
begin
  inherited Items[Index] := Value;
end;

{ TStructureCollection }

function TStructureCollection.Add: TStructure;
begin
  result := inherited Add as TStructure;
end;

constructor TStructureCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TStructure, Model);
end;

function TStructureCollection.GetItems(Index: Integer): TStructure;
begin
  result := inherited Items[Index] as TStructure;
end;

function TStructureCollection.GetStructureByName(AName: string): TStructure;
var
  index: Integer;
  AnItem: TStructure;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    AnItem := Items[index];
    if AnsiCompareText(AName, AnItem.Name) = 0 then
    begin
      result := AnItem;
      Break;
    end;
  end;
end;

procedure TStructureCollection.SetItems(Index: Integer;
  const Value: TStructure);
begin
  inherited Items[Index] := Value;
end;

{ TStructureTimeItem }

procedure TStructureTimeItem.Assign(Source: TPersistent);
var
  TimeSource: TStructureTimeItem;
begin
  if Source is TStructureTimeItem then
  begin
    TimeSource := TStructureTimeItem(Source);
    StartTime := TimeSource.StartTime;
    EndTime := TimeSource.EndTime;
    Used := TimeSource.Used;
  end;
  inherited;
end;

constructor TStructureTimeItem.Create(Collection: TCollection);
begin
  inherited;
  FStoredStartTime := TRealStorage.Create;
  FStoredEndTime := TRealStorage.Create;
  FStoredStartTime.OnChange := OnInvalidateModelEvent;
  FStoredEndTime.OnChange := OnInvalidateModelEvent;
end;

destructor TStructureTimeItem.Destroy;
begin
  FStoredStartTime.Free;
  FStoredEndTime.Free;
  inherited;
end;

function TStructureTimeItem.GetEndTime: double;
begin
  result := FStoredEndTime.Value;
end;

function TStructureTimeItem.GetStartTime: double;
begin
  result := FStoredStartTime.Value;
end;

function TStructureTimeItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  TimeSource: TStructureTimeItem;
begin
  result := (AnotherItem is TStructureTimeItem);
  if result then
  begin
    TimeSource := TStructureTimeItem(AnotherItem);
    result := (StartTime = TimeSource.StartTime)
      and (EndTime = TimeSource.EndTime)
      and (Used = TimeSource.Used);
  end;

end;

procedure TStructureTimeItem.SetEndTime(const Value: double);
begin
  FStoredEndTime.Value := Value;
end;

procedure TStructureTimeItem.SetStartTime(const Value: double);
begin
  FStoredStartTime.Value := Value;
end;

procedure TStructureTimeItem.SetStoredEndTime(const Value: TRealStorage);
begin
  FStoredEndTime.Assign(Value);
end;

procedure TStructureTimeItem.SetStoredStartTime(const Value: TRealStorage);
begin
  FStoredStartTime.Assign(Value);
end;

procedure TStructureTimeItem.SetUsed(const Value: boolean);
begin
  SetBooleanProperty(FUsed, Value);
end;

{ TStructureTimes }

function TStructureTimes.Add: TStructureTimeItem;
begin
  result := inherited Add as TStructureTimeItem;
end;

constructor TStructureTimes.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TStructureTimeItem, Model);
end;

function TStructureTimes.GetItem(Index: Integer): TStructureTimeItem;
begin
  result := inherited Items[Index] as TStructureTimeItem;
end;

function TStructureTimes.Last: TStructureTimeItem;
begin
  Result := Items[Count-1];
end;

procedure TStructureTimes.SetItem(Index: Integer;
  const Value: TStructureTimeItem);
begin
  inherited Items[Index] := Value;
end;

end.
