unit Mt3dmsChemSpeciesUnit;

interface

uses
  OrderedCollectionUnit, DataSetUnit, Classes,
  GoPhastTypes, OrderedCollectionInterfaceUnit;

type
  TCustomChemSpeciesCollection = class;

  TChemSpeciesItem = class(TOrderedItem)
  private
    FName: string;
    FInitialConcDataArrayName: string;
    FSorbOrImmobInitialConcDataArrayName: string;
    FFirstSorbParamDataArrayName: string;
    FSecondSorbParamDataArrayName: string;
    FReactionRateDisolvedDataArrayName: string;
    FReactionRateSorbedDataArrayName: string;
    FHalfSaturationConstantDataArrayName: string;

    FInitialConcDisplayName: string;
    FSorbOrImmobInitialConcDisplayName: string;
    FFirstSorbParamDisplayName: string;
    FSecondSorbParamDisplayName: string;
    FReactionRateDisolvedDisplayName: string;
    FReactionRateSorbedDisplayName: string;
    FHalfSaturationConstantDisplayName: string;

    FInitialConcentrationFileName: string;
    FUseInitialConcentrationFile: boolean;
    FImmobilePartioningCoefficientDataArrayName: string;
    FImmobilePartioningCoefficientDisplayName: string;
    FUztInitialConcDataArrayName: string;
    FUztInitialConcDisplayName: string;
    FPorosityDataArrayName: string;
    FPorosityDataArrayDisplayName: string;
    FMobileDecayRateDataArrayName: string;
    FMobileDecayRateDataArrayDisplayName: string;
    FMobileSorbedDecayRateDataArrayName: string;
    FMobileSorbedDecayRateDataArrayDisplayName: string;
    FMobileDistCoefDataArrayName: string;
    FMobileDistCoefDataArrayDisplayName: string;
    FMobileBulkDensityDataArrayName: string;
    FMobileFreundlichExponentDataArrayName: string;
    FMobileFreundlichExponentDataArrayDisplayName: string;
    FMobileSorptionCapacityDataArrayName: string;
    FMobileSorptionCapacityDataArrayDisplayName: string;
    FMobileBulkDensityDataArrayDisplayName: string;
    FImmobileDecaySorbed: TStringList;
    FImmobileInitialConcentrations: TStringList;
    FImmobileDistCoeficients: TStringList;
    FImmobileDecay: TStringList;
    FImmobileBulkDensities: TStringList;
    FImmobilePorosities: TStringList;
    FImmobileVolumeFractions: TStringList;
    FImmobileMassTransferRates: TStringList;
    FStoredRefConcentration: TRealStorage;
    FStoredDensitySlope: TRealStorage;
    procedure SetName(const Value: string); virtual;
    procedure UpdateDataArray(OnDataSetUsed: TObjectUsedEvent;
      const OldDataArrayName, NewName, NewDisplayName, NewFormula,
      AssociatedDataSets: string; ShouldCreate: boolean;
      const Classification: string);
    function Collection: TCustomChemSpeciesCollection;
    procedure RenameDependents(NewName: string);
    procedure UpdateAllDataArrays;
    procedure SetInitialConcentrationFileName(const Value: string);
    procedure SetUseInitialConcentrationFile(const Value: boolean);
    procedure SetInitialConcDataArrayName(const NewName: string);
    procedure SetSorbOrImmobInitialConcDataArrayName(const NewName: string);
    procedure SetFirstSorbParamDataArrayName(const NewName: string);
    procedure SetSecondSorbParamDataArrayName(const NewName: string);
    procedure SetReactionRateDisolvedDataArrayName(const NewName: string);
    procedure SetReactionRateSorbedDataArrayName(const NewName: string);
    procedure SetHalfSaturationConstantDataArrayName(const NewName: string);
    procedure SetImmobilePartioningCoefficientDataArrayName(const NewName: string);
    procedure SetUztInitialConcDataArrayName(const NewName: string);
    procedure SetPorosityDataArrayName(const NewName: string);
    procedure SetMobileDecayRateDataArrayName(const NewName: string);
    procedure SetMobileSorbedDecayRateDataArrayName(const NewName: string);
    procedure SetMobileDistCoefDataArrayName(const NewName: string);
    procedure SetMobileBulkDensityDataArrayName(const NewName: string);
    procedure SetMobileFreundlichExponentDataArrayName(const NewName: string);
    procedure SetMobileSorptionCapacityDataArrayName(const NewName: string);
    procedure SetImmobileBulkDensities(const Value: TStringList);
    procedure SetImmobileDecay(const Value: TStringList);
    procedure SetImmobileDecaySorbed(const Value: TStringList);
    procedure SetImmobileDistCoeficients(const Value: TStringList);
    procedure SetImmobileInitialConcentrations(const Value: TStringList);
    procedure SetImmobileMassTransferRates(const Value: TStringList);
    procedure SetImmobilePorosities(const Value: TStringList);
    procedure SetImmobileVolumeFractions(const Value: TStringList);
    procedure SetStoredDensitySlope(const Value: TRealStorage);
    procedure SetStoredRefConcentration(const Value: TRealStorage);
    function GetDensitySlope: double;
    function GetRefConcentration: double;
    procedure SetRefConcentration(const Value: double);
    procedure SetDensitySlope(const Value: double);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetIndex(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    // Buoyancy package drhodc
    property DensitySlope: double read GetDensitySlope
      write SetDensitySlope;
    // Buoyancy package crhoref
    property RefConcentration: double read GetRefConcentration
      write SetRefConcentration;
  published
    property Name: string read FName write SetName;
    // BTN package, SCONC, GWT IC package, STRT
    property InitialConcDataArrayName: string read FInitialConcDataArrayName
      write SetInitialConcDataArrayName;
    // RCT package, SRCONC
    property SorbOrImmobInitialConcDataArrayName: string
      read FSorbOrImmobInitialConcDataArrayName
      write SetSorbOrImmobInitialConcDataArrayName;
    // RCT package, SP1
    property FirstSorbParamDataArrayName: string
      read FFirstSorbParamDataArrayName write SetFirstSorbParamDataArrayName;
    // RCT package, SP2
    property SecondSorbParamDataArrayName: string
      read FSecondSorbParamDataArrayName write SetSecondSorbParamDataArrayName;
    // RCT package, RC1
    property ReactionRateDisolvedDataArrayName: string
      read FReactionRateDisolvedDataArrayName
      write SetReactionRateDisolvedDataArrayName;
    // RCT package, RC2
    property ReactionRateSorbedDataArrayName: string
      read FReactionRateSorbedDataArrayName write
      SetReactionRateSorbedDataArrayName;
    // RCT package, RC3
    property HalfSaturationConstantDataArrayName: string
      read FHalfSaturationConstantDataArrayName
      write SetHalfSaturationConstantDataArrayName;
    // RCT package, SP1IM
    property ImmobilePartioningCoefficientDataArrayName:string
      read FImmobilePartioningCoefficientDataArrayName
      write SetImmobilePartioningCoefficientDataArrayName;
   // MF6 UZF GWT initial Concentration
    property UztInitialConcDataArrayName:string
      read FUztInitialConcDataArrayName
      write SetUztInitialConcDataArrayName;
    property UseInitialConcentrationFile: boolean
      read FUseInitialConcentrationFile write SetUseInitialConcentrationFile;
    property InitialConcentrationFileName: string
      read FInitialConcentrationFileName write SetInitialConcentrationFileName;
    //MST package, POROSITY
    property PorosityDataArrayName: string read FPorosityDataArrayName
      write SetPorosityDataArrayName;
    //MST package, DECAY
    property MobileDecayRateDataArrayName: string read FMobileDecayRateDataArrayName
      write SetMobileDecayRateDataArrayName;
    //MST package, DECAY_SORBED
    property MobileSorbedDecayRateDataArrayName: string read FMobileSorbedDecayRateDataArrayName
      write SetMobileSorbedDecayRateDataArrayName;
    //MST package, DISTCOEF
    property MobileDistCoefDataArrayName: string read FMobileDistCoefDataArrayName
      write SetMobileDistCoefDataArrayName;
    //MST package, BULK_DENSITY
    property MobileBulkDensityDataArrayName: string read FMobileBulkDensityDataArrayName
      write SetMobileBulkDensityDataArrayName;
    //MST package, SP2
    property MobileFreundlichExponentDataArrayName: string read FMobileFreundlichExponentDataArrayName
      write SetMobileFreundlichExponentDataArrayName;
    //MST package, SP2
    property MobileSorptionCapacityDataArrayName: string read FMobileSorptionCapacityDataArrayName
      write SetMobileSorptionCapacityDataArrayName;
    //IST package, CIM
    property ImmobileInitialConcentrations: TStringList
      read FImmobileInitialConcentrations write SetImmobileInitialConcentrations;
    //IST package, THETAIM
    property ImmobilePorosities: TStringList read FImmobilePorosities
      write SetImmobilePorosities;
    // IST package, VOLFRAC
    property ImmobileVolumeFractions: TStringList read FImmobileVolumeFractions
      write SetImmobileVolumeFractions;
    //IST package, ZETAIM
    property ImmobileMassTransferRates: TStringList
      read FImmobileMassTransferRates write SetImmobileMassTransferRates;
    //IST package, DECAY
    property ImmobileDecay: TStringList read FImmobileDecay
      write SetImmobileDecay;
    //IST package, DECAY_SORBED
    property ImmobileDecaySorbed: TStringList read FImmobileDecaySorbed
      write SetImmobileDecaySorbed;
    //IST package, BULK_DENSITY
    property ImmobileBulkDensities: TStringList read FImmobileBulkDensities
      write SetImmobileBulkDensities;
    //IST package, DISTCOEF
    property ImmobileDistCoeficients: TStringList read FImmobileDistCoeficients
      write SetImmobileDistCoeficients;
    // Buoyancy package drhodc
    property StoredDensitySlope: TRealStorage read FStoredDensitySlope
      write SetStoredDensitySlope
    {$IFNDEF Buoyancy}
      stored False
    {$ENDIF}
      ;
    // Buoyancy package crhoref
    property StoredRefConcentration: TRealStorage read FStoredRefConcentration
      write SetStoredRefConcentration
    {$IFNDEF Buoyancy}
      stored False
    {$ENDIF}
      ;
  end;

  TCustomChemSpeciesCollection= class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: Integer): TChemSpeciesItem;
    procedure SetItem(Index: Integer; const Value: TChemSpeciesItem);
  public
    property Items[Index: Integer] : TChemSpeciesItem read GetItem
      write SetItem; default;
    function Add: TChemSpeciesItem;
    function Insert(Index: integer): TChemSpeciesItem;
    procedure UpdateDataArrays; virtual;
    procedure Loaded;
    function IndexOfName(const AName: string): integer;
  end;

  TChemSpeciesCollection = class(TCustomChemSpeciesCollection)
  public
    constructor Create(Model: IModelForTOrderedCollection);
  end;

  TMobileChemSpeciesItem = class(TChemSpeciesItem)
  private
    FDiffusionCoefDisplayName: string;
    FDiffusionCoefDataArrayName: string;
    procedure SetDiffusionCoefDataArrayName(const NewName: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetName(const Value: string); override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    // DSP Package, DMCOEF
    property DiffusionCoefDataArrayName: string read FDiffusionCoefDataArrayName
      write SetDiffusionCoefDataArrayName;
  end;

  TMobileChemSpeciesCollection = class(TCustomChemSpeciesCollection)
  private
    function GetItem(Index: Integer): TMobileChemSpeciesItem;
    procedure SetItem(Index: Integer; const Value: TMobileChemSpeciesItem);
  public
    constructor Create(Model: IModelForTOrderedCollection);
    property Items[Index: Integer] : TMobileChemSpeciesItem read GetItem
      write SetItem; default;
    function Add: TMobileChemSpeciesItem;
    procedure UpdateDataArrays; override;
    procedure UpdateAllDataArrays;
    function GetItemByName(AName: string): TMobileChemSpeciesItem;
  end;

implementation

uses
  PhastModelUnit, RbwParser, SysUtils, ModflowPackageSelectionUnit,
  frmGoPhastUnit, ScreenObjectUnit, Mt3dmsChemUnit, Mt3dmsTobUnit,
  Mt3dUztRchUnit, Mt3dUztSatEtUnit, Mt3dUztUnsatEtUnit, Mt3dUzfSeepageUnit,
  Mt3dLktUnit, Mt3dSftUnit, ModflowPackagesUnit,
  ModflowGwtSpecifiedConcUnit, UpdateDataArrayUnit;

const
  kInitConcPrefix = 'Initial_Concentration_';
  kSorbPrefix = 'Sorbed_Phase_Initial_Conc_';
  kImmobPrefix = 'Immobile_Phase_Initial_Conc_';
  kFirstSorbParamPrefix = 'Sorption_Parameter1_';
  kSecondSorbParamPrefix = 'Sorption_Parameter2_';
  kRC1Prefix = 'Reaction_Rate_Dissolved_Phase_';
  kRC2Prefix = 'Reaction_Rate_Sorbed_Phase_';
  kDiffCoefPrefix = 'Diffusion_Coefficient_';
  kRC3Prefix = 'Half_Saturation_Constant_';
  kSP1IMPrefix = 'Immobile_Partioning_Coefficient_';
  kUztStartConct = 'UZT_Unsaturated_Initial_Conc_';
  KPorosity = 'Mobile_Porosity_';
  KMobileDecayRate = 'Mobile_Decay_Rate_';
  KMobileSorbedDecayRate = 'Mobile_Sorbed_Decay_Rate_';
  KMobileDistCoef = 'Mobile_Distribution_Coefficient_';
  KMobileBulkDensity = 'Mobile_Bulk_Density_';
  KFreundlichExponent = 'Mobile_Freundlich_Exponent_';
  KSorptionCapacity = 'Mobile_Sorption_Capacity_';
  KImmobileBulkDensity = 'ImmobileBulkDensity_%0:s_%1:d';
  KrImmobileInitialConce = 'ImmobileInitialConcentration_%0:s_%1:d';
  KImmobilePorosity = 'ImmobilePorosity_%0:s_%1:d';
  KImmobileMassTransfer = 'ImmobileMassTransferRate_%0:s_%1:d';
  KImmobileDecay = 'ImmobileDecay_%0:s_%1:d';
  KImmobileDecaySorbed = 'ImmobileDecaySorbed_%0:s_%1:d';
  KImmobileDistCoeficie = 'ImmobileDistCoeficient_%0:s_%1:d';
  KImmobileVolFrac = 'ImmobileVolumeFraction_%0:s_%1:d';

resourcestring
  StrInitConcPrefix = kInitConcPrefix;
  StrSorbPrefix = kSorbPrefix;
  StrImmobPrefix = kImmobPrefix;
  StrFirstSorbParamPrefix = kFirstSorbParamPrefix;
  StrSecondSorbParamPrefix = kSecondSorbParamPrefix;
  StrRC1Prefix = kRC1Prefix;
  StrRC2Prefix = kRC2Prefix;
  StrRC3Prefix = kRC3Prefix;
  StrDiffCoefPrefix = kDiffCoefPrefix;
  StrSP1IMPrefix = kSP1IMPrefix;
  StrUztStartConct = kUztStartConct;
  StrPorosity = kPorosity;
  StrMobileDecayRate = KMobileDecayRate;
  StrMobileSorbedDecayRate = KMobileSorbedDecayRate;
  StrMobileDistCoef = KMobileDistCoef;
  StrMobileBulkDensity = KMobileBulkDensity;
  StrFreundlichExponent = KFreundlichExponent;
  StrSorptionCapacity = KSorptionCapacity;

  { TChemSpeciesItem }

procedure TChemSpeciesItem.Assign(Source: TPersistent);
var
  SourceChem: TChemSpeciesItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TChemSpeciesItem then
  begin
    SourceChem := TChemSpeciesItem(Source);
    // first copy data array names
    FInitialConcDataArrayName := SourceChem.InitialConcDataArrayName;
    FSorbOrImmobInitialConcDataArrayName :=
      SourceChem.SorbOrImmobInitialConcDataArrayName;
    FFirstSorbParamDataArrayName :=
      SourceChem.FirstSorbParamDataArrayName;
    FSecondSorbParamDataArrayName :=
      SourceChem.SecondSorbParamDataArrayName;
    FReactionRateDisolvedDataArrayName :=
      SourceChem.ReactionRateDisolvedDataArrayName;
    FReactionRateSorbedDataArrayName :=
      SourceChem.ReactionRateSorbedDataArrayName;

    FHalfSaturationConstantDataArrayName :=
      SourceChem.HalfSaturationConstantDataArrayName;

    // then change the name of the chem species
    Name := SourceChem.Name;

    // then update the data array names
    FInitialConcDataArrayName := '';
    InitialConcDataArrayName := SourceChem.InitialConcDataArrayName;

    FSorbOrImmobInitialConcDataArrayName := '';
    SorbOrImmobInitialConcDataArrayName :=
      SourceChem.SorbOrImmobInitialConcDataArrayName;

    FFirstSorbParamDataArrayName := '';
    FirstSorbParamDataArrayName :=
      SourceChem.FirstSorbParamDataArrayName;

    FSecondSorbParamDataArrayName := '';
    SecondSorbParamDataArrayName :=
      SourceChem.SecondSorbParamDataArrayName;

    FReactionRateDisolvedDataArrayName := '';
    ReactionRateDisolvedDataArrayName :=
      SourceChem.ReactionRateDisolvedDataArrayName;

    FReactionRateSorbedDataArrayName := '';
    ReactionRateSorbedDataArrayName :=
      SourceChem.ReactionRateSorbedDataArrayName;

    FHalfSaturationConstantDataArrayName :='';
    HalfSaturationConstantDataArrayName :=
      SourceChem.HalfSaturationConstantDataArrayName;

    FImmobilePartioningCoefficientDataArrayName :='';
    ImmobilePartioningCoefficientDataArrayName :=
      SourceChem.ImmobilePartioningCoefficientDataArrayName;

    FUztInitialConcDataArrayName := '';
    UztInitialConcDataArrayName :=
      SourceChem.UztInitialConcDataArrayName;

    UseInitialConcentrationFile := SourceChem.UseInitialConcentrationFile;
    InitialConcentrationFileName := SourceChem.InitialConcentrationFileName;

    PorosityDataArrayName := '';
    PorosityDataArrayName := SourceChem.PorosityDataArrayName;

    MobileDecayRateDataArrayName := '';
    MobileDecayRateDataArrayName := SourceChem.MobileDecayRateDataArrayName;

    MobileSorbedDecayRateDataArrayName := '';
    MobileSorbedDecayRateDataArrayName := SourceChem.MobileSorbedDecayRateDataArrayName;

    MobileDistCoefDataArrayName := '';
    MobileDistCoefDataArrayName := SourceChem.MobileDistCoefDataArrayName;

    MobileBulkDensityDataArrayName := '';
    MobileBulkDensityDataArrayName := SourceChem.MobileBulkDensityDataArrayName;

    MobileFreundlichExponentDataArrayName := '';
    MobileFreundlichExponentDataArrayName := SourceChem.MobileFreundlichExponentDataArrayName;

    MobileSorptionCapacityDataArrayName := '';
    MobileSorptionCapacityDataArrayName := SourceChem.MobileSorptionCapacityDataArrayName;

//    ImmobileInitialConcentrations.Clear;
    ImmobileInitialConcentrations := SourceChem.ImmobileInitialConcentrations;
//    ImmobilePorosities.Clear;
    ImmobilePorosities := SourceChem.ImmobilePorosities;
    ImmobileVolumeFractions := SourceChem.ImmobileVolumeFractions;

//    ImmobileMassTransferRates.Clear;
    ImmobileMassTransferRates := SourceChem.ImmobileMassTransferRates;
//    ImmobileDecay.Clear;
    ImmobileDecay := SourceChem.ImmobileDecay;
//    ImmobileDecaySorbed.Clear;
    ImmobileDecaySorbed := SourceChem.ImmobileDecaySorbed;
//    ImmobileBulkDensities.Clear;
    ImmobileBulkDensities := SourceChem.ImmobileBulkDensities;
//    ImmobileDistCoeficients.Clear;
    ImmobileDistCoeficients := SourceChem.ImmobileDistCoeficients;

    DensitySlope := SourceChem.DensitySlope;
    RefConcentration := SourceChem.RefConcentration;
  end;
  inherited;
end;

procedure TChemSpeciesItem.UpdateAllDataArrays;
var
  TempNames: TStringList;
  LocalModel: TPhastModel;
  IstPackage: TGwtIstPackage;
  DomainIndex: Integer;
begin
  if Collection.Model <> nil then
  begin
    // Reassigning the name will cause the data set to be created if it is
    // needed.
    InitialConcDataArrayName := InitialConcDataArrayName;
    SorbOrImmobInitialConcDataArrayName := SorbOrImmobInitialConcDataArrayName;
    FirstSorbParamDataArrayName := FirstSorbParamDataArrayName;
    SecondSorbParamDataArrayName := SecondSorbParamDataArrayName;
    ReactionRateDisolvedDataArrayName := ReactionRateDisolvedDataArrayName;
    ReactionRateSorbedDataArrayName := ReactionRateSorbedDataArrayName;
    HalfSaturationConstantDataArrayName := HalfSaturationConstantDataArrayName;
    ImmobilePartioningCoefficientDataArrayName := ImmobilePartioningCoefficientDataArrayName;
    UztInitialConcDataArrayName := UztInitialConcDataArrayName;
    PorosityDataArrayName := PorosityDataArrayName;
    MobileDecayRateDataArrayName := MobileDecayRateDataArrayName;
    MobileSorbedDecayRateDataArrayName := MobileSorbedDecayRateDataArrayName;
    MobileDistCoefDataArrayName := MobiledISTcOEFDataArrayName;
    MobileBulkDensityDataArrayName := MobileBulkDensityDataArrayName;
    MobileFreundlichExponentDataArrayName := MobileFreundlichExponentDataArrayName;
    MobileSorptionCapacityDataArrayName := MobileSorptionCapacityDataArrayName;

    TempNames := TStringList.Create;
    try
      LocalModel := Collection.Model as TPhastModel;
      if LocalModel.GwtUsed then
      begin
        IstPackage := LocalModel.ModflowPackages.GwtPackages[Index].GwtIst;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KrImmobileInitialConce, [Name, DomainIndex+1]))
        end;
        ImmobileInitialConcentrations := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobilePorosity, [Name, DomainIndex+1]))
        end;
        ImmobilePorosities := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileVolFrac, [Name, DomainIndex+1]))
        end;
        ImmobileVolumeFractions := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileMassTransfer, [Name, DomainIndex+1]))
        end;
        ImmobileMassTransferRates := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileDecay, [Name, DomainIndex+1]))
        end;
        ImmobileDecay := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileDecaySorbed, [Name, DomainIndex+1]))
        end;
        ImmobileDecaySorbed := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileBulkDensity, [Name, DomainIndex+1]))
        end;
        ImmobileBulkDensities := TempNames;

        TempNames.Clear;
        for DomainIndex := 0 to IstPackage.IstPackageProperties.Count - 1 do
        begin
          TempNames.Add(Format(KImmobileDistCoeficie, [Name, DomainIndex+1]))
        end;
        ImmobileDistCoeficients := TempNames;
      end;
    finally
      TempNames.Free;
    end;
  end;
end;

procedure TChemSpeciesItem.UpdateDataArray(OnDataSetUsed: TObjectUsedEvent;
  const OldDataArrayName, NewName, NewDisplayName, NewFormula,
  AssociatedDataSets: string; ShouldCreate: boolean;
  const Classification: string);
var
  UpdataDat: TUpdataDataArrayRecord;
begin
  if Collection.Model <> nil then
  begin
    UpdataDat.Model := Collection.Model as TCustomModel;
    UpdataDat.OnDataSetUsed := OnDataSetUsed;
    UpdataDat.OldDataArrayName := OldDataArrayName;
    UpdataDat.NewName := NewName;
    UpdataDat.NewDisplayName := NewDisplayName;
    UpdataDat.NewFormula := NewFormula;
    UpdataDat.AssociatedDataSets := AssociatedDataSets;
    UpdataDat.ShouldCreate := ShouldCreate;
    UpdataDat.Classification := Classification;
    UpdataDat.Orientation := dso3D;
    UpdataDat.DataType := rdtDouble;

    UpdateOrCreateDataArray(UpdataDat);
  end;
end;

function TChemSpeciesItem.Collection: TCustomChemSpeciesCollection;
begin
  result := inherited Collection as TCustomChemSpeciesCollection;
end;

constructor TChemSpeciesItem.Create(Collection: TCollection);
var
  LocalModel: TCustomModel;
  SpeciesIndex: integer;
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  Mt3dUzfRechConc: TMt3dUztRchConcBoundary;
  Mt3dUztSatEtConcBoundary: TMt3dUztSatEtConcBoundary;
  Mt3dUztUnsatEtConcBoundary: TMt3dUztUnsatEtConcBoundary;
  Mt3dUzSsmSinkConcBoundary: TMt3dUzSsmSinkConcBoundary;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
begin
  inherited;
  FName := 'default name';
  FImmobileDecaySorbed := TStringList.Create;
  FImmobileInitialConcentrations := TStringList.Create;
  FImmobileDistCoeficients := TStringList.Create;
  FImmobileDecay := TStringList.Create;
  FImmobileBulkDensities := TStringList.Create;
  FImmobilePorosities := TStringList.Create;
  FImmobileVolumeFractions := TStringList.Create;
  FImmobileMassTransferRates := TStringList.Create;

  FStoredDensitySlope := TRealStorage.Create(OnInvalidateModelEvent);
  FStoredRefConcentration := TRealStorage.Create(OnInvalidateModelEvent);

  if (Model <> nil) and not (csLoading in (Model as TComponent).ComponentState) then
  begin
    LocalModel := Model as TCustomModel;
    SpeciesIndex := LocalModel.MobileComponents.IndexOf(self);
    if SpeciesIndex < 0 then
    begin
      SpeciesIndex := LocalModel.ImmobileComponents.IndexOf(self);
      Assert(SpeciesIndex >= 0);
      SpeciesIndex := LocalModel.MobileComponents.Count + SpeciesIndex
    end;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];

      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;
      if ConcBoundary <> nil then
      begin
        ConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dUzfRechConc := ScreenObject.Mt3dUzfRechConc;
      if Mt3dUzfRechConc <> nil then
      begin
        Mt3dUzfRechConc.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dUztSatEtConcBoundary := ScreenObject.Mt3dUztSatEtConcBoundary;
      if Mt3dUztSatEtConcBoundary <> nil then
      begin
        Mt3dUztSatEtConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dUztUnsatEtConcBoundary := ScreenObject.Mt3dUztUnsatEtConcBoundary;
      if Mt3dUztUnsatEtConcBoundary <> nil then
      begin
        Mt3dUztUnsatEtConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dUzSsmSinkConcBoundary := ScreenObject.Mt3dUzSsmSinkConcBoundary;
      if Mt3dUzSsmSinkConcBoundary <> nil then
      begin
        Mt3dUzSsmSinkConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dLktConcBoundary := ScreenObject.Mt3dLktConcBoundary;
      if Mt3dLktConcBoundary <> nil then
      begin
        Mt3dLktConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;

      Mt3dSftConcBoundary := ScreenObject.Mt3dSftConcBoundary;
      if Mt3dSftConcBoundary <> nil then
      begin
        Mt3dSftConcBoundary.InsertNewSpecies(SpeciesIndex, Name);
      end;
    end;
  end;
end;

destructor TChemSpeciesItem.Destroy;
var
  LocalModel: TCustomModel;
  SpeciesIndex: Integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  TransObservations: TMt3dmsTransObservations;
  Mt3dUzfRechConc: TMt3dUztRchConcBoundary;
  Mt3dUztSatEtConcBoundary: TMt3dUztSatEtConcBoundary;
  Mt3dUztUnsatEtConcBoundary: TMt3dUztUnsatEtConcBoundary;
  Mt3dUzSsmSinkConcBoundary: TMt3dUzSsmSinkConcBoundary;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
  GwtCncBoundary: TCncBoundary;
  GwtSrcBoundary: TSrcBoundary;
begin
  FStoredDensitySlope.Free;
  FStoredRefConcentration.Free;

  FImmobileDecaySorbed.Free;
  FImmobileInitialConcentrations.Free;
  FImmobileDistCoeficients.Free;
  FImmobileDecay.Free;
  FImmobileBulkDensities.Free;
  FImmobileVolumeFractions.Free;
  FImmobilePorosities.Free;
  FImmobileMassTransferRates.Free;

  if (Model <> nil) and not (csDestroying in (Model as TComponent).ComponentState)
    and not (Model as TCustomModel).Clearing then
  begin
    LocalModel := Model as TCustomModel;
    SpeciesIndex := LocalModel.MobileComponents.IndexOf(self);
    if SpeciesIndex < 0 then
    begin
      SpeciesIndex := LocalModel.ImmobileComponents.IndexOf(self);
      Assert(SpeciesIndex >= 0);
      SpeciesIndex := LocalModel.MobileComponents.Count + SpeciesIndex
    end;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      ConcBoundary := ScreenObject.Mt3dmsConcBoundary;

      if ConcBoundary <> nil then
      begin
        ConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      TransObservations := ScreenObject.Mt3dmsTransObservations;
      if TransObservations <> nil then
      begin
        TransObservations.DeleteSpecies(Name);
      end;

      Mt3dUzfRechConc := ScreenObject.Mt3dUzfRechConc;
      if Mt3dUzfRechConc <> nil then
      begin
        Mt3dUzfRechConc.DeleteSpecies(SpeciesIndex);
      end;

      Mt3dUztSatEtConcBoundary := ScreenObject.Mt3dUztSatEtConcBoundary;
      if Mt3dUztSatEtConcBoundary <> nil then
      begin
        Mt3dUztSatEtConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      Mt3dUztUnsatEtConcBoundary := ScreenObject.Mt3dUztUnsatEtConcBoundary;
      if Mt3dUztUnsatEtConcBoundary <> nil then
      begin
        Mt3dUztUnsatEtConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      Mt3dUzSsmSinkConcBoundary := ScreenObject.Mt3dUzSsmSinkConcBoundary;
      if Mt3dUzSsmSinkConcBoundary <> nil then
      begin
        Mt3dUzSsmSinkConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      Mt3dLktConcBoundary := ScreenObject.Mt3dLktConcBoundary;
      if Mt3dLktConcBoundary <> nil then
      begin
        Mt3dLktConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      Mt3dSftConcBoundary := ScreenObject.Mt3dSftConcBoundary;
      if Mt3dSftConcBoundary <> nil then
      begin
        Mt3dSftConcBoundary.DeleteSpecies(SpeciesIndex);
      end;

      GwtCncBoundary := ScreenObject.GwtCncBoundary;
      if GwtCncBoundary <> nil then
      begin
        if GwtCncBoundary.ChemSpecies = Name then
        begin
          GwtCncBoundary.ChemSpecies := ''
        end;
      end;

      GwtSrcBoundary := ScreenObject.GwtSrcBoundary;
      if GwtSrcBoundary <> nil then
      begin
        if GwtSrcBoundary.ChemSpecies = Name then
        begin
          GwtSrcBoundary.ChemSpecies := ''
        end;
      end;
    end;
  end;

  inherited;
end;

function TChemSpeciesItem.GetDensitySlope: double;
begin
  result := StoredDensitySlope.Value;
end;

function TChemSpeciesItem.GetRefConcentration: double;
begin
  result := StoredRefConcentration.Value;
end;

function TChemSpeciesItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  ChemItem: TChemSpeciesItem;
  function SameStrings(Old, New: TStringList): Boolean;
  var
    index: Integer;
  begin
    Assert(Old <> nil);
    Assert(New <> nil);
    result := Old.Count = New.Count;
    if result then
    begin
      for index := 0 to Old.Count - 1 do
      begin
        result := Old[index] = New[index];
        if not result then
        begin
          break;
        end;
      end;
    end;
  end;
begin
  result := AnotherItem is TChemSpeciesItem;
  if result then
  begin
    ChemItem := TChemSpeciesItem(AnotherItem);
    result := (Name = ChemItem.Name)
      and (InitialConcDataArrayName = ChemItem.InitialConcDataArrayName)
      and (SorbOrImmobInitialConcDataArrayName = ChemItem.SorbOrImmobInitialConcDataArrayName)
      and (FirstSorbParamDataArrayName = ChemItem.FirstSorbParamDataArrayName)
      and (SecondSorbParamDataArrayName = ChemItem.SecondSorbParamDataArrayName)
      and (ReactionRateDisolvedDataArrayName = ChemItem.ReactionRateDisolvedDataArrayName)
      and (ReactionRateSorbedDataArrayName = ChemItem.ReactionRateSorbedDataArrayName)
      and (HalfSaturationConstantDataArrayName = ChemItem.HalfSaturationConstantDataArrayName)
      and (ImmobilePartioningCoefficientDataArrayName = ChemItem.ImmobilePartioningCoefficientDataArrayName)
      and (UztInitialConcDataArrayName = ChemItem.UztInitialConcDataArrayName)
      and (UseInitialConcentrationFile = ChemItem.UseInitialConcentrationFile)
      and (InitialConcentrationFileName = ChemItem.InitialConcentrationFileName)
      and (PorosityDataArrayName = ChemItem.PorosityDataArrayName)
      and (MobileDecayRateDataArrayName = ChemItem.MobileDecayRateDataArrayName)
      and (MobileSorbedDecayRateDataArrayName = ChemItem.MobileSorbedDecayRateDataArrayName)
      and (MobileDistCoefDataArrayName = ChemItem.MobileDistCoefDataArrayName)
      and (MobileBulkDensityDataArrayName = ChemItem.MobileBulkDensityDataArrayName)
      and (MobileFreundlichExponentDataArrayName = ChemItem.MobileFreundlichExponentDataArrayName)
      and (MobileSorptionCapacityDataArrayName = ChemItem.MobileSorptionCapacityDataArrayName)
      and SameStrings(ImmobileInitialConcentrations,  ChemItem.ImmobileInitialConcentrations)
      and SameStrings(ImmobilePorosities,  ChemItem.ImmobilePorosities)
      and SameStrings(ImmobileVolumeFractions,  ChemItem.FImmobileVolumeFractions)
      and SameStrings(ImmobileMassTransferRates,  ChemItem.ImmobileMassTransferRates)
      and SameStrings(ImmobileDecay,  ChemItem.ImmobileDecay)
      and SameStrings(ImmobileDecaySorbed,  ChemItem.ImmobileDecaySorbed)
      and SameStrings(ImmobileBulkDensities,  ChemItem.ImmobileBulkDensities)
      and SameStrings(ImmobileDistCoeficients,  ChemItem.ImmobileDistCoeficients)
  end;
end;

procedure TChemSpeciesItem.RenameDependents(NewName: string);
var
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  TransObservations: TMt3dmsTransObservations;
  Mt3dUztRechConc: TMt3dUztRchConcBoundary;
  Mt3dUztSatEtConcBoundary: TMt3dUztSatEtConcBoundary;
  Mt3dUztUnsatEtConcBoundary: TMt3dUztUnsatEtConcBoundary;
  Mt3dUzSsmSinkConcBoundary: TMt3dUzSsmSinkConcBoundary;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
begin
  if FName <> NewName then
  begin
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];

        ConcBoundary := AScreenObject.Mt3dmsConcBoundary;
        if ConcBoundary <> nil then
        begin
          ConcBoundary.RenameSpecies(FName, NewName);
        end;

        TransObservations := AScreenObject.Mt3dmsTransObservations;
        if TransObservations <> nil then
        begin
          TransObservations.RenameSpecies(FName, NewName);
        end;

        Mt3dUztRechConc := AScreenObject.Mt3dUzfRechConc;
        if Mt3dUztRechConc <> nil then
        begin
          Mt3dUztRechConc.RenameSpecies(FName, NewName);
        end;

        Mt3dUztSatEtConcBoundary := AScreenObject.Mt3dUztSatEtConcBoundary;
        if Mt3dUztSatEtConcBoundary <> nil then
        begin
          Mt3dUztSatEtConcBoundary.RenameSpecies(FName, NewName);
        end;

        Mt3dUztUnsatEtConcBoundary := AScreenObject.Mt3dUztUnsatEtConcBoundary;
        if Mt3dUztUnsatEtConcBoundary <> nil then
        begin
          Mt3dUztUnsatEtConcBoundary.RenameSpecies(FName, NewName);
        end;

        Mt3dUzSsmSinkConcBoundary := AScreenObject.Mt3dUzSsmSinkConcBoundary;
        if Mt3dUzSsmSinkConcBoundary <> nil then
        begin
          Mt3dUzSsmSinkConcBoundary.RenameSpecies(FName, NewName);
        end;

        Mt3dLktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
        if Mt3dLktConcBoundary <> nil then
        begin
          Mt3dLktConcBoundary.RenameSpecies(FName, NewName);
        end;

        Mt3dSftConcBoundary := AScreenObject.Mt3dSftConcBoundary;
        if Mt3dSftConcBoundary <> nil then
        begin
          Mt3dSftConcBoundary.RenameSpecies(FName, NewName);
        end;
      end;
    end;
  end;
end;

procedure TChemSpeciesItem.SetRefConcentration(const Value: double);
begin
  StoredRefConcentration.Value := Value;
end;

procedure TChemSpeciesItem.SetDensitySlope(const Value: double);
begin
  StoredDensitySlope.Value := Value;
end;

procedure TChemSpeciesItem.SetFirstSorbParamDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsFirstSorbParamUsed,
      FFirstSorbParamDataArrayName, NewName,
      FFirstSorbParamDisplayName, '1.', 'MT3DMS or MT3D-USGS RCT package, SP1',
      LocalModel.AnyMt3dSorbParameter, StrMt3dClassification);
  end;
  SetCaseSensitiveStringProperty(FFirstSorbParamDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetHalfSaturationConstantDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dUsgsMonodUsed,
      FHalfSaturationConstantDataArrayName, NewName,
      FHalfSaturationConstantDisplayName, '1.', 'MT3D-USGS RCT package, RC3',
      LocalModel.AnyMt3dUsgsMonod, StrMt3dClassification);
  end;
  SetCaseSensitiveStringProperty(FHalfSaturationConstantDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetImmobileBulkDensities(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileBulkDensities.Count < Value.Count do
    begin
      FImmobileBulkDensities.Add('');
    end;
    while FImmobileBulkDensities.Count > Value.Count do
    begin
      FImmobileBulkDensities.Delete(FImmobileBulkDensities.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileBulkDensities[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '1.7';
      AssociatedDataSets := 'GWT IST Package: BULK_DENSITY';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected and IstProp.Sorption;
      UpdateDataArray(LocalModel.GwtImmobileBulkDensityUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileBulkDensities[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileBulkDensities <> Value then
    begin
      FImmobileBulkDensities.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileDecay(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileDecay.Count < Value.Count do
    begin
      FImmobileDecay.Add('');
    end;
    while FImmobileDecay.Count > Value.Count do
    begin
      FImmobileDecay.Delete(FImmobileDecay.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileDecay[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.001';
      AssociatedDataSets := 'GWT IST Package: DECAY';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected
        and (IstProp.FirstOrderDecay or IstProp.ZeroOrderDecay);
      UpdateDataArray(LocalModel.GwtImmobileDecayUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileDecay[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileDecay <> Value then
    begin
      FImmobileDecay.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileDecaySorbed(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileDecaySorbed.Count < Value.Count do
    begin
      FImmobileDecaySorbed.Add('');
    end;
    while FImmobileDecaySorbed.Count > Value.Count do
    begin
      FImmobileDecaySorbed.Delete(FImmobileDecaySorbed.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileDecaySorbed[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.001';
      AssociatedDataSets := 'GWT IST Package: BULK_DECAY_SORBED';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected and IstProp.Sorption
        and (IstProp.FirstOrderDecay or IstProp.ZeroOrderDecay);
      UpdateDataArray(LocalModel.GwtImmobileDecaySorbedUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileDecaySorbed[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileDecaySorbed <> Value then
    begin
      FImmobileDecaySorbed.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileDistCoeficients(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileDistCoeficients.Count < Value.Count do
    begin
      FImmobileDistCoeficients.Add('');
    end;
    while FImmobileDistCoeficients.Count > Value.Count do
    begin
      FImmobileDistCoeficients.Delete(FImmobileDistCoeficients.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and GwtIst.IsSelected
        and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileDistCoeficients[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.176';
      AssociatedDataSets := 'GWT IST Package: DISTCOEF';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected and IstProp.Sorption;
      UpdateDataArray(LocalModel.GwtImmobileDistCoefUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileDistCoeficients[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileDistCoeficients <> Value then
    begin
      FImmobileDistCoeficients.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileInitialConcentrations(
  const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileInitialConcentrations.Count < Value.Count do
    begin
      FImmobileInitialConcentrations.Add('');
    end;
    while FImmobileInitialConcentrations.Count > Value.Count do
    begin
      FImmobileInitialConcentrations.Delete(FImmobileInitialConcentrations.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileInitialConcentrations[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.';
      AssociatedDataSets := 'GWT IST Package: CIM';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected;
      UpdateDataArray(LocalModel.GwtImmobileCimUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileInitialConcentrations[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileInitialConcentrations <> Value then
    begin
      FImmobileInitialConcentrations.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileMassTransferRates(
  const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while ImmobileMassTransferRates.Count < Value.Count do
    begin
      ImmobileMassTransferRates.Add('');
    end;
    while ImmobileMassTransferRates.Count > Value.Count do
    begin
      ImmobileMassTransferRates.Delete(ImmobileMassTransferRates.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := ImmobileMassTransferRates[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.01';
      AssociatedDataSets := 'GWT IST Package: ZETAIM';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected;
      UpdateDataArray(LocalModel.GwtImmobileZetaimUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      ImmobileMassTransferRates[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if ImmobileMassTransferRates <> Value then
    begin
      ImmobileMassTransferRates.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobilePartioningCoefficientDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dUsgsDualSeparateUsed,
      FImmobilePartioningCoefficientDataArrayName, NewName,
      FImmobilePartioningCoefficientDisplayName, '1.', 'MT3D-USGS RCT package, SP1IM',
      LocalModel.AnyMt3dUsgsDualSeparate, StrMt3dClassification);
  end;
  SetCaseSensitiveStringProperty(FImmobilePartioningCoefficientDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetImmobilePorosities(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobilePorosities.Count < Value.Count do
    begin
      FImmobilePorosities.Add('');
    end;
    while FImmobilePorosities.Count > Value.Count do
    begin
      FImmobilePorosities.Delete(FImmobilePorosities.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobilePorosities[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.25';
      AssociatedDataSets := 'GWT IST Package: VOLFRAC';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected;
      UpdateDataArray(LocalModel.GwtImmobileThetaimUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobilePorosities[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobilePorosities <> Value then
    begin
      FImmobilePorosities.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetImmobileVolumeFractions(const Value: TStringList);
var
  DomainIndex: Integer;
  OldName: string;
  NewName: string;
  NewDisplayName: string;
  AssociatedDataSets: string;
  NewFormula: string;
  GwtPackages: TGwtPackageCollection;
  GwtIst: TGwtIstPackage;
  IstProp: TIstPackageItem;
  LocalModel: TPhastModel;
  ShouldCreate: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    GwtPackages := LocalModel.ModflowPackages.GwtPackages;
    if Index < GwtPackages.Count then
    begin
      GwtIst := GwtPackages[Index].GwtIst;
    end
    else
    begin
      GwtIst := nil;
    end;
    while FImmobileVolumeFractions.Count < Value.Count do
    begin
      FImmobileVolumeFractions.Add('');
    end;
    while FImmobileVolumeFractions.Count > Value.Count do
    begin
      FImmobileVolumeFractions.Delete(FImmobileVolumeFractions.Count-1);
    end;
    for DomainIndex := 0 to Value.Count - 1 do
    begin
      if (GwtIst <> nil) and (DomainIndex < GwtIst.IstPackageProperties.Count) then
      begin
        IstProp := GwtIst.IstPackageProperties[DomainIndex];
      end
      else
      begin
        IstProp := nil;
      end;
      OldName := FImmobileVolumeFractions[DomainIndex];
      NewName := Value[DomainIndex];
      NewDisplayName := NewName;
      NewFormula := '0.25';
      AssociatedDataSets := 'GWT IST Package: VOLFRAC';
      ShouldCreate := (IstProp <> nil) and GwtIst.IsSelected;
      UpdateDataArray(LocalModel.GwtImmobileVolumeFractUsed, OldName, NewName,
        NewDisplayName, NewFormula, AssociatedDataSets, ShouldCreate, StrGwtClassification);
      FImmobileVolumeFractions[DomainIndex] := NewName;
    end;
  end
  else
  begin
    if FImmobileVolumeFractions <> Value then
    begin
      FImmobileVolumeFractions.Assign(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetIndex(Value: Integer);
var
  OldIndex: Integer;
  LocalModel: TCustomModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ConcBoundary: TMt3dmsConcBoundary;
  Mt3dUztRechConc: TMt3dUztRchConcBoundary;
  Mt3dUztSatEtConcBoundary: TMt3dUztSatEtConcBoundary;
  Mt3dUztUnsatEtConcBoundary: TMt3dUztUnsatEtConcBoundary;
  Mt3dUzSsmSinkConcBoundary: TMt3dUzSsmSinkConcBoundary;
  Mt3dLktConcBoundary: TMt3dLktConcBoundary;
  Mt3dSftConcBoundary: TMt3dSftBoundary;
begin
  OldIndex := Index;
  inherited;
  if OldIndex <> Value then
  begin
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      if LocalModel.MobileComponents.IndexOf(self) < 0 then
      begin
        OldIndex := OldIndex + LocalModel.MobileComponents.Count;
        Value := Value + LocalModel.MobileComponents.Count;
      end;

      for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];

        ConcBoundary := AScreenObject.Mt3dmsConcBoundary;
        if ConcBoundary <> nil then
        begin
          ConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dUztRechConc := AScreenObject.Mt3dUzfRechConc;
        if Mt3dUztRechConc <> nil then
        begin
          Mt3dUztRechConc.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dUztSatEtConcBoundary := AScreenObject.Mt3dUztSatEtConcBoundary;
        if Mt3dUztSatEtConcBoundary <> nil then
        begin
          Mt3dUztSatEtConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dUztUnsatEtConcBoundary := AScreenObject.Mt3dUztUnsatEtConcBoundary;
        if Mt3dUztUnsatEtConcBoundary <> nil then
        begin
          Mt3dUztUnsatEtConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dUzSsmSinkConcBoundary := AScreenObject.Mt3dUzSsmSinkConcBoundary;
        if Mt3dUzSsmSinkConcBoundary <> nil then
        begin
          Mt3dUzSsmSinkConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dLktConcBoundary := AScreenObject.Mt3dLktConcBoundary;
        if Mt3dLktConcBoundary <> nil then
        begin
          Mt3dLktConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

        Mt3dSftConcBoundary := AScreenObject.Mt3dSftConcBoundary;
        if Mt3dSftConcBoundary <> nil then
        begin
          Mt3dSftConcBoundary.ChangeSpeciesPosition(OldIndex, Value);
        end;

      end;
    end;
  end;
end;

procedure TChemSpeciesItem.SetInitialConcDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsInitialConcUsed,
      FInitialConcDataArrayName, NewName,
      FInitialConcDisplayName, '0', 'MT3DMS or MT3D-USGS BTN package, SCONC'
        + sLineBreak + 'MODFLOW 6 GWT IC package, STRT',
      True, StrMT3DMS_GWT_Classificaton);
  end;

  SetCaseSensitiveStringProperty(FInitialConcDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetInitialConcentrationFileName(const Value: string);
begin
  SetCaseInsensitiveStringProperty(FInitialConcentrationFileName, Value);
  if (Model <> nil) and (Value <> '') then
  begin
    if frmGoPhast.PhastModel.FilesToArchive.IndexOf(Value) < 0 then
    begin
      frmGoPhast.PhastModel.FilesToArchive.Add(Value);
    end;
  end;
end;

procedure TChemSpeciesItem.SetMobileBulkDensityDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if  (MstPackage.Sorption <> gscNone) then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileBulkDensityUsed,
      FMobileBulkDensityDataArrayName, NewName,
      FMobileBulkDensityDataArrayDisplayName, '1.7', 'GWT MST Package: BULK_DENSITY',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileBulkDensityDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetMobileDecayRateDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if MstPackage.ZeroOrderDecay or MstPackage.FirstOrderDecay then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileDecayUsed,
      FMobileDecayRateDataArrayName, NewName,
      FMobileDecayRateDataArrayDisplayName, '0.001', 'GWT MST Package: DECAY',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileDecayRateDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetMobileDistCoefDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if MstPackage.Sorption <> gscNone then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileDistibutionCoefUsed,
      FMobileDistCoefDataArrayName, NewName,
      FMobileDistCoefDataArrayDisplayName, '0.176', 'GWT MST Package: DISTCOEF',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileDistCoefDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetMobileFreundlichExponentDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if MstPackage.Sorption = gscFreundlich then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileFreundlichExponentUsed,
      FMobileFreundlichExponentDataArrayName, NewName,
      FMobileFreundlichExponentDataArrayDisplayName, '0.7', 'GWT MST Package: SP2',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileFreundlichExponentDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetMobileSorbedDecayRateDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if (MstPackage.ZeroOrderDecay or MstPackage.FirstOrderDecay)
          and (MstPackage.Sorption > gscNone) then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileSorbedDecayUsed,
      FMobileSorbedDecayRateDataArrayName, NewName,
      FMobileSorbedDecayRateDataArrayDisplayName, '0.001', 'GWT MST Package: DECAY_SORBED',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileSorbedDecayRateDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetMobileSorptionCapacityDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected then
      begin
        if MstPackage.Sorption = gscLangmuir then
        begin
          DataSetUsed := True;
        end;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileSorptionCapacityUsed,
      FMobileSorptionCapacityDataArrayName, NewName,
      FMobileSorptionCapacityDataArrayDisplayName, '0.003', 'GWT MST Package: SP2',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FMobileSorptionCapacityDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetName(const Value: string);
var
  LocalModel: TPhastModel;
  DomainIndex: Integer;
  OldDataSetName: string;
  OldRoot: string;
  NewRoot: string;
  GwtIst: TGwtIstPackage;
  NewImobileDataSetNames: TStringList;
  NewDataSetName: string;
  AName: string;
begin
  Assert(Value <> '');
  // data array names may need to change even if the species name does not.
  NewImobileDataSetNames := TStringList.Create;
  try
    if UpperCase(FName) = UpperCase(Value) then
    begin
      OldRoot := GenerateNewRoot(FName);
      NewRoot := GenerateNewRoot(Value);
      FInitialConcDisplayName := StringReplace(FInitialConcDisplayName,
        OldRoot,NewRoot, []);
      InitialConcDataArrayName := StringReplace(InitialConcDataArrayName,
        OldRoot,NewRoot, []);

      FSorbOrImmobInitialConcDisplayName := StringReplace(
        FSorbOrImmobInitialConcDisplayName,
        OldRoot,NewRoot, []);
      SorbOrImmobInitialConcDataArrayName := StringReplace(
        SorbOrImmobInitialConcDataArrayName,
        OldRoot,NewRoot, []);

      FFirstSorbParamDisplayName := StringReplace(
        FFirstSorbParamDisplayName,
        OldRoot,NewRoot, []);
      FirstSorbParamDataArrayName := StringReplace(
        FirstSorbParamDataArrayName,
        OldRoot,NewRoot, []);

      FSecondSorbParamDisplayName := StringReplace(
        FSecondSorbParamDisplayName,
        OldRoot,NewRoot, []);
      SecondSorbParamDataArrayName := StringReplace(
        SecondSorbParamDataArrayName,
        OldRoot,NewRoot, []);

      FReactionRateDisolvedDisplayName := StringReplace(
        FReactionRateDisolvedDisplayName,
        OldRoot,NewRoot, []);
      ReactionRateDisolvedDataArrayName := StringReplace(
        ReactionRateDisolvedDataArrayName,
        OldRoot,NewRoot, []);

      FReactionRateSorbedDisplayName := StringReplace(
        FReactionRateSorbedDisplayName,
        OldRoot,NewRoot, []);
      ReactionRateSorbedDataArrayName := StringReplace(
        ReactionRateSorbedDataArrayName,
        OldRoot,NewRoot, []);

      FHalfSaturationConstantDisplayName := StringReplace(
        FHalfSaturationConstantDisplayName,
        OldRoot,NewRoot, []);
      HalfSaturationConstantDataArrayName := StringReplace(
        HalfSaturationConstantDataArrayName,
        OldRoot,NewRoot, []);

      FImmobilePartioningCoefficientDisplayName := StringReplace(
        FImmobilePartioningCoefficientDisplayName,
        OldRoot,NewRoot, []);
      ImmobilePartioningCoefficientDataArrayName := StringReplace(
        ImmobilePartioningCoefficientDataArrayName,
        OldRoot,NewRoot, []);

      FUztInitialConcDisplayName := StringReplace(
        FUztInitialConcDisplayName,
        OldRoot,NewRoot, []);
      UztInitialConcDataArrayName := StringReplace(
        UztInitialConcDataArrayName,
        OldRoot,NewRoot, []);

      FPorosityDataArrayDisplayName := StringReplace(
        FPorosityDataArrayDisplayName,
        OldRoot,NewRoot, []);
      PorosityDataArrayName := StringReplace(
        PorosityDataArrayName,
        OldRoot,NewRoot, []);

      FMobileDecayRateDataArrayDisplayName := StringReplace(
        FMobileDecayRateDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileDecayRateDataArrayName := StringReplace(
        MobileDecayRateDataArrayName,
        OldRoot,NewRoot, []);

      FMobileSorbedDecayRateDataArrayDisplayName := StringReplace(
        FMobileSorbedDecayRateDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileSorbedDecayRateDataArrayName := StringReplace(
        MobileSorbedDecayRateDataArrayName,
        OldRoot,NewRoot, []);

      FMobileDistCoefDataArrayDisplayName := StringReplace(
        FMobileDistCoefDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileDistCoefDataArrayName := StringReplace(
        MobileDistCoefDataArrayName,
        OldRoot,NewRoot, []);

      FMobileBulkDensityDataArrayDisplayName := StringReplace(
        FMobileBulkDensityDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileBulkDensityDataArrayName := StringReplace(
        MobileBulkDensityDataArrayName,
        OldRoot,NewRoot, []);

      FMobileFreundlichExponentDataArrayDisplayName := StringReplace(
        FMobileFreundlichExponentDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileFreundlichExponentDataArrayName := StringReplace(
        MobileFreundlichExponentDataArrayName,
        OldRoot,NewRoot, []);

      FMobileSorptionCapacityDataArrayDisplayName := StringReplace(
        FMobileSorptionCapacityDataArrayDisplayName,
        OldRoot,NewRoot, []);
      MobileSorptionCapacityDataArrayName := StringReplace(
        MobileSorptionCapacityDataArrayName,
        OldRoot,NewRoot, []);

      NewImobileDataSetNames.Assign(ImmobileInitialConcentrations);
      for DomainIndex := 0 to ImmobileInitialConcentrations.Count - 1 do
      begin
        OldDataSetName := ImmobileInitialConcentrations[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileInitialConcentrations := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobilePorosities);
      for DomainIndex := 0 to ImmobilePorosities.Count - 1 do
      begin
        OldDataSetName := ImmobilePorosities[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobilePorosities := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileVolumeFractions);
      for DomainIndex := 0 to ImmobileVolumeFractions.Count - 1 do
      begin
        OldDataSetName := ImmobileVolumeFractions[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileVolumeFractions := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileMassTransferRates);
      for DomainIndex := 0 to ImmobileMassTransferRates.Count - 1 do
      begin
        OldDataSetName := ImmobileMassTransferRates[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileMassTransferRates := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileDecay);
      for DomainIndex := 0 to ImmobileDecay.Count - 1 do
      begin
        OldDataSetName := ImmobileDecay[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileDecay := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileDecaySorbed);
      for DomainIndex := 0 to ImmobileDecaySorbed.Count - 1 do
      begin
        OldDataSetName := ImmobileDecaySorbed[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileDecaySorbed := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileBulkDensities);
      for DomainIndex := 0 to ImmobileBulkDensities.Count - 1 do
      begin
        OldDataSetName := ImmobileBulkDensities[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileBulkDensities := NewImobileDataSetNames;

      NewImobileDataSetNames.Assign(ImmobileDistCoeficients);
      for DomainIndex := 0 to ImmobileDistCoeficients.Count - 1 do
      begin
        OldDataSetName := ImmobileDistCoeficients[DomainIndex];
        NewDataSetName := StringReplace(OldDataSetName, OldRoot,NewRoot, []);
        NewImobileDataSetNames[DomainIndex] := NewDataSetName;
      end;
      ImmobileDistCoeficients := NewImobileDataSetNames;
    end
    else
    begin
      FInitialConcDisplayName := GenerateNewRoot(StrInitConcPrefix + Value);
      InitialConcDataArrayName := GenerateNewRoot(kInitConcPrefix + Value);
      LocalModel := Model as TPhastModel;
      if LocalModel = nil then
      begin
        LocalModel := frmGoPhast.PhastModel;
      end;
      if LocalModel.ModflowPackages.Mt3dmsChemReact.SorptionChoice
        = scFirstOrderKinetic then
      begin
        FSorbOrImmobInitialConcDisplayName :=
          GenerateNewRoot(StrSorbPrefix + Value);
        SorbOrImmobInitialConcDataArrayName :=
          GenerateNewRoot(kSorbPrefix + Value);
      end
      else
      begin
        FSorbOrImmobInitialConcDisplayName :=
          GenerateNewRoot(kImmobPrefix + Value);
        SorbOrImmobInitialConcDataArrayName :=
          GenerateNewRoot(kImmobPrefix + Value);
      end;
      FFirstSorbParamDisplayName :=
        GenerateNewRoot(StrFirstSorbParamPrefix + Value);
      FirstSorbParamDataArrayName :=
        GenerateNewRoot(kFirstSorbParamPrefix + Value);

      FSecondSorbParamDisplayName :=
        GenerateNewRoot(StrSecondSorbParamPrefix + Value);
      SecondSorbParamDataArrayName :=
        GenerateNewRoot(kSecondSorbParamPrefix + Value);

      FReactionRateDisolvedDisplayName :=
        GenerateNewRoot(StrRC1Prefix + Value);
      ReactionRateDisolvedDataArrayName :=
        GenerateNewRoot(kRC1Prefix + Value);

      FReactionRateSorbedDisplayName :=
        GenerateNewRoot(StrRC2Prefix + Value);
      ReactionRateSorbedDataArrayName :=
        GenerateNewRoot(kRC2Prefix + Value);

      FHalfSaturationConstantDisplayName :=
        GenerateNewRoot(StrRC3Prefix + Value);
      HalfSaturationConstantDataArrayName :=
        GenerateNewRoot(kRC3Prefix + Value);

      FImmobilePartioningCoefficientDisplayName :=
        GenerateNewRoot(StrSP1IMPrefix + Value);
      ImmobilePartioningCoefficientDataArrayName :=
        GenerateNewRoot(kSP1IMPrefix + Value);

      FUztInitialConcDisplayName :=
        GenerateNewRoot(StrUztStartConct + Value);
      UztInitialConcDataArrayName :=
        GenerateNewRoot(kUztStartConct + Value);

      FPorosityDataArrayDisplayName :=
        GenerateNewRoot(StrPorosity + Value);
      PorosityDataArrayName :=
        GenerateNewRoot(kPorosity + Value);

      FMobileDecayRateDataArrayDisplayName :=
        GenerateNewRoot(StrMobileDecayRate + Value);
      MobileDecayRateDataArrayName :=
        GenerateNewRoot(KMobileDecayRate + Value);

      FMobileSorbedDecayRateDataArrayDisplayName :=
        GenerateNewRoot(StrMobileSorbedDecayRate + Value);
      MobileSorbedDecayRateDataArrayName :=
        GenerateNewRoot(KMobileSorbedDecayRate + Value);

      FMobileDistCoefDataArrayDisplayName :=
        GenerateNewRoot(StrMobileDistCoef + Value);
      MobileDistCoefDataArrayName :=
        GenerateNewRoot(KMobileDistCoef + Value);

      FMobileBulkDensityDataArrayDisplayName :=
        GenerateNewRoot(StrMobileBulkDensity + Value);
      MobileBulkDensityDataArrayName :=
        GenerateNewRoot(KMobileBulkDensity + Value);

      FMobileFreundlichExponentDataArrayDisplayName :=
        GenerateNewRoot(StrFreundlichExponent + Value);
      MobileFreundlichExponentDataArrayName :=
        GenerateNewRoot(KFreundlichExponent + Value);

      FMobileSorptionCapacityDataArrayDisplayName :=
        GenerateNewRoot(StrSorptionCapacity + Value);
      MobileSorptionCapacityDataArrayName :=
        GenerateNewRoot(KSorptionCapacity + Value);

      if Index < LocalModel.ModflowPackages.GwtPackages.Count then
      begin
        GwtIst := LocalModel.ModflowPackages.GwtPackages[Index].GwtIst;
        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileBulkDensity, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileBulkDensities := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KrImmobileInitialConce, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileInitialConcentrations := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobilePorosity, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobilePorosities := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileVolFrac, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileVolumeFractions := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileMassTransfer, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileMassTransferRates := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileDecay, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileDecay := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileDecaySorbed, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileDecaySorbed := NewImobileDataSetNames;

        NewImobileDataSetNames.Clear;
        for DomainIndex := 0 to GwtIst.IstPackageProperties.Count - 1 do
        begin
          AName := GenerateNewRoot(Format(
            KImmobileDistCoeficie, [Value, DomainIndex+1]));
          NewImobileDataSetNames.Add(AName);
        end;
        ImmobileDistCoeficients := NewImobileDataSetNames;
      end;
    end;
    RenameDependents(Value);
    SetCaseSensitiveStringProperty(FName, Value);
  finally
    NewImobileDataSetNames.Free;
  end;
end;

procedure TChemSpeciesItem.SetPorosityDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
  MstPackage: TGwtMstPackage;
  GwtPackagesItem: TGwtPackagesItem;
  DataSetUsed: Boolean;
begin
  LocalModel := Collection.Model as TPhastModel;

  if (LocalModel <> nil) then
  begin
    DataSetUsed := False;
    if LocalModel.GwtUsed then
    begin
      GwtPackagesItem :=  LocalModel.ModflowPackages.GwtPackages[Index];
      MstPackage := GwtPackagesItem.GwtMst;
      if MstPackage.IsSelected and MstPackage.SeparatePorosity then
      begin
        DataSetUsed := True;
      end;
    end;
    UpdateDataArray(LocalModel.GwtMobileSeparatePorosityUsed,
      FPorosityDataArrayName, NewName,
      FPorosityDataArrayDisplayName, '0.25', 'GWT MST Package: POROSITY',
      DataSetUsed, StrGwtClassification);
  end;

  SetCaseSensitiveStringProperty(FPorosityDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetReactionRateDisolvedDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dmsReactionRateDisolvedUsed,
      FReactionRateDisolvedDataArrayName, NewName,
      FReactionRateDisolvedDisplayName, '1E-6', 'MT3DMS or MT3D-USGS RCT package, RC1',
      LocalModel.AnyMt3dReactions, StrMt3dClassification);
  end;

  SetCaseSensitiveStringProperty(FReactionRateDisolvedDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetReactionRateSorbedDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dmsReactionRateSorbedUsed,
      FReactionRateSorbedDataArrayName, NewName,
      FReactionRateSorbedDisplayName, '1E-6', 'MT3DMS or MT3D-USGS RCT package, RC2',
      LocalModel.AnyMt3dReactions, StrMt3dClassification);
  end;

  SetCaseSensitiveStringProperty(FReactionRateSorbedDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetSecondSorbParamDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsSecondSorbParamUsed,
      FSecondSorbParamDataArrayName, NewName,
      FSecondSorbParamDisplayName, '1.', 'MT3DMS or MT3D-USGS RCT package, SP2',
      LocalModel.AnyMt3dSorbParameter, StrMt3dClassification);
  end;

  SetCaseSensitiveStringProperty(FSecondSorbParamDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetSorbOrImmobInitialConcDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mt3dMsSorbImmobInitialConcUsed,
      FSorbOrImmobInitialConcDataArrayName, NewName,
      FSorbOrImmobInitialConcDisplayName, '0', 'MT3DMS or MT3D-USGS RCT package, SRCONC',
      LocalModel.AnyMt3dSorbImmobConc, StrMt3dClassification);
  end;
  SetCaseSensitiveStringProperty(FSorbOrImmobInitialConcDataArrayName, NewName);
end;

procedure TChemSpeciesItem.SetStoredDensitySlope(const Value: TRealStorage);
begin
  FStoredDensitySlope.Assign(Value);
end;

procedure TChemSpeciesItem.SetStoredRefConcentration(const Value: TRealStorage);
begin
  FStoredRefConcentration.Assign(Value);
end;

procedure TChemSpeciesItem.SetUseInitialConcentrationFile(const Value: boolean);
begin
  SetBooleanProperty(FUseInitialConcentrationFile, Value);
end;

procedure TChemSpeciesItem.SetUztInitialConcDataArrayName(const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;
  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.Mf6UzfInitialConcentrationUsed,
      FUztInitialConcDataArrayName, NewName,
      FUztInitialConcDisplayName, '0.', 'MODFLOW 6 UZT package, strt',
      LocalModel.AnyUzfInitialConcentrationUsed, StrGwtClassification);
  end;
  SetCaseSensitiveStringProperty(FUztInitialConcDataArrayName, NewName);
end;

{ TConcentrationCollection }

function TCustomChemSpeciesCollection.Add: TChemSpeciesItem;
begin
  Result := inherited Add as TChemSpeciesItem;
end;

constructor TChemSpeciesCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TChemSpeciesItem, Model);
end;

function TCustomChemSpeciesCollection.GetItem(Index: Integer): TChemSpeciesItem;
begin
  result := inherited Items[Index] as TChemSpeciesItem
end;

function TCustomChemSpeciesCollection.IndexOfName(const AName: string): integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[index].Name, AName) = 0 then
    begin
      result := index;
      exit;
    end;
  end;
end;

function TCustomChemSpeciesCollection.Insert(Index: integer): TChemSpeciesItem;
begin
  result := inherited Insert(Index) as TChemSpeciesItem;
end;

procedure TCustomChemSpeciesCollection.Loaded;
var
  index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[index].Name := Items[index].Name;
  end;
end;

procedure TCustomChemSpeciesCollection.SetItem(Index: Integer;
  const Value: TChemSpeciesItem);
begin
  inherited Items[Index] := Value;
end;


procedure TCustomChemSpeciesCollection.UpdateDataArrays;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    // ensure the data sets have been created if needed.
    Items[Index].Name := Items[Index].Name;
  end;
end;

{ TMobileChemSpeciesItem }

procedure TMobileChemSpeciesItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TMobileChemSpeciesItem then
  begin
    DiffusionCoefDataArrayName :=
      TMobileChemSpeciesItem(Source).DiffusionCoefDataArrayName;
  end;
  inherited;

end;

function TMobileChemSpeciesItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := inherited and (AnotherItem is TMobileChemSpeciesItem);
  if result then
  begin
    Result := DiffusionCoefDataArrayName =
      TMobileChemSpeciesItem(AnotherItem).DiffusionCoefDataArrayName
  end;
end;

procedure TMobileChemSpeciesItem.SetDiffusionCoefDataArrayName(
  const NewName: string);
var
  LocalModel: TPhastModel;
begin
  LocalModel := Collection.Model as TPhastModel;

  if LocalModel <> nil then
  begin
    UpdateDataArray(LocalModel.ModDispDataArrayUsed,
      FDiffusionCoefDataArrayName, NewName,
      FDiffusionCoefDisplayName, '0', 'MT3DMS or MT3D-USGS DSP Package, DMCOEF',
      LocalModel.AnyDispersionMultiDiffusion, StrMt3dClassification);
  end;

  SetCaseSensitiveStringProperty(FDiffusionCoefDataArrayName, NewName);
end;

procedure TMobileChemSpeciesItem.SetName(const Value: string);
begin
  Assert(Value <> '');
  // data array names may need to change even if the species name does not.
//  if FName <> Value then
  begin
    if UpperCase(FName) = UpperCase(Value) then
    begin
      FDiffusionCoefDisplayName := StringReplace(FDiffusionCoefDisplayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
      DiffusionCoefDataArrayName := StringReplace(DiffusionCoefDataArrayName,
        GenerateNewRoot(FName),GenerateNewRoot(Value), []);
    end
    else
    begin
      FDiffusionCoefDisplayName := GenerateNewRoot(StrDiffCoefPrefix + Value);
      DiffusionCoefDataArrayName := GenerateNewRoot(kDiffCoefPrefix + Value);
    end;
  end;
  inherited;
end;

{ TMobileChemSpeciesCollection }

function TMobileChemSpeciesCollection.Add: TMobileChemSpeciesItem;
begin
  result := inherited Add as TMobileChemSpeciesItem
end;

constructor TMobileChemSpeciesCollection.Create(Model: IModelForTOrderedCollection);
begin
  inherited Create(TMobileChemSpeciesItem, Model);
end;

function TMobileChemSpeciesCollection.GetItem(
  Index: Integer): TMobileChemSpeciesItem;
begin
  result := inherited Items[Index] as TMobileChemSpeciesItem;
end;

function TMobileChemSpeciesCollection.GetItemByName(
  AName: string): TMobileChemSpeciesItem;
var
  index: Integer;
begin
  result := nil;
  for index := 0 to Count - 1 do
  begin
    if SameText(Items[index].Name, AName) then
    begin
      result := Items[index];
      Break;
    end;
  end;
end;

procedure TMobileChemSpeciesCollection.SetItem(Index: Integer;
  const Value: TMobileChemSpeciesItem);
begin
  inherited Items[Index] := Value;
end;

procedure TMobileChemSpeciesCollection.UpdateAllDataArrays;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].UpdateAllDataArrays;
  end;
end;

procedure TMobileChemSpeciesCollection.UpdateDataArrays;
var
  Index: Integer;
begin
  inherited;
  for Index := 0 to Count - 1 do
  begin
    // ensure the data sets have been created if needed.
    Items[Index].DiffusionCoefDataArrayName :=
      Items[Index].DiffusionCoefDataArrayName
  end;
end;

end.
