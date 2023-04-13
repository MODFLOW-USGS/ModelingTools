unit ModflowSubsidenceDefUnit;

interface

uses
  Classes, GoPhastTypes, OrderedCollectionUnit, SysUtils, PestObsUnit,
  System.Generics.Collections, InterpolatedObsCellUnit;

type
  TUseLayerNumberItem = class(TOrderedItem)
  private
    FLayerNumber: integer;
    procedure SetLayerNumber(const Value: integer);
  public
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure Assign(Source: TPersistent); override;
  published
    property LayerNumber: integer read FLayerNumber write SetLayerNumber;
  end;

  TUseLayersCollection = class(TOrderedCollection)
  private
    function GetItem(Index: integer): TUseLayerNumberItem;
    procedure SetItem(Index: integer; const Value: TUseLayerNumberItem);
  public
    function Add: TUseLayerNumberItem;
    Constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: integer]: TUseLayerNumberItem read GetItem
      write SetItem; default;
    function GetItemByLayerNumber(LayerNumber: integer): TUseLayerNumberItem;
  end;

  TCustomSubLayerItem = class(TOrderedItem)
  private
    FUsedLayers: TUseLayersCollection;
    FUseInAllLayers: boolean;
    FName: string;
    FAssociatedModelDataSetNames: TStringList;
    procedure SetName(const Value: string);
    procedure SetUsedLayers(const Value: TUseLayersCollection);
    procedure SetUseInAllLayers(const Value: boolean);
  protected
    FDataArrayTypes: TStringList;
    FDataArrayDisplayTypes: TStringList;
    procedure UpdateArrayNames(NewNames, NewDisplayNames: TStringList); virtual; abstract;
    // update the @link(TDataArray.AssociatedDataSets) property.
    procedure UpdateAssociatedDataSetNames(NewNames: TStringList);
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure SetDataArrayName(var StoredName: string; NewName, NewDisplayName: string;
      CreateDataArray: boolean);
    procedure SetArrayNames(NewName: string);
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    Destructor Destroy; override;
  published
    property UseInAllLayers: boolean read FUseInAllLayers
      write SetUseInAllLayers;
    property UsedLayers: TUseLayersCollection read FUsedLayers
      write SetUsedLayers;
    property Name: string read FName write SetName;
  end;

  TSubNoDelayBedLayerItem = class(TCustomSubLayerItem)
  private
    FElasticSkeletalStorageCoefficientDataArrayName: string;
    FInelasticSkeletalStorageCoefficientDataArrayName: string;
    FPreconsolidationHeadDataArrayName: string;
    FInitialCompactionDataArrayName: string;
    FInitialElasticCompactionDataArrayName: string;
    FInitialInelasticCompactionDataArrayName: string;

    FPreconsolidationHeadDisplayName: string;
    FElasticSkeletalStorageCoefficientDisplayName: string;
    FInelasticSkeletalStorageCoefficientDisplayName: string;
    FInitialCompactionDisplayName: string;
    FInitialElasticCompactionDisplayName: string;
    FInitialInelasticCompactionDisplayName: string;
    procedure SetElasticSkeletalStorageCoefficientDataArrayName(
      const Value: string);
    procedure SetInelasticSkeletalStorageCoefficientDataArrayName(
      const Value: string);
    procedure SetInitialCompactionDataArrayName(const Value: string);
    procedure SetPreconsolidationHeadDataArrayName(const Value: string);
    procedure Loaded;
    procedure SetInitialElasticCompactionDataArrayName(const Value: string);
    procedure SetInitialInelasticCompactionDataArrayName(const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure UpdateArrayNames(NewNames, NewDisplayNames: TStringList); override;
    procedure UpdateRequiredDataArrays;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
  published
    // data set 5 or 8. HC
    property PreconsolidationHeadDataArrayName: string
      read FPreconsolidationHeadDataArrayName
      write SetPreconsolidationHeadDataArrayName;
    // data set 6 or 9. Sfe
    property ElasticSkeletalStorageCoefficientDataArrayName: string
      read FElasticSkeletalStorageCoefficientDataArrayName
      write SetElasticSkeletalStorageCoefficientDataArrayName;
    // data set 7 or 10. Sfv
    property InelasticSkeletalStorageCoefficientDataArrayName: string
      read FInelasticSkeletalStorageCoefficientDataArrayName
      write SetInelasticSkeletalStorageCoefficientDataArrayName;
    // data set 8 replaced in MODFLOW-OWHM. Com
    property InitialCompactionDataArrayName: string
      read FInitialCompactionDataArrayName
      write SetInitialCompactionDataArrayName;
    // data set 11 in MODFLOW-OWHM. ComE
    property InitialElasticCompactionDataArrayName: string
      read FInitialElasticCompactionDataArrayName
      write SetInitialElasticCompactionDataArrayName;
    // data set 12 in MODFLOW-OWHM. ComV
    property InitialInelasticCompactionDataArrayName: string
      read FInitialInelasticCompactionDataArrayName
      write SetInitialInelasticCompactionDataArrayName;
  end;

  // @name is defined in order to gain access to protected methods of
  // @link(TLayerOwnerCollection);
  TCustomSubLayer = class(TLayerOwnerCollection);

  TSubNoDelayBedLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSubNoDelayBedLayerItem;
    procedure SetItem(Index: integer; const Value: TSubNoDelayBedLayerItem);
  public
    function Add: TSubNoDelayBedLayerItem;
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: integer]: TSubNoDelayBedLayerItem read GetItem
      write SetItem; default;
    procedure Loaded;
    procedure UpdateRequiredDataArrays;
  end;

  TSubDelayBedLayerItem = class(TCustomSubLayerItem)
  private
    FInterbedPreconsolidationHeadDataArrayName: string;
    FElasticSpecificStorageDataArrayName: string;
    FInterbedStartingCompactionDataArrayName: string;
    FVerticalHydraulicConductivityDataArrayName: string;
    FInterbedEquivalentThicknessDataArrayName: string;
    FInterbedStartingHeadDataArrayName: string;
    FInelasticSpecificStorageDataArrayName: string;
    FEquivNumberDataArrayName: string;
    FInterbedStartingInelasticCompactionDataArrayName: string;
    FInterbedStartingElasticCompactionDataArrayName: string;

    FEquivNumberDisplayName: string;
    FVerticalHydraulicConductivityDisplayName: string;
    FElasticSpecificStorageDisplayName: string;
    FInelasticSpecificStorageDisplayName: string;
    FInterbedStartingHeadDisplayName: string;
    FInterbedPreconsolidationHeadDisplayName: string;
    FInterbedStartingCompactionDisplayName: string;
    FInterbedEquivalentThicknessDisplayName: string;
    FInterbedStartingInelasticCompactionDisplayName: string;
    FInterbedStartingElasticCompactionDisplayName: string;
    procedure SetElasticSpecificStorageDataArrayName(const Value: string);
    procedure SetEquivNumberDataArrayName(const Value: string);
    procedure SetInelasticSpecificStorageDataArrayName(const Value: string);
    procedure SetInterbedEquivalentThicknessDataArrayName(const Value: string);
    procedure SetInterbedPreconsolidationHeadDataArrayName(const Value: string);
    procedure SetInterbedStartingCompactionDataArrayName(const Value: string);
    procedure SetInterbedStartingHeadDataArrayName(const Value: string);
    procedure SetVerticalHydraulicConductivityDataArrayName
      (const Value: string);
    procedure Loaded;
    procedure SetInterbedStartingElasticCompactionDataArrayName(
      const Value: string);
    procedure SetInterbedStartingInelasticCompactionDataArrayName(
      const Value: string);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
    procedure UpdateRequiredDataArrays;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    procedure UpdateArrayNames(NewNames, NewDisplayNames: TStringList); override;
  published
    // data set 4. RNB
    property EquivNumberDataArrayName: string read FEquivNumberDataArrayName
      write SetEquivNumberDataArrayName;
    // data set 9 or 13. DP
    property VerticalHydraulicConductivityDataArrayName: string
      read FVerticalHydraulicConductivityDataArrayName
      write SetVerticalHydraulicConductivityDataArrayName;
    // data set 9 or 13. DP
    property ElasticSpecificStorageDataArrayName: string
      read FElasticSpecificStorageDataArrayName
      write SetElasticSpecificStorageDataArrayName;
    // data set 9 or 13. DP
    property InelasticSpecificStorageDataArrayName: string
      read FInelasticSpecificStorageDataArrayName
      write SetInelasticSpecificStorageDataArrayName;
    // data set 10 or 14. Dstart
    property InterbedStartingHeadDataArrayName: string
      read FInterbedStartingHeadDataArrayName
      write SetInterbedStartingHeadDataArrayName;
    // data set 11 or 15. DHC
    property InterbedPreconsolidationHeadDataArrayName: string
      read FInterbedPreconsolidationHeadDataArrayName
      write SetInterbedPreconsolidationHeadDataArrayName;
    // data set 12 not in MODFLOW-OWHM. DCOM
    property InterbedStartingCompactionDataArrayName: string
      read FInterbedStartingCompactionDataArrayName
      write SetInterbedStartingCompactionDataArrayName;
    // data set 13 or 18. DZ
    property InterbedEquivalentThicknessDataArrayName: string
      read FInterbedEquivalentThicknessDataArrayName
      write SetInterbedEquivalentThicknessDataArrayName;
    // data set 16 in MODFLOW-OWHM. DCOME
    property InterbedStartingElasticCompactionDataArrayName: string
      read FInterbedStartingElasticCompactionDataArrayName
      write SetInterbedStartingElasticCompactionDataArrayName;
    // data set 17 in MODFLOW-OWHM. DCOMV
    property InterbedStartingInelasticCompactionDataArrayName: string
      read FInterbedStartingInelasticCompactionDataArrayName
      write SetInterbedStartingInelasticCompactionDataArrayName;
  end;

  TSubDelayBedLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSubDelayBedLayerItem;
    procedure SetItem(Index: integer; const Value: TSubDelayBedLayerItem);
  public
    function Add: TSubDelayBedLayerItem;
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: integer]: TSubDelayBedLayerItem read GetItem
      write SetItem; default;
    procedure Loaded;
    procedure UpdateRequiredDataArrays;
  end;

  TSwtWaterTableItem = class(TCustomSubLayerItem)
  private
    FWaterTableCompressibleThicknessDataArrayName: string;
    FWaterTableInitialVoidRatioDataArrayName: string;
    FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName: string;
    FWaterTableInitialCompactionDataArrayName: string;
    FWaterTableCompressionIndexDataArrayName: string;
    FWaterTableRecompressionIndexDataArrayName: string;
    FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName: string;

    FWaterTableCompressibleThicknessDisplayName: string;
    FWaterTableInitialElasticSkeletalSpecificStorageDisplayName: string;
    FWaterTableInitialInelasticSkeletalSpecificStorageDisplayName: string;
    FWaterTableRecompressionIndexDisplayName: string;
    FWaterTableCompressionIndexDisplayName: string;
    FWaterTableInitialVoidRatioDisplayName: string;
    FWaterTableInitialCompactionDisplayName: string;
    procedure SetWaterTableCompressibleThicknessDataArrayName(
      const Value: string);
    procedure SetWaterTableCompressionIndexDataArrayName(const Value: string);
    procedure SetWaterTableInitialCompactionDataArrayName(const Value: string);
    procedure SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName(
      const Value: string);
    procedure SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName(
      const Value: string);
    procedure SetWaterTableInitialVoidRatioDataArrayName(const Value: string);
    procedure SetWaterTableRecompressionIndexDataArrayName(const Value: string);
    procedure Loaded;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    Constructor Create(Collection: TCollection); override;
    procedure UpdateArrayNames(NewNames, NewDisplayNames: TStringList); override;
  published
    // data set 7 THICK.
    property WaterTableCompressibleThicknessDataArrayName: string
      read FWaterTableCompressibleThicknessDataArrayName
      write SetWaterTableCompressibleThicknessDataArrayName;
    // data set 8 Sse
    property WaterTableInitialElasticSkeletalSpecificStorageDataArrayName: string
      read FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName
      write SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
    // data set 9 Ssv
    property WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName: string
      read FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName
      write SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
    // data set 10 Cr
    property WaterTableRecompressionIndexDataArrayName: string
      read FWaterTableRecompressionIndexDataArrayName
      write SetWaterTableRecompressionIndexDataArrayName;
    // data set 11 Cc
    property WaterTableCompressionIndexDataArrayName: string
      read FWaterTableCompressionIndexDataArrayName
      write SetWaterTableCompressionIndexDataArrayName;
    // data set 12 Void
    property WaterTableInitialVoidRatioDataArrayName: string
      read FWaterTableInitialVoidRatioDataArrayName
      write SetWaterTableInitialVoidRatioDataArrayName;
    // data set 13 Sub
    property WaterTableInitialCompactionDataArrayName: string
      read FWaterTableInitialCompactionDataArrayName
      write SetWaterTableInitialCompactionDataArrayName;
  end;

  TWaterTableLayers = class(TCustomSubLayer)
  private
    function GetItem(Index: integer): TSwtWaterTableItem;
    procedure SetItem(Index: integer; const Value: TSwtWaterTableItem);
  public
    function Add: TSwtWaterTableItem;
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: integer]: TSwtWaterTableItem read GetItem
      write SetItem; default;
    procedure Loaded;
  end;

  TCustomSubObsItem = class(TCustomTimeObservationItem)
  private
    FObsType: string;
    FInterbedSystem: string;
    // @name is not owned by @classname.
    FCells: TBaseInterpolatedObs;
    FInterbed: TCustomSubLayerItem;
    procedure SetObsType(const Value: string);
    procedure SetInterbedSystem(const Value: string);
    function GetInterbedSystem: string;
    procedure SetInterbed(const Value: TCustomSubLayerItem);
  protected
    function GetObsTypeString: string; override;
    procedure SetObsTypeString(const Value: string); override;
  public
    property Interbed: TCustomSubLayerItem read FInterbed write SetInterbed;
    procedure Assign(Source: TPersistent); override;
    function ObservationType: string; override;
    function Units: string; override;
    property Cells: TBaseInterpolatedObs read FCells write FCells;
    procedure Loaded;
  published
    property ObsType: string read FObsType write SetObsType stored True;
    property InterbedSystem: string read GetInterbedSystem write SetInterbedSystem;
    property GUID;
  end;

  TSubObsItem = class(TCustomSubObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(Value: Integer); override;
  end;

  TSubObsItemList = TList<TSubObsItem>;

  TSwtObsItem = class(TCustomSubObsItem)
  protected
    function GetObsTypeIndex: Integer; override;
    procedure SetObsTypeIndex(Value: Integer); override;
  end;

  TSwtObsItemList = TList<TSwtObsItem>;

  TCustomSubObservations = class(TCustomComparisonCollection)
  private
    function GetUsed: Boolean;
  public
    property Used: Boolean read GetUsed;
  end;

  TSubObservations = class(TCustomSubObservations)
  private
    function GetSubItem(Index: Integer): TSubObsItem;
    procedure SetSubItem(Index: Integer; const Value: TSubObsItem);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TSubObsItem read GetSubItem
      write SetSubItem; default;
    function Add: TSubObsItem;
    procedure Loaded;
  end;

  TSubObsCollectionList = TList<TSubObservations>;

  TSwtObservations = class(TCustomSubObservations)
  private
    function GetSwtItem(Index: Integer): TSwtObsItem;
    procedure SetSwtItem(Index: Integer; const Value: TSwtObsItem);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent; ScreenObject: TObject);
    property Items[Index: Integer]: TSwtObsItem read GetSwtItem
      write SetSwtItem; default;
    function Add: TSwtObsItem;
    procedure Loaded;
  end;

  TSwtObsCollectionList = TList<TSwtObservations>;

var
  SubsidenceTypes: TStringList;
  SubsidenceUnits: TStringList;

  SwtTypes: TStringList;


const
  // Subsidence types
  rsSUBSIDENCE = 'SUBSIDENCE';
  rsLAYERCOMPACT = 'LAYER COMPACTION';
  rsNDSYSCOMPACT = 'NDSYS COMPACTION';
  rsDSYSCOMPACTI = 'DSYS COMPACTION';
  rsZDISPLACEMEN = 'Z DISPLACEMENT';
  rsNDCRITICALHE = 'ND CRITICAL HEAD';
  rsDCRITICALHEA = 'D CRITICAL HEAD';

  // SWT types
  rsSUBSIDENCE2   = 'SUBSIDENCE';
  rsLAYERCOMPACT2 = 'LAYER COMPACTION';
  rsSYSTMCOMPACT = 'SYSTM COMPACTION';
  rsZDISPLACEMEN2 = 'Z DISPLACEMENT';
  rsPRECONSOLSTR = 'PRECONSOL STRESS';
  rsCHANGEINPCST = 'CHANGE IN PCSTRS';
  rsGEOSTATICSTR = 'GEOSTATIC STRESS';
  rsCHANGEINGSTR = 'CHANGE IN G-STRS';
  rsEFFECTIVESTR = 'EFFECTIVE STRESS';
  rsCHANGEINEFFS = 'CHANGE IN EFF-ST';
  rsVOIDRATIO    = 'VOID RATIO';
  rsTHICKNESS    = 'THICKNESS';
  rsCENTERELEVAT = 'CENTER ELEVATION';

implementation

uses
  PhastModelUnit, DataSetUnit, RbwParser,
  ModflowPackageSelectionUnit, ModflowPackagesUnit, ScreenObjectUnit,
  frmGoPhastUnit, LayerStructureUnit, DataSetNamesUnit;

const
  kNoDelayPreconsolid = 'No_Delay_Preconsolidation_Head';
  kNoDelayElasticSke = 'No_Delay_Elastic_Skeletal_Storage_Coefficient';
  kNoDelayInelasticS = 'No_Delay_Inelastic_Skeletal_Storage_Coefficient';
  kNoDelayInitialCom = 'No_Delay_Initial_Compaction';

  kNoDelayInitialComE = 'No_Delay_Initial_Elastic_Compaction';
  kNoDelayInitialComV = 'No_Delay_Initial_Inelastic_Compaction';

  kDelayEquivalentNum = 'Delay_Equivalent_Number';
  kDelayVerticalHydra = 'Delay_Vertical_Hydraulic_Conductivity';
  kDelayElasticSpecif = 'Delay_Elastic_Specific_Storage';
  kDelayInelasticSpec = 'Delay_Inelastic_Specific_Storage';
  kDelayInterbedStartHead = 'Delay_Interbed_Starting_Head';
  kDelayInterbedPreco = 'Delay_Interbed_Preconsolidation_Head';
  kDelayInterbedStartCompact = 'Delay_Interbed_Starting_Compaction';

  kDelayInterbedStartCompactE = 'Delay_Interbed_Starting_Elastic_Compaction';
  kDelayInterbedStartCompactV = 'Delay_Interbed_Starting_Inelastic_Compaction';

  kDelayInterbedEquiv = 'Delay_Interbed_Equivalent_Thickness';
  kCompressibleThickne = 'Compressible_Thickness';
  kInitialElasticSkel = 'Initial_Elastic_Skeletal_Specific_Storage';
  kInitialInelasticSk = 'Initial_Inelastic_Skeletal_Specific_Storage';
  kRecompressionIndex = 'Recompression_Index';
  kCompressionIndex = 'Compression_Index';
  kInitialVoidRatio = 'Initial_Void_Ratio';
  kInitialCompaction = 'Initial_Compaction';

resourcestring
  StrNoDelayPreconsolid = kNoDelayPreconsolid;
  StrNoDelayElasticSke =  kNoDelayElasticSke;
  StrNoDelayInelasticS =  kNoDelayInelasticS;
  StrNoDelayInitialCom =  kNoDelayInitialCom;
  StrNoDelayInitialComE =  kNoDelayInitialComE;
  StrNoDelayInitialComV =  kNoDelayInitialComV;
  StrDelayEquivalentNum = kDelayEquivalentNum;
  StrDelayVerticalHydra = kDelayVerticalHydra;
  StrDelayElasticSpecif = kDelayElasticSpecif;
  StrDelayInelasticSpec = kDelayInelasticSpec;
  StrDelayInterbedStartHead = kDelayInterbedStartHead;
  StrDelayInterbedPreco = kDelayInterbedPreco;
  StrDelayInterbedStartCompact = kDelayInterbedStartCompact;
  StrDelayInterbedStartCompactE = kDelayInterbedStartCompactE;
  StrDelayInterbedStartCompactV = kDelayInterbedStartCompactV;
  StrDelayInterbedEquiv = kDelayInterbedEquiv;
  StrCompressibleThickne  = kCompressibleThickne;
  StrInitialElasticSkel   = kInitialElasticSkel;
  StrInitialInelasticSk   = kInitialInelasticSk;
  StrRecompressionIndex   = kRecompressionIndex;
  StrCompressionIndex     = kCompressionIndex;
  StrInitialVoidRatio     = kInitialVoidRatio;
  StrInitialCompaction    = kInitialCompaction;

procedure InitializeSubsidenceTypes;
begin
  SubsidenceTypes := TStringList.Create;
  SubsidenceUnits := TStringList.Create;
  
  SubsidenceTypes.Add(rsSUBSIDENCE);    SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsLAYERCOMPACT);  SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsNDSYSCOMPACT);  SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsDSYSCOMPACTI);  SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsZDISPLACEMEN);  SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsNDCRITICALHE);  SubsidenceUnits.Add('L');
  SubsidenceTypes.Add(rsDCRITICALHEA);  SubsidenceUnits.Add('L');
end;

procedure InitializeSwtTypes;
begin
  SwtTypes := TStringList.Create;
  SwtTypes.Add(rsSUBSIDENCE2);
  SwtTypes.Add(rsLAYERCOMPACT2);
  SwtTypes.Add(rsSYSTMCOMPACT);
  SwtTypes.Add(rsZDISPLACEMEN2);
  SwtTypes.Add(rsPRECONSOLSTR);
  SwtTypes.Add(rsCHANGEINPCST);
  SwtTypes.Add(rsGEOSTATICSTR);
  SwtTypes.Add(rsCHANGEINGSTR);
  SwtTypes.Add(rsEFFECTIVESTR);
  SwtTypes.Add(rsCHANGEINEFFS);
  SwtTypes.Add(rsVOIDRATIO);
  SwtTypes.Add(rsTHICKNESS);
  SwtTypes.Add(rsCENTERELEVAT);
end;


{ TSubNoDelayBedLayers }

procedure TSubNoDelayBedLayerItem.Assign(Source: TPersistent);
var
  SubSource: TSubNoDelayBedLayerItem;
begin
  inherited;
  // if Assign is updated, update IsSame too.
  if Source is TSubNoDelayBedLayerItem then
  begin
    SubSource := TSubNoDelayBedLayerItem(Source);
    PreconsolidationHeadDataArrayName :=
      SubSource.PreconsolidationHeadDataArrayName;
    ElasticSkeletalStorageCoefficientDataArrayName :=
      SubSource.ElasticSkeletalStorageCoefficientDataArrayName;
    InelasticSkeletalStorageCoefficientDataArrayName :=
      SubSource.InelasticSkeletalStorageCoefficientDataArrayName;
    InitialCompactionDataArrayName := SubSource.InitialCompactionDataArrayName;
    InitialElasticCompactionDataArrayName := SubSource.InitialElasticCompactionDataArrayName;
    InitialInelasticCompactionDataArrayName := SubSource.InitialInelasticCompactionDataArrayName;
  end;
end;

constructor TSubNoDelayBedLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add(kNoDelayPreconsolid);
  FDataArrayTypes.Add(kNoDelayElasticSke);
  FDataArrayTypes.Add(kNoDelayInelasticS);
  FDataArrayTypes.Add(kNoDelayInitialCom);
  FDataArrayTypes.Add(kNoDelayInitialComE);
  FDataArrayTypes.Add(kNoDelayInitialComV);

  FDataArrayDisplayTypes.Add(StrNoDelayPreconsolid);
  FDataArrayDisplayTypes.Add(StrNoDelayElasticSke);
  FDataArrayDisplayTypes.Add(StrNoDelayInelasticS);
  FDataArrayDisplayTypes.Add(StrNoDelayInitialCom);
  FDataArrayDisplayTypes.Add(StrNoDelayInitialComE);
  FDataArrayDisplayTypes.Add(StrNoDelayInitialComV);

  FAssociatedModelDataSetNames.Add('MODFLOW SUB: HC (MODFLOW-2005 Data Set 5, MODFLOW-OWHM) Data Set 8');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Sfe (MODFLOW-2005 Data Set 6, MODFLOW-OWHM) Data Set 9)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Sfv (MODFLOW-2005 Data Set 7, MODFLOW-OWHM) Data Set 10)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Com (MODFLOW-2005 Data Set 8)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: ComE (MODFLOW-OWHM Data Set 11)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: ComV (MODFLOW-OWHM Data Set 12)');
end;

function TSubNoDelayBedLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSubNoDelayBedLayerItem;
begin
  result := (AnotherItem is TSubNoDelayBedLayerItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSubNoDelayBedLayerItem(AnotherItem);
     result :=
       (PreconsolidationHeadDataArrayName =
         SubItem.PreconsolidationHeadDataArrayName)
       and (ElasticSkeletalStorageCoefficientDataArrayName =
         SubItem.ElasticSkeletalStorageCoefficientDataArrayName)
       and (InelasticSkeletalStorageCoefficientDataArrayName =
         SubItem.InelasticSkeletalStorageCoefficientDataArrayName)
       and (InitialCompactionDataArrayName =
         SubItem.InitialCompactionDataArrayName)
       and (InitialElasticCompactionDataArrayName =
         SubItem.InitialElasticCompactionDataArrayName)
       and (InitialInelasticCompactionDataArrayName =
         SubItem.InitialInelasticCompactionDataArrayName)
  end;
end;

procedure TSubNoDelayBedLayerItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.DataArrayManager.GetDataSetByName(ArrayName);
    Assert( DataArray <> nil);
    PhastModel.TopGridObserver.TalksTo(DataArray);
    DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(PreconsolidationHeadDataArrayName);
  UpdateTalksTo(ElasticSkeletalStorageCoefficientDataArrayName);
  UpdateTalksTo(InelasticSkeletalStorageCoefficientDataArrayName);
  if not (PhastModel.ModelSelection in [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
  ]) then
  begin
    UpdateTalksTo(InitialCompactionDataArrayName);
  end
  else
  begin
    UpdateTalksTo(InitialElasticCompactionDataArrayName);
    UpdateTalksTo(InitialInelasticCompactionDataArrayName);
  end;

  Names := TStringList.Create;
  try
    Names.Add(PreconsolidationHeadDataArrayName);
    Names.Add(ElasticSkeletalStorageCoefficientDataArrayName);
    Names.Add(InelasticSkeletalStorageCoefficientDataArrayName);
    Names.Add(InitialCompactionDataArrayName);
    Names.Add(InitialElasticCompactionDataArrayName);
    Names.Add(InitialInelasticCompactionDataArrayName);

    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;
end;

procedure TSubNoDelayBedLayerItem.
  SetElasticSkeletalStorageCoefficientDataArrayName(const Value: string);
begin
  SetDataArrayName(FElasticSkeletalStorageCoefficientDataArrayName,
    Value, FElasticSkeletalStorageCoefficientDisplayName, True);
end;

procedure TSubNoDelayBedLayerItem.
  SetInelasticSkeletalStorageCoefficientDataArrayName(const Value: string);
begin
  SetDataArrayName(FInelasticSkeletalStorageCoefficientDataArrayName,
    Value, FInelasticSkeletalStorageCoefficientDisplayName, True);
end;

procedure TSubNoDelayBedLayerItem.SetInitialCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInitialCompactionDataArrayName,
    Value, FInitialCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in (ModflowSelection - [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ])));

end;

procedure TSubNoDelayBedLayerItem.SetInitialElasticCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInitialElasticCompactionDataArrayName,
    Value, FInitialElasticCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ] ));
//  FInitialElasticCompactionDataArrayName := Value;
end;

procedure TSubNoDelayBedLayerItem.SetInitialInelasticCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInitialInelasticCompactionDataArrayName,
    Value, FInitialInelasticCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ] ));
//  FInitialInelasticCompactionDataArrayName := Value;
end;

procedure TSubNoDelayBedLayerItem.SetPreconsolidationHeadDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FPreconsolidationHeadDataArrayName,
    Value, FPreconsolidationHeadDisplayName, True);
end;

procedure TSubNoDelayBedLayerItem.UpdateArrayNames(NewNames, NewDisplayNames: TStringList);
begin
  Assert(NewNames.Count= 6);
  Assert(NewDisplayNames.Count= 6);

  FPreconsolidationHeadDisplayName := NewDisplayNames[0];
  PreconsolidationHeadDataArrayName := NewNames[0];

  FElasticSkeletalStorageCoefficientDisplayName := NewDisplayNames[1];
  ElasticSkeletalStorageCoefficientDataArrayName := NewNames[1];

  FInelasticSkeletalStorageCoefficientDisplayName := NewDisplayNames[2];
  InelasticSkeletalStorageCoefficientDataArrayName := NewNames[2];

  FInitialCompactionDisplayName := NewDisplayNames[3];
  InitialCompactionDataArrayName := NewNames[3];

  FInitialElasticCompactionDisplayName := NewDisplayNames[4];
  InitialElasticCompactionDataArrayName := NewNames[4];

  FInitialInelasticCompactionDisplayName := NewDisplayNames[5];
  InitialInelasticCompactionDataArrayName := NewNames[5];

  UpdateAssociatedDataSetNames(NewNames);
end;

procedure TSubNoDelayBedLayerItem.UpdateRequiredDataArrays;
var
  NewNames: TStringList;
begin
  PreconsolidationHeadDataArrayName := PreconsolidationHeadDataArrayName;
  ElasticSkeletalStorageCoefficientDataArrayName := ElasticSkeletalStorageCoefficientDataArrayName;
  InelasticSkeletalStorageCoefficientDataArrayName := InelasticSkeletalStorageCoefficientDataArrayName;
  InitialCompactionDataArrayName := InitialCompactionDataArrayName;
  InitialElasticCompactionDataArrayName := InitialElasticCompactionDataArrayName;
  InitialInelasticCompactionDataArrayName := InitialInelasticCompactionDataArrayName;

  NewNames := TStringList.Create;
  try
    NewNames.Add(PreconsolidationHeadDataArrayName);
    NewNames.Add(ElasticSkeletalStorageCoefficientDataArrayName);
    NewNames.Add(InelasticSkeletalStorageCoefficientDataArrayName);
    NewNames.Add(InitialCompactionDataArrayName);
    NewNames.Add(InitialElasticCompactionDataArrayName);
    NewNames.Add(InitialInelasticCompactionDataArrayName);

    UpdateAssociatedDataSetNames(NewNames);
  finally
    NewNames.Free;
  end;
end;

{ TSubDelayBedLayers }

procedure TSubDelayBedLayerItem.Assign(Source: TPersistent);
var
  SubSource: TSubDelayBedLayerItem;
begin
  inherited;
  // if Assign is updated, update IsSame too.
  if Source is TSubDelayBedLayerItem then
  begin
    SubSource := TSubDelayBedLayerItem(Source);
    EquivNumberDataArrayName := SubSource.EquivNumberDataArrayName;
    VerticalHydraulicConductivityDataArrayName :=
      SubSource.VerticalHydraulicConductivityDataArrayName;
    ElasticSpecificStorageDataArrayName :=
      SubSource.ElasticSpecificStorageDataArrayName;
    InelasticSpecificStorageDataArrayName :=
      SubSource.InelasticSpecificStorageDataArrayName;
    InterbedStartingHeadDataArrayName :=
      SubSource.InterbedStartingHeadDataArrayName;
    InterbedPreconsolidationHeadDataArrayName :=
      SubSource.InterbedPreconsolidationHeadDataArrayName;
    InterbedStartingCompactionDataArrayName :=
      SubSource.InterbedStartingCompactionDataArrayName;
    InterbedEquivalentThicknessDataArrayName :=
      SubSource.InterbedEquivalentThicknessDataArrayName;
    InterbedStartingElasticCompactionDataArrayName :=
      SubSource.InterbedStartingElasticCompactionDataArrayName;
    InterbedStartingInelasticCompactionDataArrayName :=
      SubSource.InterbedStartingInelasticCompactionDataArrayName;
  end;
end;

constructor TSubDelayBedLayerItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add(kDelayEquivalentNum);
  FDataArrayTypes.Add(kDelayVerticalHydra);
  FDataArrayTypes.Add(kDelayElasticSpecif);
  FDataArrayTypes.Add(kDelayInelasticSpec);
  FDataArrayTypes.Add(kDelayInterbedStartHead);
  FDataArrayTypes.Add(kDelayInterbedPreco);
  FDataArrayTypes.Add(kDelayInterbedStartCompact);
  FDataArrayTypes.Add(kDelayInterbedEquiv);
  FDataArrayTypes.Add(kDelayInterbedStartCompactE);
  FDataArrayTypes.Add(kDelayInterbedStartCompactV);

  FDataArrayDisplayTypes.Add(StrDelayEquivalentNum);
  FDataArrayDisplayTypes.Add(StrDelayVerticalHydra);
  FDataArrayDisplayTypes.Add(StrDelayElasticSpecif);
  FDataArrayDisplayTypes.Add(StrDelayInelasticSpec);
  FDataArrayDisplayTypes.Add(StrDelayInterbedStartHead);
  FDataArrayDisplayTypes.Add(StrDelayInterbedPreco);
  FDataArrayDisplayTypes.Add(StrDelayInterbedStartCompact);
  FDataArrayDisplayTypes.Add(StrDelayInterbedEquiv);
  FDataArrayDisplayTypes.Add(StrDelayInterbedStartCompactE);
  FDataArrayDisplayTypes.Add(StrDelayInterbedStartCompactV);

  FAssociatedModelDataSetNames.Add('MODFLOW SUB: RNB (Data Set 4)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (MODFLOW Data Set 9, MODFLOW-OWHM Data Set 13); NZ (MODFLOW Data Set 14, MODFLOW-OWHM Data Set 19)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (MODFLOW Data Set 9, MODFLOW-OWHM Data Set 13); NZ (MODFLOW Data Set 14, MODFLOW-OWHM Data Set 19)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DP (MODFLOW Data Set 9, MODFLOW-OWHM Data Set 13); NZ (MODFLOW Data Set 14, MODFLOW-OWHM Data Set 19)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: Dstart (MODFLOW Data Set 10, MODFLOW-OWHM Data Set 14)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DHC (MODFLOW Data Set 11, MODFLOW-OWHM Data Set 15)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DCOM (MODFLOW Data Set 12)');
  FAssociatedModelDataSetNames.Add('MODFLOW SUB: DZ (MODFLOW Data Set 13), MODFLOW-OWHM Data Set 18');
  FAssociatedModelDataSetNames.Add('MODFLOW-OWHM SUB: DCOME (Data Set 16)');
  FAssociatedModelDataSetNames.Add('MODFLOW-OWHM SUB: DCOMV (Data Set 17)');
end;

function TSubDelayBedLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSubDelayBedLayerItem;
begin
  result := (AnotherItem is TSubDelayBedLayerItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSubDelayBedLayerItem(AnotherItem);
     result :=
       (EquivNumberDataArrayName = SubItem.EquivNumberDataArrayName)
       and (VerticalHydraulicConductivityDataArrayName =
         SubItem.VerticalHydraulicConductivityDataArrayName)
       and (ElasticSpecificStorageDataArrayName =
         SubItem.ElasticSpecificStorageDataArrayName)
       and (InelasticSpecificStorageDataArrayName =
         SubItem.InelasticSpecificStorageDataArrayName)
       and (InterbedStartingHeadDataArrayName =
         SubItem.InterbedStartingHeadDataArrayName)
       and (InterbedPreconsolidationHeadDataArrayName =
         SubItem.InterbedPreconsolidationHeadDataArrayName)
       and (InterbedStartingCompactionDataArrayName =
         SubItem.InterbedStartingCompactionDataArrayName)
       and (InterbedEquivalentThicknessDataArrayName =
         SubItem.InterbedEquivalentThicknessDataArrayName)
       and (InterbedStartingElasticCompactionDataArrayName =
         SubItem.InterbedStartingElasticCompactionDataArrayName)
       and (InterbedStartingInelasticCompactionDataArrayName =
         SubItem.InterbedStartingInelasticCompactionDataArrayName)
  end;
end;

procedure TSubDelayBedLayerItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.DataArrayManager.GetDataSetByName(ArrayName);
    Assert( DataArray <> nil);
    PhastModel.TopGridObserver.TalksTo(DataArray);
    DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(EquivNumberDataArrayName);
  UpdateTalksTo(VerticalHydraulicConductivityDataArrayName);
  UpdateTalksTo(ElasticSpecificStorageDataArrayName);
  UpdateTalksTo(InelasticSpecificStorageDataArrayName);
  UpdateTalksTo(InterbedStartingHeadDataArrayName);
  UpdateTalksTo(InterbedPreconsolidationHeadDataArrayName);
  if not (PhastModel.ModelSelection in [msModflowFMP
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
  ]) then
  begin
    UpdateTalksTo(InterbedStartingCompactionDataArrayName);
  end;
  UpdateTalksTo(InterbedEquivalentThicknessDataArrayName);
  if PhastModel.ModelSelection in [msModflowFMP
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
  ] then
  begin
    UpdateTalksTo(InterbedStartingElasticCompactionDataArrayName);
    UpdateTalksTo(InterbedStartingInelasticCompactionDataArrayName);
  end;

  Names := TStringList.Create;
  try
    Names.Add(EquivNumberDataArrayName);
    Names.Add(VerticalHydraulicConductivityDataArrayName);
    Names.Add(ElasticSpecificStorageDataArrayName);
    Names.Add(InelasticSpecificStorageDataArrayName);
    Names.Add(InterbedStartingHeadDataArrayName);
    Names.Add(InterbedPreconsolidationHeadDataArrayName);
    Names.Add(InterbedStartingCompactionDataArrayName);
    Names.Add(InterbedEquivalentThicknessDataArrayName);
    Names.Add(InterbedStartingElasticCompactionDataArrayName);
    Names.Add(InterbedStartingInelasticCompactionDataArrayName);
    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;

end;

procedure TSubDelayBedLayerItem.SetElasticSpecificStorageDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FElasticSpecificStorageDataArrayName,
    Value, FElasticSpecificStorageDisplayName, True);
end;

procedure TSubDelayBedLayerItem.
  SetEquivNumberDataArrayName(const Value: string);
begin
  SetDataArrayName(FEquivNumberDataArrayName,
    Value, FEquivNumberDisplayName, True);
end;

procedure TSubDelayBedLayerItem.SetInelasticSpecificStorageDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInelasticSpecificStorageDataArrayName,
    Value, FInelasticSpecificStorageDisplayName, True);
end;

procedure TSubDelayBedLayerItem.SetInterbedEquivalentThicknessDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedEquivalentThicknessDataArrayName,
    Value, FInterbedEquivalentThicknessDisplayName, True);
end;

procedure TSubDelayBedLayerItem.SetInterbedPreconsolidationHeadDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SubPackage.ReadDelayRestartFileName = '');
  SetDataArrayName(FInterbedPreconsolidationHeadDataArrayName,
    Value, FInterbedPreconsolidationHeadDisplayName,
    CreateDataArray);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingCompactionDataArrayName,
    Value, FInterbedStartingCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in (ModflowSelection - [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ])));
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingElasticCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingElasticCompactionDataArrayName,
    Value, FInterbedStartingElasticCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ]));
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingHeadDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SubPackage.ReadDelayRestartFileName = '');
  SetDataArrayName(FInterbedStartingHeadDataArrayName,
    Value, FInterbedStartingHeadDisplayName, CreateDataArray);
end;

procedure TSubDelayBedLayerItem.SetInterbedStartingInelasticCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FInterbedStartingInelasticCompactionDataArrayName,
    Value, FInterbedStartingInelasticCompactionDisplayName,
    (Model = nil) or (Model.ModelSelection in [msModflowFmp
              {$IFDEF OWHMV2}
              , msModflowOwhm2
              {$ENDIF}
    ]));
end;

procedure TSubDelayBedLayerItem.SetVerticalHydraulicConductivityDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FVerticalHydraulicConductivityDataArrayName,
    Value, FVerticalHydraulicConductivityDisplayName, True);
end;

procedure TSubDelayBedLayerItem.UpdateArrayNames(NewNames, NewDisplayNames: TStringList);
begin
  Assert(NewNames.Count= 10);
  Assert(NewDisplayNames.Count= 10);

  FEquivNumberDisplayName := NewDisplayNames[0];
  EquivNumberDataArrayName := NewNames[0];

  FVerticalHydraulicConductivityDisplayName := NewDisplayNames[1];
  VerticalHydraulicConductivityDataArrayName := NewNames[1];

  FElasticSpecificStorageDisplayName := NewDisplayNames[2];
  ElasticSpecificStorageDataArrayName := NewNames[2];

  FInelasticSpecificStorageDisplayName := NewDisplayNames[3];
  InelasticSpecificStorageDataArrayName := NewNames[3];

  FInterbedStartingHeadDisplayName := NewDisplayNames[4];
  InterbedStartingHeadDataArrayName := NewNames[4];

  FInterbedPreconsolidationHeadDisplayName := NewDisplayNames[5];
  InterbedPreconsolidationHeadDataArrayName := NewNames[5];

  FInterbedStartingCompactionDisplayName := NewDisplayNames[6];
  InterbedStartingCompactionDataArrayName := NewNames[6];

  FInterbedEquivalentThicknessDisplayName := NewDisplayNames[7];
  InterbedEquivalentThicknessDataArrayName := NewNames[7];

  FInterbedStartingElasticCompactionDisplayName := NewDisplayNames[8];
  InterbedStartingElasticCompactionDataArrayName := NewNames[8];

  FInterbedStartingInelasticCompactionDisplayName := NewDisplayNames[9];
  InterbedStartingInelasticCompactionDataArrayName := NewNames[9];

  UpdateAssociatedDataSetNames(NewNames);
end;

procedure TSubDelayBedLayerItem.UpdateRequiredDataArrays;
var
  NewNames: TStringList;
begin
  EquivNumberDataArrayName := EquivNumberDataArrayName;
  VerticalHydraulicConductivityDataArrayName := VerticalHydraulicConductivityDataArrayName;
  ElasticSpecificStorageDataArrayName := ElasticSpecificStorageDataArrayName;
  InelasticSpecificStorageDataArrayName := InelasticSpecificStorageDataArrayName;
  InterbedStartingHeadDataArrayName := InterbedStartingHeadDataArrayName;
  InterbedPreconsolidationHeadDataArrayName := InterbedPreconsolidationHeadDataArrayName;
  InterbedStartingCompactionDataArrayName := InterbedStartingCompactionDataArrayName;
  InterbedEquivalentThicknessDataArrayName := InterbedEquivalentThicknessDataArrayName;
  InterbedStartingElasticCompactionDataArrayName := InterbedStartingElasticCompactionDataArrayName;
  InterbedStartingInelasticCompactionDataArrayName := InterbedStartingInelasticCompactionDataArrayName;

  NewNames := TStringList.Create;
  try
    NewNames.Add(EquivNumberDataArrayName);
    NewNames.Add(VerticalHydraulicConductivityDataArrayName);
    NewNames.Add(ElasticSpecificStorageDataArrayName);
    NewNames.Add(InelasticSpecificStorageDataArrayName);
    NewNames.Add(InterbedStartingHeadDataArrayName);
    NewNames.Add(InterbedPreconsolidationHeadDataArrayName);
    NewNames.Add(InterbedStartingCompactionDataArrayName);
    NewNames.Add(InterbedEquivalentThicknessDataArrayName);
    NewNames.Add(InterbedStartingElasticCompactionDataArrayName);
    NewNames.Add(InterbedStartingInelasticCompactionDataArrayName);

    UpdateAssociatedDataSetNames(NewNames);
  finally
    NewNames.Free;
  end;

end;

{ TSubDelayBedLayers }

function TSubDelayBedLayers.Add: TSubDelayBedLayerItem;
begin
  result := inherited Add as TSubDelayBedLayerItem;
end;

constructor TSubDelayBedLayers.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TSubDelayBedLayerItem, Model);
end;

function TSubDelayBedLayers.GetItem(Index: integer): TSubDelayBedLayerItem;
begin
  result := inherited Items[Index] as TSubDelayBedLayerItem;
end;

procedure TSubDelayBedLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TSubDelayBedLayers.SetItem(Index: integer;
  const Value: TSubDelayBedLayerItem);
begin
  inherited Items[Index] := Value;
end;

procedure TSubDelayBedLayers.UpdateRequiredDataArrays;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].UpdateRequiredDataArrays;
  end;
end;

{ TSubNoDelayBedLayers }

function TSubNoDelayBedLayers.Add: TSubNoDelayBedLayerItem;
begin
  result := inherited Add as TSubNoDelayBedLayerItem;
end;

constructor TSubNoDelayBedLayers.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TSubNoDelayBedLayerItem, Model);
end;

function TSubNoDelayBedLayers.GetItem(Index: integer): TSubNoDelayBedLayerItem;
begin
  result := inherited Items[Index] as TSubNoDelayBedLayerItem;
end;

procedure TSubNoDelayBedLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TSubNoDelayBedLayers.SetItem(Index: integer;
  const Value: TSubNoDelayBedLayerItem);
begin
  inherited Items[Index] := Value;
end;

procedure TSubNoDelayBedLayers.UpdateRequiredDataArrays;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].UpdateRequiredDataArrays;
  end;
end;

{ TUseLayerNumberItem }

procedure TUseLayerNumberItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TUseLayerNumberItem then
  begin
    LayerNumber := TUseLayerNumberItem(Source).LayerNumber;
  end;
  inherited;
end;

function TUseLayerNumberItem.IsSame(AnotherItem: TOrderedItem): boolean;
begin
  result := LayerNumber = TUseLayerNumberItem(AnotherItem).LayerNumber;
end;

procedure TUseLayerNumberItem.SetLayerNumber(const Value: integer);
begin
  SetIntegerProperty(FLayerNumber, Value)
end;

{ TUseLayersCollection }

function TUseLayersCollection.Add: TUseLayerNumberItem;
begin
  result := inherited Add as TUseLayerNumberItem;
end;

constructor TUseLayersCollection.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TUseLayerNumberItem, Model);
end;

function TUseLayersCollection.GetItem(Index: integer): TUseLayerNumberItem;
begin
  result := inherited Items[Index] as TUseLayerNumberItem;
end;

function TUseLayersCollection.GetItemByLayerNumber(
  LayerNumber: integer): TUseLayerNumberItem;
var
  Index: Integer;
  Item: TUseLayerNumberItem;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if Item.LayerNumber = LayerNumber then
    begin
      result := Item;
      Exit;
    end;
  end;
end;

procedure TUseLayersCollection.SetItem(Index: integer;
  const Value: TUseLayerNumberItem);
begin
  inherited Items[Index] := Value;
end;

{ TCustomSubLayers }

procedure TCustomSubLayerItem.Assign(Source: TPersistent);
var
  SubSource: TCustomSubLayerItem;
begin
  // if Assign is updated, update IsSame too.
  if Source is TCustomSubLayerItem then
  begin
    SubSource := TCustomSubLayerItem(Source);
    Name := SubSource.Name;
    UsedLayers := SubSource.UsedLayers;
    UseInAllLayers := SubSource.UseInAllLayers;
  end;
  inherited;
end;

constructor TCustomSubLayerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FAssociatedModelDataSetNames:= TStringList.Create;
  FDataArrayTypes := TStringList.Create;
  FDataArrayDisplayTypes := TStringList.Create;
  FUsedLayers := TUseLayersCollection.Create(
    (Collection as TOrderedCollection).Model as TCustomModel);
end;

destructor TCustomSubLayerItem.Destroy;
var
  ScreenObjectIndex: Integer;
  LocalModel: TCustomModel;
  AScreenObject: TScreenObject;
  SubObservations: TSubObservations;
  ObsIndex: Integer;
  SubObs: TCustomSubObsItem;
begin
  FUsedLayers.Free;
  FDataArrayDisplayTypes.Free;
  FDataArrayTypes.Free;
  FAssociatedModelDataSetNames.Free;

  if (Model <> nil) and not (csDestroying in (Model as TComponent).ComponentState) then
  begin
    LocalModel := Model as TCustomModel;
    for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
      SubObservations := AScreenObject.ModflowSubObservations;
      if SubObservations <> nil then
      begin
        for ObsIndex := 0 to SubObservations.Count - 1 do
        begin
          SubObs := SubObservations[ObsIndex];
          if SubObs.Interbed = self then
          begin
            SubObs.Interbed := nil;
          end;
        end;
      end;
    end;
  end;

  inherited;
end;

function TCustomSubLayerItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TCustomSubLayerItem;
begin
  result := AnotherItem is TCustomSubLayerItem;
  if result then
  begin
    SubItem := TCustomSubLayerItem(AnotherItem);
    result := (Name = SubItem.Name)
      and (UseInAllLayers = SubItem.UseInAllLayers)
      and UsedLayers.IsSame(SubItem.UsedLayers)
  end;
end;

procedure TCustomSubLayerItem.SetArrayNames(NewName: string);
var
  NewNames, NewDisplayNames: TStringList;
  ANewName: string;
  Index: integer;
begin
  NewNames := TStringList.Create;
  NewDisplayNames := TStringList.Create;
  try
    NewName := ValidName(NewName);
    NewNames.Capacity := FDataArrayTypes.Count;
    NewDisplayNames.Capacity := FDataArrayDisplayTypes.Count;
    for Index := 0 to FDataArrayTypes.Count - 1 do
    begin
      ANewName := NewName + '_' + FDataArrayTypes[Index];
      NewNames.Add(ANewName);
      ANewName := NewName + '_' + FDataArrayDisplayTypes[Index];
      NewDisplayNames.Add(ANewName);
    end;
    UpdateArrayNames(NewNames, NewDisplayNames);
  finally
    NewDisplayNames.Free;
    NewNames.Free;
  end;
end;

procedure TCustomSubLayerItem.SetDataArrayName(var StoredName: string;
  NewName, NewDisplayName: string; CreateDataArray: boolean);
var
  LocalModel: TPhastModel;
  DataArray: TDataArray;
  NewFormula: string;
//  Compiler: TRbwParser;
//  Position: Integer;
  LocalCollection: TCustomSubLayer;
//  NewNames: TStringList;
begin
  LocalCollection := Collection as TCustomSubLayer;
//  NewNames := TStringList.Create;
  try
    if LocalCollection.Model <> nil then
    begin
      LocalModel := Model as TPhastModel;
      if not (csLoading in LocalModel.ComponentState) then
      begin
        if StoredName = '' then
        begin
          DataArray := nil;
        end
        else
        begin
          DataArray := LocalModel.DataArrayManager.GetDataSetByName(StoredName);
        end;
        if DataArray = nil then
        begin
          DataArray := LocalModel.DataArrayManager.GetDataSetByName(NewName);
          if DataArray <> nil then
          begin
            DataArray.OnDataSetUsed := LocalModel.SubsidenceDataArrayUsed;
            LocalCollection.AddOwnedDataArray(DataArray);
          end;
        end;
        if DataArray <> nil then
        begin
          // rename data array.
          LocalModel.TopGridObserver.StopsTalkingTo(DataArray);
          DataArray.StopsTalkingTo(LocalModel.ThreeDGridObserver);

          DataArray.Classification := StrSubSidence + '|' + Name;
          if StoredName <> NewName then
          begin
            LocalModel.RenameDataArray(DataArray, NewName, NewDisplayName);
          end;
        end
        else if CreateDataArray then
        begin
          // create a new data array.

          // First get formula for new layer.
          NewFormula := '0.';

          // create new data array.
          DataArray := LocalModel.DataArrayManager.CreateNewDataArray(TDataArray,
            NewName, NewFormula, NewDisplayName,
            [dcName, dcType, dcOrientation, dcEvaluatedAt],
            rdtDouble, eaBlocks, dsoTop, StrSubSidence + '|' + Name);
          DataArray.OnDataSetUsed := LocalModel.SubsidenceDataArrayUsed;

          LocalCollection.AddOwnedDataArray(DataArray);
//          NewNames.Add(DataArray.Name);
        end;
        if (DataArray <> nil) and (not LocalModel.Clearing) then
        begin
          LocalModel.TopGridObserver.TalksTo(DataArray);
//          DataArray.TalksTo(LocalModel.ThreeDGridObserver);
//          LocalModel.ThreeDGridObserver.StopsTalkingTo(DataArray);

          DataArray.UpdateDimensions(LocalModel.LayerCount,
            LocalModel.RowCount, LocalModel.ColumnCount);
//          DataArray.AssociatedDataSets := AssociatedDataSets;
        end;
      end;
    end;
//    if NewNames.Count > 0 then
//    begin
//      UpdateAssociatedDataSetNames(NewNames);
//    end;
  finally
//    NewNames.Free;
    if StoredName <> NewName then
    begin
      StoredName := NewName;
      InvalidateModel;
    end;
  end;
end;

procedure TCustomSubLayerItem.SetName(const Value: string);
begin
  SetCaseSensitiveStringProperty(FName, Value);
  SetArrayNames(FName);
end;


procedure TCustomSubLayerItem.SetUsedLayers(const Value: TUseLayersCollection);
begin
  FUsedLayers.Assign(Value);
end;

procedure TCustomSubLayerItem.SetUseInAllLayers(const Value: boolean);
begin
  SetBooleanProperty(FUseInAllLayers, Value);
end;

procedure TCustomSubLayerItem.UpdateAssociatedDataSetNames(
  NewNames: TStringList);
var
  LocalCollection: TCustomSubLayer;
  Model : TPhastModel;
  DataArray: TDataArray;
  Index: integer;
begin
  LocalCollection := Collection as TCustomSubLayer;
  if LocalCollection.Model <> nil then
  begin
    Model := LocalCollection.Model as TPhastModel;
    if not (csLoading in Model.ComponentState) then
    begin
      Assert(NewNames.Count = FAssociatedModelDataSetNames.Count);
      for Index := 0 to NewNames.Count - 1 do
      begin
        DataArray := Model.DataArrayManager.GetDataSetByName(NewNames[Index]);
        if DataArray <> nil then
        begin
          DataArray.AssociatedDataSets := FAssociatedModelDataSetNames[Index];
        end;
      end;
    end;
  end;

end;

{ TSubWaterTableItem }

procedure TSwtWaterTableItem.Assign(Source: TPersistent);
var
  SubItem: TSwtWaterTableItem;
begin
  inherited;
  // if Assign is updated, update IsSame too.
  if Source is TSwtWaterTableItem then
  begin
     SubItem := TSwtWaterTableItem(Source);
     WaterTableCompressibleThicknessDataArrayName :=
       SubItem.WaterTableCompressibleThicknessDataArrayName;
     WaterTableInitialElasticSkeletalSpecificStorageDataArrayName :=
       SubItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName;
     WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName :=
       SubItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName;
     WaterTableRecompressionIndexDataArrayName :=
       SubItem.WaterTableRecompressionIndexDataArrayName;
     WaterTableCompressionIndexDataArrayName :=
       SubItem.WaterTableCompressionIndexDataArrayName;
     WaterTableInitialVoidRatioDataArrayName :=
       SubItem.WaterTableInitialVoidRatioDataArrayName;
     WaterTableInitialCompactionDataArrayName :=
       SubItem.WaterTableInitialCompactionDataArrayName;
  end;
end;

constructor TSwtWaterTableItem.Create(Collection: TCollection);
begin
  inherited;
  FDataArrayTypes.Add(kCompressibleThickne);
  FDataArrayTypes.Add(kInitialElasticSkel);
  FDataArrayTypes.Add(kInitialInelasticSk);
  FDataArrayTypes.Add(kRecompressionIndex);
  FDataArrayTypes.Add(kCompressionIndex);
  FDataArrayTypes.Add(kInitialVoidRatio);
  FDataArrayTypes.Add(kInitialCompaction);

  FDataArrayDisplayTypes.Add(StrCompressibleThickne);
  FDataArrayDisplayTypes.Add(StrInitialElasticSkel);
  FDataArrayDisplayTypes.Add(StrInitialInelasticSk);
  FDataArrayDisplayTypes.Add(StrRecompressionIndex);
  FDataArrayDisplayTypes.Add(StrCompressionIndex);
  FDataArrayDisplayTypes.Add(StrInitialVoidRatio);
  FDataArrayDisplayTypes.Add(StrInitialCompaction);

  FAssociatedModelDataSetNames.Add('MODFLOW SWT: THICK (Data Set 7)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Sse (Data Set 8)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Ssv (Data Set 9)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Cr (Data Set 10)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: Cc (Data Set 11)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: VOID (Data Set 12)');
  FAssociatedModelDataSetNames.Add('MODFLOW SWT: SUB (Data Set 13)');
end;

function TSwtWaterTableItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  SubItem: TSwtWaterTableItem;
begin
  result := (AnotherItem is TSwtWaterTableItem)
    and inherited IsSame(AnotherItem);
  if result then
  begin
     SubItem := TSwtWaterTableItem(AnotherItem);
     result :=
       (WaterTableCompressibleThicknessDataArrayName =
         SubItem.WaterTableCompressibleThicknessDataArrayName)
       and (WaterTableInitialElasticSkeletalSpecificStorageDataArrayName =
         SubItem.WaterTableInitialElasticSkeletalSpecificStorageDataArrayName)
       and (WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName =
         SubItem.WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName)
       and (WaterTableRecompressionIndexDataArrayName =
         SubItem.WaterTableRecompressionIndexDataArrayName)
       and (WaterTableCompressionIndexDataArrayName =
         SubItem.WaterTableCompressionIndexDataArrayName)
       and (WaterTableInitialVoidRatioDataArrayName =
         SubItem.WaterTableInitialVoidRatioDataArrayName)
       and (WaterTableInitialCompactionDataArrayName =
         SubItem.WaterTableInitialCompactionDataArrayName)
  end;
end;

procedure TSwtWaterTableItem.Loaded;
var
  PhastModel: TPhastModel;
  Names: TStringList;
  procedure UpdateTalksTo(Const ArrayName: string);
  var
    DataArray: TDataArray;
  begin
    DataArray := PhastModel.DataArrayManager.GetDataSetByName(ArrayName);
    if( DataArray <> nil) then
    begin
      PhastModel.TopGridObserver.TalksTo(DataArray);
      DataArray.OnDataSetUsed := PhastModel.SubsidenceDataArrayUsed;
    end;
  end;
begin
  PhastModel := Model as TPhastModel;
  UpdateTalksTo(WaterTableCompressibleThicknessDataArrayName);
  UpdateTalksTo(WaterTableInitialElasticSkeletalSpecificStorageDataArrayName);
  UpdateTalksTo(WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName);
  UpdateTalksTo(WaterTableRecompressionIndexDataArrayName);
  UpdateTalksTo(WaterTableCompressionIndexDataArrayName);
  UpdateTalksTo(WaterTableInitialVoidRatioDataArrayName);
  UpdateTalksTo(WaterTableInitialCompactionDataArrayName);

  Names := TStringList.Create;
  try
    Names.Add(WaterTableCompressibleThicknessDataArrayName);
    Names.Add(WaterTableInitialElasticSkeletalSpecificStorageDataArrayName);
    Names.Add(WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName);
    Names.Add(WaterTableRecompressionIndexDataArrayName);
    Names.Add(WaterTableCompressionIndexDataArrayName);
    Names.Add(WaterTableInitialVoidRatioDataArrayName);
    Names.Add(WaterTableInitialCompactionDataArrayName);
    UpdateAssociatedDataSetNames(Names);
  finally
    Names.Free;
  end;

end;

procedure TSwtWaterTableItem.SetWaterTableCompressibleThicknessDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableCompressibleThicknessDataArrayName,
    Value, FWaterTableCompressibleThicknessDisplayName, True);
end;

procedure TSwtWaterTableItem.SetWaterTableCompressionIndexDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csCompressionReComp);
  SetDataArrayName(FWaterTableCompressionIndexDataArrayName,
    Value, FWaterTableCompressionIndexDisplayName,
    CreateDataArray);
end;

procedure TSwtWaterTableItem.SetWaterTableInitialCompactionDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableInitialCompactionDataArrayName,
    Value, FWaterTableInitialCompactionDisplayName, True);
end;

procedure TSwtWaterTableItem.
  SetWaterTableInitialElasticSkeletalSpecificStorageDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csSpecificStorage);
  SetDataArrayName(
    FWaterTableInitialElasticSkeletalSpecificStorageDataArrayName,
    Value, FWaterTableInitialElasticSkeletalSpecificStorageDisplayName,
    CreateDataArray);
end;

procedure TSwtWaterTableItem.
  SetWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csSpecificStorage);
  SetDataArrayName(
    FWaterTableInitialInelasticSkeletalSpecificStorageDataArrayName,
    Value, FWaterTableInitialInelasticSkeletalSpecificStorageDisplayName,
    CreateDataArray);
end;

procedure TSwtWaterTableItem.SetWaterTableInitialVoidRatioDataArrayName(
  const Value: string);
begin
  SetDataArrayName(FWaterTableInitialVoidRatioDataArrayName,
    Value, FWaterTableInitialVoidRatioDisplayName, True);
end;

procedure TSwtWaterTableItem.SetWaterTableRecompressionIndexDataArrayName(
  const Value: string);
var
  CreateDataArray: Boolean;
begin
  CreateDataArray := (Model <> nil) and ((Model as TPhastModel).ModflowPackages.
    SwtPackage.CompressionSource = csCompressionReComp);
  SetDataArrayName(FWaterTableRecompressionIndexDataArrayName,
    Value, FWaterTableRecompressionIndexDisplayName, CreateDataArray);
end;

procedure TSwtWaterTableItem.UpdateArrayNames(NewNames, NewDisplayNames: TStringList);
begin
  Assert(NewNames.Count= 7);
  Assert(NewDisplayNames.Count= 7);

  FWaterTableCompressibleThicknessDisplayName := NewDisplayNames[0];
  WaterTableCompressibleThicknessDataArrayName := NewNames[0];

  FWaterTableInitialElasticSkeletalSpecificStorageDisplayName := NewDisplayNames[1];
  WaterTableInitialElasticSkeletalSpecificStorageDataArrayName := NewNames[1];

  FWaterTableInitialInelasticSkeletalSpecificStorageDisplayName := NewDisplayNames[2];
  WaterTableInitialInelasticSkeletalSpecificStorageDataArrayName := NewNames[2];

  FWaterTableRecompressionIndexDisplayName := NewDisplayNames[3];
  WaterTableRecompressionIndexDataArrayName := NewNames[3];

  FWaterTableCompressionIndexDisplayName := NewDisplayNames[4];
  WaterTableCompressionIndexDataArrayName := NewNames[4];

  FWaterTableInitialVoidRatioDisplayName := NewDisplayNames[5];
  WaterTableInitialVoidRatioDataArrayName := NewNames[5];

  FWaterTableInitialCompactionDisplayName := NewDisplayNames[6];
  WaterTableInitialCompactionDataArrayName := NewNames[6];

  UpdateAssociatedDataSetNames(NewNames);

end;

{ TWaterTableLayers }

function TWaterTableLayers.Add: TSwtWaterTableItem;
begin
  result := inherited Add as TSwtWaterTableItem
end;

constructor TWaterTableLayers.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TSwtWaterTableItem, Model);
end;

function TWaterTableLayers.GetItem(Index: integer): TSwtWaterTableItem;
begin
  result := inherited Items[Index] as TSwtWaterTableItem
end;

procedure TWaterTableLayers.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TWaterTableLayers.SetItem(Index: integer;
  const Value: TSwtWaterTableItem);
begin
  inherited Items[Index] := Value;
end;

{ TSubObsItem }

procedure TCustomSubObsItem.Assign(Source: TPersistent);
var
  SubSource: TCustomSubObsItem;
begin
  if Source is TCustomSubObsItem then
  begin
    SubSource := TCustomSubObsItem(Source);
    ObsType := SubSource.ObsType;
    InterbedSystem := SubSource.InterbedSystem;
    Interbed := SubSource.Interbed;
  end;
  inherited;
end;

function TCustomSubObsItem.GetInterbedSystem: string;
begin
  if FInterbed <> nil then
  begin
    result := FInterbed.Name
  end
  else
  begin
    result := FInterbedSystem;
  end;
end;

function TSubObsItem.GetObsTypeIndex: Integer;
begin
  result := SubsidenceTypes.IndexOf(ObsType);
end;

function TCustomSubObsItem.GetObsTypeString: string;
begin
  result := ObsType;
end;

procedure TCustomSubObsItem.Loaded;
var
  LayerGroups: TLayerStructure;
  LayerGroupIndex: Integer;
  LayerGroup: TLayerGroup;
  InterbedIndex: Integer;
  Item: TCustomSubLayerItem;
begin
  LayerGroups := frmGoPhast.PhastModel.LayerStructure;
  for LayerGroupIndex := 0 to LayerGroups.Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    for InterbedIndex := 0 to LayerGroup.SubNoDelayBedLayers.Count - 1 do
    begin
      Item := LayerGroup.SubNoDelayBedLayers[InterbedIndex];
      if InterbedSystem = Item.Name then
      begin
        Interbed := Item;
        Exit;
      end;
    end;
    for InterbedIndex := 0 to LayerGroup.SubDelayBedLayers.Count - 1 do
    begin
      Item := LayerGroup.SubDelayBedLayers[InterbedIndex];
      if InterbedSystem = Item.Name then
      begin
        Interbed := Item;
        Exit;
      end;
    end;
    for InterbedIndex := 0 to LayerGroup.WaterTableLayers.Count - 1 do
    begin
      Item := LayerGroup.WaterTableLayers[InterbedIndex];
      if InterbedSystem = Item.Name then
      begin
        Interbed := Item;
        Exit;
      end;
    end;
  end;
end;

function TCustomSubObsItem.ObservationType: string;
begin
  result := ObsType;
end;

procedure TCustomSubObsItem.SetInterbed(const Value: TCustomSubLayerItem);
begin
  FInterbed := Value;
  if FInterbed <> nil then
  begin
    InterbedSystem := FInterbed.Name;
  end;
end;

procedure TCustomSubObsItem.SetInterbedSystem(const Value: string);
begin
  if FInterbedSystem <> Value then
  begin
    BeginUpdate;
    try
      FInterbedSystem := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TCustomSubObsItem.SetObsType(const Value: string);
begin
  if FObsType <> Value then
  begin
    BeginUpdate;
    try
      FObsType := Value;
      InvalidateModel;
    finally
      EndUpdate;
    end;
  end;
end;

procedure TSubObsItem.SetObsTypeIndex(Value: Integer);
begin
  ObsType := SubsidenceTypes[Value];
end;

procedure TCustomSubObsItem.SetObsTypeString(const Value: string);
begin
  ObsType := Value;
end;

function TCustomSubObsItem.Units: string;
begin
  result := SubsidenceTypes[ObsTypeIndex];
end;

{ TSubObservations }

function TSubObservations.Add: TSubObsItem;
begin
  result := inherited Add as TSubObsItem;
end;

constructor TSubObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TSubObsItem, InvalidateModelEvent, ScreenObject);
end;

function TSubObservations.GetSubItem(Index: Integer): TSubObsItem;
begin
  result := inherited Items[Index] as TSubObsItem;
end;

function TCustomSubObservations.GetUsed: Boolean;
begin
  result := Count > 0;
end;

procedure TSubObservations.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSubObservations.SetSubItem(Index: Integer;
  const Value: TSubObsItem);
begin
  inherited Items[Index] := Value;
end;

{ TSwtObsItem }

function TSwtObsItem.GetObsTypeIndex: Integer;
begin
  result := SwtTypes.IndexOf(ObsType);
end;

procedure TSwtObsItem.SetObsTypeIndex(Value: Integer);
begin
  ObsType := SwtTypes[Value];
end;

{ TSwtObservations }

function TSwtObservations.Add: TSwtObsItem;
begin
  result := inherited Add as TSwtObsItem;
end;

constructor TSwtObservations.Create(InvalidateModelEvent: TNotifyEvent;
  ScreenObject: TObject);
begin
  inherited Create(TSwtObsItem, InvalidateModelEvent, ScreenObject);
end;

function TSwtObservations.GetSwtItem(Index: Integer): TSwtObsItem;
begin
  result := inherited Items[Index] as TSwtObsItem;
end;

procedure TSwtObservations.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TSwtObservations.SetSwtItem(Index: Integer; const Value: TSwtObsItem);
begin
  inherited Items[Index] := Value;
end;

initialization;
  InitializeSubsidenceTypes;
  InitializeSwtTypes;

finalization
  SubsidenceTypes.Free;
  SubsidenceUnits.Free;
  SwtTypes.Free;

end.

