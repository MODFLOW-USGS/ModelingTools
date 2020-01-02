unit ModflowCsubUnit;

interface

uses
  GoPhastTypes, System.Classes;

type
  TCSubPackageData = class(TPhastCollectionItem)
  private
    FStoredInitialElasticSpecificStorage: TRealStorage;
    FStoredInitialInelasticSpecificStorage: TRealStorage;
    FStoredThickness: TRealStorage;
    FStoredInitialDelayHeadOffset: TRealStorage;
    FStoredInitialPorosity: TRealStorage;
    FStoredDelayKv: TRealStorage;
    FStoredInitialOffset: TRealStorage;
    FInterbedSystemName: string;
    FStoredEquivInterbedNumber: TRealStorage;
    procedure SetStoredDelayKv(const Value: TRealStorage);
    procedure SetStoredEquivInterbedNumber(const Value: TRealStorage);
    procedure SetStoredInitialDelayHeadOffset(const Value: TRealStorage);
    procedure SetStoredInitialElasticSpecificStorage(const Value: TRealStorage);
    procedure SetStoredInitialInelasticSpecificStorage(const Value: TRealStorage);
    procedure SetStoredInitialPorosity(const Value: TRealStorage);
    procedure SetInterbedSystemName(const Value: string);
    procedure SetStoredInitialOffset(const Value: TRealStorage);
    procedure SetStoredThickness(const Value: TRealStorage);
    function GetInterbedSystemName: string;
    function GetInitialOffset: Double;
    procedure SetInitialOffset(const Value: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property InitialOffset: Double read GetInitialOffset write SetInitialOffset;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
//    property InitialOffset: Double read Get write Set;
  published
    property InterbedSystemName: string read GetInterbedSystemName write SetInterbedSystemName;
    {pcs0—is the initial offset from the calculated initial effective stress or initial preconsolidation
    stress in the interbed, in units of height of a column of water. PCS0 is the initial
    preconsolidation stress if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    INITIAL PRECONSOLIDATION STRESS are specified in the OPTIONS block. If
    HEAD BASED is specified in the OPTIONS block, PCS0 is the initial offset from the calculated
    initial head or initial preconsolidation head in the CSUB interbed and the initial preconsolidation
    stress is calculated from the calculated initial effective stress or calculated initial geostatic stress,
    respectively.}
    property StoredInitialOffset: TRealStorage read FStoredInitialOffset write SetStoredInitialOffset;
    {thick frac—is the interbed thickness or cell fraction of the interbed. Interbed thickness is specified as
    a fraction of the cell thickness if CELL FRACTION is specified in the OPTIONS block.}
    property StoredThickness: TRealStorage read FStoredThickness write SetStoredThickness;
    {rnb—is the interbed material factor equivalent number of interbeds in the interbed system represented
    by the interbed. RNB must be greater than or equal to 1 if CDELAY is DELAY. Otherwise, RNB
    can be any value.}
    property StoredEquivInterbedNumber: TRealStorage read FStoredEquivInterbedNumber write SetStoredEquivInterbedNumber;
    {ssv cc—is the initial inelastic specific storage or compression index of the interbed. The compression
    index is specified if COMPRESSION INDICES is specified in the OPTIONS block. Specified
    or calculated interbed inelastic specific storage values are not adjusted from initial values if
    HEAD BASED is specified in the OPTIONS block.}
    property StoredInitialInelasticSpecificStorage: TRealStorage read FStoredInitialInelasticSpecificStorage write SetStoredInitialInelasticSpecificStorage;
    //sse cr—is the initial elastic coarse-grained material specific storage or recompression index of the
    //interbed. The recompression index is specified if COMPRESSION INDICES is specified in the
    //OPTIONS block. Specified or calculated interbed elastic specific storage values are not adjusted
    //from initial values if HEAD BASED is specified in the OPTIONS block.
    property StoredInitialElasticSpecificStorage: TRealStorage read FStoredInitialElasticSpecificStorage write SetStoredInitialElasticSpecificStorage;
    //theta—is the initial porosity of the interbed.
    property StoredInitialPorosity: TRealStorage read FStoredInitialPorosity write SetStoredInitialPorosity;
    //kv—is the vertical hydraulic conductivity of the delay interbed. KV must be greater than 0 if CDELAY
    //is DELAY. Otherwise, KV can be any value.
    property StoredDelayKv: TRealStorage read FStoredDelayKv write SetStoredDelayKv;
    //h0—is the initial offset from the head in cell cellid or the initial head in the delay interbed. H0
    //is the initial head in the delay bed if SPECIFIED INITIAL INTERBED STATE or SPECIFIED
    //INITIAL DELAY HEAD are specified in the OPTIONS block. H0 can be any value if CDELAY
    //is NODELAY.
    property StoredInitialDelayHeadOffset: TRealStorage read FStoredInitialDelayHeadOffset write SetStoredInitialDelayHeadOffset;
  end;

implementation

{ TCSubPackageData }

procedure TCSubPackageData.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TCSubPackageData.Create(Collection: TCollection);
begin
  inherited;
  FStoredInitialElasticSpecificStorage := TRealStorage.Create;
  FStoredInitialElasticSpecificStorage.OnChange := OnInvalidateModel;

  FStoredInitialInelasticSpecificStorage := TRealStorage.Create;
  FStoredInitialInelasticSpecificStorage.OnChange := OnInvalidateModel;

  FStoredThickness := TRealStorage.Create;
  FStoredThickness.OnChange := OnInvalidateModel;

  FStoredInitialDelayHeadOffset := TRealStorage.Create;
  FStoredInitialDelayHeadOffset.OnChange := OnInvalidateModel;

  FStoredInitialPorosity := TRealStorage.Create;
  FStoredInitialPorosity.OnChange := OnInvalidateModel;

  FStoredDelayKv := TRealStorage.Create;
  FStoredDelayKv.OnChange := OnInvalidateModel;

  FStoredInitialOffset := TRealStorage.Create;
  FStoredInitialOffset.OnChange := OnInvalidateModel;

  FStoredEquivInterbedNumber := TRealStorage.Create;
  FStoredEquivInterbedNumber.OnChange := OnInvalidateModel;
end;

destructor TCSubPackageData.Destroy;
begin
  FStoredEquivInterbedNumber.Free;
  FStoredInitialOffset.Free;
  FStoredDelayKv.Free;
  FStoredInitialPorosity.Free;
  FStoredInitialDelayHeadOffset.Free;
  FStoredThickness.Free;
  FStoredInitialInelasticSpecificStorage.Free;
  FStoredInitialElasticSpecificStorage.Free;

  inherited;
end;

function TCSubPackageData.GetInitialOffset: Double;
begin
  result := StoredInitialOffset.Value;
end;

function TCSubPackageData.GetInterbedSystemName: string;
begin
  Result := FInterbedSystemName
end;

procedure TCSubPackageData.SetStoredDelayKv(const Value: TRealStorage);
begin
  FStoredDelayKv.Assign(Value);
end;

procedure TCSubPackageData.SetStoredEquivInterbedNumber(const Value: TRealStorage);
begin
  FStoredEquivInterbedNumber.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialDelayHeadOffset(const Value: TRealStorage);
begin
  FStoredInitialDelayHeadOffset.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialElasticSpecificStorage(
  const Value: TRealStorage);
begin
  FStoredInitialElasticSpecificStorage.Assign(Value);
end;

procedure TCSubPackageData.SetStoredInitialInelasticSpecificStorage(
  const Value: TRealStorage);
begin
  FStoredInitialInelasticSpecificStorage.Assign(Value);
end;

procedure TCSubPackageData.SetInitialOffset(const Value: Double);
begin
  StoredInitialOffset.Value := Value;
end;

procedure TCSubPackageData.SetStoredInitialPorosity(const Value: TRealStorage);
begin
  FStoredInitialPorosity.Assign(Value);
end;

procedure TCSubPackageData.SetInterbedSystemName(const Value: string);
begin
  FInterbedSystemName := Value;
end;

procedure TCSubPackageData.SetStoredInitialOffset(const Value: TRealStorage);
begin
  FStoredInitialOffset.Assign(Value);
end;

procedure TCSubPackageData.SetStoredThickness(const Value: TRealStorage);
begin
  FStoredThickness.Assign(Value);
end;

end.
