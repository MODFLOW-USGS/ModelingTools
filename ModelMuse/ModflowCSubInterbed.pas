unit ModflowCSubInterbed;

interface

uses
  OrderedCollectionUnit, System.Classes, GoPhastTypes, System.SysUtils,
  DataSetUnit, RbwParser, OrderedCollectionInterfaceUnit;

type
  TCSubInterbedType = (itNoDelay, itDelay);

  TCSubInterbed = class(TOrderedItem)
  private
    FName: string;
    FInterbedType: TCSubInterbedType;
    FDelayKvName: string;
    FEquivInterbedNumberName: string;
    FInitialDelayHeadOffset: string;
    FInitialElasticSpecificStorage: string;
    FInitialInelasticSpecificStorage: string;
    FInitialOffset: string;
    FInitialPorosity: string;
    FThickness: string;
    FCSubBoundName: string;
    FDataSetNames: TStringList;
    procedure SetInterbedType(const Value: TCSubInterbedType);
    procedure SetName(const Value: string);
    procedure RenameInterbed(const NewInterbedName: string);
    procedure CreateOrRenameDataArray(var InterbedName: string; DataType: TRbwDataType;
      Extension, DisplayExtension: string; const NewInterbedName: string;
      AModel: TBaseModel);
    function GetDataSetNames: TStringList;
    procedure UpdateInterbedDataSetNames;
    procedure Loaded;
//    procedure UpdatePackageDataFormulas;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
    function DataArrayUsed(ADataArray: TDataArray): Boolean;
    // @name defines the name of the @link(TDataArray) that specifies pcs0
    property InitialOffset: string read FInitialOffset;
    // @name defines the name of the @link(TDataArray) that specifies thick_frac
    property Thickness: string read FThickness;
    // @name defines the name of the @link(TDataArray) that specifies rnb
    property EquivInterbedNumberName: string read FEquivInterbedNumberName;
    // @name defines the name of the @link(TDataArray) that specifies ssv_cc
    property InitialInelasticSpecificStorage: string read FInitialInelasticSpecificStorage;
    // @name defines the name of the @link(TDataArray) that specifies sse_cr
    property InitialElasticSpecificStorage: string read FInitialElasticSpecificStorage;
    // @name defines the name of the @link(TDataArray) that specifies theta
    property InitialPorosity: string read FInitialPorosity;
    // @name defines the name of the @link(TDataArray) that specifies kv
    property DelayKvName: string read FDelayKvName;
    // @name defines the name of the @link(TDataArray) that specifies boundname
    property CSubBoundName: string read FCSubBoundName;
    // @name defines the name of the @link(TDataArray) that specifies h0
    property InitialDelayHeadOffset: string read FInitialDelayHeadOffset;
    property DataSetNames: TStringList read GetDataSetNames;
  published
    property Name: string read FName write SetName;
    property InterbedType: TCSubInterbedType read FInterbedType write SetInterbedType;
  end;

  TCSubInterbeds = class(TLayerOwnerCollection)
  private
    function GetItem(Index: Integer): TCSubInterbed;
    procedure SetItem(Index: Integer; const Value: TCSubInterbed);
  public
    function Add: TCSubInterbed;
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    property Items[Index: Integer]: TCSubInterbed read GetItem write SetItem; default;
    function DataArrayUsed(ADataArray: TDataArray): Boolean;
    function GetInterbedByName(AName: string): TCSubInterbed;
    procedure UpdateInterbedDataSetNames;
    procedure Loaded;
  end;

implementation

uses
  PhastModelUnit, GlobalVariablesUnit, frmGoPhastUnit,
  ModflowPackagesUnit, ModflowPackageSelectionUnit, DataArrayManagerUnit,
  DataSetNamesUnit;

const
  KDelayKv = 'DelayKv';
  KEquivInterbedNumber = 'EquivInterbedNumber';
  KInitialDelayHeadOffs = 'InitialDelayHeadOffset';
  KInitialElasticSpecif = 'InitialElasticSpecificStorage';
  KInitialInelasticSpec = 'InitialInelasticSpecificStorage';
  KInitialOffset = 'InitialStressOffset';
  KInitialPorosity = 'InitialPorosity';
  KThickness = 'InterbedThickness';
  KInitialPreconsolidat = 'InitialPreconsolidationStress';
  KCellThicknessFractio = 'CellThicknessFraction';
  KInitialInelasticComp = 'InitialInelasticRecompressionIndex';
  KInitialElasticCompre = 'InitialElasticCompressionIndex';
  KInitialDelayHead = 'InitialDelayHead';
  
  KCSubBoundName = 'CSUB_BoundName';

resourcestring
  StrDelayKv = KDelayKv;
  StrEquivInterbedNumber = KEquivInterbedNumber;
  StrInitialDelayHeadOffs = KInitialDelayHeadOffs;
  StrInitialElasticSpecif = KInitialElasticSpecif;
  StrInitialInelasticSpec = KInitialInelasticSpec;
  StrInitialOffset = KInitialOffset;
  StrInitialPorosity = KInitialPorosity;
  StrThickness = KThickness;
  StrInitialPreconsolidat = KInitialPreconsolidat;
  StrCellThicknessFractio = KCellThicknessFractio;
  StrInitialInelasticComp = KInitialInelasticComp;
  StrInitialElasticCompre = KInitialElasticCompre;
  StrInitialDelayHead = KInitialDelayHead;

  StrCSubBoundName = KCSubBoundName;

{ TCSubInterbed }

procedure TCSubInterbed.Assign(Source: TPersistent);
var
  InterbedSource: TCSubInterbed;
begin
  if Source is TCSubInterbed then
  begin
    InterbedSource := TCSubInterbed(Source);
    Name := InterbedSource.Name;
    InterbedType := InterbedSource.InterbedType;
  end
  else
  begin
    inherited;
  end;
end;

function TCSubInterbed.IsSame(AnotherItem: TOrderedItem): boolean;
var
  InterbedSource: TCSubInterbed;
begin
  result := AnotherItem is TCSubInterbed;
  if result then
  begin
    InterbedSource := TCSubInterbed(AnotherItem);
    result := (Name = InterbedSource.Name)
      and (InterbedType = InterbedSource.InterbedType);
  end;
end;

procedure TCSubInterbed.Loaded;
var
  LocalModel: TCustomModel;
//  CSubPackage: TCSubPackageSelection;
  DataArray: TDataArray;
begin
  Exit;
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
//    CSubPackage := LocalModel.ModflowPackages.CSubPackage;

    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FInitialOffset);
    DataArray.Loaded;

    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FThickness);
    DataArray.Loaded;

    if InterbedType = itDelay then
    begin
      DataArray := LocalModel.DataArrayManager.GetDataSetByName(FEquivInterbedNumberName);
      DataArray.Loaded;
    end;

    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FInitialInelasticSpecificStorage);
    DataArray.Loaded;

    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FInitialElasticSpecificStorage);
    DataArray.Loaded;

    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FInitialPorosity);
    DataArray.Loaded;

    if InterbedType = itDelay then
    begin
      DataArray := LocalModel.DataArrayManager.GetDataSetByName(FDelayKvName);
      DataArray.Loaded;
   end;

    if InterbedType = itDelay then
    begin
      DataArray := LocalModel.DataArrayManager.GetDataSetByName(FInitialDelayHeadOffset);
      DataArray.Loaded;
    end;

  end;
end;

procedure TCSubInterbed.RenameInterbed(const NewInterbedName: string);
var
  LocalModel: TCustomModel;
  CSubPackage: TCSubPackageSelection;
//  DataArray: TDataArray;
begin
  if Model <> nil then
  begin
    LocalModel := Model as TCustomModel;
    CSubPackage := LocalModel.ModflowPackages.CSubPackage;

    if CSubPackage.SpecifyInitialPreconsolidationStress then
    begin
      CreateOrRenameDataArray(FInitialOffset, rdtDouble, KInitialPreconsolidat,
        StrInitialPreconsolidat, NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      CreateOrRenameDataArray(FInitialOffset, rdtDouble, KInitialOffset,
        StrInitialOffset, NewInterbedName, Model as TCustomModel);
    end;

    if CSubPackage.InterbedThicknessMethod = itmThickness then
    begin
      CreateOrRenameDataArray(FThickness, rdtDouble, KThickness, StrThickness,
        NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      CreateOrRenameDataArray(FThickness, rdtDouble, KCellThicknessFractio,
        StrCellThicknessFractio, NewInterbedName, Model as TCustomModel);
    end;

    if InterbedType = itDelay then
    begin
      CreateOrRenameDataArray(FEquivInterbedNumberName, rdtDouble,
        KEquivInterbedNumber, StrEquivInterbedNumber, NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      FEquivInterbedNumberName := '';
    end;

    if CSubPackage.CompressionMethod = coRecompression then
    begin
      CreateOrRenameDataArray(FInitialInelasticSpecificStorage, rdtDouble,
        KInitialInelasticComp, StrInitialInelasticComp, NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      CreateOrRenameDataArray(FInitialInelasticSpecificStorage, rdtDouble,
        KInitialInelasticSpec, StrInitialInelasticSpec, NewInterbedName, Model as TCustomModel);
    end;

    if CSubPackage.CompressionMethod = coRecompression then
    begin
      CreateOrRenameDataArray(FInitialElasticSpecificStorage, rdtDouble,
        KInitialElasticCompre, StrInitialElasticCompre, NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      CreateOrRenameDataArray(FInitialElasticSpecificStorage, rdtDouble,
        KInitialElasticSpecif, StrInitialElasticSpecif, NewInterbedName, Model as TCustomModel);
    end;

    CreateOrRenameDataArray(FInitialPorosity, rdtDouble, KInitialPorosity,
      StrInitialPorosity, NewInterbedName, Model as TCustomModel);

    if InterbedType = itDelay then
    begin
      CreateOrRenameDataArray(FDelayKvName, rdtDouble, KDelayKv, StrDelayKv,
        NewInterbedName, Model as TCustomModel);
    end
    else
    begin
      FDelayKvName := '';
    end;

    if InterbedType = itDelay then
    begin
      if CSubPackage.SpecifyInitialDelayHead then
      begin
        CreateOrRenameDataArray(FInitialDelayHeadOffset, rdtDouble,
          KInitialDelayHead, StrInitialDelayHead, NewInterbedName, Model as TCustomModel);
      end
      else
      begin
        CreateOrRenameDataArray(FInitialDelayHeadOffset, rdtDouble,
          KInitialDelayHeadOffs, StrInitialDelayHeadOffs, NewInterbedName, Model as TCustomModel);
      end;
    end
    else
    begin
      FInitialDelayHeadOffset := '';
    end;

    CreateOrRenameDataArray(FCSubBoundName, rdtString, KCSubBoundName,
      StrCSubBoundName, NewInterbedName, Model as TCustomModel);
  end;
end;

procedure TCSubInterbed.CreateOrRenameDataArray(var InterbedName: string;
  DataType: TRbwDataType; Extension, DisplayExtension: string;
  const NewInterbedName: string; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  InterbedDisplayName: string;
  GlobalVariables: TGlobalVariables;
  Basename: string;
  Index: Integer;
  NewFormula: String;
  ClassType: TDataArrayType;
begin
  LocalModel := AModel as TCustomModel;
  Assert(LocalModel <> nil);
  DataArrayManager := LocalModel.DataArrayManager;
  if InterbedName = '' then
  begin
    DataArray := nil;
  end
  else
  begin
    DataArray := DataArrayManager.GetDataSetByName(InterbedName);
  end;
  Basename := ValidName(NewInterbedName) + '_';
  InterbedName := Basename + Extension;
  InterbedDisplayName := Basename + DisplayExtension;
  GlobalVariables := frmGoPhast.PhastModel.GlobalVariables;
  Index := 1;
  while (GlobalVariables.IndexOfVariable(InterbedName) >= 0)
    or (GlobalVariables.IndexOfVariable(InterbedDisplayName) >= 0) do
  begin
    Basename := ValidName(NewInterbedName) + IntToStr(Index) + '_';
    Inc(Index);
    InterbedName := Basename + Extension;
    InterbedDisplayName := Basename + DisplayExtension;
  end;
  if DataArray = nil then
  begin
    DataArray := DataArrayManager.GetDataSetByName(InterbedName);
  end;
  if DataArray = nil then
  begin
    ClassType := nil;
    Case DataType of
      rdtDouble:
        begin
          NewFormula := '0.';
          ClassType := TRealSparseDataSet;
        end;
      rdtString:
        begin
          NewFormula := '""';
          ClassType := TStringSparseDataSet;
        end;
      else Assert(false);
    end;
    DataArray := DataArrayManager.CreateNewDataArray(ClassType, InterbedName,
      NewFormula, InterbedDisplayName, StandardLock + [dcFormula], DataType, eaBlocks,
      dso3D, StrSubsidence + '|' + NewInterbedName);
    LocalModel.UpdateDataArrayDimensions(DataArray);
    DataArray.OnDataSetUsed := LocalModel.CSubInterbedDataArrayUsed;
    (Collection as TCSubInterbeds).AddOwnedDataArray(DataArray);
  end
  else
  begin
    LocalModel.RenameDataArray(DataArray, InterbedName, InterbedDisplayName);
    DataArray.Lock := StandardLock + [dcFormula];
    DataArray.OnDataSetUsed := LocalModel.CSubInterbedDataArrayUsed;
    DataArray.Classification := StrSubsidence + '|' + NewInterbedName;
  end;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      if ChildModel <> nil then
      begin
        CreateOrRenameDataArray(InterbedName, rdtDouble, Extension, DisplayExtension,
          NewInterbedName, ChildModel);
      end;
    end;
  end;
end;

function TCSubInterbed.DataArrayUsed(ADataArray: TDataArray): Boolean;
var
  AName: string;
begin
  Assert(ADataArray <> nil);
  AName := ADataArray.name;
//  result := (AName = FDelayKvName)
//    or (AName = FEquivInterbedNumberName)
//    or (AName = FInitialDelayHeadOffset)
  result := (AName = FInitialElasticSpecificStorage)
    or (AName = FInitialInelasticSpecificStorage)
    or (AName = FInitialOffset)
    or (AName = FInitialPorosity)
    or (AName = FThickness)
    or (AName = FCSubBoundName);
  if not result and (InterbedType = itDelay) then
  begin
    result := (AName = FDelayKvName)
      or (AName = FEquivInterbedNumberName)
      or (AName = FInitialDelayHeadOffset)
  end;
end;

destructor TCSubInterbed.Destroy;
begin
  FDataSetNames.Free;
  inherited;
end;

function TCSubInterbed.GetDataSetNames: TStringList;
begin
  if FDataSetNames = nil then
  begin
    FDataSetNames := TStringList.Create;
  end
  else
  begin
    FDataSetNames.Clear;
  end;
  if InterbedType = itDelay then
  begin
    FDataSetNames.Add(FDelayKvName);
    FDataSetNames.Add(FEquivInterbedNumberName);
    FDataSetNames.Add(FInitialDelayHeadOffset);
  end;
  FDataSetNames.Add(FInitialElasticSpecificStorage);
  FDataSetNames.Add(FInitialInelasticSpecificStorage);
  FDataSetNames.Add(FInitialOffset);
  FDataSetNames.Add(FInitialPorosity);
  FDataSetNames.Add(FThickness);
  FDataSetNames.Add(FCSubBoundName);
  result := FDataSetNames;
end;

procedure TCSubInterbed.SetInterbedType(const Value: TCSubInterbedType);
begin
  if FInterbedType <> Value then
  begin
    FInterbedType := Value;
    RenameInterbed(Name);
    InvalidateModel;
  end;
end;

procedure TCSubInterbed.SetName(const Value: string);
begin
  if FName <> Value then
  begin
    RenameInterbed(Value);
  end;
  SetCaseSensitiveStringProperty(FName, Value);
end;

procedure TCSubInterbed.UpdateInterbedDataSetNames;
begin
  RenameInterbed(Name);
end;

//procedure TCSubInterbed.UpdatePackageDataFormulas;
//var
//  LocalModel: TCustomModel;
//  ObjectIndex: Integer;
//  ScreenObject: TScreenObject;
//begin
//  if Model <> nil then
//  begin
//    LocalModel := Model as TCustomModel;
//    for ObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
//    begin
//      ScreenObject := LocalModel.ScreenObjects[ObjectIndex];
//      if (ScreenObject.ModflowCSub <> nil) and ScreenObject.ModflowCSub.CSubPackageData.Used then
//      begin
//        ScreenObject.ModflowCSub.CSubPackageData.Loaded;
//      end;
//    end;
//  end;
//end;

{ TCSubInterbeds }

function TCSubInterbeds.Add: TCSubInterbed;
begin
  result := inherited Add as TCSubInterbed;
end;

constructor TCSubInterbeds.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TCSubInterbed, Model);
end;

function TCSubInterbeds.DataArrayUsed(ADataArray: TDataArray): Boolean;
var
  Index: Integer;
begin
  result := False;
  for Index := 0 to Count -1 do 
  begin
    result := Items[Index].DataArrayUsed(ADataArray);
    if result then
    begin
      Exit;
    end;
  end;
end;

function TCSubInterbeds.GetInterbedByName(AName: string): TCSubInterbed;
var
  Index: Integer;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    if Items[Index].Name = AName then
    begin
      result := Items[Index];
      Exit;
    end;
  end;
end;

function TCSubInterbeds.GetItem(Index: Integer): TCSubInterbed;
begin
  result := inherited Items[index] as TCSubInterbed;
end;

procedure TCSubInterbeds.Loaded;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].Loaded;
  end;
end;

procedure TCSubInterbeds.SetItem(Index: Integer; const Value: TCSubInterbed);
begin
  inherited Items[index] := Value;
end;

procedure TCSubInterbeds.UpdateInterbedDataSetNames;
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].UpdateInterbedDataSetNames
  end;
end;

end.
