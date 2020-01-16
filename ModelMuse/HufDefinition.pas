unit HufDefinition;

interface

uses
  Classes, RbwParser, GoPhastTypes, OrderedCollectionUnit;

type
  TVK_Method = (vkVK, vkVANI);
  TPrintParam = (pprHK, pprHANI, pprVK, pprSS, pprSY);

  THydrogeologicUnits = class;
  THydrogeologicUnit = class;

  THufUsedParameter = class(TOrderedItem)
  private
    FUseZone: boolean;
    FUseMultiplier: boolean;
    FParameterName: string;
    FZoneName: string;
    FMultiplierName: string;
    FParmeter: TModflowParameter;
    FMultiplierArrayName: string;
    FZoneArrayName: String;
    procedure SetParameterName(const Value: string);
    procedure SetUseMultiplier(const Value: boolean);
    procedure SetUseZone(const Value: boolean);
    procedure FillDataArrayNames(DataArrayNames: TStrings);
    function HufUnit: THydrogeologicUnit;
    function HufUnits: THydrogeologicUnits;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure CreateOrUpdataDataArray(var LayerName: string;
      DataType: TRbwDataType; UseDataArray: boolean; AModel: TBaseModel);
    procedure RenameLayer(const NewHufName: string);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RenameDataArrays(NewRoot: string; AModel: TBaseModel);
    function GetParameter: TModflowParameter;
    function ArrayRoot: string;
    function UniqueName(Candidate: string; Names: TStringList): string;
    procedure SendNotifications;
    procedure NotifyParamChange;
    procedure UnlockDataSets;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Parameter: TModflowParameter read GetParameter write FParmeter;
    procedure GenerateMultiplierArrayName;
    procedure GenerateZoneArrayName;
    property MultiplierArrayName: string read FMultiplierArrayName;
    property ZoneArrayName: string read FZoneArrayName;
    property ZoneDataSetName: string read FZoneName;
    property MultiplierDataSetName: string read FMultiplierName;
    function Description: string;
  published
    property ParameterName: string read FParameterName write SetParameterName;
    property UseZone: boolean read FUseZone write SetUseZone;
    property UseMultiplier: boolean read FUseMultiplier write SetUseMultiplier;
  end;

  THufUsedParameters = class(TOrderedCollection)
  private
    FHufUnit: THydrogeologicUnit;
    FHufUnits: THydrogeologicUnits;
    procedure FillDataArrayNames(DataArrayNames: TStrings);
    procedure RenameParameters(const OldName, NewName: string);
    function GetItem(Index: integer): THufUsedParameter;
    procedure RenameLayer(const NewHufName: string);
    procedure RemoveUsedParameter(const ParameterName: string);
  public
    property Items[Index: integer]: THufUsedParameter read GetItem; default;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel; HufUnit: THydrogeologicUnit);
    function GetUsedParameterByName(const ParameterName: string): THufUsedParameter;
    function IsUsed(const ParameterName: string): boolean;
    procedure Notify(Item: TCollectionItem; Action: TCollectionNotification); override;
  end;

  TPrintItem = class(TPhastCollectionItem)
  private
    FPrint: boolean;
    FPrintParam: TPrintParam;
    procedure SetPrint(const Value: boolean);
    procedure SetPrintParam(const Value: TPrintParam);
    function GetPrintString: string;
  public
    function ShouldPrint: boolean;
  published
    property PrintParam: TPrintParam read FPrintParam write SetPrintParam;
    property Print: boolean read FPrint write SetPrint;
    property PrintString: string read GetPrintString;
  end;

  TPrintCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    function GetPrint(Index: TPrintParam): boolean;
    procedure SetPrint(Index: TPrintParam; const Value: boolean);
    // @name ensures that one and only one @link(TPrintItem) exists for each
    // @link(TPrintParam) and that their order is the same as the
    // order of @link(TPrintParam).
    procedure UpdateItems;
    function GetItemByPrintParam(Index: TPrintParam): TPrintItem;
    function GetItem(Index: TPrintParam): TPrintItem;
  public
    property Print[Index: TPrintParam]: boolean read GetPrint write SetPrint;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    property Items[Index: TPrintParam]: TPrintItem read GetItem; default;
    { TODO -cRefactor : Consider replacing Model with a TNotifyEvent or interface. }
    //
    property Model: TBaseModel read FModel;
  end;

  THydrogeologicUnit = class(TOrderedItem)
  private
    FThickessArrayName: string;
    FTopArrayName: string;
    FVerticalAnisotropy: double;
    FPrintFormat: integer;
    FHufName: string;
    FHorizontalAnisotropy: double;
    FVK_Method: TVK_Method;
    FHufUsedParameters: THufUsedParameters;
    FPrintItems: TPrintCollection;
    procedure SetHorizontalAnisotropy(const Value: double);
    procedure SetHufName(Value: string);
    procedure SetPrintFormat(const Value: integer);
    procedure SetVerticalAnisotropy(const Value: double);
    procedure SetVK_Method(const Value: TVK_Method);
    procedure SetHufUsedParameters(const Value: THufUsedParameters);
    function GetPrint(Index: TPrintParam): boolean;
    procedure SetPrint(Index: TPrintParam; const Value: boolean);
    function ComparePrint(OtherUnit: THydrogeologicUnit): Boolean;
    procedure SetPrintItems(const Value: TPrintCollection);
    procedure RenameLayer(const NewHufName: string);
    procedure FillDataArrayNames(DataArrayNames: TStrings);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure CreateOrRenameDataArray(var LayerName: string;
      Extension, DisplayExtension: string; const NewHufName: string;
      AModel: TBaseModel);
    function HufUnits: THydrogeologicUnits;
    procedure RemoveUsedParameter(const ParameterName: string);
    function UsesParameterType(ParamType: TParameterType): Boolean;
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    property Print[Index: TPrintParam]: boolean read GetPrint write SetPrint;
    procedure RenameParameters(const OldName, NewName: string);
    Destructor Destroy; override;
    // @name is the name of the @link(TDataArray) that
    // defines the top of the hydrogeologic unit.
    property TopDataArrayName: string read FTopArrayName;
    // @name is the name of the @link(TDataArray) that
    // defines the thickness of the hydrogeologic unit.
    property ThickessDataArrayName: string read FThickessArrayName;
    function UsesHaniParam: boolean;
    function UsesVkParam: boolean;
    function UsesParameter(Parameter: TModflowParameter): THufUsedParameter;
  published
    property HufName: string read FHufName write SetHufName;
    property HorizontalAnisotropy: double read FHorizontalAnisotropy
      write SetHorizontalAnisotropy;
    property VK_Method: TVK_Method read FVK_Method write SetVK_Method;
    property VerticalAnisotropy: double read FVerticalAnisotropy
      write SetVerticalAnisotropy;
    property PrintFormat: integer read FPrintFormat write SetPrintFormat default 1;
    property HufUsedParameters: THufUsedParameters read FHufUsedParameters
      write SetHufUsedParameters;
    property PrintItems: TPrintCollection read FPrintItems write SetPrintItems;
  end;

  THydrogeologicUnits = class(TLayerOwnerCollection)
  private
    function GetItems(Index: integer): THydrogeologicUnit;
    procedure SetItems(Index: integer; const Value: THydrogeologicUnit);
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: TBaseModel);
    property Items[Index: integer]: THydrogeologicUnit read GetItems write SetItems; default;
    procedure RenameParameters(const OldName, NewName: string);
    function GetUnitByName(UnitName: string): THydrogeologicUnit;
    procedure FillDataArrayNames(DataArrayNames: TStrings);
    procedure RemoveUsedParameter(const ParameterName: string);
  end;

  THufParameter = class(TModflowParameter)
  private
    Procedure UpdateUsedParameters(const NewName: string);
  protected
    procedure SetParameterName(const Value: string); override;
  public
    Destructor Destroy; override;
  end;

  THufModflowParameters = class(TEnhancedOrderedCollection)
  private
    function GetItem(Index: integer): THufParameter;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    function GetParameterByName(const ParameterName: string): THufParameter;
    property Items[Index: integer]: THufParameter read GetItem; default;
    function CountParameters(ParamTypes: TParameterTypes): integer;
    function GetParamByName(Const AName: string): THufParameter;
  end;

implementation

uses
  SysUtils, PhastModelUnit, DataSetUnit, frmGoPhastUnit, Math, 
  ModflowParameterUnit, GlobalVariablesUnit;

const
  kMultiplier = '_Multiplier';
  kZone = '_Zone';

resourcestring
  StrMultiplier = kMultiplier;
  StrZone = kZone;
  StrHydrogeologicUnit = 'Hydrogeologic unit = %0:s; Parameter = %1:s';

{ THufUsedParameters }

procedure THufUsedParameter.Assign(Source: TPersistent);
var
  OtherParam: THufUsedParameter;
begin
  // if Assign is updated, update IsSame too.
  if Source is THufUsedParameter then
  begin
    OtherParam := THufUsedParameter(Source);
    ParameterName := OtherParam.ParameterName;
    UseZone := OtherParam.UseZone;
    UseMultiplier := OtherParam.UseMultiplier;
  end
  else
  begin
    inherited;
  end;
end;

procedure THufUsedParameter.SendNotifications;
begin
  NotifyParamChange;
end;

procedure THufUsedParameter.RenameDataArrays(NewRoot: string; AModel: TBaseModel);
var
  NewName: string;
  NewDisplayName: string;
  DataArray: TDataArray;
  LocalModel: TCustomModel;
  DataArrayManager: TDataArrayManager;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
begin
  LocalModel := AModel as TCustomModel;
  if LocalModel <> nil then
  begin
    DataArrayManager := LocalModel.DataArrayManager;
    if UseMultiplier then
    begin
      DataArray := DataArrayManager.GetDataSetByName(FMultiplierName);
      if DataArray <> nil then
      begin
        NewName := NewRoot + kMultiplier;
        NewDisplayName := NewRoot + StrMultiplier;
        if FMultiplierName <> NewName then
        begin
          LocalModel.RenameDataArray(DataArray, NewName, NewDisplayName);
        end;
        FMultiplierName := NewName;
      end;
    end;
    if UseZone then
    begin
      DataArray := DataArrayManager.GetDataSetByName(FZoneName);
      if DataArray <> nil then
      begin
        NewName := NewRoot + kZone;
        NewDisplayName := NewRoot + StrZone;
        if FZoneName <> NewName then
        begin
          LocalModel.RenameDataArray(DataArray, NewName, NewDisplayName);
        end;
        FZoneName := NewName;
      end;
    end;
  end;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      RenameDataArrays(NewRoot, ChildModel);
    end;
  end;
end;

procedure THufUsedParameter.CreateOrUpdataDataArray(var LayerName: string;
  DataType: TRbwDataType; UseDataArray: boolean; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  Extension: string;
  Formula: string;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LayerDisplayName: string;
  DisplayExtension: string;
begin
  LocalModel := AModel as TCustomModel;
  if LocalModel <> nil then
  begin
    DataArrayManager := LocalModel.DataArrayManager;
    if UseDataArray then
    begin
      if DataType = rdtBoolean then
      begin
        Extension := kZone;
        DisplayExtension := StrZone;
        Formula := 'False';
      end
      else
      begin
        Assert(DataType = rdtDouble);
        Extension := kMultiplier;
        DisplayExtension := StrMultiplier;
        Formula := '1.';
      end;
      LayerName := HufUnit.FHufName + '_' + ParameterName + Extension;
      LayerDisplayName := HufUnit.FHufName + '_' + ParameterName + DisplayExtension;
      DataArray := DataArrayManager.GetDataSetByName(LayerName);
      if DataArray = nil then
      begin
        DataArray := DataArrayManager.CreateNewDataArray(TDataArray, LayerName,
          Formula, LayerDisplayName, StandardLock, DataType, eaBlocks, dsoTop, StrHUF);
        LocalModel.UpdateDataArrayDimensions(DataArray);
//        if frmGoPhast.Grid = nil then
//        begin
//          DataArray.UpdateDimensions(0,0,0);
//        end
//        else
//        begin
//          DataArray.UpdateDimensions(frmGoPhast.Grid.LayerCount,
//            frmGoPhast.Grid.RowCount, frmGoPhast.Grid.ColumnCount);
//        end;

//        DataArray.OnNameChange := LocalModel.DataArrayNameChange;

        DataArray.OnDataSetUsed := LocalModel.HufDataArrayUsed;
        HufUnits.AddOwnedDataArray(DataArray);
      end
      else
      begin
        DataArray.DisplayName := LayerDisplayName;
        DataArray.UpdateWithName(LayerName);
        DataArray.Lock := StandardLock;
//        DataArray.OnNameChange := LocalModel.DataArrayNameChange;
        DataArray.OnDataSetUsed := LocalModel.HufDataArrayUsed;
      end;
    end
    else
    begin
      DataArray := DataArrayManager.GetDataSetByName(LayerName);
      if DataArray <> nil then
      begin
        DataArray.Lock := [];
      end;
    end;
  end;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      CreateOrUpdataDataArray(LayerName, DataType, UseDataArray, ChildModel);
    end;
  end;
end;

function THufUsedParameter.Description: string;
begin
  result := Format(StrHydrogeologicUnit,
    [HufUnit.HufName, ParameterName]);
end;

destructor THufUsedParameter.Destroy;
begin
  UnlockDataSets;
  inherited;
end;

procedure THufUsedParameter.NotifyParamChange;
var
  AParam: TModflowParameter;
  LocalModel: TCustomModel;
begin
  LocalModel := Model as TCustomModel;
  if LocalModel is TChildModel then
  begin
    LocalModel := TChildModel(LocalModel).ParentModel as TCustomModel;
  end;
  if (LocalModel <> nil) and not (csLoading in LocalModel.ComponentState) then
  begin
    AParam := Parameter;
    AParam.NotifyParamChange(AParam.ParameterType);
  end;
end;

procedure THufUsedParameter.FillDataArrayNames(DataArrayNames: TStrings);
begin
  if UseMultiplier and (FMultiplierName <> '') then
  begin
    DataArrayNames.Add(FMultiplierName);
  end;
  if UseZone and (FZoneName <> '') then
  begin
    DataArrayNames.Add(FZoneName);
  end;
end;

procedure THufUsedParameter.GenerateMultiplierArrayName;
begin
  FMultiplierArrayName := UniqueName(ArrayRoot + 'M', UsedMultiplierArrayNames);
end;

procedure THufUsedParameter.GenerateZoneArrayName;
begin
  FZoneArrayName := UniqueName(ArrayRoot + 'Z', UsedZoneArrayNames);
end;

function THufUsedParameter.GetParameter: TModflowParameter;
begin
  if FParmeter = nil then
  begin
    FParmeter := frmGoPhast.PhastModel.HufParameters.
      GetParameterByName(ParameterName);
    Assert(FParmeter <> nil);
  end;
  Result := FParmeter;
end;

function THufUsedParameter.HufUnit: THydrogeologicUnit;
begin
  result := (Collection as THufUsedParameters).FHufUnit;
end;

function THufUsedParameter.HufUnits: THydrogeologicUnits;
begin
  result := (Collection as THufUsedParameters).FHufUnits;
end;

function THufUsedParameter.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherParam: THufUsedParameter;
begin
  OtherParam := AnotherItem as THufUsedParameter;
  result :=
    (ParameterName = OtherParam.ParameterName)
    and (UseZone = OtherParam.UseZone)
    and (UseMultiplier = OtherParam.UseMultiplier)
end;

function THufUsedParameter.ArrayRoot: string;
var
  ParentUnit: THydrogeologicUnit;
begin
  ParentUnit := HufUnit;
  result := Copy(ParentUnit.HufName, 1, 4)
    + Copy(ParameterName, 1, 4);
end;

procedure THufUsedParameter.RenameLayer(const NewHufName: string);
var
  NewRoot: string;
begin
  NewRoot := NewHufName + '_' + ParameterName;
  RenameDataArrays(NewRoot, Model);
end;

procedure THufUsedParameter.SetParameterName(const Value: string);
var
  NewRoot: string;
//  PhastModel: TPhastModel;
begin
  if FParameterName <> Value then
  begin
    NewRoot := HufUnit.HufName + '_' + Value;
    RenameDataArrays(NewRoot, Model);
    FParameterName := Value;
    InvalidateModel;
    NotifyParamChange;
  end;
end;

procedure THufUsedParameter.SetUseMultiplier(const Value: boolean);
begin
  if FUseMultiplier <> Value then
  begin
    FUseMultiplier := Value;
    CreateOrUpdataDataArray(FMultiplierName, rdtDouble, FUseMultiplier, Model);
    SendNotifications;
    InvalidateModel;
    NotifyParamChange;
  end;
end;

procedure THufUsedParameter.SetUseZone(const Value: boolean);
begin
  if FUseZone <> Value then
  begin
    FUseZone := Value;
    CreateOrUpdataDataArray(FZoneName, rdtBoolean, FUseZone, Model);
    SendNotifications;
    InvalidateModel;
    NotifyParamChange;
  end;
end;

function THufUsedParameter.UniqueName(Candidate: string;
  Names: TStringList): string;
var
  MaxRootLength: Integer;
  Root: string;
  Index: integer;
begin
  Names.CaseSensitive := False;
  if Names.IndexOf(Candidate) < 0 then
  begin
    result := Candidate;
  end
  else
  begin
    result := Candidate + '1';
    Index := 1;
    Root := Candidate;
    while Names.IndexOf(result) >= 0 do
    begin
      Inc(Index);
      MaxRootLength := 9 - Trunc(Log10(Index));
      if Length(Candidate) > MaxRootLength then
      begin
        SetLength(Root, MaxRootLength);
      end;
      result := Root + IntToStr(Index);
      while Length(result) < 10 do
      begin
        result := result + ' ';
      end;
    end;
  end;
end;

procedure THufUsedParameter.UnlockDataSets;
var
  Model: TCustomModel;
  DataArray: TDataArray;
begin
  if (Collection as THufUsedParameters).Model <> nil then
  begin
    Model := (Collection as THufUsedParameters).Model as TCustomModel;
    if not (csLoading in Model.ComponentState) then
    begin
      DataArray := Model.DataArrayManager.GetDataSetByName(FMultiplierName);
      if DataArray <> nil then
      begin
        DataArray.Lock := DataArray.Lock - [dcName];
      end;
      DataArray := Model.DataArrayManager.GetDataSetByName(FZoneName);
      if DataArray <> nil then
      begin
        DataArray.Lock := DataArray.Lock - [dcName];
      end;
    end;
  end;
end;

{ THufUsedParameters }

constructor THufUsedParameters.Create(Model: TBaseModel; HufUnit: THydrogeologicUnit);
begin
  inherited Create(THufUsedParameter, Model);
  FHufUnit := HufUnit;
  FHufUnits := FHufUnit.HufUnits;
end;

procedure THufUsedParameters.FillDataArrayNames(DataArrayNames: TStrings);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].FillDataArrayNames(DataArrayNames);
  end;
end;

function THufUsedParameters.GetItem(Index: integer): THufUsedParameter;
begin
  result := inherited Items[Index] as THufUsedParameter;
end;

function THufUsedParameters.GetUsedParameterByName(
  const ParameterName: string): THufUsedParameter;
var
  Index: Integer;
  Item: THufUsedParameter;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    Item := Items[Index];
    if AnsiCompareText(ParameterName, Item.ParameterName) = 0 then
    begin
      result := Item;
      break;
    end;
  end;
end;

function THufUsedParameters.IsUsed(const ParameterName: string): boolean;
begin
  result := GetUsedParameterByName(ParameterName) <> nil;
end;

procedure THufUsedParameters.Notify(Item: TCollectionItem;
  Action: TCollectionNotification);
begin
  inherited;
  if Action = TCollectionNotification.cnDeleting then
  begin
    (Item as THufUsedParameter).NotifyParamChange;
  end;
end;

procedure THufUsedParameters.RemoveUsedParameter(const ParameterName: string);
var
  UsedParam: THufUsedParameter;
begin
  UsedParam := GetUsedParameterByName(ParameterName);
  if UsedParam <> nil then
  begin
    UsedParam.Free;
  end;
end;

procedure THufUsedParameters.RenameLayer(const NewHufName: string);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].RenameLayer(NewHufName);
  end;
end;

procedure THufUsedParameters.RenameParameters(const OldName, NewName: string);
var
  Param: THufUsedParameter;
begin
  Param := GetUsedParameterByName(OldName);
  if Param <> nil then
  begin
    Param.ParameterName := NewName;
  end;
end;

{ THydrogeologicUnit }

procedure THydrogeologicUnit.Assign(Source: TPersistent);
var
  OtherUnit: THydrogeologicUnit;
  Index: TPrintParam;
begin
  // if Assign is updated, update IsSame too.
  if Source is THydrogeologicUnit then
  begin
    OtherUnit := THydrogeologicUnit(Source);
    HufName := OtherUnit.HufName;
    HorizontalAnisotropy := OtherUnit.HorizontalAnisotropy;
    VK_Method := OtherUnit.VK_Method;
    VerticalAnisotropy := OtherUnit.VerticalAnisotropy;
    PrintFormat := OtherUnit.PrintFormat;
    for Index := Low(TPrintParam) to High(TPrintParam) do
    begin
      Print[Index] := OtherUnit.Print[Index];
    end;
    HufUsedParameters := OtherUnit.HufUsedParameters;
  end
  else
  begin
    inherited;
  end;
end;

constructor THydrogeologicUnit.Create(Collection: TCollection);
begin
  inherited;
  FPrintItems := TPrintCollection.Create(Model);
  FHufUsedParameters := THufUsedParameters.Create(
    Model, self);
  FHorizontalAnisotropy := 1;
  FVerticalAnisotropy := 1;
  FPrintFormat := 1;
end;


destructor THydrogeologicUnit.Destroy;
var
  LocalModel: TCustomModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  AModel: TCustomModel;
  PhastModel: TPhastModel;
  procedure UnlockDataArrays(AModel: TCustomModel);
  var
    DataArray: TDataArray;
    DataArrayManager: TDataArrayManager;
  begin
    DataArrayManager := AModel.DataArrayManager;
    DataArray := DataArrayManager.GetDataSetByName(FTopArrayName);
    if DataArray <> nil then
    begin
      DataArray.Lock := [];
    end;
    DataArray := DataArrayManager.GetDataSetByName(FThickessArrayName);
    if DataArray <> nil then
    begin
      DataArray.Lock := [];
    end;
  end;
begin
  FHufUsedParameters.Free;
  FPrintItems.Free;
  LocalModel := Model as TCustomModel;
  if LocalModel <> nil then
  begin
    AModel := LocalModel;
    UnlockDataArrays(AModel);
//    DataArrayManager := AModel.DataArrayManager;
//    DataArray := DataArrayManager.GetDataSetByName(FTopArrayName);
//    if DataArray <> nil then
//    begin
//      DataArray.Lock := [];
//    end;
//    DataArray := DataArrayManager.GetDataSetByName(FThickessArrayName);
//    if DataArray <> nil then
//    begin
//      DataArray.Lock := [];
//    end;
    if LocalModel is TPhastModel then
    begin
      PhastModel := TPhastModel(LocalModel);
      for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
      begin
        ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
        UnlockDataArrays(ChildModel);
      end;
    end;
  end;
  inherited;
end;

procedure THydrogeologicUnit.CreateOrRenameDataArray(var LayerName: string;
  Extension, DisplayExtension: string; const NewHufName: string; AModel: TBaseModel);
var
  LocalModel: TCustomModel;
  DataArray: TDataArray;
  DataArrayManager: TDataArrayManager;
  PhastModel: TPhastModel;
  ChildIndex: Integer;
  ChildModel: TChildModel;
  LayerDisplayName: string;
  GlobalVariables: TGlobalVariables;
  Basename: string;
begin
  LocalModel := AModel as TCustomModel;
  Assert(LocalModel <> nil);
  DataArrayManager := LocalModel.DataArrayManager;
  if LayerName = '' then
  begin
    DataArray := nil;
  end
  else
  begin
    DataArray := DataArrayManager.GetDataSetByName(LayerName);
  end;
  Basename := NewHufName;
  LayerName := Basename + Extension;
  LayerDisplayName := Basename + DisplayExtension;
  GlobalVariables := frmGoPhast.PhastModel.GlobalVariables;
  while (GlobalVariables.IndexOfVariable(LayerName) >= 0)
    or (GlobalVariables.IndexOfVariable(LayerDisplayName) >= 0) do
  begin
    Basename := Basename + '_';
    LayerName := Basename + Extension;
    LayerDisplayName := Basename + DisplayExtension;
  end;
  if DataArray = nil then
  begin
    DataArray := DataArrayManager.GetDataSetByName(LayerName);
  end;
  if DataArray = nil then
  begin
    DataArray := DataArrayManager.CreateNewDataArray(TDataArray, LayerName,
      '0.', LayerDisplayName, StandardLock, rdtDouble, eaBlocks, dsoTop, StrHUF);
    LocalModel.UpdateDataArrayDimensions(DataArray);
    DataArray.OnDataSetUsed := LocalModel.HufDataArrayUsed;
    HufUnits.AddOwnedDataArray(DataArray);
  end
  else
  begin
    LocalModel.RenameDataArray(DataArray, LayerName, LayerDisplayName);
    DataArray.Lock := StandardLock;
    DataArray.OnDataSetUsed := LocalModel.HufDataArrayUsed;
  end;
  if AModel is TPhastModel then
  begin
    PhastModel := TPhastModel(AModel);
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      ChildModel := PhastModel.ChildModels[ChildIndex].ChildModel;
      CreateOrRenameDataArray(LayerName, Extension, DisplayExtension,
        NewHufName, ChildModel);
    end;
  end;
end;

procedure THydrogeologicUnit.FillDataArrayNames(DataArrayNames: TStrings);
begin
  if FTopArrayName <> '' then
  begin
    DataArrayNames.Add(FTopArrayName);
  end;
  if FThickessArrayName <> '' then
  begin
    DataArrayNames.Add(FThickessArrayName);
  end;
  HufUsedParameters.FillDataArrayNames(DataArrayNames);
end;

function THydrogeologicUnit.ComparePrint(OtherUnit: THydrogeologicUnit): Boolean;
var
  Index: TPrintParam;
begin
  for Index := Low(TPrintParam) to High(TPrintParam) do
  begin
    result := Print[Index] = OtherUnit.Print[Index];
    if not result then
    begin
      break;
    end;
  end;
end;

function THydrogeologicUnit.GetPrint(Index: TPrintParam): boolean;
begin
  result := FPrintItems.Print[Index];
end;

function THydrogeologicUnit.HufUnits: THydrogeologicUnits;
begin
  result := Collection as THydrogeologicUnits;
end;

function THydrogeologicUnit.IsSame(AnotherItem: TOrderedItem): boolean;
var
  OtherUnit: THydrogeologicUnit;
begin
  OtherUnit := AnotherItem as THydrogeologicUnit;
  result :=
    (HufName = OtherUnit.HufName)
    and (HorizontalAnisotropy = OtherUnit.HorizontalAnisotropy)
    and (VK_Method = OtherUnit.VK_Method)
    and (VerticalAnisotropy = OtherUnit.VerticalAnisotropy)
    and (PrintFormat = OtherUnit.PrintFormat)
    and HufUsedParameters.IsSame(OtherUnit.HufUsedParameters);
  if result then
  begin
    result := ComparePrint(OtherUnit);
  end;
end;

procedure THydrogeologicUnit.RenameParameters(const OldName, NewName: string);
begin
  HufUsedParameters.RenameParameters(OldName, NewName);
end;

procedure THydrogeologicUnit.SetHorizontalAnisotropy(const Value: double);
var
  LocalModel: TCustomModel;
begin
  if FHorizontalAnisotropy <> Value then
  begin
    FHorizontalAnisotropy := Value;
    InvalidateModel;
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      LocalModel.HufKyNotifier.UpToDate := False;
      LocalModel.HufKyNotifier.UpToDate := True;
    end;
  end;
end;

procedure THydrogeologicUnit.RemoveUsedParameter(const ParameterName: string);
begin
  HufUsedParameters.RemoveUsedParameter(ParameterName);
end;

procedure THydrogeologicUnit.RenameLayer(const NewHufName: string);
var
  LocalModel: TCustomModel;
  DataArray: TDataArray;
begin
  if Model <> nil then
  begin
    CreateOrRenameDataArray(FTopArrayName, kTop, StrTop, NewHufName, Model);
    CreateOrRenameDataArray(FThickessArrayName, kHufThickness,
      StrHufThickness, NewHufName, Model);
    LocalModel := Model as TCustomModel;
    DataArray := LocalModel.DataArrayManager.GetDataSetByName(FThickessArrayName);
    Assert(DataArray <> nil);
    DataArray.CheckMin := True;
    DataArray.Min := 0;
    HufUsedParameters.RenameLayer(NewHufName);
  end;
end;

procedure THydrogeologicUnit.SetHufName(Value: string);
begin
  Value := ValidName(Value);
  if FHufName <> Value then
  begin
    RenameLayer(Value);
    FHufName := Value;
    InvalidateModel;
  end;
end;

procedure THydrogeologicUnit.SetHufUsedParameters(
  const Value: THufUsedParameters);
begin
  FHufUsedParameters.Assign(Value);
end;

procedure THydrogeologicUnit.SetPrint(Index: TPrintParam; const Value: boolean);
begin
  FPrintItems.Print[Index] := Value;
end;

procedure THydrogeologicUnit.SetPrintFormat(const Value: integer);
begin
  if FPrintFormat <> Value then
  begin
    FPrintFormat := Value;
    InvalidateModel;
  end;
end;

procedure THydrogeologicUnit.SetPrintItems(const Value: TPrintCollection);
begin
  FPrintItems.Assign(Value);
end;

procedure THydrogeologicUnit.SetVerticalAnisotropy(const Value: double);
var
  LocalModel: TCustomModel;
begin
  if FVerticalAnisotropy <> Value then
  begin
    FVerticalAnisotropy := Value;
    InvalidateModel;
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      LocalModel.HufKzNotifier.UpToDate := False;
      LocalModel.HufKzNotifier.UpToDate := True;
    end;
  end;
end;

procedure THydrogeologicUnit.SetVK_Method(const Value: TVK_Method);
var
  LocalModel: TCustomModel;
begin
  if FVK_Method <> Value then
  begin
    FVK_Method := Value;
    InvalidateModel;
    LocalModel := Model as TCustomModel;
    if LocalModel <> nil then
    begin
      LocalModel.HufKzNotifier.UpToDate := False;
      LocalModel.HufKzNotifier.UpToDate := True;
    end;
  end;
end;

function THydrogeologicUnit.UsesHaniParam: boolean;
begin
  Result := UsesParameterType(ptHUF_HANI);
end;

function THydrogeologicUnit.UsesParameter(
  Parameter: TModflowParameter): THufUsedParameter;
var
  UsedParam: THufUsedParameter;
  Index: integer;
begin
  result := nil;
  for Index := 0 to HufUsedParameters.Count - 1 do
  begin
    UsedParam := HufUsedParameters[Index];
    if SameText(Parameter.ParameterName, UsedParam.ParameterName) then
    begin
      result := UsedParam;
      Exit;
    end;
  end;
end;

function THydrogeologicUnit.UsesParameterType(ParamType: TParameterType): boolean;
var
  UsedParam: THufUsedParameter;
  ParamIndex: Integer;
begin
  result := false;
  for ParamIndex := 0 to HufUsedParameters.Count - 1 do
  begin
    UsedParam := HufUsedParameters[ParamIndex];
    if UsedParam.Parameter.ParameterType = ParamType then
    begin
      result := True;
      break;
    end;
  end;
end;

function THydrogeologicUnit.UsesVkParam: boolean;
begin
  Result := UsesParameterType(ptHUF_VK);
end;

{ THydrogeologicUnits }

constructor THydrogeologicUnits.Create(Model: TBaseModel);
begin
  inherited Create(THydrogeologicUnit, Model);
end;

procedure THydrogeologicUnits.FillDataArrayNames(DataArrayNames: TStrings);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].FillDataArrayNames(DataArrayNames)
  end;
end;

function THydrogeologicUnits.GetItems(Index: integer): THydrogeologicUnit;
begin
  result := inherited Items[Index] as THydrogeologicUnit;
end;

function THydrogeologicUnits.GetUnitByName(
  UnitName: string): THydrogeologicUnit;
var
  Index: Integer;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    if SameText(Items[Index].HufName, UnitName) then
    begin
      result := Items[Index];
      Exit;
    end;
  end;
end;

procedure THydrogeologicUnits.RemoveUsedParameter(const ParameterName: string);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].RemoveUsedParameter(ParameterName);
  end;
end;

procedure THydrogeologicUnits.RenameParameters(const OldName, NewName: string);
var
  Index: Integer;
begin
  for Index := 0 to Count - 1 do
  begin
    Items[Index].RenameParameters(OldName, NewName);
  end;
end;

procedure THydrogeologicUnits.SetItems(Index: integer;
  const Value: THydrogeologicUnit);
begin
  inherited Items[Index] := Value;
end;

{ TPrintItem }

function TPrintItem.GetPrintString: string;
begin
  case FPrintParam of
    pprHK: result := ' HK';
    pprHANI: result := ' HANI';
    pprVK: result := ' VK';
    pprSS: result := ' SS';
    pprSY: result := ' SY';
    else Assert(False);
  end;
end;

procedure TPrintItem.SetPrint(const Value: boolean);
begin
  if FPrint <> Value then
  begin
    InvalidateModel;
    FPrint := Value;
  end;
end;

procedure TPrintItem.SetPrintParam(const Value: TPrintParam);
begin
  if FPrintParam <> Value then
  begin
    InvalidateModel;
    FPrintParam := Value;
  end;
end;

function TPrintItem.ShouldPrint: boolean;
var
  Model: TCustomModel;
begin
  result := Print;
  if result and (PrintParam in [pprSS, pprSY]) then
  begin
    Model := (Collection as TPrintCollection).Model as TCustomModel;
    result := Model.ModflowFullStressPeriods.TransientModel;
  end;
  
end;

{ TPrintCollection }

constructor TPrintCollection.Create(Model: TBaseModel);
var
  Index: TPrintParam;
  NewItem: TPrintItem;
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TPrintItem, InvalidateModelEvent);
  for Index := Low(TPrintParam) to High(TPrintParam) do
  begin
    NewItem := Add as TPrintItem;
    NewItem.PrintParam := Index;
    NewItem.Print := False;
  end;
end;

function TPrintCollection.GetItem(Index: TPrintParam): TPrintItem;
begin
  result := GetItemByPrintParam(Index);
end;

function TPrintCollection.GetItemByPrintParam(Index: TPrintParam): TPrintItem;
var
  NeedToUpdateItems: Boolean;
  Position: Integer;
  Item: TPrintItem;
begin
  // This procedure is somewhat complicated so that if TPrintParam is
  // modified, it will still work properly.
  NeedToUpdateItems := False;
  Position := Ord(Index);
  if Position >= Count then
  begin
    NeedToUpdateItems := True;
  end
  else
  begin
    Item := inherited Items[Position] as TPrintItem;
    if Item.PrintParam <> Index then
    begin
      NeedToUpdateItems := True;
    end;
  end;
  if NeedToUpdateItems then
  begin
    UpdateItems;
  end;
  result := inherited Items[Position] as TPrintItem;
  Assert(result.PrintParam = Index);
end;

procedure TPrintCollection.UpdateItems;
var
  ExistingItemTypes : set of TPrintParam;
  ItemIndex: Integer;
  Item: TPrintItem;
  PrintParamIndex: TPrintParam;
  PrintArray: array[Low(TPrintParam)..High(TPrintParam)] of TPrintItem;
begin
  ExistingItemTypes := [];
  for ItemIndex := Count - 1 downto 0 do
  begin
    Item := inherited Items[ItemIndex] as TPrintItem;
    if Item.PrintParam in ExistingItemTypes then
    begin
      Delete(ItemIndex);
    end
    else
    begin
      Include(ExistingItemTypes, Item.PrintParam);
    end;
  end;
  for PrintParamIndex := Low(TPrintParam) to High(TPrintParam) do
  begin
    if not (PrintParamIndex in ExistingItemTypes) then
    begin
      Item := Insert(Ord(PrintParamIndex)) as TPrintItem;
      Item.FPrintParam := PrintParamIndex;
      Item.Print := False;
    end;
  end;
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := inherited Items[ItemIndex] as TPrintItem;
    PrintArray[Item.FPrintParam] := Item;
  end;
  for PrintParamIndex := Low(TPrintParam) to High(TPrintParam) do
  begin
    PrintArray[PrintParamIndex].Index := Ord(PrintParamIndex);
  end;
end;

function TPrintCollection.GetPrint(Index: TPrintParam): boolean;
var
  Item: TPrintItem;
begin
  Item := GetItemByPrintParam(Index);
  result := Item.Print;
end;

procedure TPrintCollection.SetPrint(Index: TPrintParam; const Value: boolean);
var
  Item: TPrintItem;
begin
  Item := GetItemByPrintParam(Index);
  Item.Print := Value;
end;

{ THufModflowParameters }

function THufModflowParameters.CountParameters(
  ParamTypes: TParameterTypes): integer;
var
  Index: Integer;
  Param: THufParameter;
begin
  result := 0;
  for Index := 0 to Count - 1 do
  begin
    Param := Items[Index];
    if Param.ParameterType in ParamTypes then
    begin
      Inc(result);
    end;
  end;
end;

constructor THufModflowParameters.Create(Model: TBaseModel);
begin
  inherited Create(THufParameter, Model);
end;

function THufModflowParameters.GetItem(Index: integer): THufParameter;
begin
  result := inherited Items[Index] as THufParameter;
end;

function THufModflowParameters.GetParamByName(
  const AName: string): THufParameter;
var
  Index: Integer;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    if SameText(AName, Items[Index].ParameterName) then
    begin
      result :=  Items[Index];
      Exit;
    end;
  end;
end;

function THufModflowParameters.GetParameterByName(
  const ParameterName: string): THufParameter;
var
  Index: Integer;
  HufParameter: THufParameter;
begin
  result := nil;
  for Index := 0 to Count - 1 do
  begin
    HufParameter := Items[Index] as THufParameter;
    if SameText(HufParameter.ParameterName, ParameterName) then
    begin
      result := HufParameter;
      Exit;
    end;
  end;
end;

{ THufParameter }

destructor THufParameter.Destroy;
var
  LocalModel: TPhastModel;
begin
  LocalModel := Model as TPhastModel;
  if (LocalModel <> nil) and not (csDestroying in LocalModel.ComponentState)
    and not LocalModel.Clearing then
  begin
    LocalModel.HydrogeologicUnits.RemoveUsedParameter(ParameterName);
  end;
  inherited;
end;

procedure THufParameter.SetParameterName(const Value: string);
var
  NewName: string;
begin
  NewName := CorrectParamName(Value);
  if FParameterName <> NewName then
  begin
    if FParameterName <> '' then
    begin
      UpdateUsedParameters(NewName);
    end;
    FParameterName := NewName;
    InvalidateModel;
  end;
end;

procedure THufParameter.UpdateUsedParameters(const NewName: string);
var
  PhastModel: TPhastModel;
begin
  PhastModel := Model  as TPhastModel;
  if PhastModel <> nil then
  begin
    PhastModel.HydrogeologicUnits.RenameParameters(FParameterName, NewName);
  end;
end;

end.
