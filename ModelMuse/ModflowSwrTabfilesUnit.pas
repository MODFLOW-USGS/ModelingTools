unit ModflowSwrTabfilesUnit;

interface

uses
  GoPhastTypes, Classes, Contnrs, SysUtils;

type
  TTabType = (ttRain, ttEvap, ttLatFlow, ttStage, ttStructure, ttTime);
  TInterpolationMethod = (imNone, imAverage, imInterpolate);
  TReachSelectionMethod = (rsmAll, rsmObjects, rsmValue, rsmReaches);
  TTabFormat = (tfText, tfBinary);

  TTabFileItem = class(TPhastCollectionItem)
  private
    FReachSelectionMethod: TReachSelectionMethod;
    FTabType: TTabType;
    FValue: Integer;
    FObjectNames: string;
    FObjects: TObjectList;
    FFullTabFileName: string;
    FInterpolationMethod: TInterpolationMethod;
    FReaches: TIntegerCollection;
    FTabFormat: TTabFormat;
    function GetObjectNames: string;
    procedure SetInterpolationMethod(const Value: TInterpolationMethod);
    procedure SetObjectNames(const Value: string);
    procedure SetReachSelectionMethod(const Value: TReachSelectionMethod);
    procedure SetTabType(const Value: TTabType);
    procedure SetValue(const Value: Integer);
    procedure UpdateObjects;
    procedure SetTabFileName(const Value: string);
    function GetParentModel: TBaseModel;
    procedure SetReaches(const Value: TIntegerCollection);
    procedure SetTabFormat(const Value: TTabFormat);
    function GetTabFileName: string;
    function GetFullTabFileName: string;
    procedure SetFullTabFileName(const Value: string);
  protected
    property ParentModel: TBaseModel read GetParentModel;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Loaded;
    property FullTabFileName: string read GetFullTabFileName write SetFullTabFileName;
  published
    // CTABTYPE
    property TabType: TTabType read FTabType write SetTabType;
    // ITABUNIT
    property TabFileName: string read GetTabFileName write SetTabFileName;
    // ITABUNIT
    property TabFormat: TTabFormat read FTabFormat write SetTabFormat;
    // CINTP
    property InterpolationMethod: TInterpolationMethod read FInterpolationMethod
      write SetInterpolationMethod;
    // CTABRCH
    property ReachSelectionMethod: TReachSelectionMethod
      read FReachSelectionMethod write SetReachSelectionMethod;
    // ITABRCH
    property ObjectNames: string read GetObjectNames write SetObjectNames;
    // ITABRCH
    property Value: Integer read FValue write SetValue;
    // ITABRCH
    property Reaches: TIntegerCollection read FReaches write SetReaches;
  end;

  TTabFileCollection = class(TPhastCollection)
  private
    FModel: TBaseModel;
    function GetItems(Index: Integer): TTabFileItem;
    procedure SetItems(Index: Integer; const Value: TTabFileItem);
  public
    procedure Loaded;
    constructor Create(AModel: TBaseModel);
    property Items[Index: Integer]: TTabFileItem read GetItems write SetItems; default;
    function IndexOfFileName(const FileName: string): Integer;
    function Add: TTabFileItem;
  end;

implementation

uses
  ScreenObjectUnit, PhastModelUnit, AnsiStrings;

{ TTabFileItem }

procedure TTabFileItem.Assign(Source: TPersistent);
var
  SourceItem: TTabFileItem;
begin
  if Source is TTabFileItem then
  begin
    SourceItem := TTabFileItem(Source);
    TabType := SourceItem.TabType;
    FullTabFileName := SourceItem.FullTabFileName;
    TabFormat := SourceItem.TabFormat;
    InterpolationMethod := SourceItem.InterpolationMethod;
    ReachSelectionMethod := SourceItem.ReachSelectionMethod;
    ObjectNames := SourceItem.ObjectNames;
    Value := SourceItem.Value;
    Reaches := SourceItem.Reaches;
  end
  else
  begin
    inherited;
  end;


end;

constructor TTabFileItem.Create(Collection: TCollection);
begin
  inherited;
  FObjects := TObjectList.Create(False);
  FReaches := TIntegerCollection.Create(OnInvalidateModel);
end;

destructor TTabFileItem.Destroy;
begin
  FReaches.Free;
  FObjects.Free;
  inherited;
end;

function TTabFileItem.GetTabFileName: string;
var
  LocalModel: TCustomModel;
begin
  result := FullTabFileName;
  if ParentModel <> nil then
  begin
    LocalModel := ParentModel as TCustomModel;
    Result := ExtractRelativePath(LocalModel.ModelFileName, result);
  end;
end;

function TTabFileItem.GetFullTabFileName: string;
begin
  result := FFullTabFileName;
end;

function TTabFileItem.GetObjectNames: string;
var
  ObjectList: TStringList;
  Index: integer;
  AnObject: TObject;
begin
  if FObjects.Count > 0 then
  begin
    ObjectList := TStringList.Create;
    try
      for Index := 0 to FObjects.Count - 1 do
      begin
        AnObject := FObjects[Index];
        ObjectList.AddObject(TScreenObject(AnObject).Name, AnObject)
      end;
      result := ObjectList.CommaText;
      result := StringReplace(result, ',', ', ', [rfReplaceAll])
    finally
      ObjectList.Free;
    end;
  end
  else
  begin
    result := FObjectNames;
  end;
end;

function TTabFileItem.GetParentModel: TBaseModel;
begin
  result := (Collection as TTabFileCollection).FModel;
end;

procedure TTabFileItem.Loaded;
begin
  inherited;
  UpdateObjects;
end;

procedure TTabFileItem.SetTabFileName(const Value: string);
begin
  FullTabFileName := ExpandFileName(Value);
end;

procedure TTabFileItem.SetFullTabFileName(const Value: string);
begin
  SetStringProperty(FFullTabFileName, Value);
end;

procedure TTabFileItem.SetInterpolationMethod(
  const Value: TInterpolationMethod);
begin
  if FInterpolationMethod <> Value then
  begin
    FInterpolationMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTabFileItem.SetObjectNames(const Value: string);
begin
  SetStringProperty(FObjectNames, Value);
  UpdateObjects
end;

procedure TTabFileItem.SetReaches(const Value: TIntegerCollection);
begin
  FReaches.Assign(Value);
end;

procedure TTabFileItem.SetReachSelectionMethod(
  const Value: TReachSelectionMethod);
begin
  if FReachSelectionMethod <> Value then
  begin
    FReachSelectionMethod := Value;
    InvalidateModel;
  end;
end;

procedure TTabFileItem.SetTabFormat(const Value: TTabFormat);
begin
  if FTabFormat <> Value then
  begin
    FTabFormat := Value;
    InvalidateModel;
  end;
end;

procedure TTabFileItem.SetTabType(const Value: TTabType);
begin
  if FTabType <> Value then
  begin
    FTabType := Value;
    InvalidateModel;
  end;
end;

procedure TTabFileItem.SetValue(const Value: Integer);
begin
  SetIntegerProperty(FValue, Value);
end;

procedure TTabFileItem.UpdateObjects;
var
  ObjectList: TStringList;
  LocalModel: TPhastModel;
  AnObject: TScreenObject;
  LocalNames: string;
  Index: integer;
begin
  LocalModel := ParentModel as TPhastModel;
  if (FObjectNames <> '') and (LocalModel <> nil) then
  begin
    if csReading in LocalModel.ComponentState then
    begin
      Exit;
    end;
    ObjectList := TStringList.Create;
    try
      ObjectList.CommaText := FObjectNames;
      if LocalModel <> nil then
      begin
        FObjects.Clear;
        for Index := 0 to ObjectList.Count - 1 do
        begin
          AnObject := LocalModel.GetScreenObjectByName(ObjectList[Index]);
          if AnObject <> nil then
          begin
            FObjects.Add(AnObject);
          end;
        end;
      end;
    finally
      ObjectList.Free;
    end;
    if FObjects.Count > 0 then
    begin
      LocalNames := GetObjectNames;
      SetStringProperty(FObjectNames, LocalNames);
    end
    else
    begin
      SetStringProperty(FObjectNames, '');
    end;
  end
  else
  begin
    FObjects.Clear
  end;
end;

{ TTabFileCollection }

function TTabFileCollection.Add: TTabFileItem;
begin
  result := inherited Add as TTabFileItem;
end;

constructor TTabFileCollection.Create(AModel: TBaseModel);
var
  OnInvalidateEvent: TNotifyEvent;
begin
  FModel := AModel;
  if Assigned(FModel) then
  begin
    OnInvalidateEvent := FModel.Invalidate;
  end
  else
  begin
    OnInvalidateEvent := nil;
  end;
  inherited Create(TTabFileItem, OnInvalidateEvent);
end;

function TTabFileCollection.GetItems(Index: Integer): TTabFileItem;
begin
  result := inherited Items[Index] as TTabFileItem;
end;

function TTabFileCollection.IndexOfFileName(const FileName: string): Integer;
var
  index: Integer;
begin
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if AnsiCompareText(Items[index].TabFileName, FileName) = 0 then
    begin
      Result := index;
      Break;
    end;
  end;
end;

procedure TTabFileCollection.Loaded;
var
  index: Integer;
begin
  for index := 0 to Count - 1 do
  begin
    Items[index].Loaded;
  end;
end;

procedure TTabFileCollection.SetItems(Index: Integer;
  const Value: TTabFileItem);
begin
  inherited Items[Index] := Value;
end;

end.
