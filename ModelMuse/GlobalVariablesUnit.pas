unit GlobalVariablesUnit;

interface

uses Classes, SysUtils, RbwParser, SubscriptionUnit,
  OrderedCollectionUnit, GoPhastTypes, OrderedCollectionInterfaceUnit;

type
  TVariableRecord = record
    StringValue: string;
    case Format: TRbwDataType of
      rdtDouble: (RealValue: double);
      rdtInteger: (IntegerValue: integer);
      rdtBoolean: (BooleanValue: boolean);
  end;

  TGlobalVariables = class;

  TGlobalVariable = class(TObserver)
  private
    FValues: TVariableRecord;
    FCollection: TGlobalVariables;
    FComment: string;
    FLocked: Boolean;
    function GetBooleanValue: boolean;
    function GetIntegerValue: integer;
    function GetRealValue: double;
    function GetStringValue: string;
    procedure SetBooleanValue(const Value: boolean);
    procedure SetFormat(const Value: TRbwDataType);
    procedure SetIntegerValue(const Value: integer);
    procedure SetRealValue(const Value: double);
    procedure SetStringValue(const Value: string);
    function StoreBooleanValue: Boolean;
    function StoreIntegerValue: Boolean;
    function StoreStringValue: Boolean;
    function GetFormat: TRbwDataType;
    function StoreRealValue: Boolean;
    procedure InvalidateModel;
    procedure SetComment(const Value: string);
    procedure NotifyObservers;
    procedure SetLocked(const Value: Boolean);
  protected
    procedure SetName(const NewName: TComponentName); override;
  public
    constructor Create(AnOwner: TComponent); override;
  published
    procedure Assign(Source: TPersistent); override;
    property Format: TRbwDataType read GetFormat write SetFormat stored True;
    property RealValue: double read GetRealValue write SetRealValue
      stored StoreRealValue;
    property IntegerValue: integer read GetIntegerValue write SetIntegerValue
      stored StoreIntegerValue;
    property BooleanValue: boolean read GetBooleanValue write SetBooleanValue
      stored StoreBooleanValue;
    property StringValue: string read GetStringValue write SetStringValue
      stored StoreStringValue;
    property Comment: string read FComment write SetComment;
    property Locked: Boolean read FLocked write SetLocked;
  end;

  TGlobalVariableItem = class(TOrderedItem)
  private
    FVariable: TGlobalVariable;
    procedure SetVariable(const Value: TGlobalVariable);
  protected
    function IsSame(AnotherItem: TOrderedItem): boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    property Variable: TGlobalVariable read FVariable write SetVariable;
  end;

  TGlobalVariables = class(TOrderedCollection)
  private
    FSearchList: TStringList;
    FVariableNames: TStringList;
    function GetVariable(Index: integer): TGlobalVariable;
    procedure SetVariable(Index: integer; const Value: TGlobalVariable);
    function GetGlobalVariableNames: TStringList;
  protected
    procedure FreeSearchList;
  public
    procedure Assign(Source: TPersistent); override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    constructor Create(Model: ICustomModelInterfaceForTOrderedCollection);
    Destructor Destroy; override;
    function IndexOfVariable(Name: string): integer;
    function GetVariableByName(Const Name: string): TGlobalVariable;
    property Variables[Index: integer]: TGlobalVariable read GetVariable
      write SetVariable; default;
    procedure Sort; overload;
    property GlobalVariableNames: TStringList read GetGlobalVariableNames;
  end;

implementation



{ TGlobalVariable }

procedure TGlobalVariable.Assign(Source: TPersistent);
var
  SourceVariable: TGlobalVariable;
begin
  if Source is TGlobalVariable then
  begin
    SourceVariable := TGlobalVariable(Source);
    Name := SourceVariable.Name;
    Format := SourceVariable.Format;
    Comment := SourceVariable.Comment;
    case Format of
      rdtDouble: RealValue := SourceVariable.RealValue;
      rdtInteger: IntegerValue := SourceVariable.IntegerValue;
      rdtBoolean: BooleanValue := SourceVariable.BooleanValue;
      rdtString: StringValue := SourceVariable.StringValue;
    end;
    Locked := SourceVariable.Locked;
  end
  else
  begin
    inherited;
  end;
end;

procedure TGlobalVariable.NotifyObservers;
begin
  if FCollection.Model <> nil then
  begin
    UpToDate := False;
    UpToDate := True;
  end;
end;

constructor TGlobalVariable.Create(AnOwner: TComponent);
begin
  inherited;
  UpToDate := True;
end;

function TGlobalVariable.GetBooleanValue: boolean;
begin
  Assert(Format = rdtBoolean);
  result := FValues.BooleanValue;
end;

function TGlobalVariable.GetFormat: TRbwDataType;
begin
  result := FValues.Format;
end;

function TGlobalVariable.GetIntegerValue: integer;
begin
  Assert(Format = rdtInteger);
  result := FValues.IntegerValue;
end;

function TGlobalVariable.GetRealValue: double;
begin
  Assert(Format = rdtDouble);
  result := FValues.RealValue;
end;

function TGlobalVariable.GetStringValue: string;
begin
  Assert(Format = rdtString);
  result := FValues.StringValue;
end;

procedure TGlobalVariable.InvalidateModel;
begin
  FCollection.InvalidateModel;
end;

procedure TGlobalVariable.SetBooleanValue(const Value: boolean);
begin
  Format := rdtBoolean;
  if FValues.BooleanValue <> Value then
  begin
    InvalidateModel;
    FValues.BooleanValue := Value;
    NotifyObservers;
  end;
end;

procedure TGlobalVariable.SetComment(const Value: string);
begin
  if FComment <> Value then
  begin
    InvalidateModel;
    FComment := Value;
  end;
end;

procedure TGlobalVariable.SetFormat(const Value: TRbwDataType);
var
  RealValue: Extended;
  IntValue: integer;
begin
  if FValues.Format <> Value then
  begin
    InvalidateModel;
    case FValues.Format of
      rdtDouble:
        begin
          case Value of
            rdtDouble:
              begin
                Assert(False);
              end;
            rdtInteger:
              begin
                FValues.IntegerValue := Round(FValues.RealValue);
              end;
            rdtBoolean:
              begin
                FValues.BooleanValue := FValues.RealValue <> 0;
              end;
            rdtString:
              begin
                FValues.StringValue := FortranFloatToStr(FValues.RealValue);
              end;
            else Assert(False);
          end;
        end;
      rdtInteger:
        begin
          case Value of
            rdtDouble:
              begin
                FValues.RealValue := FValues.IntegerValue;
              end;
            rdtInteger:
              begin
                Assert(False);
              end;
            rdtBoolean:
              begin
                FValues.BooleanValue := FValues.IntegerValue <> 0;
              end;
            rdtString:
              begin
                FValues.StringValue := IntToStr(FValues.IntegerValue);
              end;
            else Assert(False);
          end;
        end;
      rdtBoolean:
        begin
          case Value of
            rdtDouble:
              begin
                FValues.RealValue := Ord(FValues.BooleanValue);
              end;
            rdtInteger:
              begin
                FValues.IntegerValue := Ord(FValues.BooleanValue);
              end;
            rdtBoolean:
              begin
                Assert(False);
              end;
            rdtString:
              begin
                if FValues.BooleanValue then
                begin
                  FValues.StringValue := 'True';
                end
                else
                begin
                  FValues.StringValue := 'False';
                end;
              end;
            else Assert(False);
          end;
        end;
      rdtString:
        begin
          case Value of
            rdtDouble:
              begin
                if TryStrToFloat(FValues.StringValue, RealValue) then
                begin
                  FValues.RealValue := RealValue;
                end
                else
                begin
                  FValues.RealValue := 0;
                end;
              end;
            rdtInteger:
              begin
                if TryStrToInt(FValues.StringValue, IntValue) then
                begin
                  FValues.IntegerValue := IntValue;
                end
                else
                begin
                  FValues.IntegerValue := 0;
                end;
              end;
            rdtBoolean:
              begin
                FValues.BooleanValue := UpperCase(FValues.StringValue) = 'TRUE';
              end;
            rdtString:
              begin
                Assert(False);
              end;
            else Assert(False);
          end;
        end;
      else Assert(False);
    end;
    FValues.Format := Value;
    NotifyObservers;
  end;
end;

procedure TGlobalVariable.SetIntegerValue(const Value: integer);
begin
  Format := rdtInteger;
  if FValues.IntegerValue <> Value then
  begin
    InvalidateModel;
    FValues.IntegerValue := Value;
    NotifyObservers;
  end;
end;

procedure TGlobalVariable.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
end;

procedure TGlobalVariable.SetName(const NewName: TComponentName);
begin
  if Name <> NewName then
  begin
    InvalidateModel;
    inherited SetName(NewName);
    FCollection.FreeSearchList;
    NotifyObservers;
  end;
end;

procedure TGlobalVariable.SetRealValue(const Value: double);
begin
  Format := rdtDouble;
  if FValues.RealValue <> Value then
  begin
    InvalidateModel;
    FValues.RealValue := Value;
    NotifyObservers;
  end;
end;

procedure TGlobalVariable.SetStringValue(const Value: string);
begin
  Format := rdtString;
  if FValues.StringValue <> Value then
  begin
    InvalidateModel;
    FValues.StringValue := Value;
    NotifyObservers;
  end;
end;

function TGlobalVariable.StoreBooleanValue: Boolean;
begin
  result := Format = rdtBoolean;
end;

function TGlobalVariable.StoreIntegerValue: Boolean;
begin
  result := Format = rdtInteger;
end;

function TGlobalVariable.StoreRealValue: Boolean;
begin
  result := Format = rdtDouble;
end;

function TGlobalVariable.StoreStringValue: Boolean;
begin
  result := Format = rdtString;
end;

{ TGlobalVariables }

procedure TGlobalVariables.Assign(Source: TPersistent);
begin
  inherited;
  Sort;
end;

constructor TGlobalVariables.Create(Model: ICustomModelInterfaceForTOrderedCollection);
begin
  inherited Create(TGlobalVariableItem, Model);
  FSearchList := nil;
end;

destructor TGlobalVariables.Destroy;
begin
  FVariableNames.Free;
  FreeSearchList;
  inherited;
end;

procedure TGlobalVariables.FreeSearchList;
begin
  FreeAndNil(FSearchList);
end;

function TGlobalVariables.GetGlobalVariableNames: TStringList;
var
  index: Integer;
begin
  if FVariableNames = nil then
  begin
    FVariableNames := TStringList.Create
  end
  else
  begin
    FVariableNames.Clear;
  end;
  for index := 0 to Count - 1 do
  begin
    FVariableNames.Add(Variables[index].Name);
  end;
  result := FVariableNames;
end;

function TGlobalVariables.GetVariable(Index: integer): TGlobalVariable;
begin
  result := (Items[Index] as TGlobalVariableItem).Variable;
end;

function TGlobalVariables.GetVariableByName(
  const Name: string): TGlobalVariable;
var
  Index: integer;
begin
  result := nil;
  Index := IndexOfVariable(Name);
  if Index >= 0 then
  begin
    result := Variables[Index];
  end;
end;

procedure TGlobalVariables.Sort;
var
  List: TStringList;
  ItemIndex: Integer;
  Item: TGlobalVariableItem;
begin
  List := TStringList.Create;
  try
    List.CaseSensitive := False;
    for ItemIndex := 0 to Count - 1 do
    begin
      Item := Items[ItemIndex] as TGlobalVariableItem;
      List.AddObject(Item.Variable.Name, Item);
    end;
    List.Sort;
    for ItemIndex := 0 to List.Count - 1 do
    begin
      Item := List.Objects[ItemIndex] as TGlobalVariableItem;
      Item.Index := ItemIndex;
    end;
  finally
    List.Free;
  end;
end;

function TGlobalVariables.IndexOfVariable(Name: string): integer;
var
  Index: Integer;
  Item: TGlobalVariableItem;
begin
  Name := UpperCase(Name);
  if Count > 10 then
  begin
    if FSearchList = nil then
    begin
      FSearchList := TStringList.Create;
      for Index := 0 to Count - 1 do
      begin
        FSearchList.AddObject(UpperCase(Variables[Index].Name), Items[Index]);
      end;
      FSearchList.Sorted := True;
    end;
    result := FSearchList.IndexOf(Name);
    if result >= 0 then
    begin
      Item := FSearchList.Objects[result] as TGlobalVariableItem;
      result := Item.Index;
    end;
  end
  else
  begin
    result := -1;
    for Index := 0 to Count - 1 do
    begin
      if UpperCase(Variables[Index].Name) = Name then
      begin
        result := Index;
        break;
      end;
    end;
  end;
end;

procedure TGlobalVariables.SetVariable(Index: integer;
  const Value: TGlobalVariable);
begin
  (Items[Index] as TGlobalVariableItem).Variable := Value;
end;


{ TGlobalVariableItem }

procedure TGlobalVariableItem.Assign(Source: TPersistent);
begin
  // if Assign is updated, update IsSame too.
  if Source is TGlobalVariableItem then
  begin
    Variable := TGlobalVariableItem(Source).Variable;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGlobalVariableItem.Create(Collection: TCollection);
begin
  inherited;
  FVariable:= TGlobalVariable.Create(nil);
  FVariable.FCollection := Collection as TGlobalVariables;
  FVariable.SetSubComponent(True);
  AlwaysAssignForeignId := True;
  (Collection as TGlobalVariables).FreeSearchList;
end;

destructor TGlobalVariableItem.Destroy;
begin
  FVariable.Free;
  if Collection <> nil then
  begin
    (Collection as TGlobalVariables).FreeSearchList;
  end;
  inherited;
end;

function TGlobalVariableItem.IsSame(AnotherItem: TOrderedItem): boolean;
var
  AnItem: TGlobalVariableItem;
begin
  result := AnotherItem is TGlobalVariableItem;
  if result then
  begin
    AnItem := TGlobalVariableItem(AnotherItem);
    result := (Variable.Format = AnItem.Variable.Format)
      and (Variable.Comment = AnItem.Variable.Comment)
      and (Variable.Name = AnItem.Variable.Name);
    if result then
    begin
      case Variable.Format of
        rdtDouble:
          begin
            result := Variable.RealValue = AnItem.Variable.RealValue;
          end;
        rdtInteger:
          begin
            result := Variable.IntegerValue = AnItem.Variable.IntegerValue;
          end;
        rdtBoolean:
          begin
            result := Variable.BooleanValue = AnItem.Variable.BooleanValue;
          end;
        rdtString:
          begin
            result := Variable.StringValue = AnItem.Variable.StringValue;
          end;
        else Assert(False);
      end;
    end;
  end;
end;

procedure TGlobalVariableItem.SetVariable(const Value: TGlobalVariable);
begin
  FVariable.Assign(Value);
end;

end.
