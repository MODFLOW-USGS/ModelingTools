unit LockedGlobalVariableChangers;

interface

uses
  System.Classes, GoPhastTypes, GlobalVariablesUnit, RbwParser,
  LockedGlobalVariableChangersInterfaceUnit, GlobalVariablesInterfaceUnit;

type
  TCustomDefinedGlobalObject = class(TObject)
  private
    FLocked: Boolean;
    procedure SetLocked(const Value: Boolean);
  protected
    FModel: IModelForTCustomDefinedGlobalObject;
    FOldName: string;
    FNewName: string;
    FComment: string;
    function GetVariable: IGlobalVariable;
    function DataType: TRbwDataType; virtual; abstract;
  public
    constructor Create(Model: IModelForTCustomDefinedGlobalObject; const OldName, NewName,
      Comment: string);
    procedure Rename;
    property Locked: Boolean read FLocked write SetLocked;
  end;

  TDefineGlobalIntegerObject = class(TCustomDefinedGlobalObject)
  protected
    function DataType: TRbwDataType; override;
  public
    procedure SetValue(Value: Integer);
  end;

  TDefineGlobalStringObject = class(TCustomDefinedGlobalObject)
  protected
    function DataType: TRbwDataType; override;
  public
    procedure SetValue(Value: string);
  end;


implementation

{ TDefineGlobalIntegerObject }

constructor TCustomDefinedGlobalObject.Create(Model: IModelForTCustomDefinedGlobalObject; const OldName,
  NewName, Comment: string);
begin
  FModel := Model;
  FOldName := OldName;
  FNewName := NewName;
  FComment := Comment;
  FLocked := True;
end;

procedure TCustomDefinedGlobalObject.Rename;
var
//  LocalModel: TPhastModel;
  NewVariables: TGlobalVariables;
  Variable: IGlobalVariable;
  OldNames: TStringList;
  NewNames: TStringList;
begin
  if FModel = nil then
  begin
    Exit
  end;

//  FModel := (FModel as TPhastModel);

  NewVariables := TGlobalVariables.Create(nil);
  try
    NewVariables.Assign(FModel.GlobalVariablesI as TGlobalVariables);

    Variable := NewVariables.GetVariableByName(FOldName);
    if Variable <> nil then
    begin

      OldNames := TStringList.Create;
      NewNames := TStringList.Create;
      try
        OldNames.Add(FOldName);
        NewNames.Add(FNewName);
        FModel.UpdateFormulas(OldNames, NewNames);
        Variable.Name := FNewName;
        FModel.GlobalVariablesI := NewVariables;
        FModel.RestoreSubscriptions;
      finally
        NewNames.Free;
        OldNames.Free;
      end;
    end;
  finally
    NewVariables.Free;
  end;
end;

procedure TCustomDefinedGlobalObject.SetLocked(const Value: Boolean);
begin
  FLocked := Value;
  (GetVariable as TGlobalVariable).Locked := FLocked;
end;

function TDefineGlobalIntegerObject.DataType: TRbwDataType;
begin
  result := rdtInteger;
end;

procedure TDefineGlobalIntegerObject.SetValue(Value: Integer);
var
  Variable: IGlobalVariable;
begin
  Variable := GetVariable;
  Variable.IntegerValue := Value;
end;

function TCustomDefinedGlobalObject.GetVariable: IGlobalVariable;
var
  GlobalVariables: TGlobalVariables;
  AVar: IGlobalVariable;
begin
  result := nil;
  if FModel = nil then
  begin
    Exit;
  end;
  result := FModel.GlobalVariablesI.GetVariableByName(FNewName);
  if result = nil then
  begin
    GlobalVariables := TGlobalVariables.Create(nil);
    try
      GlobalVariables.Assign(FModel.GlobalVariablesI as TGlobalVariables);
      AVar := (GlobalVariables.Add as TGlobalVariableItem).Variable;
      AVar.Format := DataType;
      AVar.Name := FNewName;
      FModel.GlobalVariablesI := GlobalVariables;
    finally
      GlobalVariables.Free;
    end;
    result := FModel.GlobalVariablesI.GetVariableByName(FNewName);
  end;
  (result as TGlobalVariable).Locked := FLocked;
  result.Comment := FComment;
end;

{ TDefineGlobalStringObject }

function TDefineGlobalStringObject.DataType: TRbwDataType;
begin
  result := rdtString;
end;

procedure TDefineGlobalStringObject.SetValue(Value: string);
var
  Variable: IGlobalVariable;
begin
  if FModel = nil then
  begin
    Exit;
  end;
  Variable := GetVariable;
  Variable.StringValue := Value;
end;

end.
