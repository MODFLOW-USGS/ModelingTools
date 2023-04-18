unit LockedGlobalVariableChangers;

interface

uses
  System.Classes, GoPhastTypes, GlobalVariablesUnit, RbwParser,
  LockedGlobalVariableChangersInterfaceUnit;

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
    function GetVariable: TGlobalVariable;
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
  Variable: TGlobalVariable;
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
    NewVariables.Assign(FModel.GlobalVariables);

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
        FModel.GlobalVariables := NewVariables;
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
  GetVariable.Locked := FLocked;
end;

function TDefineGlobalIntegerObject.DataType: TRbwDataType;
begin
  result := rdtInteger;
end;

procedure TDefineGlobalIntegerObject.SetValue(Value: Integer);
var
  Variable: TGlobalVariable;
begin
  Variable := GetVariable;
  Variable.IntegerValue := Value;
end;

function TCustomDefinedGlobalObject.GetVariable: TGlobalVariable;
var
  GlobalVariables: TGlobalVariables;
  AVar: TGlobalVariable;
begin
  result := nil;
  if FModel = nil then
  begin
    Exit;
  end;
  result := FModel.GlobalVariables.GetVariableByName(FNewName);
  if result = nil then
  begin
    GlobalVariables := TGlobalVariables.Create(nil);
    try
      GlobalVariables.Assign(FModel.GlobalVariables);
      AVar := (GlobalVariables.Add as TGlobalVariableItem).Variable;
      AVar.Format := DataType;
      AVar.Name := FNewName;
      FModel.GlobalVariables := GlobalVariables;
    finally
      GlobalVariables.Free;
    end;
    result := FModel.GlobalVariables.GetVariableByName(FNewName);
  end;
  result.Locked := FLocked;
  result.Comment := FComment;
end;

{ TDefineGlobalStringObject }

function TDefineGlobalStringObject.DataType: TRbwDataType;
begin
  result := rdtString;
end;

procedure TDefineGlobalStringObject.SetValue(Value: string);
var
  Variable: TGlobalVariable;
begin
  if FModel = nil then
  begin
    Exit;
  end;
  Variable := GetVariable;
  Variable.StringValue := Value;
end;

end.
