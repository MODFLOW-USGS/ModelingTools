unit LockedGlobalVariableChangers;

interface

uses
  System.Classes, GoPhastTypes, GlobalVariablesUnit, RbwParser;

type
  TCustomDefinedGlobalObject = class(TObject)
  private
    FLocked: Boolean;
    procedure SetLocked(const Value: Boolean);
  protected
    FModel: TBaseModel;
    FOldName: string;
    FNewName: string;
    FComment: string;
    function GetVariable: TGlobalVariable;
    function DataType: TRbwDataType; virtual; abstract;
  public
    constructor Create(Model: TBaseModel; const OldName, NewName,
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

uses
  PhastModelUnit;

{ TDefineGlobalIntegerObject }

constructor TCustomDefinedGlobalObject.Create(Model: TBaseModel; const OldName,
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
  LocalModel: TPhastModel;
  NewVariables: TGlobalVariables;
  Variable: TGlobalVariable;
  OldNames: TStringList;
  NewNames: TStringList;
begin
  LocalModel := (FModel as TPhastModel);

  NewVariables := TGlobalVariables.Create(nil);
  try
    NewVariables.Assign(LocalModel.GlobalVariables);

    Variable := NewVariables.GetVariableByName(FOldName);
    if Variable <> nil then
    begin

      OldNames := TStringList.Create;
      NewNames := TStringList.Create;
      try
        OldNames.Add(FOldName);
        NewNames.Add(FNewName);
        LocalModel.UpdateFormulas(OldNames, NewNames);
        Variable.Name := FNewName;
        LocalModel.GlobalVariables := NewVariables;
        LocalModel.FormulaManager.RestoreSubscriptions;
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
  LocalModel: TPhastModel;
begin
  LocalModel := (FModel as TPhastModel);
  result := LocalModel.GlobalVariables.GetVariableByName(FNewName);
  if result = nil then
  begin
    result := (LocalModel.GlobalVariables.Add as TGlobalVariableItem)
      .Variable;
    result.Format := DataType;
    result.Name := FNewName;
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
  Variable := GetVariable;
  Variable.StringValue := Value;
end;

end.
