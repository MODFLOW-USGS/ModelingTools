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
  if FModel = nil then
  begin
    Exit
  end;

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
  CompilerList: TList;
  CompilerIndex: Integer;
  Compiler: TRbwParser;
  VariableIndex: Integer;
  CompilerVariable: TCustomVariable;
  RealVariable: TRealVariable;
  IntegerVariable: TIntegerVariable;
  BooleanVariable: TBooleanVariable;
  StringVariable: TStringVariable;
  ValueChanged: Boolean;
begin
  result := nil;
  if FModel = nil then
  begin
    Exit;
  end;
  LocalModel := (FModel as TPhastModel);
  result := LocalModel.GlobalVariables.GetVariableByName(FNewName);
  if result = nil then
  begin
    result := (LocalModel.GlobalVariables.Add as TGlobalVariableItem)
      .Variable;
    result.Format := DataType;
    result.Name := FNewName;
    CompilerList := TList.Create;
    try
      LocalModel.FillCompilerList(CompilerList);
      for CompilerIndex := 0 to CompilerList.Count - 1 do
      begin
        Compiler := CompilerList[CompilerIndex];
        VariableIndex := Compiler.IndexOfVariable(result.Name);
        if VariableIndex < 0 then
        begin
          case result.Format of
            rdtDouble:
              begin
                Compiler.CreateVariable(result.Name, StrGlobalVariables,
                  result.RealValue, result.Name);
              end;
            rdtInteger:
              begin
                Compiler.CreateVariable(result.Name, StrGlobalVariables,
                  result.IntegerValue, result.Name);
              end;
            rdtBoolean:
              begin
                Compiler.CreateVariable(result.Name, StrGlobalVariables,
                  result.BooleanValue, result.Name);
              end;
            rdtString:
              begin
                Compiler.CreateVariable(result.Name, StrGlobalVariables,
                  result.StringValue, result.Name);
              end;
            else Assert(False);
          end;
        end
        else
        begin
          CompilerVariable := Compiler.Variables[VariableIndex]
            as TCustomVariable;
          case result.Format of
            rdtDouble:
              begin
                RealVariable := CompilerVariable as TRealVariable;
                if (CompilerIndex = 0)
                  and (RealVariable.Value <> result.RealValue) then
                begin
                  ValueChanged := True;
                end;
                RealVariable.Value := result.RealValue;
              end;
            rdtInteger:
              begin
                IntegerVariable := CompilerVariable as TIntegerVariable;
                if (CompilerIndex = 0)
                  and (IntegerVariable.Value <> result.IntegerValue) then
                begin
                  ValueChanged := True;
                end;
                IntegerVariable.Value := result.IntegerValue;
              end;
            rdtBoolean:
              begin
                BooleanVariable := CompilerVariable as TBooleanVariable;
                if (CompilerIndex = 0)
                  and (BooleanVariable.Value <> result.BooleanValue) then
                begin
                  ValueChanged := True;
                end;
                BooleanVariable.Value := result.BooleanValue;
              end;
            rdtString:
              begin
                StringVariable := CompilerVariable as TStringVariable;
                if (CompilerIndex = 0)
                  and (StringVariable.Value <> result.StringValue) then
                begin
                  ValueChanged := True;
                end;
                StringVariable.Value := result.StringValue;
              end;
            else Assert(False);
          end;
        end;
      end
    finally
      CompilerList.Free;
    end;
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
