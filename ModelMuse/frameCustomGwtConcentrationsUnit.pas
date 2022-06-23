unit frameCustomGwtConcentrationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  GoPhastTypes, Vcl.ExtCtrls, Vcl.StdCtrls, SsButtonEd;

type
  TframeCustomGwtConcentrations = class(TFrame)
    rdgConcentrations: TRbwDataGrid4;
    pnl1: TPanel;
    btnedInitialConcentration: TssButtonEdit;
    lblInitialConcentration: TLabel;
    procedure btnedInitialConcentrationChange(Sender: TObject);
    procedure rdgConcentrationsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgConcentrationsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    function GetPestMethod(ACol: Integer): TPestParamMethod;
    function GetPestMethodAssigned(ACol: Integer): Boolean;
    function GetPestModifier(ACol: Integer): string;
    function GetPestModifierAssigned(ACol: Integer): Boolean;
    procedure SetPestMethod(ACol: Integer; const Value: TPestParamMethod);
    procedure SetPestMethodAssigned(ACol: Integer; const Value: Boolean);
    procedure SetPestModifier(ACol: Integer; const Value: string);
    procedure SetPestModifierAssigned(ACol: Integer; const Value: Boolean);
    { Private declarations }
  protected
    FDataAssigned: Boolean;
    FPestBlockParametersAndDataSets: TStringList;
    FPestParameters: TStringList;
    function GetPestModifiers: TStringList; virtual;
    procedure InitializeControls;
    property PestModifiers: TStringList read GetPestModifiers;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Property PestMethod[ACol: Integer]: TPestParamMethod
      read GetPestMethod write SetPestMethod;
    Property PestModifier[ACol: Integer]: string
      read GetPestModifier write SetPestModifier;
    Property PestMethodAssigned[ACol: Integer]: Boolean
      read GetPestMethodAssigned write SetPestMethodAssigned;
    Property PestModifierAssigned[ACol: Integer]: Boolean
      read GetPestModifierAssigned write SetPestModifierAssigned;
    { Public declarations }
  end;

implementation

uses
  frmCustomGoPhastUnit, frmGoPhastUnit, PhastModelUnit, ModflowParameterUnit,
  DataSetUnit, OrderedCollectionUnit;



{$R *.dfm}

var
  FPestMethods: TStringList;

{ TframeCustomGwtConcentrations }

procedure TframeCustomGwtConcentrations.InitializeControls;
var
  RowIndex: Integer;
  ColIndex: Integer;
  DataArrayManager: TDataArrayManager;
  DataSetIndex: Integer;
  ADataArray: TDataArray;
  ModflowSteadyParameters: TModflowSteadyParameters;
  ParameterIndex: Integer;
  AParameter: TModflowSteadyParameter;
begin
  FxButton.Canvas.Font := Font;
  btnedInitialConcentration.Glyph := FxButton;
  btnedInitialConcentration.Text := '';

  rdgConcentrations.BeginUpdate;
  try
    for RowIndex := 1 to rdgConcentrations.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgConcentrations.ColCount - 1 do
      begin
        rdgConcentrations.Cells[ColIndex, RowIndex] := '';
      end;
    end;
    rdgConcentrations.Cells[0,0] := StrStartingTime;
    rdgConcentrations.Cells[1,0] := StrEndingTime;
    rdgConcentrations.Cells[2,0] := StrStatus;

    rdgConcentrations.UseSpecialFormat[0, PestModifierRow] := True;
    rdgConcentrations.UseSpecialFormat[0, PestMethodRow] := True;
    rdgConcentrations.SpecialFormat[0, PestModifierRow] := rcf4String;
    rdgConcentrations.SpecialFormat[0, PestMethodRow] := rcf4String;
    rdgConcentrations.Cells[0, PestModifierRow] := StrPestModifier;
    rdgConcentrations.Cells[0, PestMethodRow] := StrModificationMethod;
    for ColIndex := 3 to rdgConcentrations.ColCount - 1 do
    begin
      PestMethod[ColIndex] := ppmMultiply;
    end;
  finally
    rdgConcentrations.EndUpdate;
  end;

  FPestParameters.Clear;
  FPestBlockParametersAndDataSets.Clear;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  for DataSetIndex := 0 to DataArrayManager.DataSetCount - 1 do
  begin
    ADataArray := DataArrayManager.DataSets[DataSetIndex];
    if ADataArray.PestParametersUsed then
    begin
      if ADataArray.EvaluatedAt = eaBlocks then
      begin
        FPestBlockParametersAndDataSets.AddObject(ADataArray.Name, ADataArray);
      end;
    end;
  end;
  ModflowSteadyParameters := frmGoPhast.PhastModel.ModflowSteadyParameters;
  for ParameterIndex := 0 to ModflowSteadyParameters.Count - 1 do
  begin
    AParameter := ModflowSteadyParameters[ParameterIndex];
    if AParameter.ParameterType = ptPEST then
    begin
      FPestBlockParametersAndDataSets.AddObject(AParameter.ParameterName, AParameter);
      FPestParameters.AddObject(AParameter.ParameterName, AParameter);
    end;
  end;
  FPestBlockParametersAndDataSets.Sorted := True;
  FPestBlockParametersAndDataSets.Sorted := False;
  FPestParameters.Sorted := True;
  FPestParameters.Sorted := False;
  FPestBlockParametersAndDataSets.Insert(0, strNone);
  FPestParameters.Insert(0, strNone);


  FDataAssigned := False;
end;

procedure TframeCustomGwtConcentrations.rdgConcentrationsSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Column: TRbwColumn4;
begin
  if (ACol = 2)  and  (ARow <= PestRowOffset) then
  begin
    CanSelect := False;
  end;
  if rdgConcentrations.Drawing then
  begin
    Exit;
  end;
  if (ARow >= 1) and (ACol >= 3) then
  begin
    Column := rdgConcentrations.Columns[ACol];
    if (ARow <= PestRowOffset)  then
    begin
      Column.ComboUsed := True;
      Column.LimitToList := True;
      if ARow = PestMethodRow then
      begin
        Column.PickList := FPestMethods
      end
      else
      begin
        Column.PickList := PestModifiers;
      end;
    end
    else
    begin
      Column.ButtonUsed := True;
      Column.LimitToList := False;
    end;
  end;
end;

procedure TframeCustomGwtConcentrations.rdgConcentrationsSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  FDataAssigned := True;
end;

procedure TframeCustomGwtConcentrations.btnedInitialConcentrationChange(
  Sender: TObject);
begin
  FDataAssigned := True;
end;

constructor TframeCustomGwtConcentrations.Create(AOwner: TComponent);
begin
  inherited;
  FPestBlockParametersAndDataSets := TStringList.Create;
  FPestParameters := TStringList.Create;
end;

destructor TframeCustomGwtConcentrations.Destroy;
begin
  FPestBlockParametersAndDataSets.Free;
  FPestParameters.Free;
  inherited;
end;

function TframeCustomGwtConcentrations.GetPestMethod(
  ACol: Integer): TPestParamMethod;
var
  ItemIndex: Integer;
begin
  ItemIndex := FPestMethods.IndexOf(
    rdgConcentrations.Cells[ACol,PestMethodRow]);
  if ItemIndex >= 0 then
  begin
    result := TPestParamMethod(ItemIndex);
  end
  else
  begin
    result := ppmMultiply;
  end;
end;

function TframeCustomGwtConcentrations.GetPestMethodAssigned(
  ACol: Integer): Boolean;
begin
  result := FPestMethods.IndexOf(rdgConcentrations.Cells[ACol,PestMethodRow]) >= 0;
end;

function TframeCustomGwtConcentrations.GetPestModifier(ACol: Integer): string;
begin
  result := rdgConcentrations.Cells[ACol, PestModifierRow];
  if result = strNone then
  begin
    result := '';
  end;
end;

function TframeCustomGwtConcentrations.GetPestModifierAssigned(
  ACol: Integer): Boolean;
begin
  result := rdgConcentrations.Cells[ACol, PestModifierRow] <> '';
end;

function TframeCustomGwtConcentrations.GetPestModifiers: TStringList;
begin
  result := FPestBlockParametersAndDataSets;
end;

procedure TframeCustomGwtConcentrations.SetPestMethod(ACol: Integer;
  const Value: TPestParamMethod);
begin
  rdgConcentrations.Cells[ACol,PestMethodRow] := FPestMethods[Ord(Value)];
end;

procedure TframeCustomGwtConcentrations.SetPestMethodAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := '';
  end;
end;

procedure TframeCustomGwtConcentrations.SetPestModifier(ACol: Integer;
  const Value: string);
begin
  if Value = '' then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := strNone;
  end
  else
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := Value;
  end;
end;

procedure TframeCustomGwtConcentrations.SetPestModifierAssigned(ACol: Integer;
  const Value: Boolean);
begin
  if not Value then
  begin
    rdgConcentrations.Cells[ACol, PestModifierRow] := '';
  end;
end;

initialization
  FPestMethods := TStringList.Create;
  FPestMethods.Add(StrMultiply);
  FPestMethods.Add(StrAdd);

finalization
 FPestMethods.Free;

end.
