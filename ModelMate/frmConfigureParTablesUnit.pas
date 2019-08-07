unit frmConfigureParTablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms,
  Dialogs, Grids, StdCtrls, DataGrid,
  GlobalBasicData, GlobalData, GlobalTypesUnit, JupiterUnit, Utilities,
  ModelMateClassesUnit, ModelMateUtilities, ComCtrls, JvExComCtrls, JvStatusBar,
  Buttons;

type
  TFormConfigureParTables = class(TForm)
    dgParCols: TEcDataGrid;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    lblStatus: TLabel;
    cbFreezeParNames: TCheckBox;
    procedure btnOKClick(Sender: TObject);
    procedure dgParColsClick(Sender: TObject);
    procedure dgParColsSelectCell(Sender: TObject; ACol, ARow: Integer;
              var CanSelect: Boolean);
    procedure dgParColsUserChanged(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StatusBar1Click(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure SetPickList(ARow: Integer);
  public
    { Public declarations }
    FreezeNames: boolean;
    function GetHint(aRow: Integer): string;
    procedure PopulateGrid(psSource: TParameterSetup);
  end;

var
  FormConfigureParTables: TFormConfigureParTables;
  ParameterSetupLocal: TParameterSetup;
  slPCMShowHide: TStringList;
  slPCMGpDefault: TStringList;
  slPCMItemGp: TStringList;
  slPCMAllOptions: TStringList;

implementation

{$R *.dfm}

function TFormConfigureParTables.GetHint(aRow: Integer): string;
var
  Caption, TestStr: string;
  I: Integer;
begin
    result := '';
    // Need to determine attribute type, given Caption.
    Caption := dgParCols.Cells[0, aRow];
    for I := 0 to ParameterSetupLocal.NumAtt - 1 do
    begin
      TestStr := ParameterSetupLocal.ParAttributes[I].Caption;
      if TestStr = Caption then
      begin
        result := Caption + ': ' + ParameterSetupLocal.ParAttributes[I].Hint;
      end;
    end;
end;

procedure TFormConfigureParTables.btnOKClick(Sender: TObject);
begin
  ParameterSetupCurrent.Assign(ParameterSetupLocal);
  UpdateCurrentProject;
  if ParameterSetupChanged then ProjChanged := True;
  FreezeNames := cbFreezeParNames.Checked;
end;

procedure TFormConfigureParTables.dgParColsClick(Sender: TObject);
// If user clicks on an attribute, show hint
var
  pt: TPoint;
  grid: TStringGrid;
  aCol, aRow: integer;
begin
  GetCursorPos(pt);
  grid := Sender as TStringGrid;
  pt := grid.ScreenToClient(pt);
  grid.MouseToCell(pt.x, pt.Y, aCol, aRow);
  if aRow > 0 then
    begin
      SetPickList(aRow);
    end;
end;

procedure TFormConfigureParTables.dgParColsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  SetPickList(ARow);
  lblStatus.Caption := GetHint(aRow);
end;

procedure TFormConfigureParTables.dgParColsUserChanged(Sender: TObject);
var
  IRow, J: integer;
  Caption, Str, TestStr: string;
begin
  for IRow := 1 to dgParCols.RowCount - 1 do
    begin
      Caption := dgParCols.Cells[0, IRow];
      for J := 0 to ParameterSetupLocal.NumAtt - 1 do
      begin
        with ParameterSetupLocal do
        begin
          TestStr := ParAttributes[J].Caption;
          if Caption = TestStr then
          begin
            Str := dgParCols.Cells[2, IRow];
            if Str = 'Parameter Groups Table' then
                 ParAttributes[J].ControlMethod := cmByGroup;
            if Str = 'Parameters Table' then
                 ParAttributes[J].ControlMethod := cmByItem;
            if Str = 'No Table (use default)' then
                 ParAttributes[J].ControlMethod := cmByDefault;
            if Str = 'Show in Parameters Table' then
                 ParAttributes[J].ControlMethod := cmByItem;
            if Str = 'Hide' then
                 ParAttributes[J].ControlMethod := cmByDefault;
          end;
        end;
      end;
    end;
  ParameterSetupLocal.SetCombinedMethods;
  ParameterSetupChanged := True;
end;

procedure TFormConfigureParTables.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  ParameterSetupLocal := TParameterSetup.Create(self);
  slPCMShowHide := TStringList.Create;
  slPCMShowHide.Add('Show in Parameters Table');
  slPCMShowHide.Add('Hide');
  //
  slPCMGpDefault := TStringList.Create;
  slPCMGpDefault.Add('Parameter Groups Table');
  slPCMGpDefault.Add('No Table (use default)');
  //
  slPCMItemGp := TStringList.Create;
  slPCMItemGp.Add('Parameters Table');
  slPCMItemGp.Add('Parameter Groups Table');
  //
  slPCMAllOptions := TStringList.Create;
  slPCMAllOptions.Add('Parameters Table');
  slPCMAllOptions.Add('Parameter Groups Table');
  slPCMAllOptions.Add('No Table (use default)');
end;

procedure TFormConfigureParTables.FormDestroy(Sender: TObject);
begin
  FreeAndNil(slPCMShowHide);
  FreeAndNil(slPCMGpDefault);
  FreeAndNil(slPCMItemGp);
  FreeAndNil(slPCMAllOptions);
end;

procedure TFormConfigureParTables.FormShow(Sender: TObject);
begin
  // Populate DataGrid control dgParCols using current values
  ParameterSetupLocal.Assign(ParameterSetupCurrent);
  PopulateGrid(ParameterSetupLocal);
  { SelectedIndex needs to be changed from default (0); if not,
    Clicking first (upper left) non-fixed cell does not trigger
    event OnClick or OnSelectCell }
  dgParCols.SelectedIndex := 2;
  cbFreezeParNames.Checked := FreezeNames;
  lblStatus.Caption := 'Click in right column to display explanation of attribute';
end;

procedure TFormConfigureParTables.PopulateGrid(psSource: TParameterSetup);
var
  I, IP1: integer;
  setATDisplay: set of TParamAttType;
begin
  // Initialize
  setATDisplay := [patAdjustable, patConstraints, patDerived,
                   patMaxChange, patPerturbAmt, patReasRange,
                   patScalePval, patSenMethod, patTolPar,
                   patTransform];  // Later: atSosIncrement, atNonLinearInterval
  // Set up DataGrid dgParCols using values from specified ParamSetup
  IP1 := 0;
  for I := 0 to NumParAttributes - 1 do
  begin
    with psSource do
      begin
        if ParAttributes[I].ParamAttType in setATDisplay then
          begin
            IP1 := IP1 + 1;
            dgParCols.Cells[0,IP1] := ParAttributes[I].Caption;
            if ParAttributes[I].ParamAttType = patDerived then
              begin
                case ParAttributes[I].ControlMethod of
                  cmByItem: dgParCols.Cells[2,IP1] := 'Show in Parameters Table';
                  cmByDefault: dgParCols.Cells[2,IP1] := 'Hide';
                end;
                dgParCols.Cells[1,IP1] := 'Hide';
              end
            else
              begin
                case ParAttributes[I].ControlMethod of
                  cmByGroup: dgParCols.Cells[2,IP1] := 'Parameter Groups Table';
                  cmByItem: dgParCols.Cells[2,IP1] := 'Parameters Table';
                  cmByDefault: dgParCols.Cells[2,IP1] := 'No Table (use default)';
                end;
                dgParCols.Cells[1,IP1] := ParAttributes[I].DefaultText;
              end;
          end;
      end;
  end;

end;

procedure TFormConfigureParTables.SetPickList(ARow: Integer);
begin
  // Assign pick list for this attribute
  case ARow of
    1: dgParCols.Columns[2].PickList := slPCMShowHide; // Derived.
    2: dgParCols.Columns[2].PickList := slPCMAllOptions;  // Adjustable.
    3: dgParCols.Columns[2].PickList := slPCMAllOptions;  // Transform.
    4: dgParCols.Columns[2].PickList := slPCMAllOptions;  // TolPar.
    5: dgParCols.Columns[2].PickList := slPCMAllOptions;  // MaxChange.
    6: dgParCols.Columns[2].PickList := slPCMAllOptions;  // PerturbAmt.
    7: dgParCols.Columns[2].PickList := slPCMAllOptions;  // ReasRange.
    8: dgParCols.Columns[2].PickList := slPCMAllOptions;  // SenMethod.
    9: dgParCols.Columns[2].PickList := slPCMAllOptions;  // Constraints.
    10: dgParCols.Columns[2].PickList := slPCMAllOptions; // ScalePval.
  end;
end;

procedure TFormConfigureParTables.StatusBar1Click(Sender: TObject);
begin
  ShowMessage(lblStatus.Caption);
end;

initialization

finalization

end.
