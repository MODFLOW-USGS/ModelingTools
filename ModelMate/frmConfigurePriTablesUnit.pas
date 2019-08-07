unit frmConfigurePriTablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  Grids, DataGrid, PriorInfoUnit, GlobalBasicData,
  GlobalTypesUnit, Utilities, Buttons;

type
  TFormConfigPriTables = class(TForm)
    lblHint: TLabel;
    dgPriCols: TEcDataGrid;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dgPriColsUserChanged(Sender: TObject);
    procedure dgPriColsSelectCell(Sender: TObject;
        ACol, ARow: Integer; var CanSelect: Boolean);
    procedure dgPriColsClick(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure AssignHint(aRow: Integer);
    procedure PopulateGrid(psSource: TPriorSetup);
    procedure SetPickList(ARow: Integer);
  public
    { Public declarations }
    PriorSetupConfigureChanged: boolean;
    PriorSetupConfigure: TPriorSetup;
  end;

var
  FormConfigPriTables: TFormConfigPriTables;

implementation

{$R *.dfm}

procedure TFormConfigPriTables.AssignHint(aRow: Integer);
var
  Str: string;
  TestStr: string;
  I: Integer;
  Caption: string;
begin
  // Need to determine attribute type, given Caption.
  Caption := dgPriCols.Cells[0, aRow];
  for I := 0 to PriorSetupConfigure.NumAtt - 1 do
    begin
      TestStr := PriorSetupConfigure.PriAttributes[I].Caption;
      if TestStr = Caption then
      begin
        Str := Caption + ': ' + PriorSetupConfigure.PriAttributes[I].Hint;
        lblHint.Caption := Str;
      end;
    end;
end;

procedure TFormConfigPriTables.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormConfigPriTables.btnOKClick(Sender: TObject);
begin
//  if PriorSetupChanged then
//    begin
//      PriorSetupCurrent.Assign(PriorSetupLocal);
//      ProjChanged := True;
//    end;
//  Close;
end;

procedure TFormConfigPriTables.dgPriColsClick(Sender: TObject);
var
  pt: TPoint;
  grid: TStringGrid;
  aCol, aRow, I: integer;
  Str: string;
begin
  GetCursorPos(pt);
  grid := Sender as TStringGrid;
  pt := grid.ScreenToClient(pt);
  grid.MouseToCell(pt.x, pt.Y, aCol, aRow);
  if aRow > 0 then
    begin
      SetPickList(aRow);
      if (aCol = 0) then
        begin
          I := aRow - 1;
          Str := PriorSetupConfigure.PriAttributes[I].Caption + ': ' +
                 PriorSetupConfigure.PriAttributes[I].Hint;
          lblHint.Caption := Str;
        end;
    end;
end;

procedure TFormConfigPriTables.dgPriColsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  AssignHint(aRow);
  SetPickList(ARow);
end;

procedure TFormConfigPriTables.dgPriColsUserChanged(Sender: TObject);
var
  IRow, J: integer;
  Caption, Str, TestStr: string;
begin
  for IRow := 0 to dgPriCols.RowCount - 1 do
    begin
      Caption := dgPriCols.Cells[0, IRow];
      for J := 0 to PriorSetupConfigure.NumAtt - 1 do
      begin
        with PriorSetupConfigure do
        begin
          TestStr := PriAttributes[J].Caption;
          if Caption = TestStr then
          begin
            Str := dgPriCols.Cells[2, IRow];
            if Str = 'Prior-Information Groups Table' then
                 PriAttributes[J].ControlMethod := cmByGroup;
            if Str = 'Prior-Information Table' then
                 PriAttributes[J].ControlMethod := cmByItem;
            if Str = 'No Table (use default)' then
                 PriAttributes[J].ControlMethod := cmByDefault;
          end;
        end;
      end;
    end;
  PriorSetupConfigureChanged := True;
end;

procedure TFormConfigPriTables.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  PriorSetupConfigure := TPriorSetup.Create(self);
end;

procedure TFormConfigPriTables.FormDestroy(Sender: TObject);
begin
  PriorSetupConfigure.Free;
end;

procedure TFormConfigPriTables.FormShow(Sender: TObject);
begin
//  PriorSetupLocal.Assign(PriorSetupCurrent);
  // Populate DataGrid control dgPriCols using current values
  PopulateGrid(PriorSetupConfigure);
  { SelectedIndex needs to be changed from default (0); if not,
    Clicking first (upper left) non-fixed cell does not trigger
    event OnClick or OnSelectCell }
  dgPriCols.SelectedIndex := 2;
  dgPriCols.Row := 6;
  PriorSetupConfigureChanged := False;
  lblHint.Caption := 'Click in right column to display explanation of attribute';
end;

procedure TFormConfigPriTables.PopulateGrid(psSource: TPriorSetup);
var
  I, IP1: integer;
  setATDisplay: set of TPriAttType;
begin
  // Initialize.
  setATDisplay := [piatStatistic, piatStatflag, piatUseFlag,
                   piatPlotSymbol, piatWtMultiplier, piatCovMatrix];
  IP1 := 0;
  for I := 0 to NumPriAttributes - 1 do
  begin
    with psSource do
      begin
        if PriAttributes[I].PriAttType in setATDisplay then
          begin
            IP1 := IP1 + 1;
            dgPriCols.Cells[0,IP1] := PriAttributes[I].Caption;
            case PriAttributes[I].ControlMethod of
              cmByGroup: dgPriCols.Cells[2,IP1] := 'Prior-Information Groups Table';
              cmByItem: dgPriCols.Cells[2,IP1] := 'Prior-Information Table';
              cmByDefault: dgPriCols.Cells[2,IP1] := 'No Table (use default)';
            end;
            dgPriCols.Cells[1,IP1] := PriAttributes[I].DefaultText;
            if PriAttributes[I].Caption = 'Variance-Covariance Matrix Name' then
              dgPriCols.Cells[1,IP1] := '<blank string>'
            else if (PriAttributes[I].Caption = 'Statistic')
                     or (PriAttributes[I].Caption = 'StatFlag') then
              dgPriCols.Cells[1,IP1] := '<no default defined>';
          end;
      end;
  end;
end;

procedure TFormConfigPriTables.SetPickList(ARow: Integer);
begin
  // Assign pick list for this attribute
  case ARow of
    1: dgPriCols.Columns[2].PickList := slCMPriGps;     // Statistic.
    2: dgPriCols.Columns[2].PickList := slCMPriGps;     // StatFlag.
    3: dgPriCols.Columns[2].PickList := slCMPriGpsDefault; // UseFlag.
    4: dgPriCols.Columns[2].PickList := slCMPriGpsDefault; // PlotSymbol.
    5: dgPriCols.Columns[2].PickList := slCMPriGpsDefault; // WtMultiplier.
    6: dgPriCols.Columns[2].PickList := slCMPriGpsDefault; // CovMatrix.
  end;
end;

end.
