unit frmConfigurePredTablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataGrid, StdCtrls,
  DependentsUnit, GlobalBasicData, GlobalTypesUnit,
  Utilities, Buttons;

type
  TFormConfigurePredTables = class(TForm)
    dgPredCols: TEcDataGrid;
    lblHint: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure dgPredColsClick(Sender: TObject);
    procedure dgPredColsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgPredColsUserChanged(Sender: TObject);
  private
    { Private declarations }
    procedure AssignHint(aRow: Integer);
    procedure PopulateGrid(prsSource: TPredictionSetup);
    procedure SetPickList(ARow: Integer);
  public
    { Public declarations }
    PredictionSetupConfigureChanged: boolean;
    PredictionSetupConfigure: TPredictionSetup;
  end;

var
  FormConfigurePredTables: TFormConfigurePredTables;

implementation

{$R *.dfm}

procedure TFormConfigurePredTables.AssignHint(aRow: Integer);
var
  Caption, Str, TestStr: string;
  I: Integer;
begin
  // Need to determine attribute type, given Caption.
  Caption := dgPredCols.Cells[0, aRow];
  for I := 0 to PredictionSetupConfigure.NumAtt - 1 do
    begin
      TestStr := PredictionSetupConfigure.PredAttributes[I].Caption;
      if TestStr = Caption then
      begin
        Str := Caption + ': ' + PredictionSetupConfigure.PredAttributes[I].Hint(dcPred);
        lblHint.Caption := Str;
      end;
    end;
end;

procedure TFormConfigurePredTables.btnOKClick(Sender: TObject);
begin
//  if PredictionSetupChanged then
//    begin
//      PredictionSetupCurrent.Assign(PredictionSetupLocal);
//      ProjChanged := True;
//    end;
//  Close;
end;

procedure TFormConfigurePredTables.dgPredColsClick(Sender: TObject);
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
          Str := PredictionSetupConfigure.PredAttributes[I].Caption + ': ' +
                 PredictionSetupConfigure.PredAttributes[I].Hint(dcPred);
          lblHint.Caption := Str;
        end;
    end;
end;

procedure TFormConfigurePredTables.dgPredColsSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  AssignHint(aRow);
  SetPickList(ARow);
end;

procedure TFormConfigurePredTables.dgPredColsUserChanged(Sender: TObject);
var
  IRow, J: integer;
  Caption, Str, TestStr: string;
begin
  for IRow := 0 to dgPredCols.RowCount - 1 do
    begin
      Caption := dgPredCols.Cells[0, IRow];
      for J := 0 to PredictionSetupConfigure.NumAtt - 1 do
      begin
        with PredictionSetupConfigure do
        begin
          TestStr := PredAttributes[J].Caption;
          if Caption = TestStr then
          begin
            Str := dgPredCols.Cells[2, IRow];
            if Str = 'Prediction Groups Table' then
                 PredAttributes[J].ControlMethod := cmByGroup;
            if Str = 'Predictions Table' then
                 PredAttributes[J].ControlMethod := cmByItem;
            if Str = 'No Table (use default)' then
                 PredAttributes[J].ControlMethod := cmByDefault;
          end;
        end;
      end;
    end;
  PredictionSetupConfigureChanged := True;
end;

procedure TFormConfigurePredTables.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  PredictionSetupConfigure := TPredictionSetup.Create(self);
end;

procedure TFormConfigurePredTables.FormDestroy(Sender: TObject);
begin
  PredictionSetupConfigure.Free;
end;

procedure TFormConfigurePredTables.FormShow(Sender: TObject);
begin
//  PredictionSetupLocal.Assign(PredictionSetupCurrent);
  // Populate DataGrid control dgPredCols using current values.
  PopulateGrid(PredictionSetupConfigure);
  { SelectedIndex needs to be changed from default (0); if not,
    Clicking first (upper left) non-fixed cell does not trigger
    event OnClick or OnSelectCell }
  dgPredCols.SelectedIndex := 2;
  dgPredCols.Row := 4;
  PredictionSetupConfigureChanged := False;
  lblHint.Caption := 'Click in right column to display explanation of attribute';
end;

procedure TFormConfigurePredTables.PopulateGrid(prsSource: TPredictionSetup);
var
  I, IP1: integer;
  setATDisplay: set of TDepAttType;
begin
  // Initialize.
  setATDisplay := [datMeasStatistic, datMeasStatFlag,
                   datUseFlag, datPlotSymbol];
  IP1 := 0;
  for I := 0 to NumDepAttributes - 1 do
  begin
    with prsSource do
      begin
        if PredAttributes[I].DepAttType in setATDisplay then
          begin
            IP1 := IP1 + 1;
            dgPredCols.Cells[0,IP1] := PredAttributes[I].Caption;
            case PredAttributes[I].ControlMethod of
              cmByGroup: dgPredCols.Cells[2,IP1] := 'Prediction Groups Table';
              cmByItem: dgPredCols.Cells[2,IP1] := 'Predictions Table';
              cmByDefault: dgPredCols.Cells[2,IP1] := 'No Table (use default)';
            end;
            dgPredCols.Cells[1,IP1] := PredAttributes[I].DefaultText(dcPred);

            if IsBlank(dgPredCols.Cells[1,IP1]) then
              begin
                if dgPredCols.Cells[0,IP1] = 'Variance-Covariance Matrix Name' then
                  begin
                    dgPredCols.Cells[1,IP1] := '<blank string>';
                  end
                else
                  begin
                    dgPredCols.Cells[1,IP1] := '<no default defined>';
                  end;
              end;

          end;
      end;
  end;
end;

procedure TFormConfigurePredTables.SetPickList(ARow: Integer);
begin
  // Assign pick list for this attribute.
  case ARow of
//    1: dgPredCols.Columns[2].PickList := slPrCMPredDefault; // GroupName.
    1: dgPredCols.Columns[2].PickList := slPrCMPredGps;     // MeasStatistic.
    2: dgPredCols.Columns[2].PickList := slPrCMPredGps;     // MeasStatFlag.
    3: dgPredCols.Columns[2].PickList := slPrCMGpsDefault;  // UseFlag.
    4: dgPredCols.Columns[2].PickList := slPrCMGpsDefault;  // PlotSymbol.
  end;
end;

end.
