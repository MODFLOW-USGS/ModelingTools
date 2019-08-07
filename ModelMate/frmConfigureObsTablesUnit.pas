unit frmConfigureObsTablesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Grids, DataGrid, StdCtrls, DependentsUnit, GlobalBasicData,
  GlobalTypesUnit, Utilities, Buttons;

type
  TFormConfigureObsTables = class(TForm)
    dgObsCols: TEcDataGrid;
    lblHint: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure FormShow(Sender: TObject);
    procedure dgObsColsUserChanged(Sender: TObject);
    procedure dgObsColsSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgObsColsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure AssignHint(aRow: Integer);
    procedure PopulateGrid(osSource: TObservationSetup);
    procedure SetPickList(ARow: Integer);
  public
    { Public declarations }
    ObservationSetupConfigureChanged: boolean;
    ObservationSetupConfigure: TObservationSetup;
  end;

var
  FormConfigureObsTables: TFormConfigureObsTables;

implementation

{$R *.dfm}

procedure TFormConfigureObsTables.AssignHint(aRow: Integer);
var
  Str: string;
  TestStr: string;
  I: Integer;
  Caption: string;
begin
    // Need to determine attribute type, given Caption
    Caption := dgObsCols.Cells[0, aRow];
    for I := 0 to ObservationSetupConfigure.NumAtt - 1 do
      begin
        TestStr := ObservationSetupConfigure.ObsAttributes[I].Caption;
        if TestStr = Caption then
          begin
            Str := Caption + ': ' + ObservationSetupConfigure.ObsAttributes[I].Hint(dcObs);
            lblHint.Caption := Str;
          end;
      end;
end;

procedure TFormConfigureObsTables.SetPickList(ARow: Integer);
begin
  // Assign pick list for this attribute
  case ARow of
    1: dgObsCols.Columns[2].PickList := slOCMObsGps;     // Statistic
    2: dgObsCols.Columns[2].PickList := slOCMObsGps;     // StatFlag
    3: dgObsCols.Columns[2].PickList := slOCMGpsDefault; // UseFlag
    4: dgObsCols.Columns[2].PickList := slOCMGpsDefault; // PlotSymbol
    5: dgObsCols.Columns[2].PickList := slOCMAllOptions; // WtMultiplier
    6: dgObsCols.Columns[2].PickList := slOCMGpsDefault; // CovMatrix
    7: dgObsCols.Columns[2].PickList := slOCMAllOptions; // NonDetect
    8: dgObsCols.Columns[2].PickList := slOCMAllOptions; // WtOSConstant
  end;
end;

procedure TFormConfigureObsTables.dgObsColsClick(Sender: TObject);
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

procedure TFormConfigureObsTables.dgObsColsSelectCell(Sender: TObject; ACol, ARow: Integer;
  var CanSelect: Boolean);
begin
  AssignHint(aRow);
  SetPickList(ARow);
end;

procedure TFormConfigureObsTables.dgObsColsUserChanged(Sender: TObject);
var
  IRow, J: integer;
  Caption, Str, TestStr: string;
begin
  for IRow := 0 to dgObsCols.RowCount - 1 do
    begin
      Caption := dgObsCols.Cells[0, IRow];
      for J := 0 to ObservationSetupConfigure.NumAtt - 1 do
      begin
        with ObservationSetupConfigure do
        begin
          TestStr := ObsAttributes[J].Caption;
          if Caption = TestStr then
          begin
            Str := dgObsCols.Cells[2, IRow];
            if Str = 'Observation Groups Table' then
                 ObsAttributes[J].ControlMethod := cmByGroup;
            if Str = 'Observations Table' then
                 ObsAttributes[J].ControlMethod := cmByItem;
            if Str = 'No Table (use default)' then
                 ObsAttributes[J].ControlMethod := cmByDefault;
          end;
        end;
      end;
    end;
  ObservationSetupConfigureChanged := True;
end;

procedure TFormConfigureObsTables.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  ObservationSetupConfigure := TObservationSetup.Create(self);
end;

procedure TFormConfigureObsTables.FormDestroy(Sender: TObject);
begin
  ObservationSetupConfigure.Free;
end;

procedure TFormConfigureObsTables.FormShow(Sender: TObject);
begin
//  ObservationSetupConfigure.Assign(ObservationSetupCurrent);
  // Populate DataGrid control dgObsCols using current values
  PopulateGrid(ObservationSetupConfigure);
  { SelectedIndex needs to be changed from default (0); if not,
    Clicking first (upper left) non-fixed cell does not trigger
    event OnClick or OnSelectCell }
  dgObsCols.SelectedIndex := 2;
  dgObsCols.Row := 8;
  ObservationSetupConfigureChanged := False;
  lblHint.Caption := 'Click in right column to display explanation of attribute';
end;

procedure TFormConfigureObsTables.PopulateGrid(osSource: TObservationSetup);
var
  I, IP1: integer;
  setATDisplay: set of TDepAttType;
begin
  // Initialize
  setATDisplay := [datStatistic, datStatflag, datUseFlag,
                   datPlotSymbol, datWtMultiplier, datNonDetect,
                   datWtOSConstant, datCovMatrix];
  IP1 := 0;
  for I := 0 to NumDepAttributes - 1 do
  begin
    with osSource do
      begin
        if ObsAttributes[I].DepAttType in setATDisplay then
          begin
            IP1 := IP1 + 1;
            dgObsCols.Cells[0,IP1] := ObsAttributes[I].Caption;
            case ObsAttributes[I].ControlMethod of
              cmByGroup: dgObsCols.Cells[2,IP1] := 'Observation Groups Table';
              cmByItem: dgObsCols.Cells[2,IP1] := 'Observations Table';
              cmByDefault: dgObsCols.Cells[2,IP1] := 'No Table (use default)';
            end;
            dgObsCols.Cells[1,IP1] := ObsAttributes[I].DefaultText(dcObs);
            if IsBlank(dgObsCols.Cells[1,IP1]) then
              begin
                if dgObsCols.Cells[0,IP1] = 'Variance-Covariance Matrix Name' then
                  begin
                    dgObsCols.Cells[1,IP1] := '<blank string>';
                  end
                else
                  begin
                    dgObsCols.Cells[1,IP1] := '<no default defined>';
                  end;
              end;
          end;
      end;
  end;
end;

end.
