unit frameLakeOutletUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameGridUnit, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, Vcl.Buttons, Vcl.ExtCtrls,
  ModflowLakMf6Unit, JvExStdCtrls, JvCombobox, JvListComb, ArgusDataEntry,
  frameFormulaGridUnit, System.UITypes;

type
  TOutletColumn = (ocStart, ocEnd, ocRate, ocInvert, ocWidth, ocSlope, coRoughness);

  TframeLakeOutlet = class(TframeFormulaGrid)
    lblOutletLake: TLabel;
    comboOutletType: TJvImageComboBox;
    lblOutletType: TLabel;
    comboOutlet: TComboBox;
    procedure GridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdeOutletChange(Sender: TObject);
    procedure comboOutletTypeChange(Sender: TObject);
    procedure GridButtonClick(Sender: TObject; ACol, ARow: Integer);
  private
    FOnChange: TNotifyEvent;
    FGridButtonEvent: TGridButtonEvent;
    FFrameLoaded: boolean;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure UpdateNextTimeCell(DataGrid: TRbwDataGrid4; ACol, ARow: Integer);
    procedure SetFrameLoaded(const Value: boolean);
    { Private declarations }
  public
    procedure InitializeControls;
    procedure GetData(const LakeOutlet: TLakeOutlet);
    procedure SetData(const LakeOutlet: TLakeOutlet);
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property GridButtonEvent: TGridButtonEvent read FGridButtonEvent
      write FGridButtonEvent;
    property FrameLoaded: boolean read FFrameLoaded write SetFrameLoaded;
    { Public declarations }
  end;

var
  frameLakeOutlet: TframeLakeOutlet;

implementation

uses
  frmGoPhastUnit, GoPhastTypes, frmCustomGoPhastUnit;

resourcestring
  StrRate = 'Rate';
  StrInvert = 'Invert';
  StrWidth = 'Width';
  StrSlope = 'Slope';
  StrRoughness = 'Roughness';
  StrNoOutletLakeIsSp = 'No outlet lake is specified for outlet %d.';

{$R *.dfm}

{ TframeLakeOutlet }

procedure TframeLakeOutlet.Changed;
begin
  if Assigned(OnChange) then
  begin
    OnChange(self);
  end;
end;

procedure TframeLakeOutlet.comboOutletTypeChange(Sender: TObject);
begin
  inherited;
  Changed;
end;

procedure TframeLakeOutlet.GetData(const LakeOutlet: TLakeOutlet);
var
  ItemIndex: Integer;
  OutletItem: TLakeOutletTimeItem;
  OutletName: string;
begin
  InitializeControls;

  OutletName := LakeOutlet.OutletObjectName;
  if OutletName = '' then
  begin
    comboOutlet.ItemIndex := 0;
  end
  else
  begin
    comboOutlet.ItemIndex := comboOutlet.Items.IndexOf(OutletName);
  end;

//  rdeOutlet.IntegerValue := LakeOutlet.Outlet;
  comboOutletType.ItemIndex := Ord(LakeOutlet.OutletType);
  seNumber.AsInteger := LakeOutlet.LakeTimes.Count;
  if Assigned(seNumber.OnChange) then
  begin
    seNumber.OnChange(seNumber);
  end;

  for ItemIndex := 0 to LakeOutlet.LakeTimes.Count - 1 do
  begin
    OutletItem := LakeOutlet.LakeTimes[ItemIndex];
    Grid.RealValue[Ord(ocStart), ItemIndex+1] := OutletItem.StartTime;
    Grid.RealValue[Ord(ocEnd), ItemIndex+1] := OutletItem.EndTime;
    Grid.Cells[Ord(ocRate), ItemIndex+1] := OutletItem.Rate;
    Grid.Cells[Ord(ocInvert), ItemIndex+1] := OutletItem.Invert;
    Grid.Cells[Ord(ocWidth), ItemIndex+1] := OutletItem.Width;
    Grid.Cells[Ord(ocSlope), ItemIndex+1] := OutletItem.Slope;
    Grid.Cells[Ord(coRoughness), ItemIndex+1] := OutletItem.Roughness;
  end;
end;

procedure TframeLakeOutlet.GridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  if Assigned(GridButtonEvent) then
  begin
    GridButtonEvent(Sender, ACol, ARow);
  end;
end;

procedure TframeLakeOutlet.GridSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  inherited;
  UpdateNextTimeCell(Sender as TRbwDataGrid4, ACol, ARow);
  Changed;
end;

procedure TframeLakeOutlet.InitializeControls;
var
  ColIndex: Integer;
begin
  FirstFormulaColumn := 2;

  Grid.BeginUpdate;
  try
    Grid.Cells[Ord(ocStart), 0] := StrStartingTime;
    Grid.Cells[Ord(ocEnd), 0] := StrEndingTime;
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithStartTimes(Grid, Ord(ocStart));
    frmGoPhast.PhastModel.ModflowStressPeriods.
      FillPickListWithEndTimes(Grid, Ord(ocEnd));

    Grid.Cells[Ord(ocRate), 0] := StrRate;
    Grid.Cells[Ord(ocInvert), 0] := StrInvert;
    Grid.Cells[Ord(ocWidth), 0] := StrWidth;
    Grid.Cells[Ord(ocSlope), 0] := StrSlope;
    Grid.Cells[Ord(coRoughness), 0] := StrRoughness;
  finally
    Grid.EndUpdate;
  end;

  for ColIndex := 0 to Grid.ColCount - 1 do
  begin
    Grid.Columns[ColIndex].AutoAdjustColWidths := False;
    Grid.Columns[ColIndex].ButtonFont := Font;
  end;

end;

procedure TframeLakeOutlet.rdeOutletChange(Sender: TObject);
begin
  inherited;
  Changed;
end;

procedure TframeLakeOutlet.SetData(const LakeOutlet: TLakeOutlet);
var
  ItemIndex: Integer;
  OutletItem: TLakeOutletTimeItem;
begin
  if comboOutlet.ItemIndex >= 0 then
  begin
    LakeOutlet.OutletObject := comboOutlet.Items.Objects[comboOutlet.ItemIndex];
  end
  else
  begin
    LakeOutlet.OutletObject := nil;
  end;
  if LakeOutlet.OutletObject = nil then
  begin
    Beep;
    MessageDlg(Format(StrNoOutletLakeIsSp, [LakeOutlet.OutletIndex]), mtWarning, [mbOK], 0);
  end;
  LakeOutlet.OutletType := TLakeOutletType(comboOutletType.ItemIndex);
  LakeOutlet.LakeTimes.Count := seNumber.AsInteger;
  for ItemIndex := 0 to LakeOutlet.LakeTimes.Count - 1 do
  begin
    OutletItem := LakeOutlet.LakeTimes[ItemIndex];
    OutletItem.StartTime := Grid.RealValue[Ord(ocStart), ItemIndex+1];
    OutletItem.EndTime := Grid.RealValue[Ord(ocEnd), ItemIndex+1];
    OutletItem.Rate := Grid.Cells[Ord(ocRate), ItemIndex+1];
    OutletItem.Invert := Grid.Cells[Ord(ocInvert), ItemIndex+1];
    OutletItem.Width := Grid.Cells[Ord(ocWidth), ItemIndex+1];
    OutletItem.Slope := Grid.Cells[Ord(ocSlope), ItemIndex+1];
    OutletItem.Roughness := Grid.Cells[Ord(coRoughness), ItemIndex+1];
  end;
end;

procedure TframeLakeOutlet.SetFrameLoaded(const Value: boolean);
begin
  FFrameLoaded := Value;
end;

procedure TframeLakeOutlet.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TframeLakeOutlet.UpdateNextTimeCell(DataGrid: TRbwDataGrid4;
  ACol, ARow: Integer);
//var
//  SelectIndex: Integer;
begin
  if FrameLoaded then
  begin
    frmCustomGoPhastUnit.UpdateNextTimeCell(DataGrid, ACol, ARow);
  end;
//  if FrameLoaded and (ARow >= DataGrid.FixedRows) and (ACol in [0, 1])
//    {and (FLastTimeColumn = 1)} then
//  begin
//    SelectIndex := DataGrid.ItemIndex[ACol, ARow];
//    if SelectIndex >= 0 then
//    begin
//      if (ACol = 0) then
//      begin
//        if DataGrid.Cells[1, ARow] = '' then
//        begin
//          DataGrid.ItemIndex[1, ARow] := SelectIndex;
//        end;
//      end
//      else if (ACol = 1) then
//      begin
//        if (ARow + 1 < DataGrid.RowCount) and
//          (DataGrid.Cells[0, ARow + 1] = '') then
//        begin
//          if SelectIndex + 1 < DataGrid.Columns[0].PickList.Count then
//          begin
//            DataGrid.ItemIndex[0, ARow + 1] := SelectIndex + 1;
//          end;
//        end;
//      end;
//    end;
//  end;
end;

end.
