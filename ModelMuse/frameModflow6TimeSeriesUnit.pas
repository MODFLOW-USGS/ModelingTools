unit frameModflow6TimeSeriesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, Vcl.ExtCtrls,
  Modflow6TimeSeriesUnit, Math, GoPhastTypes, Modflow6TimeSeriesCollectionsUnit;

type


  TframeModflow6TimeSeries = class(TFrame)
    pnlBottom: TPanel;
    seTimeSeriesCount: TJvSpinEdit;
    seTimeCount: TJvSpinEdit;
    lblTimeSeriesCount: TLabel;
    lblTimeCount: TLabel;
    btnDeleteTimeSeries: TButton;
    btnDeleteTime: TButton;
    rrdgTimeSeries: TRbwRowDataGrid;
    pnlTop: TPanel;
    edGroupName: TEdit;
    btnInsertTime: TButton;
    procedure rrdgTimeSeriesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seTimeSeriesCountChange(Sender: TObject);
    procedure seTimeCountChange(Sender: TObject);
    procedure btnDeleteTimeSeriesClick(Sender: TObject);
    procedure btnDeleteTimeClick(Sender: TObject);
    procedure rrdgTimeSeriesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnInsertTimeClick(Sender: TObject);
  private
    FTimesSeriesGroupItem: TimeSeriesCollectionItem;
    FTimesSeriesGroup: TTimesSeriesCollection;
    procedure InitializeTimeRows;
    { Private declarations }
  public
    procedure InitializeGrid;
    procedure GetData(ATimesSeriesGroupItem: TimeSeriesCollectionItem; PestNames: TStringList);
    procedure SetData;
    { Public declarations }
  end;

implementation

resourcestring
  StrName = 'Name';
  StrTimes = 'Times';
  StrScaleFactorSFAC = 'Scale Factor (SFAC)';
  StrInterpolationMethod = 'Interpolation Method (METHOD)';

{$R *.dfm}

type
  TTimeSeriesRows = (tsrName, tsrPestModifier, tsrPestMethod, tsrScaleFactor,
    tsrInterpolation, tsrFirstTime);
  TTimeSeriesColumns = (tscLabel, tscTimes, tscFirstSeries);

procedure TframeModflow6TimeSeries.btnDeleteTimeClick(Sender: TObject);
begin
  if (rrdgTimeSeries.Row >= Ord(tsrFirstTime))
    and (seTimeCount.AsInteger > 1) then
  begin
    rrdgTimeSeries.DeleteRow(rrdgTimeSeries.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger -1;
    seTimeCountChange(nil);
  end;
end;

procedure TframeModflow6TimeSeries.btnDeleteTimeSeriesClick(Sender: TObject);
begin
  if rrdgTimeSeries.Col >= 2 then
  begin
    rrdgTimeSeries.Objects[rrdgTimeSeries.Col, 0].Free;
    rrdgTimeSeries.Objects[rrdgTimeSeries.Col, 0] := nil;
    rrdgTimeSeries.DeleteColumn(rrdgTimeSeries.Col);
    seTimeSeriesCount.AsInteger := seTimeSeriesCount.AsInteger -1;
    seTimeSeriesCountChange(nil);
  end;
end;

procedure TframeModflow6TimeSeries.btnInsertTimeClick(Sender: TObject);
begin
  if (rrdgTimeSeries.Row >= Ord(tsrFirstTime)) then
  begin
    rrdgTimeSeries.InsertRow(rrdgTimeSeries.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger +1;
    seTimeCountChange(nil);
  end;
end;

procedure TframeModflow6TimeSeries.GetData(
  ATimesSeriesGroupItem: TimeSeriesCollectionItem; PestNames: TStringList);
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  ASeries: TMf6TimeSeries;
  ColIndex: Integer;
  ASeriesItem: TTimeSeriesItem;
begin
  FTimesSeriesGroupItem := ATimesSeriesGroupItem;
  FTimesSeriesGroup := ATimesSeriesGroupItem.TimesSeriesCollection;
  edGroupName.Text := FTimesSeriesGroup.GroupName;
  rrdgTimeSeries.BeginUpdate;
  try
    rrdgTimeSeries.Rows[Ord(tsrPestModifier)].PickList := PestNames;
    seTimeSeriesCount.AsInteger := FTimesSeriesGroup.Count;
    seTimeCount.AsInteger := Max(1,FTimesSeriesGroup.Times.Count);
    InitializeGrid;
    for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
    begin
      rrdgTimeSeries.RealValue[Ord(tscTimes), TimeIndex + Ord(tsrFirstTime)]
        := FTimesSeriesGroup.Times[TimeIndex].Value;
    end;

    for SeriesIndex := 0 to FTimesSeriesGroup.Count - 1 do
    begin
      ColIndex := SeriesIndex + Ord(tscFirstSeries);
      ASeriesItem := FTimesSeriesGroup[SeriesIndex];
      rrdgTimeSeries.Objects[SeriesIndex + Ord(tscFirstSeries), 0]
        := ASeriesItem;
      ASeries := ASeriesItem.TimeSeries;
      rrdgTimeSeries.Columns[SeriesIndex + Ord(tscFirstSeries)].
        AutoAdjustColWidths := True;
      rrdgTimeSeries.Cells[ColIndex, Ord(tsrName)] := ASeries.SeriesName;
      if ASeries.ScaleFactorParameter = '' then
      begin
        rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestModifier)] := 0;
      end
      else
      begin
        rrdgTimeSeries.ItemIndex[ColIndex,Ord(tsrPestModifier)] :=
          PestNames.IndexOf(ASeries.ScaleFactorParameter);
      end;
      rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestMethod)] :=
        Ord(ASeries.ParamMethod);
      rrdgTimeSeries.RealValue[ColIndex, Ord(tsrScaleFactor)] :=
        ASeries.ScaleFactor;
      rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrInterpolation)] :=
        Ord(ASeries.InterpolationMethod);

      Assert(ASeries.Count = FTimesSeriesGroup.Times.Count);
      for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
      begin
        rrdgTimeSeries.RealValue[ColIndex, TimeIndex + Ord(tsrFirstTime)]
          := ASeries[TimeIndex].Value;
      end;
    end;
  finally
    rrdgTimeSeries.EndUpdate;
  end;
end;

procedure TframeModflow6TimeSeries.InitializeTimeRows;
var
  TimeIndex: Integer;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    for TimeIndex := 1 to seTimeCount.AsInteger do
    begin
      rrdgTimeSeries.Cells[Ord(tscLabel), TimeIndex-1 + Ord(tsrFirstTime)] :=
        IntToStr(TimeIndex);
      rrdgTimeSeries.Rows[TimeIndex-1 + Ord(tsrFirstTime)].Format := rcf4Real;
    end;
  finally
    rrdgTimeSeries.EndUpdate
  end;
end;

procedure TframeModflow6TimeSeries.InitializeGrid;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    rrdgTimeSeries.Cells[Ord(tscLabel), Ord(tsrName)] := StrName;
    rrdgTimeSeries.Cells[Ord(tscTimes), Ord(tsrName)] := StrTimes;
    rrdgTimeSeries.Cells[Ord(tscLabel), Ord(tsrPestModifier)] := StrPestModifier;
    rrdgTimeSeries.Cells[Ord(tscLabel), Ord(tsrPestMethod)] := StrModificationMethod;
    rrdgTimeSeries.Cells[Ord(tscLabel), Ord(tsrScaleFactor)] := StrScaleFactorSFAC;
    rrdgTimeSeries.Cells[Ord(tscLabel), Ord(tsrInterpolation)] := StrInterpolationMethod;
    InitializeTimeRows;
  finally
    rrdgTimeSeries.EndUpdate
  end;
end;

procedure TframeModflow6TimeSeries.rrdgTimeSeriesBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  if (rrdgTimeSeries.Cells[ACol, ARow] = '')
    and (ARow in [Ord(tsrName), Ord(tsrPestMethod),
    Ord(tsrScaleFactor), Ord(tsrInterpolation)]) then
  begin
    if (ACol >= Ord(tscFirstSeries)) then
    begin
      rrdgTimeSeries.Canvas.Brush.Color := clRed;
    end
    else
    begin
      rrdgTimeSeries.Canvas.Brush.Color := clBtnFace;
    end;
  end;
  if (ARow in [Ord(tsrName), Ord(tsrPestModifier)])
    and (ACol = Ord(tscTimes)) then
  begin
    rrdgTimeSeries.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeModflow6TimeSeries.rrdgTimeSeriesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = 1) then
  begin
    if ARow in [Ord(tsrName)..Ord(tsrInterpolation)] then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeModflow6TimeSeries.SetData;
const
  NoValue = 3.0E30;
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  ASeries: TMf6TimeSeries;
  ColIndex: Integer;
begin
  FTimesSeriesGroup.GroupName := edGroupName.Text;
  FTimesSeriesGroup.Count := seTimeSeriesCount.AsInteger;
  FTimesSeriesGroup.TimeCount := seTimeCount.AsInteger;
  for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
  begin
    FTimesSeriesGroup.Times[TimeIndex].Value :=
      rrdgTimeSeries.RealValueDefault[
      Ord(tscTimes), TimeIndex + Ord(tsrFirstTime), NoValue];
  end;

  for SeriesIndex := 0 to FTimesSeriesGroup.Count - 1 do
  begin
    ColIndex := SeriesIndex + Ord(tscFirstSeries);
    ASeries := FTimesSeriesGroup[SeriesIndex].TimeSeries;
    ASeries.SeriesName := rrdgTimeSeries.Cells[ColIndex, Ord(tsrName)];
    if rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestModifier)] <= 0 then
    begin
      ASeries.ScaleFactorParameter := '';
    end
    else
    begin
      ASeries.ScaleFactorParameter :=
        rrdgTimeSeries.Cells[ColIndex,Ord(tsrPestModifier)]
    end;
    ASeries.ParamMethod :=
      TPestParamMethod(rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestMethod)]);
    ASeries.ScaleFactor :=
      rrdgTimeSeries.RealValueDefault[ColIndex, Ord(tsrScaleFactor), NoValue];
    ASeries.InterpolationMethod :=
      TMf6InterpolationMethods(rrdgTimeSeries.ItemIndex[
      ColIndex, Ord(tsrInterpolation)]);

    Assert(ASeries.Count = FTimesSeriesGroup.Times.Count);
    for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
    begin
      ASeries[TimeIndex].Value :=
        rrdgTimeSeries.RealValueDefault[
        ColIndex, TimeIndex + Ord(tsrFirstTime), NoValue];
    end;
  end;
end;

procedure TframeModflow6TimeSeries.seTimeCountChange(Sender: TObject);
begin
  rrdgTimeSeries.RowCount := seTimeCount.AsInteger + Ord(tsrFirstTime);
  InitializeTimeRows;
  btnDeleteTime.Enabled := seTimeCount.AsInteger > 1;
end;

procedure TframeModflow6TimeSeries.seTimeSeriesCountChange(Sender: TObject);
var
  ColIndex: Integer;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    for ColIndex := rrdgTimeSeries.ColCount - 1
      downto seTimeSeriesCount.AsInteger + 2 do
    begin
      rrdgTimeSeries.Objects[ColIndex, 0].Free;
      rrdgTimeSeries.Objects[ColIndex, 0] := nil;
    end;
    rrdgTimeSeries.ColCount := seTimeSeriesCount.AsInteger + 2;
    btnDeleteTimeSeries.Enabled := seTimeSeriesCount.AsInteger > 0;
    for ColIndex := Ord(tscFirstSeries) to rrdgTimeSeries.ColCount - 1 do
    begin
      if rrdgTimeSeries.Objects[ColIndex, 0] = nil then
      begin
        if FTimesSeriesGroup.Count > ColIndex - Ord(tscFirstSeries) then
        begin
          rrdgTimeSeries.Objects[ColIndex, 0] :=
            FTimesSeriesGroup[ColIndex - Ord(tscFirstSeries)]
        end
        else
        begin
          rrdgTimeSeries.Objects[ColIndex, 0] := FTimesSeriesGroup.Add;
        end;
      end;
      rrdgTimeSeries.Columns[ColIndex].AutoAdjustColWidths := True;
      if (rrdgTimeSeries.Cells[ColIndex, Ord(tsrPestModifier)] = '')
        and (rrdgTimeSeries.Rows[Ord(tsrPestModifier)].PickList.Count > 0) then
      begin
        rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestModifier)] := 0;
      end;
      if rrdgTimeSeries.Cells[ColIndex, Ord(tsrPestMethod)] = ''  then
      begin
        rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrPestMethod)] := 0;
      end;
      if rrdgTimeSeries.Cells[ColIndex, Ord(tsrScaleFactor)] = ''  then
      begin
        rrdgTimeSeries.Cells[ColIndex, Ord(tsrScaleFactor)] := '1';
      end;
      if rrdgTimeSeries.Cells[ColIndex, Ord(tsrInterpolation)] = ''  then
      begin
        rrdgTimeSeries.ItemIndex[ColIndex, Ord(tsrInterpolation)] := 0;
      end;
    end;
  finally
    rrdgTimeSeries.EndUpdate;
  end;
end;

end.
