unit frameModflow6DynamicTimeSeriesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, Vcl.ExtCtrls,
  Modflow6DynamicTimeSeriesInterfaceUnit, GoPhastTypes,
  Modflow6TimeSeriesCollectionsInterfaceUnit, System.Math,
  Modflow6TimeSeriesInterfaceUnit;

type
  TframeModflow6DynamicTimeSeries = class(TFrame)
    pnlBottom: TPanel;
    lblTimeSeriesCount: TLabel;
    lblTimeCount: TLabel;
    seTimeSeriesCount: TJvSpinEdit;
    seTimeCount: TJvSpinEdit;
    btnDeleteTimeSeries: TButton;
    btnDeleteTime: TButton;
    btnInsertTime: TButton;
    pnlTop: TPanel;
    edGroupName: TEdit;
    rrdgTimeSeries: TRbwRowDataGrid;
    procedure rrdgTimeSeriesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seTimeSeriesCountChange(Sender: TObject);
    procedure btnDeleteTimeSeriesClick(Sender: TObject);
    procedure btnDeleteTimeClick(Sender: TObject);
    procedure seTimeCountChange(Sender: TObject);
    procedure rrdgTimeSeriesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnInsertTimeClick(Sender: TObject);
    procedure rrdgTimeSeriesEndUpdate(Sender: TObject);
  private
//    FTimesSeriesGroupItem: IDynamicTimeSeriesItem;
    FTimesSeriesGroup: IDyanmicTimesSeriesCollection;
    procedure InitializeTimeRows;
    { Private declarations }
  public
    procedure InitializeGrid;
    procedure GetData(ATimesSeriesGroup: IDyanmicTimesSeriesCollection; PestNames: TStringList);
    procedure SetData;
//    property GroupItem: IDynamicTimeSeriesItem read FTimesSeriesGroupItem;
    { Public declarations }
  end;

implementation

uses
  frameModflow6TimeSeriesUnit, OrderedCollectionUnit;

{$R *.dfm}

type
  TTimeSeriesRows = (tsrName, tsrPestModifier, tsrPestMethod, tsrScaleFactor,
    tsrInterpolation, tsrFirstTime);
  TTimeSeriesColumns = (tscLabel, tscTimes, tscFirstSeries);

{ TframeModflow6DynamicTimeSeries }

procedure TframeModflow6DynamicTimeSeries.btnDeleteTimeClick(Sender: TObject);
begin
  if (rrdgTimeSeries.Row >= Ord(tsrFirstTime))
    and (seTimeCount.AsInteger > 1) then
  begin
    rrdgTimeSeries.DeleteRow(rrdgTimeSeries.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger -1;
    seTimeCountChange(nil);
  end;
end;

procedure TframeModflow6DynamicTimeSeries.btnDeleteTimeSeriesClick(
  Sender: TObject);
var
  TimeSeries: IDynamicTimeSeries;
  ACollection: TOrderedCollection;
begin
  if rrdgTimeSeries.Col >= 2 then
  begin
    ACollection := rrdgTimeSeries.Objects[rrdgTimeSeries.Col, 0] as TOrderedCollection;
    if ACollection <> nil then
    begin
      if ACollection.QueryInterface(IDynamicTimeSeries, TimeSeries) <> 0 then
      begin
        Assert(False);
      end;
    end
    else
    begin
      TimeSeries := nil;
    end;
    if TimeSeries <> nil then
    begin
      TimeSeries.Deleted := True;
    end;
    rrdgTimeSeries.Objects[rrdgTimeSeries.Col, 0] := nil;
    rrdgTimeSeries.DeleteColumn(rrdgTimeSeries.Col);
    seTimeSeriesCount.AsInteger := seTimeSeriesCount.AsInteger -1;
    seTimeSeriesCountChange(nil);
  end;
end;

procedure TframeModflow6DynamicTimeSeries.btnInsertTimeClick(Sender: TObject);
begin
  if (rrdgTimeSeries.Row >= Ord(tsrFirstTime)) then
  begin
    rrdgTimeSeries.InsertRow(rrdgTimeSeries.Row);
    seTimeCount.AsInteger := seTimeCount.AsInteger +1;
    seTimeCountChange(nil);
  end;
end;

procedure TframeModflow6DynamicTimeSeries.GetData(
  ATimesSeriesGroup: IDyanmicTimesSeriesCollection; PestNames: TStringList);
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  ASeries: IDynamicTimeSeries;
  ColIndex: Integer;
  ASeriesItem: ITimeSeriesItem;
  SeriesCount: Integer;
  ASeriesI: ITimeSeries;
begin
//  FTimesSeriesGroupItem := ATimesSeriesGroupItem;
  FTimesSeriesGroup := ATimesSeriesGroup;

  edGroupName.Text := String(FTimesSeriesGroup.GroupName);
  rrdgTimeSeries.BeginUpdate;
  try
    rrdgTimeSeries.Rows[Ord(tsrPestModifier)].PickList := PestNames;
    SeriesCount := 0;
    for SeriesIndex := 0 to FTimesSeriesGroup.Count - 1 do
    begin
      ASeriesItem := FTimesSeriesGroup.ItemsI[SeriesIndex];
      if ASeriesItem.TimeSeriesI.Deleted then
      begin
        Continue;
      end;
      Inc(SeriesCount);
    end;

    seTimeSeriesCount.AsInteger := SeriesCount;
    seTimeCount.AsInteger := Max(1,FTimesSeriesGroup.Times.Count);
    InitializeGrid;
    for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
    begin
      rrdgTimeSeries.RealValue[Ord(tscTimes), TimeIndex + Ord(tsrFirstTime)]
        := FTimesSeriesGroup.Times[TimeIndex].Value;
    end;

    ColIndex := Ord(tscFirstSeries);
    for SeriesIndex := 0 to FTimesSeriesGroup.Count - 1 do
    begin
//      ColIndex := SeriesIndex + Ord(tscFirstSeries);
      ASeriesItem := FTimesSeriesGroup.ItemsI[SeriesIndex];
      if ASeriesItem.TimeSeriesI.Deleted then
      begin
        Continue;
      end;
      rrdgTimeSeries.Objects[ColIndex, 0] := ASeriesItem.TimeSeriesI as TObject;
      ASeriesI := ASeriesItem.TimeSeriesI;
      if ASeriesI.QueryInterface(IDynamicTimeSeries, ASeries) <> 0 then
      begin
        Assert(False);
      end;
      rrdgTimeSeries.Columns[ColIndex].
        AutoAdjustColWidths := True;
      rrdgTimeSeries.Cells[ColIndex, Ord(tsrName)] := String(ASeries.SeriesName);
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
        rrdgTimeSeries.Cells[ColIndex, TimeIndex + Ord(tsrFirstTime)]
          := ASeries.Items[TimeIndex].Value;
      end;
      Inc(ColIndex);
    end;
  finally
    rrdgTimeSeries.EndUpdate;
  end;
end;

procedure TframeModflow6DynamicTimeSeries.InitializeGrid;
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

procedure TframeModflow6DynamicTimeSeries.InitializeTimeRows;
var
  TimeIndex: Integer;
  RowItem: TRbwRow;
  Row: Integer;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    for TimeIndex := 1 to seTimeCount.AsInteger do
    begin
      Row := TimeIndex-1 + Ord(tsrFirstTime);
      rrdgTimeSeries.Cells[Ord(tscLabel), Row] := IntToStr(TimeIndex);

      RowItem  := rrdgTimeSeries.Rows[Row];
      RowItem.Format := rcf4String;
      RowItem.ButtonUsed := True;
      RowItem.ButtonCaption := 'F()';
      RowItem.ButtonWidth := 35;

      rrdgTimeSeries.UseSpecialFormat[Ord(tscTimes),Row] := True;
      rrdgTimeSeries.SpecialFormat[Ord(tscTimes),Row] := rcf4Real;
      rrdgTimeSeries.SpecialButtonAllowed[Ord(tscTimes),Row] := False;
    end;
  finally
    rrdgTimeSeries.EndUpdate
  end;
end;

procedure TframeModflow6DynamicTimeSeries.rrdgTimeSeriesBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  FirstTime: Double;
  SecondTime: double;
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
  if (ACol = Ord(tscTimes)) and (ARow >= Ord(tsrFirstTime) + 1) then
  begin
    if TryStrToFloat(rrdgTimeSeries.Cells[ACol, ARow-1], FirstTime)
      and TryStrToFloat(rrdgTimeSeries.Cells[ACol, ARow-1], SecondTime) then
    begin
      if FirstTime > SecondTime then
      begin
        rrdgTimeSeries.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
  if (ARow in [Ord(tsrName), Ord(tsrPestModifier)])
    and (ACol = Ord(tscTimes)) then
  begin
    rrdgTimeSeries.Canvas.Brush.Color := clBtnFace;
  end;
end;

procedure TframeModflow6DynamicTimeSeries.rrdgTimeSeriesEndUpdate(Sender:
    TObject);
begin
  seTimeCount.AsInteger := rrdgTimeSeries.RowCount - Ord(tsrFirstTime);
  seTimeSeriesCount.AsInteger := rrdgTimeSeries.ColCount - Ord(tscFirstSeries);
end;

procedure TframeModflow6DynamicTimeSeries.rrdgTimeSeriesSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
begin
  if (ACol = 1) then
  begin
    if ARow in [Ord(tsrName)..Ord(tsrInterpolation)] then
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeModflow6DynamicTimeSeries.SetData;
const
  NoValue = 3.0E30;
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  ASeries: IDynamicTimeSeries;
  ACollection: TOrderedCollection;
begin
  FTimesSeriesGroup.GroupName := AnsiString(edGroupName.Text);
  FTimesSeriesGroup.TimeCount := seTimeCount.AsInteger;
  for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
  begin
    FTimesSeriesGroup.Times[TimeIndex].Value :=
      rrdgTimeSeries.RealValueDefault[
      Ord(tscTimes), TimeIndex + Ord(tsrFirstTime), NoValue];
  end;

  for SeriesIndex := Ord(tscFirstSeries) to rrdgTimeSeries.ColCount - 1 do
  begin
    ACollection := rrdgTimeSeries.Objects[SeriesIndex, 0] as TOrderedCollection;
    if ACollection <> nil then
    begin
      if ACollection.QueryInterface(IDynamicTimeSeries, ASeries) <> 0 then
      begin
        Assert(False);
      end;
    end
    else
    begin
      ASeries := nil;
    end;
    ASeries.SeriesName := AnsiString(rrdgTimeSeries.Cells[SeriesIndex, Ord(tsrName)]);
    if rrdgTimeSeries.ItemIndex[SeriesIndex, Ord(tsrPestModifier)] <= 0 then
    begin
      ASeries.ScaleFactorParameter := '';
    end
    else
    begin
      ASeries.ScaleFactorParameter :=
        rrdgTimeSeries.Cells[SeriesIndex,Ord(tsrPestModifier)]
    end;
    ASeries.ParamMethod :=
      TPestParamMethod(rrdgTimeSeries.ItemIndex[SeriesIndex, Ord(tsrPestMethod)]);
    ASeries.ScaleFactor :=
      rrdgTimeSeries.RealValueDefault[SeriesIndex, Ord(tsrScaleFactor), 1];
    ASeries.InterpolationMethod :=
      TMf6InterpolationMethods(rrdgTimeSeries.ItemIndex[
      SeriesIndex, Ord(tsrInterpolation)]);

    Assert(ASeries.Count = FTimesSeriesGroup.Times.Count);
    for TimeIndex := 0 to FTimesSeriesGroup.Times.Count - 1 do
    begin
      ASeries.Items[TimeIndex].Value :=
        rrdgTimeSeries.Cells[
        SeriesIndex, TimeIndex + Ord(tsrFirstTime)];
    end;
  end;
end;

procedure TframeModflow6DynamicTimeSeries.seTimeCountChange(Sender: TObject);
begin
  rrdgTimeSeries.RowCount := seTimeCount.AsInteger + Ord(tsrFirstTime);
  InitializeTimeRows;
  btnDeleteTime.Enabled := seTimeCount.AsInteger > 1;
end;

procedure TframeModflow6DynamicTimeSeries.seTimeSeriesCountChange(
  Sender: TObject);
var
  ColIndex: Integer;
  TimeSeries: IDynamicTimeSeries;
  ASeries: IDynamicTimeSeries;
  SeriesToUse: IDynamicTimeSeries;
  ColToUse: Integer;
  SeriesIndex: Integer;
  ACollection: TOrderedCollection;
  ASeriesI: ITimeSeries;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    for ColIndex := rrdgTimeSeries.ColCount - 1
      downto seTimeSeriesCount.AsInteger + 2 do
    begin
      ACollection  := rrdgTimeSeries.Objects[ColIndex, 0] as TOrderedCollection;
      if ACollection <> nil then
      begin
        if ACollection.QueryInterface(IDynamicTimeSeries,TimeSeries) <> 0  then
        begin
          Assert(False);
        end;
        TimeSeries.Deleted := True;
      end
      else
      begin
        TimeSeries := nil;
      end;
      rrdgTimeSeries.Objects[ColIndex, 0] := nil;
    end;
    rrdgTimeSeries.ColCount := seTimeSeriesCount.AsInteger + 2;
    btnDeleteTimeSeries.Enabled := seTimeSeriesCount.AsInteger > 0;
    for ColIndex := Ord(tscFirstSeries) to rrdgTimeSeries.ColCount - 1 do
    begin
      SeriesToUse := nil;
      ColToUse := Ord(tscFirstSeries)-1;
      for SeriesIndex := 0 to FTimesSeriesGroup.Count - 1 do
      begin
        ASeriesI := FTimesSeriesGroup.ItemsI[SeriesIndex].TimeSeriesI;
        if ASeriesI.QueryInterface(IDynamicTimeSeries, ASeries) <> 0 then
        begin
          Assert(False);
        end;
//        ASeries := FTimesSeriesGroup.ItemsI[SeriesIndex].TimeSeriesI;
        if ASeries.Deleted then
        begin
          Continue;
        end;
        Inc(ColToUse);
        if ColToUse = ColIndex then
        begin
          SeriesToUse := ASeries;
          break;
        end;
      end;
      if SeriesToUse = nil then
      begin
        rrdgTimeSeries.Objects[ColIndex, 0] := FTimesSeriesGroup.AddI.TimeSeriesI as TObject;
      end
      else
      begin
        rrdgTimeSeries.Objects[ColIndex, 0] := SeriesToUse as TObject;
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
