unit frameModflow6TimeSeriesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.Grids, RbwDataGrid4,
  Vcl.StdCtrls, Vcl.Mask, JvExMask, JvSpin, Vcl.ExtCtrls,
  Math, GoPhastTypes,
  Modflow6TimeSeriesCollectionsInterfaceUnit, Modflow6TimeSeriesInterfaceUnit;

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
    btnCopyColumn: TButton;
    procedure rrdgTimeSeriesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seTimeSeriesCountChange(Sender: TObject);
    procedure seTimeCountChange(Sender: TObject);
    procedure btnDeleteTimeSeriesClick(Sender: TObject);
    procedure btnDeleteTimeClick(Sender: TObject);
    procedure rrdgTimeSeriesBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure btnInsertTimeClick(Sender: TObject);
    procedure btnCopyColumnClick(Sender: TObject);
  private
    // @name contains @link(FTimesSeriesGroup)
    FTimesSeriesGroupItem: ITimeSeriesCollectionItem;
    FTimesSeriesGroup: ITimesSeriesCollection;
    procedure InitializeTimeRows;
    { Private declarations }
  public
    procedure InitializeGrid;
    procedure GetData(ATimesSeriesGroupItem: ITimeSeriesCollectionItem; PestNames: TStringList);
    procedure SetData;
    property GroupItem: ITimeSeriesCollectionItem read FTimesSeriesGroupItem;
    { Public declarations }
  end;

resourcestring
  StrName = 'Name';
  StrTimes = 'Times';
  StrScaleFactorSFAC = 'Scale Factor (SFAC)';
  StrInterpolationMethod = 'Interpolation Method (METHOD)';

implementation

uses
  Vcl.Clipbrd;

{$R *.dfm}

type
  TTimeSeriesRows = (tsrName, tsrPestModifier, tsrPestMethod, tsrScaleFactor,
    tsrInterpolation, tsrFirstTime);
  TTimeSeriesColumns = (tscLabel, tscTimes, tscFirstSeries);

procedure TframeModflow6TimeSeries.btnCopyColumnClick(Sender: TObject);
var
  Lines: TStringList;
  CanSelect: Boolean;
begin
  if rrdgTimeSeries.SelectedCol >= 0 then
  begin
    Lines := TStringList.Create;
    try
      for var RowIndex := 0 to rrdgTimeSeries.RowCount - 1 do
      begin
        CanSelect := True;
        rrdgTimeSeriesSelectCell(rrdgTimeSeries, rrdgTimeSeries.SelectedCol, RowIndex, CanSelect);
        if CanSelect then
        begin
          Lines.Add(rrdgTimeSeries.Cells[rrdgTimeSeries.SelectedCol, RowIndex]);
        end;
      end;
      if Lines.Count > 0 then
      begin
        ClipBoard.AsText := Lines.Text;
      end;
    finally
      Lines.Free;
    end;
  end;
end;

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
var
  TimeSeries: IMf6TimeSeries;
  ACollection: TPhastCollection;
begin
  if rrdgTimeSeries.Col >= 2 then
  begin
    ACollection := rrdgTimeSeries.Objects[rrdgTimeSeries.Col, 0] as TPhastCollection;
    if ACollection <> nil then
    begin
      if ACollection.QueryInterface(IMf6TimeSeries, TimeSeries) <> 0 then
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
  ATimesSeriesGroupItem: ITimeSeriesCollectionItem; PestNames: TStringList);
var
  TimeIndex: Integer;
  SeriesIndex: Integer;
  ASeries: IMf6TimeSeries;
  ColIndex: Integer;
  ASeriesItem: ITimeSeriesItem;
  SeriesCount: Integer;
  ASeriesI: ITimeSeries;
begin
  FTimesSeriesGroupItem := ATimesSeriesGroupItem;
  FTimesSeriesGroup := ATimesSeriesGroupItem.TimesSeriesCollectionI;
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
      if ASeriesI.QueryInterface(IMf6TimeSeries, ASeries) <> 0 then
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
        rrdgTimeSeries.RealValue[ColIndex, TimeIndex + Ord(tsrFirstTime)]
          := ASeries.Values[TimeIndex];
      end;
      Inc(ColIndex);
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
  ASeries: IMf6TimeSeries;
  ACollection: TPhastCollection;
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
    ACollection := rrdgTimeSeries.Objects[SeriesIndex, 0] as TPhastCollection;
    if ACollection <> nil then
    begin
      if ACollection.QueryInterface(IMf6TimeSeries, ASeries) <> 0 then
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
      ASeries.Values[TimeIndex] :=
        rrdgTimeSeries.RealValueDefault[
        SeriesIndex, TimeIndex + Ord(tsrFirstTime), NoValue];
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
  TimeSeries: IMf6TimeSeries;
  ASeries: IMf6TimeSeries;
  SeriesToUse: IMf6TimeSeries;
  ColToUse: Integer;
  SeriesIndex: Integer;
  ACollection: TPhastCollection;
  ASeriesI: ITimeSeries;
begin
  rrdgTimeSeries.BeginUpdate;
  try
    for ColIndex := rrdgTimeSeries.ColCount - 1
      downto seTimeSeriesCount.AsInteger + 2 do
    begin
      ACollection  := rrdgTimeSeries.Objects[ColIndex, 0] as TPhastCollection;
      if ACollection <> nil then
      begin
        if ACollection.QueryInterface(IMf6TimeSeries,TimeSeries) <> 0  then
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
        if ASeriesI.QueryInterface(IMf6TimeSeries, ASeries) <> 0 then
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
