unit frmFormatUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Buttons, ArgusDataEntry, Chart, Grids, DataGrid, ComCtrls, Series,
  TeEngine, Spin, ExtCtrls;

type
  Natural = 0..MaxInt;

  TfrmFormat = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    FontDialog1: TFontDialog;
    PageControl1: TPageControl;
    tabGeneral: TTabSheet;
    Label2: TLabel;
    memoTitle: TMemo;
    GroupBox1: TGroupBox;
    Label3: TLabel;
    Label1: TLabel;
    lblXMax: TLabel;
    Label6: TLabel;
    edXAxisTitle: TEdit;
    cbAutoX: TCheckBox;
    adeXMin: TArgusDataEntry;
    adeXMax: TArgusDataEntry;
    adeXTickInterval: TArgusDataEntry;
    cbXGrid: TCheckBox;
    Button2: TButton;
    Button3: TButton;
    cbXTick: TCheckBox;
    GroupBox2: TGroupBox;
    Label5: TLabel;
    lblYMin: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    edYAxisTitle: TEdit;
    cbAutoY: TCheckBox;
    adeYMin: TArgusDataEntry;
    adeYMax: TArgusDataEntry;
    adeYTickInterval: TArgusDataEntry;
    cbYGrid: TCheckBox;
    Button4: TButton;
    Button5: TButton;
    cbYTick: TCheckBox;
    Button1: TButton;
    cbLegend: TCheckBox;
    btnLegendFont: TButton;
    tabLineSeries: TTabSheet;
    dgLineSeries: TECDataGrid;
    ColorDialog1: TColorDialog;
    BitBtn3: TBitBtn;
    cbYLog: TCheckBox;
    cbXLog: TCheckBox;
    edXFormat: TEdit;
    Label8: TLabel;
    Label9: TLabel;
    edYFormat: TEdit;
    seLeft: TSpinEdit;
    Label10: TLabel;
    seRight: TSpinEdit;
    Label11: TLabel;
    seTop: TSpinEdit;
    Label12: TLabel;
    seBottom: TSpinEdit;
    Label13: TLabel;
    Panel1: TPanel;
    cbMultiSelect: TCheckBox;
    adeValue: TArgusDataEntry;
    procedure cbAutoXClick(Sender: TObject);
    procedure cbAutoYClick(Sender: TObject);
    procedure BitBtn2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button5Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure btnLegendFontClick(Sender: TObject);
    procedure dgLineSeriesSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: String);
    procedure dgLineSeriesSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure dgLineSeriesEditButtonClick(Sender: TObject);
    procedure dgLineSeriesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure cbMultiSelectClick(Sender: TObject);
    procedure adeValueChange(Sender: TObject);
    procedure dgLineSeriesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { Private declarations }
    Chart : TChart;
    XTickFont, YTickFont, LegendFont : TFont;
    OldValue : string;
    procedure SetData;
    function PickFont(AFont: TFont): TFont;
  public
    procedure GetData(AChart : TChart; HelpContext : THelpContext);
    { Public declarations }
  end;

  TMySeriesProperties = (mspTitle, mspVisible, mspWidth, mspLineColor, mspShowSymbol,
    mspSymbol, mspSymbolColor, mspSymbHorSize, mspSymbVertSize);

function InternationalStrToFloat(Value : string) : extended;
    
var
  frmFormat: TfrmFormat;

implementation

{$R *.DFM}

uses TypInfo;

function InternationalStrToFloat(Value : string) : extended;
var
  DecimalLocation : integer;
  CommaSeparator : array[0..255] of Char;
  Index : integer;
begin

  GetLocaleInfo(LOCALE_USER_DEFAULT, LOCALE_SDECIMAL,@CommaSeparator,255);
  if String(CommaSeparator) = '.' then
  begin
    DecimalLocation := Pos(',',Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := '.';
    end;
  end;
  if String(CommaSeparator) = ',' then
  begin
    DecimalLocation := Pos('.',Value);
    if DecimalLocation > 0 then
    begin
      Value[DecimalLocation] := ',';
    end;
  end;
  Value := Trim(Value);
  for Index := 2 to Length(Value) do
  begin
    if (Value[Index] = '+') or (Value[Index] = '-') then
    begin
      if (Value[Index-1] <> 'E') and (Value[Index-1] <> 'e') then
      begin
        Value := Copy(Value,1,Index-1) + 'E' + Copy(Value,Index, Length(Value));
        Break;
      end;
    end;
  end;

{  DecimalPosition := Pos(',',Value);
  if DecimalPosition > 0 then
  begin
    Value[DecimalPosition] := '.';
  end; }
  Result := StrToFloat(Value);
end;

function EnumName(Value: Natural; info : PTypeInfo) : string;
var
  Data: PTypeData;
begin
  Data := GetTypeData(Info);
  if (Value < Data^.MinValue) or (Value > Data^.MaxValue) then
    result := Format('%s(%d)', [Info^.Name,Value])
  else
{$ifdef Win32}
    Result := GetEnumName(Info,Value);
{$else}
    Result := GetEnumName(Info,Value)^;
{$endif}
end;

function EnumValue(TypeInfo: PTypeInfo; const Name: string): Integer;
begin
  result := GetEnumValue(TypeInfo, Name);
end;

procedure TfrmFormat.cbAutoXClick(Sender: TObject);
begin
  adeXMin.Enabled := not cbAutoX.Checked;
  adeXMax.Enabled := not cbAutoX.Checked;
end;

procedure TfrmFormat.cbAutoYClick(Sender: TObject);
begin
  adeYMin.Enabled := not cbAutoY.Checked;
  adeYMax.Enabled := not cbAutoY.Checked;

end;

procedure TfrmFormat.BitBtn2Click(Sender: TObject);
begin
  SetData;
end;

procedure TfrmFormat.GetData(AChart : TChart; HelpContext : THelpContext);
var
  Index : integer;
  ASeries : TChartSeries;
  ACustomSeries : TCustomSeries;
  LineSeriesIndex : integer;
//  AColor : TColor;
begin
  self.HelpContext := HelpContext;
  Chart := AChart;
  with AChart do
  begin
    memoTitle.Lines.Assign(Title.Text);
    edXAxisTitle.Text := BottomAxis.Title.Caption;
    edYAxisTitle.Text := LeftAxis.Title.Caption;

    memoTitle.Font := Title.Font;
    edXAxisTitle.Font := BottomAxis.Title.Font;
    edYAxisTitle.Font := LeftAxis.Title.Font;
    XTickFont.Assign(BottomAxis.LabelsFont);
    YTickFont.Assign(LeftAxis.LabelsFont);
    LegendFont.Assign(Legend.Font);

    cbAutoX.Checked := BottomAxis.Automatic;
    adeXMin.Text := FloatToStr(BottomAxis.Minimum);
    adeXMax.Text := FloatToStr(BottomAxis.Maximum);
    cbAutoY.Checked := LeftAxis.Automatic;
    adeYMin.Text := FloatToStr(LeftAxis.Minimum);
    adeYMax.Text := FloatToStr(LeftAxis.Maximum);

    adeXTickInterval.Text := FloatToStr(BottomAxis.Increment);
    adeYTickInterval.Text := FloatToStr(LeftAxis.Increment);

    cbXGrid.Checked := BottomAxis.Grid.Visible;
    cbYGrid.Checked := LeftAxis.Grid.Visible;

    cbXTick.Checked := BottomAxis.Ticks.Visible;
    cbYTick.Checked := LeftAxis.Ticks.Visible;

    cbXLog.Checked := BottomAxis.Logarithmic;
    cbYLog.Checked := LeftAxis.Logarithmic;

    cbLegend.Checked := Legend.Visible;
    cbLegend.Enabled := Legend.TotalLegendItems > 0;
    btnLegendFont.Enabled := cbLegend.Enabled;

    cbAutoXClick(cbAutoX);
    cbAutoYClick(cbAutoY);

    edXFormat.Text := BottomAxis.AxisValuesFormat;
    edYFormat.Text := LeftAxis.AxisValuesFormat  ;

    seLeft.Value   := MarginLeft   ;
    seRight.Value  := MarginRight  ;
    seTop.Value    := MarginTop    ;
    seBottom.Value := MarginBottom ;

    dgLineSeries.RowCount := SeriesCount + 1;

    LineSeriesIndex := 1;
    for Index := 0 to SeriesCount -1 do
    begin
      ASeries := Series[Index];
      if (ASeries is TCustomSeries) and (ASeries.YValues.Count > 0) then
      begin
        ACustomSeries := ASeries as TCustomSeries;

        dgLineSeries.Objects[0,LineSeriesIndex] := ACustomSeries;

        dgLineSeries.Cells[Ord(mspTitle),LineSeriesIndex] := ACustomSeries.Title;

        if ACustomSeries.Active then
        begin
          dgLineSeries.Cells[Ord(mspVisible),LineSeriesIndex]
            := dgLineSeries.Columns[Ord(mspVisible)].Picklist[1];
        end
        else
        begin
          dgLineSeries.Cells[Ord(mspVisible),LineSeriesIndex]
            := dgLineSeries.Columns[Ord(mspVisible)].Picklist[0];
        end;

        dgLineSeries.Cells[Ord(mspWidth),LineSeriesIndex]
          := IntToStr(ACustomSeries.LinePen.Width);

        dgLineSeries.Cells[Ord(mspLineColor),LineSeriesIndex]
          := IntToStr(ACustomSeries.SeriesColor);

        if ACustomSeries.Pointer.Visible then
        begin
          dgLineSeries.Cells[Ord(mspShowSymbol),LineSeriesIndex]
            := dgLineSeries.Columns[Ord(mspShowSymbol)].Picklist[1];
        end
        else
        begin
          dgLineSeries.Cells[Ord(mspShowSymbol),LineSeriesIndex]
            := dgLineSeries.Columns[Ord(mspShowSymbol)].Picklist[0];
        end;

        dgLineSeries.Cells[Ord(mspSymbol),LineSeriesIndex]
          := EnumName(Ord(ACustomSeries.Pointer.Style),
          TypeInfo(TSeriesPointerStyle));

        dgLineSeries.Cells[Ord(mspSymbolColor),LineSeriesIndex]
          := IntToStr(ACustomSeries.Pointer.Brush.Color);

        dgLineSeries.Cells[Ord(mspSymbHorSize),LineSeriesIndex]
          := IntToStr(ACustomSeries.Pointer.HorizSize);

        dgLineSeries.Cells[Ord(mspSymbVertSize),LineSeriesIndex]
          := IntToStr(ACustomSeries.Pointer.VertSize);

        Inc(LineSeriesIndex);
      end;
    end;

    dgLineSeries.RowCount := LineSeriesIndex;
    if dgLineSeries.RowCount > 1 then
    begin
      dgLineSeries.FixedRows := 1;
      tabLineSeries.TabVisible := True;
    end
    else
    begin
      tabLineSeries.TabVisible := False;
    end;
  end;
end;

procedure TfrmFormat.SetData;
var
  Index : integer;
  ACustomSeries : TCustomSeries;
  AColor : TColor;
begin
  with Chart do
  begin
    BottomAxis.Automatic            := cbAutoX.Checked ;
    BottomAxis.Minimum  := InternationalStrToFloat(adeXMin.Text);
    BottomAxis.Maximum  := InternationalStrToFloat(adeXMax.Text);
    LeftAxis.Automatic              := cbAutoY.Checked;
    LeftAxis.Minimum    := InternationalStrToFloat(adeYMin.Text);
    LeftAxis.Maximum    := InternationalStrToFloat(adeYMax.Text);

    Title.Text.Assign(memoTitle.Lines);
    BottomAxis.Title.Caption := edXAxisTitle.Text;
    LeftAxis.Title.Caption   := edYAxisTitle.Text;

    BottomAxis.Increment := InternationalStrToFloat(adeXTickInterval.Text);
    LeftAxis.Increment   := InternationalStrToFloat(adeYTickInterval.Text);

    BottomAxis.Grid.Visible := cbXGrid.Checked;
    LeftAxis.Grid.Visible   := cbYGrid.Checked;

    Title.Font.Assign(memoTitle.Font);
    BottomAxis.Title.Font.Assign(edXAxisTitle.Font);
    LeftAxis.Title.Font.Assign(edYAxisTitle.Font);
    BottomAxis.LabelsFont.Assign(XTickFont);
    LeftAxis.LabelsFont.Assign(YTickFont);
    Legend.Font.Assign(LegendFont);

    BottomAxis.Ticks.Visible := cbXTick.Checked;
    LeftAxis.Ticks.Visible := cbYTick.Checked;
    BottomAxis.TicksInner.Visible := cbXTick.Checked;
    LeftAxis.TicksInner.Visible := cbYTick.Checked;
    TopAxis.Ticks.Visible := cbXTick.Checked;
    RightAxis.Ticks.Visible := cbYTick.Checked;
    TopAxis.TicksInner.Visible := cbXTick.Checked;
    RightAxis.TicksInner.Visible := cbYTick.Checked;

    BottomAxis.Logarithmic :=  cbXLog.Checked ;
    LeftAxis.Logarithmic   :=  cbYLog.Checked ;

    BottomAxis.AxisValuesFormat := edXFormat.Text ;
    LeftAxis.AxisValuesFormat   := edYFormat.Text ;

    MarginLeft   := seLeft.Value   ;
    MarginRight  := seRight.Value  ;
    MarginTop    := seTop.Value    ;
    MarginBottom := seBottom.Value ;

    Legend.Visible := cbLegend.Checked;

    for Index := 1 to dgLineSeries.RowCount -1 do
    begin
      ACustomSeries := dgLineSeries.Objects[0,Index] as TCustomSeries;
      try
        ACustomSeries.BeginUpdate;


        ACustomSeries.Title := dgLineSeries.Cells[Ord(mspTitle),Index];

        ACustomSeries.Active :=
          (dgLineSeries.Cells[Ord(mspVisible),Index]
          = dgLineSeries.Columns[Ord(mspVisible)].Picklist[1]);

        ACustomSeries.LinePen.Width
          := Round(InternationalStrToFloat(dgLineSeries.Cells[Ord(mspWidth),Index]));

        AColor := StrToInt(dgLineSeries.Cells[Ord(mspLineColor),Index]);
        if AColor <> ACustomSeries.SeriesColor then
        begin
          ACustomSeries.SeriesColor := AColor;
        end;

        ACustomSeries.Pointer.Visible :=
          (dgLineSeries.Cells[Ord(mspShowSymbol),Index]
          = dgLineSeries.Columns[Ord(mspShowSymbol)].Picklist[1]);

        ACustomSeries.Pointer.Style :=
          TSeriesPointerStyle(EnumValue(TypeInfo(TSeriesPointerStyle),
          dgLineSeries.Cells[Ord(mspSymbol),Index]));

        AColor := StrToInt(dgLineSeries.Cells[Ord(mspSymbolColor),Index]);
        if AColor <> ACustomSeries.Pointer.Brush.Color then
        begin
          ACustomSeries.Pointer.Brush.Color := AColor
        end;

        ACustomSeries.Pointer.HorizSize
          := StrToInt(dgLineSeries.Cells[Ord(mspSymbHorSize),Index]);

        ACustomSeries.Pointer.VertSize
          := StrToInt(dgLineSeries.Cells[Ord(mspSymbVertSize),Index]);

      finally
        ACustomSeries.EndUpdate;
      end;

    end;
  end;
end;


procedure TfrmFormat.FormCreate(Sender: TObject);
var
  AStyle : TSeriesPointerStyle;
begin
  XTickFont := TFont.Create;
  YTickFont := TFont.Create;
  LegendFont := TFont.Create;
  for AStyle := Low(TSeriesPointerStyle) to High(TSeriesPointerStyle) do
  begin
    dgLineSeries.Columns[Ord(mspSymbol)].PickList.Add(EnumName(Ord(AStyle),
      TypeInfo(TSeriesPointerStyle)));
  end;

end;

procedure TfrmFormat.FormDestroy(Sender: TObject);
begin
  XTickFont.Free;
  YTickFont.Free;
  LegendFont.Free;
end;

function TfrmFormat.PickFont(AFont : TFont) : TFont;
begin
  FontDialog1.Font := AFont;
  if FontDialog1.Execute then
  begin
    result := FontDialog1.Font
  end
  else
  begin
    result := AFont;
  end;
end;

procedure TfrmFormat.Button1Click(Sender: TObject);
begin
  memoTitle.Font.Assign(PickFont(memoTitle.Font));
end;

procedure TfrmFormat.Button2Click(Sender: TObject);
begin
  edXAxisTitle.Font.Assign(PickFont(edXAxisTitle.Font));
end;

procedure TfrmFormat.Button5Click(Sender: TObject);
begin
  edYAxisTitle.Font.Assign(PickFont(edYAxisTitle.Font));
end;

procedure TfrmFormat.Button3Click(Sender: TObject);
begin
  XTickFont.Assign(PickFont(XTickFont));
end;

procedure TfrmFormat.Button4Click(Sender: TObject);
begin
  YTickFont.Assign(PickFont(YTickFont));
end;

procedure TfrmFormat.btnLegendFontClick(Sender: TObject);
begin
  LegendFont.Assign(PickFont(LegendFont));
end;

procedure TfrmFormat.dgLineSeriesSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: String);
var
  AnInteger : integer;
begin
  if (ACol >= Ord(mspSymbHorSize)) and (ARow > 0) then
  begin
    if Value <> '' then
    begin
      try
        AnInteger := StrToInt(Value);
        if AnInteger < 0 then
        begin
          Beep;
          dgLineSeries.Cells[ACol,ARow] := OldValue;
        end;
      except on EConvertError do
        begin
          Beep;
          dgLineSeries.Cells[ACol,ARow] := OldValue;
        end;
      end;
    end;

  end;
  if (ACol = Ord(mspLineColor)) and (ARow > 0) then
  begin
    dgLineSeries.Invalidate;
  end;
end;

procedure TfrmFormat.dgLineSeriesSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
var
  FirstColumn : integer;
  ColumnFormat : TColumnFormat;
begin
  OldValue := dgLineSeries.Cells[ACol,ARow];
  if cbMultiSelect.Checked and (dgLineSeries.Selection.Left = ACol)
    and (dgLineSeries.Selection.Top = ARow) then
  begin
    FirstColumn := dgLineSeries.Selection.Left;
    ColumnFormat := dgLineSeries.Columns[FirstColumn].Format;
    case ColumnFormat of
      cfNumber:
        begin
          adeValue.DataType := dtInteger;
        end;
      cfString:
        begin
          adeValue.DataType := dtString;
        end;
      else Assert(False);
    end;
    if (FirstColumn = 1) or (FirstColumn = 4) or (FirstColumn = 5) then
    begin
      adeValue.Style := csDropDownList;
      adeValue.Items.Clear;
      adeValue.Items.AddStrings(dgLineSeries.Columns[FirstColumn].PickList);
      adeValue.ItemIndex := adeValue.Items.IndexOf(dgLineSeries.Cells[dgLineSeries.Selection.Left,
        dgLineSeries.Selection.Top]);
    end
    else begin
      adeValue.Style := csSimple;
      adeValue.Items.Clear;
    end;
    adeValue.Text := dgLineSeries.Cells[dgLineSeries.Selection.Left,
      dgLineSeries.Selection.Top];
  end;
end;

procedure TfrmFormat.dgLineSeriesEditButtonClick(Sender: TObject);
var
  AColor : TColor;
begin
  AColor := StrToInt(dgLineSeries.Cells[dgLineSeries.Col,dgLineSeries.Row]);
  ColorDialog1.Color := AColor;
  if ColorDialog1.Execute then
  begin
    dgLineSeries.Cells[dgLineSeries.Col,dgLineSeries.Row]
      := IntToStr(ColorDialog1.Color);
  end;
end;

procedure TfrmFormat.dgLineSeriesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  AColor : TColor;
begin
  if (ARow > 0) and ((ACol = Ord(mspLineColor))
    or (ACol = Ord(mspSymbolColor))) then
  begin
    AColor := StrToInt(dgLineSeries.Cells[ACol,ARow]);
    if (ACol = Ord(mspSymbolColor)) and (AColor =clTeeColor) then
    begin
      AColor := StrToInt(dgLineSeries.Cells[Ord(mspLineColor),ARow])
    end;
    dgLineSeries.Canvas.Brush.Color := AColor;
    dgLineSeries.Canvas.TextRect(Rect,Rect.Left+2,Rect.Top+2,'');
  end;
end;

procedure TfrmFormat.cbMultiSelectClick(Sender: TObject);
begin
  adeValue.Enabled := cbMultiSelect.Checked;
  if cbMultiSelect.Checked then
  begin
    dgLineSeries.Options := dgLineSeries.Options
      + [goRangeSelect,goAlwaysShowEditor] - [goEditing];
  end
  else
  begin
    dgLineSeries.Options := dgLineSeries.Options
      - [goRangeSelect,goAlwaysShowEditor] + [goEditing];
  end;
end;

procedure TfrmFormat.adeValueChange(Sender: TObject);
var
  ColIndex, RowIndex : integer;
begin
  if not (csLoading in ComponentState)
    and not (csReading in ComponentState) then
  begin
    if ActiveControl = adeValue then
    begin
      ColIndex := dgLineSeries.Selection.Left;
      for RowIndex := dgLineSeries.Selection.Top to dgLineSeries.Selection.Bottom do
      begin
        dgLineSeries.Cells[ColIndex,RowIndex] := adeValue.Text;
      end;
    end;
  end;

end;

procedure TfrmFormat.dgLineSeriesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol, ARow : integer;
  CanSelect: Boolean;
begin
  dgLineSeries.MouseToCell( X, Y, ACol, ARow);
  dgLineSeries.Col := ACol;
  dgLineSeries.Row := ARow;
  CanSelect := True;
  dgLineSeriesSelectCell(dgLineSeries, ACol, ARow, CanSelect);
end;

end.
