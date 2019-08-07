unit frmSpecifyContoursUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, DataSetUnit, StdCtrls, Buttons, ExtCtrls, Mask,
  JvExMask, JvSpin, Grids, RbwDataGrid4, JvDialogs, JvExStdCtrls, JvCheckBox,
  ArgusDataEntry, ColorSchemes;

type
  TfrmSpecifyContours = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    rdgContourData: TRbwDataGrid4;
    GridPanel1: TGridPanel;
    sbAddRow: TSpeedButton;
    sbInsertRow: TSpeedButton;
    sbDeleteRow: TSpeedButton;
    seRowCount: TJvSpinEdit;
    lblRowCount: TLabel;
    clrDlg: TJvColorDialog;
    cbAutomaticColors: TJvCheckBox;
    Panel1: TPanel;
    rdeLineThickness: TRbwDataEntry;
    procedure FormCreate(Sender: TObject); override;
    procedure rdgContourDataBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure rdgContourDataButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure seRowCountChange(Sender: TObject);
    procedure rdgContourDataSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgContourDataEndUpdate(Sender: TObject);
    procedure sbAddRowClick(Sender: TObject);
    procedure sbInsertRowClick(Sender: TObject);
    procedure sbDeleteRowClick(Sender: TObject);
    procedure cbAutomaticColorsClick(Sender: TObject);
    procedure rdgContourDataColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgContourDataHorizontalScroll(Sender: TObject);
    procedure rdeLineThicknessChange(Sender: TObject);
    procedure rdgContourDataMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormDestroy(Sender: TObject); override;
  private
    FContourColors: TColorParameters;
    FLogTransform: Boolean;
    procedure UpdateRowCount;
    procedure ArrangeControls;
    procedure FillThicknessCells;
    procedure NumberGrid;
    { Private declarations }
  public
    procedure GetData(Contours: TContours; ColorScheme, ColorCycles: integer;
      ColorExponent: real; LogTransform: boolean);
    procedure SetData(var Contours: TContours);
    { Public declarations }
  end;


implementation

uses GR32, ContourUnit, Math;

resourcestring
  StrN = 'N';
  StrContourValue = 'Contour value';
  StrLineThicknessPixe = 'Line thickness (pixels)';
  StrContourColor = 'Contour color';

{$R *.dfm}

type
  TContourColumns = (ccN, ccValue, ccThickness, ccColor);

procedure TfrmSpecifyContours.ArrangeControls;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;  
  LayoutControls(rdgContourData, rdeLineThickness, nil, Ord(ccThickness), 0);
end;

procedure TfrmSpecifyContours.cbAutomaticColorsClick(Sender: TObject);
begin
  inherited;
  rdgContourData.Invalidate;
end;

procedure TfrmSpecifyContours.FormCreate(Sender: TObject);
begin
  inherited;
  FContourColors := TColorParameters.Create;
  rdgContourData.Cells[Ord(ccN),0] := StrN;
  rdgContourData.Cells[Ord(ccValue),0] := StrContourValue;
  rdgContourData.Cells[Ord(ccThickness),0] := StrLineThicknessPixe;
  rdgContourData.Cells[Ord(ccColor),0] := StrContourColor;
end;

procedure TfrmSpecifyContours.FormDestroy(Sender: TObject);
begin
  inherited;
  FContourColors.Free;
end;

procedure TfrmSpecifyContours.NumberGrid;
var
  Index: Integer;
begin
  for Index := 1 to rdgContourData.RowCount - 1 do
  begin
    rdgContourData.Cells[Ord(ccN),Index] := IntToStr(Index);
  end;
end;

procedure TfrmSpecifyContours.GetData(Contours: TContours; ColorScheme,
  ColorCycles: integer; ColorExponent: real; LogTransform: boolean);
var
  Index: Integer;
  Row: Integer;
begin
  FContourColors.ColorCycles := ColorCycles;
  FContourColors.ColorScheme := ColorScheme;
  FContourColors.ColorExponent := ColorExponent;
  FLogTransform := LogTransform;
  rdeLineThickness.Text := IntToStr(DefaultLineThickness);
  if Contours = nil then
  begin
    seRowCount.AsInteger := 0;
    cbAutomaticColors.Checked := True;
  end
  else
  begin
    seRowCount.AsInteger := Length(Contours.ContourValues);
    cbAutomaticColors.Checked := Contours.AutomaticColors;
    rdgContourData.BeginUpdate;
    try
      for Index := 0 to Length(Contours.ContourValues) - 1 do
      begin
        Row := Index + 1;
        rdgContourData.Cells[Ord(ccN),Row] := IntToStr(Row);
        if FLogTransform then
        begin
          rdgContourData.Cells[Ord(ccValue),Row] :=
            FloatToSTr(Power(10.,Contours.ContourValues[Index]));
        end
        else
        begin
          rdgContourData.Cells[Ord(ccValue),Row] :=
            FloatToSTr(Contours.ContourValues[Index]);
        end;
        rdgContourData.Cells[Ord(ccThickness),Row] :=
          FloatToSTr(Contours.LineThicknesses[Index]);
        rdgContourData.Cells[Ord(ccColor),Row] :=
          IntToHex(WinColor(Contours.ContourColors[Index]),6);
      end;
    finally
      rdgContourData.EndUpdate;
    end;
  end;
  ArrangeControls;
end;

procedure TfrmSpecifyContours.rdeLineThicknessChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(rdgContourData, Ord(ccThickness),
    rdeLineThickness.Text);
end;

procedure TfrmSpecifyContours.rdgContourDataBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  Value: Integer;
  AColor: TColor;
  MaxValue: double;
  MinValue: double;
  ContourValue: double;
  LogMax: Double;
  LogMin: Double;
  LogValue: Double;
begin
  inherited;
  if (ACol = Ord(ccColor)) then
  begin
    if cbAutomaticColors.Checked then
    begin
      if TryStrToFloat(rdgContourData.Cells[Ord(ccValue),1], MinValue)
        and TryStrToFloat(rdgContourData.Cells[Ord(ccValue),
        rdgContourData.RowCount-1], MaxValue)
        and TryStrToFloat(rdgContourData.Cells[Ord(ccValue),ARow],
        ContourValue) then
      begin
        if (MaxValue > MinValue) and (MaxValue >= ContourValue)
          and (ContourValue >= MinValue) then
        begin
          if FLogTransform then
          begin
            if (MinValue < 0) or (ContourValue < 0) or (MaxValue < 0) then
            begin
              AColor := clBlack;
            end
            else
            begin
              LogMax := Log10(MaxValue);
              LogMin := Log10(MinValue);
              LogValue := Log10(ContourValue);
              AColor := FContourColors.FracToColor((LogMax-LogValue)/
                (LogMax - LogMin));
            end;
          end
          else
          begin
            AColor := FContourColors.FracToColor((MaxValue-ContourValue)/
              (MaxValue - MinValue));
          end;
          rdgContourData.Canvas.Brush.Color := AColor;
        end;
      end;
    end
    else
    begin
      if TryStrToInt('$' + rdgContourData.Cells[ACol, ARow], Value) then
      begin
        AColor := Value;
        rdgContourData.Canvas.Brush.Color := AColor;
      end;
    end;
  end;
end;

procedure TfrmSpecifyContours.rdgContourDataButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  Value: Integer;
  AColor: TColor;
begin
  inherited;
  if TryStrToInt('$' + rdgContourData.Cells[ACol, ARow], Value) then
  begin
    AColor := Value
  end
  else
  begin
    AColor := clBlack;
  end;
  clrDlg.Color := AColor;
  if clrDlg.Execute then
  begin
    rdgContourData.Cells[ACol, ARow] := IntToHex(clrDlg.Color,6);
  end;
end;

procedure TfrmSpecifyContours.rdgContourDataColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  ArrangeControls;
end;

procedure TfrmSpecifyContours.UpdateRowCount;
begin
  if seRowCount <> nil then
  begin
    seRowCount.AsInteger := rdgContourData.RowCount -1;
    NumberGrid;
  end;
end;

procedure TfrmSpecifyContours.rdgContourDataEndUpdate(Sender: TObject);
begin
  inherited;
  UpdateRowCount;
  ArrangeControls;

end;

procedure TfrmSpecifyContours.rdgContourDataHorizontalScroll(Sender: TObject);
begin
  inherited;
  ArrangeControls;
end;

procedure TfrmSpecifyContours.rdgContourDataMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(rdgContourData, rdeLineThickness, Ord(ccThickness));
end;

procedure TfrmSpecifyContours.rdgContourDataSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ACol = Ord(ccColor)) and cbAutomaticColors.Checked then
  begin
    CanSelect := False;
  end;
end;

procedure TfrmSpecifyContours.sbAddRowClick(Sender: TObject);
begin
  inherited;
  seRowCount.AsInteger := seRowCount.AsInteger + 1;
  UpdateRowCount;
  FillThicknessCells;
end;

procedure TfrmSpecifyContours.sbDeleteRowClick(Sender: TObject);
begin
  inherited;
  if rdgContourData.Row >= 1 then
  begin
    if seRowCount.AsInteger = 1 then
    begin
      seRowCount.AsInteger := 0;
      NumberGrid;
    end
    else
    begin
      rdgContourData.DeleteRow(rdgContourData.Row);
      UpdateRowCount;
    end;
  end;

end;

procedure TfrmSpecifyContours.sbInsertRowClick(Sender: TObject);
begin
  inherited;
  if rdgContourData.Row >= 1 then
  begin
    rdgContourData.InsertRow(rdgContourData.Row);
    UpdateRowCount;
    FillThicknessCells;
  end;
end;

procedure TfrmSpecifyContours.FillThicknessCells;
var
  Index: Integer;
begin
  for Index := 1 to rdgContourData.RowCount - 1 do
  begin
    if rdgContourData.Cells[Ord(ccThickness),Index] = '' then
    begin
      rdgContourData.Cells[Ord(ccThickness),Index] := rdeLineThickness.Text;
    end;
  end;
end;

procedure TfrmSpecifyContours.seRowCountChange(Sender: TObject);
begin
  inherited;
  if seRowCount.AsInteger = 0 then
  begin
    rdgContourData.RowCount := 2;
  end
  else
  begin
    rdgContourData.RowCount := seRowCount.AsInteger + 1;
  end;
  sbDeleteRow.Enabled := seRowCount.AsInteger > 0;
  FillThicknessCells;
  NumberGrid;
end;

procedure TfrmSpecifyContours.SetData(var Contours: TContours);
var
  Count: Integer;
  Index: Integer;
  Value: double;
  Thickness: double;
  AColor: TColor;
  ColorValue: integer;
begin
  Count := seRowCount.AsInteger;
  if Count = 0 then
  begin
    FreeAndNil(Contours);
  end
  else
  begin
    if Contours = nil then
    begin
      Contours:= TContours.Create;
    end;
    Contours.AutomaticColors := cbAutomaticColors.Checked;
    Contours.Count := Count;
    Count := 0;
    for Index := 1 to rdgContourData.RowCount - 1 do
    begin
      if TryStrToFloat(rdgContourData.Cells[Ord(ccValue),Index], Value)
        and TryStrToFloat(rdgContourData.Cells[Ord(ccThickness),Index], Thickness)
        and (Contours.AutomaticColors
        or TryStrToInt('$' + rdgContourData.Cells[Ord(ccColor),Index], ColorValue))
        then
      begin
        AColor := ColorValue;
        if FLogTransform then
        begin
          if Value <= 0 then
          begin
            Continue;
          end;
          Contours.ContourValues[Count] := Log10(Value);
        end
        else
        begin
          Contours.ContourValues[Count] := Value;
        end;
        Contours.LineThicknesses[Count] := Thickness;
        if not Contours.AutomaticColors then
        begin
          Contours.ContourColors[Count] := Color32(AColor);
        end;
        Inc(Count);
      end;
    end;
    Contours.Count := Count;
    Contours.Sort;
  end;
end;

end.
