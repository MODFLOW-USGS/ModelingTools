unit frmTimeUnitsConverterUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  frameGridUnit, Vcl.Buttons, Vcl.ExtCtrls;

type
  TUnitsCol = (ucInput, ucOutput);
  TTimeUnits = (tuSeconds, tuMinutes, tuHours, tuDays, tuJulianYears);

  TfrmTimeUnitsConverter = class(TfrmCustomGoPhast)
    pnlButtons: TPanel;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    frameTimeGrid: TframeGrid;
    pnlTop: TPanel;
    comboInputUnits: TComboBox;
    lblInputUnits: TLabel;
    lblOutputUnits: TLabel;
    comboOutputUnits: TComboBox;
    procedure FormCreate(Sender: TObject); override;
    procedure frameTimeGridGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure btnOKClick(Sender: TObject);
    procedure comboUnitsChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure ConvertAValue(ARow: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses
  Vcl.Clipbrd;

{$R *.dfm}

const
  // Years in the Julian calendar were exactly 365.25 days long.
  // This results in an error of 3 days every 400 years (approximately).
  // MODFLOW uses Julian years and not the more precise length in the
  // Gregorian calendar.
  ConversionFactors: array[TTimeUnits] of double =
    (1, 1/60, 1/3600, 1/3600/24, 1/3600/24/365.25);

procedure TfrmTimeUnitsConverter.btnOKClick(Sender: TObject);
var
  AStringList: TStringList;
begin
  inherited;
  AStringList := TStringList.Create;
  try
    AStringList.Assign(frameTimeGrid.Grid.Cols[Ord(ucOutput)]);
    AStringList.Delete(0);
    Clipboard.AsText := AStringList.Text;
  finally
    AStringList.Free;
  end;
end;

procedure TfrmTimeUnitsConverter.comboUnitsChange(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := 1 to frameTimeGrid.Grid.RowCount - 1 do
  begin
    ConvertAValue(RowIndex);
  end;
end;

procedure TfrmTimeUnitsConverter.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  inherited;
  Release;
end;

procedure TfrmTimeUnitsConverter.FormCreate(Sender: TObject);
begin
  inherited;
  frameTimeGrid.Grid.Cells[Ord(ucInput), 0] := 'Input value';
  frameTimeGrid.Grid.Cells[Ord(ucOutput), 0] := 'Output value';
end;

procedure TfrmTimeUnitsConverter.ConvertAValue(ARow: Integer);
var
  ConvertedValue: Extended;
  UnitsInput: TTimeUnits;
  AValue: Extended;
  UnitsOutput: TTimeUnits;
begin
  if TryStrToFloat(frameTimeGrid.Grid.Cells[Ord(ucInput), ARow], AValue) then
  begin
    if (comboInputUnits.ItemIndex >= 0) and (comboOutputUnits.ItemIndex >= 0) then
    begin
      if comboInputUnits.ItemIndex = comboOutputUnits.ItemIndex then
      begin
        frameTimeGrid.Grid.Cells[Ord(ucOutput), ARow] := frameTimeGrid.Grid.Cells[Ord(ucInput), ARow];
      end
      else
      begin
        UnitsInput := TTimeUnits(comboInputUnits.ItemIndex);
        UnitsOutput := TTimeUnits(comboOutputUnits.ItemIndex);
        ConvertedValue := AValue / ConversionFactors[UnitsInput] * ConversionFactors[UnitsOutput];
        frameTimeGrid.Grid.Cells[Ord(ucOutput), ARow] := FloatToStr(ConvertedValue);
      end;
    end
    else
    begin
      frameTimeGrid.Grid.Cells[Ord(ucOutput), ARow] := '';
    end;
  end
  else
  begin
    frameTimeGrid.Grid.Cells[Ord(ucOutput), ARow] := '';
  end;
end;

procedure TfrmTimeUnitsConverter.frameTimeGridGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  if (ARow > 0) and (ACol = Ord(ucInput)) then
  begin
    ConvertAValue(ARow);
  end;
end;

end.
