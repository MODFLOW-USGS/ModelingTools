{@abstract(@name defines @link(TframePrintFrequency)
  which is used to display an edit box for specifying
  a print-frequency along with a combobox for displaying the units
  of the print-frequency.  Instances of
  @link(TframePrintFrequency) are displayed
  in cells of a TStringGrid in
  @link(frmPrintFrequencyUnit.TfrmPrintFrequency).)}
unit framePrintFrequencyUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, GoPhastTypes, ExtCtrls, ArgusDataEntry;

type
  {@abstract(@name is used to display an edit box for specifying
    a print-frequency along with a combobox for displaying the units
    of the print-frequency.  Instances of @classname are displayed
    in cells of a TStringGrid in
    @link(frmPrintFrequencyUnit.TfrmPrintFrequency).)}
  TframePrintFrequency = class(TFrame)
    // @name allows the user to edit the units of the print-frequency.
    // See @link(comboUnitsChange).
    comboUnits: TComboBox;
    // @name holds @link(comboUnits).
    // See @link(Splitter).
    panelCombo: TPanel;
    // @name holds @link(rdeFrequency).
    // See @link(Splitter).
    panelEdit: TPanel;
    // @name allows the user to edit the value of the print-frequency.
    rdeFrequency: TRbwDataEntry;
    // @name allows the user to resize the size of @link(comboUnits)
    // and @link(rdeFrequency) by resizing @link(panelCombo)
    // and @link(panelEdit).
    Splitter: TSplitter;
    // @name changes the DataType or @link(rdeFrequency) or disables
    // it based on the units the user selects.
    // See @link(comboUnits).
    procedure comboUnitsChange(Sender: TObject);
  private
    // See @link(Frequency).
    function GetFrequency: double;
    // See @link(Units).
    function GetUnits: TFrequencyUnits;
    // See @link(Frequency).
    procedure SetFrequency(const Value: double);
    // See @link(Units).
    procedure SetUnits(const Value: TFrequencyUnits);
    { Private declarations }
  public
    // @name copies the @link(Units) and @link(Frequency) of Frame
    // to the instance that calls @name.
    procedure CopyData(const Frame: TframePrintFrequency);
    // @name is the print-frequency.
    property Frequency: double read GetFrequency write SetFrequency;
    // @name sets ParentFont to @true.
    procedure Loaded; override;
    // @name represents the units of the Print-frequency or allows the
    // user to print at the end of a stress period.
    property Units: TFrequencyUnits read GetUnits write SetUnits;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TframePrintFrequency }

procedure TframePrintFrequency.comboUnitsChange(Sender: TObject);
begin
  case Units of
    fuDefault, fuSeconds, fuMinutes, fuHours, fuDays, fuYears:
      begin
        rdeFrequency.Enabled := True;
        rdeFrequency.DataType := dtReal;
      end;
    fuStep:
      begin
        rdeFrequency.Enabled := True;
        rdeFrequency.Text := IntToStr(Round(StrToFloat(rdeFrequency.Text)));
        rdeFrequency.DataType := dtInteger;
      end;
    fuEnd:
      begin
        rdeFrequency.Enabled := False;
      end;
  else
    Assert(False);
  end;

end;

procedure TframePrintFrequency.SetFrequency(const Value: double);
begin
  if (Units = fuStep) and (Value <> Round(Value)) then
  begin
    Units := fuDefault;
  end;
  case Units of
    fuDefault, fuSeconds, fuMinutes, fuHours, fuDays, fuYears:
      begin
        rdeFrequency.Enabled := True;
        rdeFrequency.Text := FloatToStr(Value);
      end;
    fuStep:
      begin
        rdeFrequency.Enabled := True;
        rdeFrequency.Text := IntToStr(Round(Value));
      end;
    fuEnd:
      begin
        rdeFrequency.Enabled := False;
        if Round(Value) = Value then
        begin
          rdeFrequency.Text := IntToStr(Round(Value));
        end
        else
        begin
          rdeFrequency.Text := FloatToStr(Value);
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TframePrintFrequency.SetUnits(const Value: TFrequencyUnits);
begin
  comboUnits.ItemIndex := Ord(Value);
  comboUnitsChange(nil);
end;

function TframePrintFrequency.GetFrequency: double;
begin
  result := StrToFloat(rdeFrequency.Text);
end;

function TframePrintFrequency.GetUnits: TFrequencyUnits;
begin
  result := TFrequencyUnits(comboUnits.ItemIndex);
end;

procedure TframePrintFrequency.Loaded;
begin
  inherited;
  ParentFont := True;
end;

procedure TframePrintFrequency.CopyData(const Frame: TframePrintFrequency);
begin
  Frequency := Frame.Frequency;
  Units := Frame.Units;
end;

end.

