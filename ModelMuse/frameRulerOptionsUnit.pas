{@abstract(@name defines @link(TframeRulerOptions) which is used
  to edit the display format of a TRbwRuler.)}
unit frameRulerOptionsUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, RbwRuler, ArgusDataEntry, Mask, JvExMask, JvSpin;

type
  {@abstract(@name is used to edit the display format of a TRbwRuler.)
  @member(lblPrecision @name shows the text "Precision.")
  @member(lblSampleNumber @name shows the text "Sample number.")
  @member(lblPreviewLabel @name shows the text "Preview:.")
  }
  TframeRulerOptions = class(TFrame)

    lblPrecision: TLabel;
    lblSampleNumber: TLabel;
    lblPreviewLabel: TLabel;
    // @name is used to give a preview of what the numbers on the TRbwRuler
    // will look like.
    lblPreview: TLabel;
    // @name is used to allow the user to enter the number that will
    // be shown in @link(lblPreview).
    // See @link(rdePreviewNumberChange).
    rdePreviewNumber: TRbwDataEntry;
    lblDigits: TLabel;
    sePrecision: TJvSpinEdit;
    seDigits: TJvSpinEdit;
    lblSpacing: TLabel;
    seSpacing: TJvSpinEdit;
    // @name causes the preview number in @link(lblPreview) to be updated.
    procedure rdePreviewNumberChange(Sender: TObject);
    // @name is the OnChange event handler for @link(sePrecision).  It calls
    // @link(SetPreview).
    procedure sePrecisionChange(Sender: TObject);
  private
    // @name is the TRbwRuler that is being edited.
    FRuler: TRbwRuler;
    // @name causes the preview number in @link(lblPreview) to be updated.
    procedure SetPreview;
    { Private declarations }
  public
    // @name copies the data from Ruler to the controls in @classname.
    procedure GetData(const Ruler: TRbwRuler);
    // @name copies the data from the controls in @classname to the
    // TRbwRuler that is being edited.
    procedure SetData;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TframeRulerOptions }

procedure TframeRulerOptions.SetPreview;
begin
  if (sePrecision <> nil) and (rdePreviewNumber.Text <> '') then
  begin
    lblPreview.Caption := FloatToStrF(StrToFloat(rdePreviewNumber.Text),
      ffNumber, sePrecision.AsInteger, seDigits.AsInteger);
  end;
end;

procedure TframeRulerOptions.rdePreviewNumberChange(Sender: TObject);
begin
  SetPreview;
end;

procedure TframeRulerOptions.GetData(const Ruler: TRbwRuler);
begin
  Assert(Ruler <> nil);
  FRuler := Ruler;
  sePrecision.Value := FRuler.RulerPrecision;
  seDigits.Value := FRuler.RulerDigits;
  seSpacing.Value := FRuler.RulerDesiredSpacing;
  SetPreview;
end;

procedure TframeRulerOptions.SetData;
begin
  FRuler.RulerPrecision := sePrecision.AsInteger;
  FRuler.RulerDigits := seDigits.AsInteger;
  FRuler.RulerDesiredSpacing := seSpacing.AsInteger;
end;

procedure TframeRulerOptions.sePrecisionChange(Sender: TObject);
begin
  SetPreview;
end;

end.

