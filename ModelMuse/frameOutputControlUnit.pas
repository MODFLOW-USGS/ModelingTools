unit frameOutputControlUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, Mask, JvExMask, JvSpin, ArgusDataEntry, JvExStdCtrls,
  JvCombobox, JvListComb, RbwController, ModflowOutputControlUnit;

type
  TframeOutputControl = class(TFrame)
    lblOutputType: TLabel;
    lblFrequency: TLabel;
    lblN: TLabel;
    spN: TJvSpinEdit;
    lblExternalFormat: TLabel;
    lblResult: TLabel;
    adeD: TRbwDataEntry;
    lblDot: TLabel;
    adeW: TRbwDataEntry;
    rcExternalFormat: TRbwController;
    comboP: TJvImageComboBox;
    comboREdit: TJvImageComboBox;
    comboFrequency: TJvImageComboBox;
    cbSaveExternal: TCheckBox;
    cbPrintListing: TCheckBox;
    lblListinglFormat: TLabel;
    rcListingFormat: TRbwController;
    comboPrintStyle: TJvImageComboBox;
    comboPrintFormat: TJvImageComboBox;
    lblSaveType: TLabel;
    comboSaveType: TJvImageComboBox;
    procedure comboSaveTypeChange(Sender: TObject);
    procedure comboFrequencyChange(Sender: TObject);
    procedure comboPChange(Sender: TObject);
    procedure comboREditChange(Sender: TObject);
    procedure adeWChange(Sender: TObject);
    procedure adeDChange(Sender: TObject);
    procedure cbSaveExternalClick(Sender: TObject);
    procedure cbPrintListingClick(Sender: TObject);
  private
    procedure EnableExternalController;
    procedure EnableListingController;
    procedure EnableN;
    procedure ShowFormat;
    function OutputFormat: string;
    procedure EnableComboFrequency;
    { Private declarations }
  public
    procedure Loaded; override;
    procedure GetData(Data: THeadDrawdownOutputControl);
    procedure SetData(Data: THeadDrawdownOutputControl);
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeOutputControl.adeDChange(Sender: TObject);
begin
  ShowFormat
end;

procedure TframeOutputControl.adeWChange(Sender: TObject);
begin
  ShowFormat
end;

procedure TframeOutputControl.cbPrintListingClick(Sender: TObject);
begin
  EnableComboFrequency;
  EnableListingController;
  EnableN;
end;

procedure TframeOutputControl.cbSaveExternalClick(Sender: TObject);
begin
  comboSaveType.Enabled := cbSaveExternal.Checked;
  lblSaveType.Enabled := comboSaveType.Enabled;
  EnableComboFrequency;
  EnableExternalController;
  EnableN;
end;

procedure TframeOutputControl.comboFrequencyChange(Sender: TObject);
begin
  EnableN;
end;

procedure TframeOutputControl.comboPChange(Sender: TObject);
begin
  ShowFormat;
end;

procedure TframeOutputControl.comboREditChange(Sender: TObject);
begin
  ShowFormat
end;

procedure TframeOutputControl.comboSaveTypeChange(Sender: TObject);
begin
  EnableExternalController;
  EnableN;
end;

procedure TframeOutputControl.EnableComboFrequency;
begin
  comboFrequency.Enabled := cbSaveExternal.Checked or cbPrintListing.Checked;
end;

procedure TframeOutputControl.EnableExternalController;
begin
  rcExternalFormat.Enabled := (comboSaveType.ItemIndex = 0)
    and cbSaveExternal.Checked;
end;

procedure TframeOutputControl.EnableListingController;
begin
  rcListingFormat.Enabled := cbPrintListing.Checked;
end;

procedure TframeOutputControl.EnableN;
begin
  spN.Enabled := comboFrequency.Enabled;
  lblN.Enabled := spN.Enabled;
end;

procedure TframeOutputControl.GetData(Data: THeadDrawdownOutputControl);
begin
  cbSaveExternal.Checked := Data.SaveInExternalFile;
  cbPrintListing.Checked := Data.PrintInListing;
  comboFrequency.ItemIndex := Ord(Data.FrequencyChoice);
  spN.AsInteger := Data.Frequency;
  comboSaveType.ItemIndex := Ord(Data.OutputFileType);
  comboP.ItemIndex := Ord(Data.ExternalFormat.ExtFormatPrefix);
  comboREdit.ItemIndex := Ord(Data.ExternalFormat.NumberFormat);
  adeW.Text := IntToStr(Data.ExternalFormat.Width);
  adeD.Text := IntToStr(Data.ExternalFormat.Decimals);
  comboPrintStyle.ItemIndex := Ord(Data.Wrapping);
  comboPrintFormat.ItemIndex := Ord(Data.PrintFormat);
end;

procedure TframeOutputControl.Loaded;
begin
  inherited;
  ShowFormat;
end;

function TframeOutputControl.OutputFormat: string;
begin
  result := '';
  if csLoading in ComponentState then
  begin
    Exit;
  end;
  result := '(10(1X' + comboP.Text + comboREdit.Text
    + adeW.Text + '.' + adeD.Text + '))';
end;

procedure TframeOutputControl.SetData(Data: THeadDrawdownOutputControl);
begin
  Data.SaveInExternalFile := cbSaveExternal.Checked;
  Data.PrintInListing := cbPrintListing.Checked;
  Data.FrequencyChoice := TFrequencyChoice(comboFrequency.ItemIndex);
  Data.Frequency := spN.AsInteger;
  Data.OutputFileType := TOutputFileType(comboSaveType.ItemIndex);
  Data.ExternalFormat.ExtFormatPrefix := TExtFormatPrefix(comboP.ItemIndex);
  Data.ExternalFormat.NumberFormat := TNumberFormat(comboREdit.ItemIndex);
  Data.ExternalFormat.Width := StrToInt(adeW.Output);
  Data.ExternalFormat.Decimals := StrToInt(adeD.Output);
  Data.Wrapping := TWrapping(comboPrintStyle.ItemIndex);
  Data.PrintFormat := TPrintFormat(comboPrintFormat.ItemIndex);
end;

procedure TframeOutputControl.ShowFormat;
begin
  lblResult.Caption := OutputFormat;
end;

end.
