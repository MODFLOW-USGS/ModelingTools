unit framePackageMnw1Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, framePackageUnit, RbwController,
  StdCtrls, JvExStdCtrls, JvCombobox, JvListComb, Mask, JvExMask, JvSpin,
  ArgusDataEntry, JvToolEdit, ModflowPackageSelectionUnit;

type
  TframePackageMnw1 = class(TframePackage)
    seMaxIterations: TJvSpinEdit;
    lblMaxIterations: TLabel;
    comboLosstype: TJvImageComboBox;
    lblLosstype: TLabel;
    rdeLossExponent: TRbwDataEntry;
    lblLossExponent: TLabel;
    fedWellFileName: TJvFilenameEdit;
    lblWellFileName: TLabel;
    lblByNode: TLabel;
    fedByNode: TJvFilenameEdit;
    lblQSum: TLabel;
    fedQSum: TJvFilenameEdit;
    comboByNodeFrequency: TJvImageComboBox;
    lblByNodeFrequency: TLabel;
    lblQSumFrequency: TLabel;
    comboQSumFrequency: TJvImageComboBox;
    procedure comboLosstypeChange(Sender: TObject);
    procedure rcSelectionControllerEnabledChange(Sender: TObject);
    procedure fedByNodeChange(Sender: TObject);
    procedure fedQSumChange(Sender: TObject);
    procedure fedWellFileNameChange(Sender: TObject);
  private
    procedure EnableLossExponent;
    procedure EnableByNodeFrequency;
    procedure EnableQSumFrequency;
    procedure SetFileEditColor(FileEdit: TJvFilenameEdit);
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageMnw1: TframePackageMnw1;

implementation



{$R *.dfm}

{ TframePackageMnw1 }

procedure TframePackageMnw1.comboLosstypeChange(Sender: TObject);
begin
  inherited;
  EnableLossExponent;
end;

procedure TframePackageMnw1.GetData(Package: TModflowPackageSelection);
var
  Mnw1Package: TMnw1Package;
begin
  inherited;
  Mnw1Package := Package as TMnw1Package;
  seMaxIterations.AsInteger := Mnw1Package.MaxMnwIterations;
  comboLosstype.ItemIndex := Ord(Mnw1Package.LossType);
  rdeLossExponent.RealValue := Mnw1Package.LossExponent;
  try
    fedWellFileName.FileName := Mnw1Package.WellFileName;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;

  try
    fedByNode.FileName := Mnw1Package.ByNodeFileName;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;

  try
    fedQSum.FileName := Mnw1Package.QSumFileName;
  except on EComboEditError do
    begin
      // do nothing.
    end;

  end;
  comboByNodeFrequency.ItemIndex := Ord(Mnw1Package.ByNodePrintFrequency);
  comboQSumFrequency.ItemIndex := Ord(Mnw1Package.QSumPrintFrequency);
  comboLosstypeChange(nil);
  fedByNodeChange(nil);
  fedQSumChange(nil);
  fedWellFileNameChange(nil);
end;

procedure TframePackageMnw1.rcSelectionControllerEnabledChange(Sender: TObject);
begin
  inherited;
  EnableLossExponent;
  EnableByNodeFrequency;
  EnableQSumFrequency;
end;

procedure TframePackageMnw1.SetData(Package: TModflowPackageSelection);
var
  Mnw1Package: TMnw1Package;
begin
  inherited;
  Mnw1Package := Package as TMnw1Package;
  Mnw1Package.MaxMnwIterations := seMaxIterations.AsInteger;
  Mnw1Package.LossType := TMnw1LossType(comboLosstype.ItemIndex);
  Mnw1Package.LossExponent := rdeLossExponent.RealValue;
  Mnw1Package.WellFileName := fedWellFileName.FileName;
  Mnw1Package.ByNodeFileName := fedByNode.FileName;
  Mnw1Package.QSumFileName := fedQSum.FileName;
  Mnw1Package.ByNodePrintFrequency :=
    TMnw1PrintFrequency(comboByNodeFrequency.ItemIndex);
  Mnw1Package.QSumPrintFrequency :=
    TMnw1PrintFrequency(comboQSumFrequency.ItemIndex);
end;

procedure TframePackageMnw1.SetFileEditColor(FileEdit: TJvFilenameEdit);
var
  Color: Integer;
  FileName: TFileName;
  FileDir: string;
begin
  Color := clWindow;
  FileName := FileEdit.FileName;
  if FileName <> '' then
  begin
    FileDir := ExtractFileDir(FileName);
    if not DirectoryExists(FileDir) then
    begin
      Color := clRed;
    end;
  end;
  FileEdit.Color := Color;
end;

procedure TframePackageMnw1.EnableQSumFrequency;
begin
  comboQSumFrequency.Enabled := rcSelectionController.Enabled
    and (fedQSum.FileName <> '');
end;

procedure TframePackageMnw1.EnableByNodeFrequency;
begin
  comboByNodeFrequency.Enabled := rcSelectionController.Enabled
    and (fedByNode.FileName <> '');
end;

procedure TframePackageMnw1.EnableLossExponent;
begin
  rdeLossExponent.Enabled := rcSelectionController.Enabled
    and (comboLosstype.ItemIndex = Ord(mlt1NonLinear));
end;

procedure TframePackageMnw1.fedByNodeChange(Sender: TObject);
begin
  inherited;
  EnableByNodeFrequency;
  SetFileEditColor(fedByNode);
end;

procedure TframePackageMnw1.fedQSumChange(Sender: TObject);
begin
  inherited;
  EnableQSumFrequency;
  SetFileEditColor(fedQSum);
end;

procedure TframePackageMnw1.fedWellFileNameChange(Sender: TObject);
begin
  inherited;
  SetFileEditColor(fedWellFileName);
end;

end.
