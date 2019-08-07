unit framePackageMnw2Unit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, framePackageUnit, RbwController, StdCtrls, JvExStdCtrls, JvCombobox,
  JvListComb, ModflowPackageSelectionUnit;

type
  TframePackageMnw2 = class(TframePackage)
    lblPrintOption: TLabel;
    comboPrintOption: TJvImageComboBox;
    gbMnwiOptions: TGroupBox;
    cbWellOutput: TCheckBox;
    cbSummarizeByWell: TCheckBox;
    cbSummarizeByNode: TCheckBox;
  private
    { Private declarations }
  public
    procedure GetData(Package: TModflowPackageSelection); override;
    procedure SetData(Package: TModflowPackageSelection); override;
    { Public declarations }
  end;

var
  framePackageMnw2: TframePackageMnw2;

implementation

{$R *.dfm}

{ TframePackageMnw2 }

procedure TframePackageMnw2.GetData(Package: TModflowPackageSelection);
var
  Mnw2Selection: TMultinodeWellSelection;
begin
  inherited;
  Mnw2Selection := Package as TMultinodeWellSelection;
  comboPrintOption.ItemIndex := Ord(Mnw2Selection.PrintOption);
  cbWellOutput.Checked := Mnw2Selection.CreateWellFile;
  cbSummarizeByWell.Checked := Mnw2Selection.SummarizeByWell;
  cbSummarizeByNode.Checked := Mnw2Selection.SummarizeByNode;
end;

procedure TframePackageMnw2.SetData(Package: TModflowPackageSelection);
var
  Mnw2Selection: TMultinodeWellSelection;
begin
  inherited;
  Mnw2Selection := Package as TMultinodeWellSelection;
  Mnw2Selection.PrintOption := TMnw2PrintOption(comboPrintOption.ItemIndex);
  Mnw2Selection.CreateWellFile := cbWellOutput.Checked;
  Mnw2Selection.SummarizeByWell := cbSummarizeByWell.Checked;
  Mnw2Selection.SummarizeByNode := cbSummarizeByNode.Checked;
end;

end.
