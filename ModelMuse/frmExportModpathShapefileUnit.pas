unit frmExportModpathShapefileUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, CustomExtendedDialogForm;

type
  TfrmExportModpathShapefile = class(TCustomExtendedDialog)
    comboModelSelection: TComboBox;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmExportModpathShapefile: TfrmExportModpathShapefile;

implementation

{$R *.dfm}

end.
