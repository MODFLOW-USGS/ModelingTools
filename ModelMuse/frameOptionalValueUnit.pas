unit frameOptionalValueUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  ArgusDataEntry;

type
  TframeOptionalValue = class(TFrame)
    cbUsed: TCheckBox;
    RdeValue: TRbwDataEntry;
    LblVariableLabel: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

end.
