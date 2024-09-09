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
    procedure cbUsedClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeOptionalValue.cbUsedClick(Sender: TObject);
begin
  RdeValue.Enabled := cbUsed.Checked
end;

end.
