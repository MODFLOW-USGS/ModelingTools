unit frameScreenObjectWelUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectCondParamUnit,
  Vcl.Grids, RbwDataGrid4, ArgusDataEntry, JvExControls, JvxCheckListBox,
  Vcl.StdCtrls, Vcl.Buttons, Vcl.Mask, JvExMask, JvSpin, Vcl.ExtCtrls,
  JvToolEdit;

type
  TframeScreenObjectWel = class(TframeScreenObjectCondParam)
    fedTabfile: TJvFilenameEdit;
    lblTabfile: TLabel;
    procedure fedTabfileChange(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frameScreenObjectWel: TframeScreenObjectWel;

implementation

{$R *.dfm}

procedure TframeScreenObjectWel.fedTabfileChange(Sender: TObject);
begin
  inherited;
  if (fedTabfile.FileName = '') or FileExists(fedTabfile.FileName) then
  begin
    fedTabfile.Color := clWindow;
  end
  else
  begin
    fedTabfile.Color := clRed;
  end;
end;

end.
