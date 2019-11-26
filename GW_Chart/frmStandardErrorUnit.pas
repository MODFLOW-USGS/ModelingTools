unit frmStandardErrorUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ArgusDataEntry, Buttons;

type
  TfrmStandardError = class(TForm)
    adeStandardError: TArgusDataEntry;
    lblStandardErrorOfRegression: TLabel;
    OpenDialog1: TOpenDialog;
    btnRead: TButton;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    procedure btnReadClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmStandardError: TfrmStandardError;

implementation

{$R *.DFM}

procedure TfrmStandardError.btnReadClick(Sender: TObject);
var
  AStringList : TStringList;
  Index : integer;
  AString : string;
  EqualPos : integer;
begin
  if OpenDialog1.Execute then
  begin
    AStringList := TStringList.Create;
    try
      AStringList.LoadFromFile(OpenDialog1.FileName);
      for Index := 0 to AStringList.Count -1 do
      begin
        AString := AStringList[Index];
        if Pos('STANDARD ERROR OF THE REGRESSION',AString) > 0 then
        begin
          EqualPos := Pos('=',AString);
          if EqualPos > 0 then
          begin
            Delete(AString,1,EqualPos);
            adeStandardError.Text := trim(AString);
            Exit;
          end;
        end;
      end;
      Beep;
      MessageDlg('Standard error of regression not found in '
        + OpenDialog1.FileName, mtWarning, [mbOK], 0);
    finally
      AStringList.Free;
    end;
  end;
end;

end.
