unit frmAboutFgdcMetaEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, System.Rtti,
  FMX.Grid.Style, FMX.Grid;

type
  TfrmAboutMetaEditor = class(TForm)
    memoDisclaimer: TMemo;
    pnlBottom: TPanel;
    btnClose: TButton;
    sgAcknowledgements: TStringGrid;
    scItem: TStringColumn;
    scURL: TStringColumn;
    lblVersion: TLabel;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmAboutMetaEditor: TfrmAboutMetaEditor;

implementation

uses
  DisclaimerTextUnit, frmMetaDataUnit;

{$R *.fmx}

procedure TfrmAboutMetaEditor.FormCreate(Sender: TObject);
begin
  memoDisclaimer.Lines.Add(DisclaimerString);
  sgAcknowledgements.Cells[0,0] := 'Turbopower Abbrevia components';
  sgAcknowledgements.Cells[1,0] := 'https://github.com/TurboPack/Abbrevia';
  sgAcknowledgements.Cells[0,1] := 'Xml.VerySimple.pas';
  sgAcknowledgements.Cells[1,1] := 'https://github.com/Dennis1000/verysimplexml';
  lblVersion.Text := 'Version ' + KFgdcMetaEditorVersion;
end;

end.
