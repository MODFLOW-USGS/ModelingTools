unit frmSettingsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls,
  Vcl.ComCtrls, ErrorMessages, JclAnsiStrings;

type
  TSettings = class(TComponent)
  published
  end;

  TfrmSettings = class(TForm)
    pcSettings: TPageControl;
    Panel1: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    TabPrimary: TTabSheet;
    tabSecondary: TTabSheet;
    tabObsID: TTabSheet;
    tabIgnored: TTabSheet;
    memoPrimary: TMemo;
    memoSecondary: TMemo;
    memoObservations: TMemo;
    memoIgnored: TMemo;
    tabErrors: TTabSheet;
    tabWarnings: TTabSheet;
    memoErrors: TMemo;
    memoWarnings: TMemo;
    procedure btnOKClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    procedure SetData;
    procedure GetIdentifiers(Lines: TStrings; Identifiers: TAnsiStringList);
    procedure SetIdentifiers(Lines: TStrings; Identifiers: TAnsiStringList);
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

var
  frmSettings: TfrmSettings;

implementation

uses
  ModflowIdentifiersUnit, frmListAnalyzerUnit;

{$R *.dfm}

procedure TfrmSettings.btnOKClick(Sender: TObject);
begin
  SetData;
  if frmMain.OpenDialog1.FileName <> '' then
  begin
    frmMain.OpenAFile(frmMain.OpenDialog1.FileName);
  end;
end;

procedure TfrmSettings.FormShow(Sender: TObject);
begin
  pcSettings.ActivePageIndex := 0;
end;

{ TfrmSettings }

procedure TfrmSettings.GetData;
begin
  GetIdentifiers(memoPrimary.Lines, PackageIdentifiers);
  GetIdentifiers(memoSecondary.Lines, BoundaryIdentifiers);
  GetIdentifiers(memoObservations.Lines, ObsIdentifiers);
  GetIdentifiers(memoIgnored.Lines, NonIdentifiers);
  GetIdentifiers(memoErrors.Lines, ErrorValues);
  GetIdentifiers(memoWarnings.Lines, WarningValues);
end;

procedure TfrmSettings.SetData;
begin
  SetIdentifiers(memoPrimary.Lines, PackageIdentifiers);
  SetIdentifiers(memoSecondary.Lines, BoundaryIdentifiers);
  SetIdentifiers(memoObservations.Lines, ObsIdentifiers);
  SetIdentifiers(memoIgnored.Lines, NonIdentifiers);
  SetIdentifiers(memoErrors.Lines, ErrorValues);
  SetIdentifiers(memoWarnings.Lines, WarningValues);
end;

procedure TfrmSettings.SetIdentifiers(Lines: TStrings;
  Identifiers: TAnsiStringList);
begin
  Identifiers.Assign(Lines);
end;

procedure TfrmSettings.GetIdentifiers(Lines: TStrings; Identifiers: TAnsiStringList);
begin
  Lines.BeginUpdate;
  try
    Identifiers.Sorted := True;
    Lines.Assign(Identifiers);
  finally
    Lines.EndUpdate;
  end;
end;

end.
