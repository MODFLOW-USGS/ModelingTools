{@abstract(The main purpose of @name is to define @link(TfrmJpegWarning)
  which is used to display a warning to the user that
  GDI+ is not installed.)}
unit frmJpegWarningUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls;

type
  {@abstract(@name is used to display a warning to the user that
    GDI+ is not installed.)}
  TfrmJpegWarning = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes the @classname.
    btnClose: TBitBtn;
    // @name: TButton;
    // See @link(btnGoToWebSiteClick).
    btnGoToWebSite: TButton;
    // @name: TBitBtn;
    // Clicking @name shows help on @classname.
    btnHelp: TBitBtn;
    // @name: TLabel;
    // @name warns the user that GDI+ is not installed.
    lblWarning: TLabel;
    // @name: TMemo;
    // @name shows the URL for GDI+
    memoURL: TMemo;
    // @name: TPanel;
    // @name holds the buttons at the bottom of @classname.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name holds @link(lblWarning).
    pnlWarning: TPanel;
    // Clicking @name opens a web browser with the URL for GDI+
    procedure btnGoToWebSiteClick(Sender: TObject);
    // @name initializes @classname.
    procedure FormCreate(Sender: TObject); override;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses RbwInternetUtilities;

{$R *.dfm}

procedure TfrmJpegWarning.FormCreate(Sender: TObject);
begin
  inherited;
  lblWarning.Width := pnlWarning.Width - 16;
end;

procedure TfrmJpegWarning.btnGoToWebSiteClick(Sender: TObject);
var
  Browser: string;
begin
  inherited;
  LaunchUrl(Browser,
    'http://www.microsoft.com/downloads/details.aspx?familyid=6A63AB9C-DF12-4D41-933C-BE590FEAA05A');
end;

end.

