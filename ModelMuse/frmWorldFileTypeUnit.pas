unit frmWorldFileTypeUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, JvExControls, JvLinkLabel, Buttons;

type
  TfrmWorldFileType = class(TfrmCustomGoPhast)
    rbRaster: TRadioButton;
    rbCAD: TRadioButton;
    lblQuestion: TLabel;
    lnklblRaster: TJvLinkLabel;
    lnklblCAD: TJvLinkLabel;
    BitBtn1: TBitBtn;
    procedure lnklblRasterLinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: string);
    procedure JvLinkLabel2LinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmWorldFileType: TfrmWorldFileType;

implementation

uses JvLinkLabelTools;

{$R *.dfm}

procedure TfrmWorldFileType.lnklblRasterLinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: string);
begin
  inherited;
  TWebTools.OpenWebPage('http://webhelp.esri.com/arcgisdesktop/9.3/index.cfm?id=3046&pid=3034&topicname=World_files_for_raster_datasets');
end;

procedure TfrmWorldFileType.JvLinkLabel2LinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: string);
begin
  inherited;
   TWebTools.OpenWebPage('http://support.esri.com/index.cfm?fa=knowledgebase.techarticles.articleShow&d=16106');
end;

end.
