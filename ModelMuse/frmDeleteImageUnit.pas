unit frmDeleteImageUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, CheckLst, Buttons, ExtCtrls;

type
  TfrmDeleteImage = class(TfrmCustomGoPhast)
    clbBitmaps: TCheckListBox;
    pnlBottom: TPanel;
    btnOK: TBitBtn;
    BitBtn1: TBitBtn;
    btnHelp: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
  private
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmDeleteImage: TfrmDeleteImage;

implementation

uses
  frmGoPhastUnit, CompressedImageUnit;

{$R *.dfm}

{ TfrmDeleteImage }

procedure TfrmDeleteImage.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmDeleteImage.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmDeleteImage.GetData;
var
  Index: Integer;
begin
  for Index := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
  begin
    clbBitmaps.Items.Add((frmGoPhast.PhastModel.Bitmaps.Items[Index]
      as TCompressedBitmapItem).Name);
  end;
end;

procedure TfrmDeleteImage.SetData;
var
  Index: Integer;
begin
  for Index := clbBitmaps.Items.Count - 1 downto 0 do
  begin
    if clbBitmaps.Checked[Index] then
    begin
      frmGoPhast.PhastModel.Bitmaps.Delete(Index);
    end;
  end;
end;

end.
