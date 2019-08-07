{@abstract(The main purpose of @name is to define @link(TfrmSelectImage)
  which is used to select among multiple imported bitmaps
  when the user want to edit a bitmap and has imported more than one
  bitmap.)}
unit frmSelectImageUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons;

type
  {@abstract(@name is used to select among multiple imported bitmaps
    when the user want to edit a bitmap and has imported more than one
    bitmap.)}
  TfrmSelectImage = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help of @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TComboBox;
    // @name lists the names of the bitmaps that have been imported.
    comboBitmaps: TComboBox;
    // @name: TLabel;
    lblSelect: TLabel;
    // @name displays an instance of @link(TfrmImportBitmap) using the
    // bitmap that is selected in @link(comboBitmaps).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name fills @link(comboBitmaps) with the naes of the bitmaps that have
    // been selected.
    procedure GetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

uses frmGoPhastUnit, CompressedImageUnit, frmImportBitmapUnit;

{$R *.dfm}

{ TfrmSelectImage }

procedure TfrmSelectImage.GetData;
var
  Index: integer;
  Item: TCompressedBitmapItem;
begin
  comboBitmaps.Items.Capacity := frmGoPhast.PhastModel.Bitmaps.Count;
  for Index := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
  begin
    Item := frmGoPhast.PhastModel.Bitmaps.Items[Index] as TCompressedBitmapItem;
    comboBitmaps.Items.AddObject(Item.Name, Item);
  end;
  if comboBitmaps.Items.Count > 0 then
  begin
    comboBitmaps.ItemIndex := 0;
    btnOK.Enabled := True;
  end;
end;

procedure TfrmSelectImage.btnOKClick(Sender: TObject);
begin
  inherited;
  Hide;
  with TfrmImportBitmap.Create(nil) do
  begin
    try
      GetData(comboBitmaps.Items.Objects[comboBitmaps.ItemIndex] as
        TCompressedBitmapItem);
      ShowModal;
    finally
      Free;
    end;
  end;
end;

procedure TfrmSelectImage.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

end.

