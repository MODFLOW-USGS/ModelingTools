
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit glyphform;

interface

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  GlyphCombo, ExtCtrls, StdCtrls, ImgList;

type
  TfrmGlyphComboDsgn = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    lbItems: TListBox;
    Label1: TLabel;
    edCaption: TEdit;
    btnAdd: TButton;
    btnRemove: TButton;
    Button1: TButton;
    Button2: TButton;
    Label2: TLabel;
    edImageIndex: TEdit;
    Sample: TImage;
    Button3: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure btnRemoveClick(Sender: TObject);
    procedure edImageIndexChange(Sender: TObject);
    procedure lbItemsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    Combo: TksoGlyphComboBox;
  public
    { Public declarations }
  end;

var
  frmGlyphComboDsgn: TfrmGlyphComboDsgn;

procedure Execute(ACombo: TksoGlyphComboBox);

implementation {===============================================================}

{$R *.DFM}

procedure Execute(ACombo: TksoGlyphComboBox);
var
  i: integer;
begin
  if ACombo = nil then Exit;
  with TfrmGlyphComboDsgn.Create(Application) do
  begin
    Caption := Caption + ' - [' + ACombo.Name + ']';
    Combo.Items.Assign(ACombo.Items);
    for i := 0 to Combo.Items.count-1 do
      lbItems.Items.Add(Combo.Items.Names[i]);
    Combo.Images := ACombo.Images;
    if ShowModal = mrOk then
      ACombo.Items.Assign(Combo.Items);
    Free;
  end;
end;

procedure TfrmGlyphComboDsgn.btnAddClick(Sender: TObject);
begin
  // add to combo
  if edImageIndex.Text = '' then
    Combo.Items.Add(edCaption.Text+'=-1')
  else
    Combo.Items.Add(edCaption.Text+'='+edImageIndex.Text);
  // add to ListBox
  lbItems.Items.Add(edCaption.Text);
end;

procedure TfrmGlyphComboDsgn.Button3Click(Sender: TObject);
begin
  if lbItems.ItemIndex < 0 then Exit; 
  // add to combo
  if edImageIndex.Text = '' then
    Combo.Items[lbItems.ItemIndex] := edCaption.Text+'=-1'
  else
    Combo.Items[lbItems.ItemIndex] := edCaption.Text+'='+edImageIndex.Text;
  // add to ListBox
  lbItems.Items[lbItems.ItemIndex] := edCaption.Text;
end;

procedure TfrmGlyphComboDsgn.btnRemoveClick(Sender: TObject);
begin
  if lbItems.ItemIndex < 0 then Exit;
  // remove from combo
  Combo.Items.Delete(lbItems.ItemIndex);
  // remo from ListBox
  lbItems.Items.Delete(lbItems.ItemIndex);
end;

procedure TfrmGlyphComboDsgn.edImageIndexChange(Sender: TObject);
var
  B: TBitmap;
begin
  if Combo.Images = nil then Exit;
  if (edImageIndex.Text = '') or (edImageIndex.Text = '-1') then Exit;
  if StrToInt(edImageIndex.Text) >= Combo.Images.Count then Exit;

  B := TBitmap.Create;
  Combo.Images.GetBitmap(StrToInt(edImageIndex.Text), B);
  B.Transparent := true;
  Sample.Picture.Bitmap := B;
  B.Free;
end;

procedure TfrmGlyphComboDsgn.lbItemsClick(Sender: TObject);
var
  Index: integer;
begin
  Index := lbItems.ItemIndex;
  edCaption.Text := lbItems.Items[Index];
  edImageIndex.Text := Combo.Items.Values[Combo.Items.Names[Index]];
end;

procedure TfrmGlyphComboDsgn.FormCreate(Sender: TObject);
begin
  Combo := TksoGlyphComboBox.Create(Self);
end;

procedure TfrmGlyphComboDsgn.FormDestroy(Sender: TObject);
begin
  Combo.Free;
end;

end.
