
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit ColorForm2000;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo, BaseComboForm, ComCtrls, Buttons;

type

  TfrmColorCombo2000 = class(TfrmCustomCombo)
    btnDefault: TSpeedButton;
    Label1: TLabel;
    stdColor1: TSpeedButton;
    stdColor2: TSpeedButton;
    stdColor7: TSpeedButton;
    stdColor5: TSpeedButton;
    stdColor3: TSpeedButton;
    stdColor8: TSpeedButton;
    stdColor6: TSpeedButton;
    stdColor4: TSpeedButton;
    stdColor9: TSpeedButton;
    stdColor10: TSpeedButton;
    stdColor15: TSpeedButton;
    stdColor13: TSpeedButton;
    stdColor11: TSpeedButton;
    stdColor16: TSpeedButton;
    stdColor14: TSpeedButton;
    stdColor12: TSpeedButton;
    Label2: TLabel;
    customColor1: TSpeedButton;
    customColor2: TSpeedButton;
    customColor7: TSpeedButton;
    customColor5: TSpeedButton;
    customColor3: TSpeedButton;
    customColor8: TSpeedButton;
    customColor6: TSpeedButton;
    customColor4: TSpeedButton;
    customColor9: TSpeedButton;
    customColor10: TSpeedButton;
    customColor15: TSpeedButton;
    customColor13: TSpeedButton;
    customColor11: TSpeedButton;
    customColor16: TSpeedButton;
    customColor14: TSpeedButton;
    customColor12: TSpeedButton;
    Bevel1: TBevel;
    procedure FormCreate(Sender: TObject);
    procedure btnDefaultClick(Sender: TObject);
    procedure stdColor1Click(Sender: TObject);
    procedure customColor1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure AfterSetCombo; override;
  end;

var
  frmColorCombo2000: TfrmColorCombo2000;

implementation {===============================================================}

uses ColorCombo2000;

{$R *.DFM}

procedure GetStandardGlyph(B: TBitmap; index: Integer);
begin
  with B.Canvas do
  begin
    Brush.Color := clBtnFace;
    Rectangle(-1, -1, B.Width+1, B.Height+1);
    Pen.Color := clBtnShadow;
    Brush.Color := ColorValues[index];
    Rectangle(1, 1, B.Width-1, B.Height-1);
  end;
end;

procedure GetCustomGlyph(B: TBitmap; index: Integer; CustomList: TStrings);
begin
  with B.Canvas do
  begin
    Brush.Color := clBtnFace;
    Rectangle(-1, -1, B.Width+1, B.Height+1);
    Pen.Color := clBtnShadow;
    Brush.Color := StringToColor(CustomList[index]);
    Rectangle(1, 1, B.Width-1, B.Height-1);
  end;
end;

procedure TfrmColorCombo2000.AfterSetCombo;
var
  i: integer;
  B: TBitmap;
  Comp: TComponent;
begin
  with (Combo as TksoColorComboBox2000) do
  begin
    if ColorKind = ckDefault then btnDefault.Down := true;
    B := TBitmap.Create;
    B.Width := 14;
    B.Height := 14;
    for i := 0 to ColorCount-1 do
    begin
      GetStandardGlyph(B, i);
      Comp := Self.FindComponent('stdColor'+IntToStr(i+1));
      if (Comp <> nil) and (Comp is TSpeedButton) then
      with Comp as TSpeedButton do
      begin
        Glyph := B;
        Glyph.Transparent := false;
        Transparent := false;
        Tag := i;
        if (ColorKind = ckStandard) and (ColorIndex = i) then
          Down := true;
      end;
    end;
    for i := 0 to ColorCount-1 do
    begin
      GetCustomGlyph(B, i, CustomColors);
      Comp := Self.FindComponent('customColor'+IntToStr(i+1));
      if (Comp <> nil) and (Comp is TSpeedButton) then
      with Comp as TSpeedButton do
      begin
        Glyph := B;
        Glyph.Transparent := false;
        Transparent := false;
        Tag := i;
        if (ColorKind = ckCustom) and (ColorIndex = i) then
          Down := true;
      end;
    end;
    B.Free;
  end;
end;

procedure TfrmColorCombo2000.FormCreate(Sender: TObject);
begin
  FixedWidth := true;
end;

procedure TfrmColorCombo2000.btnDefaultClick(Sender: TObject);
begin
  with (Combo as TksoColorComboBox2000) do
  begin
    ColorValue := DefaultColor;
    ColorKind := ckDefault;
    ColorIndex := 0;
  end;
  Close;
end;

procedure TfrmColorCombo2000.stdColor1Click(Sender: TObject);
begin
  with (Combo as TksoColorComboBox2000) do
  begin
    ColorValue := ColorValues[(Sender as TComponent).Tag];
    ColorKind := ckStandard;
    ColorIndex := (Sender as TComponent).Tag;
  end;
  Close;
end;

procedure TfrmColorCombo2000.customColor1Click(Sender: TObject);
begin
  with (Combo as TksoColorComboBox2000) do
  begin
    ColorValue := StringToColor(CustomColors[(Sender as TComponent).Tag]);
    ColorKind := ckCustom;
    ColorIndex := (Sender as TComponent).Tag;
  end;
  Close;
end;

end.
