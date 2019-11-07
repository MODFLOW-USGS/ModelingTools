
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit glyphcombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo, ComboBox;

type

{ TksoGlyphComboBox }

  TksoGlyphComboBox = class(TksoCustomComboBox)
  private
    FImages: TImageList;
    procedure SetImages(const Value: TImageList);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    { inherited }
    procedure DrawItem(Canvas: TCanvas; Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
  published
    property Color;
    property Ctl3D;
    property DragMode;
    property DragCursor;
    property DropDownCount;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ItemHeight;
    property Images: TImageList read FImages write SetImages;
    property Items;
    property BorderStyle;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property Sorted;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnClick;
    property OnChange;
    property OnDrawItem;
    property OnMeasureItem;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
  end;

implementation {===============================================================}

const

  DefaultHeight = 16;

{ TksoGlyphComboBox }

constructor TksoGlyphComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Style := csOwnerDrawVariable;
end;

destructor TksoGlyphComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoGlyphComboBox.DrawItem(Canvas: TCanvas; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  ImageIndex: integer;
  B: TBitmap;
begin
  if Index = -1 then Exit;
  with Canvas do
  begin
    FillRect(Rect);
    ImageIndex := -1;
    try
      ImageIndex := StrToInt(Items.Values[Items.Names[Index]]);
    except
    end;
    if (FImages <> nil) and (ImageIndex <> -1) then
    begin
      B := TBitmap.Create;
      FImages.GetBitmap(ImageIndex, B);
      B.Transparent := true;
      Draw(Rect.left+1, Rect.top+1, B);
      B.Free;
    end;
    Inc(Rect.left, Rect.bottom-Rect.top+3);
    TextOut(Rect.left+1, Rect.top + (Rect.bottom-Rect.top-TextHeight(Text)) div 2,
      Items.Names[Index]);
  end;
end;

procedure TksoGlyphComboBox.MeasureItem(Index: Integer;
  var Height: Integer);
begin
  if FImages = nil then
    Height := DefaultHeight
  else
    Height := FImages.Height+3;
end;

procedure TksoGlyphComboBox.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FImages) then
    FImages := nil;
end;

procedure TksoGlyphComboBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  if (FImages <> nil) and (FImages.Height > 0) then
    ItemHeight := FImages.Height;
end;

end.
