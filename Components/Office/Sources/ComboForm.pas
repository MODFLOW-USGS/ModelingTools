
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit ComboForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo, BaseComboForm;

type

  TfrmCombo = class(TfrmCustomCombo)
    StrList: TListBox;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure StrListMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure StrListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure StrListKeyPress(Sender: TObject; var Key: Char);
    procedure StrListDrawItem(Control: TWinControl; Index: Integer;
      Rect: TRect; State: TOwnerDrawState);
    procedure StrListMeasureItem(Control: TWinControl; Index: Integer;
      var Height: Integer);
  private
  public
    { Public declarations }
    procedure AfterSetCombo; override;
  end;

var
  frmCombo: TfrmCombo;

implementation {===============================================================}

uses ComboBox;

{$R *.DFM}

{ TfrmTextCombo }

procedure TfrmCombo.AfterSetCombo;
begin
  // Initialization
  Font := (Combo as TksoCustomComboBox).Font;
  with (Combo as TksoCustomComboBox) do
  begin
    if Style = csOwnerDrawFixed then
     StrList.Style := lbOwnerDrawFixed;
    if Style = csOwnerDrawVariable then
     StrList.Style := lbOwnerDrawVariable;
  end;
  StrList.Sorted := (Combo as TksoCustomComboBox).Sorted;
  StrList.ItemHeight := (Combo as TksoCustomComboBox).ItemHeight;
  StrList.Items.Assign((Combo as TksoCustomComboBox).Items);
  StrList.Color := (Combo as TksoCustomComboBox).Color;
  ClientHeight := StrList.ItemHeight *
    (Combo as TksoCustomComboBox).DropDownCount + 4;
end;

procedure TfrmCombo.FormResize(Sender: TObject);
begin
  StrList.BoundsRect := Rect(2, 2, ClientWidth-2, ClientHeight-2);
end;

procedure TfrmCombo.FormPaint(Sender: TObject);
var
  Rect: TRect;
begin
  Rect := ClientRect;
  DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT); 
end;

procedure TfrmCombo.StrListMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  Item: integer;
begin
  // Hot select
  Item := StrList.ItemAtPos(Point(X, Y), true);
  if Item <> -1 then
    StrList.ItemIndex := Item;
end;

procedure TfrmCombo.StrListMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Item: integer;
begin
  // End select
  Item := StrList.ItemAtPos(Point(X, Y), true);
  if Item <> -1 then
    (Combo as TksoCustomComboBox).ItemIndex := StrList.ItemIndex;
  Close;
end;

procedure TfrmCombo.StrListKeyPress(Sender: TObject; var Key: Char);
begin
  case Ord(Key) of
    VK_RETURN: begin
      Key := #0;
      (Combo as TksoCustomComboBox).ItemIndex := StrList.ItemIndex;
      Close;
    end;
    VK_ESCAPE: begin
      Key := #0;
      Close;
    end;
  end;
end;

procedure TfrmCombo.StrListDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
begin
  (Combo as TksoCustomComboBox).DrawItem(StrList.Canvas, Index, Rect, State);
end;

procedure TfrmCombo.StrListMeasureItem(Control: TWinControl;
  Index: Integer; var Height: Integer);
begin
  (Combo as TksoCustomComboBox).MeasureItem(Index, Height);
end;

end.
