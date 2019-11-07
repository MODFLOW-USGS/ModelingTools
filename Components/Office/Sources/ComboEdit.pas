
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit comboedit;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, BaseCombo;

type

  TksoComboEdit = class(TCustomEdit)
  private
    Combo: TksoAbstractComboBox;
    { Private declarations }
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
  protected
    { Protected declarations }
    procedure Change; override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
  end;

implementation {===============================================================}

uses ComboForm, ComboBox;

{ TksoComboEdit }

constructor TksoComboEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentColor := true;
  ParentFont := true;
  BorderStyle := bsNone;
  Visible := false;
  TabStop := true;
  if Owner is TksoAbstractComboBox then
    Combo := Owner as TksoAbstractComboBox;
end;

destructor TksoComboEdit.Destroy;
begin
  inherited Destroy;
end;

procedure TksoComboEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited ;
  if Combo <> nil then
  begin
    Combo.State := csNormal;
    Combo.Invalidate;
    if Combo is TksoCustomComboBox then
      (Combo as TksoCustomComboBox).ExitFocus;
  end;
end;

procedure TksoComboEdit.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited ;
  if Combo <> nil then
  begin
    Combo.State := csFocused;
    Combo.Invalidate;
    if Combo is TksoCustomComboBox then
      (Combo as TksoCustomComboBox).EnterFocus;
  end;
end;

procedure TksoComboEdit.CNKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_UP: begin
      Message.Result := 0;
    end;
    VK_DOWN: begin
      Message.Result := 0;
    end;
  else
    inherited ;
  end;
end;

procedure TksoComboEdit.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_UP: begin
      Combo.Previous;
      Message.Result := 0;
    end;
    VK_DOWN: begin
      Combo.Next;
      Message.Result := 0;
    end;
  else
    inherited ;
  end;
end;

procedure TksoComboEdit.Change;
begin
  if Combo <> nil then
    Combo.Change;
end;

end.
