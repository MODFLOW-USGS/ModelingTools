
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit BaseComboForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, AxCtrls, StdCtrls, BaseCombo;

type

  TfrmCustomCombo = class(TForm)
    procedure FormPaint(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FCombo: TksoAbstractComboBox;
    FFixedWidth: boolean;
    FFreeAfterDrop: boolean;
    procedure WMActivate(var Msg: TWMActivate); message WM_ACTIVATE;
    procedure WMMouseActivate(var Msg: TWMMouseActivate);
      message WM_MOUSEACTIVATE;
    function GetParentWnd: HWnd;
    procedure SetCombo(const Value: TksoAbstractComboBox);
    procedure SetFixedWidth(const Value: boolean);
    procedure SetFreeAfterDrop(const Value: boolean);
    { Private declarations }
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure AfterSetCombo; virtual; abstract;
    property Combo: TksoAbstractComboBox read FCombo write SetCombo;
    property FixedWidth: boolean read FFixedWidth write SetFixedWidth;
    property FreeAfterDrop: boolean read FFreeAfterDrop write SetFreeAfterDrop;
  end;

var
  frmCustomCombo: TfrmCustomCombo;

implementation {===============================================================}

{$R *.DFM}

constructor TfrmCustomCombo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFixedWidth := false;
  FFreeAfterDrop := true;
end;

procedure TfrmCustomCombo.WMActivate(var Msg: TWMActivate);
begin
  inherited ;
  if (Msg.Active = WA_ACTIVE) and (GetParentWnd <> 0) and
     (Owner is TWinControl) and ((Owner as TWinControl).Visible) then
    SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
  if (Msg.Active = WA_INACTIVE) then
    Close;
end;

procedure TfrmCustomCombo.WMMouseActivate(var Msg: TWMMouseActivate);
begin
end;

function TfrmCustomCombo.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
{$IFNDEF kso_ACTIVEX}
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  While P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
{$ELSE}
  P := GetParent((Owner as TWinControl).Handle);
  Last := P;
  While P <> 0 do
  begin
    Last := P;
    P := GetParent(P);
  end;
  Result := Last;
{$ENDIF}
end;

procedure TfrmCustomCombo.FormPaint(Sender: TObject);
var
  R: TRect;
begin
  inherited ;
  R := ClientRect;
  DrawEdge(Canvas.Handle, R, BDR_RAISEDINNER or BDR_RAISEDOUTER,
    BF_RECT);
end;

procedure TfrmCustomCombo.SetCombo(const Value: TksoAbstractComboBox);
begin
  FCombo := Value;
  AfterSetCombo;
end;

procedure TfrmCustomCombo.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  FCombo.AfterDropDown;
end;

procedure TfrmCustomCombo.SetFixedWidth(const Value: boolean);
begin
  FFixedWidth := Value;
end;

procedure TfrmCustomCombo.SetFreeAfterDrop(const Value: boolean);
begin
  FFreeAfterDrop := Value;
end;

end.
