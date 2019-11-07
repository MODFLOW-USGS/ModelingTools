
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit basecombo;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Buttons, OfficeTypes, OfficeUtils;

const

  SepWidth                 = 3; // Separator Width
  ButtonWidth: integer     = 17; // Button Width

type

  TComboState = (csNormal, csFocused, csDown, csDisable);

  TksoAbstractComboBox = class(TCustomControl)
  private
    { Private declarations }
    FMouseInControl: Boolean;
    FBorderStyle: TksoBorderStyle;
    FState: TComboState;
    FComboForm: TCustomForm;
    FOnDropDown: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FFlat: boolean;
    FBorderStyleFocused: TksoBorderStyle;
    FBorderStyleFlat: TksoBorderStyle;
    // rbw begin change
    FDropDownHeight: integer;
    // rbw end change
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure CNKeyDown(var Message: TWMKeyDown); message CN_KEYDOWN;
    { base functions }
    procedure SetBorderStyle(const Value: TksoBorderStyle);
    procedure SetBorderStyleFlat(const Value: TksoBorderStyle);
    procedure SetBorderStyleFocused(const Value: TksoBorderStyle);
    procedure SetFlat(const Value: boolean);
  protected
    procedure DoDropDown(Sender: TObject); virtual;
    { Protected declarations }
    procedure Paint; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    { Abstract declarations }
    function GetDropDownForm: TCustomForm; virtual;
    procedure DrawBox(Canvas: TCanvas; R: TRect; State: TComboState); virtual; abstract;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure AfterDropDown; dynamic;
    { Abstract declarations }
    procedure Change; dynamic;
    procedure Previous; dynamic; abstract;
    procedure Next; dynamic; abstract;
    { Public properys }
    property State: TComboState read FState write FState;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    { Published declarations }
    property Align;
    property BorderStyle: TksoBorderStyle read FBorderStyle write SetBorderStyle;
    property BorderStyleFlat: TksoBorderStyle read FBorderStyleFlat write SetBorderStyleFlat;
    property BorderStyleFocused: TksoBorderStyle read FBorderStyleFocused write SetBorderStyleFocused;
    // rbw begin change
    property DropDownHeight: integer read FDropDownHeight write FDropDownHeight;
    // rbw end change
    property Flat: boolean read FFlat write SetFlat;
    property Visible;
  end;

var
  DownImage: TBitmap;
  
implementation {===============================================================}

uses BaseComboForm;

{$R *.res}


{ TksoAbstractComboBox }

constructor TksoAbstractComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 250;
  Height := 22;
  Color := clWindow;
  ParentColor := false;
  TabStop := true;
  ControlStyle := [csCaptureMouse, csClickEvents, csSetCaption, csOpaque,
    csDoubleClicks, csReplicatable, csReflector];

  FBorderStyle := kbsSunken;
  FFlat := false;
  // rbw begin change
  with GetDropDownForm do
  begin
    DropDownHeight := Height;
    Free;
  end;
  // rbw end change
end;

destructor TksoAbstractComboBox.Destroy;
begin
  inherited Destroy;
end;

procedure TksoAbstractComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TksoAbstractComboBox.CMMouseEnter(var Message: TMessage);
begin
  FMouseInControl := true;
  if FState = csNormal then Invalidate;
end;

procedure TksoAbstractComboBox.CMMouseLeave(var Message: TMessage);
begin
  FMouseInControl := false;
  if FState = csNormal then Invalidate;
end;

procedure TksoAbstractComboBox.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  R: TRect;
begin
  inherited ;
  if not Focused then SetFocus;
  R := ClientRect;
  if (PointInRect(Point(X, Y), R)) and (ssLeft in Shift) then
    DoDropDown(Self);
end;

procedure TksoAbstractComboBox.CNKeyDown(var Message: TWMKeyDown);
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

procedure TksoAbstractComboBox.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_UP: begin
      Previous;
      Message.Result := 0;
    end;
    VK_DOWN: begin
      Next;
      Message.Result := 0;
    end;
  else
    inherited ;
  end;
end;

procedure TksoAbstractComboBox.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited ;
  FState := csNormal;
  Invalidate;
end;

procedure TksoAbstractComboBox.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited ;
  FState := csFocused;
  Invalidate;
end;

procedure TksoAbstractComboBox.Paint;
var
  BoxRect, R: TRect;
  Cash: TBitmap;
begin
  Cash := TBitmap.Create;
  try
    Cash.Width := Width;
    Cash.Height := Height;
    R := GetClientRect;
    // Box area
    BoxRect := R;
    InflateRect(BoxRect, -2, -2);
    Dec(BoxRect.right, ButtonWidth);
    Cash.Canvas.Brush.Color := Color;
    { Draw background }
    Cash.Canvas.Rectangle(0, 0, Width, Height);
    { Draw border }
    if FFlat then
      if (FState = csFocused) or FMouseInControl then
        DrawBorder(Cash.Canvas, R, FBorderStyleFocused)
      else
        DrawBorder(Cash.Canvas, R, FBorderStyleFlat)
    else
      DrawBorder(Cash.Canvas, R, FBorderStyle);
    { Draw button }
    InflateRect(R, -2, -2);
    R.left := R.right - ButtonWidth;
    DrawButton(Cash.Canvas, R, FFlat, (FState = csFocused) or FMouseInControl, State = csDown);
    { Draw down image }
    if FState <> csDown then
      Cash.Canvas.Draw(R.left + (R.right-R.left - DownImage.Width) div 2,
        R.top + (R.bottom-R.top - DownImage.Height) div 2, DownImage)
    else
      Cash.Canvas.Draw(R.left + (R.right-R.left - DownImage.Width) div 2 + 1,
        R.top + (R.bottom-R.top - DownImage.Height) div 2 + 1, DownImage);
    { Draw box }
    DrawBox(Cash.Canvas, BoxRect, FState);
    Canvas.Draw(0, 0, Cash);
  finally
    Cash.Free;
  end;
end;

function TksoAbstractComboBox.GetDropDownForm: TCustomForm;
begin
  Result := nil;
end;

procedure TksoAbstractComboBox.DoDropDown(Sender: TObject);
var
  P: TPoint;
begin
  Invalidate;
  P := ClientToScreen(Point(0, Height));
  if (Assigned(FComboForm)) and ((FComboForm as TfrmCustomCombo).FreeAfterDrop) then
    FComboForm.Free;
  FComboForm := GetDropDownForm;
  FComboForm.Left := P.X;
  FComboForm.Top := P.Y;
  // rbw begin change
  FComboForm.Height := DropDownHeight;
  // rbw end change
  if not (FComboForm as TfrmCustomCombo).FixedWidth then
    FComboForm.Width := Width;
  (FComboForm as TfrmCustomCombo).Combo := Self;
  // DropDown Event
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
  FComboForm.Show;
  FState := csDown;
end;

procedure TksoAbstractComboBox.AfterDropDown;
begin
  if Focused then
    FState := csFocused
  else
    FState := csNormal;
  Invalidate;
  Change;
end;

procedure TksoAbstractComboBox.SetBorderStyle(const Value: TksoBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    Invalidate;
  end;
end;

procedure TksoAbstractComboBox.Change;
begin
  If Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TksoAbstractComboBox.SetBorderStyleFlat(const Value: TksoBorderStyle);
begin
  FBorderStyleFlat := Value;
  Invalidate;
end;

procedure TksoAbstractComboBox.SetBorderStyleFocused(const Value: TksoBorderStyle);
begin
  FBorderStyleFocused := Value;
  Invalidate;
end;

procedure TksoAbstractComboBox.SetFlat(const Value: boolean);
begin
  FFlat := Value;
  if csLoading in ComponentState then Exit;
  if not FFlat then
  begin
    FBorderStyle := kbsSunken;
    FBorderStyleFlat := kbsSolid;
    FBorderStyleFocused := kbsOuterSunken;
  end
  else
  begin
    FBorderStyle := kbsSunken;
    FBorderStyleFlat := kbsSolid;
    FBorderStyleFocused := kbsOuterSunken;
  end;
  Invalidate;
end;

initialization
  DownImage := TBitmap.Create;
  DownImage.Handle := LoadBitmap(HInstance, 'KSO_DOWN');
  DownImage.Transparent := true;
  ButtonWidth := GetSystemMetrics(SM_CXHSCROLL);
finalization
  DownImage.Free;
end.
