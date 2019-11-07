
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeToolBar;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, OfficeTypes, OfficeUtils;

const

  RollBtnWidth     = 10;
  TabHeight        = 10;
  TabWidth         = 50;

type

{ TksoOfficeDock }

  TksoOfficeToolBar = class;

  TksoOfficeDock = class(TCustomControl)
  private
    { tabs }
    function ShowTabs: boolean;
    { propery sets }
    function GetToolBar(index: integer): TksoOfficeToolBar;
    function GetToolBarCount: integer;
  protected
    function GetClientRect: TRect; override;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Drawing }
    procedure Paint; override;
    { Properies }
    property ToolBarCount: integer read GetToolBarCount;
    property ToolBars[index: integer]: TksoOfficeToolBar read GetToolBar;
  published
    property Align;
    property Color;
    property ParentColor;
  end;

{ TksoOfficeToolBar }

  TksoOfficeToolBar = class(TCustomControl)
  private
    FRolled: boolean;
    FNormalHeight: integer;
    FButtonDown: boolean;
    { drawing }
    function GetButtonRect: TRect;
    procedure DrawRollButton;
    { properties }
    procedure SetRolled(const Value: boolean);
  protected
    function GetClientRect: TRect; override;
    procedure SetParent(AParent: TWinControl); override;
    { mouse }
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Drawing }
    procedure Paint; override;
  published
    property Color;
    property ParentColor;
    property Rolled: boolean read FRolled write SetRolled;
  end;

implementation {===============================================================}

{$R OfficeToolBar.res}

var
  RollUpBmp: TBitmap; 
  RollDownBmp: TBitmap; 

{ TksoOfficeDock }

constructor TksoOfficeDock.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Align := alTop;
  Width := 185;
  Height := 22;
  Color := clWindow;
end;

destructor TksoOfficeDock.Destroy;
begin
  inherited Destroy;
end;

procedure TksoOfficeDock.AlignControls(AControl: TControl; var Rect: TRect);
var
  R: TRect;
  i: integer;
begin
  DisableAlign;
  try
    if Align in [alTop, alBottom] then
      R := Classes.Rect(0, 0, Width, 0)
    else
      R := Classes.Rect(0, 0, 0, Height);
    { Align not rolled toolbars}
    for i := 0 to ToolBarCount-1 do
      with ToolBars[i] do
        if not Rolled then
        begin
          EnableAlign;
          try
            case Self.Align of
              alTop, alBottom: begin
                BoundsRect := Classes.Rect(R.left, R.top, R.right, R.top + Height);
                Inc(R.top, Height);
                R.bottom := R.top;
              end;
              alLeft, alRight: begin
                BoundsRect := Classes.Rect(R.left, R.top, R.left+Width, R.bottom);
                Inc(R.left, Width);
                R.right := R.left;
              end;
            end;
          finally
            DisableAlign
          end;
        end;
    { Set self bounds}
    if Align in [alTop, alBottom] then
    begin
      if ShowTabs then
        Height := R.bottom + TabHeight
      else
        Height := R.bottom;
      R := inherited GetClientRect;
      R.top := R.bottom - TabHeight;
    end
    else
    begin
      if ShowTabs then
        Width := R.right + TabHeight
      else
        Width := R.right;
      R := inherited GetClientRect;
      R.left := R.right - TabHeight;
    end;
    { Align rolled toolbars}
    for i := 0 to ToolBarCount-1 do
      with ToolBars[i] do
        if Rolled then
        begin
          case Self.Align of
            alTop, alBottom: begin
              BoundsRect := Classes.Rect(R.left, R.top, R.left+TabWidth, R.bottom);
              OffsetRect(R, TabWidth, 0);
            end;
            alLeft, alRight: begin
              BoundsRect := Classes.Rect(R.left, R.top, R.right, R.top+TabWidth);
              OffsetRect(R, 0, TabWidth);
            end;
          end;
        end;
  finally
    EnableAlign;
  end;
end;

function TksoOfficeDock.GetToolBar(index: integer): TksoOfficeToolBar;
var
  i: integer;
begin
  Result := nil;
  i := 0;
  while i < ControlCount do
  begin
    if Controls[i] is TksoOfficeToolBar then
    begin
      if i = index then
      begin
        Result := Controls[i] as TksoOfficeToolBar;
        Break;
      end;
      Inc(i);
    end;
  end;
end;

function TksoOfficeDock.GetToolBarCount: integer;
var
  i: integer;
begin
  i := 0;
  while i < ControlCount do
  begin
    if Controls[i] is TksoOfficeToolBar then
    begin
      Inc(i);
    end;
  end;
  Result := i;
end;

function TksoOfficeDock.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
  if not ShowTabs then Exit;
  case Align of
    alLeft, alRight: Result := Rect(0, 0, Width - TabHeight, Height);
    alTop, alBottom: Result := Rect(0, 0, Width, Height - TabHeight);
  end;
end;

function TksoOfficeDock.ShowTabs: boolean;
var
  i: integer;
begin
  for i := 0 to ToolBarCount-1 do
    if ToolBars[i].Rolled then
    begin
      Result := true;
      Exit;
    end;
  Result := false;
end;

procedure TksoOfficeDock.Paint;
begin
  with Canvas do
  begin
    Pen.Style := psClear;
    Brush.Style := bsSolid;
    Brush.Color := Color;
    Rectangle(0, 0, Width+1, Height+1);
  end;
end;

{ TksoOfficeToolBar }

constructor TksoOfficeToolBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents,
    csSetCaption, csOpaque, csDoubleClicks, csReplicatable];
  Width := 185;
  Height := 22;
  ShowHint := false;
  ParentShowHint := false;
end;

destructor TksoOfficeToolBar.Destroy;
begin
  inherited Destroy;
end;

function TksoOfficeToolBar.GetClientRect: TRect;
begin
  if not Rolled then
    if Parent.Align in [alTop, alBottom] then
      Result := Rect(RollBtnWidth+2, 0, Width, Height)
    else
      Result := Rect(0, RollBtnWidth+2, Width, Height)
end;

function TksoOfficeToolBar.GetButtonRect: TRect;
begin
  if Parent.Align in [alTop, alBottom] then
    Result := Rect(0, 0, RollBtnWidth, Height)
  else
    Result := Rect(0, 0, Width, RollBtnWidth);
end;

procedure TksoOfficeToolBar.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) then
  begin
    if not Rolled then
    begin
      if PtInRect(GetButtonRect, Point(X, Y)) then
        FButtonDown := true;
    end
    else
    begin
      FButtonDown := true;
    end;
    DrawRollButton;
  end;
end;

procedure TksoOfficeToolBar.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if (Button = mbLeft) then
  begin
    if not Rolled then
    begin
      if PtInRect(GetButtonRect, Point(X, Y)) then
        Rolled := true;
    end
    else
    begin
      Rolled := false;
    end;
    FButtonDown := false;
    DrawRollButton;
  end;
end;

procedure TksoOfficeToolBar.DrawRollButton;
var
  R: TRect;
begin
  with Canvas do
  begin
    if not Rolled then
    begin
      { Draw rollbtn }
      Pen.Style := psSolid;
      Brush.Style := bsSolid;
      if not FButtonDown then
      begin
        Brush.Color := Color;
        R := GetButtonRect;
        FillRect(R);
        DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT);
        { Draw btm }
        Draw(2, 2, RollUpBmp);
      end
      else
      begin
        Brush.Color := clBtnShadow;
        R := GetButtonRect;
        FillRect(R);
        DrawEdge(Handle, R, BDR_SUNKENINNER, BF_RECT);
        { Draw btm }
        Draw(3, 3, RollUpBmp);
      end;
    end
    else
    begin
      { Draw rollbtn }
      Pen.Style := psSolid;
      R := inherited GetClientRect;
      if not FButtonDown then
      begin
        Brush.Color := Color;
        Rectangle(0, 0, Width+1, Height+1);
        DrawEdge(Handle, R, BDR_RAISEDINNER, BF_RECT);
        { Draw btm }
        Draw(3, 2, RollDownBmp);
      end
      else
      begin
        Brush.Color := clBtnShadow;
        Rectangle(0, 0, Width+1, Height+1);
        DrawEdge(Handle, R, BDR_SUNKENINNER, BF_RECT);
        { Draw btm }
        Draw(4, 3, RollDownBmp);
      end;
    end;
  end;
end;

procedure TksoOfficeToolBar.Paint;
begin
  with Canvas do
  begin
    if not Rolled then
    begin
      { Draw face }
      Pen.Style := psClear;
      Brush.Style := bsSolid;
      Brush.Color := Color;
      Rectangle(0, 0, Width+1, Height+1);
      DrawRollButton;
    end
    else
    begin
      DrawRollButton;
    end;
  end;
end;

procedure TksoOfficeToolBar.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);
end;

procedure TksoOfficeToolBar.SetRolled(const Value: boolean);
var
  R: TRect;
  i: integer;
begin
  if Value = FRolled then Exit;
  { Save old height }
  if (not FRolled) and Value then
  begin
    if Parent.Align in [alTop, alBottom] then
      FNormalHeight := Height
    else
      FNormalHeight := Width;
    { Hide controls }
    for i := 0 to ControlCount-1 do
    begin
      Controls[i].Tag := Integer(Controls[i].Visible);
      Controls[i].Visible := false; 
    end;
    ShowHint := true;
  end;
  { Restore from height }
  if (FRolled) and (not Value) then
  begin
    FRolled := false;
    if Parent.Align in [alTop, alBottom] then
      Height := FNormalHeight
    else
      Width := FNormalHeight;
    { Show controls }
    for i := 0 to ControlCount-1 do
      Controls[i].Visible := Boolean(Controls[i].Tag);
    ShowHint := false;
  end
  else
  begin
    { Set value }
    FRolled := Value;
    { Align owner }
    if Parent is TksoOfficeDock then
      (Parent as TksoOfficeDock).AlignControls(nil, R);
  end;
end;

initialization
  RollUpBmp := TBitmap.Create;
  RollUpBmp.Handle := LoadBitmap(HInstance, 'KSO_TBROLLUP');
  RollUpBmp.Transparent := true;
  RollDownBmp := TBitmap.Create;
  RollDownBmp.Handle := LoadBitmap(HInstance, 'KSO_TBROLLDOWN');
  RollDownBmp.Transparent := true;
finalization
  RollDownBmp.Free;
  RollUpBmp.Free;
end.
