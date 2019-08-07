unit InPlaceEditUnit;

interface

uses
  Windows, Messages, Classes, Controls, Forms, Mask, SysUtils;

type
  TRbwInplaceEdit = class(TCustomMaskEdit)
  private
    FMResult : TModalResult;
  protected
    procedure KeyPress(var Key: Char); override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X: Integer; Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property ModalResult:TModalResult read FMResult;
    procedure SetFocus; reintroduce;
    procedure Run;
  published
    property OnChange;
    property MouseCapture;
    property Font;
  end;

implementation

type
  TSelection = record
    StartPos: Integer;
    EndPos: Integer;
  end;

{ TRbwInplaceEdit }

constructor TRbwInplaceEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
//  BorderStyle := bsNone;
  FMResult:=mrNone;
  AutoSize := True;
end;

procedure TRbwInplaceEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE;
end;

procedure TRbwInplaceEdit.KeyPress(var Key: Char);
begin
  case Key of
    #27: // Esc key
      begin
        FMResult := mrCancel;
        Key := #0;
        Exit
      end;
    #13: // Enter key
      begin
        FMResult := mrOK;
        Key := #0;
        Exit
      end;
  end;
  if Key <> #0 then
  begin
    MouseCapture := True;
  end;
  inherited KeyPress(Key);
end;

procedure TRbwInplaceEdit.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  if (X < 0) or (X > Width)
    or (Y < 0) or (Y > Height) then
  begin
    FMResult := mrOK;
    if MouseCapture then
    begin
      MouseCapture := False;
      Mouse_Event(MOUSEEVENTF_LEFTDOWN, 0, 0, 0, 0);
    end;
  end
  else
  begin
    MouseCapture := True;
  end;
  inherited;
end;

procedure TRbwInplaceEdit.Run;
begin
  if Visible then
  begin
    repeat
      Application.ProcessMessages;
    until (ModalResult <> mrNone);
  end;
end;

procedure TRbwInplaceEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then
    Windows.SetFocus(Handle);
end;

end.
