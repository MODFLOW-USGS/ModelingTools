unit Rbw95Button;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TVAlignment = (vaTop,vaBottom,vaCenter);

  TRbw95Button = class(TButton)
  private
    FWordWrap: boolean;
    FFlat: boolean;
    FAlignment: TAlignment;
    FVerticalAlignment: TVAlignment;
    procedure SetWordWrap(const Value: boolean);
    procedure SetAlignment(const Value: TAlignment);
    procedure SetFlat(const Value: boolean);
    procedure SetVerticalAlignment(const Value: TVAlignment);
    { Private declarations }
  protected
    Function GetStyle : integer;
    // Calls GetWindowLong to determine the windows style.
    procedure SetStyle(const i : integer);
    // calls SetWindowLong to set the Windows style.
    { Protected declarations }
  public
    constructor Create(AOwner: TComponent); override;
    // Creates an instance of TRbw95Button;
    { Public declarations }
  published
    property Alignment : TAlignment read FAlignment write SetAlignment;
    // Alignment defines whether the horizontal alignment of the text is left-
    // aligned, right-aligned or centered.
    Property VerticalAlignment : TVAlignment read FVerticalAlignment
      write SetVerticalAlignment;
    // VerticalAlignment defines whether the vertical alignment of the text
    // is top aligned, bottom-aligned, or centered.
    property Flat : boolean read FFlat write SetFlat;
    // Flat determines whether the button has a Flat appearance or a normal
    // appearance.
    property WordWrap : boolean read FWordWrap write SetWordWrap;
    // WordWrap detemines whether word-wrapping of the button caption occurs.
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbw95Button]);
end;

{ TRbw95Button }

constructor TRbw95Button.Create(AOwner: TComponent);
begin
  inherited;
  FVerticalAlignment := vaCenter;
  FAlignment := taCenter;
end;

function TRbw95Button.GetStyle: integer;
begin
  result :=GetWindowLong(Handle,GWL_STYLE );
  if result = 0 then
  begin
    RaiseLastWin32Error;
  end;
end;

procedure TRbw95Button.SetAlignment(const Value: TAlignment);
var
  i : integer;
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    i := GetStyle;
    i := i and not BS_LEFT and not BS_RIGHT and not BS_CENTER;
    case FAlignment of
      taLeftJustify: SetStyle(i or BS_LEFT);
      taRightJustify: SetStyle(i or BS_RIGHT);
      taCenter: SetStyle(i or BS_CENTER);
    end;
    Invalidate;
  end;
end;


procedure TRbw95Button.SetFlat(const Value: boolean);
var
  i : integer;
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    i := GetStyle;
    if FFlat then SetStyle(i or BS_FLAT)
    else SetStyle(i and not BS_FLAT);
    Invalidate;
  end;
end;

procedure TRbw95Button.SetStyle(const i: integer);
begin
  SetLastError(0);
  if SetWindowLong(Handle,GWL_STYLE, i) = 0 then
  begin
    RaiseLastWin32Error;
  end;
end;

procedure TRbw95Button.SetVerticalAlignment(
  const Value: TVAlignment);
var
  i : integer;
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    i := GetStyle;
    i := i and not BS_TOP and not BS_BOTTOM and not BS_VCENTER;
    case FVerticalAlignment of
      vaTop: SetStyle(i or BS_TOP);
      vaBottom: SetStyle(i or BS_BOTTOM);
      vaCenter: SetStyle(i or BS_VCENTER);
    end;
    Invalidate;
  end;
end;

procedure TRbw95Button.SetWordWrap(const Value: boolean);
var
  i : integer;
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    i := GetStyle;
    if FWordWrap then SetStyle(i or BS_MULTILINE)
    else SetStyle(i and not BS_MULTILINE);
    Invalidate;
  end;
end;

end.
