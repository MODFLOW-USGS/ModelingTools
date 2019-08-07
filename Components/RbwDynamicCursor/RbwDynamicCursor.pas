unit RbwDynamicCursor;
{TRbwDynamicCursor provides a convenient way to draw a cursor at runtime.}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs;

type
  TDrawCursor = procedure(Sender : TObject;
    const AndImage, XorImage : TBitMap) of object;

  ECustomCursorError = class(Exception);
  {ECustomCursorError is raised if you attempt to assign the
  TRbwDynamicCursor.Cursor property to one of the predefined
  cursor constants.}

  TRbwDynamicCursor = class(TComponent)
  private
    Modified : boolean;
    FHotPointY: integer;
    FHotPointX: integer;
    FCursor: TCursor;
    FOnDrawCursor: TDrawCursor;
    procedure SetCursor(const Value: TCursor);
    procedure SetHotPointX(const Value: integer);
    procedure SetHotPointY(const Value: integer);
    procedure SetOnDrawCursor(const Value: TDrawCursor);
    { Private declarations }
  protected
    Procedure DrawCursor; virtual;
    function GetCursor : TCursor; virtual;
    function GetCursorHeight : integer; virtual;
    function GetCursorWidth : integer; virtual;
    procedure DrawDefaultCursor(const AndImage, XorImage : TBitMap); virtual;
    { Protected declarations }
  public
    property CursorHeight : integer read GetCursorHeight;
    {CursorHeight is the height of the cursor in pixels supported by the current
      display driver.}
    property CursorWidth : integer read GetCursorWidth;
    {CursorWidth is the Width of the cursor in pixels supported by the current
      display driver. }
    Constructor Create(AOwner: TComponent); override;
    {Create calls the inherited create and then searches for other
    instances of TRbwDynamicCursor.  It then sets the Cursor property
    to a different value than all the other existing TRbwDynamicCursor's.}
    procedure RedrawCursor; virtual;
    {RedrawCursor should be called when the cursor defined by TRbwDynamicCursor
    needs to be changed. }
    { Public declarations }
  published
    property Cursor : TCursor read GetCursor write SetCursor;
    {Cursor is a TCursor value that may be assigned to instances of other
    components, such as TForm that have a Cursor property.  Doing so, will
    cause the cursor of that other component to be the one defined using
    TRbwDynamicCursor.  You should be certain that the value assigned to
    Cursor does not conflict with a custom cursor defined by something
    other than a TRbwDynamicCursor.}
    Property HotPointX : integer read FHotPointX write SetHotPointX;
    {HotPointX specifies the horizontal position of the cursor's hot spot.
    Setting HotPointX will not cause the cursor to be redrawn.}
    Property HotPointY : integer read FHotPointY write SetHotPointY;
    {HotPointY specifies the vertical position of the cursor's hot spot.
    Setting HotPointY will not cause the cursor to be redrawn.}
    property OnDrawCursor : TDrawCursor read FOnDrawCursor
      write SetOnDrawCursor;
    {OnDrawCursor occurs after the creation of a pair of monochrome TBitMap's
    used to define the cursor that appears on the screen.  These TBitMap's
    are passed to the event handler which should draw on the TBitMap's
    Canvas's to define the cursor that appears on the screen.  If the event
    handler is not assigned, the protected method, DrawDefaultCursor, is
    called instead.  The AndImage will apply bitwise and operation to the
    area overlain by the cursor.  The XorImage will apply bitwise xor
    operation to the area overlain by the cursor.}
    { Published declarations }
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('RBW', [TRbwDynamicCursor]);
end;

{ TRbwDynamicCursor }

constructor TRbwDynamicCursor.Create(AOwner: TComponent);
var
  InvalidCursorConstants : array of TCursor;
  ArraySize : integer;
  AComponent : TComponent;
  InvalidCursor : boolean;
  Index : integer;
  procedure CheckInvalidConstant(const Component : TComponent);
  var
    Index : integer;
    AComponent : TComponent;
  begin
    if (Component is TRbwDynamicCursor) and (Component <> self) then
    begin
      Inc(ArraySize);
      SetLength(InvalidCursorConstants,ArraySize);
      InvalidCursorConstants[ArraySize-1] := TRbwDynamicCursor(Component).FCursor;
    end;
    for Index := 0 to Component.ComponentCount -1 do
    begin
      AComponent := Component.Components[Index];
      CheckInvalidConstant(AComponent);
    end;
  end;
begin
  inherited;
  FCursor := 1;
  Modified := True;
  ArraySize := 0;
  AComponent := AOwner;
  while AComponent <> nil do
  begin
    CheckInvalidConstant(AComponent);
    AComponent := AComponent.Owner;
  end;
  repeat
    InvalidCursor := False;
    for Index := 0 to ArraySize-1 do
    begin
      if FCursor = InvalidCursorConstants[Index] then
      begin
        FCursor := FCursor + 1;
        InvalidCursor := True;
      end;
    end;

  until not InvalidCursor;
  RedrawCursor;
end;

procedure TRbwDynamicCursor.DrawCursor;
var
  MaskSize : integer;
  AndMask, XorMask : PByte;
  AndImage, XorImage : TBitmap;
  ThisCursorWidth, ThisCursorHeight : integer;
  NewCursor : HCURSOR;
  procedure GetBits(const BitMap : TBitmap; var Bits : PByte);
  var
    AComponent : TComponent;
    AHandle : HWND;
    BitMapInfo : PBitmapInfo;
    RegularBitMapInfo : Windows.TBitmap;
  begin
    // size of memory pointed to by Bits must be BitMap.Height * BitMap.Width
    AComponent := Owner;
    AHandle := 0;
    while AComponent <> nil do
    begin
      if AComponent is TForm then
      begin
        AHandle := TForm(AComponent).Canvas.Handle;
        break;
      end;
    end;
    if AHandle <> 0 then
    begin
      if GetObject(BitMap.Handle, SizeOf(Windows.TBitmap), @RegularBitMapInfo)
        = 0 then
      begin
        RaiseLastOSError;
      end;

      GetMem(BitMapInfo, SizeOf(TBitmapInfo)+256*SizeOf(TRGBQuad));

      try
        BitMapInfo^.bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
        if GetDIBits(AHandle, BitMap.Handle, 0, BitMap.Height,  nil, BitMapInfo^,
          DIB_RGB_COLORS) = 0 then
        begin
          RaiseLastOSError;
        end;
        if GetDIBits(AHandle, BitMap.Handle, 0, BitMap.Height,  bits, BitMapInfo^,
          DIB_RGB_COLORS) = 0 then
        begin
          RaiseLastOSError;
        end;
      finally
        FreeMem(BitMapInfo);
      end;
    end;

  end;
begin
  if Modified and not (csDesigning in ComponentState)
    and not (csLoading in ComponentState) then
  begin
    ThisCursorWidth := CursorWidth;
    ThisCursorHeight := CursorHeight;
    MaskSize := (ThisCursorWidth div 8)*ThisCursorHeight;

    AndImage := TBitmap.Create;
    with AndImage do
    begin
      Monochrome := True;

      Width := ThisCursorWidth;
      Height := ThisCursorHeight;

      Canvas.Brush.Color := clWhite;
      canvas.Pen.Color := clBlack;
      Canvas.FillRect(Canvas.ClipRect);
    end;

    XorImage := TBitmap.Create;
    With XorImage do
    begin
      Monochrome := True;

      Width := ThisCursorWidth;
      Height := ThisCursorHeight;

      Canvas.Brush.Color := clBlack;
      canvas.Pen.Color := clBlack;
      Canvas.FillRect(Canvas.ClipRect);
    end;

    if Assigned(OnDrawCursor) then
    begin
      FOnDrawCursor(self,AndImage,XorImage);
    end
    else
    begin
      DrawDefaultCursor(AndImage,XorImage);
    end;

    GetMem(AndMask,MaskSize);
    GetMem(XOrMask,MaskSize);

    if GetBitmapBits(AndImage.Handle, MaskSize, AndMask) = 0 then
    begin
      RaiseLastOSError;
    end;
    if GetBitmapBits(XorImage.Handle, MaskSize, XorMask) = 0 then
    begin
      RaiseLastOSError;
    end;

{    GetBits(AndImage,AndMask);
    GetBits(XorImage,XorMask);  }

    NewCursor := CreateCursor(hInstance, FHotPointX,FHotPointY,
      ThisCursorWidth, ThisCursorHeight, AndMask, XorMask);
    if NewCursor = 0 then
    begin
      RaiseLastOSError;
    end;

    Screen.Cursors[FCursor] := NewCursor;

    AndImage.Free;
    XorImage.Free;

    FreeMem(AndMask);
    FreeMem(XOrMask);

    Modified := False;
  end
end;

procedure TRbwDynamicCursor.DrawDefaultCursor(const AndImage,
  XorImage: TBitMap);
var
  Width, Height, Size : integer;
  Center : integer;
  XY : integer;
begin
  Width := CursorWidth;
  Height := CursorHeight;
  if Width < Height then
  begin
    Size := Width;
  end
  else
  begin
    Size := Height;
  end;
  Center := Size div 2;
  Size := Center * 2;
  XY := 0;
  with AndImage do
  begin
    While XY < Center do
    begin
      Canvas.Ellipse(XY,XY,Size-1-XY,Size-1-XY);
      XY := XY + 3;
    end;
  end;
end;

function TRbwDynamicCursor.GetCursor: TCursor;
begin
  DrawCursor;
  result := FCursor;
end;

function TRbwDynamicCursor.GetCursorHeight: integer;
begin
  result := GetSystemMetrics(SM_CYCURSOR);
  if result = 0 then RaiseLastOSError;
end;

function TRbwDynamicCursor.GetCursorWidth: integer;
begin
  result := GetSystemMetrics(SM_CXCURSOR);
  if result = 0 then RaiseLastOSError;
end;

procedure TRbwDynamicCursor.RedrawCursor;
begin
  Modified := True;
  DrawCursor;
end;

procedure TRbwDynamicCursor.SetCursor(const Value: TCursor);
begin
  if (Value <= 0) and (Value >= -22) then
  begin
    raise ECustomCursorError.Create(IntToStr(Value)
      + ' conflicts with an existing cursor constant');
  end;
  if FCursor <> Value then
  begin
    FCursor := Value;
    Modified := True;
  end;
end;

procedure TRbwDynamicCursor.SetHotPointX(const Value: integer);
begin
  if FHotPointX <> Value then
  begin
    FHotPointX := Value;
    Modified := True;
  end;
end;

procedure TRbwDynamicCursor.SetHotPointY(const Value: integer);
begin
  if FHotPointY <> Value then
  begin
    FHotPointY := Value;
    Modified := True;
  end;
end;

procedure TRbwDynamicCursor.SetOnDrawCursor(const Value: TDrawCursor);
begin
  if @FOnDrawCursor <> @Value then
  begin
    FOnDrawCursor := Value;
    Modified := True;
  end;
end;

end.
 