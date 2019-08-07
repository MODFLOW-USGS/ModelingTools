unit CombineBitmapsUnit;

interface

uses Types, Classes, QGraphics;

type
  TBitmapArray = array of TBitmap;

procedure CombineBitMaps(const Sources : array of TBitmap;
  const Destination : TBitMap; const TransparentColor : TColor); overload;

procedure CombineBitMaps(const Sources : TBitmapArray;
  const Destination : TBitMap; const TransparentColor : TColor); overload;

procedure CombineBitMaps(const Sources : TList;
  const Destination : TBitMap; const TransparentColor : TColor); overload;

implementation

procedure CombineBitMaps(const Sources : array of TBitmap;
  const Destination : TBitMap; const TransparentColor : TColor); overload;
var
  SourceIndex : integer;
  ABitMap : TBitmap;
  ARect : TRect;
begin
  ARect.Left := 0;
  ARect.Top := 0;
  ARect.Right := Destination.Width-1 ;
  ARect.Bottom := Destination.Height-1 ;

  Destination.Canvas.Brush.Color := TransparentColor;
  Destination.Canvas.FillRect(ARect);

  for SourceIndex := 0 to Length(Sources) -1 do
  begin
    ABitMap := Sources[SourceIndex];
    ABitMap.TransparentColor := TransparentColor;
    Destination.Canvas.Draw(0,0,ABitMap);
  end;
  Destination.TransparentColor := TransparentColor;
end;

procedure CombineBitMaps(const Sources : TBitmapArray;
  const Destination : TBitMap; const TransparentColor : TColor); overload;
var
  SourceIndex : integer;
  ABitMap : TBitmap;
  ARect : TRect;
begin
  ARect.Left := 0;
  ARect.Top := 0;
  ARect.Right := Destination.Width-1 ;
  ARect.Bottom := Destination.Height-1 ;

  Destination.Canvas.Brush.Color := TransparentColor;
  Destination.Canvas.FillRect(ARect);

  for SourceIndex := 0 to Length(Sources) -1 do
  begin
    ABitMap := Sources[SourceIndex];
    ABitMap.TransparentColor := TransparentColor;
//    Destination.Canvas.StretchDraw(ARect,ABitMap);
    Destination.Canvas.Draw(0,0,ABitMap);
  end;
  Destination.TransparentColor := TransparentColor;
end;

procedure CombineBitMaps(const Sources : TList;
  const Destination : TBitMap; const TransparentColor : TColor); overload;
var
  SourceIndex : integer;
  ABitMap : TBitmap;
  ARect : TRect;
begin
  ARect.Left := 0;
  ARect.Top := 0;
  ARect.Right := Destination.Width-1 ;
  ARect.Bottom := Destination.Height-1 ;

  Destination.Canvas.Brush.Color := TransparentColor;
  Destination.Canvas.FillRect(ARect);

  for SourceIndex := 0 to Sources.Count -1 do
  begin
    ABitMap := Sources[SourceIndex];
    ABitMap.TransparentColor := TransparentColor;
//    Destination.Canvas.StretchDraw(ARect,ABitMap);
    Destination.Canvas.Draw(0,0,ABitMap);
  end;
  Destination.TransparentColor := TransparentColor;
end;

end.
