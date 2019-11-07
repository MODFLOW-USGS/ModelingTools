
{*********************************************}
{  This unit is a part of OfficeVCL library   }
{  Copyright © 1998-2001 Evgeny A. Kryukov    }
{  See License.txt for licence information    }
{                                             }
{  http://www.eksoftware.org                  } 
{  evgeny@eksoftware.org                      }
{                                             }
{*********************************************}

unit OfficeUtils;

{$I OFFICEVER.INC}
{$P+,S-,W-,R-,H+}
{$C PRELOAD}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, OfficeTypes;

function PointInRect(P: TPoint; R: TRect): boolean;

{ region functions }

function CreateRegionFromBitmap(hBmp: TBitmap; TransColor: TColor): HRGN;

{ draw functions }

procedure CopyParentImage(Control: TControl; Dest: TCanvas);

procedure PaintBackground(Control: TWinControl; Canvas: TCanvas);

procedure DrawBorder(Canvas: TCanvas; Rect: TRect; Style: TksoBorderStyle);
procedure DrawButton(Canvas: TCanvas; Rect: TRect; Flat, Focused, Down: boolean);

function GetAntialiasedBitmap(const Bitmap: TBitmap): TBitmap;

implementation {===============================================================}

function PointInRect(P: TPoint; R: TRect): boolean;
begin
  Result := (P.X >= R.Left) and (P.X <= R.right) and
     (P.Y >= R.Top) and (P.Y <= R.bottom);
end;

type
  TParentControl = class(TWinControl);

procedure PaintBackground(Control: TWinControl; Canvas: TCanvas);
var
  B: TBitmap;
begin
  if Assigned(Control) and Assigned(Control.Parent) then
  begin
    B := TBitmap.Create;
    try
      B.Width := TParentControl(Control.Parent).Width;
      B.Height := TParentControl(Control.Parent).Height;

      SendMessage(Control.Parent.Handle, WM_ERASEBKGND, B.Canvas.Handle, 0);
      TParentControl(Control.Parent).PaintControls(B.Canvas.Handle, nil);

      { Paint to DC }
      Canvas.Draw(-Control.Left, -Control.Top, B); 
    finally
      B.Free;
    end;
  end;
end;

procedure CopyParentImage(Control: TControl; Dest: TCanvas);
var
  I, Count, X, Y, SaveIndex: Integer;
  DC: HDC;
  R, SelfR, CtlR: TRect;
begin
  if Control.Parent = nil then Exit;
  Count := Control.Parent.ControlCount;
  DC := Dest.Handle;
  SelfR := Bounds(Control.Left, Control.Top, Control.Width, Control.Height);
  X := -Control.Left; Y := -Control.Top;
  // Copy parent control image
  SaveIndex := SaveDC(DC);
  SetViewportOrgEx(DC, X, Y, nil);
  IntersectClipRect(DC, 0, 0, Control.Parent.ClientWidth,
  Control.Parent.ClientHeight);
  TParentControl(Control.Parent).PaintWindow(DC);
  RestoreDC(DC, SaveIndex);
  // Copy images of graphic controls
  for I := 0 to Count - 1 do
  begin
    if (Control.Parent.Controls[I] <> nil) and
      not (Control.Parent.Controls[I] is TWinControl) then
    begin
      if Control.Parent.Controls[I] = Control then Break;
      with Control.Parent.Controls[I] do
      begin
        CtlR := Bounds(Left, Top, Width, Height);
        if Bool(IntersectRect(R, SelfR, CtlR)) and Visible then
        begin
          SaveIndex := SaveDC(DC);
          SetViewportOrgEx(DC, Left + X, Top + Y, nil);
          IntersectClipRect(DC, 0, 0, Width, Height);
          Perform(WM_PAINT, DC, 0);
          RestoreDC(DC, SaveIndex);
        end;
      end;
    end;
  end;
end;


{ draw funtions }

procedure DrawBorder(Canvas: TCanvas; Rect: TRect; Style: TksoBorderStyle);
begin
  case Style of
    kbsNone: ;
    kbsFlat:
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid; 
      Canvas.Pen.Color := clBtnShadow;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom); 
    end;
    kbsSingle:
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
    end;
    kbsSolid:
    begin
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
      InflateRect(Rect, -1, -1);
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
    end;
    kbsEtched:
    begin
      DrawEdge(Canvas.Handle, Rect, EDGE_ETCHED, BF_RECT);
    end;
    kbsBump:
    begin
      DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_RECT);
      InflateRect(Rect, -1, -1);
      DrawEdge(Canvas.Handle, Rect, BDR_SUNKENOUTER, BF_RECT);
    end;
    kbsSunken:
    begin
      DrawEdge(Canvas.Handle, Rect, EDGE_SUNKEN, BF_RECT);
    end;
    kbsRaised:
    begin
      DrawEdge(Canvas.Handle, Rect, EDGE_RAISED, BF_RECT);
    end;
    kbsDown:
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clWindowFrame;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
      InflateRect(Rect, -1, -1);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
    end;
    kbsUp:
    begin
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Style := psSolid;

      Canvas.Pen.Color := clWindowFrame;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
      Canvas.Pen.Color := clBtnHighlight;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right-1, Rect.bottom-1);
      InflateRect(Rect, -1, -1);
      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
      InflateRect(Rect, -1, -1);
      Canvas.Pen.Color := clBtnShadow;
      Canvas.MoveTo(Rect.left-1, Rect.bottom);
      Canvas.LineTo(Rect.right, Rect.bottom);
      Canvas.LineTo(Rect.right, Rect.top-2);
    end;
    kbsOuterRaised:
    begin
      DrawEdge(Canvas.Handle, Rect, BDR_RAISEDINNER, BF_RECT);
      InflateRect(Rect, -1, -1);
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
    end;
    kbsOuterSunken:
    begin
      DrawEdge(Canvas.Handle, Rect, BDR_SUNKENOUTER, BF_RECT);
      InflateRect(Rect, -1, -1);
      Canvas.Pen.Style := psSolid;
      Canvas.Pen.Color := clBtnFace;
      Canvas.Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
    end;
  end;
end;

procedure DrawButton(Canvas: TCanvas; Rect: TRect; Flat, Focused, Down: boolean);
begin
  with Canvas do
  begin
    if Flat then
    begin
      { Draw flat }
      if not Down then
        if not Focused then
        begin
          { Flat nofocused }
          Pen.Style := psSolid;
          Pen.Color := clBtnHighlight;
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
          Brush.Style := bsClear;
          Rectangle(Rect.left+1, Rect.top, Rect.right, Rect.bottom);
        end
        else
        begin
          { Flat focused }
          Pen.Style := psClear;
          Brush.Style := bsSolid;
          Brush.Color := clBtnFace;
          Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom+1);
          Inc(Rect.Left);
          DrawEdge(Handle, Rect, BDR_RAISEDINNER, BF_RECT);
        end
      else
      begin
        { Flat down }
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := clBtnFace;
        Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom+1);
        Inc(Rect.Left);
        DrawEdge(Handle, Rect, BDR_SUNKENOUTER, BF_RECT);
      end;
    end
    else
    begin
      { Normal draw }
      if not Down then
      begin
        Pen.Style := psClear;
        Brush.Style := bsSolid;
        Brush.Color := clBtnFace;
        Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
        DrawEdge(Handle, Rect, BDR_RAISEDOUTER or BDR_RAISEDINNER, BF_RECT)
      end
      else
      begin
        Pen.Style := psSolid;
        Pen.Color := clBtnShadow;
        Brush.Style := bsSolid;
        Brush.Color := clBtnFace;
        Rectangle(Rect.left, Rect.top, Rect.right, Rect.bottom);
      end;
    end;
  end;
end;

function GetAntialiasedBitmap(const Bitmap: TBitmap): TBitmap;
type
  TByteArray = array [0..MaxLongint - 1] of Byte;
  PByteArray = ^TByteArray;
var
  Antialias: TBitmap;
  X, Y: Integer;
  Line1, Line2, Line: PByteArray;
begin
 Assert(Bitmap <> nil);
 if Bitmap.PixelFormat <> pf24bit then
   Bitmap.PixelFormat := pf24bit;
 Antialias := TBitmap.Create;
 with Bitmap do
 begin
   Antialias.PixelFormat := pf24bit;
   Antialias.Width := Width div 2;
   Antialias.Height := Height div 2;
   for Y := 0 to Antialias.Height - 1 do
   begin
     Line1 := ScanLine[Y * 2];
     Line2 := ScanLine[Y * 2 + 1];
     Line := Antialias.ScanLine[Y];
     for X := 0 to Antialias.Width - 1 do
     begin
       Line[X * 3] := (Integer(Line1[X * 6]) + Integer(Line2[X * 6]) +
         Integer(Line1[X * 6 + 3]) + Integer(Line2[X * 6 + 3])) div 4;
       Line[X * 3 + 1] := (Integer(Line1[X * 6 + 1]) + Integer(Line2[X * 6 + 1]) +
         Integer(Line1[X * 6 + 3 + 1]) + Integer(Line2[X * 6 + 3 + 1])) div 4;
       Line[X * 3 + 2] := (Integer(Line1[X * 6 + 2]) + Integer(Line2[X * 6 + 2]) +
         Integer(Line1[X * 6 + 3 + 2]) + Integer(Line2[X * 6 + 3 + 2])) div 4;
     end;
    end;
  end;
  Result := Antialias;
end;

function MinByte(B1, B2: byte): byte;
begin
     if B1 < B2 then
        Result := B1
     else
         Result := B2;
end;

function CreateRegionFromBitmap(hBmp: TBitmap; TransColor: TColor): HRGN;
const
  ALLOC_UNIT = 100;
  Tolerance = 0;
var
  MemDC, DC: HDC;
  BitmapInfo: TBitmapInfo;
  hbm32, holdBmp, holdMemBmp: HBitmap;
  pbits32 : Pointer;
  bm32 : BITMAP;
  maxRects: DWORD;
  hData: HGLOBAL;
  pData: PRgnData;
  b, LR, LG, LB, HR, HG, HB: Byte;
  p32: pByte;
  x, x0, y: integer;
  p: pLongInt;
  pr: PRect;
  h: HRGN;
begin
  Result := 0;
  if hBmp <> nil then
  begin
    { Create a memory DC inside which we will scan the bitmap contents }
    MemDC := CreateCompatibleDC(0);
    if MemDC <> 0 then
    begin
     { Create a 32 bits depth bitmap and select it into the memory DC }
      with BitmapInfo.bmiHeader do
      begin
        biSize          := sizeof(TBitmapInfoHeader);
        biWidth         := hBmp.Width;
        biHeight        := hBmp.Height;
        biPlanes        := 1;
        biBitCount      := 32;
        biCompression   := BI_RGB; { (0) uncompressed format }
        biSizeImage     := 0;
        biXPelsPerMeter := 0;
        biYPelsPerMeter := 0;
        biClrUsed       := 0;
        biClrImportant  := 0;
      end;
      hbm32 := CreateDIBSection(MemDC, BitmapInfo, DIB_RGB_COLORS, pbits32,0, 0);
      if hbm32 <> 0 then
      begin
        holdMemBmp := SelectObject(MemDC, hbm32);
        {
          Get how many bytes per row we have for the bitmap bits
          (rounded up to 32 bits)
        }
        GetObject(hbm32, SizeOf(bm32), @bm32);
        while (bm32.bmWidthBytes mod 4) > 0 do
          inc(bm32.bmWidthBytes);
        DC := CreateCompatibleDC(MemDC);
        { Copy the bitmap into the memory DC }
        holdBmp := SelectObject(DC, hBmp.Handle);
        BitBlt(MemDC, 0, 0, hBmp.Width, hBmp.Height, DC, 0, 0, SRCCOPY);
        {
          For better performances, we will use the ExtCreateRegion() function
          to create the region. This function take a RGNDATA structure on
          entry. We will add rectangles by
          amount of ALLOC_UNIT number in this structure
        }
        maxRects := ALLOC_UNIT;
        hData := GlobalAlloc(GMEM_MOVEABLE, sizeof(TRgnDataHeader) +
           SizeOf(TRect) * maxRects);
        pData := GlobalLock(hData);
        pData^.rdh.dwSize := SizeOf(TRgnDataHeader);
        pData^.rdh.iType := RDH_RECTANGLES;
        pData^.rdh.nCount := 0;
        pData^.rdh.nRgnSize := 0;
        SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
        { Keep on hand highest and lowest values for the "transparent" pixel }
        LR := GetRValue(ColorToRGB(TransColor));
        LG := GetGValue(ColorToRGB(TransColor));
        LB := GetBValue(ColorToRGB(TransColor));
        { Add the value of the tolerance to the "transparent" pixel value }
        HR := MinByte($FF, LR + GetRValue(ColorToRGB(Tolerance)));
        HG := MinByte($FF, LG + GetGValue(ColorToRGB(Tolerance)));
        HB := MinByte($FF, LB + GetBValue(ColorToRGB(Tolerance)));
        {
          Scan each bitmap row from bottom to top,
          the bitmap is inverted vertically
        }
        p32 := bm32.bmBits;
        inc(PChar(p32), (bm32.bmHeight - 1) * bm32.bmWidthBytes);
        for y := 0 to hBmp.Height-1 do
        begin
          { Scan each bitmap pixel from left to right }
          x := -1;
          while x+1 < hBmp.Width do
          begin
            inc(x);
            { Search for a continuous range of "non transparent pixels" }
            x0 := x;
            p := PLongInt(p32);
            inc(PChar(p), x * SizeOf(LongInt));
            while x < hBmp.Width do
            begin
              b := GetBValue(p^);                 // Changed from GetRValue(p^)
              if (b >= LR) and (b <= HR) then
              begin
                b := GetGValue(p^);               // Left alone
                if (b >= LG) and (b <= HG) then
                begin
                  b := GetRValue(p^);             // Changed from GetBValue(p^)
                  if (b >= LB) and (b <= hb) then
                    { This pixel is "transparent" }
                    break;
                end;
              end;
              inc(PChar(p), SizeOf(LongInt));
              inc(x);
            end;
            if x > x0 then
            begin
              {
                Add the pixels (x0, y) to (x, y+1) as a new rectangle in
                the region
              }
              if pData^.rdh.nCount >= maxRects then
              begin
                GlobalUnlock(hData);
                inc(maxRects, ALLOC_UNIT);
                hData := GlobalReAlloc(hData, SizeOf(TRgnDataHeader) +
                   SizeOf(TRect) * maxRects, GMEM_MOVEABLE);
                pData := GlobalLock(hData);
                Assert(pData <> NIL);
              end;
              pr := @pData^.Buffer[pData^.rdh.nCount * SizeOf(TRect)];
              SetRect(pr^, x0, y, x, y+1);
              if x0 < pData^.rdh.rcBound.Left then
                pData^.rdh.rcBound.Left := x0;
              if y < pData^.rdh.rcBound.Top then
                pData^.rdh.rcBound.Top := y;
              if x > pData^.rdh.rcBound.Right then
                pData^.rdh.rcBound.Left := x;
              if y+1 > pData^.rdh.rcBound.Bottom then
                pData^.rdh.rcBound.Bottom := y+1;
              inc(pData^.rdh.nCount);
              {
               On Windows98, ExtCreateRegion() may fail if the number of
               rectangles is too large (ie: > 4000). Therefore, we have to
               create the region by multiple steps
              }
              if pData^.rdh.nCount = 2000 then
              begin
                h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) +
                   (SizeOf(TRect) * maxRects), pData^);
                Assert(h <> 0);
                if Result <> 0 then
                begin
                  CombineRgn(Result, Result, h, RGN_OR);
                  DeleteObject(h);
                end else
                  Result := h;
                pData^.rdh.nCount := 0;
                SetRect(pData^.rdh.rcBound, MaxInt, MaxInt, 0, 0);
              end;
            end;
          end;
          {
            Go to next row (remember, the bitmap is inverted vertically)
            that is why we use DEC!
          }
          Dec(PChar(p32), bm32.bmWidthBytes);
        end;
        { Create or extend the region with the remaining rectangle }
        h := ExtCreateRegion(NIL, SizeOf(TRgnDataHeader) +
           (SizeOf(TRect) * maxRects), pData^);
        Assert(h <> 0);
        if Result <> 0 then
        begin
          CombineRgn(Result, Result, h, RGN_OR);
          DeleteObject(h);
        end else
          Result := h;
        { Clean up }
        GlobalFree(hData);
        SelectObject(DC, holdBmp);
        DeleteDC(DC);
        DeleteObject(SelectObject(MemDC, holdMemBmp));
      end;
    end;
    DeleteDC(MemDC);
  end;
end;

end.
