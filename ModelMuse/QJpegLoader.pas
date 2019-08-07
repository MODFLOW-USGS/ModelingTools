{-------------------------------------------------------------------------------
 QJpegLoader.pas

 Copyright (c) 2004, Andreas Hausladen (Andreas.Hausladen@gmx.de)
 All rights reserved.

 Permission is hereby granted, free of charge, to any person obtaining a copy of
 this software and associated documentation files(the "Software"), to deal in
 the Software without restriction, including without limitation the rights
 to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 copies of the Software, and to permit persons to whom the Software is furnished
 to do so, subject to the following conditions:

 The above copyright notice and this permission notice shall be included in
 all copies or substantial portions of the Software.

 THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 THE SOFTWARE.
--------------------------------------------------------------------------------

Last Modified: $Date: 2004/09/02 11:54:17 $
}

unit QJpegLoader;

interface

uses
  {$IFDEf MSWINDOWS}
  Windows, ActiveX,
  {$ENDIF MSWINDOWS}
  SysUtils, Classes,
  Graphics;

type
  TJpegBitmap = class(TBitmap)
  public
    {$IFDEF MSWINDOWS}
    procedure LoadFromStream(Stream: TStream); override; // requires at least IE4 under Win95/NT4
    procedure SaveToStream(Stream: TStream); override;   // requires GDI+
    {$ENDIF MSWINDOWS}
  end;

implementation

{$IFDEF MSWINDOWS}
uses
  Consts;

procedure TJpegBitmap.LoadFromStream(Stream: TStream);
var
  Pic: IPicture;
  hBmp: Cardinal;
  BmpInfo: Windows.TBitmap;
  DestDC, DC: Windows.HDC;
begin
  Width := 0;
  Height := 0;

  if OleLoadPicture(TStreamAdapter.Create(Stream), 0, False, IPicture, Pic) <> S_OK then
    raise EInvalidGraphicOperation.Create(SUnknownImageFormat);

  Pic.get_Handle(hBmp);
  Windows.GetObject(hBmp, SizeOf(BmpInfo), @BmpInfo);

  if BmpInfo.bmBitsPixel <= 1 then
    PixelFormat := pf1bit
  else if BmpInfo.bmBitsPixel <= 8 then
    PixelFormat := pf8bit
  else
    PixelFormat := pf32bit;
  Width := BmpInfo.bmWidth;
  Height := BmpInfo.bmHeight;

  if (Width <> 0) and (Height <> 0) then
  begin
    Canvas.Start;
    try
      DestDC := Windows.HDC(QPainter_handle(Canvas.Handle));
      DC := Windows.CreateCompatibleDC(DestDC);
      Windows.SelectObject(DC, hBmp);
      Windows.BitBlt(DestDC, 0, 0, Width, Height, DC, 0, 0, Windows.SRCCOPY);
      Windows.DeleteDC(DC);
    finally
      Canvas.Stop;
    end;
    Format := 'JPEG';
  end;
end;

// some GDI+ types
type
  GPIMAGE = class(TObject);
  GPBITMAP = class(GPIMAGE);
  GPSTATUS = Integer;

  PImageCodecInfo = ^TImageCodecInfo;
  TImageCodecInfo = packed record
    Clsid: TGUID;
    FormatID: TGUID;
    CodecName: PWideChar;
    DllName: PWideChar;
    FormatDescription: PWideChar;
    FilenameExtension: PWideChar;
    MimeType: PWideChar;
    Flags: Cardinal;
    Version: Cardinal;
    SigCount: Cardinal;
    SigSize: Cardinal;
    SigPattern: PByte;
    SigMask: PByte;
  end;

  PGdiplusStartupInput = ^TGdiplusStartupInput;
  TGdiplusStartupInput = packed record
    GdiplusVersion: Cardinal;               // Must be 1
    DebugEventCallback: Pointer;
    SuppressBackgroundThread: BOOL;         // FALSE unless you're prepared to call
                                            // the hook/unhook functions properly
    SuppressExternalCodecs: BOOL;           // FALSE unless you want GDI+ only to use
  end;                                      // its internal image codecs.

  TGdiplusStartupProc = function(out token: ULONG; input: PGdiplusStartupInput;
    output: Pointer): GPSTATUS; stdcall;
  TGdiplusShutdownProc = procedure(token: ULONG); stdcall;
  TGdipSaveImageToStreamProc = function(image: GPIMAGE; stream: IStream;
    clsidEncoder: PGUID; encoderParams: Pointer = nil): GPSTATUS; stdcall;
  TGdipCreateBitmapFromHBITMAPProc = function(hbm: HBITMAP; hpal: HPALETTE;
    out bitmap: GPBITMAP): GPSTATUS; stdcall;
  TGdipDisposeImageProc = function(image: GPIMAGE): GPSTATUS; stdcall;
  TGdipGetImageEncodersSizeProc = function(out numEncoders: UINT;
    out size: Cardinal): GPSTATUS; stdcall;
  TGdipGetImageEncodersProc = function(numEncoders, size: Cardinal;
    encoders: PIMAGECODECINFO): GPSTATUS; stdcall;

var
  GdiplusStartup: TGdiplusStartupProc = nil;
  GdiplusShutdown: TGdiplusShutdownProc = nil;
  GdipSaveImageToStream: TGdipSaveImageToStreamProc = nil;
  GdipCreateBitmapFromHBITMAP: TGdipCreateBitmapFromHBITMAPProc = nil;
  GdipDisposeImage: TGdipDisposeImageProc = nil;
  GdipGetImageEncodersSize: TGdipGetImageEncodersSizeProc = nil;
  GdipGetImageEncoders: TGdipGetImageEncodersProc = nil;

function GetEncoderClsid(const ImageFormat: WideString; out Clsid: TGUID): Integer;
var
  Count, Size: Cardinal;
  Codecs: array of TImageCodecInfo;
begin
  Result := -1;
  Count := 0;
  Size := 0;
  GdipGetImageEncodersSize(Count, Size);
  if Count > 0 then
  begin
    SetLength(Codecs, Size div SizeOf(TImageCodecInfo) + 1);
    GdipGetImageEncoders(Count, Size, @Codecs[0]);
    for Result := 0 to Count - 1 do
    begin
      if Codecs[Result].MimeType = ImageFormat then
      begin
        Clsid := Codecs[Result].Clsid;
        Exit;
      end;
    end;
    Result := -1;
  end;
end;

procedure TJpegBitmap.SaveToStream(Stream: TStream);
var
  Lib: HMODULE;
  gdiplusToken: ULONG;
  gdiplusStartupInput: TGdiplusStartupInput;
  bitmap: GPBITMAP;
  jpgClsid: TGUID;
begin
  if Format = 'JPEG' then
  begin
    if not Assigned(GdiplusStartup) then
    begin
      Lib := GetModuleHandle('gdiplus.dll');
      if Lib = 0 then
      begin
        Lib := LoadLibrary('gdiplus.dll'); // will be freed on application termination
        if Lib = 0 then
          raise EInvalidGraphicOperation.Create(SUnableToWrite);
      end;
      GdiplusStartup := GetProcAddress(Lib, 'GdiplusStartup');
      GdiplusShutdown := GetProcAddress(Lib, 'GdiplusShutdown');
      GdipSaveImageToStream := GetProcAddress(Lib, 'GdipSaveImageToStream');
      GdipCreateBitmapFromHBITMAP := GetProcAddress(Lib, 'GdipCreateBitmapFromHBITMAP');
      GdipDisposeImage := GetProcAddress(Lib, 'GdipDisposeImage');
      GdipGetImageEncodersSize := GetProcAddress(Lib, 'GdipGetImageEncodersSize');
      GdipGetImageEncoders := GetProcAddress(Lib, 'GdipGetImageEncoders');
    end;
    FillChar(gdiplusStartupInput, SizeOf(gdiplusStartupInput), 0);
    gdiplusStartupInput.GdiplusVersion := 1;
    bitmap := nil;
    GdiplusStartup(gdiplusToken, @gdiplusStartupInput, nil);
    try
      GetEncoderClsid('image/jpeg', jpgClsid);
      GdipCreateBitmapFromHBITMAP(QPixmap_hbm(Handle), 0, bitmap);
      GdipSaveImageToStream(bitmap, TStreamAdapter.Create(Stream), @jpgClsid);
    finally
      if bitmap <> nil then
        GdipDisposeImage(bitmap);
      GdiplusShutdown(gdiplusToken);
    end;
  end
  else
  begin
    Format := 'BMP';
    try
      inherited SaveToStream(Stream);
    finally
      Format := 'JPEG';
    end;
  end;
end;

{ Hooks to allow the TBitmap to load jpegs like in Kylix and Delphi 6. }

function OrgQImageIO_read(handle: QImageIOH): Boolean; cdecl; external QtIntf name QtNamePrefix + 'QImageIO_read';
function OrgQImageIO_write(handle: QImageIOH): Boolean; cdecl; external QtIntf name QtNamePrefix + 'QImageIO_write';

type
  TBitmapPrivate = class(TGraphic)
  public {private}
    FPixelFormat: TPixelFormat;
    FTransparentMode: TTransparentMode;
    FImage: QImageH;
  end;


function HookedQImageIO_read(handle: QImageIOH): Boolean; cdecl;
var
  dev: QIODeviceH;
  Size: Cardinal;
  MemStream: TMemoryStream;
  JpegBitmap: TJpegBitmap;
begin
  if handle <> nil then
  begin
    dev := QImageIO_ioDevice(handle);
    Size := QIODevice_size(dev);
    if (Size > 0) and (StrIComp(QImageIO_format(handle), 'JPEG') = 0) then
    begin
      JpegBitmap := TJpegBitmap.Create;
      try
        MemStream := TMemoryStream.Create;
        try
          MemStream.SetSize(Size);
          QIODevice_readBlock(dev, MemStream.Memory, Size);
          JpegBitmap.LoadFromStream(MemStream);
        finally
          MemStream.Free;
        end;

        JpegBitmap.ImageNeeded;
        QImage_copy(TBitmapPrivate(JpegBitmap).FImage, QImageIO_image(handle));
        Result := True;
      finally
        JpegBitmap.Free;
      end;
      Exit;
    end;
  end;
  try
    Result := OrgQImageIO_read(handle);
  except
    Result := False;
  end;
end;

function HookedQImageIO_write(handle: QImageIOH): Boolean; cdecl;
var
  dev: QIODeviceH;
  MemStream: TMemoryStream;
  JpegBitmap: TJpegBitmap;
begin
  if handle <> nil then
  begin
    dev := QImageIO_ioDevice(handle);
    if StrIComp(QImageIO_format(handle), 'JPEG') = 0 then
    begin
      JpegBitmap := TJpegBitmap.Create;
      try
        JpegBitmap.Format := 'JPEG';
        TBitmapPrivate(JpegBitmap).FImage := QImage_create(QImageIO_image(handle));
        JpegBitmap.HandleNeeded;

        MemStream := TMemoryStream.Create;
        try
          JpegBitmap.SaveToStream(MemStream);
          QIODevice_writeBlock(dev, MemStream.Memory, MemStream.Size);
        finally
          MemStream.Free;
        end;

        Result := True;
      finally
        JpegBitmap.Free;
      end;
      Exit;
    end;
  end;
  try
    Result := OrgQImageIO_write(handle);
  except
    Result := False;
  end;
end;


type
  PJumpEntry = ^TJumpEntry;
  TJumpEntry = packed record
    Jmp: Byte;
    Address: Integer;
  end;

var
  OrgRead: Integer;
  OrgWrite: Integer;

procedure HookImageIO;
var
  P: PJumpEntry;
  OldProt: Cardinal;
begin
  P := @Qt.QImageIO_read;
  VirtualProtect(P, 4096, PAGE_EXECUTE_READWRITE, OldProt);
  try
    P^.Jmp := $E9;
    OrgRead := P^.Address;
    P^.Address := Cardinal(@HookedQImageIO_read) - Cardinal(P) - SizeOf(TJumpEntry);
  finally
    VirtualProtect(P, 4096, OldProt, nil);
  end;

  P := @Qt.QImageIO_write;
  VirtualProtect(P, 4096, PAGE_EXECUTE_READWRITE, OldProt);
  try
    P^.Jmp := $E9;
    OrgWrite := P^.Address;
    P^.Address := Cardinal(@HookedQImageIO_write) - Cardinal(P) - SizeOf(TJumpEntry);
  finally
    VirtualProtect(P, 4096, OldProt, nil);
  end;
end;

procedure UnhookImageIO;
var
  P: PJumpEntry;
  OldProt: Cardinal;
begin
  P := @Qt.QImageIO_read;
  VirtualProtect(P, 4096, PAGE_EXECUTE_READWRITE, OldProt);
  try
    P^.Jmp := $FF;
    P^.Address := OrgRead;
  finally
    VirtualProtect(P, 4096, OldProt, nil);
  end;

  P := @Qt.QImageIO_write;
  VirtualProtect(P, 4096, PAGE_EXECUTE_READWRITE, OldProt);
  try
    P^.Jmp := $FF;
    P^.Address := OrgWrite;
  finally
    VirtualProtect(P, 4096, OldProt, nil);
  end;
end;

initialization
  if GetFileFormats.FindExt('jpg') = nil then
    TPicture.RegisterFileFormat('jpg', 'JPEG', TJpegBitmap);
  if GetFileFormats.FindExt('jpeg') = nil then
    TPicture.RegisterFileFormat('jpeg', 'JPEG', TJpegBitmap);

  HookImageIO;

finalization
  UnhookImageIO;

  TPicture.UnregisterGraphicClass(TJpegBitmap);

{$ENDIF MSWINDOWS}

{
Histroy:

$Log: QJpegLoader.pas,v $
Revision 1.2  2004/09/02 11:54:17  Andreas
Fixed Write bug
Added QImageIO_read/write hooks for TBitmap.LoadFromStream/SaveToStream


}

end.
 
