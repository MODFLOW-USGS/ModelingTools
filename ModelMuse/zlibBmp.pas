unit zlibBmp;

{ ----------------------------------------------------------------------------
    TZLIBBitmap
    ---
    The TZLIBBitmap is a replacement for Delphi's TBitmap. It implements a
    powerful data compression schema using filters and zlib's deflate
    method to achieve compression rates as good as those attained by converting
    your bitmaps to PNG image format. Only data streamed down to the DFM
    file gets compressed (i.e.: SaveToStream/File saves an ordinary Windows
    Bitmap). That makes the use of ZLIBBitmap completely transparent to the
    application, to the developer and to Delphi IDE.

    TZLIBBitmap can read .BMP files since it is actually a TBitmap and can also
    handle ZLib Compressed Bitmaps (see SaveMethod bellow). The default
    extension for ZLib Compressed Bitmaps is .ZBM.

    TZLIBBitmap inherits from TBitmap all its properties and methods,
    introducing the following methods and properties:

    - SavingMode: TSavingMode
      - Tells which file format to use when streaming down the bitmap data to
      mediums other than the DFM file:
         . fmBitmap -> Unmodified bitmap data (Windows Bitmap)
         . fmZBM    -> ZLib Compressed Bitmap data. If saved to a file the .ZBM
                       extension should be used.
      When streaming to the Delphi form file the class writes a ZBM formatted
      stream regardless of the SavingMode property.
      Defaults to fmBitmap for full compatibility with TBitmap.

    SaveToStream (overriden)
      - Depends on the value of the SavingMode property. If it is fmZBM it
      writes a ZBM formatted stream (This is the actual compressed stream that
      is written to the DFM). This can later be read by LoadFromStream because
      the TZLIBBitmap.LoadFromStream can handle both Windows Bitmaps and ZLIB
      Compressed Bitmaps transparently. This option is provided for
      developers that need to store the ZBM stream in mediums other than the
      DFM file (database blobs, communication streams, etc.)
      If SavingMode is fmBitmap, it will call the inherited TBitmap.SaveToStream
      which will save an ordinary bitmap stream.

    LoadFromStream (overriden)
      - Loads a ZBM or Bitmap formatted stream. It first tries to recognize the
      stream as a ZBM stream calling ReadData if successfull, otherwise it calls
      the inherited LoadFromStream, which will try to load a bitmap stream.

    FilterMethod: TFilterMethod
      - This property tells which filter method to apply when writting the
      ZBM stream. The default method (fmPaeth) works better with the vast
      majority of images, so there's little if any reason to change this, but
      some images can compress better with other filters. It is up to the
      developer to choose which filter to apply or let the default.

    FUTURE OPTIMIZATIONS
    --------------------
    I don't need to compress the data again if it hasn't changed. I only need
    to hold the compressed data and then stream it down when needed. If the
    bitmap gets changed, I will need to recompress it. All I have to do is to
    create a memory stream with the compressed data, releasing it if the bitmap
    changes. When writting data, if the compressed data is available, I write it
    straight, otherwise I make the compression procedure again.

    License
    -------
    This code is freeware and its use in free or commercial products is granted.
    The code is provided as is and no warranty is supplied. USE IT AT YOUR OWN
    RISK. Although free this code is copyrighted 2000 by Felipe Machado, you
    can't claim authorship of it for any reason.
    If you use this component please send me an e-mail. I'll be glad to know
    your impressions, suggestions and critics (see contact below)

    Contact (Bug reports, etc.)
    ---------------------------
    Questions, bug reports, comments, they are all welcome. Please send then to

    felipe.machado@mail.com

    I will try to respond as soon as possible to every request. Thanks for the
    interest.

    ---
    (C) 2000, Felipe Rocha Machado
---------------------------------------------------------------------------- }

interface

uses
  SysUtils, Classes, QGraphics, ZLIB;

type
  { redefined here so you won't be required to add ZLIB to the uses clause }
  TCompressionLevel = ZLIB.TCompressionLevel;

  { signature for ZBM file format }
  { #213 Z B M #13 #10 #26 #10 }
  TZBMSignature = Array[0..7] of char;

  { filtering methods }
  TFilterMethod = (fmNone, fmPaeth, fmSub, fmUp, fmAverage);

  { Saving Mode - standard bitmap stream or ZLIB compressed stream }
  TSavingMode = (smBitmap, smZBM);

  { small header used to control encoding/decoding }
  TZBMHeader = packed record
    Signature: TZBMSignature;
    FilterMethod: TFilterMethod; // filter applied to the bitmap scanlines
    TotalSize: Cardinal;         // uncompressed size
  end;

  { Defaults for every newly created ZLIB bitmap }
  TZBMDefaults = record
    SavingMode: TSavingMode;
    FilterMethod: TFilterMethod;
    CompressionLevel: TCompressionLevel; { clNone, clFastest, clDefault, clMax }
  end;

  { ZLIBBitmap class declaration }
  TZLIBBitmap = class(TBitmap)
  private
    FSavingMode: TSavingMode;
    function GetBpp: Byte;
  protected
    Header: TZBMHeader;
    procedure EncodeFilter(Dest: TBitmap);
    procedure DecodeFilter;
    procedure ReadData(Stream: TStream); override;
    procedure WriteData(Stream: TStream); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property FilterMethod: TFilterMethod read Header.FilterMethod
      write Header.FilterMethod;
    property SavingMode: TSavingMode read FSavingMode write FSavingMode;
  end;

var
  { Default behavior for ZLIB Bitmaps - mimics a class variable }
  ZBMDefaults: TZBMDefaults = (SavingMode: smBitmap;
                               FilterMethod: fmPaeth;
                               CompressionLevel: clMax); // this line was missing

implementation

type
  { array used to access memory by index }
  PHugeArray = ^THugeArray;
  THugeArray = Array[0..MaxLongInt div SizeOf(Byte) div 8 - 1] of Byte;

const
  ZBMSignature: TZBMSignature = #213'ZBM'#13#10#26#10;

{ TZLIBBitmap }

constructor TZLIBBitmap.Create;
begin
  inherited Create;
  FilterMethod := ZBMDefaults.FilterMethod;
  FSavingMode := ZBMDefaults.SavingMode;
end;

procedure TZLIBBitmap.DecodeFilter;
var
  src, prev: PHugeArray;
  srcdif, size: Cardinal;
  bpp: Byte;
  zerobuffer: Pointer;
  { --- Local procedures for speed/readability --- }
  procedure DecNone; // should never be called since no change is done to scanlines
  begin
    // nothing to do - already decoded
  end;
  procedure DecPaeth;
  var
    x, y: Integer;
    a, b, c, pa, pb, pc, p: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      { first pixel is special case }
      for x := 0 to bpp-1 do
        src[x] := (src[x] + prev[x]) AND $FF;
      { now do paeth for every other byte }
      for x := bpp to size-1 do
      begin
        a := src[x-bpp];
        b := prev[x];
        c := prev[x-bpp];
        p := b - c;
        pc := a - c;
        pa := Abs(p);
        pb := Abs(pc);
        pc := Abs(p + pc);
        if (pa <= pb) AND (pa <= pc) then p := a else
        if (pb <= pc) then p := b
                      else p := c;
        src[x] := (src[x] + p) AND $FF;
      end;
      prev := src;
      Inc(Integer(src), srcdif);
    end;
  end;
  procedure DecSub;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      for x := bpp to size-1 do
        src[x] := (src[x] + src[x-bpp]) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
    end;
  end;
  procedure DecUp;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      for x := 0 to size-1 do
        src[x] := (src[x] + prev[x]) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
    end;
  end;
  procedure DecAverage;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      { first pixel is special case }
       for x := 0 to bpp-1 do
         src[x] := (src[x] + prev[x] div 2) AND $FF;
       for x := Bpp to size-1 do
         src[x] := (src[x] + (src[x-bpp]+prev[x]) div 2) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
    end;
  end;

begin
  bpp := GetBpp;
  size := bpp * Width;
  if bpp = 1 then
    case PixelFormat of
      pf1bit: size := size div 8;
      pf4bit: size := size div 2;
    end;
  src := ScanLine[0];
  if Height > 1 then srcdif := Integer(ScanLine[1]) - Integer(src)
                else srcdif := 0;
  GetMem(zerobuffer, size);
  try
    FillChar(zerobuffer^,size,0);
    prev := zerobuffer;
    case Header.FilterMethod of
      fmNone:    DecNone;
      fmPaeth:   DecPaeth;
      fmSub:     DecSub;
      fmUp:      DecUp;
      fmAverage: DecAverage;
    end;
  finally
    FreeMem(zerobuffer);
  end;
end;

procedure TZLIBBitmap.EncodeFilter(Dest: TBitmap);
var
  src, dst, prev: PHugeArray;
  srcdif, dstdif: Cardinal;
  size: Cardinal;
  bpp: Byte;
  zerobuffer: Pointer;
//  a, b, c, pa, pb, pc: Byte;
  { local functions for speed and readability }
  procedure DoNone; // should never be called since no change is done to scanlines
  var
    y: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      Move(src^, dst^, size);
      prev := src;
      Inc(Integer(src), srcdif);
      Inc(Integer(dst), dstdif);
    end;
  end;
  procedure DoPaeth;
  var
    y, x: Integer;
    a, b, c, p, pa, pb, pc: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      { first pixel is special case }
      for x := 0 to bpp-1 do
        dst[x] := (src[x] - prev[x]) AND $FF;
      (* Paeth(x) = Raw(x) - PaethPredictor(Raw(x-bpp), Prior(x), Prior(x-bpp)) *)
      // a = left, b = above, c = upper left
      for x := bpp to size - 1 do
      begin
        a := src[x-bpp];
        b := prev[x];
        c := prev[x-bpp];
        p := a + b - c;        // initial estimate
        pa := abs(p - a);      // distances to a, b, c
        pb := abs(p - b);
        pc := abs(p - c);
        { return nearest of a,b,c,
          breaking ties in order a,b,c. }
        if (pa <= pb) AND (pa <= pc) then p := a
        else if (pb <= pc) then p := b
        else p := c;
        dst[x] := (src[x] - p) AND $FF;
      end;
      prev := src;
      Inc(Integer(src), srcdif);
      Inc(Integer(dst), dstdif);
    end;
  end;
  procedure DoSub;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      for x := bpp to size-1 do
        dst[x] := (src[x] - src[x-bpp]) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
      Inc(Integer(dst), dstdif);
    end;
  end;
  procedure DoUp;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      for x := 0 to size-1 do
        dst[x] := (src[x] - prev[x]) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
      Inc(Integer(dst), dstdif);
    end;
  end;
  procedure DoAverage;
  var
    y, x: Integer;
  begin
    for y := 0 to Height - 1 do
    begin
      { first pixel is special case }
       for x := 0 to bpp-1 do
         dst[x] := (src[x] - prev[x] div 2) AND $FF;
       for x := Bpp to size-1 do
         dst[x] := (src[x] - (src[x-bpp]+prev[x]) div 2) AND $FF;
      prev := src;
      Inc(Integer(src), srcdif);
      Inc(Integer(dst), dstdif);
    end;
  end;

begin
  bpp := GetBpp;
  size := bpp * Width;
  if bpp = 1 then
    case PixelFormat of
      pf1bit: size := size div 8;
      pf4bit: size := size div 2;
    end;
  src := ScanLine[0];
  dst := Dest.ScanLine[0];
  if Height > 1 then
  begin
    srcdif := Integer(ScanLine[1]) - Integer(src);
    dstdif := Integer(Dest.ScanLine[1]) - Integer(dst);
  end
  else
  begin
    srcdif := 0;
    dstdif := 0;
  end;
  GetMem(zerobuffer, size);
  try
    FillChar(zerobuffer^,size,0);
    prev := zerobuffer;
    case Header.FilterMethod of
      fmNone   : DoNone;
      fmPaeth  : DoPaeth;
      fmSub    : DoSub;
      fmUp     : DoUp;
      fmAverage: DoAverage;
    end;
  finally
    FreeMem(zerobuffer);
  end;
end;

function TZLIBBitmap.GetBpp: Byte;
begin
  case PixelFormat of
    pf15bit,
    pf16bit: Result := 2;
    pf24bit: Result := 3;
    pf32bit: Result := 4;
  else
    Result := 1;
  end;
end;

procedure TZLIBBitmap.LoadFromStream(Stream: TStream);
var
  p: Integer;
begin
  p := Stream.Position;
  Stream.Read(Header, SizeOf(Header));
  Stream.Position := p;
  if Header.Signature <> ZBMSignature then // not a ZBM stream ...
    inherited LoadFromStream(Stream)       // ... try reading a Bitmap stream
  else
    ReadData(Stream);                      // it's a ZBM stream. Read it.
  FilterMethod := ZBMDefaults.FilterMethod;
end;

procedure TZLIBBitmap.ReadData(Stream: TStream);
var
  DecStream: TDecompressionStream;
  tmpStream: TMemoryStream;
begin
  tmpStream := TMemoryStream.Create;
  try
    Stream.Read(Header, SizeOf(Header));
    if Header.Signature <> ZBMSignature then
      raise EInvalidGraphic.Create('Invalid ZBM signature!');
    DecStream := TDecompressionStream.Create(Stream);
    try
      tmpStream.CopyFrom(DecStream, Header.TotalSize);
    finally
      DecStream.Free;
    end;
    tmpStream.Position := 0;
    inherited LoadFromStream(tmpStream);
    if Header.FilterMethod <> fmNone then DecodeFilter;
  finally
    tmpStream.Free;
  end;
end;

procedure TZLIBBitmap.SaveToStream(Stream: TStream);
begin
  if SavingMode = smBitmap then Inherited SaveToStream(Stream)
                           else WriteData(Stream);
end;

procedure TZLIBBitmap.WriteData(Stream: TStream);
var
  CmpStream: TCOMPRESSIONSTREAM;
  tmpStream: TMemoryStream;
  bmp: TBitmap;
begin
  tmpStream := TMemoryStream.Create;
  try
    CmpStream := TCOMPRESSIONSTREAM.Create(ZBMDefaults.CompressionLevel, tmpStream);
    try
      if Header.FilterMethod = fmNone then
        inherited SaveToStream(CmpStream) // compresses unmodified bitmap
      else
      begin
        bmp := TBitmap.Create;
        try
          bmp.Assign(Self);
          EncodeFilter(bmp);
          bmp.SaveToStream(CmpStream); // compresses filtered bitmap 
        finally
          bmp.Free;
        end;
      end;
      Header.TotalSize := CmpStream.Position;
    finally
      CmpStream.Free; // this will flush pending data to tmpStream
    end;
    tmpStream.Position := 0;
    Header.Signature := ZBMSignature;
    Stream.Write(Header, SizeOf(Header)); // writes ZBM header
    Stream.WriteBuffer(tmpStream.Memory^, tmpStream.Size);
  finally
    tmpStream.Free;
  end;
end;

initialization
  RegisterClass(TZLIBBitmap);
{  TPicture.UnregisterGraphicClass(TBitmap);
  TPicture.RegisterFileFormat('bmp', 'Windows Bitmaps (TZLIBBitmap)', TZLIBBitmap);
  TPicture.RegisterFileFormat('zbm', 'ZLIB Compressed Bitmap', TZLIBBitmap);
finalization
  TPicture.UnregisterGraphicClass(TZLIBBitmap);
  TPicture.RegisterFileFormat('bmp', 'Windows Bitmaps', TBitmap);  }
end.

