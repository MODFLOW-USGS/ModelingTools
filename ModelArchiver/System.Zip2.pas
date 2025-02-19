{*******************************************************}
{ Utility for creating and extracting Zip Files         }
{                                                       }
{See .ZIP File Format Specification at                  }
{http://www.pkware.com/documents/casestudies/APPNOTE.TXT}
{for more information on the .ZIP File Format.          }
{                                                       }
{ Support for Compression modes 0(store) and 8(deflate) }
{ Are implemented in this unit.                         }
{*******************************************************}

                        

unit System.Zip2;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Classes,
  System.Zip.Common;

type
  /// <summary> Zip Compression Method Enumeration </summary>
  TZipCompression = (
    zcStored    = 0,
    zcShrunk,
    zcReduce1,
    zcReduce2,
    zcReduce3,
    zcReduce4,
    zcImplode,
    zcTokenize,
    zcDeflate,
    zcDeflate64,
    zcPKImplode,
    {11 RESERVED}
    zcBZIP2    = 12,
    {13 RESERVED}
    zcLZMA     = 14,
    {15-17 RESERVED}
    zcTERSE    = 18,
    zcLZ77,
    zcWavePack = 97,
    zcPPMdI1
  );

/// <summary> Converts ZIP compression method value to string </summary>
function TZipCompressionToString(Compression: TZipCompression): string;

const
  SIGNATURE_ZIPENDOFHEADER: UInt32 = $06054B50;
  SIGNATURE_CENTRALHEADER:  UInt32 = $02014B50;
  SIGNATURE_LOCALHEADER:    UInt32 = $04034B50;
  SIGNATURE_ZIP64_ENDOFCENTRALDIRECTORY: UInt32 = $06064B50;
  SIGNATURE_ZIP64_ENDOFCENTRALDIRECTORYLOCATOR: UInt32 = $07064B50;

  LOCALHEADERSIZE = 26;
  CENTRALHEADERSIZE = 42;

  ZIP_Version20 = 20;

  EXTRAFIELD_ID_ZIP64: UInt16 = $0001;
  EXTRAFIELD_ID_NTFS: UInt16  = $000A;

  ZIP64 = $FFFFFFFF;

type
  /// <summary> Final block written to zip file</summary>
  TZipEndOfCentralHeader = packed record
    DiskNumber:          UInt16;
    CentralDirStartDisk: UInt16;
    NumEntriesThisDisk:  UInt16;
    CentralDirEntries:   UInt16;
    CentralDirSize:      UInt32;
    CentralDirOffset:    UInt32;
    CommentLength:       UInt16;
    {Comment: RawByteString}
  end;
  /// <summary> TZipHeader contains information about a file in a zip archive.
  /// </summary>
  /// <remarks>
  /// <para>
  /// This record is overloaded for use in reading/writing ZIP
  /// [Local file header] and the Central Directory's [file header].
  /// </para>
  /// <para> See PKZIP Application Note section V. General Format of a .ZIP file
  ///  sub section J. Explanation of fields for more detailed description
  //   of each field's usage.
  /// </para>
  /// </remarks>
  TZipHeader = packed record
    MadeByVersion:      UInt16; // Start of Central Header
    RequiredVersion:    UInt16; // Start of Local Header
    Flag:               UInt16;
    CompressionMethod:  UInt16;
    ModifiedDateTime:   UInt32;
    CRC32:              UInt32;
    CompressedSize:     UInt32;
    UncompressedSize:   UInt32;
    FileNameLength:     UInt16;
    ExtraFieldLength:   UInt16; // End of Local Header
    FileCommentLength:  UInt16;
    DiskNumberStart:    UInt16;
    InternalAttributes: UInt16;
    ExternalAttributes: UInt32;
    LocalHeaderOffset:  UInt32; // End of Central Header
    FileName: TBytes;
    ExtraField: TBytes;
    FileComment: TBytes;
    function IsZIP64: Boolean;
    function GetZIP64_CompressedSize: UInt64;
    procedure SetZIP64_CompressedSize(const Value: UInt64);
    function GetZIP64_UncompressedSize: UInt64;
    procedure SetZIP64_UncompressedSize(const Value: UInt64);
    procedure SetExtraField_NTFS(const aFileName: string);
    property ZIP64_CompressedSize: UInt64 read GetZIP64_CompressedSize write
        SetZIP64_CompressedSize;
    property ZIP64_UncompressedSize: UInt64 read GetZIP64_UncompressedSize write
        SetZIP64_UncompressedSize;
  end;

  PZipHeader = ^TZipHeader;

  TZipExtraField = packed record
    HeaderID: UInt16;
    DataSize: UInt16;
    Data: TBytes;
    constructor Create(const aRawData: TBytes); overload;
    constructor Create(const aHeaderID: UInt16; const aData: TBytes); overload;
    procedure SetData(const A: TBytes);
    class operator Implicit(const A: TZipExtraField): TBytes;
  end;

  TZipExtraField_ZIP64 = packed record
    UncompressedSize : UInt64;
    CompressedSize   : UInt64;
    class operator Implicit(const aBytes: TBytes): TZipExtraField_ZIP64;
    class operator Implicit(const A: TZipExtraField_ZIP64): TZipExtraField;
    class operator Implicit(const A: TZipExtraField_ZIP64): TBytes;
    class operator Implicit(const A: TZipExtraField): TZipExtraField_ZIP64;
  end;

  {$ifdef MSWINDOWS}
  TZipExtraField_NTFS = packed record
    Reserved : UInt32;
    Tag1     : UInt16; // 0x0001
    Size1    : UInt16;
    MTime    : UInt64;
    ATime    : UInt64;
    CTime    : UInt64;
    constructor Create(const aFileName: string);
    class operator Implicit(const aBytes: TBytes): TZipExtraField_NTFS;
    class operator Implicit(const A: TZipExtraField_NTFS): TBytes;
    class operator Implicit(const A: TZipExtraField_NTFS): TZipExtraField;
    class operator Implicit(const A: TZipExtraField): TZipExtraField_NTFS;
  end;
  {$endif}

  TZipExtraFields = packed record
    Items: TArray<TZipExtraField>;
    function Get(const aHeaderID: UInt16; out aItem: TZipExtraField; out Index:
        NativeInt): Boolean;
    function New: NativeInt; overload;
    procedure Add(const A: TZipExtraField);
    class operator Implicit(const aBytes: TBytes): TZipExtraFields;
    class operator Implicit(const A: TZipExtraFields): TBytes;
    class operator Implicit(const A: TZipExtraFields): TZipExtraField_ZIP64;
  end;

  TZip64_EndOfCentralDirectory = packed record
    Signature               : UInt32; // $06064b50
    RecordSize              : UInt64;
    VersionMadeBy           : UInt16;
    VersionNeededToExtract  : UInt16;
    DiskNumber              : UInt32;
    StartDiskNumber         : UInt32;
    EntriesOnDisk           : UInt64;
    TotalEntries            : UInt64;
    DirectorySize           : UInt64;
    DirectoryOffset         : UInt64;
    class operator Implicit(const A: TZip64_EndOfCentralDirectory): TBytes;
    procedure Init;
  end;

  TZip64_EndOfCentralDirectoryLocator = packed record
    Signature               : UInt32; // $07064b50
    StartDiskNumber         : UInt32;
    RelativeOffset          : UInt64;
    TotalDisks              : UInt32;
    class operator Implicit(const A: TZip64_EndOfCentralDirectoryLocator): TBytes;
    procedure Init;
  end;

  /// <summary> Exception type for all Zip errors. </summary>
  EZipException = class( Exception );

  TZipMode = (zmClosed, zmRead, zmReadWrite, zmWrite);

  TZipFile = class;
  /// <summary> Function to Create a Compression/Decompression stream </summary>
  /// <remarks>
  ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
  ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
  /// </remarks>
  TStreamConstructor = reference to function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream;

  /// <summary> Class for creating and reading .ZIP files.
  /// </summary>
  TZipFile = class
  private type
    TCompressionDict = TDictionary< TZipCompression , TPair<TStreamConstructor, TStreamConstructor > >;
  private class var
    FCompressionHandler: TCompressionDict;
  private
    FMode: TZipMode;
    FStream: TStream;
    FFileStream: TFileStream;
    FStartFileData: Int64;
    FEndFileData: Int64;
    FFiles: TList<TZipHeader>;
    FComment: TBytes;
    FUTF8Support: Boolean;
    FOnCompress: TCompressEvent;
    FOnDecompress: TDecompressEvent;
    function TBytesToString(B: TBytes): string;
    function StringToTBytes(S: string): TBytes;
    function GetFileComment(Index: Integer): string;
    function GetFileCount: Integer;
    function GetFileInfo(Index: Integer): TZipHeader;
    function GetFileInfos: TArray<TZipHeader>;
    function GetFileName(Index: Integer): string;
    function GetFileNames: TArray<string>;
    function GetComment: string;
    procedure ReadCentralHeader;
    procedure SetFileComment(Index: Integer; Value: string);
    procedure SetComment(Value: string);
    procedure SetUTF8Support(const Value: Boolean);
    function LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
    function ZIP64_LocateEndOfCentralHeader(var Header: TZip64_EndOfCentralDirectory): Boolean;
  public
    class constructor Create;
    class destructor Destroy;

    /// <remarks>
    ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
    ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
    /// </remarks>
    class procedure RegisterCompressionHandler(Compression: TZipCompression;
      CompressStream, DecompressStream: TStreamConstructor);

    class procedure UnregisterCompressionHandler(Compression: TZipCompression);

    /// <param name="ZipFileName">Path to Zip File</param>
    /// <returns>Is the .ZIP file valid</returns>
    class function IsValid(const ZipFileName: string): Boolean; static;

    /// <summary> Extract a ZipFile</summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path to extract to disk</param>
    class procedure ExtractZipFile(const ZipFileName: string; const Path: string); static;

    /// <summary> Zip the contents of a directory </summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path of directory to zip</param>
    /// <param name="Compression">Compression mode.</param>
    class procedure ZipDirectoryContents(const ZipFileName: string; const Path: string;
      Compression: TZipCompression = zcDeflate); static;

    /// <summary> Create a TZipFile</summary>
    constructor Create;

    /// <remarks> Destroy will close an open zipfile before disposing of it</remarks>
    destructor Destroy; override;

    /// <summary> Opens a ZIP file for reading or writing.</summary>
    /// <param name="ZipFileName">Path to ZipFile</param>
    /// <param name="OpenMode"> File Mode to open file.
    ///   <c>zmWrite</c> Creates a new ZIP file for writing.
    ///   <c>zmReadWrite</c> Opens the file for reading and allows adding
    ///      additional new files.
    ///   <c>zmRead</c> Opens the file for reading.
    ///</param>
    procedure Open(const ZipFileName: string; OpenMode: TZipMode); overload;
    procedure Open(ZipFileStream: TStream; OpenMode: TZipMode); overload;

    /// <remarks>
    ///   Closing is required to write the ZipFile's
    ///   Central Directory to disk. Closing a file that is open for writing
    ///   writes additonal metadata that is required for reading the file.
    /// </remarks>
    procedure Close;

    /// <summary> Extract a single file </summary>
    /// <remarks>
    ///  <c>FileName</c> specifies a file in the ZIP file. All slashes
    ///  in ZIP file names should be '/'.
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">File name in the archive</param>
    /// <param name="Path">Path to extract to disk</param>
    /// <param name="CreateSubdirs">The output should create sub directories specified in the ZIP file</param>
    procedure Extract(const FileName: string; const Path: string = ''; CreateSubdirs: Boolean = True); overload;
    procedure Extract(Index: Integer; const Path: string = ''; CreateSubdirs: Boolean = True); overload;
    /// <summary> Extract All files </summary>
    /// <param name="Path">Path to extract to.</param>
    procedure ExtractAll(const Path: string = '');

    /// <summary> Read a file from arcive to an array of Bytes </summary>
    /// <remarks>
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">ZIP file FileName</param>
    /// <param name="Bytes">Output bytes</param>
    ///
    procedure Read(const FileName: string; out Bytes: TBytes); overload;
    procedure Read(Index: Integer; out Bytes: TBytes); overload;
    /// <summary> Get a stream to read a file from disk </summary>
    /// <remarks>
    ///   The Stream returned by this function is a decomression stream
    ///   wrapper around the interal Stream reading the zip file. You must
    ///   Free this stream before using other TZipFile methods that change the
    ///   contents of the ZipFile, such as Read or Add.
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">ZIP file FileName</param>
    /// <param name="Stream">Output Stream</param>
    /// <param name="LocalHeader">Local File header</param>
    procedure Read(const FileName: string; out Stream: TStream; out LocalHeader: TZipHeader); overload;
    procedure Read(Index: Integer; out Stream: TStream; out LocalHeader: TZipHeader); overload;

    /// <summary> Add a file to the ZIP file </summary>
    /// <param name="FileName">FileName to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.
    ///   If Ommitted, <C>ExtractFileName(FileName)</C> will be used.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(const FileName: string; const ArchiveFileName: string = '';
      Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Bytes to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.</param>
    /// <param name="Compression">Compression mode.</param>
    ///
    procedure Add(Data: TBytes; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(Data: TStream; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file. Allows programmer to specify
    ///  the Local and Central Header data for more flexibility on what gets written.
    ///  Minimal vailidation is done on the Header parameters; speficying bad options
    ///  could result in a corrupted zip file. </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="LocalHeader">The local header data</param>
    /// <param name="CentralHeader">A Pointer to an optional central header. If no
    /// central Header is provided, the Local Header information is used. </param>
    procedure Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader = nil); overload;



    /// <summary> Translate from FileName to index in ZIP Central Header
    /// </summary>
    /// <remarks>
    ///  A ZIP file may have dupicate entries with the same name. This
    ///  function will return the index of the first.
    /// </remarks>
    /// <param name="FileName">Path + Name of file in the arcive.</param>
    /// <returns>The index of the file in the archive, or -1 on failure.
    /// </returns>
    function IndexOf(const FileName: string): Integer;

    /// <returns> The mode the TZipFile is opened to</returns>
    property Mode: TZipMode read FMode;

    /// <returns>Total files in ZIP File</returns>
    property FileCount: Integer read GetFileCount;

    /// <returns>An array of FileNames in the ZIP file</returns>
    property FileNames: TArray<string> read GetFileNames;
    /// <returns>An array of the TZipHeader of the files in the ZIP file</returns>
    property FileInfos: TArray<TZipHeader> read GetFileInfos;

    /// <returns>FileName of a File in the ZipFile</returns>
    property FileName[Index: Integer]: string read GetFileName;
    /// <returns>TZipHeader of a File in the ZipFile</returns>
    property FileInfo[Index: Integer]: TZipHeader read GetFileInfo;
    /// <remarks>
    ///  File Comments can be changed for files opened in write mode at any point.
    ///  The comment is written when the Central Directory is written to disk.
    ///  Comments can be a maximum of 65535 bytes long. If a longer comment is supplied,
    ///  It is truncated before writing to the ZIP File.
    /// </remarks>
    property FileComment[Index: Integer]: string read GetFileComment write SetFileComment;
    /// <remarks>
    ///  Comments can be a maximum of 65535 bytes long. If a longer comment is supplied,
    ///  It is truncated before writing to the ZIP File.
    /// </remarks>
    property Comment: string read GetComment write SetComment;
    property UTF8Support: Boolean read FUTF8Support write SetUTF8Support default True;

    property OnCompress: TCompressEvent read FOnCompress write FOnCompress;
    property OnDecompress: TDecompressEvent read FOnDecompress write FOnDecompress;
  end;

implementation

uses
  System.RTLConsts,
  System.ZLib,
  System.ZLib.Progress
{$ifdef MSWINDOWS}, Winapi.Windows{$endif}
  ;

function TZipHeader.GetZIP64_CompressedSize: UInt64;
var Z64: TZipExtraField_ZIP64;
    Ex: TZipExtraFields;
begin
  Result := 0;
  if Length(ExtraField) > 0 then begin
    Ex := ExtraField;
    Z64 := Ex;
    Result := Z64.CompressedSize;
  end;
  if Result = 0 then
    Result := CompressedSize;
end;

function TZipHeader.GetZIP64_UncompressedSize: UInt64;
var Z64: TZipExtraField_ZIP64;
    Ex: TZipExtraFields;
begin
  Result := 0;
  if Length(ExtraField) > 0 then begin
    Ex := ExtraField;
    Z64 := Ex;
    Result := Z64.UncompressedSize;
  end;
  if Result = 0 then
    Result := UncompressedSize;
end;

function TZipHeader.IsZIP64: Boolean;
begin
  Result := (ZIP64_CompressedSize > ZIP64) or (ZIP64_UncompressedSize > ZIP64);
end;

procedure TZipHeader.SetExtraField_NTFS(const aFileName: string);
var Ex: TZipExtraFields;
    NTFS: TZipExtraField_NTFS;
begin
  if not FileExists(aFileName) then Exit;

  Ex := ExtraField;
  NTFS := TZipExtraField_NTFS.Create(aFileName);
  Ex.Add(NTFS);
  ExtraField := Ex;
end;

procedure TZipHeader.SetZIP64_CompressedSize(const Value: UInt64);
var Z64: TZipExtraField_ZIP64;
    Ex: TZipExtraFields;
begin
  if Value > ZIP64 then begin
    Ex := ExtraField;
    Z64 := Ex;
    Z64.CompressedSize := Value;
    Ex.Add(Z64);
    ExtraField := Ex;

    CompressedSize := ZIP64;
  end else
    CompressedSize := Value;
  ExtraFieldLength := Length(ExtraField);
end;

procedure TZipHeader.SetZIP64_UncompressedSize(const Value: UInt64);
var Z64: TZipExtraField_ZIP64;
    Ex: TZipExtraFields;
begin
  if Value > ZIP64 then begin
    Ex := ExtraField;
    Z64 := Ex;
    Z64.UncompressedSize := Value;
    Ex.Add(Z64);
    ExtraField := Ex;

    UncompressedSize := ZIP64;
  end else
    UncompressedSize := Value;
  ExtraFieldLength := Length(ExtraField);
end;

class operator TZip64_EndOfCentralDirectory.Implicit(
  const A: TZip64_EndOfCentralDirectory): TBytes;
begin
  SetLength(Result, SizeOf(A));
  Move(A, Result[0], SizeOf(A));
end;

procedure TZip64_EndOfCentralDirectory.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
  Signature := SIGNATURE_ZIP64_ENDOFCENTRALDIRECTORY;
  RecordSize := $2C;
  VersionMadeBy := ZIP_Version20;
  VersionNeededToExtract := ZIP_Version20;
end;

class operator TZip64_EndOfCentralDirectoryLocator.Implicit(
  const A: TZip64_EndOfCentralDirectoryLocator): TBytes;
begin
  SetLength(Result, SizeOf(A));
  Move(A, Result[0], SizeOf(A));
end;

procedure TZip64_EndOfCentralDirectoryLocator.Init;
begin
  FillChar(Self, SizeOf(Self), 0);
  Signature := SIGNATURE_ZIP64_ENDOFCENTRALDIRECTORYLOCATOR;
end;

constructor TZipExtraField.Create(const aHeaderID: UInt16; const aData: TBytes);
begin
  HeaderID := aHeaderID;
  SetData(aData);
end;

constructor TZipExtraField.Create(const aRawData: TBytes);
var iHeadSize: Integer;
begin
  iHeadSize := SizeOf(HeaderID) + SizeOf(DataSize);

  if Length(aRawData) < iHeadSize then
    raise EZipException.Create('Invalid Zip ExtraField');

  Move(aRawData[0], Self, iHeadSize);

  SetLength(Data, Length(aRawData) - iHeadSize);
  Move(aRawData[iHeadSize], Data[0], Length(Data));

  if DataSize <> Length(Data) then
    raise EZipException.Create('Invalid Zip ExtraField');
end;

class operator TZipExtraField.Implicit(const A: TZipExtraField): TBytes;
var iHeaderID, iDataSize, iData: NativeUInt;
begin
  iData := Length(A.Data);
  iHeaderID := SizeOf(A.HeaderID);
  iDataSize := SizeOf(A.DataSize);

  SetLength(Result, iHeaderID + iDataSize + iData);
  Move(A.HeaderID, Result[0], iHeaderID);
  Move(A.DataSize, Result[iHeaderID], iDataSize);
  if iData > 0 then
    Move(A.Data[0], Result[iHeaderID + iDataSize], iData);
end;

procedure TZipExtraField.SetData(const A: TBytes);
begin
  Data := Copy(A, 0, Length(A));
  DataSize := Length(Data);
end;

class operator TZipExtraField_ZIP64.Implicit(const aBytes: TBytes): TZipExtraField_ZIP64;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(aBytes) > 0 then
    Move(aBytes[0], Result, Length(aBytes));
end;

class operator TZipExtraField_ZIP64.Implicit(const A: TZipExtraField_ZIP64): TZipExtraField;
begin
  Result := TZipExtraField.Create(EXTRAFIELD_ID_ZIP64, A);
end;

class operator TZipExtraField_ZIP64.Implicit(const A: TZipExtraField_ZIP64): TBytes;
begin
  SetLength(Result, SizeOf(A));
  Move(A, Result[0], SizeOf(A));
end;

class operator TZipExtraField_ZIP64.Implicit(const A: TZipExtraField): TZipExtraField_ZIP64;
begin
  if A.DataSize > 0 then
    Result := A.Data
  else
    Result := nil;
end;

{$ifdef MSWINDOWS}
constructor TZipExtraField_NTFS.Create(const aFileName: string);
var F: TWin32FileAttributeData;
begin
  FillChar(Self, SizeOf(Self), 0);

  Tag1 := $0001;

  Size1 := SizeOf(MTime) + SizeOf(ATime) + SizeOf(CTime);
  {$WARN SYMBOL_PLATFORM OFF}Win32Check(GetFileAttributesEx(PChar(aFileName), GetFileExInfoStandard, @F));{$WARN SYMBOL_PLATFORM ON}
  Move(F.ftCreationTime, CTime, SizeOf(CTime));
  Move(F.ftLastAccessTime, ATime, SizeOf(ATime));
  Move(F.ftLastWriteTime, MTime, SizeOf(MTime));
end;

class operator TZipExtraField_NTFS.Implicit(const A: TZipExtraField_NTFS):
    TBytes;
begin
  SetLength(Result, SizeOf(A));
  Move(A, Result[0], SizeOf(A));
end;

class operator TZipExtraField_NTFS.Implicit(
  const A: TZipExtraField_NTFS): TZipExtraField;
begin
  Result := TZipExtraField.Create(EXTRAFIELD_ID_NTFS, A);
end;

class operator TZipExtraField_NTFS.Implicit(
  const A: TZipExtraField): TZipExtraField_NTFS;
begin
  if A.DataSize > 0 then
    Result := A.Data
  else
    Result := nil;
end;

class operator TZipExtraField_NTFS.Implicit(const aBytes: TBytes):
    TZipExtraField_NTFS;
begin
  FillChar(Result, SizeOf(Result), 0);
  if Length(aBytes) > 0 then
    Move(aBytes[0], Result, Length(aBytes));
end;
{$endif}

class operator TZipExtraFields.Implicit(
  const A: TZipExtraFields): TBytes;
var F: TZipExtraField;
    B: TBytes;
    i: Integer;
begin
  SetLength(Result, 0);
  for F in A.Items do begin
    B := F;
    i := Length(Result);
    SetLength(Result, i + Length(B));
    Move(B[0], Result[i], Length(B));
  end;
end;

class operator TZipExtraFields.Implicit(
  const aBytes: TBytes): TZipExtraFields;
var iOffSet: Integer;
    pSize: ^UInt16;
    B: TBytes;
begin
  iOffSet := 0;
  SetLength(Result.Items, 0);
  while iOffSet < Length(aBytes) do begin
    pSize := @aBytes[iOffSet + 2];
    B := Copy(aBytes, iOffSet, 2{HeaderID} + 2{DataSize} + pSize^);
    Result.Items[Result.New] := TZipExtraField.Create(B);
    Inc(iOffSet, Length(B));
  end;
end;

procedure TZipExtraFields.Add(const A: TZipExtraField);
var F: TZipExtraField;
    i: NativeInt;
begin
  if not Get(A.HeaderID, F, i) then
    i := New;
  Items[i] := A;
end;

function TZipExtraFields.Get(const aHeaderID: UInt16; out aItem:
    TZipExtraField; out Index: NativeInt): Boolean;
var i: NativeInt;
begin
  Index := -1;
  for i := 0 to Length(Items) - 1 do begin
    if Items[i].HeaderID = aHeaderID then begin
      aItem := Items[i];
      Index := i;
      Break;
    end;
  end;
  Result := Index <> -1;
end;

class operator TZipExtraFields.Implicit(
  const A: TZipExtraFields): TZipExtraField_ZIP64;
var F: TZipExtraField;
    i: NativeInt;
begin
  if not A.Get(EXTRAFIELD_ID_ZIP64, F, i) then
    F := TZipExtraField.Create(EXTRAFIELD_ID_ZIP64, nil);
  Result := F;
end;

function TZipExtraFields.New: NativeInt;
begin
  Result := Length(Items) + 1;
  SetLength(Items, Result);
  Dec(Result);
end;

procedure VerifyRead(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  if Stream.Read(Buffer[0], Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt8; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt16; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt32; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  if Stream.Write(Buffer[0], {0,} Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt8; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt16; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt32; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

type
  /// <summary> Helper class for reading a segment of another stream.</summary>
  TStoredStream = class( TStream )
  private
    FStream: TStream;
    FPos: Int64;
    FCompress: TCompressEvent;
    FDecompress: TDecompressEvent;
    FZipFileSize: UInt64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(Stream: TStream; const aCompress: TCompressEvent; const
        aDecompress: TDecompressEvent);

    function Read(var Buffer; Count: Longint): Longint; overload; override;
    function Write(const Buffer; Count: Longint): Longint; overload; override;
//    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; overload; //override;
//    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; overload; //override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TStoredStream }

constructor TStoredStream.Create(Stream: TStream; const aCompress:
    TCompressEvent; const aDecompress: TDecompressEvent);
begin
  FStream := Stream;
  FPos := FStream.Position;
  FCompress := aCompress;
  FDecompress := aDecompress;
end;

function TStoredStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TStoredStream.Read(var Buffer; Count: Integer): Longint;
begin
  Result := FStream.Read(Buffer, {Offset,} Count);
  if Assigned(FDecompress) then
    FDeCompress(Self, Self.Position, Self.Size);
end;

//function TStoredStream.Read(Buffer : TBytes; Offset, Count: Longint): Longint;
//begin
//  Result := FStream.Read(Buffer, Offset, Count);
//  if Assigned(FDecompress) then
//    FDeCompress(Self, Self.Position, Self.Size);
//end;

function TStoredStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin)
end;

function TStoredStream.Write(const Buffer; Count: Integer): Longint;
begin
  Result := FStream.Write(Buffer, {Offset,} Count);
  if Assigned(FCompress) then
    FCompress(Self, Self.Position, FZipFileSize, FStream.Size);
end;

//function TStoredStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
//begin
//  Result := FStream.Write(Buffer, Offset, Count);
//  if Assigned(FCompress) then
//    FCompress(Self, Self.Position, FZipFileSize, FStream.Size);
//end;

function TZipCompressionToString(Compression: TZipCompression): string;
begin
  case Compression of
    zcStored:    Result := 'Stored';                // do not localize
    zcShrunk:    Result := 'Shrunk';                // do not localize
    zcReduce1:   Result := 'Reduced1';              // do not localize
    zcReduce2:   Result := 'Reduced2';              // do not localize
    zcReduce3:   Result := 'Reduced3';              // do not localize
    zcReduce4:   Result := 'Reduced4';              // do not localize
    zcImplode:   Result := 'Imploded';              // do not localize
    zcTokenize:  Result := 'Tokenized';             // do not localize
    zcDeflate:   Result := 'Deflated';              // do not localize
    zcDeflate64: Result := 'Deflated64';            // do not localize
    zcPKImplode: Result := 'Imploded(TERSE)';       // do not localize
    zcBZIP2:     Result := 'BZIP2';                 // do not localize
    zcLZMA:      Result := 'LZMA';                  // do not localize
    zcTERSE:     Result := 'TERSE';                 // do not localize
    zcLZ77:      Result := 'LZ77';                  // do not localize
    zcWavePack:  Result := 'WavPack';               // do not localize
    zcPPMdI1:    Result := 'PPMd version I, Rev 1'; // do not localize
    else
      Result := 'Unknown';
  end;
end;

{ TZipFile }

function TZipFile.TBytesToString(B: TBytes) : string;
var
  E: TEncoding;
begin
  if FUTF8Support then 
    E := TEncoding.GetEncoding(65001)
  else
    E := TEncoding.GetEncoding(437);
  try
    Result := E.GetString(B);
  finally
    E.Free;
  end;
end;

class procedure TZipFile.UnregisterCompressionHandler(
  Compression: TZipCompression);
begin
  FCompressionHandler.Remove(Compression);
end;

function TZipFile.StringToTBytes(S: string): TBytes;
var
  E: TEncoding;
begin
  if FUTF8Support then 
    E := TEncoding.GetEncoding(65001)
  else
    E := TEncoding.GetEncoding(437);
  try
    Result := E.GetBytes(S);
  finally
    E.Free;
  end;
end;

function TZipFile.GetComment: string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FComment);
end;

function TZipFile.GetFileComment(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FFiles[Index].FileComment);
end;

function TZipFile.GetFileCount: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles.Count;
end;

function TZipFile.GetFileInfo(Index: Integer): TZipHeader;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles[Index];
end;

function TZipFile.GetFileInfos: TArray<TZipHeader>;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles.ToArray;
end;

function TZipFile.GetFileName(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FFiles[Index].FileName);
end;

function TZipFile.GetFileNames: TArray<string>;
var
  I: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  SetLength(Result, FFiles.Count);
  for I := 0 to High(Result) do
    Result[I] := TBytesToString(FFiles[I].FileName);
end;

procedure TZipFile.ReadCentralHeader;
var
  I: Integer;
  Signature: UInt32;
  LEndHeader: TZipEndOfCentralHeader;
  LHeader: TZipHeader;
  Z64: TZip64_EndOfCentralDirectory;
begin
  FFiles.Clear;
  if FStream.Size = 0 then
    Exit;
  // Read End Of Centeral Direcotry Header
  if not LocateEndOfCentralHeader(LEndHeader) then
    raise EZipException.CreateRes(@SZipErrorRead);
  // Move to the beginning of the CentralDirectory
  FStream.Position := LEndHeader.CentralDirOffset;
  if LEndHeader.CentralDirOffset = ZIP64 then begin
    if ZIP64_LocateEndOfCentralHeader(Z64) then
      FStream.Position := Z64.DirectoryOffset;
  end;
  // Save Begginning of Central Directory. This is where new files
  // get written to, and where the new central directory gets written when
  // closing.
  FEndFileData := LEndHeader.CentralDirOffset;
  // Read File Headers
  for I := 0 to LEndHeader.CentralDirEntries - 1 do
  begin
    // Verify Central Header signature
    FStream.Read(Signature, Sizeof(Signature));
    if Signature <> SIGNATURE_CENTRALHEADER then
      raise EZipException.CreateRes(@SZipInvalidCentralHeader);
    // Read Central Header
    VerifyRead(FStream, LHeader.MadeByVersion,      Sizeof(UInt16));
    VerifyRead(FStream, LHeader.RequiredVersion,    Sizeof(UInt16));
    VerifyRead(FStream, LHeader.Flag,               Sizeof(UInt16));
    VerifyRead(FStream, LHeader.CompressionMethod,  Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ModifiedDateTime,   Sizeof(UInt32));
    VerifyRead(FStream, LHeader.CRC32,              Sizeof(UInt32));
    VerifyRead(FStream, LHeader.CompressedSize,     Sizeof(UInt32));
    VerifyRead(FStream, LHeader.UncompressedSize,   Sizeof(UInt32));
    VerifyRead(FStream, LHeader.FileNameLength,     Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ExtraFieldLength,   Sizeof(UInt16));
    VerifyRead(FStream, LHeader.FileCommentLength,  Sizeof(UInt16));
    VerifyRead(FStream, LHeader.DiskNumberStart,    Sizeof(UInt16));
    VerifyRead(FStream, LHeader.InternalAttributes, Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ExternalAttributes, Sizeof(UInt32));
    VerifyRead(FStream, LHeader.LocalHeaderOffset,  Sizeof(UInt32));

    // Read Dynamic length fields (FileName, ExtraField, FileComment)
    if LHeader.FileNameLength > 0 then
    begin
      SetLength(LHeader.FileName, LHeader.FileNameLength);
      VerifyRead(FStream, LHeader.FileName[0], LHeader.FileNameLength);
    end;
    if LHeader.ExtraFieldLength > 0 then
    begin
      SetLength(LHeader.ExtraField, LHeader.ExtraFieldLength);
      VerifyRead(FStream, LHeader.ExtraField[0], LHeader.ExtraFieldLength);
    end;
    if LHeader.FileCommentLength > 0 then
    begin
      SetLength(LHeader.FileComment, LHeader.FileCommentLength);
      VerifyRead(FStream, LHeader.FileComment[0], LHeader.FileCommentLength);
    end;
    if (LHeader.Flag and (1 shl 11)) = 0 then
      FUTF8Support := False;

    // Save File Header in interal list
    FFiles.Add(LHeader);
  end;
end;

procedure TZipFile.SetComment(Value: string);
begin
  FComment := StringToTBytes(Value);
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  if Length(FComment) > $FFFF then
    SetLength(FComment, $FFFF);
end;

procedure TZipFile.SetFileComment(Index: Integer; Value: string);
var
  LFile: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  LFile := FFiles[Index];

  LFile.FileComment := StringToTBytes(Value);
  if Length(LFile.FileComment) > $FFFF then
    SetLength(LFile.FileComment, $FFFF);
  LFile.FileCommentLength := Length(LFile.FileComment);
  FFiles[Index] := LFile;
end;

procedure TZipFile.SetUTF8Support(const Value: Boolean);
begin
  if Value = FUTF8Support then Exit;
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  // Resetting this flag would require re-writing all the local headers with the
  // new strings and flag, and adjusting the offsets.
  if FFiles.Count <> 0 then
    raise EZipException.CreateRes(@SZipNotEmpty);

                                         
  FUTF8Support := Value;
end;

class constructor TZipFile.Create;
begin
  FCompressionHandler := TCompressionDict.Create;

  RegisterCompressionHandler(zcStored,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TStoredStream.Create(InStream, ZipFile.OnCompress, ZipFile.OnDecompress);
      (Result as TStoredStream).FZipFileSize := Item.ZIP64_UncompressedSize;
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TStoredStream.Create(InStream, ZipFile.OnCompress, ZipFile.OnDecompress);
    end);

  RegisterCompressionHandler(zcDeflate,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZCompressionStream2.Create(InStream, zcDefault, -15, ZipFile.OnCompress);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZDecompressionStream2.Create(InStream, -15, ZipFile.OnDecompress);
    end);
end;

class destructor TZipFile.Destroy;
begin
  FCompressionHandler.Free;
end;

class procedure TZipFile.RegisterCompressionHandler(
  Compression: TZipCompression; CompressStream, DecompressStream: TStreamConstructor);
begin
  FCompressionHandler.AddOrSetValue(Compression,
    TPair<TStreamConstructor, TStreamConstructor>.Create(CompressStream, DecompressStream));
end;

class function TZipFile.IsValid(const ZipFileName: string): Boolean;
var
  Z: TZipFile;
  Header: TZipEndOfCentralHeader;
begin
  Result := False;
  try
    Z := TZipFile.Create;
    try
      Z.FStream := TFileStream.Create(ZipFileName, fmOpenRead);
      try
        Result := Z.LocateEndOfCentralHeader(Header);
      finally
        Z.FStream.Free;
      end;
    finally
      Z.Free;
    end;
  except on E: EStreamError do
    // Swallow only Stream exceptions and return False
  end;
end;

function TZipFile.LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
var
  I: Integer;
  LBackRead, LReadSize, LMaxBack: UInt32;
  LBackBuf: TBytes;
begin
  if FStream.Size < $FFFF then
    LMaxBack := FStream.Size
  else
    LMaxBack := $FFFF;
  LBackRead := 4;
  SetLength(LBackBuf, $404 - 1);
  while LBackRead < LMaxBack do
  begin
    if LBackRead + Cardinal(Length(LBackBuf) - 4) > LMaxBack then
      LBackRead := LMaxBack
    else
      Inc(LBackRead, Length(LBackBuf) -4);
    FStream.Position := FStream.Size - LBackRead;
    if Length(LBackBuf) < (FStream.Size - FStream.Position) then
      LReadSize := Length(LBackBuf)
    else
      LReadSize := FStream.Size - FStream.Position;
    VerifyRead(FStream, LBackBuf[0], LReadSize);

    for I := LReadSize - 4 downto 0 do
    begin
      if (LBackBuf[I]   = ((SIGNATURE_ZIPENDOFHEADER       ) and $FF)) and
         (LBackBuf[I+1] = ((SIGNATURE_ZIPENDOFHEADER shr  8) and $FF)) and
         (LBackBuf[I+2] = ((SIGNATURE_ZIPENDOFHEADER shr 16) and $FF)) and
         (LBackBuf[I+3] = ((SIGNATURE_ZIPENDOFHEADER shr 24) and $FF)) then
      begin
        Move(LBackBuf[I+4], Header, SizeOf(Header));
        if Header.CommentLength > 0 then
        begin
          FStream.Position := FStream.Size - LBackRead + I + 4 + SizeOf(Header);
          SetLength(FComment, Header.CommentLength);
          FStream.Read(FComment, Header.CommentLength);
        end
        else
          SetLength(FComment, 0);
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

class procedure TZipFile.ExtractZipFile(const ZipFileName: string; const Path: string);
var
  LZip: TZipFile;
begin
  LZip := TZipFile.Create;
  try
    LZip.Open(ZipFileName, zmRead);
    LZip.ExtractAll(Path);
    LZip.Close;
  finally
    LZip.Free;
  end;
end;

function TZipFile.ZIP64_LocateEndOfCentralHeader(
  var Header: TZip64_EndOfCentralDirectory): Boolean;
var B: TBytes;
    i, iSize: Integer;
begin
  Result := False;

  iSize := $FFFF;
  if FStream.Size < iSize then
    iSize := FStream.Size;
  SetLength(B, iSize);

  FStream.Seek(-iSize, soFromEnd);

  i := FStream.Read(B[0], iSize) - 1 - SizeOf(Header);
  while i >= 0 do begin
    if PCardinal(@B[i])^ = SIGNATURE_ZIP64_ENDOFCENTRALDIRECTORY then begin
      Move(B[i], Header, SizeOf(Header));
      Result := True;
      Break;
    end;
    Dec(i);
  end;
end;

class procedure TZipFile.ZipDirectoryContents(const ZipFileName: string; const Path: string;
  Compression: TZipCompression);
var
  LZipFile: TZipFile;
  LFile: string;
  LZFile: string;
  LPath: string;
begin
  LZipFile := TZipFile.Create;
  try
    LZipFile.Open(ZipFileName, zmWrite);
    LPath := System.SysUtils.IncludeTrailingPathDelimiter(Path);
    for LFile in TDirectory.GetFiles(Path, '*', TSearchOption.soAllDirectories) do
    begin
      // Strip off root path
{$IFDEF MSWINDOWS}
      LZFile := StringReplace(Copy(LFile, Length(LPath) + 1, Length(LFile)), '\', '/', [rfReplaceAll]);
{$ELSE}
      LZFile := Copy(LFile, Length(LPath) + 1, Length(LFile));
{$ENDIF MSWINDOWS}
      LZipFile.Add(LFile, LZFile, Compression);
    end;
  finally
    LZipFile.Free;
  end;
end;

constructor TZipFile.Create;
begin
  inherited Create;
  FFiles := TList<TZipHeader>.Create;
  FMode := zmClosed;
  FUTF8Support := True;
end;

destructor TZipFile.Destroy;
begin
  Close; // In case a file is open for writing currently

  FFiles.Free;
  inherited;
end;

procedure TZipFile.Open(const ZipFileName: string; OpenMode: TZipMode);
var
  LMode: LongInt;
  LFileStream: TFileStream;
begin
  Close; // In case the user had a file open
  case OpenMode of
    zmRead:      LMode := fmOpenRead + fmShareDenyWrite;
    zmReadWrite: LMode := fmOpenReadWrite;
    zmWrite:     LMode := fmCreate;
    else
      raise EZipException.CreateRes(@sArgumentInvalid);
  end;
  LFileStream := TFileStream.Create(ZipFileName, LMode);
  try
    Open(LFileStream, OpenMode);
    FFileStream := LFileStream;
  except
    FreeAndNil(LFileStream);
    raise;
  end;
end;

procedure TZipFile.Open(ZipFileStream: TStream; OpenMode: TZipMode);
begin
  Close; // In case the user had a file open
  if OpenMode = zmClosed then
    raise EZipException.CreateRes(@sArgumentInvalid);
  if (OpenMode = zmRead) and (ZipFileStream.Size = 0) then
    raise EZipException.CreateRes(@SReadError);

  FStream := ZipFileStream;
  FStartFileData := FStream.Position;
  if OpenMode in [zmRead, zmReadWrite] then
  try
    // Read the Central Header to verify it's a valid zipfile
    ReadCentralHeader;
  except
    // If it's an invalid zipfile, cleanup
    FStream := nil;
    raise;
  end;
  FMode := OpenMode;
end;

procedure TZipFile.Close;
var
  LHeader: TZipHeader;
  LEndOfHeader: TZipEndOfCentralHeader;
  I: Integer;
  Signature: UInt32;
  iCentralDirSize: UInt32;
  Z64_End: TZip64_EndOfCentralDirectory;
  Z64_EndLocator: TZip64_EndOfCentralDirectoryLocator;
  bIsZIP64: Boolean;
begin
  try
    // Only need to write Central Directory and End Of Central Directory if writing
    if (FMode = zmReadWrite) or (FMode = zmWrite) then
    begin
      bIsZIP64 := False;
      FStream.Position := FEndFileData;
      Signature := SIGNATURE_CENTRALHEADER;
      // Write File Signatures
      for I := 0 to FFiles.Count - 1 do
      begin
        LHeader := FFiles[I];

        if not bIsZip64 then
          bIsZip64 := LHeader.IsZIP64;

        VerifyWrite(FStream, Signature, SizeOf(Signature));
//        VerifyWrite(FStream, LHeader.MadeByVersion,  CENTRALHEADERSIZE);
        VerifyWrite(FStream, LHeader.MadeByVersion,      Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.RequiredVersion,    Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.Flag,               Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.CompressionMethod,  Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ModifiedDateTime,   Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.CRC32,              Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.CompressedSize,     Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.UncompressedSize,   Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.FileNameLength,     Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ExtraFieldLength,   Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.FileCommentLength,  Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.DiskNumberStart,    Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.InternalAttributes, Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ExternalAttributes, Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.LocalHeaderOffset,  Sizeof(UInt32));

        if LHeader.FileNameLength <> 0 then
          VerifyWrite(FStream, LHeader.FileName[0], LHeader.FileNameLength);
        if LHeader.ExtraFieldLength <> 0 then
          VerifyWrite(FStream, LHeader.ExtraField[0], LHeader.ExtraFieldLength);
        if LHeader.FileCommentLength <> 0 then
          VerifyWrite(FStream, LHeader.FileComment[0], LHeader.FileCommentLength);
      end;

      if not bIsZip64 then
        bIsZip64 := FStream.Position > ZIP64;

      iCentralDirSize := FStream.Position - FEndFileData;

      if bIsZip64 then begin
        Z64_EndLocator.Init;
        Z64_EndLocator.StartDiskNumber := 0;
        Z64_EndLocator.RelativeOffset := FStream.Position;
        Z64_EndLocator.TotalDisks := 1;

        Z64_End.Init;
        Z64_End.EntriesOnDisk := FFiles.Count;
        Z64_End.TotalEntries := FFiles.Count;
        Z64_End.DirectorySize := iCentralDirSize;
        Z64_End.DirectoryOffset := FEndFileData;

        VerifyWrite(FStream, Z64_End, SizeOf(Z64_End));
        VerifyWrite(FStream, Z64_EndLocator, SizeOf(Z64_EndLocator));
      end;

      // Only support writing single disk .ZIP files
      FillChar(LEndOfHeader, Sizeof(LEndOfHeader), 0);
      LEndOfHeader.CentralDirEntries := FFiles.Count;
      LEndOfHeader.NumEntriesThisDisk := FFiles.Count;
      LEndOfHeader.CentralDirSize := iCentralDirSize;
      LEndOfHeader.CentralDirOffset := FEndFileData;
      if FEndFileData > ZIP64 then
        LEndOfHeader.CentralDirOffset := ZIP64;
      // Truncate comment if it's too long
      if Length(FComment) > $FFFF then
        SetLength(FComment, $FFFF);
      LEndofHeader.CommentLength := Length(FComment);
      // Write End Of Centeral Directory
      Signature := SIGNATURE_ZIPENDOFHEADER;
      VerifyWrite(FStream, Signature, SizeOf(Signature));
//      VerifyWrite(FStream, LEndOfHeader, SizeOf(LEndOfHeader));
      VerifyWrite(FStream, LEndOfHeader.DiskNumber,          SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirStartDisk, SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.NumEntriesThisDisk,  SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirEntries,   SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirSize,      SizeOf(UInt32));
      VerifyWrite(FStream, LEndOfHeader.CentralDirOffset,    SizeOf(UInt32));
      VerifyWrite(FStream, LEndOfHeader.CommentLength,       SizeOf(UInt16));

      if LEndOfHeader.CommentLength > 0 then
        VerifyWrite(FStream, FComment[0], LEndOfHeader.CommentLength);
    end;
  finally
    FMode := zmClosed;
    FFiles.Clear;
    FStream := nil;
    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);
  end;
end;

procedure TZipFile.Extract(const FileName: string; const Path: string; CreateSubDirs: Boolean);
begin
  Extract(IndexOf(FileName), Path, CreateSubdirs);
end;

procedure TZipFile.Extract(Index: Integer; const Path: string; CreateSubdirs: Boolean);
var
  LInStream, LOutStream: TStream;
  LHeader: TZipHeader;
  LDir, LFileName: string;
  Ex: TZipExtraFields;
  NTFS: TZipExtraField_NTFS;
  i: NativeInt;
  F: TZipExtraField;
begin
  // Get decompression stream for file
  Read(Index, LInStream, LHeader);
  try
    LFileName := TBytesToString(LHeader.FileName);
{$IFDEF MSWINDOWS} // ZIP stores files with '/', so translate to a relative Windows path.
    LFileName := StringReplace(LFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}
    // CreateSubDirs = False assumes the user passed in the path where they want the file to end up
    if CreateSubdirs then
      LFileName := TPath.Combine(Path, LFileName)
    else
      LFileName := TPath.Combine(Path, ExtractFileName(LFileName));
    // Force directory creation
    LDir := ExtractFileDir(LFileName);
    if CreateSubdirs and (LDir <> '') then
      TDirectory.CreateDirectory(ExtractFileDir(LFileName));
    // Open the File For output
    if LFileName{.Chars}[Length(LFileName){.Length-1}] = PathDelim then
      Exit; // Central Directory Entry points at a directory, not a file.
    LOutStream := TFileStream.Create(LFileName, fmCreate);
    try // And Copy from the decompression stream.
      // See Bit 3 at http://www.pkware.com/documents/casestudies/APPNOTE.TXT
      if (LHeader.Flag and (1 SHL 3)) = 0 then
      begin
        // Empty files should not be read
        if LHeader.ZIP64_UncompressedSize > 0 then
          LOutStream.CopyFrom(LInStream, LHeader.ZIP64_UncompressedSize);
      end
      else
      begin
        LOutStream.CopyFrom(LInStream, LHeader.ZIP64_UncompressedSize);
      end;
    finally
      if (LOutStream as TFileStream).Handle <> INVALID_HANDLE_VALUE then begin
        if LHeader.ExtraFieldLength > 0 then begin
          Ex := LHeader.ExtraField;
          if Ex.Get(EXTRAFIELD_ID_NTFS, F, i) then begin
            NTFS := F;
            SetFileTime((LOutStream as TFileStream).Handle, @NTFS.CTime, @NTFS.ATime, @NTFS.MTime);
          end;
        end;
      end;
      LOutStream.Free;
    end;
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.ExtractAll(const Path: string);
var
  I: Integer;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);
  for I := 0 to FFiles.Count - 1 do
    Extract(I, Path);
end;

procedure TZipFile.Read(const FileName: string; out Bytes: TBytes);
begin
  Read(IndexOf(FileName), Bytes);
end;

procedure TZipFile.Read(Index: Integer; out Bytes: TBytes);
var
  LStream: TStream;
  LHeader: TZipHeader;
  ReadStart, ReadBytes: Int64;
begin
  Read(Index, LStream, LHeader);
  try
    if (LHeader.Flag and (1 SHL 3)) = 0 then
    begin
      SetLength(Bytes, FFiles[Index].UncompressedSize);
      if FFiles[Index].UncompressedSize > 0 then // Special case for empty files.
        VerifyRead(LStream, Bytes[0], LHeader.UncompressedSize);
    end
    else
    begin
      //CRC, Uncompressed, and Compressed Size follow the compressed data.
      SetLength(Bytes, 4096);
      ReadStart := 0;
      ReadBytes := 0; // Supress warning
      while True do
      begin
        ReadBytes := LStream.Read(Bytes[ReadStart], Length(Bytes)-ReadStart);
        if ReadBytes < (Length(Bytes) - ReadStart) then
          break;
        ReadStart := ReadStart + ReadBytes;
        SetLength(Bytes, Length(Bytes)*2);
      end;
      SetLength(Bytes, ReadStart + ReadBytes);
    end;
  finally
    LStream.Free;
  end;
end;
//{$ENDIF}

procedure TZipFile.Read(const FileName: string; out Stream: TStream; out LocalHeader: TZipHeader);
begin
  Read(IndexOf(FileName), Stream, LocalHeader);
end;

procedure TZipFile.Read(Index: Integer; out Stream: TStream; out LocalHeader: TZipHeader);
var
  Signature: UInt32;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);

  if (Index < 0) or (Index > FFiles.Count) then
    raise EZipException.CreateRes(@SFileNotFound);

  // Local Header doesn't have thse fields
  LocalHeader.MadeByVersion := 0;
  SetLength(LocalHeader.FileComment, 0);
  LocalHeader.FileCommentLength  := 0;
  LocalHeader.DiskNumberStart    := 0;
  LocalHeader.InternalAttributes := 0;
  LocalHeader.ExternalAttributes := 0;
  LocalHeader.LocalHeaderOffset  := 0;

  // Move to beginning of Local Header
  FStream.Position := FFiles[Index].LocalHeaderOffset + FStartFileData;
  // Verify local header signature
  FStream.Read(Signature, Sizeof(Signature));
  if Signature <> SIGNATURE_LOCALHEADER then
    raise EZipException.CreateRes(@SZipInvalidLocalHeader);
  // Read local header
//  FStream.Read(LocalHeader.RequiredVersion, LOCALHEADERSIZE);
    FStream.Read(LocalHeader.RequiredVersion,    Sizeof(UInt16));
    FStream.Read(LocalHeader.Flag,               Sizeof(UInt16));
    FStream.Read(LocalHeader.CompressionMethod,  Sizeof(UInt16));
    FStream.Read(LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
    FStream.Read(LocalHeader.CRC32,              Sizeof(UInt32));
    FStream.Read(LocalHeader.CompressedSize,     Sizeof(UInt32));
    FStream.Read(LocalHeader.UncompressedSize,   Sizeof(UInt32));
    FStream.Read(LocalHeader.FileNameLength,     Sizeof(UInt16));
    FStream.Read(LocalHeader.ExtraFieldLength,   Sizeof(UInt16));
  // Read Name and extra fields
  SetLength(LocalHeader.FileName, LocalHeader.FileNameLength);
  FStream.Read(LocalHeader.FileName[0], LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
    FStream.Read(LocalHeader.ExtraField[0], LocalHeader.ExtraFieldLength);
  end;
  // Create Decompression stream.
  Stream := FCompressionHandler[TZipCompression(FFiles[Index].CompressionMethod)].Value(FStream, Self, LocalHeader);
end;

procedure TZipFile.Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);
var
  DataStart: Int64;
  LCompressStream: TStream;
  Signature: UInt32;
  LStartPos: Int64;
  LBuffer: TBytes;
//  DataBuffer = array
begin
  // Seek to End of zipped data
  FStream.Position := FEndFileData;
  LocalHeader.LocalHeaderOffset := FEndFileData;
  // Require at least version 2.0
  if LocalHeader.MadeByVersion < ZIP_Version20 then
    LocalHeader.MadeByVersion := ZIP_Version20;
  if LocalHeader.RequiredVersion < ZIP_Version20 then
    LocalHeader.RequiredVersion := ZIP_Version20;

  // Trust the length of the strings over the Length members
  LocalHeader.FileNameLength   := Length(LocalHeader.FileName);
  LocalHeader.ExtraFieldLength := Length(LocalHeader.ExtraField);
  if CentralHeader = nil then
    CentralHeader := @LocalHeader
  else
  begin // Trust the length of the strings over the Length members
    CentralHeader^.FileNameLength   := Length(CentralHeader^.FileName);
    CentralHeader^.ExtraFieldLength := Length(CentralHeader^.ExtraField);
  end;
  CentralHeader^.FileCommentLength  := Length(CentralHeader^.FileComment);

  // Write Signature, Header, and FileName
  Signature := SIGNATURE_LOCALHEADER;
  VerifyWrite(FStream, Signature, SizeOf(Signature));
//  VerifyWrite(FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
    VerifyWrite(FStream, LocalHeader.RequiredVersion,    Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.Flag,               Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.CompressionMethod,  Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.CRC32,              Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.CompressedSize,     Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.UncompressedSize,   Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.FileNameLength,     Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.ExtraFieldLength,   Sizeof(UInt16));

  VerifyWrite(FStream, LocalHeader.FileName[0], LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
    VerifyWrite(FStream, LocalHeader.ExtraField[0], LocalHeader.ExtraFieldLength);
  // Save position to calcuate Compressed Size
  LStartPos := FStream.Position;
  DataStart := Data.Position;
  LocalHeader.ZIP64_UncompressedSize := Data.Size - DataStart;
  // Write Compressed data
  LCompressStream := FCompressionHandler[TZipCompression(LocalHeader.CompressionMethod)].Key(FStream, self, LocalHeader);
  try
    if TZipCompression(LocalHeader.CompressionMethod) in [zcDeflate, zcLZMA] then
    begin
      LCompressStream.CopyFrom(Data, Data.Size);
//      LCompressStream.Write(Data, 0)
    end
    else
      LCompressStream.CopyFrom(Data, LocalHeader.ZIP64_UncompressedSize);
  finally
    LCompressStream.Free;
  end;

  // Calcuate CompressedSize
  LocalHeader.ZIP64_CompressedSize := FStream.Position - LStartPos;
  Data.Position := DataStart;
  SetLength(LBuffer, $4000);
  // Calcuate Uncompressed data's CRC
  while Data.Position < LocalHeader.ZIP64_UncompressedSize do
    LocalHeader.CRC32 := crc32(LocalHeader.CRC32, @LBuffer[0],
      Data.Read(LBuffer[0], Length(LBuffer)));
  CentralHeader.ZIP64_UncompressedSize := LocalHeader.ZIP64_UncompressedSize;
  CentralHeader.ZIP64_CompressedSize := LocalHeader.ZIP64_CompressedSize;
  CentralHeader.CRC32 := LocalHeader.CRC32;
  // Save new End of zipped data mark
  FEndFileData := FStream.Position;
  // Move to beginning of Local Header offset and rewrite header
  // with correct CompressedSize and CRC32
  FStream.Position := LocalHeader.LocalHeaderOffset + SizeOf(UInt32);
//  FStream.Write(LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  FStream.Write(LocalHeader.RequiredVersion,    Sizeof(UInt16));
  FStream.Write(LocalHeader.Flag,               Sizeof(UInt16));
  FStream.Write(LocalHeader.CompressionMethod,  Sizeof(UInt16));
  FStream.Write(LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
  FStream.Write(LocalHeader.CRC32,              Sizeof(UInt32));
  FStream.Write(LocalHeader.CompressedSize,     Sizeof(UInt32));
  FStream.Write(LocalHeader.UncompressedSize,   Sizeof(UInt32));
  FStream.Write(LocalHeader.FileNameLength,     Sizeof(UInt16));
  FStream.Write(LocalHeader.ExtraFieldLength,   Sizeof(UInt16));
  FStream.Write(LocalHeader.FileName[0], LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
    FStream.Write(LocalHeader.ExtraField[0], LocalHeader.ExtraFieldLength);

  FFiles.Add(CentralHeader^);
end;

procedure TZipFile.Add(const FileName: string; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
  LHeader: TZipHeader;
  LArchiveFileName: string;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  LHeader.Flag := 0;
  LInStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LHeader.Flag := 0;
    LHeader.CompressionMethod := UInt16(Compression);
    LHeader.ModifiedDateTime := DateTimeToFileDate( tfile.GetLastWriteTime(FileName) );
    LHeader.ZIP64_UncompressedSize := LInStream.Size;
    LHeader.InternalAttributes := 0;
    LHeader.ExternalAttributes := 0;                                               
    if ArchiveFileName <> '' then
	  LArchiveFileName := ArchiveFileName
	else
      LArchiveFileName := ExtractFileName(FileName);
    if FUTF8Support then
      LHeader.Flag := LHeader.Flag or (1 SHL 11); // Language encoding flag, UTF8
    LHeader.FileName := StringToTBytes(LArchiveFileName);
    LHeader.FileNameLength := Length(LHeader.FileName);
    LHeader.SetExtraField_NTFS(FileName);

//    LHeader.ExtraFieldLength := 0;
    Add(LInStream, LHeader);
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.Add(Data: TBytes; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  LInStream := TBytesStream.Create(Data);
  try
    Add(LInStream, ArchiveFileName, Compression);
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.Add(Data: TStream; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LHeader: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  LHeader.Flag := 0;
  LHeader.CompressionMethod := UInt16(Compression);
  LHeader.ModifiedDateTime := DateTimeToFileDate( Now );
  LHeader.InternalAttributes := 0;
  LHeader.ExternalAttributes := 0;                                               
  if FUTF8Support then
    LHeader.Flag := LHeader.Flag or (1 SHL 11); // Language encoding flag, UTF8
  LHeader.FileName := StringToTBytes(ArchiveFileName);
  LHeader.FileNameLength := Length(LHeader.FileName);

  LHeader.ExtraFieldLength := 0;
  Add(Data, LHeader);
end;


function TZipFile.IndexOf(const FileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFiles.Count - 1 do
    if SameText(TBytesToString(FFiles[I].FileName), FileName) then
      Exit(I);
end;

end.

