unit System.Zip.TestCase;

interface

uses TestFramework, System.Zip2;

type
  TTestCase_Zip = class(TTestCase)
  protected
    function GetCompression: TZipCompression; virtual; abstract;
  published
    procedure TestCase_Zip_File;
  end;

  TTestCase_Zip_zcStored = class(TTestCase_Zip)
  protected
    function GetCompression: TZipCompression; override;
  end;

  TTestCase_Zip_zcDeflate = class(TTestCase_Zip)
  protected
    function GetCompression: TZipCompression; override;
  end;

//  TTestCase_Zip_zcLZMA = class(TTestCase_Zip)
//  protected
//    function GetCompression: TZipCompression; override;
//  end;

implementation

uses Classes, System.SysUtils, System.IOUtils{, System.Zip.LZMA};

procedure TTestCase_Zip.TestCase_Zip_File;
const
  Mult = 1024;
var fZip, fData: string;
    i: Int64;
    Z: TZipFile;
    B, C, D: TBytes;
    FileName : string;
    FileLength: Int64;
  ShortFileLength: Integer;
  index: Integer;
  InnerIndex: Integer;
  DataFile: TFileStream;
  MultIndex: Integer;
  OldFile: TFileStream;
  NewFile: TFileStream;
begin
  FileName :=  TPath.GetTempFileName;
  fZip := TPath.ChangeExtension(FileName, '.zip');
  fData := TPath.ChangeExtension(FileName, '.bin');
  TFile.Delete(FileName);

  DataFile := TFile.Create(fData);
  try
    Randomize;
    ShortFileLength := 4 * 1024 * 1024;
    SetLength(B, ShortFileLength);
    for MultIndex := 0 to Mult - 1 do
    begin
      for index := 0 to ShortFileLength - 1 do
      begin
        B[index] := Random(128);
      end;
      DataFile.WriteBuffer(B[0], Length(B));
    end;
  finally
    DataFile.Free;
  end;

  TFile.Copy(fData, FileName);
  try

//  Randomize;
//  ShortFileLength := 10 * 1024 * 1024;
//  FileLength := ShortFileLength * Mult;
//  SetLength(B, FileLength);
//  i := 0;
//  for index := 0 to ShortFileLength - 1 do
//  begin
//    for InnerIndex := 0 to Mult - 1 do
//    begin
//      B[i] := Random(128);
//      Inc(i);
//    end;
//  end;
//  Assert(i = FileLength);
//
//
//
//  TFile.WriteAllBytes(fData, B);

  // Compress File
  Z := TZipFile.Create;
  Z.Open(fZip, zmWrite);
  try
    Z.Add(fData, '', GetCompression);
  finally
    Z.Close;
    Z.Free;
    TFile.Delete(fData);
  end;

  // Decompress File
  TZipFile.ExtractZipFile(fZip, TPath.GetTempPath);
  TFile.Delete(fZip);

  CheckTrue(TFile.Exists(fData));
  CheckTrue(TFile.Exists(FileName));

  OldFile := TFile.OpenRead(FileName);
  NewFile := TFile.OpenRead(fData);
  try
    SetLength(C, ShortFileLength);
    SetLength(D, ShortFileLength);
    for MultIndex := 0 to Mult - 1 do
    begin
      OldFile.ReadBuffer(C[0], ShortFileLength);
      NewFile.ReadBuffer(D[0], ShortFileLength);
      CheckTrue(CompareMem(D, C, ShortFileLength));
    end;
  finally
    OldFile.Free;
    NewFile.Free;
  end;


//  C := TFile.ReadAllBytes(fData);
//  CheckEquals(Length(B), Length(C));
//  CheckTrue(CompareMem(B, C, Length(B)));
//
//  C := TFile.ReadAllBytes(FileName);
//  CheckEquals(Length(B), Length(C));
//  CheckTrue(CompareMem(B, C, Length(B)));

  finally
    TFile.Delete(fData);
    TFile.Delete(FileName);
  end;
end;

function TTestCase_Zip_zcStored.GetCompression: TZipCompression;
begin
  Result := zcStored;
end;

function TTestCase_Zip_zcDeflate.GetCompression: TZipCompression;
begin
  Result := zcDeflate;
end;

//function TTestCase_Zip_zcLZMA.GetCompression: TZipCompression;
//begin
//  Result := zcLZMA;
//end;

initialization
  RegisterTests([
    TTestCase_Zip_zcStored.Suite
  , TTestCase_Zip_zcDeflate.Suite
//  , TTestCase_Zip_zcLZMA.Suite
  ]);
end.
