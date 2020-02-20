//@name is used to write a Surfer Grid File version 7.

unit SurferGridFileWriterUnit;

interface

uses
  System.Types, SurferGridFileReaderUnit, System.SysUtils, System.Classes;

type
  T2DDoubleArray = array of TDoubleDynArray;

procedure WriteSurferGridFile(FileName: string; Header: TGrid7Header;
  Data: T2DDoubleArray);

implementation

procedure WriteSurferGridFile(FileName: string; Header: TGrid7Header;
  Data: T2DDoubleArray);
const
  Version: Integer = 1;
var
  FoundFirstValue: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
  MinValue: double;
  MaxValue: double;
  AValue: double;
  FileHeader: TSurfer7Header;
  GrdFile: TFileStream;
begin
  Assert(Header.nRow = Length(Data));
  Assert(Header.nRow > 0);
  Assert(Header.nCol = Length(Data[0]));
  FoundFirstValue := False;
  MinValue := 0.0;
  MaxValue := 0.0;
  for RowIndex := 0 to Header.nRow - 1 do
  begin
    for ColIndex := 0 to Header.nCol - 1 do
    begin
      AValue := Data[RowIndex,ColIndex];
      if AValue < Header.BlankValue then
      begin
        if FoundFirstValue then
        begin
          if AValue < MinValue then
          begin
            MinValue := AValue;
          end;
          if AValue > MaxValue then
          begin
            MaxValue := AValue;
          end;
        end
        else
        begin
          MinValue := AValue;
          MaxValue := AValue;
          FoundFirstValue := True;
        end;
      end;
    end;
  end;
  Header.zMin := MinValue;
  Header.zMax := MaxValue;


  FileHeader.ID := $42525344;
  FileHeader.Size := 4;

  GrdFile := TFileStream.Create(FileName,
    fmCreate or fmShareCompat or fmShareDenyWrite);
  try

    GrdFile.Write(FileHeader, SizeOf(FileHeader));
    GrdFile.Write(Version, SizeOf(Version));

    FileHeader.ID := $44495247;
    FileHeader.Size := SizeOf(Header);
    GrdFile.Write(FileHeader, SizeOf(FileHeader));

    GrdFile.Write(Header, SizeOf(Header));


    FileHeader.ID := $41544144;
    FileHeader.Size := SizeOf(double) * Header.nRow * Header.nCol;
    GrdFile.Write(FileHeader, SizeOf(FileHeader));

    for RowIndex := 0 to Header.nRow - 1 do
    begin
      for ColIndex := 0 to Header.nCol - 1 do
      begin
        AValue := Data[ColIndex,RowIndex];
        GrdFile.Write(AValue, SizeOf(AValue));
      end;
    end;

  finally
    GrdFile.Free;
  end;

end;

end.
