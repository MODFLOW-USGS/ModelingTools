unit SutraFileWriterUnit;

interface

uses
  CustomModflowWriterUnit, PhastModelUnit, SysUtils,
  Generics.Collections;

type
  TSutraFileType = (sftInp, sftIcs, sftLst, sftRst, sftNod, sftEle,
    sftObs, sftObc, sftBcof, sftBcos, sftBcop, sftBcou, sftSmy, sftBcopg,
    sftBcoug, sftLkin, sftLkbc, sftLkar, sftLkbu, sftLkst, sftLkrs, sftLkn,
    sftLkh);

  TSutraFileRecord = record
    FileName: string;
    UnitNumber: integer;
  end;

  TSutraFileObject = class(TObject)
    FileName: string;
    UnitNumber: integer;
  end;

  TSutraFileList = TObjectList<TSutraFileObject>;

  TSutraFileWriter = class(TCustomFileWriter)
  private
    FNextUnitNumber: Integer;
    FFiles: array[TSutraFileType] of TSutraFileRecord;
    FBoundaries: TSutraFileList;
    FFileRoot: string;
    FArchive: Boolean;
    procedure InternalWriteFile(SutraFileName: string);
  protected
    class function Extension: string; override;
  public
    Constructor Create(AModel: TCustomModel; FileRoot: string); reintroduce;
    destructor Destroy; override;
    procedure AddFile(FileType: TSutraFileType; const FileName: string);
    procedure AddBoundaryFile(const FileName: string);
    procedure WriteFile;
  end;

var
  SutraFileWriter: TSutraFileWriter = nil;

implementation

uses
  ArchiveNodeInterface;

{ TSutraFileWriter }

procedure TSutraFileWriter.AddBoundaryFile(const FileName: string);
var
  Item: TSutraFileObject;
begin
  Item := TSutraFileObject.Create;
  FBoundaries.Add(Item);
  Item.UnitNumber := FNextUnitNumber;
  Inc(FNextUnitNumber);
  Item.FileName := FileName;
end;

procedure TSutraFileWriter.AddFile(FileType: TSutraFileType;
  const FileName: string);
begin
  Assert(FFiles[FileType].UnitNumber = -1);
  FFiles[FileType].UnitNumber := FNextUnitNumber;
  Inc(FNextUnitNumber);
  FFiles[FileType].FileName := FileName;
  case FileType of
    sftInp, sftIcs, sftLkin, sftLkbc, sftLkar:
      begin
        Model.AddModelInputFile(FileName);
      end;
    sftLst, sftRst, sftNod, sftEle, sftSmy, sftLkst, sftLKrs, sftLkbu, sftLkn,
      sftLkh, sftBcof, sftBcos, sftBcop, sftBcou, sftBcopg, sftBcoug:
      begin
        Model.AddModelOutputFile(FileName);
      end;
    sftObs, sftObc: ; // Do nothing. The .obs and .obc file names will not be the names of the files actually created by SUTRA.
    else
      Assert(False);
  end;
end;

constructor TSutraFileWriter.Create(AModel: TCustomModel; FileRoot: string);
var
  FileType: TSutraFileType;
//    SutraFileName: string;
begin
  inherited Create(AModel, etExport);
  FBoundaries := TSutraFileList.Create;
  FNextUnitNumber := 20;
  for FileType := Low(TSutraFileType) to High(TSutraFileType) do
  begin
    FFiles[FileType].UnitNumber := -1;
  end;
  FFileRoot := FileRoot;

end;

destructor TSutraFileWriter.Destroy;
begin
  FBoundaries.Free;
  inherited;
end;

class function TSutraFileWriter.Extension: string;
begin
  Assert(False);
end;

procedure TSutraFileWriter.WriteFile;
var
  SutraFileName: string;
begin
  SutraFileName := ExtractFileDir(FFileRoot);
  SutraFileName := IncludeTrailingPathDelimiter(SutraFileName);
  SutraFileName := SutraFileName + 'SUTRA.FIL';

  AddFile(sftLst, ChangeFileExt(FFileRoot, '.lst'));
  AddFile(sftRst, ChangeFileExt(FFileRoot, '.rst'));
  AddFile(sftNod, ChangeFileExt(FFileRoot, '.nod'));
  AddFile(sftEle, ChangeFileExt(FFileRoot, '.ele'));
  AddFile(sftSmy, ChangeFileExt(FFileRoot, '.smy'));

  FArchive := False;
  InternalWriteFile(SutraFileName);
  FArchive := True;
  InternalWriteFile(SutraFileName + ArchiveExt);
  Model.AddModelInputFile(SutraFileName + ArchiveExt);
//  CloseFile;
end;

procedure TSutraFileWriter.InternalWriteFile(SutraFileName: string);
var
  FileIndex: Integer;
  FileType: TSutraFileType;
  Item: TSutraFileObject;
  AFileName: string;
  ModelName: string;
begin
  ModelName := ExtractFileName(FFileRoot);
  ModelName := ChangeFileExt(ModelName, '');
  OpenFile(SutraFileName);
  try
    for FileType := Low(TSutraFileType) to High(TSutraFileType) do
    begin
      if FFiles[FileType].UnitNumber > 0 then
      begin
        case FileType of
          sftInp:
            WriteString('''INP''');
          sftIcs:
            WriteString('''ICS''');
          sftLst:
            WriteString('''LST''');
          sftRst:
            WriteString('''RST''');
          sftNod:
            WriteString('''NOD''');
          sftEle:
            WriteString('''ELE''');
          sftObs:
            WriteString('''OBS''');
          sftObc:
            WriteString('''OBC''');
          sftBcof:
            WriteString('''BCOF''');
          sftBcos:
            WriteString('''BCOS''');
          sftBcop:
            WriteString('''BCOP''');
          sftBcou:
            WriteString('''BCOU''');
          sftBcopg:
            WriteString('''BCOPG''');
          sftBcoug:
            WriteString('''BCOUG''');
          sftSmy:
            WriteString('''SMY''');
          sftLkin:
            WriteString('''LKIN''');
          sftLkbc:
            WriteString('''LKBC''');
          sftLkbu:
            WriteString('''LKBU''');
          sftLkar:
            WriteString('''LKAR''');
          sftLkst:
            WriteString('''LKST''');
          sftLKrs:
            WriteString('''LKRS''');
          sftLkn:
            WriteString('''LKN''');
          sftLkh:
            WriteString('''LKH''');
        else
          Assert(False);
        end;
        WriteInteger(FFiles[FileType].UnitNumber);
        if FArchive then
        begin
          if FileType in [sftLst, sftRst, sftNod, sftEle, sftSmy, sftLkst, sftLKrs, sftObs, sftObc] then
          begin
            AFileName := ExtractFileName(FFiles[FileType].FileName);
            AFileName := '..\..\output\' + ModelName + '\'+ AFileName
          end
          else
          begin
            AFileName := ExtractRelativePath(FFileRoot, FFiles[FileType].FileName);
          end;
          WriteString(' ''' + AFileName + '''');
        end
        else
        begin
          WriteString(' ''' + ExtractRelativePath(FFileRoot, FFiles[FileType].FileName) + '''');
        end;
        NewLine;
      end;
    end;
    for FileIndex := 0 to FBoundaries.Count - 1 do
    begin
      Item := FBoundaries[FileIndex];
      WriteString('''BCS''');
      WriteInteger(Item.UnitNumber);
//      if FileType in [sftLst, sftRst, sftNod, sftEle, sftSmy, sftLkst, sftLKrs, sftObs, sftObc] then
//      begin
//        AFileName := ExtractFileName(Item.FileName);
//        AFileName := '..\..\output\' + ModelName + '\'+ AFileName;
//      end
//      else
//      begin
        AFileName := ExtractRelativePath(FFileRoot, Item.FileName);
//      end;
      WriteString(' ''' + AFileName + '''');
      NewLine;
      Model.AddModelInputFile(Item.FileName);
    end;
  finally
    CloseFile;
  end;
end;

end.
