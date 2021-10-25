// metadata format is described on https://www.fgdc.gov/metadata/csdgm/01.html
// through https://www.fgdc.gov/metadata/csdgm/09.html
unit ArchiveSaveUnit;

interface

uses
  System.Classes, System.SysUtils, AbBase, AbBrowse, AbZBrows, AbZipper,
  System.Generics.Collections, ArchiveNodeInterface, System.Types;

type
  TActivity = (aSaveArchive, aSaveDescriptions, aMove, aGetNames);
  TDescriptionWriting = (dwAll, dwModel);

  TProgressEvent = Procedure (Sender: TObject; Fraction: double) of object;

  EArchiveException = class(Exception);

  TFileData = class(TObject)
    FileName: string;
    // file size in MB
    FileSize: double;
    FileType: string;
    Description: string;
    URL: string;
  end;

  TFileUpdateInfo = record
    FullFileName: string;
    ArchiveFileName: string;
  end;

  TFileUpdateList = TList<TFileUpdateInfo>;


  TArchiveSaver = class(TObject)
  private
    const
    Indent = '    ';
    var
    FBaseURL: string;
//    FFileDescriptions: TStrings;
    procedure FillFileDescriptions(WhatToWrite: TDescriptionWriting);
    procedure ZipADirectory(RootDirectory, DirectoryName: string; Prefix: string);
    function DirectorySize(DirectoryName: string): Int64;
    function FileSize(FileName: string): Int64;
    function LocateDescription(ParentID, ID: string): string;
    procedure UpdateXML(MetaDataFileName: TFileName; InputMetaDataFileName: TFileName = '');
    procedure SetAppendFileDescriptions(const Value: boolean);
    procedure SetAppendMetadataFileDescriptions(const Value: boolean);
    procedure SetBaseURL(const Value: string);
//    procedure SetFileDescriptions(const Value: TStrings);
    var
    FAppendFileDescriptions: boolean;
    FAppendMetadataFileDescriptions: boolean;
    FOnProgress: TProgressEvent;
    FZipFile: TAbZipper;
    FCurrentZipFile: TAbZipper;
    FRootNodes: TArchiveNodeList;
    FBaseNode: IArchiveNodeInterface;
    FDescriptions: TStringList;
    FCurrentDescriptions: TStringList;
    FActivity: TActivity;
    FMaxProgress: double;
    FProgress: double;
    FBigFiles: TStringList;
    FRootZipFileName: string;
    FDiskFileName: string;
    FRootDirectory: string;
    FWhatToWrite: TDescriptionWriting;
    FFileDataList: TObjectList<TFileData>;
    FUpdateList: TFileUpdateList;
    procedure HandleNode(Node: IArchiveNodeInterface; Prefix: string);
    procedure ArchiveNode(Node: IArchiveNodeInterface; Prefix: string);
    function GetArchivePath(Node: IArchiveNodeInterface): string;
    procedure WriteDescription(Prefix, Text: string; MaxLineLength: integer);
    procedure CalculateMaxProgress;
    procedure CountNodeProgress(Node: IArchiveNodeInterface);
    procedure DoOnProgress(Node: IArchiveNodeInterface);
//    function GetFileSize(FileName: string): Int64;
  public
    constructor Create;
    destructor Destroy; override;
    procedure SaveArchive(ZipFileName: string);
    procedure MoveFilesToArchive(DirectoryName: string);
    procedure CreateArchive(DirectoryName: string);
    procedure WriteFileDescriptions(TextFileName: string;
      WhatToWrite: TDescriptionWriting);
    procedure UpdateMetaData(DirectoryName: string; InputMetaDataFileName: string = '');
    property RootNodes: TArchiveNodeList read FRootNodes write FRootNodes;
    property OnProgress: TProgressEvent read FOnProgress write FOnProgress;
    property AppendFileDescriptions: boolean read FAppendFileDescriptions write SetAppendFileDescriptions;
    property AppendMetadataFileDescriptions: boolean read FAppendMetadataFileDescriptions write SetAppendMetadataFileDescriptions;
//    property FileDescriptions: TStrings read FFileDescriptions write SetFileDescriptions;
    property SkippedFiles: TStringList read FBigFiles;
    procedure AppendFileDescriptionsToReadmeFileInArchive(DirectoryName: string);
    function GetUpdateInfo(DirectoryName: string): TArray<TFileUpdateInfo>;
    property BaseURL: string read FBaseURL write SetBaseURL;
  end;

implementation

uses
  System.IOUtils, Xml.VerySimple, System.StrUtils, AbArcTyp, AbUnzper;

//var FourGigaBytes: Int64;

var
  UrlTemplate: string = 'https://water.usgs.gov/GIS/dsdl/gwmodels/%0:s/%1:s%2:s';

{ TArchiveSaver }

procedure TArchiveSaver.CalculateMaxProgress;
var
  NodeIndex: Integer;
  ANode: IArchiveNodeInterface;
begin
  FMaxProgress := 0;
  for NodeIndex   := 0 to RootNodes.Count - 1 do
  begin
    ANode := RootNodes[NodeIndex] ;
    CountNodeProgress(ANode);
  end;
end;

procedure TArchiveSaver.CountNodeProgress(Node: IArchiveNodeInterface);
var
  FileName: string;
  ChildIndex: Integer;
//  AFileSize: Int64;
begin
  case FActivity of
    aSaveArchive, aMove:
      begin
        if Node.NodeType = ntFile then
        begin
          FileName := Node.NodeText;
          FMaxProgress := FMaxProgress + FileSize(FileName);
        end;
      end;
    aSaveDescriptions, aGetNames:
      begin
        FMaxProgress := FMaxProgress + 1;
      end;
    else
      Assert(False);
  end;
  for ChildIndex := 0 to Node.Count - 1 do
  begin
    CountNodeProgress(Node.Children[ChildIndex]);
  end;
end;

constructor TArchiveSaver.Create;
begin
  FBigFiles := TStringList.Create;
  FFileDataList := TObjectList<TFileData>.Create;
  FDescriptions := TStringList.Create;
  FUpdateList := TFileUpdateList.Create;
  FAppendFileDescriptions := True;
end;

//function TArchiveSaver.FileSize(FileName: string): Int64;
//var
//  MyFileReader: TFileStream;
//begin
//  MyFileReader := TFile.OpenRead(FileName);
//  try
//    result := MyFileReader.Size;
//  finally
//    MyFileReader.Free;
//  end;
//end;

function TArchiveSaver.DirectorySize(DirectoryName: string): Int64;
var
  FileNames: TStringDynArray;
  index: Integer;
begin
  result := 0;
  FileNames := TDirectory.GetFiles(DirectoryName, '*.*',
    TSearchOption.soAllDirectories);
  for index := 0 to Length(FileNames) - 1 do
  begin
    Result := result + FileSize(FileNames[index]);
  end;
end;

procedure TArchiveSaver.ZipADirectory(RootDirectory, DirectoryName: string; Prefix: string);
var
  FileNames: TStringDynArray;
  ZipFile: TAbZipper;
  ZipFileName: string;
  FileIndex: Integer;
//  FileStream: TFileStream;
//  ArchivePath: string;
//  BigFiles: Boolean;
//  AFileSize: Int64;
  AFileData: TFileData;
  DirName: string;
  UnZipper: TAbUnZipper;
  AFileName: string;
  FileList: TStringList;
  FilePosition: Integer;
  FileDir: string;
begin

  ZipFileName := Prefix + ExtractFileName(DirectoryName) + '.zip';
  ZipFileName := IncludeTrailingPathDelimiter(TDirectory.GetParent(
    DirectoryName)) + ZipFileName;
  FileNames := TDirectory.GetFiles(DirectoryName, '*.*',
    TSearchOption.soAllDirectories);
//  BigFiles := False;
  ZipFile := TAbZipper.Create(nil);
  try
    ZipFile.FileName := ZipFileName;
    try
      ZipFile.BaseDirectory := DirectoryName;
      ZipFile.TempDirectory := DirectoryName;
      ZipFile.StoreOptions := ZipFile.StoreOptions + [soReplace, soRecurse];
      ZipFile.AddFiles('*.*', faAnyFile);
    finally
      ZipFile.Save;
    end;
  finally
    ZipFile.Free;
  end;

  UnZipper := TAbUnZipper.Create(nil);
  try
    UnZipper.OpenArchive(ZipFileName);
    try
      UnZipper.BaseDirectory := DirectoryName;
        FileList := TStringList.Create;
        try
          for FileIndex := 0 to UnZipper.ZipArchive.Count - 1 do
          begin
            AFileName := UnZipper.Items[FileIndex].FileName;
            if TPath.DirectorySeparatorChar <> '/' then
            begin
              AFileName := StringReplace(AFileName, '/',
                TPath.DirectorySeparatorChar, [rfReplaceAll, rfIgnoreCase]);
            end;
            FileList.Add(AFileName);
          end;
          for FileIndex := 0 to Length(FileNames) - 1 do
          begin
            AFileName := ExtractRelativePath(IncludeTrailingPathDelimiter(DirectoryName), FileNames[FileIndex]);
            FilePosition := FileList.IndexOf(AFileName);
            if FilePosition >= 0 then
            begin
              TFile.Delete(FileNames[FileIndex]);
            end
            else
            begin
              FBigFiles.Add(FileNames[FileIndex]);
            end;
          end;
        finally
          FileList.Free;
        end;

    finally
      UnZipper.CloseArchive;
    end;
  finally
    UnZipper.Free;
  end;


  AFileData := TFileData.Create;
  FFileDataList.Add(AFileData);
  AFileData.FileName := ExtractFileName(ZipFileName);
  AFileData.FileSize := FileSize(ZipFileName) / 1024 / 1024;
  AFileData.FileType := 'ZIP File';
  DirName := ExtractFileName(ExcludeTrailingPathDelimiter(DirectoryName));
  if Prefix = '' then
  begin
    AFileData.Description := LocateDescription(DirName,
      DirName);
  end
  else
  begin
    AFileData.Description := LocateDescription(LeftStr(Prefix, Length(Prefix)-1),
      DirName);
  end;

  if IncludeTrailingPathDelimiter(RootDirectory)
    = IncludeTrailingPathDelimiter(DirectoryName) then
  begin
    FileDir := ''
  end
  else
  begin
    FileDir := Copy(DirectoryName,
      Length(IncludeTrailingPathDelimiter(RootDirectory)) + 1, MaxInt)
      + '/';
    FileDir :=  StringReplace(FileDir, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  end;

  AFileData.URL := Format(UrlTemplate,
    [ExtractFileName(ExcludeTrailingPathDelimiter(FRootDirectory)),
    FileDir, AFileData.FileName]);

  FileNames := TDirectory.GetFiles(DirectoryName, '*.*',
    TSearchOption.soAllDirectories);
  if Length(FileNames) = 0 then
  begin
    TDirectory.Delete(DirectoryName, True);
  end;
end;

procedure TArchiveSaver.UpdateMetaData(DirectoryName: string;
  InputMetaDataFileName: string = '');
var
  FileNames: TStringDynArray;
  FileIndex: Integer;
  AFileName: string;
  Extension: string;
  MetaDataFile: string;
  ParentDirectory: string;
  ParentParent: string;
  AFileData: TFileData;
  Prefix: string;
  ID: string;
  UsedDirectory: string;
  RootDirName: string;
begin
  RootDirName := ExtractFileName(ExcludeTrailingPathDelimiter(DirectoryName));
  FRootDirectory := IncludeTrailingPathDelimiter(DirectoryName);
  MetaDataFile := '';
  FFileDataList.Clear;
  FileNames := TDirectory.GetFiles(DirectoryName, '*.*',
    TSearchOption.soAllDirectories);
  for FileIndex := 0 to Length(FileNames) - 1 do
  begin
    AFileName := FileNames[FileIndex];
    Extension := UpperCase(ExtractFileExt(AFileName));
    if (Extension <> '') and (Extension[1] = '.') then
    begin
      Extension := Copy(Extension, 2, MaxInt);
    end;
    ParentDirectory := ExtractFileDir(AFileName);
    ParentParent := TDirectory.GetParent(ParentDirectory);
    ParentDirectory := Copy(ParentDirectory, Length(ParentParent)+2, MaxInt);
    if Extension = 'XML' then
    begin
      if ParentDirectory = 'webrelease' then
      begin
        MetaDataFile := AFileName;
      end;
    end;

    AFileData := TFileData.Create;
    FFileDataList.Add(AFileData);
    AFileData.FileName := ExtractFileName(AFileName);
    AFileData.FileSize := FileSize(AFileName) / 1024 / 1024;
    AFileData.FileType := Extension + ' File';

    if Extension <> 'ZIP' then
    begin
      ID := AFileData.FileName;
      if ParentDirectory = 'webrelease' then
      begin
        Prefix := 'webrelease';
      end
      else
      begin
        Prefix := AFileData.FileName;
      end;
    end
    else
    begin
      ID := ChangeFileExt(AFileData.FileName, '');
      Prefix := ChangeFileExt(ID, '');
      Prefix := ChangeFileExt(Prefix, '');
    end;
    AFileData.Description := LocateDescription(Prefix, ID);
    if ParentDirectory = RootDirName then
    begin
      UsedDirectory := '';
    end
    else
    begin
      UsedDirectory := ParentDirectory + '/';
      UsedDirectory := StringReplace(UsedDirectory, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    end;
    AFileData.URL := Format(UrlTemplate,
      [ExtractFileName(ExcludeTrailingPathDelimiter(FRootDirectory)),
      UsedDirectory, AFileData.FileName])
  end;
  if MetaDataFile <> '' then
  begin
    UpdateXML(MetaDataFile, InputMetaDataFileName);
  end
  else
  begin
    raise Exception.Create('No metadata file present in webrelease directory.');
  end;
end;

procedure TArchiveSaver.AppendFileDescriptionsToReadmeFileInArchive(DirectoryName: string);
var
  ReadMeFileName: string;
  ReadMeFile: TStringList;
begin
  ReadMeFileName := IncludeTrailingPathDelimiter(DirectoryName) + 'readme.txt';
  if FileExists(ReadMeFileName) then
  begin
    ReadMeFile := TStringList.Create;
    try
      ReadMeFile.LoadFromFile(ReadMeFileName);
      FillFileDescriptions(dwModel);
      ReadMeFile.AddStrings(FDescriptions);
      FillFileDescriptions(dwAll);
      ReadMeFile.AddStrings(FDescriptions);
      ReadMeFile.SaveToFile(ReadMeFileName);
    finally
      ReadMeFile.Free;
    end;
  end;
end;

procedure TArchiveSaver.FillFileDescriptions(WhatToWrite: TDescriptionWriting);
var
  NodeIndex: Integer;
  ANode: IArchiveNodeInterface;
begin
  FActivity := aSaveDescriptions;
  FWhatToWrite := WhatToWrite;
  if Assigned(FOnProgress) then
  begin
    CalculateMaxProgress;
    FProgress := 0;
  end;
  FDescriptions.Clear;
  FCurrentDescriptions := FDescriptions;
  FDescriptions.Add(Indent + 'Files:');
  FDescriptions.Add(Indent + '-----');
  for NodeIndex := 0 to RootNodes.Count - 1 do
  begin
    ANode := RootNodes[NodeIndex];
    HandleNode(ANode, Indent);
  end;
end;

procedure TArchiveSaver.UpdateXML(MetaDataFileName: TFileName; InputMetaDataFileName: TFileName = '');
var
  Xml: TXmlVerySimple;
  distinfo: TXmlNode;
  stdorder: TXmlNode;
  FileIndex: Integer;
  AFileData: TFileData;
  digform: TXmlNode;
  digtinfo: TXmlNode;
  formname: TXmlNode;
  formvern: TXmlNode;
  formspec: TXmlNode;
  formcont: TXmlNode;
  transize: TXmlNode;
  digtopt: TXmlNode;
  onlinopt: TXmlNode;
  computer: TXmlNode;
  networka: TXmlNode;
  networkr: TXmlNode;
//  fees: TXmlNode;
  ExistingNodes: TList<TXmlNode>;
  ItemIndex: Integer;
  ExistingNode: TXmlNode;
begin
  Xml := TXmlVerySimple.Create;
  try
    if InputMetaDataFileName = '' then
    begin
      InputMetaDataFileName := MetaDataFileName;
    end;
    Xml.NodeIndentStr := #9;
    Xml.LoadFromFile(InputMetaDataFileName);
    distinfo := Xml.DocumentElement.Find('distinfo');
    if distinfo = nil then
    begin
      distinfo := Xml.DocumentElement.AddChild('distinfo');
    end;
    stdorder := distinfo.Find('stdorder');
    if stdorder = nil then
    begin
      stdorder := distinfo.AddChild('stdorder');
    end;

    stdorder.ChildNodes.OwnsObjects := False;
    ExistingNodes := TList<TXmlNode>.Create;
    try
      for ItemIndex := 0 to stdorder.ChildNodes.Count - 1 do
      begin
        ExistingNode := stdorder.ChildNodes[ItemIndex];
        if ExistingNode.Name <> 'digform' then
        begin
          ExistingNodes.Add(stdorder.ChildNodes[ItemIndex]);
        end
        else
        begin
          ExistingNode.Free;
        end;
      end;
      stdorder.Clear;

      for ItemIndex := 0 to ExistingNodes.Count - 1 do
      begin
        ExistingNode := ExistingNodes[ItemIndex];
        if ExistingNode.Name = 'nondig' then
        begin
          stdorder.ChildNodes.Add(ExistingNode);
          ExistingNodes[ItemIndex] := nil
        end;
      end;

      for FileIndex := 0 to FFileDataList.Count - 1 do
      begin
        AFileData := FFileDataList[FileIndex];
        digform := stdorder.AddChild('digform');
        digtinfo := digform.AddChild('digtinfo');
        formname := digtinfo.AddChild('formname');
        formname.Text := AFileData.FileName;
        formvern := digtinfo.AddChild('formvern');
        formvern.Text := '1.0';
        formspec := digtinfo.AddChild('formspec');
        formspec.Text := AFileData.FileType;
        formcont := digtinfo.AddChild('formcont');
        formcont.Text := Trim(AFileData.Description);
        transize := digtinfo.AddChild('transize');
        transize.Text := Format('%g', [AFileData.FileSize]);
        digtopt := digform.AddChild('digtopt');
        onlinopt := digtopt.AddChild('onlinopt');
        computer := onlinopt.AddChild('computer');
        networka := computer.AddChild('networka');
        networkr := networka.AddChild('networkr');
        networkr.Text := AFileData.URL;
      end;

      for ItemIndex := 0 to ExistingNodes.Count - 1 do
      begin
        ExistingNode := ExistingNodes[ItemIndex];
        if ExistingNode <> nil then
        begin
          stdorder.ChildNodes.Add(ExistingNode);
        end;
      end;

    finally
      ExistingNodes.Free;
      stdorder.ChildNodes.OwnsObjects := True;
    end;
      //          Lines.Add(Format(Template, [AFileData.FileName, AFileData.FileType,
      //            Trim(AFileData.Description), AFileData.FileSize, AFileData.URL]));
//    finally
//    end;
    Xml.SaveToFile(MetaDataFileName);
  finally
    Xml.Free;
  end;
end;


function TArchiveSaver.LocateDescription(ParentID, ID: string): string;
var
  RootIndex: Integer;
  ARootNode: IArchiveNodeInterface;
  ChildIndex: Integer;
  AChildNode: IArchiveNodeInterface;
begin
  result := '';
  for RootIndex := 0 to RootNodes.Count - 1 do
  begin
    ARootNode := RootNodes[RootIndex];
    if ARootNode.NodeText = ParentID then
    begin
      if ARootNode.NodeText = ID then
      begin
        Result := ARootNode.Description;
        Exit;
      end
      else
      begin
        for ChildIndex := 0 to ARootNode.Count - 1 do
        begin
          AChildNode := ARootNode.Children[ChildIndex];
          if AChildNode.NodeType = ntFile then
          begin
            if ExtractFileName(AChildNode.NodeText) = ID then
            begin
              Result := AChildNode.Description;
              Exit;
            end;
          end
          else
          begin
            if AChildNode.NodeText = ID then
            begin
              Result := AChildNode.Description;
              Exit;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TArchiveSaver.CreateArchive(DirectoryName: string);
const
  OneGB: Int64 = 1024*1024*1024;
var
  SubDirectories: TStringDynArray;
  DirIndex: Integer;
  ModelDirectories: TStringDynArray;
  ModelIndex: Integer;
  SizeLimit: Int64;
  DirName: string;
  WebFiles: TStringDynArray;
  FileIndex: Integer;
  AFile: string;
  Extension: string;
  MetaDataFile: string;
  AFileData: TFileData;
  Files: TStringDynArray;
  RootDir: string;
  FileDir: string;
begin
  SizeLimit := OneGB div 2 * 5; // 2.5 GB
  FBigFiles.Clear;
//  SizeLimit := 1024*1024;
  MetaDataFile := '';
  FFileDataList.Clear;


  MoveFilesToArchive(DirectoryName);
  RootDir := ExcludeTrailingPathDelimiter(DirectoryName);
  Files := TDirectory.GetFiles(DirectoryName);
  for FileIndex := 0 to Length(Files) - 1 do
  begin
    AFile := Files[FileIndex];
    Extension := UpperCase(ExtractFileExt(AFile));
    if (Extension <> '') and (Extension[1] = '.') then
    begin
      Extension := Copy(Extension, 2, MaxInt);
    end;
    AFileData := TFileData.Create;
    FFileDataList.Add(AFileData);
    AFileData.FileName := ExtractFileName(AFile);
    FileDir := ExcludeTrailingPathDelimiter(ExtractFileDir(AFile));
    if RootDir = FileDir then
    begin
      FileDir := '';
    end
    else
    begin
      FileDir := Copy(FileDir, Length(RootDir)+2, MaxInt) + '/';
      FileDir := StringReplace(FileDir, '\', '/', [rfReplaceAll, rfIgnoreCase]);
    end;
    AFileData.FileSize := FileSize(AFile) / 1024 / 1024;
    AFileData.FileType := Extension + ' File';
    AFileData.Description := LocateDescription(AFileData.FileName,
      AFileData.FileName);
    AFileData.URL := Format(UrlTemplate,
      [ExtractFileName(ExcludeTrailingPathDelimiter(FRootDirectory)),
      FileDir, AFileData.FileName])
  end;

  SubDirectories := TDirectory.GetDirectories(DirectoryName);
  for DirIndex := 0 to Length(SubDirectories) - 1 do
  begin
    DirName := ExtractFileName(SubDirectories[DirIndex]);
    if DirName = 'webrelease' then
    begin
      WebFiles := TDirectory.GetFiles(SubDirectories[DirIndex]);
      for FileIndex := 0 to Length(WebFiles) - 1 do
      begin
        AFile := WebFiles[FileIndex];
        Extension := UpperCase(ExtractFileExt(AFile));
        if (Extension <> '') and (Extension[1] = '.') then
        begin
          Extension := Copy(Extension, 2, MaxInt);
        end;
        if Extension = 'XML' then
        begin
          MetaDataFile := AFile;
        end;

        AFileData := TFileData.Create;
        FFileDataList.Add(AFileData);
        AFileData.FileName := ExtractFileName(AFile);
        AFileData.FileSize := FileSize(AFile) / 1024 / 1024;
        AFileData.FileType := Extension + ' File';
        AFileData.Description := LocateDescription('webrelease',
          AFileData.FileName);
        AFileData.URL := Format(UrlTemplate,
          [ExtractFileName(ExcludeTrailingPathDelimiter(FRootDirectory)), 'webrelease/', AFileData.FileName])
      end;
//      Continue;
    end
    else if (DirName = 'model') or (DirName = 'output') then
    begin
      if DirectorySize(SubDirectories[DirIndex]) > SizeLimit then
      begin
        ModelDirectories := TDirectory.GetDirectories(SubDirectories[DirIndex]);
        for ModelIndex := 0 to Length(ModelDirectories) - 1 do
        begin
//          ZipADirectory(ModelDirectories[ModelIndex], DirName + '.');
          ZipADirectory(DirectoryName, ModelDirectories[ModelIndex], '');
        end;
      end
      else
      begin
        ZipADirectory(DirectoryName, SubDirectories[DirIndex], '');
      end;
    end
    else
    begin
      ZipADirectory(DirectoryName, SubDirectories[DirIndex], '')
    end;
  end;
  if (MetaDataFile <> '') and AppendMetadataFileDescriptions then
  begin
    UpdateXML(MetaDataFile);
  end;
end;

destructor TArchiveSaver.Destroy;
begin
  FUpdateList.Free;
  FFileDataList.Free;
  FBigFiles.Free;
  FDescriptions.Free;
  inherited;
end;

procedure TArchiveSaver.DoOnProgress(Node: IArchiveNodeInterface);
var
  FileName: string;
begin
  if Assigned(FOnProgress) and (FMaxProgress <> 0) then
  begin
    case FActivity of
      aSaveArchive, aMove, aGetNames:
        begin
          if Node.NodeType = ntFile then
          begin
            FileName := Node.NodeText;
            FProgress := FProgress + FileSize(FileName);
          end;
        end;
      aSaveDescriptions:
        begin
          FProgress := FProgress + 1;
        end;
      else
        Assert(False);
    end;
    FOnProgress(self, FProgress/FMaxProgress);
  end;
end;

function TArchiveSaver.GetArchivePath(Node: IArchiveNodeInterface): string;
var
  ParentNode: IArchiveNodeInterface;
  function ContinuePath: boolean;
  begin
    result := ParentNode <> nil;
    if result then
    begin
      if FActivity in [aSaveArchive] then
      begin
        result := (ParentNode <> FBaseNode)
          and (ParentNode.NodeType <> ntArchiveRoot);
      end
      else
      begin
        result := (ParentNode.NodeType <> ntArchiveRoot);
      end;
    end;
  end;
begin
  result := '';
  ParentNode := Node.ParentNode;
  while ContinuePath do
  begin
    if result = '' then
    begin
      result := StringReplace(ParentNode.NodeText, ' ', '_', [rfReplaceAll, rfIgnoreCase]);
      if result = 'model.externalfiles' then
      begin
        result := 'externalfiles';
      end;
    end
    else
    begin
      result := IncludeTrailingPathDelimiter(
        StringReplace(ParentNode.NodeText, ' ', '_', [rfReplaceAll, rfIgnoreCase])) + result;
    end;
    ParentNode := ParentNode.ParentNode;
  end;
end;

function TArchiveSaver.GetUpdateInfo(
  DirectoryName: string): TArray<TFileUpdateInfo>;

var
  NodeIndex: Integer;
  ANode: IArchiveNodeInterface;
//  CurDir: string;
//  FreeSpace: Int64;
begin
  FActivity := aGetNames;
  FUpdateList.Clear;
  FRootDirectory := IncludeTrailingPathDelimiter(DirectoryName);
//  TDirectory.GetFiles(FRootDirectory);
  CalculateMaxProgress;
  if Assigned (FOnProgress) then
  begin
    FProgress := 0
  end;
  for NodeIndex   := 0 to RootNodes.Count - 1 do
  begin
    ANode := RootNodes[NodeIndex] ;
    HandleNode(ANode, Indent);
  end;

  result := FUpdateList.ToArray;
end;

procedure TArchiveSaver.HandleNode(Node: IArchiveNodeInterface; Prefix: string);
var
  ChildIndex: Integer;
  ChildNode: IArchiveNodeInterface;
  LocalZipFile: TAbZipper;
  ArchiveFileName: string;
  DiskFileName: string;
  FileStream: TFileStream;
  CategoryNode: IArchiveNodeInterface;
//  EmptyData: TBytes;
  LocalDescriptions: TStringList;
  TempName: string;
begin
  if Node.NodeType = ntSkip then
  begin
    Exit;
  end;
  if Node.NodeType = ntFile then
  begin
    ArchiveNode(Node, Prefix);
  end
  else
  begin
    LocalZipFile := nil;
    ArchiveFileName := '';
    if (Node.Count > 0)
      and (Node.NodeType in [ntModel, ntCategoryCompressed]) then
    begin
      case FActivity of
        aSaveArchive:
          begin
            LocalZipFile := TAbZipper.Create(nil);
            FCurrentZipFile := LocalZipFile;
            TempName := TPath.GetTempFileName;
            DiskFileName := ChangeFileExt(TempName, '.zip');
            TFile.Delete(TempName);
//            LocalZipFile.Open(DiskFileName, zmWrite);
            LocalZipFile.FileName := DiskFileName;
            LocalZipFile.BaseDirectory := ExtractFileDir(LocalZipFile.FileName);
            LocalZipFile.StoreOptions := LocalZipFile.StoreOptions + [soReplace];
          end;
        aSaveDescriptions:
          begin
            LocalDescriptions := TStringList.Create;
            FCurrentDescriptions := LocalDescriptions;
//            ArchiveNode(Node, Prefix);
          end;
        aMove, aGetNames: ; // do nothing
        else
          Assert(False);
      end;

      if Node.NodeType in [ntModel, ntFolder] then
      begin
        CategoryNode := Node.ParentNode;
      end
      else
      begin
        CategoryNode := Node;
      end;

      ArchiveFileName := IncludeTrailingPathDelimiter(GetArchivePath(Node))
        +  IncludeTrailingPathDelimiter(Node.NodeText) ;
      case FActivity of
        aSaveArchive:
          begin
            if CategoryNode.NodeText = StrModelOutputFiles then
            begin
//              SetLength(EmptyData, 0);
//              FZipFile.Add(EmptyData, ArchiveFileName);
            end;
          end;
        aSaveDescriptions:
          begin
            if (FWhatToWrite = dwAll) or (Node.NodeType = ntModel) then
            begin

              FCurrentDescriptions.Add('');
              if FWhatToWrite = dwAll then
              begin
                FCurrentDescriptions.Add(Prefix + ArchiveFileName);
                FCurrentDescriptions.Add(Prefix + Indent + 'Description:');
                FCurrentDescriptions.Add(Prefix + Indent + '-----------');
              end
              else
              begin
                FCurrentDescriptions.Add(Prefix + Node.NodeText);
                FCurrentDescriptions.Add(Prefix + Indent + '-----------');
              end;
              WriteDescription(Prefix + Indent, Node.Description, 80);
              if (FWhatToWrite = dwAll) and (Node.Count > 0)
                and (Node.Children[0].NodeType = ntFile) then
              begin
                FCurrentDescriptions.Add('');
                FCurrentDescriptions.Add(Prefix + Indent + 'Files:');
                FCurrentDescriptions.Add(Prefix + Indent + '-----');
              end;
            end;
          end;
        aMove, aGetNames: ; // do nothing
        else
          Assert(False);
      end;
      ArchiveFileName := IncludeTrailingPathDelimiter(GetArchivePath(Node))
        + Node.NodeText +  '.zip';

      if FActivity = aSaveArchive then
      begin
        if CategoryNode.NodeText = StrModelOutputFiles then
        begin
          ArchiveFileName := IncludeTrailingPathDelimiter(GetArchivePath(Node))
            + StrModelOutputFiles + '.'
            + Node.NodeText +  '.zip';
        end;
//        DiskFileName := TPath.GetTempFileName;
//        LocalZipFile.Open(DiskFileName, zmWrite);
        FDiskFileName := ArchiveFileName;
      end
      else
      begin
        FDiskFileName := FRootZipFileName;
      end;
      FBaseNode := Node;
    end
    else
    begin
      LocalZipFile := nil;
      LocalDescriptions := nil;
      FDiskFileName := FRootZipFileName;
    end;
    try
      for ChildIndex := 0 to Node.Count - 1 do
      begin
        ChildNode := Node.Children[ChildIndex];
        HandleNode(ChildNode, Prefix + Indent);
      end;
    finally
      case FActivity of
        aSaveArchive:
          begin
            if LocalZipFile <> nil then
            begin
              try
//                LocalZipFile.Close;
                LocalZipFile.Save;
                LocalZipFile.Free;
                FCurrentZipFile := FZipFile;
                FileStream := TFile.OpenRead(DiskFileName);
                try
                  if Assigned(FOnProgress) then
                  begin
                    FMaxProgress := FMaxProgress + FileStream.Size;
//                    FOnProgress(self, FProgress/FMaxProgress);
                  end;
//                  if FileStream.Size >= FourGigaBytes then
//                  begin
//                    raise EArchiveException.Create(Format(
//                      'Error creating %s. File is greater than four gigabytes in size',
//                      [ArchiveFileName]));
//                  end
//                  else
//                  begin
                    FZipFile.AddFromStream(ArchiveFileName, FileStream);
//                  end;
                  if Assigned(FOnProgress) then
                  begin
                    FProgress := FProgress + FileStream.Size;
                    FOnProgress(self, FProgress/FMaxProgress);
                  end;
                finally
                  FileStream.Free;
                end;
              finally
                TFile.Delete(DiskFileName);
              end;
              FBaseNode := nil;
            end;
          end;
        aSaveDescriptions:
          begin
            if LocalDescriptions <> nil then
            begin
              FCurrentDescriptions := FDescriptions;
              FDescriptions.AddStrings(LocalDescriptions);
              FreeAndNil(LocalDescriptions);
            end;
          end;
        aMove, aGetNames: ; // do nothing
        else
          Assert(False);
      end;
    end;
  end;
  DoOnProgress(Node);
end;

procedure TArchiveSaver.MoveFilesToArchive(DirectoryName: string);
var
  NodeIndex: Integer;
  ANode: IArchiveNodeInterface;
  CurDir: string;
  FreeSpace: Int64;
begin
  FActivity := aMove;
  FRootDirectory := IncludeTrailingPathDelimiter(DirectoryName);
  TDirectory.GetFiles(FRootDirectory);
  if not TDirectory.IsEmpty(FRootDirectory) then
  begin
    raise EArchiveException.Create('The archive directory must be empty');
  end;
  CalculateMaxProgress;
  if Assigned (FOnProgress) then
  begin
    FProgress := 0
  end;
  CurDir := GetCurrentDir;
  try
    SetCurrentDir(DirectoryName);
    FreeSpace := DiskFree(0);
    if FreeSpace < FMaxProgress then
    begin
      raise EArchiveException.Create(Format(
        'Not enough free disk space. Required disk space = %d',
        [FMaxProgress]));
    end;
  finally
    SetCurrentDir(CurDir);
  end;
  for NodeIndex   := 0 to RootNodes.Count - 1 do
  begin
    ANode := RootNodes[NodeIndex] ;
    HandleNode(ANode, Indent);
  end;

  if AppendFileDescriptions then
  begin
    AppendFileDescriptionsToReadmeFileInArchive(DirectoryName);
  end;
end;

procedure TArchiveSaver.ArchiveNode(Node: IArchiveNodeInterface; Prefix: string);
var
  FullPath: string;
  ArchivePath: string;
  FileStream: TFileStream;
  ArchiveFileName: string;
  ArchiveDirectory: string;
  ShouldHandle: Boolean;
  UpdateInfo: TFileUpdateInfo;
begin
  ArchivePath := GetArchivePath(Node);
  if ArchivePath <> '' then
  begin
    ArchivePath := IncludeTrailingPathDelimiter(ArchivePath);
  end;
  FullPath := Node.NodeText;
  ShouldHandle := TFile.Exists(FullPath);
//  if not ShouldHandle and (Node.NodeType in [ntCategory, ntModel, ntFolder,
//    ntArchiveRoot, ntCategoryUncompressed, ntCategoryCompressed])  then
//  begin
//    ShouldHandle := FActivity = aSaveDescriptions;
//  end;
  if ShouldHandle then
  begin
    if Pos(Node.ModelDirectory, FullPath) = 1 then
    begin
      ArchivePath := ArchivePath
        + ExtractRelativePath(Node.ModelDirectory, FullPath)
    end
    else
    begin
      ArchivePath := ArchivePath + ExtractFileName(FullPath);
    end;

    if ExtractFileExt(ArchivePath) = ArchiveExt then
    begin
      ArchivePath := ChangeFileExt(ArchivePath, '');
    end;

    case FActivity of
      aSaveArchive:
        begin
          FileStream := TFile.OpenRead(FullPath);
          try
//            if FileStream.Size >= FourGigabytes then
//            begin
////              FBigFiles.Add(Format('"%0:s" "%1:s"', [FullPath, ArchivePath]))
//              FBigFiles.Add(Format('"%0:s" "%1:s" in "%2:s"', [FullPath, ArchivePath, FDiskFileName]));
//            end
//            else
//            begin
              FCurrentZipFile.AddFromStream(ArchivePath, FileStream);
//            end;
          finally
            FileStream.Free;
          end;
        end;
      aSaveDescriptions:
        begin
          if FWhatToWrite = dwAll then
          begin
            WriteDescription(Prefix, ArchivePath + ':', 80);
            WriteDescription(Prefix + Indent, Node.Description, 80);
            FCurrentDescriptions.Add('');
          end;
//          FCurrentDescriptions.Add(Prefix + ArchivePath + ': ' + Node.Description)
        end;
      aMove:
        begin
          // copy file
          ArchiveFileName := FRootDirectory + ArchivePath;
          ArchiveDirectory := ExtractFileDir(ArchiveFileName);
          if not TDirectory.Exists(ArchiveDirectory) then
          begin
            TDirectory.CreateDirectory(ArchiveDirectory);
          end;
          if TFile.Exists(ArchiveFileName) then
          begin
            raise EArchiveException.Create(Format(
              'Cannot move %0:s to %1:s because a file by that name is already exists in that location',
              [FullPath, ArchiveFileName]));
          end;
          TFile.Copy(FullPath, ArchiveFileName);
        end;
      aGetNames:
        begin
          UpdateInfo.FullFileName := FullPath;
          UpdateInfo.ArchiveFileName := ArchivePath;
          FUpdateList.Add(UpdateInfo);
        end;
      else
        Assert(False);

    end;
  end
  else
  begin

    raise EArchiveError.Create(Format(
      'The file %0:s under %1:s does not exist', [FullPath, ArchivePath]));
  end;
end;


procedure TArchiveSaver.SaveArchive(ZipFileName: string);
var
  NodeIndex: Integer;
  ANode: IArchiveNodeInterface;
begin
  FBigFiles.Clear;
  FActivity := aSaveArchive;
  if Assigned (FOnProgress) then
  begin
    CalculateMaxProgress;
    FProgress := 0
  end;
  FZipFile := TAbZipper.Create(nil);
  try
    FCurrentZipFile := FZipFile;
    FRootZipFileName := ZipFileName;
    FZipFile.FileName := ZipFileName;
    FZipFile.BaseDirectory := ExtractFileDir(FZipFile.FileName);
    FZipFile.StoreOptions := FZipFile.StoreOptions + [soReplace];
//    FZipFile.Open(ZipFileName, zmWrite);
    try
      for NodeIndex   := 0 to RootNodes.Count - 1 do
      begin
        ANode := RootNodes[NodeIndex] ;
        HandleNode(ANode, Indent);
      end;
    finally
      FZipFile.Save;
//      FZipFile.Close;
    end;
  finally
    FreeAndNil(FZipFile);
  end;
end;

procedure TArchiveSaver.SetAppendFileDescriptions(const Value: boolean);
begin
  FAppendFileDescriptions := Value;
end;

procedure TArchiveSaver.SetAppendMetadataFileDescriptions(const Value: boolean);
begin
  FAppendMetadataFileDescriptions := Value;
end;

procedure TArchiveSaver.SetBaseURL(const Value: string);
begin
  FBaseURL := Value;
  if FBaseURL = '' then
  begin
    UrlTemplate:= 'https://water.usgs.gov/GIS/dsdl/gwmodels/%0:s/%1:s%2:s';
  end
  else
  begin
    if FBaseURL[Length(FBaseURL)] <> '/' then
    begin
      FBaseURL := FBaseURL + '/';
    end;
    UrlTemplate:= FBaseURL + '%0:s/%1:s%2:s';
  end;

end;

procedure TArchiveSaver.WriteDescription(Prefix, Text: string;
  MaxLineLength: integer);
var
  IntList: TList<Integer>;
  CharIndex: Integer;
  StartIndex: Integer;
  PrefixLength: Integer;
  TextToWrite: string;
  Lines: TStringList;
  LineIndex: Integer;
begin
  Lines := TStringList.Create;
  IntList := TList<Integer>.Create;
  try
    Lines.Text := Text;
    for LineIndex := 0 to Lines.Count - 1 do
    begin
      Text := Lines[LineIndex];
      IntList.Clear;
      for CharIndex := 1 to Length(Text) do
      begin
        if Text[CharIndex] = ' ' then
        begin
          IntList.Add(CharIndex)
        end;
      end;
      IntList.Add(Length(Text)+1);
      StartIndex := 1;
      PrefixLength := Length(Prefix);
      for CharIndex := 1 to IntList.Count - 1 do
      begin
        if PrefixLength + IntList[CharIndex] - StartIndex > MaxLineLength then
        begin
          TextToWrite := Copy(Text, StartIndex, IntList[CharIndex-1]-StartIndex);
          FCurrentDescriptions.Add(Prefix + TextToWrite);
          StartIndex := IntList[CharIndex-1]+1;
        end;
      end;
      TextToWrite := Copy(Text, StartIndex, MaxInt);
      FCurrentDescriptions.Add(Prefix + TextToWrite);
    end;
//    FCurrentDescriptions.Add('');
  finally
    IntList.Free;
    Lines.Free;
  end;
end;

procedure TArchiveSaver.WriteFileDescriptions(TextFileName: string;
  WhatToWrite: TDescriptionWriting);
begin
  FillFileDescriptions(WhatToWrite);
  FDescriptions.SaveToFile(TextFileName);
end;

function TArchiveSaver.FileSize(FileName: string): Int64;
var
  FileStream: TFileStream;
begin
  if TFile.Exists(FileName) then
  begin
    FileStream := TFile.OpenRead(FileName);
    try
      result := FileStream.Size;
    finally
      FileStream.Free;
    end;
  end
  else
  begin
    result := 0;
  end;
end;

initialization
// FourGigaBytes := 1024*1024;
// FourGigaBytes := FourGigaBytes*1024*4;

end.


