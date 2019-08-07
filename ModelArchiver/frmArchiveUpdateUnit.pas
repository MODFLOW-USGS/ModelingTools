unit frmArchiveUpdateUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.TreeView, System.Generics.Collections, AbBase, AbBrowse, AbZBrows,
  ArchiveSaveUnit, FMX.StdCtrls, FMX.Controls.Presentation, AbZipper,
  FMX.ScrollBox, FMX.Memo;

type
  TExistingArchiveItem = class(TTreeviewItem)
  private
    FChildItems: TDictionary<string,TExistingArchiveItem>;
    FArchiveLastModified: TDateTime;
    FArchiveFullFileName: string;
    FOriginalFileLastModified: TDateTime;
    FOriginalFileFullFileName: string;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ChildItems: TDictionary<string,TExistingArchiveItem>
      read FChildItems;
    property ArchiveLastModified: TDateTime read FArchiveLastModified
      write FArchiveLastModified;
    property ArchiveFullFileName: string read FArchiveFullFileName
      write FArchiveFullFileName;
    property OriginalFileLastModified: TDateTime read FOriginalFileLastModified
      write FOriginalFileLastModified;
    property OriginalFileFullFileName: string read FOriginalFileFullFileName
      write FOriginalFileFullFileName;
  end;

  TfrmArchiveUpdate = class(TForm)
    tvExistingArchive: TTreeView;
    zpbrwsExistingZipFile: TAbZipBrowser;
    pnlBottom: TPanel;
    btnUpdate: TButton;
    zprUpdate: TAbZipper;
    btnCancel: TButton;
    btnHelp: TButton;
    memo2: TMemo;
    spl1: TSplitter;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure btnUpdateClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnCancelClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure tvExistingArchiveChangeCheck(Sender: TObject);
  private
    const
      Epsilon = 1e-4;
    procedure ZipDirectory(ModelDir: string);
    var
    FUpdateItems: TArray<TFileUpdateInfo>;
    FArchiveDirectory: string;
    FNewDirectories: TStringList;
    FDisplayingData: Boolean;
    procedure DisplayZipFile(const FileName: string;
      TreeItem: TExistingArchiveItem);
    procedure PaintItem(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure AddToArchive(Item: TExistingArchiveItem);
    procedure RemoveFromArchive(Item: TExistingArchiveItem);
    procedure ReplaceInArchive(Item: TExistingArchiveItem);
    procedure ZipModelOrOutput(ModelDir: string);
    { Private declarations }
  public
    property UpdateItems: TArray<TFileUpdateInfo> read FUpdateItems write FUpdateItems;
    procedure DisplayArchive(const DirectoryName: string);
    { Public declarations }
  end;

var
  frmArchiveUpdate: TfrmArchiveUpdate;

implementation

uses
  System.IOUtils, System.StrUtils, AbZipTyp, AbArcTyp, frmModelArchiverUnit;

{$R *.fmx}

{ TfrmArchiveUpdate }

procedure TfrmArchiveUpdate.DisplayZipFile(const FileName: string;
  TreeItem: TExistingArchiveItem);
var
  Index: Integer;
  AZipItem: TAbZipItem;
  Splitter: TStringList;
  AnItem: TExistingArchiveItem;
  ParentItem: TExistingArchiveItem;
  SubdirIndex: Integer;
  ChildFileName: string;
  AFileName: string;
begin
  TreeItem.Text := ChangeFileExt(ExtractFileName(FileName), '');
  Splitter := TStringList.Create;
  zpbrwsExistingZipFile.OpenArchive(FileName);
  try
    Splitter.StrictDelimiter := True;
    Splitter.Delimiter := '/';
    for Index := 0 to zpbrwsExistingZipFile.Count - 1 do
    begin
      AZipItem := zpbrwsExistingZipFile.Items[Index];
      AFileName := AZipItem.FileName;
      if AFileName[Length(AFileName)] = '/' then
      begin
        AFileName := Copy(AFileName, 1, Length(AFileName)-1);
      end;
      Splitter.DelimitedText := AFileName;

      if not TreeItem.ChildItems.TryGetValue(Splitter[0], AnItem) then
      begin
        AnItem := TExistingArchiveItem.Create(self);
        AnItem.Hint := 'no hint: ';
        AnItem.OnPaint := PaintItem;
        AnItem.Parent := TreeItem;
        AnItem.Text := Splitter[0];
        TreeItem.ChildItems.Add(Splitter[0], AnItem);
        if not AZipItem.IsDirectory then
        begin
          AnItem.ArchiveFullFileName := Splitter[0];
        end;
      end;

      if Splitter.Count = 1 then
      begin
        AnItem.ArchiveLastModified := AZipItem.LastModTimeAsDateTime;
      end;

      ParentItem := AnItem;
      for SubdirIndex := 1 to Splitter.Count - 1 do
      begin
        ChildFileName := Splitter[SubdirIndex];
        if not ParentItem.ChildItems.TryGetValue(ChildFileName, AnItem) then
        begin
          AnItem := TExistingArchiveItem.Create(self);
          AnItem.Hint := 'no hint: ';
          AnItem.OnPaint := PaintItem;
          AnItem.Parent := ParentItem;
          ParentItem.ChildItems.Add(ChildFileName, AnItem);
          AnItem.Text := ChildFileName;
          if SubdirIndex = Splitter.Count - 1 then
          begin
            AnItem.ArchiveLastModified := AZipItem.LastModTimeAsDateTime;
            AnItem.ArchiveFullFileName := ChildFileName;
          end;
        end;
        ParentItem := AnItem;
      end;
    end;
  finally
    zpbrwsExistingZipFile.CloseArchive;
    Splitter.Free;
  end;
end;

procedure TfrmArchiveUpdate.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  tvExistingArchive.Clear;
end;

procedure TfrmArchiveUpdate.FormCreate(Sender: TObject);
begin
  FNewDirectories := TStringList.Create;
end;

procedure TfrmArchiveUpdate.FormDestroy(Sender: TObject);
begin
  FNewDirectories.Free;
end;

procedure TfrmArchiveUpdate.PaintItem(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  AnItem: TExistingArchiveItem;
  ItemList: TList<TExistingArchiveItem>;
  ChildIndex: Integer;
  ChildItem: TExistingArchiveItem;
  procedure AddChildren(ParentItem: TExistingArchiveItem);
  var
    ChildArray: TArray<TPair<string,TExistingArchiveItem>>;
    ChildItem: TExistingArchiveItem;
    ChildIndex: Integer;
  begin
    if ParentItem.FChildItems.Count > 0 then
    begin
      ChildArray := ParentItem.FChildItems.ToArray;
      for ChildIndex := 0 to Length(ChildArray) - 1 do
      begin
        ChildItem := ChildArray[ChildIndex].Value;
        AddChildren(ChildItem);;
      end;
    end
    else
    begin
      ItemList.Add(ParentItem);
    end;
  end;
begin
  if (Sender is TExistingArchiveItem) then
  begin
    AnItem := TExistingArchiveItem(Sender);
    if AnItem.FChildItems.Count = 0 then
    begin
      if AnItem.ArchiveFullFileName = '' then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Blue;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
      end
      else  if AnItem.OriginalFileFullFileName = '' then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);

        {
        Canvas.Fill.Color := TAlphaColorRec.Black;
        Canvas.Font.Style := [TFontStyle.fsStrikeOut];
        ARect.Inflate(-40, 0, 0, 0);
        Canvas.FillText(ARect, AnItem.Text, False, 1, [],
          TTextAlign.Leading, TTextAlign.Center);
        ARect.Inflate(40, 0, 0, 0);
        }

      end
      else if Abs(AnItem.OriginalFileLastModified
        - AnItem.ArchiveLastModified) > Epsilon then
      begin
        Canvas.Fill.Color := TAlphaColorRec.Yellow;
        Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
      end;
    end
    else
    begin
      ItemList := TList<TExistingArchiveItem>.Create;
      try
        AddChildren(AnItem);
        for ChildIndex := 0 to ItemList.Count - 1 do
        begin
          ChildItem := ItemList[ChildIndex];
          if (ChildItem.ArchiveFullFileName = '')
            and (ChildItem.OriginalFileFullFileName = '') then
          begin
            Continue;
          end;
          if ChildItem.ArchiveFullFileName = '' then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Blue;
            Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
            break;
          end
        end;
        for ChildIndex := 0 to ItemList.Count - 1 do
        begin
          ChildItem := ItemList[ChildIndex];
          if (ChildItem.ArchiveFullFileName = '')
            and (ChildItem.OriginalFileFullFileName = '') then
          begin
            Continue;
          end;
          if ChildItem.OriginalFileFullFileName = '' then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Red;
            Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
            break;
          end
        end;
        for ChildIndex := 0 to ItemList.Count - 1 do
        begin
          ChildItem := ItemList[ChildIndex];
          if (ChildItem.ArchiveFullFileName = '')
            or (ChildItem.OriginalFileFullFileName = '') then
          begin
            Continue;
          end;
          if (Abs(ChildItem.OriginalFileLastModified
            - ChildItem.ArchiveLastModified) > Epsilon) then
          begin
            Canvas.Fill.Color := TAlphaColorRec.Yellow;
            Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
            break;
          end
        end;
      finally
        ItemList.Free;
      end;
    end;
  end;
end;

function ArchiveFileName(Item: TExistingArchiveItem): string;
var
  ParentItem: TExistingArchiveItem;
begin
  result := Item.Text;
  ParentItem := Item.ParentItem as TExistingArchiveItem;
  while ParentItem <> nil do
  begin
    if (ParentItem.ArchiveFullFileName <> '')
      and SameText(ExtractFileExt(ParentItem.ArchiveFullFileName), '.zip') then
    begin
//      result := StringReplace(Result, PathDelim, '/', [rfReplaceAll, rfIgnoreCase]);
      break;
    end
    else
    begin
      result := ParentItem.Text + PathDelim + result;
    end;
    ParentItem := ParentItem.ParentItem as TExistingArchiveItem;
  end;
end;

function ZipfileName(Item: TExistingArchiveItem): string;
var
  ParentItem: TExistingArchiveItem;
begin
  result := '';
  ParentItem := Item.ParentItem as TExistingArchiveItem;
  while ParentItem <> nil do
  begin
    if ParentItem.ArchiveFullFileName <> '' then
    begin
      result := ParentItem.ArchiveFullFileName;
      if (SameText(ExtractFileExt(result), '.zip')) then
      begin
        break;
      end;
    end;
    ParentItem := ParentItem.ParentItem as TExistingArchiveItem;
  end;
end;

procedure TfrmArchiveUpdate.AddToArchive(Item: TExistingArchiveItem);
var
//  AStream: TFileStream;
  ZipName: string;
  ArchiveName: string;
//  ItemIndex: Integer;
//  AbItem: TAbZipItem;
  DirName: string;
  NewDirectory: string;
  TestDir: string;
begin
  ZipName := ZipfileName(Item);
  ArchiveName := ArchiveFileName(Item);
  if ZipName <> '' then
  begin
    DirName := ChangeFileExt(ZipName, '');
    if not TDirectory.Exists(DirName) then
    begin
        TDirectory.CreateDirectory(DirName);
        FNewDirectories.Add(DirName);
    end;
    ArchiveName := IncludeTrailingPathDelimiter(DirName) + ArchiveName;
    NewDirectory := ExtractFileDir(ArchiveName);
    if not TDirectory.Exists(NewDirectory) then
    begin
      TestDir := TDirectory.GetParent(NewDirectory);
      while not TDirectory.Exists(TestDir) do
      begin
        FNewDirectories.Add(TestDir);
        TestDir := TDirectory.GetParent(TestDir);
      end;

      TDirectory.CreateDirectory(NewDirectory);
      FNewDirectories.Add(NewDirectory);
    end;
    TFile.Copy(Item.OriginalFileFullFileName, ArchiveName);
    zprUpdate.FileName := ZipName;
    zprUpdate.BaseDirectory := DirName;
    zprUpdate.TempDirectory := DirName;
    zprUpdate.StoreOptions := zprUpdate.StoreOptions + [soReplace, soRecurse];
    zprUpdate.AddFiles('*.*', faAnyFile);
//    TFile.Delete(ArchiveName);
  end
  else
  begin
    ArchiveName := IncludeTrailingPathDelimiter(FArchiveDirectory) + ArchiveName;
    if not TDirectory.Exists(ExtractFileDir(ArchiveName)) then
    begin
      TDirectory.CreateDirectory(ExtractFileDir(ArchiveName));
    end;
    TFile.Copy(Item.OriginalFileFullFileName, ArchiveName);
  end;
end;

procedure TfrmArchiveUpdate.RemoveFromArchive(Item: TExistingArchiveItem);
var
  ZipName: string;
  ArchiveName: string;
begin
  ZipName := ZipfileName(Item);
  if ZipName <> '' then
  begin
    zprUpdate.FileName := ZipName;
    zprUpdate.DeleteFiles(Item.ArchiveFullFileName);
  end
  else
  begin
    ArchiveName := Item.ArchiveFullFileName;
    TFile.Delete(ArchiveName);
  end;
end;

procedure TfrmArchiveUpdate.ReplaceInArchive(Item: TExistingArchiveItem);
begin
  RemoveFromArchive(Item);
  AddToArchive(Item);
end;

procedure TfrmArchiveUpdate.tvExistingArchiveChangeCheck(Sender: TObject);
  procedure UpdateChildren(ParentItem: TTreeViewItem);
  var
    ChildIndex: Integer;
    ChildItem: TTreeViewItem;
  begin
    for ChildIndex := 0 to ParentItem.Count - 1 do
    begin
      ChildItem := ParentItem.Items[ChildIndex];
      ChildItem.IsChecked := ParentItem.IsChecked;
      UpdateChildren(ChildItem);
    end;
  end;
var
  AnItem: TTreeViewItem;
begin
  if FDisplayingData then
  begin
    Exit;
  end;
  if Sender is TTreeViewItem then
  begin
    AnItem := TTreeViewItem(Sender);
    UpdateChildren(AnItem);
  end;
end;

procedure TfrmArchiveUpdate.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmArchiveUpdate.btnHelpClick(Sender: TObject);
begin
  OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmArchiveUpdate.btnUpdateClick(Sender: TObject);
var
  ChildIndex: Integer;
  AnItem: TExistingArchiveItem;
  DirIndex: Integer;
  ModelDir: string;
  procedure UpdateItem(ParentItem: TExistingArchiveItem);
  var
    ChildIndex: Integer;
  begin
    if ParentItem.Count > 0 then
    begin
      for ChildIndex := 0 to ParentItem.Count - 1 do
      begin
        UpdateItem(ParentItem.Items[ChildIndex] as TExistingArchiveItem);
      end;
    end
    else
    begin
      if ParentItem.IsChecked then
      begin
        if (ParentItem.ArchiveFullFileName = '')
          and (ParentItem.OriginalFileFullFileName <> '')
          and TFile.Exists(ParentItem.OriginalFileFullFileName) then
        begin
          AddToArchive(ParentItem);
        end
        else if (ParentItem.ArchiveFullFileName <> '')
          and (ParentItem.OriginalFileFullFileName = '') then
        begin
          RemoveFromArchive(ParentItem);
        end
        else if Abs(ParentItem.OriginalFileLastModified
          - ParentItem.ArchiveLastModified) > Epsilon then
        begin
          ReplaceInArchive(ParentItem);
        end;
      end;
    end;
  end;
begin
  FNewDirectories.Clear;
  for ChildIndex := 0 to tvExistingArchive.Count - 1 do
  begin
    AnItem := tvExistingArchive.Items[ChildIndex] as TExistingArchiveItem;
    UpdateItem(AnItem);
  end;
  zprUpdate.CloseArchive;

  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'bin';
  ZipDirectory(ModelDir);
  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'ancillary';
  ZipDirectory(ModelDir);
  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'georef';
  ZipDirectory(ModelDir);
  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'source';
  ZipDirectory(ModelDir);
  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'model';
  ZipModelOrOutput(ModelDir);
  ModelDir := IncludeTrailingPathDelimiter(FArchiveDirectory) + 'output';
  ZipModelOrOutput(ModelDir);

  for DirIndex := 0 to FNewDirectories.Count - 1 do
  begin
    if TDirectory.Exists(FNewDirectories[DirIndex]) then
    begin
      TDirectory.Delete(FNewDirectories[DirIndex], True);
    end;
  end;
  frmModelArchiver.acUpdateReadmeExecute(nil);
  frmModelArchiver.miUpdateMetaDataClick(nil);
  Close;
end;

procedure TfrmArchiveUpdate.DisplayArchive(const DirectoryName: string);
var
  Files: TStringDynArray;
  AFileName: string;
  AnItem: TExistingArchiveItem;
  Index: Integer;
  DirLength: Integer;
  Splitter: TStringList;
  RootItems: TDictionary<string,TExistingArchiveItem>;
  SubdirIndex: Integer;
  ChildFileName: string;
  ParentItem: TExistingArchiveItem;
  FullFileName: string;
  IsZipfile: Boolean;
  AnUpdateItem: TFileUpdateInfo;
  PPItem: TTreeViewItem;
begin
  FDisplayingData := True;
  Assert(TDirectory.Exists(DirectoryName));
  FArchiveDirectory := DirectoryName;
  DirLength := Length(IncludeTrailingPathDelimiter(DirectoryName));
  Files := TDirectory.GetFiles(DirectoryName, '*',
    TSearchOption.soAllDirectories);

  Splitter := TStringList.Create;
  RootItems := TDictionary<string,TExistingArchiveItem>.Create;
  try
    Splitter.StrictDelimiter := True;
    Splitter.Delimiter := PathDelim;
    for Index := 0 to Length(Files) - 1 do
    begin
      FullFileName := Files[Index];
      AFileName := RightStr(FullFileName, Length(FullFileName) - DirLength);
      Splitter.DelimitedText := AFileName;

      IsZipfile := AnsiSameText(ExtractFileExt(FullFileName), '.zip');

      if not RootItems.TryGetValue(Splitter[0], AnItem) then
      begin
        AnItem := TExistingArchiveItem.Create(self);
        AnItem.Hint := 'no hint: ';
        AnItem.OnPaint := PaintItem;
        AnItem.Parent := tvExistingArchive;
        if IsZipfile and (Splitter.Count = 1) then
        begin
          DisplayZipFile(FullFileName, AnItem);
        end
        else
        begin
          AnItem.Text := Splitter[0];
        end;
        RootItems.Add(AnItem.Text, AnItem);
        if Splitter.Count = 1 then
        begin
          AnItem.ArchiveLastModified := TFile.GetLastWriteTime(FullFileName);
          AnItem.ArchiveFullFileName := FullFileName;
        end;
      end;

      ParentItem := AnItem;
      for SubdirIndex := 1 to Splitter.Count - 1 do
      begin
        ChildFileName := Splitter[SubdirIndex];
        if not ParentItem.ChildItems.TryGetValue(ChildFileName, AnItem) then
        begin
          AnItem := TExistingArchiveItem.Create(self);
          AnItem.Hint := 'no hint: ';
          AnItem.OnPaint := PaintItem;
          AnItem.Parent := ParentItem;
          ParentItem.ChildItems.Add(ChildFileName, AnItem);
          if IsZipfile and (SubdirIndex = Splitter.Count - 1) then
          begin
            DisplayZipFile(FullFileName, AnItem);
          end
          else
          begin
            AnItem.Text := ChildFileName;
          end;
          if (SubdirIndex = Splitter.Count - 1) then
          begin
            AnItem.ArchiveLastModified := TFile.GetLastWriteTime(FullFileName);
            AnItem.ArchiveFullFileName := FullFileName;
          end;
        end;
        ParentItem := AnItem;
      end;

    end;

    for Index := 0 to Length(UpdateItems) -1 do
    begin
      AnUpdateItem := UpdateItems[Index];
      FullFileName := AnUpdateItem.FullFileName;
      Splitter.DelimitedText := AnUpdateItem.ArchiveFileName;

      if not RootItems.TryGetValue(Splitter[0], AnItem) then
      begin
        AnItem := TExistingArchiveItem.Create(self);
//        AnItem.Hint := 'no hint: ';
        AnItem.OnPaint := PaintItem;
        AnItem.Parent := tvExistingArchive;
        AnItem.Text := Splitter[0];
        RootItems.Add(AnItem.Text, AnItem);
      end;

      if Splitter.Count = 1 then
      begin
        AnItem.OriginalFileLastModified := TFile.GetLastWriteTime(FullFileName);
        AnItem.OriginalFileFullFileName := FullFileName;
      end;

      ParentItem := AnItem;
      for SubdirIndex := 1 to Splitter.Count - 1 do
      begin
        ChildFileName := Splitter[SubdirIndex];
        if not ParentItem.ChildItems.TryGetValue(ChildFileName, AnItem)
          and not ParentItem.ChildItems.TryGetValue(ChildFileName + '.zip', AnItem) then
        begin
          AnItem := TExistingArchiveItem.Create(self);
//          AnItem.Hint := 'no hint: ';
          AnItem.OnPaint := PaintItem;
          AnItem.Parent := ParentItem;
          ParentItem.ChildItems.Add(ChildFileName, AnItem);
          AnItem.Text := ChildFileName;
        end;
        if (SubdirIndex = Splitter.Count - 1) then
        begin
          AnItem.OriginalFileLastModified := TFile.GetLastWriteTime(FullFileName);
          AnItem.OriginalFileFullFileName := FullFileName;
          AnItem.Hint := DateTimeToStr(AnItem.OriginalFileLastModified);
        end;
        ParentItem := AnItem;
      end;

      if ParentItem.ArchiveFullFileName = '' then
      begin
        ParentItem.IsChecked := True;
      end
      else  if ParentItem.OriginalFileFullFileName = '' then
      begin
        ParentItem.IsChecked := True;
      end
      else if Abs(ParentItem.OriginalFileLastModified
        - ParentItem.ArchiveLastModified) > Epsilon then
      begin
        ParentItem.IsChecked := True;
      end;


      if ParentItem.IsChecked then
      begin
        PPItem := ParentItem.ParentItem;
        while (PPItem <> nil) and not PPItem.IsChecked do
        begin
          PPItem.IsChecked := True;
          PPItem := PPItem.ParentItem;
        end;
      end;

    end;
  finally
    RootItems.Free;
    Splitter.Free;
    FDisplayingData := False;
  end;

//  tvExistingArchive.ExpandAll;
end;

procedure TfrmArchiveUpdate.ZipDirectory(ModelDir: string);
var
//  SubDirs: TStringDynArray;
//  DirIndex: Integer;
//  ASubDir: string;
//  FileNames: TStringDynArray;
  Zipfile: string;
//  FileIndex: Integer;
//  FileName: string;
begin
  if not TDirectory.Exists(ModelDir) then
  begin
    Exit;
  end;
  Zipfile := ModelDir + '.zip';
  if not TFile.Exists(Zipfile) then
  begin
    zprUpdate.BaseDirectory := ModelDir;
    zprUpdate.TempDirectory := ModelDir;
    zprUpdate.FileName := Zipfile;
    zprUpdate.StoreOptions := zprUpdate.StoreOptions + [soReplace, soRecurse];
    zprUpdate.AddFiles('*.*', faAnyFile);
    zprUpdate.CloseArchive;
    TDirectory.Delete(ModelDir, True);
  end;
end;

procedure TfrmArchiveUpdate.ZipModelOrOutput(ModelDir: string);
var
  ModelFiles: TStringDynArray;
  HasZipFiles: Boolean;
  FileIndex: Integer;
  SubDirs: TStringDynArray;
  ASubDir: string;
  DirIndex: Integer;
begin
  if not TDirectory.Exists(ModelDir) then
  begin
    Exit;
  end;
  ModelFiles := TDirectory.GetFiles(ModelDir);
  HasZipFiles := False;
  for FileIndex := 0 to Length(ModelFiles) - 1 do
  begin
    if SameText(ExtractFileExt(ModelFiles[FileIndex]), '.zip') then
    begin
      HasZipFiles := True;
      Break;
    end;
  end;
  SubDirs := TDirectory.GetDirectories(ModelDir);
  if HasZipFiles then
  begin
    for DirIndex := 0 to Length(SubDirs) - 1 do
    begin
      ASubDir := SubDirs[DirIndex];
      zprUpdate.CloseArchive;
      zprUpdate.BaseDirectory := ASubDir;
      zprUpdate.TempDirectory := ASubDir;
      zprUpdate.FileName := ASubDir + '.zip';
      zprUpdate.StoreOptions := zprUpdate.StoreOptions + [soReplace, soRecurse];
      zprUpdate.AddFiles('*.*', faAnyFile);
      zprUpdate.CloseArchive;
      TDirectory.Delete(ASubDir, True);
    end;
  end
  else
  begin
    zprUpdate.CloseArchive;
    zprUpdate.BaseDirectory := ModelDir;
    zprUpdate.TempDirectory := ModelDir;
    zprUpdate.FileName := ModelDir + '.zip';
    zprUpdate.StoreOptions := zprUpdate.StoreOptions + [soReplace, soRecurse];
    zprUpdate.AddFiles('*.*', faAnyFile);
    zprUpdate.CloseArchive;
    TDirectory.Delete(ModelDir, True);
  end;
end;

{ TExistingArchiveItem }

constructor TExistingArchiveItem.Create(AOwner: TComponent);
begin
  inherited;
  FChildItems := TDictionary<string,TExistingArchiveItem>.Create;
  ShowHint := True;
end;

destructor TExistingArchiveItem.Destroy;
begin
  FChildItems.Free;
  inherited;
end;

end.
