unit frmModelFilesUnit;

interface

uses Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.Grid,
  FMX.TreeView, FMX.Layouts, FMX.ListBox, frmModelArchiverUnit, FMX.StdCtrls,
  FMX.Controls.Presentation, System.RegularExpressions, FMX.Graphics;

type
  TfrmModelFiles = class(TForm)
    pnl2: TPanel;
    pnlOutput: TPanel;
    lstOutputExtensions: TListBox;
    lbl1: TLabel;
    pnlInput: TPanel;
    lstInputExtensions: TListBox;
    lbl2: TLabel;
    pnl5: TPanel;
    btnOk: TButton;
    tvFiles: TTreeView;
    spl1: TSplitter;
    spl2: TSplitter;
    pnlAncillary: TPanel;
    lstAncillaryExtensions: TListBox;
    lbl4: TLabel;
    spl4: TSplitter;
    btnCancel: TButton;
    btnHelp: TButton;
    procedure tvFilesChangeCheck(Sender: TObject);
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExtensionsChangeCheck(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
  private
    FDirectoryName: string;
    FModelInput: TStringList;
    FModelOutput: TStringList;
    FModpathInput: TStringList;
    FModpathOutput: TStringList;
    FZoneBudgetInput: TStringList;
    FZoneBudgetOutput: TStringList;
    FMt3dInput: TStringList;
    FMt3dOutput: TStringList;
    FOtherInput: TStringList;
    FOtherOutput: TStringList;
    procedure AddSelectedFilesToArchiveTreeView(UsedFiles: TStringList;
      Category: string);
    procedure GetUsedExtensions(UsedExtensions: TStringList;
      ExtensionListBox: TListBox);
    procedure GetUsedFiles(UsedExtensions: TStringList; UsedFiles: TStringList;
      SelectedFiles: TStringList);
    procedure GetSelectedFiles(SelectedFiles: TStringList);
    procedure AddFilesToArchiveTreeView;
    procedure ItemExpanded(Sender: TObject);
    { Private declarations }
  public
    procedure GetFileData(DirectoryName: string);
    { Public declarations }
  end;

var
  frmModelFiles: TfrmModelFiles;

implementation

uses
  ArchiveNodeInterface, System.IOUtils, ExtensionTypeUnit;

type
  TTreeViewCrack = class(TTreeView);


{$R *.fmx}

{ TfrmModelFiles }

function SortUsedFiles(List: TStringList; Index1, Index2: Integer): Integer;
var
  Splitter1: TStringList;
  Splitter2: TStringList;
begin
  Splitter1 := TStringList.Create;
  Splitter2 := TStringList.Create;
  try
    Splitter1.Delimiter := PathDelim;
    Splitter2.Delimiter := PathDelim;
    Splitter1.StrictDelimiter := True;
    Splitter2.StrictDelimiter := True;
    Splitter1.DelimitedText := List[Index1];
    Splitter2.DelimitedText := List[Index2];
    result := Splitter1.Count - Splitter2.Count;
    if result = 0 then
    begin
      Result := AnsiCompareText(List[Index1], List[Index2])
    end;
  finally
    Splitter2.Free;
    Splitter1.Free;
  end;
end;

procedure TfrmModelFiles.AddSelectedFilesToArchiveTreeView(
  UsedFiles: TStringList; Category: string);
var
  ParentDir: string;
  FileIndex: Integer;
  ParentModelItem: TArchiveObject;
  FileName: string;
  AnItem: TTreeViewItem;
  ParentCategory: TArchiveObject;
  FileItem: TArchiveObject;
  ArchiveTreeView: TTreeView;
  Splitter: TStringList;
  ModelName: string;
  TreeViewItem: TCustomExpandItem;
  ExtObj: TExtensionObject;
  PCatTreeItem: TTreeViewItem;
  PCatObject: TArchiveObject;
  MPathObject: TArchiveObject;
  FileExtension: string;
  FolderCount: Integer;
  FolderIndex: Integer;
  NewParentModelItem: TArchiveObject;
  FolderName: string;
  procedure UpdateModelName;
  begin
    if (FModpathInput.IndexOf(FileExtension) >= 0)
      or (FModpathOutput.IndexOf(FileExtension) >= 0) then
    begin
      if Pos('MODPATH', UpperCase(ModelName)) <= 0 then
      begin
        ModelName := ModelName + '_Modpath';
      end;
    end
    else if (FZoneBudgetInput.IndexOf(FileExtension) >= 0)
      or (FZoneBudgetOutput.IndexOf(FileExtension) >= 0) then
    begin
      if Pos('Zonebudget', UpperCase(ModelName)) <= 0 then
      begin
        ModelName := ModelName + '_Zonebudget';
      end;
    end
    else if (FMt3dInput.IndexOf(FileExtension) >= 0)
      or (FMt3dOutput.IndexOf(FileExtension) >= 0) then
    begin
      if Pos('MT3D', UpperCase(ModelName)) <= 0 then
      begin
        ModelName := ModelName + '_MT3D';
      end;
    end
    else if (FOtherInput.IndexOf(FileExtension) >= 0)
      or (FOtherOutput.IndexOf(FileExtension) >= 0) then
    begin
      if Pos('OTHER', UpperCase(ModelName)) <= 0 then
      begin
        ModelName := ModelName + '_Other';
      end;
    end;
  end;
begin
  if UsedFiles.Count = 0 then
  begin
    Exit;
  end;
  frmModelArchiver.InitializeDescriptionSearchObjects;

  ParentDir := '';
  ParentModelItem := nil;
  ArchiveTreeView := frmModelArchiver.tvArchive;
  ParentCategory := ArchiveTreeView.ItemByText(Category).TagObject as TArchiveObject;
  if ParentCategory = nil then
  begin
    TreeViewItem := TCustomExpandItem.Create(self);
    TreeViewItem.OnChangeExpanded := ItemExpanded;
    ParentCategory := TArchiveObject.Create(TreeViewItem);
    ParentCategory.TreeViewItem.Text := Category;
    ParentCategory.NodeType := ntCategory;
    ParentCategory.TreeViewItem.Parent := ArchiveTreeView;
    ParentCategory.FileSource := fsManual;
    ParentCategory.ModelName := Category;
    TreeViewItem.OnPaint := frmModelArchiver.PaintItem;
  end;

  UsedFiles.CustomSort(SortUsedFiles);

  for FileIndex := 0 to UsedFiles.Count - 1 do
  begin
    AnItem := UsedFiles.Objects[FileIndex] as TTreeViewItem;
    FileName := AnItem.Text;
    FileExtension := ExtractFileExt(FileName);
    if FileExtension = ArchiveExt then
    begin
      FileName := ChangeFileExt(FileName, '');
      FileExtension := ExtractFileExt(FileName);
    end;
    Assert(AnItem <> nil);
    if AnItem.Text <> ParentDir then
    begin
      ParentDir := AnItem.Text;
      Splitter := TStringList.Create;
      try
        Splitter.Delimiter := PathDelim;
        Splitter.StrictDelimiter := True;
        Splitter.DelimitedText := ParentDir;
        ModelName := '';
        ParentModelItem := nil;
        if Splitter.Count > 0 then
        begin
          Splitter.Delete(Splitter.Count-1);
        end;
        FolderCount := Splitter.Count;
        while (ParentModelItem = nil) and (Splitter.Count > 0) do
        begin
          ModelName := Splitter[Splitter.Count-1];
          Splitter.Delete(Splitter.Count-1);
          UpdateModelName;
          ParentModelItem := ParentCategory.GetChildByName(ModelName);
          if (ParentModelItem <> nil) and (Splitter.Count < FolderCount -1) then
          begin
            FolderCount := Splitter.Count;
            Splitter.DelimitedText := ParentDir;
            Splitter.Delete(Splitter.Count-1);
            for FolderIndex := FolderCount+1 to Splitter.Count - 1 do
            begin
              FolderName := Splitter[FolderIndex];
              NewParentModelItem := ParentModelItem.GetChildByName(FolderName);
              if NewParentModelItem = nil then
              begin
                TreeViewItem := TCustomExpandItem.Create(self);
                TreeViewItem.OnChangeExpanded := ItemExpanded;
                NewParentModelItem := TArchiveObject.Create(TreeViewItem);
                NewParentModelItem.TreeViewItem.Text := FolderName;
                NewParentModelItem.ModelName := FolderName;
                NewParentModelItem.NodeType := ntFolder;
                NewParentModelItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
                NewParentModelItem.FileSource := fsManual;
                NewParentModelItem.ModelDirectory := ParentDir;
                TreeViewItem.OnPaint := frmModelArchiver.PaintItem;
              end;
              ParentModelItem := NewParentModelItem;
            end;
          end;
        end;
        if ParentModelItem = nil then
        begin
          ParentDir := AnItem.Text;
          ModelName := '';
          Splitter.DelimitedText := ParentDir;

          while (ModelName = '') and (Splitter.Count > 0) do
          begin
            Splitter.Delete(Splitter.Count-1);
            ModelName := Splitter[Splitter.Count-1];
            UpdateModelName;
            ParentDir := IncludeTrailingPathDelimiter(Splitter.DelimitedText);
          end;
        end;
      finally
        Splitter.Free;
      end;
      if ParentModelItem = nil then
      begin
        TreeViewItem := TCustomExpandItem.Create(self);
        TreeViewItem.OnChangeExpanded := ItemExpanded;
        ParentModelItem := TArchiveObject.Create(TreeViewItem);
        ParentModelItem.TreeViewItem.Text := ModelName;
        ParentModelItem.ModelName := ModelName;
        ParentModelItem.NodeType := ntModel;
        ParentModelItem.TreeViewItem.Parent := ParentCategory.TreeViewItem;
        ParentModelItem.FileSource := fsManual;
        ParentModelItem.ModelDirectory := ParentDir;
        TreeViewItem.OnPaint := frmModelArchiver.PaintItem;
      end;
    end;
    FileName := UsedFiles[FileIndex];
    FileItem := ParentModelItem.GetChildByName(FileName);
    if FileItem = nil then
    begin
      TreeViewItem := TCustomExpandItem.Create(self);
      TreeViewItem.OnChangeExpanded := ItemExpanded;
      FileItem := TArchiveObject.Create(TreeViewItem);
      FileItem.TreeViewItem.Text := FileName;
      FileItem.NodeType := ntFile;
      frmModelArchiver.AssignDescription(FileItem, ExtObj);
      TreeViewItem.OnPaint := frmModelArchiver.PaintItem;
      if ExtObj <> nil then
      begin
        case ExtObj.ExtensionType of
          etModelInput, etModelOutput, etAncillary:
            FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
          etModpathInput, etModpathOutput:
            begin
              if Pos('modpath', LowerCase(ParentModelItem.TreeViewItem.Text)) > 0 then
              begin
                FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
              end
              else
              begin
                PCatTreeItem := ParentModelItem.TreeViewItem.ParentItem;
                PCatObject := PCatTreeItem.TagObject as TArchiveObject;
                MPathObject := PCatObject.GetChildByName(
                  ParentModelItem.TreeViewItem.Text + '_Modpath');
                if MPathObject = nil then
                begin
                  TreeViewItem := TCustomExpandItem.Create(self);
                  TreeViewItem.OnChangeExpanded := ItemExpanded;
                  MPathObject := TArchiveObject.Create(TreeViewItem);
                  MPathObject.TreeViewItem.Text := ParentModelItem.TreeViewItem.Text + '_Modpath';
                  MPathObject.ModelName := MPathObject.TreeViewItem.Text;
                  MPathObject.NodeType := ntModel;
                  MPathObject.TreeViewItem.Parent := PCatObject.TreeViewItem;
                  MPathObject.FileSource := fsManual;
                  MPathObject.ModelDirectory := ParentDir;
                  TreeViewItem.OnPaint := frmModelArchiver.PaintItem;
                end;
                FileItem.TreeViewItem.Parent := MPathObject.TreeViewItem;
              end;
            end;
          etZoneBudgetInput, etZoneBudgetOutput:
            FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
          etMt3dmsInput, etMt3dmsOutput:
            FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
          etOtherInput, etOtherOutput:
            FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
        end;
      end
      else
      begin
        FileItem.TreeViewItem.Parent := ParentModelItem.TreeViewItem;
      end;
      FileItem.FileSource := fsManual;
      FileItem.ModelDirectory := ParentModelItem.ModelDirectory;
    end;
  end;
end;

procedure TfrmModelFiles.btnHelpClick(Sender: TObject);
begin
  OpenHelpUrl(btnHelp.HelpKeyword);
end;

procedure TfrmModelFiles.btnOkClick(Sender: TObject);
begin
  AddFilesToArchiveTreeView;
end;

procedure TfrmModelFiles.FormCreate(Sender: TObject);
var
  PanelWidths: Single;
begin
  PanelWidths := (pnlInput.Width + pnlOutput.Width + pnlAncillary.Width)/3;
  pnlInput.Width := PanelWidths;
  pnlAncillary.Width := PanelWidths;

  FModelInput := TStringList.Create;
  FModelOutput := TStringList.Create;
  FModpathInput := TStringList.Create;
  FModpathOutput := TStringList.Create;
  FZoneBudgetInput := TStringList.Create;
  FZoneBudgetOutput := TStringList.Create;
  FMt3dInput := TStringList.Create;
  FMt3dOutput := TStringList.Create;
  FOtherInput := TStringList.Create;
  FOtherOutput := TStringList.Create;

end;

procedure TfrmModelFiles.FormDestroy(Sender: TObject);
begin
  FModelInput.Free;
  FModelOutput.Free;
  FModpathInput.Free;
  FModpathOutput.Free;
  FZoneBudgetInput.Free;
  FZoneBudgetOutput.Free;
  FMt3dInput.Free;
  FMt3dOutput.Free;
  FOtherInput.Free;
  FOtherOutput.Free;

end;

procedure TfrmModelFiles.FormKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(btnHelp.HelpKeyword);
  end;
end;

procedure TfrmModelFiles.AddFilesToArchiveTreeView;
var
  SelectedFiles: TStringList;
  UsedExtensions: TStringList;
  UsedFiles: TStringList;
  procedure HandleFileItem(AnItem: TTreeViewItem);
  var
    ItemIndex: Integer;
    FileName: string;
  begin
    if AnItem.Count > 0 then
    begin
      for ItemIndex := 0 to AnItem.Count - 1 do
      begin
        HandleFileItem(AnItem.Items[ItemIndex]);
      end;
    end
    else if AnItem.IsChecked then
    begin
      FileName := AnItem.TagString;
      Assert(FileName <> '');
      SelectedFiles.AddObject(FileName, AnItem);
    end;
  end;
begin
  frmModelArchiver.tvArchive.BeginUpdate;
  SelectedFiles := TStringList.Create;
  UsedExtensions := TStringList.Create;
  UsedFiles := TStringList.Create;
  try
    GetSelectedFiles(SelectedFiles);
    UsedExtensions.CaseSensitive := False;

    GetUsedExtensions(UsedExtensions, lstAncillaryExtensions);
    GetUsedFiles(UsedExtensions, UsedFiles, SelectedFiles);
    AddSelectedFilesToArchiveTreeView(UsedFiles, StrAncillary);

    GetUsedExtensions(UsedExtensions, lstInputExtensions);
    GetUsedFiles(UsedExtensions, UsedFiles, SelectedFiles);
    AddSelectedFilesToArchiveTreeView(UsedFiles, StrModelInputFiles);

    GetUsedExtensions(UsedExtensions, lstOutputExtensions);
    GetUsedFiles(UsedExtensions, UsedFiles, SelectedFiles);
    AddSelectedFilesToArchiveTreeView(UsedFiles, StrModelOutputFiles);
  finally
    UsedExtensions.Free;
    SelectedFiles.Free;
    UsedFiles.Free;
    frmModelArchiver.tvArchive.EndUpdate;
  end;
end;

procedure TfrmModelFiles.GetFileData(DirectoryName: string);
var
  Files: TStringDynArray;
  FileIndex: Integer;
  AFileName: string;
  DriveItem: TCustomExpandItem;
  Splitter: TStringList;
//  FileDrive: string;
  DirIndex: Integer;
  ADir: string;
  RootNode: TTreeViewItem;
  AChildNode: TCustomExpandItem;
  Extensions: TStringList;
  DirItem: TTreeViewItem;
  AChildItem: TTreeViewItem;
  ChildIndex: Integer;
  ExtIndex: Integer;
  AnExt: TExtensionObject;
  ListControl: TListBox;
  Item: TListBoxItem;
  Index: integer;
  UsedExtensions: TStringList;
  ExtensionsMatch: Boolean;
  ExtensionIndex: Integer;
  Extension: string;
//  SomeMatches: TMatchCollection;
//  MatchIndex: Integer;
//  AMatch: TMatch;
  SearchExt: string;
  function GetChildByText(RootNode: TTreeViewItem; Text: string): TTreeViewItem;
  var
    ChildIndex: Integer;
  begin
    result := nil;
    for ChildIndex := 0 to RootNode.Count - 1 do
    begin
      if RootNode.Items[ChildIndex].Text = Text then
      begin
        Result := RootNode.Items[ChildIndex] as TTreeViewItem;
        break;
      end;
    end;
  end;
begin
  FDirectoryName := DirectoryName;
  tvFiles.BeginUpdate;
  lstInputExtensions.BeginUpdate;
  lstOutputExtensions.BeginUpdate;
  lstAncillaryExtensions.BeginUpdate;
  try
    tvFiles.Clear;
    lstInputExtensions.Clear;
    lstOutputExtensions.Clear;
    lstAncillaryExtensions.Clear;

    Files := TDirectory.GetFiles(DirectoryName, TSearchOption.soAllDirectories,
      function(const Path: string;
          const SearchRec: TSearchRec): Boolean
      begin
        result := True;
      end);
    Extensions := TStringList.Create;
    Splitter := TStringList.Create;
    UsedExtensions := TStringList.Create;
    try
      UsedExtensions.CaseSensitive := False;
      UsedExtensions.Duplicates := dupIgnore;
      for Index := 0 to frmModelArchiver.Extensions.Count - 1 do
      begin
        AnExt := frmModelArchiver.Extensions[Index];
        UsedExtensions.Add(AnExt.Extension);
      end;
      UsedExtensions.Sorted := True;
      tvFiles.BeginUpdate;
      try
        Splitter.Delimiter := PathDelim;
        Splitter.StrictDelimiter := True;
        Extensions.Sorted := True;
        Extensions.CaseSensitive := False;
        Extensions.Duplicates := dupIgnore;
        for FileIndex := 0 to Length(Files) - 1 do
        begin
          AFileName := Files[FileIndex];
          Extensions.Add(ExtractFileExtendedExt(AFileName));
          Splitter.DelimitedText := ExtractFileDir(AFileName);
          RootNode := nil;
          for DirIndex := 0 to Splitter.Count - 1 do
          begin
            ADir := Splitter[DirIndex];
            if ADir = '' then
            begin
              Continue;
            end;
            if RootNode = nil then
            begin
              DriveItem := tvFiles.ItemByText(ADir) as TCustomExpandItem;
              if DriveItem = nil then
              begin
                DriveItem := TCustomExpandItem.Create(self);
                DriveItem.Text := ADir;
                DriveItem.IsChecked := True;
                DriveItem.Parent := tvFiles;
                DriveItem.IsExpanded := true;
                DriveItem.OnChangeExpanded := ItemExpanded;
//                DriveItem.OnPaint := PaintItem;
              end;
              RootNode := DriveItem;
            end
            else
            begin
              AChildNode := GetChildByText(RootNode, ADir) as TCustomExpandItem;
              if AChildNode = nil then
              begin
                AChildNode := TCustomExpandItem.Create(self);
                AChildNode.Text := ADir;
                AChildNode.IsChecked := True;
                AChildNode.Parent := RootNode;
                AChildNode.IsExpanded := true;
                AChildNode.OnChangeExpanded := ItemExpanded;
              end;
              RootNode := AChildNode;
            end;
            if DirIndex = Splitter.Count - 1 then
            begin
              AChildNode := TCustomExpandItem.Create(self);
              AChildNode.Text := AFileName;
              Extension := ExtractFileExtendedExt(AFileName);
              ExtensionsMatch := UsedExtensions.IndexOf(Extension) >= 0;
              if not ExtensionsMatch then
              begin
                for ExtensionIndex := 0 to UsedExtensions.Count - 1 do
                begin
                  SearchExt := UsedExtensions[ExtensionIndex];
                  if Pos('*', SearchExt) <= 0 then
                  begin
                    Continue;
                  end;
                  SearchExt := StringReplace(SearchExt, '*', '',
                    [rfReplaceAll, rfIgnoreCase]);
                  SearchExt := LowerCase(SearchExt);
                  if Pos(SearchExt, LowerCase(Extension)) = 1 then
//                  if TRegEx.IsMatch(Extension, UsedExtensions[ExtensionIndex], [roIgnoreCase]) then
                  begin
                    ExtensionsMatch := True;
                    break;
                  end;
//                  except
//                    ShowMessage(ExtensionIndex.ToString);
//                    Beep;
//                  end;
                end;
              end;
              AChildNode.IsChecked := ExtensionsMatch;
              AChildNode.Parent := RootNode;
              AChildNode.TagString := AFileName;
              AChildNode.IsExpanded := true;
              AChildNode.OnChangeExpanded := ItemExpanded;
            end;
          end;
        end;
        DirectoryName := '';
        while (tvFiles.Count = 1) and (tvFiles.Items[0].Count = 1) do
        begin
          DirItem := tvFiles.Items[0];
          if DirectoryName <> '' then
          begin
            DirectoryName := DirectoryName + PathDelim;
          end;
          DirectoryName := DirectoryName + DirItem.Text;
          for ChildIndex := 0 to DirItem.Count - 1 do
          begin
            AChildItem := DirItem.Items[0];
            AChildItem.Parent := tvFiles;
          end;
          DirItem.Free;
        end;
        if tvFiles.Count = 1 then
        begin
          DirItem := tvFiles.Items[0];
          if DirectoryName <> '' then
          begin
            DirectoryName := DirectoryName + PathDelim;
          end;
          DirectoryName := DirectoryName + DirItem.Text;
          if DirectoryName <> '' then
          begin
            DirectoryName := DirectoryName + PathDelim;
          end;
          DirItem.Text := DirectoryName;
        end;
        tvFiles.CollapseAll;
        if tvFiles.Count >= 1 then
        begin
          DirItem := tvFiles.Items[0];
          DirItem.Expand;
        end;
      finally
        tvFiles.EndUpdate;
      end;
      lstInputExtensions.Items := Extensions;
      lstOutputExtensions.Items := Extensions;
      lstAncillaryExtensions.Items := Extensions;

      for Index := 0 to frmModelArchiver.Extensions.Count - 1 do
      begin
        AnExt := frmModelArchiver.Extensions[Index];
//        ExtIndex := Extensions.IndexOf(AnExt.Extension);
//        if ExtIndex >= 0 then
        if Pos('*', AnExt.Extension) <= 0 then
        begin
          ListControl := nil;
          ExtIndex := Extensions.IndexOf(AnExt.Extension);
          if ExtIndex >= 0 then
          begin
            case AnExt.ExtensionType of
              etModelInput, etModpathInput, etZoneBudgetInput, etMt3dmsInput,
                etOtherInput: ListControl := lstInputExtensions;
              etModelOutput, etModpathOutput, etZoneBudgetOutput, etMt3dmsOutput,
                etOtherOutput: ListControl := lstOutputExtensions;
              etAncillary: ListControl := lstAncillaryExtensions;
            end;
            Item := ListControl.ListItems[ExtIndex];
            Item.IsChecked := True;

            case AnExt.ExtensionType of
              etModelInput: FModelInput.Add(AnExt.Extension);
              etModelOutput: FModelOutput.Add(AnExt.Extension);
              etModpathInput: FModpathInput.Add(AnExt.Extension);
              etModpathOutput: FModpathOutput.Add(AnExt.Extension);
              etZoneBudgetInput: FZoneBudgetInput.Add(AnExt.Extension);
              etZoneBudgetOutput: FZoneBudgetOutput.Add(AnExt.Extension);
              etMt3dmsInput: FMt3dInput.Add(AnExt.Extension);
              etMt3dmsOutput: FMt3dOutput.Add(AnExt.Extension);
              etOtherInput: FOtherInput.Add(AnExt.Extension);
              etOtherOutput: FOtherOutput.Add(AnExt.Extension);
              etAncillary: ;
            end;
          end;
        end
        else
        begin
          SearchExt := StringReplace(AnExt.Extension, '*', '',
            [rfReplaceAll, rfIgnoreCase]);
          SearchExt := LowerCase(SearchExt);
          for ExtIndex := 0 to Extensions.Count - 1 do
          begin
//            if Pos('*', AnExt.Extension) <= 0 then
//            begin
//              Continue;
//            end;
            try
            if Pos(SearchExt, LowerCase(Extensions[ExtIndex])) = 1 then
//            if TRegEx.IsMatch(Extensions[ExtIndex], AnExt.Extension, [roIgnoreCase]) then
            begin
              ListControl := nil;
              case AnExt.ExtensionType of
                etModelInput, etModpathInput, etZoneBudgetInput, etMt3dmsInput,
                  etOtherInput: ListControl := lstInputExtensions;
                etModelOutput, etModpathOutput, etZoneBudgetOutput,
                  etMt3dmsOutput, etOtherOutput: ListControl := lstOutputExtensions;
                etAncillary: ListControl := lstAncillaryExtensions;
              end;
              Item := ListControl.ListItems[ExtIndex];
              Item.IsChecked := True;
//              break;
            end;
            except
              Beep;
              ShowMessage(ExtIndex.ToString);
            end;
          end;
        end;
      end;

    finally
      Splitter.Free;
      Extensions.Free;
      UsedExtensions.Free;
    end
  finally
    tvFiles.EndUpdate;
    lstInputExtensions.EndUpdate;
    lstOutputExtensions.EndUpdate;
    lstAncillaryExtensions.EndUpdate;
  end;
end;

procedure TfrmModelFiles.GetSelectedFiles(SelectedFiles: TStringList);
  procedure HandleFileItem(AnItem: TTreeViewItem);
  var
    ItemIndex: Integer;
    FileName: string;
  begin
    if AnItem.Count > 0 then
    begin
      for ItemIndex := 0 to AnItem.Count - 1 do
      begin
        HandleFileItem(AnItem.Items[ItemIndex]);
      end;
    end
    else if AnItem.IsChecked then
    begin
      FileName := AnItem.TagString;
      Assert(FileName <> '');
      SelectedFiles.AddObject(FileName, AnItem);
    end;
  end;
var
  NodeIndex: Integer;
  AnItem: TTreeViewItem;
begin
  for NodeIndex := 0 to tvFiles.Count - 1 do
  begin
    AnItem := tvFiles.Items[NodeIndex];
    HandleFileItem(AnItem);
  end;
end;

procedure TfrmModelFiles.GetUsedExtensions(UsedExtensions: TStringList;
  ExtensionListBox: TListBox);
var
  ExtensionIndex: Integer;
  Item: TListBoxItem;
begin
  UsedExtensions.Clear;
  for ExtensionIndex := 0 to ExtensionListBox.Count - 1 do
  begin
    Item := ExtensionListBox.ListItems[ExtensionIndex];
    if Item.IsChecked then
    begin
      UsedExtensions.Add(Item.Text);
    end;
  end;
end;

procedure TfrmModelFiles.GetUsedFiles(UsedExtensions, UsedFiles,
  SelectedFiles: TStringList);
var
  FileIndex: Integer;
  FileName: string;
  Extension: string;
begin
  UsedFiles.Clear;
  if UsedExtensions.Count > 0 then
  begin
    for FileIndex := 0 to SelectedFiles.Count - 1 do
    begin
      FileName := SelectedFiles[FileIndex];
      Extension := ExtractFileExtendedExt(FileName);
      if UsedExtensions.IndexOf(Extension) >= 0 then
      begin
        UsedFiles.AddObject(FileName, SelectedFiles.Objects[FileIndex]);
      end;
    end;
  end;
end;


procedure TfrmModelFiles.ItemExpanded(Sender: TObject);
var
  ItemHeight: Single;
begin
  ItemHeight := tvFiles.ItemHeight;
  tvFiles.ItemHeight := ItemHeight * 10;
  tvFiles.ItemHeight := ItemHeight;


//  VScrollBar := TTreeViewCrack(tvFiles).VScrollBar;
//  if VScrollBar <> nil then
//  begin
//    VPosition := VScrollBar.Value;
//    VScrollBar.Value := 0;
//    VScrollBar.Value := tvFiles.ClientHeight*3;
//    VScrollBar.Value := VPosition;
//  end;
end;

procedure TfrmModelFiles.ExtensionsChangeCheck(Sender: TObject);
var
  Item: TListBoxItem;
  ItemIndex: Integer;
  TVItem: TTreeViewItem;
  procedure HandleItem(AnItem: TTreeViewItem);
  var
    ItemExtension: string;
    ChildIndex: Integer;
  begin
    ItemExtension := ExtractFileExtendedExt(AnItem.Text);
    if AnsiCompareText(ItemExtension, Item.Text) = 0 then
    begin
      AnItem.IsChecked := Item.IsChecked;
    end;
    for ChildIndex := 0 to AnItem.Count - 1 do
    begin
      HandleItem(AnItem.Items[ChildIndex]);
    end;
  end;
begin
  Item := Sender as TListBoxItem;
  for ItemIndex := 0 to tvFiles.Count - 1 do
  begin
    TVItem := tvFiles.Items[ItemIndex];
    HandleItem(TVItem);
  end;

end;

procedure TfrmModelFiles.tvFilesChangeCheck(Sender: TObject);
  procedure CheckChildren(ANode: TTreeViewItem);
  var
    ChildIndex: Integer;
    ChildNode: TTreeViewItem;
  begin
    for ChildIndex := 0 to ANode.Count - 1 do
    begin
      ChildNode := ANode[ChildIndex];
      ChildNode.IsChecked := ANode.IsChecked;
      CheckChildren(ChildNode);
    end;
  end;
var
  Item: TTreeViewItem;
begin
  Item := Sender as TTreeViewItem;
  if Item <> nil then
  begin
    CheckChildren(Item);
  end;
end;

end.
