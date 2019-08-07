{#BACKUP ModelArchiver_Icon.ico}

unit frmModelArchiverUnit;

interface

uses Winapi.Windows,
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Menus, FMX.ListBox,
  FMX.Layouts, FMX.TreeView, ArchiveNodeInterface,
  System.Generics.Collections, FMX.StdCtrls, FMX.Controls.Presentation,
  FMX.Graphics, FMX.ScrollBox, FMX.Memo, System.Actions, FMX.ActnList,
  ExtensionTypeUnit, FMX.Platform, Math, System.RegularExpressions,
  FMX.TabControl, Web.HTTPApp, Web.DBWeb, FMX.Edit, System.ImageList,
  FMX.ImgList;

type
  TFileSource = (fsUnknown, fsManual, fsArchiveList, fsArchiveListEdited,
    fsArchiveListDeleted);

  TArchiveTabs = (atStart, atReadMe, atGeoRef, atShapefile, atGraphic, atMetaData,
    atExtensions, ArchiveLists, atFiles, atStructure, atCreate);

  TCustomExpandItem = class(TTreeViewItem)
  private
    FOnChangeExpanded: TNotifyEvent;
    FOnChangeCollapsed: TNotifyEvent;
  published
    property OnChangeExpanded: TNotifyEvent read FOnChangeExpanded
      write FOnChangeExpanded;
    property OnChangeCollapsed: TNotifyEvent read FOnChangeCollapsed
      write FOnChangeCollapsed;
  protected
    procedure SetIsExpanded(const Value: boolean); override;
  public
    constructor Create(AOwner: TComponent); override;

  end;

  TArchiveObject = class(TComponent, IArchiveNodeInterface)
  private
    FNodeType: TNodeType;
    FFileSource: TFileSource;
    FTreeViewItem: TTreeViewItem;
    FOriginalModelName: string;
    FModelDirectory: string;
    FDescription: string;
    FArchiveList: string;
    function GetParentNode: IArchiveNodeInterface;
    function GetModelDirectory: string;
    function GetNodeType: TNodeType;
    function GetNodeText: string;
    function GetChild(Index: Integer): IArchiveNodeInterface;
    function GetCount: Integer;
    procedure SetFileSource(const Value: TFileSource);
    procedure SetModelDirectory(const Value: string);
    procedure SetNodeType(const Value: TNodeType);
    procedure SetOriginalModelName(const Value: string);
    procedure SetDescription(const Value: string);
    function GetDescription: string;
    procedure InvalidateModel;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetChildByName(AName: string): TArchiveObject;
    property NodeType: TNodeType read GetNodeType write SetNodeType;
    property FileSource: TFileSource read FFileSource write SetFileSource;
    property ModelName: string read FOriginalModelName write SetOriginalModelName;
    property ModelDirectory: string read GetModelDirectory write SetModelDirectory;
    property TreeViewItem: TTreeViewItem read FTreeViewItem;
    // @name indicates the type of file such as a MODFLOW name file.
    property Description: string read GetDescription write SetDescription;
    property ArchiveList: string read FArchiveList write FArchiveList;
  end;

  TArchiveItemList = TList<TArchiveObject>;

  TfrmModelArchiver = class(TForm)
    tvArchive: TTreeView;
    lstArchives: TListBox;
    mnbr1: TMenuBar;
    mniFile: TMenuItem;
    mniOpenFileLists: TMenuItem;
    spl1: TSplitter;
    mniCreateArchive: TMenuItem;
    dlgSaveArchive: TSaveDialog;
    pmModifyTree: TPopupMenu;
    mniAddFile: TMenuItem;
    mniAddCategory: TMenuItem;
    mniDelete: TMenuItem;
    mniEdit: TMenuItem;
    pnlArchive: TPanel;
    mniSelectModelFiles: TMenuItem;
    mniSave: TMenuItem;
    dlgSaveFile: TSaveDialog;
    mniOpen: TMenuItem;
    dlgOpenFile: TOpenDialog;
    pnlArchiveLists: TPanel;
    lblArchiveLists: TLabel;
    lblArchiveStructure: TLabel;
    pmList: TPopupMenu;
    mniDeleteArchiveList: TMenuItem;
    dlgOpenFiles: TOpenDialog;
    dlgOpenArchiveLists: TOpenDialog;
    ActionList1: TActionList;
    acOpenFileListsDirectory: TAction;
    mniOpenFileListDirectory: TMenuItem;
    mniEditExtensions: TMenuItem;
    mniFileLists: TMenuItem;
    mniModelFiles: TMenuItem;
    mniDefaultExtensions: TMenuItem;
    mniAddModel: TMenuItem;
    MemoDescription: TMemo;
    pnlDescription: TPanel;
    lblDescription: TLabel;
    mniExportFileDescriptions: TMenuItem;
    sdDescriptions: TSaveDialog;
    pbSaveProgress: TProgressBar;
    mniAddFolder: TMenuItem;
    mniViewDocument: TMenuItem;
    Splitter1: TSplitter;
    mniChangeModelOrder: TMenuItem;
    mniMoveFiles: TMenuItem;
    mniFileSize: TMenuItem;
    mniArchive: TMenuItem;
    mniExportModelDescriptions: TMenuItem;
    miUpdateMetaData: TMenuItem;
    miExit: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    tcMain: TTabControl;
    tiArchiveLists: TTabItem;
    tiArchiveStructure: TTabItem;
    tiStart: TTabItem;
    btnArchiveWebSite: TButton;
    memoGettingStarted: TMemo;
    tiReadMeFile: TTabItem;
    odReadme: TOpenDialog;
    cbIncludeDescriptions: TCheckBox;
    tiModelgeoref: TTabItem;
    odGeoRef: TOpenDialog;
    tiBrowseGraphic: TTabItem;
    odBrowseGraphic: TOpenDialog;
    Memo3: TMemo;
    tiMetadata: TTabItem;
    btnMetadataEditor: TButton;
    btnMetadataParser: TButton;
    btnMetadataDescription: TButton;
    odMetadata: TOpenDialog;
    btnDoiCreate: TButton;
    Panel1: TPanel;
    Splitter2: TSplitter;
    acSelectArchiveList: TAction;
    Panel2: TPanel;
    Splitter3: TSplitter;
    Memo4: TMemo;
    tiFileExtensions: TTabItem;
    tiFiles: TTabItem;
    memoFileExtensions: TMemo;
    btnFileExtensions: TButton;
    acEditFileExtensions: TAction;
    acUseDefaultExtensions: TAction;
    btnDefaultExtensions: TButton;
    btnFiles: TButton;
    Memo5: TMemo;
    pnlNextBack: TPanel;
    btnNext: TButton;
    btnBack: TButton;
    ImageList1: TImageList;
    acNextTabAction: TNextTabAction;
    acPreviousTabAction: TPreviousTabAction;
    Panel3: TPanel;
    lbContents: TListBox;
    lbiGettingStarted: TListBoxItem;
    lbiReadme: TListBoxItem;
    lbiModelGeoref: TListBoxItem;
    lbiBrowseGraphic: TListBoxItem;
    lbiMetadata: TListBoxItem;
    lbiFileExtensions: TListBoxItem;
    lbiArchiveLists: TListBoxItem;
    lbiFiles: TListBoxItem;
    lbiArchiveStructure: TListBoxItem;
    tiCreateArchive: TTabItem;
    acMoveFileToArchiveDirectory: TAction;
    acUpdateMetadata: TAction;
    acCreateArchive: TAction;
    btnUpdateMetadata: TButton;
    btnMoveFilesToArchiveDir: TButton;
    btnCreateArchive: TButton;
    lblArchiveDirectory: TLabel;
    btnCreateDoiReadme: TButton;
    tiGeoref: TTabItem;
    edGeoRefShapefile: TEdit;
    btnGeoRefShapefile: TEditButton;
    Memo1: TMemo;
    odShapefile: TOpenDialog;
    Label1: TLabel;
    edReadMe: TEdit;
    edModelGeoRef: TEdit;
    btnModelGeoRef: TEditButton;
    edBrowseGraphic: TEdit;
    btnBrowseGraphic: TEditButton;
    edArchiveDirectory: TEdit;
    btnSelectArchiveDirectory: TEditButton;
    lblReadme: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    lblMetaData: TLabel;
    memoMetadata: TMemo;
    MemoReadme: TMemo;
    Memo2: TMemo;
    memoCreateArchive: TMemo;
    Button1: TButton;
    acSelectFiles: TAction;
    miHelp: TMenuItem;
    miContents: TMenuItem;
    acContents: TAction;
    memoArchiveLists: TMemo;
    btnUpdateReadme: TButton;
    acUpdateReadme: TAction;
    miUpdateReadme: TMenuItem;
    pnl1: TPanel;
    cbIncludeFileDescriptions: TCheckBox;
    miUndelete: TMenuItem;
    btnEditMetadata: TButton;
    btnEditReadme: TButton;
    btnReadme: TEditButton;
    btnEditGeoRef: TButton;
    btn1: TButton;
    acUpdatePaths: TAction;
    pnl2: TPanel;
    btnOpenFileLists: TButton;
    btnOpenFileListDirectory: TButton;
    btnUpdatePaths: TButton;
    pnl3: TPanel;
    btnUpdatePaths2: TButton;
    edDoi1: TEdit;
    lblDoi1: TLabel;
    edDoi2: TEdit;
    lblDoi2: TLabel;
    lbl1: TLabel;
    edFileUrlRoot: TEdit;
    miAbout: TMenuItem;
    btnMetadataWizard: TButton;
    edMetadata: TEdit;
    btnMetadata: TEditButton;
    procedure mniOpenFileListsClick(Sender: TObject);
    procedure mniCreateArchiveClick(Sender: TObject);
    procedure tvArchiveClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
    procedure mniAddFileClick(Sender: TObject);
    procedure mniAddModelClick(Sender: TObject);
    procedure mniAddCategoryClick(Sender: TObject);
    procedure mniEditClick(Sender: TObject);
    procedure tvArchiveMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure tvArchiveMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mniSelectModelFilesClick(Sender: TObject);
    procedure mniSaveClick(Sender: TObject);
    procedure mniOpenClick(Sender: TObject);
    procedure mniDeleteArchiveListClick(Sender: TObject);
    procedure PaintItem(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
    procedure acOpenFileListsDirectoryExecute(Sender: TObject);
    procedure mniEditExtensionsClick(Sender: TObject);
    procedure mniDefaultExtensionsClick(Sender: TObject);
    procedure MemoDescriptionChange(Sender: TObject);
    procedure mniExportFileDescriptionsClick(Sender: TObject);
    procedure mniViewDocumentClick(Sender: TObject);
    procedure mniChangeModelOrderClick(Sender: TObject);
    procedure tvArchiveDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure mniMoveFilesClick(Sender: TObject);
    procedure mniFileSizeClick(Sender: TObject);
    procedure mniExportModelDescriptionsClick(Sender: TObject);
    procedure miUpdateMetaDataClick(Sender: TObject);
    procedure miExitClick(Sender: TObject);
    procedure miCutClick(Sender: TObject);
    procedure miPasteClick(Sender: TObject);
    procedure btnArchiveWebSiteClick(Sender: TObject);
    procedure btnReadMeClick(Sender: TObject);
    procedure btnModelGeoRefClick(Sender: TObject);
    procedure btnBrowseGraphicClick(Sender: TObject);
    procedure btnMetadataEditorClick(Sender: TObject);
    procedure btnMetadataParserClick(Sender: TObject);
    procedure btnMetadataDescriptionClick(Sender: TObject);
    procedure btnMetadataClick(Sender: TObject);
    procedure btnDoiCreateClick(Sender: TObject);
    procedure lbContentsChange(Sender: TObject);
    procedure tcMainChange(Sender: TObject);
    procedure btnSelectArchiveDirectoryClick(Sender: TObject);
    procedure edReadMeChange(Sender: TObject);
    procedure edModelGeoRefChange(Sender: TObject);
    procedure edBrowseGraphicChange(Sender: TObject);
    procedure edMetadataChange(Sender: TObject);
    procedure btnGeoRefShapefileClick(Sender: TObject);
    procedure edGeoRefShapefileChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure lstArchivesMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure lstArchivesMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure acContentsExecute(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure lstArchivesDragDrop(Sender: TObject; const Data: TDragObject;
      const Point: TPointF);
    procedure lstArchivesMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Single);
    procedure acUpdateReadmeExecute(Sender: TObject);
    procedure miUndeleteClick(Sender: TObject);
    procedure btnEditReadmeClick(Sender: TObject);
    procedure btnEditMetadataClick(Sender: TObject);
    procedure btnEditGeoRefClick(Sender: TObject);
    procedure btn1Click(Sender: TObject);
    procedure acUpdatePathsExecute(Sender: TObject);
    procedure edDoi2Change(Sender: TObject);
    procedure edDoi1Change(Sender: TObject);
    procedure cbIncludeFileDescriptionsChange(Sender: TObject);
    procedure edFileUrlRootChange(Sender: TObject);
    procedure miAboutClick(Sender: TObject);
    procedure btnMetadataWizardClick(Sender: TObject);
  private
    FModelNode: TArchiveObject;
    FItem: TArchiveObject;
    FSelectedNodes: TArchiveItemList;
    FCopiedNodes: TArchiveItemList;
    FBaseNodes: TArchiveNodeList;
    FInputNode: TArchiveObject;
    FOutputNode: TArchiveObject;
    FAncillaryNode: TArchiveObject;
    FReadMeNode: TArchiveObject;
    FGeoRefNode: TArchiveObject;
    FExtensions: TExtensionList;
    FBinNode: TArchiveObject;
    FGeoRefDirNode: TArchiveObject;
    FWebReleaseNode: TArchiveObject;
    FCurrentItem: TArchiveObject;
    FSourceNode: TArchiveObject;
    FNonPublicNode: TArchiveObject;
    FSelectingItem: Boolean;
    FRegExpressions: TStringList;
    FExtensionDictionary: TDictionary<String, TExtensionObject>;
    FPasteItem: TArchiveObject;
    FSelectedList: TListBoxItem;
    FShouldSaveFile: Boolean;
    FCaptionRoot: string;
    procedure InitializeFormForModel;
    procedure InitializeFormForCategory;
    procedure InitializeFormForFolder;
    procedure SaveArchive(DirectoryName: string);
    procedure UpdateSelectedNodes;
    procedure ShowArchiveStructure;
    function AddManualCategory(CategoryName: string): TArchiveObject;
    procedure SetTreeViewHint(Sender: TObject);
    function GetParentCategoryItem: TArchiveObject;
    procedure EnableAddFile;
    procedure SetDefaultExtensions;
    function GetChildByName(AName: string): TArchiveObject;
    procedure ItemExpanded(Sender: TObject);
    procedure SaveExtensionsToFile;
    procedure LoadExtensionsFromFile;
    function GetExtensionFileName: string;
    procedure EraseTreeViewHint(Sender: TObject);
    procedure SetListBoxItemsSize;
    procedure FillExtensionDictionary(ExtensionDictionary:
      System.Generics.Collections.TDictionary<string, TExtensionObject>;
      RegExtensions: TStringList);
    procedure CreateBaseNodes;
    procedure EnableAddFolder;
    Procedure SaverProgress(Sender: TObject; Fraction: double);
    function GetParentNonFileItem: TArchiveObject;
    procedure mniAddModelOrFolder(Sender: TObject; Directory: string);
    procedure AddFiles(ParentItem: TArchiveObject; AddedFiles: TStrings; SkipCheckFolder: boolean = False);
    procedure MoveFilesArchive(DirectoryName: string);
    function CalculateUncompressedModelSize: Int64;
    procedure UpdateMetaData(DirectoryName: string);
    procedure InvalidateArchiveTreeView;
    function GetDropTarget(DropTarget: TTreeViewItem; var DropArchive: TTreeViewItem): boolean;
    procedure MoveNodes(DropArchive: TTreeViewItem; NodeList: TArchiveItemList);
    function GetBrowseGraphicItem: TTreeViewItem;
    function GetMetadataItem: TTreeViewItem;
    function GetShapefileItem: TTreeViewItem;
    procedure AskToSaveModel;
    procedure AddChildNode(ParentItem: TArchiveObject;
      var ChildNode: TArchiveObject; Directory: string);
    procedure UpdateReadmeTxt;
    function GetIniFileName: string;
    procedure LoadIniFile;
    procedure SaveIniFile;
    procedure UpdateMetaDataEdit;
    { Private declarations }
  public
    procedure AssignDescription(ArchiveFileObject: TArchiveObject;
      out ExtObj: TExtensionObject);
    procedure InitializeDescriptionSearchObjects;
    property Extensions: TExtensionList read FExtensions;
    procedure SortModels(ModelNamesInDesiredOrder: TStrings);
    { Public declarations }
  end;

var
  frmModelArchiver: TfrmModelArchiver;

const
  KModelArchiverVersion = '1.0';

procedure OpenHelpUrl(FileName: string);

implementation

uses
  Xml.VerySimple, frmAddModelOrClassUnit, System.IOUtils, ArchiveSaveUnit,
  frmModelFilesUnit, frmExtensionsUnit, frmPreviewUnit, frmArrangeModelsUnit,
  Mobile.OpenURL, fOpen, frmReadmeUnit, frmHelpUnit, frmEditMetadataUnit,
  frmArchiveUpdateUnit, frmUpdatePathsUnit, System.IniFiles,
  frmAboutModelArchiverUnit;

resourcestring
  StrNodeType = 'Node_Type';
  StrCategory = 'Category';
  StrModel = 'Model';
  StrFileName = 'Model_FileName';
  StrArchiveStructure = 'Archive_Structure';
  StrArchiveLists = 'ArchiveLists';
  StrArchiveListFileNam = 'ArchiveList_File_Name';
  StrModelName = 'Model_Name';
  StrArchiveRoot = 'Archive_Root';
  StrUncompressedCategor = 'Uncompressed_Category';
  StrCompressedCategory = 'Compressed_Category';
  StrFileType = 'FileType';
  StrInput = 'Input';
  StrOutput = 'Output';
  StrAncillary = 'Ancillary';
  StrAncillaryLC = 'ancillary';
  StrDescription = 'Description';
  StrModelDirectory = 'ModelDirectory';
  StrFolder = 'Folder';
  StrTheUncompressedArc = 'The uncompressed archive size is %f Gb. This prog' +
  'ram can not create .zip files correctly if the zip file will be more than' +
  ' 4 GB in size. Do you want to continue?';
  StrMODPATHInput = 'MODPATH_Input';
  StrMODPATHOutput = 'MODPATH_Output';
  StrZoneBudgetInput = 'ZoneBudget_Input';
  StrZoneBudgetOutput = 'ZoneBudget_Output';
  StrMT3DMSInput = 'MT3DMS_Input';
  StrMT3DMSOutput = 'MT3DMS_Output';
  StrThereIsntEnought = 'There isn''t enought free space on the selected dri' +
  've. You need %d kb more space.';
  StrArchiveDirectory = 'Archive_Directory';
  StrAppendDescriptions = 'Append_Descriptions';
  StrAppendMetadataDescriptions = 'Append_Metadata_Descriptions';
  StrTheReadmetxtFile = 'The readme.txt file in the archive directory will b' +
  'e replaced by a new version. Do you want to continue?';
  StrOtherInput = 'Other_Input';
  StrOtherOutput = 'Other_Output';
  StrURLs = 'URLs';
  StrDOICreate = 'DOI_Create';
  StrFileUrlRoot = 'FileUrlRoot';
  StrSwiObsExtInput = 'SwiObsExtInput';
  StrSwiObsExtOutput = 'SwiObsExtOutput';

const
  StrFileSource = 'File_Source';
  StrUnknown = 'Unknown';
  StrManual = 'Manual';
  StrArchiveList = 'ArchiveList';
  StrArchiveListEd = 'ArchiveListEdited';
  StrArchiveListDeleted = 'ArchiveListDeleted';

{$R *.fmx}

//var FourGigaBytes: Int64;

type
  TTreeViewCrack = class(TTreeView);


procedure TfrmModelArchiver.miAboutClick(Sender: TObject);
begin
  frmAboutModelArchiver.ShowModal;
end;

procedure TfrmModelArchiver.miCutClick(Sender: TObject);
var
  Item: TTreeViewItem;
  ArchiveObj: TArchiveObject;
begin
  if Focused = nil then
  begin
    Exit;
  end
  else if Focused is TTreeView then
  begin
    Item := tvArchive.Selected;
    if Item <> nil then
    begin
      ArchiveObj := Item.TagObject as TArchiveObject;
      if FBaseNodes.IndexOf(ArchiveObj) >= 0 then
      begin
        Beep;
        MessageDlg('You can not cut this node.', TMsgDlgType.mtWarning,
          [TMsgDlgBtn.mbOK], 0);
        Exit;
      end
    end;

    UpdateSelectedNodes;
    FCopiedNodes.Clear;
    FCopiedNodes.AddRange(FSelectedNodes.ToArray);
    FPasteItem := nil;
  end
  else if Focused is TCustomMemo then
  begin
    TCustomMemo(Focused).CopyToClipboard;
  end
  else if Focused is TCustomEdit then
  begin
    TCustomEdit(Focused).CopyToClipboard;
  end;

end;

procedure TfrmModelArchiver.miExitClick(Sender: TObject);
begin
  Close
end;

procedure TfrmModelArchiver.miPasteClick(Sender: TObject);
var
  DropTarget: TTreeViewItem;
  DropArchive: TTreeViewItem;
//var
//  DropTarget: TArchiveObject;
begin
// Paste
  if Focused = nil then
  begin
    Exit;
  end
  else if Focused is TTreeView  then
  begin
    if FPasteItem = nil then
    begin
      Beep;
      MessageDlg('You need to select a parent node.', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
      DropTarget := FPasteItem.TreeViewItem;
      if (DropTarget <> nil) then  { is there an item here? }
      begin
        if not GetDropTarget(DropTarget, DropArchive) then
        begin
          Exit;
        end;
        MoveNodes(DropArchive, FCopiedNodes);
        FCopiedNodes.Clear;
      end;
    end;
  end
  else if Focused is TCustomMemo then
  begin
    TCustomMemo(Focused).PasteFromClipboard;
  end
  else if Focused is TCustomEdit then
  begin
    TCustomEdit(Focused).PasteFromClipboard;
  end;
end;

procedure TfrmModelArchiver.miUndeleteClick(Sender: TObject);
var
  Item: TTreeViewItem;
  ArchiveObj: TArchiveObject;
begin
  Item := tvArchive.Selected;
  if Item <> nil then
  begin
    ArchiveObj := Item.TagObject as TArchiveObject;
    if ArchiveObj.FFileSource = fsArchiveListDeleted then
    begin
      ArchiveObj.FFileSource := fsArchiveListEdited
    end;
    FShouldSaveFile := True;
  end;
end;

procedure TfrmModelArchiver.miUpdateMetaDataClick(Sender: TObject);
var
  NewFileName: string;
//var
//  Directory: string;
begin
  if TDirectory.Exists(edArchiveDirectory.Text) then
  begin
    if not cbIncludeFileDescriptions.IsChecked then
    begin
      Beep;
      MessageDlg('The "' + cbIncludeFileDescriptions.Text + '" checkbox has not been checked so the metadata will not be updated.',
        TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
      UpdateMetaDataEdit;
      if TFile.Exists(edMetadata.Text)  then
      begin
        NewFileName := IncludeTrailingPathDelimiter(edArchiveDirectory.Text);
        NewFileName := IncludeTrailingPathDelimiter(NewFileName  + 'webrelease');
        NewFileName := NewFileName + ExtractFileName(edMetadata.Text);
        TFile.Copy(edMetadata.Text, NewFileName, True);
        UpdateMetaData(edArchiveDirectory.Text);
      end
      else
      begin
        Beep;
        MessageDlg('The metadata file has not been specified so the metadata will not be updated.',
          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      end;
    end;
  end
  else
  begin
    Beep;
    MessageDlg('The archive directory does not exist',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmModelArchiver.mniAddCategoryClick(Sender: TObject);
var
  CategoryName: string;
begin
  InitializeFormForCategory;
  if (frmModelOrClass.ShowModal = mrOk)
    and (frmModelOrClass.edtName.Text <> '') then
  begin
    CategoryName := frmModelOrClass.edtName.Text;
    AddManualCategory(CategoryName);
  end;
end;

procedure TfrmModelArchiver.mniAddFileClick(Sender: TObject);
var
  ParentItem: TArchiveObject;
  AddedFiles: TStrings;
begin
  dlgOpenFiles.Options := dlgOpenFiles.Options + [TOpenOption.ofAllowMultiSelect];
  if dlgOpenFiles.Execute then
  begin
    if tvArchive.Selected = nil then
    begin
      Beep;
      MessageDlg('You need to select a parent node.', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], 0);
      Exit;

    end;
    ParentItem := tvArchive.Selected.TagObject as TArchiveObject;
    Assert(ParentItem <> nil);
    if ParentItem.NodeType = ntFile then
    begin
      ParentItem := ParentItem.TreeViewItem.ParentItem.TagObject as TArchiveObject;
      Assert(ParentItem <> nil);
      Assert(ParentItem.NodeType <> ntFile);
    end;

    AddedFiles := dlgOpenFiles.Files;
    AddFiles(ParentItem, AddedFiles);
  end;
end;

function TfrmModelArchiver.GetChildByName(AName: string): TArchiveObject;
var
  ItemIndex: Integer;
//  ArchiveObj: TArchiveObject;
begin
  result := nil;
  for ItemIndex := 0 to tvArchive.Count - 1 do
  begin
    if tvArchive.Items[ItemIndex].Text = AName then
    begin
      result := tvArchive.Items[ItemIndex].TagObject as TArchiveObject;
      break;
    end;
//    if tvArchive.Items[ItemIndex].TagObject is TArchiveObject then
//    begin
//      ArchiveObj := TArchiveObject(tvArchive.Items[ItemIndex].TagObject);
//      if ArchiveObj.NodeType = ntFile then
//      begin
//        if ExtractFileName(tvArchive.Items[ItemIndex].Text) = ExtractFileName(AName) then
//        begin
//          result := ArchiveObj;
//          break;
//        end;
//      end;
//    end;
  end;
end;

function TfrmModelArchiver.GetParentNonFileItem: TArchiveObject;
begin
  if tvArchive.Selected = nil then
  begin
    result := nil
  end
  else
  begin
    result := tvArchive.Selected.TagObject as TArchiveObject;
    Assert(result <> nil);
    while result.NodeType = ntFile do
    begin
      result := result.TreeViewItem.ParentItem.TagObject as TArchiveObject;
      Assert(result <> nil);
    end;
  end;
end;


function TfrmModelArchiver.GetParentCategoryItem: TArchiveObject;
begin
  if tvArchive.Selected = nil then
  begin
    result := nil
  end
  else
  begin
    result := tvArchive.Selected.TagObject as TArchiveObject;
    Assert(result <> nil);
    while not (result.NodeType in [ntCategory, ntArchiveRoot,
      ntCategoryUncompressed, ntCategoryCompressed]) do
    begin
      result := result.TreeViewItem.ParentItem.TagObject as TArchiveObject;
      Assert(result <> nil);
    end;
  end;
end;

procedure TfrmModelArchiver.EnableAddFolder;
var
  ShouldEnable: Boolean;
  ParentItem: TArchiveObject;
begin
  ShouldEnable := tvArchive.Selected <> nil;

  if ShouldEnable then
  begin
    ParentItem := GetParentNonFileItem;
    if (ParentItem = FReadMeNode)
      or (ParentItem = FGeoRefNode)
      or (ParentItem = FWebReleaseNode)
      or (ParentItem = FGeoRefDirNode)
      then
    begin
      ShouldEnable := False;
    end;
  end;

  mniAddFolder.Enabled := ShouldEnable

end;

procedure TfrmModelArchiver.EnableAddFile;
begin
  if (tvArchive.Selected <> nil) and
    ((tvArchive.Selected.TagObject = FReadMeNode)
    or (tvArchive.Selected.TagObject = FGeoRefNode)) then
  begin
    mniAddFile.Enabled := tvArchive.Selected.Count <= 1;
  end
  else
  begin
    mniAddFile.Enabled := tvArchive.Selected <> nil;
  end;
end;

procedure TfrmModelArchiver.AddChildNode(ParentItem: TArchiveObject;
  var ChildNode: TArchiveObject; Directory: string);
var
  TreeViewItem: TCustomExpandItem;
begin
  ChildNode := ParentItem.GetChildByName(
    frmModelOrClass.edtName.Text) as TArchiveObject;
  if ChildNode = nil then
  begin
    TreeViewItem := TCustomExpandItem.Create(self);
    TreeViewItem.OnMouseEnter := SetTreeViewHint;
    TreeViewItem.OnMouseLeave := EraseTreeViewHint;
    TreeViewItem.OnChangeExpanded := ItemExpanded;
    TreeViewItem.OnDragDrop := tvArchiveDragDrop;
    ChildNode := TArchiveObject.Create(TreeViewItem);
    TreeViewItem.OnPaint := PaintItem;
    TreeViewItem.Parent := ParentItem.TreeViewItem;
    TreeViewItem.Text := frmModelOrClass.edtName.Text;
    ChildNode.SetNodeType(ntModel);
    ChildNode.FileSource := fsManual;
    ChildNode.ModelDirectory :=
      IncludeTrailingPathDelimiter(Directory);
    FShouldSaveFile := True;
  end;
end;


procedure TfrmModelArchiver.mniAddModelOrFolder(Sender: TObject;
  Directory: string);
var
  ParentItem: TArchiveObject;
  ChildNode: TArchiveObject;
begin
  if Sender = mniAddModel then
  begin
    InitializeFormForModel;
    frmModelOrClass.edModelDirectory.Text := Directory;
    ParentItem := nil
  end
  else
  begin
    ParentItem := GetParentNonFileItem;
    if (ParentItem = FReadMeNode)
      or (ParentItem = FGeoRefNode)
      or (ParentItem = FWebReleaseNode)
      or (ParentItem = FGeoRefDirNode)
      then
    begin
      Beep;
      MessageDlg('Subfolders in this item are not allowed.',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;
    InitializeFormForFolder;
    frmModelOrClass.edModelDirectory.Text := Directory;
  end;
  if (frmModelOrClass.ShowModal = mrOk)
    and (frmModelOrClass.edtName.Text <> '') then
  begin
    FModelNode := nil;
    if ParentItem <> nil then
    begin
      AddChildNode(ParentItem, ChildNode, frmModelOrClass.edModelDirectory.Text);
      FModelNode := ChildNode;
    end;
    if (Sender <> mniAddModel) and (FModelNode <> nil) then
    begin
      FModelNode.NodeType := ntFolder;
    end;

    if Sender = mniAddModel then
    begin
//      if ParentItem <> FInputNode then
      begin
        AddChildNode(FInputNode, ChildNode, frmModelOrClass.edModelDirectory.Text);
      end;
//      if ParentItem <> FOutputNode then
      begin
        AddChildNode(FOutputNode, ChildNode, frmModelOrClass.edModelDirectory.Text);
      end;
//      if ParentItem <> FAncillaryNode then
      begin
        AddChildNode(FAncillaryNode, ChildNode, frmModelOrClass.edModelDirectory.Text);
      end;
    end
    else
    if ParentItem = nil then
    begin
      Beep;
      MessageDlg('Subfolders in this item are not allowed.',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end;
  end;
end;

procedure TfrmModelArchiver.mniAddModelClick(Sender: TObject);
begin
  mniAddModelOrFolder(Sender, '');
end;

procedure TfrmModelArchiver.mniChangeModelOrderClick(Sender: TObject);
begin
  frmHelp.Hide;
  frmArrangeModels.GetModelData(FInputNode);
  frmArrangeModels.ShowModal;
end;

procedure TfrmModelArchiver.mniCreateArchiveClick(Sender: TObject);
var
  ErrorMessages: TStringList;
  ANode: TTreeViewItem;
  FileName: string;
  ArchiveSize: Int64;
  Directory: string;
  FreeSpace: Int64;
  Totalspace: Int64;
  Files: TStringDynArray;
begin
  ArchiveSize := CalculateUncompressedModelSize;
  ErrorMessages := TStringList.Create;
  try
    if FReadMeNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('The file "readme.txt" is required but has not been specified.')
    end;
    if FReadMeNode.TreeViewItem.Count = 1 then
    begin
      ANode := FReadMeNode.TreeViewItem.Items[0];
      FileName := ExtractFileName(ANode.Text);
      if AnsiCompareText('readme.txt', FileName) <> 0 then
      begin
        ErrorMessages.Add('The file specified for "readme.txt" is is not named "readme.txt."')
      end;
    end;
    if FReadMeNode.TreeViewItem.Count > 1 then
    begin
      ErrorMessages.Add('Only one "readme.txt" file for the entire archive may be specified.')
    end;

    if FGeoRefNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('The file "modelgeoref.txt" is required but has not been specified.')
    end;
    if FGeoRefNode.TreeViewItem.Count = 1 then
    begin
      ANode := FGeoRefNode.TreeViewItem.Items[0];
      FileName := ExtractFileName(ANode.Text);
      if AnsiCompareText('modelgeoref.txt', FileName) <> 0 then
      begin
        ErrorMessages.Add('The file specified for "modelgeoref.txt" is is not named "modelgeoref.txt."')
      end;
    end;
    if FGeoRefNode.TreeViewItem.Count > 1 then
    begin
      ErrorMessages.Add('Only one "modelgeoref.txt" file may be specified.')
    end;

    if FBinNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('No binary files have been specified in the bin directory.')
    end;

    if FGeoRefDirNode.TreeViewItem.Count < 3 then
    begin
      ErrorMessages.Add('The georef directory must contain the model outline.')
    end;

    if FWebReleaseNode.GetCount < 2 then
    begin
      ErrorMessages.Add('The webrelease directory must contain a thumbnail image of the model domain and FGDC XML metadata.')
    end;

    if ErrorMessages.Count > 0 then
    begin
      ErrorMessages.Insert(0, 'There are problems with this archive.');
      ErrorMessages.Add('');
      ErrorMessages.Add('Do you want to continue anyway?');
      if MessageDlg(ErrorMessages.Text, TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
      begin
        Exit;
      end;
    end;

  finally
    ErrorMessages.Free;
  end;

  if TDirectory.Exists(edArchiveDirectory.Text) then
  begin
    Directory := edArchiveDirectory.Text;
    Files := TDirectory.GetFiles(Directory, '*.*', TSearchOption.soAllDirectories);
    if Length(Files) > 0 then
    begin
      Beep;
      MessageDlg('The archive directory must be empty',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
      Directory := edArchiveDirectory.Text;
      System.SysUtils.GetDiskFreeSpaceEx(PChar(Directory), FreeSpace, Totalspace, nil);
  //    FreeSpace := DiskFree(ExtractFileDrive(Directory));
      if FreeSpace > ArchiveSize then
      begin
        SaveArchive(Directory);
      end
      else
      begin
        Beep;
        MessageDlg(Format(StrThereIsntEnought, [(ArchiveSize-FreeSpace)/1024]),
          TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      end;
    end;
  end
  else
  begin
    Beep;
    MessageDlg('The archive directory does not exist',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;


//  if SelectDirectory('Select directory for Archive', '', Directory) then
//  begin
//  end;

//  if dlgSaveArchive.Execute then
//  begin
//    SaveArchive(dlgSaveArchive.FileName);
//  end;
end;

procedure TfrmModelArchiver.mniEditExtensionsClick(Sender: TObject);
begin
  frmHelp.Hide;
  frmExtensions.GetExtensionData;
  frmExtensions.ShowModal;
end;

procedure TfrmModelArchiver.mniExportFileDescriptionsClick(Sender: TObject);
var
  ArchiveSaver: TArchiveSaver;
begin
  if sdDescriptions.Execute then
  begin
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ArchiveSaver.RootNodes := FBaseNodes;
      ArchiveSaver.WriteFileDescriptions(sdDescriptions.FileName, dwAll);
    finally
      ArchiveSaver.Free;
    end;
  end;
end;

procedure TfrmModelArchiver.mniExportModelDescriptionsClick(Sender: TObject);
var
  ArchiveSaver: TArchiveSaver;
  ModelNodes: TArchiveNodeList;
begin
  if sdDescriptions.Execute then
  begin
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ModelNodes := TArchiveNodeList.Create;
      try
        ModelNodes.Add(FInputNode);
        ArchiveSaver.RootNodes := ModelNodes;
        ArchiveSaver.WriteFileDescriptions(sdDescriptions.FileName, dwModel);
      finally
        ModelNodes.Free;
      end;
    finally
      ArchiveSaver.Free;
    end;
  end;
end;

Function TfrmModelArchiver.CalculateUncompressedModelSize: Int64;
var
  NodeIndex: Integer;
  AnItem: TTreeViewItem;
  ArchiveObject: TArchiveObject;
  procedure HandleNode(ANode: IArchiveNodeInterface);
  var
    FileName: string;
    FileStream: TFileStream;
    ChildIndex: Integer;
  begin
    if ANode.NodeType = ntFile then
    begin
      FileName := ANode.NodeText;
      if TFile.Exists(FileName) then
      begin
        FileStream := TFile.OpenRead(FileName);
        try
          result := result + FileStream.Size;
        finally
          FileStream.Free;
        end;
      end;
    end;

    for ChildIndex := 0 to ANode.Count - 1 do
    begin
      HandleNode(ANode.Children[ChildIndex]);
    end;
  end;
begin
  result := 0;
  for NodeIndex := 0 to tvArchive.Count - 1 do
  begin
    AnItem := tvArchive.Items[NodeIndex];
    ArchiveObject :=  AnItem.TagObject as TArchiveObject;
    HandleNode(ArchiveObject);
  end;
end;

procedure TfrmModelArchiver.cbIncludeFileDescriptionsChange(Sender: TObject);
begin
  edFileUrlRoot.Enabled := cbIncludeFileDescriptions.IsChecked;
end;

procedure TfrmModelArchiver.mniFileSizeClick(Sender: TObject);
var
  ArchiveSize: Int64;
begin
  ArchiveSize := CalculateUncompressedModelSize;
  if ArchiveSize < 1024 then
  begin
    ShowMessage(Format('The uncompressed archive size is %d bytes.', [ArchiveSize]));
  end
  else if ArchiveSize < 1024*1024 then
  begin
    ShowMessage(Format('The uncompressed archive size is %f Kb.', [ArchiveSize/1024]));
  end
  else if ArchiveSize < 1024*1024*1024 then
  begin
    ShowMessage(Format('The uncompressed archive size is %f Mb.', [ArchiveSize/1024/1024]));
  end
  else
  begin
    ShowMessage(Format('The uncompressed archive size is %f Gb.', [ArchiveSize/1024/1024/1024]));
  end;
end;

procedure TfrmModelArchiver.mniMoveFilesClick(Sender: TObject);
var
  ErrorMessages: TStringList;
  ANode: TTreeViewItem;
  FileName: string;
//  Directory: string;
  Files: TStringDynArray;
begin
  ErrorMessages := TStringList.Create;
  try
    if FReadMeNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('The file "readme.txt" is required but has not been specified.')
    end;
    if FReadMeNode.TreeViewItem.Count = 1 then
    begin
      ANode := FReadMeNode.TreeViewItem.Items[0];
      FileName := ExtractFileName(ANode.Text);
      if AnsiCompareText('readme.txt', FileName) <> 0 then
      begin
        ErrorMessages.Add('The file specified for "readme.txt" is is not named "readme.txt."')
      end;
    end;
    if FReadMeNode.TreeViewItem.Count > 1 then
    begin
      ErrorMessages.Add('Only one "readme.txt" file for the entire archive may be specified.')
    end;

    if FGeoRefNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('The file "modelgeoref.txt" is required but has not been specified.')
    end;
    if FGeoRefNode.TreeViewItem.Count = 1 then
    begin
      ANode := FGeoRefNode.TreeViewItem.Items[0];
      FileName := ExtractFileName(ANode.Text);
      if AnsiCompareText('modelgeoref.txt', FileName) <> 0 then
      begin
        ErrorMessages.Add('The file specified for "modelgeoref.txt" is is not named "modelgeoref.txt."')
      end;
    end;
    if FGeoRefNode.TreeViewItem.Count > 1 then
    begin
      ErrorMessages.Add('Only one "modelgeoref.txt" file may be specified.')
    end;

    if FBinNode.TreeViewItem.Count = 0 then
    begin
      ErrorMessages.Add('No binary files have been specified in the bin directory.')
    end;

    if FGeoRefDirNode.TreeViewItem.Count < 3 then
    begin
      ErrorMessages.Add('The georef directory must contain the model outline.')
    end;

    if FWebReleaseNode.GetCount < 2 then
    begin
      ErrorMessages.Add('The webrelease directory must contain a thumbnail image of the model domain and FGDC XML metadata.')
    end;

    if ErrorMessages.Count > 0 then
    begin
      ErrorMessages.Insert(0, 'There are problems with this archive.');
      ErrorMessages.Add('');
      ErrorMessages.Add('Do you want to continue anyway?');
      if MessageDlg(ErrorMessages.Text, TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes then
      begin
        Exit;
      end;
    end;

  finally
    ErrorMessages.Free;
  end;

  if TDirectory.Exists(edArchiveDirectory.Text) then
  begin
    Files := TDirectory.GetFiles(edArchiveDirectory.Text, '*.*', TSearchOption.soAllDirectories);
    if Length(Files) > 0 then
    begin
      Beep;
      MessageDlg('The archive directory must be empty',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    end
    else
    begin
      MoveFilesArchive(edArchiveDirectory.Text);
    end;
  end
  else
  begin
    Beep;
    MessageDlg('The archive directory does not exist',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

procedure TfrmModelArchiver.mniDefaultExtensionsClick(Sender: TObject);
begin
  SetDefaultExtensions;
end;

procedure TfrmModelArchiver.mniDeleteArchiveListClick(Sender: TObject);
var
  ItemIndex: Integer;
  AnItem: TListBoxItem;
  ItemsToDelete: TObjectList<TListBoxItem>;
  ItemNames: TStringList;
  NameIndex: Integer;
begin
  lstArchives.BeginUpdate;
  try
    ItemNames := TStringList.Create;
    ItemsToDelete := TObjectList<TListBoxItem>.Create(False);
    try
      for ItemIndex := 0 to lstArchives.Items.Count - 1 do
      begin
        AnItem := lstArchives.ListItems[ItemIndex];
        if AnItem.IsSelected then
        begin
          ItemsToDelete.Add(AnItem);
          ItemNames.Add(AnItem.Text);
        end;
      end;
      if ItemNames.Count > 0 then
      begin
        if ItemNames.Count > 1 then
        begin
          for NameIndex := 0 to ItemNames.Count - 2 do
          begin
            ItemNames[NameIndex] := ItemNames[NameIndex] + ',';
          end;
          ItemNames.Insert(ItemNames.Count - 1, 'and');
        end;
        if MessageDlg(Format('Are you sure you want to delete %s?', [Trim(ItemNames.Text)]), TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
        begin
          ItemsToDelete.OwnsObjects := True;
        end;
      end
      else
      begin
        MessageDlg('Nothing to delete.', TMsgDlgType.mtInformation,
          [TMsgDlgBtn.mbOK], 0);
        Exit;
      end;
    finally
      ItemsToDelete.Free;
      ItemNames.Free;
    end;
  finally
    lstArchives.EndUpdate;
  end;
  ShowArchiveStructure;
end;

procedure TfrmModelArchiver.mniDeleteClick(Sender: TObject);
var
  Item: TTreeViewItem;
  ArchiveObj: TArchiveObject;
begin
  Item := tvArchive.Selected;
  if Item <> nil then
  begin
    ArchiveObj := Item.TagObject as TArchiveObject;
    if FBaseNodes.IndexOf(ArchiveObj) >= 0 then
    begin
      Beep;
      MessageDlg('You can not delete this node.', TMsgDlgType.mtWarning,
        [TMsgDlgBtn.mbOK], 0);
    end
    else if MessageDlg(Format('Are you sure you want to delete %s?', [Item.Text]),
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes then
    begin
      if ArchiveObj.FFileSource in [fsArchiveList, fsArchiveListEdited,
        fsArchiveListDeleted] then
      begin
        ArchiveObj.FFileSource := fsArchiveListDeleted
      end
      else
      begin
        Item.Free;
      end;
      FShouldSaveFile := True;
    end;
  end;
end;

procedure TfrmModelArchiver.mniEditClick(Sender: TObject);
var
  Item: TArchiveObject;
  ParentItem: TArchiveObject;
  OldName: string;
  ModelItem: TArchiveObject;
begin
  Item  := tvArchive.Selected.TagObject as TArchiveObject;

  if FBaseNodes.IndexOf(Item) >= 0 then
  begin
    Beep;
    MessageDlg('You can not rename this node.', TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;

  case Item.NodeType of
    ntCategory, ntArchiveRoot, ntCategoryUncompressed, ntCategoryCompressed:
      begin
        InitializeFormForCategory;
        frmModelOrClass.Caption := 'Rename Category';
        frmModelOrClass.edtName.Text := Item.TreeViewItem.Text;
        if (frmModelOrClass.ShowModal = mrOk)
          and (frmModelOrClass.edtName.Text <> '') then
        begin
          Item.TreeViewItem.Text := frmModelOrClass.edtName.Text;
          if Item.FileSource in [fsArchiveList, fsArchiveListEdited] then
          begin
            Item.FileSource := fsArchiveListEdited;
          end
          else
          begin
            Item.FileSource := fsManual;
          end;
          InvalidateArchiveTreeView;
          FShouldSaveFile := True;
        end;
      end;
    ntModel:
      begin
        InitializeFormForModel;
        frmModelOrClass.Caption := 'Rename Model';
        frmModelOrClass.edtName.Text := Item.TreeViewItem.Text;
        frmModelOrClass.edModelDirectory.Text := Item.ModelDirectory;
        if (frmModelOrClass.ShowModal = mrOk)
          and (frmModelOrClass.edtName.Text <> '') then
        begin
          OldName := Item.TreeViewItem.Text;
          ModelItem := FInputNode.GetChildByName(OldName);
          if ModelItem <> nil then
          begin
            ModelItem.TreeViewItem.Text := frmModelOrClass.edtName.Text;
            if ModelItem.FileSource in [fsArchiveList, fsArchiveListEdited] then
            begin
              ModelItem.FileSource := fsArchiveListEdited;
            end
            else
            begin
              ModelItem.FileSource := fsManual;
            end;
//            ModelItem.FileSource := fsManual;
            InvalidateArchiveTreeView;
            FShouldSaveFile := True;
          end;

          ModelItem := FOutputNode.GetChildByName(OldName);
          if ModelItem <> nil then
          begin
            ModelItem.TreeViewItem.Text := frmModelOrClass.edtName.Text;
            if ModelItem.FileSource in [fsArchiveList, fsArchiveListEdited] then
            begin
              ModelItem.FileSource := fsArchiveListEdited;
            end
            else
            begin
              ModelItem.FileSource := fsManual;
            end;
//            ModelItem.FileSource := fsManual;
            InvalidateArchiveTreeView;
            FShouldSaveFile := True;
          end;

          ModelItem := FAncillaryNode.GetChildByName(OldName);
          if ModelItem <> nil then
          begin
            ModelItem.TreeViewItem.Text := frmModelOrClass.edtName.Text;
            if ModelItem.FileSource in [fsArchiveList, fsArchiveListEdited] then
            begin
              ModelItem.FileSource := fsArchiveListEdited;
            end
            else
            begin
              ModelItem.FileSource := fsManual;
            end;
//            ModelItem.FileSource := fsManual;
            InvalidateArchiveTreeView;
            FShouldSaveFile := True;
          end;
        end;
      end;
    ntFolder:
      begin
        InitializeFormForFolder;
        frmModelOrClass.Caption := 'Rename Folder';
        frmModelOrClass.edtName.Text := Item.TreeViewItem.Text;
        frmModelOrClass.edModelDirectory.Text := Item.ModelDirectory;
        if (frmModelOrClass.ShowModal = mrOk)
          and (frmModelOrClass.edtName.Text <> '') then
        begin
          Item.TreeViewItem.Text := frmModelOrClass.edtName.Text;
          if Item.FileSource in [fsArchiveList, fsArchiveListEdited] then
          begin
            Item.FileSource := fsArchiveListEdited;
          end
          else
          begin
            Item.FileSource := fsManual;
          end;
//          Item.FileSource := fsManual;
          InvalidateArchiveTreeView;
          FShouldSaveFile := True;
        end;
      end;
    ntFile:
      begin
        dlgOpenFiles.Options :=
          dlgOpenFiles.Options - [TOpenOption.ofAllowMultiSelect];
        dlgOpenFiles.FileName := Item.TreeViewItem.Text;
        if dlgOpenFiles.Execute then
        begin
          Item.TreeViewItem.Text := dlgOpenFiles.FileName;
          if Item.FileSource in [fsArchiveList, fsArchiveListEdited] then
          begin
            Item.FileSource := fsArchiveListEdited;
          end
          else
          begin
            Item.FileSource := fsManual;
          end;
//          Item.FileSource := fsManual;
          ParentItem := Item.TreeViewItem.ParentItem.TagObject as TArchiveObject;
          if ParentItem.ModelDirectory <> '' then
          begin
            Item.ModelDirectory := ParentItem.ModelDirectory;
          end;
          InvalidateArchiveTreeView;
          FShouldSaveFile := True;
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmModelArchiver.InitializeFormForModel;
begin
  frmModelOrClass.Caption := 'Add Model';
  frmModelOrClass.lblName.Text := 'Model Name';
  frmModelOrClass.edtName.Text := '';
  frmModelOrClass.lblModelDirectory.Text := 'Model Folder';
  frmModelOrClass.edModelDirectory.Enabled := True;
end;

procedure TfrmModelArchiver.InitializeFormForFolder;
begin
  frmModelOrClass.Caption := 'Add Folder';
  frmModelOrClass.lblName.Text := 'Archive Folder Name';
  frmModelOrClass.edtName.Text := '';
  frmModelOrClass.lblModelDirectory.Text := 'Folder';
  frmModelOrClass.edModelDirectory.Enabled := True;
end;

procedure TfrmModelArchiver.ItemExpanded(Sender: TObject);
//  VScrollBar: TScrollBar;
//  VPosition: Single;
//  ChangeHeight: Boolean;
//  OldHeight: Integer;
begin
  InvalidateArchiveTreeView;

// This only works some of the time.
//  OldHeight := Height;
//  Visible := False;
//  Height := Max(10, Round(APoint.Y-35));
//  Height := OldHeight;
//  Visible := True;

//tvArchive.BeginUpdate;

// this doesn't work.
//  for ItemIndex := 0 to Item.Count - 1 do
//  begin
//    ChildItem := Item[ItemIndex];
//    ChildItem.Repaint;
//  end;

// this doesn't work.
//  for ItemIndex := 0 to Item.Count - 1 do
//  begin
//    ChildItem := Item[ItemIndex];
//    ChildItem.InvalidateRect(RectF(0,0,ChildItem.width,ChildItem.height));
//  end;


// this doesn't work.
//  Item.BeginUpdate;
//  for ItemIndex := 0 to Item.Count - 1 do
//  begin
//    ChildItem := Item[ItemIndex];
//    ChildItem.BeginUpdate;
//    ChildItem.EndUpdate;
//  end;
//  Item.EndUpdate;

// Changing the vertical scrollbar postion works sometimes.
//  VScrollBar := TTreeViewCrack(tvArchive).VScrollBar;
//  if VScrollBar <> nil then
//  begin
//    VPosition := VScrollBar.Value;
//    VScrollBar.Value := 0;
//    VScrollBar.Value := tvArchive.ClientHeight*3;
//    ChangeHeight := VScrollBar.Value < 25;
//    VScrollBar.Value := VPosition;
//    if ChangeHeight then
//    begin
//      OldHeight := Height;
//      Height := 10;
//
//      VPosition := VScrollBar.Value;
//      VScrollBar.Value := 0;
//      VScrollBar.Value := OldHeight*3;
//      VScrollBar.Value := VPosition;
//
//      Application.ProcessMessages;
//      Height := OldHeight;
//    end;
//  end;
end;

procedure TfrmModelArchiver.lbContentsChange(Sender: TObject);
begin
  tcMain.TabIndex := lbContents.ItemIndex;
end;

procedure TfrmModelArchiver.LoadExtensionsFromFile;
var
  ExtensionsXml: TXmlVerySimple;
  ExtIndex: Integer;
  DocNode: TXmlNode;
  ChildNode: TXmlNode;
  ExtensionObject: TExtensionObject;
  Attribute: TXmlAttribute;
  ExtensionFileName: string;
  LowerCaseExt: string;
  RegIndex: Integer;
  LocalExtensionDictionary: TDictionary<string,TExtensionObject>;
//  ExtIndex: Integer;
begin
  SetDefaultExtensions;

  ExtensionFileName := GetExtensionFileName;
  if TFile.Exists(ExtensionFileName) then
  begin
    LocalExtensionDictionary := TDictionary<string,TExtensionObject>.Create;
    try
      for ExtIndex := 0 to FExtensions.Count - 1 do
      begin
        ExtensionObject := FExtensions[ExtIndex];
        if not LocalExtensionDictionary.ContainsKey(LowerCase(ExtensionObject.Extension)) then
        begin
          LocalExtensionDictionary.Add(LowerCase(ExtensionObject.Extension), ExtensionObject);
        end;
      end;


      ExtensionsXml := TXmlVerySimple.Create;
      try
  //      InitializeDescriptionSearchObjects;

        ExtensionsXml.LoadFromFile(ExtensionFileName);
        DocNode := ExtensionsXml.DocumentElement;
        for ExtIndex := 0 to DocNode.ChildNodes.Count - 1 do
        begin
          ChildNode := DocNode.ChildNodes[ExtIndex];
          LowerCaseExt := LowerCase(ChildNode.Text);

          if not LocalExtensionDictionary.TryGetValue(LowerCaseExt, ExtensionObject) then
          begin
            if not FExtensionDictionary.TryGetValue(LowerCaseExt, ExtensionObject) then
            begin
              ExtensionObject := nil;
              for RegIndex := 0 to FRegExpressions.Count - 1 do
              begin
                if AnsiSameText(ChildNode.Text, FRegExpressions[RegIndex]) then
                begin
    //            end;
    //            if TRegEx.IsMatch(ChildNode.Text, FRegExpressions[RegIndex], [roIgnoreCase]) then
    //            begin
                  ExtensionObject := FRegExpressions.Objects[RegIndex] as TExtensionObject;
                  break;
                end;
              end;
              if ExtensionObject = nil then
              begin
                ExtensionObject := TExtensionObject.Create;
                ExtensionObject.Extension := ChildNode.Text;
                Extensions.Add(ExtensionObject);
              end;
            end;
          end;

          Attribute := ChildNode.AttributeList.Find(StrFileType);
          Assert(Attribute <> nil,
            Format('The required attribute "FileType" is missing in the node %s.',
            [ChildNode.Name]));
          if AnsiCompareText(Attribute.Value, StrInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etModelInput;
          end
          else if AnsiCompareText(Attribute.Value, StrOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etModelOutput;
          end
          else if AnsiCompareText(Attribute.Value, StrMODPATHInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etModpathInput;
          end
          else if AnsiCompareText(Attribute.Value, StrMODPATHOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etModpathOutput;
          end
          else if AnsiCompareText(Attribute.Value, StrZoneBudgetInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etZoneBudgetInput;
          end
          else if AnsiCompareText(Attribute.Value, StrZoneBudgetOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etZoneBudgetOutput;
          end
          else if AnsiCompareText(Attribute.Value, StrMT3DMSInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etMt3dmsInput;
          end
          else if AnsiCompareText(Attribute.Value, StrMT3DMSOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etMt3dmsOutput;
          end
        {$IFDEF SwiObsExtractor}
          else if AnsiCompareText(Attribute.Value, StrSwiObsExtInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etSwiObsExtInput;
          end
          else if AnsiCompareText(Attribute.Value, StrSwiObsExtOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etSwiObsExtOutput;
          end
        {$ENDIF}
          else if AnsiCompareText(Attribute.Value, StrOtherInput) = 0 then
          begin
            ExtensionObject.ExtensionType := etOtherInput;
          end
          else if AnsiCompareText(Attribute.Value, StrOtherOutput) = 0 then
          begin
            ExtensionObject.ExtensionType := etOtherOutput;
          end
          else if AnsiCompareText(Attribute.Value, StrAncillary) = 0 then
          begin
            ExtensionObject.ExtensionType := etAncillary;
          end
          else
          begin
            Assert(False);
          end;

          Attribute := ChildNode.AttributeList.Find(StrDescription);
          if Attribute <> nil then
          begin
            ExtensionObject.Description := Attribute.Value;
          end;
        end;
      finally
        ExtensionsXml.Free;
      end;
    finally
      LocalExtensionDictionary.Free;
    end;
    Extensions.SortRecords;
  end;
end;

procedure TfrmModelArchiver.lstArchivesDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  FileIndex: Integer;
  AFile: string;
  Files: TStringList;
begin
  Files := TStringList.Create;
  try
    Files.Duplicates := dupIgnore;
    Files.CaseSensitive := False;
    Files.Sorted := True;
    Files.Assign(lstArchives.Items);
    for FileIndex := 0 to Length(Data.Files) - 1 do
    begin
      AFile := Data.Files[FileIndex];
      if (LowerCase(ExtractFileExt(AFile)) = '.axml')
        and TFile.Exists(AFile) then
      begin
        Files.Add(AFile)
      end;
    end;
    if Files.Count <> lstArchives.Items.Count then
    begin
      lstArchives.BeginUpdate;
      try
        lstArchives.Items.Assign(Files);
      finally
        lstArchives.EndUpdate;
      end;
      FShouldSaveFile := True;
      SetListBoxItemsSize;
      ShowArchiveStructure;
    end;
  finally
    Files.Free
  end;

end;

procedure TfrmModelArchiver.lstArchivesMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
begin
  FSelectedList := lstArchives.ItemByPoint(X, Y);
end;

procedure TfrmModelArchiver.lstArchivesMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Single);
var
  AnItem: TListBoxItem;
begin
  AnItem := lstArchives.ItemByPoint(X, Y);
  if AnItem = nil then
  begin
    lstArchives.Hint := ''
  end
  else
  begin
    if lstArchives.Hint <> AnItem.Text then
    begin
      Application.CancelHint;
      lstArchives.Hint := AnItem.Text;
    end;
//    lstArchives.ShowHint := False;
//    lstArchives.ShowHint := True;
  end;
end;

procedure TfrmModelArchiver.lstArchivesMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  NewItem: TListBoxItem;
begin
  NewItem := lstArchives.ItemByPoint(X, Y);
  if (FSelectedList <> nil) and (NewItem <> nil)
    and (FSelectedList <> NewItem) then
  begin
    FSelectedList.Index := NewItem.Index;
    ShowArchiveStructure;
    FShouldSaveFile := True;
  end;
end;

procedure TfrmModelArchiver.MemoDescriptionChange(Sender: TObject);
begin
  if (not FSelectingItem) and (FCurrentItem <> nil) then
  begin
    if FCurrentItem.Description <> Trim(MemoDescription.Text) then
    begin
      FCurrentItem.Description := Trim(MemoDescription.Text);
      if FCurrentItem.FileSource in [fsArchiveList, fsArchiveListEdited] then
      begin
        FCurrentItem.FileSource := fsArchiveListEdited;
      end
      else
      begin
        FCurrentItem.FileSource := fsManual;
      end;
//      FCurrentItem.FileSource := fsManual;
    end;

  end;
end;

procedure TfrmModelArchiver.LoadIniFile;
var
  DoiURL: string;
  IniFile: TMemIniFile;
  FileUrlRoot: string;
begin
  IniFile := TMemIniFile.Create(GetIniFileName);
  try
    DoiURL := IniFile.ReadString(StrURLs, StrDOICreate,
      'https://www1.usgs.gov/csas/doi/');
    edDoi1.Text := DoiURL;
    edDoi2.Text := DoiURL;
    FileUrlRoot := IniFile.ReadString(StrURLs, StrFileUrlRoot, 'https://water.usgs.gov/GIS/dsdl/gwmodels/');
    edFileUrlRoot.Text := FileUrlRoot;
  finally
    IniFile.Free;
  end;
end;

procedure TfrmModelArchiver.SaveIniFile;
var
  IniFile: TMemIniFile;
  DoiURL: string;
  FileUrlRoot: string;
begin
  IniFile := TMemIniFile.Create(GetIniFileName);
  try
    IniFile.AutoSave := True;
    DoiURL := edDoi1.Text;
    IniFile.WriteString(StrURLs, StrDOICreate, DoiURL);
    FileUrlRoot := edFileUrlRoot.Text;
    IniFile.WriteString(StrURLs, StrFileUrlRoot, FileUrlRoot);
  finally
    IniFile.Free;
  end;
end;

function TfrmModelArchiver.GetIniFileName: string;
var
  HomeDirectory: string;
begin
  HomeDirectory := TPath.GetHomePath;
  Assert(HomeDirectory <> '');
  result := IncludeTrailingPathDelimiter(HomeDirectory);
  result := IncludeTrailingPathDelimiter(result + 'ModelArchiver');
  if not TDirectory.Exists(result) then
  begin
    TDirectory.CreateDirectory(result);
  end;
  result := result + 'ModelArchiver.ini';
end;

function TfrmModelArchiver.GetExtensionFileName: string;
var
  HomeDirectory: string;
begin
  HomeDirectory := TPath.GetHomePath;
  Assert(HomeDirectory <> '');
  result := IncludeTrailingPathDelimiter(HomeDirectory);
  result := IncludeTrailingPathDelimiter(result + 'ModelArchiver');
  if not TDirectory.Exists(result) then
  begin
    TDirectory.CreateDirectory(result);
  end;
  result := result + 'Extensions.xml';
end;

procedure TfrmModelArchiver.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  AskToSaveModel;
end;

procedure TfrmModelArchiver.FormCreate(Sender: TObject);
var
  ItemList: TStringList;
  ItemIndex: Integer;
  AnItem: TListBoxItem;
begin

  FCaptionRoot := 'ModelArchiver ' + KModelArchiverVersion;
  Caption := FCaptionRoot;
  LoadIniFile;
  mniAddModel.Enabled  := True;


  FExtensionDictionary := TDictionary<String, TExtensionObject>.Create;
  FRegExpressions := TStringList.Create;


  FExtensions := TExtensionList.Create;
  LoadExtensionsFromFile;


  Application.ShowHint := true;

  FSelectedNodes := TArchiveItemList.Create;
  FCopiedNodes := TArchiveItemList.Create;
  FBaseNodes := TArchiveNodeList.Create;
  CreateBaseNodes;

  tvArchive.RealignContent;

  ItemList := TStringList.Create;
  try
    for ItemIndex := 0 to tcMain.TabCount - 1 do
    begin
      ItemList.Add(tcMain.Tabs[ItemIndex].Text)
    end;
    lbContents.BeginUpdate;
    try
      lbContents.Items := ItemList;
      for ItemIndex := 0 to lbContents.Items.Count - 1 do
      begin
        AnItem := lbContents.ListItems[ItemIndex];
        AnItem.TextSettings.Font.Size := 16;
//        if Length(AnItem.Text) >= 12 then
//        begin
//          AnItem.WordWrap := True;
//          AnItem.Size.Height := 35;
////          AnItem.RecalcSize;
          AnItem.Repaint;
//          AnItem.Realign;
//          AnItem.Invalidate;
          AnItem.InvalidateRect(Rect(0, 0, Round(AnItem.Width), Round(AnItem.Height)));
////          AnItem.inv
//        end;
      end;
    finally
      lbContents.EndUpdate;
    end;

    tcMain.TabPosition := TTabPosition.None;
    tcMain.TabIndex := 0;

    memoGettingStarted.TextSettings.Font.Size := 16;
    memoGettingStarted.Repaint;

  finally
    ItemList.Free;
  end;

  FShouldSaveFile := False;
end;

procedure TfrmModelArchiver.FormDestroy(Sender: TObject);
begin
  FBaseNodes.Free;
  FCopiedNodes.Free;
  FSelectedNodes.Free;
  FExtensions.Free;
  FExtensionDictionary.Free;
  FRegExpressions.Free;

end;

procedure TfrmModelArchiver.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if Key = 112 then
  begin
    // F1 key;
    OpenHelpUrl(tcMain.ActiveTab.HelpKeyword);
  end;
end;

procedure TfrmModelArchiver.FormShow(Sender: TObject);
begin
  edReadMe.Width := MemoReadme.Width;
//  lstArchives.Height := pnlArchiveLists.Height - lblArchiveLists.Height
//    - btnOpenFileLists.Position.Y - 8;
end;

procedure TfrmModelArchiver.InitializeDescriptionSearchObjects;
begin
  FillExtensionDictionary(FExtensionDictionary, FRegExpressions);
end;

procedure TfrmModelArchiver.InitializeFormForCategory;
begin
  frmModelOrClass.Caption := 'Add Category';
  frmModelOrClass.lblName.Text := 'Category Name';
  frmModelOrClass.edtName.Text := '';
  frmModelOrClass.edModelDirectory.Enabled := False;
  frmModelOrClass.lblModelDirectory.Text := '';
end;

procedure TfrmModelArchiver.mniOpenClick(Sender: TObject);
var
  ArchiveXml: TXmlVerySimple;
  ChildIndex: Integer;
  AChildNode: TXmlNode;
  ArchiveListIndex: Integer;
  ArchiveListNode: TXmlNode;
  DocNode: TXmlNode;
  procedure AddArchiveItem(ANode: TXmlNode; ParentItem: TArchiveObject; ArchiveListName: string);
  var
    ArchiveItem: TArchiveObject;
    NodeAttribute: TXmlAttribute;
    NodeType: TNodeType;
    ChildIndex: integer;
    FileSource: TFileSource;
    TreeViewItem: TCustomExpandItem;
  begin
    NodeType := ntCategory;

    if AnsiCompareText(ANode.Name, StrCategory) = 0 then
    begin
      NodeType := ntCategory;
    end
    else if ANode.Name = StrModel then
    begin
      NodeType := ntModel;
    end
    else if ANode.Name = StrFileName then
    begin
      NodeType := ntFile;
    end
    else if (ANode.Name = StrArchiveRoot)then
    begin
      NodeType := ntArchiveRoot;
    end
    else if  (ANode.Name = StrUncompressedCategor)then
    begin
      NodeType := ntCategoryUncompressed;
    end
    else if (ANode.Name = StrCompressedCategory) then
    begin
      NodeType := ntCategoryCompressed;
    end
    else if (ANode.Name = StrFolder) then
    begin
      NodeType := ntFolder;
    end
    else
    begin
      Assert(False);
    end;

    NodeAttribute := ANode.AttributeList.Find(StrFileSource);
    FileSource := fsUnknown;
    if NodeAttribute <> nil then
    begin
      if NodeAttribute.Value = StrUnknown then
      begin
        FileSource := fsUnknown;
      end
      else if NodeAttribute.Value = StrManual then
      begin
        FileSource := fsManual;
      end
      else if NodeAttribute.Value = StrArchiveList then
      begin
        FileSource := fsArchiveList;
      end
      else if NodeAttribute.Value = StrArchiveListEd then
      begin
        FileSource := fsArchiveListEdited;
      end
      else if NodeAttribute.Value = StrArchiveListDeleted then
      begin
        FileSource := fsArchiveListDeleted;
      end
      else
      begin
        Assert(False);
      end;
    end;

    if ParentItem = nil then
    begin
      ArchiveItem := GetChildByName(ANode.Text);
    end
    else
    begin
      ArchiveItem := ParentItem.GetChildByName(ANode.Text)
    end;
    if ArchiveItem = nil then
    begin
      if ParentItem = nil then
      begin
        tvArchive.BeginUpdate;
      end
      else
      begin
        ParentItem.TreeViewItem.BeginUpdate;
      end;
      TreeViewItem := TCustomExpandItem.Create(self);
      TreeViewItem.OnMouseEnter := SetTreeViewHint;
      TreeViewItem.OnMouseLeave := EraseTreeViewHint;
      TreeViewItem.OnChangeExpanded := ItemExpanded;
      TreeViewItem.OnDragDrop := tvArchiveDragDrop;

      ArchiveItem := TArchiveObject.Create(TreeViewItem);
      TreeViewItem.OnPaint := PaintItem;
      if NodeType = ntFile then
      begin
        ArchiveItem.TreeViewItem.Text := ExpandFileName(ANode.Text);
      end
      else
      begin
        ArchiveItem.TreeViewItem.Text := ANode.Text;
      end;
      ArchiveItem.NodeType := NodeType;
      ArchiveItem.FileSource := FileSource;

      if FileSource in [fsArchiveList, fsArchiveListEdited, fsArchiveListDeleted] then
      begin
        ArchiveItem.ArchiveList := ArchiveListName;
      end;
      if ParentItem = nil then
      begin
        TreeViewItem.Parent := tvArchive;
        tvArchive.EndUpdate
      end
      else
      begin
        TreeViewItem.Parent := ParentItem.TreeViewItem;
        ParentItem.TreeViewItem.EndUpdate;
      end;
    end;

    NodeAttribute := ANode.AttributeList.Find(StrModelName);
    if NodeAttribute = nil then
    begin
      ArchiveItem.ModelName := '';
    end
    else
    begin
      ArchiveItem.ModelName := NodeAttribute.Value;
    end;

    NodeAttribute := ANode.AttributeList.Find(StrDescription);
    if NodeAttribute <> nil then
    begin
      ArchiveItem.Description := NodeAttribute.Value;
    end;

    NodeAttribute := ANode.AttributeList.Find(StrModelDirectory);
    if NodeAttribute = nil then
    begin
      ArchiveItem.ModelDirectory := '';
    end
    else
    begin
      ArchiveItem.ModelDirectory := ExpandFileName(NodeAttribute.Value);
    end;

    if ParentItem <> nil then
    begin
      ArchiveItem.TreeViewItem.Parent := ParentItem.TreeViewItem;
    end
    else
    begin
      ArchiveItem.TreeViewItem.Parent := tvArchive;
    end;


    for ChildIndex := 0 to ANode.ChildNodes.Count - 1 do
    begin
      AddArchiveItem(ANode.ChildNodes[ChildIndex], ArchiveItem, '');
    end;
  end;
begin
  AskToSaveModel;
  if dlgOpenFile.Execute then
  begin
    tvArchive.Clear;
    lstArchives.Clear;

    FSelectedNodes.Clear;
    FCopiedNodes.Clear;
    FBaseNodes.Clear;
    CreateBaseNodes;

    tvArchive.RealignContent;

    Caption := dlgOpenFile.FileName + '; ' + FCaptionRoot;

    dlgSaveFile.FileName := dlgOpenFile.FileName;
    SetCurrentDir(ExtractFileDir(dlgSaveFile.FileName));
    tvArchive.BeginUpdate;
    ArchiveXml := TXmlVerySimple.Create;
    try
      ArchiveXml.LoadFromFile(dlgOpenFile.FileName);
      DocNode := ArchiveXml.DocumentElement;
      for ChildIndex := 0 to DocNode.ChildNodes.Count - 1 do
      begin
        AChildNode := DocNode.ChildNodes[ChildIndex];
        if AChildNode.Name = StrArchiveLists then
        begin
          for ArchiveListIndex := 0 to AChildNode.ChildNodes.Count - 1 do
          begin
            ArchiveListNode := AChildNode.ChildNodes[ArchiveListIndex];
            lstArchives.Items.Add(ExpandFileName(ArchiveListNode.Text));
          end;
        end
        else if (AChildNode.Name = StrCategory)
          or (AChildNode.Name = StrArchiveRoot)
          or (AChildNode.Name = StrUncompressedCategor)
          or (AChildNode.Name = StrCompressedCategory) then
        begin
          AddArchiveItem(AChildNode, nil, '');
        end
        else if AChildNode.Name = StrArchiveDirectory then
        begin
          edArchiveDirectory.Text := ExpandFileName(AChildNode.Text);
        end
        else if AChildNode.Name = StrAppendDescriptions then
        begin
          if LowerCase(AChildNode.Text) = 'true' then
          begin
            cbIncludeDescriptions.IsChecked := True;
          end
          else
          begin
            cbIncludeDescriptions.IsChecked := False;
          end;
        end
        else if AChildNode.Name = StrAppendMetadataDescriptions then
        begin
          if LowerCase(AChildNode.Text) = 'true' then
          begin
            cbIncludeFileDescriptions.IsChecked := True;
          end
          else
          begin
            cbIncludeFileDescriptions.IsChecked := False;
          end;
        end;
      end;
    finally
      ArchiveXml.Free;
      tvArchive.EndUpdate;
    end;
    SetListBoxItemsSize;
    ShowArchiveStructure;
    FShouldSaveFile := False;
    tcMainChange(nil);
  end;
end;

procedure TfrmModelArchiver.mniOpenFileListsClick(Sender: TObject);
begin
  if dlgOpenArchiveLists.Execute then
  begin
    lstArchives.BeginUpdate;
    try
      lstArchives.Items.AddStrings(dlgOpenArchiveLists.Files);
    finally
      lstArchives.EndUpdate;
    end;
    FShouldSaveFile := True;
  end;
  SetListBoxItemsSize;
  ShowArchiveStructure;
end;

procedure TfrmModelArchiver.SaveExtensionsToFile;
var
  ExtensionsXml: TXmlVerySimple;
  ExtensionIndex: Integer;
  ExtItem: TExtensionObject;
  ExtensionNode: TXmlNode;
  DocNode: TXmlNode;
begin
  ExtensionsXml := TXmlVerySimple.Create;
  try
    DocNode := ExtensionsXml.AddChild('Model_Extensions');
    Extensions.SortRecords;
    for ExtensionIndex := 0 to Extensions.Count - 1 do
    begin
      ExtItem := Extensions[ExtensionIndex];
      ExtensionNode := DocNode.AddChild('Extension_Item');
      ExtensionNode.Text := ExtItem.Extension;
      case ExtItem.ExtensionType of
        etModelInput:
          ExtensionNode.SetAttribute(StrFileType, StrInput);
        etModelOutput:
          ExtensionNode.SetAttribute(StrFileType, StrOutput);
        etModpathInput:
          ExtensionNode.SetAttribute(StrFileType, StrMODPATHInput);
        etModpathOutput:
          ExtensionNode.SetAttribute(StrFileType, StrMODPATHOutput);
        etZoneBudgetInput:
          ExtensionNode.SetAttribute(StrFileType, StrZoneBudgetInput);
        etZoneBudgetOutput:
          ExtensionNode.SetAttribute(StrFileType, StrZoneBudgetOutput);
        etMt3dmsInput:
          ExtensionNode.SetAttribute(StrFileType, StrMT3DMSInput);
        etMt3dmsOutput:
          ExtensionNode.SetAttribute(StrFileType, StrMT3DMSOutput);
      {$IFDEF SwiObsExtractor}
        etSwiObsExtInput
          ExtensionNode.SetAttribute(StrFileType, StrSwiObsExtInput);
        etSwiObsExtOutput
          ExtensionNode.SetAttribute(StrFileType, StrSwiObsExtOutput);
      {$ENDIF}
        etOtherInput:
          ExtensionNode.SetAttribute(StrFileType, StrOtherInput);
        etOtherOutput:
          ExtensionNode.SetAttribute(StrFileType, StrOtherOutput);
        etAncillary:
          ExtensionNode.SetAttribute(StrFileType, StrAncillary);
      end;
      ExtensionNode.SetAttribute(StrDescription, ExtItem.Description);
    end;
    ExtensionsXml.SaveToFile(GetExtensionFileName);
  finally
    ExtensionsXml.Free;
  end;
end;

procedure TfrmModelArchiver.SaverProgress(Sender: TObject; Fraction: double);
begin
  pbSaveProgress.Value := Fraction * 100;
  Application.ProcessMessages;
end;

procedure TfrmModelArchiver.mniSaveClick(Sender: TObject);
var
  ArchiveXml: TXmlVerySimple;
  NodeIndex: Integer;
  ANode: TArchiveObject;
  XmlNode: TXmlNode;
  ChildNode: TXmlNode;
  FileIndex: Integer;
  DocNode: TXmlNode;
  ADirectory: string;
  ArchiveDir: string;
  procedure AddNode(ANode: TArchiveObject; ParentXmlNode: TXmlNode);
  var
    XmlNode: TXmlNode;
    NodeName: string;
    ChildIndex: Integer;
    ChildNode: TArchiveObject;
  begin
    NodeName := '';
    case ANode.FNodeType of
      ntCategory: NodeName := StrCategory;
      ntModel: NodeName := StrModel;
      ntFolder: NodeName := StrFolder;
      ntFile: NodeName := StrFileName;
      ntArchiveRoot: NodeName := StrArchiveRoot;
      ntCategoryUncompressed: NodeName := StrUncompressedCategor;
      ntCategoryCompressed: NodeName := StrCompressedCategory;
      else Assert(False);
    end;

    if ParentXmlNode = nil then
    begin
      XmlNode := DocNode.AddChild(NodeName)
    end
    else
    begin
      XmlNode := ParentXmlNode.AddChild(NodeName)
    end;

    if ANode.NodeType = ntFile then
    begin
      XmlNode.Text := ExtractRelativePath(dlgSaveFile.FileName, ANode.TreeViewItem.Text);
    end
    else
    begin
      XmlNode.Text := ANode.TreeViewItem.Text;
    end;
    case ANode.FileSource of
      fsUnknown: XmlNode.SetAttribute(StrFileSource, StrUnknown);
      fsManual:  XmlNode.SetAttribute(StrFileSource, StrManual);
      fsArchiveList:  XmlNode.SetAttribute(StrFileSource, StrArchiveList);
      fsArchiveListEdited:  XmlNode.SetAttribute(StrFileSource, StrArchiveListEd);
      fsArchiveListDeleted:  XmlNode.SetAttribute(StrFileSource, StrArchiveListDeleted);
    end;
    if ANode.ModelName <> '' then
    begin
      XmlNode.SetAttribute(StrModelName, ANode.ModelName);
    end;
    XmlNode.SetAttribute(StrDescription, ANode.Description);

    XmlNode.SetAttribute(StrModelDirectory, ExtractRelativePath(dlgSaveFile.FileName, ANode.ModelDirectory));

    for ChildIndex := 0 to ANode.TreeViewItem.Count - 1 do
    begin
      ChildNode := ANode.TreeViewItem.Items[ChildIndex].TagObject as TArchiveObject;
      AddNode(ChildNode, XmlNode);
    end;
  end;
begin
  if dlgSaveFile.FileName <> '' then
  begin
    ADirectory := ExtractFileDir(dlgSaveFile.FileName);
    if TDirectory.Exists(ADirectory) then
    begin
//      SetCurrentDir(ADirectory);
      dlgSaveFile.InitialDir := ADirectory;
    end;
  end;
  if dlgSaveFile.Execute then
  begin
    ArchiveXml := TXmlVerySimple.Create;
    try
      DocNode := ArchiveXml.AddChild(StrArchiveStructure);

      XmlNode := DocNode.AddChild(StrArchiveLists);
      for FileIndex := 0 to lstArchives.Count - 1 do
      begin
        ChildNode := XmlNode.AddChild(StrArchiveListFileNam);
//        ChildNode.Text := lstArchives.Items[FileIndex];
        ChildNode.Text := ExtractRelativePath(dlgSaveFile.FileName, lstArchives.Items[FileIndex]);
        ChildNode.SetAttribute(StrFileSource, StrManual);
      end;

      for NodeIndex := 0 to tvArchive.Count - 1 do
      begin
        ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
        AddNode(ANode, nil);
      end;

      if edArchiveDirectory.Text <> '' then
      begin
        ArchiveDir := ExtractRelativePath(dlgSaveFile.FileName, edArchiveDirectory.Text);
        ChildNode := DocNode.AddChild(StrArchiveDirectory);
        ChildNode.Text := ArchiveDir;
      end;

      ChildNode := DocNode.AddChild(StrAppendDescriptions);
      if cbIncludeDescriptions.IsChecked then
      begin
        ChildNode.Text := 'True';
      end
      else
      begin
        ChildNode.Text := 'False';
      end;

      ChildNode := DocNode.AddChild(StrAppendMetadataDescriptions);
      if cbIncludeFileDescriptions.IsChecked then
      begin
        ChildNode.Text := 'True';
      end
      else
      begin
        ChildNode.Text := 'False';
      end;

      ArchiveXml.SaveToFile(dlgSaveFile.FileName);
    finally
      ArchiveXml.Free;
    end;

    SaveExtensionsToFile;

    FShouldSaveFile := False;
  end;
end;

procedure TfrmModelArchiver.mniSelectModelFilesClick(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Model Files Directory', '', Directory) then
  begin
    frmHelp.Hide;
    frmModelFiles.GetFileData(Directory);
    frmModelFiles.ShowModal;
  end;
end;

procedure TfrmModelArchiver.mniViewDocumentClick(Sender: TObject);
var
  ArchiveObject: TArchiveObject;
  FileName: string;
  Extension: string;
begin
  if tvArchive.Selected <> nil then
  begin
    ArchiveObject := tvArchive.Selected.TagObject as TArchiveObject;
    FileName := tvArchive.Selected.Text;
    if (ArchiveObject.NodeType = ntFile)
      and TFile.Exists(FileName) then
    begin
      with TfrmPreview.Create(self) do
      begin
        Caption := 'Preview ' + FileName;
        Extension := Lowercase(ExtractFileExt(FileName));
        if (Extension = '.png') or (Extension = '.jpg') or (Extension = '.jpeg') then
        begin
          img1.Bitmap.LoadFromFile(FileName);
          tbc1.ActiveTab := tabImage;
        end
        else
        begin
          memoPreview.Lines.LoadFromFile(FileName);
          tbc1.ActiveTab := tabText;
        end;
        Show;
      end;
    end;
  end;
end;

procedure TfrmModelArchiver.AssignDescription(ArchiveFileObject: TArchiveObject;
  out ExtObj: TExtensionObject);
var
  FileName: string;
  Extension: string;
  LowerCaseExtension: string;

  RegIndex: Integer;
  SearchExt: string;
begin
  ExtObj := nil;
  FileName := ArchiveFileObject.TreeViewItem.Text;
  Extension := ExtractFileExt(FileName);
  if Extension = ArchiveExt then
  begin
    FileName := ChangeFileExt(FileName, '');
    Extension := ExtractFileExt(FileName);
  end;
  Extension := ExtractFileExtendedExt(FileName);
  LowerCaseExtension := LowerCase(Extension);
  if FExtensionDictionary.TryGetValue(LowerCaseExtension, ExtObj) then
  begin
    ArchiveFileObject.Description := ExtObj.Description;
  end
  else
  begin
    for RegIndex := 0 to FRegExpressions.Count - 1 do
    begin
      SearchExt := FRegExpressions[RegIndex];
      SearchExt := StringReplace(SearchExt, '*', '',
        [rfReplaceAll, rfIgnoreCase]);
      SearchExt := LowerCase(SearchExt);
      if Pos(SearchExt, LowerCase(Extension)) = 1 then
//      if TRegEx.IsMatch(FileName, FRegExpressions[RegIndex], [roIgnoreCase]) then
      begin
        ExtObj := FRegExpressions.Objects[RegIndex] as TExtensionObject;
        ArchiveFileObject.Description := ExtObj.Description;
      end;
    end;
  end;
end;


procedure TfrmModelArchiver.btn1Click(Sender: TObject);
var
  RootNodes: TArchiveNodeList;
  NodeIndex: Integer;
  ANode: TArchiveObject;
  ArchiveSaver: TArchiveSaver;
  ArchiveDirectory: string;
begin
  ArchiveDirectory := edArchiveDirectory.Text;
  RootNodes := TArchiveNodeList.Create;
  try
    for NodeIndex   := 0 to tvArchive.Count - 1 do
    begin
      ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
      RootNodes.Add(ANode);
    end;
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ArchiveSaver.RootNodes := RootNodes;
      frmArchiveUpdate.UpdateItems :=
        ArchiveSaver.GetUpdateInfo(ArchiveDirectory);
    finally
      ArchiveSaver.Free;
    end;
  finally
    RootNodes.Free;
  end;

  frmArchiveUpdate.DisplayArchive(ArchiveDirectory);
  frmArchiveUpdate.ShowModal;
end;

procedure TfrmModelArchiver.btnMetadataWizardClick(Sender: TObject);
begin
  OpenURL('https://usgs.github.io/fort-pymdwizard/');
end;

procedure TfrmModelArchiver.btnArchiveWebSiteClick(Sender: TObject);
begin
  OpenURL('https://water.usgs.gov/ogw/policy/gw-model/');
  // http://water.usgs.gov/ogw/policy/gw-model/
end;

procedure TfrmModelArchiver.btnBrowseGraphicClick(Sender: TObject);
begin
  odBrowseGraphic.FileName := edBrowseGraphic.Text;
  if odBrowseGraphic.Execute then
  begin
    edBrowseGraphic.Text := odBrowseGraphic.FileName;
  end;
end;

procedure TfrmModelArchiver.btnDoiCreateClick(Sender: TObject);
begin
  OpenUrl(edDoi1.Text);
end;

procedure TfrmModelArchiver.btnEditGeoRefClick(Sender: TObject);
var
  FileName: string;
  GeoRefFile: TStringList;
begin
  FileName := Trim(edModelGeoRef.Text);
  if FileName = '' then
  begin
    btnModelGeoRefClick(nil);
    FileName := Trim(edModelGeoRef.Text);
    if FileName = '' then
    begin
      Beep;
      MessageDlg('You must specify a modelgeoref.txt file name before you can edit it', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;
  end;
  if not TFile.Exists(FileName) then
  begin
    GeoRefFile := TStringList.Create;
    try
      GeoRefFile.Add('# Reference 1');
      GeoRefFile.Add('# Include the reference to the model documentation report and doi.');
      GeoRefFile.Add('');
      GeoRefFile.Add('# Reference 2');
      GeoRefFile.Add('# Include the reference to the model data release and doi.');
      GeoRefFile.Add('# Datum');
      GeoRefFile.Add('');
      GeoRefFile.Add('upper_left  ');
      GeoRefFile.Add('upper_right ');
      GeoRefFile.Add('lower_right ');
      GeoRefFile.Add('lower_left  ');

      GeoRefFile.SaveToFile(FileName);
    finally
      GeoRefFile.Free;
    end;
  end;
  frmHelp.Hide;
  frmReadme.GetFile(edModelGeoRef.Text);
  frmReadme.ShowModal;
end;

procedure TfrmModelArchiver.btnEditMetadataClick(Sender: TObject);
begin
  if Trim(edMetadata.Text) = '' then
  begin
    btnMetadataClick(nil);
    if Trim(edMetadata.Text) = '' then
    begin
      Beep;
      MessageDlg('You must specify a metadata file name before you can edit it', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;
  end;

  frmHelp.Hide;
  frmEditMetadata.EditFile(edMetadata.Text);
  frmEditMetadata.ShowModal;
end;

procedure TfrmModelArchiver.btnEditReadmeClick(Sender: TObject);
var
  FileName: string;
  ReadMeFile: TStringList;
begin
  FileName := Trim(edReadMe.Text);
 if FileName = '' then
  begin
    btnReadMeClick(nil);
    FileName := Trim(edReadMe.Text);
    if FileName = '' then
    begin
      Beep;
      MessageDlg('You must specify a readme.txt file name before you can edit it', TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;
  end;
  if not TFile.Exists(FileName) then
  begin
    ReadMeFile := TStringList.Create;
    try
      ReadMeFile.SaveToFile(FileName);
    finally
      ReadMeFile.Free;
    end;
  end;
  frmHelp.Hide;
  frmReadme.GetFile(edReadMe.Text);
  frmReadme.ShowModal;
end;

procedure TfrmModelArchiver.btnGeoRefShapefileClick(Sender: TObject);
begin
  if odShapefile.Execute then
  begin
    edGeoRefShapefile.Text := odShapefile.FileName;
  end;
end;

procedure TfrmModelArchiver.btnMetadataParserClick(Sender: TObject);
begin
  OpenURL('https://mrdata.usgs.gov/validation/');
end;

procedure TfrmModelArchiver.btnModelGeoRefClick(Sender: TObject);
begin
  odGeoRef.FileName := edModelGeoRef.Text;
  if odGeoRef.Execute then
  begin
    edModelGeoRef.Text := odGeoRef.FileName;
  end;
end;

procedure TfrmModelArchiver.btnMetadataClick(Sender: TObject);
begin
  odMetadata.FileName := edMetadata.Text;
  if odMetadata.Execute then
  begin
    edMetadata.Text := odMetadata.FileName;
  end;
end;

procedure TfrmModelArchiver.btnMetadataDescriptionClick(Sender: TObject);
begin
  OpenUrl('https://water.usgs.gov/ogw/policy/gw-model/modelers-prep-metadata.html');
end;

procedure TfrmModelArchiver.btnMetadataEditorClick(Sender: TObject);
begin
  OpenURL('https://www1.usgs.gov/csas/ome/')
end;

procedure TfrmModelArchiver.btnReadMeClick(Sender: TObject);
begin
  odReadme.FileName := edReadMe.Text;
  if odReadme.Execute then
  begin
    edReadMe.Text := odReadme.FileName;
  end;
end;

procedure TfrmModelArchiver.btnSelectArchiveDirectoryClick(Sender: TObject);
var
  Directory: string;
begin
  if SelectDirectory('Archive Directory', '', Directory) then
  begin
    edArchiveDirectory.Text := Directory;
  end;
end;

procedure TfrmModelArchiver.ShowArchiveStructure;
var
  ArchiveListIndex: Integer;
  ArchiveListName: string;
  FileNames: TStrings;
  NodesToDelete: TObjectList<TTreeViewItem>;
  ArchiveItem: TArchiveObject;
  ItemIndex: Integer;
  ModelDirectory: string;
  CategoryIndex: Integer;
  AnItem: TTreeViewItem;
  ADirectory: string;
  ListOfArchiveLists: TStringList;
  function ChildByText(ParentNode: TArchiveObject; AText: String): TArchiveObject;
  var
    ChildIndex: Integer;
    Item: TTreeViewItem;
  begin
    result := nil;
    if ParentNode = nil then
    begin
      for ChildIndex := 0 to tvArchive.Count - 1 do
      begin
        Item := tvArchive.Items[ChildIndex];
        if CompareText(AText, Item.Text) = 0 then
        begin
          result := Item.TagObject as TArchiveObject;
          Exit;
        end;
      end;
    end
    else
    begin
      for ChildIndex := 0 to ParentNode.TreeViewItem.Count - 1 do
      begin
        Item := ParentNode.TreeViewItem.Items[ChildIndex];
        if CompareText(AText, Item.Text) = 0 then
        begin
          result := Item.TagObject as TArchiveObject;
          Exit;
//        end
//        else if CompareText(ExtractFileName(AText), ExtractFileName(Item.Text)) = 0 then
//        begin
//          result := Item.TagObject as TArchiveObject;
//          if result.NodeType  = ntFile then
//          begin
//            Exit;
//          end
//          else
//          begin
//            result := nil;
//          end;
        end;
      end;
    end;
  end;
  function ChildFileByText(ParentNode: TArchiveObject; AText: String): TArchiveObject;
  begin
    if ExtractFileDrive(AText) = '' then
    begin
      result := ChildByText(ParentNode, ParentNode.ModelDirectory + AText);
    end
    else
    begin
      result := ChildByText(ParentNode, AText);
    end;
  end;

  procedure AddChildNodes(RootNode: TArchiveObject; XmlNode: TXmlNode; ArchiveListName: string);
  var
    ChildIndex: Integer;
    ChildNode: TXmlNode;
    NodeType: string;
    Attr: TXmlAttribute;
    ModelName: string;
    Filetype: string;
    ChildArchiveObject: TArchiveObject;
    TreeViewItem: TCustomExpandItem;
    Splitter: TStringList;
    ModelTypeName: string;
    CategoryString: string;
    ParentObject: TArchiveObject;
    ChildFileObject: TArchiveObject;
    Dummy: TExtensionObject;
  begin
    for ChildIndex := 0 to XmlNode.ChildNodes.Count - 1 do
    begin
      ChildNode := XmlNode.ChildNodes[ChildIndex];

      NodeType := ChildNode.Name;
      Attr := ChildNode.AttributeList.Find('Model_Name');
      if Attr = nil then
      begin
        Attr := XmlNode.AttributeList.Find('Model_Name');
      end;
      Assert(Attr <> nil,
          Format('The required attribute "Model_Name" is missing in the node %s.',
          [ChildNode.Name]));
      ModelName := Attr.Value;
      ModelName := StringReplace(ModelName, ' ', '_',
        [rfReplaceAll, rfIgnoreCase]);

      Attr := ChildNode.AttributeList.Find('FileType');
      if Attr = nil then
      begin
        Attr := XmlNode.AttributeList.Find('FileType');
      end;
      Assert(Attr <> nil,
          Format('The required attribute "FileType" is missing in the node %s.',
          [ChildNode.Name]));
      Filetype := Attr.Value;

      if (Pos('input', LowerCase(Filetype)) > 0)
        or (Pos(StrModelOutputFiles, LowerCase(Filetype)) > 0) then
      begin
        Splitter := TStringList.Create;
        try
          Splitter.Delimiter := ' ';
          Splitter.StrictDelimiter := True;
          Splitter.DelimitedText := Filetype;
          if (Splitter.Count = 2) and (
            (LowerCase(Splitter[1]) = StrModelOutputFiles)
            or (LowerCase(Splitter[1]) = 'input'))
            then
          begin
            ModelTypeName := Splitter[0];
            if (ModelTypeName = 'Model') then
            begin
              ModelTypeName := '';
            end
            else
            begin
              ModelTypeName := '_' + ModelTypeName;
            end;
          end;
        finally
          Splitter.Free;
        end;
        CategoryString := ModelName + ModelTypeName;
        ChildArchiveObject := ChildByText(RootNode, CategoryString);
        if ChildArchiveObject = nil then
        begin
          TreeViewItem := TCustomExpandItem.Create(self);
          TreeViewItem.OnMouseEnter := SetTreeViewHint;
          TreeViewItem.OnMouseLeave := EraseTreeViewHint;
          TreeViewItem.OnChangeExpanded := ItemExpanded;
          TreeViewItem.OnDragDrop := tvArchiveDragDrop;
          ChildArchiveObject := TArchiveObject.Create(TreeViewItem);
          TreeViewItem.OnPaint := PaintItem;
          ChildArchiveObject.TreeViewItem.Parent := RootNode.TreeViewItem;
          ChildArchiveObject.TreeViewItem.Text := CategoryString;
          ChildArchiveObject.ModelName := CategoryString;
          ChildArchiveObject.NodeType := ntModel;
          ChildArchiveObject.FileSource := fsArchiveList;
          ChildArchiveObject.ArchiveList := ArchiveListName;
          ChildArchiveObject.Description := '';
          ChildArchiveObject.ModelDirectory := ModelDirectory;
        end;
        ParentObject := ChildArchiveObject;
      end
      else
      begin
        ParentObject := RootNode;
      end;

      if NodeType = 'FileName' then
      begin

        ChildFileObject := ChildFileByText(ParentObject, ExpandFileName(ChildNode.Text));
        if ChildFileObject = nil then
        begin
          TreeViewItem := TCustomExpandItem.Create(self);
          TreeViewItem.OnMouseEnter := SetTreeViewHint;
          TreeViewItem.OnMouseLeave := EraseTreeViewHint;
          TreeViewItem.OnChangeExpanded := ItemExpanded;
          TreeViewItem.OnDragDrop := tvArchiveDragDrop;
          ChildFileObject := TArchiveObject.Create(TreeViewItem);
          TreeViewItem.OnPaint := PaintItem;
          ChildFileObject.TreeViewItem.Parent := ParentObject.TreeViewItem;

          if TFile.Exists(ChildNode.Text) then
          begin
            ChildFileObject.TreeViewItem.Text := ExpandFileName(ChildNode.Text);
          end
          else
          begin
            ChildFileObject.TreeViewItem.Text := ModelDirectory + ChildNode.Text;
          end;

          ChildFileObject.ModelName := ModelName;
          ChildFileObject.NodeType := ntFile;
          ChildFileObject.FileSource := fsArchiveList;
          ChildFileObject.ArchiveList := ArchiveListName;
          ChildFileObject.ModelDirectory := ModelDirectory;
          AssignDescription(ChildFileObject, Dummy);
        end;
      end;
    end;
  end;
  procedure HandleArchiveList(ArchiveListName: string);
  var
    Xml: TXmlVerySimple;
    SectionIndex: Integer;
    XmlSection: TXmlNode;
    DocNode: TXmlNode;
    Attr: TXmlAttribute;
    TreeViewItemText: string;
    BaseNode: TArchiveObject;
    Filetype: string;
  begin
    if not TFile.Exists(ArchiveListName) then
    begin
      Exit;
    end;
    Xml := TXmlVerySimple.Create;
    try
      Xml.LoadFromFile(ArchiveListName);
      ModelDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(ArchiveListName));
      SetCurrentDir(ModelDirectory);

      DocNode := Xml.DocumentElement;

      for SectionIndex := 0 to DocNode.ChildNodes.Count - 1 do
      begin
        XmlSection := DocNode.ChildNodes[SectionIndex];

        TreeViewItemText := XmlSection.Name;
        Attr := XmlSection.AttributeList.Find('FileType');
        Assert(Attr <> nil,
          Format('The required attribute "FileType" is missing in the node %s.',
          [XmlSection.Name]));
        Filetype := Attr.Value;

        if (Pos('input', LowerCase(Filetype)) > 0) then
        begin
          TreeViewItemText := StrModelInputFiles;
        end
        else if (Pos(StrModelOutputFiles, LowerCase(Filetype)) > 0) then
        begin
          TreeViewItemText := StrModelOutputFiles;
        end;

        BaseNode := ChildByText(nil, TreeViewItemText);
        Assert(BaseNode <> nil);

        AddChildNodes(BaseNode, XmlSection, ArchiveListName);
      end;
    finally
      Xml.Free;
    end;
  end;
  procedure RemoveArchiveListNodes(ParentItem: TArchiveObject; NodeType: TNodeType);
  var
    ChildIndex: Integer;
    ChildItem: TArchiveObject;
  begin
    for ChildIndex:= 0 to ParentItem.TreeViewItem.Count - 1 do
    begin
      ChildItem := ParentItem.TreeViewItem[ChildIndex].TagObject as TArchiveObject;
      if (ChildItem.FNodeType = NodeType) then
      begin
        if (ChildItem.FileSource = fsArchiveList)
          and (ChildItem.TreeViewItem.Count = 0) then
        begin
          NodesToDelete.Add(ChildItem.TreeViewItem);
        end
        else if (ChildItem.FileSource = fsArchiveListDeleted)
          and (ChildItem.TreeViewItem.Count = 0)
          and (ChildItem.ArchiveList <> '') then
        begin
          if ListOfArchiveLists.IndexOf(ChildItem.ArchiveList) < 0 then
          begin
            NodesToDelete.Add(ChildItem.TreeViewItem);
          end;
        end
      end
      else
      begin
        RemoveArchiveListNodes(ChildItem, NodeType);
      end;
    end;
  end;
  procedure SortAnItem(ParentItem: TTreeViewItem);
  var
    ChildIndex: Integer;
  begin
    if (ParentItem.Count > 0)
      and (ParentItem <> FInputNode.TreeViewItem)
      and (ParentItem <> FOutputNode.TreeViewItem) then
    begin
      ParentItem.Sort(
        function (Left, Right: TFmxObject): Integer
        begin
          result := AnsiCompareText((Left as TTreeViewItem).Text,
            (Right as TTreeViewItem).Text);
        end);

      for ChildIndex := 0 to ParentItem.Count - 1 do
      begin
        SortAnItem(ParentItem.Items[ChildIndex]);
      end;
    end;
  end;
begin
  ListOfArchiveLists := TStringList.Create;
  tvArchive.BeginUpdate;
  try
    ListOfArchiveLists.Assign(lstArchives.Items);
    NodesToDelete := TObjectList<TTreeViewItem>.Create;
    try
      for ItemIndex := 0 to tvArchive.Count - 1 do
      begin
        ArchiveItem := tvArchive.Items[ItemIndex].TagObject as TArchiveObject;
        RemoveArchiveListNodes(ArchiveItem, ntFile);
      end;
      NodesToDelete.Clear;
      for ItemIndex := 0 to tvArchive.Count - 1 do
      begin
        ArchiveItem := tvArchive.Items[ItemIndex].TagObject as TArchiveObject;
        RemoveArchiveListNodes(ArchiveItem, ntModel);
      end;
      NodesToDelete.Clear;
      for ItemIndex := 0 to tvArchive.Count - 1 do
      begin
        ArchiveItem := tvArchive.Items[ItemIndex].TagObject as TArchiveObject;
        RemoveArchiveListNodes(ArchiveItem, ntCategory);
      end;
    finally
      NodesToDelete.Free;
    end;

    FileNames := lstArchives.Items;
    InitializeDescriptionSearchObjects;
    ADirectory := GetCurrentDir;
    try
      for ArchiveListIndex := 0 to FileNames.Count - 1 do
      begin
        ArchiveListName := FileNames[ArchiveListIndex];
        HandleArchiveList(ArchiveListName);
      end;
    finally
      SetCurrentDir(ADirectory);
    end;

    for CategoryIndex := 0 to tvArchive.Count - 1 do
    begin
      AnItem := tvArchive.Items[CategoryIndex];
      SortAnItem(AnItem);
    end;
  finally
    tvArchive.EndUpdate;
    ListOfArchiveLists.Free;
  end;
end;

procedure TfrmModelArchiver.SortModels(ModelNamesInDesiredOrder: TStrings);
begin
  tvArchive.BeginUpdate;
  try
    FInputNode.TreeViewItem.IsExpanded := False;
    FInputNode.TreeViewItem.Sort(function (Left, Right: TFmxObject): Integer
      var
        LeftItem: TTreeViewItem;
        RightItem: TTreeViewItem;
      begin
        LeftItem := Left as TTreeViewItem;
        RightItem := Right as TTreeViewItem;
        result := (ModelNamesInDesiredOrder.IndexOf(LeftItem.Text) -
          ModelNamesInDesiredOrder.IndexOf(RightItem.Text));
      end);
    FOutputNode.TreeViewItem.IsExpanded := False;
    FOutputNode.TreeViewItem.Sort(function (Left, Right: TFmxObject): Integer
      var
        LeftItem: TTreeViewItem;
        RightItem: TTreeViewItem;
      begin
        LeftItem := Left as TTreeViewItem;
        RightItem := Right as TTreeViewItem;
        result := (ModelNamesInDesiredOrder.IndexOf(LeftItem.Text) -
          ModelNamesInDesiredOrder.IndexOf(RightItem.Text));
      end);
  finally
    tvArchive.EndUpdate;
    FShouldSaveFile := True;
  end;
end;

procedure TfrmModelArchiver.UpdateMetaDataEdit;
var
  MetaData: TTreeViewItem;
begin
  MetaData := GetMetadataItem;
  if MetaData = nil then
  begin
    edMetadata.Text := '';
  end
  else
  begin
    edMetadata.Text := MetaData.Text;
  end;
end;

procedure TfrmModelArchiver.UpdateReadmeTxt();
var
  ChildNode: TTreeViewItem;
begin
  if FReadMeNode.TreeViewItem.Count = 0 then
  begin
    edReadMe.Text := '';
  end
  else
  begin
    ChildNode := FReadMeNode.TreeViewItem.Items[0];
    edReadMe.Text := ChildNode.Text;
  end;
end;

function GetHelpDirectory: string;
begin
  result := ExtractFileDir(ParamStr(0));
  result := IncludeTrailingPathDelimiter(result) + 'Help';
  result := IncludeTrailingPathDelimiter(result);
end;

procedure OpenHelpUrl(FileName: string);
var
  ContentsName: string;
  URL: string;
begin
  ContentsName := GetHelpDirectory + FileName;
  URL := StringReplace(ContentsName, '\', '/', [rfReplaceAll, rfIgnoreCase]);
  frmHelp.edt1.Text := URL;
  frmHelp.Show;
//  URL := 'file:///' + StringReplace(ContentsName, '\', '/', [rfReplaceAll, rfIgnoreCase]);
//  TMisc.Open(URL);
end;

procedure TfrmModelArchiver.AskToSaveModel;
begin
  if FShouldSaveFile and
    (MessageDlg('Do you want to save your project', TMsgDlgType.mtConfirmation,
    [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
  begin
    mniSaveClick(nil);
  end;
end;

procedure TfrmModelArchiver.MoveNodes(DropArchive: TTreeViewItem; NodeList: TArchiveItemList);
var
  NodeIndex: Integer;
  ParentNodeType: TNodeType;
begin
  for NodeIndex := 0 to NodeList.Count - 1 do
  begin
    FItem := NodeList[NodeIndex];
    ParentNodeType := (DropArchive.TagObject as TArchiveObject).NodeType;
    if (ParentNodeType < FItem.NodeType)
      or ((ParentNodeType = ntModel) and (FItem.NodeType = ntModel)) then
    begin
      FItem.TreeViewItem.Parent := DropArchive;
      if ParentNodeType = ntModel then
      begin
        (DropArchive.TagObject as TArchiveObject).NodeType := ntFolder;
      end;
      if (DropArchive.TagObject as TArchiveObject).ModelDirectory <> '' then
      begin
        FItem.ModelDirectory := (DropArchive.TagObject as TArchiveObject).ModelDirectory;
      end;
    end;
  end;
  FShouldSaveFile := True;
end;

function TfrmModelArchiver.GetDropTarget(DropTarget: TTreeViewItem; var DropArchive: TTreeViewItem): Boolean;
begin
  result := True;
  DropArchive := DropTarget as TTreeViewItem;
  if (DropArchive.TagObject as TArchiveObject).NodeType = ntFile then
  begin
    DropArchive := DropArchive.ParentItem;
    // as TArchiveObject;
    if (DropTarget = nil) or (DropTarget.TagObject = FItem) then
    begin
      result := False;
    end;
  end;
end;

procedure TfrmModelArchiver.InvalidateArchiveTreeView;
var
  ItemHeight: Single;
begin
  // This attempts to force a repaint of newly visible TTreeViewItems.
  // It works for most things but not everything.
  ItemHeight := tvArchive.ItemHeight;
  tvArchive.ItemHeight := ItemHeight * 10;
  tvArchive.ItemHeight := ItemHeight;
  tvArchive.InvalidateContentSize;
end;

procedure TfrmModelArchiver.AddFiles(ParentItem: TArchiveObject; AddedFiles: TStrings;
  SkipCheckFolder: boolean = False);
var
  Question: string;
  MenuItemSender: TMenuItem;
  FileIndex: Integer;
  ChildNode: TArchiveObject;
  TreeViewItem: TCustomExpandItem;
  Dummy: TExtensionObject;
begin
  if AddedFiles.Count > 1 then
  begin
    InitializeDescriptionSearchObjects;

    if (ParentItem = FInputNode) or (ParentItem = FOutputNode) or (ParentItem = FAncillaryNode) then
    begin
      Question := 'Should these files be part of a new model?';
      MenuItemSender := mniAddModel;
    end
    else
    begin
      Question := 'Should these files be part of a new folder?';
      MenuItemSender := mniAddFolder;
    end;
    if (not SkipCheckFolder)
      and (MessageDlg(Question, TMsgDlgType.mtInformation, [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) = mrYes) then
    begin
      FModelNode := nil;
      mniAddModelOrFolder(MenuItemSender, ExtractFileDir(dlgOpenFiles.FileName));
      if FModelNode <> nil then
      begin
        ParentItem := FModelNode;
      end;
    end;
  end;
  for FileIndex := 0 to AddedFiles.Count - 1 do
  begin
    ChildNode := ParentItem.GetChildByName(AddedFiles[FileIndex]) as TArchiveObject;
    if ChildNode = nil then
    begin
      TreeViewItem := TCustomExpandItem.Create(self);
      TreeViewItem.OnMouseEnter := SetTreeViewHint;
      TreeViewItem.OnMouseLeave := EraseTreeViewHint;
      TreeViewItem.OnChangeExpanded := ItemExpanded;
      TreeViewItem.OnDragDrop := tvArchiveDragDrop;
      ChildNode := TArchiveObject.Create(TreeViewItem);
      TreeViewItem.OnPaint := PaintItem;
      TreeViewItem.Parent := ParentItem.TreeViewItem;
      //        ParentItem.TreeViewItem.IsExpanded := True;
      TreeViewItem.Text := AddedFiles[FileIndex];
      if ParentItem.ModelDirectory <> '' then
      begin
        ChildNode.ModelDirectory := ParentItem.ModelDirectory;
      end
      else
      begin
        ChildNode.ModelDirectory := IncludeTrailingPathDelimiter(ExtractFileDir(AddedFiles[FileIndex]));
      end;
      ChildNode.SetNodeType(ntFile);
      ChildNode.FileSource := fsManual;

      AssignDescription(ChildNode, Dummy);
      FShouldSaveFile := True;
    end;
  end;
end;

procedure TfrmModelArchiver.EraseTreeViewHint(Sender: TObject);
begin
  tvArchive.Hint := ''
end;

procedure TfrmModelArchiver.SetTreeViewHint(Sender: TObject);
var
  ArchiveItem: TTreeViewItem;
begin
  ArchiveItem := Sender as TTreeViewItem;
  tvArchive.Hint := ArchiveItem.Text + sLineBreak + ArchiveItem.Hint;
  // Trigger display of hint window.
  MouseMove([], 0, 0);

end;

procedure TfrmModelArchiver.acContentsExecute(Sender: TObject);
//var
//  FileName: string;
begin
  OpenHelpUrl('index.html');
end;

procedure TfrmModelArchiver.acOpenFileListsDirectoryExecute(Sender: TObject);
var
  Directory: string;
  Files: TStringDynArray;
  Index: integer;
begin
  if SelectDirectory('Model Files Directory', '', Directory) then
  begin
    Files := TDirectory.GetFiles(Directory, '*.axml', TSearchOption.soAllDirectories);
    lstArchives.BeginUpdate;
    try
      for Index := 0 to Length(Files) - 1 do
      begin
        lstArchives.Items.Add(Files[Index]);
      end;
    finally
      lstArchives.EndUpdate;
    end;
  end;
  SetListBoxItemsSize;
  ShowArchiveStructure;
end;

procedure TfrmModelArchiver.acUpdatePathsExecute(Sender: TObject);
var
  OldDirectory: string;
  NewDirectory: string;
  ItemIndex: Integer;
  AnItem: string;
  OldDirLowerCase: string;
  OldDirLength: Integer;
  ATreeViewItem: TTreeViewItem;
  procedure HandleTreeViewItem(TVItem: TTreeViewItem);
  var
    ItemIndex: Integer;
    ArchiveObject: TArchiveObject;
    ModDir: string;
    FileName: string;
  begin
    if TVItem.TagObject <> nil then
    begin
      ArchiveObject := TVItem.TagObject as TArchiveObject;
      ModDir := ArchiveObject.ModelDirectory;
      if Pos(OldDirLowerCase, LowerCase(ModDir)) = 1 then
      begin
        ModDir := NewDirectory + Copy(ModDir, OldDirLength+1, MAXINT);
        ArchiveObject.ModelDirectory := ModDir;
      end;
    end;
    FileName := TVItem.Text;
    if Pos(OldDirLowerCase, LowerCase(FileName)) = 1 then
    begin
      FileName := NewDirectory + Copy(FileName, OldDirLength+1, MAXINT);
      TVItem.Text := FileName;
    end;
    for ItemIndex := 0 to TVItem.Count - 1 do
    begin
      HandleTreeViewItem(TVItem.Items[ItemIndex]);
    end;
  end;
begin
  frmUpdatePaths.ShowModal;
  if frmUpdatePaths.ModalResult = mrOK then
  begin
    OldDirectory := IncludeTrailingPathDelimiter(frmUpdatePaths.edOldModelDirectory.Text);
    NewDirectory := IncludeTrailingPathDelimiter(frmUpdatePaths.edNewModelDirectory.Text);
    if not TDirectory.Exists(NewDirectory) then
    begin
      Beep;
      MessageDlg('The new directory does not exist.',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;
    if OldDirectory = '' then
    begin
      Beep;
      MessageDlg('The previous directory has not been specified correctly.',
        TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
      Exit;
    end;

    OldDirLowerCase := LowerCase(OldDirectory);
    OldDirLength := Length(OldDirectory);
    lstArchives.BeginUpdate;
    try
      for ItemIndex := 0 to lstArchives.Count - 1 do
      begin
        AnItem := lstArchives.Items[ItemIndex];
        if Pos(OldDirLowerCase, LowerCase(AnItem)) = 1 then
        begin
          AnItem := NewDirectory + Copy(AnItem, OldDirLength+1, MAXINT);
          lstArchives.Items[ItemIndex] := AnItem;
        end;
      end;
    finally
      lstArchives.EndUpdate;
    end;

    for ItemIndex := 0 to tvArchive.Count - 1 do
    begin
      ATreeViewItem := tvArchive.Items[ItemIndex];
      HandleTreeViewItem(ATreeViewItem);
    end;



    ShowArchiveStructure;
  end;
end;

procedure TfrmModelArchiver.acUpdateReadmeExecute(Sender: TObject);
var
  ArchiveSaver: TArchiveSaver;
//  ModelNodes: TArchiveNodeList;
  ReadmeOriginalFileName: string;
  OutputDirectory: string;
  OutputFileName: string;
begin
  UpdateReadmeTxt;
  ReadmeOriginalFileName := edReadMe.Text;
  if not TFile.Exists(ReadmeOriginalFileName) then
  begin
    Beep;
    MessageDlg('The readme.txt file has not been specified correctly.',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;

  OutputDirectory := edArchiveDirectory.Text;
  if not TDirectory.Exists(OutputDirectory) then
  begin
    Beep;
    MessageDlg('The archive directory has not been specified correctly.',
      TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;

  OutputFileName := IncludeTrailingPathDelimiter(OutputDirectory) + 'readme.txt';
  if TFile.Exists(OutputFileName) and (Sender <> nil) then
  begin
    Beep;
    if (MessageDlg(StrTheReadmetxtFile, TMsgDlgType.mtWarning,
      [TMsgDlgBtn.mbYes, TMsgDlgBtn.mbNo], 0) <> mrYes) then
    begin
      Exit;
    end;
  end;

  TFile.Copy(ReadmeOriginalFileName, OutputFileName, True);

  ArchiveSaver := TArchiveSaver.Create;
  try
    ArchiveSaver.BaseURL := edFileUrlRoot.Text;
    ArchiveSaver.OnProgress := SaverProgress;
    ArchiveSaver.RootNodes := FBaseNodes;
    ArchiveSaver.AppendFileDescriptionsToReadmeFileInArchive(OutputDirectory);
  finally
    ArchiveSaver.Free;
  end;

  if Sender <> nil then
  begin
    ShowMessage('Done');
  end;
end;

function TfrmModelArchiver.AddManualCategory(CategoryName: string): TArchiveObject;
var
  Item: TCustomExpandItem;
begin
  Item := TCustomExpandItem.Create(self);
  Item.OnChangeExpanded := ItemExpanded;
  Item.Parent := tvArchive;

  result := TArchiveObject.Create(Item);
  Item.OnPaint := PaintItem;
  Item.OnDragDrop := tvArchiveDragDrop;
  Item.Text := CategoryName;
  result.SetNodeType(ntCategory);
  result.FileSource := fsManual;
  Item.OnMouseEnter := SetTreeViewHint;
  Item.OnMouseLeave := EraseTreeViewHint;
  FShouldSaveFile := True;
end;

procedure TfrmModelArchiver.UpdateMetaData(DirectoryName: string);
var
  NodeIndex: Integer;
  ANode: TArchiveObject;
  ArchiveSaver: TArchiveSaver;
  RootNodes: TArchiveNodeList;
  CS: IFMXCursorService;
  ACursor: TCursor;
//  SkippedFiles: TStringList;
//  SkippedFileName: string;
begin
  ACursor := crDefault;
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    ACursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;

  RootNodes := TArchiveNodeList.Create;
  try
    for NodeIndex   := 0 to tvArchive.Count - 1 do
    begin
      ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
      RootNodes.Add(ANode);
    end;
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ArchiveSaver.RootNodes := RootNodes;
      ArchiveSaver.AppendMetadataFileDescriptions :=
        cbIncludeFileDescriptions.IsChecked;
      ArchiveSaver.UpdateMetaData(DirectoryName, edMetadata.Text);
    finally
      ArchiveSaver.Free;
    end;
  finally
    RootNodes.Free;
    if Assigned(CS) then
    begin
      CS.SetCursor(ACursor);
    end;
  end;
  Beep;
  ShowMessage('Done');
end;

procedure TfrmModelArchiver.SaveArchive(DirectoryName: string);
var
  NodeIndex: Integer;
  ANode: TArchiveObject;
  ArchiveSaver: TArchiveSaver;
  RootNodes: TArchiveNodeList;
  CS: IFMXCursorService;
  ACursor: TCursor;
  SkippedFiles: TStringList;
  SkippedFileName: string;
begin
  ACursor := crDefault;
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    ACursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;

  RootNodes := TArchiveNodeList.Create;
  try
    for NodeIndex   := 0 to tvArchive.Count - 1 do
    begin
      ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
      RootNodes.Add(ANode);
    end;
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ArchiveSaver.RootNodes := RootNodes;

      ArchiveSaver.AppendFileDescriptions := cbIncludeDescriptions.IsChecked;
      ArchiveSaver.AppendMetadataFileDescriptions := cbIncludeFileDescriptions.IsChecked;

      ArchiveSaver.CreateArchive(DirectoryName);

      if ArchiveSaver.SkippedFiles.Count > 0 then
      begin
        SkippedFiles := TStringList.Create;
        try
          SkippedFiles.Add('"Skipped file" "Archive location" "Zip file"');
          SkippedFiles.AddStrings(ArchiveSaver.SkippedFiles);
          SkippedFileName := ChangeFileExt(DirectoryName, '');
          SkippedFileName := IncludeTrailingPathDelimiter(DirectoryName);
          SkippedFileName := SkippedFileName + 'SkippedFiles.txt';
          SkippedFiles.SaveToFile(SkippedFileName);
        finally
          SkippedFiles.Free;
        end;
        Beep;
        MessageDlg('This program did not save some files to a zip file. A list of such files and where they belong has been saved to '
          + SkippedFileName + '. Use another program to add those files to the archive.',
          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0);
      end;
    finally
      ArchiveSaver.Free;
    end;
  finally
    RootNodes.Free;
    if Assigned(CS) then
    begin
      CS.SetCursor(ACursor);
    end;
  end;
  Beep;
  ShowMessage('Done');
  FShouldSaveFile := True;
end;

procedure TfrmModelArchiver.MoveFilesArchive(DirectoryName: string);
var
  NodeIndex: Integer;
  ANode: TArchiveObject;
  ArchiveSaver: TArchiveSaver;
  RootNodes: TArchiveNodeList;
  CS: IFMXCursorService;
  ACursor: TCursor;
begin
  ACursor := crDefault;
  if TPlatformServices.Current.SupportsPlatformService(IFMXCursorService) then
  begin
    CS := TPlatformServices.Current.GetPlatformService(IFMXCursorService) as IFMXCursorService;
  end;
  if Assigned(CS) then
  begin
    ACursor := CS.GetCursor;
    CS.SetCursor(crHourGlass);
  end;

  RootNodes := TArchiveNodeList.Create;
  try
    for NodeIndex   := 0 to tvArchive.Count - 1 do
    begin
      ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
      RootNodes.Add(ANode);
    end;
    ArchiveSaver := TArchiveSaver.Create;
    try
      ArchiveSaver.BaseURL := edFileUrlRoot.Text;
      ArchiveSaver.OnProgress := SaverProgress;
      ArchiveSaver.RootNodes := RootNodes;
      ArchiveSaver.MoveFilesToArchive(DirectoryName);

    finally
      ArchiveSaver.Free;
    end;
  finally
    RootNodes.Free;
    if Assigned(CS) then
    begin
      CS.SetCursor(ACursor);
    end;
  end;
  Beep;
  ShowMessage('Done');
end;

procedure TfrmModelArchiver.SetDefaultExtensions;
begin
  FExtensions.SetDefaultExtensions;
end;

procedure TfrmModelArchiver.SetListBoxItemsSize;
var
  Index: integer;
  ItemWidth: single;
begin
  ItemWidth := 0.;
  for Index := 0 to lstArchives.Count - 1 do
  begin
    lstArchives.ListItems[Index].TextSettings.Font.Size := 20;
    ItemWidth := Max(0, lstArchives.Canvas.TextWidth(
      lstArchives.ListItems[Index].Text));
  end;
  lstArchives.ItemWidth := ItemWidth;
end;

procedure TfrmModelArchiver.FillExtensionDictionary(ExtensionDictionary:
  System.Generics.Collections.TDictionary<string, TExtensionObject>;
  RegExtensions: TStringList);
var
  DeleteList: System.Generics.Collections.TList<Integer>;
  DeleteIndex: Integer;
  ExtIndex: Integer;
  LowerCaseExt: string;
begin
  ExtensionDictionary.Clear;
  RegExtensions.Clear;

  DeleteList := TList<Integer>.Create;
  try
    for ExtIndex := 0 to Extensions.Count - 1 do
    begin
      LowerCaseExt := LowerCase(Extensions[ExtIndex].Extension);
      if ExtensionDictionary.ContainsKey(LowerCaseExt) then
      begin
        DeleteList.Add(ExtIndex);
      end
      else
      begin
        if Pos('*', LowerCaseExt) > 0 then
        begin
          RegExtensions.AddObject(Extensions[ExtIndex].Extension,Extensions[ExtIndex] )
        end
        else
        begin
          ExtensionDictionary.Add(LowerCaseExt, Extensions[ExtIndex]);
        end;
      end;
    end;
    for DeleteIndex := DeleteList.Count - 1 downto 0 do
    begin
      ExtIndex := DeleteList[DeleteIndex];
      Extensions.Delete(ExtIndex);
    end;
  finally
    DeleteList.Free;
  end;
end;

procedure TfrmModelArchiver.CreateBaseNodes;
var
  ANode: TArchiveObject;
begin
  //  cltpnlHelp.Visible := False;
  tvArchive.BeginUpdate;
  try
    ANode := AddManualCategory('readme.txt');
//    ANode.TreeViewItem.Hint := 'Required file';
    ANode.NodeType := ntArchiveRoot;
    ANode.Description := 'A file that documents the structure of the model archive';
    FReadMeNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory('modelgeoref.txt');
//    ANode.TreeViewItem.Hint := 'Required file';
    ANode.NodeType := ntArchiveRoot;
    ANode.Description := 'ASCII file with the four corners of the model domain';
    FGeoRefNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory(StrBinary);
    ANode.NodeType := ntCategoryCompressed;
    ANode.Description := 'This directory contains the executables used to do the analysis.';
    FBinNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory(StrGeoref);
    ANode.NodeType := ntCategoryCompressed;
    ANode.Description := 'This directory contains a shape file defining the active and inactive areas of the model.';
    FGeoRefDirNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory(StrModelInputFiles);
    ANode.Description := 'This directory contains all the input files for the models and usgs.model.reference files.';
    FInputNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory(StrModelOutputFiles);
    ANode.Description := 'This directory contains all the output files for the models.';
    FOutputNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory('source');
    ANode.Description := 'Source code for the models used to run the model simulations.';
    ANode.NodeType := ntCategoryCompressed;
    FSourceNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory('webrelease');
    ANode.Description := 'FGDC metadata and thumbnail image of maximum model domain';
    ANode.NodeType := ntCategoryUncompressed;
    FWebReleaseNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory(StrAncillaryLC);
    ANode.Description := 'This directory contains ancillary data.';
    ANode.NodeType := ntCategoryCompressed;
    FAncillaryNode := ANode;
    FBaseNodes.Add(ANode);

    ANode := AddManualCategory('nonpublic');
    ANode.Description := 'Non-public data such as PII, proprietary data, or proprietary code (optional)';
    ANode.NodeType := ntCategoryCompressed;
    FNonPublicNode := ANode;
    FBaseNodes.Add(ANode);
  finally
    tvArchive.EndUpdate;
  end;
end;

procedure TfrmModelArchiver.edBrowseGraphicChange(Sender: TObject);
var
  BrowseGraphic: TTreeViewItem;
  BrowseGraphicList: TStringList;
begin
  if edBrowseGraphic.Text = '' then
  begin
    Exit;
  end;
  BrowseGraphic := GetBrowseGraphicItem;
  if BrowseGraphic = nil then
  begin
    BrowseGraphicList := TStringList.Create;
    try
      BrowseGraphicList.Add(edBrowseGraphic.Text);
      AddFiles(FWebReleaseNode, BrowseGraphicList);
    finally
      BrowseGraphicList.Free;
    end;
  end
  else
  begin
    BrowseGraphic.Text := edBrowseGraphic.Text;
  end;
end;

procedure TfrmModelArchiver.edDoi1Change(Sender: TObject);
begin
  if edDoi1.Text <> edDoi2.Text then
  begin
    edDoi2.Text := edDoi1.Text;
    SaveIniFile;
  end;
end;

procedure TfrmModelArchiver.edDoi2Change(Sender: TObject);
begin
  if edDoi1.Text <> edDoi2.Text then
  begin
    edDoi1.Text := edDoi2.Text;
    SaveIniFile;
  end;
end;

procedure TfrmModelArchiver.edFileUrlRootChange(Sender: TObject);
begin
  SaveIniFile;
end;

procedure TfrmModelArchiver.edGeoRefShapefileChange(Sender: TObject);
var
  GeoRefShapeItem: TTreeViewItem;
  GeoRefShapeFiles: TStringList;
//  ChildNode: TTreeViewItem;
  procedure AddShapeFiles;
  var
    Extensions: TStringList;
    AFileName: string;
    ExtensionIndex: Integer;
    ChildItem: TTreeViewItem;
  begin
    Extensions := TStringList.Create;
    try
      Extensions.Add('.shp');
      Extensions.Add('.shx');
      Extensions.Add('.dbf');
      Extensions.Add('.apr');
      Extensions.Add('.sbn');
      Extensions.Add('.sbx');
      Extensions.Add('.fbn');
      Extensions.Add('.fbx');
      Extensions.Add('.ain');
      Extensions.Add('.aih');
      Extensions.Add('.ixs');
      Extensions.Add('.mxs');
      Extensions.Add('.prj');
      Extensions.Add('.atx');
      Extensions.Add('.qix');
      Extensions.Add('.shp.xml');

      AFileName := edGeoRefShapefile.Text;

      GeoRefShapeFiles := TStringList.Create;
      try
        for ExtensionIndex := 0 to Extensions.Count - 1 do
        begin
          AFileName := ChangeFileExt(AFileName, Extensions[ExtensionIndex]);
          if TFile.Exists(AFileName) then
          begin
            GeoRefShapeFiles.Add(AFileName)
          end;
        end;
        While FGeoRefDirNode.TreeViewItem.Count > 0 do
        begin
          ChildItem := FGeoRefDirNode.TreeViewItem[0];
          ChildItem.Free;
        end;
        AddFiles(FGeoRefDirNode, GeoRefShapeFiles, True);
      finally
        GeoRefShapeFiles.Free;
      end;

    finally
      Extensions.Free;
    end;
  end;
begin
  if edGeoRefShapefile.Text = '' then
  begin
    Exit;
  end;
  GeoRefShapeItem := GetShapefileItem;
  if GeoRefShapeItem = nil then
  begin
    AddShapeFiles;
  end
  else
  begin
    if (edGeoRefShapefile.Text <> GeoRefShapeItem.Text)
      and TFile.Exists(edGeoRefShapefile.Text) then
    begin
      AddShapeFiles;
    end;
  end;
end;

procedure TfrmModelArchiver.edModelGeoRefChange(Sender: TObject);
var
  ReadMeFiles: TStringList;
  ChildNode: TTreeViewItem;
begin
  if edModelGeoRef.Text = '' then
  begin
    Exit;
  end;
  if FGeoRefNode.TreeViewItem.Count = 0 then
  begin
    ReadMeFiles := TStringList.Create;
    try
      ReadMeFiles.Add(edModelGeoRef.Text);
      AddFiles(FGeoRefNode, ReadMeFiles);
    finally
      ReadMeFiles.Free;
    end;
  end
  else
  begin
    ChildNode := FGeoRefNode.TreeViewItem.Items[0];
    ChildNode.Text := edModelGeoRef.Text;
  end;
end;

procedure TfrmModelArchiver.edMetadataChange(Sender: TObject);
var
  MetaData: TTreeViewItem;
  MetaDataList: TStringList;
begin
  if edMetadata.Text = '' then
  begin
    Exit;
  end;
  MetaData := GetMetaDataItem;
  if MetaData = nil then
  begin
    MetaDataList := TStringList.Create;
    try
      MetaDataList.Add(edMetadata.Text);
      AddFiles(FWebReleaseNode, MetaDataList);
    finally
      MetaDataList.Free;
    end;
  end
  else
  begin
    MetaData.Text := edMetadata.Text;
  end;
end;

procedure TfrmModelArchiver.edReadMeChange(Sender: TObject);
var
  ReadMeFiles: TStringList;
  ChildNode: TTreeViewItem;
begin
  if edReadMe.Text = '' then
  begin
    Exit;
  end;
  if FReadMeNode.TreeViewItem.Count = 0 then
  begin
    ReadMeFiles := TStringList.Create;
    try
      ReadMeFiles.Add(edReadMe.Text);
      AddFiles(FReadMeNode, ReadMeFiles);
    finally
      ReadMeFiles.Free;
    end;
  end
  else
  begin
    ChildNode := FReadMeNode.TreeViewItem.Items[0];
    ChildNode.Text := edReadMe.Text;
  end;
end;

procedure TfrmModelArchiver.UpdateSelectedNodes;
var
  NodeIndex: Integer;
  ANode: TArchiveObject;
  procedure AddSelectedNodes(ANode: TArchiveObject);
  var
    NodeIndex: Integer;
  begin
    if ANode.TreeViewItem.IsSelected then
    begin
      FSelectedNodes.Add(ANode);
    end
    else
    begin
      for NodeIndex := 0 to ANode.TreeViewItem.Count - 1 do
      begin
        AddSelectedNodes(ANode.TreeViewItem.Items[NodeIndex].TagObject as TArchiveObject);
      end;
    end;
  end;
begin
  FSelectedNodes.Clear;
  for NodeIndex := 0 to tvArchive.Count - 1 do
  begin
    ANode := tvArchive.Items[NodeIndex].TagObject as TArchiveObject;
    AddSelectedNodes(ANode);
  end;
end;

procedure TfrmModelArchiver.PaintItem(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
var
  ArchiveItem: TTreeViewItem;
  ArchiveObject: TArchiveObject;
  procedure DeletedDisplay;
  begin
    Canvas.Fill.Color := TAlphaColorRec.Alpha or TAlphaColor($020202);
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.5);
  end;
  procedure ErrorDisplay;
  begin
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.2);
  end;
  procedure WarningDisplay;
  begin
    Canvas.Fill.Color := TAlphaColorRec.Yellow;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
  end;
  procedure DuplicateDisplay;
  begin
    Canvas.Fill.Color := TAlphaColorRec.Blue;
    Canvas.FillRect(ARect, 0, 0, AllCorners, 0.4);
  end;
  function ChildWarning(Item: TTreeViewItem): boolean;
  var
    ArchiveObject: TArchiveObject;
    ChildIndex: Integer;
  begin
    ArchiveObject := Item.TagObject as TArchiveObject;
    result := (ArchiveObject.Description = '')
      and (ArchiveObject.FileSource <> fsArchiveListDeleted);
    if not result then
    begin
      for ChildIndex := 0 to Item.Count - 1 do
      begin
        result := ChildWarning(Item[ChildIndex]);
        if result then
        begin
          Exit;
        end;
      end;
    end;
  end;
  function ChildError(Item: TTreeViewItem): boolean;
  var
    ArchiveObject: TArchiveObject;
    ChildIndex: Integer;
  begin
    result := False;
    ArchiveObject := Item.TagObject as TArchiveObject;
    if ArchiveObject.NodeType = ntFile then
    begin
      if not TFile.Exists(Item.Text) then
      begin
        result := True;
      end;
    end
    else
    begin
      for ChildIndex := 0 to Item.Count - 1 do
      begin
        result := ChildError(Item[ChildIndex]);
        if result then
        begin
          Exit;
        end;
      end;
    end;
  end;
  function CheckDuplicate(Item: TTreeViewItem): boolean;
  var
    Extension: string;
    AltName: string;
    ParentItem: TTreeViewItem;
    ChildIndex: Integer;
    AChildItem: TTreeViewItem;
    ArchiveObject: TArchiveObject;
    NodeName: string;
    ChildObject: TArchiveObject;
  begin
    result := False;
    ArchiveObject := Item.TagObject as TArchiveObject;
    if ArchiveObject.FileSource = fsArchiveListDeleted then
    begin
      Exit;
    end;
    if ArchiveObject.FNodeType = ntFile then
    begin
      Extension := ExtractFileExt(Item.Text);
      if LowerCase(Extension) = ArchiveExt then
      begin
        AltName := ChangeFileExt(Item.Text, '');
      end
      else
      begin
        AltName := Item.Text + ArchiveExt;
      end;
      AltName := ExtractFileName(AltName);
      NodeName := ExtractFileName(Item.Text);
      ParentItem := Item.ParentItem;
      for ChildIndex := 0 to ParentItem.Count - 1 do
      begin
        AChildItem := ParentItem.Items[ChildIndex];
        ChildObject := AChildItem.TagObject as TArchiveObject;
        if ChildObject.FileSource = fsArchiveListDeleted then
        begin
          Continue;
        end;
        if SameText(ExtractFileName(AChildItem.Text), AltName) then
        begin
          result := True;
          break;
        end;
        if AChildItem = Item then
        begin
          Continue;
        end;
        if SameText(ExtractFileName(AChildItem.Text), NodeName) then
        begin
          result := True;
          break;
        end;
      end;
    end
    else
    begin
      for ChildIndex := 0 to Item.Count - 1 do
      begin
        AChildItem := Item.Items[ChildIndex];
        result := CheckDuplicate(AChildItem);
        if result then
        begin
          Exit;
        end;
      end;
    end;
    if result then
    begin
      DuplicateDisplay
    end;
  end;
begin
  if Sender is TTreeViewItem then
  begin
    ArchiveItem := TTreeViewItem(Sender);
    ArchiveObject := ArchiveItem.TagObject as TArchiveObject;
    if ArchiveObject <> nil then
    begin
      if ArchiveObject.FileSource = fsArchiveListDeleted then
      begin
        DeletedDisplay;
      end
      else
      begin
        if ChildWarning(ArchiveObject.TreeViewItem) then
        begin
          WarningDisplay;
        end;
        if ArchiveObject.NodeType = ntFile then
        begin
          if not TFile.Exists(ArchiveItem.Text) then
          begin
            ErrorDisplay
          end;
        end
        else if ArchiveObject.NodeType = ntArchiveRoot then
        begin
          if ArchiveItem.Count <> 1 then
          begin
            ErrorDisplay
          end;
        end ;
        if ArchiveItem.Count > 0 then
        begin
          if ChildError(ArchiveItem) then
          begin
            ErrorDisplay
          end;
        end;
        if ((ArchiveObject = FGeoRefDirNode) or (ArchiveObject = FWebReleaseNode)
          or (ArchiveObject = FSourceNode) or (ArchiveObject = FBinNode))
          and (ArchiveItem.Count = 0) then
        begin
          ErrorDisplay
        end;
        CheckDuplicate(ArchiveItem);
      end;
    end;
  end;
end;

function TfrmModelArchiver.GetMetadataItem: TTreeViewItem;
var
  AnItem: TTreeViewItem;
  Index: integer;
  Extension: string;
begin
  result := nil;
  for Index := 0 to FWebReleaseNode.TreeViewItem.Count - 1 do
  begin
    AnItem := FWebReleaseNode.TreeViewItem.Items[Index];
    Extension := LowerCase(ExtractFileExt(AnItem.Text));
    if Extension = '.xml' then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

function TfrmModelArchiver.GetShapefileItem: TTreeViewItem;
var
  AnItem: TTreeViewItem;
  Index: integer;
  Extension: string;
begin
  result := nil;
  for Index := 0 to FGeoRefDirNode.TreeViewItem.Count - 1 do
  begin
    AnItem := FGeoRefDirNode.TreeViewItem.Items[Index];
    Extension := LowerCase(ExtractFileExt(AnItem.Text));
    if Extension = '.shp' then
    begin
      result := AnItem;
      Exit;
    end;
  end;
end;

function TfrmModelArchiver.GetBrowseGraphicItem: TTreeViewItem;
var
  GraphicExtensions: TStringList;
  Index: integer;
  AnItem: TTreeViewItem;
  Extension: string;
begin
  result := nil;
  GraphicExtensions := TStringList.Create;
  try
    GraphicExtensions.Add('.jpg');
    GraphicExtensions.Add('.jpeg');
    GraphicExtensions.Add('.png');
    GraphicExtensions.Add('.gif');

    for Index := 0 to FWebReleaseNode.TreeViewItem.Count - 1 do
    begin
      AnItem := FWebReleaseNode.TreeViewItem.Items[Index];
      Extension := LowerCase(ExtractFileExt(AnItem.Text));
      if GraphicExtensions.IndexOf(Extension) >= 0 then
      begin
        result := AnItem;
        Exit;
      end;
    end;

  finally
    GraphicExtensions.Free;
  end;
end;

procedure TfrmModelArchiver.tcMainChange(Sender: TObject);
var
  ChildNode: TTreeViewItem;
  BrowseGraphic: TTreeViewItem;
  GeoRefShapeItem: TTreeViewItem;
begin
  lbContents.ItemIndex := tcMain.TabIndex;
  case TArchiveTabs(tcMain.TabIndex) of
    atStart: ; //
    atReadMe:
      begin
        UpdateReadmeTxt;
      end;
    atGeoRef:
      begin
        if FGeoRefNode.TreeViewItem.Count = 0 then
        begin
          edModelGeoRef.Text := '';
        end
        else
        begin
          ChildNode := FGeoRefNode.TreeViewItem.Items[0];
          edModelGeoRef.Text := ChildNode.Text;
        end;
      end;
    atShapefile:
      begin
        GeoRefShapeItem := GetShapefileItem;
        if GeoRefShapeItem = nil then
        begin
          edGeoRefShapefile.Text := '';
        end
        else
        begin
          edGeoRefShapefile.Text := GeoRefShapeItem.Text;
        end;
      end;
    atGraphic:
      begin
        BrowseGraphic := GetBrowseGraphicItem;
        if BrowseGraphic = nil then
        begin
          edBrowseGraphic.Text := '';
        end
        else
        begin
          edBrowseGraphic.Text := BrowseGraphic.Text;
        end;
      end;
    atMetaData:
      begin
        UpdateMetaDataEdit;
      end;
    atExtensions,
    ArchiveLists,
    atFiles,
    atStructure,
    atCreate: ;
  end;
end;

procedure TfrmModelArchiver.tvArchiveClick(Sender: TObject);
begin
  EnableAddFile;
  EnableAddFolder;
  mniDelete.Enabled := tvArchive.Selected <> nil;
  mniEdit.Enabled := tvArchive.Selected <> nil;
end;

procedure TfrmModelArchiver.tvArchiveDragDrop(Sender: TObject;
  const Data: TDragObject; const Point: TPointF);
var
  ParentItem: TTreeViewItem;
  ArchiveObject: TArchiveObject;
  Files: TStringList;
  FileNameIndex: Integer;
  AFile: string;
  ChildObject: TArchiveObject;
  FileNames: TStringDynArray;
  FileIndex: Integer;
  ChildFileNames: TStringList;
begin
  if (Sender is TTreeViewItem) and (Length(Data.Files) > 0) then
  begin
    ParentItem := TTreeViewItem(Sender);
    tvArchive.Selected := ParentItem;
    ArchiveObject := ParentItem.TagObject as TArchiveObject;
    while ArchiveObject.NodeType in [ntModel, ntFile, ntFolder] do
    begin
      ParentItem := ParentItem.ParentItem;
      ArchiveObject := ParentItem.TagObject as TArchiveObject;
    end;

    Files := TStringList.Create;
    try
      for FileNameIndex := 0 to Length(Data.Files) - 1 do
      begin
        AFile := Data.Files[FileNameIndex];
        if TDirectory.Exists(AFile) then
        begin
          AddChildNode(ArchiveObject, ChildObject, AFile);
          ChildObject.FNodeType := ntFolder;
          ChildObject.TreeViewItem.Text := ExtractFileName(AFile);
          FileNames := TDirectory.GetFiles(AFile);
          ChildFileNames := TStringList.Create;
          try
            for FileIndex := 0 to Length(FileNames) - 1 do
            begin
              if not TDirectory.Exists(FileNames[FileIndex]) then
              begin
                ChildFileNames.Add(FileNames[FileIndex])
              end;
            end;
            AddFiles(ChildObject, ChildFileNames)
          finally
            ChildFileNames.Free;
          end;
        end
        else
        begin
          Files.Add(AFile);
        end;
      end;
      AddFiles(ArchiveObject, Files)
    finally
      Files.Free;
    end;
  end;
end;

procedure TfrmModelArchiver.tvArchiveMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  Item: TTreeViewItem;
  ArchiveItem: TArchiveObject;
begin
  FItem := nil;
  FCurrentItem := nil;
  if Button = TMouseButton.mbLeft then  { drag only if left button pressed }
  begin
    if Sender = tvArchive then
    begin
      Item := tvArchive.ItemByPoint(X, Y);
      if Item <> nil then  { is there an item here? }
      begin
        ArchiveItem := Item.TagObject as TArchiveObject;
        FCurrentItem := ArchiveItem;
//        if ArchiveItem.NodeType <> ntCategory then
        begin
          FItem := ArchiveItem;

        end;
      end;
    end;
  end;
  FPasteItem := FItem;

  FSelectingItem := True;
  try
    if FItem = nil then
    begin
      memoDescription.Enabled := false;
      memoDescription.Lines.Clear;
    end
    else
    begin
      memoDescription.Enabled := True;
      memoDescription.Lines.Text := FItem.Description;
    end;
  finally
    FSelectingItem := False;
  end;
end;

procedure TfrmModelArchiver.tvArchiveMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Single);
var
  DropTarget: TTreeViewItem;
  DropArchive: TTreeViewItem;
//  NodeList: TArchiveItemList;
begin
  if FItem = nil then
  begin
    Exit;
  end;
  if Button = TMouseButton.mbLeft then  { drag only if left button pressed }
  begin
    if Sender = tvArchive then
    begin
      DropTarget := tvArchive.ItemByPoint(X, Y);

      if DropTarget <> nil then
      begin
        Assert(FItem <> nil);
        if DropTarget.ParentItem = FItem.FTreeViewItem.ParentItem then
        begin
          FItem.FTreeViewItem.Index := DropTarget.Index;
        end
        else
        begin
          if (DropTarget.TagObject <> FItem) then  { is there an item here? }
          begin
            if not GetDropTarget(DropTarget, DropArchive) then
            begin
              Exit;
            end;
            UpdateSelectedNodes;
    //        NodeList := FSelectedNodes;
            MoveNodes(DropArchive, FSelectedNodes);
          end;
        end;
        FShouldSaveFile := True;
      end;
      end;
  end;
  FItem := nil;
end;

constructor TCustomExpandItem.Create(AOwner: TComponent);
begin
  inherited;
//  OnPaint := frmModelArchiver.PaintItem;
  Font.Size := 50;
end;

procedure TCustomExpandItem.SetIsExpanded(const Value: boolean);
var
  LWasExpanded: boolean;
begin
  LWasExpanded := IsExpanded;
  inherited;
  if (IsExpanded) AND (LWasExpanded = False) then
    if Assigned(OnChangeExpanded) then
      OnChangeExpanded(self)
    else
  else if Assigned(OnChangeCollapsed) then
    OnChangeCollapsed(self);
end;

  { TArchiveObject }

constructor TArchiveObject.Create(AOwner: TComponent);
begin
  inherited;
  FTreeViewItem := AOwner as TTreeViewItem;
  FTreeViewItem.TagObject := self;
  InvalidateModel;
end;

destructor TArchiveObject.Destroy;
begin
  InvalidateModel;
  inherited;
end;

function TArchiveObject.GetChild(Index: Integer): IArchiveNodeInterface;
begin
  Result := TreeViewItem.Items[Index].TagObject as TArchiveObject;
end;

function TArchiveObject.GetChildByName(AName: string): TArchiveObject;
var
  ItemIndex: Integer;
begin
  result := nil;
  for ItemIndex := 0 to TreeViewItem.Count - 1 do
  begin
    if TreeViewItem.Items[ItemIndex].Text = AName then
    begin
      result := TreeViewItem.Items[ItemIndex].TagObject as TArchiveObject;
    end;
  end;
end;

function TArchiveObject.GetCount: Integer;
begin
  result := TreeViewItem.Count;
end;

function TArchiveObject.GetDescription: string;
begin
  result := FDescription;
end;

function TArchiveObject.GetModelDirectory: string;
begin
  Result := FModelDirectory;
end;

function TArchiveObject.GetNodeText: string;
var
  PNode: IArchiveNodeInterface;
  ParentText: string;
begin
  result := TreeViewItem.Text;
  PNode := GetParentNode;
  if Assigned(PNode) then
  begin
    ParentText := PNode.NodeText;
    if (ParentText = 'model') or (ParentText = 'output') then
    begin
      ParentText := ParentText + '.';
      if Pos(ParentText, result) <> 1 then
      begin
        result := ParentText + result;
      end;
    end;
  end;
end;

function TArchiveObject.GetNodeType: TNodeType;
begin
  if FileSource = fsArchiveListDeleted then
  begin
    result := ntSkip;
  end
  else
  begin
    result := FNodeType;
  end;
end;

function TArchiveObject.GetParentNode: IArchiveNodeInterface;
begin
  if TreeViewItem.ParentItem = nil then
  begin
    result := nil;
  end
  else
  begin
    Result := TreeViewItem.ParentItem.TagObject as TArchiveObject;
  end;
end;

procedure TArchiveObject.InvalidateModel;
begin
  if frmModelArchiver <> nil then
  begin
    frmModelArchiver.FShouldSaveFile := True;
  end;
end;

procedure TArchiveObject.SetDescription(const Value: string);
begin
  FDescription := Value;
  if TreeViewItem <> nil then
  begin
    TreeViewItem.Hint := Value;
  end;
  InvalidateModel;
end;

procedure TArchiveObject.SetFileSource(const Value: TFileSource);
begin
  FFileSource := Value;
  InvalidateModel;
end;

procedure TArchiveObject.SetModelDirectory(const Value: string);
begin
  FModelDirectory := Value;
  InvalidateModel;
end;

procedure TArchiveObject.SetNodeType(const Value: TNodeType);
begin
  FNodeType := Value;
  InvalidateModel;
end;

procedure TArchiveObject.SetOriginalModelName(const Value: string);
begin
  FOriginalModelName := Value;
  InvalidateModel;
end;

initialization
// FourGigaBytes := 1024*1024;
// FourGigaBytes := FourGigaBytes*1024*4;

end.
