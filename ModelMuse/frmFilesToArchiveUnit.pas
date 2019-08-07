unit frmFilesToArchiveUnit;

interface

uses System.UITypes,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, JvExStdCtrls,
  UndoItems, StrUtils, JvExControls, JvLinkLabel, ComCtrls, JvRichEdit,
  Vcl.Menus, System.Generics.Collections;

type
  TfrmFilesToArchive = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    btnArchive: TButton;
    sdArchive: TSaveDialog;
    btnAddFiles: TButton;
    odAddFiles: TOpenDialog;
    JvLinkLabel1: TJvLinkLabel;
    btnArchiveList: TButton;
    dlgSaveArchiveList: TSaveDialog;
    tvArchive: TTreeView;
    pm1: TPopupMenu;
    mniAddFiles: TMenuItem;
    mniDelete: TMenuItem;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure btnArchiveClick(Sender: TObject);
    procedure JvLinkLabel1LinkClick(Sender: TObject; LinkNumber: Integer;
      LinkText, LinkParam: string);
    procedure btnArchiveListClick(Sender: TObject);
    procedure tvArchiveCustomDrawItem(Sender: TCustomTreeView; Node: TTreeNode;
      State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure tvArchiveDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure tvArchiveDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure mniAddFilesClick(Sender: TObject);
    procedure mniDeleteClick(Sender: TObject);
  private
//    FFilesToArchive: TStringList;
    FBinaryFileNode: TTreeNode;
    FAncillaryNode: TTreeNode;
    FModelInputFilesNode: TTreeNode;
    FModelOutputFiles: TTreeNode;
    FModpathInputFiles: TTreeNode;
    FModpathOutputFiles: TTreeNode;
    FZonebudgetInputFiles: TTreeNode;
    FZonebudgetOutputFiles: TTreeNode;
    FMt3dmsInputFiles: TTreeNode;
    FMt3dmsOutputFiles: TTreeNode;
  {$IFDEF SWIObs}
    FSwiObsExtInputFiles: TTreeNode;
    FSwiObsExtOutputFiles: TTreeNode;
  {$ENDIF}
    FRootNodes: TList<TTreeNode>;
    FChildInputFileNodes: TList<TTreeNode>;
    FChildOutputFileNodes: TList<TTreeNode>;
    FChildModpathInputFileNodes: TList<TTreeNode>;
    FChildModpathOutputFileNodes: TList<TTreeNode>;
    FChildZoneBudgetInputFileNodes: TList<TTreeNode>;
    FChildZoneBudgetOutputFileNodes: TList<TTreeNode>;
  {$IFDEF SWIObs}
    FChildSwiObsExtractorInputFileNodes: TList<TTreeNode>;
    FChildSwiObsExtractorOutputFileNodes: TList<TTreeNode>;
  {$ENDIF}
    FChildMt3dmsInputFileNodes: TList<TTreeNode>;
    FChildMt3dmsOutputFileNodes: TList<TTreeNode>;
    procedure GetData;
    procedure SetData;
    procedure AddFilesToTree(FileNames: TStrings; Classification: string;
      var ANode: TTreeNode);
    procedure EnableCreateArchive;
    { Private declarations }
  public
    { Public declarations }
  end;

  TFileLists = class(TObject)
  private
    FAuxilliaryFiles: TStringList;
    FModelInputFiles: TStringList;
    FModelOutputFiles: TStringList;
    FModpathInputFiles: TStringList;
    FModpathOutputFiles: TStringList;
    FZonebudgetInputFiles: TStringList;
    FZonebudgetOutputFiles: TStringList;
    FMt3dmsInputFiles: TStringList;
    FMt3dmsOutputFiles: TStringList;
  {$IFDEF SWIObs}
    FSwiObsExtInputFiles: TStringList;
    FSwiObsExtOutputFiles: TStringList;
  {$ENDIF}
    function SameContents(Strings1, Strings2: TStrings): boolean;
  public
    constructor Create;
    destructor Destroy; override;
    function IsSame(OtherFiles: TFileLists): Boolean;
  end;

  TListOfFileLists = class(TObjectList<TFileLists>)
    function IsSame(OtherFiles: TListOfFileLists): Boolean;
  end;

  TUndoFilesToArchive = class(TCustomUndo)
  private
    FOriginalFiles: TListOfFileLists;
    FNewFiles: TListOfFileLists;
    procedure AssignFilesToModel(Files: TListOfFileLists);
  protected
    function Description: string; override;
  public
    constructor Create(var NewFiles: TListOfFileLists);
    destructor Destroy; override;
    // @name does the command for the first time.
    procedure DoCommand; override;
    // @name undoes the command.
    procedure Undo; override;
    function Changed: boolean;
  end;


implementation

uses frmGoPhastUnit, JvLinkLabelTools, AbExcept, System.IOUtils, System.Contnrs,
  PhastModelUnit, ArchiveNodeInterface, ModelMuseUtilities;

resourcestring
  StrChangedFilesToArc = 'changed files to archive';
  StrModelMuseCanOnlyC = 'ModelMuse can only create archives in the zip form' +
  'at.';
  StrModpathInputFiles = 'Modpath Input Files';
  StrModpathOutputFiles = 'Modpath Output Files';
  StrZonebudgetInputFil = 'Zonebudget Input Files';
  StrZonebudgetOutputFi = 'Zonebudget Output Files';
  StrMT3DMSInput = 'MT3DMS or MT3D-USGS Input Files';
  StrMT3DMSOutput = 'MT3DMS or MT3D-USGS Output Files';
  StrSWIObservationExtr = 'SWI Observation Extractor Input Files';
  StrSWIObservationExtrOut = 'SWI Observation Extractor Output Files';

{$R *.dfm}

procedure TfrmFilesToArchive.btnArchiveClick(Sender: TObject);
begin
  inherited;
  sdArchive.FileName := frmGoPhast.PhastModel.ArchiveName;
  if sdArchive.Execute then
  begin
    try
      frmGoPhast.PhastModel.CreateArchive(sdArchive.FileName);
    except on E: EAbException do
      begin
        Beep;
        MessageDlg(E.message, mtError, [mbOK], 0);
      end;
    end;
  end;
end;

procedure TfrmFilesToArchive.btnArchiveListClick(Sender: TObject);
var
  ArchiveName: string;
begin
  inherited;
  if frmGoPhast.PhastModel.ModelFileName = '' then
  begin
    ArchiveName := GetCurrentDir + '\Archive.axml'
  end
  else
  begin
    ArchiveName := ChangeFileExt(frmGoPhast.PhastModel.ModelFileName, '.axml');
  end;
  dlgSaveArchiveList.FileName := ArchiveName;
  if dlgSaveArchiveList.Execute then
  begin
    frmGoPhast.PhastModel.SaveArchiveList(dlgSaveArchiveList.FileName);
  end;
end;

procedure TfrmFilesToArchive.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmFilesToArchive.EnableCreateArchive;
var
  index: Integer;
  ButtonEnabled: Boolean;
begin
  ButtonEnabled := False;
  for index := 0 to FRootNodes.Count - 1 do
  begin
    ButtonEnabled := FRootNodes[index]. HasChildren;
    if ButtonEnabled then
    begin
      break;
    end;
  end;
  btnArchive.Enabled := ButtonEnabled;
end;

procedure TfrmFilesToArchive.FormCreate(Sender: TObject);
begin
  inherited;
  FRootNodes := TList<TTreeNode>.Create;
  FChildInputFileNodes := TList<TTreeNode>.Create;
  FChildOutputFileNodes := TList<TTreeNode>.Create;
  FChildModpathInputFileNodes := TList<TTreeNode>.Create;
  FChildModpathOutputFileNodes := TList<TTreeNode>.Create;
  FChildZoneBudgetInputFileNodes := TList<TTreeNode>.Create;
  FChildZoneBudgetOutputFileNodes := TList<TTreeNode>.Create;
{$IFDEF SWIObs}
  FChildSwiObsExtractorInputFileNodes := TList<TTreeNode>.Create;
  FChildSwiObsExtractorOutputFileNodes := TList<TTreeNode>.Create;
{$ENDIF}
  FChildMt3dmsInputFileNodes := TList<TTreeNode>.Create;
  FChildMt3dmsOutputFileNodes := TList<TTreeNode>.Create;
//  FFilesToArchive := TStringList.Create;
  GetData;
  EnableCreateArchive;
//  btnArchive.Enabled := FFilesToArchive.Count > 0;
end;

procedure TfrmFilesToArchive.FormDestroy(Sender: TObject);
begin
  inherited;
//  FFilesToArchive.Free;
  FChildOutputFileNodes.Free;
  FChildInputFileNodes.Free;
  FChildModpathInputFileNodes.Free;
  FChildModpathOutputFileNodes.Free;
  FChildZoneBudgetInputFileNodes.Free;
  FChildZoneBudgetOutputFileNodes.Free;
{$IFDEF SWIObs}
  FChildSwiObsExtractorInputFileNodes.Free;
  FChildSwiObsExtractorOutputFileNodes.Free;
{$ENDIF}
  FChildMt3dmsInputFileNodes.Free;
  FChildMt3dmsOutputFileNodes.Free;
  FRootNodes.Free;
end;

procedure TfrmFilesToArchive.GetData;
var
  InputFiles: TStringList;
  ProgramFiles: TStringList;
  ChildIndex: Integer;
  AChildModel: TChildModel;
  ProgramIndex: Integer;
  PosIndex: Integer;
  ChildNode: TTreeNode;
begin
//  FFilesToArchive.Clear;
//  FFilesToArchive.Duplicates := dupIgnore;
//  FFilesToArchive.Sorted := True;
//  if frmGoPhast.PhastModel.ModelFileName <> '' then
//  begin
//    FFilesToArchive.Add(frmGoPhast.PhastModel.ModelFileName);
//  end;
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.FilesToArchive);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ModelInputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ModelOutputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ModpathInputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ModpathOutputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ZonebudgetInputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.ZonebudgetOutputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.Mt3dmsInputFiles);
//  FFilesToArchive.AddStrings(frmGoPhast.PhastModel.Mt3dmsOutputFiles);
//  reFilesToSave.Lines := FFilesToArchive;

  InputFiles := TStringList.Create;
  ProgramFiles := TStringList.Create;
  try
    InputFiles.Duplicates := dupIgnore;
    InputFiles.Sorted := True;

    InputFiles.AddStrings(frmGoPhast.PhastModel.FilesToArchive);
    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      InputFiles.AddStrings(AChildModel.FilesToArchive);
    end;
    AddFilesToTree(InputFiles, StrAncillary, FAncillaryNode);

    ProgramFiles.Duplicates := dupIgnore;
    ProgramFiles.Sorted := True;
    frmGoPhast.PhastModel.AddModelProgramsToList(ProgramFiles);
    InputFiles.Clear;
    InputFiles.AddStrings(frmGoPhast.PhastModel.ModelInputFiles);

    for ProgramIndex := 0 to ProgramFiles.Count - 1 do
    begin
      PosIndex := InputFiles.IndexOf(ProgramFiles[ProgramIndex]);
      if PosIndex >= 0 then
      begin
        InputFiles.Delete(PosIndex);
      end;
    end;
    AddFilesToTree(ProgramFiles, StrBinary, FBinaryFileNode);
    AddFilesToTree(InputFiles, StrModelInputFiles, FModelInputFilesNode);

    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
      InputFiles.Clear;
      AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      InputFiles.AddStrings(AChildModel.ModelInputFiles);
      for ProgramIndex := 0 to ProgramFiles.Count - 1 do
      begin
        PosIndex := InputFiles.IndexOf(ProgramFiles[ProgramIndex]);
        if PosIndex >= 0 then
        begin
          InputFiles.Delete(PosIndex);
        end;
      end;
      AddFilesToTree(InputFiles, AChildModel.ModelName + ' - ' + StrModelInputFiles,
        ChildNode);
      FChildInputFileNodes.Add(ChildNode);
    end;


    frmGoPhast.PhastModel.AddModelProgramsToList(ProgramFiles);

  finally
    InputFiles.Free;
    ProgramFiles.Free;
  end;

//  AddFilesToTree(frmGoPhast.PhastModel.ModelInputFiles, 'Model Input Files',
//    FModelInputFilesNode);
  AddFilesToTree(frmGoPhast.PhastModel.ModelOutputFiles, StrModelOutputFiles,
    FModelOutputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
      AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      AddFilesToTree(AChildModel.ModelOutputFiles,
        AChildModel.ModelName + ' - ' + StrModelOutputFiles, ChildNode);
      FChildOutputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.ModpathInputFiles, StrModpathInputFiles,
    FModpathInputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
      AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      AddFilesToTree(AChildModel.ModpathInputFiles,
        AChildModel.ModelName + ' - ' + StrModpathInputFiles, ChildNode);
      FChildModpathInputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.ModpathOutputFiles, StrModpathOutputFiles,
    FModpathOutputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.ModpathOutputFiles,
      AChildModel.ModelName + ' - ' + StrModpathOutputFiles, ChildNode);
    FChildModpathOutputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.ZonebudgetInputFiles, StrZonebudgetInputFil,
    FZonebudgetInputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.ZonebudgetInputFiles,
      AChildModel.ModelName + ' - ' + StrZonebudgetInputFil, ChildNode);
    FChildZoneBudgetInputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.ZonebudgetOutputFiles, StrZonebudgetOutputFi,
    FZonebudgetOutputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.ZonebudgetOutputFiles,
      AChildModel.ModelName + ' - ' + StrZonebudgetOutputFi, ChildNode);
    FChildZoneBudgetOutputFileNodes.Add(ChildNode);
  end;

{$IFDEF SWIObs}
  AddFilesToTree(frmGoPhast.PhastModel.SwiObsExtractorInputFiles, StrSWIObservationExtr,
    FSwiObsExtInputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.SwiObsExtractorInputFiles,
      AChildModel.ModelName + ' - ' + StrSWIObservationExtr, ChildNode);
    FChildZoneBudgetInputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.SwiObsExtractorOutputFiles, StrSWIObservationExtrOut,
    FSwiObsExtOutputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.SwiObsExtractorOutputFiles,
      AChildModel.ModelName + ' - ' + StrSWIObservationExtrOut, ChildNode);
    FChildSwiObsExtractorInputFileNodes.Add(ChildNode);
  end;
{$ENDIF}

  AddFilesToTree(frmGoPhast.PhastModel.Mt3dmsInputFiles, StrMT3DMSInput,
    FMt3dmsInputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.Mt3dmsInputFiles,
      AChildModel.ModelName + ' - ' + StrMT3DMSInput, ChildNode);
    FChildMt3dmsInputFileNodes.Add(ChildNode);
  end;

  AddFilesToTree(frmGoPhast.PhastModel.Mt3dmsOutputFiles, StrMT3DMSOutput,
    FMt3dmsOutputFiles);
  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AddFilesToTree(AChildModel.Mt3dmsOutputFiles,
      AChildModel.ModelName + ' - ' + StrMT3DMSOutput, ChildNode);
    FChildMt3dmsOutputFileNodes.Add(ChildNode);
  end;
end;

procedure TfrmFilesToArchive.JvLinkLabel1LinkClick(Sender: TObject;
  LinkNumber: Integer; LinkText, LinkParam: string);
begin
  inherited;
//  TWebTools.OpenWebPage('http://water.usgs.gov/admin/memo/GW/gw11.01.html');
//  TWebTools.OpenWebPage('http://water.usgs.gov/admin/memo/GW/gw2015.02.pdf');
  TWebTools.OpenWebPage('https://water.usgs.gov/admin/memo/GW/gw2016.02.pdf');
end;

procedure TfrmFilesToArchive.mniAddFilesClick(Sender: TObject);
var
  RootNode: TTreeNode;
  FileName: string;
  FileIndex: Integer;
begin
  inherited;
  if odAddFiles.Execute then
  begin
    RootNode := tvArchive.Selected;
    if RootNode = nil then
    begin
      RootNode := FAncillaryNode;
    end;
    if FRootNodes.IndexOf(RootNode) < 0 then
    begin
      RootNode := RootNode.Parent;
    end;
    for FileIndex := 0 to odAddFiles.Files.Count - 1 do
    begin
      FileName := odAddFiles.Files[FileIndex];
      tvArchive.Items.AddChild(RootNode, FileName)
    end;
  end;
end;

procedure TfrmFilesToArchive.mniDeleteClick(Sender: TObject);
var
  ANode: TTreeNode;
  NextNode: TTreeNode;
  NodesToDelete: TObjectList;
begin
  inherited;
  NodesToDelete := TObjectList.Create;
  try
    ANode := tvArchive.Items.GetFirstNode;
    while ANode <> nil do
    begin
      NextNode := ANode.GetNext;
      if ANode.Selected and (FRootNodes.IndexOf(ANode) < 0) then
      begin
        NodesToDelete.Add(ANode);
      end;
      ANode := NextNode
    end;
  finally
    NodesToDelete.Free;
  end;
  EnableCreateArchive;
end;

procedure TfrmFilesToArchive.AddFilesToTree(FileNames: TStrings;
  Classification: string; var ANode: TTreeNode);
var
  index: Integer;
begin
  ANode := tvArchive.Items.Add(nil, Classification);
  FRootNodes.Add(ANode);
  for index := 0 to FileNames.Count - 1 do
  begin
    tvArchive.Items.AddChild(ANode, FileNames[index]);
  end;
end;

procedure TfrmFilesToArchive.SetData;
var
  NewFiles: TListOfFileLists;
  Undo2: TUndoFilesToArchive;
  AList: TFileLists;
  ChildIndex: Integer;
//  AChildModel: TChildModel;
  procedure AddNodeTextToStrings(RootNode: TTreeNode; FileNames: TStringList);
  var
    ANode: TTreeNode;
  begin
    ANode := RootNode.getFirstChild;
    FileNames.Sorted := True;
    while ANode <> nil do
    begin
      if FileNames.IndexOf(ANode.Text) < 0 then
      begin
        FileNames.Add(ANode.Text);
      end;
      ANode := ANode.getNextSibling;
    end;
  end;
begin
  NewFiles := TListOfFileLists.Create;
  try
    AList := TFileLists.Create;
    NewFiles.Add(AList);
    AddNodeTextToStrings(FAncillaryNode, AList.FAuxilliaryFiles);
    AddNodeTextToStrings(FModelInputFilesNode, AList.FModelInputFiles);
    AddNodeTextToStrings(FBinaryFileNode, AList.FModelInputFiles);
    AddNodeTextToStrings(FModelOutputFiles, AList.FModelOutputFiles);
    AddNodeTextToStrings(FModpathInputFiles, AList.FModpathInputFiles);
    AddNodeTextToStrings(FModpathOutputFiles, AList.FModpathOutputFiles);
    AddNodeTextToStrings(FZonebudgetInputFiles, AList.FZonebudgetInputFiles);
    AddNodeTextToStrings(FZonebudgetOutputFiles, AList.FZonebudgetOutputFiles);
  {$IFDEF SWIObs}
    AddNodeTextToStrings(FSwiObsExtInputFiles, AList.FSwiObsExtInputFiles);
    AddNodeTextToStrings(FSwiObsExtOutputFiles, AList.FSwiObsExtOutputFiles);
  {$ENDIF}
    AddNodeTextToStrings(FMt3dmsInputFiles, AList.FMt3dmsInputFiles);
    AddNodeTextToStrings(FMt3dmsOutputFiles, AList.FMt3dmsOutputFiles);

    for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
    begin
//      AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
      AList := TFileLists.Create;
      NewFiles.Add(AList);
      AddNodeTextToStrings(FChildInputFileNodes[ChildIndex], AList.FModelInputFiles);
      AddNodeTextToStrings(FChildOutputFileNodes[ChildIndex], AList.FModelOutputFiles);
      AddNodeTextToStrings(FChildModpathInputFileNodes[ChildIndex], AList.FModpathInputFiles);
      AddNodeTextToStrings(FChildModpathOutputFileNodes[ChildIndex], AList.FModpathOutputFiles);
      AddNodeTextToStrings(FChildZoneBudgetInputFileNodes[ChildIndex], AList.FZonebudgetInputFiles);
      AddNodeTextToStrings(FChildZoneBudgetOutputFileNodes[ChildIndex], AList.FZonebudgetOutputFiles);
    {$IFDEF SWIObs}
      AddNodeTextToStrings(FChildSwiObsExtractorInputFileNodes[ChildIndex], AList.FSwiObsExtInputFiles);
      AddNodeTextToStrings(FChildSwiObsExtractorOutputFileNodes[ChildIndex], AList.FZonebudgetOutputFiles);
    {$ENDIF}
      AddNodeTextToStrings(FChildMt3dmsInputFileNodes[ChildIndex], AList.FMt3dmsInputFiles);
      AddNodeTextToStrings(FChildMt3dmsOutputFileNodes[ChildIndex], AList.FMt3dmsOutputFiles);
   end;

    Undo2 := TUndoFilesToArchive.Create(NewFiles);
    try
      if Undo2.Changed then
      begin
        frmGoPhast.UndoStack.Submit(Undo2)
      end
      else
      begin
        Undo2.Free;
      end;
    except
      Undo2.free;
//      raise;
    end;
  finally
    NewFiles.Free;
  end;
end;

procedure TfrmFilesToArchive.tvArchiveCustomDrawItem(Sender: TCustomTreeView;
  Node: TTreeNode; State: TCustomDrawState; var DefaultDraw: Boolean);
var
  FileName: string;
begin
  inherited;
  if FRootNodes.IndexOf(Node) < 0  then
  begin
    FileName := Node.Text;

    if not TPath.DriveExists(FileName) then
    begin
      tvArchive.Canvas.Brush.Color := clRed;
    end
    else if not TFile.Exists(FileName) then
    begin
      tvArchive.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmFilesToArchive.tvArchiveDragDrop(Sender, Source: TObject; X,
  Y: Integer);
var
  Dst: TTreeNode;
  ANode: TTreeNode;
  NodesToMove: TList<TTreeNode>;
  NodeIndex: Integer;
begin
//  Src := tvArchive.Selected;
  Dst := tvArchive.GetNodeAt(X,Y);
  if FRootNodes.IndexOf(Dst) < 0 then
  begin
    Dst := Dst.Parent
  end;
  NodesToMove := TList<TTreeNode>.Create;
  try
    ANode := tvArchive.Items.GetFirstNode;
    while ANode <> nil do
    begin
      if ANode.Selected and (Self.FRootNodes.IndexOf(ANode) < 0) and
        (ANode.Parent <> Dst) then
      begin
        NodesToMove.Add(ANode);
      end;
      ANode := ANode.GetNext;
    end;
    for NodeIndex := 0 to NodesToMove.Count - 1 do
    begin
      NodesToMove[NodeIndex].MoveTo(Dst, naAddChild);
    end;
  finally
    NodesToMove.Free;
  end;

//  Src.MoveTo(Dst, naAddChild);
end;

procedure TfrmFilesToArchive.tvArchiveDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
var
  Src, Dst: TTreeNode;
begin
  Src := tvArchive.Selected;
  Dst := tvArchive.GetNodeAt(X,Y);
  Accept := (Sender = Source) and Assigned(Dst) and (Src<>Dst)
    and (FRootNodes.IndexOf(Src) < 0);
end;

{ TFileLists }

constructor TFileLists.Create;
begin
  FAuxilliaryFiles := TStringList.Create;
  FModelInputFiles := TStringList.Create;
  FModelOutputFiles := TStringList.Create;
  FModpathInputFiles := TStringList.Create;
  FModpathOutputFiles := TStringList.Create;
  FZonebudgetInputFiles := TStringList.Create;
  FZonebudgetOutputFiles := TStringList.Create;
{$IFDEF SWIObs}
  FSwiObsExtInputFiles := TStringList.Create;
  FSwiObsExtOutputFiles := TStringList.Create;
{$ENDIF}
  FMt3dmsInputFiles := TStringList.Create;
  FMt3dmsOutputFiles := TStringList.Create;
end;

destructor TFileLists.Destroy;
begin
  FAuxilliaryFiles.Free;
  FModelInputFiles.Free;
  FModelOutputFiles.Free;
  FModpathInputFiles.Free;
  FModpathOutputFiles.Free;
  FZonebudgetInputFiles.Free;
  FZonebudgetOutputFiles.Free;
{$IFDEF SWIObs}
  FSwiObsExtInputFiles.Free;
  FSwiObsExtOutputFiles.Free;
{$ENDIF}
  FMt3dmsInputFiles.Free;
  FMt3dmsOutputFiles.Free;
  inherited;
end;

function TFileLists.IsSame(OtherFiles: TFileLists): Boolean;
begin
  result := SameContents(FAuxilliaryFiles, OtherFiles.FAuxilliaryFiles)
    and SameContents(FModelInputFiles, OtherFiles.FModelInputFiles)
    and SameContents(FModelOutputFiles, OtherFiles.FModelOutputFiles)
    and SameContents(FModpathInputFiles, OtherFiles.FModpathInputFiles)
    and SameContents(FModpathOutputFiles, OtherFiles.FModpathOutputFiles)
    and SameContents(FZonebudgetInputFiles, OtherFiles.FZonebudgetInputFiles)
    and SameContents(FZonebudgetOutputFiles, OtherFiles.FZonebudgetOutputFiles)
  {$IFDEF SWIObs}
    and SameContents(FSwiObsExtInputFiles, OtherFiles.FSwiObsExtInputFiles)
    and SameContents(FSwiObsExtOutputFiles, OtherFiles.FSwiObsExtOutputFiles)
  {$ENDIF}
    and SameContents(FMt3dmsInputFiles, OtherFiles.FMt3dmsInputFiles)
    and SameContents(FMt3dmsOutputFiles, OtherFiles.FMt3dmsOutputFiles)
end;

function TFileLists.SameContents(Strings1, Strings2: TStrings): boolean;
var
  index: Integer;
begin
  result := Strings1.Count = Strings2.Count;
  if result then
  begin
    for index := 0 to Strings1.Count - 1 do
    begin
      result := Strings1[index] = Strings2[index];
      if not Result then
      begin
        Exit;
      end;
    end;
  end;
end;

{ TNewUndoFilesToArchive }

procedure TUndoFilesToArchive.AssignFilesToModel(Files: TListOfFileLists);
var
  ChildIndex: Integer;
  AChildModel: TChildModel;
  AList: TFileLists;
begin
  Assert(Files.Count = frmGoPhast.PhastModel.ChildModels.Count+1);
  AList := Files[0];

  frmGoPhast.PhastModel.FilesToArchive := AList.FAuxilliaryFiles;
  frmGoPhast.PhastModel.ModelInputFiles := AList.FModelInputFiles;
  frmGoPhast.PhastModel.ModelOutputFiles := AList.FModelOutputFiles;
  frmGoPhast.PhastModel.ModpathInputFiles := AList.FModpathInputFiles;
  frmGoPhast.PhastModel.ModpathOutputFiles := AList.FModpathOutputFiles;
  frmGoPhast.PhastModel.ZonebudgetInputFiles := AList.FZonebudgetInputFiles;
  frmGoPhast.PhastModel.ZonebudgetOutputFiles := AList.FZonebudgetOutputFiles;
{$IFDEF SWIObs}
  frmGoPhast.PhastModel.SwiObsExtractorInputFiles := AList.FSwiObsExtInputFiles;
  frmGoPhast.PhastModel.SwiObsExtractorOutputFiles := AList.FSwiObsExtOutputFiles;
{$ENDIF}
  frmGoPhast.PhastModel.Mt3dmsInputFiles := AList.FMt3dmsInputFiles;
  frmGoPhast.PhastModel.Mt3dmsOutputFiles := AList.FMt3dmsOutputFiles;

  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AList := Files[ChildIndex+1];
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AChildModel.FilesToArchive := AList.FAuxilliaryFiles;
    AChildModel.ModelInputFiles := AList.FModelInputFiles;
    AChildModel.ModelOutputFiles := AList.FModelOutputFiles;
    AChildModel.ModpathInputFiles := AList.FModpathInputFiles;
    AChildModel.ModpathOutputFiles := AList.FModpathOutputFiles;
    AChildModel.ZonebudgetInputFiles := AList.FZonebudgetInputFiles;
    AChildModel.ZonebudgetOutputFiles := AList.FZonebudgetOutputFiles;
  {$IFDEF SWIObs}
    AChildModel.SwiObsExtractorInputFiles := AList.FSwiObsExtInputFiles;
    AChildModel.SwiObsExtractorOutputFiles := AList.FSwiObsExtOutputFiles;
  {$ENDIF}
    AChildModel.Mt3dmsInputFiles := AList.FMt3dmsInputFiles;
    AChildModel.Mt3dmsOutputFiles := AList.FMt3dmsOutputFiles;
  end;

end;

function TUndoFilesToArchive.Changed: boolean;
begin
  Result := not FOriginalFiles.IsSame(FNewFiles);
end;

constructor TUndoFilesToArchive.Create(var NewFiles: TListOfFileLists);
var
  ChildIndex: Integer;
  AChildModel: TChildModel;
  AList: TFileLists;
begin
  FOriginalFiles := TListOfFileLists.Create;
  AList := TFileLists.Create;
  FOriginalFiles.Add(AList);
  AList.FAuxilliaryFiles.AddStrings(frmGoPhast.PhastModel.FilesToArchive);
  AList.FModelInputFiles.AddStrings(frmGoPhast.PhastModel.ModelInputFiles);
  AList.FModelOutputFiles.AddStrings(frmGoPhast.PhastModel.ModelOutputFiles);
  AList.FModpathInputFiles.AddStrings(frmGoPhast.PhastModel.ModpathInputFiles);
  AList.FModpathOutputFiles.AddStrings(frmGoPhast.PhastModel.ModpathOutputFiles);
  AList.FZonebudgetInputFiles.AddStrings(frmGoPhast.PhastModel.ZonebudgetInputFiles);
  AList.FZonebudgetOutputFiles.AddStrings(frmGoPhast.PhastModel.ZonebudgetOutputFiles);
{$IFDEF SWIObs}
  AList.FSwiObsExtInputFiles.AddStrings(frmGoPhast.PhastModel.SwiObsExtractorInputFiles);
  AList.FSwiObsExtOutputFiles.AddStrings(frmGoPhast.PhastModel.SwiObsExtractorOutputFiles);
{$ENDIF}
  AList.FMt3dmsInputFiles.AddStrings(frmGoPhast.PhastModel.Mt3dmsInputFiles);
  AList.FMt3dmsOutputFiles.AddStrings(frmGoPhast.PhastModel.Mt3dmsOutputFiles);

  for ChildIndex := 0 to frmGoPhast.PhastModel.ChildModels.Count - 1 do
  begin
    AChildModel := frmGoPhast.PhastModel.ChildModels[ChildIndex].ChildModel;
    AList := TFileLists.Create;
    FOriginalFiles.Add(AList);
    AList.FAuxilliaryFiles.AddStrings(AChildModel.FilesToArchive);
    AList.FModelInputFiles.AddStrings(AChildModel.ModelInputFiles);
    AList.FModelOutputFiles.AddStrings(AChildModel.ModelOutputFiles);
    AList.FModpathInputFiles.AddStrings(AChildModel.ModpathInputFiles);
    AList.FModpathOutputFiles.AddStrings(AChildModel.ModpathOutputFiles);
    AList.FZonebudgetInputFiles.AddStrings(AChildModel.ZonebudgetInputFiles);
    AList.FZonebudgetOutputFiles.AddStrings(AChildModel.ZonebudgetOutputFiles);
  {$IFDEF SWIObs}
    AList.FSwiObsExtInputFiles.AddStrings(AChildModel.SwiObsExtractorInputFiles);
    AList.FSwiObsExtOutputFiles.AddStrings(AChildModel.SwiObsExtractorOutputFiles);
  {$ENDIF}
    AList.FMt3dmsInputFiles.AddStrings(AChildModel.Mt3dmsInputFiles);
    AList.FMt3dmsOutputFiles.AddStrings(AChildModel.Mt3dmsOutputFiles);
  end;

  FNewFiles := NewFiles;
  NewFiles := nil;
end;

function TUndoFilesToArchive.Description: string;
begin
  result := StrChangedFilesToArc;
end;

destructor TUndoFilesToArchive.Destroy;
begin
  FOriginalFiles.Free;
  FNewFiles.Free;
  inherited;
end;

procedure TUndoFilesToArchive.DoCommand;
begin
  inherited;
  AssignFilesToModel(FNewFiles);
end;

procedure TUndoFilesToArchive.Undo;
begin
  inherited;
  AssignFilesToModel(FOriginalFiles);
end;

{ TListOfFileLists }

function TListOfFileLists.IsSame(OtherFiles: TListOfFileLists): Boolean;
var
  index: Integer;
begin
  result := Count = OtherFiles.Count;
  if result then
  begin
    for index := 0 to Count - 1 do
    begin
      Result := Items[index].IsSame(OtherFiles.Items[index]);
      if not Result then
      begin
        Exit;
      end;
    end;
  end;

end;

end.
