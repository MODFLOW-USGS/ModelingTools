unit frmErrorsAndWarningsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ExtCtrls, VirtualTrees,
  GoPhastTypes, Menus, ScreenObjectUnit;

type
  TErrMessages = class(TStringList)
  private
    FObjects: TList;
  public
    constructor Create;
    destructor Destroy; override;
    property ObjectList: TList read FObjects write FObjects;
    function AddObject(const S: string; AObject: TObject): Integer; override;
  end;

  TModelMessages = class(TObject)
  private
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    FModel: TBaseModel;
    FChildNodes: TList;
    FNode: PVirtualNode;
    FMessages: TErrMessages;
  public
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    Constructor Create(Model: TBaseModel);
    Destructor Destroy; override;
  end;

  TModelMessageList = class(TObject)
  private
    FList: TList;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    function GetModelMessages(Model: TBaseModel): TModelMessages;
  public
    Constructor Create;
    Destructor Destroy; override;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    property ModelMessages[Model: TBaseModel]: TModelMessages
      read GetModelMessages;
    procedure Clear;
  end;

  // @name is used to display error and warning messages that are generated
  // during export of a model.
  TfrmErrorsAndWarnings = class(TfrmCustomGoPhast)
    // @name is the TPanel at the bottom of the TForm.
    pnlBottom: TPanel;
    // @name closes the @classname.
    btnClose: TBitBtn;
    // @name is used to display the error and warning messages in a tree view.
    vstWarningsAndErrors: TVirtualStringTree;
    // @name is the Help button.
    btnHelp: TBitBtn;
    Timer1: TTimer;
    btnSave: TButton;
    sdSaveFileDlg: TSaveDialog;
    btnClear: TButton;
    btnCopy: TButton;
    pmSelectEdit: TPopupMenu;
    miSelect: TMenuItem;
    miGoto: TMenuItem;
    miEdit: TMenuItem;
    miIgnorethistypeoferrororwarning1: TMenuItem;
    // @name creates the lists that hold the errors and warnings.
    // It also initializes the size of the record associated with
    // nodes in @link(vstWarningsAndErrors).
    procedure FormCreate(Sender: TObject); override;
    // @name destroys the lists that hold the errors and warnings
    // and other data associated with the @classname..
    procedure FormDestroy(Sender: TObject); override;
    // @name determines the text to display in each cell.
    procedure vstWarningsAndErrorsGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
    // @name initializes all the nodes so they can be multi-line.
    procedure vstWarningsAndErrorsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    // @name determines the height of the current node.
    procedure vstWarningsAndErrorsMeasureItem(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; var NodeHeight: Integer);
    procedure FormResize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnCopyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure vstWarningsAndErrorsContextPopup(Sender: TObject;
      MousePos: TPoint; var Handled: Boolean);
    procedure miSelectClick(Sender: TObject);
    procedure miEditClick(Sender: TObject);
    procedure miGotoClick(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure miIgnorethistypeoferrororwarning1Click(Sender: TObject);
  private
    // @name is a list of the PVirtualNodes beneath @link(FErrorNode).
    FErrorChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the error messages. @link(FErrorModels) is associated with
    // @name through the data record associated with @name.
    FErrorNode: PVirtualNode;
    // @name is a list of the error messages generated during export
    // of a project.  Its Objects property contains lists of the locations
    // to which the error messages apply.
    FErrorModels: TErrMessages;
    FErrorModelMessageList: TModelMessageList;

    // @name is a list of the PVirtualNodes beneath @link(FWarningNode).
    FWarningChildNodes: TList;
    // @name is the node in @link(vstWarningsAndErrors) that has
    // beneath it all the warning messages. @link(FWarningModels) is associated
    // with @name through the data record associated with @name.
    FWarningNode: PVirtualNode;
    // @name is a list of the warning messages generated during export
    // of a project.  Its Objects property contains lists of the cells
    // to which the warning messages apply.
    FWarningModels: TErrMessages;
    FWarningModelMessageList: TModelMessageList;
    FDelayShowing: boolean;
    FShouldShow: Boolean;
    class var FFormTop: Integer;
    class var FFormLeft: Integer;
//    class var FFormTop: Integer;
//    class var FFormLeft: Integer;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is used to add an error or warning to @classname.
    // It first creates RootNode if it does not exist and associates
    // RootList with it.  Then if checks if an error message like
    // TypeOfErrorOrWarning has been created.  If not, it creates a
    // PVirtualNode for it (stored in Children)
    // and associates a TStringList with it.
    // The TStringList is also stored in RootList in its Objects property.
    // Finally, ErrorOrWarning is added to the TStringList and a new
    // PVirtualNode is created.
    procedure AddErrorOrWarning(Model: TBaseModel; RootList: TErrMessages;
      const TypeOfErrorOrWarning, ErrorOrWarning: string;
      var RootNode: PVirtualNode; Children: TList;
      ModelMessageList: TModelMessageList; AnObject: TObject);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveWarningOrErrorGroup(Model: TBaseModel;
      const TypeOfErrorOrWarning: string;
      RootList: TStringList; Children: TList;
      ModelMessageList: TModelMessageList);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure CheckDeleteRootNode(Model: TBaseModel; var Node: PVirtualNode;
      RootList: TStringList; ModelMessageList: TModelMessageList);
    procedure GetErrorsAndWarnings(ErrorsAndWarings: TStringList);

    // @name creates a new root node (PVirtualNode) and associates List
    // with it. @seealso(FErrorNode) @seealso(FWarningNode)
    procedure InitializeRootNode(var Node: PVirtualNode; List: TErrMessages);
    procedure GetSelectedScreenObjects(ScreenObjects: TScreenObjectList);
    procedure SetDelayShowing(const Value: boolean);
    class procedure SetFormLeft(const Value: Integer); static;
    class procedure SetFormTop(const Value: Integer); static;
//    class procedure SetFormTop(const Value: Integer); static;
//    class procedure SetFormLeft(const Value: Integer); static;
    { Private declarations }
  public
    function HasMessages: boolean;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is used to add a new error message to @classname.
    // Root is the type of error, Error is the specific cell to which the
    // error applies.
    Procedure AddError(Model: TBaseModel; const Root, Error: string;
      AnObject: TObject = nil);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    // @name is used to add a new warning message to @classname.
    // Root is the type of warning, Warning is the specific cell to which the
    // warning applies.
    Procedure AddWarning(Model: TBaseModel; const Root, Warning: string;
      AnObject: TObject = nil);
    // @name deletes the warning and error messages and clears
    // @link(vstWarningsAndErrors).
    Procedure Clear;
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveWarningGroup(Model: TBaseModel;
       const TypeOfWarning: string);
    { TODO -cRefactor : Consider replacing Model with an interface. }
    //
    procedure RemoveErrorGroup(Model: TBaseModel; const TypeOfError: string);
    procedure ShowAfterDelay;
    procedure BeginUpdate;
    procedure EndUpdate;
    property DelayShowing: boolean read FDelayShowing write SetDelayShowing;
    procedure Show;
    class property FormTop: Integer read FFormTop write SetFormTop;
    class property FormLeft: Integer read FFormLeft write SetFormLeft;
    { Public declarations }
  end;

function frmErrorsAndWarnings: TfrmErrorsAndWarnings;

procedure FreefrmErrorsAndWarnings;

implementation

uses Math, frmProgressUnit, Clipbrd, contnrs, UndoItemsScreenObjects,
  frmGoPhastUnit, frmGoToUnit;

resourcestring
  StrErrors = 'Errors';
  StrWarnings = 'Warnings';
{$R *.dfm}

var
  FfrmErrorsAndWarnings: TfrmErrorsAndWarnings = nil;
  TextToIgnore: TStringList;

type
  PErrorWarningRec = ^TErrorWarningRec;
  TErrorWarningRec = record
    List: TErrMessages;
  end;

procedure FreefrmErrorsAndWarnings;
begin
  FreeAndNil(FfrmErrorsAndWarnings);
end;

function frmErrorsAndWarnings: TfrmErrorsAndWarnings;
begin
  if FfrmErrorsAndWarnings = nil then
  begin
    FfrmErrorsAndWarnings := TfrmErrorsAndWarnings.Create(nil);
  end;
  FfrmErrorsAndWarnings.Handle;
  result := FfrmErrorsAndWarnings;
end;

{ TfrmErrorsAndWarnings }

procedure TfrmErrorsAndWarnings.AddErrorOrWarning(Model: TBaseModel;
  RootList: TErrMessages;
  const TypeOfErrorOrWarning, ErrorOrWarning: string;
  var RootNode: PVirtualNode; Children: TList;
  ModelMessageList: TModelMessageList; AnObject: TObject);
var
  ChildNode: PVirtualNode;
  RootIndex: integer;
  Data: PErrorWarningRec;
  ErrorMessages: TErrMessages;
  ModelMessages: TModelMessages;
  NewLeft: integer;
begin
  if TextToIgnore.IndexOf(TypeOfErrorOrWarning) >= 0 then
  begin
    Exit;
  end;
  Assert(Model <> nil);
  if RootNode = nil then
  begin
    InitializeRootNode(RootNode, RootList);
  end;

  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode = nil) then
  begin
    vstWarningsAndErrors.ChildCount[RootNode] := RootNode.ChildCount + 1;
    ModelMessages.FNode := RootNode.LastChild;
    Data := vstWarningsAndErrors.GetNodeData(ModelMessages.FNode);
    Data.List := ModelMessages.FMessages;
    vstWarningsAndErrors.HasChildren[ModelMessages.FNode] := True;
    RootList.Add(Model.DisplayName);
  end;

  RootIndex := ModelMessages.FMessages.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := ModelMessages.FMessages.Objects[RootIndex] as TErrMessages;
    ChildNode := ModelMessages.FChildNodes[RootIndex];
  end
  else
  begin
    ErrorMessages := TErrMessages.Create;
    ModelMessages.FMessages.AddObject(TypeOfErrorOrWarning, ErrorMessages);
    ChildNode := vstWarningsAndErrors.AddChild(ModelMessages.FNode);
    ModelMessages.FChildNodes.Add(ChildNode);
    Data := vstWarningsAndErrors.GetNodeData(ChildNode);
    Data.List := ErrorMessages;
    vstWarningsAndErrors.HasChildren[ChildNode] := True;
    if frmProgressMM <> nil then
    begin
      frmProgressMM.AddMessage(TypeOfErrorOrWarning);
    end;
  end;

  if ErrorMessages.Count = 1000 then
  begin
    ErrorMessages.Add('More than 1000 similar messages.');
    vstWarningsAndErrors.ChildCount[ChildNode] :=
      vstWarningsAndErrors.ChildCount[ChildNode] + 1;
    ErrorMessages.ObjectList.Add(AnObject);
    Exit;
  end
  else if ErrorMessages.Count > 1000 then
  begin
    ErrorMessages.ObjectList.Add(AnObject);
    Exit;
  end;
  if AnObject <> nil then
  begin
    // Only TScreenObjects are handled here. To change this, the code
    // that handles enabling the popup menu must also be changed.
    Assert(AnObject is TScreenObject);
  end;
  ErrorMessages.AddObject(ErrorOrWarning, AnObject);
  vstWarningsAndErrors.ChildCount[ChildNode] :=
    vstWarningsAndErrors.ChildCount[ChildNode] + 1;
  if (frmProgressMM <> nil) and frmProgressMM.Visible then
  begin
    if (Left + Width > frmProgressMM.Left) and (Left > 0) then
    begin
      NewLeft := frmProgressMM.Left - Width;
      if NewLeft < 0 then
      begin
        NewLeft := 0
      end;
      Left := NewLeft;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.miIgnorethistypeoferrororwarning1Click(
  Sender: TObject);
var
  Node: PVirtualNode;
  IgnoreText: string;
begin
  inherited;
  Node := vstWarningsAndErrors.FocusedNode;
  if not vstWarningsAndErrors.HasChildren[Node] then
  begin
    Node := vstWarningsAndErrors.NodeParent[Node]
  end;
  if (Node <> FErrorNode) and (Node <> FWarningNode) then
  begin
    IgnoreText := vstWarningsAndErrors.Text[Node, 0];
    TextToIgnore.Add(IgnoreText);
  end;
end;

procedure TfrmErrorsAndWarnings.InitializeRootNode(var Node: PVirtualNode;
  List: TErrMessages);
var
  Data: PErrorWarningRec;
begin
  vstWarningsAndErrors.RootNodeCount := vstWarningsAndErrors.RootNodeCount + 1;
  Node := vstWarningsAndErrors.RootNode.LastChild;
  Data := vstWarningsAndErrors.GetNodeData(Node);
  Data.List := List;
  vstWarningsAndErrors.HasChildren[Node] := True;
end;

procedure TfrmErrorsAndWarnings.miEditClick(Sender: TObject);
var
  ScreenObjects: TScreenObjectList;
  ScreenObject: TScreenObject;
begin
  inherited;
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedScreenObjects(ScreenObjects);
    if ScreenObjects.Count >= 1 then
    begin
      ScreenObject := ScreenObjects[0];
      SelectAScreenObject(ScreenObject);
      frmGoPhast.EditScreenObjects;
    end;
  finally
    ScreenObjects.Free;
  end;

end;

procedure TfrmErrorsAndWarnings.miGotoClick(Sender: TObject);
var
  ScreenObjects: TScreenObjectList;
  ScreenObject: TScreenObject;
  UndoShowHide: TUndoShowHideScreenObject;
begin
  inherited;
  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedScreenObjects(ScreenObjects);
    if ScreenObjects.Count >= 1 then
    begin
      ScreenObject := ScreenObjects[0];
      if not ScreenObject.Visible then
      begin
        UndoShowHide := TUndoShowHideScreenObject.Create;
        UndoShowHide.AddScreenObjectToChange(ScreenObject);
        frmGoPhast.UndoStack.Submit(UndoShowHide);
      end;

      GoToObject(ScreenObject);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmErrorsAndWarnings.miSelectClick(Sender: TObject);
var
  ScreenObjects: TScreenObjectList;
begin
  inherited;


  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedScreenObjects(ScreenObjects);
    if ScreenObjects.Count > 0 then
    begin
      SelectMultipleScreenObjects(ScreenObjects);
    end;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmErrorsAndWarnings.RemoveWarningOrErrorGroup(Model: TBaseModel;
  const TypeOfErrorOrWarning: string; RootList: TStringList; Children: TList;
      ModelMessageList: TModelMessageList);
var
  RootIndex: Integer;
  ErrorMessages: TStringList;
  ChildNode: PVirtualNode;
  ModelMessages: TModelMessages;
begin
  Assert(Model <> nil);
  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode = nil) then
  begin
    Exit;
  end;
  RootIndex := ModelMessages.FMessages.IndexOf(TypeOfErrorOrWarning);
  if RootIndex >= 0 then
  begin
    ErrorMessages := ModelMessages.FMessages.Objects[RootIndex] as TErrMessages;
    ChildNode := ModelMessages.FChildNodes[RootIndex];

    vstWarningsAndErrors.DeleteNode(ChildNode);
    ModelMessages.FMessages.Delete(RootIndex);
    ModelMessages.FChildNodes.Delete(RootIndex);
    ErrorMessages.Free;
  end
end;

procedure TfrmErrorsAndWarnings.SetDelayShowing(const Value: boolean);
begin
  FDelayShowing := Value;
  if not FDelayShowing and FShouldShow then
  begin
    FShouldShow := False;
    Show;
  end;

end;

class procedure TfrmErrorsAndWarnings.SetFormLeft(const Value: Integer);
begin
  FFormLeft := Value;
end;

class procedure TfrmErrorsAndWarnings.SetFormTop(const Value: Integer);
begin
  FFormTop := Value;
end;

procedure TfrmErrorsAndWarnings.Show;
begin
  if DelayShowing then
  begin
    FShouldShow := True;
  end
  else
  begin
    inherited;
  end;
end;

procedure TfrmErrorsAndWarnings.ShowAfterDelay;
begin
  Timer1.Enabled := True;
end;

procedure TfrmErrorsAndWarnings.GetErrorsAndWarnings(
  ErrorsAndWarings: TStringList);
const
  TabChar = ''#9'';
var
  Index: Integer;
  ALine: string;
  Node: PVirtualNode;
begin
  Node := vstWarningsAndErrors.GetFirst;
  while Node <> nil do
  begin
    ALine := '';
    for Index := 0 to vstWarningsAndErrors.GetNodeLevel(Node) do
    begin
      ALine := ALine + TabChar;
    end;
    ErrorsAndWarings.Add(ALine + vstWarningsAndErrors.Text[Node, 0]);
    Node := vstWarningsAndErrors.GetNext(Node);
  end;
end;

procedure TfrmErrorsAndWarnings.Timer1Timer(Sender: TObject);
begin
  inherited;
  Timer1.Enabled := False;
  Show;
end;

procedure TfrmErrorsAndWarnings.RemoveErrorGroup(Model: TBaseModel;
  const TypeOfError: string);
begin
  RemoveWarningOrErrorGroup(Model, TypeOfError, FErrorModels,
    FErrorChildNodes, FErrorModelMessageList);
  CheckDeleteRootNode(Model, FErrorNode, FErrorModels, FErrorModelMessageList);
end;

procedure TfrmErrorsAndWarnings.CheckDeleteRootNode(Model: TBaseModel;
  var Node: PVirtualNode; RootList: TStringList;
  ModelMessageList: TModelMessageList);
var
  ModelMessages: TModelMessages;
  RootIndex: Integer;
begin
  Assert(Model <> nil);
  ModelMessages := ModelMessageList.GetModelMessages(Model);
  if (ModelMessages.FNode <> nil)
    and not vstWarningsAndErrors.HasChildren[ModelMessages.FNode] then
  begin
    vstWarningsAndErrors.DeleteNode(ModelMessages.FNode);
    ModelMessages.FNode := nil
  end;

  if ModelMessages.FNode = nil then
  begin
    RootIndex := RootList.IndexOf(Model.DisplayName);
    if RootIndex >= 0 then
    begin
      RootList.Delete(RootIndex);
    end;
  end;

  if (Node <> nil) and not vstWarningsAndErrors.HasChildren[Node] then
  begin

    vstWarningsAndErrors.DeleteNode(Node);
    Node := nil;
  end;
end;

procedure TfrmErrorsAndWarnings.RemoveWarningGroup(Model: TBaseModel;
  const TypeOfWarning: string);
begin
  RemoveWarningOrErrorGroup(Model, TypeOfWarning, FWarningModels,
    FWarningChildNodes, FWarningModelMessageList);
  CheckDeleteRootNode(Model, FWarningNode, FWarningModels,
    FWarningModelMessageList);
end;

procedure TfrmErrorsAndWarnings.AddError(Model: TBaseModel;
  const Root, Error: string; AnObject: TObject = nil);
begin
  AddErrorOrWarning(Model, FErrorModels, Root, Error, FErrorNode,
    FErrorChildNodes, FErrorModelMessageList, AnObject);
end;

procedure TfrmErrorsAndWarnings.AddWarning(Model: TBaseModel;
  const Root, Warning: string; AnObject: TObject = nil);
begin
  AddErrorOrWarning(Model, FWarningModels, Root, Warning,
    FWarningNode, FWarningChildNodes, FWarningModelMessageList, AnObject);
end;

procedure TfrmErrorsAndWarnings.BeginUpdate;
begin
  vstWarningsAndErrors.BeginUpdate;
end;

procedure TfrmErrorsAndWarnings.btnClearClick(Sender: TObject);
begin
  inherited;
  Clear;
end;

procedure TfrmErrorsAndWarnings.btnCopyClick(Sender: TObject);
var
  ErrorsAndWarings: TStringList;
begin
  inherited;
  ErrorsAndWarings := TStringList.Create;
  try
    GetErrorsAndWarnings(ErrorsAndWarings);
    Clipboard.AsText := ErrorsAndWarings.Text;
  finally
    ErrorsAndWarings.Free;
  end;
end;

procedure TfrmErrorsAndWarnings.btnSaveClick(Sender: TObject);
var
  ErrorsAndWarings: TStringList;
begin
  inherited;
  if sdSaveFileDlg.Execute then
  begin
    ErrorsAndWarings := TStringList.Create;
    try
      GetErrorsAndWarnings(ErrorsAndWarings);
      ErrorsAndWarings.SaveToFile(sdSaveFileDlg.FileName);
    finally
      ErrorsAndWarings.Free;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.Clear;
begin
  vstWarningsAndErrors.Clear;
  FErrorNode := nil;
  FWarningNode := nil;
  FErrorModels.Clear;
  FWarningModels.Clear;
  FErrorChildNodes.Clear;
  FWarningChildNodes.Clear;
  FWarningModelMessageList.Clear;
  FErrorModelMessageList.Clear;
end;

procedure TfrmErrorsAndWarnings.EndUpdate;
begin
  vstWarningsAndErrors.EndUpdate;
end;

procedure TfrmErrorsAndWarnings.GetSelectedScreenObjects(
  ScreenObjects: TScreenObjectList);
var
  FirstScreenObject: TScreenObject;
  AnObject: TObject;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  Data: PErrorWarningRec;
  ObjectIndex: Integer;
  CurrentScreenObject: TScreenObject;
begin
  Node := vstWarningsAndErrors.FocusedNode;
  if Node <> nil then
  begin
    Data := vstWarningsAndErrors.GetNodeData(Node);
    if Assigned(Data) and (Data.List <> nil) then
    begin
      FirstScreenObject := nil;
      for ObjectIndex := 0 to Data.List.ObjectList.Count - 1 do
      begin
        AnObject := Data.List.ObjectList[ObjectIndex];
        // AnObject might be TErrMessages
        if (AnObject <> nil) and (AnObject is TScreenObject) then
        begin
          CurrentScreenObject := TScreenObject(AnObject);
          if FirstScreenObject <> nil then
          begin
            if FirstScreenObject.ViewDirection
              <> CurrentScreenObject.ViewDirection then
            begin
              Continue;
            end;
          end
          else
          begin
            FirstScreenObject := CurrentScreenObject;
          end;
          if ScreenObjects.IndexOf(CurrentScreenObject) >= 0 then
          begin
            Continue;
          end;
          ScreenObjects.Add(CurrentScreenObject);
        end;
      end;
    end
    else
    begin
      ParentNode := vstWarningsAndErrors.NodeParent[Node];
      Data := vstWarningsAndErrors.GetNodeData(ParentNode);
      AnObject := Data.List.Objects[Node.Index];
      if AnObject is TScreenObject then
      begin
        CurrentScreenObject := TScreenObject(AnObject);
        if CurrentScreenObject <> nil then
        begin
          ScreenObjects.Add(CurrentScreenObject);
        end;
      end;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.FormCreate(Sender: TObject);
begin
  inherited;
  vstWarningsAndErrors.NodeDataSize := SizeOf(TErrorWarningRec);
  FErrorModels := TErrMessages.Create;
  FWarningModels := TErrMessages.Create;
  FErrorModelMessageList:= TModelMessageList.Create;

  FErrorChildNodes:= TList.Create;
  FWarningChildNodes:= TList.Create;
  FWarningModelMessageList:= TModelMessageList.Create;
end;

procedure TfrmErrorsAndWarnings.FormDestroy(Sender: TObject);
begin
  inherited;
  FormTop := Top;
  FormLeft := Left;
  Clear;
  FErrorModels.Free;
  FWarningModels.Free;
  FErrorChildNodes.Free;
  FWarningChildNodes.Free;
  FErrorModelMessageList.Free;
  FWarningModelMessageList.Free;
  vstWarningsAndErrors.Clear;
end;

procedure TfrmErrorsAndWarnings.FormHide(Sender: TObject);
begin
  inherited;
  FormTop := Top;
  FormLeft := Left;
end;

procedure TfrmErrorsAndWarnings.FormResize(Sender: TObject);
var
  Indent: integer;
begin
  inherited;
  // Using the Indent local variable here instead of
  // vstWarningsAndErrors.Indent avoids generating
  // a compiler warning.
  Indent := vstWarningsAndErrors.Indent;
  vstWarningsAndErrors.Header.Columns[0].Width :=
    vstWarningsAndErrors.ClientWidth - Indent;
end;

procedure TfrmErrorsAndWarnings.FormShow(Sender: TObject);
var
  NewTop: integer;
  NewBottom: Integer;
begin
  inherited;
  if (FormTop <> 0) and (FormLeft <> 0)  then
  begin
    Top := FormTop;
    Left := FormLeft;
  end;
  if (frmProgressMM <> nil) and frmProgressMM.Visible then
  begin
    NewTop := Top + frmProgressMM.Top;
    NewBottom := NewTop + Height;
    if NewBottom > Screen.Height then
    begin
      NewTop := Screen.Height - Height;
    end;
    Top := NewTop;
  end;
end;

function TfrmErrorsAndWarnings.HasMessages: boolean;
begin
  result := (FErrorModels.Count > 0) or (FWarningModels.Count > 0)
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsContextPopup(
  Sender: TObject; MousePos: TPoint; var Handled: Boolean);
var
  ScreenObjects: TScreenObjectList;
begin
  inherited;

  ScreenObjects := TScreenObjectList.Create;
  try
    GetSelectedScreenObjects(ScreenObjects);
    miSelect.Enabled := ScreenObjects.Count > 0;
    miEdit.Enabled := ScreenObjects.Count = 1;
    miGoto.Enabled := miEdit.Enabled;
  finally
    ScreenObjects.Free;
  end;
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  Data: PErrorWarningRec;
  ParentNode: PVirtualNode;
begin
  inherited;
  // A handler for the OnGetText event is always needed as it provides the
  // tree with the string data to display.
  // Note that we are now using string instead of WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    if Node = FErrorNode then
    begin
      CellText := StrErrors;
    end
    else if Node = FWarningNode then
    begin
      CellText := StrWarnings;
    end
    else
    begin
      ParentNode := Sender.NodeParent[Node];
      Data := Sender.GetNodeData(ParentNode);
      if Assigned(Data) and (Data.List <> nil)
        and (Data.List.Count > Node.Index) then
      begin
        CellText := Data.List[Node.Index];
      end;
    end;
  end;
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
begin
  inherited;
  Include(InitialStates, ivsMultiline);
end;

procedure TfrmErrorsAndWarnings.vstWarningsAndErrorsMeasureItem(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  var NodeHeight: Integer);
var
  CellCaption: String;
  Flags: UINT;
  Rect: TRect;
  TextString: string;
  Count: Integer;
  ANode: PVirtualNode;
begin
  inherited;
  Count := -2;
  ANode := Node;
  While (ANode <> nil) do
  begin
    Inc(Count);
    ANode := Sender.NodeParent[ANode];
  end;
  Count := Max(0, Count);
  vstWarningsAndErrorsGetText(Sender, Node, 0, ttNormal, CellCaption);
  TextString := CellCaption;

  Flags := DT_NOPREFIX or DT_Left or DT_CALCRECT or DT_WORDBREAK;

  Rect.Top := 0;
  Rect.Left := 4 + Count*27;
  Rect.Bottom := 300;
  Rect.Right := vstWarningsAndErrors.Header.Columns[0].Width - 40;

  TargetCanvas.Font := Font;
  NodeHeight := DrawText(TargetCanvas.Handle, PChar(TextString),
        Length(TextString), Rect, Flags) + 4;
  NodeHeight := Min(NodeHeight, High(Word));

  vstWarningsAndErrors.NodeHeight[Node] := NodeHeight;
end;


{ TModelMessages }

constructor TModelMessages.Create(Model: TBaseModel);
begin
  FModel := Model;
  FMessages := TErrMessages.Create;
  FChildNodes:= TList.Create;
end;

destructor TModelMessages.Destroy;
var
  Index: Integer;
begin
  FChildNodes.Free;
  for Index := FMessages.Count - 1 downto 0 do
  begin
    FMessages.Objects[Index].Free;
  end;
  FMessages.Free;
  inherited;
end;

{ TModelMessageList }

procedure TModelMessageList.Clear;
begin
  FList.Clear;
end;

constructor TModelMessageList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TModelMessageList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TModelMessageList.GetModelMessages(Model: TBaseModel): TModelMessages;
var
  Index: Integer;
  Item: TModelMessages;
begin
  for Index := 0 to FList.Count - 1 do
  begin
    Item := FList[Index];
    if Item.FModel = Model then
    begin
      result := Item;
      Exit;
    end;
  end;
  result := TModelMessages.Create(Model);
  FList.Add(result);
end;

{ TErrMessages }

function TErrMessages.AddObject(const S: string; AObject: TObject): Integer;
begin
  result := inherited;
  FObjects.Add(AObject);
end;

constructor TErrMessages.Create;
begin
  inherited;
  FObjects := TList.Create;
end;

destructor TErrMessages.Destroy;
begin
  FObjects.Free;
  inherited;
end;

initialization
  TextToIgnore := TStringList.Create;
  TextToIgnore.Sorted := True;
  TextToIgnore.Duplicates := dupIgnore;

finalization
  TextToIgnore.Free;
  FfrmErrorsAndWarnings.Free;

end.
