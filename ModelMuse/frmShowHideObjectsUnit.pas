{@abstract(@name is used to define @link(TfrmShowHideObjects) which is
  used to show or hide @link(TScreenObject)s either
  individually or based on the data sets or boundary conditions they affect.)}
unit frmShowHideObjectsUnit;

interface

uses
  UndoItemsScreenObjects, Windows, SysUtils, Types, Classes, Variants,
  Graphics, Controls, Forms, Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls,
  Buttons, ExtCtrls, Menus, ScreenObjectUnit, VirtualTrees, Contnrs,
  frmCustomSelectObjectsUnit, ImgList, System.ImageList;

type
  TSelectChoice = (scShow, scSelect);

  {@abstract(@name is used to show or hide @link(TScreenObject)s either
    individually or based on the data sets or boundary conditions they affect.)}
  TfrmShowHideObjects = class(TfrmCustomSelectObjects)
    // @name is associated with @link(TfrmCustomSelectObjects.vstObjects)
    // and holds @link(miSelect)
    // and @link(miSelect);
    // See @link(vstObjectsContextPopup).
    pmSelectEdit: TPopupMenu;
    // @name is the Select menu item of @link(pmSelectEdit).  Clicking
    // it selects the @link(TScreenObject) of the selected node.
    miSelect: TMenuItem;
    // @name is the Edit menu item of @link(pmSelectEdit).  Clicking
    // it edits the @link(TScreenObject) of the selected node.
    miEdit: TMenuItem;
    ilAngles: TImageList;
    ilDifferentAngle: TImageList;
    miGoto: TMenuItem;
    grpShowOrSelect: TGroupBox;
    rgShowOrSelect: TRadioGroup;
    rgOrientation: TRadioGroup;
    edSearchTerm: TEdit;
    btnShowOrSelect: TButton;
    miDeselect: TMenuItem;
    miAddToSelection: TMenuItem;
    btnEditAllSelected: TButton;
    // @name calls Release and sets frmShowHideObjects to nil.
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    // @name is the event handler for the OnCreate event of @classname.
    procedure FormCreate(Sender: TObject); override;
    // See @link(miEdit).  Also the event handler for
    // @link(vstObjects).OnDblClick.
    procedure miEditClick(Sender: TObject);
    // See @link(miSelect).
    procedure miSelectClick(Sender: TObject);
    // @name calls @link(AdjustFormPosition).
    procedure FormShow(Sender: TObject);
    // @name is used to show or hide @link(TScreenObject)s when a checkbox
    // is checked or unchecked.
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    // @name is used to enable or disable items in @link(pmSelectEdit)
    procedure vstObjectsContextPopup(Sender: TObject; MousePos: TPoint;
      var Handled: Boolean);
    procedure vstObjectsPaintText(Sender: TBaseVirtualTree;
      const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      TextType: TVSTTextType);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure vstObjectsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure miGotoClick(Sender: TObject);
    procedure rgShowOrSelectClick(Sender: TObject);
    procedure btnShowOrSelectClick(Sender: TObject);
    procedure vstObjectsGetImageIndexEx(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
      var Ghosted: Boolean; var ImageIndex: VirtualTrees.TImageIndex;
      var ImageList: TCustomImageList);
    procedure FormDestroy(Sender: TObject); override;
    procedure miDeselectClick(Sender: TObject);
    procedure miAddToSelectionClick(Sender: TObject);
    procedure btnEditAllSelectedClick(Sender: TObject);
  private
    FUndoShowHide: TUndoShowHideScreenObject;
    FCount: integer;
    FSupressUndo: boolean;
  private
    // @name gets the @link(TScreenObject) associated with the selected
    // node in @link(vstObjects).
    function GetSelectedScreenObject: TScreenObject;
    // @name enables the menu items in @link(pmSelectEdit)
    // if Node.Parent has a
    // list of @link(TScreenObject) in its Data.
    procedure vstEnablePopupMenuItems(Node: PVirtualNode);
    procedure CreateAngleImages;
  { Private declarations }
  protected
    // See @link(TfrmCustomSelectObjects.CanEdit).
    procedure SetCanEdit(const Value: boolean); override;
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean; override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    procedure RecordExpandedObjectNodes;
    procedure RestoreExpandedObjectNodes;
  public
    property SupressUndo: boolean read FSupressUndo write FSupressUndo;
    { Public declarations }
  end;

var
  // @name holds the instance of the @link(TfrmShowHideObjects) dialog box.
  frmShowHideObjects: TfrmShowHideObjects = nil;

implementation

uses StrUtils, frmGoPhastUnit, DataSetUnit, GoPhastTypes, ModelMuseUtilities,
  ModflowPackagesUnit, InteractiveTools, FastGEO, UndoItems, frmGoToUnit;

resourcestring
  StrSObjectsContainin = '%s objects containing search term';
  StrShow = 'Show';
  StrSelect = 'Select';

{$R *.dfm}

{ TfrmShowHideObjects }

procedure TfrmShowHideObjects.btnEditAllSelectedClick(Sender: TObject);
begin
  inherited;
  frmGoPhast.EditScreenObjects;
end;

procedure TfrmShowHideObjects.btnShowOrSelectClick(Sender: TObject);
var
  List: TScreenObjectList;
  index: Integer;
  AScreenObject: TScreenObject;
  SearchTerm: string;
  ShouldSelect: Boolean;
  FUndoShowHide: TUndoShowHideScreenObject;
  Changed: Boolean;
  ViewDirection: TViewDirection;
begin
  inherited;
  SearchTerm := edSearchTerm.Text;
  if SearchTerm = '' then
  begin
    Exit;
  end;

  case TSelectChoice(rgShowOrSelect.ItemIndex) of
    scShow:
      begin
        Screen.Cursor := crHourGlass;
        FSettingData := True;
        vstObjects.BeginUpdate;
        try
          FCount := -1;
          FUndoShowHide := TUndoShowHideScreenObject.Create;
          try
            Changed := False;
            for index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
            begin
              AScreenObject := frmGoPhast.PhastModel.ScreenObjects[index];
              if AScreenObject.Deleted or AScreenObject.Visible then
              begin
                Continue;
              end;
              if Pos(SearchTerm, AScreenObject.Name) > 0 then
              begin
                AScreenObject.Visible := True;
                Changed := True;
              end;
            end;
            FUndoShowHide.SetPostSelection;
            if Changed then
            begin
              frmGoPhast.UndoStack.Submit(FUndoShowHide);
            end
            else
            begin
              FreeAndNil(FUndoShowHide);
            end;
          except
            FreeAndNil(FUndoShowHide);
            raise;
          end;
          SetCheckStates;
        finally
          vstObjects.EndUpdate;
          FSettingData := False;
          Screen.Cursor := crDefault;
        end;
      end;
    scSelect:
      begin
        ViewDirection := TViewDirection(rgOrientation.ItemIndex);
        List := TScreenObjectList.Create;
        try
          for index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
          begin
            AScreenObject := frmGoPhast.PhastModel.ScreenObjects[index];
            if AScreenObject.Deleted then
            begin
              Continue;
            end;
            if AScreenObject.ViewDirection <> ViewDirection then
            begin
              Continue;
            end;
            ShouldSelect := Pos(SearchTerm, AScreenObject.Name) > 0;
            if not ShouldSelect then
            begin
              Continue;
            end;
            List.Add(AScreenObject);
          end;
          if List.Count > 0 then
          begin
            SelectMultipleScreenObjects(List);
          end;
        finally
          List.Free;
        end;
      end;
    else
      Assert(False);
  end;


end;

procedure TfrmShowHideObjects.CreateAngleImages;
var
  BMP: TBitmap;
  index: Integer;
  Angle: double;
  X: integer;
  Y: integer;
begin
  BMP := TBitmap.Create;
  try
    BMP.Width := 20;
    BMP.Height := 20;
    BMP.Canvas.Brush.Color := clWindow;
    for index := 0 to 360 do
    begin
      Angle := index*Pi/180;
      X := Round(Cos(Angle)*10);
      Y := Round(Sin(Angle)*10);

      BMP.Canvas.Pen.Color := clRed;
      BMP.Canvas.FillRect(Rect(0,0,20,20));
      BMP.Canvas.MoveTo(10+X, 10-Y);
      BMP.Canvas.LineTo(10-X, 10+Y);

      ilAngles.Add(BMP, nil);

      BMP.Canvas.Pen.Color := clGray;
      BMP.Canvas.FillRect(Rect(0,0,20,20));
      BMP.Canvas.MoveTo(10+X, 10-Y);
      BMP.Canvas.LineTo(10-X, 10+Y);
      ilDifferentAngle.Add(BMP, nil);
    end;
  finally
    BMP.Free;
  end;
end;

procedure TfrmShowHideObjects.FormCreate(Sender: TObject);
begin
  inherited;
  CreateAngleImages;
  FSupressUndo := False;
  GetData;
  RestoreExpandedObjectNodes;
end;


procedure TfrmShowHideObjects.FormDestroy(Sender: TObject);
var
  i: Integer;
  FormPosition: TRect;
begin
  RecordExpandedObjectNodes;

  FormPosition.Top := Top;
  FormPosition.Left := Left;
  FormPosition.Width := Width;
  FormPosition.Height := Height;
  frmGoPhast.ObjectsPosition := FormPosition;

  inherited;

  // Let vstObjects finish with any threads to prevent an access violation.
  for i := 1 to 5 do
  begin
    sleep(10);
    application.processmessages;
  end;
  vstObjects.RootNodeCount := 0;
end;

procedure TfrmShowHideObjects.FormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
const
  Key_Delete = VK_DELETE; // = 46
begin
  inherited;
  if Key = Key_Delete then
  begin
    frmGoPhast.FormKeyUp(Sender, Key, Shift);
  end;
end;

procedure TfrmShowHideObjects.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
  if FSettingData then
  begin
    Exit;
  end;
  Action := caFree;
  frmShowHideObjects := nil;

  inherited;
end;

function TfrmShowHideObjects.GetSelectedScreenObject: TScreenObject;
var
  Data: PMyRec;
begin
  result := nil;

  if vstObjects.FocusedNode = nil then
  begin
    Exit;
  end;

  Data := vstObjects.GetNodeData(vstObjects.NodeParent[vstObjects.FocusedNode]);
  if (Data <> nil) and (Data.ScreenObjects <> nil) then
  begin
    result := Data.ScreenObjects[vstObjects.FocusedNode.Index];
  end;
end;

procedure TfrmShowHideObjects.miSelectClick(Sender: TObject);
var
  ScreenObject: TScreenObject;
begin
  inherited;
  ScreenObject := GetSelectedScreenObject;
  if ScreenObject <> nil then
  begin
    SelectAScreenObject(ScreenObject);
  end;
end;

procedure TfrmShowHideObjects.RecordExpandedObjectNodes;
var
  ANode: PVirtualNode;
  Key: string;
begin
  if frmGoPhast.FObjectsExpanded = nil then
  begin
    Exit;
  end;
//  vstObjects.BeginUpdate;
//  try
    frmGoPhast.FObjectsExpanded.Clear;
    ANode := vstObjects.GetFirst;
    while ANode <> nil do
    begin
      if vstObjects.Expanded[ANode] then
      begin
        Key := NodeString(ANode);
        if not frmGoPhast.FObjectsExpanded.ContainsKey(Key) then
        begin
          frmGoPhast.FObjectsExpanded.Add(Key, True);
        end;
      end;
      ANode := vstObjects.GetNext(ANode)
    end;
//  finally
//    vstObjects.EndUpdate;
//  end;
end;

procedure TfrmShowHideObjects.RestoreExpandedObjectNodes;
var
  ANode: PVirtualNode;
  Expanded: Boolean;
begin
  vstObjects.BeginUpdate;
  try
    ANode := vstObjects.GetFirst;
    while ANode <> nil do
    begin
      if frmGoPhast.FObjectsExpanded.TryGetValue(NodeString(ANode), Expanded) then
      begin
        vstObjects.Expanded[ANode] := Expanded;
      end
      else
      begin
        vstObjects.Expanded[ANode] := False;
      end;
      ANode := vstObjects.GetNext(ANode)
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmShowHideObjects.rgShowOrSelectClick(Sender: TObject);
begin
  inherited;
  case TSelectChoice(rgShowOrSelect.ItemIndex) of
    scShow:
      begin
        btnShowOrSelect.Caption := Format(StrSObjectsContainin, [StrShow]);
        rgOrientation.Enabled := False;
      end;
    scSelect:
      begin
        btnShowOrSelect.Caption := Format(StrSObjectsContainin, [StrSelect]);
        rgOrientation.Enabled := True;
      end;
    else
      Assert(False);
  end;
end;

procedure TfrmShowHideObjects.vstEnablePopupMenuItems(Node: PVirtualNode);
var
  Data: PMyRec;
begin
  If Node = nil then
  begin
    Data := nil;
  end
  else
  begin
    Data := vstObjects.GetNodeData(vstObjects.NodeParent[Node]);
  end;
  miSelect.Enabled := (Data <> nil) and (Data.ScreenObjects <> nil);
  miEdit.Enabled := miSelect.Enabled;
  miGoto.Enabled := miSelect.Enabled;
  miDeselect.Enabled := miSelect.Enabled;
  miAddToSelection.Enabled := miSelect.Enabled;
end;

procedure TfrmShowHideObjects.HandleChecked(AScreenObject: TScreenObject);
begin
  if not AScreenObject.Visible then
  begin
    FCount := FUndoShowHide.AddScreenObjectToChange(AScreenObject);
  end;
end;

procedure TfrmShowHideObjects.HandleUnchecked(AScreenObject: TScreenObject);
begin
  if AScreenObject.Visible then
  begin
    FCount := FUndoShowHide.AddScreenObjectToChange(AScreenObject);
  end;
end;

procedure TfrmShowHideObjects.vstObjectsChecked(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
begin
  inherited;
  if FSettingData or FSettingData2 or FSettingData3 then
  begin
    Exit;
  end;
//  if (Sender.NodeParent[Node] = nil) then
//  begin
//    Exit;
//  end;
  if not FOkToDoCheck then
  begin
    Exit;
  end;
  Screen.Cursor := crHourGlass;
  FSettingData := True;
  Sender.BeginUpdate;
  try
    FCount := -1;
    FUndoShowHide := TUndoShowHideScreenObject.Create;
    try
      HandleCheckChange(Node, Sender);
      FUndoShowHide.SetPostSelection;
      if (FCount >= 0) and not SupressUndo then
      begin
        frmGoPhast.UndoStack.Submit(FUndoShowHide);
      end
      else
      begin
        FreeAndNil(FUndoShowHide);
      end;
    except
      FreeAndNil(FUndoShowHide);
      raise;
    end;
    SetCheckStates;
  finally
    Sender.EndUpdate;
    FSettingData := False;
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmShowHideObjects.vstObjectsContextPopup(Sender: TObject;
  MousePos: TPoint; var Handled: Boolean);
begin
  inherited;
  vstEnablePopupMenuItems(vstObjects.FocusedNode);
end;


procedure TfrmShowHideObjects.vstObjectsGetImageIndexEx(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Kind: TVTImageKind;
  Column: TColumnIndex; var Ghosted: Boolean; var ImageIndex: VirtualTrees.TImageIndex;
  var ImageList: TCustomImageList);
var
  Data: PMyRec;
  ScreenObject: TScreenObject;
  Angle: double;
begin
  inherited;
  ImageIndex := -1;
  if (frmGoPhast.PhastModel.ModelSelection in SutraSelection)
    and (Kind in [ikNormal, ikSelected]) then
  begin
    Data := vstObjects.GetNodeData(vstObjects.NodeParent[Node]);
    if (Data <> nil) and (Data.ScreenObjects <> nil) then
    begin
      ScreenObject := Data.ScreenObjects[Node.Index];
      if ScreenObject.ViewDirection = vdFront then
      begin
        Angle := ScreenObject.SutraAngle*180/Pi;
        if Angle < 0 then
        begin
          Angle := Angle + 360;
        end;
        ImageIndex := Round(Angle);
        if Abs(frmGoPhast.PhastModel.SutraMesh.CrossSection.Angle
          - ScreenObject.SutraAngle) > 1e-6 then
        begin
          ImageList := ilDifferentAngle;
        end
        else
        begin
          ImageList := ilAngles;
        end;
      end;
    end;
  end;
end;

procedure TfrmShowHideObjects.vstObjectsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitInfo;
  Data: PMyRec;
  ScreenObject: TScreenObject;
  NewLocation: TSegment2D;
  Undo: TUndoSpecifyCrossSection;
  Node: PVirtualNode;
begin
  inherited;
  vstObjects.GetHitTestInfoAt(X, Y, False, HitInfo);
  if hiOnNormalIcon in HitInfo.HitPositions then
  begin
    if (frmGoPhast.PhastModel.ModelSelection in SutraSelection) then
    begin
      Node := vstObjects.GetNodeAt(X, Y);
      Data := vstObjects.GetNodeData(vstObjects.NodeParent[Node]);
      if (Data <> nil) and (Data.ScreenObjects <> nil) then
      begin
        ScreenObject := Data.ScreenObjects[Node.Index];
        if ScreenObject.ViewDirection = vdFront then
        begin
          SetNewCrossSectionAngle(ScreenObject.SutraAngle, NewLocation);
          Undo := TUndoSpecifyCrossSection.Create(NewLocation);
          frmGoPhast.UndoStack.Submit(Undo);
        end;
      end;
    end;
  end;
  if (hiOnItemLabel in HitInfo.HitPositions) then
  begin
    Node := vstObjects.GetNodeAt(X, Y);
    if vstObjects.HasChildren[Node]  then
    begin
      vstObjects.Expanded[Node] := not vstObjects.Expanded[Node];
    end;
  end;
end;

procedure TfrmShowHideObjects.vstObjectsPaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
  Data: PMyRec;
  ScreenObject: TScreenObject;
  Index: Integer;
  HasSelected: Boolean;
  ChildNodes: TList;
  NodeIndex: Integer;
  ChildNode : PVirtualNode;
  procedure GetChildNodes(ANode: PVirtualNode; ChildNodes: TList);
  var
    ChildNode : PVirtualNode;
  begin
    ChildNode := ANode.FirstChild;
    While ChildNode <> nil do
    begin
      ChildNodes.Add(ChildNode);
      if ChildNode.ChildCount > 0 then
      begin
        GetChildNodes(ChildNode, ChildNodes);
      end;
      ChildNode := ChildNode.NextSibling;
    end;
  end;
begin
  inherited;
  Data := vstObjects.GetNodeData(vstObjects.NodeParent[Node]);
  if (Data <> nil) and (Data.ScreenObjects <> nil) then
  begin
    ScreenObject := Data.ScreenObjects[Node.Index];
    if ScreenObject = nil then
    begin
      TargetCanvas.Font.Style := [];
    end
    else if ScreenObject.Selected then
    begin
      TargetCanvas.Font.Style := [fsBold];
    end
    else
    begin
      TargetCanvas.Font.Style := [];
    end;
  end
  else if vsExpanded in Node.States then
  begin
    TargetCanvas.Font.Style := [];
  end
  else
  begin
    Data := vstObjects.GetNodeData(Node);
    if (Data <> nil) and (Data.ScreenObjects <> nil) then
    begin
      HasSelected := False;
      for Index := 0 to Data.ScreenObjects.Count - 1 do
      begin
        ScreenObject := Data.ScreenObjects[Index];
        if (ScreenObject <> nil) and ScreenObject.Selected then
        begin
          HasSelected := True;
          break;
        end;
      end;
      if HasSelected then
      begin
        TargetCanvas.Font.Style := [fsBold];
      end
      else
      begin
        TargetCanvas.Font.Style := [];
      end;
    end
    else
    begin
      if Node.ChildCount > 0 then
      begin
        HasSelected := False;
        ChildNodes := TList.Create;
        try
          GetChildNodes(Node, ChildNodes);
          for NodeIndex := 0 to ChildNodes.Count - 1 do
          begin
            ChildNode := ChildNodes[NodeIndex];
            Data := vstObjects.GetNodeData(ChildNode);
            if (Data <> nil) and (Data.ScreenObjects <> nil) then
            begin
              for Index := 0 to Data.ScreenObjects.Count - 1 do
              begin
                ScreenObject := Data.ScreenObjects[Index];
                if (ScreenObject <> nil) and ScreenObject.Selected then
                begin
                  HasSelected := True;
                  break;
                end;
              end;
              if HasSelected then
              begin
                break;
              end;
            end;
          end;
        finally
          ChildNodes.Free;
        end;
        if HasSelected then
        begin
          TargetCanvas.Font.Style := [fsBold];
        end
        else
        begin
          TargetCanvas.Font.Style := [];
        end;
      end;
    end;
  end;

end;

function TfrmShowHideObjects.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := ScreenObject.Visible;
end;

procedure TfrmShowHideObjects.miAddToSelectionClick(Sender: TObject);
var
  ScreenObject: TScreenObject;
begin
  inherited;
  ScreenObject := GetSelectedScreenObject;
  if ScreenObject <> nil then
  begin
    AddAScreenObjectToSelection(ScreenObject);
  end;
end;

procedure TfrmShowHideObjects.miDeselectClick(Sender: TObject);
var
  ScreenObject: TScreenObject;
begin
  inherited;
  ScreenObject := GetSelectedScreenObject;
  if ScreenObject <> nil then
  begin
    DeselectAScreenObject(ScreenObject);
  end;
end;

procedure TfrmShowHideObjects.miEditClick(Sender: TObject);
var
  ScreenObject: TScreenObject;
begin
  inherited;
  if not CanEdit then Exit;
  CanEdit := False;
  try
    ScreenObject := GetSelectedScreenObject;
    if ScreenObject <> nil then
    begin
      miSelectClick(nil);
      frmGoPhast.EditScreenObjects;
    end
    else if (Sender = vstObjects) and (vstObjects.FocusedNode <> nil) then
    begin
      if vstObjects.Expanded[vstObjects.FocusedNode] then
      begin
        vstObjects.Expanded[vstObjects.FocusedNode] := False;
      end
      else
      begin
        vstObjects.Expanded[vstObjects.FocusedNode] := True;
      end;
    end;
  finally
    CanEdit := True;
  end;
end;

procedure TfrmShowHideObjects.miGotoClick(Sender: TObject);
var
  ScreenObject: TScreenObject;
  UndoShowHide: TUndoShowHideScreenObject;
begin
  inherited;
  ScreenObject := GetSelectedScreenObject;
  if ScreenObject <> nil then
  begin
    if not ScreenObject.Visible then
    begin
      UndoShowHide := TUndoShowHideScreenObject.Create;
      UndoShowHide.AddScreenObjectToChange(ScreenObject);
      frmGoPhast.UndoStack.Submit(UndoShowHide);
    end;

    GoToObject(ScreenObject);
  end;
end;

procedure TfrmShowHideObjects.FormShow(Sender: TObject);
var
  FormPosition: TRect;
begin
  inherited;
  if frmGoPhast.ObjectsPosition.IsEmpty then
  begin
    AdjustFormPosition(dpRight);
  end
  else
  begin
    FormPosition := frmGoPhast.ObjectsPosition;
    Top := FormPosition.Top;
    Left := FormPosition.Left;
    Width := FormPosition.Width;
    Height := FormPosition.Height;
  end;
end;

procedure TfrmShowHideObjects.SetCanEdit(const Value: boolean);
begin
  inherited;
  miSelect.Enabled := Value;
  miEdit.Enabled := Value;
  miGoto.Enabled := Value;
  miDeselect.Enabled := Value;
  miAddToSelection.Enabled := Value;
end;

initialization
  // PVirtualNode will be cast to TObject.  This only works if they
  // are the same size.
  Assert(SizeOf(PVirtualNode) = SizeOf(TObject));

finalization
  frmShowHideObjects.Free;

end.
