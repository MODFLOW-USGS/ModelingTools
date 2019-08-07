unit frmSelectObjectsForEditingUnit;

interface

uses System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomSelectObjectsUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, ScreenObjectUnit, Menus, System.Actions, Vcl.ActnList;

type
  TfrmSelectObjectsForEditing = class(TfrmCustomSelectObjects)
    btnOK: TBitBtn;
    rgViewDirection: TRadioGroup;
    btnDelete: TBitBtn;
    pmChangeStates: TPopupMenu;
    miCheckSelected: TMenuItem;
    miUncheckSelected: TMenuItem;
    btnEditFeature: TButton;
    actlst1: TActionList;
    acCheckSelected: TAction;
    acUncheckSelected: TAction;
    btnCheckSelected: TButton;
    btnCheckSelected1: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure vstObjectsChecked(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure btnOKClick(Sender: TObject);
    procedure rgViewDirectionClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure acCheckSelectedClick(Sender: TObject);
    procedure acUncheckSelectedClick(Sender: TObject);
    procedure btnEditFeatureClick(Sender: TObject);
  private
    { TODO : Consider replacing this with a TScreenObjectList}
    //
    FListOfScreenObjects: TList;
    procedure SetData;
    procedure UpdateScreenObjectList;
    procedure UpdateCheckedNodes;
    { Private declarations }
  protected
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean; override;
    procedure HandleChecked(AScreenObject: TScreenObject); override;
    procedure HandleUnchecked(AScreenObject: TScreenObject); override;
    function CanSelect(ScreenObject: TScreenObject): boolean; override;
    procedure HandleCheckChange(Node: PVirtualNode; Sender: TBaseVirtualTree); override;
  public
    destructor Destroy; override;
    { Public declarations }
  end;

implementation

uses
  frmGoPhastUnit, frmScreenObjectPropertiesUnit, GoPhastTypes,
  UndoItemsScreenObjects, frmEditFeatureFormulaUnit;

{$R *.dfm}

procedure TfrmSelectObjectsForEditing.btnDeleteClick(Sender: TObject);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  ScreenObject: TScreenObject;
  Undo: TUndoDeleteScreenObjects;
begin
  inherited;
  if FListOfScreenObjects.Count > 0 then
  begin
    ListOfScreenObjects:= TScreenObjectList.Create;
    try
      ListOfScreenObjects.Capacity := FListOfScreenObjects.Count;
      for Index := 0 to FListOfScreenObjects.Count - 1 do
      begin
        ScreenObject := FListOfScreenObjects[Index];
        ListOfScreenObjects.Add(ScreenObject);
      end;

      Undo := TUndoDeleteScreenObjects.Create(ListOfScreenObjects);
      frmGoPhast.UndoStack.Submit(Undo);
    finally
      ListOfScreenObjects.Free;
    end;
  end;
end;

procedure TfrmSelectObjectsForEditing.btnEditFeatureClick(Sender: TObject);
var
  ScreenObjects: TScreenObjectList;
  index: Integer;
  frmEditFeatureFormula: TfrmEditFeatureFormula;
begin
  inherited;
  if not frmGoPhast.CanEdit then Exit;
  frmGoPhast.CanEdit := False;
  try
    if FListOfScreenObjects.Count > 0 then
    begin
      ScreenObjects := TScreenObjectList.Create;
      try
        ScreenObjects.Capacity := FListOfScreenObjects.Count;
        for index := 0 to FListOfScreenObjects.Count - 1 do
        begin
          ScreenObjects.Add(
            TScreenObject(FListOfScreenObjects[index]));
        end;
        frmEditFeatureFormula := TfrmEditFeatureFormula.Create(nil);
        try
          frmEditFeatureFormula.GetData(ScreenObjects);
          frmEditFeatureFormula.ShowModal;
        finally
          frmEditFeatureFormula.Free
        end;
      finally
        ScreenObjects.Free;
      end;
    end;
  finally
    frmGoPhast.CanEdit := True;
  end;

end;

procedure TfrmSelectObjectsForEditing.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

function TfrmSelectObjectsForEditing.CanSelect(
  ScreenObject: TScreenObject): boolean;
begin
  result := Ord(ScreenObject.ViewDirection) = rgViewDirection.ItemIndex;
end;

destructor TfrmSelectObjectsForEditing.Destroy;
begin
  FListOfScreenObjects.Free;
  inherited;
end;

procedure TfrmSelectObjectsForEditing.acUncheckSelectedClick(Sender: TObject);
begin
  inherited;
  vstObjects.BeginUpdate;
  try
    UpdateStringTreeViewCheckedState(vstObjects, vstObjects.RootNode, csUnCheckedNormal);
//    SetStateOfMultipleNodes(vstObjects.RootNode, csCheckedNormal);
  finally
    vstObjects.EndUpdate;
  end;

end;

procedure TfrmSelectObjectsForEditing.UpdateScreenObjectList;
var
  ScreenObject: TScreenObject;
  Index: Integer;
begin
  FListOfScreenObjects.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    if ScreenObject.Selected
      and (Ord(ScreenObject.ViewDirection) = rgViewDirection.ItemIndex) then
    begin
      FListOfScreenObjects.Add(ScreenObject);
    end;
  end;
end;

procedure TfrmSelectObjectsForEditing.FormCreate(Sender: TObject);
begin
  FListOfScreenObjects:= TList.Create;
  inherited;
  UpdateScreenObjectList;
  GetData;
  btnEditFeature.Enabled := frmGoPhast.ModelSelection in
    (ModflowSelection + SutraSelection);
end;

procedure TfrmSelectObjectsForEditing.UpdateCheckedNodes;
var
  ANode: PVirtualNode;
  ChildNode: PVirtualNode;
  NextNode: PVirtualNode;
  ParentNode: PVirtualNode;
  NodeData: PMyRec;
  ObjectIndex: integer;
  AScreenObject: TScreenObject;
  TreeStates: set of TCheckState;
  procedure AssignParentMixed(ParentNode: PVirtualNode);
  begin
    while (ParentNode <> nil)
      and (vstObjects.CheckState[ParentNode] <> csMixedNormal) do
    begin
      vstObjects.CheckState[ParentNode] := csMixedNormal;
      ParentNode := vstObjects.NodeParent[ParentNode];
    end;
  end;
  procedure AssignParentNotMixed(ParentNode: PVirtualNode);
  var
    ChildNode: PVirtualNode;
    FirstState: TCheckState;
  begin
    if ParentNode = nil then
    begin
      Exit;
    end;
    ChildNode := vstObjects.GetFirstChild(ParentNode);
    Assert(ChildNode <> nil);
    FirstState := vstObjects.CheckState[ChildNode];
    if FirstState = csMixedNormal then
    begin
      if vstObjects.CheckState[ParentNode] <> csMixedNormal then
      begin
        vstObjects.CheckState[ParentNode] := csMixedNormal;
        AssignParentMixed(ParentNode);
      end;
    end
    else
    begin
      while ChildNode <> nil do
      begin
        if vstObjects.CheckState[ChildNode] <> FirstState then
        begin
//          vstObjects.CheckState[ParentNode] := csMixedNormal;
          AssignParentMixed(ParentNode);
          Exit;
        end;
        ChildNode := vstObjects.GetNextSibling(ChildNode);
      end;
      if vstObjects.CheckState[ParentNode] <> FirstState then
      begin
        vstObjects.CheckState[ParentNode] := FirstState;
        ParentNode := vstObjects.NodeParent[ParentNode];
        AssignParentNotMixed(ParentNode);
      end;
    end;
  end;
begin
  FSettingData := True;
  try
    ANode := vstObjects.GetFirst;
    while ANode <> nil do
    begin
      NodeData := vstObjects.GetNodeData(ANode);
      if (NodeData.ScreenObjects <> nil) and (NodeData.ScreenObjects.Count > 0) then
      begin
        TreeStates := [];
        ObjectIndex := 0;
        ChildNode := vstObjects.GetFirstChild(ANode);
        while ChildNode <> nil do
        begin
          AScreenObject := NodeData.ScreenObjects[ObjectIndex];
          if ShouldCheckBoxBeChecked(AScreenObject) then
          begin
            vstObjects.CheckState[ChildNode] := csCheckedNormal;
            Include(TreeStates, csCheckedNormal);
          end
          else
          begin
            vstObjects.CheckState[ChildNode] := csUncheckedNormal;
            Include(TreeStates, csUncheckedNormal);
          end;

          Inc(ObjectIndex);
          ChildNode := vstObjects.GetNextSibling(ChildNode);
        end;

        if (csCheckedNormal in TreeStates) and (csUncheckedNormal in TreeStates)  then
        begin
          vstObjects.CheckState[ANode] := csMixedNormal;
          ParentNode := vstObjects.NodeParent[ANode];
          AssignParentMixed(ParentNode);
        end
        else
        begin
          if (csCheckedNormal in TreeStates) then
          begin
            vstObjects.CheckState[ANode] := csCheckedNormal;
          end
          else if (csUncheckedNormal in TreeStates) then
          begin
            vstObjects.CheckState[ANode] := csUncheckedNormal;
          end;
          ParentNode := vstObjects.NodeParent[ANode];
          AssignParentNotMixed(ParentNode);
        end;

        NextNode := vstObjects.GetNextSibling(ANode);
        if NextNode = nil then
        begin
          ANode := vstObjects.GetNext(ANode);
        end
        else
        begin
          ANode := NextNode;
        end;
      end
      else
      begin
        ANode := vstObjects.GetNext(ANode);
      end;
    end;
  finally
    FSettingData := False;
  end;
end;

procedure TfrmSelectObjectsForEditing.HandleCheckChange(Node: PVirtualNode;
  Sender: TBaseVirtualTree);
begin
  inherited;

end;

procedure TfrmSelectObjectsForEditing.HandleChecked(
  AScreenObject: TScreenObject);
begin
  if FListOfScreenObjects.IndexOf(AScreenObject)< 0 then
  begin
    FListOfScreenObjects.Add(AScreenObject)
  end;
end;

procedure TfrmSelectObjectsForEditing.HandleUnchecked(
  AScreenObject: TScreenObject);
begin
  FListOfScreenObjects.Remove(AScreenObject);
end;

procedure TfrmSelectObjectsForEditing.acCheckSelectedClick(Sender: TObject);
begin
  inherited;
  vstObjects.BeginUpdate;
  try
    UpdateStringTreeViewCheckedState(vstObjects, vstObjects.RootNode, csCheckedNormal);
//    SetStateOfMultipleNodes(vstObjects.RootNode, csCheckedNormal);
  finally
    vstObjects.EndUpdate;
  end;

end;

procedure TfrmSelectObjectsForEditing.rgViewDirectionClick(Sender: TObject);
begin
  inherited;
  if FListOfScreenObjects <> nil then
  begin
    UpdateScreenObjectList;
    GetData;
  end;
end;

procedure TfrmSelectObjectsForEditing.SetData;
begin
  if not frmGoPhast.CanEdit then Exit;
  frmGoPhast.CanEdit := False;
  try
    if FListOfScreenObjects.Count > 0 then
    begin
      Assert(frmScreenObjectProperties <> nil);

      frmScreenObjectProperties.GetDataForMultipleScreenObjects(
        FListOfScreenObjects);
      frmScreenObjectProperties.ShowModal
    end;
  finally
    frmGoPhast.CanEdit := True;
  end;
end;

function TfrmSelectObjectsForEditing.ShouldCheckBoxBeChecked(
  ScreenObject: TScreenObject): boolean;
begin
  result := FListOfScreenObjects.IndexOf(ScreenObject) >= 0;
end;

procedure TfrmSelectObjectsForEditing.vstObjectsChecked(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
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
    HandleCheckChange(Node, Sender);
  finally
    Sender.EndUpdate;
    FSettingData := False;
    Screen.Cursor := crDefault;
  end;
  UpdateCheckedNodes;
//  vstObjects.InitializedNodes;
end;

end.
