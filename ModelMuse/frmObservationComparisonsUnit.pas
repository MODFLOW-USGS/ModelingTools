unit frmObservationComparisonsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Buttons, Vcl.ExtCtrls, SsButtonEd, RbwStringTreeCombo,
  System.Generics.Collections, VirtualTrees;

type
  TfrmObservationComparisons = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnHelp: TBitBtn;
    treecomboInPlaceEditor: TRbwStringTreeCombo;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure treecomboInPlaceEditorTreeFreeNode(Sender: TBaseVirtualTree;
      Node: PVirtualNode);
    procedure treecomboInPlaceEditorTreeGetNodeDataSize(
      Sender: TBaseVirtualTree; var NodeDataSize: Integer);
    procedure treecomboInPlaceEditorTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: string);
    procedure treecomboInPlaceEditorTreeInitNode(Sender: TBaseVirtualTree;
      ParentNode, Node: PVirtualNode;
      var InitialStates: TVirtualNodeInitStates);
  private
    { Private declarations }
    FObsDictionary: TDictionary<string, PVirtualNode>;
    procedure GetData;
    procedure InitializeInPlaceEditor;
  public
    { Public declarations }
  end;

var
  frmObservationComparisons: TfrmObservationComparisons;

implementation

uses
  frmGoPhastUnit, ScreenObjectUnit, ModflowMnw2Unit, frmErrorsAndWarningsUnit;

resourcestring
  StrTheFollowingObject = 'The following objects contain duplicate observati' +
  'on names';
  StrObject0sObse = 'Object: "%0:s"; Observation name: "%1:s".';

{$R *.dfm}

type
  TObsTreeItem = class(TObject)
    ObsTypeName: string;
    ScreenObject: TScreenObject;
    Obs: TMnw2ObsItem;
    function Caption: string;
    function Key: string; overload;
    class function Key(ObsTypeName: string; ScreenObject: TScreenObject; Obs: TMnw2ObsItem): string; overload;
  end;
  PObsTreeItem = ^TObsTreeItem;


{ TfrmObservationComparisons }

procedure TfrmObservationComparisons.FormCreate(Sender: TObject);
begin
  inherited;
  FObsDictionary := TDictionary<string, PVirtualNode>.Create;

  GetData;
end;

procedure TfrmObservationComparisons.FormDestroy(Sender: TObject);
begin
  FObsDictionary.Free;
  inherited;
end;

procedure TfrmObservationComparisons.GetData;
begin
  InitializeInPlaceEditor;
  if ModalResult = mrNone then
  begin

  end;
end;

procedure TfrmObservationComparisons.InitializeInPlaceEditor;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  ObsIndex: Integer;
  AnObs: TMnw2ObsItem;
  Mnw2Observations: TMnw2Observations;
  UsedTypes: TStringList;
  TIndex: Integer;
  ObsTypeName: string;
  ObsTreeItem: TObsTreeItem;
  ANode: PVirtualNode;
  ObsTypeNode: PVirtualNode;
  ParentNodeKey: string;
  ParentNode: PVirtualNode;
  NodeCaption: string;
  ObsNode: PVirtualNode;
  ErrorsAdded: Boolean;
begin
  ErrorsAdded := False;
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel,
    StrTheFollowingObject);
  treecomboInPlaceEditor.Tree.BeginUpdate;
  UsedTypes := TStringList.Create;
  try
    UsedTypes.Sorted := True;
    UsedTypes.Duplicates := dupIgnore;
    for ObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if (AScreenObject.ModflowMnw2Boundary <> nil)
        and AScreenObject.ModflowMnw2Boundary.Used
        and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
      begin
        Mnw2Observations := AScreenObject.ModflowMnw2Boundary.Observations;
        for ObsIndex := 0 to Mnw2Observations.Count - 1 do
        begin
          AnObs := Mnw2Observations[ObsIndex];
          UsedTypes.Add(AnObs.ObservationType);
        end;
      end;
    end;

    for TIndex := 0 to UsedTypes.Count - 1 do
    begin
      ObsTypeName := UsedTypes[TIndex];
      ObsTreeItem := TObsTreeItem.Create;
//      ObsTreeItem.Caption := ObsTypeName;
      ObsTreeItem.Obs := nil;
      ObsTreeItem.ObsTypeName := ObsTypeName;
      ObsTreeItem.ScreenObject := nil;
      ANode := treecomboInPlaceEditor.Tree.AddChild(nil, ObsTreeItem);
      FObsDictionary.Add(ObsTypeName, ANode);
    end;

    for ObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ObjectIndex];
      if AScreenObject.Deleted then
      begin
        Continue;
      end;
      if (AScreenObject.ModflowMnw2Boundary <> nil)
        and AScreenObject.ModflowMnw2Boundary.Used
        and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
      begin
        Mnw2Observations := AScreenObject.ModflowMnw2Boundary.Observations;
        for ObsIndex := 0 to Mnw2Observations.Count - 1 do
        begin
          AnObs := Mnw2Observations[ObsIndex];

          ParentNodeKey :=
            TObsTreeItem.Key(AnObs.ObservationType, AScreenObject, nil);
          if not FObsDictionary.TryGetValue(ParentNodeKey, ParentNode) then
          begin
            ObsTypeNode := FObsDictionary[AnObs.ObservationType];

            ObsTreeItem := TObsTreeItem.Create;
//            ObsTreeItem.Caption := ParentNodeKey;
            ObsTreeItem.Obs := nil;
            ObsTreeItem.ObsTypeName := ObsTypeName;
            ObsTreeItem.ScreenObject := AScreenObject;
            ParentNode := treecomboInPlaceEditor.Tree.AddChild(ObsTypeNode, ObsTreeItem);
            FObsDictionary.Add(ParentNodeKey, ParentNode);
          end;

          NodeCaption := TObsTreeItem.Key(AnObs.ObservationType, AScreenObject, AnObs);

          if FObsDictionary.TryGetValue(NodeCaption, ObsNode) then
          begin
            frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
              StrTheFollowingObject,
              Format(StrObject0sObse, [AScreenObject.Name, AnObs.Name]),
              AScreenObject);
            ErrorsAdded := True;
          end
          else
          begin
            ObsTreeItem := TObsTreeItem.Create;
//            ObsTreeItem.Caption := NodeCaption;
            ObsTreeItem.Obs := AnObs;
            ObsTreeItem.ObsTypeName := ObsTypeName;
            ObsTreeItem.ScreenObject := AScreenObject;
            ANode := treecomboInPlaceEditor.Tree.AddChild(ParentNode, ObsTreeItem);
            FObsDictionary.Add(NodeCaption, ANode);
          end;

        end;
      end;
    end;

  finally
    UsedTypes.Free;
    treecomboInPlaceEditor.Tree.EndUpdate;
  end;

  if ErrorsAdded then
  begin
    ModalResult := mrCancel;
    frmErrorsAndWarnings.ShowAfterDelay;
  end;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeFreeNode(
  Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ObsTreeItem: PObsTreeItem;
begin
  ObsTreeItem := Sender.GetNodeData(Node);
  ObsTreeItem^.Free;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeGetNodeDataSize(
  Sender: TBaseVirtualTree; var NodeDataSize: Integer);
begin
  inherited;
  NodeDataSize := SizeOf(TObsTreeItem);
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: string);
var
  NodeData: PObsTreeItem;
begin
  NodeData := Sender.GetNodeData(Node);
  CellText := NodeData^.Caption;
end;

procedure TfrmObservationComparisons.treecomboInPlaceEditorTreeInitNode(
  Sender: TBaseVirtualTree; ParentNode, Node: PVirtualNode;
  var InitialStates: TVirtualNodeInitStates);
var
  ObsTreeItem: PObsTreeItem;
begin
  ObsTreeItem := Sender.GetNodeData(Node);
  if ObsTreeItem^.Obs <> nil then
  begin
    Assert(ObsTreeItem^.ScreenObject <> nil);
//    ObsTreeItem^.Caption := Format('%0:s.%1:s.%2:s',
//      [ObsTreeItem^.ObsTypeName, ObsTreeItem^.ScreenObject.Name,
//      ObsTreeItem^.Obs.Name]);
  end
  else if ObsTreeItem^.ScreenObject <> nil then
  begin
//    ObsTreeItem^.Caption := Format('%0:s.%1:s',
//      [ObsTreeItem^.ObsTypeName, ObsTreeItem^.ScreenObject.Name]);
  end
  else
  begin
//    ObsTreeItem^.Caption := ObsTreeItem^.ObsTypeName;
  end;
end;

{ TObsTreeItem }

function TObsTreeItem.Caption: string;
begin
  if Obs <> nil then
  begin
    Result := Obs.Name;
  end
  else if ScreenObject <> nil then
  begin
    Result := ScreenObject.Name;
  end
  else
  begin
    Result := ObsTypeName;
  end;
end;

function TObsTreeItem.Key: string;
begin
  result := ObsTypeName;
  if ScreenObject <> nil then
  begin
    result := result + '.' + ScreenObject.Name;
    if Obs <> nil then
    begin
      result := result + '.' + Obs.Name;
    end;
  end;
end;

class function TObsTreeItem.Key(ObsTypeName: string;
  ScreenObject: TScreenObject; Obs: TMnw2ObsItem): string;
begin
  result := ObsTypeName;
  if ScreenObject <> nil then
  begin
    result := result + '.' + ScreenObject.Name;
    if Obs <> nil then
    begin
      result := result + '.' + Obs.Name;
    end;
  end;
end;

end.


