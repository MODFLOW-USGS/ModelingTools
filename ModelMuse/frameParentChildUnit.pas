unit frameParentChildUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls;

type
  TOnMoveNodeEvent = procedure (Sender: TObject; Node: TTreeNode) of object;

  // @name is used for rearranging parent and child objects.
  // Constraints:
  // Each child must belong to exactly one parent.
  // Children can not also be parents.
  TframeParentChild = class(TFrame)
    tvTree: TTreeView;
    procedure tvTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FDragging: Boolean;
    FOnMoveNode: TOnMoveNodeEvent;
    { Private declarations }
  public
    property OnMoveNode: TOnMoveNodeEvent read FOnMoveNode write FOnMoveNode;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeParentChild.tvTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
  index: Integer;
  MovedNode: TTreeNode;
  NodeList: TList;
begin
  if FDragging then
  begin
    Exit;
  end;
  FDragging := True;
  try
    if Sender <> Source then Exit;
    if tvTree.SelectionCount < 1 then Exit;
    HT := tvTree.GetHitTestInfoAt(X, Y);
    AnItem := tvTree.GetNodeAt(X, Y);
    if (AnItem <> nil)  then
    begin
      if (HT - [htOnItem, htOnIcon, htOnRight] <> HT) then
      begin
        tvTree.Items.BeginUpdate;
        try
          NodeList:= TList.Create;
          try
            for index := 0 to tvTree.SelectionCount - 1 do
            begin
              MovedNode := tvTree.Selections[index];

              if (MovedNode.Parent <> nil) and (MovedNode <> AnItem) then
              begin
                NodeList.Add(MovedNode);
              end;
            end;

            if AnItem.Parent <> nil then
            begin
              AttachMode := naInsert;
            end
            else
            begin
              AttachMode := naAddChild;
            end;

            for index := 0 to NodeList.Count - 1 do
            begin
              MovedNode := NodeList[index];
              MovedNode.Selected := False;
            end;

            for index := 0 to NodeList.Count - 1 do
            begin
              MovedNode := NodeList[index];
              MovedNode.MoveTo(AnItem, AttachMode);
              if Assigned(OnMoveNode) then
              begin
                OnMoveNode(self, MovedNode);
              end;
            end;

            tvTree.Select(NodeList);

          finally
            NodeList.Free;
          end;
        finally
          tvTree.Items.EndUpdate;
        end;
      end;
    end;
  finally
    FDragging := False;
  end;
end;

procedure TframeParentChild.tvTreeDragOver(Sender, Source: TObject; X, Y: Integer;
  State: TDragState; var Accept: Boolean);
var
  Src, Dst: TTreeNode;
begin
  Src := tvTree.Selected;
  Dst := tvTree.GetNodeAt(X,Y);
  Accept := Assigned(Dst) and (Src<>Dst);
end;

end.
