unit frameParentChildUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  JvExComCtrls, JvComCtrls;

type
  // @name is used for rearranging parent and child objects.
  // Constraints:
  // Each child must belong to exactly one parent.
  // Children can not also be parents.
  TframeParentChild = class(TFrame)
    tvTree: TJvTreeView;
    procedure tvTreeDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvTreeDragDrop(Sender, Source: TObject; X, Y: Integer);
  private
    FDragging: Boolean;
    { Private declarations }
  public
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
  MovedItem: TTreeNode;
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
              MovedItem := tvTree.Selections[index];

              if (MovedItem.Parent <> nil) and (MovedItem <> AnItem) then
              begin
                NodeList.Add(MovedItem);
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
              MovedItem := NodeList[index];
              MovedItem.Selected := False;
            end;

            for index := 0 to NodeList.Count - 1 do
            begin
              MovedItem := NodeList[index];
              MovedItem.MoveTo(AnItem, AttachMode);
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
