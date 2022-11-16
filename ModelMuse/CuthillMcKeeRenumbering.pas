unit CuthillMcKeeRenumbering;

// Modified from
// http://ciprian-zavoianu.blogspot.com/2009/01/project-bandwidth-reduction_18.html

// E. Cuthill and J. McKee. Reducing the bandwidth of sparse symmetric matrices
// In Proc. 24th Nat. Conf. ACM, pages 157–172, 1969.

interface

uses System.UITypes,
  Winapi.Windows, System.Types, MeshRenumberingTypes, System.SysUtils, Vcl.Dialogs;

type
  TNode = record
    InitialLabel : integer;
    NewLabel : integer;
    Neighbours : TIntegerDynArray;
  end;

  TNodeArray = array of TNode;

procedure RenumberMesh(Mesh: IMesh);

implementation

resourcestring
  StrErrorRenumberingTh = 'Error renumbering the mesh. Check that the mesh i' +
  's valid.';

procedure InitializeVariables(var LevStr: TNodeArray;
  var PermutationVector: TIntegerDynArray; var Selected: TIntegerDynArray);
var
  i: Integer;
begin
  for i := 0 to Length(LevStr) - 1 do
  begin
    Selected[i] := 0;
    PermutationVector[i] := -1;
  end;
end;


procedure SortLevelStructure(var MinIndex: Integer; LevStr: TNodeArray);
var
  k: Integer;
  aux: Integer;
  i: Integer;
  min: Integer;
  j: Integer;
begin
  //sort the neighbour's list in LevStr in increasing order of degree
  min := Length(LevStr);
  MinIndex := -1;
  for i := 0 to Length(LevStr) - 1 do
  begin
    for j := 0 to Length(LevStr[i].Neighbours) - 2 do
    begin
      for k := j + 1 to Length(LevStr[i].Neighbours) - 1 do
      begin
        if Length(LevStr[LevStr[i].Neighbours[j]].Neighbours)
          > Length(LevStr[LevStr[i].Neighbours[k]].Neighbours) then
        begin
          aux := LevStr[i].Neighbours[j];
          LevStr[i].Neighbours[j] := LevStr[i].Neighbours[k];
          LevStr[i].Neighbours[k] := aux;
        end;
      end;
    end;
    //determine a good starting node
    if Length(LevStr[i].Neighbours) < min then
    begin
      min := Length(LevStr[i].Neighbours);
      MinIndex := i;
    end;
  end;
end;

procedure DoCuthillMcKee(var PermutationVector: TIntegerDynArray;
  Selected: TIntegerDynArray; var LevStr: TNodeArray; MinIndex: Integer);
var
  min: Integer;
  RStartIndex: Integer;
  REndIndex: Integer;
  Unconnected: Boolean;
  i: Integer;
begin
  RStartIndex := 0;
  REndIndex := 0;
  Selected[MinIndex] := 1;
  PermutationVector[RStartIndex] := MinIndex;
  Inc(REndIndex);
  LevStr[MinIndex].NewLabel := RStartIndex;
  //perform Cuthill - McKee on every connected component of the graph stored in LevStr
  repeat
    Unconnected := false;
    while (REndIndex < Length(LevStr)) do
    begin
      for i := 0 to Length(LevStr[PermutationVector[RStartIndex]].Neighbours) - 1 do
        if Selected[LevStr[PermutationVector[RStartIndex]].Neighbours[i]] = 0 then
        begin
          Selected[LevStr[PermutationVector[RStartIndex]].Neighbours[i]] := 1;
          Inc(REndIndex);
          LevStr[LevStr[PermutationVector[RStartIndex]].Neighbours[i]].NewLabel := REndIndex - 1;
          PermutationVector[REndIndex - 1] := LevStr[PermutationVector[RStartIndex]].Neighbours[i];
        end;
      Inc(RStartIndex);
      if RStartIndex > (REndIndex - 1) then
      begin
        Unconnected := true;
        Break;
      end;
    end;
    if Unconnected then
    begin
      MinIndex := -1;
      min := Length(LevStr);
      for i := 0 to Length(LevStr) - 1 do
      begin
        if Selected[LevStr[i].InitialLabel] = 0 then
          if Length(LevStr[i].Neighbours) < min then
          begin
            min := Length(LevStr[i].Neighbours);
            MinIndex := i;
          end;
      end;
      PermutationVector[RStartIndex] := MinIndex;
      Inc(REndIndex);
      LevStr[MinIndex].NewLabel := RStartIndex;
      Selected[MinIndex] := 1;
    end;
  until not (Unconnected);
end;

procedure RenumberMesh(Mesh: IMesh);
var
  LevStr: TNodeArray;
  PermutationVector: TIntegerDynArray;
  Selected: TIntegerDynArray;
  ElementIndex: Integer;
  AnElement: IElement;
  OuterNodeIndex: Integer;
  Node1: INode;
  InnerNodeIndex: Integer;
  Node2: INode;
  MinIndex: Integer;
  NodeIndex: Integer;
  ANode: INode;
  ElIndex: Integer;
  NodeList: TINodeList;
  procedure AddNewLink(I, J: Integer);
  var
    NodeIndex: Integer;
    ArrayLength: Integer;
  begin
    ArrayLength := Length(LevStr[I].Neighbours);
    for NodeIndex := 0 to ArrayLength - 1 do
    begin
      if LevStr[I].Neighbours[NodeIndex] = J then
      begin
        Exit;
      end;
    end;
    SetLength(LevStr[I].Neighbours, ArrayLength+1);
    LevStr[I].Neighbours[ArrayLength] := J;
  end;
begin
  try
    if (Mesh.ElementCount = 0) then
    begin
      Exit;
    end;
    SetLength(LevStr, Mesh.NodeCount);
    SetLength(PermutationVector, Mesh.NodeCount);
    SetLength(Selected, Mesh.NodeCount);

    InitializeVariables(LevStr, PermutationVector, Selected);

    for ElementIndex := 0 to Mesh.ElementCount - 1 do
    begin
      AnElement := Mesh.Elements[ElementIndex];
      AnElement.BypassUpdate := True;
      AnElement.ElementNumber := -1;
      AnElement.BypassUpdate := False;
    end;

    for NodeIndex := 0 to Mesh.NodeCount - 1 do
    begin
      ANode := Mesh.Nodes[NodeIndex];
      ANode.BypassUpdate := True;
      ANode.NodeNumber := NodeIndex;
      ANode.BypassUpdate := False;
      Assert(ANode.ActiveElementCount > 0);
    end;

    for ElementIndex := 0 to Mesh.ElementCount - 1 do
    begin
      AnElement := Mesh.Elements[ElementIndex];
      for OuterNodeIndex := 0 to AnElement.NodeCount - 2 do
      begin
        Node1 := AnElement.Nodes[OuterNodeIndex];
        LevStr[OuterNodeIndex].InitialLabel := OuterNodeIndex;
        LevStr[OuterNodeIndex].NewLabel := 0;
        AddNewLink(Node1.NodeNumber, Node1.NodeNumber);
        for InnerNodeIndex := OuterNodeIndex + 1 to AnElement.NodeCount - 1 do
        begin
          Node2 := AnElement.Nodes[InnerNodeIndex];
          AddNewLink(Node1.NodeNumber, Node2.NodeNumber);
          AddNewLink(Node2.NodeNumber, Node1.NodeNumber);
        end;
      end;
    end;

    SortLevelStructure(MinIndex, LevStr);
    DoCuthillMcKee(PermutationVector, Selected, LevStr, MinIndex);

    NodeList := TINodeList.Create;
    try
      ElementIndex := 0;
      for NodeIndex := 0 to Mesh.NodeCount - 1 do
      begin
        ANode := Mesh.Nodes[NodeIndex];
        ANode.BypassUpdate := True;
        ANode.NodeNumber := LevStr[NodeIndex].NewLabel;
        ANode.BypassUpdate := False;
        NodeList.Add(ANode);
      end;

      NodeList.Sort(TINodeComparer.Construct(
        function(const L, R: INode): Integer
        begin
          result := L.NodeNumber - R.NodeNumber;
        end));

      for NodeIndex := 0 to NodeList.Count - 1 do
      begin
        ANode := NodeList[NodeIndex];
        for ElIndex := 0 to ANode.ActiveElementCount - 1 do
        begin
          AnElement := ANode.ActiveElements[ElIndex];
          if AnElement.ElementNumber < 0 then
          begin
            AnElement.BypassUpdate := True;
            AnElement.ElementNumber := ElementIndex;
            AnElement.BypassUpdate := False;
            Inc(ElementIndex);
          end;
        end;
      end;
    finally
      NodeList.Free;
    end;
  except on ERangeError do
    begin
      Beep;
      MessageDlg(StrErrorRenumberingTh, mtError, [mbOK], 0);
    end;
  end;
end;

end.
