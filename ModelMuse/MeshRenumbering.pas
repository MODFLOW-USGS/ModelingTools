{
  Based on:
  Sloan, S.W., and Randolph, M.F., 1983. Automatic element reordering for
  finite element analysis with frontal solution schemes. International
  Journal for Numerical Methods in Engineering. 19: 1153-1181.

  There is a small change in TNodeHandler.AssignActiveNodeIncrement
  from what is described in Sloan and Randolph.
}

unit MeshRenumbering;

interface

uses
  Generics.Collections, Generics.Defaults, FastGEO, MeshRenumberingTypes;

procedure RenumberMesh(Mesh: IMesh);

implementation

type
  TNodeList = TList<INode>;
  TElementList = TList<IElement>;

  TElementHandler = class(TObject)
    FElement: IElement;
    FIntegerList: TList<Integer>;
    constructor Create(Element: IElement);
    destructor Destroy; override;
  end;

  TElementHandlerObjectList = TObjectList<TElementHandler>;

  TElementHandlerComparer = TComparer<TElementHandler>;

  TNodeHandler = class;

  TNodeHandlerList = TList<TNodeHandler>;
  TNodeHandlerObjectList = TObjectList<TNodeHandler>;

  TNodeHandler = class(TObject)
    FNode: INode;
    FAdjacentNodes: TNodeHandlerList;
    FActive: Boolean;
    FAssigned: Boolean;
    FActiveNodeIncrement: integer;
    FOrderingEfficiency: Int64;
    FWidth: integer;
    FDepth: integer;
    FLevel: integer;
    FMaxFrontWidth: integer;
    constructor Create(Node: INode);
    destructor Destroy; override;
  private
    procedure GetAdjacentNodes(NodeHandlers: TNodeHandlerObjectList);
    procedure AssignActiveNodeIncrement;
  end;

procedure GetNextNodeToAdd(ActiveNodes: TNodeHandlerList;
  var NextNodeIndex: integer);
var
  NextNodeHandler: TNodeHandler;
  NodeIndex: integer;
  NodeHandler: TNodeHandler;
begin
  for NodeIndex := 0 to ActiveNodes.Count - 1 do
  begin
    NodeHandler := ActiveNodes[NodeIndex];
    NodeHandler.AssignActiveNodeIncrement;
  end;

  // Find the node that will add the fewest nodes to the active nodes.
  NextNodeHandler := nil;
  NextNodeIndex := -1;
  for NodeIndex := 0 to ActiveNodes.Count - 1 do
  begin
    NodeHandler := ActiveNodes[NodeIndex];
    if NextNodeHandler = nil then
    begin
      NextNodeHandler := NodeHandler;
      NextNodeIndex := NodeIndex;
    end
    else
    begin
      if NodeHandler.FActiveNodeIncrement <
        NextNodeHandler.FActiveNodeIncrement then
      begin
        NextNodeHandler := NodeHandler;
        NextNodeIndex := NodeIndex;
      end
    end;
  end;
end;

procedure GetPeripheralNodes(StartingNodes, AllNodeHandlers: TNodeHandlerList;
  Root: TNodeHandler);
var
  NodeQueue: TNodeHandlerList;
  NodeIndex: integer;
  NodeHandler: TNodeHandler;
  MaxWidth: integer;
  CurrentLevel: integer;
  CurrentWidth: integer;
  InnerNodeIndex: integer;
  AnotherNodeHandler: TNodeHandler;
  NewNodes: TNodeHandlerList;
begin
  for NodeIndex := 0 to AllNodeHandlers.Count - 1 do
  begin
    NodeHandler := AllNodeHandlers[NodeIndex];
    NodeHandler.FLevel := -1;
  end;

  NewNodes := TNodeHandlerList.Create;
  try
    if StartingNodes.IndexOf(Root) < 0 then
    begin
      StartingNodes.Add(Root);
    end;
    Root.FLevel := 0;
    NodeQueue := TNodeHandlerList.Create;
    try
      for NodeIndex := 0 to Root.FAdjacentNodes.Count - 1 do
      begin
        NodeHandler := Root.FAdjacentNodes[NodeIndex];
        NodeQueue.Add(NodeHandler);
        NodeHandler.FLevel := 1;
      end;
      MaxWidth := NodeQueue.Count;
      CurrentLevel := 1;
      CurrentWidth := MaxWidth;

      NodeIndex := 0;
      while NodeIndex < NodeQueue.Count do
      begin
        NodeHandler := NodeQueue[NodeIndex];
        for InnerNodeIndex := 0 to NodeHandler.FAdjacentNodes.Count - 1 do
        begin
          AnotherNodeHandler := NodeHandler.FAdjacentNodes[InnerNodeIndex];
          if AnotherNodeHandler.FLevel = -1 then
          begin
            NodeQueue.Add(AnotherNodeHandler);
            AnotherNodeHandler.FLevel := NodeHandler.FLevel + 1;
            if AnotherNodeHandler.FLevel = CurrentLevel then
            begin
              Inc(CurrentWidth);
            end
            else
            begin
              CurrentWidth := 1;
              CurrentLevel := AnotherNodeHandler.FLevel;
            end;
            if CurrentWidth > MaxWidth then
            begin
              MaxWidth := CurrentWidth;
            end;
          end;
        end;

        Inc(NodeIndex);
      end;
      Root.FWidth := MaxWidth;
      Root.FDepth := CurrentLevel;

      for NodeIndex := NodeQueue.Count - 1 downto 0 do
      begin
        NodeHandler := NodeQueue[NodeIndex];
        if NodeHandler.FLevel = CurrentLevel then
        begin
          if StartingNodes.IndexOf(NodeHandler) < 0 then
          begin
            StartingNodes.Add(NodeHandler);
            NewNodes.Add(NodeHandler);
          end;
        end
        else
        begin
          break;
        end;
      end;

    finally
      NodeQueue.Free;
    end;

    for NodeIndex := 0 to NewNodes.Count - 1 do
    begin
      NodeHandler := NewNodes[NodeIndex];
      GetPeripheralNodes(StartingNodes, AllNodeHandlers, NodeHandler);
    end;
  finally
    NewNodes.Free;
  end;

end;

function CompareNodes(Node1, Node2: TNodeHandler): integer;
begin
  result := Node1.FDepth - Node2.FDepth;
  if result = 0 then
  begin
    result := Node2.FWidth - Node1.FWidth;
  end;
end;

procedure UpdateStartingNodes(StartingNodes, AllNodeHandlers: TNodeHandlerList;
  Root: TNodeHandler);
var
  NodeIndex: integer;
  BestNode: TNodeHandler;
  NodeHandler: TNodeHandler;
  CompareResult: integer;
  InnerNodeIndex: integer;
  NewPositionIndex: integer;
begin
  GetPeripheralNodes(StartingNodes, AllNodeHandlers, Root);
  BestNode := StartingNodes[StartingNodes.Count - 1];
  for NodeIndex := StartingNodes.Count - 2 downto 0 do
  begin
    NodeHandler := StartingNodes[NodeIndex];
    CompareResult := CompareNodes(BestNode, NodeHandler);
    if CompareResult > 0 then
    begin
      // BestNode is still best
      StartingNodes[NodeIndex] := nil;
    end
    else if CompareResult < 0 then
    begin
      BestNode := NodeHandler;
      for InnerNodeIndex := StartingNodes.Count - 1 downto NodeIndex + 1 do
      begin
        StartingNodes.Delete(InnerNodeIndex)
      end;
    end;
  end;
  NewPositionIndex := 0;
  for NodeIndex := 0 to StartingNodes.Count - 1 do
  begin
    if (StartingNodes[NodeIndex] <> nil) then
    begin
      if (NewPositionIndex <> NodeIndex) then
      begin
        StartingNodes[NewPositionIndex] := StartingNodes[NodeIndex];
      end;
      Inc(NewPositionIndex);
    end;
  end;
  for NodeIndex := StartingNodes.Count - 1 downto NewPositionIndex do
  begin
    StartingNodes.Delete(NodeIndex);
  end;
end;

procedure SetAllNodesInactive(AllNodes: TNodeHandlerObjectList);
var
  NodeIndex: integer;
  NodeHandler: TNodeHandler;
begin
  for NodeIndex := 0 to AllNodes.Count - 1 do
  begin
    NodeHandler := AllNodes[NodeIndex];
    NodeHandler.FActive := False;
    NodeHandler.FAssigned := False;
  end;
end;

procedure AddNewActiveNodes(NextNodeHandler: TNodeHandler;
  ActiveNodes: TNodeHandlerList);
var
  NodeIndex: integer;
  AnotherNodeHandler: TNodeHandler;
begin
  for NodeIndex := 0 to NextNodeHandler.FAdjacentNodes.Count - 1 do
  begin
    AnotherNodeHandler := NextNodeHandler.FAdjacentNodes[NodeIndex];
    if not AnotherNodeHandler.FActive then
    begin
      ActiveNodes.Add(AnotherNodeHandler);
      AnotherNodeHandler.FActive := True;
    end;
  end;
end;

procedure RenumberMesh(Mesh: IMesh);
var
  NodeHandlers: TNodeHandlerObjectList;
  NodeIndex: integer;
  Node: INode;
  NodeHandler: TNodeHandler;
  ActiveNodes: TNodeHandlerList;
  AssignedNodes: TNodeHandlerList;
  NextNodeHandler: TNodeHandler;
  NextNodeIndex: integer;
  OrderingEfficiency: Int64;
  StartingNode: TNodeHandler;
  StartingNodes: TNodeHandlerList;
  StartingNodeIndex: integer;
  MaxFrontWidth: integer;
  CurrentMaxFrontWidth: integer;
  ElementIndex: integer;
  Element: IElement;
  ElementList: TElementHandlerObjectList;
  ElementHandler: TElementHandler;
  DeleteIndex: integer;
begin
  if Mesh.NodeCount = 0 then
  begin
    Exit;
  end;
  NodeHandlers := TNodeHandlerObjectList.Create;
  try
    // Create node TNodeHandler's
    // Assert(Mesh.NodeCount > 0);
    for NodeIndex := 0 to Mesh.NodeCount - 1 do
    begin
      Node := Mesh.Nodes[NodeIndex];
      NodeHandler := TNodeHandler.Create(Node);
      NodeHandlers.Add(NodeHandler);
      Node.BypassUpdate := True;
      Node.NodeNumber := NodeIndex;
      Node.BypassUpdate := False;
    end;

    // Get the list of nodes adjacent to each node.
    for NodeIndex := 0 to NodeHandlers.Count - 1 do
    begin
      NodeHandler := NodeHandlers[NodeIndex];
      NodeHandler.GetAdjacentNodes(NodeHandlers);
    end;

    AssignedNodes := TNodeHandlerList.Create;
    ActiveNodes := TNodeHandlerList.Create;
    StartingNodes := TNodeHandlerList.Create;
    try
      // Pick a node as the first node
      NodeHandler := NodeHandlers[0];

      // Spending effort picking a "good" first node doesn't help.

      {
      for NodeIndex := 0 to NodeHandlers.Count - 1 do
      begin
        NodeHandler := NodeHandlers[NodeIndex];
        NodeHandler.AssignActiveNodeIncrement;
      end;

      NextNodeHandler := nil;
//      NextNodeIndex := -1;
      for NodeIndex := 0 to NodeHandlers.Count - 1 do
      begin
        NodeHandler := NodeHandlers[NodeIndex];
        if NextNodeHandler = nil then
        begin
          NextNodeHandler := NodeHandler;
//          NextNodeIndex := NodeIndex;
        end
        else
        begin
          if NodeHandler.FActiveNodeIncrement <
            NextNodeHandler.FActiveNodeIncrement then
          begin
            NextNodeHandler := NodeHandler;
//            NextNodeIndex := NodeIndex;
          end
        end;
      end;
      NodeHandler := NextNodeHandler;
      }


      UpdateStartingNodes(StartingNodes, NodeHandlers, NodeHandler);
      MaxFrontWidth := NodeHandlers.Count + 1;
      for StartingNodeIndex := StartingNodes.Count - 1 downto 0 do
      begin
        SetAllNodesInactive(NodeHandlers);

        StartingNode := StartingNodes[StartingNodeIndex];
        StartingNode.FNode.BypassUpdate := True;
        StartingNode.FNode.NodeNumber := 0;
        StartingNode.FNode.BypassUpdate := False;
        StartingNode.FAssigned := True;
        StartingNode.FActive := True;
        AssignedNodes.Clear;
        ActiveNodes.Clear;

        AssignedNodes.Add(StartingNode);
        // Add the nodes adjacent to the first node to the active nodes.
        AddNewActiveNodes(StartingNode, ActiveNodes);

        if ActiveNodes.Count >= MaxFrontWidth then
        begin
          StartingNode.FMaxFrontWidth := NodeHandlers.Count + 1;
          StartingNodes.Delete(StartingNodeIndex);
          Continue;
        end;
        CurrentMaxFrontWidth := ActiveNodes.Count;
        // initialize OrderingEfficiency
        OrderingEfficiency := Sqr(ActiveNodes.Count);

        // Find the node that will add the fewest nodes to the active nodes.
        GetNextNodeToAdd(ActiveNodes, NextNodeIndex);

        while NextNodeIndex >= 0 do
        begin
          NextNodeHandler := ActiveNodes[NextNodeIndex];
          NextNodeHandler.FNode.BypassUpdate := True;
          NextNodeHandler.FNode.NodeNumber :=
            AssignedNodes.Add(NextNodeHandler);
          NextNodeHandler.FNode.BypassUpdate := False;
          ActiveNodes.Delete(NextNodeIndex);
          NextNodeHandler.FAssigned := True;
          AddNewActiveNodes(NextNodeHandler, ActiveNodes);
          if ActiveNodes.Count >= MaxFrontWidth then
          begin
            StartingNodes.Delete(StartingNodeIndex);
            break;
          end;
          if ActiveNodes.Count > CurrentMaxFrontWidth then
          begin
            CurrentMaxFrontWidth := ActiveNodes.Count;
          end;
          // update OrderingEfficiency
          OrderingEfficiency := OrderingEfficiency + Sqr(ActiveNodes.Count);

          GetNextNodeToAdd(ActiveNodes, NextNodeIndex);
        end;
        if ActiveNodes.Count >= MaxFrontWidth then
        begin
          StartingNode.FMaxFrontWidth := NodeHandlers.Count + 1;
          Continue;
        end;
        StartingNode.FMaxFrontWidth := CurrentMaxFrontWidth;
        StartingNode.FOrderingEfficiency := OrderingEfficiency;
        if MaxFrontWidth > CurrentMaxFrontWidth then
        begin
          MaxFrontWidth := CurrentMaxFrontWidth;
          for DeleteIndex := StartingNodes.Count - 1
            downto StartingNodeIndex + 1 do
          begin
            StartingNodes.Delete(DeleteIndex);
          end;
        end;
      end;
      Assert(StartingNodes.Count = 1);
      SetAllNodesInactive(NodeHandlers);

      StartingNode := StartingNodes[0];
      StartingNode.FNode.BypassUpdate := True;
      StartingNode.FNode.NodeNumber := 0;
      StartingNode.FNode.BypassUpdate := False;
      StartingNode.FAssigned := True;
      StartingNode.FActive := True;
      AssignedNodes.Clear;
      ActiveNodes.Clear;
      AssignedNodes.Add(StartingNode);
      AddNewActiveNodes(StartingNode, ActiveNodes);
      GetNextNodeToAdd(ActiveNodes, NextNodeIndex);
      while NextNodeIndex >= 0 do
      begin
        NextNodeHandler := ActiveNodes[NextNodeIndex];
        NextNodeHandler.FNode.BypassUpdate := True;
        NextNodeHandler.FNode.NodeNumber := AssignedNodes.Add(NextNodeHandler);
        NextNodeHandler.FNode.BypassUpdate := False;
        ActiveNodes.Delete(NextNodeIndex);
        NextNodeHandler.FAssigned := True;
        AddNewActiveNodes(NextNodeHandler, ActiveNodes);
        GetNextNodeToAdd(ActiveNodes, NextNodeIndex);
      end;
    finally
      AssignedNodes.Free;
      ActiveNodes.Free;
      StartingNodes.Free;
    end;
  finally
    NodeHandlers.Free;
  end;

  ElementList := TElementHandlerObjectList.Create;
  try
    for ElementIndex := 0 to Mesh.ElementCount - 1 do
    begin
      Element := Mesh.Elements[ElementIndex];
      ElementHandler := TElementHandler.Create(Element);
      ElementList.Add(ElementHandler);
    end;

    ElementList.Sort(TElementHandlerComparer.Construct(
      function(const L, R: TElementHandler): integer
      var
        Index: integer;
      begin
        result := 0;
        for Index := 0 to L.FIntegerList.Count - 1 do
        begin
          result := L.FIntegerList[Index] - R.FIntegerList[Index];
          if result <> 0 then
          begin
            break;
          end;
        end;
      end));
    for ElementIndex := 0 to ElementList.Count - 1 do
    begin
      ElementHandler := ElementList[ElementIndex];
      ElementHandler.FElement.BypassUpdate := True;
      ElementHandler.FElement.ElementNumber := ElementIndex;
      ElementHandler.FElement.BypassUpdate := False;
    end;
  finally
    ElementList.Free;
  end;

end;

{ TNodeHandler }

procedure TNodeHandler.AssignActiveNodeIncrement;
var
  NodeIndex: integer;
  AnotherNode: TNodeHandler;
begin
  FActiveNodeIncrement := -1;
  for NodeIndex := 0 to FAdjacentNodes.Count - 1 do
  begin
    AnotherNode := FAdjacentNodes[NodeIndex];
    if not AnotherNode.FActive then
    begin
      Inc(FActiveNodeIncrement);
    end;
  end;

  // This step is different from what is in the original source.
  // It provides a little improvement for Fishnet meshes.
  if (FNode.ActiveElementCount in [1,2]) and (FActiveNodeIncrement <= 0) then
  begin
    Inc(FActiveNodeIncrement);
  end;

  // This was no better than the above method.
//  if (FNode.ActiveElementCount = 1) and (FActiveNodeIncrement <= 0) then
//  begin
//    Inc(FActiveNodeIncrement);
//  end;
//  if (FNode.ActiveElementCount = 2) and (FActiveNodeIncrement <= 0) then
//  begin
//    Inc(FActiveNodeIncrement, 2);
//  end;

// These ways of djusting the way FActiveNodeIncrement is assigned don't help.
//  if FNode.ActiveElementCount in [1,2] then
//  begin
//    Inc(FActiveNodeIncrement);
//  end;
//  if FNode.NodeType <> ntInner  then
//  begin
//    Inc(FActiveNodeIncrement);
//    if FNode.ActiveElementCount = 1 then
//    begin
//      Inc(FActiveNodeIncrement,2);
//    end;
//    if FNode.ActiveElementCount = 2 then
//    begin
//      Inc(FActiveNodeIncrement);
//    end;
//  end;
end;

constructor TNodeHandler.Create(Node: INode);
begin
  FActive := False;
  FNode := Node;
  FAdjacentNodes := TNodeHandlerList.Create;
end;

destructor TNodeHandler.Destroy;
begin
  FAdjacentNodes.Free;
  inherited;
end;

procedure TNodeHandler.GetAdjacentNodes(NodeHandlers: TNodeHandlerObjectList);
var
  ElementIndex: integer;
  ANode: INode;
  Element: IElement;
  NodeIndex: integer;
  Handler: TNodeHandler;
begin
  for ElementIndex := 0 to FNode.ActiveElementCount - 1 do
  begin
    Element := FNode.ActiveElements[ElementIndex];
    for NodeIndex := 0 to Element.NodeCount - 1 do
    begin
      ANode := Element.Nodes[NodeIndex];
      if (ANode <> FNode) then
      begin
        Handler := NodeHandlers[ANode.NodeNumber];
        if (FAdjacentNodes.IndexOf(Handler) < 0) then
        begin
          FAdjacentNodes.Add(Handler);
        end;
      end;
    end;
  end;
end;

{ TElementHandler }

constructor TElementHandler.Create(Element: IElement);
var
  NodeIndex: integer;
  Node: INode;
begin
  FElement := Element;
  FIntegerList := TList<Integer>.Create;
  for NodeIndex := 0 to FElement.NodeCount - 1 do
  begin
    Node := FElement.Nodes[NodeIndex];
    FIntegerList.Add(Node.NodeNumber);
  end;
  FIntegerList.Sort;
end;

destructor TElementHandler.Destroy;
begin
  FIntegerList.Free;
  inherited;
end;

end.
