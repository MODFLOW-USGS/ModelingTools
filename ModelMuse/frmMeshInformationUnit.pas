unit frmMeshInformationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, StdCtrls,
  ExtCtrls, Buttons, Mask, JvExMask, JvSpin, ComCtrls,
  Grids, RbwDataGrid4,
//  {$IF CompilerVersion >= 23.0}
//  // Delphi XE2 and up
//  VCLTee.TeEngine, VCLTee.Series, VCLTee.TeeProcs, VCLTee.Chart,
//  {$ELSE}
//  // Delphi XE and earlier
//  TeEngine, Series, TeeProcs, Chart,
//  {$IFEND}
  Generics.Defaults, GrayTabs;

type
  TfrmMeshInformation = class(TfrmCustomGoPhast)
    pnl1: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    pnl2: TPanel;
    lblBandwidth: TLabel;
    lblNumberOfNodes: TLabel;
    lblNumberOfElements: TLabel;
    pc1: TPageControl;
    tabElementAngles: TTabSheet;
    pnl3: TPanel;
    lblBinSize: TLabel;
    seBinSize: TJvSpinEdit;
    splitterVertical: TSplitter;
    tabElementCounts: TTabSheet;
    pnl4: TPanel;
    rdgBadElements: TRbwDataGrid4;
    rdgElementAngles: TRbwDataGrid4;
    splHorizontal: TSplitter;
    splNodes: TSplitter;
    rdgNodes: TRbwDataGrid4;
    tabAspectRatio: TTabSheet;
    splAspectRatio: TSplitter;
    rdgAspectRatio: TRbwDataGrid4;
    pnlAspectRatio: TPanel;
    lblAspectRatioBinSize: TLabel;
    seAspectRatioBinSize: TJvSpinEdit;
    pbHistogram: TPaintBox;
    pbAspectRatio: TPaintBox;
    pbElementPerNode: TPaintBox;
    procedure seBinSizeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject); override;
    procedure rdgBadElementsButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure rdgElementAnglesButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure rdgNodesButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure rdgAspectRatioButtonClick(Sender: TObject; ACol, ARow: Integer);
    procedure seAspectRatioBinSizeChange(Sender: TObject);
    procedure pbHistogramPaint(Sender: TObject);
    procedure pbAspectRatioPaint(Sender: TObject);
    procedure pbElementPerNodePaint(Sender: TObject);
  private
    procedure UpdateAngleCountHistogram;
    procedure UpdateDeviationCountHistogram;
    procedure UpdateAspectRatioHistogram;
    { Private declarations }
  public
    procedure GetData;
    { Public declarations }
  end;

  TEleValueSortItem = class(TObject)
    ElementNumber : integer;
    Value: double;
  end;

  TElementValueComparer = class(TComparer<TEleValueSortItem>)
    function Compare(const Left, Right: TEleValueSortItem): Integer; override;
  end;

  TDeviationSortItem = class(TObject)
    Deviation: Integer;
    NodeNumber: Integer;
  end;

  TDeviationComparer = class(TComparer<TDeviationSortItem>)
    function Compare(const Left, Right: TDeviationSortItem): Integer; override;
  end;

var
  frmMeshInformation: TfrmMeshInformation = nil;

implementation

uses
  frmGoPhastUnit, SutraMeshUnit, IntListUnit, FastGEO, frmGoToUnit,
  GoPhastTypes, Generics.Collections, Math,
  UITypes, xygraph;

resourcestring
  StrBandwidthD = 'Bandwidth: %d';
  StrNumberOfNodes = 'Number of nodes: %d';
  StrNumberOfElements = 'Number of elements: %d';
  StrInvalidElement = 'Invalid Elements';
  StrElement = 'Element';
  StrAngle = 'Angle';
  StrDeviation = 'Deviation';
  StrNode = 'Node';
  StrAspectRatio = 'Aspect Ratio';
  StrYouMustDefineOne = 'You must define one or more layers before some mesh' +
  ' data for 3D meshes can be displayed.';

{$R *.dfm}

{ TfrmMeshInformation }

procedure TfrmMeshInformation.FormCreate(Sender: TObject);
begin
  inherited;
  rdgBadElements.Cells[0,0] := StrInvalidElement;
  pc1.ActivePageIndex := 0;

  rdgElementAngles.Cells[0,0] := StrAngle;
  rdgElementAngles.Cells[1,0] := StrElement;

  rdgNodes.Cells[0,0] := StrDeviation;
  rdgNodes.Cells[1,0] := StrNode;

  rdgAspectRatio.Cells[0,0] := StrAspectRatio;
  rdgAspectRatio.Cells[1,0] := StrElement;

end;

procedure TfrmMeshInformation.FormShow(Sender: TObject);
begin
  inherited;
//  AssignFonts(chtHistogram);
//  AssignFonts(chtElementPerNode);
//  AssignFonts(chtAspectRatio);
end;

procedure TfrmMeshInformation.GetData;
var
  Mesh: TSutraMesh3D;
begin

  Mesh := frmGoPhast.PhastModel.SutraMesh;
  lblBandwidth.Caption := Format(StrBandwidthD, [Mesh.Bandwidth]);
  case Mesh.MeshType of
    mt2D, mtProfile:
      begin
        lblNumberOfNodes.Caption := Format(StrNumberOfNodes,
          [Mesh.Mesh2D.Nodes.Count]);
        lblNumberOfElements.Caption := Format(StrNumberOfElements,
          [Mesh.Mesh2D.Elements.Count]);
      end;
    mt3D:
      begin
        lblNumberOfNodes.Caption := Format(StrNumberOfNodes,
          [Mesh.ActiveNodeCount]);
        lblNumberOfElements.Caption := Format(StrNumberOfElements,
          [Mesh.ActiveElementCount]);
        if Mesh.LayerCount = 0 then
        begin
          Beep;
          MessageDlg(StrYouMustDefineOne, mtWarning, [mbOK], 0);
        end;
      end;
    else Assert(False);
  end;
  UpdateAngleCountHistogram;
  UpdateDeviationCountHistogram;
  UpdateAspectRatioHistogram;
end;

type
  TGridCrack = class(TCustomGrid);

procedure TfrmMeshInformation.pbElementPerNodePaint(Sender: TObject);
var
  NegativeCounts: TIntegerList;
  PositiveCounts: TIntegerList;
  index: Integer;
  Mesh2D: TSutraMesh2D;
  ANode: TSutraNode2D;
  CountList: TIntegerList;
//  ACount: Integer;
  Delta: Integer;
  Mesh3D: TSutraMesh3D;
  SortList: TObjectList<TDeviationSortItem>;
  NodeNumber: Integer;
  ANode3D: TSutraNode3D;
  Item: TDeviationSortItem;
  LayerIndex: Integer;
  NodeIndex: Integer;
  Deviation: integer;
  Sorter: TDeviationComparer;
  MaxCount: Integer;
  PositionIndex: Integer;
  labels: array of string;
begin
  NegativeCounts := TIntegerList.Create;
  PositiveCounts := TIntegerList.Create;
  SortList:= TObjectList<TDeviationSortItem>.Create;
  try
    Mesh3D := frmGoPhast.PhastModel.SutraMesh;
    Mesh2D := Mesh3D.Mesh2D;
    for index := 0 to Mesh2D.Nodes.Count - 1 do
    begin
      ANode := Mesh2D.Nodes[index];
      Delta := ANode.ElementCount - ANode.IdealElementCount;
      Deviation := Delta;
      if Delta >= 0 then
      begin
        CountList := PositiveCounts;
      end
      else
      begin
        Delta := - Delta;
        CountList := NegativeCounts;
      end;
      while CountList.Count <= Delta do
      begin
        CountList.Add(0);
      end;
      CountList[Delta] := CountList[Delta] + 1;
      if Abs(Delta) > 1 then
      begin
        NodeNumber := -1;
        if Mesh3D.MeshType = mt3D then
        begin
          if Mesh3D.LayerCount = 0 then
          begin
            Exit;
          end;
          for LayerIndex := 0 to Mesh3D.LayerCount do
          begin
            ANode3D := Mesh3D.NodeArray[
              LayerIndex,ANode.Number];
            if ANode3D.Active then
            begin
              NodeNumber := ANode3D.Number+1;
              break;
            end
          end;
        end
        else
        begin
          NodeNumber := ANode.Number+1;
        end;
        if NodeNumber >= 0 then
        begin
          Item := TDeviationSortItem.Create;
          SortList.Add(Item);
          Item.Deviation := Deviation;
          Item.NodeNumber := NodeNumber;
        end;
      end;
    end;

    MaxCount := 0;
    for Index := 0 to NegativeCounts.Count - 1 do
    begin
      MaxCount := Max(MaxCount, NegativeCounts[Index]);
    end;
    for Index := 0 to PositiveCounts.Count - 1 do
    begin
      MaxCount := Max(MaxCount, PositiveCounts[Index]);
    end;


    xycleargraph(pbElementPerNode,clwhite,clblack,8/8);

    xystartgraph(0,100,0,100,40,20,30,40,clipon);
    xysetgridlines(clred);
    SetLength(labels, NegativeCounts.Count+PositiveCounts.Count);
    PositionIndex := 0;
    for Index := NegativeCounts.Count - 1 downto 1 do
    begin
      labels[PositionIndex] := IntToStr(-Index);
      Inc(PositionIndex)
    end;
    for Index := 0 to PositiveCounts.Count - 1 do
    begin
      labels[PositionIndex] := IntToStr(-Index);
      Inc(PositionIndex)
    end;
    xyxlabelaxis(clblack, labels, 'Difference', 1, 0, gridoff);
    xyyaxis(clblack,0,MaxCount,0,0,'Number of Nodes',5,gridon,lin,fixed, 2);

    PositionIndex := 1;
    for Index := NegativeCounts.Count - 1 downto 1 do
    begin
//      serAngles.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
      xybar(clblue,PositionIndex,0.7,NegativeCounts[Index],0);
      Inc(PositionIndex)
    end;
    for Index := 0 to PositiveCounts.Count - 1 do
    begin
      xybar(clblue,PositionIndex,0.7,PositiveCounts[Index],0);
      Inc(PositionIndex)
    end;


    xytitle(clBlack,'Actual Minus Ideal Element Count per Node');


//    serDeviations.Clear;
//    for Index := NegativeCounts.Count - 1 downto 1 do
//    begin
//      ACount := NegativeCounts[Index];
//      serDeviations.AddBar(ACount, IntToStr(-Index), clTeeColor);
//    end;
//    for Index := 0 to PositiveCounts.Count - 1 do
//    begin
//      ACount := PositiveCounts[Index];
//      serDeviations.AddBar(ACount, IntToStr(Index), clTeeColor);
//    end;
    if SortList.Count > 0 then
    begin
      Sorter := TDeviationComparer.Create;
      try
        SortList.Sort(Sorter);
      finally
        Sorter.Free;
      end;
      rdgNodes.Visible := True;
      splNodes.Visible := True;
      if splNodes.Left > rdgNodes.Left then
      begin
        splNodes.Left := rdgNodes.Left
      end;
      rdgNodes.RowCount := SortList.Count + 1;
      for NodeIndex := 0 to SortList.Count - 1 do
      begin
        Item := SortList[NodeIndex];
        rdgNodes.Cells[0, NodeIndex+1] := IntToStr(Item.Deviation);
        rdgNodes.Cells[1, NodeIndex+1] := IntToStr(Item.NodeNumber);
      end;
    end
    else
    begin
      rdgNodes.Visible := False;
      splNodes.Visible := False;
    end;
  finally
    SortList.Free;
    PositiveCounts.Free;
    NegativeCounts.Free;
  end;
end;

procedure TfrmMeshInformation.pbAspectRatioPaint(Sender: TObject);
var
  StartIndex: Integer;
  Bin: Integer;
  LastBin: double;
  AspectRatio: Double;
  Bins: TIntegerList;
  BinTitles: TStringList;
  AnElement2D: TSutraElement2D;
  ElementIndex: Integer;
  BinIndex: Integer;
  BinSize: double;
  Mesh: TSutraMesh3D;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  EleAngleSortList: TObjectList<TEleValueSortItem>;
  SortItem: TEleValueSortItem;
  index: Integer;
  Comparer: TElementValueComparer;
  ElNumber: Integer;
  VE: double;
  labels: array of string;
  MaxCount: Integer;
begin

  Mesh := frmGoPhast.PhastModel.SutraMesh;
  if Mesh.MeshType = mtProfile then
  begin
    VE := frmGoPhast.PhastModel.Exaggeration;
  end
  else
  begin
    VE := 1;
  end;
  BinSize := seAspectRatioBinSize.Value;
  if BinSize <= 0 then
  begin
    Exit;
  end;
  LastBin := 0;
  Bins := TIntegerList.Create;
  BinTitles := TStringList.Create;
  EleAngleSortList := TObjectList<TEleValueSortItem>.Create;
  try
    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
    begin
      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
      ElNumber := -1;
      if Mesh.MeshType = mt3D then
      begin
        for LayerIndex := 0 to Mesh.LayerCount - 1 do
        begin
          AnElement3D := Mesh.ElementArray[
            LayerIndex,AnElement2D.ElementNumber];
          if AnElement3D.Active then
          begin
            ElNumber := AnElement3D.ElementNumber+1;
            break;
          end;
        end;
      end
      else
      begin
        ElNumber := AnElement2D.ElementNumber+1;
      end;
      if ElNumber < 0 then
      begin
        Continue;
      end;


        AspectRatio := AnElement2D.AspectRatio(VE);
        SortItem := TEleValueSortItem.Create;
        EleAngleSortList.Add(SortItem);
        SortItem.ElementNumber := ElNumber;
        SortItem.Value := AspectRatio;
        Bin := Trunc(AspectRatio / BinSize);
        while Bin >= Bins.Count do
        begin
          Bins.Add(0);
          BinTitles.Add(Format('%0:g-%1:g', [LastBin, LastBin + BinSize]));
          LastBin := LastBin + BinSize;
        end;
        Bins[Bin] := Bins[Bin] + 1;

    end;
    StartIndex := 0;
    while (StartIndex < Bins.Count) and (Bins[StartIndex] = 0) do
    begin
      Inc(StartIndex);
    end;

    MaxCount := 0;
    for Index := StartIndex to Bins.Count - 1 do
    begin
      MaxCount := Max(MaxCount, Bins[Index]);
    end;

    xycleargraph(pbAspectRatio,clwhite,clblack,8/8);

    xystartgraph(0,100,0,100,40,20,30,40,clipon);
    xysetgridlines(clred);
    SetLength(labels, BinTitles.Count-StartIndex);
    for index := StartIndex to BinTitles.Count - 1 do
    begin
      labels[index-StartIndex] := BinTitles[index];
    end;

    xyxlabelaxis(clblack, labels, 'Aspect Ratio', 1, 0, gridoff);
    xyyaxis(clblack,0,MaxCount,0,0,'Number of Elements',5,gridon,lin,fixed, 2);
//      xylegendentry(1,'bar');

//    xytitle(clBlack,'Element Angles');


//    serAngles.Clear;
    for BinIndex := StartIndex to Bins.Count - 1 do
    begin
//      serAngles.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
      xybar(clblue,BinIndex-StartIndex+1,0.7,Bins[BinIndex],0);
    end;



//    serAspectRatio.Clear;
//    for BinIndex := StartIndex to Bins.Count - 1 do
//    begin
//      serAspectRatio.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
//    end;
    Comparer := TElementValueComparer.Create;
    try
      EleAngleSortList.Sort(Comparer);
    finally
      Comparer.Free;
    end;
    rdgAspectRatio.BeginUpdate;
    try
      if EleAngleSortList.Count = 0 then
      begin
        rdgAspectRatio.RowCount := 2;
        rdgAspectRatio.Cells[0,1] := '';
        rdgAspectRatio.Cells[1,1] := '';
      end
      else
      begin
        rdgAspectRatio.RowCount := EleAngleSortList.Count + 1;
        for index := 0 to EleAngleSortList.Count - 1 do
        begin
          SortItem := EleAngleSortList[index];
          rdgAspectRatio.Cells[0,index+1] := Format('%.4f', [SortItem.Value]);
          rdgAspectRatio.Cells[1,index+1] := IntToStr(SortItem.ElementNumber)
        end;
      end;
    finally
      rdgAspectRatio.EndUpdate;
    end;
  finally
    Bins.Free;
    BinTitles.Free;
    EleAngleSortList.Free;
  end;
end;

procedure TfrmMeshInformation.pbHistogramPaint(Sender: TObject);
var
  Index: Integer;
  labels: array of string;
  StartIndex: Integer;
  NodeIndex: Integer;
  Bin: Integer;
  LastBin: Integer;
  Angle: Double;
  Bins: TIntegerList;
  Node1: TPoint2D;
  BinTitles: TStringList;
  Node3: TPoint2D;
  AnElement2D: TSutraElement2D;
  ElementIndex: Integer;
  BinIndex: Integer;
  BinSize: Integer;
  Node2: TPoint2D;
  Mesh: TSutraMesh3D;
  BadElements: TIntegerList;
  BadElement: Boolean;
  LayerIndex: Integer;
  AnElement3D: TSutraElement3D;
  ElementNumberIndex: Integer;
  EleAngleSortList: TObjectList<TEleValueSortItem>;
  SortItem: TEleValueSortItem;
//  index: Integer;
  Comparer: TElementValueComparer;
  ElNumber: Integer;
  VE: double;
  MaxCount: Integer;
begin
  inherited;
//  labels[0] := 'January';

  VE := frmGoPhast.PhastModel.Exaggeration;
  Mesh := frmGoPhast.PhastModel.SutraMesh;
  BinSize := seBinSize.AsInteger;
  if BinSize <= 0 then
  begin
    Exit;
  end;
  LastBin := 0;
  Bins := TIntegerList.Create;
  BinTitles := TStringList.Create;
  BadElements := TIntegerList.Create;
  EleAngleSortList := TObjectList<TEleValueSortItem>.Create;
  try
    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
    begin
      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
      ElNumber := -1;
      if Mesh.MeshType = mt3D then
      begin
        for LayerIndex := 0 to Mesh.LayerCount - 1 do
        begin
          AnElement3D := Mesh.ElementArray[
            LayerIndex,AnElement2D.ElementNumber];
          if AnElement3D.Active then
          begin
            ElNumber := AnElement3D.ElementNumber+1;
            break;
          end;
        end;
      end
      else
      begin
        ElNumber := AnElement2D.ElementNumber+1;
      end;
      if ElNumber < 0 then
      begin
        Continue;
      end;
      Node1 := AnElement2D.Nodes[2].Node.Location;
      if Mesh.MeshType = mtProfile then
      begin
        Node1.Y := Node1.Y*VE;
      end;
      Node2 := AnElement2D.Nodes[3].Node.Location;
      if Mesh.MeshType = mtProfile then
      begin
        Node2.Y := Node2.Y*VE;
      end;
      BadElement := False;
      for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
      begin
        Node3 := AnElement2D.Nodes[NodeIndex].Node.Location;
        if Mesh.MeshType = mtProfile then
        begin
          Node3.Y := Node3.Y*VE;
        end;
        Angle := OrientedVertexAngle(Node1, Node2, Node3, CounterClockwise);
        Assert(Angle >= 0);
        if Angle >= 180 then
        begin
          BadElement := True;
        end;
        SortItem := TEleValueSortItem.Create;
        EleAngleSortList.Add(SortItem);
        SortItem.ElementNumber := ElNumber;
        SortItem.Value := Angle;
        Bin := Trunc(Angle / BinSize);
        while Bin >= Bins.Count do
        begin
          Bins.Add(0);
          BinTitles.Add(Format('%0:d-%1:d', [LastBin, LastBin + BinSize]));
          LastBin := LastBin + BinSize;
        end;
        Bins[Bin] := Bins[Bin] + 1;
        Node1 := Node2;
        Node2 := Node3;
      end;

      if BadElement then
      begin
        BadElements.Add(ElNumber);
      end;
    end;
    StartIndex := 0;
    while (StartIndex < Bins.Count) and (Bins[StartIndex] = 0) do
    begin
      Inc(StartIndex);
    end;

    MaxCount := 0;
    for Index := StartIndex to Bins.Count - 1 do
    begin
      MaxCount := Max(MaxCount, Bins[Index]);
    end;

    xycleargraph(pbHistogram,clwhite,clblack,8/8);

    xystartgraph(0,100,0,100,40,20,30,40,clipon);
    xysetgridlines(clred);
    SetLength(labels, BinTitles.Count-StartIndex);
    for index := StartIndex to BinTitles.Count - 1 do
    begin
      labels[index-StartIndex] := BinTitles[index];
    end;

    xyxlabelaxis(clblack, labels, 'Angle Size (degrees)', 1, 0, gridoff);
    xyyaxis(clblack,0,MaxCount,0,0,'Number of Angles',5,gridon,lin,fixed, 2);
//      xylegendentry(1,'bar');

    xytitle(clBlack,'Element Angles');


//    serAngles.Clear;
    for BinIndex := StartIndex to Bins.Count - 1 do
    begin
//      serAngles.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
      xybar(clblue,BinIndex-StartIndex+1,0.7,Bins[BinIndex],0);
    end;
    rdgBadElements.Visible := BadElements.Count > 0;
    splHorizontal.Visible := rdgBadElements.Visible;
    if splHorizontal.Visible then
    begin
      if splHorizontal.Top > rdgBadElements.Top then
      begin
        splHorizontal.Top := rdgBadElements.Top + rdgBadElements.Height;
      end;
    end;
    if BadElements.Count > 0 then
    begin
      rdgBadElements.BeginUpdate;
      try
        rdgBadElements.RowCount := BadElements.Count+1;
        for ElementNumberIndex := 0 to BadElements.Count - 1 do
        begin
          rdgBadElements.Cells[0,ElementNumberIndex+1] :=
            IntToStr(BadElements[ElementNumberIndex]);
        end;
      finally
        rdgBadElements.EndUpdate;
        rdgBadElements.Row := 1;
        TGridCrack(rdgBadElements).HideEditor;
      end;
    end;
    Comparer := TElementValueComparer.Create;
    try
      EleAngleSortList.Sort(Comparer);
    finally
      Comparer.Free;
    end;
    rdgElementAngles.BeginUpdate;
    try
      if EleAngleSortList.Count = 0 then
      begin
        rdgElementAngles.RowCount := 2;
        rdgElementAngles.Cells[0,1] := '';
        rdgElementAngles.Cells[1,1] := '';
      end
      else
      begin
        rdgElementAngles.RowCount := EleAngleSortList.Count + 1;
        for index := 0 to EleAngleSortList.Count - 1 do
        begin
          SortItem := EleAngleSortList[index];
          rdgElementAngles.Cells[0,index+1] := Format('%.2f', [SortItem.Value]);
          rdgElementAngles.Cells[1,index+1] := IntToStr(SortItem.ElementNumber)
        end;
      end;
    finally
      rdgElementAngles.EndUpdate;
    end;
  finally
    Bins.Free;
    BinTitles.Free;
    BadElements.Free;
    EleAngleSortList.Free;
  end;

end;

procedure TfrmMeshInformation.rdgAspectRatioButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  ElementNumber: Integer;
begin
  inherited;
  if TryStrToInt(rdgAspectRatio.Cells[ACol, ARow], ElementNumber) then
  begin
    ElementNumber := ElementNumber-1;
    MoveToMesh(ElementNumber, mmtElement, [vdTop, vdFront]);
  end;
end;

procedure TfrmMeshInformation.rdgBadElementsButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  ElementNumber: Integer;
begin
  inherited;
  if TryStrToInt(rdgBadElements.Cells[ACol, ARow], ElementNumber) then
  begin
    ElementNumber := ElementNumber-1;
    MoveToMesh(ElementNumber, mmtElement, [vdTop, vdFront]);
  end;
end;

procedure TfrmMeshInformation.rdgElementAnglesButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  ElementNumber: Integer;
begin
  inherited;
  if TryStrToInt(rdgElementAngles.Cells[ACol, ARow], ElementNumber) then
  begin
    ElementNumber := ElementNumber-1;
    MoveToMesh(ElementNumber, mmtElement, [vdTop, vdFront]);
  end;
end;

procedure TfrmMeshInformation.rdgNodesButtonClick(Sender: TObject; ACol,
  ARow: Integer);
var
  NodeNumber: Integer;
begin
  inherited;
  if TryStrToInt(rdgNodes.Cells[ACol, ARow], NodeNumber) then
  begin
    NodeNumber := NodeNumber-1;
    MoveToMesh(NodeNumber, mmtNode, [vdTop, vdFront]);
  end;
end;

procedure TfrmMeshInformation.seAspectRatioBinSizeChange(Sender: TObject);
begin
  inherited;
  UpdateAspectRatioHistogram;
end;

procedure TfrmMeshInformation.seBinSizeChange(Sender: TObject);
begin
  inherited;
  UpdateAngleCountHistogram
end;

procedure TfrmMeshInformation.UpdateAngleCountHistogram;
//var
//  StartIndex: Integer;
//  NodeIndex: Integer;
//  Bin: Integer;
//  LastBin: Integer;
//  Angle: Double;
//  Bins: TIntegerList;
//  Node1: TPoint2D;
//  BinTitles: TStringList;
//  Node3: TPoint2D;
//  AnElement2D: TSutraElement2D;
//  ElementIndex: Integer;
//  BinIndex: Integer;
//  BinSize: Integer;
//  Node2: TPoint2D;
//  Mesh: TSutraMesh3D;
//  BadElements: TIntegerList;
//  BadElement: Boolean;
//  LayerIndex: Integer;
//  AnElement3D: TSutraElement3D;
//  ElementNumberIndex: Integer;
//  EleAngleSortList: TObjectList<TEleValueSortItem>;
//  SortItem: TEleValueSortItem;
//  index: Integer;
//  Comparer: TElementValueComparer;
//  ElNumber: Integer;
//  VE: double;
begin

//  VE := frmGoPhast.PhastModel.Exaggeration;
//  Mesh := frmGoPhast.PhastModel.Mesh;
//  BinSize := seBinSize.AsInteger;
//  if BinSize <= 0 then
//  begin
//    Exit;
//  end;
//  LastBin := 0;
//  Bins := TIntegerList.Create;
//  BinTitles := TStringList.Create;
//  BadElements := TIntegerList.Create;
//  EleAngleSortList := TObjectList<TEleValueSortItem>.Create;
//  try
//    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
//    begin
//      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
//      ElNumber := -1;
//      if Mesh.MeshType = mt3D then
//      begin
//        for LayerIndex := 0 to Mesh.LayerCount - 1 do
//        begin
//          AnElement3D := Mesh.ElementArray[
//            LayerIndex,AnElement2D.ElementNumber];
//          if AnElement3D.Active then
//          begin
//            ElNumber := AnElement3D.ElementNumber+1;
//            break;
//          end;
//        end;
//      end
//      else
//      begin
//        ElNumber := AnElement2D.ElementNumber+1;
//      end;
//      if ElNumber < 0 then
//      begin
//        Continue;
//      end;
//      Node1 := AnElement2D.Nodes[2].Node.Location;
//      if Mesh.MeshType = mtProfile then
//      begin
//        Node1.Y := Node1.Y*VE;
//      end;
//      Node2 := AnElement2D.Nodes[3].Node.Location;
//      if Mesh.MeshType = mtProfile then
//      begin
//        Node2.Y := Node2.Y*VE;
//      end;
//      BadElement := False;
//      for NodeIndex := 0 to AnElement2D.Nodes.Count - 1 do
//      begin
//        Node3 := AnElement2D.Nodes[NodeIndex].Node.Location;
//        if Mesh.MeshType = mtProfile then
//        begin
//          Node3.Y := Node3.Y*VE;
//        end;
//        Angle := OrientedVertexAngle(Node1, Node2, Node3, CounterClockwise);
//        Assert(Angle >= 0);
//        if Angle >= 180 then
//        begin
//          BadElement := True;
//        end;
//        SortItem := TEleValueSortItem.Create;
//        EleAngleSortList.Add(SortItem);
//        SortItem.ElementNumber := ElNumber;
//        SortItem.Value := Angle;
//        Bin := Trunc(Angle / BinSize);
//        while Bin >= Bins.Count do
//        begin
//          Bins.Add(0);
//          BinTitles.Add(Format('%0:d-%1:d', [LastBin, LastBin + BinSize]));
//          LastBin := LastBin + BinSize;
//        end;
//        Bins[Bin] := Bins[Bin] + 1;
//        Node1 := Node2;
//        Node2 := Node3;
//      end;
//
//      if BadElement then
//      begin
//        BadElements.Add(ElNumber);
//      end;
//    end;
//    StartIndex := 0;
//    while (StartIndex < Bins.Count) and (Bins[StartIndex] = 0) do
//    begin
//      Inc(StartIndex);
//    end;
//
//    serAngles.Clear;
//    for BinIndex := StartIndex to Bins.Count - 1 do
//    begin
//      serAngles.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
//    end;
//    rdgBadElements.Visible := BadElements.Count > 0;
//    splHorizontal.Visible := rdgBadElements.Visible;
//    if splHorizontal.Visible then
//    begin
//      if splHorizontal.Top > rdgBadElements.Top then
//      begin
//        splHorizontal.Top := rdgBadElements.Top + rdgBadElements.Height;
//      end;
//    end;
//    if BadElements.Count > 0 then
//    begin
//      rdgBadElements.BeginUpdate;
//      try
//        rdgBadElements.RowCount := BadElements.Count+1;
//        for ElementNumberIndex := 0 to BadElements.Count - 1 do
//        begin
//          rdgBadElements.Cells[0,ElementNumberIndex+1] :=
//            IntToStr(BadElements[ElementNumberIndex]);
//        end;
//      finally
//        rdgBadElements.EndUpdate;
//        rdgBadElements.Row := 1;
//        TGridCrack(rdgBadElements).HideEditor;
//      end;
//    end;
//    Comparer := TElementValueComparer.Create;
//    try
//      EleAngleSortList.Sort(Comparer);
//    finally
//      Comparer.Free;
//    end;
//    rdgElementAngles.BeginUpdate;
//    try
//      if EleAngleSortList.Count = 0 then
//      begin
//        rdgElementAngles.RowCount := 2;
//        rdgElementAngles.Cells[0,1] := '';
//        rdgElementAngles.Cells[1,1] := '';
//      end
//      else
//      begin
//        rdgElementAngles.RowCount := EleAngleSortList.Count + 1;
//        for index := 0 to EleAngleSortList.Count - 1 do
//        begin
//          SortItem := EleAngleSortList[index];
//          rdgElementAngles.Cells[0,index+1] := Format('%.2f', [SortItem.Value]);
//          rdgElementAngles.Cells[1,index+1] := IntToStr(SortItem.ElementNumber)
//        end;
//      end;
//    finally
//      rdgElementAngles.EndUpdate;
//    end;
//  finally
//    Bins.Free;
//    BinTitles.Free;
//    BadElements.Free;
//    EleAngleSortList.Free;
//  end;
end;

procedure TfrmMeshInformation.UpdateAspectRatioHistogram;
//var
//  StartIndex: Integer;
//  Bin: Integer;
//  LastBin: double;
//  AspectRatio: Double;
//  Bins: TIntegerList;
//  BinTitles: TStringList;
//  AnElement2D: TSutraElement2D;
//  ElementIndex: Integer;
//  BinIndex: Integer;
//  BinSize: double;
//  Mesh: TSutraMesh3D;
//  LayerIndex: Integer;
//  AnElement3D: TSutraElement3D;
//  EleAngleSortList: TObjectList<TEleValueSortItem>;
//  SortItem: TEleValueSortItem;
//  index: Integer;
//  Comparer: TElementValueComparer;
//  ElNumber: Integer;
//  VE: double;
begin

//  Mesh := frmGoPhast.PhastModel.Mesh;
//  if Mesh.MeshType = mtProfile then
//  begin
//    VE := frmGoPhast.PhastModel.Exaggeration;
//  end
//  else
//  begin
//    VE := 1;
//  end;
//  BinSize := seAspectRatioBinSize.Value;
//  if BinSize <= 0 then
//  begin
//    Exit;
//  end;
//  LastBin := 0;
//  Bins := TIntegerList.Create;
//  BinTitles := TStringList.Create;
//  EleAngleSortList := TObjectList<TEleValueSortItem>.Create;
//  try
//    for ElementIndex := 0 to Mesh.Mesh2D.Elements.Count - 1 do
//    begin
//      AnElement2D := Mesh.Mesh2D.Elements[ElementIndex];
//      ElNumber := -1;
//      if Mesh.MeshType = mt3D then
//      begin
//        for LayerIndex := 0 to Mesh.LayerCount - 1 do
//        begin
//          AnElement3D := Mesh.ElementArray[
//            LayerIndex,AnElement2D.ElementNumber];
//          if AnElement3D.Active then
//          begin
//            ElNumber := AnElement3D.ElementNumber+1;
//            break;
//          end;
//        end;
//      end
//      else
//      begin
//        ElNumber := AnElement2D.ElementNumber+1;
//      end;
//      if ElNumber < 0 then
//      begin
//        Continue;
//      end;
//
//
//        AspectRatio := AnElement2D.AspectRatio(VE);
//        SortItem := TEleValueSortItem.Create;
//        EleAngleSortList.Add(SortItem);
//        SortItem.ElementNumber := ElNumber;
//        SortItem.Value := AspectRatio;
//        Bin := Trunc(AspectRatio / BinSize);
//        while Bin >= Bins.Count do
//        begin
//          Bins.Add(0);
//          BinTitles.Add(Format('%0:g-%1:g', [LastBin, LastBin + BinSize]));
//          LastBin := LastBin + BinSize;
//        end;
//        Bins[Bin] := Bins[Bin] + 1;
//
//    end;
//    StartIndex := 0;
//    while (StartIndex < Bins.Count) and (Bins[StartIndex] = 0) do
//    begin
//      Inc(StartIndex);
//    end;
//    serAspectRatio.Clear;
//    for BinIndex := StartIndex to Bins.Count - 1 do
//    begin
//      serAspectRatio.AddBar(Bins[BinIndex], BinTitles[BinIndex], clTeeColor);
//    end;
//    Comparer := TElementValueComparer.Create;
//    try
//      EleAngleSortList.Sort(Comparer);
//    finally
//      Comparer.Free;
//    end;
//    rdgAspectRatio.BeginUpdate;
//    try
//      if EleAngleSortList.Count = 0 then
//      begin
//        rdgAspectRatio.RowCount := 2;
//        rdgAspectRatio.Cells[0,1] := '';
//        rdgAspectRatio.Cells[1,1] := '';
//      end
//      else
//      begin
//        rdgAspectRatio.RowCount := EleAngleSortList.Count + 1;
//        for index := 0 to EleAngleSortList.Count - 1 do
//        begin
//          SortItem := EleAngleSortList[index];
//          rdgAspectRatio.Cells[0,index+1] := Format('%.4f', [SortItem.Value]);
//          rdgAspectRatio.Cells[1,index+1] := IntToStr(SortItem.ElementNumber)
//        end;
//      end;
//    finally
//      rdgAspectRatio.EndUpdate;
//    end;
//  finally
//    Bins.Free;
//    BinTitles.Free;
//    EleAngleSortList.Free;
//  end;
end;

procedure TfrmMeshInformation.UpdateDeviationCountHistogram;
//var
//  NegativeCounts: TIntegerList;
//  PositiveCounts: TIntegerList;
//  index: Integer;
//  Mesh2D: TSutraMesh2D;
//  ANode: TSutraNode2D;
//  CountList: TIntegerList;
//  ACount: Integer;
//  Delta: Integer;
//  Mesh3D: TSutraMesh3D;
//  SortList: TObjectList<TDeviationSortItem>;
//  NodeNumber: Integer;
//  ANode3D: TSutraNode3D;
//  Item: TDeviationSortItem;
//  LayerIndex: Integer;
//  NodeIndex: Integer;
//  Deviation: integer;
//  Sorter: TDeviationComparer;
begin
//  NegativeCounts := TIntegerList.Create;
//  PositiveCounts := TIntegerList.Create;
//  SortList:= TObjectList<TDeviationSortItem>.Create;
//  try
//    Mesh3D := frmGoPhast.PhastModel.Mesh;
//    Mesh2D := Mesh3D.Mesh2D;
//    for index := 0 to Mesh2D.Nodes.Count - 1 do
//    begin
//      ANode := Mesh2D.Nodes[index];
//      Delta := ANode.ElementCount - ANode.IdealElementCount;
//      Deviation := Delta;
//      if Delta >= 0 then
//      begin
//        CountList := PositiveCounts;
//      end
//      else
//      begin
//        Delta := - Delta;
//        CountList := NegativeCounts;
//      end;
//      while CountList.Count <= Delta do
//      begin
//        CountList.Add(0);
//      end;
//      CountList[Delta] := CountList[Delta] + 1;
//      if Abs(Delta) > 1 then
//      begin
//        NodeNumber := -1;
//        if Mesh3D.MeshType = mt3D then
//        begin
//          if Mesh3D.LayerCount = 0 then
//          begin
//            Exit;
//          end;
//          for LayerIndex := 0 to Mesh3D.LayerCount do
//          begin
//            ANode3D := Mesh3D.NodeArray[
//              LayerIndex,ANode.Number];
//            if ANode3D.Active then
//            begin
//              NodeNumber := ANode3D.Number+1;
//              break;
//            end
//          end;
//        end
//        else
//        begin
//          NodeNumber := ANode.Number+1;
//        end;
//        if NodeNumber >= 0 then
//        begin
//          Item := TDeviationSortItem.Create;
//          SortList.Add(Item);
//          Item.Deviation := Deviation;
//          Item.NodeNumber := NodeNumber;
//        end;
//      end;
//    end;
//    serDeviations.Clear;
//    for Index := NegativeCounts.Count - 1 downto 1 do
//    begin
//      ACount := NegativeCounts[Index];
//      serDeviations.AddBar(ACount, IntToStr(-Index), clTeeColor);
//    end;
//    for Index := 0 to PositiveCounts.Count - 1 do
//    begin
//      ACount := PositiveCounts[Index];
//      serDeviations.AddBar(ACount, IntToStr(Index), clTeeColor);
//    end;
//    if SortList.Count > 0 then
//    begin
//      Sorter := TDeviationComparer.Create;
//      try
//        SortList.Sort(Sorter);
//      finally
//        Sorter.Free;
//      end;
//      rdgNodes.Visible := True;
//      splNodes.Visible := True;
//      if splNodes.Left > rdgNodes.Left then
//      begin
//        splNodes.Left := rdgNodes.Left
//      end;
//      rdgNodes.RowCount := SortList.Count + 1;
//      for NodeIndex := 0 to SortList.Count - 1 do
//      begin
//        Item := SortList[NodeIndex];
//        rdgNodes.Cells[0, NodeIndex+1] := IntToStr(Item.Deviation);
//        rdgNodes.Cells[1, NodeIndex+1] := IntToStr(Item.NodeNumber);
//      end;
//    end
//    else
//    begin
//      rdgNodes.Visible := False;
//      splNodes.Visible := False;
//    end;
//  finally
//    SortList.Free;
//    PositiveCounts.Free;
//    NegativeCounts.Free;
//  end;
end;

{ TElementAngleComparer }

function TElementValueComparer.Compare(const Left,
  Right: TEleValueSortItem): Integer;
begin
  result := Sign(Left.Value - Right.Value);
  if result = 0 then
  begin
    result := Left.ElementNumber - Right.ElementNumber;
  end;
end;

{ TDeviationComparer }

function TDeviationComparer.Compare(const Left,
  Right: TDeviationSortItem): Integer;
begin
  result := Right.Deviation - Left.Deviation;
  if result = 0 then
  begin
    result := Left.NodeNumber - Right.NodeNumber;
  end;
end;

end.
