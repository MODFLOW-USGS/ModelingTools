unit frmSwrReachGeometryUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, frameGridUnit,
  StdCtrls, Buttons, ExtCtrls, JvPageList, JvExExtCtrls, JvNetscapeSplitter,
  JvExControls, ModflowSwrReachGeometryUnit, UndoItems, frameReachGeomGridUnit,
  framePlotGridUnit, Generics.Collections;

type
  TCrossSectionColumns = (cscX, scsElev);

  TTableColumns = (tcElev, tcVol, tcWetPerimeter, tcSurfaceArea,
    tcCrossSectionArea);

  TUndoReachGeom = class(TCustomUndo)
  private
    FOldReachGeom: TReachGeometryCollection;
    FNewReachGeom: TReachGeometryCollection;
  protected
    function Description: string; override;
  public
    constructor Create(var ReachGeometry: TReachGeometryCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSwrReachGeometry = class(TfrmCustomGoPhast)
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    jvplGeometry: TJvPageList;
    splttrMain: TJvNetscapeSplitter;
    jvspCrossSection: TJvStandardPage;
    frameCrossSection: TframePlotGrid;
    jvspTable: TJvStandardPage;
    frameTable: TframePlotGrid;
    jvspBlank: TJvStandardPage;
    frameGeometry: TframeReachGeomGrid;
    procedure FormCreate(Sender: TObject); override;
    procedure frameGeometryGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameGeometrysbAddClick(Sender: TObject);
    procedure frameGeometrysbInsertClick(Sender: TObject);
    procedure frameGeometrysbDeleteClick(Sender: TObject);
    procedure frameGeometryseNumberChange(Sender: TObject);
    procedure frameGeometryGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameGeometryGridEndUpdate(Sender: TObject);
    procedure frameCrossSectionsbAddClick(Sender: TObject);
    procedure frameCrossSectionsbInsertClick(Sender: TObject);
    procedure frameCrossSectionsbDeleteClick(Sender: TObject);
    procedure frameCrossSectionseNumberChange(Sender: TObject);
    procedure frameTablesbAddClick(Sender: TObject);
    procedure frameTablesbInsertClick(Sender: TObject);
    procedure frameTablesbDeleteClick(Sender: TObject);
    procedure frameTableseNumberChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure frameTableGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameCrossSectionGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure FormDestroy(Sender: TObject); override;
    procedure frameTableGridEndUpdate(Sender: TObject);
    procedure frameCrossSectionGridEndUpdate(Sender: TObject);
    procedure frameGeometryGridRowMoved(Sender: TObject; FromIndex,
      ToIndex: Integer);
    procedure frameGeometryGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameCrossSectionpbPlotPaint(Sender: TObject);
    procedure frameTablepbPlotPaint(Sender: TObject);
    procedure frameTableGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FReachGeometry: TReachGeometryCollection;
    FSelectedRow: Integer;
    FDisplayingReach: Boolean;
    procedure AssignReachToRow(AReach: TReachGeometryItem);
    procedure DisplayReachCrossSection(AReach: TReachGeometryItem);
    procedure DisplayReachTable(AReach: TReachGeometryItem);
    procedure DisplayCurrentRow(ARow: Integer = -1);
    function GetCurrentReach: TReachGeometryItem;
    procedure GetData;
    procedure SetData;
    procedure RenumberGeomGridRows;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSwrReachGeometry: TfrmSwrReachGeometry;

implementation

uses
  RbwDataGrid4, frmGoPhastUnit, xygraph;

resourcestring
  StrGeoNum = 'IGEONUM';
  StrNameIGEONUM = 'Name';
  StrGeometryTypeIGEOT = 'Geometry Type (IGEOTYPE)';
  StrConductanceMethod = 'Conductance Method (IGCNDOP)';
  StrManningsRoughness = 'Manning’s Roughness (GMANNING)';
  StrWidthGWIDTH = 'Width (GWIDTH)';
  StrBottomElevationGB = 'Bottom Elevation (GBELEV)';
  StrSideSlopeGSSLOPE = 'Side Slope (GSSLOPE)';
  StrConductanceGCND = 'Conductance (GCND)';
  StrLeakanceGLK = 'Leakance (GLK)';
  StrAverageHorizontalD = 'Average Horizontal Distance to Cell Center (GCNDL' +
  'N)';
  StrExtinctionDepthGE = 'Extinction Depth (GETEXTD)';
  StrXXB = 'X (XB)';
  StrBottomElevationEL = 'Bottom Elevation (ELEVB)';
  StrElevationELEV = 'Elevation (ELEV)';
  StrVolumeVOL = 'Volume (VOL)';
  StrWettedPerimeterWE = 'Wetted Perimeter (WETPER)';
  StrSurfaceAreaSAREA = 'Surface Area (SAREA)';
  StrCrossSectionalArea = 'Cross-Sectional Area (XAREA)';

{$R *.dfm}

procedure TfrmSwrReachGeometry.DisplayReachCrossSection(
  AReach: TReachGeometryItem);
var
  Grid: TRbwDataGrid4;
  index: Integer;
  Item: TReachCrossSectionItem;
begin
  Grid := frameCrossSection.Grid;
  frameCrossSection.seNumber.AsInteger := AReach.CrossSection.Count;
  frameCrossSection.seNumber.OnChange(nil);
  if AReach.CrossSection.Count > 0 then
  begin
    Grid.BeginUpdate;
    try
      for index := 0 to AReach.CrossSection.Count - 1 do
      begin
        Item := AReach.CrossSection[index];
        Grid.RealValue[Ord(cscX),index+1] := Item.X;
        Grid.RealValue[Ord(scsElev),index+1] := Item.Elev;
      end;
    finally
      Grid.EndUpdate;
    end;
  end
  else
  begin
    Grid.Row := 1;
    frameCrossSection.ClearSelectedRow
  end;
end;

procedure TfrmSwrReachGeometry.DisplayReachTable(AReach: TReachGeometryItem);
var
  Grid: TRbwDataGrid4;
  index: Integer;
  Item: TReachTableItem;
begin
  Grid := frameTable.Grid;
  frameTable.seNumber.AsInteger := AReach.Table.Count;
  frameTable.seNumber.OnChange(nil);
  if AReach.Table.Count > 0 then
  begin
    Grid.BeginUpdate;
    try
      for index := 0 to AReach.Table.Count - 1 do
      begin
        Item := AReach.Table[index];
        Grid.RealValue[Ord(tcElev),index+1] := Item.Elevation;
        Grid.RealValue[Ord(tcVol),index+1] := Item.Volume;
        Grid.RealValue[Ord(tcWetPerimeter),index+1] := Item.WettedPerimeter;
        Grid.RealValue[Ord(tcSurfaceArea),index+1] := Item.SurfaceArea;
        Grid.RealValue[Ord(tcCrossSectionArea),index+1] := Item.CrossSectionArea;
      end;
    finally
      Grid.EndUpdate;
    end;
  end
  else
  begin
    Grid.Row := 1;
    frameTable.ClearSelectedRow;
  end;
end;

procedure TfrmSwrReachGeometry.AssignReachToRow(AReach: TReachGeometryItem);
var
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
begin
  RowIndex := AReach.Index + 1;
  Grid := frameGeometry.Grid;
  Grid.BeginUpdate;
  try
    Grid.Cells[Ord(gcName), RowIndex] := AReach.Name;
    Grid.ItemIndex[Ord(gcType), RowIndex] := Ord(AReach.GeometryType);
    Grid.ItemIndex[Ord(gcMethod), RowIndex] := Ord(AReach.ConductanceMethod);
    Grid.RealValue[Ord(gcRoughness), RowIndex] := AReach.Roughness;
    Grid.RealValue[Ord(gcWidth), RowIndex] := AReach.Width;
    Grid.RealValue[Ord(gcBottomElevation), RowIndex] := AReach.BottomElevation;
    Grid.RealValue[Ord(gcSideSlope), RowIndex] := AReach.SideSlope;
    Grid.RealValue[Ord(gcConductance), RowIndex] := AReach.Conductance;
    Grid.RealValue[Ord(gcLeakance), RowIndex] := AReach.Leakance;
    Grid.RealValue[Ord(gcCenterDistance), RowIndex] := AReach.CenterDistance;
    Grid.RealValue[Ord(gcExtinctionDepth), RowIndex] := AReach.ExtinctionDepth;
  finally
    Grid.EndUpdate;
  end;
end;

procedure TfrmSwrReachGeometry.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSwrReachGeometry.DisplayCurrentRow(ARow: Integer = -1);
var
  AReach: TReachGeometryItem;
//  procedure DisplayGeomFrames;
//  begin
////    frameGeometry.Align := alTop;
////    frameGeometry.Height := ClientHeight
////      - jvplGeometry.Height - splttrMain.Height - pnlBottom.Height;
////    jvplGeometry.Visible := True;
////    splttrMain.Visible := True;
////     (frameGeometry.Height + jvplGeometry.Height) div 2;
////    splttrMain.Top := jvplGeometry.Top - splttrMain.Height;
//  end;
begin
  if FReachGeometry = nil then
  begin
    Exit;
  end;
  FDisplayingReach := True;
  try
    if ARow = -1 then
    begin
      ARow := frameGeometry.Grid.SelectedRow;
    end;
    FSelectedRow := ARow;
    if (ARow >= 1) and (ARow <= FReachGeometry.Count) then
    begin
      AReach := FReachGeometry[ARow-1];
      case AReach.GeometryType of
        gtRectangular, gtTrapezoidal, gtWholeCell:
          begin
            jvplGeometry.ActivePage := jvspBlank;
//            splttrMain.Visible := False;
//            frameGeometry.Align := alClient;
          end;
        gtIrregular:
          begin
//            DisplayGeomFrames;
            jvplGeometry.ActivePage := jvspCrossSection;
            DisplayReachCrossSection(AReach);
            frameCrossSection.pbPlot.Invalidate;
          end;
        gtTable:
          begin
//            DisplayGeomFrames;
            jvplGeometry.ActivePage := jvspTable;
            DisplayReachTable(AReach);
            frameTable.pbPlot.Invalidate;
          end;
        else Assert(False);
      end;
    end
    else
    begin
      jvplGeometry.ActivePage := jvspBlank;
//      jvplGeometry.Visible := False;
//      splttrMain.Visible := False;
//      frameGeometry.Align := alClient;
    end;
  finally
    FDisplayingReach := False;
  end;
end;

function TfrmSwrReachGeometry.GetCurrentReach: TReachGeometryItem;
begin
  if FReachGeometry = nil then
  begin
    result := nil;
    Exit;
  end;
  if (frameGeometry.Grid.SelectedRow >= 1)
    and (frameGeometry.Grid.SelectedRow <= FReachGeometry.Count) then
  begin
    result := FReachGeometry[frameGeometry.Grid.SelectedRow - 1];
  end
  else
  begin
    result := nil;
  end;
end;

procedure TfrmSwrReachGeometry.GetData;
var
  Index: Integer;
begin
  FReachGeometry := TReachGeometryCollection.Create(nil);
  FReachGeometry.Assign(frmGoPhast.PhastModel.SwrReachGeometry);
  frameGeometry.seNumber.AsInteger := FReachGeometry.Count;
  frameGeometry.Grid.BeginUpdate;
  try
    RenumberGeomGridRows;
    for Index := 0 to FReachGeometry.Count - 1 do
    begin
      AssignReachToRow(FReachGeometry[Index]);
    end;
  finally
    frameGeometry.Grid.EndUpdate;
  end;
end;

procedure TfrmSwrReachGeometry.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoReachGeom.Create(FReachGeometry));
end;

procedure TfrmSwrReachGeometry.RenumberGeomGridRows;
var
  RowIndex: Integer;
begin
  for RowIndex := 1 to frameGeometry.Grid.RowCount - 1 do
  begin
    frameGeometry.Grid.Cells[Ord(gcNumber), RowIndex] := IntToStr(RowIndex);
  end;
end;

procedure TfrmSwrReachGeometry.FormCreate(Sender: TObject);
begin
  inherited;
  frameGeometry.Grid.BeginUpdate;
  try
    frameGeometry.Grid.Cells[Ord(gcNumber), 0] := StrGeoNum;
    frameGeometry.Grid.Cells[Ord(gcName), 0] := StrNameIGEONUM;
    frameGeometry.Grid.Cells[Ord(gcType), 0] := StrGeometryTypeIGEOT;
    frameGeometry.Grid.Cells[Ord(gcMethod), 0] := StrConductanceMethod;
    frameGeometry.Grid.Cells[Ord(gcRoughness), 0] := StrManningsRoughness;
    frameGeometry.Grid.Cells[Ord(gcWidth), 0] := StrWidthGWIDTH;
    frameGeometry.Grid.Cells[Ord(gcBottomElevation), 0] := StrBottomElevationGB;
    frameGeometry.Grid.Cells[Ord(gcSideSlope), 0] := StrSideSlopeGSSLOPE;
    frameGeometry.Grid.Cells[Ord(gcConductance), 0] := StrConductanceGCND;
    frameGeometry.Grid.Cells[Ord(gcLeakance), 0] := StrLeakanceGLK;
    frameGeometry.Grid.Cells[Ord(gcCenterDistance), 0] := StrAverageHorizontalD;
    frameGeometry.Grid.Cells[Ord(gcExtinctionDepth), 0] := StrExtinctionDepthGE;
  finally
    frameGeometry.Grid.EndUpdate;
  end;

  frameCrossSection.Grid.BeginUpdate;
  try
    frameCrossSection.Grid.Cells[Ord(cscX), 0] := StrXXB;
    frameCrossSection.Grid.Cells[Ord(scsElev), 0] := StrBottomElevationEL;
  finally
    frameCrossSection.Grid.EndUpdate;
  end;

  frameTable.Grid.BeginUpdate;
  try
    frameTable.Grid.Cells[Ord(tcElev), 0] := StrElevationELEV;
    frameTable.Grid.Cells[Ord(tcVol), 0] := StrVolumeVOL;
    frameTable.Grid.Cells[Ord(tcWetPerimeter), 0] := StrWettedPerimeterWE;
    frameTable.Grid.Cells[Ord(tcSurfaceArea), 0] := StrSurfaceAreaSAREA;
    frameTable.Grid.Cells[Ord(tcCrossSectionArea), 0] := StrCrossSectionalArea;
  finally
    frameTable.Grid.EndUpdate;
  end;

  FSelectedRow := -1;

//  jvplGeometry.Visible := False;
//  splttrMain.Visible := False;
//  frameGeometry.Align := alClient;

  GetData;
end;

procedure TfrmSwrReachGeometry.FormDestroy(Sender: TObject);
begin
  inherited;
  FReachGeometry.Free;
end;

procedure TfrmSwrReachGeometry.frameCrossSectionGridEndUpdate(Sender: TObject);
begin
  inherited;
  if not FDisplayingReach then
  begin
    frameCrossSection.GridEndUpdate(Sender);
    frameCrossSection.pbPlot.Invalidate;
  end;

end;

procedure TfrmSwrReachGeometry.frameCrossSectionGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  AReach: TReachGeometryItem;
  Grid: TRbwDataGrid4;
  Item: TReachCrossSectionItem;
  CrossSectionColumn: TCrossSectionColumns;
begin
  inherited;
  if (ACol >= 0) and (ARow >= 1) and (Value <> '') and not FDisplayingReach then
  begin
    AReach := GetCurrentReach;
    if (AReach <> nil) then
    begin
      while ARow > AReach.CrossSection.Count do
      begin
        AReach.CrossSection.Add;
      end;
      frameCrossSection.seNumber.AsInteger := AReach.CrossSection.Count;
      Grid := frameCrossSection.Grid;
      Item := AReach.CrossSection[ARow-1];
      CrossSectionColumn := TCrossSectionColumns(ACol);
      case CrossSectionColumn of
        cscX:
          begin
            Item.X := Grid.RealValueDefault[ACol, ARow, 0];
          end;
        scsElev:
          begin
            Item.Elev := Grid.RealValueDefault[ACol, ARow, 0];
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TfrmSwrReachGeometry.frameCrossSectionpbPlotPaint(Sender: TObject);
var
  XList, YList: TList<Double>;
  Grid: TRbwDataGrid4;
  X: double;
  Y: double;
  RowIndex: Integer;
  Data: Tdatatype;
  MinX: double;
  MaxX: double;
  MinY: double;
  MaxY: double;
  index: integer;
begin
  inherited;
  XList := TList<Double>.Create;
  YList := TList<Double>.Create;
  try
    Grid := frameCrossSection.Grid;
    XList.Capacity := Grid.RowCount - 1;
    YList.Capacity := Grid.RowCount - 1;
    MinX := 0;
    MaxX := 0;
    MinY := 0;
    MaxY := 0;
    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      if TryStrToFloat(Grid.Cells[Ord(cscX), RowIndex], X)
        and TryStrToFloat(Grid.Cells[Ord(scsElev), RowIndex], Y) then
      begin
        if XList.Count = 0 then
        begin
          MinX := X;
          MaxX := X;
          MinY := Y;
          MaxY := Y;
        end
        else
        begin
          if X < MinX then
          begin
            MinX := X;
          end
          else if X > MaxX then
          begin
            MaxX := X;
          end;
          if Y < MinY then
          begin
            MinY := Y;
          end
          else if Y > MaxY then
          begin
            MaxY := Y;
          end;
        end;
        XList.Add(X);
        YList.Add(Y);
      end;
    end;
    if XList.Count > 0 then
    begin
      xysetdataarray(Data, XList.Count, 1);
      try
        xycleargraph(frameCrossSection.pbPlot,clWhite,clBlack,1);

        xystartgraph(0, 100, 0, 100, 50, 50, 50, 50, clipoff);

        xyxaxis(clBlack,MinX,MaxX,
          (MaxX-MinX)/10,0,'X',1,False,False,True, 2);

        xyyaxis(clBlack,MinY,MaxY,
          (MaxY-MinY)/10,0,'Elevation',5,False,False,True, 2);

        for index := 0 to XList.Count - 1 do
        begin
          Data[index+1, 0] := XList[index];
          Data[index+1, 1] := YList[index];
        end;

        xysymbol(1,0,0);
        xyplotarray(data,0,2);

        xyfinish;
      except on E: exception do
        begin
          ShowMessage(e.message);
          Exit;
        end;
      end;
    end;
  finally
    XList.Free;
    YList.Free;
  end;
end;

procedure TfrmSwrReachGeometry.frameCrossSectionsbAddClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  AReach := GetCurrentReach;
  if (AReach <> nil) then
  begin
    AReach.CrossSection.Add;
  end;
  frameCrossSection.Grid.Row := frameCrossSection.Grid.RowCount -1;
  frameCrossSection.sbAddClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameCrossSectionsbDeleteClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;

  AReach := GetCurrentReach;
  if AReach <> nil then
  begin
    if frameCrossSection.Grid.SelectedRow >= 1 then
    begin
      AReach.CrossSection.Delete(frameCrossSection.Grid.SelectedRow-1)
    end;
  end;

  frameCrossSection.sbDeleteClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameCrossSectionsbInsertClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  AReach := GetCurrentReach;
  if AReach <> nil then
  begin
    if frameCrossSection.Grid.SelectedRow >= 1 then
    begin
      AReach.CrossSection.Insert(frameCrossSection.Grid.SelectedRow-1)
    end;
  end;
  frameCrossSection.sbInsertClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameCrossSectionseNumberChange(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  if FReachGeometry = nil then
  begin
    Exit;
  end;
  if not FDisplayingReach then
  begin
    AReach := GetCurrentReach;
    if (AReach <> nil) then
    begin
      if AReach.CrossSection.Count <> frameCrossSection.seNumber.AsInteger then
      begin
        AReach.CrossSection.Count := frameCrossSection.seNumber.AsInteger;
      end;
    end;
  end;
  frameCrossSection.seNumberChange(Sender);

end;

procedure TfrmSwrReachGeometry.frameGeometryGridBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  FirstIndex: Integer;
begin
  inherited;
  if (ACol = Ord(gcName)) and (ARow >= frameGeometry.Grid.FixedRows) then
  begin
    FirstIndex := frameGeometry.Grid.Cols[ACol].IndexOf(
      frameGeometry.Grid.Cells[ACol, ARow]);
    if FirstIndex <> ARow then
    begin
      frameGeometry.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmSwrReachGeometry.frameGeometryGridEndUpdate(Sender: TObject);
begin
  inherited;
  frameGeometry.GridEndUpdate(Sender);
  DisplayCurrentRow;
end;

procedure TfrmSwrReachGeometry.frameGeometryGridRowMoved(Sender: TObject;
  FromIndex, ToIndex: Integer);
begin
  inherited;
//  FReachGeometry[FromIndex-1].Index := ToIndex-1;
  RenumberGeomGridRows;
  DisplayCurrentRow;
end;

procedure TfrmSwrReachGeometry.frameGeometryGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  GeoCol: TGeometryColumns;
  TypeIndex: Integer;
  MethodIndex: Integer;
begin
  inherited;
  if not frameGeometry.Grid.Drawing and (ARow <> FSelectedRow) then
  begin
    FSelectedRow := ARow;
    if not frameGeometry.Grid.DistributingText then
    begin
      DisplayCurrentRow(FSelectedRow);
    end;
  end;
  if ARow >= 1 then
  begin
    GeoCol := TGeometryColumns(ACol);
    TypeIndex := frameGeometry.Grid.ItemIndex[Ord(gcType), ARow];
    MethodIndex := frameGeometry.Grid.ItemIndex[Ord(gcMethod), ARow];
    case GeoCol of
      gcName, gcType, gcMethod, gcRoughness: ;
      gcWidth, gcBottomElevation:
        begin
          if TypeIndex >= 0 then
          begin
            CanSelect := TGeometryType(TypeIndex)
              in [gtRectangular, gtTrapezoidal];
          end
          else
          begin
            CanSelect := False;
          end;
        end;
      gcSideSlope:
        begin
          if TypeIndex >= 0 then
          begin
            CanSelect := TGeometryType(TypeIndex) = gtTrapezoidal;
          end
          else
          begin
            CanSelect := False;
          end;
        end;
      gcConductance:
        begin
          if MethodIndex >= 0 then
          begin
            CanSelect := TConductanceMethod(MethodIndex) = cmFixed;
          end
          else
          begin
            CanSelect := False;
          end;
        end;
      gcLeakance:
        begin
          if MethodIndex >= 0 then
          begin
            CanSelect := TConductanceMethod(MethodIndex)
              in [cmReachLeakance, cmLeakanceAndK];
          end
          else
          begin
            CanSelect := False;
          end;
        end;
      gcCenterDistance:
        begin
          if MethodIndex >= 0 then
          begin
            CanSelect := TConductanceMethod(MethodIndex)
              in [cmK, cmLeakanceAndK];
          end
          else
          begin
            CanSelect := False;
          end;
        end;
      gcExtinctionDepth:
        begin
          if TypeIndex >= 0 then
          begin
            CanSelect := TGeometryType(TypeIndex) = gtWholeCell;
          end
          else
          begin
            CanSelect := False;
          end;
        end;
    end;
  end;
end;

procedure TfrmSwrReachGeometry.frameGeometryGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  AReach: TReachGeometryItem;
  GeoCol: TGeometryColumns;
  Grid: TRbwDataGrid4;
  ItemIndex: Integer;
begin
  inherited;
  Grid := frameGeometry.Grid;
  if (ARow >= 1) and (ACol in [Ord(gcType), Ord(gcMethod)]) then
  begin
    Grid.Invalidate;
  end;
  if (ACol >= 0) and (ARow >= 1) and (Value <> '') then
  begin
    while ARow > FReachGeometry.Count do
    begin
      FReachGeometry.Add;
    end;
    AReach := FReachGeometry[ARow-1];
    GeoCol := TGeometryColumns(ACol);
    case GeoCol of
      gcName:
        begin
          AReach.Name := Value;
        end;
      gcType:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AReach.GeometryType := TGeometryType(ItemIndex);
          end;
        end;
      gcMethod:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            AReach.ConductanceMethod := TConductanceMethod(ItemIndex);
          end;
        end;
      gcRoughness:
        begin
          AReach.Roughness := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcWidth:
        begin
          AReach.Width := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcBottomElevation:
        begin
          AReach.BottomElevation := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcSideSlope:
        begin
          AReach.SideSlope := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcConductance:
        begin
          AReach.Conductance := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcLeakance:
        begin
          AReach.Leakance := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcCenterDistance:
        begin
          AReach.CenterDistance := Grid.RealValueDefault[ACol, ARow,0];
        end;
      gcExtinctionDepth:
        begin
          AReach.ExtinctionDepth := Grid.RealValueDefault[ACol, ARow,0];
        end;
    end;
  end;
  if (not Grid.DistributingText) and (ACol = Ord(gcType))  then
  begin
    DisplayCurrentRow(ARow);
  end;
end;

procedure TfrmSwrReachGeometry.frameGeometrysbAddClick(Sender: TObject);
begin
  inherited;
  FReachGeometry.Add;
  frameGeometry.Grid.Row := frameGeometry.Grid.RowCount -1;
  frameGeometry.sbAddClick(Sender);
end;

procedure TfrmSwrReachGeometry.frameGeometrysbDeleteClick(Sender: TObject);
begin
  inherited;
  if frameGeometry.Grid.SelectedRow >= 1 then
  begin
    FReachGeometry.Delete(frameGeometry.Grid.SelectedRow-1)
  end;
  frameGeometry.sbDeleteClick(Sender);
  DisplayCurrentRow;
end;

procedure TfrmSwrReachGeometry.frameGeometrysbInsertClick(Sender: TObject);
begin
  inherited;
  if frameGeometry.Grid.SelectedRow >= 1 then
  begin
    FReachGeometry.Insert(frameGeometry.Grid.SelectedRow-1)
  end;
  frameGeometry.sbInsertClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameGeometryseNumberChange(Sender: TObject);
begin
  inherited;
  if FReachGeometry <> nil then
  begin
    FReachGeometry.Count := frameGeometry.seNumber.AsInteger;
    frameGeometry.Grid.BeginUpdate;
    try
      frameGeometry.seNumberChange(Sender);
      RenumberGeomGridRows;
    finally
      frameGeometry.Grid.EndUpdate;
    end;
  end;
end;

procedure TfrmSwrReachGeometry.frameTableGridEndUpdate(Sender: TObject);
begin
  inherited;
  if not FDisplayingReach then
  begin
    frameTable.GridEndUpdate(Sender);
    frameTable.pbPlot.Invalidate;
  end;

end;

procedure TfrmSwrReachGeometry.frameTableGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  frameTable.pbPlot.Invalidate;
end;

procedure TfrmSwrReachGeometry.frameTableGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  AReach: TReachGeometryItem;
  Item: TReachTableItem;
  TableColumn: TTableColumns;
  Grid: TRbwDataGrid4;
begin
  inherited;

  if (ACol >= 0) and (ARow >= 1) and (Value <> '') then
  begin
    AReach := GetCurrentReach;
    if (AReach <> nil) then
    begin
      while ARow > AReach.Table.Count do
      begin
        AReach.Table.Add;
      end;
      frameTable.seNumber.AsInteger := AReach.Table.Count;

      Grid := frameTable.Grid;
      Item := AReach.Table[ARow-1];
      TableColumn := TTableColumns(ACol);
      case TableColumn of
        tcElev:
          begin
            Item.Elevation := Grid.RealValueDefault[ACol, ARow,0];
          end;
        tcVol:
          begin
            Item.Volume := Grid.RealValueDefault[ACol, ARow,0];
          end;
        tcWetPerimeter:
          begin
            Item.WettedPerimeter := Grid.RealValueDefault[ACol, ARow,0];
          end;
        tcSurfaceArea:
          begin
            Item.SurfaceArea := Grid.RealValueDefault[ACol, ARow,0];
          end;
        tcCrossSectionArea:
          begin
            Item.CrossSectionArea := Grid.RealValueDefault[ACol, ARow,0];
          end;
        else
          Assert(False);
      end;
    end;
  end;
end;

procedure TfrmSwrReachGeometry.frameTablepbPlotPaint(Sender: TObject);
var
  ElevList: TList<Double>;
  VolumeList: TList<Double>;
  WetPerimeterList: TList<Double>;
  SurfaceAreaList: TList<Double>;
  CrossSectionAreaList: TList<Double>;
  Data: Tdatatype;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  Elev: double;
  Volume: double;
  WetPerimeter: double;
  SurfaceArea: double;
  CrossSectionArea: double;
  ElevMin: Double;
  ElevMax: Double;
  VolumeMin: Double;
  VolumeMax: Double;
  WetPerimeterMin: Double;
  WetPerimeterMax: Double;
  SurfaceAreaMin: Double;
  SurfaceAreaMax: Double;
  CrossSectionAreaMin: Double;
  CrossSectionAreaMax: Double;
  index: Integer;
  PlotGraph: Boolean;
begin
  inherited;
  ElevList := TList<Double>.Create;
  VolumeList := TList<Double>.Create;
  WetPerimeterList := TList<Double>.Create;
  SurfaceAreaList := TList<Double>.Create;
  CrossSectionAreaList := TList<Double>.Create;
  try
    Grid := frameTable.Grid;
    ElevMin := 0;
    ElevMax := 0;
    VolumeMin := 0;
    VolumeMax := 0;
    WetPerimeterMin := 0;
    WetPerimeterMax := 0;
    SurfaceAreaMin := 0;
    SurfaceAreaMax := 0;
    CrossSectionAreaMin := 0;
    CrossSectionAreaMax := 0;
    for RowIndex := 1 to Grid.RowCount - 1 do
    begin
      if TryStrToFloat(Grid.Cells[Ord(tcElev), RowIndex], Elev)
        and TryStrToFloat(Grid.Cells[Ord(tcVol), RowIndex], Volume)
        and TryStrToFloat(Grid.Cells[Ord(tcWetPerimeter), RowIndex], WetPerimeter)
        and TryStrToFloat(Grid.Cells[Ord(tcSurfaceArea), RowIndex], SurfaceArea)
        and TryStrToFloat(Grid.Cells[Ord(tcCrossSectionArea), RowIndex], CrossSectionArea)
        then
      begin
        if ElevList.Count = 0 then
        begin
          ElevMin := Elev;
          ElevMax := Elev;
          VolumeMin := Volume;
          VolumeMax := Volume;
          WetPerimeterMin := WetPerimeter;
          WetPerimeterMax := WetPerimeter;
          SurfaceAreaMin := SurfaceArea;
          SurfaceAreaMax := SurfaceArea;
          CrossSectionAreaMin := CrossSectionArea;
          CrossSectionAreaMax := CrossSectionArea;
        end
        else
        begin
          if Elev < ElevMin then
          begin
            ElevMin := Elev
          end
          else if Elev > ElevMax then
          begin
            ElevMax := Elev;
          end;

          if Volume < VolumeMin then
          begin
            VolumeMin := Volume
          end
          else if Volume > VolumeMax then
          begin
            VolumeMax := Volume;
          end;

          if WetPerimeter < WetPerimeterMin then
          begin
            WetPerimeterMin := WetPerimeter
          end
          else if WetPerimeter > WetPerimeterMax then
          begin
            WetPerimeterMax := WetPerimeter;
          end;

          if SurfaceArea < SurfaceAreaMin then
          begin
            SurfaceAreaMin := SurfaceArea
          end
          else if SurfaceArea > SurfaceAreaMax then
          begin
            SurfaceAreaMax := SurfaceArea;
          end;

          if CrossSectionArea < CrossSectionAreaMin then
          begin
            CrossSectionAreaMin := CrossSectionArea
          end
          else if CrossSectionArea > CrossSectionAreaMax then
          begin
            CrossSectionAreaMax := CrossSectionArea;
          end;
        end;
        ElevList.Add(Elev);
        VolumeList.Add(Volume);
        WetPerimeterList.Add(WetPerimeter);
        SurfaceAreaList.Add(SurfaceArea);
        CrossSectionAreaList.Add(CrossSectionArea);
      end;
    end;
    if ElevList.Count > 0 then
    begin
      xycleargraph(frameTable.pbPlot,clWhite,clBlack,1);

      try
        xysetdataarray(Data, ElevList.Count, 1);

        // Volume vs Elev Graph
        PlotGraph := False;
        if Grid.Column <= 0 then
        begin
          xystartgraph(10, 50, 0, 50, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end
        else if Grid.Column = Ord(tcVol) then
        begin
          xystartgraph(10, 100, 0, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end;

        if PlotGraph then
        begin
          xyxaxis(clBlack,ElevMin,ElevMax,
            (ElevMax-ElevMin)/10,0,'Elevation',1,False,False,True, 2);

          xyyaxis(clBlack,VolumeMin,VolumeMax,
            (VolumeMax-VolumeMin)/10,0,'Volume',5,False,False,True, 2);

          for index := 0 to ElevList.Count - 1 do
          begin
            Data[index+1, 0] := ElevList[index];
            Data[index+1, 1] := VolumeList[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,0,2);

          xyfinish;
        end;

        // Volume vs Wetted Perimeter Graph
        PlotGraph := False;
        if Grid.Column <= 0 then
        begin
          xystartgraph(50, 100, 0, 50, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end
        else if Grid.Column = Ord(tcWetPerimeter) then
        begin
          xystartgraph(10, 100, 0, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end;

        if PlotGraph then
        begin
          xyxaxis(clBlack,ElevMin,ElevMax,
            (ElevMax-ElevMin)/10,0,'Elevation',1,False,False,True, 2);

          xyyaxis(clBlack,WetPerimeterMin,WetPerimeterMax,
            (WetPerimeterMax-WetPerimeterMin)/10,0,'Wetted Perimeter',5,False,False,True, 2);

          for index := 0 to ElevList.Count - 1 do
          begin
            Data[index+1, 0] := ElevList[index];
            Data[index+1, 1] := WetPerimeterList[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,0,2);

          xyfinish;
        end;

        // Volume vs Surface Area Graph
        PlotGraph := False;
        if Grid.Column <= 0 then
        begin
          xystartgraph(10, 50, 50, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end
        else if Grid.Column = Ord(tcSurfaceArea) then
        begin
          xystartgraph(10, 100, 0, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end;

        if PlotGraph then
        begin
          xyxaxis(clBlack,ElevMin,ElevMax,
            (ElevMax-ElevMin)/10,0,'Elevation',1,False,False,True, 2);

          xyyaxis(clBlack,SurfaceAreaMin,SurfaceAreaMax,
            (SurfaceAreaMax-SurfaceAreaMin)/10,0,'Surface Area',5,False,False,True, 2);

          for index := 0 to ElevList.Count - 1 do
          begin
            Data[index+1, 0] := ElevList[index];
            Data[index+1, 1] := SurfaceAreaList[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,0,2);

          xyfinish;
        end;

        // Volume vs Cross Section Area Graph
        PlotGraph := False;
        if Grid.Column <= 0 then
        begin
          xystartgraph(50, 100, 50, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end
        else if Grid.Column = Ord(tcCrossSectionArea) then
        begin
          xystartgraph(10, 100, 0, 100, 50, 50, 50, 50, clipoff);
          PlotGraph := True;
        end;

        if PlotGraph then
        begin
          xyxaxis(clBlack,ElevMin,ElevMax,
            (ElevMax-ElevMin)/10,0,'Elevation',1,False,False,True, 2);

          xyyaxis(clBlack,CrossSectionAreaMin,CrossSectionAreaMax,
            (CrossSectionAreaMax-CrossSectionAreaMin)/10,0,'Cross Sectional Area',5,False,False,True, 2);

          for index := 0 to ElevList.Count - 1 do
          begin
            Data[index+1, 0] := ElevList[index];
            Data[index+1, 1] := CrossSectionAreaList[index];
          end;

          xysymbol(1,0,0);
          xyplotarray(data,0,2);

          xyfinish;
        end;


      except on E: exception do
        begin
          ShowMessage(e.message);
          Exit;
        end;
      end;
    end;
  finally
    CrossSectionAreaList.Free;
    SurfaceAreaList.Free;
    WetPerimeterList.Free;
    VolumeList.Free;
    ElevList.Free;
  end;
end;

procedure TfrmSwrReachGeometry.frameTablesbAddClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  AReach := GetCurrentReach;
  if AReach <> nil then
  begin
    AReach.Table.Add;
  end;
  frameTable.Grid.Row := frameTable.Grid.RowCount -1;
  frameTable.sbAddClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameTablesbDeleteClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  AReach := GetCurrentReach;
  if AReach <> nil then
  begin
    if frameTable.Grid.SelectedRow >= 1 then
    begin
      AReach.Table.Delete(frameTable.Grid.SelectedRow-1)
    end;
  end;
  frameTable.sbDeleteClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameTablesbInsertClick(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  AReach := GetCurrentReach;
  if AReach <> nil then
  begin
    if frameTable.Grid.SelectedRow >= 1 then
    begin
      AReach.Table.Insert(frameTable.Grid.SelectedRow-1)
    end;
  end;
  frameTable.sbInsertClick(Sender);

end;

procedure TfrmSwrReachGeometry.frameTableseNumberChange(Sender: TObject);
var
  AReach: TReachGeometryItem;
begin
  inherited;
  if not FDisplayingReach then
  begin
    AReach := GetCurrentReach;
    if AReach <> nil then
    begin
      AReach.Table.Count := frameTable.seNumber.AsInteger;
    end;
  end;
  frameTable.seNumberChange(Sender);

end;

{ TUndoReachGeom }

constructor TUndoReachGeom.Create(var ReachGeometry: TReachGeometryCollection);
begin
  FOldReachGeom := TReachGeometryCollection.Create(nil);
  FOldReachGeom.Assign(frmGoPhast.PhastModel.SwrReachGeometry);

  FNewReachGeom := ReachGeometry;
  ReachGeometry := nil;
end;

function TUndoReachGeom.Description: string;
begin
  result := 'change SWR reach geometry';
end;

destructor TUndoReachGeom.Destroy;
begin
  FOldReachGeom.Free;
  FNewReachGeom.Free;
  inherited;
end;

procedure TUndoReachGeom.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SwrReachGeometry := FNewReachGeom;
end;

procedure TUndoReachGeom.Undo;
begin
  frmGoPhast.PhastModel.SwrReachGeometry := FOldReachGeom;
  inherited;
end;

end.
