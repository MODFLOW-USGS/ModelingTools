{@abstract(The main purpose of @name is to define @link(TfrmGoTo)
  which is used to move the viewpoint to a selected position,
 cell, or @link(TScreenObject).)}
unit frmGoToUnit;

interface

uses
  Winapi.Windows, System.UITypes,UndoItemsScreenObjects, SysUtils, Types,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls, Buttons,
  CompressedImageUnit, ExtCtrls, Spin, ArgusDataEntry, Mask, JvExMask, JvSpin,
  AbstractGridUnit, GoPhastTypes, ScreenObjectUnit, GrayTabs;

type
  TNavigationType = (ntPosition, ntGrid, mtMesh, ntDisv, ntObject, ntImage);
  TMeshMoveTo = (mmtNode, mmtElement);

  {@abstract(@name is used to move the viewpoint to a selected position,
   cell, or @link(TScreenObject).)}
  TfrmGoTo = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name indicates that the position on  the front view of the model
    // will be changed.
    // See @link(cbClick).
    cbFront: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the object being moved to will also be selected.
    cbSelectObject: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the position on  the side view of the model
    // will be changed.
    // See @link(cbClick).
    cbSide: TCheckBox;
    // @name: TCheckBox;
    // @name indicates that the position on  the top view of the model
    // will be changed.
    // See @link(cbClick).
    cbTop: TCheckBox;
    // @name: TLabel;
    // @name displays "Column".
    lblCol: TLabel;
    // @name: TLabel;
    // @name displays "Layer".
    lblLay: TLabel;
    // @name: TLabel;
    // @name displays "Row".
    lblRow: TLabel;
    // @name: TLabel;
    // @name displays "X".
    lblX: TLabel;
    // @name: TLabel;
    // @name displays "X'".
    lblXPrime: TLabel;
    // @name: TLabel;
    // @name displays "Y".
    lblY: TLabel;
    // @name: TLabel;
    // @name displays "Y'".
    lblYPrime: TLabel;
    // @name: TLabel;
    // @name displays "Z".
    lblZ: TLabel;
    // @name: TListView;
    // @name displays a list of @link(TScreenObject)s.
    lvScreenObjects: TListView;
    // @name: TPageControl;
    // @name show the options for moving to a new location.
    pcMain: TPageControl;
    // @name: TPanel;
    // @name holds the buttons at the bottom of the disk.
    pnlBottom: TPanel;
    // @name: TPanel;
    // @name holds @link(cbSelectObject).
    pnlObject: TPanel;
    // @name: TRbwDataEntry;
    // @name specifies the X-coordinate to move to.
    rdeX: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the X'-coordinate to move to.
    rdeXPrime: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Y-coordinate to move to.
    rdeY: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Y'-coordinate to move to.
    rdeYPrime: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name specifies the Z-coordinate to move to.
    rdeZ: TRbwDataEntry;
    // @name: TTabSheet;
    // @name holds controls for moving to a particular cell.
    tabCell: TTabSheet;
    // @name: TTabSheet;
    // @name holds controls for moving to a particular @link(TScreenObject).
    tabObject: TTabSheet;
    // @name: TTabSheet;
    // @name holds controls for moving to a position.
    tabPosition: TTabSheet;
    seCol: TJvSpinEdit;
    seRow: TJvSpinEdit;
    seLayer: TJvSpinEdit;
    tabImage: TTabSheet;
    lvImages: TListView;
    lblModel: TLabel;
    comboModel: TComboBox;
    tabMesh: TTabSheet;
    rgNodeElement: TRadioGroup;
    lblNumber: TLabel;
    seNumber: TJvSpinEdit;
    tabDISV: TTabSheet;
    seDisvLayer: TJvSpinEdit;
    seDisvCell: TJvSpinEdit;
    lblDisvCell: TLabel;
    lblDisvLayer: TLabel;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name enables and disables controls depending on which of
    // @link(cbFront), @link(cbSide), and @link(cbTop) are checked.
    procedure cbClick(Sender: TObject);
    // @name initializes @classname and calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name enables moving to a particular cell if a grid has been defined.
    procedure FormShow(Sender: TObject);
    procedure pcMainChange(Sender: TObject);
    procedure comboModelChange(Sender: TObject);
    procedure rgNodeElementClick(Sender: TObject);
  private
    // @name stores information about the current position.
    procedure GetData;
    // @name moves the selected view or views to the selected
    // cell, object, or position.
    procedure SetData;
    procedure SetGridSpinEditMax(Grid: TCustomModelGrid);
    procedure GetMeshItemClosestToCenter;
    { Private declarations }
  public
    { Public declarations }
  end;

{
  @name moves the top view of the model so that the point
  (XCoordinate,YCoordinate) is at the center of the view.
}
procedure SetTopPosition(const XCoordinate, YCoordinate: real);
procedure SetTopCornerPosition(const XCoordinate, YCoordinate: real);

{
  @name moves the front view of the model so that the point
  (XCoordinate,ZCoordinate) is at the center of the view.
}
procedure SetFrontPosition(const XCoordinate, ZCoordinate: real);
procedure SetFrontCornerPosition(const XCoordinate, ZCoordinate: real);

{
  @name moves the side view of the model so that the point
  (YCoordinate,ZCoordinate) is at the center of the view.
}
procedure SetSidePosition(const YCoordinate, ZCoordinate: real);
procedure SetSideCornerPosition(const YCoordinate, ZCoordinate: real);

// @name moves the top view of the model to the cell at Column, Row.
procedure MoveToTopCell(Grid: TCustomModelGrid; const Column, Row: integer);

// @name moves the front view of the model to the cell at Column, Layer.
procedure MoveToFrontCell(Grid: TCustomModelGrid; const Column, Layer: integer);

// @name moves the side view of the model to the cell at Row, Layer.
procedure MoveToSideCell(Grid: TCustomModelGrid; const Row, Layer: integer);

procedure MoveToImage(BitMapItem: TCompressedBitmapItem);
procedure MoveToDisv(CellNumber, LayerNumber: Integer;
  MoveToViews: TViewDirections);
procedure MoveToMesh(NodeOrElementNumber: Integer; MeshMoveTo: TMeshMoveTo;
  MoveToViews: TViewDirections);

procedure GoToObject(AScreenObject: TScreenObject);

implementation

uses frmGoPhastUnit,
  DataSetUnit, FastGEO, PhastModelUnit, QuadTreeClass, UndoItems,
  DrawMeshTypesUnit, MeshRenumberingTypes, ModflowIrregularMeshUnit,
  SutraMeshUnit;

resourcestring
  StrElement = 'Element';
  StrBlock = 'Block';
  StrSDoesNotHaveAny = '%s does not have any vertices. To edit or delete it ' +
  'use "Object|Edit|Select Objects to Edit or Delete".';

{$R *.dfm}

procedure SetTopPosition(const XCoordinate, YCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameTopView.ZoomBox do
  begin
    if (frmGoPhast.PhastModel.Mesh3D <> nil)
      and frmGoPhast.PhastModel.Mesh3D.Is3DMesh then
    begin
      DeltaX := (X(frmGoPhast.frameFrontView.ZoomBox.Image32.Width) - X(0)) / 2;
    end
    else
    begin
      DeltaX := (X(Image32.Width) - X(0)) / 2;
    end;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := XCoordinate - DeltaX;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.frameTopView.InvalidateScreenObjectCoordinates;
    frmGoPhast.TopDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdTop);
  end;
end;

procedure SetTopCornerPosition(const XCoordinate, YCoordinate: real);
var
  DeltaY: double;
begin
  with frmGoPhast.frameTopView.ZoomBox do
  begin
    DeltaY := (Y(0) - Y(Image32.Height));
    OriginX := XCoordinate;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.frameTopView.InvalidateScreenObjectCoordinates;
    frmGoPhast.TopDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdTop);
  end;
end;

procedure SetFrontPosition(const XCoordinate, ZCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameFrontView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := XCoordinate - DeltaX;
    OriginY := ZCoordinate - DeltaY;
    frmGoPhast.frameFrontView.InvalidateScreenObjectCoordinates;
    frmGoPhast.FrontDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdFront);
  end;
end;

procedure SetFrontCornerPosition(const XCoordinate, ZCoordinate: real);
var
  DeltaY: double;
begin
  with frmGoPhast.frameFrontView.ZoomBox do
  begin
    DeltaY := (Y(0) - Y(Image32.Height));
    OriginX := XCoordinate;
    OriginY := ZCoordinate - DeltaY;
    frmGoPhast.frameFrontView.InvalidateScreenObjectCoordinates;
    frmGoPhast.FrontDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdFront);
  end;
end;


procedure SetSidePosition(const YCoordinate, ZCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  if frmGoPhast.PhastModel.Mesh3D <> nil then
  begin
    Exit;
  end;
  with frmGoPhast.frameSideView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0)) / 2;
    DeltaY := (Y(0) - Y(Image32.Height)) / 2;
    OriginX := ZCoordinate + DeltaX;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.frameSideView.InvalidateScreenObjectCoordinates;
    frmGoPhast.SideDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdSide);
  end;
end;

procedure SetSideCornerPosition(const YCoordinate, ZCoordinate: real);
var
  DeltaX, DeltaY: double;
begin
  with frmGoPhast.frameSideView.ZoomBox do
  begin
    DeltaX := (X(Image32.Width) - X(0));
    DeltaY := (Y(0) - Y(Image32.Height));
    OriginX := ZCoordinate + DeltaX;
    OriginY := YCoordinate - DeltaY;
    frmGoPhast.frameSideView.InvalidateScreenObjectCoordinates;
    frmGoPhast.SideDiscretizationChanged := True;
    InvalidateImage32;
    frmGoPhast.AdjustScales;
    frmGoPhast.SynchronizeViews(vdSide);
  end;
end;

procedure MoveToTopCell(Grid: TCustomModelGrid; const Column, Row: integer);
var
  XCoordinate, YCoordinate: double;
  TopPoint: TPoint2D;
begin
  TopPoint := Grid.TwoDElementCenter(Column, Row);
  XCoordinate := TopPoint.X;
  YCoordinate := TopPoint.Y;
  SetTopPosition(XCoordinate, YCoordinate);
end;

procedure MoveToFrontCell(Grid: TCustomModelGrid; const Column, Layer: integer);
var
  XCoordinate, ZCoordinate: double;
  FrontPoint: T3DRealPoint;
begin
  FrontPoint := Grid.ThreeDElementCenter(Column,
    Grid.SelectedRow, Layer);
  XCoordinate := FrontPoint.X;
  ZCoordinate := FrontPoint.Z;
  SetFrontPosition(XCoordinate, ZCoordinate);
end;

procedure MoveToSideCell(Grid: TCustomModelGrid; const Row, Layer: integer);
var
  YCoordinate, ZCoordinate: double;
  SidePoint: T3DRealPoint;
begin
  SidePoint := Grid.ThreeDElementCenter(
    Grid.SelectedColumn, Row, Layer);
  YCoordinate := SidePoint.Y;
  ZCoordinate := SidePoint.Z;
  SetSidePosition(YCoordinate, ZCoordinate);
end;

{ TfrmGoTo }

procedure TfrmGoTo.GetMeshItemClosestToCenter;
var
  APoint: TPoint2D;
  Z: double;
  Index: Integer;
  Limits: TGridLimit;
  Location: TPoint2D;
  ClosestLayer: Integer;
  LayerIndex: Integer;
  QuadTree: TRbwQuadTree;
  ClosestDistance: double;
  TestDistance: double;
  FoundFirst: Boolean;
  Mesh: IMesh3D;
  ANode: INode2D;
  ANode3D: INode3D;
  AnElement3D: IElement3D;
  AnElement: IElement2D;
  procedure AddElementsToQuadTree;
  var
    Index: Integer;
    LayerIndex: Integer;
  begin
    for Index := 0 to Mesh.Mesh2DI.ElementCount - 1 do
    begin
      AnElement := Mesh.Mesh2DI.ElementsI2D[Index];
      if not Mesh.Is3DMesh then
      begin
        Location := AnElement.Center;
        QuadTree.AddPoint(Location.x, Location.y, AnElement);
      end
      else
      begin
        for LayerIndex := 0 to Mesh.LayerCount-1 do
        begin
          AnElement3D := Mesh.ElementArrayI[LayerIndex,
            AnElement.ElementNumber];
          if AnElement3D.Active then
          begin
            Location := AnElement.Center;
            QuadTree.AddPoint(Location.x, Location.y, AnElement);
            break;
          end;
        end;
      end;
    end;
  end;
  function Closest3DElement: IElement3D;
  var
    LayerIndex: Integer;
  begin
    with frmGoPhast.frameFrontView.ZoomBox do
    begin
      Z := Y(Image32.Height div 2);
    end;
    FoundFirst := False;
    ClosestLayer := -1;
    ClosestDistance := -1;
    for LayerIndex := 0 to Mesh.LayerCount-1 do
    begin
      AnElement3D := Mesh.ElementArrayI[LayerIndex,
        AnElement.ElementNumber];

      if AnElement3D.Active then
      begin
        TestDistance := Abs(Z - AnElement3D.CenterElevation);
        if FoundFirst then
        begin
          if TestDistance < ClosestDistance then
          begin
            ClosestDistance := TestDistance;
            ClosestLayer := LayerIndex;
          end;
        end
        else
        begin
          ClosestLayer := LayerIndex;
          ClosestDistance := TestDistance;
          FoundFirst := True;
        end;
      end;
    end;
    result := Mesh.ElementArrayI[ClosestLayer, AnElement.ElementNumber];
  end;
begin
  Mesh := frmGoPhast.PhastModel.Mesh3D;
  if Mesh = nil then
  begin
    Exit;
  end;
  if Mesh is TModflowDisvGrid then
  begin
    seNumber.MaxValue := Mesh.ActiveElementCount;
  end
  else
  begin
    if not Mesh.Is3DMesh then
    begin
      case rgNodeElement.ItemIndex of
        0:
          begin
            // Nodes
            seNumber.MaxValue := Mesh.Mesh2DI.NodeCount;
          end;
        1:
          begin
            // Elements
            seNumber.MaxValue := Mesh.Mesh2DI.ElementCount;
          end;
        else Assert(False);
      end;
    end
    else
    begin
      case rgNodeElement.ItemIndex of
        0:
          begin
            // Nodes
            seNumber.MaxValue := Mesh.ActiveNodeCount;
          end;
        1:
          begin
            // Elements
            seNumber.MaxValue := Mesh.ActiveElementCount;
          end;
        else Assert(False);
      end;
    end;
  end;
  seDisvCell.MaxValue := Mesh.Mesh2DI.ElementCount;
  seDisvLayer.MaxValue := Mesh.LayerCount;

  with frmGoPhast.frameTopView.ZoomBox do
  begin
    APoint.X := X(Image32.Width div 2);
    APoint.Y := Y(Image32.Height div 2);
  end;

  QuadTree := TRbwQuadTree.Create(nil);
  try
    Limits := Mesh.MeshLimits(vdTop, 0);
    QuadTree.XMax := Limits.MaxX;
    QuadTree.XMin := Limits.MinX;
    QuadTree.YMax := Limits.MaxY;
    QuadTree.YMin := Limits.MinY;
    if Mesh is TModflowDisvGrid then
    begin
      AddElementsToQuadTree;
      AnElement := IElement2D(QuadTree.NearestPointsFirstData(APoint.x, APoint.y));
      AnElement3D := Closest3DElement;
      seDisvCell.AsInteger := AnElement3D.ElementNumber2D + 1;
      seDisvLayer.AsInteger := AnElement3D.Layer + 1;
    end
    else
    begin
      case rgNodeElement.ItemIndex of
        0:
          begin
            // Nodes
            for Index := 0 to Mesh.Mesh2DI.NodeCount - 1 do
            begin
              ANode := Mesh.Mesh2DI.NodesI2D[Index];
              if not Mesh.Is3DMesh then
              begin
                Location := ANode.Location;
                QuadTree.AddPoint(Location.x, Location.y, ANode);
              end
              else
              begin
                for LayerIndex := 0 to Mesh.LayerCount do
                begin
                  ANode3D := Mesh.NodeArrayI[LayerIndex, ANode.NodeNumber];
                  if ANode3D.Active then
                  begin
                    Location := ANode.Location;
                    QuadTree.AddPoint(Location.x, Location.y, ANode);
                    break;
                  end;
                end;
              end;
            end;
            ANode := INode2D(QuadTree.NearestPointsFirstData(APoint.x, APoint.y));
            if not Mesh.Is3DMesh then
            begin
              seNumber.AsInteger := ANode.NodeNumber + 1;
            end
            else
            begin
              with frmGoPhast.frameFrontView.ZoomBox do
              begin
                Z := Y(Image32.Height div 2);
              end;
              FoundFirst := False;
              ClosestLayer := -1;
              ClosestDistance := -1;
              for LayerIndex := 0 to Mesh.LayerCount do
              begin
                ANode3D := Mesh.NodeArrayI[LayerIndex, ANode.NodeNumber];
                if ANode3D.Active then
                begin
                  TestDistance := Abs(Z - ANode3D.Z);
                  if FoundFirst then
                  begin
                    if TestDistance < ClosestDistance then
                    begin
                      ClosestDistance := TestDistance;
                      ClosestLayer := LayerIndex;
                    end;
                  end
                  else
                  begin
                    ClosestLayer := LayerIndex;
                    ClosestDistance := TestDistance;
                    FoundFirst := True;
                  end;
                end;
              end;
              ANode3D := Mesh.NodeArrayI[ClosestLayer, ANode.NodeNumber];
              seNumber.AsInteger := ANode3D.NodeNumber + 1;
            end;
          end;
        1:
          begin
            // Elements
            AddElementsToQuadTree;
            AnElement := IElement2D(QuadTree.NearestPointsFirstData(APoint.x, APoint.y));
            if not Mesh.Is3DMesh then
            begin
              seNumber.AsInteger := AnElement.ElementNumber + 1;
            end
            else
            begin
              AnElement3D := Closest3DElement;
              seNumber.AsInteger := AnElement3D.ElementNumber + 1;
            end;
          end;
      end;
    end;
  finally
    QuadTree.Free;
  end;


end;

procedure TfrmGoTo.GetData;
var
  APoint: TPoint2D;
  TopCell: T2DTopCell;
  FrontCell: T2DFrontCell;
  Index: Integer;
  AScreenObject: TScreenObject;
  Item: TListItem;
  TabVisible: boolean;
  AList: TList;
  Layer: integer;
  BitMapItem: TCompressedBitmapItem;
  Grid: TCustomModelGrid;
  DrawMesh: IDrawMesh;
  Mesh: IMesh3D;
//  DrawMesh: TSutraMesh3D;
  procedure SetGridSpinEditValue(SE: TJvSpinEdit; NewValue: integer);
  begin
    if NewValue <= SE.MaxValue then
    begin
      SE.AsInteger := NewValue;
    end
    else
    begin
      SE.AsInteger := Round(SE.MaxValue);
    end;
  end;
begin
  Screen.Cursor := crHourGlass;
  try
    Grid := frmGoPhast.Grid;
    if Grid = nil then
    begin
      DrawMesh := frmGoPhast.PhastModel.DrawMesh;
      Mesh := frmGoPhast.PhastModel.Mesh3D;
    end
    else
    begin
      DrawMesh := nil;
      Mesh := nil;
    end;

    with frmGoPhast.frameTopView.ZoomBox do
    begin
      APoint.X := X(Image32.Width div 2);
      APoint.Y := Y(Image32.Height div 2);
      rdeX.Text := FloatToStr(APoint.X);
      rdeY.Text := FloatToStr(APoint.Y);
      if Grid <> nil then
      begin
        APoint := Grid.RotateFromRealWorldCoordinatesToGridCoordinates(APoint);
      end
      else if DrawMesh <> nil then
      begin
        APoint := DrawMesh.RotateFromRealWorldCoordinatesToMeshCoordinates(APoint);
      end;
      rdeXPrime.Text := FloatToStr(APoint.X);
      rdeYPrime.Text := FloatToStr(APoint.Y);
    end;
    with frmGoPhast.frameFrontView.ZoomBox do
    begin
      rdeZ.Text := FloatToStr(Y(Image32.Height div 2));
    end;
    if (Grid <> nil)
      and (Grid.ColumnCount > 0)
      and (Grid.RowCount > 0)
      and (Grid.LayerCount > 0) then
    begin
      SetGridSpinEditMax(Grid);
      seCol.MinValue := 1;
      seRow.MinValue := 1;
      seLayer.MinValue := 1;

      TopCell := Grid.TopContainingCell(APoint, eaBlocks, False);
      SetGridSpinEditValue(seCol,TopCell.Col+1);
      SetGridSpinEditValue(seRow,TopCell.Row+1);
      with frmGoPhast.frameFrontView.ZoomBox do
      begin
        APoint.X := X(Image32.Width div 2);
        APoint.Y := Y(Image32.Height div 2);
      end;
      case frmGoPhast.ModelSelection of
        msUndefined: Assert(False);
        msPhast:
          begin
            FrontCell := frmGoPhast.PhastGrid.FrontContainingCell(APoint, eaBlocks);
            SetGridSpinEditValue(seLayer,FrontCell.Lay+1);
          end;
        msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
          msModflowFmp, msModflowCfp, msModflow2015
          {$IFDEF OWHMV2}
          , msModflowOwhm2
          {$ENDIF}
          :
          begin
            Layer := frmGoPhast.ModflowGrid.NearestLayerPosition(seCol.AsInteger-1,
              frmGoPhast.ModflowGrid.SelectedRow, APoint.Y);
            SetGridSpinEditValue(seLayer,Layer+1);
          end;
        msFootPrint:
          begin
            Layer := 0;
            SetGridSpinEditValue(seLayer,Layer+1);
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      tabCell.TabVisible := False;
    end;

    if (Mesh <> nil) and (Mesh.Mesh2DI.ElementCount > 0)
      and ((not Mesh.Is3DMesh) or (Mesh.ElementCount > 0)) then
    begin
      if Mesh is TModflowDisvGrid then
      begin
        tabMesh.TabVisible := False;
      end
      else
      begin
        tabDISV.TabVisible := False;
      end;
      GetMeshItemClosestToCenter;
    end
    else
    begin
      tabDISV.TabVisible := False;
      tabMesh.TabVisible := False;
    end;

    AList := TList.Create;
    try
      for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
      begin
        AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
        if not AScreenObject.Deleted then
        begin
          AList.Add(AScreenObject);
        end;
      end;

      TabVisible := AList.Count > 0;
      AList.Sort(ScreenObjectCompare);

      for Index := 0 to AList.Count - 1 do
      begin
        AScreenObject := AList[Index];
        Item := lvScreenObjects.Items.Add;
        Item.Caption := AScreenObject.Name;
        Item.Data := AScreenObject;
      end;
    finally
      AList.Free;
    end;
    tabObject.TabVisible := TabVisible;

    for Index := 0 to frmGoPhast.PhastModel.Bitmaps.Count - 1 do
    begin
      BitMapItem := frmGoPhast.PhastModel.Bitmaps.Items[Index]
        as TCompressedBitmapItem;
      if BitMapItem.Visible then
      begin
        Item := lvImages.Items.Add;
        Item.Caption := BitMapItem.Name;
        Item.Data := BitMapItem;
      end;
    end;
    tabImage.TabVisible := lvImages.Items.Count > 0;

  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure GoToObject(AScreenObject: TScreenObject);
var
  XCoordinate, YCoordinate: double;
begin
  if AScreenObject.Count > 0 then
  begin
    XCoordinate := AScreenObject.Points[0].X;
    YCoordinate := AScreenObject.Points[0].Y;
    case AScreenObject.ViewDirection of
      vdTop:
        begin
          SetTopPosition(XCoordinate, YCoordinate);
        end;
      vdFront:
        begin
          SetFrontPosition(XCoordinate, YCoordinate);
        end;
      vdSide:
        begin
          SetSidePosition(YCoordinate, XCoordinate);
        end;
    else
      Assert(False);
    end;
  end
  else
  begin
    Beep;
    MessageDlg(Format(StrSDoesNotHaveAny, [AScreenObject.Name]),
      mtWarning, [mbOK], 0);
  end;
end;


procedure TfrmGoTo.SetData;
var
  XCoordinate, YCoordinate, ZCoordinate: double;
  Column, Row, Layer: integer;
  Item: TListItem;
  AScreenObject: TScreenObject;
  Undo: TUndoChangeSelection;
  UndoShowHide: TUndoShowHideScreenObject;
  BitMapItem: TCompressedBitmapItem;
  Model: TCustomModel;
  MeshMoveTo: TMeshMoveTo;
  NodeOrElementNumber: Integer;
  MoveToViews: TViewDirections;
  CellNumber: Integer;
  LayerNumber: Integer;
begin
  Screen.Cursor := crHourGlass;
  try
    case TNavigationType(pcMain.ActivePageIndex) of
      ntPosition: // Position
        begin
          if cbTop.Checked then
          begin
            XCoordinate := StrToFloat(rdeX.Text);
            YCoordinate := StrToFloat(rdeY.Text);
            SetTopPosition(XCoordinate, YCoordinate);
          end;
          if cbFront.Checked then
          begin
            XCoordinate := StrToFloat(rdeXPrime.Text);
            ZCoordinate := StrToFloat(rdeZ.Text);
            SetFrontPosition(XCoordinate, ZCoordinate);
          end;
          if cbSide.Checked then
          begin
            YCoordinate := StrToFloat(rdeYPrime.Text);
            ZCoordinate := StrToFloat(rdeZ.Text);
            SetSidePosition(YCoordinate, ZCoordinate);
          end;

        end;
      ntGrid: //Cell
        begin
          Column := seCol.AsInteger - 1;
          Row := seRow.AsInteger - 1;
          Layer := seLayer.AsInteger - 1;
          Model := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
          if cbTop.Checked then
          begin
            MoveToTopCell(Model.Grid, Column, Row);
          end;
          if cbFront.Checked then
          begin
            MoveToFrontCell(Model.Grid, Column, Layer);
          end;
          if cbSide.Checked then
          begin
            MoveToSideCell(Model.Grid, Row, Layer);
          end;

        end;
      mtMesh:
        begin
          MeshMoveTo := TMeshMoveTo(rgNodeElement.ItemIndex);
          NodeOrElementNumber := seNumber.AsInteger-1;
          MoveToViews := [];
          if cbTop.Checked then
          begin
            Include(MoveToViews, vdTop);
          end;
          if cbFront.Checked then
          begin
            Include(MoveToViews, vdFront);
          end;
          MoveToMesh(NodeOrElementNumber, MeshMoveTo, MoveToViews);
        end;
      ntDisv:
        begin
          CellNumber := seDisvCell.AsInteger -1;
          LayerNumber := seDisvLayer.AsInteger -1;
          if cbTop.Checked then
          begin
            Include(MoveToViews, vdTop);
          end;
          if cbFront.Checked then
          begin
            Include(MoveToViews, vdFront);
          end;
          MoveToDisv(CellNumber, LayerNumber, MoveToViews);
        end;
      ntObject: // Object
        begin
          Item := lvScreenObjects.Selected;
          if Item <> nil then
          begin
            AScreenObject := Item.Data;
            if AScreenObject.Count > 0 then
            begin
              if cbSelectObject.Checked then
              begin
                Undo := TUndoChangeSelection.Create;

                frmGoPhast.ResetSelectedScreenObjects;
                AScreenObject.Selected := True;

                Undo.SetPostSelection;

                if Undo.SelectionChanged then
                begin
                  frmGoPhast.UndoStack.Submit(Undo);
                end
                else
                begin
                  Undo.Free;
                end;
              end
              else if not AScreenObject.Visible then
              begin
                UndoShowHide := TUndoShowHideScreenObject.Create;
                UndoShowHide.AddScreenObjectToChange(AScreenObject);
                frmGoPhast.UndoStack.Submit(UndoShowHide);
              end;

              GoToObject(AScreenObject);
//              XCoordinate := AScreenObject.Points[0].X;
//              YCoordinate := AScreenObject.Points[0].Y;
//              case AScreenObject.ViewDirection of
//                vdTop:
//                  begin
//                    SetTopPosition(XCoordinate, YCoordinate);
//                  end;
//                vdFront:
//                  begin
//                    SetFrontPosition(XCoordinate, YCoordinate);
//                  end;
//                vdSide:
//                  begin
//                    SetSidePosition(YCoordinate, XCoordinate);
//                  end;
//              else
//                Assert(False);
//              end;
            end;
          end;
        end;
      ntImage: // images
        begin
          Item := lvImages.Selected;
          if Item <> nil then
          begin
            BitMapItem := Item.Data;
            MoveToImage(BitMapItem);
          end;
        end
    else
      Assert(False);
    end;
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfrmGoTo.SetGridSpinEditMax(Grid: TCustomModelGrid);
begin
  if Grid <> nil then
  begin
    seCol.MaxValue := Grid.ColumnCount;
    seRow.MaxValue := Grid.RowCount;
    seLayer.MaxValue := Grid.LayerCount;
  end;
end;


procedure TfrmGoTo.FormCreate(Sender: TObject);
begin
  inherited;
  pcMain.ActivePage := tabCell;
  pcMain.ActivePageIndex := 0;


  FillComboWithModelNames(comboModel);

  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    if frmGoPhast.PhastModel.SutraMesh.MeshType <> mt3D then
    begin
      cbFront.Enabled := False;
      cbSide.Enabled := False;
      cbFront.Checked := False;
      cbSide.Checked := False;
    end;
  end;

  GetData;
end;

procedure TfrmGoTo.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmGoTo.cbClick(Sender: TObject);
begin
  inherited;
  rdeX.Enabled := cbTop.Checked;
  rdeY.Enabled := cbTop.Checked;
  rdeZ.Enabled := cbFront.Checked or cbSide.Checked;
  rdeXPrime.Enabled := cbFront.Checked;
  rdeYPrime.Enabled := cbSide.Checked;
  seCol.Enabled := cbTop.Checked or cbFront.Checked;
  seRow.Enabled := cbTop.Checked or cbSide.Checked;
  seLayer.Enabled := cbFront.Checked or cbSide.Checked;
end;

procedure TfrmGoTo.comboModelChange(Sender: TObject);
var
  Model: TCustomModel;
begin
  inherited;
  Model := comboModel.Items.Objects[comboModel.ItemIndex] as TCustomModel;
  SetGridSpinEditMax(Model.Grid);
end;

procedure TfrmGoTo.pcMainChange(Sender: TObject);
begin
  inherited;
  cbTop.Enabled := (pcMain.ActivePage <> tabObject)
    and (pcMain.ActivePage <> tabImage);
  cbFront.Enabled := cbTop.Enabled;
  cbSide.Enabled := cbTop.Enabled
    and not (frmGoPhast.ModelSelection in SutraSelection);
  HelpKeyWord := pcMain.ActivePage.HelpKeyword;
end;

procedure TfrmGoTo.rgNodeElementClick(Sender: TObject);
begin
  inherited;
  GetMeshItemClosestToCenter;
end;

procedure TfrmGoTo.FormShow(Sender: TObject);
begin
  inherited;
  tabCell.Visible := seCol.Enabled;
  case frmGoPhast.ModelSelection of
    msUndefined: Assert(False);
    msPhast, msSutra22, msSutra30, msSutra40: tabCell.Caption := StrElement;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015
      {$IFDEF OWHMV2}
      , msModflowOwhm2
      {$ENDIF}
      :
      begin
        tabCell.Caption := StrBlock;
      end
    else Assert(False);
  end;
end;

procedure MoveToImage(BitMapItem: TCompressedBitmapItem);
var
  X: double;
  Y: double;
  PointIndex: Integer;
  MeasPoint: TMeasurementPointItem;
begin
  if (BitMapItem = nil) or (BitMapItem.MeasurementPoints.Count = 0) then
  begin
    Exit;
  end;
  X := 0.0;
  Y := 0.0;
  for PointIndex := 0 to BitMapItem.MeasurementPoints.Count - 1 do
  begin
    MeasPoint := BitMapItem.MeasurementPoints.Items[PointIndex]
      as TMeasurementPointItem;
    X := X + MeasPoint.X;
    Y := Y + MeasPoint.Y;
  end;
  X := X/BitMapItem.MeasurementPoints.Count;
  Y := Y/BitMapItem.MeasurementPoints.Count;
  case BitMapItem.ViewDirection of
    vdTop: SetTopPosition(X,Y);
    vdFront: SetFrontPosition(X,Y);
    vdSide: SetSidePosition(Y,X);
    else Assert(False);
  end;
end;

procedure MoveToDisv(CellNumber, LayerNumber: Integer;
  MoveToViews: TViewDirections);
var
  Mesh: IMesh3D;
  DrawMesh: IDrawMesh;
  AnElement3D: IElement3D;
  ElementLocation: TPoint3D;
  TopLocation: TPoint2D;
  ALine: TLine2D;
  ClosestPoint: TPoint2D;
  NewCrossSection: TSegment2D;
begin
  Mesh := frmGoPhast.PhastModel.Mesh3D;
  DrawMesh := frmGoPhast.PhastModel.DrawMesh;
  AnElement3D := Mesh.ElementArrayI[LayerNumber, CellNumber];

  ElementLocation := AnElement3D.CenterLocation;
  if vdTop in MoveToViews then
  begin
    frmGoPhast.PhastModel.CombinedDisplayLayer := LayerNumber;
    SetTopPosition(ElementLocation.x, ElementLocation.y);
  end;
  if vdFront in MoveToViews then
  begin
    TopLocation.x := ElementLocation.x;
    TopLocation.y := ElementLocation.y;

    ALine := EquateLine(DrawMesh.CrossSection.StartPoint,
      DrawMesh.CrossSection.EndPoint);
    ClosestPoint := ClosestPointOnLineFromPoint(ALine,
      TopLocation);
    NewCrossSection[1].x := ALine[1].x + TopLocation.x-ClosestPoint.x;
    NewCrossSection[1].y := ALine[1].y + TopLocation.y-ClosestPoint.y;
    NewCrossSection[2].x := ALine[2].x + TopLocation.x-ClosestPoint.x;
    NewCrossSection[2].y := ALine[2].y + TopLocation.y-ClosestPoint.y;
    frmGoPhast.UndoStack.Submit(TUndoMoveCrossSection.Create(NewCrossSection));

    TopLocation := DrawMesh.
      RotateFromRealWorldCoordinatesToMeshCoordinates(
      TopLocation);
    SetFrontPosition(TopLocation.x, ElementLocation.Z);
  end;

end;

procedure MoveToMesh(NodeOrElementNumber: Integer; MeshMoveTo: TMeshMoveTo;
  MoveToViews: TViewDirections);
var
  TopLocation: TPoint2D;
  ElementLocation: TPoint3D;
  NodeIndex: Integer;
  ElementIndex: integer;
  LayerIndex: integer;
  ALine: TLine2D;
  ClosestPoint: TPoint2D;
  NewCrossSection: TSegment2D;
  Mesh: IMesh3D;
  ANode2D: INode2D;
  AnElement2D: IElement2D;
  ANode3D: INode3D;
  AnElement3D: IElement3D;
  DrawMesh: IDrawMesh;
begin
  Mesh := frmGoPhast.PhastModel.Mesh3D;
  DrawMesh := frmGoPhast.PhastModel.DrawMesh;
  if not Mesh.Is3DMesh then
  begin
    case MeshMoveTo of
      mmtNode:
        begin
          //Nodes
          if vdTop in MoveToViews then
          begin
            ANode2D := Mesh.Mesh2DI.NodesI2D[NodeOrElementNumber];
            TopLocation := ANode2D.Location;
            SetTopPosition(TopLocation.x, TopLocation.y);
          end;
        end;
      mmtElement:
        begin
          // Elements
          if vdTop in MoveToViews then
          begin
            AnElement2D := Mesh.Mesh2DI.ElementsI2D[NodeOrElementNumber];
            TopLocation := AnElement2D.Center;
            SetTopPosition(TopLocation.x, TopLocation.y);
          end;
        end;
      else Assert(False);
    end;
  end
  else
  begin
    case MeshMoveTo of
      mmtNode:
        begin
          //Nodes
          for NodeIndex := 0 to Mesh.NodeCount - 1 do
          begin
            ANode3D := Mesh.NodesI[NodeIndex];
            if ANode3D.Active and (ANode3D.NodeNumber = NodeOrElementNumber) then
            begin
              TopLocation := ANode3D.NodeLocation2D;
              if vdTop in MoveToViews then
              begin
                for LayerIndex := 0 to Mesh.LayerCount do
                begin
                  if Mesh.NodeArrayI[LayerIndex,ANode3D.NodeNumber2D]
                    = ANode3D then
                  begin
                    frmGoPhast.PhastModel.CombinedDisplayLayer := LayerIndex;
                    break;
                  end;
                end;
                SetTopPosition(TopLocation.x, TopLocation.y);
              end;

              if vdFront in MoveToViews then
              begin
                ALine := EquateLine(DrawMesh.CrossSection.StartPoint,
                  DrawMesh.CrossSection.EndPoint);
                ClosestPoint := ClosestPointOnLineFromPoint(ALine,
                  TopLocation);
                NewCrossSection[1].x := ALine[1].x + TopLocation.x-ClosestPoint.x;
                NewCrossSection[1].y := ALine[1].y + TopLocation.y-ClosestPoint.y;
                NewCrossSection[2].x := ALine[2].x + TopLocation.x-ClosestPoint.x;
                NewCrossSection[2].y := ALine[2].y + TopLocation.y-ClosestPoint.y;
                frmGoPhast.UndoStack.Submit(TUndoMoveCrossSection.Create(NewCrossSection));

                TopLocation := DrawMesh.
                  RotateFromRealWorldCoordinatesToMeshCoordinates(
                  TopLocation);
                SetFrontPosition(TopLocation.x, ANode3D.Z);
              end;
              break;
            end;
          end;
        end;
      mmtElement:
        begin
          // Elements
//                      ElementNumber := NodeOrElementNumber;
          for ElementIndex := 0 to Mesh.ElementCount - 1 do
          begin
            AnElement3D := Mesh.ElementsI[ElementIndex];
            if AnElement3D.Active and (AnElement3D.ElementNumber =
              NodeOrElementNumber) then
            begin
              ElementLocation := AnElement3D.CenterLocation;
              if vdTop in MoveToViews then
              begin
                for LayerIndex := 0 to Mesh.LayerCount-1 do
                begin
                  if Mesh.ElementArrayI[LayerIndex,AnElement3D.ElementNumber2D]
                    = AnElement3D then
                  begin
                    frmGoPhast.PhastModel.CombinedDisplayLayer := LayerIndex;
                  end;
                end;
                SetTopPosition(ElementLocation.x, ElementLocation.y);
              end;
              if vdFront in MoveToViews then
              begin
                TopLocation.x := ElementLocation.x;
                TopLocation.y := ElementLocation.y;

                ALine := EquateLine(DrawMesh.CrossSection.StartPoint,
                  DrawMesh.CrossSection.EndPoint);
                ClosestPoint := ClosestPointOnLineFromPoint(ALine,
                  TopLocation);
                NewCrossSection[1].x := ALine[1].x + TopLocation.x-ClosestPoint.x;
                NewCrossSection[1].y := ALine[1].y + TopLocation.y-ClosestPoint.y;
                NewCrossSection[2].x := ALine[2].x + TopLocation.x-ClosestPoint.x;
                NewCrossSection[2].y := ALine[2].y + TopLocation.y-ClosestPoint.y;
                frmGoPhast.UndoStack.Submit(TUndoMoveCrossSection.Create(NewCrossSection));

                TopLocation := DrawMesh.
                  RotateFromRealWorldCoordinatesToMeshCoordinates(
                  TopLocation);
                SetFrontPosition(TopLocation.x, ElementLocation.Z);
              end;
              break;
            end;
          end;
        end;
      else Assert(False);
    end;
  end;

end;

end.
