unit framePolygonUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  frameCustomMetaDataUnit, System.Rtti,
  {$IF CompilerVersion >= 31}
  FMX.Grid.Style,
  {$ENDIF}
  FMX.Grid,
  FMX.Controls.Presentation, FMX.ScrollBox, FMX.Memo, FMX.Edit, FMX.EditBox,
  FMX.SpinBox, MetaDataInterfacesUnit, FMX.Layouts, FMX.Platform;

type
  TLatLongCol = (llcLongitude, llcLatitude);

  TframePolygon = class(TframeCustomMetaData)
    sgPolygon: TStringGrid;
    scolLongitude: TStringColumn;
    scolLatitude: TStringColumn;
    spnbxRows: TSpinBox;
    btnPaste: TButton;
    procedure sgPolygonDrawColumnCell(Sender: TObject; const Canvas: TCanvas;
      const Column: TColumn; const Bounds: TRectF; const Row: Integer;
      const Value: TValue; const State: TGridDrawStates);
    procedure spnbxRowsChange(Sender: TObject);
    procedure sgPolygonEditingDone(Sender: TObject; const ACol, ARow: Integer);
    procedure btnPasteClick(Sender: TObject);
  private
    FData: IPolygonDataItem;
    FGettingData: Boolean;
    FClipboardSvc: IFMXClipboardService;
    procedure DistributeText(AGrid: TStringGrid; ACol, ARow: Integer);
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetMetaData(Data: ICustomMetaData); override;
    { Public declarations }
  end;

var
  framePolygon: TframePolygon;

implementation

{$R *.fmx}

procedure TframePolygon.DistributeText(AGrid: TStringGrid; ACol, ARow: Integer);
var
  Lines: TStringList;
  AValue: string;
  Splitter: TStringList;
  LineIndex: Integer;
  ColIndex: Integer;
  GridRow: Integer;
  GridCol: Integer;
begin
  Lines := TStringList.Create;
  try
    AValue := AGrid.Cells[ACol, ARow];
    Lines.Text := AValue;
    if (Lines.Count > 1) or (Pos(#9,AValue) > 0)  then
    begin
      if AGrid.RowCount < ARow + Lines.Count then
      begin
        spnbxRows.Value := ARow + Lines.Count;
        if Assigned(spnbxRows.OnChange) then
        begin
          spnbxRows.OnChange(spnbxRows);
        end;
//        AGrid.RowCount := ARow + Lines.Count
      end;
      Splitter := TStringList.Create;
      AGrid.BeginUpdate;
      try
        Splitter.Delimiter := #9;
        Splitter.StrictDelimiter := True;
        for LineIndex := 0 to Lines.Count - 1 do
        begin
          GridRow := ARow + LineIndex;

          Splitter.DelimitedText := Lines[LineIndex];
          for ColIndex := 0 to Splitter.Count - 1 do
          begin
            GridCol := ACol + ColIndex;
            if GridCol < AGrid.ColumnCount then
            begin
              AGrid.Cells[GridCol, GridRow] := Splitter[ColIndex];
              if Assigned(AGrid.OnEditingDone) then
              begin
                AGrid.OnEditingDone(AGrid, GridCol, GridRow);
              end;
            end
            else
            begin
              break;
            end;
          end;
        end;
      finally
        AGrid.EndUpdate;
        Splitter.Free;
      end;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TframePolygon.btnPasteClick(Sender: TObject);
var
  ARow: Integer;
  ACol: Integer;
begin
  inherited;
  ARow := sgPolygon.Selected;
  ACol := sgPolygon.ColumnIndex;
  if (ARow >= 0) and (ACol >= 0) and (FClipboardSvc <> nil)
    and not FClipboardSvc.GetClipboard.IsEmpty then
  begin
    sgPolygon.Cells[ACol, ARow] := FClipboardSvc.GetClipboard.ToString;
    DistributeText(sgPolygon, ACol, ARow);
  end;
end;

constructor TframePolygon.Create(AOwner: TComponent);
begin
  inherited;
  if not TPlatformServices.Current.SupportsPlatformService(
    IFMXClipboardService, IInterface(FClipboardSvc)) then
  begin
    FClipboardSvc := nil;
  end;
end;

destructor TframePolygon.Destroy;
begin
  FClipboardSvc := nil;
  inherited;
end;

procedure TframePolygon.GetMetaData(Data: ICustomMetaData);
var
  PointIndex: Integer;
  APoint: TPolyPoint;
begin
  inherited;
  sgPolygon.Columns[Ord(llcLongitude)].Header := 'Longitude';
  sgPolygon.Columns[Ord(llcLatitude)].Header := 'Latitude';

  FGettingData := True;
  try
    FData := Data as IPolygonDataItem;
    While FData.Polygon.Count < 4 do
    begin
      FData.Polygon.Add;
    end;
    spnbxRows.Value := FData.Polygon.Count;
    spnbxRowsChange(nil);
    for PointIndex := 0 to FData.Polygon.Count - 1 do
    begin
      APoint := FData.Polygon[PointIndex];
      sgPolygon.Cells[Ord(llcLongitude), PointIndex] := APoint.Longitude.ToString;
      sgPolygon.Cells[Ord(llcLatitude), PointIndex] := APoint.Latitude.ToString;
    end;
  finally
    FGettingData := False;

  end;
end;

procedure TframePolygon.sgPolygonDrawColumnCell(Sender: TObject;
  const Canvas: TCanvas; const Column: TColumn; const Bounds: TRectF;
  const Row: Integer; const Value: TValue; const State: TGridDrawStates);
var
  D: Double;
  Rect : TRectF;
  DFirst: Double;
begin
  if not TryStrToFloat(sgPolygon.Cells[Column.Index, Row], D) then
  begin
    Rect := Bounds;
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(Rect, 0, 0, AllCorners, 0.2);
    Exit;
  end
  else if (Column = scolLongitude) and (D = 180) then
  begin
    Rect := Bounds;
    Canvas.Fill.Color := TAlphaColorRec.Red;
    Canvas.FillRect(Rect, 0, 0, AllCorners, 0.2);
    Exit;
  end;
  if (Row = sgPolygon.RowCount -1) then
  begin
    if TryStrToFloat(sgPolygon.Cells[Column.Index, 0], DFirst) then
    begin
      if D <> DFirst then
      begin
        Rect := Bounds;
        Canvas.Fill.Color := TAlphaColorRec.Red;
        Canvas.FillRect(Rect, 0, 0, AllCorners, 0.2);
        Exit;
      end;
    end;
  end;
end;

procedure TframePolygon.sgPolygonEditingDone(Sender: TObject; const ACol,
  ARow: Integer);
var
  APoint: TPolyPoint;
  Col: TLatLongCol;
  Value: Double;
begin
  inherited;
  if FGettingData then
  begin
    Exit;
  end;
//  DistributeText(sgPolygon, ACol, ARow);
  if TryStrToFloat(sgPolygon.Cells[ACol, ARow], Value) then
  begin
    case ACol  of
      0:
        begin
          // Longitude
          if Value > 180 then
          begin
            Value := 180;
            sgPolygon.Cells[ACol, ARow] := '180';
          end
          else if Value < -180 then
          begin
            Value := -180;
            sgPolygon.Cells[ACol, ARow] := '-180';
          end;
        end;
      1:
        begin
          // Latitude
          if Value > 90 then
          begin
            Value := 90;
            sgPolygon.Cells[ACol, ARow] := '90';
          end
          else if Value < -90 then
          begin
            Value := -180;
            sgPolygon.Cells[ACol, ARow] := '-90';
          end;
        end;
    end;
    APoint := FData.Polygon[ARow];
    Col := TLatLongCol(ACol);
    case Col of
      llcLongitude:
        begin
          APoint.Longitude := Value;
        end;
      llcLatitude:
        begin
          APoint.Latitude := Value;
        end;
    end;
  end;
end;

procedure TframePolygon.spnbxRowsChange(Sender: TObject);
begin
  inherited;
  sgPolygon.RowCount := Round(spnbxRows.Value);
  if not FGettingData then
  begin
    while FData.Polygon.Count > spnbxRows.Value do
    begin
      FData.Polygon.Items[FData.Polygon.Count-1].Free;
    end;
    while FData.Polygon.Count < spnbxRows.Value do
    begin
      FData.Polygon.Add;
    end;
  end;
end;

end.
