unit frameDiscretizationUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, StdCtrls, ExtCtrls, ArgusDataEntry, Buttons, Grids, RbwDataGrid4,
  GoPhastTypes;

type
  TframeDiscretization = class(TFrame)
    lbl1: TLabel;
    lbl2: TLabel;
    pnlDiscritization: TPanel;
    spl1: TSplitter;
    rdgSubLayerBoundaries: TRbwDataGrid4;
    pnl1: TPanel;
    lbl3: TLabel;
    pnlPaintboxParent: TPanel;
    pbSubLayers: TPaintBox;
    rdeGrowthRate: TRbwDataEntry;
    rdeVDiscretization: TRbwDataEntry;
    rgMethod: TRadioGroup;
    sbInsertLine: TSpeedButton;
    sbMoveLine: TSpeedButton;
    sbDeleteLine: TSpeedButton;
    procedure rdeVDiscretizationChange(Sender: TObject);
    procedure rdeGrowthRateChange(Sender: TObject);
    procedure rdeGrowthRateExit(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
    procedure rdgSubLayerBoundariesEndUpdate(Sender: TObject);
    procedure rdgSubLayerBoundariesExit(Sender: TObject);
    procedure rdgSubLayerBoundariesSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure rdgSubLayerBoundariesSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure pbSubLayersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSubLayersMouseEnter(Sender: TObject);
    procedure pbSubLayersMouseLeave(Sender: TObject);
    procedure pbSubLayersMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbSubLayersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbSubLayersPaint(Sender: TObject);
  private
    FSettingUnit: Boolean;
    FLayerPositions: TOneDIntegerArray;
    FSelectedUnits: TList;
    FMovingLine: Boolean;
    FLineBeingMoved: integer;
    FMouseInPaintBox: Boolean;
    FMouseY: Integer;
    procedure EnableGrowthRateControl;
    procedure UpdateStringGrid;
    procedure UpdateLayerPositions;
    procedure UpdateSelectedUnitLayers;
    procedure SetSpacing(const GrowthRate: real;
      GrowthMethod: TGrowthMethod; const SubLayers: integer;
      out Fractions: TRealArray);
    procedure GetLayerPostions(const Fractions: TRealArray;
      out LayerPostions: TOneDIntegerArray);
    function IsOnLine(Y: Integer; out WhichLine: integer): boolean;
    procedure StartMove(X, Y: Integer);
    procedure DeleteLine(Y: integer);
    procedure InsertLine(Y: integer);
    procedure MoveLine(Y: integer);
    function ConvertY(Y: integer): real;
    procedure RearrangeValuesInStringGrid;
    procedure SetPbCursor(X,Y: integer);
    function InBox(X, Y: integer): boolean;
    { Private declarations }
  public
    property SettingUnit: Boolean read FSettingUnit write FSettingUnit;
    procedure SetControlValues;
    constructor Create(AnOwner: TComponent); override;
    destructor Destroy; override;
    // List must contain Link(TGrowthControls)s.
    procedure UpdateSelectedUnits(List: TList);
    procedure Loaded; override;
    { Public declarations }
  end;

resourcestring
  StrLayerBoundary = 'Layer boundary';

implementation

uses
  RealListUnit, LayerStructureUnit, CursorsFoiledAgain;



{$R *.dfm}

function TframeDiscretization.ConvertY(Y: integer): real;
begin
  result := (pbSubLayers.Height - 10 - Y)/(pbSubLayers.Height - 20);
end;

constructor TframeDiscretization.Create(AnOwner: TComponent);
begin
  inherited;
  FSelectedUnits := TList.Create;
end;

procedure TframeDiscretization.DeleteLine(Y: integer);
var
  LineToDelete: Integer;
  Index: integer;
begin
  if IsOnLine(Y, LineToDelete) then
  begin
    for Index := LineToDelete+1 to rdgSubLayerBoundaries.RowCount - 1 do
    begin
      rdgSubLayerBoundaries.Cells[0,Index] :=
        rdgSubLayerBoundaries.Cells[0,Index+1];
    end;
    rdgSubLayerBoundaries.Cells[0, rdgSubLayerBoundaries.RowCount-1] := '';
    RearrangeValuesInStringGrid;
    UpdateLayerPositions;
    UpdateSelectedUnitLayers;
    pbSubLayers.Invalidate;
  end;
end;

destructor TframeDiscretization.Destroy;
begin
  FSelectedUnits.Free;
  inherited;
end;

procedure TframeDiscretization.EnableGrowthRateControl;
var
  VDisc: integer;
begin
  if rdeVDiscretization.Text <> '' then
  begin
    VDisc := StrToInt(rdeVDiscretization.Text);
    rdeGrowthRate.Enabled := (VDisc > 1)
      and (TGrowthMethod(rgMethod.ItemIndex) in [gmUp..gmEdge]);
  end;
end;

procedure TframeDiscretization.GetLayerPostions(const Fractions: TRealArray;
  out LayerPostions: TOneDIntegerArray);
var
  Index: Integer;
begin
  SetLength(LayerPostions, Length(Fractions));
  for Index := 0 to Length(Fractions) - 1 do
  begin
    LayerPostions[Index] :=
      Round((pbSubLayers.Height-20)*(1-Fractions[Index])+10);
  end;
end;

function TframeDiscretization.InBox(X, Y: integer): boolean;
begin
  result := (X > 10) and (Y > 10) and
    (X < pbSubLayers.Width - 10) and (Y < pbSubLayers.Height - 10);
end;

procedure TframeDiscretization.InsertLine(Y: integer);
var
  NumLayers: integer;
  LayerGroup: TGrowthControls;
  LayerIndex: integer;
begin
  rgMethod.ItemIndex := Integer(gmCustom);
  NumLayers := StrToInt(rdeVDiscretization.Text) + 1;
  rdgSubLayerBoundaries.Cells[0,NumLayers-1]
    := FloatToStr(ConvertY(Y));
  rdeVDiscretization.Text := IntToStr(NumLayers);
  rdeVDiscretizationChange(rdeVDiscretization);
  for LayerIndex := 0 to FSelectedUnits.Count - 1 do
  begin
    LayerGroup := FSelectedUnits[LayerIndex];
    While LayerGroup.LayerCount < NumLayers do
    begin
      LayerGroup.LayerCollection.Add;
    end;
  end;
  RearrangeValuesInStringGrid;
  UpdateLayerPositions;
  UpdateSelectedUnitLayers;
  pbSubLayers.Invalidate;
end;

function TframeDiscretization.IsOnLine(Y: Integer;
  out WhichLine: integer): boolean;
var
  Index: Integer;
begin
  result := False;
  WhichLine := -1;
  for Index := 0 to Length(FLayerPositions) - 1 do
  begin
    result := Abs(FLayerPositions[Index] - Y) <= 3;
    if result then
    begin
      WhichLine := Index;
      Exit;
    end;
  end;
end;

procedure TframeDiscretization.Loaded;
begin
  inherited;
  pnlPaintboxParent.DoubleBuffered:= True;
  rdgSubLayerBoundaries.Cells[0,0] := StrLayerBoundary;
end;

procedure TframeDiscretization.MoveLine(Y: integer);
begin
  rdgSubLayerBoundaries.Cells[0, FLineBeingMoved+1]
    := FloatToStr(ConvertY(Y));
  RearrangeValuesInStringGrid;
  UpdateSelectedUnitLayers;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.pbSubLayersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  StartMove(X, Y);
end;

procedure TframeDiscretization.pbSubLayersMouseEnter(Sender: TObject);
begin
  FMouseInPaintBox := True;
end;

procedure TframeDiscretization.pbSubLayersMouseLeave(Sender: TObject);
begin
  FMouseInPaintBox := False;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.pbSubLayersMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
begin
  SetPbCursor(X,Y);
  FMouseY := Y;
  if FMovingLine or sbInsertLine.Down
   then
  begin
    pbSubLayers.Invalidate;
  end;
end;

procedure TframeDiscretization.pbSubLayersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if InBox(X,Y) then
  begin
    if sbInsertLine.Down then
    begin
      InsertLine(Y);
    end
    else if sbMoveLine.Down and FMovingLine then
    begin
      MoveLine(Y);
    end
    else if sbDeleteLine.Down then
    begin
      DeleteLine(Y);
    end;
    FMovingLine := False;
  end;
end;

procedure TframeDiscretization.pbSubLayersPaint(Sender: TObject);
var
  Index: Integer;
begin
  pbSubLayers.Canvas.Brush.Color := clWhite;
  pbSubLayers.Canvas.Rectangle(0,0,pbSubLayers.Width, pbSubLayers.Height);
  pbSubLayers.Canvas.MoveTo(10,10);
  pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,10);
  pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,pbSubLayers.Height-10);
  pbSubLayers.Canvas.LineTo(10,pbSubLayers.Height-10);
  pbSubLayers.Canvas.LineTo(10,10);

  for Index := 0 to Length(FLayerPositions) - 1 do
  begin
    pbSubLayers.Canvas.MoveTo(10,FLayerPositions[Index]);
    pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,FLayerPositions[Index]);
  end;

  if (FMovingLine or sbInsertLine.Down) and FMouseInPaintBox then
  begin
    pbSubLayers.Canvas.Pen.Style := psDot;
    try
      pbSubLayers.Canvas.MoveTo(10,FMouseY);
      pbSubLayers.Canvas.LineTo(pbSubLayers.Width-10,FMouseY);
    finally
      pbSubLayers.Canvas.Pen.Style := psSolid;
    end;
  end;
end;

procedure TframeDiscretization.rdeGrowthRateChange(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  UpdateStringGrid;
  UpdateLayerPositions;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.rdeGrowthRateExit(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TGrowthControls;
  GrowthRate: double;
begin
  inherited;
  if not FSettingUnit then
  begin
    try
      if rdeGrowthRate.Text <> '' then
      begin
        GrowthRate := StrToFloat(rdeGrowthRate.Text);
        if GrowthRate >= 1 then
        begin
          for Index := 0 to FSelectedUnits.Count - 1 do
          begin
            SelectedUnit := FSelectedUnits[Index];
            SelectedUnit.GrowthRate := GrowthRate;
          end;
          UpdateSelectedUnitLayers;
        end;
        end;
    except on E: EConvertError do
      begin
        // do nothing
      end;
    end;
  end;
end;

procedure TframeDiscretization.rdeVDiscretizationChange(Sender: TObject);
var
  VDisc: integer;
begin
  inherited;
  if csLoading in ComponentState then Exit;
  if rdeVDiscretization.Text <> '' then
  begin
    try
      VDisc := StrToInt(rdeVDiscretization.Text);
      rdgSubLayerBoundaries.Enabled := VDisc > 1;
      if rdgSubLayerBoundaries.Enabled then
      begin
        rdgSubLayerBoundaries.Color := clWindow;
      end
      else
      begin
        rdgSubLayerBoundaries.Color := clBtnFace;
      end;
      EnableGrowthRateControl;
      rgMethod.Enabled := VDisc > 1;
      if VDisc > 1 then
      begin
        rdgSubLayerBoundaries.RowCount := VDisc;
      end;
      UpdateStringGrid;
      UpdateLayerPositions;
      UpdateSelectedUnitLayers;
    except on E: EConvertError do
      begin
        // ignore
      end;
    end;
    pbSubLayers.Invalidate;
  end
  else
  begin
    rdeGrowthRate.Enabled := False;
    rgMethod.Enabled := False;
  end;

  if not FSettingUnit then
  begin
    SetControlValues;
  end;
end;

procedure TframeDiscretization.rdgSubLayerBoundariesEndUpdate(Sender: TObject);
var
  NewDiscretization: Integer;
begin
  inherited;
  if rdeVDiscretization <> nil then
  begin
    if rdgSubLayerBoundaries.Enabled then
    begin
      NewDiscretization := rdgSubLayerBoundaries.RowCount;
    end
    else
    begin
      NewDiscretization := 1
    end;
    rdeVDiscretization.Text := IntToStr(NewDiscretization);

//    if not rgMethod.Enabled then
//    begin
//      Exit;
//    end;
//    if rdgSubLayerBoundaries.Updating then
//    begin
//      Exit;
//    end;
//    rgMethod.ItemIndex := Integer(gmCustom);
//    UpdateLayerPositions;
//    if not FSettingUnit then
//    begin
//      UpdateSelectedUnitLayers;
//    end;
//    pbSubLayers.Invalidate;

  end;
end;

procedure TframeDiscretization.rdgSubLayerBoundariesExit(Sender: TObject);
begin
  UpdateStringGrid;
  UpdateSelectedUnitLayers;
end;

procedure TframeDiscretization.rdgSubLayerBoundariesSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  CanSelect := rgMethod.Enabled;
end;

procedure TframeDiscretization.rdgSubLayerBoundariesSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  if not rgMethod.Enabled then
  begin
    Exit;
  end;
//  if rdgSubLayerBoundaries.DistributingText then
//  begin
//    Exit;
//  end;
  rgMethod.ItemIndex := Integer(gmCustom);
  UpdateLayerPositions;
  if not FSettingUnit then
  begin
    UpdateSelectedUnitLayers;
  end;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.RearrangeValuesInStringGrid;
var
  Index: Integer;
  Fractions: TRealArray;
  LayerIndex: Integer;
  LayerGroup: TGrowthControls;
begin
//  if rdgSubLayerBoundaries.Updating then
//  begin
//    Exit;
//  end;
  for LayerIndex := 0 to FSelectedUnits.Count - 1 do
  begin
    LayerGroup := FSelectedUnits[LayerIndex];
    rgMethod.ItemIndex := Integer(gmCustom);
    LayerGroup.GrowthMethod := gmCustom;
    SetSpacing(LayerGroup.GrowthRate, gmCustom,
      LayerGroup.LayerCount, Fractions);
    rdeVDiscretization.Text := IntToStr(Length(Fractions) + 1);
    rdeVDiscretizationChange(rdeVDiscretization);
    LayerGroup.LayerCollection.Clear;
    for Index := 0 to Length(Fractions)-1 do
    begin
      rdgSubLayerBoundaries.Cells[0, Index + 1]
        := FloatToStr(Fractions[Index]);
      (LayerGroup.LayerCollection.Add as TLayerFraction).Fraction :=
        Fractions[Index];
    end;
  end;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.rgMethodClick(Sender: TObject);
var
  Index: integer;
  SelectedUnit: TGrowthControls;
begin
  inherited;
//  if rdgSubLayerBoundaries.DistributingText then
//  begin
//    rdgSubLayerBoundaries.Cells[rdgSubLayerBoundaries.Col, rdgSubLayerBoundaries.Row] := '';
//  end;
  EnableGrowthRateControl;
  UpdateStringGrid;
  UpdateLayerPositions;
  if not FSettingUnit then
  begin
    for Index := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      SelectedUnit.GrowthMethod := TGrowthMethod(rgMethod.ItemIndex);
    end;
    UpdateSelectedUnitLayers;
  end;
  pbSubLayers.Invalidate;
end;

procedure TframeDiscretization.SetControlValues;
var
  SelectedUnit: TGrowthControls;
  FirstUnit: TGrowthControls;
  Same: boolean;
  procedure AssignGrowthRate;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.GrowthRate = SelectedUnit.GrowthRate;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeGrowthRate.Text := FloatToStr(FirstUnit.GrowthRate);
    end
    else
    begin
      rdeGrowthRate.Text := '';
    end;
  end;
  procedure AssignGrowthMethod;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.GrowthMethod = SelectedUnit.GrowthMethod;
      if not Same then
      begin
        break;
      end;
    end;

    if Same then
    begin
      rgMethod.ItemIndex := Ord(FirstUnit.GrowthMethod);
    end
    else
    begin
      rgMethod.ItemIndex := -1;
    end;
  end;

  procedure AssignDiscretization;
  var
    Index: integer;
  begin
    Same := True;
    for Index := 1 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[Index];
      Same := FirstUnit.LayerCollection.Count =
        SelectedUnit.LayerCollection.Count;
      if not Same then
      begin
        break;
      end;
    end;
    if Same then
    begin
      rdeVDiscretization.Text := IntToStr(FirstUnit.
        LayerCollection.Count + 1);
    end
    else
    begin
      rdeVDiscretization.Text := '';
    end;
    rdeVDiscretizationChange(nil);
  end;



  procedure AssignCustomPostions;
  var
    Index: integer;
    ARealList: TRealList;
  begin
    if FirstUnit.GrowthMethod = gmCustom then
    begin
      Same := True;
      for Index := 1 to FSelectedUnits.Count - 1 do
      begin
        SelectedUnit := FSelectedUnits[Index];
        Same := (SelectedUnit.GrowthMethod = gmCustom) and
          SelectedUnit.LayerCollection.IsSame(FirstUnit.LayerCollection);
        if not Same then
        begin
          break;
        end;
      end;
      if Same then
      begin
        ARealList := TRealList.Create;
        try
          ARealList.Capacity := FirstUnit.LayerCollection.Count;
          for Index := 0 to FirstUnit.LayerCollection.Count - 1 do
          begin
            ARealList.Add((FirstUnit.LayerCollection.Items[Index]
              as TLayerFraction).Fraction)
          end;
          ARealList.Sort;
          ARealList.Reverse;
          for Index := 0 to ARealList.Count - 1 do
          begin
            rdgSubLayerBoundaries.Cells[0,
              {rdgSubLayerBoundaries.RowCount -} Index+1]
              := FloatToStr(ARealList[Index]);
          end;
        finally
          ARealList.Free;
        end;
//        for Index := 0 to FirstUnit.LayerCollection.Count - 1 do
//        begin
////          rdgSubLayerBoundaries.Cells[0,
////            {rdgSubLayerBoundaries.RowCount -} Index+1]
////            := FloatToStr((FirstUnit.LayerCollection.Items[Index]
////            as TLayerFraction).Fraction);
//          rdgSubLayerBoundaries.Cells[0,
//            rdgSubLayerBoundaries.RowCount - Index+1]
//            := FloatToStr((FirstUnit.LayerCollection.Items[Index]
//            as TLayerFraction).Fraction);
//        end;
        UpdateLayerPositions;
      end
      else
      begin
        for Index := 0 to SelectedUnit.LayerCollection.Count - 1 do
        begin
          rdgSubLayerBoundaries.Cells[0,
            rdgSubLayerBoundaries.RowCount - Index] := '';
        end;
      end;
    end
  end;

begin
  if csDestroying in ComponentState then Exit;

  FSettingUnit := True;
  try
    rdeGrowthRate.Enabled := FSelectedUnits.Count >= 1;
    rdeVDiscretization.Enabled := FSelectedUnits.Count >= 1;
    rgMethod.Enabled := FSelectedUnits.Count >= 1;

    if FSelectedUnits.Count = 0 then
    begin
      Exit;
    end;
    FirstUnit := FSelectedUnits[0];

    FirstUnit := FSelectedUnits[0];
    AssignGrowthRate;
    AssignDiscretization;
    AssignGrowthMethod;
    AssignCustomPostions;
  finally
    FSettingUnit := False;
  end;
end;

procedure TframeDiscretization.SetPbCursor(X, Y: integer);
var
  Dummy: integer;
begin
  if FMovingLine then Exit;
  if not InBox(X,Y) then
  begin
    pbSubLayers.Cursor := crDefault;
  end
  else if sbInsertLine.Down then
  begin
    pbSubLayers.Cursor := crHorizontal;
  end
  else if IsOnLine(Y, Dummy) then
  begin
    if sbMoveLine.Down then
    begin
      pbSubLayers.Cursor := crMoveRow;
    end
    else if sbDeleteLine.Down then
    begin
      pbSubLayers.Cursor := crDelete;
    end
    else
    begin
      pbSubLayers.Cursor := crDefault;
    end;
  end
  else
  begin
    pbSubLayers.Cursor := crDefault;
  end;
end;

procedure TframeDiscretization.SetSpacing(const GrowthRate: real;
  GrowthMethod: TGrowthMethod; const SubLayers: integer;
  out Fractions: TRealArray);
var
  Index: Integer;
  Sum: Real;
  CurrentLength: Real;
  StopIndex: Integer;
  RealList: TRealList;
  Value: Real;
  StartIndex: Integer;
begin
  SetLength(Fractions, 0);
  try
    if SubLayers > 1 then
    begin
      SetLength(Fractions, SubLayers-1);

      RealList := TRealList.Create;
      try
        if GrowthMethod = gmCustom then
        begin
          for Index := 1 to rdgSubLayerBoundaries.RowCount - 1 do
          begin
            try
              if rdgSubLayerBoundaries.Cells[0,Index] <> '' then
              begin
                RealList.Add(StrToFloat(rdgSubLayerBoundaries.Cells[0,Index]));
              end;
            except on E: EConvertError do
              begin
                // ignore
              end;
            end;
          end;
          if RealList.Count = 0 then
          begin
            SetLength(Fractions, 0);
            GrowthMethod := gmUniform;
          end;
        end;

        case GrowthMethod of
          gmUniform:
            begin
              if Length(Fractions) > 0 then
              begin
                for Index := 0 to SubLayers - 2 do
                begin
                  Fractions[SubLayers - 2 - Index] := (Index+1)/SubLayers;
                end;
              end;
            end;
          gmUp:
            begin
              Sum := 1;
              CurrentLength := 1;
              for Index := 0 to SubLayers-2 do
              begin
                Fractions[SubLayers - 2 - Index] := Sum;
                CurrentLength := CurrentLength * GrowthRate;
                Sum := Sum + CurrentLength;
              end;
              for Index := 0 to SubLayers - 2 do
              begin
                Fractions[Index] := Fractions[Index]/ Sum;
              end;
            end;
          gmDown:
            begin
              Sum := 1;
              CurrentLength := 1;
              for Index := 0 to SubLayers-2 do
              begin
                Fractions[SubLayers - 2 - Index] := Sum;
                CurrentLength := CurrentLength / GrowthRate;
                Sum := Sum + CurrentLength;
              end;
              for Index := 0 to SubLayers - 2 do
              begin
                Fractions[Index] := Fractions[Index]/ Sum;
              end;
            end;
          gmMiddle, gmEdge:
            begin
              if Odd(SubLayers) then
              begin
                StopIndex := (SubLayers div 2);
              end
              else
              begin
                StopIndex := (SubLayers div 2)-1;
              end;

              Sum := 1;
              CurrentLength := 1;
              for Index := 0 to StopIndex-1 do
              begin
                Fractions[SubLayers - 2 - Index] := Sum;
                case GrowthMethod of
                  gmMiddle:
                    begin
                      CurrentLength := CurrentLength * GrowthRate;
                    end;
                  gmEdge:
                    begin
                      CurrentLength := CurrentLength / GrowthRate;
                    end;
                  else Assert(False);
                end;
                Sum := Sum + CurrentLength;
              end;
              StartIndex := StopIndex;
              if not Odd(SubLayers) then
              begin
                Fractions[SubLayers - 2 - StartIndex] := Sum;
                Sum := Sum + CurrentLength;
                Inc(StartIndex);
              end;
              for Index := StartIndex to SubLayers-2 do
              begin
                Fractions[SubLayers - 2 - Index] := Sum;
                case GrowthMethod of
                  gmMiddle:
                    begin
                      CurrentLength := CurrentLength / GrowthRate;
                    end;
                  gmEdge:
                    begin
                      CurrentLength := CurrentLength * GrowthRate;
                    end;
                  else Assert(False);
                end;
                Sum := Sum + CurrentLength;
              end;
              for Index := 0 to SubLayers - 2 do
              begin
                Fractions[Index] := Fractions[Index]/ Sum;
              end;
            end;
          gmCustom:
            begin
                RealList.Sort;
                for Index := RealList.Count - 1 downto 0 do
                begin
                  Value := RealList[Index];
                  if (Value >= 1) or (Value <= 0) then
                  begin
                    RealList.Delete(Index);
                    Continue;
                  end;
                  if Index > 0 then
                  begin
                    if Value = RealList[Index-1] then
                    begin
                      RealList.Delete(Index);
                    end;
                  end;
                end;
                SetLength(Fractions, RealList.Count);
                for Index := 0 to RealList.Count - 1 do
                begin
                  Fractions[RealList.Count - 1-  Index] := RealList[Index];
                end;
            end;
          else
            begin
              // multiple layers selected
              // do nothing
            end;
        end;
      finally
        RealList.Free;
      end;
    end;
  except on E: EConvertError do
    begin
      // ignore
    end;
  end;
end;

procedure TframeDiscretization.StartMove(X, Y: Integer);
begin
  FMovingLine := sbMoveLine.Down and IsOnLine(Y,FLineBeingMoved);
end;

procedure TframeDiscretization.UpdateLayerPositions;
var
  Fractions: TRealArray;
  GrowthRate: double;
  LayerCount: integer;
begin
//  if rdgSubLayerBoundaries.Updating then
//  begin
//    Exit;
//  end;
  if rgMethod.ItemIndex < 0 then
  begin
    Exit;
  end;
  if TryStrToFloat(rdeGrowthRate.Text, GrowthRate) and (GrowthRate >= 1) then
  begin
    if rgMethod.ItemIndex >= 0 then
    begin
      if TryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        SetSpacing(GrowthRate, TGrowthMethod(rgMethod.ItemIndex),
          LayerCount, Fractions);
        GetLayerPostions(Fractions, FLayerPositions);
      end;
    end;
  end;
end;

procedure TframeDiscretization.UpdateSelectedUnitLayers;
var
  Fractions: TRealArray;
  Index: integer;
  LayerFraction: TLayerFraction;
  SelectedUnit: TGrowthControls;
  GroupIndex: integer;
  LayerCount: integer;
begin
//  if rdgSubLayerBoundaries.Updating then
//  begin
//    Exit;
//  end;
  if not FSettingUnit then
  begin
    for GroupIndex := 0 to FSelectedUnits.Count - 1 do
    begin
      SelectedUnit := FSelectedUnits[GroupIndex];
      if not TryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        LayerCount := SelectedUnit.LayerCount;
      end;

      SetSpacing(SelectedUnit.GrowthRate, SelectedUnit.GrowthMethod,
        LayerCount, Fractions);

      if Length(Fractions) <> SelectedUnit.LayerCollection.Count then
      begin
        SelectedUnit.LayerCollection.Clear;
      end;
      for Index := 0 to Length(Fractions) - 1 do
      begin
        if Index >= SelectedUnit.LayerCollection.Count then
        begin
          SelectedUnit.LayerCollection.Add;
        end;
        LayerFraction := SelectedUnit.LayerCollection.
          Items[Index] as TLayerFraction;
        LayerFraction.Fraction := Fractions[Length(Fractions)-Index-1];
      end;
    end;
  end;
end;

procedure TframeDiscretization.UpdateSelectedUnits(List: TList);
begin
  // List must contain TGrowthControls s
  FSelectedUnits.Assign(List);
end;

procedure TframeDiscretization.UpdateStringGrid;
var
  LayerCount: integer;
  Fractions: TRealArray;
  Index: Integer;
  GrowthRate: double;
begin
//  if rdgSubLayerBoundaries.Updating then
//  begin
//    Exit;
//  end;
  if TryStrToFloat(rdeGrowthRate.Text, GrowthRate) and (GrowthRate >= 1) then
  begin
    if rgMethod.ItemIndex >= 0 then
    begin
      if tryStrToInt(rdeVDiscretization.Text, LayerCount) then
      begin
        if LayerCount > 1 then
        begin
          SetSpacing(GrowthRate, TGrowthMethod(rgMethod.ItemIndex),
            LayerCount, Fractions);
          if (TGrowthMethod(rgMethod.ItemIndex) = gmCustom)
            and ((Length(Fractions)+1) <> rdgSubLayerBoundaries.RowCount) then
          begin
            Exit;
          end;
          for Index := 0 to Length(Fractions) - 1 do
          begin
            rdgSubLayerBoundaries.Cells[0,Index+1]
              := FloatToStr(Fractions[Index]);
          end;
        end;
      end;
    end;
  end;
end;

end.
