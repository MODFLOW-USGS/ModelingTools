unit frameScreenObjectUzfMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, Vcl.ComCtrls, JvToolEdit,
  ModflowPackageSelectionUnit;

type
  TUzfColumns = (ucStartTime, ucEndTime, ucInfiltration, ucPotentialEt,
    ucExtinctionDepth, ucExtinctionWaterContent, ucAirEntryPotential,
    ucRootPotential, ucRootActivity);

  TframeScreenObjectUzfMf6 = class(TframeScreenObjectNoParam)
    pcUzf: TPageControl;
    tabSteadyProperties: TTabSheet;
    tabTime: TTabSheet;
    edSurfaceDepressionDepth: TJvComboEdit;
    edVerticalSaturatedK: TJvComboEdit;
    edResidualWaterContent: TJvComboEdit;
    edSaturatedWaterContent: TJvComboEdit;
    edInitialWaterContent: TJvComboEdit;
    edBrooksCoreyEpsilon: TJvComboEdit;
    lblSurfaceDepressionDepth: TLabel;
    lblVerticalSaturatedK: TLabel;
    lblResidualWaterContent: TLabel;
    lblSaturatedWaterContent: TLabel;
    lblInitialWaterContent: TLabel;
    lblBrooksCoreyEpsilon: TLabel;
    procedure edSurfaceDepressionDepthChange(Sender: TObject);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
  private
    FIntializedFrame: Boolean;
    FOnEdited: TNotifyEvent;
    FGettingData: Boolean;
    FGroundwaterET: TUzfGwEtChoice;
    FUnsatET: TUzfUnsatEtChoice;
    procedure InitializeFrame;
    procedure Edited;
    { Private declarations }
  public
    procedure GetData(List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnEdited: TNotifyEvent read FOnEdited write FOnEdited;
    { Public declarations }
  end;

var
  frameScreenObjectUzfMf6: TframeScreenObjectUzfMf6;

implementation

uses
  GoPhastTypes, ModflowUzfMf6Unit, frmGoPhastUnit, DataSetUnit,
  ScreenObjectUnit, PhastModelUnit;

resourcestring
  StrInfiltration = 'Infiltration (finf)';
  StrPotentialET = 'Potential ET (pet)';
  StrExtinctionDepth = 'Extinction depth (extdp)';
  StrExtinctionWaterCon = 'Extinction water content (extwc)';
  StrAirEntryPotential = 'Air entry potential (ha)';
  StrRootPotential = 'Root potential (hroot)';
  StrRootActivity = 'Root activity (rootact)';

{$R *.dfm}

{ TframeScreenObjectUzfMf6 }

procedure TframeScreenObjectUzfMf6.InitializeFrame;
begin
  pcUzf.ActivePageIndex := 0;
  if FIntializedFrame then
  begin
    Exit;
  end;
  FIntializedFrame := True;

  MoveGridToTabSheet(tabTime);
//  pnlBottom.Parent := tabTime;
//  pnlGrid.Align := alClient;
//  pnlGrid.Parent := tabTime;

  rdgModflowBoundary.Cells[Ord(ucStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(ucEndTime), 0] := StrEndingTime;
  rdgModflowBoundary.Cells[Ord(ucInfiltration), 0] := StrInfiltration;
  rdgModflowBoundary.Cells[Ord(ucPotentialEt), 0] := StrPotentialET;
  rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), 0] := StrExtinctionDepth;
  rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), 0] := StrExtinctionWaterCon;
  rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), 0] := StrAirEntryPotential;
  rdgModflowBoundary.Cells[Ord(ucRootPotential), 0] := StrRootPotential;
  rdgModflowBoundary.Cells[Ord(ucRootActivity), 0] := StrRootActivity;
end;

procedure TframeScreenObjectUzfMf6.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  UzfCol: TUzfColumns;
begin
  inherited;
  if CanSelect then
  begin
    if (ARow >= rdgModflowBoundary.FixedRows) and (ACol >= 0) then
    begin
      UzfCol := TUzfColumns(ACol);
      case UzfCol of
        ucStartTime, ucEndTime, ucInfiltration: ;
        ucPotentialEt:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare]);
          end;
        ucExtinctionDepth:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare]);
          end;
        ucExtinctionWaterContent:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare])
              and (FUnsatET = uuecWaterContent);
          end;
        ucAirEntryPotential:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare])
              and (FUnsatET = uuecCapillaryPressure);
          end;
        ucRootPotential:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare])
              and (FUnsatET = uuecCapillaryPressure);
          end;
        ucRootActivity:
          begin
            CanSelect :=
              (FGroundwaterET in [ugecSimulateUnsatOnly, ugecLinear, ugecSquare])
              and (FUnsatET = uuecCapillaryPressure);
          end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectUzfMf6.rdgModflowBoundarySetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectUzfMf6.Edited;
begin
  if Assigned(FOnEdited) and not FGettingData then
  begin
    FOnEdited(self);
  end;
end;

procedure TframeScreenObjectUzfMf6.edSurfaceDepressionDepthChange(Sender: TObject);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectUzfMf6.GetData(List: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  UzfBoundary: TUzfMf6Boundary;
  FoundFirst: Boolean;
  ItemIndex: Integer;
  AnItem: TUzfMf6Item;
  TimeDataIdentical: Boolean;
  FirstUzf: TUzfMf6Boundary;
  UzfMf6Package: TUzfMf6PackageSelection;
begin
  FGettingData := True;

  try
    UzfMf6Package := frmGoPhast.PhastModel.ModflowPackages.UzfMf6Package;
    FGroundwaterET := UzfMf6Package.GroundwaterET;
    FUnsatET := UzfMf6Package.UnsatET;

    InitializeFrame;
    GetStartTimes(Ord(ucStartTime));
    GetEndTimes(Ord(ucEndTime));
    ClearGrid(rdgModflowBoundary);

//    edSurfaceDepressionDepth.Text := '';
//    edVerticalSaturatedK.Text := '';
//    edResidualWaterContent.Text := '';
//    edSaturatedWaterContent.Text := '';
//    edInitialWaterContent.Text := '';
//    edBrooksCoreyEpsilon.Text := '';

    seNumberOfTimes.AsInteger := 0;
    FoundFirst := False;
    TimeDataIdentical := True;
    FirstUzf := nil;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      UzfBoundary := List[ScreenObjectIndex].ScreenObject.ModflowUzfMf6Boundary;
      if (UzfBoundary <> nil) and UzfBoundary.Used then
      begin
        if not FoundFirst then
        begin
          FoundFirst := True;

          edSurfaceDepressionDepth.Text := UzfBoundary.SurfaceDepressionDepth;
          edVerticalSaturatedK.Text := UzfBoundary.VerticalSaturatedK;
          edResidualWaterContent.Text := UzfBoundary.ResidualWaterContent;
          edSaturatedWaterContent.Text := UzfBoundary.SaturatedWaterContent;
          edInitialWaterContent.Text := UzfBoundary.InitialWaterContent;
          edBrooksCoreyEpsilon.Text := UzfBoundary.BrooksCoreyEpsilon;

          seNumberOfTimes.asInteger := UzfBoundary.Values.Count;
          seNumberOfTimes.OnChange(seNumberOfTimes);
          for ItemIndex := 0 to UzfBoundary.Values.Count - 1 do
          begin
            AnItem := UzfBoundary.Values[ItemIndex] as TUzfMf6Item;

            rdgModflowBoundary.RealValue[Ord(ucStartTime), ItemIndex+1]
              := AnItem.StartTime;
            rdgModflowBoundary.RealValue[Ord(ucEndTime), ItemIndex+1]
              := AnItem.EndTime;
            rdgModflowBoundary.Cells[Ord(ucInfiltration), ItemIndex+1]
              := AnItem.Infiltration;
            rdgModflowBoundary.Cells[Ord(ucPotentialEt), ItemIndex+1]
              := AnItem.PotentialET;
            rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), ItemIndex+1]
              := AnItem.ExtinctionDepth;
            rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), ItemIndex+1]
              := AnItem.ExtinctionWaterContent;
            rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), ItemIndex+1]
              := AnItem.AirEntryPotential;
            rdgModflowBoundary.Cells[Ord(ucRootPotential), ItemIndex+1]
              := AnItem.RootPotential;
            rdgModflowBoundary.Cells[Ord(ucRootActivity), ItemIndex+1]
              := AnItem.RootActivity;
          end;

          FirstUzf := UzfBoundary;
        end
        else
        begin
          if edSurfaceDepressionDepth.Text <> UzfBoundary.SurfaceDepressionDepth then
          begin
            edSurfaceDepressionDepth.Text := '';
          end;
          if edVerticalSaturatedK.Text <> UzfBoundary.VerticalSaturatedK then
          begin
            edVerticalSaturatedK.Text := '';
          end;
          if edResidualWaterContent.Text <> UzfBoundary.ResidualWaterContent then
          begin
            edResidualWaterContent.Text := '';
          end;
          if edSaturatedWaterContent.Text <> UzfBoundary.SaturatedWaterContent then
          begin
            edSaturatedWaterContent.Text := '';
          end;
          if edInitialWaterContent.Text <> UzfBoundary.InitialWaterContent then
          begin
            edInitialWaterContent.Text := '';
          end;
          if edBrooksCoreyEpsilon.Text <> UzfBoundary.BrooksCoreyEpsilon then
          begin
            edBrooksCoreyEpsilon.Text := '';
          end;

          if TimeDataIdentical
            and not UzfBoundary.Values.IsSame(FirstUzf.Values) then
          begin
            ClearGrid(rdgModflowBoundary);
            TimeDataIdentical := False;
          end;
        end;
      end;
    end;
  finally
    FGettingData := False;
  end;
end;

procedure TframeScreenObjectUzfMf6.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TUzfMf6Boundary;
  BoundaryUsed: Boolean;
  TimeIndex: Integer;
  NewValues: TUzfMf6Collection;
  StartTime: double;
  EndTime: double;
  NewItem: TUzfMf6Item;
  Brooks_Corey_Epsilon: TDataArray;
  Initial_Unsaturated_Water_Content: TDataArray;
  Residual_Water_Content: TDataArray;
  Saturated_Water_Content: TDataArray;
  Surface_Depression_Depth: TDataArray;
  Vertical_Saturated_K: TDataArray;
  AScreenObject: TScreenObject;
  DataSetIndex: Integer;
  function NonBlank(const Formula: string): string;
  begin
    if Formula = '' then
    begin
      result := '0';
    end
    else
    begin
      result := Formula;
    end;
  end;
  procedure RemoveLinksToUnusedDataSets;
  begin
    if Brooks_Corey_Epsilon <> nil then
    begin
      AScreenObject.RemoveDataSet(Brooks_Corey_Epsilon)
    end;
    if Initial_Unsaturated_Water_Content <> nil then
    begin
      AScreenObject.RemoveDataSet(Initial_Unsaturated_Water_Content)
    end;
    if Residual_Water_Content <> nil then
    begin
      AScreenObject.RemoveDataSet(Residual_Water_Content)
    end;
    if Saturated_Water_Content <> nil then
    begin
      AScreenObject.RemoveDataSet(Saturated_Water_Content)
    end;
    if Surface_Depression_Depth <> nil then
    begin
      AScreenObject.RemoveDataSet(Surface_Depression_Depth)
    end;
    if Vertical_Saturated_K <> nil then
    begin
      AScreenObject.RemoveDataSet(Vertical_Saturated_K)
    end;
  end;
begin
  Brooks_Corey_Epsilon := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
  Initial_Unsaturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
  Residual_Water_Content := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6ReisidualWaterContent);
  Saturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SaturatedWaterContent);
  Surface_Depression_Depth := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
  Vertical_Saturated_K := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(StrUzfMf6VerticalSaturatedK);


  NewValues := nil;
  try
    if not ClearAll  then
    begin
      NewValues := TUzfMf6Collection.Create(nil, nil, nil);
      for TimeIndex := 0 to seNumberOfTimes.AsInteger - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(ucStartTime), TimeIndex+1], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(ucEndTime), TimeIndex+1], EndTime)
          and (rdgModflowBoundary.Cells[
          Ord(ucInfiltration), TimeIndex+1] <> '') then
        begin
          NewItem := NewValues.Add as TUzfMf6Item;
          NewItem.StartTime := StartTime;
          NewItem.EndTime := EndTime;
          NewItem.Infiltration := rdgModflowBoundary.Cells[
            Ord(ucInfiltration), TimeIndex+1];
          NewItem.PotentialET := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucPotentialEt), TimeIndex+1]);
          NewItem.ExtinctionDepth := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucExtinctionDepth), TimeIndex+1]);
          NewItem.ExtinctionWaterContent := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucExtinctionWaterContent), TimeIndex+1]);
          NewItem.AirEntryPotential := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucAirEntryPotential), TimeIndex+1]);
          NewItem.RootPotential := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucRootPotential), TimeIndex+1]);
          NewItem.RootActivity := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucRootActivity), TimeIndex+1]);
        end;
      end;
    end;

    for Index := 0 to List.Count - 1 do
    begin
      Item := List.Items[Index];
      AScreenObject := Item.ScreenObject;
      Boundary := Item.ScreenObject.ModflowUzfMf6Boundary;
      BoundaryUsed := (Boundary <> nil) and Boundary.Used;

      if ClearAll then
      begin
        if BoundaryUsed then
        begin
          Boundary.Clear;
        end;
        RemoveLinksToUnusedDataSets;
      end
      else if SetAll or BoundaryUsed then
      begin
        if Boundary = nil then
        begin
          Item.ScreenObject.CreateModflowUzfMf6Boundary;
          Boundary := Item.ScreenObject.ModflowUzfMf6Boundary;
        end;

        if edSurfaceDepressionDepth.Text <> '' then
        begin
          Boundary.SurfaceDepressionDepth := edSurfaceDepressionDepth.Text;
          if Surface_Depression_Depth <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Surface_Depression_Depth);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.SurfaceDepressionDepth;
          end;
        end;
        if edVerticalSaturatedK.Text <> '' then
        begin
          Boundary.VerticalSaturatedK := edVerticalSaturatedK.Text;
          if Vertical_Saturated_K <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Vertical_Saturated_K);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.VerticalSaturatedK;
          end;
        end;
        if edResidualWaterContent.Text <> '' then
        begin
          Boundary.ResidualWaterContent := edResidualWaterContent.Text;
          if Residual_Water_Content <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Residual_Water_Content);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.ResidualWaterContent;
          end;
        end;
        if edSaturatedWaterContent.Text <> '' then
        begin
          Boundary.SaturatedWaterContent := edSaturatedWaterContent.Text;
          if Saturated_Water_Content <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Saturated_Water_Content);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.SaturatedWaterContent;
          end;
        end;
        if edInitialWaterContent.Text <> '' then
        begin
          Boundary.InitialWaterContent := edInitialWaterContent.Text;
          if Initial_Unsaturated_Water_Content <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Initial_Unsaturated_Water_Content);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.InitialWaterContent;
          end;
        end;
        if edBrooksCoreyEpsilon.Text <> '' then
        begin
          Boundary.BrooksCoreyEpsilon := edBrooksCoreyEpsilon.Text;
          if Brooks_Corey_Epsilon <> nil then
          begin
            DataSetIndex := AScreenObject.AddDataSet(Brooks_Corey_Epsilon);
            AScreenObject.DataSetFormulas[DataSetIndex] :=
              Boundary.BrooksCoreyEpsilon;
          end;
        end;

        if NewValues.Count > 0 then
        begin
          Boundary.Values := NewValues;
        end;
      end
      else
      begin
        RemoveLinksToUnusedDataSets;
      end;
    end;
  finally
    NewValues.Free;
  end;
end;

end.
