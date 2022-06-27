unit frameScreenObjectUzfMf6Unit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, Vcl.ComCtrls, JvToolEdit,
  ModflowPackageSelectionUnit, JvExControls, JvPageList, JvExComCtrls,
  JvPageListTreeView, frameUzfGwtConcentrationsUnit;

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
    tabGWT: TTabSheet;
    tvGwt: TJvPageListTreeView;
    splSplit: TSplitter;
    jplGwt: TJvPageList;
    procedure edSurfaceDepressionDepthChange(Sender: TObject);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure btnInsertClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure seNumberOfTimesChange(Sender: TObject);
  private
    FIntializedFrame: Boolean;
    FOnEdited: TNotifyEvent;
    FGettingData: Boolean;
    FGroundwaterET: TUzfGwEtChoice;
    FUnsatET: TUzfUnsatEtChoice;
    FGwtFrameList: TUzfGwtObjectList;
    procedure InitializeFrame;
    procedure Edited;
    { Private declarations }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  ScreenObjectUnit, PhastModelUnit, Mt3dmsChemSpeciesUnit;

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
const
  InfiltrationPosition = 6;
  PotentialETPosition = 7;
  ExtinctionDepthPosition = 8;
  ExtinctionWaterContentPosition = 9;
  AirEntryPotentialPosition = 10;
  RootPotentialPosition = 11;
  RootActivityPosition = 12;
var
//  RowIndex: Integer;
  ColIndex: Integer;
begin
  pcUzf.ActivePageIndex := 0;

  seNumberOfTimes.OnChange(seNumberOfTimes);
  rdgModflowBoundary.UseSpecialFormat[0, PestModifierRow] := True;
  rdgModflowBoundary.UseSpecialFormat[0, PestMethodRow] := True;
  rdgModflowBoundary.SpecialFormat[0, PestModifierRow] := rcf4String;
  rdgModflowBoundary.SpecialFormat[0, PestMethodRow] := rcf4String;
  rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
  rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;

  PestMethod[Ord(ucInfiltration)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(InfiltrationPosition);
  PestMethod[Ord(ucPotentialEt)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(PotentialETPosition);
  PestMethod[Ord(ucExtinctionDepth)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(ExtinctionDepthPosition);
  PestMethod[Ord(ucExtinctionWaterContent)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(ExtinctionWaterContentPosition);
  PestMethod[Ord(ucAirEntryPotential)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(AirEntryPotentialPosition);
  PestMethod[Ord(ucRootPotential)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(RootPotentialPosition);
  PestMethod[Ord(ucRootActivity)] :=
    TUzfMf6Boundary.DefaultBoundaryMethod(RootActivityPosition);

  if FIntializedFrame then
  begin
    Exit;
  end;
  FIntializedFrame := True;

  MoveGridToTabSheet(tabTime);

  seNumberOfTimes.OnChange(seNumberOfTimes);

  rdgModflowBoundary.Cells[Ord(ucStartTime), 0] := StrStartingTime;
  rdgModflowBoundary.Cells[Ord(ucEndTime), 0] := StrEndingTime;
  rdgModflowBoundary.Cells[Ord(ucInfiltration), 0] := StrInfiltration;
  rdgModflowBoundary.Cells[Ord(ucPotentialEt), 0] := StrPotentialET;
  rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), 0] := StrExtinctionDepth;
  rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), 0] := StrExtinctionWaterCon;
  rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), 0] := StrAirEntryPotential;
  rdgModflowBoundary.Cells[Ord(ucRootPotential), 0] := StrRootPotential;
  rdgModflowBoundary.Cells[Ord(ucRootActivity), 0] := StrRootActivity;


  for ColIndex := 0 to rdgModflowBoundary.ColCount - 1 do
  begin
    rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := False;
  end;


end;

procedure TframeScreenObjectUzfMf6.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  UzfCol: TUzfColumns;
begin
  inherited;

  if CanSelect then
  begin
    if (ARow >= rdgModflowBoundary.FixedRows+PestRowOffset) and (ACol >= 0) then
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
var
  SpeciesIndex: Integer;
begin
  inherited;
  Edited;
  if (ARow >= rdgModflowBoundary.FixedRows + PestRowOffset)
    and (ACol in [Ord(ucStartTime), Ord(ucEndtime)]) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.Cells[ACol, ARow]
        := rdgModflowBoundary.Cells[ACol, ARow];
    end;
  end;
end;

procedure TframeScreenObjectUzfMf6.seNumberOfTimesChange(Sender: TObject);
var
  SpeciesIndex: Integer;
begin
  inherited;
  for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
  begin
    FGwtFrameList[SpeciesIndex].rdgConcentrations.RowCount := rdgModflowBoundary.RowCount;
  end;
end;

procedure TframeScreenObjectUzfMf6.btnDeleteClick(Sender: TObject);
var
  SpeciesIndex: Integer;
begin
  if (rdgModflowBoundary.RowCount > 2 + PestRowOffset)
    and (rdgModflowBoundary.Row> 0 + PestRowOffset) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.DeleteRow(rdgModflowBoundary.SelectedRow);
    end;
  end;
  inherited;
end;

procedure TframeScreenObjectUzfMf6.btnInsertClick(Sender: TObject);
var
  SpeciesIndex: Integer;
begin
  if (seNumberOfTimes.AsInteger > 0) then
  begin
    for SpeciesIndex := 0 to FGwtFrameList.Count - 1 do
    begin
      FGwtFrameList[SpeciesIndex].rdgConcentrations.InsertRow(rdgModflowBoundary.SelectedRow);
    end;
  end;
  inherited;
end;

constructor TframeScreenObjectUzfMf6.Create(AOwner: TComponent);
begin
  inherited;
  FGwtFrameList := TUzfGwtObjectList.Create;
end;

destructor TframeScreenObjectUzfMf6.Destroy;
begin
  FGwtFrameList.Free;
  inherited;
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
  SpeciesIndex: Integer;
  ASpecies: TMobileChemSpeciesItem;
  APage: TJvStandardPage;
  AGwtFrame: TframeUzfGwtConcentrations;
  ANode: TJvPageIndexNode;
begin
  FGettingData := True;

  try
    UzfMf6Package := frmGoPhast.PhastModel.ModflowPackages.UzfMf6Package;
    FGroundwaterET := UzfMf6Package.GroundwaterET;
    FUnsatET := UzfMf6Package.UnsatET;

    ClearGrid(rdgModflowBoundary);
    InitializeFrame;
    GetStartTimes(Ord(ucStartTime));
    GetEndTimes(Ord(ucEndTime));

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

          PestModifier[Ord(ucInfiltration)] := UzfBoundary.PestInfiltrationFormula;
          PestMethod[Ord(ucInfiltration)] := UzfBoundary.PestInfiltrationMethod;
          PestModifier[Ord(ucPotentialEt)] := UzfBoundary.PestPotentialETFormula;
          PestMethod[Ord(ucPotentialEt)] := UzfBoundary.PestPotentialETMethod;
          PestModifier[Ord(ucExtinctionDepth)] := UzfBoundary.PestExtinctionDepthFormula;
          PestMethod[Ord(ucExtinctionDepth)] := UzfBoundary.PestExtinctionDepthMethod;
          PestModifier[Ord(ucExtinctionWaterContent)] := UzfBoundary.PestExtinctionWaterContentFormula;
          PestMethod[Ord(ucExtinctionWaterContent)] := UzfBoundary.PestExtinctionWaterContentMethod;
          PestModifier[Ord(ucAirEntryPotential)] := UzfBoundary.PestAirEntryPotentialFormula;
          PestMethod[Ord(ucAirEntryPotential)] := UzfBoundary.PestAirEntryPotentialMethod;
          PestModifier[Ord(ucRootPotential)] := UzfBoundary.PestRootPotentialFormula;
          PestMethod[Ord(ucRootPotential)] := UzfBoundary.PestRootPotentialMethod;
          PestModifier[Ord(ucRootActivity)] := UzfBoundary.PestRootActivityFormula;
          PestMethod[Ord(ucRootActivity)] := UzfBoundary.PestRootActivityMethod;

          seNumberOfTimes.asInteger := UzfBoundary.Values.Count;
          seNumberOfTimes.OnChange(seNumberOfTimes);
          for ItemIndex := 0 to UzfBoundary.Values.Count - 1 do
          begin
            AnItem := UzfBoundary.Values[ItemIndex] as TUzfMf6Item;

            rdgModflowBoundary.RealValue[Ord(ucStartTime), ItemIndex+1+PestRowOffset]
              := AnItem.StartTime;
            rdgModflowBoundary.RealValue[Ord(ucEndTime), ItemIndex+1+PestRowOffset]
              := AnItem.EndTime;
            rdgModflowBoundary.Cells[Ord(ucInfiltration), ItemIndex+1+PestRowOffset]
              := AnItem.Infiltration;
            rdgModflowBoundary.Cells[Ord(ucPotentialEt), ItemIndex+1+PestRowOffset]
              := AnItem.PotentialET;
            rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), ItemIndex+1+PestRowOffset]
              := AnItem.ExtinctionDepth;
            rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), ItemIndex+1+PestRowOffset]
              := AnItem.ExtinctionWaterContent;
            rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), ItemIndex+1+PestRowOffset]
              := AnItem.AirEntryPotential;
            rdgModflowBoundary.Cells[Ord(ucRootPotential), ItemIndex+1+PestRowOffset]
              := AnItem.RootPotential;
            rdgModflowBoundary.Cells[Ord(ucRootActivity), ItemIndex+1+PestRowOffset]
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

          if FirstUzf.PestInfiltrationFormula <> UzfBoundary.PestInfiltrationFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucInfiltration), PestModifierRow] := '';
          end;
          if FirstUzf.PestInfiltrationMethod <> UzfBoundary.PestInfiltrationMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucInfiltration), PestMethodRow] := '';
          end;

          if FirstUzf.PestPotentialETFormula <> UzfBoundary.PestPotentialETFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucPotentialEt), PestModifierRow] := '';
          end;
          if FirstUzf.PestExtinctionDepthMethod <> UzfBoundary.PestExtinctionDepthMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucPotentialEt), PestMethodRow] := '';
          end;

          if FirstUzf.PestExtinctionDepthFormula <> UzfBoundary.PestExtinctionDepthFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), PestModifierRow] := '';
          end;
          if FirstUzf.PestExtinctionDepthMethod <> UzfBoundary.PestExtinctionDepthMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), PestMethodRow] := '';
          end;

          if FirstUzf.PestExtinctionWaterContentFormula <> UzfBoundary.PestExtinctionWaterContentFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), PestModifierRow] := '';
          end;
          if FirstUzf.PestExtinctionWaterContentMethod <> UzfBoundary.PestExtinctionWaterContentMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), PestMethodRow] := '';
          end;

          if FirstUzf.PestAirEntryPotentialFormula <> UzfBoundary.PestAirEntryPotentialFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), PestModifierRow] := '';
          end;
          if FirstUzf.PestAirEntryPotentialMethod <> UzfBoundary.PestAirEntryPotentialMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), PestMethodRow] := '';
          end;

          if FirstUzf.PestRootPotentialFormula <> UzfBoundary.PestRootPotentialFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucRootPotential), PestModifierRow] := '';
          end;
          if FirstUzf.PestRootPotentialMethod <> UzfBoundary.PestRootPotentialMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucRootPotential), PestMethodRow] := '';
          end;

          if FirstUzf.PestRootActivityFormula <> UzfBoundary.PestRootActivityFormula then
          begin
            rdgModflowBoundary.Cells[Ord(ucRootActivity), PestModifierRow] := '';
          end;
          if FirstUzf.PestRootActivityMethod <> UzfBoundary.PestRootActivityMethod then
          begin
            rdgModflowBoundary.Cells[Ord(ucRootActivity), PestMethodRow] := '';
          end;

          if FirstUzf.PestInfiltrationFormula <> UzfBoundary.PestInfiltrationFormula then
          begin
            PestModifierAssigned[Ord(ucInfiltration)] := False;
          end;
          if FirstUzf.PestInfiltrationMethod <> UzfBoundary.PestInfiltrationMethod then
          begin
            PestMethodAssigned[Ord(ucInfiltration)] := False;
          end;

          if FirstUzf.PestPotentialETFormula <> UzfBoundary.PestPotentialETFormula then
          begin
            PestModifierAssigned[Ord(ucPotentialEt)] := False;
          end;
          if FirstUzf.PestPotentialETMethod <> UzfBoundary.PestPotentialETMethod then
          begin
            PestMethodAssigned[Ord(ucPotentialEt)] := False;
          end;

          if FirstUzf.PestExtinctionDepthFormula <> UzfBoundary.PestExtinctionDepthFormula then
          begin
            PestModifierAssigned[Ord(ucExtinctionDepth)] := False;
          end;
          if FirstUzf.PestExtinctionDepthMethod <> UzfBoundary.PestExtinctionDepthMethod then
          begin
            PestMethodAssigned[Ord(ucExtinctionDepth)] := False;
          end;

          if FirstUzf.PestExtinctionWaterContentFormula <> UzfBoundary.PestExtinctionWaterContentFormula then
          begin
            PestModifierAssigned[Ord(ucExtinctionWaterContent)] := False;
          end;
          if FirstUzf.PestExtinctionWaterContentMethod <> UzfBoundary.PestExtinctionWaterContentMethod then
          begin
            PestMethodAssigned[Ord(ucExtinctionWaterContent)] := False;
          end;

          if FirstUzf.PestAirEntryPotentialFormula <> UzfBoundary.PestAirEntryPotentialFormula then
          begin
            PestModifierAssigned[Ord(ucAirEntryPotential)] := False;
          end;
          if FirstUzf.PestAirEntryPotentialMethod <> UzfBoundary.PestAirEntryPotentialMethod then
          begin
            PestMethodAssigned[Ord(ucAirEntryPotential)] := False;
          end;

          if FirstUzf.PestRootPotentialFormula <> UzfBoundary.PestRootPotentialFormula then
          begin
            PestModifierAssigned[Ord(ucRootPotential)] := False;
          end;
          if FirstUzf.PestRootPotentialMethod <> UzfBoundary.PestRootPotentialMethod then
          begin
            PestMethodAssigned[Ord(ucRootPotential)] := False;
          end;

          if FirstUzf.PestRootActivityFormula <> UzfBoundary.PestRootActivityFormula then
          begin
            PestModifierAssigned[Ord(ucRootActivity)] := False;
          end;
          if FirstUzf.PestRootActivityMethod <> UzfBoundary.PestRootActivityMethod then
          begin
            PestMethodAssigned[Ord(ucRootActivity)] := False;
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
    rdgModflowBoundary.HideEditor;

    tabGWT.TabVisible := frmGoPhast.PhastModel.GwtUsed;
    if tabGWT.TabVisible then
    begin
      tvGwt.Items.Clear;
      for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
      begin
        ASpecies := frmGoPhast.PhastModel.MobileComponents[SpeciesIndex];
        if SpeciesIndex >= jplGwt.PageCount then
        begin
          APage := TJvStandardPage.Create(self);
          APage.PageList := jplGwt;
          AGwtFrame := TframeUzfGwtConcentrations.Create(nil);
          FGwtFrameList.Add(AGwtFrame);
          AGwtFrame.Parent := APage;
          AGwtFrame.Align := alClient;
        end
        else
        begin
          AGwtFrame := FGwtFrameList[SpeciesIndex];
        end;
        ANode := tvGwt.Items.Add(nil, ASpecies.Name) as TJvPageIndexNode;
        ANode.PageIndex := SpeciesIndex;
        AGwtFrame.GetData(List, SpeciesIndex);
        if SpeciesIndex = 0 then
        begin
          ANode.Selected := True;
        end;
      end;
    end
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
  SpeciesIndex: Integer;
  AGwtFrame: TframeUzfGwtConcentrations;
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
  Brooks_Corey_Epsilon := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6BrooksCoreyEpsilon);
  Initial_Unsaturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6InitialUnsaturatedWaterContent);
  Residual_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6ReisidualWaterContent);
  Saturated_Water_Content := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6SaturatedWaterContent);
  Surface_Depression_Depth := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6SurfaceDepressionDepth);
  Vertical_Saturated_K := frmGoPhast.PhastModel.DataArrayManager.
    GetDataSetByName(StrUzfMf6VerticalSaturatedK);


  NewValues := nil;
  try
    if not ClearAll  then
    begin
      NewValues := TUzfMf6Collection.Create(nil, nil, nil);
      for TimeIndex := 0 to seNumberOfTimes.AsInteger - 1 do
      begin
        if TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(ucStartTime), TimeIndex+1+PestRowOffset], StartTime)
          and TryStrToFloat(rdgModflowBoundary.Cells[
          Ord(ucEndTime), TimeIndex+1+PestRowOffset], EndTime)
          and (rdgModflowBoundary.Cells[
          Ord(ucInfiltration), TimeIndex+1+PestRowOffset] <> '') then
        begin
          NewItem := NewValues.Add as TUzfMf6Item;
          NewItem.StartTime := StartTime;
          NewItem.EndTime := EndTime;
          NewItem.Infiltration := rdgModflowBoundary.Cells[
            Ord(ucInfiltration), TimeIndex+1+PestRowOffset];
          NewItem.PotentialET := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucPotentialEt), TimeIndex+1+PestRowOffset]);
          NewItem.ExtinctionDepth := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucExtinctionDepth), TimeIndex+1+PestRowOffset]);
          NewItem.ExtinctionWaterContent := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucExtinctionWaterContent), TimeIndex+1+PestRowOffset]);
          NewItem.AirEntryPotential := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucAirEntryPotential), TimeIndex+1+PestRowOffset]);
          NewItem.RootPotential := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucRootPotential), TimeIndex+1+PestRowOffset]);
          NewItem.RootActivity := NonBlank(rdgModflowBoundary.Cells[
            Ord(ucRootActivity), TimeIndex+1+PestRowOffset]);
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

        if rdgModflowBoundary.Cells[Ord(ucInfiltration), PestModifierRow] <> '' then
        begin
          Boundary.PestInfiltrationFormula := PestModifier[Ord(ucInfiltration)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucInfiltration), PestMethodRow] <> '' then
        begin
          Boundary.PestInfiltrationMethod := PestMethod[Ord(ucInfiltration)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucPotentialEt), PestModifierRow] <> '' then
        begin
          Boundary.PestPotentialETFormula := PestModifier[Ord(ucPotentialEt)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucPotentialEt), PestMethodRow] <> '' then
        begin
          Boundary.PestPotentialETMethod := PestMethod[Ord(ucPotentialEt)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), PestModifierRow] <> '' then
        begin
          Boundary.PestExtinctionDepthFormula := PestModifier[Ord(ucExtinctionDepth)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucExtinctionDepth), PestMethodRow] <> '' then
        begin
          Boundary.PestExtinctionDepthMethod := PestMethod[Ord(ucExtinctionDepth)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), PestModifierRow] <> '' then
        begin
          Boundary.PestExtinctionWaterContentFormula := PestModifier[Ord(ucExtinctionWaterContent)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucExtinctionWaterContent), PestMethodRow] <> '' then
        begin
          Boundary.PestExtinctionWaterContentMethod := PestMethod[Ord(ucExtinctionWaterContent)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), PestModifierRow] <> '' then
        begin
          Boundary.PestAirEntryPotentialFormula := PestModifier[Ord(ucAirEntryPotential)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucAirEntryPotential), PestMethodRow] <> '' then
        begin
          Boundary.PestAirEntryPotentialMethod := PestMethod[Ord(ucAirEntryPotential)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucRootPotential), PestModifierRow] <> '' then
        begin
          Boundary.PestRootPotentialFormula := PestModifier[Ord(ucRootPotential)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucRootPotential), PestMethodRow] <> '' then
        begin
          Boundary.PestRootPotentialMethod := PestMethod[Ord(ucRootPotential)];
        end;

        if rdgModflowBoundary.Cells[Ord(ucRootActivity), PestModifierRow] <> '' then
        begin
          Boundary.PestRootActivityFormula := PestModifier[Ord(ucRootActivity)];
        end;
        if rdgModflowBoundary.Cells[Ord(ucRootActivity), PestMethodRow] <> '' then
        begin
          Boundary.PestRootActivityMethod := PestMethod[Ord(ucRootActivity)];
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

  if tabGWT.TabVisible then
  begin
    for SpeciesIndex := 0 to frmGoPhast.PhastModel.MobileComponents.Count - 1 do
    begin
      AGwtFrame := FGwtFrameList[SpeciesIndex];
      AGwtFrame.setData(List, SpeciesIndex);
    end;
  end;
end;

end.
