unit frameScreenObjectCfpPipesUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, ExtCtrls,
  StdCtrls, RbwEdit, UndoItemsScreenObjects, Vcl.Mask, Vcl.ComCtrls;

type
  TframeScreenObjectCfpPipes = class(TframeScreenObject)
    pnlCaption: TPanel;
    pcCfpPipes: TPageControl;
    tabProperties: TTabSheet;
    tabOutput: TTabSheet;
    cbRecordPipes: TCheckBox;
    cbRecordNodes: TCheckBox;
    edDiameter: TLabeledEdit;
    btnDiameter: TButton;
    edTortuosity: TLabeledEdit;
    btnTortuosity: TButton;
    edRoughnessHeight: TLabeledEdit;
    btnRoughnessHeight: TButton;
    edLowerCriticalR: TLabeledEdit;
    btnLowerCriticalR: TButton;
    edHigherCriticalR: TLabeledEdit;
    btnHigherCriticalR: TButton;
    edConductancePermeability: TLabeledEdit;
    btnConductancePermeability: TButton;
    edElevation: TLabeledEdit;
    btnElevation: TButton;
    cbRecordTimeSeriesPipes: TCheckBox;
    cbRecordTimeSeriesNodes: TCheckBox;
    procedure edDiameterChange(Sender: TObject);
    procedure edTortuosityChange(Sender: TObject);
    procedure edRoughnessHeightChange(Sender: TObject);
    procedure edLowerCriticalRChange(Sender: TObject);
    procedure edHigherCriticalRChange(Sender: TObject);
    procedure edConductancePermeabilityChange(Sender: TObject);
    procedure edElevationChange(Sender: TObject);
    procedure cbRecordPipesClick(Sender: TObject);
    procedure cbRecordNodesClick(Sender: TObject);
    procedure cbRecordTimeSeriesNodesClick(Sender: TObject);
    procedure cbRecordTimeSeriesPipesClick(Sender: TObject);
    procedure edDiameterExit(Sender: TObject);
  private
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    FVariableRoughness: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectCfpPipes: TframeScreenObjectCfpPipes;

implementation

uses
  ScreenObjectUnit, ModflowCfpPipeUnit, frmGoPhastUnit,
  ModflowPackageSelectionUnit, GoPhastTypes;

resourcestring
  StrConduitWallConduct = 'Conduit wall conductance (L^2/T) (K_EXCHANGE)';
  StrConduitWallPermeab = 'Conduit wall permeability term (L/T) (K_EXCHANGE)';

{$R *.dfm}

{ TframeScreenObjectCfpPipes }

procedure TframeScreenObjectCfpPipes.cbRecordNodesClick(Sender: TObject);
begin
  inherited;
  cbRecordNodes.AllowGrayed := False;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.cbRecordPipesClick(Sender: TObject);
begin
  inherited;
  cbRecordPipes.AllowGrayed := False;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.cbRecordTimeSeriesNodesClick(Sender:
    TObject);
begin
  cbRecordTimeSeriesNodes.AllowGrayed := False;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.cbRecordTimeSeriesPipesClick(Sender:
    TObject);
begin
  cbRecordTimeSeriesPipes.AllowGrayed := False;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeScreenObjectCfpPipes.edConductancePermeabilityChange(
  Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edDiameterChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edDiameterExit(Sender: TObject);
begin
  inherited;
  if not Changing and not FVariableRoughness then
  begin
    if (edRoughnessHeight.Text = '') and (edDiameter.Text <> '') then
    begin
      edRoughnessHeight.Text := edDiameter.Text + '/10';
    end;
  end;
end;

procedure TframeScreenObjectCfpPipes.edElevationChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edHigherCriticalRChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edLowerCriticalRChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edRoughnessHeightChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.edTortuosityChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpPipes.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  ABoundary: TCfpPipeBoundary;
  ScreenObjectIndex: Integer;
begin
  FVariableRoughness := False;
  pcCfpPipes.ActivePageIndex := 0;
  Assert(ScreenObjectList.Count >= 1);
  Changing := True;
  try
    InitializeControls;
    ListOfScreenObjects := TScreenObjectList.Create;
    try
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        ABoundary := AScreenObject.ModflowCfpPipes;
        if (ABoundary <> nil) and ABoundary.Used then
        begin
          ListOfScreenObjects.Add(AScreenObject);
        end;
      end;
      if ListOfScreenObjects.Count > 0 then
      begin
        ABoundary := ListOfScreenObjects[0].ModflowCfpPipes;
        edDiameter.Text := ABoundary.Diameter;
        edTortuosity.Text := ABoundary.Tortuosity;
        edRoughnessHeight.Text := ABoundary.RoughnessHeight;
        edLowerCriticalR.Text := ABoundary.LowerCriticalR;
        edHigherCriticalR.Text := ABoundary.HigherCriticalR;
        edConductancePermeability.Text := ABoundary.ConductancePermeability;
        if edElevation.Enabled then
        begin
          edElevation.Text := ABoundary.Elevation;
        end;
        cbRecordPipes.Checked := ABoundary.RecordPipeValues;
        cbRecordNodes.Checked := ABoundary.RecordNodeValues;
        cbRecordTimeSeriesPipes.Checked := ABoundary.TimesSeriesPipes;
        cbRecordTimeSeriesNodes.Checked := ABoundary.TimesSeriesNodes;
      end;
      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := ListOfScreenObjects[ScreenObjectIndex].ModflowCfpPipes;
        if edDiameter.Text <> ABoundary.Diameter then
        begin
          edDiameter.Text := ''
        end;
        if edTortuosity.Text <> ABoundary.Tortuosity then
        begin
          edTortuosity.Text := ''
        end;
        if edRoughnessHeight.Text <> ABoundary.RoughnessHeight then
        begin
          edRoughnessHeight.Text := '';
          FVariableRoughness := True;
        end;
        if edLowerCriticalR.Text <> ABoundary.LowerCriticalR then
        begin
          edLowerCriticalR.Text := ''
        end;
        if edHigherCriticalR.Text <> ABoundary.HigherCriticalR then
        begin
          edHigherCriticalR.Text := ''
        end;
        if edConductancePermeability.Text <> ABoundary.ConductancePermeability then
        begin
          edConductancePermeability.Text := ''
        end;
        if edElevation.Enabled and (edElevation.Text <> ABoundary.Elevation) then
        begin
          edElevation.Text := ''
        end;
        if cbRecordPipes.Checked <> ABoundary.RecordPipeValues then
        begin
          cbRecordPipes.AllowGrayed := True;
          cbRecordPipes.State := cbGrayed;
        end;
        if cbRecordNodes.Checked <> ABoundary.RecordNodeValues then
        begin
          cbRecordNodes.AllowGrayed := True;
          cbRecordNodes.State := cbGrayed;
        end;

        if cbRecordTimeSeriesPipes.Checked <> ABoundary.TimesSeriesPipes then
        begin
          cbRecordPipes.AllowGrayed := True;
          cbRecordPipes.State := cbGrayed;
        end;
        if cbRecordTimeSeriesPipes.Checked <> ABoundary.TimesSeriesNodes then
        begin
          cbRecordTimeSeriesNodes.AllowGrayed := True;
          cbRecordTimeSeriesNodes.State := cbGrayed;
        end;
      end;
    finally
      ListOfScreenObjects.Free;
    end;
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectCfpPipes.InitializeControls;
begin
  edDiameter.Text := '';
  edTortuosity.Text := '';
  edRoughnessHeight.Text := '';
  edLowerCriticalR.Text := '2000';
  edHigherCriticalR.Text := '4000';
  edTortuosity.Text := '1';
  edConductancePermeability.Text := '';
  edElevation.Text := '';
  edElevation.Enabled := frmGoPhast.PhastModel.ModflowPackages.
    ConduitFlowProcess.CfpElevationChoice = cecIndividual;
  btnElevation.Enabled := edElevation.Enabled;
  cbRecordPipes.Checked := False;
  cbRecordNodes.Checked := False;
  case frmGoPhast.PhastModel.ModflowPackages.ConduitFlowProcess.CfpExchange of
    ceNodeConductance:
      begin
        edConductancePermeability.EditLabel.Caption := StrConduitWallConduct;
      end;
    ceWallPermeability:
      begin
        edConductancePermeability.EditLabel.Caption := StrConduitWallPermeab;
      end;
    else
      Assert(False);
  end;
{$IFDEF OWHMV2}
  cbRecordTimeSeriesPipes.Enabled := (frmGoPhast.ModelSelection = msModflowOwhm2)
    and frmGoPhast.PhastModel.ModflowPackages.
    ConduitFlowProcess.UseCads;
{$ELSE}
  cbRecordTimeSeriesPipes.Enabled := False;
{$ENDIF}
  cbRecordTimeSeriesNodes.Enabled := cbRecordTimeSeriesPipes.Enabled;
end;

procedure TframeScreenObjectCfpPipes.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCfpPipeBoundary;
  BoundaryUsed: Boolean;
begin
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    Item := List[ScreenObjectIndex];
    ScreenObject := Item.ScreenObject;
    Boundary := ScreenObject.ModflowCfpPipes;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;
    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.IsUsed := False;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        ScreenObject.CreateCfpBoundary;
        Boundary := ScreenObject.ModflowCfpPipes;
      end;
      if Boundary <> nil then
      begin
        Boundary.IsUsed := True;
        if edDiameter.Text <> '' then
        begin
          Boundary.Diameter := edDiameter.Text;
        end;
        if edTortuosity.Text <> '' then
        begin
          Boundary.Tortuosity := edTortuosity.Text;
        end;
        if edRoughnessHeight.Text <> '' then
        begin
          Boundary.RoughnessHeight := edRoughnessHeight.Text;
        end;
        if edLowerCriticalR.Text <> '' then
        begin
          Boundary.LowerCriticalR := edLowerCriticalR.Text;
        end;
        if edHigherCriticalR.Text <> '' then
        begin
          Boundary.HigherCriticalR := edHigherCriticalR.Text;
        end;
        if edConductancePermeability.Text <> '' then
        begin
          Boundary.ConductancePermeability := edConductancePermeability.Text;
        end;
        if edElevation.Enabled and (edElevation.Text <> '') then
        begin
          Boundary.Elevation := edElevation.Text;
        end;
        if cbRecordPipes.State <> cbGrayed then
        begin
          Boundary.RecordPipeValues := cbRecordPipes.Checked;
        end;
        if cbRecordNodes.State <> cbGrayed then
        begin
          Boundary.RecordNodeValues := cbRecordNodes.Checked;
        end;

        if cbRecordTimeSeriesPipes.State <> cbGrayed then
        begin
          Boundary.TimesSeriesPipes := cbRecordTimeSeriesPipes.Checked;
        end;
        if cbRecordTimeSeriesNodes.State <> cbGrayed then
        begin
          Boundary.TimesSeriesNodes := cbRecordTimeSeriesNodes.Checked;
        end;
      end;
    end;
  end;
end;

end.
