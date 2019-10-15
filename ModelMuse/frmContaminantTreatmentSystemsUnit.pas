unit frmContaminantTreatmentSystemsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.ExtCtrls,
  JvExExtCtrls, JvNetscapeSplitter, Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons,
  JvExStdCtrls, JvListBox, frameGridUnit, Mt3dCtsSystemUnit, UndoItems;

type
  TCstWellColumns = (cwcStartTime, cwcEndTime, cwcExtractionWells, cwcInjectionWells);
  TCstExternalFlowColumns = (cefcStartTime, cefcEndTime, cstOutflow, cstInflow, cstInflowConc);
  TInjectionOptionColumns = (iocStartTime, iocEndTime, iocTreament1, iocValue1);

  TUndoEditCTS = class(TCustomUndo)
  private
    FOldCTS: TCtsSystemCollection;
    FNewCts: TCtsSystemCollection;
  protected
    function Description: string; override;
  public
    procedure DoCommand; override;
    procedure Undo; override;
    constructor Create(var NewCts: TCtsSystemCollection);
    destructor Destroy; override;
  end;

  TfrmContaminantTreatmentSystems = class(TfrmCustomGoPhast)
    tvTreatmentSystems: TTreeView;
    splttr1: TJvNetscapeSplitter;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnCancelBtn: TBitBtn;
    btnOkBtn: TBitBtn;
    btnDeleteSystem: TButton;
    btnAddSystem: TButton;
    pnlMain: TPanel;
    pgcMain: TPageControl;
    tabWells: TTabSheet;
    tabExternalFlows: TTabSheet;
    frameExternalFlows: TframeGrid;
    tabTreatments: TTabSheet;
    pnlTreatmentOptions: TPanel;
    lblTreatmentOption: TLabel;
    comboTreatmentOption: TComboBox;
    pgcTreatments: TPageControl;
    tabDefaultOptions: TTabSheet;
    frameDefaultOptions: TframeGrid;
    tabIndividualWellOptions: TTabSheet;
    splttr2: TJvNetscapeSplitter;
    tvIndividualObjectOptions: TTreeView;
    pnl1: TPanel;
    pnl2: TPanel;
    cbUseDefaultOptions: TCheckBox;
    frameIndividualWellOptions: TframeGrid;
    pnlTop: TPanel;
    edSystemName: TLabeledEdit;
    frameWells: TframeGrid;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure tvTreatmentSystemsChange(Sender: TObject; Node: TTreeNode);
    procedure comboTreatmentOptionChange(Sender: TObject);
    procedure tvIndividualObjectOptionsChange(Sender: TObject; Node: TTreeNode);
    procedure btnAddSystemClick(Sender: TObject);
    procedure btnDeleteSystemClick(Sender: TObject);
    procedure btnOkBtnClick(Sender: TObject);
    procedure frameWellsGridButtonClick(Sender: TObject; ACol, ARow: Integer);
  private
    FCtsSystems: TCtsSystemCollection;
    FWellObjects: TStringList;
    FSelectedSystem: TCtsSystem;
    NCOMP: Integer;
    FWellItem: TIndividualWellInjectionItem;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmContaminantTreatmentSystems: TfrmContaminantTreatmentSystems;

implementation

uses
  frmGoPhastUnit, ModflowPackageSelectionUnit, PhastModelUnit, ScreenObjectUnit,
  ModflowTimeUnit, frmEditSelectedWellsUnit;

{$R *.dfm}

{ TfrmContaminantTreatmentSystems }


procedure TfrmContaminantTreatmentSystems.btnAddSystemClick(
  Sender: TObject);
var
  NewSystem: TCtsSystem;
begin
  inherited;
  NewSystem := FCtsSystems.Add.CtsSystem;
  NewSystem.Name := Format('CTS %d', [FCtsSystems.count]);
  tvTreatmentSystems.Items.AddObject(nil, NewSystem.Name, NewSystem);
end;

procedure TfrmContaminantTreatmentSystems.btnDeleteSystemClick(Sender: TObject);
var
  ASystemItem: TCtsSystemItem;
begin
  inherited;
  ASystemItem := tvTreatmentSystems.Selected.Data;
  ASystemItem.Free;
  tvTreatmentSystems.Selected.Free;
  tvTreatmentSystemsChange(nil, nil);
end;

procedure TfrmContaminantTreatmentSystems.btnOkBtnClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmContaminantTreatmentSystems.comboTreatmentOptionChange(
  Sender: TObject);
begin
  inherited;
  tabIndividualWellOptions.TabVisible :=
    TTreatmentDistribution(comboTreatmentOption.ItemIndex) = tlIndividual;
end;

procedure TfrmContaminantTreatmentSystems.FormCreate(Sender: TObject);
begin
  inherited;
  FCtsSystems := TCtsSystemCollection.Create(nil);
  FWellObjects := TStringList.Create;
  GetData;
end;

procedure TfrmContaminantTreatmentSystems.FormDestroy(Sender: TObject);
begin
  inherited;
  FCtsSystems.Free;
  FWellObjects.Free;
end;

procedure TfrmContaminantTreatmentSystems.frameWellsGridButtonClick(
  Sender: TObject; ACol, ARow: Integer);
var
  AvailableWells: TStringList;
  SelectedWells: TStringList;
begin
  inherited;
  AvailableWells := TStringList.Create;
  SelectedWells := TStringList.Create;
  frmEditSelectedWells := TfrmEditSelectedWells.Create(nil);
  try
    AvailableWells.Assign(FWellObjects);
    SelectedWells.DelimitedText := frameWells.Grid.Cells[ACol, ARow];
    frmEditSelectedWells.GetData(AvailableWells, SelectedWells);
    if frmEditSelectedWells.ShowModal = mrOk then
    begin
      frmEditSelectedWells.SetData(SelectedWells);
      frameWells.Grid.Cells[ACol, ARow] := SelectedWells.DelimitedText;
    end;
  finally
    AvailableWells.Free;
    SelectedWells.Free;
    FreeAndNil(frmEditSelectedWells);
  end;
end;

procedure TfrmContaminantTreatmentSystems.GetData;
var
  WellPackageChoice: TCtsWellPackageChoice;
  LocalModel: TPhastModel;
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
  SystemIndex: Integer;
  ASystem: TCtsSystem;
  StressPeriods: TModflowStressPeriods;
  SystemItem: TCtsSystemItem;
begin
  LocalModel := frmGoPhast.PhastModel;

  NCOMP := LocalModel.NumberOfMt3dChemComponents;
  frameExternalFlows.Grid.ColCount := NCOMP + 4;
  frameDefaultOptions.Grid.ColCount := NCOMP*2+ 2;
  frameIndividualWellOptions.Grid.ColCount := NCOMP*2+ 2;

  WellPackageChoice := LocalModel.ModflowPackages.Mt3dCts.WellPackageChoice;

  StressPeriods := LocalModel.ModflowStressPeriods;
  StressPeriods.FillPickListWithStartTimes(frameWells.Grid, 0);
  StressPeriods.FillPickListWithEndTimes(frameWells.Grid, 1);
  StressPeriods.FillPickListWithStartTimes(frameExternalFlows.Grid, 0);
  StressPeriods.FillPickListWithEndTimes(frameExternalFlows.Grid, 1);
  StressPeriods.FillPickListWithStartTimes(frameDefaultOptions.Grid, 0);
  StressPeriods.FillPickListWithEndTimes(frameDefaultOptions.Grid, 1);
  StressPeriods.FillPickListWithStartTimes(frameIndividualWellOptions.Grid, 0);
  StressPeriods.FillPickListWithEndTimes(frameIndividualWellOptions.Grid, 1);

  for ScreenObjectIndex := 0 to LocalModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := LocalModel.ScreenObjects[ScreenObjectIndex];
    if AScreenObject.Deleted then
    begin
      Continue;
    end;
    case WellPackageChoice of
      cwpcMnw2:
        begin
          if (AScreenObject.ModflowMnw2Boundary <> nil)
            and AScreenObject.ModflowMnw2Boundary.Used then
          begin
            FWellObjects.AddObject(AScreenObject.Name, AScreenObject);
          end;
        end;
      cwpcWel:
        begin
          if (AScreenObject.ModflowWellBoundary <> nil)
            and AScreenObject.ModflowWellBoundary.Used then
          begin
            FWellObjects.AddObject(AScreenObject.Name, AScreenObject);
          end;
        end;
      else
        Assert(False);
    end;
  end;
  FWellObjects.Sorted := True;

  FCtsSystems.Assign(LocalModel.CtsSystems);
  for SystemIndex := 0 to FCtsSystems.Count - 1 do
  begin
    SystemItem := FCtsSystems[SystemIndex];
    ASystem := SystemItem.CtsSystem;
    tvTreatmentSystems.Items.AddObject(nil, ASystem.Name, SystemItem);
  end;
end;

procedure TfrmContaminantTreatmentSystems.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoEditCTS.Create(FCtsSystems));
end;

procedure TfrmContaminantTreatmentSystems.tvIndividualObjectOptionsChange(
  Sender: TObject; Node: TTreeNode);
var
  InjIndex: Integer;
  InjItem: TCtsInjectionTimeItem;
  Treatment: TInjectionOptionItem;
  ColIndex: Integer;
  CompIndex: Integer;
begin
  inherited;
  if FWellItem <> nil then
  begin
    FWellItem.UseDefaultInjectionOptions := cbUseDefaultOptions.Checked;
    FWellItem.Injections.Count := frameIndividualWellOptions.seNumber.AsInteger;

    for InjIndex := 0 to FWellItem.Injections.Count - 1 do
    begin
      InjItem := FWellItem.Injections[InjIndex];
      InjItem.StartTime :=
        frameIndividualWellOptions.Grid.RealValue[Ord(iocStartTime), InjIndex+1];
      InjItem.EndTime :=
        frameIndividualWellOptions.Grid.RealValue[Ord(iocEndTime), InjIndex+1];
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        Treatment.TreatmentOption :=
          TTreatmentOption(frameIndividualWellOptions.Grid.ItemIndex
          [ColIndex, InjIndex+1]);
        Inc(ColIndex);
        Treatment.Value :=
          frameIndividualWellOptions.Grid.Cells[ColIndex, InjIndex+1];
        Inc(ColIndex);
      end;
    end;
  end;
  if Node <> nil then
  begin
    FWellItem := Node.Data;
  end
  else
  begin
    FWellItem := nil;
  end;
  if FWellItem <> nil then
  begin
    cbUseDefaultOptions.Checked := FWellItem.UseDefaultInjectionOptions;
    frameIndividualWellOptions.seNumber.AsInteger := FWellItem.Injections.Count;

    for InjIndex := 0 to FWellItem.Injections.Count - 1 do
    begin
      InjItem := FWellItem.Injections[InjIndex];
      frameIndividualWellOptions.Grid.RealValue[Ord(iocStartTime), InjIndex+1] :=
        InjItem.StartTime;
      frameIndividualWellOptions.Grid.RealValue[Ord(iocEndTime), InjIndex+1] :=
        InjItem.EndTime;
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        frameIndividualWellOptions.Grid.ItemIndex[ColIndex, InjIndex+1] :=
          Ord(Treatment.TreatmentOption);
        Inc(ColIndex);
        frameIndividualWellOptions.Grid.Cells[ColIndex, InjIndex+1] :=
          Treatment.Value;
        Inc(ColIndex);
      end;
    end;

  end;
end;

procedure TfrmContaminantTreatmentSystems.tvTreatmentSystemsChange(
  Sender: TObject; Node: TTreeNode);
var
  ExtractionIndex: Integer;
  CtsObject: TCtsObjectItem;
  ExternalFlowIndex: Integer;
  ExternalFlowsItem: TCtsExternalFlowsItem;
  CompIndex: Integer;
  DefaultInjIndex: Integer;
  InjItem: TCtsInjectionTimeItem;
  Treatment: TInjectionOptionItem;
  ColIndex: Integer;
  WellIndex: Integer;
  WellItem: TIndividualWellInjectionItem;
  SystemItem: TCtsSystemItem;
begin
  inherited;
  if FSelectedSystem <> nil then
  begin
    tvIndividualObjectOptionsChange(nil, nil);

    FSelectedSystem.Name := edSystemName.Text;

    FSelectedSystem.CtsObjects.Count := frameWells.seNumber.AsInteger;
    for ExtractionIndex := 0 to FSelectedSystem.CtsObjects.Count - 1 do
    begin
      CtsObject := FSelectedSystem.CtsObjects[ExtractionIndex];
      CtsObject.StartTime :=
        frameWells.Grid.RealValue[Ord(cwcStartTime), ExtractionIndex+1];
      CtsObject.EndTime :=
        frameWells.Grid.RealValue[Ord(cwcEndTime), ExtractionIndex+1];
      CtsObject.ExtractionWellObjects.DelimitedText :=
        frameWells.Grid.Cells[Ord(cwcExtractionWells), ExtractionIndex+1];
      CtsObject.InjectionWellObjects.DelimitedText :=
        frameWells.Grid.Cells[Ord(cwcInjectionWells), ExtractionIndex+1];
    end;

    FSelectedSystem.ExternalFlows.Count := frameExternalFlows.seNumber.AsInteger;
    for ExternalFlowIndex := 0 to FSelectedSystem.ExternalFlows.Count - 1 do
    begin
      ExternalFlowsItem := FSelectedSystem.ExternalFlows[ExternalFlowIndex];
      ExternalFlowsItem.StartTime :=
        frameWells.Grid.RealValue[Ord(cefcStartTime), ExternalFlowIndex+1];
      ExternalFlowsItem.EndTime :=
        frameWells.Grid.RealValue[Ord(cefcEndTime), ExternalFlowIndex+1];
      ExternalFlowsItem.Outflow :=
        frameWells.Grid.Cells[Ord(cstOutflow), ExternalFlowIndex+1];
      ExternalFlowsItem.Inflow :=
        frameWells.Grid.Cells[Ord(cstInflow), ExternalFlowIndex+1];
      While ExternalFlowsItem.InflowConcentrations.Count < NCOMP do
      begin
        ExternalFlowsItem.InflowConcentrations.Add.Value := '0';
      end;
      for CompIndex := 0 to NCOMP - 1 do
      begin
        ExternalFlowsItem.InflowConcentrations[CompIndex].Value :=
          frameWells.Grid.Cells[Ord(cstInflow) + CompIndex, ExternalFlowIndex+1];
      end;
    end;

    FSelectedSystem.TreatmentDistribution :=
      TTreatmentDistribution(comboTreatmentOption.ItemIndex);

    FSelectedSystem.DefaultInjectionOptions.Count := frameDefaultOptions.seNumber.AsInteger;

    for DefaultInjIndex := 0 to FSelectedSystem.DefaultInjectionOptions.Count - 1 do
    begin
      InjItem := FSelectedSystem.DefaultInjectionOptions[DefaultInjIndex];
      InjItem.StartTime :=
        frameDefaultOptions.Grid.RealValue[Ord(iocStartTime), DefaultInjIndex+1];
      InjItem.EndTime :=
        frameDefaultOptions.Grid.RealValue[Ord(iocEndTime), DefaultInjIndex+1];
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];

        Treatment.TreatmentOption :=
          TTreatmentOption(frameDefaultOptions.Grid.ItemIndex[
          ColIndex, DefaultInjIndex+1]);
        Inc(ColIndex);

        Treatment.Value := frameDefaultOptions.Grid.Cells[
          ColIndex, DefaultInjIndex+1];
        Inc(ColIndex);
      end;
    end;

  end;
  if Node <> nil then
  begin
    SystemItem := Node.Data;
    if SystemItem <> nil then
    begin
      FSelectedSystem := SystemItem.CtsSystem;
    end
    else
    begin
      FSelectedSystem := nil;
    end;
  end
  else
  begin
    FSelectedSystem := nil;
  end;
  if FSelectedSystem <> nil then
  begin
    edSystemName.Text := FSelectedSystem.Name;

    frameWells.seNumber.AsInteger := FSelectedSystem.CtsObjects.Count;
    for ExtractionIndex := 0 to FSelectedSystem.CtsObjects.Count - 1 do
    begin
      CtsObject := FSelectedSystem.CtsObjects[ExtractionIndex];
      frameWells.Grid.RealValue[Ord(cwcStartTime), ExtractionIndex+1] :=
        CtsObject.StartTime;
      frameWells.Grid.RealValue[Ord(cwcEndTime), ExtractionIndex+1] :=
        CtsObject.EndTime;
      frameWells.Grid.Cells[Ord(cwcExtractionWells), ExtractionIndex+1] :=
        CtsObject.ExtractionWellObjects.DelimitedText;
      frameWells.Grid.Cells[Ord(cwcInjectionWells), ExtractionIndex+1] :=
        CtsObject.InjectionWellObjects.DelimitedText;
    end;

    frameExternalFlows.seNumber.AsInteger := FSelectedSystem.ExternalFlows.Count;
    for ExternalFlowIndex := 0 to FSelectedSystem.ExternalFlows.Count - 1 do
    begin
      ExternalFlowsItem := FSelectedSystem.ExternalFlows[ExternalFlowIndex];
      frameExternalFlows.Grid.RealValue[Ord(cefcStartTime), ExternalFlowIndex+1] :=
        ExternalFlowsItem.StartTime;
      frameExternalFlows.Grid.RealValue[Ord(cefcEndTime), ExternalFlowIndex+1] :=
        ExternalFlowsItem.EndTime;
      frameExternalFlows.Grid.Cells[Ord(cstOutflow), ExternalFlowIndex+1] :=
        ExternalFlowsItem.Outflow;
      frameExternalFlows.Grid.Cells[Ord(cstInflow), ExternalFlowIndex+1] :=
        ExternalFlowsItem.Inflow;
      While ExternalFlowsItem.InflowConcentrations.Count < NCOMP do
      begin
        ExternalFlowsItem.InflowConcentrations.Add.Value := '0';
      end;
      for CompIndex := 0 to NCOMP - 1 do
      begin
        frameExternalFlows.Grid.Cells[Ord(cstInflow) + CompIndex, ExternalFlowIndex+1] :=
          ExternalFlowsItem.InflowConcentrations[CompIndex].Value;
      end;
    end;

    comboTreatmentOption.ItemIndex := Ord(FSelectedSystem.TreatmentDistribution);

    frameDefaultOptions.seNumber.AsInteger :=
      FSelectedSystem.DefaultInjectionOptions.Count;

    for DefaultInjIndex := 0 to FSelectedSystem.DefaultInjectionOptions.Count - 1 do
    begin
      InjItem := FSelectedSystem.DefaultInjectionOptions[DefaultInjIndex];
      frameDefaultOptions.Grid.RealValue[Ord(iocStartTime), DefaultInjIndex+1] :=
        InjItem.StartTime;
      frameDefaultOptions.Grid.RealValue[Ord(iocEndTime), DefaultInjIndex+1] :=
        InjItem.EndTime;
      While InjItem.InjectionOptions.Count < NCOMP do
      begin
        Treatment := InjItem.InjectionOptions.Add;
        Treatment.Value := '0';
        Treatment.TreatmentOption := toPercentage;
      end;
      ColIndex := Ord(iocTreament1);
      for CompIndex := 0 to NCOMP - 1 do
      begin
        Treatment := InjItem.InjectionOptions[CompIndex];
        frameDefaultOptions.Grid.ItemIndex[ColIndex, DefaultInjIndex+1] :=
          Ord(Treatment.TreatmentOption);
        Inc(ColIndex);
        frameDefaultOptions.Grid.Cells[ColIndex, DefaultInjIndex+1] :=
          Treatment.Value;
        Inc(ColIndex);
      end;
    end;

    tvIndividualObjectOptions.Items.Clear;
    for WellIndex := 0 to FSelectedSystem.Injections.Count - 1 do
    begin
      WellItem := FSelectedSystem.Injections[WellIndex];
      tvIndividualObjectOptions.Items.AddObject(nil,
        WellItem.InjectionWellObjectName, WellItem);
    end;

  end;
end;

{ TUndoEditCTS }

constructor TUndoEditCTS.Create(var NewCts: TCtsSystemCollection);
begin
  FOldCTS := TCtsSystemCollection.Create(nil);
  FOldCTS.Assign(frmGoPhast.PhastModel.CtsSystems);
  FNewCts := NewCts;
  NewCts := nil;
end;

function TUndoEditCTS.Description: string;
begin
  result := 'change MT3D-USGS Contaminant Treatment System';
end;

destructor TUndoEditCTS.Destroy;
begin
  FOldCTS.Free;
  FNewCts.Free;
  inherited;
end;

procedure TUndoEditCTS.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.CtsSystems := FNewCts;
end;

procedure TUndoEditCTS.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.CtsSystems := FOldCTS;
end;

end.
