unit frmManageSutraBoundaryObservationsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  JvExStdCtrls, JvListBox, JvCombobox, JvListComb, ArgusDataEntry, Vcl.Mask,
  JvExMask, JvSpin, Vcl.Grids, RbwDataGrid4, JvExComCtrls, JvComCtrls, JvEdit,
  Vcl.ComCtrls, RbwParser, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Menus, JvExExtCtrls,
  JvNetscapeSplitter, framePestObsUnit, SutraPestObsUnit, ScreenObjectUnit;

type
  TfrmManageSutraBoundaryObservations = class(TfrmCustomGoPhast)
    spltr1: TJvNetscapeSplitter;
    pmSelectEditAvailable: TPopupMenu;
    miSelectAvailable: TMenuItem;
    miEditAvailable: TMenuItem;
    miGotoAvailable: TMenuItem;
    miHideAvailable: TMenuItem;
    pmSelectEditUsed: TPopupMenu;
    miSelectUsed: TMenuItem;
    miEditUsed: TMenuItem;
    miGoToUsed: TMenuItem;
    miHideUsed: TMenuItem;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnCancel: TBitBtn;
    btnOk: TBitBtn;
    btnDeleteObservation: TButton;
    btnAddObservation: TButton;
    rparserThreeDFormulaElements: TRbwParser;
    tvFluxObservations: TTreeView;
    pnlMain: TPanel;
    pnlTop: TPanel;
    lblObservationName: TLabel;
    lblTreatment: TLabel;
    edObservationName: TJvEdit;
    comboTreatment: TComboBox;
    pcMain: TJvPageControl;
    tabObservationsTimes: TTabSheet;
    tabObjects: TTabSheet;
    lblSrcLabel: TLabel;
    lblDstLabel: TLabel;
    lblFactor: TLabel;
    lbSrcList: TJvListBox;
    btnIncBtn: TButton;
    btnIncAllBtn: TButton;
    btnExclBtn: TButton;
    btnExclAllBtn: TButton;
    lbDstList: TJvListBox;
    edFactorFormula: TJvEdit;
    btnFactorFormula: TButton;
    frameSutraFluxObs: TframePestObs;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FFluxObs: TSutraFluxObs;
    FFluidFluxObjects: TScreenObjectList;
    FUFluxObjects: TScreenObjectList;
    FSelectedObservation: TCustomSutraFluxObservations;
    procedure GetData;
    procedure SetData;
    procedure SetSelectedObservation(const Value: TCustomSutraFluxObservations);
    property SelectedObservation: TCustomSutraFluxObservations read FSelectedObservation write SetSelectedObservation;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmManageSutraBoundaryObservations: TfrmManageSutraBoundaryObservations;

implementation

uses
  frmGoPhastUnit, SutraBoundariesUnit;

{$R *.dfm}

procedure TfrmManageSutraBoundaryObservations.FormCreate(Sender: TObject);
begin
  inherited;
  FFluxObs := TSutraFluxObs.Create(nil);
  FFluidFluxObjects := TScreenObjectList.Create;
  FUFluxObjects := TScreenObjectList.Create;
  frameSutraFluxObs.InitializeControls;
end;

procedure TfrmManageSutraBoundaryObservations.FormDestroy(Sender: TObject);
begin
  inherited;
  FFluxObs.Free;
  FUFluxObjects.Free;
  FFluidFluxObjects.Free;
end;

procedure TfrmManageSutraBoundaryObservations.GetData;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  FluidFlux: Boolean;
  UFlux: Boolean;
  SutraBoundaries: TSutraBoundaries;
  ParentNode: TTreeNode;
  FlowItem: TSutraFlFluxObservationGroup;
  ANode: TTreeNode;
  UItem: TSutraUFluxObservationGroup;
begin
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;

    FluidFlux := False;
    UFlux := False;

    SutraBoundaries := ScreenObject.SutraBoundaries;
    if SutraBoundaries.SpecifiedPressure.Used then
    begin
      FluidFlux := True;
      UFlux := True;
    end
    else if SutraBoundaries.FluidSource.Used then
    begin
      UFlux := True;
    end
    else if SutraBoundaries.SpecifiedConcTemp.Used then
    begin
      UFlux := True;
    end
    else if SutraBoundaries.GeneralFlowBoundary.Used then
    begin
      FluidFlux := True;
      UFlux := True;
    end
    else if SutraBoundaries.GenTransportBoundary.Used then
    begin
      UFlux := True;
    end;

    if FluidFlux then
    begin
      FFluidFluxObjects.Add(ScreenObject);
    end;
    if UFlux then
    begin
      FUFluxObjects.Add(ScreenObject);
    end;
  end;

  FFluxObs.Assign(frmGoPhast.PhastModel.SutraFluxObs);

  ParentNode := tvFluxObservations.Items.Add(nil, 'Calculated fluid flow boundaries');
  ParentNode.Data := FFluxObs.FluidFlux;
  for Index := 0 to FFluxObs.FluidFlux.Count - 1 do
  begin
    FlowItem := FFluxObs.FluidFlux[Index];
    ANode := tvFluxObservations.Items.AddChild(ParentNode,
      FlowItem.ObsGroup.ObservationName);
    ANode.Data := FlowItem.ObsGroup;
  end;

  ParentNode := tvFluxObservations.Items.Add(nil, 'Calculated U rate boundaries');
  ParentNode.Data := FFluxObs.UFlux;
  for Index := 0 to FFluxObs.UFlux.Count - 1 do
  begin
    UItem := FFluxObs.UFlux[Index];
    ANode := tvFluxObservations.Items.AddChild(ParentNode,
      UItem.ObsGroup.ObservationName);
    ANode.Data := UItem.ObsGroup;
  end;

end;

procedure TfrmManageSutraBoundaryObservations.SetData;
begin

end;

procedure TfrmManageSutraBoundaryObservations.SetSelectedObservation(
  const Value: TCustomSutraFluxObservations);
begin
  FSelectedObservation := Value;
end;

end.
