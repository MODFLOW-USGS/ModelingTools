unit frmCustomSelectObjectsUnit;

interface

uses
  System.Types, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Contnrs, frmCustomGoPhastUnit, VirtualTrees, StdCtrls, Buttons,
  ExtCtrls, ScreenObjectUnit;

type
  TVisibleGroupState = (vgsUndefined, vgsUnChecked, vgsChecked, vgs3State);

  // @name is a record we use to store data in the nodes.
  // Since the application is responsible to manage all data including the node's caption
  // this record can be considered as minimal requirement in all VT applications.
  // Extend it to whatever your application needs.
  PMyRec = ^TMyRec;
  TMyRec = record
    Caption: String;
    Classification: String;
    ScreenObjects: TList;
    GroupState: TVisibleGroupState;
    IsDataSetNode: boolean;
  end;

  TfrmCustomSelectObjects = class(TfrmCustomGoPhast)
    // @name is the panel at the bottom of the form that holds the buttons.
    pnlBottom: TPanel;
    // @name is used to close the form.
    btnClose: TBitBtn;
    // @name is used to display help for the form.
    btnHelp: TBitBtn;
    // in @link(TfrmShowHideObjects) @name is used
    // to display and change the visibility of the
    // @link(TScreenObject)s.  The user can also select or edit objects
    // with it. The data are held in a TMyRec record associated with the
    // base nodes.
    // @seealso(TfrmShowHideObjects.vstObjectsChecked)
    // @seealso(TfrmShowHideObjects.pmSelectEdit)
    vstObjects: TVirtualStringTree;
    // @name is used to initialize data associated with each node.
    procedure vstObjectsInitNode(Sender: TBaseVirtualTree; ParentNode,
      Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
    // @name is used to free memory associated with each node.
    procedure vstObjectsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
    // @name is used to return the text associated with a node.  This is
    // usually the name of a @link(TScreenObject) along with some additional
    // information.
    procedure vstObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
    // @name is used to enable or disable the activity of
    // @link(TfrmShowHideObjects.vstObjectsChecked) based on whether the check box that is
    // being checked is or is not the own the user originally changed.
    procedure vstObjectsStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
    procedure vstObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
  private
    // See @link(CanEdit).
    FCanEdit: boolean;
    FvstSutraSpecPressureNode: PVirtualNode;
    FSutraSpecPressureList: TList;
    FvstSutraSpecUNode: PVirtualNode;
    FSutraSpecUList: TList;
    FvstSutraFluidFluxNode: PVirtualNode;
    FSutraFluidFluxList: TList;
    FvstSutraUFluxNode: PVirtualNode;
    FSutraUFluxList: TList;

    FvstSutraLakeNode: PVirtualNode;
    FSutraLakeList: TList;
    FvstSutraGeneralizedFlowNode: PVirtualNode;
    FSutraGeneralizedFlowList: TList;
    FvstSutraGeneralizedTransportNode: PVirtualNode;
    FSutraGeneralizedTransportList: TList;

    FvstSutraStateObsNode: PVirtualNode;
    FSutraStateObsList: TList;

    FvstModflowSubObsNode: PVirtualNode;
    FModflowSubObsList: TList;
    FvstModflowSwtObsNode: PVirtualNode;
    FModflowSwtObsList: TList;

    FvstFootprintWellNode: PVirtualNode;
    FFootprintList: TList;
    FvstFootprintFeaturesNode: PVirtualNode;
    FSwiObsList: TList;
    FExapandedNodes: TStringList;
    FRefinementList: TList;
    FvstObsMf6Node: PVirtualNode;
    FvstCalibrationObsMf6Node: PVirtualNode;

//    FObs6List: TList;

    FvstHeadObsMf6Node: PVirtualNode;
    FHeadObs6List: TList;
    FCalibrationObs6List: TList;
    FvstDrawDownObsMf6Node: PVirtualNode;
    FDrawDownObs6List: TList;
    FvstChdObsMf6Node: PVirtualNode;
    FChdObs6List: TList;
    FvstDrnObsMf6Node: PVirtualNode;
    FDrnObs6List: TList;
    FvstEvtObsMf6Node: PVirtualNode;
    FEvtObs6List: TList;
    FvstGhbObsMf6Node: PVirtualNode;
    FGhbObs6List: TList;
    FvstRchObsMf6Node: PVirtualNode;
    FRchObs6List: TList;
    FvstRivObsMf6Node: PVirtualNode;
    FRivObs6List: TList;
    FvstWelObsMf6Node: PVirtualNode;
    FWelObs6List: TList;
    FvstToMvrObsMf6Node: PVirtualNode;
    FToMvrObs6List: TList;
    FvstLakObsMf6Node: PVirtualNode;
    FLakObs6List: TList;
    FvstMawObsMf6Node: PVirtualNode;
    FMawObs6List: TList;
    FvstSfrObsMf6Node: PVirtualNode;
    FSfrObs6List: TList;

    FvstLktObsMf6Node: PVirtualNode;
    FLktObs6List: TList;
    FvstSftObsMf6Node: PVirtualNode;
    FSftObs6List: TList;
    FvstMwtObsMf6Node: PVirtualNode;
    FMwtObs6List: TList;
    FvstUztObsMf6Node: PVirtualNode;
    FUztObs6List: TList;

    FvstUzfObsMf6Node: PVirtualNode;
    FUzfObs6List: TList;
    FvstCsubObsMf6Node: PVirtualNode;
    FCsubObs6List: TList;

    FvstGwtConcObsMf6Node: PVirtualNode;
    FGwtConcObs6List: TList;
    FvstGwtCncObsMf6Node: PVirtualNode;
    FGwtCncObs6List: TList;
    FvstGwtSrcObsMf6Node: PVirtualNode;
    FGwtSrcObs6List: TList;

    procedure RecordExpandedNodes;
    procedure RestoreExpandedNodes;

    { Private declarations }

  { Public declarations }
  protected
    FSettingData2: boolean;
    FvstChildModelNode: PVirtualNode;
    FvstModpathRoot: PVirtualNode;
    FvstModflowHfbNode: PVirtualNode;
    FvstModflowHobNode: PVirtualNode;
    FvstModflowGagNode: PVirtualNode;
    FvstModflowSfrNode: PVirtualNode;
    FvstModflowSfrMF6Node: PVirtualNode;
    FvstModflowStrNode: PVirtualNode;
    FvstModflowResNode: PVirtualNode;
    FvstModflowEtsNode: PVirtualNode;
    FvstModflowEvtNode: PVirtualNode;
    FvstModflowRchNode: PVirtualNode;
    FvstModflowDrtNode: PVirtualNode;
    FvstModflowDrtReturnLocation: PVirtualNode;
    FvstModflowDrnNode: PVirtualNode;
    FvstModflowRivNode: PVirtualNode;
    FvstModflowWellNode: PVirtualNode;
    FvstModflowLakNode: PVirtualNode;
    FvstModflowMf6LakNode: PVirtualNode;
    FvstModflowMnw1Node: PVirtualNode;
    FvstModflowMnw2Node: PVirtualNode;
    FvstModflowMawNode: PVirtualNode;
    FvstModflowGhbNode: PVirtualNode;
    FvstModflowMvrNode: PVirtualNode;
    FvstModflowCSubNode: PVirtualNode;


    FvstModflowFhbHeadNode: PVirtualNode;
    FvstModflowFhbFlowNode: PVirtualNode;
    FvstModflowFarmNode: PVirtualNode;
    FvstModflowFarmWellNode: PVirtualNode;
    FvstModflowFarmPrecipNode: PVirtualNode;
    FvstModflowFarmRefEvapNode: PVirtualNode;
    FvstModflowFarmCropIDNode: PVirtualNode;

    FvstModflowCfpRechargeNode: PVirtualNode;

    FvstModflowSwrReachNode: PVirtualNode;
    FvstModflowSwrRainNode: PVirtualNode;
    FvstModflowSwrEvapNode: PVirtualNode;
    FvstModflowSwrInflowNode: PVirtualNode;
    FvstModflowSwrStageNode: PVirtualNode;
    FvstModflowSwrDirectRunoffNode: PVirtualNode;

    FvstModflowUzfNode: PVirtualNode;
    FvstModflowUzfMf6Node: PVirtualNode;
    FvstModflowGwtCncNode: PVirtualNode;
    FvstModflowGwtSrcNode: PVirtualNode;

    FvstModflowRipNode: PVirtualNode;

    FvstMt3dmsSsm: PVirtualNode;
    FvstMt3dmsTob: PVirtualNode;
    FvstMt3dUztRech: PVirtualNode;
    FvstMt3dUztSat: PVirtualNode;
    FvstMt3dUztUnsat: PVirtualNode;
    FvstMt3dSeepage: PVirtualNode;
    FvstMt3dSft: PVirtualNode;
    FvstMt3dLkt: PVirtualNode;

    FvstSutraFeaturesNode: PVirtualNode;
    FvstSutraObsNode: PVirtualNode;

    FvstModflowHydmodNode: PVirtualNode;
    FvstModflowSwiObsNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST specified head boundary.
    FvstSpecifiedHeadNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST specified flux boundary.
    FvstSpecifiedFluxNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST well boundary.
    FvstWellNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a MODFLOW CHD boundary.
    FvstModflowChdNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies the grid element size.
    FvstSizeNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies the quadtree refinement level.
    FvstRefinementNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST river boundary.
    FvstRiverNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that isn't used for anything.
    FvstOtherObjectsNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject)
    // that specifies a PHAST leaky boundary.
    FvstLeakyNode: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TDataArray).
    FvstDataSetRootNode: PVirtualNode;
    // @name holds a PVirtualNode for each MODFLOW boundary condition.
    FvstModflowBoundaryConditionsRoot: PVirtualNode;
    // @name holds a PVirtualNode for each PHAST boundary condition.
    FvstPhastBoundaryConditionsRoot: PVirtualNode;
    // @name holds a PVirtualNode for each @link(TScreenObject).
    FvstAllObjectsNode: PVirtualNode;
    // @name holds lists of
    // @link(TScreenObject)s that set particular
    // @link(TDataArray)s.
    FDataSetLists: TObjectList;
    // @name is used to enable or disable the action of
    // @link(TfrmShowHideObjects.vstObjectsChecked).  @SeeAlso(vstObjectsStateChange)
    FOkToDoCheck: boolean;
    // @name is used in @link(UpdateScreenObjects), @link(vstObjectsInitNode),
    //  and @link(TfrmShowHideObjects.vstObjectsChecked)
    // to prevent stack overflows.
    FSettingData: boolean;
    FSettingData3: boolean;
    FDrtList: TList;
    FDrtReturnList: TList;
    FHobList: TList;
    FDrnList: TList;
    FCSubList: TList;
    FRivList: TList;
    FHydmodList: TList;
    // @name holds a list of all the @link(TScreenObject)s.
    FAllObjectsList: TList;
    FLakList: TList;
    FLakMf6List: TList;
    FMnw1List: TList;
    FMnw2List: TList;
    FMawList: TList;
    FMvrList: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW GHB
    // boundary conditions.
    FGhbList: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW FHB Head
    // boundary conditions.
    FFhbHeadList: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW FHB Flow
    // boundary conditions.
    FFhbFlowList: TList;
    // @name holds the lists of @link(TScreenObject)s that set MODFLOW Chd
    // boundary condtions.
    FChdList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Well
    // boundary conditions.
    FWellList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Specified Head
    // boundary conditions.
    FSpecifiedHeadList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Specified Flux
    // boundary conditions.
    FSpecifiedFluxList: TList;
    // @name holds the lists of @link(TScreenObject)s that set the size
    // of elements in the grid.
    FSizeList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST River
    // boundary conditions.
    FRiverList: TList;
    FMfWellList: TList;
    FModpathList: TList;
    FChildModelList: TList;
    FUzfList: TList;
    FUzfMf6List: TList;
    FGwtCncList: TList;
    FGwtSrcList: TList;
    // @name holds the lists of @link(TScreenObject)s that set PHAST Leaky
    // boundary conditions.
    FLeakyList: TList;
    FHfbList: TList;
    FSsmList: TList;
    FSftList: TList;
    FLktList: TList;
    FTobList: TList;
    FUztRechList: TList;
    FUztSatList: TList;
    FUztUnsatList: TList;
    FSfrGagList: TList;
    FUztSeepageList: TList;
    FSfrList: TList;
    FSfrMf6List: TList;
    FStrList: TList;
    FResList: TList;
    FEtsList: TList;
    FEvtList: TList;
    FFarmList: TList;

    FSwrReachList: TList;
    FSwrRainList: TList;
    FSwrEvapList: TList;
    FSwrInflowList: TList;
    FSwrStageList: TList;
    FSwrDirectRunoffList: TList;

    FFarmWellList: TList;
    FFarmPrecipList: TList;
    FFarmRefEvtList: TList;
    FFarmCropIDList: TList;
    FSutraObsList: TList;
    FCfpRechargeList: TList;

    FRipList: TList;

    // @name holds the lists of @link(TScreenObject)s that don't do anything.
    FOtherObjectsList: TList;
    FRchList: TList;
    procedure SetCheckStates;
    function ShouldCheckBoxBeChecked(ScreenObject: TScreenObject): boolean;
      virtual; abstract;
    // @name adds one or more PVirtualNodes to @link(vstObjects)
    // for @link(TScreenObject AScreenObject).
    // It skips @link(TScreenObject TScreenObjects) that are deleted.
    procedure AddScreenObject(AScreenObject: TScreenObject;
      const DataSetList: TStringList);
    // @name removes all base nodes that have no children.
    procedure ClearEmptyBaseNodes(DataSetList: TStringList);
    // @name creates all the basal nodes of @link(vstObjects)
    // that will hold the nodes
    // that have data about the @link(TScreenObject TScreenObjects).
    // @name checks that the basal nodes don't already exist before creating
    // them.
    procedure CreateBaseNodes(DataSetList: TStringList);
    // @name sets @link(FvstAllObjectsNode),
    // @link(FvstSizeNode),
    // @link(FvstDataSetRootNode),
    // @link(FvstPhastBoundaryConditionsRoot),
    // @link(FvstModflowBoundaryConditionsRoot),
    // @link(FvstSpecifiedHeadNode),
    // @link(FvstSpecifiedFluxNode),
    // @link(FvstLeakyNode),
    // @link(FvstRiverNode),
    // @link(FvstWellNode), and
    // @link(FvstOtherObjectsNode), etc. to nil.
    procedure NilBaseNodes;
    procedure HandleChecked(AScreenObject: TScreenObject); virtual; abstract;
    procedure HandleUnchecked(AScreenObject: TScreenObject); virtual; abstract;
    procedure HandleChildren(Sender: TBaseVirtualTree; Node: PVirtualNode);
    procedure SetCanEdit(const Value: boolean); virtual;
    // @name sorts the lists of @link(TScreenObject)s in a modified
    // alphabetical order.  In the modified order 'Object2" comes
    // before 'Object10' instead of after it.
    procedure SortScreenObjectLists;
    // @name sets whether the Node is checked, unchecked or mixed.
    procedure SetRootNodeStates(Node: PVirtualNode);
    procedure SetFirstNodeCheckState(var ChildData: PMyRec;
      ScreenObject: TScreenObject);
    procedure SetSubsequentNodesCheckState(var StateChanged: Boolean;
      ScreenObject: TScreenObject; var ChildData: PMyRec);
    // @name makes sure that the checked state of each node
    // reflects whether or not the associated @link(TScreenObject)
    // is visible or not.  Several nodes can be associated with a
    // single @link(TScreenObject) so TVirtualStringTree's check
    // propogation method can't completely handle this situation.
    procedure UpdateChildCheck(Node: PVirtualNode);
    procedure HandleCheckChange(Node: PVirtualNode;
      Sender: TBaseVirtualTree); virtual;
    // @name creates all the nodes in @link(vstObjects)
    procedure GetData;
    function CanSelect(ScreenObject: TScreenObject): boolean; virtual;
    function NodeString(ANode: PVirtualNode): string;
  public
    // @name is used to prevent access violations caused by attempting to
    // edit a @link(TScreenObject) while another one is already being edited.
    property CanEdit: boolean read FCanEdit write SetCanEdit;
    destructor Destroy; override;
    // @name updates the checked state of nodes of @link(vstObjects).
    // without adding or removing nodes.
    procedure UpdateDisplay;
    // @name adds or removes nodes as needed to make @link(vstObjects)
    // reflect the @link(TScreenObject TScreenObjects) that are present.
    procedure UpdateScreenObjects;
  end;

var
  frmCustomSelectObjects: TfrmCustomSelectObjects;

resourcestring
  StrAllObjects = 'All Objects';
  StrSetGridCellSize = 'Set Grid Cell Size';
  StrSetGridElementSize = 'Set Grid Element Size';
  StrSetMeshElementSize = 'Set Mesh Element Size';
  StrDataSets = 'Data Sets';
  StrPhastBoundaryConditions = 'PHAST Boundary Conditions';
  StrSpecifiedHeadPhast = 'Specified Head';
  StrSpecifiedFlux = 'Specified Flux';
  StrLeakyBoundary = 'Leaky Boundary';
  StrRiverBoundary = 'River Boundary';
  StrWell = 'Well';
  StrModflowBoundaryConditions = 'MODFLOW Features';
  StrModflowMt3dFeatures = 'MODFLOW / MT3DMS Features';
  StrUnusedObjects = 'Unused Objects';

implementation

uses StrUtils, ModflowPackagesUnit, ModflowPackageSelectionUnit,
  GoPhastTypes, frmGoPhastUnit, PhastModelUnit, SubscriptionUnit,
  ModflowHfbUnit, ModflowSfrUnit, ModflowEvtUnit, ModflowGageUnit, DataSetUnit,
  ModpathParticleUnit, ModflowUzfUnit, ModflowHobUnit, ModflowRchUnit,
  ModflowEtsUnit, ModflowBoundaryUnit, ClassificationUnit, ModflowDrtUnit,
  SutraOptionsUnit, Modflow6ObsUnit, ModflowSfr6Unit, ModflowLakMf6Unit,
  ModflowMawUnit, ModflowUzfMf6Unit;

resourcestring
  StrSUTRAFeatures = 'SUTRA Features';
  StrChildModels = 'Child Models';
  StrSUTRAObservations = 'SUTRA Observations';
  StrGAGEGagePackage = 'GAGE: Gage package';
  StrReturnLocationFor = 'Return location for ';
  StrSpecifiedPressure = 'Specified Pressure';
  StrSpecifiedHead = 'Specified Head';
  StrSpecifiedConcentrat = 'Specified Concentration';
  StrSpecifiedTemperatur = 'Specified Temperature';
  StrFluidFlux = 'Fluid Flux';
  StrMassFlux = 'Mass Flux';
  StrEnergyFlux = 'Energy Flux';
  StrFarmsIn = 'Farm IDs in %s';
  StrFarmWellsIn = 'Farm Wells in %s';
  StrPrecipInS = 'Precip. in %s';
  StrRefEvapInS = 'Ref. Evap. in %s';
  StrCropIDInS = 'Crop ID in %s';
  StrHeadsInS = 'Heads in %s';
  StrFlowsInS = 'Flows in %s';
  StrConduitRechargeFra = 'CRCH: Conduit Recharge';
  StrReachesInS = 'Reaches in %s';
  StrRainInS = 'Rain in %s';
  StrEvaporationInS = 'Evaporation in %s';
  StrLateralInflowInS = 'Lateral Inflow in %s';
  StrStageInS = 'Stage in %s';
  StrDirectRunoffInS = 'Direct Runoff in %s';
  StrFootprintWell = 'WellFootprint Well';
  StrSWIObservation = 'SWI Observation';
  StrQuadtreeRefinement = 'Quadtree refinement';
  StrWellFootprintFeatur = 'WellFootprint Features';
  StrMODFLOW6Observatio = 'Observations (MODFLOW 6)';
  StrMODFLOW6CalibrationObservatio = 'Calibration Observations (MODFLOW 6)';
  StrRechargeConcentrati = 'Recharge concentration: ';
  StrSaturatedETConcent = 'Saturated ET concentration: ';
  StrUnsaturatedETConce = 'Unsaturated ET concentration: ';
  StrUZFSinkConcentrati = 'UZF sink concentration: ';
  StrSUTRALake = 'SUTRA Lake';
  StrGeneralizedFlow = 'Generalized Flow';
  StrGeneralizedTranspor = 'Generalized Transport';
  StrSUTRAStateObservat = 'SUTRA State Calibration Observations';

{$R *.dfm}
procedure TfrmCustomSelectObjects.vstObjectsInitNode(Sender: TBaseVirtualTree;
  ParentNode, Node: PVirtualNode; var InitialStates: TVirtualNodeInitStates);
var
  Data, ParentData: PMyRec;
  AScreenObject: TScreenObject;
  Packages: TModflowPackages;
  ScreenObjectCount: cardinal;
  SutraOptions: TSutraOptions;
begin
  inherited;
  with Sender do
  begin
    Packages := frmGoPhast.PhastModel.ModflowPackages;
    SutraOptions := frmGoPhast.PhastModel.SutraOptions;
    Data := GetNodeData(Node);
    // Construct a node caption. This event is triggered once for each node but
    // appears asynchronously, which means when the node
    // is displayed not when it is added.

    if Node = FvstAllObjectsNode then
    begin
      Data.Caption := StrAllObjects;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstPhastBoundaryConditionsRoot then
    begin
      Data.Caption := StrPhastBoundaryConditions;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowBoundaryConditionsRoot then
    begin
      if Packages.Mt3dBasic.IsSelected then
      begin
        Data.Caption := StrModflowMt3dFeatures;
      end
      else
      begin
        Data.Caption := StrModflowBoundaryConditions;
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraFeaturesNode then
    begin
      Data.Caption := StrSUTRAFeatures;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstFootprintFeaturesNode then
    begin
      Data.Caption := StrWellFootprintFeatur;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstObsMf6Node then
    begin
      Data.Caption := StrMODFLOW6Observatio;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstCalibrationObsMf6Node then
    begin
      Data.Caption := StrMODFLOW6CalibrationObservatio;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstDataSetRootNode then
    begin
      Data.Caption := StrDataSets;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstLeakyNode then
    begin
      Data.Caption := StrLeakyBoundary;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstOtherObjectsNode then
    begin
      Data.Caption := StrUnusedObjects;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstRiverNode then
    begin
      Data.Caption := StrRiverBoundary;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSizeNode then
    begin
      case frmGoPhast.ModelSelection of
        msUndefined:
          begin
            Assert(False);
          end;
        msPhast:
          begin
            Data.Caption := StrSetGridElementSize;
          end;
        msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
          msModflowFmp, msModflowCfp, msFootPrint, msModflow2015:
          begin
            Data.Caption := StrSetGridCellSize;
          end;
        msSutra22, msSutra30, msSutra40:
          begin
            Data.Caption := StrSetMeshElementSize
          end
        else Assert(False);
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstRefinementNode then
    begin
      Data.Caption := StrQuadtreeRefinement;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSpecifiedFluxNode then
    begin
      Data.Caption := StrSpecifiedFlux;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSpecifiedHeadNode then
    begin
      Data.Caption := StrSpecifiedHeadPhast;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstWellNode then
    begin
      Data.Caption := StrWell;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowChdNode then
    begin
      Data.Caption := Packages.ChdBoundary.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGhbNode then
    begin
      Data.Caption := Packages.GhbBoundary.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFhbHeadNode then
    begin
      Data.Caption := Format(StrHeadsInS, [Packages.FhbPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFhbFlowNode then
    begin
      Data.Caption := Format(StrFlowsInS, [Packages.FhbPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowLakNode then
    begin
      Data.Caption := Packages.LakPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMf6LakNode then
    begin
      Data.Caption := Packages.LakMf6Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMnw1Node then
    begin
      Data.Caption := Packages.Mnw1Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMnw2Node then
    begin
      Data.Caption := Packages.Mnw2Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMawNode then
    begin
      Data.Caption := Packages.MawPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowWellNode then
    begin
      Data.Caption := Packages.WelPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowRivNode then
    begin
      Data.Caption := Packages.RivPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowDrnNode then
    begin
      Data.Caption := Packages.DrnPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dSft then
    begin
      Data.Caption := Packages.Mt3dSft.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dLkt then
    begin
      Data.Caption := Packages.Mt3dLkt.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dmsSsm then
    begin
      Data.Caption := Packages.Mt3dmsSourceSink.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dmsTob then
    begin
      Data.Caption := Packages.Mt3dmsTransObs.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dUztRech then
    begin
      Data.Caption := StrRechargeConcentrati
        + Packages.Mt3dUnsatTransport.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dUztSat then
    begin
      Data.Caption := StrSaturatedETConcent
        + Packages.Mt3dUnsatTransport.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dUztUnsat then
    begin
      Data.Caption := StrUnsaturatedETConce + Packages.Mt3dUnsatTransport.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMt3dSeepage then
    begin
      Data.Caption := StrUZFSinkConcentrati + Packages.Mt3dmsSourceSink.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowDrtNode then
    begin
      Data.Caption := Packages.DrtPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowDrtReturnLocation then
    begin
      Data.Caption := StrReturnLocationFor
        + Packages.DrtPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowRchNode then
    begin
      Data.Caption := Packages.RchPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowEvtNode then
    begin
      Data.Caption := Packages.EvtPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowEtsNode then
    begin
      Data.Caption := Packages.EtsPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowResNode then
    begin
      Data.Caption := Packages.ResPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSfrNode then
    begin
      Data.Caption := Packages.SfrPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSfrMF6Node then
    begin
      Data.Caption := Packages.SfrModflow6Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowStrNode then
    begin
      Data.Caption := Packages.StrPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGagNode then
    begin
      Data.Caption := StrGAGEGagePackage;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowUzfNode then
    begin
      Data.Caption := Packages.UzfPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowUzfMf6Node then
    begin
      Data.Caption := Packages.UzfMf6Package.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGwtCncNode then
    begin
      Data.Caption := Packages.GwtCncPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowGwtSrcNode then
    begin
      Data.Caption := Packages.GwtSrcPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowHobNode then
    begin
      Data.Caption := Packages.HobPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowHfbNode then
    begin
      Data.Caption := Packages.HfbPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end

    else if Node = FvstModflowSwrReachNode then
    begin
      Data.Caption := Format(StrReachesInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwrRainNode then
    begin
      Data.Caption := Format(StrRainInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwrEvapNode then
    begin
      Data.Caption := Format(StrEvaporationInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwrInflowNode then
    begin
      Data.Caption := Format(StrLateralInflowInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwrStageNode then
    begin
      Data.Caption := Format(StrStageInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwrDirectRunoffNode then
    begin
      Data.Caption := Format(StrDirectRunoffInS, [Packages.SwrPackage.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end

    else if Node = FvstModflowFarmNode then
    begin
      Data.Caption := Format(StrFarmsIn, [Packages.FarmProcess.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFarmWellNode then
    begin
      Data.Caption := Format(StrFarmWellsIn, [Packages.FarmProcess.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFarmPrecipNode then
    begin
      Data.Caption := Format(StrPrecipInS, [Packages.FarmProcess.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFarmRefEvapNode then
    begin
      Data.Caption := Format(StrRefEvapInS, [Packages.FarmProcess.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowFarmCropIDNode then
    begin
      Data.Caption := Format(StrCropIDInS, [Packages.FarmProcess.PackageIdentifier]);
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModpathRoot then
    begin
      Data.Caption := Packages.ModPath.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstChildModelNode then
    begin
      Data.Caption := StrChildModels;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowHydmodNode then
    begin
      Data.Caption := Packages.HydmodPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraObsNode then
    begin
      Data.Caption := StrSUTRAObservations;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraSpecPressureNode then
    begin
      case SutraOptions.TransportChoice of
        tcSolute, tcEnergy, tcFreezing: Data.Caption := StrSpecifiedPressure;
        tcSoluteHead: Data.Caption := StrSpecifiedHead;
        else Assert(False);
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraSpecUNode then
    begin
      case SutraOptions.TransportChoice of
        tcSolute, tcSoluteHead: Data.Caption := StrSpecifiedConcentrat;
        tcEnergy, tcFreezing: Data.Caption := StrSpecifiedTemperatur;
        else Assert(False);
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraFluidFluxNode then
    begin
      Data.Caption := StrFluidFlux;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraUFluxNode then
    begin
      case SutraOptions.TransportChoice of
        tcSolute, tcSoluteHead: Data.Caption := StrMassFlux;
        tcEnergy, tcFreezing: Data.Caption := StrEnergyFlux;
        else Assert(False);
      end;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraLakeNode then
    begin
      Data.Caption := StrSUTRALake;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraGeneralizedFlowNode then
    begin
      Data.Caption := StrGeneralizedFlow;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraStateObsNode then
    begin
      Data.Caption := StrSUTRAStateObservat;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSutraGeneralizedTransportNode then
    begin
      Data.Caption := StrGeneralizedTranspor;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowCfpRechargeNode then
    begin
      Data.Caption := StrConduitRechargeFra;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstFootprintWellNode then
    begin
      Data.Caption := StrFootprintWell;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwiObsNode then
    begin
      Data.Caption := StrSWIObservation;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowRipNode then
    begin
      Data.Caption := Packages.RipPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowMvrNode then
    begin
      Data.Caption := Packages.MvrPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowCSubNode then
    begin
      Data.Caption := Packages.CsubPackage.PackageIdentifier;
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSubObsNode then
    begin
      Data.Caption := 'SUB: Subsidence Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstModflowSwtObsNode then
    begin
      Data.Caption := 'SWT: Water-Table Subsidence Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstHeadObsMf6Node then
    begin
      Data.Caption := 'OBS6: Head Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstDrawDownObsMf6Node then
    begin
      Data.Caption := 'OBS6: Drawdown Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstChdObsMf6Node then
    begin
      Data.Caption := 'OBS6: CHD Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstDrnObsMf6Node then
    begin
      Data.Caption := 'OBS6: DRN Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstEvtObsMf6Node then
    begin
      Data.Caption := 'OBS6: EVT Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstGhbObsMf6Node then
    begin
      Data.Caption := 'OBS6: GHB Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstRchObsMf6Node then
    begin
      Data.Caption := 'OBS6: RCH Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstRivObsMf6Node then
    begin
      Data.Caption := 'OBS6: RIV Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstWelObsMf6Node then
    begin
      Data.Caption := 'OBS6: WEL Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstToMvrObsMf6Node then
    begin
      Data.Caption := 'OBS6: To MVR Flow Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstLakObsMf6Node then
    begin
      Data.Caption := 'OBS6: LAK Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMawObsMf6Node then
    begin
      Data.Caption := 'OBS6: MAW Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSfrObsMf6Node then
    begin
      Data.Caption := 'OBS6: SFR Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstUzfObsMf6Node then
    begin
      Data.Caption := 'OBS6: UZF Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstCsubObsMf6Node then
    begin
      Data.Caption := 'OBS6: CSUB Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstGwtConcObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT Concentration Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstGwtCncObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT CNC Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstGwtSrcObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT SRC Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstLktObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT LKT Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstSftObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT SFT Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstMwtObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT MWT Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    else if Node = FvstUztObsMf6Node then
    begin
      Data.Caption := 'OBS6: GWT UZT Observations';
      Node.CheckType := ctTriStateCheckBox;
    end
    ;


    If (ParentNode = nil) then
    begin
      Exit;
    end;

    if not (vsInitialized in ParentNode.States) then
      Sender.IsVisible[ParentNode];

    ParentData := GetNodeData(ParentNode);
    if not Assigned(ParentData) or (ParentData.ScreenObjects = nil) then
    begin
      Exit;
    end;
    Assert(ParentData.ScreenObjects.Count >= 0);
    ScreenObjectCount := ParentData.ScreenObjects.Count;
    if (Node.Index >= ScreenObjectCount) then
    begin
      Exit;
    end;

    AScreenObject := ParentData.ScreenObjects[Node.Index];
    if AScreenObject <> nil then
    begin
      Node.CheckType := ctCheckBox;
      try
        FSettingData2 := True;
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          Sender.CheckState[Node] := csCheckedNormal;
        end
        else
        begin
          Sender.CheckState[Node] := csUncheckedNormal;
        end;
      finally
        FSettingData2 := False;
      end;
    end;

    If Sender.NodeParent[ParentNode] = FvstDataSetRootNode then
    begin
      ParentNode.CheckType := ctTriStateCheckBox;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsFreeNode(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  Data: PMyRec;
begin
  inherited;
  Data := Sender.GetNodeData(Node);
  // Explicitely free the string, the VCL cannot know that there is one but needs to free
  // it nonetheless. For more fields in such a record which must be freed use Finalize(Data^) instead touching
  // every member individually.
  if Assigned(Data) then
  begin
    Data.Caption := '';
    Data.Classification := '';
    if FDataSetLists <> nil then
    begin
      FDataSetLists.Remove(Data.ScreenObjects);
    end;
    Data.ScreenObjects := nil;
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsGetText(Sender: TBaseVirtualTree; Node: PVirtualNode;
      Column: TColumnIndex; TextType: TVSTTextType; var CellText: string);
var
  Data: PMyRec;
  AScreenObject: TScreenObject;
  ParentNode : PVirtualNode;
  Index, DataSetIndex: integer;
  ScreenObjectCount: cardinal;
  DSFormula: string;
  DataArrayManager: TDataArrayManager;
begin
  inherited;
  if csDestroying in ComponentState then
  begin
    CellText := '';
    Exit;
  end;
  // A handler for the OnGetText event is always needed
  // as it provides the tree with the string data to display.
  // Note that we are now using string instead of WideString.
  Data := Sender.GetNodeData(Node);
  if Assigned(Data) then
  begin
    CellText := Data.Caption;
  end;
  Data := Sender.GetNodeData(Sender.NodeParent[Node]);
  if Assigned(Data) and (Data.ScreenObjects <> nil) then
  begin
    Assert(Data.ScreenObjects.Count >= 0);
    ScreenObjectCount := Data.ScreenObjects.Count;
    if Node.Index >= ScreenObjectCount then
    begin
      CellText := 'Error';
      Exit;
    end;
    AScreenObject := Data.ScreenObjects[Node.Index];
    if AScreenObject = nil then
    begin
      CellText := 'Error';
      Exit;
    end;
    CellText := AScreenObject.Name;
    ParentNode := Sender.NodeParent[Node];
    while (ParentNode <> nil) and (Sender.NodeParent[ParentNode]
      <> FvstDataSetRootNode) do
    begin
      ParentNode := Sender.NodeParent[ParentNode];
    end;

    CellText := CellText + ' (' + AScreenObject.Methods;

    if FvstSizeNode = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', ' + FloatToStr(AScreenObject.CellSize);
    end;

    if FvstRefinementNode = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', ' + IntToStr(AScreenObject.QuadtreeRefinementLevel);
    end;

    if FvstModflowHobNode = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', '
        + AScreenObject.ModflowHeadObservations.ObservationName;
    end;

    if FvstObsMf6Node = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', '
        + AScreenObject.Modflow6Obs.Name;
    end;

    if FvstCalibrationObsMf6Node = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', '
        + AScreenObject.Modflow6Obs.Name;
    end;

    if FvstFootprintWellNode = Sender.NodeParent[Node] then
    begin
      CellText := CellText + ', '
        + AScreenObject.FootprintWell.Withdrawal;
    end;

    If (ParentNode <> nil) and (Sender.NodeParent[ParentNode]
      = FvstDataSetRootNode) then
    begin
      DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
      for Index := 0 to DataArrayManager.DataSetCount -1 do
      begin
        if DataArrayManager.DataSets[Index].Name = Data.Caption then
        begin
          DataSetIndex := AScreenObject.
            IndexOfDataSet(DataArrayManager.DataSets[Index]);
          if DataSetIndex >= 0 then
          begin
            DSFormula := AScreenObject.DataSetFormulas[DataSetIndex];
            if Length(DSFormula) > 100 then
            begin
              DSFormula := Copy(DSFormula, 1, 100) + '...';
            end;
            CellText := CellText + '; ' + DSFormula;
            break;
          end;
        end;
      end;
    end;

    CellText := CellText  + ')'
  end;
end;

procedure TfrmCustomSelectObjects.vstObjectsStateChange(Sender: TBaseVirtualTree; Enter,
      Leave: TVirtualTreeStates);
begin
  inherited;
  if tsPainting in Enter then
  begin
    Exit;
  end;
  FOkToDoCheck := not (tsCheckPropagation in Enter);
  FOkToDoCheck := FOkToDoCheck or not (tsCheckPropagation in Leave);
end;

procedure TfrmCustomSelectObjects.vstObjectsChecking(Sender: TBaseVirtualTree; Node: PVirtualNode;
      var NewState: TCheckState; var Allowed: Boolean);
begin
  inherited;
  if not FSettingData and not FSettingData3 and (Node = FvstAllObjectsNode) then
  begin
    if Node.CheckState = csMixedNormal then
    begin
      NewState := csUncheckedNormal;
    end
    else if Node.CheckState = csMixedPressed then
    begin
      NewState := csUncheckedPressed;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.AddScreenObject(AScreenObject: TScreenObject;
      const DataSetList: TStringList);
var
  DataSetIndex: integer;
  Position: integer;
  PutInOtherObjects: boolean;
  vstDataSetNode: PVirtualNode;
  DataArray: TDataArray;
  SftObs: TSftObs;
  LktObs: TLktObs;
  MwtObs: TMwtObs;
  UztObs: TUztObs;
  SpeciesIndex: Integer;
  Procedure InitializeData(Node: PVirtualNode);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    Data.ScreenObjects.Add(AScreenObject);
    PutInOtherObjects := False;
    if ShouldCheckBoxBeChecked(AScreenObject) then
    begin
      case Data.GroupState of
        vgsUndefined:
          begin
            Data.GroupState := vgsChecked;
          end;
        vgsUnChecked:
          begin
            Data.GroupState := vgs3State;
          end;
        vgsChecked:
          begin
            // do nothing
          end;
        vgs3State:
          begin
            // do nothing
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end
    else
    begin
      case Data.GroupState of
        vgsUndefined:
          begin
            Data.GroupState := vgsUnChecked;
          end;
        vgsUnChecked:
          begin
            // do nothing
          end;
        vgsChecked:
          begin
            Data.GroupState := vgs3State;
          end;
        vgs3State:
          begin
            // do nothing
          end;
        else
          begin
            Assert(False);
          end;
      end;
    end;
    case Data.GroupState of
      vgsUndefined:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsUnChecked:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsChecked:
        begin
          vstObjects.CheckState[Node] := csCheckedNormal;
        end;
      vgs3State:
        begin
          vstObjects.CheckState[Node] := csMixedNormal;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  end;
begin
  if AScreenObject.Deleted then
  begin
    Exit;
  end;
  if not CanSelect(AScreenObject) then
  begin
    Exit;
  end;

  vstObjects.BeginUpdate;
  try
    InitializeData(FvstAllObjectsNode);
    PutInOtherObjects := True;
    if AScreenObject.CellSizeUsed then
    begin
      InitializeData(FvstSizeNode);
    end;

    if AScreenObject.QuadtreeRefinementLevel > 0 then
    begin
      InitializeData(FvstRefinementNode);
    end;

    for DataSetIndex := 0 to AScreenObject.DataSetCount - 1 do
    begin
      DataArray := AScreenObject.DataSets[DataSetIndex];
      Position :=
        DataSetList.IndexOf(DataArray.FullClassification + '|' + DataArray.Name);
      if Position < 0 then
      begin
        // The data set has been deleted.
        Continue;
      end;

      vstDataSetNode := PVirtualNode(DataSetList.Objects[Position]);
      InitializeData(vstDataSetNode);
    end;

    case AScreenObject.BoundaryTypeUsed of
      btNone:
        begin
          // do nothing
        end;
      btSpecifiedHead:
        begin
          InitializeData(FvstSpecifiedHeadNode);
        end;
      btFlux:
        begin
          InitializeData(FvstSpecifiedFluxNode);
        end;
      btLeaky:
        begin
          InitializeData(FvstLeakyNode);
        end;
      btRiver:
        begin
          InitializeData(FvstRiverNode);
        end;
      btWell:
        begin
          InitializeData(FvstWellNode);
        end;
    else
      Assert(False);
    end;

    if (AScreenObject.ModflowChdBoundary <> nil)
      and AScreenObject.ModflowChdBoundary.Used then
    begin
      InitializeData(FvstModflowChdNode);
    end;

    if (AScreenObject.ModflowGhbBoundary <> nil)
      and AScreenObject.ModflowGhbBoundary.Used then
    begin
      InitializeData(FvstModflowGhbNode);
    end;

    if (AScreenObject.ModflowFhbHeadBoundary <> nil)
      and AScreenObject.ModflowFhbHeadBoundary.Used then
    begin
      InitializeData(FvstModflowFhbHeadNode);
    end;

    if (AScreenObject.ModflowFhbFlowBoundary <> nil)
      and AScreenObject.ModflowFhbFlowBoundary.Used then
    begin
      InitializeData(FvstModflowFhbFlowNode);
    end;

    if (AScreenObject.ModflowLakBoundary <> nil)
      and AScreenObject.ModflowLakBoundary.Used then
    begin
      InitializeData(FvstModflowLakNode);
    end;

    if (AScreenObject.ModflowLak6 <> nil)
      and AScreenObject.ModflowLak6.Used then
    begin
      InitializeData(FvstModflowMf6LakNode);
    end;

    if (AScreenObject.ModflowMnw1Boundary <> nil)
      and AScreenObject.ModflowMnw1Boundary.Used then
    begin
      InitializeData(FvstModflowMnw1Node);
    end;

    if (AScreenObject.ModflowMnw2Boundary <> nil)
      and AScreenObject.ModflowMnw2Boundary.Used then
    begin
      InitializeData(FvstModflowMnw2Node);
    end;

    if (AScreenObject.ModflowMawBoundary <> nil)
      and AScreenObject.ModflowMawBoundary.Used then
    begin
      InitializeData(FvstModflowMawNode);
    end;

    if (AScreenObject.ModflowWellBoundary <> nil)
      and AScreenObject.ModflowWellBoundary.Used then
    begin
      InitializeData(FvstModflowWellNode);
    end;

    if (AScreenObject.ModflowRivBoundary <> nil)
      and AScreenObject.ModflowRivBoundary.Used then
    begin
      InitializeData(FvstModflowRivNode);
    end;

    if (AScreenObject.ModflowDrnBoundary <> nil)
      and AScreenObject.ModflowDrnBoundary.Used then
    begin
      InitializeData(FvstModflowDrnNode);
    end;

    if (AScreenObject.ModflowDrtBoundary <> nil)
      and AScreenObject.ModflowDrtBoundary.Used then
    begin
      InitializeData(FvstModflowDrtNode);
    end;

    if (AScreenObject.ModflowSwrReaches <> nil)
      and AScreenObject.ModflowSwrReaches.Used then
    begin
      InitializeData(FvstModflowSwrReachNode);
    end;
    if (AScreenObject.ModflowSwrRain <> nil)
      and AScreenObject.ModflowSwrRain.Used then
    begin
      InitializeData(FvstModflowSwrRainNode);
    end;
    if (AScreenObject.ModflowSwrEvap <> nil)
      and AScreenObject.ModflowSwrEvap.Used then
    begin
      InitializeData(FvstModflowSwrEvapNode);
    end;
    if (AScreenObject.ModflowSwrLatInflow <> nil)
      and AScreenObject.ModflowSwrLatInflow.Used then
    begin
      InitializeData(FvstModflowSwrInflowNode);
    end;
    if (AScreenObject.ModflowSwrStage <> nil)
      and AScreenObject.ModflowSwrStage.Used then
    begin
      InitializeData(FvstModflowSwrStageNode);
    end;
    if (AScreenObject.ModflowSwrDirectRunoff <> nil)
      and AScreenObject.ModflowSwrDirectRunoff.Used then
    begin
      InitializeData(FvstModflowSwrDirectRunoffNode);
    end;


    if (AScreenObject.ModflowFmpFarmID <> nil)
      and AScreenObject.ModflowFmpFarmID.Used then
    begin
      InitializeData(FvstModflowFarmNode);
    end;

    if (AScreenObject.ModflowFmpWellBoundary <> nil)
      and AScreenObject.ModflowFmpWellBoundary.Used then
    begin
      InitializeData(FvstModflowFarmWellNode);
    end;

    if (AScreenObject.ModflowFmpPrecip <> nil)
      and AScreenObject.ModflowFmpPrecip.Used then
    begin
      InitializeData(FvstModflowFarmPrecipNode);
    end;

    if (AScreenObject.ModflowFmpRefEvap <> nil)
      and AScreenObject.ModflowFmpRefEvap.Used then
    begin
      InitializeData(FvstModflowFarmRefEvapNode);
    end;

    if (AScreenObject.ModflowFmpCropID <> nil)
      and AScreenObject.ModflowFmpCropID.Used then
    begin
      InitializeData(FvstModflowFarmCropIDNode);
    end;

    if (AScreenObject.ModflowCfpRchFraction <> nil)
      and AScreenObject.ModflowCfpRchFraction.Used then
    begin
      InitializeData(FvstModflowCfpRechargeNode);
    end;

    if AScreenObject.Tag = 1 then
    begin
      InitializeData(FvstModflowDrtReturnLocation);
    end;

    if (AScreenObject.ModflowRchBoundary <> nil)
      and AScreenObject.ModflowRchBoundary.Used then
    begin
      InitializeData(FvstModflowRchNode);
    end;

    if (AScreenObject.ModflowResBoundary <> nil)
      and AScreenObject.ModflowResBoundary.Used then
    begin
      InitializeData(FvstModflowResNode);
    end;

    if (AScreenObject.ModflowEvtBoundary <> nil)
      and AScreenObject.ModflowEvtBoundary.Used then
    begin
      InitializeData(FvstModflowEvtNode);
    end;

    if (AScreenObject.ModflowEtsBoundary <> nil)
      and AScreenObject.ModflowEtsBoundary.Used then
    begin
      InitializeData(FvstModflowEtsNode);
    end;

    if (AScreenObject.ModflowHydmodData <> nil)
      and AScreenObject.ModflowHydmodData.Used then
    begin
      InitializeData(FvstModflowHydmodNode);
    end;

    if (AScreenObject.ModflowSwiObservations <> nil)
      and AScreenObject.ModflowSwiObservations.Used then
    begin
      InitializeData(FvstModflowSwiObsNode);
    end;


    if (AScreenObject.ModflowSfrBoundary <> nil)
      and AScreenObject.ModflowSfrBoundary.Used then
    begin
      InitializeData(FvstModflowSfrNode);
    end;

    if (AScreenObject.ModflowSfr6Boundary <> nil)
      and AScreenObject.ModflowSfr6Boundary.Used then
    begin
      InitializeData(FvstModflowSfrMF6Node);
    end;

    if (AScreenObject.ModflowStrBoundary <> nil)
      and AScreenObject.ModflowStrBoundary.Used then
    begin
      InitializeData(FvstModflowStrNode);
    end;

    if (AScreenObject.ModflowStreamGage <> nil)
      and AScreenObject.ModflowStreamGage.Used then
    begin
      InitializeData(FvstModflowGagNode);
    end;

    if (AScreenObject.ModflowUzfBoundary <> nil)
      and AScreenObject.ModflowUzfBoundary.Used then
    begin
      InitializeData(FvstModflowUzfNode);
    end;

    if (AScreenObject.ModflowUzfMf6Boundary <> nil)
      and AScreenObject.ModflowUzfMf6Boundary.Used then
    begin
      InitializeData(FvstModflowUzfMf6Node);
    end;

    if (AScreenObject.GwtCncBoundary <> nil)
      and AScreenObject.GwtCncBoundary.Used then
    begin
      InitializeData(FvstModflowGwtCncNode);
    end;

    if (AScreenObject.GwtSrcBoundary <> nil)
      and AScreenObject.GwtSrcBoundary.Used then
    begin
      InitializeData(FvstModflowGwtSrcNode);
    end;

    if (AScreenObject.ModflowHeadObservations <> nil)
      and AScreenObject.ModflowHeadObservations.Used then
    begin
      InitializeData(FvstModflowHobNode);
    end;

    if (AScreenObject.Modflow6Obs <> nil)
      and AScreenObject.Modflow6Obs.Used then
    begin
      if AScreenObject.Modflow6Obs.CalibrationObservations.Count > 0 then
      begin
        InitializeData(FvstCalibrationObsMf6Node);
      end;
      if ogHead in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstHeadObsMf6Node);
      end;
      if ogDrawdown in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstDrawDownObsMf6Node);
      end;
      if ogCHD in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstChdObsMf6Node);
      end;
      if ogDrain in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstDrnObsMf6Node);
      end;
      if ogWell in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstWelObsMf6Node);
      end;
      if ogGHB in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstGhbObsMf6Node);
      end;
      if ogRiv in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstRivObsMf6Node);
      end;
      if ogRch in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstRchObsMf6Node);
      end;
      if ogEVT in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstEvtObsMf6Node);
      end;
      if ogMvr in AScreenObject.Modflow6Obs.General then
      begin
        InitializeData(FvstToMvrObsMf6Node);
      end;
      if AScreenObject.Modflow6Obs.LakObs <> [] then
      begin
        InitializeData(FvstLakObsMf6Node);
      end;
      if AScreenObject.Modflow6Obs.MawObs <> [] then
      begin
        InitializeData(FvstMawObsMf6Node);
      end;
      if AScreenObject.Modflow6Obs.SfrObs <> [] then
      begin
        InitializeData(FvstSfrObsMf6Node);
      end;
      if AScreenObject.Modflow6Obs.UzfObs <> [] then
      begin
        InitializeData(FvstUzfObsMf6Node);
      end;
      if AScreenObject.Modflow6Obs.CSubObs.CSubObsSet <> [] then
      begin
        InitializeData(FvstCsubObsMf6Node);
      end;

      if ogwtConcentration in AScreenObject.Modflow6Obs.GwtObs then
      begin
        InitializeData(FvstGwtConcObsMf6Node);
      end;
      if ogwtCNC in AScreenObject.Modflow6Obs.GwtObs then
      begin
        InitializeData(FvstGwtCncObsMf6Node);
      end;
      if ogwtSRC in AScreenObject.Modflow6Obs.GwtObs then
      begin
        InitializeData(FvstGwtSrcObsMf6Node);
      end;

      SftObs := AScreenObject.Modflow6Obs.SftObs;
      for SpeciesIndex := 0 to frmGophast.PhastModel.MobileComponents.Count - 1 do
      begin
        SftObs := SftObs + AScreenObject.Modflow6Obs.CalibrationObservations.SftObs[SpeciesIndex]
      end;
      if (SftObs <> [])  then
      begin
        InitializeData(FvstSftObsMf6Node);
      end;

      LktObs := AScreenObject.Modflow6Obs.LktObs;
      for SpeciesIndex := 0 to frmGophast.PhastModel.MobileComponents.Count - 1 do
      begin
        LktObs := LktObs + AScreenObject.Modflow6Obs.CalibrationObservations.LktObs[SpeciesIndex]
      end;
      if LktObs <> [] then
      begin
        InitializeData(FvstLktObsMf6Node);
      end;

      MwtObs := AScreenObject.Modflow6Obs.MwtObs;
      for SpeciesIndex := 0 to frmGophast.PhastModel.MobileComponents.Count - 1 do
      begin
        MwtObs := MwtObs + AScreenObject.Modflow6Obs.CalibrationObservations.MwtObs[SpeciesIndex]
      end;
      if MwtObs <> [] then
      begin
        InitializeData(FvstMwtObsMf6Node);
      end;

      UztObs := AScreenObject.Modflow6Obs.UztObs;
      for SpeciesIndex := 0 to frmGophast.PhastModel.MobileComponents.Count - 1 do
      begin
        UztObs := UztObs + AScreenObject.Modflow6Obs.CalibrationObservations.UztObs[SpeciesIndex]
      end;
      if UztObs <> [] then
      begin
        InitializeData(FvstUztObsMf6Node);
      end;

    end;

    if (AScreenObject.ModflowHfbBoundary <> nil)
      and (AScreenObject.ModflowHfbBoundary.Used
      or AScreenObject.ModflowHfbBoundary.UsedMf6) then
    begin
      InitializeData(FvstModflowHfbNode);
    end;

    if (AScreenObject.ModflowRipBoundary <> nil)
      and AScreenObject.ModflowRipBoundary.Used then
    begin
      InitializeData(FvstModflowRipNode);
    end;

    if (AScreenObject.ModflowCSub <> nil)
      and AScreenObject.ModflowCSub.Used then
    begin
      InitializeData(FvstModflowCSubNode);
    end;

    if (AScreenObject.ModflowSubObservations <> nil)
      and AScreenObject.ModflowSubObservations.Used then
    begin
      InitializeData(FvstModflowSubObsNode);
    end;

    if (AScreenObject.ModflowSwtObservations <> nil)
      and AScreenObject.ModflowSwtObservations.Used then
    begin
      InitializeData(FvstModflowSwtObsNode);
    end;

    if (AScreenObject.Mt3dmsConcBoundary <> nil)
      and AScreenObject.Mt3dmsConcBoundary.Used then
    begin
      InitializeData(FvstMt3dmsSsm);
    end;

    if (AScreenObject.Mt3dSftConcBoundary <> nil)
      and AScreenObject.Mt3dSftConcBoundary.Used then
    begin
      InitializeData(FvstMt3dSft);
    end;

    if (AScreenObject.Mt3dLktConcBoundary <> nil)
      and AScreenObject.Mt3dLktConcBoundary.Used then
    begin
      InitializeData(FvstMt3dLkt);
    end;

    if (AScreenObject.Mt3dUzfRechConc <> nil)
      and AScreenObject.Mt3dUzfRechConc.Used then
    begin
      InitializeData(FvstMt3dUztRech);
    end;

    if (AScreenObject.Mt3dUztSatEtConcBoundary <> nil)
      and AScreenObject.Mt3dUztSatEtConcBoundary.Used then
    begin
      InitializeData(FvstMt3dUztSat);
    end;

    if (AScreenObject.Mt3dUztUnsatEtConcBoundary <> nil)
      and AScreenObject.Mt3dUztUnsatEtConcBoundary.Used then
    begin
      InitializeData(FvstMt3dUztUnsat);
    end;

    if (AScreenObject.Mt3dUzSsmSinkConcBoundary <> nil)
      and AScreenObject.Mt3dUzSsmSinkConcBoundary.Used then
    begin
      InitializeData(FvstMt3dSeepage);
    end;


    if (AScreenObject.Mt3dmsTransObservations <> nil)
      and AScreenObject.Mt3dmsTransObservations.Used then
    begin
      InitializeData(FvstMt3dmsTob);
    end;

    if (AScreenObject.ModpathParticles <> nil)
      and AScreenObject.ModpathParticles.Used then
    begin
      InitializeData(FvstModpathRoot);
    end;

    if AScreenObject.ChildModel <> nil then
    begin
      InitializeData(FvstChildModelNode);
    end;

    if AScreenObject.SutraBoundaries.Observations.Used then
    begin
      InitializeData(FvstSutraObsNode);
    end;

    if AScreenObject.SutraBoundaries.SpecifiedPressure.Used then
    begin
      InitializeData(FvstSutraSpecPressureNode);
    end;

    if AScreenObject.SutraBoundaries.SpecifiedConcTemp.Used then
    begin
      InitializeData(FvstSutraSpecUNode);
    end;

    if AScreenObject.SutraBoundaries.FluidSource.Used then
    begin
      InitializeData(FvstSutraFluidFluxNode);
    end;

    if AScreenObject.SutraBoundaries.MassEnergySource.Used then
    begin
      InitializeData(FvstSutraUFluxNode);
    end;

    if AScreenObject.SutraBoundaries.Lake.Used then
    begin
      InitializeData(FvstSutraLakeNode);
    end;

    if AScreenObject.SutraBoundaries.SutraStateObs.Used then
    begin
      InitializeData(FvstSutraStateObsNode);
    end;

    if AScreenObject.SutraBoundaries.GeneralFlowBoundary.Used then
    begin
      InitializeData(FvstSutraGeneralizedFlowNode);
    end;

    if AScreenObject.SutraBoundaries.GenTransportBoundary.Used then
    begin
      InitializeData(FvstSutraGeneralizedTransportNode);
    end;

    if (AScreenObject.FootprintWell <> nil)
      and AScreenObject.FootprintWell.Used then
    begin
      InitializeData(FvstFootprintWellNode);
    end;

    if (AScreenObject.ModflowMvr <> nil)
      and AScreenObject.ModflowMvr.Used then
    begin
      InitializeData(FvstModflowMvrNode);
    end;

    if PutInOtherObjects then
    begin
      InitializeData(FvstOtherObjectsNode);
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

function TfrmCustomSelectObjects.CanSelect(
  ScreenObject: TScreenObject): boolean;
begin
  result := True;
end;

procedure TfrmCustomSelectObjects.ClearEmptyBaseNodes(DataSetList: TStringList);
var
  DataSetIndex: integer;
  vstDataSetNode: PVirtualNode;
  GroupNode: PVirtualNode;
  NodeList: TList;
  NodeIndex: Integer;
  ParentNodes: TList;
  Node: PVirtualNode;
  ParentNode: PVirtualNode;
  NodeDeleted: Boolean;
  ChildCount: integer;
  procedure UpdateChildCount(var Node : PVirtualNode);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    If Data.ScreenObjects <> nil then
    begin
      vstObjects.ChildCount[Node] := Data.ScreenObjects.Count;
    end;
  end;
  procedure vstCheckDeleteNode(var Node : PVirtualNode);
  begin
    UpdateChildCount(Node);
    if vstObjects.ChildCount[Node] = 0 then
    begin
      vstObjects.DeleteNode(Node);
      Node := nil;
    end;
  end;
begin
  vstObjects.BeginUpdate;
  try
    UpdateChildCount(FvstAllObjectsNode);
    vstCheckDeleteNode(FvstOtherObjectsNode);
    vstCheckDeleteNode(FvstLeakyNode);
    vstCheckDeleteNode(FvstRiverNode);
    vstCheckDeleteNode(FvstSpecifiedFluxNode);
    vstCheckDeleteNode(FvstSpecifiedHeadNode);
    vstCheckDeleteNode(FvstWellNode);
    vstCheckDeleteNode(FvstPhastBoundaryConditionsRoot);

    vstCheckDeleteNode(FvstModflowGhbNode);
    vstCheckDeleteNode(FvstModflowFhbHeadNode);
    vstCheckDeleteNode(FvstModflowFhbFlowNode);
    vstCheckDeleteNode(FvstModflowMf6LakNode);
    vstCheckDeleteNode(FvstModflowLakNode);
    vstCheckDeleteNode(FvstModflowMnw1Node);
    vstCheckDeleteNode(FvstModflowMnw2Node);
    vstCheckDeleteNode(FvstModflowMawNode);
    vstCheckDeleteNode(FvstModflowChdNode);
    vstCheckDeleteNode(FvstModflowWellNode);
    vstCheckDeleteNode(FvstModflowRivNode);
    vstCheckDeleteNode(FvstModflowDrnNode);
    vstCheckDeleteNode(FvstModflowDrtNode);
    vstCheckDeleteNode(FvstModflowDrtReturnLocation);
    vstCheckDeleteNode(FvstModflowRchNode);
    vstCheckDeleteNode(FvstModflowResNode);
    vstCheckDeleteNode(FvstModflowRipNode);
    vstCheckDeleteNode(FvstModflowEvtNode);
    vstCheckDeleteNode(FvstModflowEtsNode);
    vstCheckDeleteNode(FvstModflowHydmodNode);
    vstCheckDeleteNode(FvstModflowSwiObsNode);
    vstCheckDeleteNode(FvstModflowSfrNode);
    vstCheckDeleteNode(FvstModflowSfrMF6Node);
    vstCheckDeleteNode(FvstModflowStrNode);
    vstCheckDeleteNode(FvstModflowGagNode);
    vstCheckDeleteNode(FvstModflowUzfNode);
    vstCheckDeleteNode(FvstModflowUzfMf6Node);
    vstCheckDeleteNode(FvstModflowGwtCncNode);
    vstCheckDeleteNode(FvstModflowGwtSrcNode);
    vstCheckDeleteNode(FvstModflowHobNode);
    vstCheckDeleteNode(FvstModflowHfbNode);
    vstCheckDeleteNode(FvstModflowMvrNode);
    vstCheckDeleteNode(FvstModflowCSubNode);
    vstCheckDeleteNode(FvstModflowSubObsNode);
    vstCheckDeleteNode(FvstModflowSwtObsNode);



    vstCheckDeleteNode(FvstModflowSwrReachNode);
    vstCheckDeleteNode(FvstModflowSwrRainNode);
    vstCheckDeleteNode(FvstModflowSwrEvapNode);
    vstCheckDeleteNode(FvstModflowSwrInflowNode);
    vstCheckDeleteNode(FvstModflowSwrStageNode);
    vstCheckDeleteNode(FvstModflowSwrDirectRunoffNode);

    vstCheckDeleteNode(FvstModflowFarmNode);
    vstCheckDeleteNode(FvstModflowFarmWellNode);
    vstCheckDeleteNode(FvstModflowFarmPrecipNode);
    vstCheckDeleteNode(FvstModflowFarmRefEvapNode);
    vstCheckDeleteNode(FvstModflowFarmCropIDNode);
    vstCheckDeleteNode(FvstModflowCfpRechargeNode);
    vstCheckDeleteNode(FvstMt3dSft);
    vstCheckDeleteNode(FvstMt3dLkt);
    vstCheckDeleteNode(FvstMt3dmsSsm);
    vstCheckDeleteNode(FvstMt3dmsTob);
    vstCheckDeleteNode(FvstMt3dUztRech);
    vstCheckDeleteNode(FvstMt3dUztSat);
    vstCheckDeleteNode(FvstMt3dUztUnsat);
    vstCheckDeleteNode(FvstMt3dSeepage);

    vstCheckDeleteNode(FvstSutraObsNode);
    vstCheckDeleteNode(FvstSutraSpecPressureNode);
    vstCheckDeleteNode(FvstSutraSpecUNode);
    vstCheckDeleteNode(FvstSutraFluidFluxNode);
    vstCheckDeleteNode(FvstSutraUFluxNode);

    vstCheckDeleteNode(FvstSutraLakeNode);
    vstCheckDeleteNode(FvstSutraGeneralizedFlowNode);
    vstCheckDeleteNode(FvstSutraGeneralizedTransportNode);
    vstCheckDeleteNode(FvstSutraStateObsNode);

    vstCheckDeleteNode(FvstFootprintWellNode);

    vstCheckDeleteNode(FvstModflowBoundaryConditionsRoot);
    vstCheckDeleteNode(FvstModpathRoot);
    vstCheckDeleteNode(FvstChildModelNode);
    vstCheckDeleteNode(FvstSutraFeaturesNode);
    vstCheckDeleteNode(FvstFootprintFeaturesNode);

    vstCheckDeleteNode(FvstHeadObsMf6Node);
    vstCheckDeleteNode(FvstDrawDownObsMf6Node);
    vstCheckDeleteNode(FvstChdObsMf6Node);
    vstCheckDeleteNode(FvstDrnObsMf6Node);
    vstCheckDeleteNode(FvstEvtObsMf6Node);
    vstCheckDeleteNode(FvstGhbObsMf6Node);
    vstCheckDeleteNode(FvstRchObsMf6Node);
    vstCheckDeleteNode(FvstRivObsMf6Node);
    vstCheckDeleteNode(FvstWelObsMf6Node);
    vstCheckDeleteNode(FvstToMvrObsMf6Node);
    vstCheckDeleteNode(FvstLakObsMf6Node);
    vstCheckDeleteNode(FvstMawObsMf6Node);
    vstCheckDeleteNode(FvstSfrObsMf6Node);
    vstCheckDeleteNode(FvstUzfObsMf6Node);
    vstCheckDeleteNode(FvstCsubObsMf6Node);

    vstCheckDeleteNode(FvstGwtConcObsMf6Node);
    vstCheckDeleteNode(FvstGwtCncObsMf6Node);
    vstCheckDeleteNode(FvstGwtSrcObsMf6Node);

    vstCheckDeleteNode(FvstLktObsMf6Node);
    vstCheckDeleteNode(FvstSftObsMf6Node);
    vstCheckDeleteNode(FvstMwtObsMf6Node);
    vstCheckDeleteNode(FvstUztObsMf6Node);

    vstCheckDeleteNode(FvstObsMf6Node);
    vstCheckDeleteNode(FvstCalibrationObsMf6Node);

    ParentNodes := TList.Create;
    try
      for DataSetIndex := DataSetList.Count -1 downto 0 do
      begin
        vstDataSetNode := PVirtualNode(DataSetList.Objects[DataSetIndex]);
        ParentNode := vstObjects.NodeParent[vstDataSetNode];
        UpdateChildCount(vstDataSetNode);
        if vstObjects.ChildCount[vstDataSetNode] = 0 then
        begin
          if ParentNodes.IndexOf(ParentNode) < 0 then
          begin
            ParentNodes.Add(ParentNode);
          end;
          vstObjects.DeleteNode(vstDataSetNode);
          DataSetList.Delete(DataSetIndex);
        end;
      end;

      NodeDeleted := True;
      while NodeDeleted do
      begin
        NodeDeleted := False;
        for NodeIndex := ParentNodes.Count - 1 downto 0 do
        begin
          Node := ParentNodes[NodeIndex];
          if vstObjects.ChildCount[Node] = 0 then
          begin
            ParentNode := vstObjects.NodeParent[Node];
            NodeDeleted := True;
            vstObjects.DeleteNode(Node);
            if (ParentNode <> FvstDataSetRootNode)
              and (ParentNodes.IndexOf(ParentNode) < 0) then
            begin
              ParentNodes.Add(ParentNode);
            end;
            ParentNodes.Delete(NodeIndex);
          end;
        end;
      end;
    finally
      ParentNodes.Free;
    end;

    NodeList := TList.Create;
    try
      ChildCount := vstObjects.ChildCount[FvstDataSetRootNode];
      for NodeIndex := 0 to ChildCount - 1 do
      begin
        if NodeIndex = 0 then
        begin
          GroupNode := vstObjects.GetFirstChild(FvstDataSetRootNode)
        end
        else
        begin
          GroupNode := vstObjects.GetNextSibling(GroupNode);
        end;
        NodeList.Add(GroupNode)
      end;

      for NodeIndex := 0 to NodeList.Count - 1 do
      begin
        GroupNode := NodeList[NodeIndex];
        vstCheckDeleteNode(GroupNode);
      end;
    finally
      NodeList.Free;
    end;

    vstCheckDeleteNode(FvstDataSetRootNode);
    vstCheckDeleteNode(FvstRefinementNode);
    vstCheckDeleteNode(FvstSizeNode);
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.CreateBaseNodes(DataSetList: TStringList);
var
  DataSetIndex: integer;
  ClassificationObject : TDataSetClassification;
  DataSet: TDataArray;
  vstDataSetNode: PVirtualNode;
  Data: PMyRec;
  ListOfObjects: TList;
  DataSetClassifications: TStringList;
  DataSetGroupNode: PVirtualNode;
  ClassificationPosition: integer;
  SortedClassifiedDataSets: TStringList;
  Classification: string;
  ALevelDescription: string;
  SeparatorPosition: integer;
  ParentNode: PVirtualNode;
  TempSeparatorPosition: Integer;
  PriorNode: PVirtualNode;
  ClassificationObjectOwnerList: TList;
  HufDataArrays: TClassificationList;
  ClassificationObjects: TClassificationList;
  LayerGroupList: TClassificationList;
  SutraLayerGroupList: TClassificationList;
  procedure InitializeNodeData(Node: PVirtualNode; List: TList);
  var
    Data: PMyRec;
  begin
    Data := vstObjects.GetNodeData(Node);
    Data.ScreenObjects := List;
    vstObjects.CheckType[Node] := ctTriStateCheckBox;
    Data.GroupState := vgsUndefined;
    Data.IsDataSetNode := False;
    if List <> nil then
    begin
      List.Clear;
    end;
  end;
  procedure FindOrCreateClassificationNode;
  begin
    ClassificationPosition :=
      DataSetClassifications.IndexOf(ALevelDescription);

    if ClassificationPosition < 0 then
    begin
      vstObjects.ChildCount[ParentNode] :=
        vstObjects.ChildCount[ParentNode] + 1;
      DataSetGroupNode := vstObjects.GetLastChild(ParentNode);
      DataSetClassifications.AddObject(
        ALevelDescription, TObject(DataSetGroupNode));
      InitializeNodeData(DataSetGroupNode, nil);
      Data := vstObjects.GetNodeData(DataSetGroupNode);
      Data.Classification := ALevelDescription;
      TempSeparatorPosition := Pos('|', ALevelDescription);
      while TempSeparatorPosition > 0 do
      begin
        ALevelDescription := Copy(ALevelDescription,
          Succ(TempSeparatorPosition), MAXINT);
        TempSeparatorPosition := Pos('|', ALevelDescription);
      end;

      Data.Caption := ALevelDescription;
      ParentNode := DataSetGroupNode;
    end
    else
    begin
      ParentNode := PVirtualNode(DataSetClassifications.
        Objects[ClassificationPosition]);
      DataSetGroupNode := ParentNode;
    end;
  end;
  procedure InitializeMF_BoundaryNode(var BC_Node, PriorNode: PVirtualNode;
    List: TList);
  begin
    if BC_Node = nil then
    begin
      BC_Node := vstObjects.InsertNode(PriorNode, amInsertAfter);
      vstObjects.ReinitNode(BC_Node, False);
    end;
    InitializeNodeData(BC_Node, List);
    PriorNode := BC_Node;
  end;
begin
  vstObjects.BeginUpdate;
  try
    if FvstAllObjectsNode = nil then
    begin
      FvstAllObjectsNode := vstObjects.InsertNode(
        vstObjects.RootNode, amAddChildFirst);
      vstObjects.ReinitNode(FvstAllObjectsNode, False);
    end;
    InitializeNodeData(FvstAllObjectsNode, FAllObjectsList);

    if FvstSizeNode = nil then
    begin
      FvstSizeNode := vstObjects.InsertNode(FvstAllObjectsNode, amInsertAfter);
      vstObjects.ReinitNode(FvstSizeNode, False);
    end;
    InitializeNodeData(FvstSizeNode, FSizeList);

    if FvstRefinementNode = nil then
    begin
      FvstRefinementNode := vstObjects.InsertNode(FvstSizeNode, amInsertAfter);
      vstObjects.ReinitNode(FvstRefinementNode, False);
    end;
    InitializeNodeData(FvstRefinementNode, FRefinementList);

    if FvstDataSetRootNode = nil then
    begin
      FvstDataSetRootNode := vstObjects.InsertNode(FvstRefinementNode, amInsertAfter);
      vstObjects.ReinitNode(FvstDataSetRootNode, False);
    end;
    InitializeNodeData(FvstDataSetRootNode, nil);

    if FvstPhastBoundaryConditionsRoot = nil then
    begin
      FvstPhastBoundaryConditionsRoot := vstObjects.InsertNode(
        FvstDataSetRootNode, amInsertAfter);
      vstObjects.ReinitNode(FvstPhastBoundaryConditionsRoot, False);
    end;
    InitializeNodeData(FvstPhastBoundaryConditionsRoot, nil);

    if FvstModflowBoundaryConditionsRoot = nil then
    begin
      FvstModflowBoundaryConditionsRoot := vstObjects.InsertNode(
        FvstPhastBoundaryConditionsRoot, amInsertAfter);
      vstObjects.ReinitNode(FvstModflowBoundaryConditionsRoot, False);
    end;
    InitializeNodeData(FvstModflowBoundaryConditionsRoot, nil);

    if FvstObsMf6Node = nil then
    begin
      FvstObsMf6Node := vstObjects.InsertNode(
        FvstModflowBoundaryConditionsRoot, amInsertAfter);
      vstObjects.ReinitNode(FvstObsMf6Node, False);
    end;
    InitializeNodeData(FvstObsMf6Node, nil);

    if FvstCalibrationObsMf6Node = nil then
    begin
      FvstCalibrationObsMf6Node := vstObjects.InsertNode(
        FvstObsMf6Node, amInsertAfter);
      vstObjects.ReinitNode(FvstCalibrationObsMf6Node, False);
    end;
    InitializeNodeData(FvstCalibrationObsMf6Node, FCalibrationObs6List);

    if FvstModpathRoot = nil then
    begin
      FvstModpathRoot := vstObjects.InsertNode(
        FvstCalibrationObsMf6Node, amInsertAfter);
      vstObjects.ReinitNode(FvstModpathRoot, False);
    end;
    InitializeNodeData(FvstModpathRoot, FModpathList);

    if FvstChildModelNode = nil then
    begin
      FvstChildModelNode := vstObjects.InsertNode(
        FvstModpathRoot, amInsertAfter);
      vstObjects.ReinitNode(FvstChildModelNode, False);
    end;
    InitializeNodeData(FvstChildModelNode, FChildModelList);

    if FvstSutraFeaturesNode = nil then
    begin
      FvstSutraFeaturesNode := vstObjects.InsertNode(
        FvstChildModelNode, amInsertAfter);
      vstObjects.ReinitNode(FvstSutraFeaturesNode, False);
    end;
    InitializeNodeData(FvstSutraFeaturesNode, nil);

    if FvstFootprintFeaturesNode = nil then
    begin
      FvstFootprintFeaturesNode := vstObjects.InsertNode(
        FvstChildModelNode, amInsertAfter);
      vstObjects.ReinitNode(FvstFootprintFeaturesNode, False);
    end;
    InitializeNodeData(FvstFootprintFeaturesNode, nil);



    if FvstOtherObjectsNode = nil then
    begin
      FvstOtherObjectsNode := vstObjects.InsertNode(
        FvstChildModelNode, amInsertAfter);
      vstObjects.ReinitNode(FvstOtherObjectsNode, False);
    end;
    InitializeNodeData(FvstOtherObjectsNode, FOtherObjectsList);

    // create children of FvstPhastBoundaryConditionsRoot
    if FvstSpecifiedHeadNode = nil then
    begin
      FvstSpecifiedHeadNode := vstObjects.InsertNode(
        FvstPhastBoundaryConditionsRoot, amAddChildFirst);
      vstObjects.ReinitNode(FvstSpecifiedHeadNode, False);
    end;
    InitializeNodeData(FvstSpecifiedHeadNode, FSpecifiedHeadList);

    if FvstSpecifiedFluxNode = nil then
    begin
      FvstSpecifiedFluxNode := vstObjects.InsertNode(
        FvstSpecifiedHeadNode, amInsertAfter);
      vstObjects.ReinitNode(FvstSpecifiedFluxNode, False);
    end;
    InitializeNodeData(FvstSpecifiedFluxNode, FSpecifiedFluxList);

    if FvstLeakyNode = nil then
    begin
      FvstLeakyNode := vstObjects.InsertNode(
        FvstSpecifiedFluxNode, amInsertAfter);
      vstObjects.ReinitNode(FvstLeakyNode, False);
    end;
    InitializeNodeData(FvstLeakyNode, FLeakyList);

    if FvstRiverNode = nil then
    begin
      FvstRiverNode := vstObjects.InsertNode(
        FvstLeakyNode, amInsertAfter);
      vstObjects.ReinitNode(FvstRiverNode, False);
    end;
    InitializeNodeData(FvstRiverNode, FRiverList);

    if FvstWellNode = nil then
    begin
      FvstWellNode := vstObjects.InsertNode(FvstRiverNode, amInsertAfter);
      vstObjects.ReinitNode(FvstWellNode, False);
    end;
    InitializeNodeData(FvstWellNode, FWellList);

    // add children of FvstObsMf6Node
    if FvstHeadObsMf6Node = nil then
    begin
      FvstHeadObsMf6Node := vstObjects.InsertNode(
        FvstObsMf6Node, amAddChildFirst);
      vstObjects.ReinitNode(FvstHeadObsMf6Node, False);
    end;
    InitializeNodeData(FvstHeadObsMf6Node, FHeadObs6List);
    PriorNode := FvstHeadObsMf6Node;

    InitializeMF_BoundaryNode(FvstDrawDownObsMf6Node, PriorNode, FDrawDownObs6List);
    InitializeMF_BoundaryNode(FvstChdObsMf6Node, PriorNode, FChdObs6List);
    InitializeMF_BoundaryNode(FvstDrnObsMf6Node, PriorNode, FDrnObs6List);
    InitializeMF_BoundaryNode(FvstEvtObsMf6Node, PriorNode, FEvtObs6List);
    InitializeMF_BoundaryNode(FvstGhbObsMf6Node, PriorNode, FGhbObs6List);
    InitializeMF_BoundaryNode(FvstRchObsMf6Node, PriorNode, FRchObs6List);
    InitializeMF_BoundaryNode(FvstRivObsMf6Node, PriorNode, FRivObs6List);
    InitializeMF_BoundaryNode(FvstWelObsMf6Node, PriorNode, FWelObs6List);
    InitializeMF_BoundaryNode(FvstToMvrObsMf6Node, PriorNode, FToMvrObs6List);
    InitializeMF_BoundaryNode(FvstLakObsMf6Node, PriorNode, FLakObs6List);
    InitializeMF_BoundaryNode(FvstMawObsMf6Node, PriorNode, FMawObs6List);
    InitializeMF_BoundaryNode(FvstSfrObsMf6Node, PriorNode, FSfrObs6List);
    InitializeMF_BoundaryNode(FvstUzfObsMf6Node, PriorNode, FUzfObs6List);
    InitializeMF_BoundaryNode(FvstCsubObsMf6Node, PriorNode, FCsubObs6List);

    InitializeMF_BoundaryNode(FvstGwtConcObsMf6Node, PriorNode, FGwtConcObs6List);
    InitializeMF_BoundaryNode(FvstGwtCncObsMf6Node, PriorNode, FGwtCncObs6List);
    InitializeMF_BoundaryNode(FvstGwtSrcObsMf6Node, PriorNode, FGwtSrcObs6List);

    InitializeMF_BoundaryNode(FvstSftObsMf6Node, PriorNode, FSftObs6List);
    InitializeMF_BoundaryNode(FvstLktObsMf6Node, PriorNode, FLktObs6List);
    InitializeMF_BoundaryNode(FvstMwtObsMf6Node, PriorNode, FMwtObs6List);
    InitializeMF_BoundaryNode(FvstUztObsMf6Node, PriorNode, FUztObs6List);

    if FvstCalibrationObsMf6Node = nil then
    begin
      FvstCalibrationObsMf6Node := vstObjects.InsertNode(
        FvstObsMf6Node, amInsertAfter);
      vstObjects.ReinitNode(FvstCalibrationObsMf6Node, False);
    end;
    InitializeNodeData(FvstCalibrationObsMf6Node, FCalibrationObs6List);

    PriorNode := FvstCalibrationObsMf6Node;

    // add children of FvstModflowBoundaryConditionsRoot
    if FvstModflowChdNode = nil then
    begin
      FvstModflowChdNode := vstObjects.InsertNode(
        FvstModflowBoundaryConditionsRoot, amAddChildFirst);
      vstObjects.ReinitNode(FvstModflowChdNode, False);
    end;
    InitializeNodeData(FvstModflowChdNode, FChdList);
    PriorNode := FvstModflowChdNode;


    InitializeMF_BoundaryNode(FvstModflowCSubNode, PriorNode, FCSubList);
    InitializeMF_BoundaryNode(FvstModflowDrnNode, PriorNode, FDrnList);
    InitializeMF_BoundaryNode(FvstModflowDrtNode, PriorNode, FDrtList);
    InitializeMF_BoundaryNode(FvstModflowDrtReturnLocation, PriorNode, FDrtReturnList);
    InitializeMF_BoundaryNode(FvstModflowEtsNode, PriorNode, FEtsList);
    InitializeMF_BoundaryNode(FvstModflowEvtNode, PriorNode, FEvtList);
    InitializeMF_BoundaryNode(FvstModflowGhbNode, PriorNode, FGhbList);
    InitializeMF_BoundaryNode(FvstModflowFhbHeadNode, PriorNode, FFhbHeadList);
    InitializeMF_BoundaryNode(FvstModflowFhbFlowNode, PriorNode, FFhbFlowList);
    InitializeMF_BoundaryNode(FvstModflowHfbNode, PriorNode, FHfbList);
    InitializeMF_BoundaryNode(FvstModflowHydmodNode, PriorNode, FHydmodList);
    InitializeMF_BoundaryNode(FvstModflowLakNode, PriorNode, FLakList);
    InitializeMF_BoundaryNode(FvstModflowMf6LakNode, PriorNode, FLakMf6List);

    InitializeMF_BoundaryNode(FvstModflowMawNode, PriorNode, FMawList);
    InitializeMF_BoundaryNode(FvstModflowMnw1Node, PriorNode, FMnw1List);
    InitializeMF_BoundaryNode(FvstModflowMnw2Node, PriorNode, FMnw2List);
    InitializeMF_BoundaryNode(FvstModflowMvrNode, PriorNode, FMvrList);
  //  InitializeMF_BoundaryNode(FvstObsMf6Node, PriorNode, FObs6List);
    InitializeMF_BoundaryNode(FvstModflowRchNode, PriorNode, FRchList);
    InitializeMF_BoundaryNode(FvstModflowResNode, PriorNode, FResList);
    InitializeMF_BoundaryNode(FvstModflowRipNode, PriorNode, FRipList);
    InitializeMF_BoundaryNode(FvstModflowRivNode, PriorNode, FRivList);
    InitializeMF_BoundaryNode(FvstModflowSfrNode, PriorNode, FSfrList);
    InitializeMF_BoundaryNode(FvstModflowSfrMF6Node, PriorNode, FSfrMf6List);
    InitializeMF_BoundaryNode(FvstModflowStrNode, PriorNode, FStrList);

    InitializeMF_BoundaryNode(FvstModflowSubObsNode, PriorNode, FModflowSubObsList);
    InitializeMF_BoundaryNode(FvstModflowSwtObsNode, PriorNode, FModflowSwtObsList);

    InitializeMF_BoundaryNode(FvstModflowGagNode, PriorNode, FSfrGagList);
    InitializeMF_BoundaryNode(FvstModflowUzfNode, PriorNode, FUzfList);
    InitializeMF_BoundaryNode(FvstModflowUzfMf6Node, PriorNode, FUzfMf6List);

    InitializeMF_BoundaryNode(FvstModflowGwtCncNode, PriorNode, FGwtCncList);
    InitializeMF_BoundaryNode(FvstModflowGwtSrcNode, PriorNode, FGwtSrcList);

    InitializeMF_BoundaryNode(FvstModflowWellNode, PriorNode, FMfWellList);
    InitializeMF_BoundaryNode(FvstModflowHobNode, PriorNode, FHobList);
    InitializeMF_BoundaryNode(FvstModflowSwiObsNode, PriorNode, FSwiObsList);


    InitializeMF_BoundaryNode(FvstModflowSwrReachNode, PriorNode, FSwrReachList);
    InitializeMF_BoundaryNode(FvstModflowSwrRainNode, PriorNode, FSwrRainList);
    InitializeMF_BoundaryNode(FvstModflowSwrEvapNode, PriorNode, FSwrEvapList);
    InitializeMF_BoundaryNode(FvstModflowSwrInflowNode, PriorNode, FSwrInflowList);
    InitializeMF_BoundaryNode(FvstModflowSwrStageNode, PriorNode, FSwrStageList);
    InitializeMF_BoundaryNode(FvstModflowSwrDirectRunoffNode, PriorNode, FSwrDirectRunoffList);

    InitializeMF_BoundaryNode(FvstModflowFarmNode, PriorNode, FFarmList);
    InitializeMF_BoundaryNode(FvstModflowFarmWellNode, PriorNode, FFarmWellList);
    InitializeMF_BoundaryNode(FvstModflowFarmPrecipNode, PriorNode, FFarmPrecipList);
    InitializeMF_BoundaryNode(FvstModflowFarmRefEvapNode, PriorNode, FFarmRefEvtList);
    InitializeMF_BoundaryNode(FvstModflowFarmCropIDNode, PriorNode, FFarmCropIDList);
    InitializeMF_BoundaryNode(FvstModflowCfpRechargeNode, PriorNode, FCfpRechargeList);


    InitializeMF_BoundaryNode(FvstMt3dLkt, PriorNode, FLktList);
    InitializeMF_BoundaryNode(FvstMt3dSft, PriorNode, FSftList);
    InitializeMF_BoundaryNode(FvstMt3dmsSsm, PriorNode, FSsmList);
    InitializeMF_BoundaryNode(FvstMt3dmsTob, PriorNode, FTobList);
    InitializeMF_BoundaryNode(FvstMt3dUztRech, PriorNode, FUztRechList);
    InitializeMF_BoundaryNode(FvstMt3dUztSat, PriorNode, FUztSatList);
    InitializeMF_BoundaryNode(FvstMt3dUztUnsat, PriorNode, FUztUnsatList);
    InitializeMF_BoundaryNode(FvstMt3dSeepage, PriorNode, FUztSeepageList);

    // Add children of FvstSutraFeaturesNode
    if FvstSutraObsNode = nil then
    begin
      FvstSutraObsNode := vstObjects.InsertNode(
        FvstSutraFeaturesNode, amAddChildFirst);
      vstObjects.ReinitNode(FvstSutraObsNode, False);
    end;
    InitializeNodeData(FvstSutraObsNode, FSutraObsList);
    PriorNode := FvstSutraObsNode;

    InitializeMF_BoundaryNode(FvstSutraSpecPressureNode, PriorNode, FSutraSpecPressureList);
    InitializeMF_BoundaryNode(FvstSutraSpecUNode, PriorNode, FSutraSpecUList);
    InitializeMF_BoundaryNode(FvstSutraFluidFluxNode, PriorNode, FSutraFluidFluxList);
    InitializeMF_BoundaryNode(FvstSutraUFluxNode, PriorNode, FSutraUFluxList);
    InitializeMF_BoundaryNode(FvstSutraLakeNode, PriorNode, FSutraLakeList);
    InitializeMF_BoundaryNode(FvstSutraGeneralizedFlowNode, PriorNode, FSutraGeneralizedFlowList);
    InitializeMF_BoundaryNode(FvstSutraGeneralizedTransportNode, PriorNode, FSutraGeneralizedTransportList);
    InitializeMF_BoundaryNode(FvstSutraStateObsNode, PriorNode, FSutraStateObsList);


    // Add children of FvstFootprintFeaturesNode
    if FvstFootprintWellNode = nil then
    begin
      FvstFootprintWellNode := vstObjects.InsertNode(
        FvstFootprintFeaturesNode, amAddChildFirst);
      vstObjects.ReinitNode(FvstFootprintWellNode, False);
    end;
    InitializeNodeData(FvstFootprintWellNode, FFootprintList);
    PriorNode := FvstFootprintWellNode;

    FDataSetLists.Clear;

    vstObjects.ChildCount[FvstDataSetRootNode] := 0;
    DataSetClassifications := TStringList.Create;
    try
      DataSetClassifications.AddObject('Data Sets', TObject(FvstDataSetRootNode));

      ClassificationObjectOwnerList := TObjectList.Create;
      try
    // Create lists used for sorting the nodes.
        HufDataArrays := TClassificationList.Create;
        ClassificationObjects:= TClassificationList.Create;
        LayerGroupList := TClassificationList.Create;
        SutraLayerGroupList := TClassificationList.Create;
        try
          FillDataSetLists(HufDataArrays,
            LayerGroupList, SutraLayerGroupList,
            ClassificationObjects,
            ClassificationObjectOwnerList);

          SortedClassifiedDataSets := TStringList.Create;
          try
            ClassifyListedObjects(SortedClassifiedDataSets, ClassificationObjects,
              [LayerGroupList, SutraLayerGroupList, HufDataArrays]);

            for DataSetIndex := 0 to SortedClassifiedDataSets.Count - 1 do
            begin
              Classification := SortedClassifiedDataSets[DataSetIndex];
              ClassificationObject := SortedClassifiedDataSets.Objects[DataSetIndex] as TDataSetClassification;
              if ClassificationObject <> nil then
              begin

                DataSet := ClassificationObject.DataArray;
                Classification := DataSet.FullClassification;
                ParentNode := FvstDataSetRootNode;
                Assert(Classification <> '');
                SeparatorPosition := Pos('|', Classification);
                while SeparatorPosition > 0 do
                begin
                  ALevelDescription := Copy(Classification, 1, Pred(SeparatorPosition));

                  FindOrCreateClassificationNode;

                  SeparatorPosition := PosEx('|', Classification,
                    Succ(SeparatorPosition));
                end;
                ALevelDescription := Classification;
                FindOrCreateClassificationNode;

                vstObjects.ChildCount[DataSetGroupNode] :=
                  vstObjects.ChildCount[DataSetGroupNode] + 1;

                vstDataSetNode := vstObjects.GetLastChild(DataSetGroupNode);

                Data := vstObjects.GetNodeData(vstDataSetNode);
                Data.Caption := DataSet.Name;

                ListOfObjects := TList.Create;
                FDataSetLists.Add(ListOfObjects);

                InitializeNodeData(vstDataSetNode, ListOfObjects);

                Data.IsDataSetNode := True;
                Data.Classification := Classification + '|' + DataSet.Name;

                DataSetList.AddObject(Data.Classification, TObject(vstDataSetNode));
              end;
            end;
          finally
            SortedClassifiedDataSets.Free;
          end;
        finally
          SutraLayerGroupList.Free;
          LayerGroupList.Free;
          ClassificationObjects.Free;
          HufDataArrays.Free;
        end;
      finally
        ClassificationObjectOwnerList.Free;
      end;
    finally
      DataSetClassifications.Free;
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

destructor TfrmCustomSelectObjects.Destroy;
begin
  FExapandedNodes.Free;
  FDataSetLists.Free;
  FDataSetLists := nil;

  FAllObjectsList.Free;
  FLeakyList.Free;
  FOtherObjectsList.Free;
  FRiverList.Free;
  FSizeList.Free;
  FRefinementList.Free;
  FSpecifiedFluxList.Free;
  FSpecifiedHeadList.Free;
  FWellList.Free;
  FChdList.Free;
  FGhbList.Free;
  FFhbHeadList.Free;
  FFhbFlowList.Free;
  FHydmodList.Free;
  FLakList.Free;
  FLakMf6List.Free;
  FMawList.Free;
  FMnw1List.Free;
  FMnw2List.Free;
  FMvrList.Free;
  FMfWellList.Free;
  FRivList.Free;
  FDrnList.Free;
  FDrtList.Free;
  FDrtReturnList.Free;
  FRchList.Free;
  FEvtList.Free;
  FEtsList.Free;
  FResList.Free;
  FSfrGagList.Free;
  FSfrList.Free;
  FSfrMf6List.Free;
  FStrList.Free;
  FUzfMf6List.Free;
  FGwtCncList.Free;
  FGwtSrcList.Free;
  FUzfList.Free;
  FHobList.Free;
  FSwiObsList.Free;
  FHfbList.Free;
  FCSubList.Free;
  FModflowSubObsList.Free;
  FModflowSwtObsList.Free;

  FSwrReachList.Free;
  FSwrRainList.Free;
  FSwrEvapList.Free;
  FSwrInflowList.Free;
  FSwrStageList.Free;
  FSwrDirectRunoffList.Free;

  FFarmList.Free;
  FFarmWellList.Free;
  FFarmPrecipList.Free;
  FFarmRefEvtList.Free;
  FFarmCropIDList.Free;
  FCfpRechargeList.Free;
//  FObs6List.Free;
  FSsmList.Free;
  FLktList.Free;
  FSftList.Free;
  FTobList.Free;
  FUztRechList.Free;
  FUztSatList.Free;
  FUztSeepageList.Free;

  FUztUnsatList.Free;
  FModpathList.Free;
  FChildModelList.Free;
  FRipList.Free;

  FSutraObsList.Free;
  FSutraSpecPressureList.Free;
  FSutraSpecUList.Free;
  FSutraFluidFluxList.Free;
  FSutraUFluxList.Free;

  FSutraLakeList.Free;
  FSutraGeneralizedFlowList.Free;
  FSutraGeneralizedTransportList.Free;
  FSutraStateObsList.Free;

  FFootprintList.Free;

  FCalibrationObs6List.Free;
  FHeadObs6List.Free;
  FDrawDownObs6List.Free;
  FChdObs6List.Free;
  FDrnObs6List.Free;
  FEvtObs6List.Free;
  FGhbObs6List.Free;
  FRchObs6List.Free;
  FRivObs6List.Free;
  FWelObs6List.Free;
  FToMvrObs6List.Free;
  FLakObs6List.Free;
  FMawObs6List.Free;
  FSfrObs6List.Free;
  FUzfObs6List.Free;
  FCsubObs6List.Free;

  FGwtConcObs6List.Free;
  FGwtCncObs6List.Free;
  FGwtSrcObs6List.Free;

  FSftObs6List.Free;
  FLktObs6List.Free;
  FMwtObs6List.Free;
  FUztObs6List.Free;
  inherited;
end;

procedure TfrmCustomSelectObjects.FormCreate(Sender: TObject);
begin
  inherited;
  vstObjects.NodeDataSize := SizeOf(TMyRec);

  FDataSetLists:= TObjectList.Create;

  FAllObjectsList:= TList.Create;
  FLeakyList:= TList.Create;
  FOtherObjectsList:= TList.Create;
  FRiverList:= TList.Create;
  FSizeList:= TList.Create;
  FRefinementList:= TList.Create;
  FSpecifiedFluxList:= TList.Create;
  FSpecifiedHeadList:= TList.Create;
  FWellList:= TList.Create;
  FChdList := TList.Create;
  FGhbList := TList.Create;
  FFhbHeadList := TList.Create;
  FFhbFlowList := TList.Create;
  FHydmodList := TList.Create;
  FLakList := TList.Create;
  FLakMf6List := TList.Create;
  FMnw1List := TList.Create;
  FMawList := TList.Create;
  FMnw2List := TList.Create;
  FMvrList := TList.Create;
  FMfWellList := TList.Create;
  FRivList := TList.Create;
  FDrnList := TList.Create;
  FDrtList := TList.Create;
  FDrtReturnList := TList.Create;
  FRchList := TList.Create;
  FEvtList := TList.Create;
  FEtsList := TList.Create;
  FResList := TList.Create;
  FSfrList := TList.Create;
  FSfrMf6List := TList.Create;
  FStrList := TList.Create;
  FSfrGagList := TList.Create;
  FUzfList := TList.Create;
  FUzfMf6List := TList.Create;

  FGwtCncList := TList.Create;
  FGwtSrcList := TList.Create;

  FCSubList := TList.Create;
  FModflowSubObsList := TList.Create;
  FModflowSwtObsList := TList.Create;

  FSwrReachList := TList.Create;
  FSwrRainList := TList.Create;
  FSwrEvapList := TList.Create;
  FSwrInflowList := TList.Create;
  FSwrStageList := TList.Create;
  FSwrDirectRunoffList := TList.Create;

  FFarmList := TList.Create;
  FFarmWellList := TList.Create;
  FFarmPrecipList := TList.Create;
  FFarmRefEvtList := TList.Create;
  FFarmCropIDList := TList.Create;
  FCfpRechargeList := TList.Create;
//  FObs6List := TList.Create;
  FHobList := TList.Create;
  FSwiObsList := TList.Create;
  FHfbList := TList.Create;
  FSsmList := TList.Create;
  FSftList := TList.Create;
  FLktList := TList.Create;
  FTobList := TList.Create;
  FUztRechList := TList.Create;
  FUztSatList := TList.Create;
  FUztUnsatList := TList.Create;
  FUztSeepageList := TList.Create;
  FModpathList := TList.Create;
  FChildModelList := TList.Create;
  FRipList := TList.Create;

  FSutraObsList := TList.Create;
  FSutraSpecPressureList := TList.Create;
  FSutraSpecUList := TList.Create;
  FSutraFluidFluxList := TList.Create;
  FSutraUFluxList := TList.Create;

  FSutraLakeList := TList.Create;
  FSutraGeneralizedFlowList := TList.Create;
  FSutraGeneralizedTransportList := TList.Create;
  FSutraStateObsList := TList.Create;

  FFootprintList := TList.Create;
  FExapandedNodes := TStringList.Create;

  FCalibrationObs6List := TList.Create;
  FHeadObs6List := TList.Create;
  FDrawDownObs6List := TList.Create;
  FChdObs6List := TList.Create;
  FDrnObs6List := TList.Create;
  FEvtObs6List := TList.Create;
  FGhbObs6List := TList.Create;
  FRchObs6List := TList.Create;
  FRivObs6List := TList.Create;
  FWelObs6List := TList.Create;
  FToMvrObs6List := TList.Create;
  FLakObs6List := TList.Create;
  FMawObs6List := TList.Create;
  FSfrObs6List := TList.Create;
  FUzfObs6List := TList.Create;
  FCsubObs6List  := TList.Create;

  FGwtConcObs6List  := TList.Create;
  FGwtCncObs6List  := TList.Create;
  FGwtSrcObs6List  := TList.Create;

  FSftObs6List  := TList.Create;
  FLktObs6List  := TList.Create;
  FMwtObs6List  := TList.Create;
  FUztObs6List  := TList.Create;

  FCanEdit := True;

end;

procedure TfrmCustomSelectObjects.FormDestroy(Sender: TObject);
var
  Node: PVirtualNode;
begin
  inherited;
  Node := vstObjects.GetFirst;
  while Node <> nil do
  begin
    vstObjects.DeleteNode(Node);
    Node := vstObjects.GetFirst;
  end;
end;

procedure TfrmCustomSelectObjects.NilBaseNodes;
begin
  FvstAllObjectsNode := nil;
  FvstSizeNode := nil;
  FvstRefinementNode := nil;
  FvstDataSetRootNode := nil;
  FvstPhastBoundaryConditionsRoot := nil;
  FvstModflowBoundaryConditionsRoot := nil;
  FvstSpecifiedHeadNode := nil;
  FvstSpecifiedFluxNode := nil;
  FvstLeakyNode := nil;
  FvstRiverNode := nil;
  FvstWellNode := nil;
  FvstOtherObjectsNode := nil;
  FvstModflowChdNode := nil;
  FvstModflowGhbNode := nil;
  FvstModflowFhbHeadNode := nil;
  FvstModflowFhbFlowNode := nil;
  FvstModflowHydmodNode := nil;
  FvstModflowSwiObsNode := nil;
  FvstModflowLakNode := nil;
  FvstModflowMf6LakNode := nil;
  FvstModflowMnw1Node := nil;
  FvstModflowMnw2Node := nil;
  FvstModflowMawNode := nil;
  FvstModflowWellNode := nil;
  FvstModflowRivNode := nil;
  FvstModflowDrnNode := nil;
  FvstModflowDrtNode := nil;
  FvstModflowDrtReturnLocation := nil;
  FvstModflowRchNode := nil;
  FvstModflowEvtNode := nil;
  FvstModflowEtsNode := nil;
  FvstModflowResNode := nil;
  FvstModflowRipNode := nil;
  FvstModflowSfrNode := nil;
  FvstModflowSfrMF6Node := nil;
  FvstModflowStrNode := nil;
  FvstModflowUzfNode := nil;
  FvstModflowUzfMf6Node := nil;
  FvstModflowGwtCncNode := nil;
  FvstModflowGwtSrcNode := nil;
  FvstModflowHobNode := nil;
  FvstModflowHfbNode := nil;
  FvstModflowMvrNode := nil;
  FvstModflowCSubNode := nil;
  FvstModflowSubObsNode := nil;
  FvstModflowSwtObsNode := nil;

  FvstModflowSwrReachNode := nil;
  FvstModflowSwrRainNode := nil;
  FvstModflowSwrEvapNode := nil;
  FvstModflowSwrInflowNode := nil;
  FvstModflowSwrStageNode := nil;
  FvstModflowSwrDirectRunoffNode := nil;

  FvstModflowFarmNode := nil;
  FvstModflowFarmWellNode := nil;
  FvstModflowFarmPrecipNode := nil;
  FvstModflowFarmRefEvapNode := nil;
  FvstModflowFarmCropIDNode := nil;
  FvstModflowCfpRechargeNode := nil;
  FvstMt3dmsSsm := nil;
  FvstMt3dmsTob := nil;
  FvstMt3dUztRech := nil;
  FvstMt3dUztSat := nil;
  FvstMt3dUztUnsat := nil;
  FvstMt3dSeepage := nil;
  FvstModflowGagNode := nil;

  FvstSutraObsNode := nil;
  FvstSutraSpecPressureNode := nil;
  FvstSutraSpecUNode := nil;
  FvstSutraFluidFluxNode := nil;
  FvstSutraUFluxNode := nil;

  FvstSutraLakeNode := nil;
  FvstSutraGeneralizedFlowNode := nil;
  FvstSutraGeneralizedTransportNode := nil;
  FvstSutraStateObsNode := nil;

  FvstFootprintWellNode := nil;

  FvstModpathRoot := nil;
  FvstChildModelNode := nil;
  FvstSutraFeaturesNode := nil;
  FvstFootprintFeaturesNode := nil;

  FvstHeadObsMf6Node := nil;
  FvstDrawDownObsMf6Node := nil;
  FvstChdObsMf6Node := nil;
  FvstDrnObsMf6Node := nil;
  FvstEvtObsMf6Node := nil;
  FvstGhbObsMf6Node := nil;
  FvstRchObsMf6Node := nil;
  FvstRivObsMf6Node := nil;
  FvstWelObsMf6Node := nil;
  FvstToMvrObsMf6Node := nil;
  FvstLakObsMf6Node := nil;
  FvstMawObsMf6Node := nil;
  FvstSfrObsMf6Node := nil;
  FvstUzfObsMf6Node := nil;
  FvstSfrObsMf6Node := nil;
  FvstUzfObsMf6Node := nil;
  FvstUzfObsMf6Node := nil;
  FvstCsubObsMf6Node := nil;

  FvstGwtConcObsMf6Node := nil;
  FvstGwtCncObsMf6Node := nil;
  FvstGwtSrcObsMf6Node := nil;

  FvstSftObsMf6Node := nil;
  FvstLktObsMf6Node := nil;
  FvstMwtObsMf6Node := nil;
  FvstUztObsMf6Node := nil;

  FvstObsMf6Node := nil;
  FvstCalibrationObsMf6Node := nil;

end;

function TfrmCustomSelectObjects.NodeString(ANode: PVirtualNode): string;
var
  NodeData: PMyRec;
  ParentNode: PVirtualNode;
begin
  NodeData := vstObjects.GetNodeData(ANode);
  result := NodeData.Caption;
  ParentNode := vstObjects.NodeParent[ANode];
  while ParentNode <> nil do
  begin
    NodeData := vstObjects.GetNodeData(ParentNode);
    result := NodeData.Caption + '|' + result;
    ParentNode := vstObjects.NodeParent[ParentNode];
  end;
end;

procedure TfrmCustomSelectObjects.RecordExpandedNodes;
var
  ANode: PVirtualNode;
begin
  vstObjects.BeginUpdate;
  try
    FExapandedNodes.Clear;
    ANode := vstObjects.GetFirst;
    while ANode <> nil do
    begin
      if vstObjects.Expanded[ANode] then
      begin
        FExapandedNodes.Add(NodeString(ANode));
      end;
      ANode := vstObjects.GetNext(ANode)
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.RestoreExpandedNodes;
var
  ANode: PVirtualNode;
begin
  vstObjects.BeginUpdate;
  try
    ANode := vstObjects.GetFirst;
    while ANode <> nil do
    begin
      if FExapandedNodes.IndexOf(NodeString(ANode)) >= 0 then
      begin
        FExapandedNodes.Add(NodeString(ANode));
        vstObjects.Expanded[ANode] := True;
      end;
      ANode := vstObjects.GetNext(ANode)
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.SetCanEdit(const Value: boolean);
begin
  FCanEdit := Value;
end;

procedure TfrmCustomSelectObjects.SetFirstNodeCheckState(var ChildData: PMyRec;
  ScreenObject: TScreenObject);
begin
  if ShouldCheckBoxBeChecked(ScreenObject) then
  begin
    ChildData.GroupState := vgsChecked;
  end
  else
  begin
    ChildData.GroupState := vgsUnChecked;
  end;
end;

procedure TfrmCustomSelectObjects.HandleChildren(Sender: TBaseVirtualTree; Node: PVirtualNode);
var
  ChildNode: PVirtualNode;
  ChildData: PMyRec;
  Index: integer;
  AScreenObject: TScreenObject;
begin
  vstObjects.BeginUpdate;
  try
    ChildNode := Sender.GetFirstChild(Node);
    while ChildNode <> nil do
    begin
      ChildData := Sender.GetNodeData(ChildNode);
      if (ChildData <> nil) and (ChildData.ScreenObjects <> nil) then
      begin
        case Sender.CheckState[Node] of
          csUncheckedNormal:
            begin
              for Index := 0 to ChildData.ScreenObjects.Count - 1 do
              begin
                AScreenObject := ChildData.ScreenObjects[Index];
                HandleUnChecked(AScreenObject);
              end;
            end;
          csCheckedNormal:
            begin
              for Index := 0 to ChildData.ScreenObjects.Count - 1 do
              begin
                AScreenObject := ChildData.ScreenObjects[Index];
                HandleChecked(AScreenObject);
              end;
            end;
          csMixedNormal:
            begin
              // do nothing.
            end;
          else
            begin
              Assert(False);
            end;
        end;
      end
      else
      begin
        Sender.CheckState[ChildNode] := Sender.CheckState[Node];
        HandleChildren(Sender, ChildNode);
      end;
      ChildNode := Sender.GetNextSibling(ChildNode);
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.SortScreenObjectLists;
var
  Index: integer;
  List: TList;
begin
  FAllObjectsList.Sort(ScreenObjectCompare);
  FLeakyList.Sort(ScreenObjectCompare);
  FOtherObjectsList.Sort(ScreenObjectCompare);
  FRiverList.Sort(ScreenObjectCompare);
  FSizeList.Sort(ScreenObjectCompare);
  FRefinementList.Sort(ScreenObjectCompare);
  FSpecifiedFluxList.Sort(ScreenObjectCompare);
  FSpecifiedHeadList.Sort(ScreenObjectCompare);
  FWellList.Sort(ScreenObjectCompare);

  FChdList.Sort(ScreenObjectCompare);
  FGhbList.Sort(ScreenObjectCompare);
  FFhbHeadList.Sort(ScreenObjectCompare);
  FFhbFlowList.Sort(ScreenObjectCompare);
  FHydmodList.Sort(ScreenObjectCompare);
  FLakList.Sort(ScreenObjectCompare);
  FLakMf6List.Sort(ScreenObjectCompare);
  FMawList.Sort(ScreenObjectCompare);
  FMnw1List.Sort(ScreenObjectCompare);
  FMnw2List.Sort(ScreenObjectCompare);
  FMvrList.Sort(ScreenObjectCompare);
  FMfWellList.Sort(ScreenObjectCompare);
  FRivList.Sort(ScreenObjectCompare);
  FDrnList.Sort(ScreenObjectCompare);
  FDrtList.Sort(ScreenObjectCompare);
  FDrtReturnList.Sort(ScreenObjectCompare);
  FRchList.Sort(ScreenObjectCompare);
  FRipList.Sort(ScreenObjectCompare);
  FEvtList.Sort(ScreenObjectCompare);
  FEtsList.Sort(ScreenObjectCompare);
  FResList.Sort(ScreenObjectCompare);
  FSfrList.Sort(ScreenObjectCompare);
  FSfrMf6List.Sort(ScreenObjectCompare);
  FStrList.Sort(ScreenObjectCompare);
  FSfrGagList.Sort(ScreenObjectCompare);
  FUzfList.Sort(ScreenObjectCompare);
  FUzfMf6List.Sort(ScreenObjectCompare);

  FGwtCncList.Sort(ScreenObjectCompare);
  FGwtSrcList.Sort(ScreenObjectCompare);

  FHobList.Sort(ScreenObjectCompare);
  FSwiObsList.Sort(ScreenObjectCompare);
  FHfbList.Sort(ScreenObjectCompare);
  FSsmList.Sort(ScreenObjectCompare);
  FSftList.Sort(ScreenObjectCompare);
  FLktList.Sort(ScreenObjectCompare);
  FTobList.Sort(ScreenObjectCompare);
  FUztRechList.Sort(ScreenObjectCompare);
  FUztSatList.Sort(ScreenObjectCompare);
  FUztUnsatList.Sort(ScreenObjectCompare);
  FCSubList.Sort(ScreenObjectCompare);
  FModflowSubObsList.Sort(ScreenObjectCompare);
  FModflowSwtObsList.Sort(ScreenObjectCompare);

  FSwrReachList.Sort(ScreenObjectCompare);
  FSwrRainList.Sort(ScreenObjectCompare);
  FSwrEvapList.Sort(ScreenObjectCompare);
  FSwrInflowList.Sort(ScreenObjectCompare);
  FSwrStageList.Sort(ScreenObjectCompare);
  FSwrDirectRunoffList.Sort(ScreenObjectCompare);

  FFarmList.Sort(ScreenObjectCompare);
  FFarmWellList.Sort(ScreenObjectCompare);
  FFarmPrecipList.Sort(ScreenObjectCompare);
  FFarmRefEvtList.Sort(ScreenObjectCompare);
  FFarmCropIDList.Sort(ScreenObjectCompare);
  FCfpRechargeList.Sort(ScreenObjectCompare);
//  FObs6List.Sort(ScreenObjectCompare);
  FModpathList.Sort(ScreenObjectCompare);
  FChildModelList.Sort(ScreenObjectCompare);

  FSutraObsList.Sort(ScreenObjectCompare);
  FSutraSpecPressureList.Sort(ScreenObjectCompare);
  FSutraSpecUList.Sort(ScreenObjectCompare);
  FSutraFluidFluxList.Sort(ScreenObjectCompare);
  FSutraUFluxList.Sort(ScreenObjectCompare);

  FSutraLakeList.Sort(ScreenObjectCompare);
  FSutraGeneralizedFlowList.Sort(ScreenObjectCompare);
  FSutraGeneralizedTransportList.Sort(ScreenObjectCompare);
  FSutraStateObsList.Sort(ScreenObjectCompare);

  FFootprintList.Sort(ScreenObjectCompare);

  FHeadObs6List.Sort(ScreenObjectCompare);
  FCalibrationObs6List.Sort(ScreenObjectCompare);
  FDrawDownObs6List.Sort(ScreenObjectCompare);
  FChdObs6List.Sort(ScreenObjectCompare);
  FDrnObs6List.Sort(ScreenObjectCompare);
  FEvtObs6List.Sort(ScreenObjectCompare);
  FGhbObs6List.Sort(ScreenObjectCompare);
  FRchObs6List.Sort(ScreenObjectCompare);
  FRivObs6List.Sort(ScreenObjectCompare);
  FWelObs6List.Sort(ScreenObjectCompare);
  FToMvrObs6List.Sort(ScreenObjectCompare);
  FLakObs6List.Sort(ScreenObjectCompare);
  FMawObs6List.Sort(ScreenObjectCompare);
  FSfrObs6List.Sort(ScreenObjectCompare);
  FUzfObs6List.Sort(ScreenObjectCompare);
  FCsubObs6List.Sort(ScreenObjectCompare);

  FGwtConcObs6List.Sort(ScreenObjectCompare);
  FGwtCncObs6List.Sort(ScreenObjectCompare);
  FGwtSrcObs6List.Sort(ScreenObjectCompare);

  FSftObs6List.Sort(ScreenObjectCompare);
  FLktObs6List.Sort(ScreenObjectCompare);
  FMwtObs6List.Sort(ScreenObjectCompare);
  FUztObs6List.Sort(ScreenObjectCompare);

  for Index := 0 to FDataSetLists.Count - 1 do
  begin
    List := FDataSetLists[Index] as TList;
    List.Sort(ScreenObjectCompare);
  end;
end;

procedure TfrmCustomSelectObjects.SetRootNodeStates(Node: PVirtualNode);
var
  Index: integer;
  ChildNode: PVirtualNode;
  Data, ChildData: PMyRec;
  ChildCount: integer;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  StateChanged: Boolean;
begin
  if Node = nil then
  begin
    Exit;
  end;
  vstObjects.BeginUpdate;
  try
    Data := vstObjects.GetNodeData(Node);
    ChildNode := nil;
    // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
    // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
    ChildCount := vstObjects.ChildCount[Node];
    for Index := 0 to ChildCount - 1 do
    begin
      if Index = 0 then
      begin
        ChildNode := vstObjects.GetFirstChild(Node);
      end
      else
      begin
        ChildNode := vstObjects.GetNextSibling(ChildNode);
      end;
      if vstObjects.ChildCount[ChildNode] > 0 then
      begin
        SetRootNodeStates(ChildNode);
      end;
      ChildData := vstObjects.GetNodeData(ChildNode);
      if ChildData.ScreenObjects <> nil then
      begin
        UpdateChildCheck(ChildNode);
        StateChanged := False;
        for ScreenObjectIndex := 0 to ChildData.ScreenObjects.Count - 1 do
        begin
          ScreenObject := ChildData.ScreenObjects[ScreenObjectIndex];
          if ScreenObjectIndex = 0 then
          begin
            SetFirstNodeCheckState(ChildData, ScreenObject);
          end
          else
          begin
  //          StateChanged := False;
            SetSubsequentNodesCheckState(StateChanged, ScreenObject, ChildData);
  //          if StateChanged then
  //          begin
  //            break;
  //          end;
          end;
        end;
      end;
      if Index = 0 then
      begin
        Data.GroupState := ChildData.GroupState;
      end
      else
      begin
        if Data.GroupState <> ChildData.GroupState then
        begin
          Data.GroupState := vgs3State;
  //        break;
        end;
      end;
    end;
    case Data.GroupState of
      vgsUndefined:
        begin
          // do nothing
        end;
      vgsUnChecked:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsChecked:
        begin
          vstObjects.CheckState[Node] := csCheckedNormal;
        end;
      vgs3State:
        begin
          vstObjects.CheckState[Node] := csMixedNormal;
        end;
      else
        begin
          Assert(False);
        end;
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.SetSubsequentNodesCheckState(
  var StateChanged: Boolean; ScreenObject: TScreenObject;
  var ChildData: PMyRec);
begin
  if ShouldCheckBoxBeChecked(ScreenObject) then
  begin
    if ChildData.GroupState = vgsUnChecked then
    begin
      ChildData.GroupState := vgs3State;
      StateChanged := True;
    end;
  end
  else
  begin
    if ChildData.GroupState = vgsChecked then
    begin
      ChildData.GroupState := vgs3State;
      StateChanged := True;
    end;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateChildCheck(Node: PVirtualNode);
var
  Index: integer;
  GroupState: TVisibleGroupState;
  ChildNode: PVirtualNode;
  Data, ChildData: PMyRec;
  AScreenObject: TScreenObject;
  ChildCount: integer;
begin
  if Node = nil then
  begin
    Exit;
  end;
  FSettingData3 := True;
  try
    GroupState := vgsUndefined;
    Data := vstObjects.GetNodeData(Node);
    ChildNode := nil;
    // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
    // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
    ChildCount := vstObjects.ChildCount[Node];
    Assert(Data.ScreenObjects.Count= ChildCount);
    for Index := 0 to ChildCount -1 do
    begin
      AScreenObject := Data.ScreenObjects[Index];
      if Index = 0 then
      begin
        ChildNode := vstObjects.GetFirstChild(Node);
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          GroupState := vgsChecked;
        end
        else
        begin
          GroupState := vgsUnChecked;
        end;
      end
      else
      begin
        ChildNode := vstObjects.GetNextSibling(ChildNode);
        if ShouldCheckBoxBeChecked(AScreenObject) then
        begin
          if GroupState <> vgsChecked then
          begin
            GroupState := vgs3State;
          end;
        end
        else
        begin
          if GroupState <> vgsUnChecked then
          begin
            GroupState := vgs3State;
          end;
        end;
      end;
      ChildData := vstObjects.GetNodeData(ChildNode);
      if ShouldCheckBoxBeChecked(AScreenObject) then
      begin
        vstObjects.CheckState[ChildNode] := csCheckedNormal;
        ChildData.GroupState := vgsChecked;
      end
      else
      begin
        vstObjects.CheckState[ChildNode] := csUncheckedNormal;
        ChildData.GroupState := vgsUnChecked;
      end;
    end;
    case GroupState of
      vgsUndefined:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsUnChecked:
        begin
          vstObjects.CheckState[Node] := csUncheckedNormal;
        end;
      vgsChecked:
        begin
          vstObjects.CheckState[Node] := csCheckedNormal;
        end;
      vgs3State:
        begin
          vstObjects.CheckState[Node] := csMixedNormal;
        end;
      else
      begin
        Assert(False);
      end;
    end;
    Data.GroupState := GroupState;
  finally
    FSettingData3 := False;
  end;
end;

procedure TfrmCustomSelectObjects.HandleCheckChange(Node: PVirtualNode;
  Sender: TBaseVirtualTree);
var
  ChildIndex: Integer;
  Data: PMyRec;
  Index: Integer;
  AScreenObject: TScreenObject;
  ParentData: PMyRec;
  ChildNode: PVirtualNode;
  ChildCount: Integer;
begin
  vstObjects.BeginUpdate;
  try
    if (Node = FvstDataSetRootNode)
      or (Node = FvstPhastBoundaryConditionsRoot)
      or (Node = FvstModflowBoundaryConditionsRoot)
      or (Node = FvstSutraFeaturesNode)
      or (Node = FvstFootprintFeaturesNode)
      or (Node = FvstObsMf6Node)
//      or (Node = FvstCalibrationObsMf6Node)
      then
    begin
      // This branch is for nodes that do not hold any objects themselves.
      if Sender.CheckState[Node] <> csMixedNormal then
      begin
        ChildNode := nil;
        // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
        // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
        ChildCount := Sender.ChildCount[Node];
        for ChildIndex := 0 to ChildCount - 1 do
        begin
          if ChildIndex = 0 then
          begin
            ChildNode := Sender.GetFirstChild(Node);
          end
          else
          begin
            ChildNode := Sender.GetNextSibling(ChildNode);
          end;
          Data := Sender.GetNodeData(ChildNode);
          if Data.ScreenObjects <> nil then
          begin
            case Sender.CheckState[Node] of
              csUncheckedNormal:
                begin
                  for Index := 0 to Data.ScreenObjects.Count - 1 do
                  begin
                    AScreenObject := Data.ScreenObjects[Index];
                    HandleUnchecked(AScreenObject);
                  end;
                end;
              csCheckedNormal:
                begin
                  for Index := 0 to Data.ScreenObjects.Count - 1 do
                  begin
                    AScreenObject := Data.ScreenObjects[Index];
                    HandleChecked(AScreenObject);
                  end;
                end;
              csMixedNormal:
                begin
                end;
            else
              // do nothing.
              begin
                Assert(False);
              end;
            end;
          end
          else
          begin
            HandleChildren(Sender, ChildNode);
          end;
        end;
      end;
    end
    else
    begin
      // This branch is for root nodes that hold objects.
      Data := Sender.GetNodeData(Node);
      if (Data <> nil) and (Data.ScreenObjects <> nil) then
      begin
        case Sender.CheckState[Node] of
          csUncheckedNormal:
            begin
              for Index := 0 to Data.ScreenObjects.Count - 1 do
              begin
                AScreenObject := Data.ScreenObjects[Index];
                HandleUnchecked(AScreenObject);
              end;
            end;
          csCheckedNormal:
            begin
              for Index := 0 to Data.ScreenObjects.Count - 1 do
              begin
                AScreenObject := Data.ScreenObjects[Index];
                HandleChecked(AScreenObject);
              end;
            end;
          csMixedNormal:
            begin
            end;
        else
          // do nothing.
          begin
            Assert(False);
          end;
        end;
      end
      else
      begin
        HandleChildren(Sender, Node);
        ParentData := Sender.GetNodeData(Sender.NodeParent[Node]);
        if (ParentData <> nil) and (ParentData.ScreenObjects <> nil) then
        begin
          AScreenObject := ParentData.ScreenObjects[Node.Index];
          case Sender.CheckState[Node] of
            csUncheckedNormal:
              begin
                HandleUnchecked(AScreenObject);
              end;
            csCheckedNormal:
              begin
                HandleChecked(AScreenObject);
              end;
          else
            begin
              Assert(False);
            end;
          end;
        end;
      end;
    end;
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateDisplay;
var
  Index, ChildIndex: integer;
  vstNode, ChildNode: PVirtualNode;
  ChildCount: integer;
  procedure UpdateDataSetChildren(Node: PVirtualNode);
  var
    ChildIndex, ChildCount: integer;
    ChildNode: PVirtualNode;
    ChildData: PMyRec;
  begin
    ChildCount := vstObjects.ChildCount[Node];
    ChildNode := nil;
    for ChildIndex := 0 to ChildCount - 1 do
    begin
      if ChildIndex = 0 then
      begin
        ChildNode := vstObjects.GetFirstChild(Node);
      end
      else
      begin
        ChildNode := vstObjects.GetNextSibling(ChildNode);
      end;
      ChildData := vstObjects.GetNodeData(ChildNode);
      if ChildData.IsDataSetNode then
      begin
        UpdateChildCheck(ChildNode);
        SetRootNodeStates(Node);
      end
      else
      begin
        UpdateDataSetChildren(ChildNode);
      end;
    end;
  end;
begin
  RecordExpandedNodes;
  vstObjects.BeginUpdate;
  try
    vstNode := nil;
    for Index := 0 to vstObjects.RootNodeCount -1 do
    begin
      if Index = 0 then
      begin
        vstNode := vstObjects.GetFirstChild(vstObjects.RootNode);
      end
      else
      begin
        vstNode := vstObjects.GetNextSibling(vstNode);
      end;
      ChildNode := nil;
      if (vstNode = FvstPhastBoundaryConditionsRoot)
        or (vstNode = FvstModflowBoundaryConditionsRoot)
        or (vstNode = FvstSutraFeaturesNode)
        or (vstNode = FvstFootprintFeaturesNode)
        or (vstNode = FvstObsMf6Node)
//        or (vstNode = FvstCalibrationObsMf6Node)
        then
      begin
        // Convert vstObjects.ChildCount[Node] from Cardinal to integer to
        // prevent Interger overflow when  vstObjects.ChildCount[Node] = 0.
        ChildCount := vstObjects.ChildCount[vstNode];
        for ChildIndex := 0 to ChildCount - 1 do
        begin
          if ChildIndex = 0 then
          begin
            ChildNode := vstObjects.GetFirstChild(vstNode);
          end
          else
          begin
            ChildNode := vstObjects.GetNextSibling(ChildNode);
          end;
          UpdateChildCheck(ChildNode);
        end;
        SetRootNodeStates(vstNode);
      end
      else if (vstNode = FvstDataSetRootNode) then
      begin
        UpdateDataSetChildren(vstNode);
      end
      else
      begin
        UpdateChildCheck(vstNode);
      end;
    end;
  finally
    vstObjects.EndUpdate;
    RestoreExpandedNodes;
  end;

  if FSettingData then
  begin
    Exit;
  end;
end;

procedure TfrmCustomSelectObjects.UpdateScreenObjects;
var
  AScreenObject: TScreenObject;
  Index: Integer;
  DataSetList: TStringList;
  SortedDataSetList: TStringList;
  DrainReturn: TDrainReturn;
  ReturnLocationObject: TScreenObject;
begin
  if FSettingData then
  begin
    Exit;
  end;
  FSettingData := True;
  RecordExpandedNodes;
  try
    DataSetList := TStringList.Create;
    SortedDataSetList := TStringList.Create;
    try
      vstObjects.BeginUpdate;
      try
        CreateBaseNodes(DataSetList);
        SortedDataSetList.Assign(DataSetList);
        SortedDataSetList.Sorted := True;

        for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          AScreenObject.Tag := 0
        end;
        for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];

          if (AScreenObject.ModflowDrtBoundary <> nil)
            and AScreenObject.ModflowDrtBoundary.Used then
          begin
            DrainReturn := AScreenObject.ModflowDrtBoundary.DrainReturn;
            if DrainReturn.ReturnChoice = rtObject then
            begin
              ReturnLocationObject := DrainReturn.ReturnObject.
                ScreenObject as TScreenObject;
              if ReturnLocationObject <> nil then
              begin
                ReturnLocationObject.Tag := 1;
              end;
            end;
          end;
        end;

        for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          AddScreenObject(AScreenObject, SortedDataSetList);
        end;
        SortScreenObjectLists;
        ClearEmptyBaseNodes(DataSetList);
        SetCheckStates;
      finally
        vstObjects.EndUpdate;
      end;
    finally
      SortedDataSetList.Free;
      DataSetList.Free;
    end;
  finally
    FSettingData := False;
    RestoreExpandedNodes;
  end;
end;

procedure TfrmCustomSelectObjects.SetCheckStates;
begin
  vstObjects.BeginUpdate;
  try
    UpdateChildCheck(FvstAllObjectsNode);
    UpdateChildCheck(FvstOtherObjectsNode);
    SetRootNodeStates(FvstPhastBoundaryConditionsRoot);
    SetRootNodeStates(FvstModflowBoundaryConditionsRoot);
    SetRootNodeStates(FvstSutraFeaturesNode);
    SetRootNodeStates(FvstFootprintFeaturesNode);
    UpdateChildCheck(FvstModpathRoot);
    SetRootNodeStates(FvstDataSetRootNode);
    UpdateChildCheck(FvstChildModelNode);
    UpdateChildCheck(FvstSizeNode);
    UpdateChildCheck(FvstRefinementNode);
    SetRootNodeStates(FvstObsMf6Node);
    SetRootNodeStates(FvstCalibrationObsMf6Node);
  finally
    vstObjects.EndUpdate;
  end;
end;

procedure TfrmCustomSelectObjects.GetData;
begin
  vstObjects.BeginUpdate;
  try
    RecordExpandedNodes;
    vstObjects.Clear;
    NilBaseNodes;
    UpdateScreenObjects;
    RestoreExpandedNodes;
  finally
    vstObjects.EndUpdate;
  end;
end;

initialization
  // PVirtualNode will be cast to TObject.  This only works if they
  // are the same size.
  Assert(SizeOf(PVirtualNode) = SizeOf(TObject));

end.

