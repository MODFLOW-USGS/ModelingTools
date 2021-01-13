unit frmPestUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  JvPageList, JvExControls, Vcl.ComCtrls, JvExComCtrls, JvPageListTreeView,
  ArgusDataEntry, PestPropertiesUnit, Vcl.Buttons, Vcl.ExtCtrls, UndoItems,
  frameGridUnit, frameAvailableObjectsUnit, PestObsUnit, frameParentChildUnit,
  PestObsGroupUnit, Vcl.Mask, JvExMask, JvToolEdit,
  FluxObservationUnit, ModflowHobUnit, System.UITypes,
  System.Generics.Collections;

type
  TPestObsGroupColumn = (pogcName, pogcUseTarget, pogcTarget, pogcFileName);
  TPilotPointColumns = (ppcX, ppcY);

  TUndoPestOptions = class(TCustomUndo)
  private
    FOldPestProperties: TPestProperties;
    FNewPestProperties: TPestProperties;
    FOldObsList: TObservationObjectList;
    FNewObsList: TObservationObjectList;
    OldPestLocation: string;
    NewPestLocation: string;
    FOldFluxObsList: TFluxObservationObjectList;
    FNewFluxObsList: TFluxObservationObjectList;
    FOldHobList: THobObjectList;
    FNewHobList: THobObjectList;
  protected
    function Description: string; override;
    procedure UpdateProperties(PestProperties: TPestProperties;
      ObsList: TObservationObjectList; FluxObsList: TFluxObservationObjectList;
      HobList: THobObjectList);
  public
    constructor Create(var NewPestProperties: TPestProperties;
      var NewObsList: TObservationObjectList;
      var NewFluxObservationList: TFluxObservationObjectList;
      var NewHobList: THobObjectList;
      PestDirectory: String);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

  TfrmPEST = class(TfrmCustomGoPhast)
    tvPEST: TJvPageListTreeView;
    plMain: TJvPageList;
    jvspBasic: TJvStandardPage;
    cbPEST: TCheckBox;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    comboTemplateCharacter: TComboBox;
    lblTemplateCharacter: TLabel;
    comboFormulaMarker: TComboBox;
    lblFormulaMarker: TLabel;
    jvspControlDataMode: TJvStandardPage;
    cbSaveRestart: TCheckBox;
    lblPestMode: TLabel;
    comboPestMode: TComboBox;
    jvspDimensions: TJvStandardPage;
    rdeMaxCompDim: TRbwDataEntry;
    lblMaxCompDim: TLabel;
    rdeZeroLimit: TRbwDataEntry;
    lblZeroLimit: TLabel;
    jvspInversionControls: TJvStandardPage;
    rdeInitialLambda: TRbwDataEntry;
    lblInitialLambda: TLabel;
    rdeLambdaAdj: TRbwDataEntry;
    comboLambdaAdj: TLabel;
    rdeIterationClosure: TRbwDataEntry;
    lblIterationClosure: TLabel;
    rdeLambdaTermination: TRbwDataEntry;
    lblLambdaTermination: TLabel;
    rdeMaxLambdas: TRbwDataEntry;
    lblMaxLambdas: TLabel;
    rdeJacobianUpdate: TRbwDataEntry;
    lblJacobianUpdate: TLabel;
    cbLamForgive: TCheckBox;
    cbDerForgive: TCheckBox;
    jvspParameterAdjustmentControls: TJvStandardPage;
    rdeMaxRelParamChange: TRbwDataEntry;
    lblMaxRelParamChange: TLabel;
    rdeMaxFacParamChange: TRbwDataEntry;
    lblMaxFacParamChange: TLabel;
    rdeFactorOriginal: TRbwDataEntry;
    lblFactorOriginal: TLabel;
    rdeBoundStick: TRbwDataEntry;
    lblBoundStick: TLabel;
    cbParameterBending: TCheckBox;
    jvspInversionControls2: TJvStandardPage;
    rdeSwitchCriterion: TRbwDataEntry;
    lblSwitchCriterion: TLabel;
    rdeSwitchCount: TRbwDataEntry;
    lblSwitchCount: TLabel;
    rdeSplitSlopeCriterion: TRbwDataEntry;
    lblSplitSlopeCriterion: TLabel;
    comboAutomaticUserIntervation: TComboBox;
    lblAutomaticUserIntervation: TLabel;
    cbSensitivityReuse: TCheckBox;
    cbBoundsScaling: TCheckBox;
    jvspIterationControls: TJvStandardPage;
    rdeMaxIterations: TRbwDataEntry;
    lblMaxIterations: TLabel;
    rdePhiReductionCriterion: TRbwDataEntry;
    lblPhiReductionCriterion: TLabel;
    rdePhiReductionCount: TRbwDataEntry;
    lblPhiReductionCount: TLabel;
    rdeNoReductionCount: TRbwDataEntry;
    lblNoReductionCount: TLabel;
    rdeSmallParameterReduction: TRbwDataEntry;
    rdeSmallParameterReductionCount: TRbwDataEntry;
    lblSmallParameterReduction: TLabel;
    lblrdeSmallParameterReductionCount: TLabel;
    rdePhiStoppingThreshold: TRbwDataEntry;
    lblPhiStoppingThreshold: TLabel;
    cbLastRun: TCheckBox;
    rdeAbandon: TRbwDataEntry;
    lblAbandon: TLabel;
    jvspOutputOptions: TJvStandardPage;
    jvspSingularValueDecomp: TJvStandardPage;
    cbWriteCov: TCheckBox;
    cbWriteCorrCoef: TCheckBox;
    cbWriteEigenvectors: TCheckBox;
    cbWriteResolution: TCheckBox;
    cbWriteJacobian: TCheckBox;
    cbWriteJacobianEveryIteration: TCheckBox;
    cbWriteVerboseRunRecord: TCheckBox;
    cbWriteIntermResidualForEveryIteration: TCheckBox;
    cbSaveParamValuesIteration: TCheckBox;
    cbSaveParamValuesModelRun: TCheckBox;
    splMain: TSplitter;
    comboSvdMode: TComboBox;
    lblSvdMode: TLabel;
    rdeMaxSingularValues: TRbwDataEntry;
    lblMaxSingularValues: TLabel;
    rdeEigenThreshold: TRbwDataEntry;
    lblEigenThreshold: TLabel;
    comboEigenWrite: TComboBox;
    lblEigenWrite: TLabel;
    jvspLqsr: TJvStandardPage;
    cbUseLqsr: TCheckBox;
    rdeMatrixTolerance: TRbwDataEntry;
    lblMatrixTolerance: TLabel;
    rdeRightHandSideTolerance: TRbwDataEntry;
    lblRightHandSideTolerance: TLabel;
    rdeConditionNumberLimit: TRbwDataEntry;
    lblConditionNumberLimit: TLabel;
    rdeMaxLqsrIterations: TRbwDataEntry;
    lblMaxLqsrIterations: TLabel;
    cbWriteLsqrOutput: TCheckBox;
    jvspObservationGroups: TJvStandardPage;
    frameObservationGroups: TframeGrid;
    dlgOpenCovarianceMatrixFile: TOpenDialog;
    jvspObsGroupAssignments: TJvStandardPage;
    frameParentObsGroups: TframeParentChild;
    diredPest: TJvDirectoryEdit;
    lblPestDirectory: TLabel;
    jvspPilotPoints: TJvStandardPage;
    rdePilotPointSpacing: TRbwDataEntry;
    lblPilotPointSpacing: TLabel;
    cbShowPilotPoints: TCheckBox;
    gbIndividualPilotPoints: TGroupBox;
    framePilotPoints: TframeGrid;
    btnImportShape: TButton;
    btnImportText: TButton;
    dlgOpenPilotPoints: TOpenDialog;
    btnBetweenObservations: TButton;
    procedure FormCreate(Sender: TObject); override;
    procedure MarkerChange(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
    procedure cbUseLqsrClick(Sender: TObject);
    procedure comboSvdModeChange(Sender: TObject);
    procedure frameObservationGroupsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameObservationGroupsGridButtonClick(Sender: TObject; ACol,
      ARow: Integer);
    procedure FormDestroy(Sender: TObject); override;
    procedure plMainChange(Sender: TObject);
    procedure frameObservationGroupssbDeleteClick(Sender: TObject);
    procedure frameObservationGroupssbInsertClick(Sender: TObject);
    procedure frameObservationGroupsseNumberChange(Sender: TObject);
    procedure frameObservationGroupsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure diredPestChange(Sender: TObject);
    procedure rdeSwitchCriterionChange(Sender: TObject);
    procedure btnImportShapeClick(Sender: TObject);
    procedure btnImportTextClick(Sender: TObject);
    procedure btnBetweenObservationsClick(Sender: TObject);
//    procedure comboObsGroupChange(Sender: TObject);
  private
    FObsList: TObservationList;
    FNewObsList: TObservationObjectList;
    FFluxObservationList: TFluxObservationList;
    FNewFluxObservationList: TFluxObservationObjectList;
    FHobList: THobList;
    FNewHobList: THobObjectList;
    FLocalObsGroups: TPestObservationGroups;
    InvalidateModelEvent: TNotifyEvent;
    FGroupDictionary: TDictionary<TPestObservationGroup, TTreeNode>;
    FGroupNameDictionary: TDictionary<string, TPestObservationGroup>;
    FNoNameNode: TTreeNode;
    procedure GetData;
    procedure SetData;
    procedure FixObsGroupNames;
    procedure HandleGroupDeletion(Group: TPestObservationGroup);
    procedure HandleAddedGroup(ObsGroup: TPestObservationGroup);
    procedure CheckPestDirectory;
    procedure ImportPilotPoints(const FileName: string);
    { Private declarations }
  public
//    procedure btnOK1Click(Sender: TObject);

    { Public declarations }
  end;

var
  frmPEST: TfrmPEST;

const StrNone = '(none)';

implementation

uses
  frmGoPhastUnit, GoPhastTypes, RbwDataGrid4, JvComCtrls, PhastModelUnit,
  PointCollectionUnit, QuadTreeClass, ShapefileUnit, System.IOUtils, FastGEO,
  ModelMuseUtilities, ScreenObjectUnit, TriCP_Routines, TriPackRoutines,
  SutraPestObsUnit;

resourcestring
  StrObservationGroupNa = 'Observation Group Name (OBGNME)';
  StrUseGroupTargetGT = 'Use Group Target (GTARG)';
  StrGroupTargetGTARG = 'Group Target (GTARG)';
  StrCovarianceMatrixFi = 'Covariance Matrix File Name (optional) (COVFLE)';
  StrTheShapeHeaderFil = 'The shape header file "%s" could not be found.';
  StrLine0d1sM = 'Line %0:d ("%1:s") must contain at least two values separa' +
  'ted by a comma or space character.';

{$R *.dfm}

procedure TfrmPEST.MarkerChange(Sender: TObject);
begin
  inherited;
  if comboTemplateCharacter.Text = comboFormulaMarker.Text then
  begin
    comboTemplateCharacter.Color := clRed;
    comboFormulaMarker.Color := clRed;
    Beep;
  end
  else
  begin
    comboTemplateCharacter.Color := clWindow;
    comboFormulaMarker.Color := clWindow;
  end;
end;

procedure TfrmPEST.plMainChange(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  RowIndex: Integer;
begin
  inherited;
  if plMain.ActivePage = jvspObservationGroups then
  begin
    FixObsGroupNames;
//    comboObsGroup.Items.Clear;
//    comboObsGroup.Items.Capacity := frameObservationGroups.seNumber.AsInteger;
    Grid := frameObservationGroups.Grid;
    for RowIndex := 1 to frameObservationGroups.seNumber.AsInteger do
    begin
      if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
      begin
//        comboObsGroup.Items.AddObject(Grid.Cells[Ord(pogcName), RowIndex],
//          Grid.Objects[Ord(pogcName), RowIndex]);
      end;
    end;
  end;
end;

procedure TfrmPEST.rdeSwitchCriterionChange(Sender: TObject);
begin
  inherited;
  if rdeSwitchCriterion.RealValue = 0 then
  begin
    rdeSwitchCriterion.Color := clRed;
  end
  else
  begin
    rdeSwitchCriterion.Color := clWindow;
  end;
end;

procedure TfrmPEST.btnBetweenObservationsClick(Sender: TObject);
var
  ScreenObjectIndex: Integer;
  PhastModel: TPhastModel;
  AScreenObject: TScreenObject;
  UsePoint: Boolean;
  QuadTree: TRbwQuadTree;
  DisLimits: TGridLimit;
  PointList: TList<TPoint2D>;
  XD: TRealArray;
  YD: TRealArray;
  NT: longint; 
  NL: longint;
  IPT: TIntArray; 
  IPL: TIntArray;
  ResultPointList: TList<TPoint2D>;
  APoint2D: TPoint2D;
  DupPoint: TPoint2D;
  PointIndex: Integer;
  APoint2D_1: TPoint2D;
  APoint2D_2: TPoint2D;
  SutraStateObs: TSutraStateObservations;
  APointer: Pointer;
  LineIndex: Integer;
begin
  inherited;
  //
  PhastModel := frmGoPhast.PhastModel;
  PointList := TList<TPoint2D>.Create;
  ResultPointList := TList<TPoint2D>.Create;
  QuadTree := TRbwQuadTree.Create(nil);
  try
    DisLimits := PhastModel.DiscretizationLimits(vdTop);
    QuadTree.XMax := DisLimits.MaxX;
    QuadTree.XMin := DisLimits.MinX;
    QuadTree.YMax := DisLimits.MaxY;
    QuadTree.YMin := DisLimits.MinY;
    for ScreenObjectIndex := 0 to PhastModel.ScreenObjectCount - 1 do
    begin
      AScreenObject := PhastModel.ScreenObjects[ScreenObjectIndex];
      if AScreenObject.Deleted or (AScreenObject.Count > 1)
        or (AScreenObject.Modflow6Obs = nil)
        or not AScreenObject.Modflow6Obs.Used then
      begin
        Continue;
      end;

      UsePoint := False;
      if (PhastModel.ModelSelection in ModflowSelection) then
      begin
        if PhastModel.ModelSelection = msModflow2015 then
        begin
          UsePoint := AScreenObject.Modflow6Obs.CalibrationObservations.Count > 0;
        end
        else
        begin
          if PhastModel.ModflowPackages.Mnw2Package.IsSelected then
          begin
            if (AScreenObject.ModflowMnw2Boundary <> nil)
              and AScreenObject.ModflowMnw2Boundary.Used
              and (AScreenObject.ModflowMnw2Boundary.Observations.Count > 0) then
            begin
              UsePoint := True;
            end;
          end;

          if PhastModel.ModflowPackages.HobPackage.IsSelected then
          begin
            if (AScreenObject.ModflowHeadObservations <> nil)
              and AScreenObject.ModflowHeadObservations.Used then
            begin
              UsePoint := True;
            end;
          end;
        end;
      end;
      if (PhastModel.ModelSelection in SutraSelection) then
      begin
        SutraStateObs := AScreenObject.SutraBoundaries.SutraStateObs;
        if SutraStateObs.Used and (SutraStateObs.Count > 0) then
        begin
          UsePoint := True;
        end;
      end;

      if UsePoint then
      begin
        APoint2D := AScreenObject.Points[0];
        if PointList.Count = 0 then
        begin
          PointList.Add(APoint2D);
          QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
        end
        else
        begin
          DupPoint := APoint2D;
          QuadTree.FirstNearestPoint(DupPoint.x, DupPoint.y, APointer);
          if (DupPoint.x <> APoint2D.x) or (DupPoint.y <> APoint2D.y) then
          begin
            PointList.Add(APoint2D);
            QuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
          end;
        end;
      end;
    end;
    
    if PointList.Count = 0 then
    begin
      Beep;
      MessageDlg('No observation points defined.', mtError, [mbOK], 0);
    end;

    if PointList.Count < 4 then
    begin

    end
    else
    begin
      SetLength(XD, PointList.Count);
      SetLength(YD, PointList.Count);
      for PointIndex := 0 to PointList.Count -1 do
      begin
        APoint2D := PointList[PointIndex];
        XD[PointIndex] := APoint2D.x;
        YD[PointIndex] := APoint2D.y;
      end;
      SetLength(IPT, 6*PointList.Count-15);
      SetLength(IPL, 6*PointList.Count);
      IDTANG_Pascal(PointList.Count, XD,YD, NT, NL, IPT, IPL);

      for LineIndex := 0 to NL - 1 do
      begin
        PointIndex := IPL[LineIndex*3];
        APoint2D_1 := PointList[PointIndex];
        PointIndex := IPL[LineIndex*3+1];
        APoint2D_2 := PointList[PointIndex];
        APoint2D.x := (APoint2D_1.x + APoint2D_2.x) / 2;
        APoint2D.y := (APoint2D_1.y + APoint2D_2.y) / 2;
        ResultPointList.Add(APoint2D);
      end;
    end;
  finally
    PointList.Free;
    ResultPointList.Free;
    QuadTree.Free;
  end;
end;

procedure TfrmPEST.btnImportShapeClick(Sender: TObject);
begin
  inherited;
  dlgOpenPilotPoints.FilterIndex := 1;
  if dlgOpenPilotPoints.Execute then
  begin
    ImportPilotPoints(dlgOpenPilotPoints.FileName);
  end;
end;

procedure TfrmPEST.btnImportTextClick(Sender: TObject);
begin
  inherited;
  dlgOpenPilotPoints.FilterIndex := 2;
  if dlgOpenPilotPoints.Execute then
  begin
    ImportPilotPoints(dlgOpenPilotPoints.FileName);
  end;
end;

procedure TfrmPEST.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;

end;

procedure TfrmPEST.cbUseLqsrClick(Sender: TObject);
begin
  inherited;
  if cbUseLqsr.Checked then
  begin
    comboSvdMode.ItemIndex := 0;
  end;
end;

//procedure TfrmPEST.comboObsGroupChange(Sender: TObject);
//var
//  AName: string;
//  ObsIndex: Integer;
//  AnObs: TCustomObservationItem;
//begin
//  inherited;
//  if comboObsGroup.ItemIndex >= 0 then
//  begin
//    AName := comboObsGroup.Text;
//    frameObsGroupAssignments.lbSrcObjects.Items.BeginUpdate;
//    frameObsGroupAssignments.lbDstObjects.Items.BeginUpdate;
//    try
//      frameObsGroupAssignments.lbSrcObjects.Items.Clear;
//      frameObsGroupAssignments.lbDstObjects.Items.Clear;
//      for ObsIndex := 0 to FNewObsList.Count - 1 do
//      begin
//        AnObs := FNewObsList[ObsIndex];
//        if AnObs.ObservationGroup = AName then
//        begin
//
//        end;
//      end;
//    finally
//      frameObsGroupAssignments.lbDstObjects.Items.EndUpdate;
//      frameObsGroupAssignments.lbSrcObjects.Items.EndUpdate;
//    end;
//  end;
//end;

procedure TfrmPEST.comboSvdModeChange(Sender: TObject);
begin
  inherited;
  if comboSvdMode.ItemIndex > 0 then
  begin
    cbUseLqsr.Checked := False;
  end;
end;

procedure TfrmPEST.diredPestChange(Sender: TObject);
begin
  inherited;
  CheckPestDirectory;
end;

procedure TfrmPEST.FormCreate(Sender: TObject);
var
  NewNode: TJvPageIndexNode;
  ControlDataNode: TJvPageIndexNode;
  ObservationNode: TJvPageIndexNode;
begin
  inherited;
  FObsList := TObservationList.Create;
  FNewObsList := TObservationObjectList.Create;

  FFluxObservationList := TFluxObservationList.Create;
  FNewFluxObservationList := TFluxObservationObjectList.Create;

  FHobList :=  THobList.Create;
  FNewHobList := THobObjectList.Create;

  InvalidateModelEvent := nil;
  FLocalObsGroups := TPestObservationGroups.Create(nil);
  FGroupDictionary := TDictionary<TPestObservationGroup, TTreeNode>.Create;
  FGroupNameDictionary := TDictionary<string, TPestObservationGroup>.Create;

  NewNode := tvPEST.Items.AddChild(
    nil, 'Basic') as TJvPageIndexNode;
  NewNode.PageIndex := jvspBasic.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, 'Pilot Points') as TJvPageIndexNode;
  NewNode.PageIndex := jvspPilotPoints.PageIndex;

  ControlDataNode := tvPEST.Items.AddChild(
    nil, 'Control Data') as TJvPageIndexNode;
  ControlDataNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Mode') as TJvPageIndexNode;
  NewNode.PageIndex := jvspControlDataMode.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Dimensions') as TJvPageIndexNode;
  NewNode.PageIndex := jvspDimensions.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Inversion Controls 1') as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Parameter Adjustment Controls') as TJvPageIndexNode;
  NewNode.PageIndex := jvspParameterAdjustmentControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Inversion Controls 2') as TJvPageIndexNode;
  NewNode.PageIndex := jvspInversionControls2.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Iteration Controls') as TJvPageIndexNode;
  NewNode.PageIndex := jvspIterationControls.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ControlDataNode, 'Output') as TJvPageIndexNode;
  NewNode.PageIndex := jvspOutputOptions.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, 'Singular Value Decomposition') as TJvPageIndexNode;
  NewNode.PageIndex := jvspSingularValueDecomp.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    nil, 'LQSR') as TJvPageIndexNode;
  NewNode.PageIndex := jvspLqsr.PageIndex;

  ObservationNode := tvPEST.Items.AddChild(
    nil, 'Observations') as TJvPageIndexNode;
  ControlDataNode.PageIndex := -1;

  NewNode := tvPEST.Items.AddChild(
    ObservationNode, 'Observation Groups') as TJvPageIndexNode;
  NewNode.PageIndex := jvspObservationGroups.PageIndex;

  NewNode := tvPEST.Items.AddChild(
    ObservationNode, 'Observation Group Assignments') as TJvPageIndexNode;
  NewNode.PageIndex := jvspObsGroupAssignments.PageIndex;

  plMain.ActivePageIndex := 0;

  framePilotPoints.Grid.Cells[Ord(ppcX), 0] := 'X';
  framePilotPoints.Grid.Cells[Ord(ppcY), 0] := 'Y';

  GetData;
end;

procedure TfrmPEST.FormDestroy(Sender: TObject);
begin
  inherited;
  FGroupNameDictionary.Free;
  FGroupDictionary.Free;
  FLocalObsGroups.Free;

  FHobList.Free;
  FNewHobList.Free;

  FFluxObservationList.Free;
  FNewFluxObservationList.Free;

  FObsList.Free;
  FNewObsList.Free;
end;

procedure TfrmPEST.frameObservationGroupsGridButtonClick(Sender: TObject; ACol,
  ARow: Integer);
begin
  inherited;
  dlgOpenCovarianceMatrixFile.FileName :=
    frameObservationGroups.Grid.Cells[ACol, ARow];
  if dlgOpenCovarianceMatrixFile.Execute then
  begin
    frameObservationGroups.Grid.Cells[ACol, ARow] :=
      dlgOpenCovarianceMatrixFile.FileName;
  end;
end;

procedure TfrmPEST.frameObservationGroupsGridSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow > 0) and (ACol = Ord(pogcTarget)) then
  begin
    CanSelect := frameObservationGroups.Grid.Checked[Ord(pogcUseTarget), ARow];
  end;
end;

procedure TfrmPEST.frameObservationGroupsGridSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Grid: TRbwDataGrid4;
  Group: TPestObservationGroup;
  OtherGroup: TPestObservationGroup;
  TreeNode: TTreeNode;
begin
  inherited;
  Grid := frameObservationGroups.Grid;
  if (ARow >= Grid.FixedRows) and (ACol = Ord(pogcName)) then
  begin
    Group := Grid.Objects[ACol, ARow] as TPestObservationGroup;
    if Group <> nil then
    begin
      if FGroupNameDictionary.TryGetValue(UpperCase(Group.ObsGroupName), OtherGroup) then
      begin
        if Group = OtherGroup then
        begin
          FGroupNameDictionary.Remove(UpperCase(Group.ObsGroupName));
        end;
      end;
      Group.ObsGroupName := ValidObsGroupName(Value);
      if Group.ObsGroupName <> '' then
      begin
        if not FGroupNameDictionary.ContainsKey(UpperCase(Group.ObsGroupName)) then
        begin
          FGroupNameDictionary.Add(UpperCase(Group.ObsGroupName), Group)
        end;
      end;

      if FGroupDictionary.TryGetValue(Group, TreeNode) then
      begin
        TreeNode.Text := Group.ObsGroupName;
      end;
    end;
  end;
end;

procedure TfrmPEST.frameObservationGroupssbDeleteClick(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  Group: TPestObservationGroup;
begin
  inherited;
  Grid := frameObservationGroups.Grid;
  if Grid.SelectedRow >= Grid.FixedRows  then
  begin
    if Grid.Objects[Ord(pogcName), Grid.SelectedRow] <> nil then
    begin
      Group := Grid.Objects[Ord(pogcName), Grid.SelectedRow] as TPestObservationGroup;
      HandleGroupDeletion(Group);
      Group.Free;
      Grid.Objects[Ord(pogcName), Grid.SelectedRow] := nil;
    end;
  end;

  frameObservationGroups.sbDeleteClick(Sender);

end;

procedure TfrmPEST.frameObservationGroupssbInsertClick(Sender: TObject);
var
  NewGroup: TPestObservationGroup;
  Grid: TRbwDataGrid4;
begin
  inherited;
  Grid := frameObservationGroups.Grid;
  NewGroup := nil;
  if Grid.SelectedRow >= Grid.FixedRows then
  begin
    NewGroup := FLocalObsGroups.Add;
  end;
  frameObservationGroups.sbInsertClick(Sender);
  if NewGroup <> nil then
  begin
    Grid.Objects[Ord(pogcName), Grid.SelectedRow] := NewGroup;
    NewGroup.Index := Grid.SelectedRow -1;
    HandleAddedGroup(NewGroup);
  end;

end;

procedure TfrmPEST.frameObservationGroupsseNumberChange(Sender: TObject);
var
  Grid: TRbwDataGrid4;
  NewGroup: TPestObservationGroup;
  Names: TStrings;
  OldGroup: TPestObservationGroup;
  index: Integer;
begin
  inherited;
  Grid := frameObservationGroups.Grid;
  Names := Grid.Cols[Ord(pogcName)];
  frameObservationGroups.seNumberChange(Sender);
  while frameObservationGroups.seNumber.AsInteger > FLocalObsGroups.Count do
  begin
    NewGroup := FLocalObsGroups.Add;
    Grid.Objects[Ord(pogcName), FLocalObsGroups.Count] := NewGroup;
    NewGroup.ObsGroupName := ValidObsGroupName(
      Grid.Cells[Ord(pogcName), FLocalObsGroups.Count]);
    HandleAddedGroup(NewGroup);
  end;
  while frameObservationGroups.seNumber.AsInteger < FLocalObsGroups.Count do
  begin
    OldGroup := FLocalObsGroups.Last as TPestObservationGroup;
    index := Names.IndexOfObject(OldGroup);
    HandleGroupDeletion(OldGroup);
    OldGroup.Free;
    if index >= 1 then
    begin
      Grid.Objects[Ord(pogcName),index] := nil;
    end;
  end;

end;

procedure TfrmPEST.GetData;
var
  PestProperties: TPestProperties;
  PestControlData: TPestControlData;
  SvdProperties: TSingularValueDecompositionProperties;
  LsqrProperties: TLsqrProperties;
  Grid: TRbwDataGrid4;
  ObsGroups: TPestObservationGroups;
  ItemIndex: Integer;
  ObsGroup: TPestObservationGroup;
  index: Integer;
  AnObs: TCustomObservationItem;
  ATempObs: TCustomObservationItem;
  Tree: TTreeView;
  NewNode: TTreeNode;
  GroupIndex: Integer;
  TreeNode: TTreeNode;
  Locations: TProgramLocations;
  FluxObs: TFluxObservationGroup;
  ATempFluxObs: TFluxObservationGroup;
  HobItem: THobItem;
  FNewHobItem: THobItem;
  PointItem: TPointItem;
//  InvalidateModelEvent: TNotifyEvent;
begin
  Locations := frmGoPhast.PhastModel.ProgramLocations;

  PestProperties := frmGoPhast.PhastModel.PestProperties;

  {$REGION 'PEST Basic'}
  cbPEST.Checked := PestProperties.PestUsed;
  comboTemplateCharacter.ItemIndex :=
    comboTemplateCharacter.Items.IndexOf(PestProperties.TemplateCharacter);

  comboFormulaMarker.ItemIndex :=
    comboFormulaMarker.Items.IndexOf(PestProperties.ExtendedTemplateCharacter);

  diredPest.Text := Locations.PestDirectory;
  CheckPestDirectory;
  {$ENDREGION}

  {$REGION 'Pilot Points'}
  cbShowPilotPoints.Checked := PestProperties.ShowPilotPoints;
  rdePilotPointSpacing.RealValue := PestProperties.PilotPointSpacing;
  ClearGrid(framePilotPoints.Grid);
  framePilotPoints.seNumber.AsInteger := PestProperties.SpecifiedPilotPoints.Count;
  framePilotPoints.Grid.BeginUpdate;
  try
    for ItemIndex := 0 to PestProperties.SpecifiedPilotPoints.Count - 1 do
    begin
      PointItem := PestProperties.SpecifiedPilotPoints.Items[ItemIndex] as TPointItem;
      framePilotPoints.Grid.RealValue[Ord(ppcX), ItemIndex+1] := PointItem.X;
      framePilotPoints.Grid.RealValue[Ord(ppcy), ItemIndex+1] := PointItem.Y;
    end;
  finally
    framePilotPoints.Grid.EndUpdate;
  end;
  framePilotPoints.seNumber.AsInteger := PestProperties.SpecifiedPilotPoints.Count;

  {$ENDREGION}


  {$REGION 'Control Data'}
  PestControlData := PestProperties.PestControlData;
  cbSaveRestart.Checked := Boolean(PestControlData.PestRestart);
  comboPestMode.ItemIndex := Ord(PestControlData.PestMode);
  {$ENDREGION}

  {$REGION 'Dimensions'}
  rdeMaxCompDim.IntegerValue := PestControlData.MaxCompressionDimension;
  rdeZeroLimit.RealValue  := PestControlData.ZeroLimit;
  {$ENDREGION}

  {$REGION 'Inversion Controls'}
  rdeInitialLambda.RealValue  := PestControlData.InitalLambda;
  rdeLambdaAdj.RealValue  := PestControlData.LambdaAdjustmentFactor;
  rdeIterationClosure.RealValue  := PestControlData.PhiRatioSufficient;
  rdeLambdaTermination.RealValue  := PestControlData.PhiReductionLambda;
  rdeMaxLambdas.IntegerValue := PestControlData.NumberOfLambdas;
  rdeJacobianUpdate.IntegerValue := PestControlData.JacobianUpdate;
  cbLamForgive.Checked := Boolean(PestControlData.LambdaForgive);
  cbDerForgive.Checked := Boolean(PestControlData.DerivedForgive);
  {$ENDREGION}

  {$REGION 'Parameter Adjustment Controls'}
  rdeMaxRelParamChange.RealValue  := PestControlData.RelativeMaxParamChange;
  rdeMaxFacParamChange.RealValue  := PestControlData.FactorMaxParamChange;
  rdeFactorOriginal.RealValue  := PestControlData.FactorOriginal;
  rdeBoundStick.IntegerValue  := PestControlData.BoundStick;
  cbParameterBending.Checked := Boolean(PestControlData.UpgradeParamVectorBending);
  {$ENDREGION}

  {$REGION 'Inversion Controls 2'}
  rdeSwitchCriterion.RealValue := PestControlData.SwitchCriterion;
  rdeSwitchCriterionChange(nil);
  rdeSwitchCount.IntegerValue := PestControlData.OptSwitchCount;
  rdeSplitSlopeCriterion.RealValue := PestControlData.SplitSlopeCriterion;
  comboAutomaticUserIntervation.ItemIndex := Ord(PestControlData.AutomaticUserIntervation);
  cbSensitivityReuse.Checked := Boolean(PestControlData.SensitivityReuse);
  cbBoundsScaling.Checked := Boolean(PestControlData.Boundscaling);
  {$ENDREGION}

  {$REGION 'Iteration Controls'}
  rdeMaxIterations.IntegerValue := PestControlData.MaxIterations;
  rdePhiReductionCriterion.RealValue := PestControlData.SlowConvergenceCriterion;
  rdePhiReductionCount.IntegerValue := PestControlData.SlowConvergenceCountCriterion;
  rdeNoReductionCount.IntegerValue := PestControlData.ConvergenceCountCriterion;
  rdeSmallParameterReduction.RealValue := PestControlData.ParameterChangeConvergenceCriterion;
  rdeSmallParameterReductionCount.IntegerValue := PestControlData.ParameterChangeConvergenceCount;
  rdePhiStoppingThreshold.RealValue := PestControlData.ObjectiveCriterion;
  cbLastRun.Checked := Boolean(PestControlData.MakeFinalRun);
  rdeAbandon.RealValue := PestControlData.PhiAbandon;
  {$ENDREGION}

  {$REGION 'Output Controls'}
  cbWriteCov.Checked := Boolean(PestControlData.WriteCovariance);
  cbWriteCorrCoef.Checked := Boolean(PestControlData.WriteCorrelations);
  cbWriteEigenvectors.Checked := Boolean(PestControlData.WriteEigenVectors);
  cbWriteResolution.Checked := Boolean(PestControlData.SaveResolution);
  cbWriteJacobian.Checked := Boolean(PestControlData.SaveJacobian);
  cbWriteJacobianEveryIteration.Checked := Boolean(PestControlData.SaveJacobianIteration);
  cbWriteVerboseRunRecord.Checked := Boolean(PestControlData.VerboseRecord);
  cbWriteIntermResidualForEveryIteration.Checked := Boolean(PestControlData.SaveInterimResiduals);
  cbSaveParamValuesIteration.Checked := Boolean(PestControlData.SaveParamIteration);
  cbSaveParamValuesModelRun.Checked := Boolean(PestControlData.SaveParamRun);
  {$ENDREGION}

  {$REGION 'Singular Value Decomposition'}
  SvdProperties := PestProperties.SvdProperties;
  comboSvdMode.ItemIndex := Ord(SvdProperties.Mode);
  rdeMaxSingularValues.IntegerValue := SvdProperties.MaxSingularValues;
  rdeEigenThreshold.RealValue := SvdProperties.EigenThreshold;
  comboEigenWrite.ItemIndex := Ord(SvdProperties.EigenWrite);
  {$ENDREGION}

  {$REGION 'LQSR'}
  LsqrProperties := PestProperties.LsqrProperties;
  cbUseLqsr.Checked := Boolean(LsqrProperties.Mode);
  rdeMatrixTolerance.RealValue := LsqrProperties.MatrixTolerance;
  rdeRightHandSideTolerance.RealValue := LsqrProperties.RightHandSideTolerance;
  rdeConditionNumberLimit.RealValue := LsqrProperties.ConditionNumberLimit;
  rdeMaxLqsrIterations.IntegerValue := LsqrProperties.MaxIteration;
  cbWriteLsqrOutput.Checked := Boolean(LsqrProperties.LsqrWrite);
  {$ENDREGION}

  {$REGION 'Observation Groups'}
  ObsGroups := nil;
  Grid := frameObservationGroups.Grid;
  Grid.BeginUpdate;
  try
    Grid.Cells[Ord(pogcName), 0] := StrObservationGroupNa;
    Grid.Cells[Ord(pogcUseTarget), 0] := StrUseGroupTargetGT;
    Grid.Cells[Ord(pogcTarget), 0] := StrGroupTargetGTARG;
    Grid.Cells[Ord(pogcFileName), 0] := StrCovarianceMatrixFi;

    ObsGroups := PestProperties.ObservationGroups;
    FLocalObsGroups.Assign(ObsGroups);
    frameObservationGroups.seNumber.AsInteger := FLocalObsGroups.Count;
    for ItemIndex := 0 to FLocalObsGroups.Count - 1 do
    begin
      ObsGroup := FLocalObsGroups[ItemIndex];
      Grid.Objects[Ord(pogcName), ItemIndex+1] := ObsGroup;
      Grid.Cells[Ord(pogcName), ItemIndex+1] := ObsGroup.ObsGroupName;
      Grid.Checked[Ord(pogcUseTarget), ItemIndex+1] := ObsGroup.UseGroupTarget;
      Grid.RealValue[Ord(pogcTarget), ItemIndex+1] :=
        ObsGroup.GroupTarget;
      Grid.Cells[Ord(pogcFileName), ItemIndex+1] :=
        ObsGroup.AbsoluteCorrelationFileName;
    end;

  finally
    Grid.EndUpdate;
    if ObsGroups <> nil then
    begin
      frameObservationGroups.seNumber.AsInteger := ObsGroups.Count;
    end;
  end;
  {$ENDREGION}

  {$REGION 'Observation Group Assignments'}
  FGroupDictionary.Clear;
  FGroupNameDictionary.Clear;
  Tree := frameParentObsGroups.tvTree;
  Tree.Items.Clear;
  FNoNameNode := Tree.Items.AddChild(nil, StrNone);
  for GroupIndex := 0 to FLocalObsGroups.Count - 1 do
  begin
    ObsGroup := FLocalObsGroups[GroupIndex];
    HandleAddedGroup(ObsGroup);
  end;

  frmGoPhast.PhastModel.FillObsItemList(FObsList, True);
  FNewObsList.Capacity := FObsList.Count;
  for index := 0 to FObsList.Count - 1 do
  begin
    AnObs := FObsList[index];
    ATempObs := TCustomObservationItem.Create(nil);
    FNewObsList.Add(ATempObs);
    ATempObs.Assign(AnObs);
    if FGroupNameDictionary.TryGetValue(UpperCase(ATempObs.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, ATempObs.Name);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, ATempObs.Name);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, ATempObs.Name);
    end;
    NewNode.Data := ATempObs;
  end;

  frmGoPhast.PhastModel.FillFluxObsList(FFluxObservationList);
  FNewFluxObservationList.Capacity := FFluxObservationList.Count;
  for index := 0 to FFluxObservationList.Count - 1 do
  begin
    FluxObs := FFluxObservationList[index];
    ATempFluxObs := TFluxObservationGroup.Create(nil);
    FNewFluxObservationList.Add(ATempFluxObs);
    ATempFluxObs.Assign(FluxObs);
    if FGroupNameDictionary.TryGetValue(UpperCase(ATempFluxObs.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, ATempFluxObs.ObservationName);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, ATempFluxObs.ObservationName);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, ATempFluxObs.ObservationName);
    end;
    NewNode.Data := ATempFluxObs;
  end;

  frmGoPhast.PhastModel.FillHobList(FHobList);
  FNewHobList.Capacity := FHobList.Count;
  for index := 0 to FHobList.Count - 1 do
  begin
    HobItem := FHobList[index];
    FNewHobItem := THobItem.Create(nil);
    FNewHobList.Add(FNewHobItem);
    FNewHobItem.Assign(HobItem);
    if FGroupNameDictionary.TryGetValue(UpperCase(FNewHobItem.ObservationGroup), ObsGroup) then
    begin
      if FGroupDictionary.TryGetValue(ObsGroup, TreeNode) then
      begin
        NewNode := Tree.Items.AddChild(TreeNode, FNewHobItem.Name);
      end
      else
      begin
        NewNode := Tree.Items.AddChild(FNoNameNode, FNewHobItem.Name);
      end;
    end
    else
    begin
      NewNode := Tree.Items.AddChild(FNoNameNode, FNewHobItem.Name);
    end;
    NewNode.Data := FNewHobItem;
  end;
  {$ENDREGION}

end;

procedure TfrmPEST.SetData;
var
  PestProperties: TPestProperties;
  InvalidateModelEvent: TNotifyEvent;
  PestControlData: TPestControlData;
  SvdProperties: TSingularValueDecompositionProperties;
  LsqrProperties: TLsqrProperties;
  RowIndex: Integer;
  Grid: TRbwDataGrid4;
  ObsGroups: TPestObservationGroups;
  AnObsGroup: TPestObservationGroup;
  ANode: TTreeNode;
  ObsGroup: TPestObservationGroup;
  ChildNode: TTreeNode;
  AnObs: TCustomObservationItem;
  AnObject: TObject;
  FluxObs: TFluxObservationGroup;
  HobItem: THobItem;
  PointItem: TPointItem;
  ItemIndex: Integer;
begin
  InvalidateModelEvent := nil;
  PestProperties := TPestProperties.Create(nil);
  try

    {$REGION 'PEST Basics'}
    PestProperties.PestUsed := cbPEST.Checked;
    if comboTemplateCharacter.Text <> '' then
    begin
      PestProperties.TemplateCharacter := comboTemplateCharacter.Text[1];
    end;
    if comboFormulaMarker.Text <> '' then
    begin
      PestProperties.ExtendedTemplateCharacter := comboFormulaMarker.Text[1];
    end;
    {$ENDREGION}

    {$REGION 'Pilot Points'}
    PestProperties.ShowPilotPoints := cbShowPilotPoints.Checked;
    PestProperties.PilotPointSpacing := rdePilotPointSpacing.RealValue;

    PestProperties.SpecifiedPilotPoints.Capacity :=
      framePilotPoints.seNumber.AsInteger;
    Grid := framePilotPoints.Grid;
    for ItemIndex := 0 to framePilotPoints.seNumber.AsInteger - 1 do
    begin
      if (Grid.Cells[Ord(ppcX), ItemIndex+1] <> '')
        and (Grid.Cells[Ord(ppcY), ItemIndex+1] <> '') then
      begin
        PointItem := PestProperties.SpecifiedPilotPoints.Add as TPointItem;
        PointItem.X := Grid.RealValue[Ord(ppcX), ItemIndex+1];
        PointItem.Y := Grid.RealValue[Ord(ppcY), ItemIndex+1];
      end;
    end;
    {$ENDREGION}


    {$REGION 'Control Data'}
    PestControlData := PestProperties.PestControlData;
    PestControlData.PestRestart := TPestRestart(cbSaveRestart.Checked);
    PestControlData.PestMode := TPestMode(comboPestMode.ItemIndex);
    {$ENDREGION}

    {$REGION 'Dimensions'}
    if rdeMaxCompDim.Text <> '' then
    begin
      PestControlData.MaxCompressionDimension := rdeMaxCompDim.IntegerValue;
    end;
    if rdeZeroLimit.Text <> '' then
    begin
      PestControlData.ZeroLimit := rdeZeroLimit.RealValue;
    end;
    {$ENDREGION}

    {$REGION 'Inversion Controls'}
    if rdeInitialLambda.Text <> '' then
    begin
      PestControlData.InitalLambda := rdeInitialLambda.RealValue;
    end;
    if rdeLambdaAdj.Text <> '' then
    begin
      PestControlData.LambdaAdjustmentFactor := rdeLambdaAdj.RealValue;
    end;
    if rdeIterationClosure.Text <> '' then
    begin
      PestControlData.PhiRatioSufficient := rdeIterationClosure.RealValue;
    end;
    if rdeLambdaTermination.Text <> '' then
    begin
      PestControlData.PhiReductionLambda := rdeLambdaTermination.RealValue;
    end;
    if rdeMaxLambdas.Text <> '' then
    begin
      PestControlData.NumberOfLambdas := rdeMaxLambdas.IntegerValue;
    end;
    if rdeJacobianUpdate.Text <> '' then
    begin
      PestControlData.JacobianUpdate := rdeJacobianUpdate.IntegerValue;
    end;
   PestControlData.LambdaForgive := TLambdaForgive(cbLamForgive.Checked);
   PestControlData.DerivedForgive := TDerivedForgive(cbDerForgive.Checked);
   {$ENDREGION}

    {$REGION 'Parameter Adjustment Controls'}
    if rdeMaxRelParamChange.Text <> '' then
    begin
      PestControlData.RelativeMaxParamChange :=  rdeMaxRelParamChange.RealValue;
    end;
    if rdeMaxFacParamChange.Text <> '' then
    begin
      PestControlData.FactorMaxParamChange :=  rdeMaxFacParamChange.RealValue;
    end;
    if rdeFactorOriginal.Text <> '' then
    begin
      PestControlData.FactorOriginal :=  rdeFactorOriginal.RealValue;
    end;
    if rdeBoundStick.Text <> '' then
    begin
      PestControlData.BoundStick :=  rdeBoundStick.IntegerValue;
    end;
   PestControlData.UpgradeParamVectorBending :=
     TUpgradeParamVectorBending(cbLamForgive.Checked);
   {$ENDREGION}

    {$REGION 'Inversion Controls 2'}
    if rdeSwitchCriterion.Text <> '' then
    begin
      PestControlData.SwitchCriterion := rdeSwitchCriterion.RealValue;
    end;
    if rdeSwitchCount.Text <> '' then
    begin
      PestControlData.OptSwitchCount := rdeSwitchCount.IntegerValue;
    end;
    if rdeSplitSlopeCriterion.Text <> '' then
    begin
      PestControlData.SplitSlopeCriterion := rdeSplitSlopeCriterion.RealValue;
    end;
    PestControlData.AutomaticUserIntervation := TAutomaticUserIntervation(comboAutomaticUserIntervation.ItemIndex);
    PestControlData.SensitivityReuse := TSensitivityReuse(cbSensitivityReuse.Checked);
    PestControlData.Boundscaling := TBoundsScaling(cbBoundsScaling.Checked);
    {$ENDREGION}

    {$REGION 'Iteration Controls'}
    if rdeMaxIterations.Text <> '' then
    begin
      PestControlData.MaxIterations := rdeMaxIterations.IntegerValue;
    end;
    if rdePhiReductionCriterion.Text <> '' then
    begin
      PestControlData.SlowConvergenceCriterion := rdePhiReductionCriterion.RealValue;
    end;
    if rdePhiReductionCount.Text <> '' then
    begin
      PestControlData.SlowConvergenceCountCriterion := rdePhiReductionCount.IntegerValue;
    end;
    if rdeNoReductionCount.Text <> '' then
    begin
      PestControlData.ConvergenceCountCriterion := rdeNoReductionCount.IntegerValue;
    end;
    if rdeSmallParameterReduction.Text <> '' then
    begin
      PestControlData.ParameterChangeConvergenceCriterion := rdeSmallParameterReduction.RealValue;
    end;
    if rdeSmallParameterReductionCount.Text <> '' then
    begin
      PestControlData.ParameterChangeConvergenceCount := rdeSmallParameterReductionCount.IntegerValue;
    end;
    if rdePhiStoppingThreshold.Text <> '' then
    begin
      PestControlData.ObjectiveCriterion := rdePhiStoppingThreshold.RealValue;
    end;
    PestControlData.MakeFinalRun := TMakeFinalRun(cbLastRun.Checked);
    if rdeAbandon.Text <> '' then
    begin
      PestControlData.PhiAbandon := rdeAbandon.RealValue;
    end;
    {$ENDREGION}

    {$REGION 'Output Options'}
    PestControlData.WriteCovariance := TWriteMatrix(cbWriteCov.Checked);
    PestControlData.WriteCorrelations := TWriteMatrix(cbWriteCorrCoef.Checked);
    PestControlData.WriteEigenVectors := TWriteMatrix(cbWriteEigenvectors.Checked);
    PestControlData.SaveResolution := TSaveResolution(cbWriteResolution.Checked);
    PestControlData.SaveJacobian := TSaveJacobian(cbWriteJacobian.Checked);
    PestControlData.SaveJacobianIteration :=
      TSaveJacobianIteration(cbWriteJacobianEveryIteration.Checked);
    PestControlData.VerboseRecord := TVerboseRecord(cbWriteVerboseRunRecord.Checked);
    PestControlData.SaveInterimResiduals :=
      TSaveInterimResiduals(cbWriteIntermResidualForEveryIteration.Checked);
    PestControlData.SaveParamIteration :=
      TSaveParamIteration(cbSaveParamValuesIteration.Checked);
    PestControlData.SaveParamRun :=
      TSaveParamRun(cbSaveParamValuesModelRun.Checked);
      {$ENDREGION}

    {$REGION 'Singular Value Decomposition'}
    SvdProperties := PestProperties.SvdProperties;
    SvdProperties.Mode := TSvdMode(comboSvdMode.ItemIndex);
    if rdeMaxSingularValues.Text <> '' then
    begin
      SvdProperties.MaxSingularValues := rdeMaxSingularValues.IntegerValue;
    end;
    if rdeEigenThreshold.Text <> '' then
    begin
      SvdProperties.EigenThreshold := rdeEigenThreshold.RealValue;
    end;
    SvdProperties.EigenWrite := TEigenWrite(comboEigenWrite.ItemIndex);
    {$ENDREGION}

    {$REGION 'LQSR'}
    LsqrProperties := PestProperties.LsqrProperties;
    LsqrProperties.Mode := TLsqrMode(cbUseLqsr.Checked);
    if rdeMatrixTolerance.Text <> '' then
    begin
      LsqrProperties.MatrixTolerance := rdeMatrixTolerance.RealValue;
    end;
    if rdeRightHandSideTolerance.Text <> '' then
    begin
      LsqrProperties.RightHandSideTolerance := rdeRightHandSideTolerance.RealValue;
    end;
    if rdeConditionNumberLimit.Text <> '' then
    begin
      LsqrProperties.ConditionNumberLimit := rdeConditionNumberLimit.RealValue;
    end;
    if rdeMaxLqsrIterations.Text <> '' then
    begin
      LsqrProperties.MaxIteration := rdeMaxLqsrIterations.IntegerValue;
    end;
    LsqrProperties.LsqrWrite := TLsqrWrite(cbWriteLsqrOutput.Checked);
    {$ENDREGION}

    {$REGION 'Observation Groups'}
    ObsGroups := PestProperties.ObservationGroups;
    Grid := frameObservationGroups.Grid;
//    ObsIndex := 0;
    for RowIndex := 1 to frameObservationGroups.seNumber.AsInteger do
    begin
      AnObsGroup := Grid.Objects[Ord(pogcName), RowIndex]
        as TPestObservationGroup;
      if (AnObsGroup = nil) and (Grid.Cells[Ord(pogcName), RowIndex] <> '') then
      begin
        AnObsGroup := FLocalObsGroups.Add;
      end;
      if AnObsGroup <> nil then
      begin
        if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
        begin
          AnObsGroup.ObsGroupName := Grid.Cells[Ord(pogcName), RowIndex];
        end;
        AnObsGroup.UseGroupTarget := Grid.Checked[Ord(pogcUseTarget), RowIndex];
        AnObsGroup.GroupTarget := Grid.RealValueDefault[Ord(pogcTarget), RowIndex, 0];
        AnObsGroup.AbsoluteCorrelationFileName := Grid.Cells[Ord(pogcFileName), RowIndex];
        AnObsGroup.Collection := ObsGroups;
      end;
    end;
    {$ENDREGION}

    {$REGION 'Observation Group Assignments'}
    ANode := FNoNameNode;
    while ANode <> nil do
    begin
      ObsGroup := ANode.Data;
      ChildNode := ANode.getFirstChild;
      while ChildNode <> nil do
      begin
        AnObject := ChildNode.Data;
        if AnObject is TCustomObservationItem then
        begin
          AnObs := TCustomObservationItem(AnObject);
          if ObsGroup = nil then
          begin
            AnObs.ObservationGroup := '';
          end
          else
          begin
            AnObs.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end
        else if AnObject is TFluxObservationGroup then
        begin
          FluxObs := TFluxObservationGroup(AnObject);
          if ObsGroup = nil then
          begin
            FluxObs.ObservationGroup := '';
          end
          else
          begin
            FluxObs.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end
        else
        begin
          HobItem := AnObject as THobItem;
          if ObsGroup = nil then
          begin
            HobItem.ObservationGroup := '';
          end
          else
          begin
            HobItem.ObservationGroup := ObsGroup.ObsGroupName;
          end;
        end;
        ChildNode := ChildNode.GetNextSibling;
      end;

      ANode := ANode.GetNextSibling;
    end;
    {$ENDREGION}

    frmGoPhast.UndoStack.Submit(TUndoPestOptions.Create(PestProperties,
      FNewObsList, FNewFluxObservationList, FNewHobList, diredPest.Text));
  finally
    PestProperties.Free
  end;

end;

procedure TfrmPEST.FixObsGroupNames;
var
  Grid: TRbwDataGrid4;
  RowIndex: Integer;
  ValidName: string;
begin
  Grid := frameObservationGroups.Grid;
  for RowIndex := 1 to frameObservationGroups.seNumber.AsInteger do
  begin
    if Grid.Cells[Ord(pogcName), RowIndex] <> '' then
    begin
      ValidName := ValidObsGroupName(Grid.Cells[Ord(pogcName), RowIndex]);
      if ValidName <> Grid.Cells[Ord(pogcName), RowIndex] then
      begin
        Grid.Cells[Ord(pogcName), RowIndex] := ValidName;
      end;
    end;
  end;
end;

procedure TfrmPEST.HandleGroupDeletion(Group: TPestObservationGroup);
var
  OtherGroup: TPestObservationGroup;
  TreeNode: TTreeNode;
  ChildNode: TTreeNode;
begin
  if FGroupNameDictionary.TryGetValue(
    UpperCase(Group.ObsGroupName), OtherGroup) then
  begin
    if Group = OtherGroup then
    begin
      FGroupNameDictionary.Remove(UpperCase(Group.ObsGroupName));
    end;
  end;
  if FGroupDictionary.TryGetValue(Group, TreeNode) then
  begin
    ChildNode := TreeNode.getFirstChild;
    while ChildNode <> nil do
    begin
      ChildNode.MoveTo(FNoNameNode, naAddChild);
      ChildNode := TreeNode.getFirstChild;
    end;
  end;
end;

procedure TfrmPEST.ImportPilotPoints(const FileName: string);
var
  Extension: string;
  DisLimits: TGridLimit;
  PointsQuadTree: TRbwQuadTree;
  ShapeHeaderFile: string;
  ShapeIndex: Integer;
  AShape: TShapeObject;
  ShapeFileReader: TShapefileGeometryReader;
  APoint: TShapePoint;
  APoint2D: TPoint2D;
  PointList: TList<TPoint2D>;
  Splitter: TStringList;
  PointIndex: Integer;
  TextReader: TStreamReader;
  ALine: string;
  LineIndex: Integer;
  Value: Extended;
  FirstRow: Int64;
  procedure HandleAPoint(APoint2D: TPoint2D);
  var
    DupPoint: TPoint2D;
    APointer: Pointer;
  begin
    if PointList.Count = 0 then
    begin
      PointList.Add(APoint2D);
      PointsQuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
    end
    else
    begin
      DupPoint := APoint2D;
      PointsQuadTree.FirstNearestPoint(DupPoint.x, DupPoint.y, APointer);
      if (DupPoint.x <> APoint2D.x) or (DupPoint.y <> APoint2D.y) then
      begin
        PointList.Add(APoint2D);
        PointsQuadTree.AddPoint(APoint2D.x, APoint2D.y, nil);
      end;
    end;
  end;
begin
  Assert(TFile.Exists(FileName));
  Extension := LowerCase(ExtractFileExt(FileName));
  PointsQuadTree := TRbwQuadTree.Create(nil);
  PointList := TList<TPoint2D>.Create;
  try
    DisLimits := frmGoPhast.PhastModel.DiscretizationLimits(vdTop);
    PointsQuadTree.XMax := DisLimits.MaxX;
    PointsQuadTree.XMin := DisLimits.MinX;
    PointsQuadTree.YMax := DisLimits.MaxY;
    PointsQuadTree.YMin := DisLimits.MinY;
    if Extension = '.shp' then
    begin
      ShapeFileReader := TShapefileGeometryReader.Create;
      try
        ShapeHeaderFile := ChangeFileExt(FileName, '.shx');
        if not TFile.Exists(ShapeHeaderFile) then
        begin
          Beep;
          MessageDlg(Format(StrTheShapeHeaderFil, [ShapeHeaderFile]), mtError,
            [mbOK], 0);
          Exit;
        end;
        ShapeFileReader.ReadFromFile(FileName, ShapeHeaderFile);
        for ShapeIndex := 0 to ShapeFileReader.Count - 1 do
        begin
          AShape := ShapeFileReader[ShapeIndex];
          for PointIndex := 0 to AShape.FNumPoints - 1 do
          begin
            APoint := AShape.FPoints[PointIndex];
            APoint2D.x := APoint.X;
            APoint2D.y := APoint.Y;
            HandleAPoint(APoint2D);
          end;
        end;
      finally
        ShapeFileReader.Free;
      end;
    end
    else  if (Extension = '.csv') or (Extension = '.txt') then
    begin
      Splitter := TStringList.Create;
      TextReader := TFile.OpenText(FileName);
      try
        LineIndex := 0;
        repeat
          ALine := TextReader.ReadLine;
          Inc(LineIndex);
          if (ALine = '') or (ALine[1] = '#') then
          begin
            Continue;
          end;
          Splitter.DelimitedText := ALine;
          if Splitter.Count >= 2 then
          begin
            if TryFortranStrToFloat(Splitter[0], Value) then
            begin
              APoint2D.x := Value;
            end
            else
            begin;
              Beep;
              MessageDlg(Format(
                'The %0:s value "%1:s" in line %2:d can not be converted to a real number.'
                , ['first', Splitter[0], LineIndex]), mtError, [mbOK], 0);
              Exit;
            end;

            if TryFortranStrToFloat(Splitter[1], Value) then
            begin
              APoint2D.y := Value;
            end
            else
            begin;
              Beep;
              MessageDlg(Format(
                'The %0:s value "%1:s" in line %2:d can not be converted to a real number.'
                , ['second', Splitter[1], LineIndex]), mtError, [mbOK], 0);
              Exit;
            end;
            HandleAPoint(APoint2D);
          end
          else
          begin
            Beep;
            MessageDlg(Format(StrLine0d1sM, [LineIndex, ALine]), mtError,
              [mbOK], 0);
            Exit;
          end;
        until (TextReader.EndOfStream);
      finally
        Splitter.Free;
        TextReader.Free;
      end;
    end
    else
    begin
      Assert(False);
    end;

    FirstRow := framePilotPoints.seNumber.AsInteger + 1;
    framePilotPoints.seNumber.AsInteger := framePilotPoints.seNumber.AsInteger
      + PointList.Count;
    for PointIndex := 0 to PointList.Count - 1 do
    begin
      APoint2D := PointList[PointIndex];
      framePilotPoints.Grid.RealValue[Ord(ppcX), FirstRow + PointIndex] := APoint2D.x;
      framePilotPoints.Grid.RealValue[Ord(ppcy), FirstRow + PointIndex] := APoint2D.y;
    end;
  finally
    PointsQuadTree.Free;
    PointList.Free;
  end;
end;

procedure TfrmPEST.HandleAddedGroup(ObsGroup: TPestObservationGroup);
var
  NewNode: TTreeNode;
begin
  NewNode := frameParentObsGroups.tvTree.Items.AddChild(nil,
    ObsGroup.ObsGroupName);
  NewNode.Data := ObsGroup;
  FGroupDictionary.Add(ObsGroup, NewNode);
  if ObsGroup.ObsGroupName <> '' then
  begin
    if not FGroupNameDictionary.ContainsKey(UpperCase(ObsGroup.ObsGroupName)) then
    begin
      FGroupNameDictionary.Add(UpperCase(ObsGroup.ObsGroupName), ObsGroup);
    end;
  end;
end;

procedure TfrmPEST.CheckPestDirectory;
begin
  if DirectoryExists(diredPest.Text) then
  begin
    diredPest.Color := clWindow;
  end
  else
  begin
    diredPest.Color := clRed;
  end;
end;

{ TUndoPestOptions }

constructor TUndoPestOptions.Create(var NewPestProperties: TPestProperties;
  var NewObsList: TObservationObjectList;
  var NewFluxObservationList: TFluxObservationObjectList;
  var NewHobList: THobObjectList;
  PestDirectory: String);
var
  InvalidateModelEvent: TNotifyEvent;
  TempList: TObservationList;
  index: Integer;
  AnObs: TCustomObservationItem;
  ATempObs: TCustomObservationItem;
  Locations: TProgramLocations;
  TempFluxObsList: TFluxObservationList;
  FluxObsIndex: Integer;
  FluxObs: TFluxObservationGroup;
  TempHobList: THobList;
  HobIndex: Integer;
  HobItem: THobItem;
begin
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  OldPestLocation := Locations.PestDirectory;
  NewPestLocation := PestDirectory;

  InvalidateModelEvent := nil;
  FOldPestProperties := TPestProperties.Create(nil);
  FOldPestProperties.Assign(frmGoPhast.PhastModel.PestProperties);
  FNewPestProperties := NewPestProperties;
  NewPestProperties := nil;

  TempList := TObservationList.Create;
  try
    TempList.Capacity := NewObsList.Count;
    frmGoPhast.PhastModel.FillObsItemList(TempList, True);
    FOldObsList  := TObservationObjectList.Create;
    FOldObsList.Capacity := TempList.Count;
    for index := 0 to TempList.Count - 1 do
    begin
      AnObs := TempList[index];
      ATempObs := TCustomObservationItem.Create(nil);
      FOldObsList.Add(ATempObs);
      ATempObs.Assign(AnObs);
    end;
  finally
    TempList.Free;
  end;

  FNewObsList := NewObsList;
  NewObsList := nil;

  TempFluxObsList := TFluxObservationList.Create;
  try
    TempFluxObsList.Capacity := NewFluxObservationList.Count;
    frmGoPhast.PhastModel.FillFluxObsList(TempFluxObsList);
    FOldFluxObsList := TFluxObservationObjectList.Create;
    for FluxObsIndex := 0 to TempFluxObsList.Count - 1 do
    begin
      FluxObs := TFluxObservationGroup.Create(nil);
      FOldFluxObsList.Add(FluxObs);
      FluxObs.Assign(TempFluxObsList[FluxObsIndex]);
    end;
  finally
    TempFluxObsList.Free;
  end;

  FNewFluxObsList := NewFluxObservationList;
  NewFluxObservationList := nil;

  TempHobList := THobList.Create;
  try
    TempHobList.Capacity := NewHobList.Count;
    frmGoPhast.PhastModel.FillHobList(TempHobList);
    FOldHobList := THobObjectList.Create;
    for HobIndex := 0 to TempHobList.Count - 1 do
    begin
      HobItem := THobItem.Create(nil);
      FOldHobList.Add(HobItem);
      HobItem.Assign(TempHobList[HobIndex]);
    end;
  finally
    TempHobList.Free;
  end;

  FNewHobList := NewHobList;
  NewHobList := nil;
end;

function TUndoPestOptions.Description: string;
begin
  result := 'change PEST properties';
end;

destructor TUndoPestOptions.Destroy;
begin
  FNewFluxObsList.Free;
  FOldFluxObsList.Free;
  FOldObsList.Free;
  FNewObsList.Free;
  FOldPestProperties.Free;
  FNewPestProperties.Free;
  FOldHobList.Free;
  FNewHobList.Free;
  inherited;
end;

procedure TUndoPestOptions.DoCommand;
var
  Locations: TProgramLocations;
begin
  inherited;
//  frmGoPhast.PhastModel.PestProperties := FNewPestProperties;
  UpdateProperties(FNewPestProperties, FNewObsList, FNewFluxObsList, FNewHobList);
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Locations.PestDirectory := NewPestLocation;
  frmGoPhast.PhastModel.SetMf2005ObsGroupNames;
  frmGoPhast.EnableManageParameters;
end;

procedure TUndoPestOptions.Undo;
var
  Locations: TProgramLocations;
begin
  inherited;
  UpdateProperties(FOldPestProperties, FOldObsList, FOldFluxObsList, FOldHobList);
  Locations := frmGoPhast.PhastModel.ProgramLocations;
  Locations.PestDirectory := OldPestLocation;
  frmGoPhast.PhastModel.SetMf2005ObsGroupNames;
  frmGoPhast.EnableManageParameters;
end;

procedure TUndoPestOptions.UpdateProperties(PestProperties: TPestProperties;
  ObsList: TObservationObjectList; FluxObsList: TFluxObservationObjectList;
  HobList: THobObjectList);
var
  ShouldUpdateView: Boolean;
  TempList: TObservationList;
  AnObs: TCustomObservationItem;
  NewObs: TCustomObservationItem;
  ObsIndex: Integer;
  TempFluxObsList: TFluxObservationList;
  FluxObsIndex: Integer;
  FluxObs: TFluxObservationGroup;
  TempHobList: THobList;
  HobIndex: Integer;
  HobObs: THobItem;
begin
  ShouldUpdateView := frmGoPhast.PhastModel.PestProperties.ShouldDrawPilotPoints
    <> PestProperties.ShouldDrawPilotPoints;
  if PestProperties.ShouldDrawPilotPoints
    and (PestProperties.PilotPointSpacing
      <> frmGoPhast.PhastModel.PestProperties.PilotPointSpacing) then
  begin
    ShouldUpdateView := True;
  end;
  frmGoPhast.PhastModel.PestProperties := PestProperties;

  TempList := TObservationList.Create;
  try
    TempList.Capacity := ObsList.Count;
    frmGoPhast.PhastModel.FillObsItemList(TempList, True);
    Assert(TempList.Count = ObsList.Count);
    for ObsIndex := 0 to TempList.Count - 1 do
    begin
      AnObs := TempList[ObsIndex];
      NewObs := ObsList[ObsIndex];
      AnObs.Assign(NewObs);
    end;
  finally
    TempList.Free;
  end;

  TempFluxObsList := TFluxObservationList.Create;
  try
    TempFluxObsList.Capacity := FluxObsList.Count;
    frmGoPhast.PhastModel.FillFluxObsList(TempFluxObsList);
    for FluxObsIndex := 0 to TempFluxObsList.Count - 1 do
    begin
      FluxObs := TempFluxObsList[FluxObsIndex];
      FluxObs.Assign(FluxObsList[FluxObsIndex]);
    end;
  finally
    TempFluxObsList.Free;
  end;

  TempHobList := THobList.Create;
  try
    TempHobList.Capacity := HobList.Count;
    frmGoPhast.PhastModel.FillHobList(TempHobList);
    for HobIndex := 0 to TempHobList.Count - 1 do
    begin
      HobObs := TempHobList[HobIndex];
      HobObs.Assign(HobList[HobIndex]);
    end;
  finally
    TempHobList.Free;
  end;

  frmGoPhast.EnablePilotPointItems;

  if ShouldUpdateView then
  begin
    frmGoPhast.SynchronizeViews(vdTop);
  end;
end;

end.
