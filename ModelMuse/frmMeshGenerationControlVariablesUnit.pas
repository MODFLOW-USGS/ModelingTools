unit frmMeshGenerationControlVariablesUnit;

interface

uses
  System.UITypes, Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frmCustomGoPhastUnit, Grids,
  RbwDataGrid4, StdCtrls, Buttons, ExtCtrls, UndoItems,
  SutraMeshUnit, ArgusDataEntry, Mask, JvExMask, JvToolEdit, JvExStdCtrls,
  JvHtControls, JvPageList, JvExControls;

type
  TMeshControlType = (mtcNone, mtcGrowthRate, mctSplittingAngle,
    mtcLineLength, mtcSymmetry, mtcConcave, mctStructure, mtcNodePlacementError);
  TMeshControlLocation = (mtlNone, mtlVariable);

  TGeompackRows = (grNone, grTolerance, grSpacingAngle, grToleranceAngle,
    grUniformnessParameter, grMeshDistributionVariation, grElementGenerationParameter,
    grAutomaticeElementCount, grDesiredElementCount, grShapeMeasure,
    grQuadSplittingValue, grKeepQuadsAlongEdges, grMaxImprovementIterations,
    grMaxSmoothingIterations, grOptimizationBasedSmoothingCriterion);

  TUndoChangeMeshGenControls = class(TCustomUndo)
  private
    FOldControls: TMeshGenerationControls;
    FNewControls: TMeshGenerationControls;
    FOldGmshLocation: string;
    FNewGmshLocation: string;
    FOldGeompackLocation: string;
    FNewGeompackLocation: string;
  protected
    function Description: string; override;
  public
    constructor Create(var Controls: TMeshGenerationControls;
      const GmshLocation, GeompackLocation: string);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmMeshGenerationControlVariables = class(TfrmCustomGoPhast)
    rdgControlVariables: TRbwDataGrid4;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnResetDefaults: TButton;
    rgMethod: TRadioGroup;
    pnlTop: TPanel;
    rgRenumberingMethod: TRadioGroup;
    htlblGmsh: TJvHTLabel;
    fedGmsh: TJvFilenameEdit;
    lblGmsh: TLabel;
    jvplMesh: TJvPageList;
    jvspFishnet: TJvStandardPage;
    jvspIrregular: TJvStandardPage;
    jvspGmsh: TJvStandardPage;
    jvspGeomPack: TJvStandardPage;
    rrdgGeompackOptions: TRbwRowDataGrid;
    pnlGeompack: TPanel;
    lblGeompack: TLabel;
    htlblGeompack: TJvHTLabel;
    fedGeompack: TJvFilenameEdit;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure btnResetDefaultsClick(Sender: TObject);
    procedure rgMethodClick(Sender: TObject);
    procedure fedGmshChange(Sender: TObject);
    procedure fedGeompackChange(Sender: TObject);
  private
    procedure GetData;
    procedure SetData;
    procedure AssignValues(MeshGenControls: TMeshGenerationControls);
    { Private declarations }
  public
    { Public declarations }
  end;

  // var
  // frmMeshGenerationControlVariables: TfrmMeshGenerationControlVariables;

implementation

uses
  frmGoPhastUnit, MeshRenumberingTypes, SutraOptionsUnit, System.IOUtils,
  QuadMeshGenerator;

resourcestring
  StrSplittingAngle = 'Splitting angle';
  StrStructure = 'Structure';
  StrNodePlacementError = 'Node placement error';
  StrLineLength = 'Line length';
  StrSymmetry = 'Symmetry';
  StrConcave = 'Concave';
  StrControlVariable = 'Control variable';
//  StrSixNodes = 'Six-Nodes';
  StrElementGrowthRate = 'Element growth rate';
  StrValue = 'Value';
  StrTheElementGrowthR = 'The element growth rate must be greater than 1.';
  StrThereIsANewerVer = 'There is a newer version of GMSH available. Do you ' +
  'want to continue anyway?';

var
  GMshDate: TDateTime;

{$R *.dfm}

procedure TfrmMeshGenerationControlVariables.btnOKClick(Sender: TObject);
begin
  inherited;
  if (rgMethod.ItemIndex = 2) and TFile.Exists(fedGmsh.FileName) then
  begin
    if TFile.GetLastWriteTime(fedGmsh.FileName) < GMshDate then
    begin
      if not (MessageDlg(StrThereIsANewerVer, mtWarning,
        [mbYes, mbNo], 0) = mrYes) then
      begin
        ModalResult := mrNone;
        Exit;
      end;
    end;
  end;
  SetData;
end;

procedure TfrmMeshGenerationControlVariables.btnResetDefaultsClick
  (Sender: TObject);
var
  MeshGenControls: TMeshGenerationControls;
begin
  inherited;
  MeshGenControls := TMeshGenerationControls.Create(nil);
  try
    AssignValues(MeshGenControls);
  finally
    MeshGenControls.Free;
  end;
end;

procedure TfrmMeshGenerationControlVariables.fedGeompackChange(Sender: TObject);
begin
  inherited;
  if FileExists(fedGeompack.FileName) then
  begin
    fedGeompack.Color := clWindow;
  end
  else
  begin
    fedGeompack.Color := clRed;
  end;
end;

procedure TfrmMeshGenerationControlVariables.fedGmshChange(Sender: TObject);
begin
  inherited;
  if FileExists(fedGmsh.FileName) then
  begin
    fedGmsh.Color := clWindow;
  end
  else
  begin
    fedGmsh.Color := clRed;
  end;
end;

procedure TfrmMeshGenerationControlVariables.FormCreate(Sender: TObject);
begin
  inherited;
  rdgControlVariables.BeginUpdate;
  try
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mctSplittingAngle)] :=
      StrSplittingAngle;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mctStructure)] := StrStructure;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcNodePlacementError)] :=
      StrNodePlacementError;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcLineLength)] := StrLineLength;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcSymmetry)] := StrSymmetry;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcConcave)] := StrConcave;
    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcGrowthRate)] := StrElementGrowthRate;

    rdgControlVariables.Cells[Ord(mtlNone), Ord(mtcNone)] := StrControlVariable;
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcNone)] := StrValue;
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNone)] := StrSixNodes;
  finally
    rdgControlVariables.EndUpdate;
  end;

  rrdgGeompackOptions.BeginUpdate;
  try
    rrdgGeompackOptions.Cells[1, Ord(grNone)] := 'Value';
    rrdgGeompackOptions.Cells[0, Ord(grTolerance)] := 'Tolerance (toler)';
    rrdgGeompackOptions.Cells[0, Ord(grSpacingAngle)] := 'Spacing angle (angspc)';
    rrdgGeompackOptions.Cells[0, Ord(grToleranceAngle)] := 'Tolerance angle (angtol)';
    rrdgGeompackOptions.Cells[0, Ord(grUniformnessParameter)] := 'Uniformness parameter (munif)';
    rrdgGeompackOptions.Cells[0, Ord(grMeshDistributionVariation)] := 'Mesh distribution variation (dmin)';
    rrdgGeompackOptions.Cells[0, Ord(grElementGenerationParameter)] := 'Element generation parameter (nmin)';
    rrdgGeompackOptions.Cells[0, Ord(grAutomaticeElementCount)] := 'Desired element count (nelemd)';
    rrdgGeompackOptions.Cells[0, Ord(grDesiredElementCount)] := 'Previouly used element count (nelemd)';
    rrdgGeompackOptions.Cells[0, Ord(grShapeMeasure)] := 'Shape measure (mtype)';
    rrdgGeompackOptions.Cells[0, Ord(grQuadSplittingValue)] := 'Quad splitting value (quadmu)';
    rrdgGeompackOptions.Cells[0, Ord(grKeepQuadsAlongEdges)] := 'Keep quads along edges (quadmu)';
    rrdgGeompackOptions.Cells[0, Ord(grMaxImprovementIterations)] := 'Max improvement iterations (nimpiter)';
    rrdgGeompackOptions.Cells[0, Ord(grMaxSmoothingIterations)] := 'Max smoothing iterations (nsmpas)';
    rrdgGeompackOptions.Cells[0, Ord(grOptimizationBasedSmoothingCriterion)] := 'Optimization-based smoothing criterion (nsmpas)';
  finally
    rrdgGeompackOptions.EndUpdate;
  end;

  GetData;
end;

procedure TfrmMeshGenerationControlVariables.GetData;
var
  MeshGenControls: TMeshGenerationControls;
begin
  MeshGenControls := frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls;
  AssignValues(MeshGenControls);
  try
    fedGmsh.FileName := frmGoPhast.PhastModel.ProgramLocations.GmshLocation;
    fedGeompack.FileName := frmGoPhast.PhastModel.ProgramLocations.GeompackLocation;
  except on EComboEditError do
    begin
      // do nothing.
    end;
  end;
  fedGmshChange(nil);
  rgMethodClick(nil);
end;

procedure TfrmMeshGenerationControlVariables.rgMethodClick(Sender: TObject);
begin
  inherited;
  jvplMesh.ActivePageIndex := rgMethod.ItemIndex
//  rdgControlVariables.Enabled := rgMethod.ItemIndex = 1;
////  rdeGrowthRate.Enabled := rdgControlVariables.Enabled;
//  if rdgControlVariables.Enabled then
//  begin
//    rdgControlVariables.Color := clWindow;
//  end
//  else
//  begin
//    rdgControlVariables.Color := clBtnFace;
//  end;
end;

procedure TfrmMeshGenerationControlVariables.SetData;
var
  MeshGenControls: TMeshGenerationControls;
  GeomPackOptions: TGeompackOptions;
  Undo: TUndoChangeMeshGenControls;
begin
  MeshGenControls := TMeshGenerationControls.Create(nil);
  try
    MeshGenControls.MeshGenerationMethod :=
      TMeshGenerationMethod(rgMethod.ItemIndex);
    MeshGenControls.RenumberingAlgorithm :=
      TRenumberingAlgorithm(rgRenumberingMethod.ItemIndex);
    MeshGenControls.SplittingAngle.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mctSplittingAngle)]);
    MeshGenControls.Structure.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctStructure)]);
    MeshGenControls.NodePlacementError.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mtcNodePlacementError)]);
    MeshGenControls.LineLength.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable),
      Ord(mtcLineLength)]);
    MeshGenControls.Symmetry.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcSymmetry)]);
    MeshGenControls.Concave.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcConcave)]);

//    MeshGenControls.AltSplittingAngle.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mctSplittingAngle)]);
//    MeshGenControls.AltStructure.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)]);
//    MeshGenControls.AltNodePlacementError.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mtcNodePlacementError)]);
//    MeshGenControls.AltLineLength.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode),
//      Ord(mtcLineLength)]);
//    MeshGenControls.AltSymmetry.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)]);
//    MeshGenControls.AltConcave.Value :=
//      StrToFloat(rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)]);

    MeshGenControls.ElementGrowthRate.Value :=
      StrToFloat(rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcGrowthRate)]);

    if MeshGenControls.ElementGrowthRate.Value <= 1 then
    begin
      Beep;
      MessageDlg(StrTheElementGrowthR, mtError, [mbOK], 0);
      ModalResult := mrNone;
      Exit;
    end;


    GeomPackOptions := MeshGenControls.GeomPackOptions;
    GeomPackOptions.Tolerance := rrdgGeompackOptions.RealValue[1, Ord(grTolerance)];
    GeomPackOptions.SpacingAngle := rrdgGeompackOptions.RealValue[1, Ord(grSpacingAngle)];
    GeomPackOptions.ToleranceAngle := rrdgGeompackOptions.RealValue[1, Ord(grToleranceAngle)];
    GeomPackOptions.UniformnessParameter := rrdgGeompackOptions.RealValue[1, Ord(grUniformnessParameter)];
    GeomPackOptions.MeshDistributionVariation := rrdgGeompackOptions.RealValue[1, Ord(grMeshDistributionVariation)];
    GeomPackOptions.ElementGenerationParameter := rrdgGeompackOptions.IntegerValue[1, Ord(grElementGenerationParameter)];
    GeomPackOptions.AutomaticeElementCount := rrdgGeompackOptions.IntegerValue[1, Ord(grAutomaticeElementCount)] = 0;
    GeomPackOptions.DesiredElementCount := rrdgGeompackOptions.IntegerValue[1, Ord(grAutomaticeElementCount)];
    GeomPackOptions.ShapeMeasure := TGeompackShapeMeasure(rrdgGeompackOptions.ItemIndex[1, Ord(grShapeMeasure)]);
    GeomPackOptions.QuadSplittingValue := rrdgGeompackOptions.RealValue[1, Ord(grQuadSplittingValue)];
    GeomPackOptions.KeepQuadsAlongEdges := rrdgGeompackOptions.Checked[1, Ord(grKeepQuadsAlongEdges)];
    GeomPackOptions.MaxImprovementIterations := rrdgGeompackOptions.IntegerValue[1, Ord(grMaxImprovementIterations)];
    GeomPackOptions.MaxSmoothingIterations := rrdgGeompackOptions.IntegerValue[1, Ord(grMaxSmoothingIterations)];
    GeomPackOptions.OptimizationBasedSmoothingCriterion := rrdgGeompackOptions.RealValue[1, Ord(grOptimizationBasedSmoothingCriterion)];


    Undo := TUndoChangeMeshGenControls.Create(MeshGenControls, fedGmsh.FileName,
      fedGeompack.FileName);
    frmGoPhast.UndoStack.Submit(Undo);
  finally
    MeshGenControls.Free;
  end;
end;

procedure TfrmMeshGenerationControlVariables.AssignValues(MeshGenControls
  : TMeshGenerationControls);
var
  GeomPackOptions: TGeompackOptions;
begin
  case MeshGenControls.MeshGenerationMethod of
    mgmFishnet, mgmIrregular, mgmGmsh, mgmGeompack:
      begin
        rgMethod.ItemIndex := Ord(MeshGenControls.MeshGenerationMethod);
      end;
    mgmUnknown:
      begin
        if frmGoPhast.PhastModel.FishnetMeshGenerator.Elements.Count > 0 then
        begin
          rgMethod.ItemIndex := Ord(mgmFishnet);
        end
        else
        begin
          rgMethod.ItemIndex := Ord(mgmIrregular);
        end;
      end;
    else Assert(False);
  end;

  if frmGoPhast.PhastModel.SutraOptions.PresSolutionMethod = psmDirect then
  begin
    rgRenumberingMethod.ItemIndex := Ord(MeshGenControls.RenumberingAlgorithm);
    if rgRenumberingMethod.ItemIndex = 0 then
    begin
      rgRenumberingMethod.ItemIndex := 1;
    end;
  end
  else
  begin
    rgRenumberingMethod.ItemIndex := 0;
  end;

  rdgControlVariables.BeginUpdate;
  try
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctSplittingAngle)] :=
      FloatToStr(MeshGenControls.SplittingAngle.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mctStructure)] :=
      FloatToStr(MeshGenControls.Structure.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcNodePlacementError)] :=
      FloatToStr(MeshGenControls.NodePlacementError.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcLineLength)] :=
      FloatToStr(MeshGenControls.LineLength.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcSymmetry)] :=
      FloatToStr(MeshGenControls.Symmetry.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcConcave)] :=
      FloatToStr(MeshGenControls.Concave.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctSplittingAngle)] :=
  //    FloatToStr(MeshGenControls.AltSplittingAngle.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mctStructure)] :=
  //    FloatToStr(MeshGenControls.AltStructure.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcNodePlacementError)] :=
  //    FloatToStr(MeshGenControls.AltNodePlacementError.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcLineLength)] :=
  //    FloatToStr(MeshGenControls.AltLineLength.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcSymmetry)] :=
  //    FloatToStr(MeshGenControls.AltSymmetry.Value);
  //  rdgControlVariables.Cells[Ord(mtlSixNode), Ord(mtcConcave)] :=
  //    FloatToStr(MeshGenControls.AltConcave.Value);
    rdgControlVariables.Cells[Ord(mtlVariable), Ord(mtcGrowthRate)] :=
      FloatToStr(MeshGenControls.ElementGrowthRate.Value);
  finally
    rdgControlVariables.EndUpdate
  end;

  GeomPackOptions := MeshGenControls.GeomPackOptions;
  rrdgGeompackOptions.BeginUpdate;
  try
    rrdgGeompackOptions.RealValue[1, Ord(grTolerance)] := GeomPackOptions.Tolerance;
    rrdgGeompackOptions.RealValue[1, Ord(grSpacingAngle)] := GeomPackOptions.SpacingAngle;
    rrdgGeompackOptions.RealValue[1, Ord(grToleranceAngle)] := GeomPackOptions.ToleranceAngle;
    rrdgGeompackOptions.RealValue[1, Ord(grUniformnessParameter)] := GeomPackOptions.UniformnessParameter;
    rrdgGeompackOptions.RealValue[1, Ord(grMeshDistributionVariation)] := GeomPackOptions.MeshDistributionVariation;
    rrdgGeompackOptions.IntegerValue[1, Ord(grElementGenerationParameter)] := GeomPackOptions.ElementGenerationParameter;
    if GeomPackOptions.AutomaticeElementCount then
    begin
      rrdgGeompackOptions.IntegerValue[1, Ord(grAutomaticeElementCount)] := 0;
    end
    else
    begin
      rrdgGeompackOptions.IntegerValue[1, Ord(grAutomaticeElementCount)] := GeomPackOptions.DesiredElementCount;
    end;
    rrdgGeompackOptions.IntegerValue[1, Ord(grDesiredElementCount)] := GeomPackOptions.DesiredElementCount;
    rrdgGeompackOptions.ItemIndex[1, Ord(grShapeMeasure)] := Ord(GeomPackOptions.ShapeMeasure);
    rrdgGeompackOptions.RealValue[1, Ord(grQuadSplittingValue)] := GeomPackOptions.QuadSplittingValue;
    rrdgGeompackOptions.Checked[1, Ord(grKeepQuadsAlongEdges)] := GeomPackOptions.KeepQuadsAlongEdges;
    rrdgGeompackOptions.IntegerValue[1, Ord(grMaxImprovementIterations)] := GeomPackOptions.MaxImprovementIterations;
    rrdgGeompackOptions.IntegerValue[1, Ord(grMaxSmoothingIterations)] := GeomPackOptions.MaxSmoothingIterations;
    rrdgGeompackOptions.RealValue[1, Ord(grOptimizationBasedSmoothingCriterion)] := GeomPackOptions.OptimizationBasedSmoothingCriterion;
  finally
    rrdgGeompackOptions.EndUpdate;
  end;

end;

{ TUndoChangeMeshGenControls }

constructor TUndoChangeMeshGenControls.Create(var Controls
  : TMeshGenerationControls; const GmshLocation, GeompackLocation: string);
begin
  FOldGmshLocation := frmGoPhast.PhastModel.ProgramLocations.GmshLocation;
  FNewGmshLocation := GmshLocation;
  FOldGeompackLocation := frmGoPhast.PhastModel.ProgramLocations.GeompackLocation;
  FNewGeompackLocation := GeompackLocation;
  FOldControls := TMeshGenerationControls.Create(nil);
  FOldControls.Assign(frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls);
  FNewControls := Controls;
  Controls := nil;
end;

function TUndoChangeMeshGenControls.Description: string;
begin
  result := 'change mesh generation controls';
end;

destructor TUndoChangeMeshGenControls.Destroy;
begin
  FOldControls.Free;
  FNewControls.Free;
  inherited;
end;

procedure TUndoChangeMeshGenControls.DoCommand;
begin
  frmGoPhast.PhastModel.ProgramLocations.GmshLocation := FNewGmshLocation;
  frmGoPhast.PhastModel.ProgramLocations.GeompackLocation := FNewGeompackLocation;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls := FNewControls;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls.Apply;
  inherited;
end;

procedure TUndoChangeMeshGenControls.Undo;
begin
  frmGoPhast.PhastModel.ProgramLocations.GmshLocation := FOldGmshLocation;
  frmGoPhast.PhastModel.ProgramLocations.GeompackLocation := FOldGeompackLocation;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls := FOldControls;
  frmGoPhast.PhastModel.SutraMesh.Mesh2D.MeshGenControls.Apply;
  inherited;
end;

initialization
  // See also StrDefaultGmshPath in PhastModelUnit
  GMshDate := EncodeDate(2022, 2, 21);

end.
