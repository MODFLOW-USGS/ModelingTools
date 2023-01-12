{
  @abstract(The purpose of @name is to define @link(TfrmDataSets)
  which provides a mechanism for the user to
  edit the @link(TDataArray)s and their properties.)

}
unit frmDataSetsUnits;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls,
  frmCustomGoPhastUnit, Buttons, ExtCtrls, Grids, RbwDataGrid4, RbwParser,
  DataSetUnit, Contnrs, GoPhastTypes, framePhastInterpolationUnit, ComCtrls,
  UndoItems, PhastDataSets, ArgusDataEntry, JvExStdCtrls, JvCombobox,
  JvListComb, RbwEdit, ClassificationUnit, JvExComCtrls, JvComCtrls;

{ TODO : Consider making this a property sheet like the object inspector. }

{ TODO : Consider using a non-modal window here. }

Type
  TOK_Variables = record
    ActiveOK: boolean;
    SpecifiedHeadOK: boolean;
    GetVContOK: Boolean;
    HufKxOK: Boolean;
    HufKyOK: Boolean;
    HufKzOK: Boolean;
    HufSsOk: Boolean;
    HufSyOk: Boolean;
  end;

  TDataArrayEdit = class;

  TEditorNotifierComponent = class(TComponent)
  private
    FEditor: TDataArrayEdit;
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
  public
    property Editor: TDataArrayEdit read FEditor;
    constructor Create(Owner: TDataArrayEdit); reintroduce;
  end;

  // @name stores a @link(TDataArray) and records changes to the properties
  // of the @link(TDataArray).
  TDataArrayEdit = class(TClassificationObject)
  private
    FDataArray: TDataArray;
    FName: string;
    FDataType: TRbwDataType;
    FOrientation: TDataSetOrientation;
    FEvaluatedAt: TEvaluatedAt;
    FUnits: string;
    FFormula: string;
    FTwoDInterpolator: TCustom2DInterpolater;
    FInterpValues: TPhastInterpolationValues;
    FNewUses: TStringList;
    FVariable: TCustomVariable;
    FExpression: TExpression;
    FNode: TTreeNode;
    FComment: string;
    FNotifier: TEditorNotifierComponent;
    FDisplayName: string;
    FAngleType: TAngleType;
    FClassification: string;
    FPestParametersUsed: Boolean;
//    FParameterLayersUsed: string;
    procedure SetTwoDInterpolator(const Value: TCustom2DInterpolater);
    procedure SetInterpValues(const Value: TPhastInterpolationValues);
    procedure SetNewUses(const Value: TStringList);
    function GetAssociatedDataSets: string;
    procedure SetExpression(const Value: TExpression);
  public
    property DataArray: TDataArray read FDataArray;
    Constructor Create(ADataArray: TDataArray);
    destructor Destroy; override;
    property Name: string read FName write FName;
    property DisplayName: string read FDisplayName write FDisplayName;
    property DataType: TRbwDataType read FDataType write FDataType;
    property Orientation: TDataSetOrientation read FOrientation
      write FOrientation;
    property EvaluatedAt: TEvaluatedAt read FEvaluatedAt write FEvaluatedAt;
    property Units: string read FUnits write FUnits;
    property AngleType: TAngleType read FAngleType write FAngleType;
    property Formula: string read FFormula write FFormula;
    property TwoDInterpolator: TCustom2DInterpolater read FTwoDInterpolator
      write SetTwoDInterpolator;
    property InterpValues: TPhastInterpolationValues read FInterpValues write SetInterpValues;
    procedure UpdateDataSet;
    // @name contains a list of the variables
    // used by the formula for the @link(DataArray).
    property NewUses: TStringList read FNewUses write SetNewUses;
    property Variable: TCustomVariable read FVariable write FVariable;
    property Expression: TExpression read FExpression write SetExpression;
    property Comment: string read FComment write FComment;
    property Classification: string read FClassification write FClassification;
    function ClassificationName: string; override;
    function FullClassification: string; override;
    property AssociatedDataSets: string read GetAssociatedDataSets;
    property PestParametersUsed: Boolean read FPestParametersUsed write FPestParametersUsed;
//    property ParameterLayersUsed: string read FParameterLayersUsed
//      write FParameterLayersUsed;
  end;

{
  @abstract(@name provides a mechanism for the user to
  edit the @link(TDataArray)s and their properties.)

  When the @name is created, a number of variables are initialized and then
  @link(GetData) is called.  @link(GetData) reads all the
  @link(TDataArray)s and displays their
  properties.  If the user clicks on the OK button, the data sets are updated
  to reflect any changes that the user has made.  The data sets are stored
  in instances of @link(TDataArrayEdit) that are stored in @link(FArrayEdits).

  @link(CurrentInterpolator) is the @link(TCustom2DInterpolater)
  of the currently
  selected data set.  Its properties are displayed in other controls.

  If the user presses the OK button, @link(SetData)
  will be called and the changes
  that the user made will be accepted.  If the user closes the dialog box in
  some other way (such as by pressing the Cancel button), the changes
  the user made will be ignored.
}
  TfrmDataSets = class(TfrmCustomGoPhast)
    // @name is used to add a new @link(TDataArray).
    // See @link(btnAddClick).
    btnAdd: TButton;
    // @name is used to close the dialog box without doing anything.
    // See @link(btnCancelClick).
    btnCancel: TBitBtn;
    // @name is used to delete a @link(TDataArray).
    // See @link(btnDeleteClick).
    btnDelete: TButton;
    // @name: TBitBtn;
    // @name is used to show help for the @classname.
    btnHelp: TBitBtn;
    // @name is used to accept all changes made in the dialog box and close the
    // dialog box.
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name is the TPanel holding buttons at the bottom of the @classname.
    pnlButtons: TPanel;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at elements on the front view of the model.
    rpFrontFormulaCompiler: TRbwParser;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at nodes on the front view of the model.
    rpFrontFormulaCompilerNodes: TRbwParser;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at elements on the side view of the model.
    rpSideFormulaCompiler: TRbwParser;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at nodes on the side view of the model.
    rpSideFormulaCompilerNodes: TRbwParser;
    // @name is used to handle formulas for 3D @link(TDataArray)s
    // evaluated at elements.
    rpThreeDFormulaCompiler: TRbwParser;
    // @name is used to handle formulas for 3D @link(TDataArray)s
    // evaluated at nodes.
    rpThreeDFormulaCompilerNodes: TRbwParser;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at elements on the top view of the model.
    rpTopFormulaCompiler: TRbwParser;
    // @name is used to handle formulas for 2D @link(TDataArray)s
    // evaluated at nodes on the top view of the model.
    rpTopFormulaCompilerNodes: TRbwParser;
    // @name is the status bar at the bottom of the dialog box.
    sbStatusBar: TStatusBar;
    // The terminal nodes of @name are used to select a @link(TDataArray)
    // to be edited.
    tvDataSets: TTreeView;
    pcDataSets: TJvPageControl;
    tabBasic: TTabSheet;
    lblName: TLabel;
    edName: TRbwEdit;
    lblType: TLabel;
    lblOrientation: TLabel;
    comboType: TJvImageComboBox;
    comboOrientation: TJvImageComboBox;
    lblEvaluatedAt: TLabel;
    lblUnits: TLabel;
    comboEvaluatedAt: TJvImageComboBox;
    lblInterpolation: TLabel;
    lblAnisotropy: TLabel;
    comboInterpolation: TJvImageComboBox;
    rdeAnisotropy: TRbwDataEntry;
    lblDefaultFormula: TLabel;
    btnEditFormula: TButton;
    tabPHAST: TTabSheet;
    framePhastInterpolation: TframePhastInterpolation;
    tabComment: TTabSheet;
    Splitter1: TSplitter;
    pnlComment: TPanel;
    lblComment: TLabel;
    pnlDescription: TPanel;
    lblAssociatedDataSets: TLabel;
    memoAssociatedDataSets: TMemo;
    Splitter2: TSplitter;
    reDefaultFormula: TRichEdit;
    reComment: TRichEdit;
    // @name displays the units with which the selected data set is measured.
    comboUnits: TComboBox;
    tabParameters: TTabSheet;
    cbParametersUsed: TCheckBox;
    // @name adds a new @link(TDataArray) at the end of @link(tvDataSets).
    procedure btnAddClick(Sender: TObject);
    // @name closes the @classname without making any changes to the
    // @link(TDataArray)s.
    procedure btnCancelClick(Sender: TObject);
    // @name deletes the @link(TDataArray) in
    // the selected item of @link(tvDataSets).
    procedure btnDeleteClick(Sender: TObject);
    // @name accepts the changes in the @link(TDataArray)s and closes the
    // @classname.
    procedure btnOKClick(Sender: TObject);
    // @name initializes many things when @classname is created.
    procedure FormCreate(Sender: TObject); override;
    // @name frees private fields of @classname when @classname is destroyed.
    procedure FormDestroy(Sender: TObject); override;
    // @name responds to framePhastInterpolation.@link(
    // TframePhastInterpolation.btnEditMixtureFormula) being clicked.
    // It allows the user to edit the formula for mixtures.
    procedure framePhastInterpolationbtnEditMixtureFormulaClick(
      Sender: TObject);
    // @name responds to framePhastInterpolation.@link(
    // TframePhastInterpolation.cbPhastInterpolation) being clicked.
    // It activates or deactivates PHAST-style interpolation.
    procedure framePhastInterpolationcbPhastInterpolationClick(
      Sender: TObject);
    // @name responds to the user editing the formula for mixtures
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationedMixFormulaChange(Sender: TObject);
    // @name responds to the user editing the first distance
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationrdeDistance1Change(Sender: TObject);
    // @name responds to the user editing the second distance
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationrdeDistance2Change(Sender: TObject);
    // @name responds to the user editing the first value
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationrdeValue1Change(Sender: TObject);
    // @name responds to the user editing the second value
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationrdeValue2Change(Sender: TObject);
    // @name responds to the user editing the interpolation direction
    // by storing the new value. See @link(TPhastInterpolationValues).
    procedure framePhastInterpolationrgInterpolationDirectionClick(
      Sender: TObject);
    // @name responds to the user editing the anisotropy
    // by storing the new value. 
    procedure rdeAnisotropyChange(Sender: TObject);
    // See @link(comboType).
    procedure comboTypeChange(Sender: TObject);
    // See @link(comboOrientation).
    procedure comboOrientationChange(Sender: TObject);
    // See @link(comboEvaluatedAt).
    procedure comboEvaluatedAtChange(Sender: TObject);
    // See @link(comboInterpolation).
    procedure comboInterpolationChange(Sender: TObject);
    // See @link(reDefaultFormula).
    procedure reDefaultFormulaExit(Sender: TObject);
    // @name changes the selected @link(TDataArray).
    procedure tvDataSetsChange(Sender: TObject; Node: TTreeNode);
    // See @link(btnEditFormula).
    procedure btnEditFormulaClick(Sender: TObject);
    // See @link(edName).
    procedure edNameExit(Sender: TObject);
    procedure reCommentExit(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure reCommentEnter(Sender: TObject);
    procedure comboUnitsChange(Sender: TObject);
    procedure tvDataSetsChanging(Sender: TObject; Node: TTreeNode;
      var AllowChange: Boolean);
    procedure cbParametersUsedClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tvDataSetsMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
  private
    { @name is the @link(TCustom2DInterpolater) of the currently
      selected @link(TDataArray).
    }
    // When the user makes a change that could make a formula invalid,
    // the user will be prompted to either automatically adjust the formula
    // or to change the data type of the data set.  The most suitable
    // default response will vary depending on what the user has done.
    // @name is used to store the default response that will
    // be presented to the user.
    // -1: The user must select what to do.
    // 0: change the data type of the data set.
    // 1: automatically adjust the formula.
    FDefaultConvertChoice: integer;
    {
      As its name implies, @name contains the @link(TDataArray)s that the user
      wants to delete.
    }
    FDeletedDataSets: TList;
    {
      @name contains a list of @link(TInterpolatorType)s. It is used
      to retrieve the type of a @link(TCustom2DInterpolater) based on its
      @link(TCustom2DInterpolater.InterpolatorName)
      and to create new interpolator instances when the
      user changes the type of interpolation to be performed.
    }
    FInterpolatorList: TList;
    {
      @name is set to @True in @link(GetData) and elsewhere to prevent certain
      methods from going into infinite loops.
    }
    FLoading: boolean;
    // @name is used to restore the value of
    // frmGoPhast.Model.UpToDate after it has been changed in @Link(GetData).
    FPriorModelUpToDate: boolean;
    // @name stores a @link(TDataArrayEdit) for each @link(TDataArray).
    FArrayEdits: TList;
    // See @link(SelectedEdit).
    FSelectedEdit: TDataArrayEdit;
    {
      When a new data set has been added or the formula for a data set has
      changed, @name creates a TExpression for that new formula.
      If the formula is not valid, @name fixes it.
    }
    procedure CreateFormula(const DataArrayEdit: TDataArrayEdit);
    // @name creates a TExpression for the formula in
    // @link(framePhastInterpolation).@link(
    // TframePhastInterpolation.edMixFormula).
    function CreateMixtureFormula: TExpression;
    // @name creates a TCustomVariable
    // that represents a @link(TDataArray) in
    // the formula compilers. If a @link(TDataArray)
    // is already represented by a
    // variable, @LINK(DeleteVariable) should be
    // called before  @link(CreateVariable).
    procedure CreateVariable(const DataArrayEdit: TDataArrayEdit);
    // @name deletes the variable that represents the
    // @link(TDataArray) in Row in the formula compilers.
    procedure DeleteVariable(const DataEdit: TDataArrayEdit);
    // @name allows the user to click the OK button (@link(btnOK))
    // only if all the formulas are valid.
    procedure EnableOK_Button(ForceCheck: boolean = False);
    // @name generates a name for a @link(TDataArray) that is valid
    // and does not conflict with the names of any existing @link(TDataArray)s.
    function GenerateNewName(Root: string; const
      CurrentDataEdit: TDataArrayEdit): string;
    // @name returns the TRbwParser that is
    // used with @link(TDataArray)s with a dso3D
    // @link(TDataSetOrientation) and the
    // @link(TEvaluatedAt) specified in its parameter.
    function Get3DCompiler(const EvaluatedAt: TEvaluatedAt): TRbwParser;
    // @name returns the TRbwParser that is used with the combination
    // of Orientation and Evaluated specified in its parameters.
    function GetCompiler(const Orientation: TDataSetOrientation;
      const EvaluatedAt: TEvaluatedAt): TRbwParser;
    // @name stores the data from all the @link(TDataArray)s in the
    // @classname so the user can edit the @link(TDataArray)s.
    procedure GetData;
    // @name sets the @link(CurrentInterpolator).
    procedure SetCurrentInterpolator(const Value: TCustom2DInterpolater);
    // When the user presses the OK button. @name will be called to
    // update all the @link(TDataArray)s that
    // have been added, deleted, or changed.
    procedure SetData;
    // @name updates which @link(TDataArray) depends on which when
    // a formula is changed. The linkage information is
    // stored in @link(TDataArrayEdit.NewUses).
    procedure UpdateLinkages;
    // @name renames the variables that represent the
    // @link(TDataArrayEdit.DataArray) in the formulu compiler.
    procedure UpdateVariableName(const DataEdit: TDataArrayEdit;
      NewName: string);
    // @name creates variables in each of the @link(TRbwParser) to
    // represent the global variables.
    procedure GetGlobalVariables;
    // @name fills @link(tvDataSets) with a hierarchical view of the
    // @link(TDataArray)s in the model.
    procedure FillDataSetsTreeView;
    // See @link(SelectedEdit).
    procedure SetSelectedEdit(const Value: TDataArrayEdit);
    // @name updates the items shown in @link(comboInterpolation)
    // so that only valid choices are shown.
    procedure UpdateInterpolationChoice(const DataEditor: TDataArrayEdit);
    // @name returns the position in @link(FArrayEdits) of the
    // @link(TDataArrayEdit) whose Name is DataSetName.
    function GetDataSetIndex(DataSetName: string): integer;
    // @name checks that a formula is valid.
    // A formula is invalid if it can not be compiled or contains a circular
    // reference.
    procedure ValidateFormula(const DataEdit: TDataArrayEdit;
      NewFormula: string);
    // If a new @link(TDataArray) has been added,
    // @name initializes the @link(TDataArrayEdit) used to edit it.
    procedure InitializeNewDataEdit(DataEdit: TDataArrayEdit);
    function GetCurrentInterpolator: TCustom2DInterpolater;
    procedure InitializeControls;
    function ItemClassification(AnItem: TTreeNode): string;
    // @name is the @link(TCustom2DInterpolater) of the currently
    // selected item in @link(tvDataSets).
    property CurrentInterpolator: TCustom2DInterpolater
      read GetCurrentInterpolator write SetCurrentInterpolator;
    procedure UpdateMixtureAllowed(ADataSet: TDataArray);
    procedure SetInterpolationMethod(NewInterpolatorName: string);
    procedure UpdateInterpolationControl;
    procedure UpdateOkVariables(VariablePosition: Integer;
      const VariableName: string; var OK_Var: TOK_Variables);
    procedure InitializeOK_Variables(var OK_Var: TOK_Variables; EvaluatedAt: TEvaluatedAt);
    procedure FillCompilerList(CompilerList: TList);
    procedure ClearVariables;
//    procedure EnableParameterLayes;
    { Private declarations }
  protected
    // @name is the TDataArrayEdit that is currently being edited.
    property SelectedEdit: TDataArrayEdit read FSelectedEdit
      write SetSelectedEdit;
  public
    constructor Create(AOwner: TComponent); override;
    { Public declarations }
  end;

// @name adds all the available @link(TCustom2DInterpolater)s to List.
procedure AddInterpolatorsToList(const List: TList);

var
  frmDataSets: TfrmDataSets = nil;

implementation

uses frmGoPhastUnit, frmFormulaUnit, frmConvertChoiceUnit, InterpolationUnit,
  GIS_Functions, PhastModelUnit, frmShowHideObjectsUnit, GlobalVariablesUnit,
  StrUtils, OrderedCollectionUnit, HufDefinition, LayerStructureUnit,
  SubscriptionUnit, Menus, frmSelectResultToImportUnit, SutraOptionsUnit;

resourcestring
  StrNone = 'none';
  StrErrorThereAppears = 'Error: There appears to be a cirular reference to ' +
  '"%s" in this formula.  Do you wish to restore the old formula';
  StrErrorThereAppearsC = 'Error: There appears to be a cirular reference to ' +
  '"%s" in this formula.';
  StrIfYouHaveAnyUnus = 'If you have any unused objects, now might be a good' +
  ' time to delete them.';
  StrErrorForFormulaFo = 'Error for formula for data set "%0:s"; %1:s';
  StrDoYouWishToDoYo = 'Do you wish to restore the old formula?';
  StrSMixtureFormula = '%s Mixture Formula';
  StrDefinedByParamet = '%s (defined by parameters)';
  StrYouMustSelectAMo = 'You must select a model type (MODFLOW, SUTRA, etc.)' +
  ' before displaying the Data Sets dialog box.';
  StrSCanNotBeUsedB = '%s can not be used because plproc.exe is not in eithe' +
  'r the PEST or ModelMuse directory.';

{$R *.dfm}

procedure AddNewInterpolator(const List: TList; const AType: TInterpolatorType);
begin
  List.Add(AType);
end;

procedure AddInterpolatorsToList(const List: TList);
begin
  // Added in order from fastest to slowest.
  AddNewInterpolator(List, TNearestPoint2DInterpolator);
  AddNewInterpolator(List, TPointAverageInterpolator);
  AddNewInterpolator(List, TLinearSfrpackInterpolator);
  AddNewInterpolator(List, TFittedSurfaceIntepolator);
  AddNewInterpolator(List, TNaturalNeighborInterp);
  AddNewInterpolator(List, TNearest2DInterpolator);
  AddNewInterpolator(List, TInvDistSqPoint2DInterpolator);
  AddNewInterpolator(List, TInvDistSq2DInterpolator);
  AddNewInterpolator(List, TCustomPlProcInterpolator);
end;

procedure TfrmDataSets.FormActivate(Sender: TObject);
begin
  inherited;
  SelectedEdit := nil;
  GetData;
end;

procedure TfrmDataSets.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  inherited;
  Release;
  frmDataSets := nil;
end;

procedure TfrmDataSets.FormCreate(Sender: TObject);
begin
  inherited;
  if frmGoPhast.ModelSelection = msUndefined then
  begin
    Beep;
    ModalResult := mrCancel;
    MessageDlg(StrYouMustSelectAMo, mtError, [mbOK], 0);
    Exit;
  end;

  if not frmGoPhast.DataSetsPosition.IsEmpty then
  begin
    Position := poDesigned;
  end;

  FLoading := True;
  pcDataSets.ActivePageIndex := 0;
  FDefaultConvertChoice := -1;
  FPriorModelUpToDate := frmGoPhast.PhastModel.UpToDate;
  // Make sure each of the formula compilers has access to the
  // GIS functions.
  AddGIS_Functions(rpTopFormulaCompiler, frmGoPhast.PhastModel.ModelSelection, eaBlocks);
  AddGIS_Functions(rpFrontFormulaCompiler, frmGoPhast.PhastModel.ModelSelection, eaBlocks);
  AddGIS_Functions(rpSideFormulaCompiler, frmGoPhast.PhastModel.ModelSelection, eaBlocks);
  AddGIS_Functions(rpThreeDFormulaCompiler, frmGoPhast.PhastModel.ModelSelection, eaBlocks);
  AddGIS_Functions(rpTopFormulaCompilerNodes, frmGoPhast.PhastModel.ModelSelection, eaNodes);
  AddGIS_Functions(rpFrontFormulaCompilerNodes, frmGoPhast.PhastModel.ModelSelection, eaNodes);
  AddGIS_Functions(rpSideFormulaCompilerNodes, frmGoPhast.PhastModel.ModelSelection, eaNodes);
  AddGIS_Functions(rpThreeDFormulaCompilerNodes, frmGoPhast.PhastModel.ModelSelection, eaNodes);

    // create the list of deleted data sets.
  FDeletedDataSets := TList.Create;

  // Create a list of available interpolator classes.  This is used
  // to create new instances of interpolators.
  FInterpolatorList := TList.Create;
  AddInterpolatorsToList(FInterpolatorList);

  // Get the data for all the data sets.
  SelectedEdit := nil;
  GetData;

  Assert(frmDataSets = nil);
  frmDataSets := self;

  reDefaultFormula.DoubleBuffered := False;
  reComment.DoubleBuffered := False;

//{$IFNDEF SUTRA}
//  comboUnits.Items.Clear;
//{$ENDIF}
end;


procedure TfrmDataSets.UpdateInterpolationChoice(const DataEditor: TDataArrayEdit);
var
  Item: TJvImageItem;
  Index: Integer;
  InterpolatorType: TInterpolatorType;
begin
  {$IF CompilerVersion > 28}
  comboInterpolation.Items.ClearAndResetID;
  {$ENDIF}
  Item := comboInterpolation.Items.Add;
  Item.Text := StrNone;
  for Index := 0 to FInterpolatorList.Count - 1 do
  begin
    InterpolatorType := FInterpolatorList[Index];
    if (DataEditor.DataType in InterpolatorType.ValidReturnTypes)
      and (DataEditor.Orientation in
      InterpolatorType.ValidOrientations) then
    begin
      Item := comboInterpolation.Items.Add;
      Item.Text := InterpolatorType.InterpolatorName;
      Assert(SizeOf(TObject) = SizeOf(InterpolatorType));
      Item.LinkedObject := TObject(InterpolatorType);
    end;
  end;
end;

function TfrmDataSets.GenerateNewName(Root: string; const
  CurrentDataEdit: TDataArrayEdit): string;
var
  Names: TStringList;
  Index: integer;
  OtherDataEdit: TDataArrayEdit;
begin
  // This function generates a name for a data set that is valid
  // and does not conflict with the names of any existing data sets.
  Names := TStringList.Create;
  try
    for Index := 0 to FArrayEdits.Count - 1 do
    begin
      OtherDataEdit := FArrayEdits[Index];
      if (OtherDataEdit <> CurrentDataEdit) then
      begin
        Names.Add(OtherDataEdit.Name);
      end;
    end;

    // Get rid of any blanks.
    for Index := Names.Count - 1 downto 0 do
    begin
      if Names[Index] = '' then
      begin
        Names.Delete(Index);
      end;
    end;

    Names.AddStrings(frmGoPhast.PhastModel.Mf6TimesSeries.TimeSeriesNames);
    Names.AddStrings(frmGoPhast.PhastModel.GlobalVariables.GlobalVariableNames);

    result := PhastModelUnit.GenerateNewName(Root, Names);
  finally
    Names.Free;
  end;
end;

procedure TfrmDataSets.InitializeNewDataEdit(DataEdit: TDataArrayEdit);
begin
  FLoading := True;
  try
    DataEdit.Name := GenerateNewName('', nil);
    DataEdit.DisplayName := DataEdit.Name;
    DataEdit.DataType := rdtDouble;
    DataEdit.Orientation := dsoTop;
    DataEdit.EvaluatedAt := eaBlocks;
    DataEdit.Units := '';
    DataEdit.AngleType := atNone;
    DataEdit.Formula := '0';
    DataEdit.TwoDInterpolator := nil;
  finally
    FLoading := False;
  end;
end;


procedure TfrmDataSets.btnAddClick(Sender: TObject);
var
  DataArrayEdit: TDataArrayEdit;
  Node: TTreeNode;
begin
  inherited;
  // This procedure adds a new data set and initializes it.

  DataArrayEdit := TDataArrayEdit.Create(nil);
  FArrayEdits.Add(DataArrayEdit);
  InitializeNewDataEdit(DataArrayEdit);
  CreateVariable(DataArrayEdit);
  CreateFormula(DataArrayEdit);

  Node := tvDataSets.Items.AddChildObject(nil, '', DataArrayEdit);

  Node.Text := DataArrayEdit.Name;

  tvDataSets.Selected := Node;
end;

procedure TfrmDataSets.btnDeleteClick(Sender: TObject);
var
  DataSet: TDataArray;
  Temp: TDataArrayEdit;
  Node: TTreeNode;
  SelectionIndex: Integer;
  Index: Integer;
  DataArrayEdit: TDataArrayEdit;
begin
  inherited;
  // This procedure deletes a data set.

  if SelectedEdit <> nil then
  begin
    Node := tvDataSets.Selected;
    Assert(Node <> nil);
    Assert(Node.Data = SelectedEdit);
    // add the data set to the list of deleted data sets.
    DataSet := SelectedEdit.DataArray;
    if DataSet <> nil then
    begin
      if dcName in DataSet.Lock then
      begin
        Exit;
      end;
      FDeletedDataSets.Add(SelectedEdit.DataArray);
    end;
    // Get rid of the variable representing the data set.
    DeleteVariable(SelectedEdit);

    Temp := SelectedEdit;
    FSelectedEdit := nil;
    FArrayEdits.Remove(Temp);
    tvDataSets.Items.Delete(Node);
  end
  else if tvDataSets.SelectionCount > 1 then
  begin
    for SelectionIndex := tvDataSets.SelectionCount - 1 downto 0 do
    begin
      Node := tvDataSets.Selections[SelectionIndex];
      Assert(Node <> nil);
      Temp := Node.Data;
      if Temp <> nil then
      begin
        DataSet := Temp.DataArray;
      end
      else
      begin
        DataSet := nil;
      end;

      if DataSet <> nil then
      begin
        if dcName in DataSet.Lock then
        begin
          Continue;
        end;
        FDeletedDataSets.Add(Temp.DataArray);
      end;
      DeleteVariable(Temp);

      FArrayEdits.Remove(Temp);
      if not Node.HasChildren then
      begin
        tvDataSets.Items.Delete(Node);
      end;
    end;
  end;
  for Index := 0 to FArrayEdits.Count - 1 do
  begin
    DataArrayEdit := FArrayEdits[Index];
    ValidateFormula(DataArrayEdit, DataArrayEdit.Formula);
  end;
  EnableOK_Button(True);
end;

procedure TfrmDataSets.btnEditFormulaClick(Sender: TObject);
var
VarPosition: Integer;

  VariableList: TList;
  Expression: TExpression;
  Variable: TCustomVariable;
  Orientation: TDataSetOrientation;
  EvaluatedAt: TEvaluatedAt;
  Index: integer;
  TempUsesList: TStringList;
  VariableName: string;
  Used: TStringList;
  VariablePosition: integer;
  OldFormula: string;
  OldFormulaOK: boolean;
  NewFormula: string;
  TempIndex: integer;
  DataSetName: string;
  DataSetIndex: integer;
  UseList: TStringList;
  DataArrayEdit: TDataArrayEdit;
  OK_Var: TOK_Variables;
  DataArrayManager: TDataArrayManager;
  DataArray: TDataArray;
begin
  inherited;
  if FSelectedEdit = nil then
  begin
    Exit;
  end;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;

  VariableList := TList.Create;
  // VariableList will hold a list of variables that can
  // be used in the function
  Used := TStringList.Create;
  // "Used" will be a list of variables that depend on
  // the data set whose formula will be edited.
  try
    Orientation := FSelectedEdit.Orientation;
    EvaluatedAt := FSelectedEdit.EvaluatedAt;
    // Add the variable whose value is being set to "Used".

    Used.Assign(FSelectedEdit.NewUses);
    Used.Sorted := True;
    InitializeOK_Variables(OK_Var, EvaluatedAt);
    for Index := 0 to FArrayEdits.Count - 1 do
    begin
      DataArrayEdit := FArrayEdits[Index];
      if FSelectedEdit <> DataArrayEdit then
      begin
        VariableName := DataArrayEdit.Name;
        VariablePosition := Used.IndexOf(VariableName);
        if (VariablePosition < 0)
          and ((Orientation = dso3D) or (Orientation = DataArrayEdit.Orientation))
          and (EvaluatedAt = DataArrayEdit.EvaluatedAt) then
        begin
          // If the variable does not depend on the
          // data set whose formula is being edited
          // and it's orientation and EvaluatedAt are OK, the variable
          // can be used in the formula.
          VariableList.Add(DataArrayEdit.Variable);
        end;
        UpdateOkVariables(VariablePosition, VariableName, OK_Var);
      end;
    end;

    // if the user makes an invalid formula, it
    // may be necessary to restore an older formula but only
    // if the formula that was already present
    // was OK to begin with.
    OldFormulaOK := FSelectedEdit.Expression <> nil;
    OldFormula := FSelectedEdit.Formula;
//    with TfrmFormula.Create(self) do
    with frmFormula do
    begin
      try
        Initialize;
        IncludeGIS_Functions(EvaluatedAt);
        if not OK_Var.ActiveOK then
        begin
          RemoveActiveOnLayer;
        end;
        if not OK_Var.SpecifiedHeadOK then
        begin
          RemoveSpecifiedHeadOnLayer;
        end;
        if not OK_Var.GetVContOK then
        begin
          RemoveGetVCont;
        end;
        if not OK_Var.HufKxOK then
        begin
          RemoveHufKx;
        end;
        if not OK_Var.HufKyOK then
        begin
          RemoveHufKy;
        end;
        if not OK_Var.HufKzOK then
        begin
          RemoveHufKz;
        end;
        PopupParent := self;

        // register the appropriate variables with the
        // parser.
        for Index := 0 to VariableList.Count - 1 do
        begin
          Variable := VariableList[Index];
          DataArray := DataArrayManager.GetDataSetByName(Variable.Name);
          if (DataArray <> nil)
            and (not DataArray.Visible) or (DataArray is TCustomSparseDataSet) then
          begin
            Continue;
          end;
          rbFormulaParser.RegisterVariable(Variable);
        end;

        // show the variables and functions
        IncludeTimeSeries := False;
        UpdateTreeList;

        // put the formula in the TfrmFormula.
        Formula := OldFormula;

        // The user edits the formula.
        ShowModal;
        if ResultSet then
        begin
          NewFormula := Formula;
          if FSelectedEdit.Formula <> NewFormula then
          begin
            FSelectedEdit.Formula := NewFormula;
            CreateFormula(FSelectedEdit);
            Expression := FSelectedEdit.Expression;

            if Expression <> nil then
            begin
              // Check that the formula does not result in
              // a circular reference.
              for Index := 0 to Used.Count - 1 do
              begin
                VariableName := Used[Index];
                // VarPosition is the row in the table that stores
                // data for the variable.
                VarPosition := GetDataSetIndex(VariableName);
                if VarPosition >= 0 then
                begin
                  DataArrayEdit := FArrayEdits[VarPosition];
                  if Expression.UsesVariable(DataArrayEdit.Variable) then
                  begin
                    FSelectedEdit.Expression := nil;
                    Beep;
                    if OldFormulaOK then
                    begin
                      if MessageDlg(Format(StrErrorThereAppears, [Used[Index]]),
                        mtError, [mbYes, mbNo], 0) = mrYes then
                      begin
                        FSelectedEdit.Formula := OldFormula;
                        CreateFormula(FSelectedEdit);
                      end;
                    end
                    else
                    begin
                      MessageDlg(Format(StrErrorThereAppearsC, [Used[Index]]),
                        mtError, [mbOK], 0)
                    end;
                    Exit;
                  end
                end;
              end;

              // update the list of which variables depend on which
              // others.;
              TempUsesList := TStringList.Create;
              try
                TempUsesList.Assign(Expression.VariablesUsed);
                // TempUsesList now has a list of the variables/datasets
                // in the expression.
                for TempIndex := 0 to TempUsesList.Count - 1 do
                begin
                  // get the name of a variable
                  DataSetName := TempUsesList[TempIndex];
                  // get the row that has that variable.
                  DataSetIndex := GetDataSetIndex(DataSetName);
                  // Get the list of variables that depends on it.
                  if DataSetIndex >= 0 then
                  begin
                    DataArrayEdit := FArrayEdits[DataSetIndex];
                    UseList := DataArrayEdit.NewUses;
                    // Add the additional variables that depend on it because
                    // they depend on the variable/dataset being edited.
                    UseList.Add(FSelectedEdit.Name);
                    UseList.AddStrings(FSelectedEdit.NewUses);
                  end;
                end;
              finally
                TempUsesList.Free;
              end;
              UpdateLinkages;
              reDefaultFormula.Text := FSelectedEdit.Formula;
              reDefaultFormula.Invalidate;
            end;
          end;
        end;
      finally
        Initialize;
//        Free;
      end;
    end;
  finally
    Used.Free;
    VariableList.Free;
    // Don't allow the user to click the OK button if any formulas are invalid.
    EnableOK_Button;
  end;
end;

procedure TfrmDataSets.GetGlobalVariables;
var
  CompilerList: TList;
begin
  CompilerList := TList.Create;
  try
    FillCompilerList(CompilerList);
    frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmDataSets.InitializeControls;
var
  Eval: TEvaluatedAt;
  EvalString: string;
begin
  pcDataSets.ActivePageIndex := 0;
  memoAssociatedDataSets.WordWrap := True;
  for Eval := Low(TEvaluatedAt) to High(TEvaluatedAt) do
  begin
    EvalString := EvalAtToString(Eval, frmGoPhast.ModelSelection, True, True);
    comboEvaluatedAt.Items[Ord(Eval)].Text := EvalString;
  end;
  case frmGoPhast.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        comboOrientation.Items[1].Brush.Color := clWhite;
        comboOrientation.Items[2].Brush.Color := clWhite;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msSutra22, msSutra30, msSutra40,
      msModflow2015
      {$IFDEF OWHMV2}
      , msModflowOwhm2
      {$ENDIF}
      :
      begin
        comboOrientation.Items[1].Brush.Color := clBtnFace;
        comboOrientation.Items[2].Brush.Color := clBtnFace;
      end;
    msFootPrint:
      begin
        comboOrientation.Items[1].Brush.Color := clBtnFace;
        comboOrientation.Items[2].Brush.Color := clBtnFace;
        comboOrientation.Items[3].Brush.Color := clBtnFace;
      end
    else
      Assert(False);
  end;
  tabPHAST.TabVisible := frmGoPhast.ModelSelection = msPhast;
end;

function TfrmDataSets.ItemClassification(AnItem: TTreeNode): string;
begin
  result := AnItem.Text;
  AnItem := AnItem.Parent;
  while AnItem <> nil do
  begin
    result := AnItem.Text + '|' + result;
    AnItem := AnItem.Parent;
  end;
end;

procedure TfrmDataSets.GetData;
var
  index: Integer;
  AnItem: TTreeNode;
  Expanded: Boolean;
begin
  FLoading := True;
  try
    InitializeControls;
    ClearVariables;
    GetGlobalVariables;

    FillDataSetsTreeView;

    for index := 0 to tvDataSets.Items.Count - 1 do
    begin
      AnItem := tvDataSets.Items[index];
      if AnItem.HasChildren then
      begin
        if frmGoPhast.FDataSetsExpanded.TryGetValue(ItemClassification(AnItem), Expanded) then
        begin
          AnItem.Expanded := Expanded;
        end
        else
        begin
          AnItem.Expanded := False;
        end;
      end;
    end;
  finally
    FLoading := False;
  end;
end;

procedure TfrmDataSets.FillDataSetsTreeView;
var
  Index: Integer;
  SelectedDataArray: TDataArray;
  ClassificationList: TStringList;
  DataArrayEdit: TDataArrayEdit;
//  NodeList: TList;
  DataArray: TDataArray;
  DataSetList: TClassificationList;
  LayerDataSetList: TList;
  LayerClassificationList: TClassificationList;
  SutraLayerClassificationList: TClassificationList;
  Position: integer;
  SelectedName: string;
  HydrogeologicUnitNames: TStringList;
  HufDataArrays: TClassificationList;
  DataArrayManager: TDataArrayManager;
  SutraLayerDataSetList: TList;
begin
  { TODO : Nearly the same code is use in TfrmFormulaUnit, TFrmGridColor,
  TfrmScreenObjectProperties, and TfrmDataSets. Find a way to combine them. }
  tvDataSets.Items.Clear;

  if FArrayEdits = nil then
  begin
    FArrayEdits := TObjectList.Create;
  end;
  SelectedEdit := nil;
  FArrayEdits.Clear;
  if frmGoPhast.Grid <> nil then
  begin
    SelectedDataArray := frmGoPhast.PhastModel.ThreeDDataSet;
    if SelectedDataArray = nil then
    begin
      SelectedDataArray := frmGoPhast.Grid.ThreeDContourDataSet;
    end;
  end
  else
  begin
    SelectedDataArray := nil;
  end;
  if SelectedDataArray = nil then
  begin
    SelectedName := '';
  end
  else
  begin
    SelectedName := SelectedDataArray.Name;
  end;

  HydrogeologicUnitNames := TStringList.Create;
  HufDataArrays := TClassificationList.Create;
  DataSetList := TClassificationList.Create;
  LayerClassificationList := TClassificationList.Create;
  SutraLayerClassificationList := TClassificationList.Create;
  LayerDataSetList := TList.Create;
  SutraLayerDataSetList := TList.Create;
  ClassificationList := TStringList.Create;
  try
    frmGoPhast.PhastModel.HydrogeologicUnits.FillDataArrayNames(
      HydrogeologicUnitNames);
    HydrogeologicUnitNames.CaseSensitive := False;
    for Index := 0 to HydrogeologicUnitNames.Count - 1 do
    begin
      HufDataArrays.Add(nil);
    end;

    frmGoPhast.PhastModel.GetModflowLayerGroupDataSets(LayerDataSetList);
    for Index := 0 to LayerDataSetList.Count - 1 do
    begin
      LayerClassificationList.Add(nil);
    end;

    frmGoPhast.PhastModel.GetSutraLayerGroupDataSets(SutraLayerDataSetList);
    for Index := 0 to SutraLayerDataSetList.Count - 1 do
    begin
      SutraLayerClassificationList.Add(nil);
    end;

    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataArray := DataArrayManager.DataSets[Index];
      if not DataArray.Visible then
      begin
        continue;
      end;
      DataArrayEdit:= TDataArrayEdit.Create(DataArray);
      FArrayEdits.Add(DataArrayEdit);
      CreateVariable(DataArrayEdit);
      DataSetList.Add(DataArrayEdit);
      Position := LayerDataSetList.IndexOf(DataArray);
      if Position >= 0 then
      begin
        LayerClassificationList[Position] := DataArrayEdit;
      end;

      Position := SutraLayerDataSetList.IndexOf(DataArray);
      if Position >= 0 then
      begin
        SutraLayerClassificationList[Position] := DataArrayEdit;
      end;

      Position := HydrogeologicUnitNames.IndexOf(DataArray.Name);
      if Position >= 0 then
      begin
        HufDataArrays[Position] := DataArrayEdit;
      end;
    end;

    ClassifyListedObjects(ClassificationList, DataSetList,
      [LayerClassificationList, SutraLayerClassificationList, HufDataArrays]);

    Assert(ClassificationList.Count> 0);
    Assert(ClassificationList[0] = StrDataSets);
    ClassificationList.Delete(0);
    Assert(ClassificationList.IndexOf(StrDataSets) < 0);

    CreateClassifiedNodes(ClassificationList, 1, tvDataSets, SelectedName);

    for Index := 0 to FArrayEdits.Count - 1 do
    begin
      DataArrayEdit := FArrayEdits[Index];
      CreateFormula(DataArrayEdit);
    end;

  finally
    SutraLayerDataSetList.Free;
    DataSetList.Free;
    LayerDataSetList.Free;
    LayerClassificationList.Free;
    SutraLayerClassificationList.Free;
    ClassificationList.Free;
    HufDataArrays.Free;
    HydrogeologicUnitNames.Free;
  end;
end;

procedure TfrmDataSets.SetData;
var
  Undo: TUndoChangeDataSets;
  DeletedDataSets, NewDataSets: TList;
  Index: Integer;
  DataSet: TDataArray;
  DataStorage: TPhastDataSetStorage;
  NewDataSetProperties : TObjectList;
  ArrayEdit: TDataArrayEdit;
  DataSetsDeleted: Boolean;
  ANode: TTreeNode;
  Key: string;
begin
  // This procedure updates the data sets based on the changes the
  // user has made.
  Screen.Cursor := crHourGlass;
  try
    memoAssociatedDataSets.WordWrap := False;

    DeletedDataSets := TList.Create;
    NewDataSets:= TList.Create;
    NewDataSetProperties := TObjectList.Create;

    try
      // store deleted data sets.
      for Index := 0 to FDeletedDataSets.Count - 1 do
      begin
        DeletedDataSets.Add(FDeletedDataSets[Index]);
      end;


      // store the new properties of the data sets.
      for Index := 0 to FArrayEdits.Count - 1 do
      begin
        ArrayEdit := FArrayEdits[Index];
        DataSet := ArrayEdit.DataArray;
        // create a new data set if required.
        if (DataSet = nil) and (ArrayEdit.Name <> '') then
        begin
          ArrayEdit.UpdateDataSet;
          DataSet := ArrayEdit.DataArray;
          NewDataSets.Add(DataSet);
          DataSet.Orientation := ArrayEdit.Orientation;
          DataSet.EvaluatedAt := ArrayEdit.EvaluatedAt;
          DataSet.Datatype := ArrayEdit.Datatype;
//          DataSet.ParametersUsed := ArrayEdit.ParametersUsed;
        end;

        // set the data set properties except for the formula.
        if (DataSet <> nil) and (ArrayEdit.Name <> '') then
        begin
          DataStorage := TPhastDataSetStorage.Create;
          NewDataSetProperties.Add(DataStorage);
          DataStorage.DataSet := DataSet;

          DataStorage.Name := ArrayEdit.Name;
          DataStorage.Orientation := ArrayEdit.Orientation;

          DataStorage.EvaluatedAt := ArrayEdit.EvaluatedAt;

          DataStorage.Datatype := ArrayEdit.Datatype;
          DataStorage.Units := ArrayEdit.Units;
          DataStorage.AngleType := ArrayEdit.AngleType;

          DataStorage.TwoDInterpolator := ArrayEdit.TwoDInterpolator;
          DataStorage.Formula := ArrayEdit.Formula;

          DataStorage.Comment := ArrayEdit.Comment;
          DataStorage.Classification := ArrayEdit.Classification;

          DataStorage.PestParametersUsed := ArrayEdit.PestParametersUsed;
//          DataStorage.ParameterLayersUsed := ArrayEdit.ParameterLayersUsed;

          if (DataStorage.Name <> DataSet.Name)
            and (Pos(StrModelResults, DataSet.Classification) > 0) then
          begin
            DataStorage.Classification := '';
          end;

          if DataSet is TCustomPhastDataSet then
          begin
            DataStorage.PhastInterpolationValues :=
              ArrayEdit.InterpValues;
          end;
        end;
      end;

      DataSetsDeleted := DeletedDataSets.Count > 0;
      Undo := TUndoChangeDataSets.Create(DeletedDataSets, NewDataSets,
        NewDataSetProperties);
    finally
      NewDataSetProperties.Free;
      NewDataSets.Free;
      DeletedDataSets.Free;
    end;
    if Undo.DataSetsChanged then
    begin
      frmGoPhast.UndoStack.Submit(Undo);
      if DataSetsDeleted then
      begin
        Beep;
        MessageDlg(StrIfYouHaveAnyUnus, mtInformation, [mbOK], 0);
      end;
    end
    else
    begin
      Undo.Free;
    end;
  finally
    Screen.Cursor := crDefault;
  end;
  frmGoPhast.FDataSetsExpanded.Clear;
  for Index := 0 to tvDataSets.Items.Count - 1 do
  begin
    ANode := tvDataSets.Items[Index];
    if ANode.HasChildren then
    begin
      Key := ItemClassification(ANode);
      if not frmGoPhast.FDataSetsExpanded.ContainsKey(Key) then
      begin
        frmGoPhast.FDataSetsExpanded.Add(Key, ANode.Expanded);
      end;

    end;
  end;
end;

procedure TfrmDataSets.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
  SelectedEdit := nil;
  GetData;
end;

procedure TfrmDataSets.cbParametersUsedClick(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  SelectedEdit.PestParametersUsed := cbParametersUsed.Checked;
end;

procedure TfrmDataSets.comboEvaluatedAtChange(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  SelectedEdit.EvaluatedAt := TEvaluatedAt(comboEvaluatedAt.ItemIndex);
end;

procedure TfrmDataSets.comboInterpolationChange(Sender: TObject);
var
  Item: TJvImageItem;
  InterpolatorType: TInterpolatorType;
  Anisotropy: double;
  NewInterpolator: TCustom2DInterpolater;
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  rdeAnisotropy.Enabled := comboInterpolation.ItemIndex >= 1;
  if comboInterpolation.ItemIndex >= 0 then
  begin
    Item := comboInterpolation.Items[comboInterpolation.ItemIndex];
    if Item.LinkedObject = nil then
    begin
      SelectedEdit.TwoDInterpolator := nil;
    end
    else
    begin
      InterpolatorType := TInterpolatorType(Item.LinkedObject);

      if (SelectedEdit.TwoDInterpolator = nil)
        or (SelectedEdit.TwoDInterpolator.ClassType  <> InterpolatorType) then
      begin
        NewInterpolator := InterpolatorType.Create(nil);
        try
          if NewInterpolator is TCustomAnisotropicInterpolator then
          begin
            if (SelectedEdit.TwoDInterpolator = nil)
              or not (SelectedEdit.TwoDInterpolator
              is TCustomAnisotropicInterpolator) then
            begin
              Anisotropy := 1;
              if TryStrToFloat(rdeAnisotropy.Text, Anisotropy) then
              begin
                TCustomAnisotropicInterpolator(NewInterpolator).
                  Anisotropy := Anisotropy;
              end;
            end
            else
            begin
                TCustomAnisotropicInterpolator(NewInterpolator).Anisotropy
                  := TCustomAnisotropicInterpolator(SelectedEdit.TwoDInterpolator).
                  Anisotropy;
            end;
          end;
          CurrentInterpolator := NewInterpolator;
        finally
          NewInterpolator.Free;
        end;
      end;
    end;
  end;
end;

procedure TfrmDataSets.comboOrientationChange(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  case frmGoPhast.ModelSelection of
    msUndefined:
      begin
        Assert(False);
      end;
    msPhast:
      begin
        SelectedEdit.Orientation :=
          TDataSetOrientation(comboOrientation.ItemIndex);
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msSutra22, msSutra30, msSutra40,
      msModflow2015
      {$IFDEF OWHMV2}
      , msModflowOwhm2
      {$ENDIF}
      :
      begin
        case comboOrientation.ItemIndex of
          0,3:
            begin
              SelectedEdit.Orientation :=
                TDataSetOrientation(comboOrientation.ItemIndex);
            end;
          1,2:
            begin
              // Front and side data set orientations are not allowed
              // in MODFLOW.
              // These items are drawn with a gray background to indicate
              // they can not be selected.
              comboOrientation.ItemIndex :=
                Ord(SelectedEdit.Orientation);
            end;
          else Assert(False);
        end;
      end;
    msFootPrint:
      begin
        case comboOrientation.ItemIndex of
          0:
            begin
              SelectedEdit.Orientation :=
                TDataSetOrientation(comboOrientation.ItemIndex);
            end;
          1,2,3:
            begin
              // Front, side and 3D data set orientations are not allowed
              // in MODFLOW.
              // These items are drawn with a gray background to indicate
              // they can not be selected.
              comboOrientation.ItemIndex :=
                Ord(SelectedEdit.Orientation);
            end;
          else Assert(False);
        end;
      end;
    else Assert(False);
  end;
  UpdateInterpolationControl;

  if SelectedEdit.Orientation = dso3D then
  begin
    comboInterpolation.Enabled := False;
  end
  else
  begin
    comboInterpolation.Enabled := True;
  end;
end;

procedure TfrmDataSets.comboTypeChange(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  SelectedEdit.DataType := TRbwDataType(comboType.ItemIndex);
  CreateVariable(SelectedEdit);
  UpdateInterpolationControl;
  reDefaultFormulaExit(nil);
end;

procedure TfrmDataSets.FormDestroy(Sender: TObject);
var
  FormPosition: TRect;
begin
  FormPosition.Top := Top;
  FormPosition.Left := Left;
  FormPosition.Width := Width;
  FormPosition.Height := Height;
  frmGoPhast.DataSetsPosition := FormPosition;

  SelectedEdit := nil;

  // free up data.
  FDeletedDataSets.Free;
  // FDataSetInterpolatorList and FArrayEdits are declared as TList's
  // but are instantiated
  // as TObjectList's so their contents are freed when they are.
  FInterpolatorList.Free;
  FArrayEdits.Free;

//  Assert(frmDataSets = self);
  frmDataSets := nil;
  inherited;
end;

procedure TfrmDataSets.FormShow(Sender: TObject);
var
  FormPosition: TRect;
begin
  inherited;
  FormPosition := frmGoPhast.DataSetsPosition;
  if not (FormPosition.IsEmpty) then
  begin
    Top := FormPosition.Top;
    Left := FormPosition.Left;
    Width := FormPosition.Width;
    Height := FormPosition.Height;
  end;
end;

procedure TfrmDataSets.ValidateFormula(const DataEdit: TDataArrayEdit;
  NewFormula: string);
var
  VariableList: TList;
  ArrayIndex: integer;
  Expression: TExpression;
  Variable: TCustomVariable;
  Orientation: TDataSetOrientation;
  Index: integer;
  TempUsesList: TStringList;
  VariableName: string;
  Used: TStringList;
  VariablePosition: integer;
  OldFormula: string;
  OldFormulaOK: boolean;
  TempIndex: integer;
  DataSetName: string;
  DataSetIndex: integer;
  UseList: TStringList;
  Value: string;
  AFormula: string;
  Tester: TRbwParser;
  CompilerList: TList;
  OtherDataEdit: TDataArrayEdit;
  ErrorMessage: string;
  EvalAt: TEvaluatedAt;
begin
  if csDestroying in ComponentState then
  begin
    Exit;
  end;
  // This procedure checks that a formula in a cell is valid.
  try
    // skip if the formula is a blank.
    Value := NewFormula;
    if Trim(Value) = '' then
    begin
      Exit;
    end;

    VariableList := TList.Create;
    // VariableList will hold a list of variables that can
    // be used in the function
    Used := TStringList.Create;
    // "Used" will be a list of variables that depend on
    // the data set whose formula will be edited.
    try
      Orientation := DataEdit.Orientation;
      // Add the variable whose value is being set to "Used".
      EvalAt := DataEdit.EvaluatedAt;

      Used.Assign(DataEdit.NewUses);

      Used.Sorted := True;

      for ArrayIndex := 0 to FArrayEdits.Count - 1 do
      begin
        OtherDataEdit := FArrayEdits[ArrayIndex];
        if OtherDataEdit <> DataEdit then
        begin
          VariableName := OtherDataEdit.Name;
          VariablePosition := Used.IndexOf(VariableName);
          if (VariablePosition < 0) and ((Orientation = dso3D)
            or (Orientation = OtherDataEdit.Orientation))
            and (EvalAt = OtherDataEdit.EvaluatedAt) then
          begin
            // if the variable does not depend on the
            // data set whose formula is being edited
            // and it's orientation is OK, the variable
            // can be used in the formula.
            VariableList.Add(OtherDataEdit.Variable);
          end;
        end;
      end;

      // if the user makes an invalid formula, it
      // may be necessary to restore it but only
      // if the formula that was already present
      // was OK to begin with.
      OldFormulaOK := DataEdit.Expression <> nil;
      OldFormula := DataEdit.Formula;
      Tester := TRbwParser.Create(self);
      //with Tester do
      begin
        try
          AddGIS_Functions(Tester, frmGoPhast.PhastModel.ModelSelection,
            DataEdit.EvaluatedAt);
          // put the formula in the TfrmFormula.
          AFormula := NewFormula;

          CompilerList := TList.Create;
          try
            CompilerList.Add(Tester);
            frmGoPhast.PhastModel.RefreshGlobalVariables(CompilerList);
          finally
            CompilerList.Free;
          end;

          // register the appropriate variables with the
          // parser.
          for Index := 0 to VariableList.Count - 1 do
          begin
            Variable := VariableList[Index];
            Tester.RegisterVariable(Variable);
          end;

          // Test that the formula is valid.
          try
            Tester.Compile(AFormula);
          except on E: ErbwParserError do
            begin
              sbStatusBar.Color := clRed;
              ErrorMessage := Format(StrErrorForFormulaFo,
                [DataEdit.Name, E.Message]);
              sbStatusBar.SimpleText := ErrorMessage;
              DataEdit.Expression := nil;
              DataEdit.Formula := AFormula;
              Beep;
              if OldFormulaOK then
              begin
                ErrorMessage := ErrorMessage + sLineBreak + sLineBreak
                  + StrDoYouWishToDoYo;
                 if MessageDlg(ErrorMessage, mtError,
                   [mbYes, mbNo], 0) = mrYes then
                 begin
                    DataEdit.Formula := OldFormula;
                    CreateFormula(DataEdit);
                 end;
              end
              else
              begin
                MessageDlg(ErrorMessage, mtError, [mbOK], 0);
              end;
              Exit;
            end;

          end;
          // The user edits the formula.
          DataEdit.Formula := NewFormula;
          CreateFormula(DataEdit);
          Expression := DataEdit.Expression;

          if Expression <> nil then
          begin
            // Check that the formula does not result in
            // a circular reference.
            for Index := 0 to Used.Count - 1 do
            begin
              VariableName := Used[Index];
              VariablePosition := GetDataSetIndex(VariableName);
              if VariablePosition >= 0 then
              begin
                OtherDataEdit := FArrayEdits[VariablePosition];
                if Expression.UsesVariable(OtherDataEdit.Variable) then
                begin
                  DataEdit.Expression := nil;
                  Beep;
                  if OldFormulaOK then
                  begin
                    if MessageDlg(Format(StrErrorThereAppears, [Used[Index]]),
                      mtError, [mbYes, mbNo], 0) = mrYes then
                    begin
                      DataEdit.Formula := OldFormula;
                      CreateFormula(DataEdit);
                    end;
                  end
                  else
                  begin
                    MessageDlg(Format(StrErrorThereAppearsC, [Used[Index]]),
                      mtError, [mbOK], 0);
                  end;

                  Exit;
                end
              end;
            end;

            // update the list of which variables depend on which
            // others.;
            TempUsesList := TStringList.Create;
            try
              TempUsesList.Assign(Expression.VariablesUsed);
              // TempUsesList now has a list of the variables/datasets
              // in the expression.
              for TempIndex := 0 to TempUsesList.Count - 1 do
              begin
                // get the name of a variable
                DataSetName := TempUsesList[TempIndex];
                // get the ArrayIndex that has that variable.
                DataSetIndex := GetDataSetIndex(DataSetName);
                if DataSetIndex >= 0 then
                begin
                  OtherDataEdit := FArrayEdits[DataSetIndex];
                  // Get the list of variables that depends on it.
                  UseList := OtherDataEdit.NewUses;
                  // Add the additional variables that depend on it because
                  // they depend on the variable/dataset being edited.
                  UseList.Add(DataEdit.Name);
                  UseList.AddStrings(DataEdit.NewUses);
                end;
              end;
            finally
              TempUsesList.Free;
            end;
          end;
        finally
          Tester.Free;
        end;
      end;
    finally
      if DataEdit = SelectedEdit then
      begin
        reDefaultFormula.Text := DataEdit.Formula;
        reDefaultFormula.Invalidate;
      end;
      Used.Free;
      VariableList.Free;
    end;
  finally
    // Don't allow the user to click the OK button if any formulas are invalid.
    EnableOK_Button;
  end;
end;

function TfrmDataSets.GetDataSetIndex(DataSetName: string): integer;
var
  ArrayIndex: integer;
  DataArrayEdit: TDataArrayEdit;
begin
  DataSetName := UpperCase(DataSetName);
  result := -1;
  for ArrayIndex := 0 to FArrayEdits.Count - 1 do
  begin
    DataArrayEdit := FArrayEdits[ArrayIndex];
    if SameText(DataArrayEdit.Name, DataSetName) then
    begin
      result := ArrayIndex;
      break;
    end;
  end;
  if result = -1 then
  begin
    for ArrayIndex := 0 to FArrayEdits.Count - 1 do
    begin
      DataArrayEdit := FArrayEdits[ArrayIndex];
      if (DataArrayEdit.DataArray <> nil) then
      begin
        if SameText(DataArrayEdit.DataArray.Name, DataSetName) then
        begin
          result := ArrayIndex;
          break;
        end;
      end;
    end;
  end;
end;

procedure TfrmDataSets.UpdateLinkages;
var
  Index, InnerIndex, DataSetIndex: integer;
  Changed: boolean;
  UseList, InnerUseList: TStringList;
  Count: integer;
  DataSetName: string;
  DataArrayEdit, InnerEdit: TDataArrayEdit;
begin
  repeat
    Changed := False;
    for Index := 0 to FArrayEdits.Count -1 do
    begin
      DataArrayEdit := FArrayEdits[Index];
      UseList := DataArrayEdit.NewUses;
      Count := UseList.Count;
      for InnerIndex := 0 to UseList.Count -1 do
      begin
        DataSetName := UseList[InnerIndex];
        DataSetIndex := GetDataSetIndex(DataSetName);
        if DataSetIndex >= 0 then
        begin
          // DataSetIndex may be less than 0 if a DataArray has been deleted.
          InnerEdit := FArrayEdits[DataSetIndex];
          // Get the list of variables that depends on it.
          InnerUseList := InnerEdit.NewUses;
          UseList.AddStrings(InnerUseList);
        end;
      end;
      if Count <> UseList.Count then
      begin
        Changed := True;
      end;
    end;
  until not Changed;
end;

function TfrmDataSets.Get3DCompiler(const EvaluatedAt: TEvaluatedAt):
  TRbwParser;
begin
  result := nil;
  case EvaluatedAt of
    eaBlocks:
      begin
        result := rpThreeDFormulaCompiler;
      end;
    eaNodes:
      begin
        result := rpThreeDFormulaCompilerNodes;
      end;
  else
    Assert(False);
  end;
end;

function TfrmDataSets.GetCompiler(const Orientation: TDataSetOrientation;
  const EvaluatedAt: TEvaluatedAt): TRbwParser;
begin
  result := nil;
  case EvaluatedAt of
    eaBlocks:
      begin
        case Orientation of
          dsoTop:
            begin
              result := rpTopFormulaCompiler;
            end;
          dsoFront:
            begin
              result := rpFrontFormulaCompiler;
            end;
          dsoSide:
            begin
              result := rpSideFormulaCompiler;
            end;
          dso3D:
            begin
              result := rpThreeDFormulaCompiler;
            end;
        else
          Assert(False);
        end;
      end;
    eaNodes:
      begin
        case Orientation of
          dsoTop:
            begin
              result := rpTopFormulaCompilerNodes;
            end;
          dsoFront:
            begin
              result := rpFrontFormulaCompilerNodes;
            end;
          dsoSide:
            begin
              result := rpSideFormulaCompilerNodes;
            end;
          dso3D:
            begin
              result := rpThreeDFormulaCompilerNodes;
            end;
        else
          Assert(False);
        end;
      end;
  else
    Assert(False);
  end;
end;

function TfrmDataSets.GetCurrentInterpolator: TCustom2DInterpolater;
begin
  if SelectedEdit = nil then
  begin
    result := nil;
  end
  else
  begin
    result := SelectedEdit.FTwoDInterpolator
  end;
end;

procedure TfrmDataSets.UpdateVariableName(const DataEdit: TDataArrayEdit;
  NewName: string);
var
  Variable: TCustomVariable;
  Position: integer;
  TempFormulaCompiler: TRbwParser;
  OldName: string;
  Index: integer;
  Expression: TExpression;
  Temp3DCompiler: TRbwParser;
  OtherDataEdit: TDataArrayEdit;
begin
  // This procedure renames the variables that represent the data
  // sets in the formulu compiler.
  if FLoading or (csCreating in ControlState) then
    Exit;
  if DataEdit = nil then
  begin
    Exit;
  end;

  if (DataEdit.DataArray <> nil) and (dcName in DataEdit.DataArray.Lock) then
  begin
    Exit;
  end;

  Variable := DataEdit.Variable;
  if DataEdit.Name <> NewName then
  begin
    DataEdit.Name := NewName;
    DataEdit.DisplayName := NewName;
  end;
  Assert(Variable <> nil);

  if Variable.Decompile <> NewName then
  begin
    NewName := GenerateNewName(NewName, DataEdit);
    if DataEdit.Name <> NewName then
    begin
      DataEdit.Name := NewName;
      DataEdit.DisplayName := NewName;
    end;
    TempFormulaCompiler := GetCompiler(DataEdit.Orientation, DataEdit.EvaluatedAt);
    Temp3DCompiler := Get3DCompiler(DataEdit.EvaluatedAt);

    OldName := Variable.Name;
    Position := TempFormulaCompiler.IndexOfVariable(OldName);
    if Position >= 0 then
    begin
      try
        TempFormulaCompiler.RenameVariable(Position, NewName, NewName);
      except on E: ERbwParserError do
        begin
          if E.ErrorType = 1 then
          begin
            NewName := GenerateNewName(DataEdit.Name, nil);
            DataEdit.Name := NewName;
            UpdateVariableName(DataEdit, NewName);
            Exit;
          end
          else
          begin
            raise;
          end;

        end;
      end;
    end;

    if TempFormulaCompiler <> Temp3DCompiler then
    begin
      Position := Temp3DCompiler.IndexOfVariable(OldName);
      if Position >= 0 then
      begin
        Temp3DCompiler.RenameVariable(Position, NewName, NewName);
      end;
    end;
    // update the displayed formulas to use the new name.
    for Index := 0 to FArrayEdits.Count - 1 do
    begin
      OtherDataEdit := FArrayEdits[Index];
      Expression := OtherDataEdit.Expression;
      if Expression <> nil then
      begin
        OtherDataEdit.Formula := Expression.DecompileDisplay;
      end;
    end;
  end;
end;

procedure TfrmDataSets.DeleteVariable(const DataEdit: TDataArrayEdit);
var
  Variable: TCustomVariable;
  VarName: string;
  TempFormulaCompiler: TRbwParser;
  Position: integer;
  Index: integer;
  Formula: TExpression;
  CompilerList: TList;
  OtherDataEdit: TDataArrayEdit;
begin
  if DataEdit = nil then
    Exit;
  // This procedure deletes the variable that represents the
  // data set at Row in the formula compilers.

  // get rid of formulas that use the variable.
  Variable := DataEdit.Variable;
  for Index := 0 to FArrayEdits.Count -1 do
  begin
    OtherDataEdit:= FArrayEdits[Index];
    Formula := OtherDataEdit.Expression;
    if Formula <> nil then
    begin
      if Formula.UsesVariable(Variable) then
      begin
        OtherDataEdit.Expression := nil;
      end;
    end;
  end;

  VarName := Variable.Name;
  CompilerList := TList.Create;
  try
    CompilerList.Add(rpTopFormulaCompiler);
    CompilerList.Add(rpFrontFormulaCompiler);
    CompilerList.Add(rpSideFormulaCompiler);
    CompilerList.Add(rpThreeDFormulaCompiler);
    CompilerList.Add(rpTopFormulaCompilerNodes);
    CompilerList.Add(rpFrontFormulaCompilerNodes);
    CompilerList.Add(rpSideFormulaCompilerNodes);
    CompilerList.Add(rpThreeDFormulaCompilerNodes);
    for Index := 0 to CompilerList.Count - 1 do
    begin
      TempFormulaCompiler := CompilerList[Index];
      Position := TempFormulaCompiler.IndexOfVariable(VarName);
      if (Position >= 0) then
      begin
        Variable := TempFormulaCompiler.Variables[Position] as TCustomVariable;
        TempFormulaCompiler.RemoveVariable(Variable);
      end;
    end;

  finally
    CompilerList.Free;
  end;
end;

procedure TfrmDataSets.edNameExit(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  UpdateVariableName(SelectedEdit, edName.Text);
  if SelectedEdit.FNode <> nil then
  begin
    SelectedEdit.FNode.Text := SelectedEdit.Name;
  end;
end;

procedure TfrmDataSets.EnableOK_Button(ForceCheck: boolean = False);
var
  Index: integer;
  Enable: boolean;
  DataEdit: TDataArrayEdit;
begin
  // allow the user to click the OK button only if all the formulas
  // are valid.
  if ForceCheck or (ComponentState = [csFreeNotification]) then
  begin
    Enable := True;
    for Index := 0 to FArrayEdits.Count-1 do
    begin
      DataEdit := FArrayEdits[Index];
      if DataEdit.Expression = nil then
      begin
        Enable := False;
        break;
      end;
    end;
    btnOK.Enabled := Enable;
  end;
end;

function TfrmDataSets.CreateMixtureFormula: TExpression;
var
  Formula: string;
  TempCompiler: TRbwParser;
  ResultType: TRbwDataType;
  CellCenter: TPoint;
  ButtonCenter: TPoint;
  Lock: TDataLock;
//  Row: integer;
begin
  // Check that a formula is valid and if not, fix it.

  Formula := framePhastInterpolation.edMixFormula.Text;
  TempCompiler := GetCompiler(FSelectedEdit.Orientation, FSelectedEdit.EvaluatedAt);
  try
    TempCompiler.Compile(Formula);
    result := TempCompiler.CurrentExpression;
    framePhastInterpolation.edMixFormula.Text := result.DecompileDisplay;
    ResultType := rdtDouble;
    if not (result.ResultType in [rdtDouble, rdtInteger]) then
    begin
      // the formula is invalid: fix it.
      CellCenter.X := framePhastInterpolation.edMixFormula.Width div 2;
      CellCenter.Y := framePhastInterpolation.edMixFormula.Height div 2;
      CellCenter :=
        framePhastInterpolation.edMixFormula.ClientToScreen(CellCenter);

      Beep;

      with TfrmConvertChoice.Create(nil) do
      begin
        try
          PopUpParent := self;
          Lock := [dcType];

          GetData(Format(StrSMixtureFormula, [FSelectedEdit.Name]),
            result.ResultType, ResultType, FDefaultConvertChoice,
            Lock);
          ButtonCenter.X := btnOK.Width div 2;
          ButtonCenter.Y := pnlButton.Height;
          ButtonCenter := btnOK.ClientToScreen(ButtonCenter);
          Left := Left + CellCenter.X - ButtonCenter.X;
          Top := Top + CellCenter.Y - ButtonCenter.Y;
          if ShowModal = mrOK then
          begin
            if rgChoice.ItemIndex = 0 then
            begin
              Assert(False);
              // change the data type of the data set.
            end
            else
            begin
              // automatically adjust the formula.
              Formula := AdjustTheFormula(Formula);
              TempCompiler.Compile(Formula);
              result := TempCompiler.CurrentExpression;
              //Formulas[Row].Free;
              //Formulas[Row] := CompiledFormula;
              framePhastInterpolation.edMixFormula.Text
                := result.DecompileDisplay;
            end;
          end;
        finally
          Free;
        end;
      end;
    end;
  except
    on E: ERbwParserError do
    begin
      Beep;
      sbStatusBar.Color := clRed;
      sbStatusBar.SimpleText := Format(StrErrorForFormulaFo,
        [FSelectedEdit.Name, E.Message]);
      result := nil;
      Exit;
    end;
  else
    begin
      Beep;
      raise;
    end;
  end;
end;


procedure TfrmDataSets.CreateVariable(const DataArrayEdit: TDataArrayEdit);
var
  NewName: string;
  DataType: TRbwDataType;
  Variable: TCustomVariable;
  TempFormulaCompiler: TRbwParser;
  Temp3DCompiler: TRbwParser;
  Classification: string;
  DataArray: TDataArray;
  OldVarPosition: Integer;
  NewDisplayName: string;
begin
  Assert(DataArrayEdit <> nil);
  // This procedure creates a TCustomVariable that represents a data set in
  // the formula compilers.
  Variable := nil;

  TempFormulaCompiler := GetCompiler(DataArrayEdit.Orientation,
    DataArrayEdit.EvaluatedAt);
  Temp3DCompiler := Get3DCompiler(DataArrayEdit.EvaluatedAt);

  NewName := DataArrayEdit.Name;
  NewDisplayName := DataArrayEdit.DisplayName;

  DataArray := DataArrayEdit.DataArray;
  if DataArray = nil then
  begin
    Classification := strDefaultClassification;
  end
  else
  begin
    Classification := DataArray.FullClassification;
  end;

  OldVarPosition := TempFormulaCompiler.IndexOfVariable(NewName);
  if OldVarPosition >= 0 then
  begin
    Variable := TempFormulaCompiler.Variables[OldVarPosition] as TCustomVariable;
    TempFormulaCompiler.RemoveVariable(Variable);
  end;
  if TempFormulaCompiler <> Temp3DCompiler then
  begin
    OldVarPosition := Temp3DCompiler.IndexOfVariable(NewName);
    if OldVarPosition >= 0 then
    begin
      Variable := Temp3DCompiler.Variables[OldVarPosition] as TCustomVariable;
      Temp3DCompiler.RemoveVariable(Variable);
    end;
  end;

  DataType := DataArrayEdit.DataType;
  case DataType of
    rdtDouble:
      begin
        Variable := TempFormulaCompiler.CreateVariable(NewName,
          Classification, 0.0, NewDisplayName);
        if TempFormulaCompiler <> Temp3DCompiler then
        begin
          Temp3DCompiler.CreateVariable(NewName, Classification, 0.0, NewDisplayName);
        end;
      end;
    rdtInteger:
      begin
        Variable := TempFormulaCompiler.CreateVariable(NewName,
          Classification, 0, NewDisplayName);
        if TempFormulaCompiler <> Temp3DCompiler then
        begin
          Temp3DCompiler.CreateVariable(NewName, Classification, 0, NewDisplayName);
        end;
      end;
    rdtBoolean:
      begin
        Variable := TempFormulaCompiler.CreateVariable(NewName,
          Classification, False, NewDisplayName);
        if TempFormulaCompiler <> Temp3DCompiler then
        begin
          Temp3DCompiler.CreateVariable(NewName, Classification, False, NewDisplayName);
        end;
      end;
    rdtString:
      begin
        Variable := TempFormulaCompiler.CreateVariable(NewName,
          Classification, '0', NewDisplayName);
        if TempFormulaCompiler <> Temp3DCompiler then
        begin
          Temp3DCompiler.CreateVariable(NewName, Classification, '0', NewDisplayName);
        end;
      end;
  else
    Assert(False);
  end;
  DataArrayEdit.Variable := Variable;
end;

constructor TfrmDataSets.Create(AOwner: TComponent);
begin
  FLoading := True;
  inherited;
  FLoading := False;
end;

//procedure TfrmDataSets.EnableParameterLayes;
//begin
//end;

procedure TfrmDataSets.ClearVariables;
var
  CompilerList: TList;
  Compiler: TRbwParser;
  index: Integer;
begin
  CompilerList := TList.Create;
  try
    FillCompilerList(CompilerList);
    for index := 0 to CompilerList.Count - 1 do
    begin
      Compiler := CompilerList[index];
      Compiler.ClearVariables;
    end;
  finally
    CompilerList.Free;
  end;
end;

procedure TfrmDataSets.FillCompilerList(CompilerList: TList);
begin
  CompilerList.Add(rpFrontFormulaCompiler);
  CompilerList.Add(rpFrontFormulaCompilerNodes);
  CompilerList.Add(rpSideFormulaCompiler);
  CompilerList.Add(rpSideFormulaCompilerNodes);
  CompilerList.Add(rpThreeDFormulaCompiler);
  CompilerList.Add(rpThreeDFormulaCompilerNodes);
  CompilerList.Add(rpTopFormulaCompiler);
  CompilerList.Add(rpTopFormulaCompilerNodes);
end;

procedure TfrmDataSets.InitializeOK_Variables(var OK_Var: TOK_Variables; EvaluatedAt: TEvaluatedAt);
var
  GeoUnit: TLayerGroup;
  GeoIndex: Integer;
  HufIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufParamIndex: Integer;
  HufParam: THufUsedParameter;
  ActiveDataSet: TDataArray;
begin
  OK_Var.SpecifiedHeadOK := FSelectedEdit.Name <> rsModflowSpecifiedHead;
  OK_Var.ActiveOK := FSelectedEdit.Name <> rsActive;

  if FSelectedEdit.FDataArray <> nil then
  begin
    ActiveDataSet := frmGoPhast.PhastModel.DataArrayManager.
      GetDataSetByName(rsActive);

    if (ActiveDataSet <> nil) and
      ActiveDataSet.IsListeningTo(FSelectedEdit.FDataArray) then
    begin
      OK_Var.ActiveOK := False;
    end;
  end;

  OK_Var.GetVContOK := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsKz)
    and (FSelectedEdit.Name <> rsModflow_CBKz);
  OK_Var.HufKxOK := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsModflow_Initial_Head)
    and (FSelectedEdit.Name <> StrHufReferenceSurface)
    and (FSelectedEdit.Name <> kModelTop);
  OK_Var.HufKyOK := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsModflow_Initial_Head)
    and (FSelectedEdit.Name <> StrHufReferenceSurface)
    and (FSelectedEdit.Name <> kModelTop);
  OK_Var.HufKzOK := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsModflow_Initial_Head)
    and (FSelectedEdit.Name <> StrHufReferenceSurface)
    and (FSelectedEdit.Name <> kModelTop);
  OK_Var.HufSsOk := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsModflow_Initial_Head)
    and (FSelectedEdit.Name <> kModelTop);
  OK_Var.HufSyOk := (EvaluatedAt = eaBlocks)
    and (FSelectedEdit.Name <> rsModflow_Initial_Head)
    and (FSelectedEdit.Name <> kModelTop);
  for GeoIndex := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := frmGoPhast.PhastModel.LayerStructure[GeoIndex];
    if FSelectedEdit.Name = GeoUnit.DataArrayName then
    begin
      OK_Var.GetVContOK := False;
      OK_Var.HufKxOK := False;
      OK_Var.HufKyOK := False;
      OK_Var.HufKzOK := False;
      OK_Var.HufSsOk := False;
      OK_Var.HufSyOk := False;
    end;
  end;
  for HufIndex := 0 to frmGoPhast.PhastModel.HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := frmGoPhast.PhastModel.HydrogeologicUnits[HufIndex];
    if FSelectedEdit.Name = HufUnit.TopDataArrayName then
    begin
      OK_Var.HufKxOK := False;
      OK_Var.HufKyOK := False;
      OK_Var.HufKzOK := False;
      OK_Var.HufSsOk := False;
      OK_Var.HufSyOk := False;
    end;
    if FSelectedEdit.Name = HufUnit.ThickessDataArrayName then
    begin
      OK_Var.HufKxOK := False;
      OK_Var.HufKyOK := False;
      OK_Var.HufKzOK := False;
      OK_Var.HufSsOk := False;
      OK_Var.HufSyOk := False;
    end;
    for HufParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      HufParam := HufUnit.HufUsedParameters[HufParamIndex];
      case HufParam.Parameter.ParameterType of
        ptHUF_HK:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufKxOK := False;
                OK_Var.HufKyOK := False;
                OK_Var.HufKzOK := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufKxOK := False;
                OK_Var.HufKyOK := False;
                OK_Var.HufKzOK := False;
              end;
            end;
          end;
        ptHUF_HANI:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufKyOK := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufKyOK := False;
              end;
            end;
          end;
        ptHUF_VK:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufKzOK := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufKzOK := False;
              end;
            end;
          end;
        ptHUF_VANI:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufKzOK := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufKzOK := False;
              end;
            end;
          end;
        ptHUF_SS:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufSsOk := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufSsOk := False;
              end;
            end;
          end;
        ptHUF_SY:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufSyOk := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufSyOk := False;
              end;
            end;
          end;
        ptHUF_SYTP:
          begin
          end;
        ptHUF_KDEP:
          begin
            if HufParam.UseMultiplier then
            begin
              if FSelectedEdit.Name = HufParam.MultiplierArrayName then
              begin
                OK_Var.HufKxOK := False;
                OK_Var.HufKyOK := False;
                OK_Var.HufKzOK := False;
              end;
            end;
            if HufParam.UseZone then
            begin
              if FSelectedEdit.Name = HufParam.ZoneArrayName then
              begin
                OK_Var.HufKxOK := False;
                OK_Var.HufKyOK := False;
                OK_Var.HufKzOK := False;
              end;
            end;
          end;
        ptHUF_LVDA:
          begin
          end;
      else
        Assert(False);
      end;
    end;
  end
end;

procedure TfrmDataSets.UpdateOkVariables(VariablePosition: Integer;
  const VariableName: string;
  var OK_Var: TOK_Variables);
var
  HufParam: THufUsedParameter;
  HufParamIndex: Integer;
  HufUnit: THydrogeologicUnit;
  HufIndex: Integer;
  GeoIndex: Integer;
  GeoUnit: TLayerGroup;
begin
  if VariableName = rsActive then
  begin
    OK_Var.ActiveOK := OK_Var.ActiveOK and (VariablePosition < 0);
  end;
  if VariableName = rsModflowSpecifiedHead then
  begin
    OK_Var.SpecifiedHeadOK := OK_Var.SpecifiedHeadOK and (VariablePosition < 0);
  end;
  if VariableName = rsKz then
  begin
    OK_Var.GetVContOK := OK_Var.GetVContOK and (VariablePosition < 0);
  end;
  if VariableName = rsModflow_CBKz then
  begin
    OK_Var.GetVContOK := OK_Var.GetVContOK and (VariablePosition < 0);
  end;
  if VariableName = rsModflow_Initial_Head then
  begin
    OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
    OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
    OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
    OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
    OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
  end;
  if VariableName = StrHufReferenceSurface then
  begin
    OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
    OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
    OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
  end;
  if VariableName = kModelTop then
  begin
    OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
    OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
    OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
    OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
    OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
  end;
  for GeoIndex := 0 to frmGoPhast.PhastModel.LayerStructure.Count - 1 do
  begin
    GeoUnit := frmGoPhast.PhastModel.LayerStructure[GeoIndex];
    if VariableName = GeoUnit.DataArrayName then
    begin
      OK_Var.GetVContOK := OK_Var.GetVContOK and (VariablePosition < 0);
      OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
      OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
      OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
      OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
      OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
    end;
  end;
  for HufIndex := 0 to frmGoPhast.PhastModel.HydrogeologicUnits.Count - 1 do
  begin
    HufUnit := frmGoPhast.PhastModel.HydrogeologicUnits[HufIndex];
    if VariableName = HufUnit.TopDataArrayName then
    begin
      OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
      OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
      OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
      OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
      OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
    end;
    if VariableName = HufUnit.ThickessDataArrayName then
    begin
      OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
      OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
      OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
      OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
      OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
    end;
    for HufParamIndex := 0 to HufUnit.HufUsedParameters.Count - 1 do
    begin
      HufParam := HufUnit.HufUsedParameters[HufParamIndex];
      case HufParam.Parameter.ParameterType of
        ptHUF_HK:
          begin
            OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
            OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
            OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
          end;
        ptHUF_HANI:
          begin
            OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
          end;
        ptHUF_VK:
          begin
            OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
          end;
        ptHUF_VANI:
          begin
            OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
          end;
        ptHUF_SS:
          begin
            OK_Var.HufSsOk := OK_Var.HufSsOk and (VariablePosition < 0);
          end;
        ptHUF_SY:
          begin
            OK_Var.HufSyOk := OK_Var.HufSyOk and (VariablePosition < 0);
          end;
        ptHUF_SYTP:
          begin
          end;
        ptHUF_KDEP:
          begin
            OK_Var.HufKxOK := OK_Var.HufKxOK and (VariablePosition < 0);
            OK_Var.HufKyOK := OK_Var.HufKyOK and (VariablePosition < 0);
            OK_Var.HufKzOK := OK_Var.HufKzOK and (VariablePosition < 0);
          end;
        ptHUF_LVDA:
          begin
          end;
      else
        Assert(False);
      end;
    end;
  end;
end;

procedure TfrmDataSets.UpdateInterpolationControl;
var
  InterpolatorName: string;
begin
  InterpolatorName := comboInterpolation.Text;
  UpdateInterpolationChoice(SelectedEdit);
  SetInterpolationMethod(InterpolatorName);
end;

procedure TfrmDataSets.SetInterpolationMethod(NewInterpolatorName: string);
var
  InterpolationNames: TStringList;
  NewIndex: Integer;
  Item: TJvImageItem;
  Index: Integer;
begin
  InterpolationNames := TStringList.Create;
  try
    for Index := 0 to comboInterpolation.Items.Count - 1 do
    begin
      Item := comboInterpolation.Items[Index];
      InterpolationNames.Add(Item.Text);
    end;
    NewIndex := InterpolationNames.IndexOf(NewInterpolatorName);
    if NewIndex < 0 then
    begin
      NewIndex := 0;
    end;
    comboInterpolation.ItemIndex := NewIndex;
    comboInterpolationChange(nil);
  finally
    InterpolationNames.Free;
  end;
end;

procedure TfrmDataSets.CreateFormula(const DataArrayEdit: TDataArrayEdit);
var
  Formula: string;
  TempCompiler: TRbwParser;
  CompiledFormula: TExpression;
  ResultType: TRbwDataType;
  FormulaCenter: TPoint;
  ButtonCenter: TPoint;
  Lock: TDataLock;
begin
  Assert(DataArrayEdit <> nil);
  // Check that a formula is valid and if not, fix it.

  Formula := DataArrayEdit.Formula;
  TempCompiler := GetCompiler(DataArrayEdit.Orientation,
    DataArrayEdit.EvaluatedAt);
  try
    TempCompiler.Compile(Formula);
    CompiledFormula := TempCompiler.CurrentExpression;
    DataArrayEdit.Formula := CompiledFormula.DecompileDisplay;
    ResultType := DataArrayEdit.DataType;
    if (ResultType = CompiledFormula.ResultType) or
      ((ResultType = rdtDouble) and (CompiledFormula.ResultType = rdtInteger))
        then
    begin
      DataArrayEdit.Expression := CompiledFormula;
    end
    else
    begin
      // the formula is invalid: fix it.
      DataArrayEdit.Expression := nil;
      FormulaCenter.X := reDefaultFormula.Width div 2;
      FormulaCenter.Y := 0;
      FormulaCenter := reDefaultFormula.ClientToScreen(FormulaCenter);

      Beep;

      with TfrmConvertChoice.Create(nil) do
      begin
        try
          PopUpParent := self;
          if DataArrayEdit.DataArray = nil then
          begin
            Lock := [];
          end
          else
          begin
            Lock := DataArrayEdit.DataArray.Lock;
          end;

          GetData(DataArrayEdit.Name,
            CompiledFormula.ResultType, ResultType, FDefaultConvertChoice,
            Lock);
          ButtonCenter.X := btnOK.Width div 2;
          ButtonCenter.Y := pnlButton.Height;
          ButtonCenter := btnOK.ClientToScreen(ButtonCenter);
          Left := Left + FormulaCenter.X - ButtonCenter.X;
          Top := Top + FormulaCenter.Y - ButtonCenter.Y;
          if ShowModal = mrOK then
          begin
            if rgChoice.ItemIndex = 0 then
            begin
              // change the data type of the data set.
              DataArrayEdit.DataType := CompiledFormula.ResultType;

              DataArrayEdit.Expression := CompiledFormula;
            end
            else
            begin
              // automatically adjust the formula.
              Formula := AdjustTheFormula(Formula);
              TempCompiler.Compile(Formula);
              CompiledFormula := TempCompiler.CurrentExpression;
              Assert(CompiledFormula <> DataArrayEdit.Expression);
              //Formulas[Row].Free;
              DataArrayEdit.Expression := CompiledFormula;
              DataArrayEdit.Formula
                := CompiledFormula.DecompileDisplay;
            end;
          end;
        finally
          Free;
        end;
      end;
    end;
  except
    on E: ERbwParserError do
    begin
      DataArrayEdit.Expression := nil;
      Beep;
      sbStatusBar.Color := clRed;
      sbStatusBar.SimpleText := Format(StrErrorForFormulaFo,
        [DataArrayEdit.Name, E.Message]);
      Exit;
    end;
  else
    begin
      DataArrayEdit.Expression := nil;
      Beep;
      raise;
    end;
  end;
end;

procedure TfrmDataSets.UpdateMixtureAllowed(ADataSet: TDataArray);
var
  ChemDataSets: TList;
  DataArrayManager: TDataArrayManager;
begin
  ChemDataSets := TList.Create;
  try
    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solution));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Equilibrium_Phases));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Surface));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Exchange));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Gas_Phase));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Solid_Solutions));
    ChemDataSets.Add(DataArrayManager.GetDataSetByName(rsChemistry_Initial_Kinetics));
    framePhastInterpolation.SetMixtureAllowed(ChemDataSets.IndexOf(ADataSet) >= 0);
  finally
    ChemDataSets.Free;
  end;
end;

procedure TfrmDataSets.tvDataSetsChange(Sender: TObject; Node: TTreeNode);
var
  NewSelectedEdit: TDataArrayEdit;
  SelectionIndex: Cardinal;
  Temp: TDataArrayEdit;
  DataSet: TDataArray;
//  ShowParametersTab: Boolean;
begin
  inherited;
  reCommentExit(nil);
  NewSelectedEdit := nil;
  if (tvDataSets.Selected <> nil)
    and (tvDataSets.SelectionCount = 1) then
  begin
    NewSelectedEdit := tvDataSets.Selected.Data;
    if NewSelectedEdit <> nil then
    begin
      NewSelectedEdit.FNode := tvDataSets.Selected
    end;
  end;
  SelectedEdit := NewSelectedEdit;

  if tvDataSets.SelectionCount > 1 then
  begin
    for SelectionIndex := tvDataSets.SelectionCount - 1 downto 0 do
    begin
      Node := tvDataSets.Selections[SelectionIndex];
      Assert(Node <> nil);
      Temp := Node.Data;
      if Temp <> nil then
      begin
        DataSet := Temp.DataArray;
      end
      else
      begin
        DataSet := nil;
      end;

      if (DataSet = nil) or not (dcName in DataSet.Lock) then
      begin
        btnDelete.Enabled := True;
        break;
      end;
    end;
  end;
end;

procedure TfrmDataSets.tvDataSetsChanging(Sender: TObject; Node: TTreeNode;
  var AllowChange: Boolean);
begin
  inherited;
  edNameExit(nil);
end;

procedure TfrmDataSets.tvDataSetsMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  HitInfo: THitTests;
begin
  inherited;
  HitInfo := tvDataSets.GetHitTestInfoAt(X, Y);
  if (htOnLabel in HitInfo) and (tvDataSets.Selected <> nil)
    and (tvDataSets.Selected.Count > 0) then
  begin
    tvDataSets.Selected.Expanded := not tvDataSets.Selected.Expanded ;
  end;
end;

procedure TfrmDataSets.SetSelectedEdit(const Value: TDataArrayEdit);
var
  NewInterpolatorName: string;
  TabParamActive: Boolean;
  AnisotropyOptions: TSutraPestAnisotropyOptions;
begin

  if (FSelectedEdit <> Value) or (Value = nil) then
  begin
    reDefaultFormulaExit(nil);
    UpdateVariableName(FSelectedEdit, edName.Text);
    FSelectedEdit := Value;
    memoAssociatedDataSets.Lines.Clear;
    if FSelectedEdit = nil then
    begin
      btnDelete.Enabled := False;

      edName.Enabled := False;
      comboType.Enabled := False;
      comboOrientation.Enabled := False;
      comboEvaluatedAt.Enabled := False;
//      edUnits.Enabled := False;
      comboUnits.Enabled := False;
      comboInterpolation.Enabled := False;
      rdeAnisotropy.Enabled := False;
      reDefaultFormula.Enabled := False;
      framePhastInterpolation.Enabled := False;
      reComment.Enabled := False;
      tabParameters.TabVisible := False;
    end
    else
    begin
      cbParametersUsed.Checked := FSelectedEdit.PestParametersUsed;

      reComment.Text := FSelectedEdit.Comment;
      reComment.Enabled := True;

      btnDelete.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcName in FSelectedEdit.DataArray.Lock);

      edName.Text := FSelectedEdit.DisplayName;
      edName.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcName in FSelectedEdit.DataArray.Lock);

      comboType.ItemIndex := Ord(FSelectedEdit.DataType);
      comboType.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcType in FSelectedEdit.DataArray.Lock);

      comboOrientation.ItemIndex := Ord(FSelectedEdit.Orientation);
      comboOrientation.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcOrientation in FSelectedEdit.DataArray.Lock);

      comboEvaluatedAt.ItemIndex := Ord(FSelectedEdit.EvaluatedAt);
      comboEvaluatedAt.Enabled := (frmGoPhast.ModelSelection
        in [msPhast, msSutra22, msSutra30, msSutra40])
        and ((FSelectedEdit.DataArray = nil)
        or not (dcEvaluatedAt in FSelectedEdit.DataArray.Lock));

//      edUnits.Text := FSelectedEdit.Units;
//      edUnits.Enabled := (FSelectedEdit.DataArray = nil)
//        or not (dcUnits in FSelectedEdit.DataArray.Lock);

      comboUnits.Text := FSelectedEdit.Units;
      comboUnits.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcUnits in FSelectedEdit.DataArray.Lock);

      if FSelectedEdit.AngleType <> atNone then
      begin
        comboUnits.Text := comboUnits.Items[Ord(FSelectedEdit.AngleType)-1];
      end;

      UpdateInterpolationChoice(FSelectedEdit);
      comboInterpolation.Enabled := (comboInterpolation.Items.Count > 1)
        and ((FSelectedEdit.DataArray = nil)
        or not (dcInterpolation in FSelectedEdit.DataArray.Lock));
      if FSelectedEdit.TwoDInterpolator = nil then
      begin
        comboInterpolation.ItemIndex := 0;
        rdeAnisotropy.Enabled := False;
        rdeAnisotropy.Text := '1';
      end
      else
      begin
        NewInterpolatorName := FSelectedEdit.TwoDInterpolator.InterpolatorName;
        SetInterpolationMethod(NewInterpolatorName);

        if FSelectedEdit.TwoDInterpolator is TCustomAnisotropicInterpolator then
        begin
          rdeAnisotropy.Text := FloatToStr(
            TCustomAnisotropicInterpolator(FSelectedEdit.TwoDInterpolator).
            Anisotropy);
          rdeAnisotropy.Enabled := True;
        end
        else
        begin
          rdeAnisotropy.Enabled := False;
        end;
      end;

      reDefaultFormula.Text := FSelectedEdit.Formula;
      reDefaultFormula.Invalidate;
      reDefaultFormula.Enabled := (FSelectedEdit.DataArray = nil)
        or not (dcFormula in FSelectedEdit.DataArray.Lock);

      if FSelectedEdit.InterpValues = nil then
      begin
        framePhastInterpolation.Enabled := False;
      end
      else
      begin
        UpdateMixtureAllowed(FSelectedEdit.DataArray);
        framePhastInterpolation.AssigningValues := True;
        framePhastInterpolation.GetFirstData(FSelectedEdit.InterpValues);
        framePhastInterpolation.AssigningValues := False;
        framePhastInterpolation.Enabled :=
          frmGoPhast.PhastModel.ModelSelection = msPhast;
      end;
      memoAssociatedDataSets.Lines.Add(FSelectedEdit.AssociatedDataSets);

      TabParamActive := pcDataSets.ActivePageIndex = tabParameters.PageIndex;
      tabParameters.TabVisible := (FSelectedEdit.DataType = rdtDouble)
        and (FSelectedEdit.DataArray <> nil)
        and FSelectedEdit.DataArray.PestParametersAllowed
        and (Pos(StrLayerDefinition, FSelectedEdit.FullClassification) <= 0)
        and (FSelectedEdit.DataArray.Name <> rsWetDryThreshold)
        and (not FSelectedEdit.DataArray.ParameterUsed);
      if tabParameters.TabVisible and
        (frmGoPhast.ModelSelection in SutraSelection) then
      begin
        AnisotropyOptions := frmGoPhast.PhastModel.SutraOptions.PestAnisotropyOptions;
        if (FSelectedEdit.DataArray.Name = KMiddlePermeability)
          and AnisotropyOptions.UsePmaxPmidAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMiddleK)
          and AnisotropyOptions.UsePmaxPmidAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMinimumPermeability)
          and AnisotropyOptions.UsePmaxPminAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMinimumK)
          and AnisotropyOptions.UsePmaxPminAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMidLongitudinalDisp)
          and AnisotropyOptions.UseAlmaxAlmidAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMinLongitudinalDisp)
          and AnisotropyOptions.UseAlmaxAlminAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMidTransverseDisp)
          and AnisotropyOptions.UseAtmaxAtmidAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
        else if (FSelectedEdit.DataArray.Name = KMinTransverseDisp)
          and AnisotropyOptions.UseAtmaxAtminAnisotropy then
        begin
          tabParameters.TabVisible := False;
        end
      end;
      if TabParamActive and not tabParameters.TabVisible then
      begin
        pcDataSets.ActivePageIndex := 0;
      end;

      if pcDataSets.ActivePageIndex < 0 then
      begin
        pcDataSets.ActivePageIndex := 0;
      end;
    end;
    btnEditFormula.Enabled := reDefaultFormula.Enabled;
  end;
end;

procedure TfrmDataSets.rdeAnisotropyChange(Sender: TObject);
var
  Anisotropy: double;
begin
  inherited;
  if (framePhastInterpolation = nil)
    or framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  // update the anisotropy of the current interpolator.
//  if (rdeAnisotropy.Text <> '') and (CurrentInterpolator <> nil) then
//  begin
//    if (CurrentInterpolator is TCustomAnisotropicInterpolator) then
//    begin
//      TCustomAnisotropicInterpolator(CurrentInterpolator).Anisotropy :=
//        StrToFloat(rdeAnisotropy.Text);
//    end;
//  end;

  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  if (SelectedEdit.TwoDInterpolator <> nil) and
    (SelectedEdit.TwoDInterpolator is TCustomAnisotropicInterpolator) then
  begin
    Anisotropy := 1;
    if TryStrToFloat(rdeAnisotropy.Text, Anisotropy) then
    begin
      TCustomAnisotropicInterpolator(SelectedEdit.TwoDInterpolator).
        Anisotropy := Anisotropy;
    end;
  end;
end;

procedure TfrmDataSets.reCommentEnter(Sender: TObject);
begin
  inherited;
  frmGoPhast.acSelectAllTop.ShortCut := 0
end;

procedure TfrmDataSets.reCommentExit(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  SelectedEdit.Comment := reComment.Text;
  frmGoPhast.acSelectAllTop.ShortCut := ShortCut(Ord('A'), [ssCtrl]);
end;

procedure TfrmDataSets.reDefaultFormulaExit(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  ValidateFormula(SelectedEdit,
    StringReplace(reDefaultFormula.Text, sLineBreak, '', [rfReplaceAll]));
end;

procedure TfrmDataSets.SetCurrentInterpolator(
  const Value: TCustom2DInterpolater);
var
  InterpValues: TPhastInterpolationValues;
  PlProcName: string;
begin
  if SelectedEdit = nil then
    Exit;

  SelectedEdit.TwoDInterpolator := Value;

  // Update the display of the interpolator properties.
  if SelectedEdit.FTwoDInterpolator = nil then
  begin
    rdeAnisotropy.Enabled := False;
    if (SelectedEdit.DataArray <> nil)
      and (SelectedEdit.DataArray is TCustomPhastDataSet) then
    begin
      InterpValues := SelectedEdit.InterpValues;
      framePhastInterpolation.cbPhastInterpolation.Enabled := True;
      framePhastInterpolation.AssigningValues := True;
      framePhastInterpolation.GetFirstData(InterpValues);
      framePhastInterpolation.AssigningValues := False;
    end
    else
    begin
      if SelectedEdit.FTwoDInterpolator is TCustomPlProcInterpolator then
      begin
        PlProcName := TCustomPlProcInterpolator.GetPlprocName;
        if PlProcName = '' then
        begin
          Beep;
          MessageDlg(Format(StrSCanNotBeUsedB,
            [TCustomPlProcInterpolator.InterpolatorName]),
            mtWarning, [mbOK], 0);
        end;
      end;
      framePhastInterpolation.GetFirstData(nil);
    end;
  end
  else
  begin
    rdeAnisotropy.Enabled :=
      (SelectedEdit.FTwoDInterpolator is TCustomAnisotropicInterpolator);
    if rdeAnisotropy.Enabled then
    begin
      rdeAnisotropy.Text := FloatToStr(TCustomAnisotropicInterpolator(
        SelectedEdit.FTwoDInterpolator).Anisotropy);
      framePhastInterpolation.cbPhastInterpolation.Checked := False;
    end;
  end;
end;

procedure TfrmDataSets.framePhastInterpolationcbPhastInterpolationClick(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
    Exit;
  framePhastInterpolation.cbPhastInterpolationClick(Sender);
  if (SelectedEdit.DataArray <> nil) and
    (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    InterpValues.UsePHAST_Interpolation :=
      framePhastInterpolation.cbPhastInterpolation.Checked;
    if InterpValues.UsePHAST_Interpolation then
    begin
      if comboInterpolation.ItemIndex <> 0 then
      begin
      comboInterpolation.ItemIndex := 0;
        comboInterpolationChange(nil);
      end;
//      SelectedEdit.TwoDInterpolator := nil;
    end;
  end;
end;

procedure TfrmDataSets.framePhastInterpolationrdeDistance1Change(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
  begin
    Exit;
  end;
  if (framePhastInterpolation.rdeDistance1.Text <> '')
    and (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    InterpValues.Distance1 :=
      StrToFloat(framePhastInterpolation.rdeDistance1.Text);
  end;
end;

procedure TfrmDataSets.framePhastInterpolationrdeDistance2Change(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
  begin
    Exit;
  end;
  if (framePhastInterpolation.rdeDistance2.Text <> '')
    and (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    InterpValues.Distance2 :=
      StrToFloat(framePhastInterpolation.rdeDistance2.Text);
  end;     
end;

procedure TfrmDataSets.framePhastInterpolationrdeValue1Change(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
  begin
    Exit;
  end;

  if (framePhastInterpolation.rdeValue1.Text <> '')
    and (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    if (SelectedEdit.DataArray  is TIntegerPhastDataSet) then
    begin
      InterpValues.IntValue1 :=
        StrToInt(framePhastInterpolation.rdeValue1.Text);
    end
    else if (SelectedEdit.DataArray  is TRealPhastDataSet) then
    begin
      InterpValues.RealValue1 :=
        StrToFloat(framePhastInterpolation.rdeValue1.Text);
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmDataSets.framePhastInterpolationrdeValue2Change(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
    Exit;
  
  if (framePhastInterpolation.rdeValue2.Text <> '')
    and (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    if (SelectedEdit.DataArray is TIntegerPhastDataSet) then
    begin
      InterpValues.IntValue2 :=
        StrToInt(framePhastInterpolation.rdeValue2.Text);
    end
    else if (SelectedEdit.DataArray is TRealPhastDataSet) then
    begin
      InterpValues.RealValue2 :=
        StrToFloat(framePhastInterpolation.rdeValue2.Text);
    end
    else
    begin
      Assert(False);
    end;
  end;
end;

procedure TfrmDataSets.framePhastInterpolationrgInterpolationDirectionClick(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  framePhastInterpolation.rgInterpolationDirectionClick(Sender);
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
    Exit;

  if (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    InterpValues.InterpolationDirection := TInterpolationDirection(
      framePhastInterpolation.rgInterpolationDirection.ItemIndex);
  end;
end;

procedure TfrmDataSets.btnCancelClick(Sender: TObject);
begin
  inherited;
//  frmGoPhast.PhastModel.UpToDate := FPriorModelUpToDate;
end;

procedure TfrmDataSets.framePhastInterpolationedMixFormulaChange(
  Sender: TObject);
var
  InterpValues: TPhastInterpolationValues;
begin
  inherited;
  if framePhastInterpolation.AssigningValues then
  begin
    Exit;
  end;
  if SelectedEdit = nil then
    Exit;

  if (framePhastInterpolation.edMixFormula.Text <> '')
    and (SelectedEdit.DataArray <> nil)
    and (SelectedEdit.DataArray is TCustomPhastDataSet) then
  begin
    InterpValues := SelectedEdit.FInterpValues;
    if (SelectedEdit.DataArray is TIntegerPhastDataSet) then
    begin
      InterpValues.MixtureFormula :=
        framePhastInterpolation.edMixFormula.Text;
    end
    else if (SelectedEdit.DataArray is TRealPhastDataSet) then
    begin
    end
    else
    begin
      Assert(False);
    end;
  end;    
end;

procedure TfrmDataSets.framePhastInterpolationbtnEditMixtureFormulaClick(
  Sender: TObject);
var
  VariableList: TList;
  ArrayEditorIndex: integer;
  Expression: TExpression;
  Variable: TCustomVariable;
  Orientation: TDataSetOrientation;
  EvaluatedAt: TEvaluatedAt;
  Index: integer;
  TempUsesList: TStringList;
  VariableName: string;
  Used: TStringList;
  VariablePosition: integer;
  OldFormula: string;
  NewFormula: string;
  TempIndex: integer;
  DataSetName: string;
  DataSetIndex: integer;
  UseList: TStringList;
  DataArrayEdit: TDataArrayEdit;
//  ARow: integer;
begin
  inherited;
  if FSelectedEdit = nil then
  begin
    Exit;
  end;
  VariableList := TList.Create;
  // VariableList will hold a list of variables that can
  // be used in the function
  Used := TStringList.Create;
  // "Used" will be a list of variables that depend on
  // the data set whose formula will be edited.
  try
    Orientation := FSelectedEdit.Orientation;
    EvaluatedAt := FSelectedEdit.EvaluatedAt;
    // Add the variable whose value is being set to "Used".

    Used.Assign(FSelectedEdit.NewUses);

    Used.Sorted := True;

    for ArrayEditorIndex := 0 to FArrayEdits.Count - 1 do
    begin
      DataArrayEdit := FArrayEdits[ArrayEditorIndex];
      if DataArrayEdit <> FSelectedEdit then
      begin
        VariableName := DataArrayEdit.Name;
        VariablePosition := Used.IndexOf(VariableName);
        if (VariablePosition < 0)
          and ((Orientation = dso3D) or (Orientation = DataArrayEdit.Orientation))
          and (EvaluatedAt = DataArrayEdit.EvaluatedAt) then
        begin
          // If the variable does not depend on the
          // data set whose formula is being edited
          // and it's orientation and EvaluatedAt are OK, the variable
          // can be used in the formula.
          VariableList.Add(DataArrayEdit.Variable);
        end;
      end;
    end;

    // if the user makes an invalid formula, it
    // may be necessary to restore it but only
    // if the formula that was already present
    // was OK to begin with.
    OldFormula := framePhastInterpolation.edMixFormula.Text;
//    with TfrmFormula.Create(self) do
    with frmFormula do
    begin
      try
        Initialize;
        PopupParent := self;
        IncludeGIS_Functions(EvaluatedAt);
        // register the appropriate variables with the
        // parser.
        for Index := 0 to VariableList.Count - 1 do
        begin
          Variable := VariableList[Index];
          rbFormulaParser.RegisterVariable(Variable);
        end;

        // show the variables and functions
        IncludeTimeSeries := False;
        UpdateTreeList;

        // put the formula in the TfrmFormula.
        Formula := framePhastInterpolation.edMixFormula.Text;
        
        // The user edits the formula.
        ShowModal;
        if ResultSet then
        begin
          NewFormula := Formula;
          if framePhastInterpolation.edMixFormula.Text <> NewFormula then
          begin
            framePhastInterpolation.edMixFormula.Text := NewFormula;

            Expression := CreateMixtureFormula;

            if Expression <> nil then
            begin
              // Check that the formula does not result in
              // a circular reference.
              for Index := 0 to Used.Count - 1 do
              begin
                VariableName := Used[Index];
                // VariablePosition is the ArrayEditorIndex in the table that stores
                // data for the variable.
                VariablePosition := GetDataSetIndex(VariableName);
                if VariablePosition >= 0 then
                begin
                  DataArrayEdit := FArrayEdits[VariablePosition];
                  if Expression.UsesVariable(DataArrayEdit.Variable) then
                  begin
                    Beep;
                    if MessageDlg(Format(StrErrorThereAppears, [Used[Index]]),
                      mtError, [mbYes, mbNo], 0) = mrYes then
                    begin
                      framePhastInterpolation.edMixFormula.Text := OldFormula;
                      CreateMixtureFormula;
                    end;

                    Exit;
                  end
                end;
              end;

              // update the list of which variables depend on which
              // others.;
              TempUsesList := TStringList.Create;
              try
                TempUsesList.Assign(Expression.VariablesUsed);
                // TempUsesList now has a list of the variables/datasets
                // in the expression.
                for TempIndex := 0 to TempUsesList.Count - 1 do
                begin
                  // get the name of a variable
                  DataSetName := TempUsesList[TempIndex];
                  // get the ArrayEditorIndex that has that variable.
                  DataSetIndex := GetDataSetIndex(DataSetName);
                  if DataSetIndex >= 0 then
                  begin
                    DataArrayEdit := FArrayEdits[DataSetIndex];
                    // Get the list of variables that depends on it.
                    UseList := DataArrayEdit.NewUses;
                    // Add the additional variables that depend on it because
                    // they depend on the variable/dataset being edited.
                    UseList.Add(FSelectedEdit.Name);
                    UseList.AddStrings(FSelectedEdit.NewUses);
                  end;
                end;
              finally
                TempUsesList.Free;
              end;
              UpdateLinkages;
            end;
          end;
        end;
      finally
        Initialize;
//        Free;
      end;
    end;
  finally
    Used.Free;
    VariableList.Free;
    // Don't allow the user to click the OK button if any formulas are invalid.
    EnableOK_Button;
  end;
end;

{ TDataArrayEdit }

function TDataArrayEdit.ClassificationName: string;
begin
  Assert(DataArray <> nil);
  result := DataArray.DisplayName;
  if DataArray.ParameterUsed then
  begin
    result := Format(StrDefinedByParamet, [result]);
  end;
end;

constructor TDataArrayEdit.Create(ADataArray: TDataArray);
var
  DataArrayManager: TDataArrayManager;
  Index: Integer;
  DataSet: TDataArray;
begin
  inherited Create;
  FNotifier := TEditorNotifierComponent.Create(self);
  FNewUses := TStringList.Create;
  FDataArray := ADataArray;
  if FDataArray <> nil then
  begin
    Name := FDataArray.Name;
    DisplayName := FDataArray.DisplayName;
    DataType := FDataArray.DataType;
    Orientation := FDataArray.Orientation;
    EvaluatedAt := FDataArray.EvaluatedAt;
    Units := FDataArray.Units;
    AngleType := FDataArray.AngleType;
    Formula := FDataArray.DisplayFormula;
    TwoDInterpolator := FDataArray.TwoDInterpolator;
    Comment := FDataArray.Comment;
    Classification := FDataArray.Classification;
    PestParametersUsed := FDataArray.PestParametersUsed;
    if ADataArray is TCustomPhastDataSet then
    begin
      FInterpValues := TPhastInterpolationValues.Create;
      FInterpValues.Assign(ADataArray);
    end;
  end;
  FNewUses.Sorted := True;
  FNewUses.Duplicates := dupIgnore;
  if FDataArray <> nil then
  begin
    FDataArray.FullUseList(FNewUses);

    DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      DataSet.Observed := False;
    end;

    FDataArray.ObserverList.NotifyOnChange(FDataArray, ckResetObserved);
    FDataArray.ObserverList.NotifyOnChange(FDataArray, ckCheckDependance);
    for Index := 0 to DataArrayManager.DataSetCount - 1 do
    begin
      DataSet := DataArrayManager.DataSets[Index];
      if DataSet <> FDataArray then
      begin
        if DataSet.Observed then
        begin
          FNewUses.Add(DataSet.Name);
        end;
      end;
    end;
  end;
end;

destructor TDataArrayEdit.Destroy;
begin
  FTwoDInterpolator.Free;
  FInterpValues.Free;
  FNewUses.Free;
  FNotifier.Free;
  inherited;
end;

function TDataArrayEdit.FullClassification: string;
begin
  Assert(DataArray <> nil);
  result := DataArray.FullClassification;
end;

function TDataArrayEdit.GetAssociatedDataSets: string;
begin
  result := '';
  if DataArray <> nil then
  begin
    result := DataArray.AssociatedDataSets;
  end;
end;

procedure TDataArrayEdit.SetExpression(const Value: TExpression);
begin
  if FExpression <> nil then
  begin
    FExpression.Notifier.RemoveFreeNotification(FNotifier);
  end;
  FExpression := Value;
  if FExpression <> nil then
  begin
    FExpression.Notifier.FreeNotification(FNotifier);
  end;
end;

procedure TDataArrayEdit.SetInterpValues(
  const Value: TPhastInterpolationValues);
begin
  if FInterpValues = nil then
  begin
    FInterpValues := TPhastInterpolationValues.Create;
  end;
  FInterpValues.Assign(Value);
end;

procedure TDataArrayEdit.SetNewUses(const Value: TStringList);
begin
  FNewUses.Assign(Value);
end;

procedure TDataArrayEdit.SetTwoDInterpolator(
  const Value: TCustom2DInterpolater);
begin
  if Value = nil then
  begin
    FreeAndNil(FTwoDInterpolator);
  end
  else
  begin
    FTwoDInterpolator.Free;
    FTwoDInterpolator := TInterpolatorType(Value.ClassType).Create(nil);
    FTwoDInterpolator.Assign(Value);
  end;
end;

procedure TDataArrayEdit.UpdateDataSet;
begin
  if FDataArray = nil then
  begin
    FDataArray := TDataArray.Create(frmGoPhast.PhastModel);
    FDataArray.UpdateWithName(Name);
//    FDataArray.OnNameChange := frmGoPhast.PhastModel.DataArrayNameChange
  end;
end;

{ TEditorNotifierComponent }

constructor TEditorNotifierComponent.Create(Owner: TDataArrayEdit);
begin
  inherited Create(nil);
  FEditor := Owner;
end;

procedure TEditorNotifierComponent.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  ExprNotifier: TNotifierComponent;
begin
  if Operation = opRemove then
  begin
    ExprNotifier := AComponent as TNotifierComponent;
    Assert(ExprNotifier.Expression = FEditor.Expression);
    FEditor.FExpression := nil;
  end;
  inherited;
end;

procedure TfrmDataSets.comboUnitsChange(Sender: TObject);
begin
  inherited;
  if FLoading or (SelectedEdit = nil) then
  begin
    Exit;
  end;
  SelectedEdit.Units := comboUnits.Text;
  SelectedEdit.AngleType := TAngleType(comboUnits.ItemIndex+1)

end;

initialization

finalization
  FreeAndNil(frmDataSets);

end.
