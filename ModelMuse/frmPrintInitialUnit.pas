{@abstract(The main purpose of @name is to define @link(TfrmPrintInitial)
  which is used to edit
  frmGoPhast.Model.@link(TPhastModel.PrintInitial).)
  @name also defines @link(TUndoPrintInitial) which is used to undo or redo changes to
  frmGoPhast.Model.@link(TPhastModel.PrintInitial).}
unit frmPrintInitialUnit;

interface

uses
  SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, PhastModelUnit, UndoItems;

type
  {@abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.PrintInitial).)}
  TfrmPrintInitial = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without doing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name specifies whether boundary conditions will be printed.
    cbBoundaryConditions: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether components will be printed.
    cbComponents: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether conductance will be printed.
    cbConductance: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether input will be echoed in the output.
    cbEchoInput: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether fluid properties will be printed.
    cbFluidProperties: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether chemistry will be printed.
    cbForceChemistryPrint: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether chemistry will be printed in the HDF file.
    cbHDF_Chemistry: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether heads will be printed in the HDF file.
    cbHDF_Heads: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether steady flow velocities will be printed in the HDF file.
    cbHDF_SteadyFlowVelocity: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether heads will be printed.
    cbHeads: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether media properties will be printed.
    cbMediaProperties: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether the solution method will be printed.
    cbSolutionMethod: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether steady flow velocities will be printed.
    cbSteadyFlowVelocities: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether well data will be printed.
    cbWells: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether chemistry will be printed in the XYZ file.
    cbXYZ_Chemistry: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether components will be printed in the XYZ file.
    cbXYZ_Components: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether heads will be printed in the XYZ file.
    cbXYZ_Heads: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether steady flow velocities will be printed in the XYZ file.
    cbXYZ_SteadyFlowVelocities: TCheckBox;
    // @name: TCheckBox;
    // @name specifies whether wells will be printed in the XYZ file.
    cbXYZ_Wells: TCheckBox;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name retrieves
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    procedure GetData;
    // @name sets
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to undo or redo changes to
    frmGoPhast.Model.@link(TPhastModel.PrintInitial).)}
  TUndoPrintInitial = class(TCustomUndo)
  protected
    // @name: TPrintInitial;
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.PrintInitial)
    // as it was when this instance of @classname was created.
    FOldPrintInitial: TPrintInitial;
    // @name describes what @name does.
    function Description: string; override;
  public
    // @name: TPrintInitial;
    // @name is a new @link(TPrintInitial) to be assigned to
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    FNewPrintInitial: TPrintInitial;
    // @name creates an instance of @classname.
    // @name assigns
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    // to @link(FOldPrintInitial).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewPrintInitial) to
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    procedure DoCommand; override;
    // @name assigns @link(FOldPrintInitial) to
    // frmGoPhast.Model.@link(TPhastModel.PrintInitial).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit;

resourcestring
  StrPrintInitial = 'print initial';

{$R *.dfm}

{ TfrmPrintInitial }

procedure TfrmPrintInitial.GetData;
begin
  with frmGoPhast.PhastModel.PrintInitial do
  begin
    cbBoundaryConditions.Checked := PrintInitialBoundaryConditions;
    cbComponents.Checked := PrintInitialComponents;
    cbConductance.Checked := PrintInitialConductance;
    cbEchoInput.Checked := PrintInitialEchoInput;
    cbFluidProperties.Checked := PrintInitialFluidProperties;
    cbForceChemistryPrint.Checked := PrintInitialForceChemistryPrint;
    cbHDF_Chemistry.Checked := PrintInitialHDF_Chemistry;
    cbHDF_Heads.Checked := PrintInitialHDF_Heads;
    cbHDF_SteadyFlowVelocity.Checked := PrintInitialHDF_SteadyFlowVelocites;
    cbHeads.Checked := PrintInitialHeads;
    cbMediaProperties.Checked := PrintInitialMediaProperties;
    cbSolutionMethod.Checked := PrintInitialSolutionMethod;
    cbSteadyFlowVelocities.Checked := PrintInitialSteadyFlowVelocities;
    cbWells.Checked := PrintInitialWells;
    cbXYZ_Chemistry.Checked := PrintInitialXYZ_Chemistry;
    cbXYZ_Components.Checked := PrintInitialXYZ_Components;
    cbXYZ_Heads.Checked := PrintInitialXYZ_Heads;
    cbXYZ_SteadyFlowVelocities.Checked := PrintInitialXYZ_SteadyFlowVelocities;
    cbXYZ_Wells.Checked := PrintInitialXYZ_Wells;
  end;
end;

procedure TfrmPrintInitial.SetData;
var
  PriorUpToDate: boolean;
  Undo: TUndoPrintInitial;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoPrintInitial.Create;
  try
    with Undo.FNewPrintInitial do
    begin
      PrintInitialBoundaryConditions := cbBoundaryConditions.Checked;
      PrintInitialComponents := cbComponents.Checked;
      PrintInitialConductance := cbConductance.Checked;
      PrintInitialEchoInput := cbEchoInput.Checked;
      PrintInitialFluidProperties := cbFluidProperties.Checked;
      PrintInitialForceChemistryPrint := cbForceChemistryPrint.Checked;
      PrintInitialHDF_Chemistry := cbHDF_Chemistry.Checked;
      PrintInitialHDF_Heads := cbHDF_Heads.Checked;
      PrintInitialHDF_SteadyFlowVelocites := cbHDF_SteadyFlowVelocity.Checked;
      PrintInitialHeads := cbHeads.Checked;
      PrintInitialMediaProperties := cbMediaProperties.Checked;
      PrintInitialSolutionMethod := cbSolutionMethod.Checked;
      PrintInitialSteadyFlowVelocities := cbSteadyFlowVelocities.Checked;
      PrintInitialWells := cbWells.Checked;
      PrintInitialXYZ_Chemistry := cbXYZ_Chemistry.Checked;
      PrintInitialXYZ_Components := cbXYZ_Components.Checked;
      PrintInitialXYZ_Heads := cbXYZ_Heads.Checked;
      PrintInitialXYZ_SteadyFlowVelocities :=
        cbXYZ_SteadyFlowVelocities.Checked;
      PrintInitialXYZ_Wells := cbXYZ_Wells.Checked;
    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmPrintInitial.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmPrintInitial.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;
{ TUndoPrintInitial }

constructor TUndoPrintInitial.Create;
begin
  inherited;
  FOldPrintInitial := TPrintInitial.Create;
  FOldPrintInitial.Assign(frmGoPhast.PhastModel.PrintInitial);
  FNewPrintInitial := TPrintInitial.Create;
end;

function TUndoPrintInitial.Description: string;
begin
  result := StrPrintInitial;
end;

destructor TUndoPrintInitial.Destroy;
begin
  FOldPrintInitial.Free;
  FNewPrintInitial.Free;
  inherited;
end;

procedure TUndoPrintInitial.DoCommand;
begin
  frmGoPhast.PhastModel.PrintInitial.Assign(FNewPrintInitial);
end;

procedure TUndoPrintInitial.Undo;
begin
  frmGoPhast.PhastModel.PrintInitial.Assign(FOldPrintInitial);
end;

end.

