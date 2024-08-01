{@abstract(The main purpose of @name is to define @link(TfrmChemistryOptions)
  which is used to turn on or off the use of "FlowOnly" and
  other chemistry-related options in PHAST.)
  @Link(TUndoChemistryOptions) is also defined in this unit.}
unit frmChemistryOptionsUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, Buttons, PhastModelUnit, UndoItems,
  ArgusDataEntry;

{
  @abstract(@name is used to turn on or off the use of "FlowOnly" and
  other chemistry-related options in PHAST.)  @Link(TfrmChemistryOptions.GetData)
  gets the data
  from the model and @Link(TfrmChemistryOptions.SetData) creates
  and @Link(TUndoChemistryOptions)
  to set it.
}
type
  TfrmChemistryOptions = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes the form without changing anything in the model.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // @name shows help for the form.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name activates of deactivates chemistry in the model.
    cbChemistry: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates equilibrium phases.
    cbEquilibriumPhases: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates exchange in the model.
    cbExchange: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates gas phases in the model.
    cbGasPhases: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates kinetics in the model.
    cbKinetics: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates solid solution in the model.
    cbSolidSolution: TCheckBox;
    // @name: TCheckBox;
    // @name activates of deactivates surface reactions in the model.
    cbSurface: TCheckBox;
    // @name: TLabel;
    // @name labels the diffusivity edit box..
    lblDiffusivity: TLabel;
    // @name: TRbwDataEntry;
    // @name is used to specify the diffusivity.
    rdeDiffusivity: TRbwDataEntry;
    // @name updates the model and closes the form.
    procedure btnOKClick(Sender: TObject);
    // @name activates the other checkboxes and the edit box when
    // @link(cbChemistry) is checked.
    procedure cbChemistryClick(Sender: TObject);
    // @name creates an instance of @classname.
    procedure FormCreate(Sender: TObject); override;
    function FormHelp(Command: Word; Data: NativeInt; var CallHelp: Boolean):
        Boolean;
  private
    // @name displays the options related to chemistry to the user.
    procedure GetData;
    // @name copies the data in the controls to the @link(TPhastModel)
    // via a @link(TUndoChemistryOptions).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  {
    @abstract(@name sets or restores chemistry related options
    in PHAST.)
  }
  TUndoChemistryOptions = class(TCustomUndo)
  protected
    // @name: @link(TChemistryOptions);
    // See @link(DoCommand).
    FNewChemistryOptions: TChemistryOptions;
    // @name: double;
    // See @link(DoCommand).
    FNewDiffusivity: double;
    // @name: boolean;
    // See @link(DoCommand).
    FNewSoluteTransport: boolean;
    // @name: @link(TChemistryOptions);
    // See @link(Undo).
    FOldChemistryOptions: TChemistryOptions;
    // @name: double;
    // See @link(Undo).
    FOldDiffusivity: double;
    // @name: boolean;
    // See @link(Undo).
    FOldSoluteTransport: boolean;
    // @name describes what this @classname does.
    function Description: string; override;
  public
    // @name creates an instance of @classname.
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call name directly.  Call Free instead.
    destructor Destroy; override;
    // @name sets the properties of the @link(TPhastModel)
    // to @link(FNewChemistryOptions),
    // @link(FNewDiffusivity) and @link(FNewSoluteTransport).
    procedure DoCommand; override;
    // @name restores  the properties of the @link(TPhastModel)
    // to @link(FOldChemistryOptions),
    // @link(FOldDiffusivity) and @link(FOldSoluteTransport).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit;

resourcestring
  StrChemistryOptions = 'chemistry options';

{$R *.dfm}

procedure TfrmChemistryOptions.cbChemistryClick(Sender: TObject);
begin
  inherited;
  cbEquilibriumPhases.Enabled := cbChemistry.Checked;
  cbSurface.Enabled := cbChemistry.Checked;
  cbExchange.Enabled := cbChemistry.Checked;
  cbGasPhases.Enabled := cbChemistry.Checked;
  cbSolidSolution.Enabled := cbChemistry.Checked;
  cbKinetics.Enabled := cbChemistry.Checked;
  rdeDiffusivity.Enabled := cbChemistry.Checked;
  lblDiffusivity.Enabled := cbChemistry.Checked;
end;

procedure TfrmChemistryOptions.GetData;
begin
  with frmGoPhast.PhastModel do
  begin
    cbChemistry.Checked := SoluteTransport;
    rdeDiffusivity.Text := FloatToStr(Diffusivity);
    cbChemistryClick(nil);
    with ChemistryOptions do
    begin
      cbEquilibriumPhases.Checked := UseEquilibriumPhases;
      cbSurface.Checked := UseSurfaceAssemblages;
      cbExchange.Checked := UseExchange;
      cbGasPhases.Checked := UseGasPhases;
      cbSolidSolution.Checked := UseSolidSolution;
      cbKinetics.Checked := UseKineticReactants;
    end;
  end;
end;

procedure TfrmChemistryOptions.SetData;
var
  Undo: TUndoChemistryOptions;
  PriorUpToDate: boolean;
begin
  Undo := nil;
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  try
    Undo := TUndoChemistryOptions.Create;
    with Undo do
    begin
      FNewSoluteTransport := cbChemistry.Checked;
      if not FNewSoluteTransport then
      begin
        FNewDiffusivity := FOldDiffusivity;
        FNewChemistryOptions.Assign(FOldChemistryOptions);
      end
      else
      begin
        FNewDiffusivity := StrToFloat(rdeDiffusivity.Text);
        with FNewChemistryOptions do
        begin
          UseEquilibriumPhases := cbEquilibriumPhases.Checked;
          UseSurfaceAssemblages := cbSurface.Checked;
          UseExchange := cbExchange.Checked;
          UseGasPhases := cbGasPhases.Checked;
          UseSolidSolution := cbSolidSolution.Checked;
          UseKineticReactants := cbKinetics.Checked;
        end;
      end;
    end;
  except
    Undo.Free;
    raise;
  end;

  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmChemistryOptions.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmChemistryOptions.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

function TfrmChemistryOptions.FormHelp(Command: Word; Data: NativeInt; var
    CallHelp: Boolean): Boolean;
begin
  inherited;
end;

{ TUndoChemistryOptions }

constructor TUndoChemistryOptions.Create;
begin
  FOldSoluteTransport := frmGoPhast.PhastModel.SoluteTransport;
  FOldDiffusivity := frmGoPhast.PhastModel.Diffusivity;
  FOldChemistryOptions := TChemistryOptions.Create;
  FOldChemistryOptions.Assign(frmGoPhast.PhastModel.ChemistryOptions);
  FNewChemistryOptions := TChemistryOptions.Create;
end;

function TUndoChemistryOptions.Description: string;
begin
  result := StrChemistryOptions;
end;

destructor TUndoChemistryOptions.Destroy;
begin
  FOldChemistryOptions.Free;
  FNewChemistryOptions.Free;
  inherited;
end;

procedure TUndoChemistryOptions.DoCommand;
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.SoluteTransport := FNewSoluteTransport;
  PhastModel.Diffusivity := FNewDiffusivity;
  PhastModel.ChemistryOptions.Assign(FNewChemistryOptions);
  PhastModel.DataArrayManager.CreateInitialDataSets;
end;

procedure TUndoChemistryOptions.Undo;
var
  PhastModel: TPhastModel;
begin
  PhastModel := frmGoPhast.PhastModel;
  PhastModel.SoluteTransport := FOldSoluteTransport;
  PhastModel.Diffusivity := FOldDiffusivity;
  PhastModel.ChemistryOptions.Assign(FOldChemistryOptions);
  PhastModel.DataArrayManager.CreateInitialDataSets;
end;

end.
