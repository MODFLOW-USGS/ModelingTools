{ @Abstract(The main purpose of @name is to define @link(TfrmSolutionMethod)
  which is used to edit
  frmGoPhast.Model.@link(TPhastModel.SolutionOptions).)

  @name is also defines @link(TUndoSolutionOptions) which is used by
  @link(TfrmSolutionMethod).}
unit frmSolutionMethodUnit;

interface

uses
  SysUtils, Types, Classes, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ExtCtrls, UndoItems,
  PhastModelUnit, ArgusDataEntry;

type
  { @Abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.SolutionOptions).)}
  TfrmSolutionMethod = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // Clicking @name closes @classname without changing anything.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // Clicking @name displays help on @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name determines whether cross dispersion in used in PHAST.
    cbCrossDispersion: TCheckBox;
    // @name: TLabel;
    // @name displays "Maximum iterations".
    lblMaximumIterations: TLabel;
    // @name: TLabel;
    // @name displays "Save directions".
    lblSaveDirections: TLabel;
    // @name: TLabel;
    // @name displays "Space differencing".
    lblSpaceDifferencing: TLabel;
    // @name: TLabel;
    // @name displays "Time differencing".
    lblTimeDifferencing: TLabel;
    // @name: TLabel;
    // @name displays "Tolerance".
    lblTolerance: TLabel;
    // @name: TRbwDataEntry;
    // @name is used to edit the maximum number of iterations used in PHAST.
    rdeMaximumIterations: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to edit the number of save directions used in PHAST.
    rdeSaveDirections: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to edit the space differencing used in PHAST.
    rdeSpaceDifferencing: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to edit the time differencing used in PHAST.
    rdeTimeDifferencing: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to edit the tolerance used in PHAST.
    rdeTolerance: TRbwDataEntry;
    // @name: TRadioGroup;
    // @name is used to choose which solver to use in PHAST.
    rgSolver: TRadioGroup;
    cbRebalanceByCell: TCheckBox;
    lblRebalanceFraction: TLabel;
    rdeRebalanceFraction: TRbwDataEntry;
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
    // @name calls @link(SetData).
    procedure btnOKClick(Sender: TObject);
    // @name enables or disables controls depending on which
    // solver is selected.
    procedure rgSolverClick(Sender: TObject);
  private
    // @name displays the values of
    // frmGoPhast.Model.@link(TPhastModel.SolutionOptions) in @classname.
    procedure GetData;
    // @name sets frmGoPhast.Model.@link(TPhastModel.SolutionOptions)
    // with the values in @classname using a @link(TUndoSolutionOptions).
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to change or reverse the change to
    frmGoPhast.Model.@link(TPhastModel.SolutionOptions).)}
  TUndoSolutionOptions = class(TCustomUndo)
  protected
    // @name: TSolutionOptions;
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.SolutionOptions)
    // as it was when this instance of @classname was created.
    FOldSolutionOptions: TSolutionOptions;
    // @name describes what @classname does.
    function Description: string; override;
  public
    // @name: TSolutionOptions;
    // @name is the new value to be assigned to
    // frmGoPhast.Model.@link(TPhastModel.SolutionOptions).
    FNewSolutionOptions: TSolutionOptions;
    // @name creates an instance of @classname, creates
    // @link(FOldSolutionOptions) and @link(FNewSolutionOptions) and
    // assigns frmGoPhast.Model.@link(TPhastModel.SolutionOptions) to
    // @link(FOldSolutionOptions).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewSolutionOptions) to
    // frmGoPhast.Model.@link(TPhastModel.SolutionOptions).
    procedure DoCommand; override;
    // @name assigns @link(FOldSolutionOptions) to
    // frmGoPhast.Model.@link(TPhastModel.SolutionOptions).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit, GoPhastTypes;

resourcestring
  StrSolutionMethod = 'solution method';

{$R *.dfm}

{ TfrmSolutionMethod }

procedure TfrmSolutionMethod.GetData;
begin
  with frmGoPhast.PhastModel.SolutionOptions do
  begin
    rgSolver.ItemIndex := Ord(SolverType);
    rdeSpaceDifferencing.Text := FloatToStr(SpaceDifferencing);
    rdeTimeDifferencing.Text := FloatToStr(TimeDifferencing);
    cbCrossDispersion.Checked := CrossDispersion;
    rdeTolerance.Text := FloatToStr(Tolerance);
    rdeSaveDirections.Text := IntToStr(SaveDirections);
    rdeMaximumIterations.Text := IntToStr(MaximumIterations);
    cbRebalanceByCell.Checked := RebalanceByCell;
    rdeRebalanceFraction.Text := FloatToStr(RebalanceFraction.Value);
  end;
  rgSolverClick(nil);
end;

procedure TfrmSolutionMethod.SetData;
var
  PriorUpToDate: boolean;
  Undo: TUndoSolutionOptions;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoSolutionOptions.Create;
  try
    with Undo.FNewSolutionOptions do
    begin
      SolverType := TPhastSolver(rgSolver.ItemIndex);
      SpaceDifferencing := StrToFloat(rdeSpaceDifferencing.Text);
      TimeDifferencing := StrToFloat(rdeTimeDifferencing.Text);
      CrossDispersion := cbCrossDispersion.Checked;
      if SolverType = psIterative then
      begin
        Tolerance := StrToFloat(rdeTolerance.Text);
        SaveDirections := StrToInt(rdeSaveDirections.Text);
        MaximumIterations := StrToInt(rdeMaximumIterations.Text);
      end;
      RebalanceByCell := cbRebalanceByCell.Checked;
      RebalanceFraction.Value := StrToFloat(rdeRebalanceFraction.Text);
    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmSolutionMethod.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSolutionMethod.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmSolutionMethod.rgSolverClick(Sender: TObject);
begin
  inherited;
  rdeTolerance.Enabled := rgSolver.ItemIndex = 1;
  lblTolerance.Enabled := rdeTolerance.Enabled;
  rdeSaveDirections.Enabled := rdeTolerance.Enabled;
  lblSaveDirections.Enabled := rdeTolerance.Enabled;
  rdeMaximumIterations.Enabled := rdeTolerance.Enabled;
  lblMaximumIterations.Enabled := rdeTolerance.Enabled;
end;

{ TUndoSolutionOptions }

constructor TUndoSolutionOptions.Create;
begin
  inherited;
  FOldSolutionOptions := TSolutionOptions.Create(nil);
  FOldSolutionOptions.Assign(frmGoPhast.PhastModel.SolutionOptions);
  FNewSolutionOptions := TSolutionOptions.Create(nil);
end;

function TUndoSolutionOptions.Description: string;
begin
  result := StrSolutionMethod;
end;

destructor TUndoSolutionOptions.Destroy;
begin
  FOldSolutionOptions.Free;
  FNewSolutionOptions.Free;
  inherited;
end;

procedure TUndoSolutionOptions.DoCommand;
begin
  frmGoPhast.PhastModel.SolutionOptions.Assign(FNewSolutionOptions);
end;

procedure TUndoSolutionOptions.Undo;
begin
  frmGoPhast.PhastModel.SolutionOptions.Assign(FOldSolutionOptions);
end;

end.

