{@abstract(The main purpose of @name is to define @link(TfrmSteadyFlow)
  which is used to edit
  frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).)

@name also defines @link(TUndoSteadyFlow) which is
used to set or undo the setting of
frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).}
unit frmSteadyFlowUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, UndoItems, PhastModelUnit,
  ArgusDataEntry;

type
  {@abstract(@name is used to edit
    frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).)}
  TfrmSteadyFlow = class(TfrmCustomGoPhast)
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
    // @name determines whether the default head change limit
    // in PHAST will be used.
    cbDefaultHeadChangeLimit: TCheckBox;
    // @name: TCheckBox;
    // @name determines whether the default maximum time step size
    // in PHAST will be used.
    cbDefaultMaxTimeStep: TCheckBox;
    // @name: TCheckBox;
    // @name determines whether the default minimum time step size
    // in PHAST will be used.
    cbDefaultMinTimeStep: TCheckBox;
    // @name: TCheckBox;
    // @name determines whether steady flow
    // will be used in PHAST.
    cbSteadyFlow: TCheckBox;
    // @name: TLabel;
    // @name displays "Flow balance tolerance".
    lblFlowBalanceTolerance: TLabel;
    // @name: TLabel;
    // @name displays "Head change target".
    lblHeadChangeLimit: TLabel;
    // @name: TLabel;
    // @name displays "Head tolerance".
    lblHeadTolerance: TLabel;
    // @name: TLabel;
    // @name displays "Iterations".
    lblIterations: TLabel;
    // @name: TLabel;
    // @name displays "Maximum time step".
    lblMaxTimeStep: TLabel;
    // @name: TLabel;
    // @name displays "Minimum time step".
    lblMinTimeStep: TLabel;
    // @name: TRbwDataEntry;
    // @name is used to specify the flow balance tolerance in PHAST.
    rdeFlowBalanceTolerance: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the head change limit in PHAST.
    rdeHeadChangeLimit: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the head tolerance in PHAST.
    rdeHeadTolerance: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the iterations in PHAST.
    rdeIterations: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the maximum time step size in PHAST.
    rdeMaxTimeStep: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is used to specify the minimum time step size in PHAST.
    rdeMinTimeStep: TRbwDataEntry;
    // @name calls @link(ValidateData) and if it returns @true, calls
    // @link(SetData) and closes @classname.
    procedure btnOKClick(Sender: TObject);
    // @name enables or disables controls relating to the
    // head change limit
    // depending on whether or not
    // @link(cbDefaultHeadChangeLimit)
    // is checked.
    procedure cbDefaultHeadChangeLimitClick(Sender: TObject);
    // @name enables or disables controls relating to the
    // maximum time step size
    // depending on whether or not
    // @link(cbDefaultMaxTimeStep)
    // is checked.
    procedure cbDefaultMaxTimeStepClick(Sender: TObject);
    // @name enables or disables controls relating to the
    // minimum time step size
    // depending on whether or not
    // @link(cbDefaultMinTimeStep)
    // is checked.
    procedure cbDefaultMinTimeStepClick(Sender: TObject);
    // @name enables or disables controls based on whether @link(cbSteadyFlow)
    // is checked.
    procedure cbSteadyFlowClick(Sender: TObject);
    // @name calls @link(GetData).
    procedure FormCreate(Sender: TObject); override;
  private
    // @name displays frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions)
    // in @classname.
    procedure GetData;
    // @name sets frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions)
    // to the value in @classname using a @link(TUndoSteadyFlow).
    procedure SetData;
    // @name returns @true if valid data has been specified.
    function ValidateData: boolean;
    { Private declarations }
  public
    { Public declarations }
  end;

  {@abstract(@name is used to set or undo the setting of
    frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).)}
  TUndoSteadyFlow = class(TCustomUndo)
  protected
    // @name: TSteadyFlowOptions;
    // @name is a copy of frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions)
    // as it was at the time this instance of @classname was created.
    FOldSteadyFlowOptions: TSteadyFlowOptions;
    // @name describes what @classname does.
    function Description: string; override;
  public
    // @name: TSteadyFlowOptions;
    // @name is a new version of
    // frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).
    FNewSteadyFlowOptions: TSteadyFlowOptions;
    // @name creates an instance of @classname.
    // @name creates @link(FOldSteadyFlowOptions) and
    // @link(FNewSteadyFlowOptions).  @name assigns
    // frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions) to
    // @link(FOldSteadyFlowOptions).
    constructor Create;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name assigns @link(FNewSteadyFlowOptions) to
    // frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).
    procedure DoCommand; override;
    // @name assigns @link(FOldSteadyFlowOptions) to
    // frmGoPhast.Model.@link(TPhastModel.SteadyFlowOptions).
    procedure Undo; override;
  end;

implementation

uses frmGoPhastUnit;

resourcestring
  StrSteadyFlow = 'steady flow';
  StrInvalidS = 'Invalid %s';
  StrInvalid0sOr1s = 'Invalid %0:s or %1:s';

{$R *.dfm}

{ TfrmSteadyFlow }

procedure TfrmSteadyFlow.GetData;
begin
  with frmGoPhast.PhastModel.SteadyFlowOptions do
  begin
    cbSteadyFlow.Checked := SteadyFlow;
    rdeHeadTolerance.Text := FloatToStr(HeadTolerance);
    rdeFlowBalanceTolerance.Text := FloatToStr(FlowBalanceTolerance);
    rdeMinTimeStep.Text := FloatToStr(MinimumTimeStep);
    rdeMaxTimeStep.Text := FloatToStr(MaximumTimeStep);
    rdeHeadChangeLimit.Text := FloatToStr(HeadChangeLimit);
    cbDefaultMinTimeStep.Checked := UseDefaultMinimumTimeStep;
    cbDefaultMaxTimeStep.Checked := UseDefaultMaximumTimeStep;
    cbDefaultHeadChangeLimit.Checked := UseDefaultHeadChangeLimit;
    rdeIterations.Text := IntToStr(Iterations);
    if UseDefaultMinimumTimeStep then
    begin
      rdeMinTimeStep.Text :=
        FloatToStr(frmGoPhast.PhastModel.Times.TimeStepLength(0));
    end;
    if UseDefaultMaximumTimeStep then
    begin
      rdeMaxTimeStep.Text := FloatToStr(1000 *
        frmGoPhast.PhastModel.Times.TimeStepLength(0));
    end;
    if UseDefaultHeadChangeLimit and (frmGoPhast.PhastGrid.LayerCount >= 1) then
    begin
      with frmGoPhast.PhastGrid do
      begin
        rdeHeadChangeLimit.Text := FloatToStr(0.3 * (LayerElevation[LayerCount]
          - LayerElevation[0]));
      end;
    end;
  end;
end;

procedure TfrmSteadyFlow.SetData;
var
  PriorUpToDate: boolean;
  Undo: TUndoSteadyFlow;
begin
  PriorUpToDate := frmGoPhast.PhastModel.UpToDate;
  Undo := TUndoSteadyFlow.Create;
  try
    with Undo.FNewSteadyFlowOptions do
    begin
      SteadyFlow := cbSteadyFlow.Checked;
      HeadTolerance := StrToFloat(rdeHeadTolerance.Text);
      FlowBalanceTolerance := StrToFloat(rdeFlowBalanceTolerance.Text);
      MinimumTimeStep := StrToFloat(rdeMinTimeStep.Text);
      MaximumTimeStep := StrToFloat(rdeMaxTimeStep.Text);
      HeadChangeLimit := StrToFloat(rdeHeadChangeLimit.Text);
      UseDefaultMinimumTimeStep := cbDefaultMinTimeStep.Checked;
      UseDefaultMaximumTimeStep := cbDefaultMaxTimeStep.Checked;
      UseDefaultHeadChangeLimit := cbDefaultHeadChangeLimit.Checked;
      Iterations := StrToInt(rdeIterations.Text);
    end;
  except
    Undo.Free;
    raise;
  end;
  frmGoPhast.PhastModel.UpToDate := PriorUpToDate;
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmSteadyFlow.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSteadyFlow.btnOKClick(Sender: TObject);
begin
  inherited;
  if ValidateData then
  begin
    SetData;
    Close;
  end;
end;

function TfrmSteadyFlow.ValidateData: boolean;
var
  ErrorMessages: TStringList;
begin
  ErrorMessages := TStringList.Create;
  try
    try
      if StrToFloat(rdeHeadTolerance.Text) = 0 then
      begin
        ErrorMessages.Add(Format(StrInvalidS, [lblHeadTolerance.Caption]));
      end;
      if StrToFloat(rdeFlowBalanceTolerance.Text) = 0 then
      begin
        ErrorMessages.Add(Format(StrInvalidS, [lblFlowBalanceTolerance.Caption]));
      end;
      if not cbDefaultMinTimeStep.Checked
        and not cbDefaultMaxTimeStep.Checked
        and (StrToFloat(rdeMinTimeStep.Text) > StrToFloat(rdeMaxTimeStep.Text))
          then
      begin
        ErrorMessages.Add(Format(StrInvalid0sOr1s,
          [lblMinTimeStep.Caption, lblMaxTimeStep.Caption]));
      end;
      if not cbDefaultHeadChangeLimit.Checked
        and (StrToFloat(rdeHeadChangeLimit.Text) = 0) then
      begin
        ErrorMessages.Add(Format(StrInvalidS, [lblHeadChangeLimit.Caption]));
      end;
      result := ErrorMessages.Count = 0;
      if not result then
      begin
        Beep;
        MessageDlg(ErrorMessages.Text, mtError, [mbOK], 0);
      end;
    except on E: EConvertError do
      begin
        result := false;
        Beep;
        MessageDlg(E.Message, mtError, [mbOK], 0);
      end
    end;
  finally
    ErrorMessages.Free;
  end;
end;

procedure TfrmSteadyFlow.cbSteadyFlowClick(Sender: TObject);
begin
  inherited;
  rdeHeadTolerance.Enabled := cbSteadyFlow.Checked;
  lblHeadTolerance.Enabled := cbSteadyFlow.Checked;

  rdeFlowBalanceTolerance.Enabled := cbSteadyFlow.Checked;
  lblFlowBalanceTolerance.Enabled := cbSteadyFlow.Checked;

  cbDefaultMinTimeStep.Enabled := cbSteadyFlow.Checked;
  cbDefaultMaxTimeStep.Enabled := cbSteadyFlow.Checked;

  cbDefaultHeadChangeLimit.Enabled := cbSteadyFlow.Checked;

  rdeIterations.Enabled := cbSteadyFlow.Checked;
  lblIterations.Enabled := cbSteadyFlow.Checked;
  
  cbDefaultMinTimeStepClick(nil);
  cbDefaultMaxTimeStepClick(nil);
  cbDefaultHeadChangeLimitClick(nil);
end;

procedure TfrmSteadyFlow.cbDefaultMinTimeStepClick(Sender: TObject);
begin
  inherited;
  rdeMinTimeStep.Enabled := cbSteadyFlow.Checked and not
    cbDefaultMinTimeStep.Checked;
  lblMinTimeStep.Enabled := cbSteadyFlow.Checked and not
    cbDefaultMinTimeStep.Checked;
end;

procedure TfrmSteadyFlow.cbDefaultMaxTimeStepClick(Sender: TObject);
begin
  inherited;
  rdeMaxTimeStep.Enabled := cbSteadyFlow.Checked and not
    cbDefaultMaxTimeStep.Checked;
  lblMaxTimeStep.Enabled := cbSteadyFlow.Checked and not
    cbDefaultMaxTimeStep.Checked;
end;

procedure TfrmSteadyFlow.cbDefaultHeadChangeLimitClick(Sender: TObject);
begin
  inherited;
  rdeHeadChangeLimit.Enabled := cbSteadyFlow.Checked and not
    cbDefaultHeadChangeLimit.Checked;
  lblHeadChangeLimit.Enabled := cbSteadyFlow.Checked and not
    cbDefaultHeadChangeLimit.Checked;
end;

{ TUndoSteadyFlow }

constructor TUndoSteadyFlow.Create;
begin
  FOldSteadyFlowOptions := TSteadyFlowOptions.Create;
  FOldSteadyFlowOptions.Assign(frmGoPhast.PhastModel.SteadyFlowOptions);
  FNewSteadyFlowOptions := TSteadyFlowOptions.Create;
end;

function TUndoSteadyFlow.Description: string;
begin
  result := StrSteadyFlow;
end;

destructor TUndoSteadyFlow.Destroy;
begin
  FOldSteadyFlowOptions.Free;
  FNewSteadyFlowOptions.Free;
  inherited;
end;

procedure TUndoSteadyFlow.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.SteadyFlowOptions.Assign(FNewSteadyFlowOptions);
end;

procedure TUndoSteadyFlow.Undo;
begin
  inherited;
  frmGoPhast.PhastModel.SteadyFlowOptions.Assign(FOldSteadyFlowOptions);
end;

end.

