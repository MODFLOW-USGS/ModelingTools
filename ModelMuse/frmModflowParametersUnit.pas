unit frmModflowParametersUnit;

interface

uses
  OrderedCollectionUnit, Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, ExtCtrls, ComCtrls, RbwDataGrid4, StdCtrls,
  JvSpin, Buttons, RbwController, Grids, ModflowParameterUnit, JvExExtCtrls,
  JvNetscapeSplitter, Mask, JvExMask, RbwEdit, ArgusDataEntry, UndoItems,
  JvPageList, JvExControls, JvComponent, ModflowTransientListParameterUnit;
        
type
  TParameterColumns = (pcName, pcValue, pcUseZone, pcUseMultiplier);

  TfrmModflowParameters = class(TfrmCustomGoPhast)
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    pnlRight: TPanel;
    pnlParameterCount: TPanel;
    lblNumParameters: TLabel;
    btnDelete: TBitBtn;
    seNumberOfParameters: TJvSpinEdit;
    rbwParamCountController: TRbwController;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    tvParameterTypes: TTreeView;
    dgSteadyParameters: TRbwDataGrid4;
    pnlTop: TPanel;
    rdeParamValue: TRbwDataEntry;
    lblParamValue: TLabel;
    cbUseZone: TCheckBox;
    cbUseMultiplier: TCheckBox;
    jvplParameters: TJvPageList;
    jvspSteadyParameters: TJvStandardPage;
    jvspTransientParameters: TJvStandardPage;
    dgTransientParameters: TRbwDataGrid4;
    jvspBlank: TJvStandardPage;
    lblBlankInstruction: TLabel;
    procedure seNumberOfParametersChange(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure dgSteadyParametersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject);
    procedure dgSteadyParametersBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure tvParameterTypesChange(Sender: TObject; Node: TTreeNode);
    procedure btnOKClick(Sender: TObject);
    procedure seNumberOfParametersEnter(Sender: TObject);
    procedure dgSteadyParametersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure dgSteadyParametersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure dgSteadyParametersColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure dgSteadyParametersMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure dgSteadyParametersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure rdeParamValueChange(Sender: TObject);
    procedure cbUseZoneClick(Sender: TObject);
    procedure cbUseMultiplierClick(Sender: TObject);
    procedure dgSteadyParametersHorizontalScroll(Sender: TObject);
  private
    SteadyParameters: TModflowSteadyParameters;
    TransientListParameters: TModflowTransientListParameters;
    PriorNumberOfParameters: integer;
    CurrentParameterType: TParameterType;
    ActiveGrid: TRbwDataGrid4;
    procedure FillTree;
    procedure GetData;
    procedure SetData;
    procedure AssignParameterToRow(RowIndex: Integer; Parameter: TModflowParameter);
    procedure ClearFirstRow;
    function NewParameterName: string;
    procedure ArrangeMultiEditControls;
    { Private declarations }
  public
    { Public declarations }
  end;

  TUndoDefineParameters = class(TCustomUndo)
  private
    FOldChdScreenObjectParams: TList;
    FNewSteadyParameters: TModflowSteadyParameters;
    FOldSteadyParameters: TModflowSteadyParameters;
    FNewTransientParameters: TModflowTransientListParameters;
    FOldTransientParameters: TModflowTransientListParameters;
    FNewDataSets: TList;
  protected
    function Description: string; override;
  public
    constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;



var
  frmModflowParameters: TfrmModflowParameters;

implementation

uses Contnrs, frmGoPhastUnit, ScreenObjectUnit, ModflowConstantHeadBoundaryUnit,
  ModflowPackagesUnit;

{$R *.dfm}

{ TfrmModflowParameters }


procedure TfrmModflowParameters.btnDeleteClick(Sender: TObject);
var
  Parameter: TModflowParameter;
begin
  inherited;
  if seNumberOfParameters.AsInteger = 1 then
  begin
    Parameter := ActiveGrid.Objects[0, 1] as TModflowParameter;
    PriorNumberOfParameters := 0;
    seNumberOfParameters.AsInteger := 0;
    ActiveGrid.Objects[0, 1] := nil;
  end
  else
  begin
    Parameter := ActiveGrid.Objects[0, ActiveGrid.SelectedRow] as TModflowParameter;
    ActiveGrid.Objects[0, ActiveGrid.SelectedRow] := nil;
    ActiveGrid.DeleteRow(ActiveGrid.SelectedRow);
    Dec(PriorNumberOfParameters);
    seNumberOfParameters.AsInteger := seNumberOfParameters.AsInteger - 1;
  end;
  case CurrentParameterType of
    ptLPF_HK..ptLPF_VKCB:
      begin
        SteadyParameters.Remove(Parameter);
      end;
    ptCHD..ptDRT:
      begin
        TransientListParameters.Remove(Parameter);
      end;
    else Assert(False);
  end;

end;

procedure TfrmModflowParameters.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmModflowParameters.cbUseMultiplierClick(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsStateInColumn(dgSteadyParameters, Ord(pcUseMultiplier),
    cbUseMultiplier.State);
end;

procedure TfrmModflowParameters.cbUseZoneClick(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsStateInColumn(dgSteadyParameters, Ord(pcUseZone),
    cbUseZone.State);
end;

procedure TfrmModflowParameters.dgSteadyParametersBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
begin
  inherited;
  if (ARow >= ActiveGrid.FixedRows)
    and (TParameterColumns(ACol) in [pcName, pcValue]) then
  begin
    if (seNumberOfParameters.AsInteger > 0)
      and (Trim(ActiveGrid.Cells[ACol, ARow]) = '') then
    begin
      ActiveGrid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TfrmModflowParameters.dgSteadyParametersColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  ArrangeMultiEditControls;
end;

procedure TfrmModflowParameters.dgSteadyParametersHorizontalScroll(Sender: TObject);
begin
  inherited;
  ArrangeMultiEditControls;
end;

procedure TfrmModflowParameters.dgSteadyParametersMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if ([ssShift, ssCtrl] * Shift) = [] then
  begin
    dgSteadyParameters.Options := dgSteadyParameters.Options + [goEditing];
  end
  else
  begin
    dgSteadyParameters.Options := dgSteadyParameters.Options - [goEditing];
  end;
end;

procedure TfrmModflowParameters.dgSteadyParametersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  EnableMultiEditControl(dgSteadyParameters, rdeParamValue, Ord(pcValue));
  EnableMultiEditControl(dgSteadyParameters, cbUseZone, Ord(pcUseZone));
  EnableMultiEditControl(dgSteadyParameters, cbUseMultiplier, Ord(pcUseMultiplier));
  lblParamValue.Enabled := rdeParamValue.Enabled;
end;

procedure TfrmModflowParameters.dgSteadyParametersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := (ARow <> 1) or (seNumberOfParameters.AsInteger > 0);
end;

procedure TfrmModflowParameters.dgSteadyParametersSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  Parameter: TModflowParameter;
  NewValue: double;
begin
  inherited;
  Parameter := ActiveGrid.Objects[0,ARow] as TModflowParameter;
  if Parameter = nil then
  begin
    Exit;
  end;
  
  case TParameterColumns(ACol) of
    pcName:
      begin
        Parameter.ParameterName := Value;
      end;
    pcValue:
      begin
        if not TryStrToFloat(Value, NewValue) then
        begin
          NewValue := 0;
        end;
        Parameter.Value := NewValue;
      end;
  end;
end;

procedure TfrmModflowParameters.dgSteadyParametersStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  Parameter: TModflowSteadyParameter;
begin
  inherited;
  Parameter := dgSteadyParameters.Objects[0,ARow] as TModflowSteadyParameter;
  if Parameter = nil then
  begin
    Exit;
  end;

  case TParameterColumns(ACol) of
    pcUseZone:
      begin
        Parameter.UseZone := Value = cbChecked;
      end;
    pcUseMultiplier:
      begin
        Parameter.UseMultiplier := Value = cbChecked;
      end;
  end;
end;

procedure TfrmModflowParameters.FillTree;
var
  PriorBaseNode: TTreeNode;
  ChildNode: TTreeNode;
  Packages: TModflowPackages;
begin
  PriorBaseNode := nil;
  Packages := frmGoPhast.PhastModel.ModflowPackages;

  if Packages.LpfPackage.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.LpfPackage.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptUndefined);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'HK (horizontal hydraulic conductivity)');
    ChildNode.Data := Pointer(ptLPF_HK);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'HANI (horizontal anisotropy)');
    ChildNode.Data := Pointer(ptLPF_HANI);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'VK (vertical hydraulic conductivity)');
    ChildNode.Data := Pointer(ptLPF_VK);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'VANI (vertical anisotropy)');
    ChildNode.Data := Pointer(ptLPF_VANI);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'SS (specific storage)');
    ChildNode.Data := Pointer(ptLPF_SS);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'SY (specific yield)');
    ChildNode.Data := Pointer(ptLPF_SY);

    ChildNode := tvParameterTypes.Items.AddChild(PriorBaseNode,
      'VKCB (vertical hydraulic conductivity of confining layer)');
    ChildNode.Data := Pointer(ptLPF_VKCB);
  end;

  if Packages.ChdBoundary.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.ChdBoundary.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptCHD);
  end;

  if Packages.GhbBoundary.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.GhbBoundary.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptGHB);
  end;

  if Packages.WelPackage.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.WelPackage.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptQ);
  end;

  if Packages.RivPackage.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.RivPackage.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptRIV);
  end;

  if Packages.DrnPackage.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.DrnPackage.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptDrn);
  end;

  if Packages.DrtPackage.IsSelected then
  begin
    PriorBaseNode := tvParameterTypes.Items.Add(PriorBaseNode,
      Packages.DrtPackage.PackageIdentifier);
    PriorBaseNode.Data := Pointer(ptDrt);
  end;
end;

procedure TfrmModflowParameters.ArrangeMultiEditControls;
begin
  LayoutControls(dgSteadyParameters, rdeParamValue, lblParamValue, Ord(pcValue));
  LayoutControls(dgSteadyParameters, cbUseZone, nil, Ord(pcUseZone), 5);
  LayoutControls(dgSteadyParameters, cbUseMultiplier, nil, Ord(pcUseMultiplier), 5);
end;

procedure TfrmModflowParameters.FormCreate(Sender: TObject);
begin
  inherited;
  lblBlankInstruction.Width := jvplParameters.Width
    - 2*(lblBlankInstruction.Left);

  dgSteadyParameters.Cells[Ord(pcName),0] := 'Name';
  dgSteadyParameters.Cells[Ord(pcValue),0] := 'Value';
  dgSteadyParameters.Cells[Ord(pcUseZone),0] := 'Use Zone';
  dgSteadyParameters.Cells[Ord(pcUseMultiplier),0] := 'Use Multiplier';

  dgSteadyParameters.ColWidths[Ord(pcUseZone)] :=
    dgSteadyParameters.ColWidths[Ord(pcUseZone)] + 20;
  dgSteadyParameters.ColWidths[Ord(pcUseMultiplier)] :=
    dgSteadyParameters.ColWidths[Ord(pcUseMultiplier)] + 20;

  dgTransientParameters.Cells[0,0] := 'Name';
  dgTransientParameters.Cells[1,0] := 'Value';

  lblParamValue.Caption := dgSteadyParameters.Cells[Ord(pcValue),0];
  cbUseZone.Caption := dgSteadyParameters.Cells[Ord(pcUseZone),0];
  cbUseMultiplier.Caption := dgSteadyParameters.Cells[Ord(pcUseMultiplier),0];

  SteadyParameters := TModflowSteadyParameters.Create(nil);
  TransientListParameters := TModflowTransientListParameters.Create(nil);
  FillTree;
  GetData;
end;

procedure TfrmModflowParameters.FormDestroy(Sender: TObject);
begin
  SteadyParameters.Free;
  TransientListParameters.Free;
  inherited;
end;

procedure TfrmModflowParameters.GetData;
begin
  SteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  TransientListParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);
end;

procedure TfrmModflowParameters.ClearFirstRow;
begin
//  ActiveGrid.Objects[0,1] := nil;
  ActiveGrid.Cells[0,1] := '';
  ActiveGrid.Cells[1,1] := '';
  if ActiveGrid = dgSteadyParameters then
  begin
    ActiveGrid.Checked[2,1] := False;
    ActiveGrid.Checked[3,1] := False;
  end;
end;

function TfrmModflowParameters.NewParameterName: string;
var
  Root: string;
  Index: Integer;
  Param: TModflowParameter;
  UpRoot, ParamUpRoot: string;
  CountString: string;
  Count, MaxCount: integer;
begin
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK: Root := 'HK_Par';
    ptLPF_HANI: Root := 'HANI_Par';
    ptLPF_VK: Root := 'VHK_Par';
    ptLPF_VANI: Root := 'VANI_Par';
    ptLPF_SS: Root := 'SS_Par';
    ptLPF_SY: Root := 'SY_Par';
    ptLPF_VKCB: Root := 'VKCB_Par';
    ptCHD: Root := 'CHD_Par';
    ptGHB: Root := 'GHB_Par';
    ptQ: Root := 'Q_Par';
    ptRIV: Root := 'RIV_Par';
    ptDRN: Root := 'DRN_Par';
    ptDRT: Root := 'DRT_Par';
    else Assert(False);
  end;
  UpRoot := UpperCase(Root);
  MaxCount := 0;
  case CurrentParameterType of
    ptUndefined: Assert(False);
    ptLPF_HK..ptLPF_VKCB:
      begin
        for Index := 0 to SteadyParameters.Count - 1 do
        begin
          Param := SteadyParameters[Index];
          ParamUpRoot := UpperCase(Param.ParameterName);
          if Pos(UpRoot, ParamUpRoot) > 0 then
          begin
            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
            if TryStrToInt(CountString, Count) then
            begin
              if Count > MaxCount then
              begin
                MaxCount := Count;
              end;
            end;
          end;
        end;
      end;
    ptCHD..ptDRT:
      begin
        for Index := 0 to TransientListParameters.Count - 1 do
        begin
          Param := TransientListParameters[Index];
          ParamUpRoot := UpperCase(Param.ParameterName);
          if Pos(UpRoot, ParamUpRoot) > 0 then
          begin
            CountString := Copy(ParamUpRoot, Length(UpRoot)+1, MAXINT);
            if TryStrToInt(CountString, Count) then
            begin
              if Count > MaxCount then
              begin
                MaxCount := Count;
              end;
            end;
          end;
        end;
      end;
    else Assert(False);
  end;
  Inc(MaxCount);
  result := Root + IntToStr(MaxCount);
end;

procedure TfrmModflowParameters.rdeParamValueChange(Sender: TObject);
begin
  inherited;
  ChangeSelectedCellsInColumn(dgSteadyParameters, Ord(pcValue), rdeParamValue.Text);
end;

procedure TfrmModflowParameters.seNumberOfParametersChange(Sender: TObject);
var
  RowIndex: Integer;
  Parameter: TModflowParameter;
begin
  inherited;
  if seNumberOfParameters.AsInteger = 0 then
  begin
    ActiveGrid.RowCount := 2;
    ClearFirstRow;
  end
  else
  begin
    ActiveGrid.RowCount := seNumberOfParameters.AsInteger +1;
  end;

  btnDelete.Enabled := seNumberOfParameters.AsInteger > 0;

  // Create new parameters.
  for RowIndex := PriorNumberOfParameters+1 to
    seNumberOfParameters.AsInteger do
  begin
    Parameter := nil;
    case CurrentParameterType of
      ptLPF_HK..ptLPF_VKCB:
        begin
          Parameter := SteadyParameters.Add as TModflowParameter;
        end;
      ptCHD..ptDRT:
        begin
          Parameter := TransientListParameters.Add as TModflowParameter;
        end
      else Assert(False);
    end;
    Parameter.ParameterType := CurrentParameterType;
    Parameter.ParameterName := NewParameterName;
    AssignParameterToRow(RowIndex, Parameter);
  end;

  // Get rid of old parameters.
  for RowIndex := PriorNumberOfParameters downto
    seNumberOfParameters.AsInteger+1 do
  begin
    Parameter := ActiveGrid.Objects[0, RowIndex] as TModflowParameter;
    if Parameter is TModflowSteadyParameter then
    begin
      SteadyParameters.Remove(Parameter);
    end
    else if Parameter is TModflowTransientListParameter then
    begin
      TransientListParameters.Remove(Parameter);
    end;
  end;
  
  PriorNumberOfParameters := seNumberOfParameters.AsInteger;
  ActiveGrid.Invalidate;
end;

procedure TfrmModflowParameters.seNumberOfParametersEnter(Sender: TObject);
begin
  inherited;
  PriorNumberOfParameters := seNumberOfParameters.AsInteger;
end;

procedure TfrmModflowParameters.SetData;
var
  Undo: TUndoDefineParameters;
begin
  Undo := TUndoDefineParameters.Create(SteadyParameters,
    TransientListParameters);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmModflowParameters.AssignParameterToRow(RowIndex: Integer; Parameter: TModflowParameter);
begin
  ActiveGrid.Objects[0, RowIndex] := Parameter;
  ActiveGrid.Cells[Ord(pcName), RowIndex] := Parameter.ParameterName;
  ActiveGrid.Cells[Ord(pcValue), RowIndex] := FloatToStr(Parameter.Value);
  if Parameter is TModflowSteadyParameter then
  begin
    ActiveGrid.Checked[Ord(pcUseZone), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseZone;
    ActiveGrid.Checked[Ord(pcUseMultiplier), RowIndex] :=
      TModflowSteadyParameter(Parameter).UseMultiplier;
  end;
end;

procedure TfrmModflowParameters.tvParameterTypesChange(Sender: TObject;
  Node: TTreeNode);
var
  ParamList: TList;
  Index: integer;
  Parameter: TModflowParameter;
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := 1 to dgSteadyParameters.RowCount - 1 do
  begin
    dgSteadyParameters.Objects[0,RowIndex] := nil;
  end;
  for RowIndex := 1 to dgTransientParameters.RowCount - 1 do
  begin
    dgTransientParameters.Objects[0,RowIndex] := nil;
  end;

  CurrentParameterType := TParameterType(Node.Data);

  case CurrentParameterType of
    ptUndefined:
      begin
        jvplParameters.ActivePage := jvspBlank;
        ActiveGrid := nil;
      end;
    ptLPF_HK..ptLPF_VKCB:
      begin
        ActiveGrid := dgSteadyParameters;
        jvplParameters.ActivePage := jvspSteadyParameters;
      end;
    ptCHD..ptDRT:
      begin
        ActiveGrid := dgTransientParameters;
        jvplParameters.ActivePage := jvspTransientParameters;
      end;
    else Assert(False);
  end;


  rbwParamCountController.Enabled := CurrentParameterType <> ptUndefined;
  if rbwParamCountController.Enabled then
  begin
    ParamList := TList.Create;
    try
      for Index := 0 to SteadyParameters.Count - 1 do
      begin
        Parameter := SteadyParameters[Index];
        if Parameter.ParameterType = CurrentParameterType then
        begin
          ParamList.Add(Parameter);
        end;
      end;
      for Index := 0 to TransientListParameters.Count - 1 do
      begin
        Parameter := TransientListParameters[Index];
        if Parameter.ParameterType = CurrentParameterType then
        begin
          ParamList.Add(Parameter);
        end;
      end;
        
      Assert(ActiveGrid <> nil);
      if ParamList.Count = 0 then
      begin
        ActiveGrid.RowCount := 2;
      end
      else
      begin
        ActiveGrid.RowCount := ParamList.Count+1;
      end;
      for RowIndex := 1 to ParamList.Count do
      begin
        Parameter := ParamList[RowIndex-1];
        AssignParameterToRow(RowIndex, Parameter);
      end;
      PriorNumberOfParameters := ParamList.Count;
      seNumberOfParameters.AsInteger := ParamList.Count;
      seNumberOfParametersChange(nil);
    finally
      ParamList.Free;
    end;
  end;
  if ActiveGrid <> nil then
  begin
    ActiveGrid.Invalidate;
  end;
end;

{ TUndoDefineParameters }

constructor TUndoDefineParameters.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters);
var
  Index: Integer;
  ScreenObject: TScreenObject;
  OldBound: TChdBoundary;
begin
  inherited Create;
  FNewDataSets := TList.Create;
  FNewSteadyParameters:= NewSteadyParameters;
  // TUndoDefineLayers takes ownership of NewSteadyParameters.
  NewSteadyParameters := nil;

  FNewTransientParameters := NewTransientParameters;
  // TUndoDefineLayers takes ownership of NewTransientParameters.
  NewTransientParameters := nil;

  FOldSteadyParameters:= TModflowSteadyParameters.Create(nil);
  FOldSteadyParameters.Assign(frmGoPhast.PhastModel.ModflowSteadyParameters);
  FOldTransientParameters:= TModflowTransientListParameters.Create(nil);
  FOldTransientParameters.Assign(frmGoPhast.PhastModel.ModflowTransientParameters);

  FOldChdScreenObjectParams := TObjectList.Create;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount  - 1 do
  begin
    ScreenObject :=  frmGoPhast.PhastModel.ScreenObjects[Index];
    OldBound := TChdBoundary.Create(nil, nil);
    OldBound.Assign(ScreenObject.ModflowChdBoundary);
    FOldChdScreenObjectParams.Add(OldBound)
  end;

end;

function TUndoDefineParameters.Description: string;
begin
  result := 'edit parameters';
end;

destructor TUndoDefineParameters.Destroy;
begin
  FOldChdScreenObjectParams.Free;
  FNewSteadyParameters.Free;
  FOldSteadyParameters.Free;
  FNewTransientParameters.Free;
  FOldTransientParameters.Free;
  FNewDataSets.Free;
  inherited;
end;

procedure TUndoDefineParameters.DoCommand;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := FNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters.ClearNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters.Assign(FNewSteadyParameters);
  frmGoPhast.PhastModel.ModflowTransientParameters.Assign(FNewTransientParameters);
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;
end;

procedure TUndoDefineParameters.Undo;
var
  Index: Integer;
  ScreenObject: TScreenObject;
  OldBound: TChdBoundary;
begin
  inherited;
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := FNewDataSets;
  frmGoPhast.PhastModel.ModflowSteadyParameters.Assign(FOldSteadyParameters);
  frmGoPhast.PhastModel.ModflowSteadyParameters.RemoveNewDataSets;
  frmGoPhast.PhastModel.ModflowTransientParameters.Assign(FOldTransientParameters);
  frmGoPhast.PhastModel.ModflowSteadyParameters.NewDataSets := nil;

  Assert(FOldChdScreenObjectParams.Count = frmGoPhast.PhastModel.ScreenObjectCount);
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount  - 1 do
  begin
    ScreenObject :=  frmGoPhast.PhastModel.ScreenObjects[Index];
    OldBound := FOldChdScreenObjectParams[Index];
    ScreenObject.ModflowChdBoundary.Assign(OldBound);
  end;
end;

end.
