unit frmManageParametersUnit;

interface

uses System.UITypes, System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, Grids, StdCtrls, RbwDataGrid4, Math,
  ModflowParameterUnit, OrderedCollectionUnit,
  ModflowTransientListParameterUnit, HufDefinition, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, RequiredDataSetsUndoUnit, UndoItemsScreenObjects,
  ModflowPackageSelectionUnit, frameGridUnit, JvExExtCtrls, JvNetscapeSplitter,
  PestParamGroupsUnit, Vcl.ComCtrls, frameAvailableObjectsUnit, JvListBox,
  frameParentChildUnit, System.Generics.Collections, JvComCtrls, JvExComCtrls;

type
  TParamColumn = (pcName, pcPackage, pcType, pcValue, pcMult, pcZone,
    pcPilotPoints, pcPestTransform, pcChangeLimitation, pcLowerBound,
    pcUpperBound, pcParamGroup, pcScaled, pcOffset, pcAbsolute, pcTiedParameter);

  TParamGroupColumn = (pgcName, pgcIncType, pgcIncrement, pgcMinIncrement,
    pgcForceCentral, pgcParamIncrementMultiplier, pgcDM3, pgcDM5,
    pgcUseSplitSlope, pgcSplitThreshold, pgcSplitDifference, pgcSplitAction);

  TfrmManageParameters = class(TfrmCustomGoPhast)
    rdgParameters: TRbwDataGrid4;
    pnlBottom: TPanel;
    lblNumParameters: TLabel;
    seNumberOfParameters: TJvSpinEdit;
    btnDelete: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnHelp: TBitBtn;
    btnImportPval: TButton;
    dlgOpenPval: TOpenDialog;
    pnlParameters: TPanel;
    frameParameterGroups: TframeGrid;
    pcParameters: TPageControl;
    tabParameters: TTabSheet;
    tabParameterGroups: TTabSheet;
    tabGroupAssignments: TTabSheet;
    tabTiedParameters: TTabSheet;
    frameParGroupAssignments: TframeParentChild;
    tvTiedParameters: TTreeView;
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure rdgParametersSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgParametersStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure btnDeleteClick(Sender: TObject);
    procedure seNumberOfParametersChange(Sender: TObject);
    procedure rdgParametersMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnOKClick(Sender: TObject);
    procedure rdgParametersBeforeDrawCell(Sender: TObject; ACol, ARow: Integer);
    procedure rdgParametersSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure btnImportPvalClick(Sender: TObject);
    procedure frameParameterGroupsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
    procedure frameParameterGroupsGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure frameParameterGroupssbDeleteClick(Sender: TObject);
    procedure frameParameterGroupsseNumberChange(Sender: TObject);
    procedure frameParameterGroupsGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure tvTiedParametersDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure tvTiedParametersDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure frameParameterGroupssbInsertClick(Sender: TObject);
  private
    FSteadyParameters: TModflowSteadyParameters;
    FHufParameters: THufModflowParameters;
    FTransientListParameters: TModflowTransientListParameters;
    FSfrParamInstances: TSfrParamInstances;
    FParamList: TList;
    FDeletingParam: Boolean;
    FParamGroups: TPestParamGroups;
//    FAvailableParams: TStringList;
    FUsedParams: TStringList;
    FNoNameNode: TTreeNode;
    FGroupDictionary: TDictionary<TPestParamGroup, TTreeNode>;
    FGroupNameDictionary: TDictionary<string, TPestParamGroup>;
    FParamDictionary: TDictionary<TModflowParameter, TTreeNode>;
    FTiedParamDictionary: TDictionary<TModflowParameter, TTreeNode>;
    FParameterNameNameDictionary: TDictionary<string, TModflowParameter>;
    FDragging: Boolean;
    FDeletingGroup: Boolean;
    FParameterTypesChanged: Boolean;
    procedure GetData;
    procedure SetData;
    procedure UpdateParameterTable;
    function ParamValid(ParamType: TParameterType): boolean;
    procedure CreateParameter(ParamIndex: TParameterType; ARow: Integer);
    procedure UpdateParameter(ParamIndex: TParameterType;
      var AParam: TModflowParameter; ARow: Integer);
    procedure CreateOrUpdateParameter(ARow: Integer);
    procedure DeleteAParam(ARow: Integer);
    procedure UpdateListOfUntiedParamNames;
    procedure CreateOrUpdateParamGroup(ARow: Integer);
    procedure CreateParamGroup(ARow: Integer);
    procedure UpdateParamNameGroupList;
    procedure RemoveGroup(ARow: Integer);
    procedure ReassignParameterGroup(Sender: TObject; Node: TTreeNode);
    procedure FreeAParam(AParam: TModflowParameter);
    procedure AssignTiedParam(TiedParamNode: TTreeNode);
    procedure AssignGroupGridRow(AGroup: TPestParamGroup; ARow: Integer);
    { Private declarations }
  public
    { Public declarations }
  end;

Type
  TUndoChangeParameters = class(TCustomUndoChangeParameters)
  private
    FExistingScreenObjects: TScreenObjectEditCollection;
    FOldProperties: TScreenObjectEditCollection;
  public
    Constructor Create(var NewSteadyParameters: TModflowSteadyParameters;
      var NewTransientParameters: TModflowTransientListParameters;
      var NewHufModflowParameters: THufModflowParameters;
      var NewSfrParamInstances: TSfrParamInstances;
      var NewParamGroups: TPestParamGroups);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses
  Contnrs, frmGoPhastUnit, PhastModelUnit, ModflowDiscretizationWriterUnit,
  ScreenObjectUnit, frmShowHideObjectsUnit, ModflowPackagesUnit, ReadPvalUnit,
  IntListUnit, GoPhastTypes, PestPropertiesUnit;

resourcestring
  StrErrorReadingPvalF = 'Error reading Pval file. Check that it is a valid ' +
  'Pval file.';
  StrErrorReadingPvalF2 = 'Error reading Pval file. The following parameter n' +
  'ames from the Pval file are not included in the model.'#13#10'%s';
  StrName = 'Name';
  StrPackage = 'Package';
  StrType = 'Type';
  StrValue = 'Value';
  StrMultiplierArray = 'Multiplier Array';
  StrZoneArray = 'Zone Array';
  StrThereAreTwoOrMo = 'There are two (or more) parameters named %s. Duplica' +
  'te parameter names are not allowed.';
  StrPilotPoints = 'Pilot Points';
  StrGroupNamePARGPNME = 'Group Name (PARGPNME)';
  StrIncrementTypeINCT = 'Increment Type (INCTYP)';
  StrParameterIncrement = 'Parameter Increment (DERINC)';
  StrMinParameterIncrem = 'Min Parameter Increment (DERINCLB)';
  StrDerivativeMethodF = 'Derivative Method (FORCEN)';
  StrDerivativeIncrement = 'Derivative Increment Multiplier (DERINCMUL)';
  Str3PointDerivativeM = '3-Point Derivative Method (DERMTHD)';
  Str5PointDerivativeM = '5-Point Derivative Method (DERMTHD)';
  StrUseSplitSlopeAnal = 'Use Split Slope Analysis (SPLITTHRESH)';
  StrSplitSlopeThreshol = 'Split Slope Threshold (SPLITTHRESH)';
  StrMaximumRelativeSlo = 'Maximum Relative Slope Difference (SPLITRELDIFF)';
  StrSPLITACTION = 'SPLITACTION';
  StrTransformPARTRANS = 'Transform (PARTRANS)';
  StrChangeLimitationP = 'Change Limitation (PARCHGLIM)';
  StrLowerBoundPARLBND = 'Lower Bound (PARLBND)';
  StrUpperBoundPARUBND = 'Upper Bound (PARUBND)';
  StrParameterGroupPAR = 'Parameter Group (PARGP)';
  StrSCALE = 'SCALE';
  StrOFFSET = 'OFFSET';
  StrTiedParameterPART = 'Tied Parameter (PARTIED)';
  StrNone = '(none)';
  StrABSPARMAXN = 'ABSPARMAX(N)';
  StrTheNameOfThePara = 'The name of the parameter "%s" matches the name of ' +
  'an existing data set, This is not allowed.';
  StrTheParameterTypeO = 'The parameter type of one or more existing paramet' +
  'ers have been changed. Changing the type of an existing parameter is usua' +
  'lly a bad idea and can have far ranging consequences including deleting a' +
  'ny boundary conditions associated with the parameter. Are you sure you wa' +
  'nt to do this?';

{$R *.dfm}
type
  TParamClassType = (pctUnknown, pctSteady, pctTransient, pctHUF, pctPEST);

  TParamRecord = record
    Package: string;
    PType: string;
    PClass: TParamClassType;
  end;

const
  ParamRecords: array[Low(TParameterType).. High(TParameterType)]
    of TParamRecord =
    ((Package: 'Unknown'; PType: 'Unknown'; PClass: pctUnknown),
     (Package: 'LPF, UPW'; PType: 'HK'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'HANI'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'VK'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'VANI'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'SS'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'SY'; PClass: pctSteady),
     (Package: 'LPF, UPW'; PType: 'VKCB'; PClass: pctSteady),
     (Package: 'RCH'; PType: 'RCH'; PClass: pctTransient),
     (Package: 'EVT'; PType: 'EVT'; PClass: pctTransient),
     (Package: 'ETS'; PType: 'ETS'; PClass: pctTransient),
     (Package: 'CHD'; PType: 'CHD'; PClass: pctTransient),
     (Package: 'GHB'; PType: 'GHB'; PClass: pctTransient),
     (Package: 'WEL'; PType: 'Q'; PClass: pctTransient),
     (Package: 'RIV'; PType: 'RIV'; PClass: pctTransient),
     (Package: 'DRN'; PType: 'DRN'; PClass: pctTransient),
     (Package: 'DRT'; PType: 'DRT'; PClass: pctTransient),
     (Package: 'SFR'; PType: 'SFR'; PClass: pctTransient),
     (Package: 'HFB'; PType: 'HFB'; PClass: pctSteady),
     (Package: 'HUF'; PType: 'HK'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'HANI'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'VK'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'VANI'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'SS'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'SY'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'SYTP'; PClass: pctSteady),
     (Package: 'HUF'; PType: 'KDEP'; PClass: pctHUF),
     (Package: 'HUF'; PType: 'LVDA'; PClass: pctSteady),
     (Package: 'STR'; PType: 'STR'; PClass: pctTransient),
     (Package: 'FMP'; PType: 'QMAX'; PClass: pctTransient),
     (Package: 'Many'; PType: 'Array'; PClass: pctPEST)
     );

  ParamGroupColumn = Ord(pgcIncType);

Type
  TCompareMethod = class(TObject)
    Method: TParamColumn;
  end;

var
  SortOrder: TList = nil;

function CompareParamNames(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
begin
  P1 := Item1;
  P2 := Item2;
  result := AnsiCompareText(P1.ParameterName, P2.ParameterName);
end;

function ComparePackages(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
  Pkg1, Pkg2: string;
begin
  P1 := Item1;
  P2 := Item2;
  Pkg1 := ParamRecords[P1.ParameterType].Package;
  Pkg2 := ParamRecords[P2.ParameterType].Package;
  result := AnsiCompareText(Pkg1, Pkg2);
end;

function CompareTypes(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
  Type1, Type2: string;
begin
  P1 := Item1;
  P2 := Item2;
  Type1 := ParamRecords[P1.ParameterType].PType;
  Type2 := ParamRecords[P2.ParameterType].PType;
  result := AnsiCompareText(Type1, Type2);
end;

function CompareValues(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
begin
  P1 := Item1;
  P2 := Item2;
  Result := Sign(P1.Value - P2.Value);

end;

function CompareMult(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
  PS1, PS2: TModflowSteadyParameter;
begin
  P1 := Item1;
  P2 := Item2;
  if P1 is TModflowSteadyParameter then
  begin
    if P2 is TModflowSteadyParameter then
    begin
      PS1 := TModflowSteadyParameter(P1);
      PS2 := TModflowSteadyParameter(P2);
      result := Sign(Ord(PS1.UseMultiplier) - Ord(PS2.UseMultiplier));
    end
    else
    begin
      result := 1;
    end;
  end
  else
  begin
    if P2 is TModflowSteadyParameter then
    begin
      result := -1;
    end
    else
    begin
      result := 0;
    end;
  end;
end;

function ComparePilotPoints(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
  PS1, PS2: TModflowSteadyParameter;
begin
  P1 := Item1;
  P2 := Item2;
  if P1 is TModflowSteadyParameter then
  begin
    if P2 is TModflowSteadyParameter then
    begin
      PS1 := TModflowSteadyParameter(P1);
      PS2 := TModflowSteadyParameter(P2);
      result := Sign(Ord(PS1.UsePilotPoints) - Ord(PS2.UsePilotPoints));
    end
    else
    begin
      result := 1;
    end;
  end
  else
  begin
    if P2 is TModflowSteadyParameter then
    begin
      result := -1;
    end
    else
    begin
      result := 0;
    end;
  end;
end;

function CompareZone(Item1, Item2: Pointer): Integer;
var
  P1, P2: TModflowParameter;
  PS1, PS2: TModflowSteadyParameter;
begin
  P1 := Item1;
  P2 := Item2;
  if P1 is TModflowSteadyParameter then
  begin
    if P2 is TModflowSteadyParameter then
    begin
      PS1 := TModflowSteadyParameter(P1);
      PS2 := TModflowSteadyParameter(P2);
      result := Sign(Ord(PS1.UseZone) - Ord(PS2.UseZone));
    end
    else
    begin
      result := 1;
    end;
  end
  else
  begin
    if P2 is TModflowSteadyParameter then
    begin
      result := -1;
    end
    else
    begin
      result := 0;
    end;
  end;
end;

function CompareParameters(Item1, Item2: Pointer): Integer;
var
  Index: Integer;
  CM: TCompareMethod;
begin
  result := 0;
  for Index := 0 to SortOrder.Count - 1 do
  begin
    CM := SortOrder[Index];
    case CM.Method of
      pcName: result := CompareParamNames(Item1, Item2);
      pcPackage: result := ComparePackages(Item1, Item2);
      pcType: result := CompareTypes(Item1, Item2);
      pcValue: result := CompareValues(Item1, Item2);
      pcMult: result := CompareMult(Item1, Item2);
      pcZone: result := CompareZone(Item1, Item2);
      pcPilotPoints: result := ComparePilotPoints(Item1, Item2);
      else
        Exit;
    end;
    if result <> 0 then
    begin
      Exit;
    end;
  end;
end;

procedure TfrmManageParameters.btnImportPvalClick(Sender: TObject);
var
  PvalList: TParamList;
  MyList: TStringList;
  ValIndex: Integer;
  Item: TParamItem;
  RowIndex: Integer;
  IntList: TIntegerList;
  InvalidParameters: TStringList;
begin
  inherited;
  if dlgOpenPval.Execute then
  begin
    PvalList := TParamList.Create;
    try
      if ReadPvalFile(dlgOpenPval.FileName, PvalList) then
      begin
        MyList := TStringList.Create;
        IntList := TIntegerList.Create;
        InvalidParameters := TStringList.Create;
        try
          MyList.Assign(rdgParameters.Cols[Ord(pcName)]);
          MyList.Delete(0);
          MyList.CaseSensitive := False;

          for ValIndex := 0 to PvalList.Count - 1 do
          begin
            Item := PvalList[ValIndex];
            RowIndex := MyList.IndexOf(Item.Name)+1;
            if RowIndex >= 1 then
            begin
              IntList.Add(RowIndex);
            end
            else
            begin
              InvalidParameters.Add(Item.Name);
            end;
          end;
          if InvalidParameters.Count > 0 then
          begin
            Beep;
            MessageDlg(Format(StrErrorReadingPvalF2, [InvalidParameters.Text]),
              mtError, [mbOK], 0);
          end
          else
          begin
            Assert(IntList.Count = PvalList.Count);
            for ValIndex := 0 to PvalList.Count - 1 do
            begin
              Item := PvalList[ValIndex];
              RowIndex := IntList[ValIndex];
              rdgParameters.Cells[Ord(pcValue), RowIndex] :=
                FloatToStr(Item.Value);
              rdgParametersSetEditText(rdgParameters, Ord(pcValue), RowIndex,
                rdgParameters.Cells[Ord(pcValue), RowIndex]);
            end;
          end;
        finally
          MyList.Free;
          IntList.Free;
          InvalidParameters.Free;
        end;
      end
      else
      begin
        Beep;
        MessageDlg(StrErrorReadingPvalF, mtError, [mbOK], 0);
      end;
    finally
      PvalList.Free;
    end;
  end;
end;

procedure TfrmManageParameters.btnOKClick(Sender: TObject);
var
  Names: TStringList;
  Index: Integer;
  DataArrayManager: TDataArrayManager;
  ExistingDataSet: TObject;
begin
  inherited;
  if FParameterTypesChanged then
  begin
    Beep;
    if (MessageDlg(StrTheParameterTypeO, mtConfirmation, [mbYes, mbNo],
      0, mbNo) <> mrYes) then
    begin
      ModalResult := mrNone;
      Exit;
    end;
  end;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  Names := TStringList.Create;
  try
    Names.Assign(rdgParameters.Cols[Ord(pcName)]);
    Names.Delete(0);
    Names.Sort;
    for Index := 1 to Names.Count - 1 do
    begin
      if UpperCase(Names[Index]) = UpperCase(Names[Index-1]) then
      begin
        Beep;
        MessageDlg(Format(StrThereAreTwoOrMo,[Names[Index]]),
          mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end;
    for Index := 0 to Names.Count - 1 do
    begin
      ExistingDataSet := DataArrayManager.GetDataSetByName(Names[Index]);
      if ExistingDataSet <> nil then
      begin
        Beep;
        MessageDlg(Format(StrTheNameOfThePara,[Names[Index]]),
          mtError, [mbOK], 0);
        ModalResult := mrNone;
        Exit;
      end;
    end;
  finally
    Names.Free;
  end;
  SetData;
end;

procedure TfrmManageParameters.btnDeleteClick(Sender: TObject);
begin
  inherited;
  if rdgParameters.SelectedRow >= 1 then
  begin
    DeleteAParam(rdgParameters.SelectedRow);
    seNumberOfParameters.AsInteger := seNumberOfParameters.AsInteger -1;
  end;
end;

procedure TfrmManageParameters.FormCreate(Sender: TObject);
var
  PackageList: TStringList;
  Index: TParameterType;
  ParamTypeList: TStringList;
  InvalidateModelEvent: TNotifyEvent;
begin
  inherited;
//  FAvailableParams := TStringList.Create;
  FUsedParams := TStringList.Create;
  FGroupDictionary:= TDictionary<TPestParamGroup, TTreeNode>.Create;    
  FGroupNameDictionary:= TDictionary<string, TPestParamGroup>.Create;
  FParamDictionary := TDictionary<TModflowParameter, TTreeNode>.Create;
  FTiedParamDictionary := TDictionary<TModflowParameter, TTreeNode>.Create;
  FParameterNameNameDictionary := TDictionary<string, TModflowParameter>.Create;
  frameParGroupAssignments.OnMoveNode := ReassignParameterGroup;

  rdgParameters.BeginUpdate;
  try
    rdgParameters.Cells[Ord(pcName), 0] := StrName;
    rdgParameters.Cells[Ord(pcPackage), 0] := StrPackage;
    rdgParameters.Cells[Ord(pcType), 0] := StrType;
    rdgParameters.Cells[Ord(pcValue), 0] := StrValue;
    rdgParameters.Cells[Ord(pcMult), 0] := StrMultiplierArray;
    rdgParameters.Cells[Ord(pcZone), 0] := StrZoneArray;
    rdgParameters.Cells[Ord(pcPilotPoints), 0] := StrPilotPoints;

    rdgParameters.Cells[Ord(pcPestTransform), 0] := StrTransformPARTRANS;
    rdgParameters.Cells[Ord(pcChangeLimitation), 0] := StrChangeLimitationP;
    rdgParameters.Cells[Ord(pcLowerBound), 0] := StrLowerBoundPARLBND;
    rdgParameters.Cells[Ord(pcUpperBound), 0] := StrUpperBoundPARUBND;
    rdgParameters.Cells[Ord(pcParamGroup), 0] := StrParameterGroupPAR;
    rdgParameters.Cells[Ord(pcScaled), 0] := StrSCALE;
    rdgParameters.Cells[Ord(pcOffset), 0] := StrOFFSET;
    rdgParameters.Cells[Ord(pcAbsolute), 0] := StrABSPARMAXN;
    rdgParameters.Cells[Ord(pcTiedParameter), 0] := StrTiedParameterPART;
  finally
    rdgParameters.EndUpdate;
  end;

  frameParameterGroups.Grid.BeginUpdate;
  try
    frameParameterGroups.Grid.Cells[Ord(pgcName), 0] := StrGroupNamePARGPNME;
    frameParameterGroups.Grid.Cells[Ord(pgcIncType), 0] := StrIncrementTypeINCT;
    frameParameterGroups.Grid.Cells[Ord(pgcIncrement), 0] := StrParameterIncrement;
    frameParameterGroups.Grid.Cells[Ord(pgcMinIncrement), 0] := StrMinParameterIncrem;
    frameParameterGroups.Grid.Cells[Ord(pgcForceCentral), 0] := StrDerivativeMethodF;
    frameParameterGroups.Grid.Cells[Ord(pgcParamIncrementMultiplier), 0] := StrDerivativeIncrement;
    frameParameterGroups.Grid.Cells[Ord(pgcDM3), 0] := Str3PointDerivativeM;
    frameParameterGroups.Grid.Cells[Ord(pgcDM5), 0] := Str5PointDerivativeM;
    frameParameterGroups.Grid.Cells[Ord(pgcUseSplitSlope), 0] := StrUseSplitSlopeAnal;
    frameParameterGroups.Grid.Cells[Ord(pgcSplitThreshold), 0] := StrSplitSlopeThreshol;
    frameParameterGroups.Grid.Cells[Ord(pgcSplitDifference), 0] := StrMaximumRelativeSlo;
    frameParameterGroups.Grid.Cells[Ord(pgcSplitAction), 0] := StrSPLITACTION;
  finally
    frameParameterGroups.Grid.EndUpdate;
  end;

  PackageList := TStringList.Create;
  try
    for Index := Low(TParameterType) to High(TParameterType) do
    begin
      if ParamValid(Index)
        and (PackageList.IndexOf(ParamRecords[Index].Package) < 0) then
      begin
        PackageList.Add(ParamRecords[Index].Package)
      end;
    end;
    PackageList.Sort;
    rdgParameters.Columns[Ord(pcPackage)].PickList := PackageList;
  finally
    PackageList.Free;
  end;

  ParamTypeList := TStringList.Create;
  try
    for Index := Low(TParameterType) to High(TParameterType) do
    begin
      if ParamValid(Index)
        and (ParamTypeList.IndexOf(ParamRecords[Index].PType) < 0) then
      begin
        ParamTypeList.Add(ParamRecords[Index].PType)
      end;
    end;
    ParamTypeList.Sort;
    rdgParameters.Columns[Ord(pcType)].PickList := ParamTypeList;
  finally
    ParamTypeList.Free;
  end;

  FSteadyParameters := TModflowSteadyParameters.Create(nil);
  FHufParameters := THufModflowParameters.Create(nil);
  FTransientListParameters := TModflowTransientListParameters.Create(nil);
  FSfrParamInstances := TSfrParamInstances.Create(nil);

  InvalidateModelEvent := nil;
  FParamGroups := TPestParamGroups.Create(InvalidateModelEvent);

  GetData;
end;

procedure TfrmManageParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FParameterNameNameDictionary.Free;
  FTiedParamDictionary.Free;
  FParamDictionary.Free;
  FGroupDictionary.Free;
  FGroupNameDictionary.Free;
  FParamGroups.Free;
  FSfrParamInstances.Free;
  FTransientListParameters.Free;
  FHufParameters.Free;
  FSteadyParameters.Free;
  FParamList.Free;
  FUsedParams.Free;
//  FAvailableParams.Free;
end;

procedure TfrmManageParameters.frameParameterGroupsGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  CanSelect: Boolean;
  Names: TStringList;
  Value: Double;
begin
  inherited;
  CanSelect := True;

  if (ARow > 0) and (ACol = Ord(pgcName)) then
  begin
    Names := TStringList.Create;
    try
      Names.Assign(frameParameterGroups.Grid.Cols[Ord(pgcName)]);
      if Names.IndexOf(frameParameterGroups.Grid.Cells[ACol, ARow]) <> ARow then
      begin
        frameParameterGroups.Grid.Canvas.Brush.Color := clRed;
      end;
    finally
      Names.Free;
    end;
  end;

  if (ARow > 0) and (ACol in [Ord(pgcIncrement),
    Ord(pgcParamIncrementMultiplier), Ord(pgcSplitDifference)]) then
  begin
    if TryStrToFloat(frameParameterGroups.Grid.Cells[ACol, ARow], Value) then
    begin
      if Value <= 0 then
      begin
        frameParameterGroups.Grid.Canvas.Brush.Color := clRed;
      end;
    end;
  end;

  frameParameterGroupsGridSelectCell(frameParameterGroups.Grid,
    ACol, ARow, CanSelect);
  if not CanSelect then
  begin
    frameParameterGroups.Grid.Canvas.Brush.Color := clbtnFace;
  end;
end;

procedure TfrmManageParameters.frameParameterGroupsGridSelectCell(
  Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
var
  Col: TParamGroupColumn;
begin
  inherited;
  if (ACol >= 0) and (ARow >= frameParameterGroups.Grid.FixedRows) then
  begin
    Col := TParamGroupColumn(ACol);
    case Col of
      pgcName: ;

      pgcIncType, pgcIncrement, pgcMinIncrement,
        pgcForceCentral, pgcParamIncrementMultiplier,
        pgcUseSplitSlope:
        begin
          CanSelect := (frameParameterGroups.Grid.Cells[Ord(pgcName), ARow] <> '');
        end;
      pgcDM3:
        begin
          CanSelect := (frameParameterGroups.Grid.Cells[Ord(pgcName), ARow] <> '')
            and (frameParameterGroups.Grid.ItemIndex[Ord(pgcForceCentral), ARow]
            in [Ord(fcAlways3), Ord(fcSwitch)])
        end;
      pgcDM5:
        begin
          CanSelect := (frameParameterGroups.Grid.Cells[Ord(pgcName), ARow] <> '')
            and (frameParameterGroups.Grid.ItemIndex[Ord(pgcForceCentral), ARow]
            in [Ord(fcAlways5), Ord(fcSwitch5)])
        end;
      pgcSplitThreshold, pgcSplitDifference, pgcSplitAction:
        begin
          CanSelect := (frameParameterGroups.Grid.Cells[Ord(pgcName), ARow] <> '')
            and frameParameterGroups.Grid.Checked[Ord(pgcUseSplitSlope), ARow];
        end;
      else
        Assert(False);
    end;
  end;
end;

procedure TfrmManageParameters.frameParameterGroupsGridSetEditText(
  Sender: TObject; ACol, ARow: Integer; const Value: string);
var
  PGroupCol: TParamGroupColumn;
  Grid: TRbwDataGrid4;
  PGroup: TPestParamGroup;
  ItemIndex: Integer;
  TreeNode: TTreeNode;
begin
  inherited;
  if (ARow >= 1) and (ACol >= 0) then
  begin
    Grid := frameParameterGroups.Grid;
    PGroupCol := TParamGroupColumn(ACol);
    CreateOrUpdateParamGroup(ARow);
    PGroup := Grid.Objects[Ord(pgcName),ARow] as TPestParamGroup;
    if PGroup = nil then
    begin
      Exit;
    end;
    case PGroupCol of
      pgcName:
        begin
          if PGroup.ParamGroupName <> '' then
          begin
            FGroupNameDictionary.Remove(UpperCase(PGroup.ParamGroupName));
          end;
          PGroup.ParamGroupName := Grid.Cells[ACol, ARow];
          if (PGroup.ParamGroupName <> '')
            and not FGroupNameDictionary.ContainsKey(
            UpperCase(PGroup.ParamGroupName)) then
          begin
            FGroupNameDictionary.Add(UpperCase(PGroup.ParamGroupName), PGroup);
          end;
          if FGroupDictionary.TryGetValue(PGroup, TreeNode) then
          begin
            TreeNode.Text := PGroup.ParamGroupName;
          end;
          UpdateParamNameGroupList;
        end;
      pgcIncType:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            PGroup.IncrementType := TIncrementType(ItemIndex);
          end;
        end;
      pgcIncrement:
        begin
          if Grid.Cells[ACol, ARow] <> '' then
          begin
            PGroup.ParamIncrement := Grid.RealValue[ACol, ARow];
          end;
        end;
      pgcMinIncrement:
        begin
          if Grid.Cells[ACol, ARow] <> '' then
          begin
            PGroup.MinParamIncrement := Grid.RealValue[ACol, ARow];
          end;
        end;
      pgcForceCentral:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            PGroup.ForceCentral := TForceCentral(ItemIndex);
          end;
        end;
      pgcParamIncrementMultiplier:
        begin
          if Grid.Cells[ACol, ARow] <> '' then
          begin
            PGroup.ParamIncrementMultiplier := Grid.RealValue[ACol, ARow];
          end;
        end;
      pgcDM3:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            PGroup.DM3 := TDerivativeMethod3(ItemIndex);
          end;
        end;
      pgcDM5:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            PGroup.DM5 := TDerivativeMethod5(ItemIndex);
          end;
        end;
      pgcUseSplitSlope:
        begin
          PGroup.UseSplitSlopeAnalysis := Grid.Checked[ACol, ARow];
        end;
      pgcSplitThreshold:
        begin
          if Grid.Cells[ACol, ARow] <> '' then
          begin
            PGroup.SplitThreshold := Grid.RealValue[ACol, ARow];
          end;
        end;
      pgcSplitDifference:
        begin
          if Grid.Cells[ACol, ARow] <> '' then
          begin
            PGroup.RelSlopeDif := Grid.RealValue[ACol, ARow];
          end;
        end;
      pgcSplitAction:
        begin
          ItemIndex := Grid.ItemIndex[ACol, ARow];
          if ItemIndex >= 0 then
          begin
            PGroup.SplitAction := TSplitAction(ItemIndex);
          end;
        end;
    end;
  end;
end;

procedure TfrmManageParameters.frameParameterGroupssbDeleteClick(
  Sender: TObject);
var
  Grid: TRbwDataGrid4;
begin
  inherited;
  FDeletingGroup := True;
  try
    Grid := frameParameterGroups.Grid;
    if Grid.SelectedRow >= Grid.FixedRows  then
    begin
      RemoveGroup(Grid.SelectedRow);
    end;
    frameParameterGroups.sbDeleteClick(Sender);
  finally
    FDeletingGroup := False;
  end;
end;

procedure TfrmManageParameters.frameParameterGroupssbInsertClick(
  Sender: TObject);
begin
  inherited;
  frameParameterGroups.sbInsertClick(Sender);

end;

procedure TfrmManageParameters.frameParameterGroupsseNumberChange(
  Sender: TObject);
var
  Grid: TRbwDataGrid4;
  NewCount: Integer;
  OldCount: Integer;
  RowIndex: Integer;
begin
  inherited;
  Grid := frameParameterGroups.Grid;
  NewCount := frameParameterGroups.seNumber.AsInteger;
  OldCount := Grid.RowCount-1;
  for RowIndex := OldCount downto NewCount+1 do
  begin
    RemoveGroup(RowIndex);
  end;

  frameParameterGroups.seNumberChange(Sender);

  for RowIndex := OldCount+1 to Grid.RowCount -1 do
  begin
    Grid.Objects[ParamGroupColumn, RowIndex] := nil;
  end;

  UpdateParamNameGroupList;
end;

procedure TfrmManageParameters.GetData;
var
  Index: Integer;
  PhastModel: TPhastModel;
  AParam: TModflowParameter;
  ItemIndex: Integer;
  AGroup: TPestParamGroup;
//  Grid: TRbwDataGrid4;
  GroupTree: TTreeView;
  ParamIndex: Integer;
  ATreeNode: TTreeNode;
  ParamNode: TTreeNode;
  TiedParam: TModflowParameter;
  ParentNode: TTreeNode;
//  ARow: Integer;
begin
  pcParameters.ActivePageIndex := 0;

  FParamList := TList.Create;
  PhastModel := frmGoPhast.PhastModel;

  if not PhastModel.PestUsed then
  begin
    rdgParameters.ColCount := Succ(Ord(pcZone));
    tabParameterGroups.TabVisible := False;
    tabGroupAssignments.TabVisible := False;
    tabTiedParameters.TabVisible := False;
    tabParameters.TabVisible := False;
    pcParameters.ActivePage := tabParameters;
  end;

  FSteadyParameters.Assign(PhastModel.ModflowSteadyParameters);
  FHufParameters.Assign(PhastModel.HufParameters);
  FTransientListParameters.Assign(PhastModel.ModflowTransientParameters);
  FSfrParamInstances.Assign(PhastModel.ModflowPackages.
    SfrPackage.ParameterInstances);
  FParamGroups.Assign(PhastModel.ParamGroups);

  GroupTree := frameParGroupAssignments.tvTree;
  GroupTree.Items.Clear;
  FGroupDictionary.Clear;
  FGroupNameDictionary.Clear;

  FNoNameNode := GroupTree.Items.AddChild(nil, StrNone);

//  Grid := frameParameterGroups.Grid;
  frameParameterGroups.seNumber.AsInteger := FParamGroups.Count;
  for ItemIndex := 0 to FParamGroups.Count - 1 do
  begin
    AGroup := FParamGroups[ItemIndex];
    AssignGroupGridRow(AGroup, ItemIndex+1);

    ATreeNode := GroupTree.Items.AddChild(nil, AGroup.ParamGroupName);
    ATreeNode.Data := AGroup;
    FGroupDictionary.Add(AGroup, ATreeNode);
    if not FGroupNameDictionary.ContainsKey(UpperCase(AGroup.ParamGroupName)) then
    begin
      FGroupNameDictionary.Add(UpperCase(AGroup.ParamGroupName), AGroup);
    end;
  end;

  UpdateParamNameGroupList;

  FParamList.Capacity := FSteadyParameters.Count
    + FTransientListParameters.Count
    + FHufParameters.Count;
  seNumberOfParameters.AsInteger := FParamList.Capacity;

  for Index := 0 to FSteadyParameters.Count - 1 do
  begin
    AParam := FSteadyParameters[Index];
    FParamList.Add(AParam);
  end;
  for Index := 0 to FTransientListParameters.Count - 1 do
  begin
    AParam := FTransientListParameters[Index];
    FParamList.Add(AParam);
  end;
  for Index := 0 to FHufParameters.Count - 1 do
  begin
    AParam := FHufParameters[Index];
    FParamList.Add(AParam);
  end;
  UpdateParameterTable;

  for ParamIndex := 0 to FParamList.Count - 1 do
  begin
    AParam := FParamList[ParamIndex];
    if FGroupNameDictionary.TryGetValue(UpperCase(AParam.ParameterGroup), AGroup)  then
    begin
      if FGroupDictionary.TryGetValue(AGroup, ATreeNode) then
      begin
       ParamNode := GroupTree.Items.AddChild(ATreeNode, AParam.ParameterName);
      end
      else
      begin
        ParamNode := GroupTree.Items.AddChild(FNoNameNode, AParam.ParameterName);
      end;
    end
    else
    begin
      ParamNode := GroupTree.Items.AddChild(FNoNameNode, AParam.ParameterName);
    end;
    ParamNode.Data := AParam;
    FParamDictionary.Add(AParam, ParamNode)
  end;

  for ParamIndex := 0 to FParamList.Count - 1 do
  begin
    AParam := FParamList[ParamIndex];
    if AParam.Transform <> ptTied then
    begin
      ParamNode := tvTiedParameters.Items.AddChild(nil, AParam.ParameterName);
      ParamNode.Data := AParam;
      FTiedParamDictionary.Add(AParam, ParamNode);
    end;
    FParameterNameNameDictionary.Add(UpperCase(AParam.ParameterName), AParam)
  end;

  for ParamIndex := 0 to FParamList.Count - 1 do
  begin
    AParam := FParamList[ParamIndex];
    if AParam.Transform = ptTied then
    begin
      if FParameterNameNameDictionary.TryGetValue(
        UpperCase(AParam.TiedParameterName), TiedParam) then
      begin
        if FTiedParamDictionary.TryGetValue(TiedParam, ParentNode) then
        begin
          ParamNode := tvTiedParameters.Items.AddChild(
            ParentNode, AParam.ParameterName);
          ParamNode.Data := AParam;
          FTiedParamDictionary.Add(AParam, ParamNode);
        end;
      end;
    end;
  end;
  FParameterTypesChanged := False;
end;

function TfrmManageParameters.ParamValid(ParamType: TParameterType): boolean;
var
  TransientModel: Boolean;
  PestOnly: Boolean;
  LpfUpwUsed: Boolean;
begin
  result := False;
  TransientModel := frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;
  PestOnly := frmGoPhast.ModelSelection in [msModflow2015] + SutraSelection;
  LpfUpwUsed := (frmGoPhast.PhastModel.LpfIsSelected
    or frmGoPhast.PhastModel.UpwIsSelected);

  case ParamType of
    ptUndefined: result := False;
    ptLPF_HK: result := not PestOnly and LpfUpwUsed;
    ptLPF_HANI: result := not PestOnly and LpfUpwUsed;
    ptLPF_VK: result := not PestOnly and LpfUpwUsed;
    ptLPF_VANI: result := not PestOnly and LpfUpwUsed;
    ptLPF_SS: result := not PestOnly and LpfUpwUsed and TransientModel;
    ptLPF_SY: result := not PestOnly and LpfUpwUsed and TransientModel;
    ptLPF_VKCB: result := not PestOnly and LpfUpwUsed;
    ptRCH: result := frmGoPhast.PhastModel.RchIsSelected;
    ptEVT: result := frmGoPhast.PhastModel.EvtIsSelected;
    ptETS: result := frmGoPhast.PhastModel.EtsIsSelected;
    ptCHD: result := frmGoPhast.PhastModel.ChdIsSelected;
    ptGHB: result := frmGoPhast.PhastModel.GhbIsSelected;
    ptQ: result := frmGoPhast.PhastModel.WelIsSelected;
    ptRIV: result := frmGoPhast.PhastModel.RivIsSelected;
    ptDRN: result := frmGoPhast.PhastModel.DrnIsSelected;
    ptDRT: result := frmGoPhast.PhastModel.DrtIsSelected;
    ptSFR: result := frmGoPhast.PhastModel.SfrIsSelected;
    ptHFB: result := frmGoPhast.PhastModel.HfbIsSelected;
    ptHUF_HK: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_HANI: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_VK: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_VANI: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_SS: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected
      and TransientModel;
    ptHUF_SY: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected
      and TransientModel;
    ptHUF_SYTP: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected
      and TransientModel;
    ptHUF_KDEP: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_LVDA: result := not PestOnly and frmGoPhast.PhastModel.HufIsSelected;
    ptSTR: result := not PestOnly and frmGoPhast.PhastModel.StrIsSelected;
    ptQMAX: result := frmGoPhast.PhastModel.FarmProcessIsSelected;
    ptPEST: result := frmGoPhast.PhastModel.PestUsed;
    else Assert(False);
  end;
end;

procedure TfrmManageParameters.CreateParameter(ParamIndex: TParameterType; ARow: Integer);
var
  AParam: TModflowParameter;
  PhastModel: TPhastModel;
  ParamNode: TTreeNode;
begin
  PhastModel := frmGoPhast.PhastModel;
  AParam := nil;
  case ParamRecords[ParamIndex].PClass of
    pctUnknown:
      begin
        Assert(False);
      end;
    pctSteady:
      begin
        AParam := FSteadyParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := True;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := True;
        if PhastModel.PestUsed then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ARow] := False;
        end;
        rdgParameters.SpecialFormat[Ord(pcMult), ARow] := rcf4Boolean;
        rdgParameters.SpecialFormat[Ord(pcZone), ARow] := rcf4Boolean;
      end;
    pctTransient:
      begin
        AParam := FTransientListParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := False;
        if PhastModel.PestUsed then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ARow] := False;
        end;
      end;
    pctHUF:
      begin
        AParam := FHufParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := False;
        if PhastModel.PestUsed then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ARow] := False;
        end;
      end;
    pctPEST:
      begin
        AParam := FSteadyParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := False;
        if PhastModel.PestUsed then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ARow] := True;
          rdgParameters.SpecialFormat[Ord(pcPilotPoints), ARow] := rcf4Boolean;
        end;
      end
  else
    Assert(False);
  end;
  rdgParameters.Objects[Ord(pcName), ARow] := AParam;
  FParamList.Add(AParam);
  AParam.ParameterType := ParamIndex;
  AParam.ParameterName := rdgParameters.Cells[Ord(pcName), ARow];
  if AParam.ParameterName <> rdgParameters.Cells[Ord(pcName), ARow] then
  begin
    rdgParameters.Cells[Ord(pcName), ARow] := AParam.ParameterName;
  end;
  AParam.Value := StrToFloatDef(rdgParameters.Cells[Ord(pcValue), ARow], 0);

  ParamNode := frameParGroupAssignments.tvTree.Items.AddChild
    (FNoNameNode, AParam.ParameterName);
  ParamNode.Data := AParam;
  FParamDictionary.Add(AParam, ParamNode);

end;

procedure TfrmManageParameters.CreateParamGroup(ARow: Integer);
var
  AParamGroup: TPestParamGroup;
  ATreeNode: TTreeNode;
begin
  AParamGroup := FParamGroups.Add;
  AParamGroup.ParamGroupName := frameParameterGroups.Grid.Cells[Ord(pgcName), ARow];
  AssignGroupGridRow(AParamGroup, ARow);
//  frameParameterGroups.Grid.Objects[Ord(pgcName), ARow] := AParamGroup;
  frameParameterGroups.Grid.SetEditorUpdateToEnd;
  ATreeNode := frameParGroupAssignments.tvTree.Items.AddChild(nil, '');
  ATreeNode.Data := AParamGroup;
  FGroupDictionary.Add(AParamGroup, ATreeNode);
end;

procedure TfrmManageParameters.UpdateParameter(ParamIndex: TParameterType;
  var AParam: TModflowParameter; ARow: Integer);
//var
//  ParamNode: TTreeNode;
begin
  case ParamRecords[ParamIndex].PClass of
    pctUnknown:
      begin
        Assert(False);
      end;
    pctSteady, pctPESt:
      begin
        if AParam is TModflowSteadyParameter then
        begin
          if AParam.ParameterType <> ParamIndex then
          begin
            FParameterTypesChanged := True;
          end;
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParameterTypesChanged := True;
          FreeAParam(AParam);
          AParam := nil;
          rdgParameters.Objects[Ord(pcName), ARow] := nil;
        end;
      end;
    pctTransient:
      begin
        if AParam is TModflowTransientListParameter then
        begin
          if AParam.ParameterType <> ParamIndex then
          begin
            FParameterTypesChanged := True;
          end;
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParameterTypesChanged := True;
          FreeAParam(AParam);
          AParam := nil;
          rdgParameters.Objects[Ord(pcName), ARow] := nil;
        end;
      end;
    pctHUF:
      begin
        if AParam is THufParameter then
        begin
          if AParam.ParameterType <> ParamIndex then
          begin
            FParameterTypesChanged := True;
          end;
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParameterTypesChanged := True;
          FreeAParam(AParam);
          AParam := nil;
          rdgParameters.Objects[Ord(pcName), ARow] := nil;
        end;
      end;
  else
    Assert(False);
  end;
end;

procedure TfrmManageParameters.CreateOrUpdateParameter(ARow: Integer);
var
  ParamIndex: TParameterType;
  AParam: TModflowParameter; 
begin
  for ParamIndex := ptLPF_HK to High(TParameterType) do
  begin
    if ParamValid(ParamIndex)
      and (rdgParameters.Cells[Ord(pcPackage), ARow] =
      ParamRecords[ParamIndex].Package)
      and (rdgParameters.Cells[Ord(pcType), ARow] =
      ParamRecords[ParamIndex].PType) then
    begin
      if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
      begin
        AParam := rdgParameters.Objects[Ord(pcName), ARow] as TModflowParameter;
        if AParam.ParameterType = ParamIndex then
        begin
          break;
        end;
        UpdateParameter(ParamIndex, AParam, ARow);
      end
      else
      begin
        AParam := nil;
      end;
      if AParam = nil then
      begin
        CreateParameter(ParamIndex, ARow);
      end;
      break;
    end;
  end;
end;

procedure TfrmManageParameters.CreateOrUpdateParamGroup(ARow: Integer);
var
  Grid: TRbwDataGrid4;
  AParamGroup: TPestParamGroup;
begin
  Grid := frameParameterGroups.Grid;
  if (ARow >= Grid.RowCount) or FDeletingGroup then
  begin
    Exit;
  end;
  if (Grid.Objects[Ord(pgcName), ARow] <> nil) then
  begin
    AParamGroup := Grid.Objects[Ord(pgcName), ARow] as TPestParamGroup;
  end
  else
  begin
    AParamGroup := nil;
  end;
  if AParamGroup = nil then
  begin
    CreateParamGroup(ARow);
  end;
end;

procedure TfrmManageParameters.DeleteAParam(ARow: Integer);
var
  AParam: TModflowParameter;
  ColIndex: Integer;
begin
  FDeletingParam := True;
  try
    AParam := rdgParameters.Objects[Ord(pcName), ARow] as TModflowParameter;
    FreeAParam(AParam);

    rdgParameters.Objects[Ord(pcName), ARow] := nil;
    if rdgParameters.RowCount > 2 then
    begin
      rdgParameters.DeleteRow(ARow);
    end
    else
    begin
      for ColIndex := 0 to rdgParameters.ColCount - 1 do
      begin
        rdgParameters.Cells[ColIndex, 1] := '';
        rdgParameters.Objects[ColIndex, 1] := nil;
      end;
    end;
  finally
    FDeletingParam := False;
  end;

end;

procedure TfrmManageParameters.ReassignParameterGroup(Sender: TObject;
  Node: TTreeNode);
var
  AParam: TModflowParameter;
  AParamGroup: TPestParamGroup;
  Params: TStrings;
  Row: Integer;
begin
  AParam := Node.Data;
  AParamGroup := Node.Parent.Data;
  if AParamGroup <> nil then
  begin
    AParam.ParameterGroup := AParamGroup.ParamGroupName;
  end
  else
  begin
    AParam.ParameterGroup := '';
  end;
  Params := rdgParameters.Cols[Ord(pcName)];
  Row := Params.IndexOfObject(AParam);
  if Row >= 1 then
  begin
    rdgParameters.Cells[Ord(pcParamGroup), Row] := AParam.ParameterGroup;
    rdgParameters.Objects[Ord(pcParamGroup), Row] := AParamGroup;
  end;
end;

procedure TfrmManageParameters.FreeAParam(AParam: TModflowParameter);
var
  ATreeNode: TTreeNode;
  ChildNode: TTreeNode;
  LinkedParamCol: TStrings;
  Row: Integer;
begin
  if AParam <> nil then
  begin
    FSfrParamInstances.DeleteInstancesOfParameter(AParam.ParameterName);
  end;
  if FParamDictionary.TryGetValue(AParam, ATreeNode) then
  begin
    ATreeNode.Free;
    FParamDictionary.Remove(AParam);
  end;
  if FTiedParamDictionary.TryGetValue(AParam, ATreeNode) then
  begin
    ChildNode := ATreeNode.GetFirstChild;
    while ChildNode <> nil do
    begin
      ChildNode.MoveTo(nil, naAddChild);
      AssignTiedParam(ChildNode);
      ChildNode := ATreeNode.GetFirstChild;
    end;
    ATreeNode.Free;
    FTiedParamDictionary.Remove(AParam);
  end;
  if AParam <> nil then
  begin
    FParameterNameNameDictionary.Remove(UpperCase(AParam.ParameterName));
    FParamList.Remove(AParam);

    LinkedParamCol := rdgParameters.Cols[Ord(pcTiedParameter)];
    Row := LinkedParamCol.IndexOf(AParam.ParameterName);
    while Row >= 1 do
    begin
      rdgParameters.Cells[Ord(pcTiedParameter), Row] := '';
      rdgParameters.Objects[Ord(pcTiedParameter), Row] := nil;
      Row := LinkedParamCol.IndexOf(AParam.ParameterName);
    end;
  end;

  AParam.Free;
end;

procedure TfrmManageParameters.RemoveGroup(ARow: Integer);
var
  AGroup: TPestParamGroup;
  TreeNode: TTreeNode;
  ParamNode: TTreeNode;
  Grid: TRbwDataGrid4;
begin
  Grid := frameParameterGroups.Grid;
  AGroup := Grid.Objects[Ord(pgcName), ARow] as TPestParamGroup;
  if AGroup = nil then
  begin
    Exit;
  end;
  FGroupNameDictionary.Remove(UpperCase(AGroup.ParamGroupName));
  if FGroupDictionary.TryGetValue(AGroup, TreeNode) then
  begin
    ParamNode := TreeNode.GetFirstChild;
    while ParamNode <> nil do
    begin
      ParamNode.MoveTo(FNoNameNode, naAddChild);
      ParamNode := TreeNode.GetFirstChild;
    end;
    FGroupDictionary.Remove(AGroup);
    TreeNode.Free;
  end;
  AGroup.Free;
  Grid.Objects[Ord(pgcName), ARow] := nil;
end;

procedure TfrmManageParameters.UpdateListOfUntiedParamNames;
var
  RowIndex: Integer;
  AParam: TModflowParameter;
  APickList: TStringList;
begin
  if not frmGoPhast.PhastModel.PestUsed then
  begin
    Exit
  end;
  APickList := TStringList.Create;
  try
    for RowIndex := 1 to rdgParameters.RowCount - 1 do
    begin
      if rdgParameters.Objects[Ord(pcName), RowIndex] <> nil then
      begin
        AParam := rdgParameters.Objects[Ord(pcName), RowIndex] as TModflowParameter;
        if (AParam.ParameterName <> '')
          and not (AParam.Transform in [ptFixed, ptTied]) then
        begin
          APickList.AddObject(AParam.ParameterName, AParam);
        end;
      end;
    end;
    APickList.Sorted := True;
    rdgParameters.Columns[Ord(pcTiedParameter)].PickList := APickList;
  finally
    APickList.Free;
  end;
end;

procedure TfrmManageParameters.UpdateParameterTable;
var
  SteadyParam: TModflowSteadyParameter;
  AModflowParam: TModflowParameter;
  ParamIndex: Integer;
  PhastModel: TPhastModel;
  PGroupItemIndex: Integer;
begin
  PhastModel := frmGoPhast.PhastModel;
  FParamList.Sort(CompareParameters);
  rdgParameters.BeginUpdate;
  try
    rdgParameters.RowCount := Max(FParamList.Count + 1, 2);
    seNumberOfParameters.AsInteger := FParamList.Count;
    for ParamIndex := 0 to FParamList.Count - 1 do
    begin
      AModflowParam := FParamList[ParamIndex];
      rdgParameters.Objects[Ord(pcName), ParamIndex + 1] := AModflowParam;
      rdgParameters.Cells[Ord(pcName), ParamIndex + 1] :=
        AModflowParam.ParameterName;
      rdgParameters.Cells[Ord(pcPackage), ParamIndex + 1] :=
        ParamRecords[AModflowParam.ParameterType].Package;
      rdgParameters.Cells[Ord(pcType), ParamIndex + 1] :=
        ParamRecords[AModflowParam.ParameterType].PType;
      rdgParameters.Cells[Ord(pcValue), ParamIndex + 1] :=
        FloatToStr(AModflowParam.Value);
      if AModflowParam is TModflowSteadyParameter then
      begin
        SteadyParam := TModflowSteadyParameter(AModflowParam);
        if not (SteadyParam.ParameterType in [ptHFB, ptPest]) then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcMult), ParamIndex + 1] := True;
          rdgParameters.UseSpecialFormat[Ord(pcZone), ParamIndex + 1] := True;
          rdgParameters.SpecialFormat[Ord(pcMult), ParamIndex + 1] :=
            rcf4Boolean;
          rdgParameters.SpecialFormat[Ord(pcZone), ParamIndex + 1] :=
            rcf4Boolean;
          rdgParameters.Checked[Ord(pcMult), ParamIndex + 1] :=
            SteadyParam.UseMultiplier;
          rdgParameters.Checked[Ord(pcZone), ParamIndex + 1] :=
            SteadyParam.UseZone;
        end
        else
        begin
          rdgParameters.UseSpecialFormat[Ord(pcMult), ParamIndex + 1] := False;
          rdgParameters.UseSpecialFormat[Ord(pcZone), ParamIndex + 1] := False;
        end;
        if PhastModel.PestUsed then
        begin
          if (SteadyParam.ParameterType = ptPest) then
          begin
            rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ParamIndex + 1] := True;
            rdgParameters.SpecialFormat[Ord(pcPilotPoints), ParamIndex + 1] :=
              rcf4Boolean;
            rdgParameters.Checked[Ord(pcPilotPoints), ParamIndex + 1] :=
              SteadyParam.UsePilotPoints;
          end
          else
          begin
            rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ParamIndex + 1] := False;
          end;
        end;
      end
      else
      begin
        rdgParameters.UseSpecialFormat[Ord(pcMult), ParamIndex + 1] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ParamIndex + 1] := False;
        if PhastModel.PestUsed then
        begin
          rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), ParamIndex + 1] := False;
        end;
      end;
      if PhastModel.PestUsed then
      begin
        rdgParameters.ItemIndex[Ord(pcPestTransform), ParamIndex + 1] :=
          Ord(AModflowParam.Transform);
        rdgParameters.ItemIndex[Ord(pcChangeLimitation), ParamIndex + 1] :=
          Ord(AModflowParam.ChangeLimitation);
        rdgParameters.RealValue[Ord(pcLowerBound), ParamIndex + 1] :=
          AModflowParam.LowerBound;
        rdgParameters.RealValue[Ord(pcUpperBound), ParamIndex + 1] :=
          AModflowParam.UpperBound;
        rdgParameters.Cells[Ord(pcParamGroup), ParamIndex + 1] :=
          AModflowParam.ParameterGroup;
        PGroupItemIndex := rdgParameters.ItemIndex[Ord(pcParamGroup), ParamIndex + 1];
        if PGroupItemIndex >= 0 then
        begin
          rdgParameters.Objects[Ord(pcParamGroup), ParamIndex + 1] :=
            rdgParameters.Columns[Ord(pcParamGroup)].Picklist.Objects[PGroupItemIndex];
        end;
        rdgParameters.RealValue[Ord(pcScaled), ParamIndex + 1] :=
          AModflowParam.Scale;
        rdgParameters.RealValue[Ord(pcOffset), ParamIndex + 1] :=
          AModflowParam.Offset;
        rdgParameters.RealValue[Ord(pcAbsolute), ParamIndex + 1] :=
          AModflowParam.AbsoluteN;
        rdgParameters.Cells[Ord(pcTiedParameter), ParamIndex + 1] :=
          AModflowParam.TiedParameterName;
      end;
    end;

    UpdateListOfUntiedParamNames;
  finally
    rdgParameters.EndUpdate;
  end;
end;

procedure TfrmManageParameters.UpdateParamNameGroupList;
var
  NewPickList: TStringList;
  PGroup: TPestParamGroup;
  ItemIndex: Integer;
  AnObject: TObject;
  RowIndex: Integer;
begin
{$IFNDEF PEST}
  Exit;
{$ENDIF}
  if not frmGoPhast.PhastModel.PestUsed then
  begin
    Exit;
  end;
  if FParamGroups = nil then
  begin
    Exit;
  end;

  NewPickList := TStringList.Create;
  try
    for ItemIndex := 0 to FParamGroups.Count - 1 do
    begin
      PGroup := FParamGroups[ItemIndex];
      NewPickList.AddObject(PGroup.ParamGroupName, PGroup);
    end;
    NewPickList.Sorted := True;

    for RowIndex := 1 to rdgParameters.RowCount - 1 do
    begin
      AnObject := rdgParameters.Objects[Ord(pcParamGroup), RowIndex];
      ItemIndex := NewPickList.IndexOfObject(AnObject);
      if ItemIndex >= 0 then
      begin
        rdgParameters.Cells[Ord(pcParamGroup), RowIndex] :=
          (AnObject as TPestParamGroup).ParamGroupName;
      end
      else
      begin
        rdgParameters.Objects[Ord(pcParamGroup), RowIndex] := nil;
        rdgParameters.Cells[Ord(pcParamGroup), RowIndex] := '';
      end;
    end;
    rdgParameters.Columns[Ord(pcParamGroup)].PickList.Clear;
    rdgParameters.Columns[Ord(pcParamGroup)].PickList.Add('none');
    rdgParameters.Columns[Ord(pcParamGroup)].PickList.AddStrings(NewPickList);
  finally
    NewPickList.Free;
  end;
end;

type TGridCrack = class(TRbwDataGrid4);

procedure TfrmManageParameters.rdgParametersBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  Names: TStringList;
  ParamIndex: TParameterType;
  OK_Combination: Boolean;
  LowerBound: Double;
  UpperBound: Double;
  DataArrayManager: TDataArrayManager;
  ExistingDataSet: TObject;
begin
  inherited;
  DataArrayManager := frmGoPhast.PhastModel.DataArrayManager;
  if (ARow > 0) then
  begin
    if (ACol = Ord(pcName)) then
    begin
      Names := TStringList.Create;
      try
        Names.Assign(rdgParameters.Cols[Ord(pcName)]);
        if Names.IndexOf(rdgParameters.Cells[ACol, ARow]) <> ARow then
        begin
          rdgParameters.Canvas.Brush.Color := clRed;
        end
        else
        begin
          ExistingDataSet := DataArrayManager.GetDataSetByName(
            rdgParameters.Cells[ACol, ARow]);
          if ExistingDataSet <> nil then
          begin
            rdgParameters.Canvas.Brush.Color := clRed;
          end;
        end;
      finally
        Names.Free;
      end;
    end
    else
    begin
      if rdgParameters.Cells[Ord(pcName), ARow] = '' then
      begin
        rdgParameters.Canvas.Brush.Color := clBtnFace;
        Exit;
      end;
    end;
    if TParamColumn(ACol) = pcType then
    begin
      OK_Combination := False;
      for ParamIndex := ptLPF_HK to High(TParameterType) do
      begin
        if (rdgParameters.Cells[Ord(pcPackage), ARow] =
          ParamRecords[ParamIndex].Package)
          and (rdgParameters.Cells[Ord(pcType), ARow] =
          ParamRecords[ParamIndex].PType) then
        begin
          OK_Combination := True;
          break;
        end;
      end;
      if not OK_Combination then
      begin
        rdgParameters.Canvas.Brush.Color := clRed;
      end;
    end;
    if (TParamColumn(ACol) = pcTiedParameter)
      and (rdgParameters.ItemIndex[Ord(pcPestTransform), ARow] = Ord(ptTied))
      and  (rdgParameters.ItemIndex[Ord(pcTiedParameter), ARow] < 0) then
    begin
      rdgParameters.Canvas.Brush.Color := clRed;
    end;
    if (TParamColumn(ACol) = pcParamGroup)
      and (rdgParameters.ItemIndex[Ord(pcParamGroup), ARow] < 0) then
    begin
      rdgParameters.Canvas.Brush.Color := clRed;
    end;

    if (TParamColumn(ACol) = pcLowerBound) then
    begin
      if rdgParameters.RealValueDefault[ACol, ARow, 0]
        > rdgParameters.RealValueDefault[Ord(pcValue), ARow, 0] then
      begin
        rdgParameters.Canvas.Brush.Color := clRed;
      end;
    end;

    if (TParamColumn(ACol) = pcUpperBound) then
    begin
      if rdgParameters.RealValueDefault[ACol, ARow, 0]
        < rdgParameters.RealValueDefault[Ord(pcValue), ARow, 0] then
      begin
        rdgParameters.Canvas.Brush.Color := clRed;
      end;
    end;

    if (TParamColumn(ACol) in [pcChangeLimitation, pcLowerBound, pcUpperBound]) then
    begin
      if rdgParameters.ItemIndex[Ord(pcChangeLimitation), ARow] = Ord(pclFactor) then
      begin
        LowerBound := rdgParameters.RealValueDefault[Ord(pcLowerBound), ARow, 0];
        UpperBound := rdgParameters.RealValueDefault[Ord(pcUpperBound), ARow, 0];
        if (LowerBound = 0) or (UpperBound = 0) then
        begin
          rdgParameters.Canvas.Brush.Color := clRed;
        end
        else if (LowerBound < 0) and (UpperBound > 0) then
        begin
          rdgParameters.Canvas.Brush.Color := clRed;
        end;
      end;
    end;

    if TParamColumn(ACol) = pcChangeLimitation then
    begin
      if (rdgParameters.ItemIndex[Ord(pcPestTransform), ARow] = Ord(ptLog)) and
        (rdgParameters.ItemIndex[Ord(pcChangeLimitation), ARow] <> Ord(pclFactor)) then
      begin
          rdgParameters.Canvas.Brush.Color := clRed;
      end;
    end;
  end;
end;

procedure TfrmManageParameters.rdgParametersMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ACol: Integer;
  ARow: Integer;
  ParCol: TParamColumn;
  Index: Integer;
  CM: TCompareMethod;
begin
  inherited;
  rdgParameters.MouseToCell(X, Y, ACol, ARow);
  if (ARow = 0) and (ACol >= 0) and (ACol < rdgParameters.ColCount) then
  begin
    TGridCrack(rdgParameters).HideEditor;
    ParCol := TParamColumn(ACol);
    for Index := 0 to SortOrder.Count - 1 do
    begin
      CM := SortOrder[Index];
      if CM.Method = ParCol then
      begin
        SortOrder.Extract(CM);
        SortOrder.Insert(0, CM);
        UpdateParameterTable;
        break;
      end;
    end;
  end;
end;

procedure TfrmManageParameters.rdgParametersSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if (ARow > 0) and (ACol >= 0) and (ACol < rdgParameters.ColCount) then
  begin
    case TParamColumn(ACol) of
      pcName: ; // do nothing
      pcType:
        begin
          CanSelect := rdgParameters.Cells[Ord(pcName), ARow] <> '';
        end;
      pcValue, pcPestTransform, pcChangeLimitation, pcLowerBound,
        pcUpperBound, pcParamGroup, pcScaled, pcOffset:
        begin
          CanSelect := rdgParameters.Objects[Ord(pcName), ARow] <> nil
        end;
      pcPackage: CanSelect := False;
      pcMult, pcZone, pcPilotPoints:
        begin
          CanSelect := rdgParameters.UseSpecialFormat[ACol, ARow];
        end;
     pcAbsolute:
       begin
         CanSelect := rdgParameters.ItemIndex[Ord(pcChangeLimitation), ARow]
           = Ord(pclAbsolute);
       end;
      pcTiedParameter:
        begin
          CanSelect := (rdgParameters.Cells[Ord(pcName), ARow] <> '')
            and (rdgParameters.ItemIndex[Ord(pcPestTransform), ARow] = Ord(ptTied));
        end
      else Assert(False);
    end;
  end;
end;

procedure TfrmManageParameters.rdgParametersSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  PCol: TParamColumn;
  ParamIndex: TParameterType;
  AParam: TModflowParameter;
  NewPackage: string;
  NewName: string;
  ItemIndex: Integer;
  PriorName: string;
  RowIndex: Integer;
  PickList: TStrings;
  ATiedName: string;
  RowParam: TModflowParameter;
begin
  inherited;
  if FDeletingParam then
    Exit;
  seNumberOfParameters.AsInteger := rdgParameters.RowCount -1;
  if (ARow > 0) and (ACol >= 0) and (ACol < rdgParameters.ColCount) then
  begin
    PriorName := '';
    PCol := TParamColumn(ACol);
    case PCol of
      pcName:
        begin
          CreateOrUpdateParameter(ARow);
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            PriorName := AParam.ParameterName;
            NewName := Trim(string(AnsiString(
              rdgParameters.Cells[Ord(pcName), ARow])));
            if AParam.ParameterType = ptSFR then
            begin
              FSfrParamInstances.UpdateParamName(AParam.ParameterName, NewName);
            end;
            AParam.ParameterName := NewName;
            if AParam.ParameterName <>
              rdgParameters.Cells[Ord(pcName), ARow] then
            begin
              rdgParameters.Cells[Ord(pcName), ARow] := AParam.ParameterName;
            end;
          end;
        end;
      pcPackage, pcType:
      begin
        if rdgParameters.Cells[Ord(pcName), ARow] <> '' then
        begin
          if PCol = pcType then
          begin
            for ParamIndex := Low(TParameterType) to High(TParameterType) do
            begin
              if ParamValid(ParamIndex)
                and (rdgParameters.Cells[Ord(pcType), ARow] =
                ParamRecords[ParamIndex].PType) then
              begin
                NewPackage := ParamRecords[ParamIndex].Package;
                rdgParameters.Cells[Ord(pcPackage), ARow] := NewPackage;
                break;
              end;
            end;
          end;
          CreateOrUpdateParameter(ARow);
        end;
      end;
      pcValue:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.Value := StrToFloatDef(rdgParameters.Cells[
              Ord(pcValue), ARow], 0);
          end;
        end;
      pcMult, pcZone, pcPilotPoints: ; // do nothing
      pcPestTransform:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            ItemIndex := rdgParameters.ItemIndex[Ord(pcPestTransform), ARow];
            if ItemIndex >= 0 then
            begin
              AParam.Transform := TPestTransform(ItemIndex);
            end;
            if AParam.Transform <> ptTied then
            begin
              rdgParameters.Cells[Ord(pcTiedParameter), ARow] := '';
              rdgParameters.Objects[Ord(pcTiedParameter), ARow] := nil;
              AParam.TiedParameterName := '';
            end;
            if AParam.Transform = ptLog then
            begin
              AParam.ChangeLimitation := pclFactor;
              rdgParameters.ItemIndex[Ord(pcChangeLimitation), ARow] :=
                Ord(pclFactor);
            end;
          end;
        end;
      pcChangeLimitation:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            ItemIndex := rdgParameters.ItemIndex[Ord(pcChangeLimitation), ARow];
            if ItemIndex >= 0 then
            begin
              AParam.ChangeLimitation := TPestChangeLimitation(ItemIndex);
            end;
          end;
        end;
      pcLowerBound:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.LowerBound := StrToFloatDef(rdgParameters.Cells[
              Ord(pcLowerBound), ARow], 0);
          end;
        end;
      pcUpperBound:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.UpperBound := StrToFloatDef(rdgParameters.Cells[
              Ord(pcUpperBound), ARow], 0);
          end;
        end;
      pcParamGroup:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.ParameterGroup := rdgParameters.Cells[
              Ord(pcParamGroup), ARow];
            ItemIndex := rdgParameters.ItemIndex[
              Ord(pcParamGroup), ARow];
            if ItemIndex >= 0 then
            begin
              rdgParameters.Objects[Ord(pcParamGroup), ARow]
                := rdgParameters.Columns[Ord(pcParamGroup)].
                PickList.Objects[ItemIndex];
            end;
          end;
        end;
      pcScaled:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.Scale := StrToFloatDef(rdgParameters.Cells[
              Ord(pcScaled), ARow], 1);
          end;
        end;
      pcOffset:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.Offset := StrToFloatDef(rdgParameters.Cells[
              Ord(pcOffset), ARow], 1);
          end;
        end;
      pcAbsolute:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.AbsoluteN := StrToFloatDef(rdgParameters.Cells[
              Ord(pcAbsolute), ARow], 1);
          end;
        end;
      pcTiedParameter:
        begin
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            AParam.TiedParameterName := rdgParameters.Cells[
              Ord(pcTiedParameter), ARow]
          end;
        end;
      else
        Assert(False);
    end;
    if (PCol in [pcName, pcType, pcPestTransform])
      and frmGoPhast.PhastModel.PestUsed then
    begin
      UpdateListOfUntiedParamNames;
      if PriorName <> '' then
      begin
        AParam := rdgParameters.Objects[Ord(pcName), ARow]
          as TModflowParameter;
      end
      else
      begin
        AParam := nil;
      end;
      PickList := rdgParameters.Columns[Ord(pcTiedParameter)].PickList;
      for RowIndex := 1 to rdgParameters.RowCount - 1 do
      begin
        ATiedName := rdgParameters.Cells[Ord(pcTiedParameter), RowIndex];
        RowParam := rdgParameters.Objects[Ord(pcName), RowIndex]
          as TModflowParameter;
        if (ATiedName <> '') and (ATiedName = PriorName) and (AParam <> nil) then
        begin
          ATiedName := AParam.ParameterName;
          rdgParameters.Cells[Ord(pcTiedParameter), RowIndex] := ATiedName;
          if RowParam <> nil then
          begin
            RowParam.TiedParameterName := ATiedName;
          end;
        end;
        if PickList.IndexOf(ATiedName) < 0 then
        begin
          rdgParameters.Cells[Ord(pcTiedParameter), RowIndex] := '';
          if (RowParam <> nil) then
          begin
            RowParam.TiedParameterName := '';
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmManageParameters.rdgParametersStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  PCol: TParamColumn;
  AParam : TModflowParameter;
  SteadyParam : TModflowSteadyParameter;
begin
  inherited;
  if (ARow > 0) and (ACol >= 0) and (ACol < rdgParameters.ColCount) then
  begin
    if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
    begin
      AParam := rdgParameters.Objects[Ord(pcName), ARow]
        as TModflowParameter;
      if AParam is TModflowSteadyParameter then
      begin
        SteadyParam := TModflowSteadyParameter(AParam);
        PCol := TParamColumn(ACol);
        case PCol of
          pcMult:
            begin
              SteadyParam.UseMultiplier :=
                rdgParameters.Checked[Ord(pcMult), ARow];
            end;
          pcZone:
            begin
              SteadyParam.UseZone := rdgParameters.Checked[Ord(pcZone), ARow];
            end;
          pcPilotPoints:
            begin
              SteadyParam.UsePilotPoints := rdgParameters.Checked[Ord(pcPilotPoints), ARow];
            end;
          else Assert(False);
        end;
      end;
    end;
  end;
end;

procedure TfrmManageParameters.seNumberOfParametersChange(Sender: TObject);
var
  FirstNewRow: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewRowCount: Integer;
  PhastModel: TPhastModel;
begin
  inherited;
  PhastModel := frmGoPhast.PhastModel;
  if seNumberOfParameters.AsInteger > rdgParameters.RowCount -1 then
  begin
    FirstNewRow := rdgParameters.RowCount;
    rdgParameters.RowCount := Max(seNumberOfParameters.AsInteger + 1, 2);
    for RowIndex := FirstNewRow to rdgParameters.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgParameters.ColCount - 1 do
      begin
        rdgParameters.Cells[ColIndex,RowIndex] := '';
      end;
      rdgParameters.UseSpecialFormat[Ord(pcMult), RowIndex] := False;
      rdgParameters.UseSpecialFormat[Ord(pcZone), RowIndex] := False;
      if PhastModel.PestUsed then
      begin
        rdgParameters.UseSpecialFormat[Ord(pcPilotPoints), RowIndex] := False;
      end;
    end;
  end
  else if seNumberOfParameters.AsInteger < rdgParameters.RowCount -1 then
  begin
    for RowIndex := rdgParameters.RowCount -1 downto
      seNumberOfParameters.AsInteger+1 do
    begin
      DeleteAParam(RowIndex);
    end;
    NewRowCount := seNumberOfParameters.AsInteger+1;
    if NewRowCount = 1 then
    begin
      NewRowCount := 2
    end;
    rdgParameters.RowCount := NewRowCount;
  end;
end;

procedure TfrmManageParameters.SetData;
var
  Undo: TUndoChangeParameters;
begin
  Undo := TUndoChangeParameters.Create(FSteadyParameters,
    FTransientListParameters, FHufParameters, FSfrParamInstances, FParamGroups);
  frmGoPhast.UndoStack.Submit(Undo);
end;

procedure TfrmManageParameters.AssignTiedParam(TiedParamNode: TTreeNode);
var
  AParam: TModflowParameter;
  ParentParameter: TModflowParameter;
  Row: Integer;
begin
  AParam := TiedParamNode.Data;
  Row := rdgParameters.Cols[Ord(pcName)].IndexOfObject(AParam);
  if Row >= 1 then
  begin
    if TiedParamNode.Parent <> nil then
    begin
      ParentParameter := TiedParamNode.Parent.Data;
      rdgParameters.ItemIndex[Ord(pcPestTransform), Row] := Ord(ptTied);
      rdgParameters.Cells[Ord(pcTiedParameter), Row] := ParentParameter.ParameterName;
      rdgParameters.Objects[Ord(pcTiedParameter), Row] := ParentParameter;
      AParam.Transform := ptTied;
      AParam.TiedParameterName := ParentParameter.ParameterName;
    end
    else
    begin
      rdgParameters.ItemIndex[Ord(pcPestTransform), Row] := Ord(ptNoTransform);
      rdgParameters.Cells[Ord(pcTiedParameter), Row] := '';
      rdgParameters.Objects[Ord(pcTiedParameter), Row] := nil;
      AParam.Transform := ptNoTransform;
      AParam.TiedParameterName := '';
    end;
  end;
end;

procedure TfrmManageParameters.AssignGroupGridRow(AGroup: TPestParamGroup; ARow: Integer);
var
  Grid: TRbwDataGrid4;
begin
  Grid := frameParameterGroups.Grid;
  Grid.Objects[Ord(pgcName), ARow] := AGroup;
  Grid.Cells[Ord(pgcName), ARow] := AGroup.ParamGroupName;
  Grid.ItemIndex[Ord(pgcIncType), ARow] := Ord(AGroup.IncrementType);
  Grid.RealValue[Ord(pgcIncrement), ARow] := AGroup.ParamIncrement;
  Grid.RealValue[Ord(pgcMinIncrement), ARow] := AGroup.MinParamIncrement;
  Grid.ItemIndex[Ord(pgcForceCentral), ARow] := Ord(AGroup.ForceCentral);
  Grid.RealValue[Ord(pgcParamIncrementMultiplier), ARow] := AGroup.ParamIncrementMultiplier;
  Grid.ItemIndex[Ord(pgcDM3), ARow] := Ord(AGroup.DM3);
  Grid.ItemIndex[Ord(pgcDM5), ARow] := Ord(AGroup.DM5);
  Grid.Checked[Ord(pgcDM5), ARow] := AGroup.UseSplitSlopeAnalysis;
  Grid.RealValue[Ord(pgcSplitThreshold), ARow] := AGroup.SplitThreshold;
  Grid.RealValue[Ord(pgcSplitDifference), ARow] := AGroup.RelSlopeDif;
  Grid.ItemIndex[Ord(pgcSplitAction), ARow] := Ord(AGroup.SplitAction);
end;

procedure TfrmManageParameters.tvTiedParametersDragDrop(Sender, Source: TObject;
  X, Y: Integer);
var
  AnItem: TTreeNode;
  AttachMode: TNodeAttachMode;
  HT: THitTests;
  index: Integer;
  MovedNode: TTreeNode;
  NodeList: TList;
begin
  if FDragging then
  begin
    Exit;
  end;
  FDragging := True;
  try
    if Sender <> Source then Exit;
    if tvTiedParameters.SelectionCount < 1 then Exit;
    HT := tvTiedParameters.GetHitTestInfoAt(X, Y);
    AnItem := tvTiedParameters.GetNodeAt(X, Y);
//    if (AnItem <> nil)  then
    begin
      if (HT - [htOnItem, htOnIcon, htOnRight, htNowhere] <> HT) then
      begin
        tvTiedParameters.Items.BeginUpdate;
        try
          NodeList:= TList.Create;
          try
            for index := 0 to tvTiedParameters.SelectionCount - 1 do
            begin
              MovedNode := tvTiedParameters.Selections[index];

              if (MovedNode <> AnItem) then
              begin
                NodeList.Add(MovedNode);
              end;
            end;

            if AnItem = nil then
            begin
              AttachMode := naAddChild;
            end
            else if AnItem.Parent <> nil then
            begin
              AttachMode := naInsert;
            end
            else
            begin
              AttachMode := naAddChild;
            end;

            for index := 0 to NodeList.Count - 1 do
            begin
              MovedNode := NodeList[index];
              MovedNode.Selected := False;
            end;

            for index := 0 to NodeList.Count - 1 do
            begin
              MovedNode := NodeList[index];
              MovedNode.MoveTo(AnItem, AttachMode);
              AssignTiedParam(MovedNode);
            end;

            tvTiedParameters.Select(NodeList);

          finally
            NodeList.Free;
          end;
        finally
          tvTiedParameters.Items.EndUpdate;
        end;
      end;
    end;
  finally
    FDragging := False;
  end;
end;

procedure TfrmManageParameters.tvTiedParametersDragOver(Sender, Source: TObject;
  X, Y: Integer; State: TDragState; var Accept: Boolean);
var
  Src, Dst: TTreeNode;
begin
  Src := tvTiedParameters.Selected;
  Dst := tvTiedParameters.GetNodeAt(X,Y);
  Accept := (Src<>Dst);
end;

{ TUndoChangeParameters }

constructor TUndoChangeParameters.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var NewHufModflowParameters: THufModflowParameters;
  var NewSfrParamInstances: TSfrParamInstances;
  var NewParamGroups: TPestParamGroups);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  ScreenObjectClass: TScreenObjectClass;
begin
  inherited;
  FExistingScreenObjects := TScreenObjectEditCollection.Create;
  FExistingScreenObjects.OwnScreenObject := False;

  FOldProperties := TScreenObjectEditCollection.Create;
  FOldProperties.OwnScreenObject := True;

  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    Item := FExistingScreenObjects.Add;
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    Item.ScreenObject := AScreenObject;

    Item := FOldProperties.Add as TScreenObjectEditItem;
    ScreenObjectClass := TScreenObjectClass(AScreenObject.ClassType);
    Item.ScreenObject := ScreenObjectClass.Create(nil);
    Item.ScreenObject.Assign(AScreenObject);
  end;

end;

destructor TUndoChangeParameters.Destroy;
begin
  FOldProperties.Free;
  FExistingScreenObjects.Free;
  inherited;
end;

procedure TUndoChangeParameters.DoCommand;
begin
  inherited;
  // Assigning the old properties undos assigning the parameter names
//  FExistingScreenObjects.Assign(FOldProperties);
  frmGoPhast.PhastModel.ClearPestParmDictionary;
  if (frmShowHideObjects <> nil) then
  begin
    frmShowHideObjects.UpdateScreenObjects;
  end;
end;

procedure TUndoChangeParameters.Undo;
begin
  inherited;
  FExistingScreenObjects.Assign(FOldProperties);
  frmGoPhast.PhastModel.ClearPestParmDictionary;
  if (frmShowHideObjects <> nil) then
  begin
    frmShowHideObjects.UpdateScreenObjects;
  end;
end;

procedure InitializeSortOrder;
var
  Index: TParamColumn;
  CM: TCompareMethod;
begin
  SortOrder.Free;
  SortOrder := TObjectList.Create;
  for Index := Low(TParamColumn) to High(TParamColumn) do
  begin
    CM := TCompareMethod.Create;
    CM.Method := Index;
    SortOrder.Add(CM)
  end;
  CM := SortOrder[Ord(pcName)];
  SortOrder.Extract(CM);
  SortOrder.Insert(2, CM);
end;

initialization
  InitializeSortOrder;

finalization
  SortOrder.Free;

end.
