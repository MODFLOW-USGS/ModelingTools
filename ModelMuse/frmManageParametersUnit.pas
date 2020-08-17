unit frmManageParametersUnit;

interface

uses System.UITypes, System.Types,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, Grids, StdCtrls, RbwDataGrid4, Math,
  ModflowParameterUnit, OrderedCollectionUnit,
  ModflowTransientListParameterUnit, HufDefinition, Buttons, Mask, JvExMask,
  JvSpin, ExtCtrls, RequiredDataSetsUndoUnit, UndoItemsScreenObjects, 
  ModflowPackageSelectionUnit;

type
  TParamColumn = (pcName, pcPackage, pcType, pcValue, pcMult, pcZone);

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
  private
    FSteadyParameters: TModflowSteadyParameters;
    FHufParameters: THufModflowParameters;
    FTransientListParameters: TModflowTransientListParameters;
    FSfrParamInstances: TSfrParamInstances;
    FParamList: TList;
    FDeletingParam: Boolean;
    procedure GetData;
    procedure SetData;
    procedure UpdateParameterTable;
    function ParamValid(ParamType: TParameterType): boolean;
    procedure CreateParameter(ParamIndex: TParameterType; ARow: Integer);
    procedure UpdateParameter(ParamIndex: TParameterType;
      var AParam: TModflowParameter; ARow: Integer);
    procedure CreateOrUpdateParameter(ARow: Integer);
    procedure DeleteAParam(ARow: Integer);
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
      var NewSfrParamInstances: TSfrParamInstances);
    Destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

implementation

uses
  Contnrs, frmGoPhastUnit, PhastModelUnit, ModflowDiscretizationWriterUnit,
  ScreenObjectUnit, frmShowHideObjectsUnit, ModflowPackagesUnit, ReadPvalUnit,
  IntListUnit;

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

{$R *.dfm}
type
  TParamClassType = (pctUnknown, pctSteady, pctTransient, pctHUF);

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
     (Package: 'FMP'; PType: 'QMAX'; PClass: pctTransient)
     );

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
      else Assert(False);
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
begin
  inherited;
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
  finally
    Names.Free;
  end;
  SetData;
end;

procedure TfrmManageParameters.btnDeleteClick(Sender: TObject);
begin
  inherited;
  DeleteAParam(rdgParameters.SelectedRow);
  seNumberOfParameters.AsInteger := seNumberOfParameters.AsInteger -1;
end;

procedure TfrmManageParameters.FormCreate(Sender: TObject);
var
  PackageList: TStringList;
  Index: TParameterType;
  ParamTypeList: TStringList;
begin
  inherited;

  rdgParameters.BeginUpdate;
  try
    rdgParameters.Cells[Ord(pcName), 0] := StrName;
    rdgParameters.Cells[Ord(pcPackage), 0] := StrPackage;
    rdgParameters.Cells[Ord(pcType), 0] := StrType;
    rdgParameters.Cells[Ord(pcValue), 0] := StrValue;
    rdgParameters.Cells[Ord(pcMult), 0] := StrMultiplierArray;
    rdgParameters.Cells[Ord(pcZone), 0] := StrZoneArray;
  finally
    rdgParameters.EndUpdate;
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

  GetData;
end;

procedure TfrmManageParameters.FormDestroy(Sender: TObject);
begin
  inherited;
  FSfrParamInstances.Free;
  FTransientListParameters.Free;
  FHufParameters.Free;
  FSteadyParameters.Free;
  FParamList.Free;
end;

procedure TfrmManageParameters.GetData;
var
  Index: Integer;
  PhastModel: TPhastModel;
  AParam: TModflowParameter;
begin
  FParamList := TList.Create;
  PhastModel := frmGoPhast.PhastModel;
  FSteadyParameters.Assign(PhastModel.ModflowSteadyParameters);
  FHufParameters.Assign(PhastModel.HufParameters);
  FTransientListParameters.Assign(PhastModel.ModflowTransientParameters);
  FSfrParamInstances.Assign(PhastModel.ModflowPackages.
    SfrPackage.ParameterInstances);

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
end;

function TfrmManageParameters.ParamValid(ParamType: TParameterType): boolean;
var
  TransientModel: Boolean;
begin
  result := False;
  TransientModel := frmGoPhast.PhastModel.ModflowStressPeriods.TransientModel;

  case ParamType of
    ptUndefined: result := False;
    ptLPF_HK: result := frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected;
    ptLPF_HANI: result := frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected;
    ptLPF_VK: result := frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected;
    ptLPF_VANI: result := frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected;
    ptLPF_SS: result := (frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected) and TransientModel;
    ptLPF_SY: result := (frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected) and TransientModel;
    ptLPF_VKCB: result := frmGoPhast.PhastModel.LpfIsSelected or frmGoPhast.PhastModel.UpwIsSelected;
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
    ptHUF_HK: result := frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_HANI: result := frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_VK: result := frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_VANI: result := frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_SS: result := frmGoPhast.PhastModel.HufIsSelected and TransientModel;
    ptHUF_SY: result := frmGoPhast.PhastModel.HufIsSelected and TransientModel;
    ptHUF_SYTP: result := frmGoPhast.PhastModel.HufIsSelected and TransientModel;
    ptHUF_KDEP: result := frmGoPhast.PhastModel.HufIsSelected;
    ptHUF_LVDA: result := frmGoPhast.PhastModel.HufIsSelected;
    ptSTR: result := frmGoPhast.PhastModel.StrIsSelected;
    ptQMAX: result := frmGoPhast.PhastModel.FarmProcessIsSelected;
    else Assert(False);
  end;
end;

procedure TfrmManageParameters.CreateParameter(ParamIndex: TParameterType; ARow: Integer);
var
  AParam: TModflowParameter;
begin
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
        rdgParameters.SpecialFormat[Ord(pcMult), ARow] := rcf4Boolean;
        rdgParameters.SpecialFormat[Ord(pcZone), ARow] := rcf4Boolean;
      end;
    pctTransient:
      begin
        AParam := FTransientListParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := False;
      end;
    pctHUF:
      begin
        AParam := FHufParameters.Add as TModflowParameter;
        rdgParameters.UseSpecialFormat[Ord(pcMult), ARow] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ARow] := False;
      end;
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
end;

procedure TfrmManageParameters.UpdateParameter(ParamIndex: TParameterType;
  var AParam: TModflowParameter; ARow: Integer);
begin
  case ParamRecords[ParamIndex].PClass of
    pctUnknown:
      begin
        Assert(False);
      end;
    pctSteady:
      begin
        if AParam is TModflowSteadyParameter then
        begin
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParamList.Remove(AParam);
          AParam.Free;
          AParam := nil;
          rdgParameters.Objects[Ord(pcName), ARow] := nil;
        end;
      end;
    pctTransient:
      begin
        if AParam is TModflowTransientListParameter then
        begin
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParamList.Remove(AParam);
          AParam.Free;
          AParam := nil;
          rdgParameters.Objects[Ord(pcName), ARow] := nil;
        end;
      end;
    pctHUF:
      begin
        if AParam is THufParameter then
        begin
          AParam.ParameterType := ParamIndex;
        end
        else
        begin
          FParamList.Remove(AParam);
          AParam.Free;
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

procedure TfrmManageParameters.DeleteAParam(ARow: Integer);
var
  AParam: TModflowParameter;
  ColIndex: Integer;
begin
  FDeletingParam := True;
  try
    AParam := rdgParameters.Objects[Ord(pcName), ARow] as TModflowParameter;
    if AParam <> nil then
    begin
      FSfrParamInstances.DeleteInstancesOfParameter(AParam.ParameterName);
    end;
    AParam.Free;
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
      end;
    end;
  finally
    FDeletingParam := False;
  end;

end;

procedure TfrmManageParameters.UpdateParameterTable;
var
  SteadyParam: TModflowSteadyParameter;
  AModflowParam: TModflowParameter;
  ParamIndex: Integer;
begin
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
        if SteadyParam.ParameterType <> ptHFB then
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
      end
      else
      begin
        rdgParameters.UseSpecialFormat[Ord(pcMult), ParamIndex + 1] := False;
        rdgParameters.UseSpecialFormat[Ord(pcZone), ParamIndex + 1] := False;
      end;
    end;
  finally
    rdgParameters.EndUpdate;
  end;
end;

type TGridCrack = class(TRbwDataGrid4);

procedure TfrmManageParameters.rdgParametersBeforeDrawCell(Sender: TObject;
  ACol, ARow: Integer);
var
  Names: TStringList;
  ParamIndex: TParameterType;
  OK_Combination: Boolean;
begin
  inherited;
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
        end;
      finally
        Names.Free;
      end;
    end;
    if TParamColumn(ACol) in [pcPackage, pcType] then
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
      pcName, pcType, pcValue: ; // do nothing
      pcPackage: CanSelect := False;
      pcMult, pcZone: CanSelect := rdgParameters.UseSpecialFormat[ACol, ARow];
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
begin
  inherited;
  if FDeletingParam then
    Exit;
  if (ARow > 0) and (ACol >= 0) and (ACol < rdgParameters.ColCount) then
  begin
    PCol := TParamColumn(ACol);
    case PCol of
      pcName:
        begin
          CreateOrUpdateParameter(ARow);
          if rdgParameters.Objects[Ord(pcName), ARow] <> nil then
          begin
            AParam := rdgParameters.Objects[Ord(pcName), ARow]
              as TModflowParameter;
            NewName := string(AnsiString(rdgParameters.Cells[Ord(pcName), ARow]));
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
      pcMult, pcZone: ; // do nothing
      else
        Assert(False);
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
            end
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
begin
  inherited;
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
    end;
  end
  else if seNumberOfParameters.AsInteger < rdgParameters.RowCount -1 then
  begin
    for RowIndex := rdgParameters.RowCount -1 downto
      seNumberOfParameters.AsInteger+1 do
    begin
      DeleteAParam(RowIndex);
//      rdgParameters.Objects[Ord(pcName), RowIndex].Free;
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
    FTransientListParameters, FHufParameters, FSfrParamInstances);
  frmGoPhast.UndoStack.Submit(Undo);
end;

{ TUndoChangeParameters }

constructor TUndoChangeParameters.Create(
  var NewSteadyParameters: TModflowSteadyParameters;
  var NewTransientParameters: TModflowTransientListParameters;
  var NewHufModflowParameters: THufModflowParameters;
  var NewSfrParamInstances: TSfrParamInstances);
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
  if (frmShowHideObjects <> nil) then
  begin
    frmShowHideObjects.UpdateScreenObjects;
  end;
end;

procedure TUndoChangeParameters.Undo;
begin
  inherited;
  FExistingScreenObjects.Assign(FOldProperties);
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
