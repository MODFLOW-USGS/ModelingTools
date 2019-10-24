unit Mt3dUztWriterUnit;

interface

uses System.UITypes, System.Classes,
  Windows, CustomModflowWriterUnit, ModflowBoundaryUnit, ModflowPackageSelectionUnit,
  ScreenObjectUnit, ModflowCellUnit, OrderedCollectionUnit,
  System.Generics.Collections, PhastModelUnit, Vcl.Forms,
  ModflowBoundaryDisplayUnit, Vcl.Dialogs;

type
  TEvaluatePhase = (epRech, epSatEt, epUnsatEt);

  TMt3dUztWriter = class(TCustomTransientArrayWriter)
  private
    function GetMissingBoundaryErrorMessage: string;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteStressPeriods; reintroduce;
  protected
    FEvaluatePhase: TEvaluatePhase;
    FRechConc: TObjectList<TValueCellList>;
    FSatEtConc: TObjectList<TValueCellList>;
    FUnsatEtConc: TObjectList<TValueCellList>;
    function CellType: TValueCellType; override;
    function GetBoundary(ScreenObject: TScreenObject): TModflowBoundary;
      override;
    function Package: TModflowPackageSelection; override;
    function ParameterType: TParameterType; override;
    procedure Evaluate; override;
    class function Extension: string; override;
    function Prefix: string; override;
    procedure ShowNoBoundaryError(const NoDefinedErrorRoot: string); override;
    procedure RemoveNoDefinedError(var NoDefinedErrorRoot: string); override;
  public
    Constructor Create(Model: TCustomModel; EvaluationType: TEvaluationType); override;
    // @name destroys the current instance of @classname.
    Destructor Destroy; override;
    procedure WriteFile(const AFileName: string);
    procedure UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
  end;

implementation

uses
  Mt3dmsChemUnit, frmErrorsAndWarningsUnit, System.SysUtils, ModflowUnitNumbers,
  frmProgressUnit, GoPhastTypes, DataSetUnit, RbwParser;

resourcestring
  StrTheUztPackageHasB = 'The %0:s package has been activated but no %1:s ' +
  'for it has been defined.';

{ TMt3dUztWriter }

function TMt3dUztWriter.CellType: TValueCellType;
begin
  result := TMt3dmsConc_Cell;
end;

constructor TMt3dUztWriter.Create(Model: TCustomModel;
  EvaluationType: TEvaluationType);
begin
  inherited;
  FRechConc := TObjectList<TValueCellList>.Create;
  FSatEtConc := TObjectList<TValueCellList>.Create;
  FUnsatEtConc := TObjectList<TValueCellList>.Create;
end;

destructor TMt3dUztWriter.Destroy;
begin
  FUnsatEtConc.Free;
  FSatEtConc.Free;
  FRechConc.Free;
  inherited;
end;

procedure TMt3dUztWriter.Evaluate;
var
  AList: TValueCellList;
  Index: Integer;
  ErrorRoot: string;
begin
  RemoveNoDefinedError(ErrorRoot);

  FEvaluatePhase := epRech;
  inherited Evaluate;
  FRechConc.Capacity := Values.Count;
  for Index := 0 to Values.Count - 1 do
  begin
    AList := Values[index];
    FRechConc.Add(AList);
  end;
  OwnsValueContents := False;
  Values.Clear;
  OwnsValueContents := True;

  if Model.ModflowPackages.UzfPackage.SimulateET then
  begin
    FEvaluatePhase := epSatEt;
    inherited Evaluate;
    FSatEtConc.Capacity := Values.Count;
    for Index := 0 to Values.Count - 1 do
    begin
      AList := Values[index];
      FSatEtConc.Add(AList);
    end;
    OwnsValueContents := False;
    Values.Clear;
    OwnsValueContents := True;

    FEvaluatePhase := epUnsatEt;
    inherited Evaluate;
    FUnsatEtConc.Capacity := Values.Count;
    for Index := 0 to Values.Count - 1 do
    begin
      AList := Values[index];
      FUnsatEtConc.Add(AList);
    end;
    OwnsValueContents := False;
    Values.Clear;
    OwnsValueContents := True;
  end;

end;

class function TMt3dUztWriter.Extension: string;
begin
  result := '.uzt';
end;

function TMt3dUztWriter.GetBoundary(
  ScreenObject: TScreenObject): TModflowBoundary;
begin
  Result := nil;
  case FEvaluatePhase of
    epRech: result := ScreenObject.Mt3dUzfRechConc;
    epSatEt: result := ScreenObject.Mt3dUztSatEtConcBoundary;
    epUnsatEt: result := ScreenObject.Mt3dUztUnsatEtConcBoundary;
    else Assert(False);
  end;
end;

function TMt3dUztWriter.Package: TModflowPackageSelection;
begin
  result := Model.ModflowPackages.Mt3dUnsatTransport;
end;

function TMt3dUztWriter.ParameterType: TParameterType;
begin
  result := ptUndefined;
end;

function TMt3dUztWriter.Prefix: string;
begin
  Result := '';
end;

procedure TMt3dUztWriter.RemoveNoDefinedError(var NoDefinedErrorRoot: string);
begin
  NoDefinedErrorRoot := Format(StrNoDefinedBoundarie,
    [Package.PackageIdentifier]);
  if FEvaluatePhase = epRech then
  begin
    frmErrorsAndWarnings.RemoveWarningGroup(Model, NoDefinedErrorRoot);
  end;
end;

procedure TMt3dUztWriter.ShowNoBoundaryError(const NoDefinedErrorRoot: string);
var
  ErrorMessage: string;
begin
  ErrorMessage := GetMissingBoundaryErrorMessage;
  frmErrorsAndWarnings.AddWarning(Model, NoDefinedErrorRoot, ErrorMessage);
end;

procedure TMt3dUztWriter.UpdateDisplay(TimeLists: TModflowBoundListOfTimeLists);
var
  RechConc: TModflowBoundaryDisplayTimeList;
  SatEtConc: TModflowBoundaryDisplayTimeList;
  UnsatEtConc: TModflowBoundaryDisplayTimeList;
  DataArray: TModflowBoundaryDisplayDataArray;
  CellList: TValueCellList;
  TimeIndex: Integer;
begin
  if not Package.IsSelected then
  begin
    UpdateNotUsedDisplay(TimeLists);
    Exit;
  end;
  Evaluate;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  if (Values.Count = 0)
    and (FRechConc.Count > 0)
    and (FSatEtConc.Count > 0)
    and (FUnsatEtConc.Count > 0)
    then
  begin
    SetTimeListsUpToDate(TimeLists);
    Exit;
  end;

  try
    frmErrorsAndWarnings.BeginUpdate;
    try
      RechConc := TimeLists[0];
      if Model.ModflowPackages.UzfPackage.SimulateET then
      begin
        SatEtConc := TimeLists[1];
        UnsatEtConc := TimeLists[2];
      end
      else
      begin
        SatEtConc := nil;
        UnsatEtConc := nil;
      end;

      for TimeIndex := 0 to FRechConc.Count - 1 do
      begin
        DataArray := RechConc[TimeIndex]
          as TModflowBoundaryDisplayDataArray;
        CellList := FRechConc[TimeIndex];
        AssignTransient2DArray(DataArray, 0, CellList, 0,
          rdtDouble, umAssign);
        Model.AdjustDataArray(DataArray);
        CellList.Cache;
      end;

      if SatEtConc <> nil then
      begin
        for TimeIndex := 0 to FSatEtConc.Count - 1 do
        begin
          DataArray := SatEtConc[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          CellList := FSatEtConc[TimeIndex];
          AssignTransient2DArray(DataArray, 0, CellList, 0,
            rdtDouble, umAssign);
          Model.AdjustDataArray(DataArray);
          CellList.Cache;
        end;
      end;

      if UnsatEtConc <> nil then
      begin
        for TimeIndex := 0 to FUnsatEtConc.Count - 1 do
        begin
          DataArray := UnsatEtConc[TimeIndex]
            as TModflowBoundaryDisplayDataArray;
          CellList := FUnsatEtConc[TimeIndex];
          AssignTransient2DArray(DataArray, 0, CellList, 0,
            rdtDouble, umAssign);
          Model.AdjustDataArray(DataArray);
          CellList.Cache;
        end;
      end;
      SetTimeListsUpToDate(TimeLists);
    finally
      frmErrorsAndWarnings.EndUpdate;
    end;
  except on E: EInvalidTime do
    begin
      Beep;
      MessageDlg(E.Message, mtError, [mbOK], 0);
    end;
  end;

//    FRechConc: TObjectList<TValueCellList>;
//    FSatEtConc: TObjectList<TValueCellList>;
//    FUnsatEtConc: TObjectList<TValueCellList>;

end;

procedure TMt3dUztWriter.WriteDataSet2;
var
  ICBCUZ, IET: Integer;
begin
  ICBCUZ := 0;
  IET := Ord(Model.ModflowPackages.UzfPackage.SimulateET);
  WriteInteger(ICBCUZ);
  WriteInteger(IET);
  WriteString(' # Record 2: ICBCUZ, IET');
  NewLine;
end;

procedure TMt3dUztWriter.WriteDataSet3;
var
  IUZFBND: TDataArray;
begin
  IUZFBND := Model.DataArrayManager.GetDataSetByName(StrUzfLayer);
  Assert(IUZFBND <> nil);
  WriteArray(IUZFBND, 0, 'Record 3: IUZFBND', StrNoValueAssigned, 'IUZFBND');
end;

procedure TMt3dUztWriter.WriteDataSet4;
//var
//  WC: TDataArray;
//  LayerIndex: Integer;
begin
  // WC is no longer included in the input
  // See lines 236-241 of btn1.f
  Exit;
//  WC := Model.DataArrayManager.GetDataSetByName(KInitialWaterContent);
//  Assert(WC <> nil);
//  for LayerIndex := 0 to WC.LayerCount - 1 do
//  begin
//    if Model.IsLayerSimulated(LayerIndex) then
//    begin
//      WriteArray(WC, LayerIndex, 'Record 4: WC', StrNoValueAssigned, 'WC');
//    end;
//  end;
end;

procedure TMt3dUztWriter.WriteDataSet5;
//var
//  SDH: TDataArray;
//  LayerIndex: Integer;
begin
  // SDH is no longer included in the input
  // See lines 236-241 of btn1.f
  Exit;
//  SDH := Model.DataArrayManager.GetDataSetByName(KSaturatedThickness);
//  Assert(SDH <> nil);
//  for LayerIndex := 0 to SDH.LayerCount - 1 do
//  begin
//    if Model.IsLayerSimulated(LayerIndex) then
//    begin
//      WriteArray(SDH, LayerIndex, 'Record 5: SDH', StrNoValueAssigned, 'SDH');
//    end;
//  end;
end;

procedure TMt3dUztWriter.WriteFile(const AFileName: string);
begin
  if not Package.IsSelected then
  begin
    Exit
  end;

  if Model.ModflowPackages.Mt3dBasic.Mt3dVersion <> mvUSGS then
  begin
    Exit;
  end;

  FNameOfFile := FileName(AFileName);
  WriteToMt3dMsNameFile(StrUzt, Mt3dUzt,
    FNameOfFile, foInput, Model);

  Evaluate;

  OpenFile(FNameOfFile);
  try

    frmProgressMM.AddMessage(StrWritingDataSet1);
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet2);
    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet3);
    // data set 3 always uses the MODFLOW-style input array readers.
    FArrayWritingFormat := awfMt3dms;
    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet4);
    // subsequent data sets use either the MODFLOW-style input array readers.
    // or the MT3D-style input array readers depending on whether or not
    // MODFLOWSTYLEARRAYS has been specified as a keyword in the BTN package.
    // At present, ModelMuse does not support writing the MODFLOWSTYLEARRAYS
    // keyword so the MT3D-style input array readers will be used.
    FArrayWritingFormat := awfMt3dms;
    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    frmProgressMM.AddMessage(StrWritingDataSet5);
    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteStressPeriods
  finally
    CloseFile;
  end;

end;

procedure TMt3dUztWriter.WriteStressPeriods;
const
  INCUZINF = 1;
  INCUZET = 1;
  INCGWET = 1;
var
  StressPeriodIndex: Integer;
  SpeciesCount: Integer;
  ValueList: TValueCellList;
  DefaultValue: double;
  SpeciesIndex: Integer;
  Dummy: TDataArray;
  DataType: TRbwDataType;
  Comment: string;
  DataTypeIndex: Integer;
  SimulateET: Boolean;
begin
  if FRechConc.Count > 0 then
  begin
    Assert(FRechConc.Count = Model.ModflowFullStressPeriods.Count);
  end;
  if FSatEtConc.Count > 0 then
  begin
    Assert(FSatEtConc.Count = Model.ModflowFullStressPeriods.Count);
  end;
  if FUnsatEtConc.Count > 0 then
  begin
    Assert(FUnsatEtConc.Count = Model.ModflowFullStressPeriods.Count);
  end;
  SpeciesCount := Model.NumberOfMt3dChemComponents;

  DefaultValue := 0;
  DataType := rdtDouble;

  SimulateET := Model.ModflowPackages.UzfPackage.SimulateET;
  for StressPeriodIndex := 0 to Model.ModflowFullStressPeriods.Count - 1 do
  begin
    WriteI10Integer(INCUZINF, 'INCUZINF');
    WriteString(' # record 6: INCUZINF. Stress period ');
    WriteInteger(StressPeriodIndex+1);
    NewLine;
    if FRechConc.Count > 0 then
    begin
      ValueList := FRechConc[StressPeriodIndex];
      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        DataTypeIndex := SpeciesIndex;
        Comment := 'record 7: CUZINF; Species ' + IntToStr(SpeciesIndex+1);
        WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
          ValueList, umAssign, False, Dummy, 'CUZINF');
      end;
    end
    else
    begin
      for SpeciesIndex := 0 to SpeciesCount - 1 do
      begin
        Comment := 'record 7: CUZINF; Species ' + IntToStr(SpeciesIndex+1);
        WriteConstantU2DINT(Comment, 0, matStructured,  'CUZINF');
      end;
    end;

    if SimulateET then
    begin
      WriteI10Integer(INCUZET, 'INCUZET');
      WriteString(' # record 8: INCUZET. Stress period ');
      WriteInteger(StressPeriodIndex+1);
      NewLine;
      if FUnsatEtConc.Count > 0 then
      begin
        ValueList := FUnsatEtConc[StressPeriodIndex];
        for SpeciesIndex := 0 to SpeciesCount - 1 do
        begin
          DataTypeIndex := SpeciesIndex;
          Comment := 'record 9: CUZET; Species ' + IntToStr(SpeciesIndex+1);
          WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
            ValueList, umAssign, False, Dummy, 'CUZET');
        end;
      end
      else
      begin
        for SpeciesIndex := 0 to SpeciesCount - 1 do
        begin
          Comment := 'record 9: CUZET; Species ' + IntToStr(SpeciesIndex+1);
          WriteConstantU2DINT(Comment, 0, matStructured,  'CUZET');
        end;
      end;

      WriteI10Integer(INCGWET, 'INCGWET');
      WriteString(' # record 10: INCGWET. Stress period ');
      WriteInteger(StressPeriodIndex+1);
      NewLine;
      if FSatEtConc.Count > 0 then
      begin
        ValueList := FSatEtConc[StressPeriodIndex];
        for SpeciesIndex := 0 to SpeciesCount - 1 do
        begin
          DataTypeIndex := SpeciesIndex;
          Comment := 'record 11: CGWET; Species ' + IntToStr(SpeciesIndex+1);
          WriteTransient2DArray(Comment, DataTypeIndex, DataType, DefaultValue,
            ValueList, umAssign, False, Dummy, 'CGWET');
        end;
      end
      else
      begin
        for SpeciesIndex := 0 to SpeciesCount - 1 do
        begin
          Comment := 'record 11: CGWET; Species ' + IntToStr(SpeciesIndex+1);
          WriteConstantU2DINT(Comment, 0, matStructured,  'CGWET');
        end;
      end;
    end;
  end;
end;

function TMt3dUztWriter.GetMissingBoundaryErrorMessage: string;
begin
  case FEvaluatePhase of
    epRech:
      result := 'Recharge concentration';
    epSatEt:
      result := 'Sat. ET concentration';
    epUnsatEt:
      result := 'Unsat. ET concentration';
  else
    Assert(False);
  end;
  Result := Format(StrTheUztPackageHasB, [Package.PackageIdentifier, result]);
end;

end.
