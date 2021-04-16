unit ModflowLgr_WriterUnit;

interface

uses CustomModflowWriterUnit, Classes, PhastModelUnit;

type
  TLgrWriter = class(TCustomModflowWriter)
  private
//    FNameOfFile: string;
    FParentNameFile: string;
    FArchive: boolean;
    FParentOutputDirectory: string;
    procedure WriteDataSet0;
    procedure WriteDataSet1;
    procedure WriteDataSet2;
    procedure WriteDataSet3;
    procedure WriteDataSet4;
    procedure WriteDataSet5;
    procedure WriteDataSet6_to_15(ChildIndex: integer);
    procedure WriteBfhData(AModel: TCustomModel; AComment: string;
      AFileName: string);
    procedure WriteFileInternal;
  protected
    class function Extension: string; override;
  public
    procedure WriteFile(const AFileName: string);

  end;

implementation

uses
  Forms, frmProgressUnit, SysUtils, ModflowUnitNumbers, LayerStructureUnit,
  GoPhastTypes, ArchiveNodeInterface, FastGEO, AbstractGridUnit;

resourcestring
  StrWritingLGRControl = 'Writing LGR control file.';

{ TLgrWriter }

class function TLgrWriter.Extension: string;
begin
  result := '.lgr';
end;

procedure TLgrWriter.WriteDataSet0;
begin
  WriteCommentLine('LGR Control File for MODFLOW-LGR created on '
    + DateToStr(Now) + ' by ' + Model.ProgramName);
end;

procedure TLgrWriter.WriteDataSet1;
begin
  WriteString('LGR');
  NewLine;
end;

procedure TLgrWriter.WriteDataSet2;
var
  NGRIDS: Integer;
begin
  NGRIDS := (Model as TPhastModel).ChildModels.Count + 1;
  WriteInteger(NGRIDS);
  WriteString(' # NGRIDS');
  NewLine;
end;

procedure TLgrWriter.WriteDataSet3;
var
  ParentNAMEFILE: string;
begin
  ParentNAMEFILE := Model.FixFileName(ExtractFileName(FParentNameFile));
  WriteString(ParentNAMEFILE);
  WriteString(' # NAME FILE');
  NewLine;
end;

procedure TLgrWriter.WriteDataSet4;
begin
  WriteString('PARENTONLY');
  NewLine;
end;

procedure TLgrWriter.WriteDataSet5;
begin
  WriteBfhData(Model, ' # Data set 5 IUPBHSV, IUPBFSV', FParentNameFile);
end;

procedure TLgrWriter.WriteDataSet6_to_15(ChildIndex: integer);
var
  ChildModel: TChildModel;
  ChildNameFile: string;
  ISHFLG: Integer;
  IBFLG: Integer;
  MXLGRITER: Integer;
  IOUTLGR: Integer;
  RELAXH: Double;
  RELAXF: Double;
  HCLOSELGR: Double;
  FCLOSELGR: Double;
  NPLBEG: Integer;
  NPRBEG: Integer;
  NPCBEG: Integer;
  NPLEND: Integer;
  NPREND: Integer;
  NPCEND: Integer;
  NCPP: Integer;
  DisItem: TChildDiscretization;
  NCPPL: Integer;
  GroupIndex: Integer;
  PhastModel: TPhastModel;
  LayerGroup: TLayerGroup;
  LayerIndex: Integer;
  Discretization: TChildDiscretizationCollection;
  FullChildNameFile: string;
  ChildModelInputDirectory: string;
  ChildModelOutputDirectory: string;
  ChildGrid: TCustomModelGrid;
  ParentGrid: TCustomModelGrid;
  TopLeftPoint: TPoint2D;
  BottomRightPoint: TPoint2D;
  TopLeftCell: T2DTopCell;
  BottomRightCell: T2DTopCell;
begin
  ChildModel := (Model as TPhastModel).ChildModels[ChildIndex].ChildModel;

  // data set 6
  FullChildNameFile := ChildModel.Child_NameFile_Name(FParentNameFile);
  ChildNameFile := ExtractFileName(FullChildNameFile);

  ChildModelInputDirectory := ChangeFileExt(ChildNameFile, '');
  ChildModelInputDirectory := '..\..\model\model.' + ChildModelInputDirectory + '\';
  ChildModelOutputDirectory := ChangeFileExt(ChildNameFile, '');
  ChildModelOutputDirectory := '..\..\output\output.' + ChildModelOutputDirectory + '\';

  if FArchive then
  begin
    ChildNameFile := ChildModelInputDirectory + ChildNameFile
  end;

  WriteString(ChildNameFile);
  WriteString(' # Data set 6 NAME FILE');
  NewLine;

  // data set 7
  WriteString('CHILDONLY # Data set 7 GRIDSTATUS');
  NewLine;

//  if True then
//  begin
//
//  end;
  // data set 8
  ISHFLG := Ord(ChildModel.StartingHeadSource);
  WriteInteger(ISHFLG);

  if Model.ModelSelection in [msModflowLGR] then
  begin
    IBFLG := -(ChildIndex+2);
  end
  else
  begin
    IBFLG := (ChildIndex+2);
  end;
  WriteInteger(IBFLG);

  // need to decide how IUCBHSV and IUCBFSV
  // will be handled.
  WriteBfhData(ChildModel, ' # Data set 8 ISHFLG IBFLG IUCBHSV IUCBFSV',
    FullChildNameFile);

  // data set 9
  MXLGRITER := 0;
  case ChildModel.CouplingMethod of
    cmOneWay: MXLGRITER := 1;
    cmTwoWay: MXLGRITER := ChildModel.MaxIterations;
    else Assert(False);
  end;
  WriteInteger(MXLGRITER);

  IOUTLGR := 0;
  case ChildModel.LgrPrintChoice of
    lpcScreen: IOUTLGR := -1;
    lpcListing: IOUTLGR := 1;
    lpcNone: IOUTLGR := 0;
    else Assert(False);
  end;
  WriteInteger(IOUTLGR);

  WriteString(' # Data set 9 MXLGRITER IOUTLGR');
  NewLine;

  // data set 10
  RELAXH := ChildModel.HeadRelaxationFactor;
  WriteFloat(RELAXH);

  RELAXF := ChildModel.FluxRelaxationFactor;
  WriteFloat(RELAXF);

  WriteString(' # Data set 10 RELAXH RELAXF');
  NewLine;

  // data set 11
  HCLOSELGR:= ChildModel.HeadClosureCriterion;
  WriteFloat(HCLOSELGR);

  FCLOSELGR:= ChildModel.FluxClosureCriterion;
  WriteFloat(FCLOSELGR);

  WriteString(' # Data set 11 HCLOSELGR FCLOSELGR');
  NewLine;

  // data set 12
  NPLBEG := 1;
  WriteInteger(NPLBEG);

  if ChildModel.FirstCol < 0 then
  begin
    ChildGrid := ChildModel.Grid;
    ParentGrid := (Model as TPhastModel).Grid;
    TopLeftPoint := ChildGrid.TwoDElementCenter(0,0);
    BottomRightPoint := ChildGrid.TwoDElementCenter(ChildGrid.ColumnCount-1,ChildGrid.RowCount-1);
    TopLeftCell := ParentGrid.TopContainingCell(TopLeftPoint, eaBlocks);
    BottomRightCell := ParentGrid.TopContainingCell(BottomRightPoint, eaBlocks);
    NPCBEG := TopLeftCell.Col+1;
    NPRBEG := TopLeftCell.Row+1;
    NPCEND := BottomRightCell.Col+1;
    NPREND := BottomRightCell.Row+1;
  end
  else
  begin
    NPRBEG := ChildModel.FirstRow+1;
    NPCBEG := ChildModel.FirstCol+1;
    NPREND := ChildModel.LastRow+1;
    NPCEND := ChildModel.LastCol+1;
  end;

  WriteInteger(NPRBEG);
  WriteInteger(NPCBEG);

  WriteString(' # Data set 12 NPLBEG NPRBEG NPCBEG');
  NewLine;

  // data set 13
  NPLEND := ChildModel.Discretization.BottomModflowParentLayerNumber;
  WriteInteger(NPLEND);

  WriteInteger(NPREND);
  WriteInteger(NPCEND);

  WriteString(' # Data set 13 NPLEND NPREND NPCEND');
  NewLine;

  // data set 14
  NCPP := ChildModel.ChildCellsPerParentCell;
  WriteInteger(NCPP);
  WriteString(' # Data set 14 NCPP');
  NewLine;

  // data set 15
  PhastModel := Model as TPhastModel;
  Discretization := ChildModel.Discretization;
  for GroupIndex := 1 to PhastModel.LayerStructure.Count - 1 do
  begin
    LayerGroup := PhastModel.LayerStructure[GroupIndex];
    if LayerGroup.RunTimeSimulated then
    begin
      for LayerIndex := 0 to LayerGroup.LayerCount - 1 do
      begin
        DisItem := Discretization.
          GetAnItemByGroupAndLayer(LayerGroup, LayerIndex);
        NCPPL := DisItem.Discretization;
        WriteInteger(NCPPL);
        if (Discretization.BottomLayerGroup = LayerGroup)
          and (Discretization.BottomLayerInUnit = LayerIndex) then
        begin
          break;
        end;
      end;
    end;
    if (Discretization.BottomLayerGroup = LayerGroup) then
    begin
      break;
    end;
  end;
  WriteString(' # Data set 15 NCPPL');
  NewLine;

end;

procedure TLgrWriter.WriteFileInternal;
var
  PhastModel: TPhastModel;
  ChildIndex: Integer;
begin
  FInputFileName := FNameOfFile;
  OpenFile(FNameOfFile);
  try
    WriteDataSet0;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet1;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet2;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet3;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet4;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    WriteDataSet5;
    Application.ProcessMessages;
    if not frmProgressMM.ShouldContinue then
    begin
      Exit;
    end;

    PhastModel := Model as TPhastModel;
    for ChildIndex := 0 to PhastModel.ChildModels.Count - 1 do
    begin
      WriteDataSet6_to_15(ChildIndex);
    end;

  finally
    CloseFile;
  end;
end;

procedure TLgrWriter.WriteFile(const AFileName: string);
begin
  frmProgressMM.AddMessage(StrWritingLGRControl);

  FArchive := False;
  FParentNameFile := ChangeFileExt(AFileName, '.nam');
  FNameOfFile := FileName(AFileName);
  WriteFileInternal;

  Application.ProcessMessages;
  if not frmProgressMM.ShouldContinue then
  begin
    Exit;
  end;

  FArchive := True;

  FParentOutputDirectory := ExtractFileName(FNameOfFile);
  FParentOutputDirectory := ChangeFileExt(FParentOutputDirectory, '');
  FParentOutputDirectory := '..\..\output\' + FParentOutputDirectory + '\';

  FNameOfFile := FNameOfFile + ArchiveExt;
  FInputFileName := FNameOfFile;
  WriteFileInternal;
  Model.AddModelInputFile(FNameOfFile);
end;

procedure TLgrWriter.WriteBfhData(AModel: TCustomModel; AComment: string;
  AFileName: string);
var
  IUPBHSV: Integer;
  IUPBFSV: Integer;
  NameFile: string;
  HeadFile: string;
  FlowFile: string;
begin
  if (AModel is TChildModel)
    and (TChildModel(AModel).CouplingMethod = cmOneWay) then
  begin
    // MODFLOW - LGR does not write anything to these
    // files in child models unless two-way coupling is used.
    IUPBHSV := 0;
    IUPBFSV := 0;
  end
  else
  begin
    if AModel.SaveBfhBoundaryConditions then
    begin
      IUPBHSV := AModel.UnitNumbers.UnitNumber(BFH_Heads);
      IUPBFSV := AModel.UnitNumbers.UnitNumber(BFH_Fluxes);
    end
    else
    begin
      IUPBHSV := 0;
      IUPBFSV := 0;
    end;
  end;
  WriteInteger(IUPBHSV);
  WriteInteger(IUPBFSV);
  WriteString(AComment);
  NewLine;
  NameFile := AModel.FixFileName(AFileName);
  Assert(AModel.NameFileWriter <> nil);
  SetCurrentNameFileWriter(AModel.NameFileWriter as TNameFileWriter);
  if IUPBHSV > 0 then
  begin
    HeadFile := ChangeFileExt(NameFile, '.bfh_head');
    if not FArchive then
    begin
//      HeadFile := FParentOutputDirectory + ExtractFileName(HeadFile);
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, IUPBHSV, HeadFile, foOutput, Model);
      end;
    end;
  end;
  if IUPBFSV > 0 then
  begin
    FlowFile := ChangeFileExt(NameFile, '.bfh_flux');
    if not FArchive then
    begin
//      FlowFile := FParentOutputDirectory + ExtractFileName(FlowFile);
      if not WritingTemplate then
      begin
        WriteToNameFile(StrDATA, IUPBFSV, FlowFile, foOutput, Model);
      end;
    end;
  end;
end;

end.
