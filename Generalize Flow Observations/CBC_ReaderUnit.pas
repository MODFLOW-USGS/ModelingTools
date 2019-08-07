unit CBC_ReaderUnit;

interface

uses
   System.SysUtils, System.Classes, System.Generics.Collections,
   ReadModflowArrayUnit, ReadFlowObsInput;

type
  TFlowGroup = class(TObject)
    KSTP: Integer;
    KPER: Integer;
    PERTIM: TModflowDouble;
    TOTIM: TModflowDouble;
    Start: Int64;
  end;

  TFlowGroupList = class(TObjectList<TFlowGroup>)
    DESC: String;
  end;

  TFlowTypes = class(TObjectList<TFlowGroupList>)
  private
    FCachedIndex: integer;
  public
    function IndexOfType(AType: String): integer;
    constructor Create;
  end;

  TCBC_Reader = class(TObject)
  private
    FFlowTypes: TFlowTypes;
    F_CBCFile: TFileStream;
    FPrecision: TPrecision;
    procedure CreateTableOfContents;
  public
    constructor Create(const FileName: string; Precision: TPrecision);
    destructor Destroy; override;
    procedure FillCellList(const AType: string; StressPeriod, TimeStep: Integer;
      CellList: TStructuredCellList; var ErrorMessage: string);
  end;

implementation

{ TFlowTypes }

constructor TFlowTypes.Create;
begin
  FCachedIndex := -1;
end;

function TFlowTypes.IndexOfType(AType: String): integer;
var
  index: Integer;
begin
  if FCachedIndex >= 0 then
  begin
    if Items[FCachedIndex].DESC = AType then
    begin
      result := FCachedIndex;
      Exit;
    end;
  end;
  result := -1;
  for index := 0 to Count - 1 do
  begin
    if Items[index].DESC = AType then
    begin
      result := index;
      FCachedIndex := Result;
      Exit;
    end;
  end;
end;

{ TCBC_Reader }

constructor TCBC_Reader.Create(const FileName: string; Precision: TPrecision);
begin
  FFlowTypes := TFlowTypes.Create;
  F_CBCFile := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  FPrecision := Precision;
  CreateTableOfContents;
end;

destructor TCBC_Reader.Destroy;
begin
  F_CBCFile.Free;
  FFlowTypes.Free;
  inherited;
end;

procedure TCBC_Reader.FillCellList(const AType: string; StressPeriod,
  TimeStep: Integer; CellList: TStructuredCellList;
  var ErrorMessage: string);
var
  FlowIndex: Integer;
  Group: TFlowGroupList;
  TimeIndex: Integer;
  Item: TFlowGroup;
  KSTP: Integer;
  KPER: Integer;
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
  NLAY: Integer;
begin
  ErrorMessage := '';
  FlowIndex := FFlowTypes.IndexOfType(AType);
  if FlowIndex < 0 then
  begin
    CellList.Clear;
    ErrorMessage := 'Ínvalid flów type';
    Exit;
  end
  else
  begin
    Group := FFlowTypes[FlowIndex];
    for TimeIndex := 0 to Group.Count - 1 do
    begin
      Item := Group[TimeIndex];
      if (Item.KPER = StressPeriod) and (Item.KSTP = TimeStep) then
      begin
        F_CBCFile.Position := Item.Start;
        case FPrecision of
          pSingle:
            begin
              ReadModflowsinglePrecFluxList(F_CBCFile, KSTP, KPER, PERTIM, TOTIM,
                DESC, NCOL, NROW, NLAY, CellList, False, True);
              Exit;
            end;
          pDouble:
            begin
              ReadModflowDoublePrecFluxList(F_CBCFile, KSTP, KPER, PERTIM, TOTIM,
                DESC, NCOL, NROW, NLAY, CellList, False, True);
              Exit;
            end;
        else
          Assert(False);
        end;
      end;
    end;
    ErrorMessage := Format(
      'Data for Stress Period %0:d, Time Step %1:d not found in cell-by-cell flow file.',
      [StressPeriod, TimeStep]);
  end;
end;

procedure TCBC_Reader.CreateTableOfContents;
var
  FlowIndex: Integer;
  PERTIM: TModflowDouble;
  DESC: TModflowDesc;
  TOTIM: TModflowDouble;
  NROW: Integer;
  KSTP: Integer;
  NCOL: Integer;
  FlowType: string;
  FlowGroup: TFlowGroup;
  KPER: Integer;
  Position: Int64;
  CellList: TStructuredCellList;
  NLAY: Integer;
  FlowGroupList: TFlowGroupList;
begin
  CellList := TStructuredCellList.Create;
  try
    while F_CBCFile.Position < F_CBCFile.Size do
    begin
      Position := F_CBCFile.Position;
      case FPrecision of
        pSingle:
          begin
            ReadModflowsinglePrecFluxList(F_CBCFile, KSTP, KPER, PERTIM, TOTIM,
              DESC, NCOL, NROW, NLAY, CellList, False, False);
          end;
        pDouble:
          begin
            ReadModflowDoublePrecFluxList(F_CBCFile, KSTP, KPER, PERTIM, TOTIM,
              DESC, NCOL, NROW, NLAY, CellList, False, False);
          end;
      else
        Assert(False);
      end;
      FlowType := Trim(string(DESC));
      FlowIndex := FFlowTypes.IndexOfType(FlowType);
      if FlowIndex < 0 then
      begin
        FlowGroupList := TFlowGroupList.Create;
        FFlowTypes.Add(FlowGroupList);
        FlowGroupList.DESC := FlowType;
      end
      else
      begin
        FlowGroupList := FFlowTypes[FlowIndex];
      end;
      FlowGroup := TFlowGroup.Create;
      FlowGroupList.Add(FlowGroup);
      FlowGroup.KSTP := KSTP;
      FlowGroup.KPER := KPER;
      FlowGroup.PERTIM := PERTIM;
      FlowGroup.TOTIM := TOTIM;
      FlowGroup.Start := Position;
    end;
  finally
    CellList.Free;
  end;
end;

end.
