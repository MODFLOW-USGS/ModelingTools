unit SubsidenceObsExtractor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, ObExtractorTypes, Generics.Collections;

type
  TCellID = record
    Layer: Integer;
    Row: Integer;
    Column: Integer;
    Fraction: double;
  end;

  TCellID_List = specialize TList<TCellID>;

  { TSubsidenceObsValue }

  TSubsidenceObsValue = class(TCustomWeightedObsValue)
  private
    FObsType: string;
    // The first cell in the list is the cell containing the observation
    // location if it is dry, then the entire obseration is inactive.
    FCellIDs: TCellID_List;
    FBeforeTime: double;
    FAfterTime: double;
    FBeforeValues: array of double;
    FAfterValues: array of double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddCellID(ACellID: TCellID);
    property ObsType: string read FObsType write FObsType;
  end;

  TSubsidenceObsValueList = specialize TList<TSubsidenceObsValue>;

  { TSubsidenceObsExtractor }

  TSubsidenceObsExtractor = class(TCustomObsExtractor)
  private
    FSubObs: TSubsidenceObsValueList;
  protected
    F3DObsTypes: TStringList;
    procedure Initialize3DObsTypes; virtual;
  public
    Constructor Create;
    destructor Destroy; override;
    procedure ExtractSimulatedValues; override;
  end;

var
  SubsidenceTypes: TStringList;

implementation

uses
  ReadModflowArrayUnit;

const
  rsSUBSIDENCE = 'SUBSIDENCE';
  rsLAYERCOMPACT = 'LAYER COMPACTION';
  rsNDSYSCOMPACT = 'NDSYS COMPACTION';
  rsDSYSCOMPACTI = 'DSYS COMPACTION';
  rsZDISPLACEMEN = 'Z DISPLACEMENT';
  rsNDCRITICALHE = 'ND CRITICAL HEAD';
  rsDCRITICALHEA = 'D CRITICAL HEAD';

procedure InitializeSubsidenceTypes;
begin
  SubsidenceTypes := TStringList.Create;
  SubsidenceTypes.Add(rsSUBSIDENCE);
  SubsidenceTypes.Add(rsLAYERCOMPACT);
  SubsidenceTypes.Add(rsNDSYSCOMPACT);
  SubsidenceTypes.Add(rsDSYSCOMPACTI);
  SubsidenceTypes.Add(rsZDISPLACEMEN);
  SubsidenceTypes.Add(rsNDCRITICALHE);
  SubsidenceTypes.Add(rsDCRITICALHEA);
end;

{ TSubsidenceObsExtractor }

procedure TSubsidenceObsExtractor.Initialize3DObsTypes;
begin
  F3DObsTypes.Add(rsLAYERCOMPACT);
  F3DObsTypes.Add(rsZDISPLACEMEN);
  F3DObsTypes.Add(rsNDSYSCOMPACT);
  F3DObsTypes.Add(rsDSYSCOMPACTI);
end;

constructor TSubsidenceObsExtractor.Create;
begin
  inherited;
  FSubObs:= TSubsidenceObsValueList.Create;
  F3DObsTypes := TStringList.Create;
  Initialize3DObsTypes;
{
if (Description = rsLAYERCOMPACT)
  or (Description = rsZDISPLACEMEN)
  or (Description = rsNDSYSCOMPACT)
  or (Description = rsDSYSCOMPACTI)
  then
}

end;

destructor TSubsidenceObsExtractor.Destroy;
begin
  F3DObsTypes.Free;
  FSubObs.Free;
  inherited Destroy;
end;

procedure TSubsidenceObsExtractor.ExtractSimulatedValues;
var
  Index: Integer;
  Obs: TSubsidenceObsValue;
  CellIndex: Integer;
  SubFile: TFileStream;
  FileSize: Int64;
  Precision: TModflowPrecision;
  KSTP: Integer;
  KPER: Integer;
  PERTIM: TModflowDouble;
  TOTIM: TModflowDouble;
  DESC: TModflowDesc;
  NCOL: Integer;
  NROW: Integer;
  ILAY: Integer;
  AnArray: TModflowDoubleArray;
  Description: string;
  CharIndex: Integer;
  ACell: TCellID;
  UseArray: Boolean;
  InterpolatedValues: array of double;
  SumWeights: double;
  SumWeightedValues: double;
begin

  for Index := 0 to Pred(FObsValueList.Count) do
  begin
    FSubObs.Add(FObsValueList[Index] as TSubsidenceObsValue);
  end;

  for Index := 0 to Pred(FSubObs.Count) do
  begin
    Obs := FSubObs[Index];
    Obs.FBeforeTime := MissingValue;
    Obs.FAfterTime := MissingValue;
    Assert(Obs.FCellIDs.Count >= 1, Format('No cells specified for the subsidence observation %s.', [Obs.ObsName]));
    SetLength(Obs.FBeforeValues, Obs.FCellIDs.Count);
    SetLength(Obs.FAfterValues, Obs.FCellIDs.Count);
    for CellIndex := 0 to Pred(Obs.FCellIDs.Count) do
    begin
      Obs.FBeforeValues[CellIndex] := MissingValue;
      Obs.FAfterValues[CellIndex] := MissingValue;
    end;
  end;

  SubFile := TFileStream.Create(ModelOutputFileName, fmOpenRead or fmShareDenyWrite);
  try
    FileSize := SubFile.Size;

    if FileSize > 0 then
    begin
      Precision := CheckArrayPrecision(SubFile);
    end
    else
    begin
      raise Exception.Create(Format('The subsidence file "%s" is empty', [ModelOutputFileName]));
    end;

    While SubFile.Position < FileSize do
    begin
      AnArray := nil;
      KSTP := -1;
      KPER := -1;
      PERTIM := -1;
      TOTIM := -1;
      NCOL := -1;
      NROW := -1;
      ILAY := -1;
      for CharIndex := 0 to Pred(Length(DESC)) do
      begin
        DESC[CharIndex] := ' ';
      end;
      case Precision of
        mpSingle:
          ReadSinglePrecisionModflowBinaryRealArray(SubFile, KSTP,
            KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
            True);
        mpDouble:
          ReadDoublePrecisionModflowBinaryRealArray(SubFile, KSTP,
            KPER, PERTIM, TOTIM, DESC, NCOL, NROW, ILAY, AnArray,
            True);
        else Assert(False, Format('Error reading data from %s', [ModelOutputFileName]));
      end;
      Description := string(Trim(DESC));

      for Index := 0 to Pred(FSubObs.Count) do
      begin
        Obs := FSubObs[Index];
        if UpperCase(Obs.ObsType) = Description then
        begin
          if F3DObsTypes.IndexOf(Description) >= 0 then
          begin
            UseArray := Abs(ILAY) = Obs.FCellIDs[0].Layer;
          end
          else
          begin
            UseArray := True;
          end;
          if UseArray then
          begin
            if TOTIM <= Obs.ObsTime then
            begin
              Obs.FBeforeTime:= TOTIM;
              for CellIndex := 0 to Pred(Obs.FCellIDs.Count) do
              begin
                ACell := Obs.FCellIDs[CellIndex];
                Obs.FBeforeValues[CellIndex] := AnArray[ACell.Row-1, ACell.Column-1];
              end;
            end;
            if (TOTIM >= Obs.ObsTime) and (Obs.FAfterTime = MissingValue) then
            begin
              Obs.FAfterTime:= TOTIM;
              for CellIndex := 0 to Pred(Obs.FCellIDs.Count) do
              begin
                ACell := Obs.FCellIDs[CellIndex];
                Obs.FAfterValues[CellIndex] := AnArray[ACell.Row-1, ACell.Column-1];
              end;
            end;
          end;
        end;
      end;
    end;

  finally
    SubFile.Free;
  end;

  for Index := 0 to Pred(FSubObs.Count) do
  begin
    Obs := FSubObs[Index];
    if (Obs.FBeforeTime = MissingValue) or (Obs.FAfterTime = MissingValue) then
    begin
      Obs.SimulatedValue := MissingValue;
      Continue;
    end;
    SetLength(InterpolatedValues, Obs.FCellIDs.Count);
    for CellIndex := 0 to Pred(Obs.FCellIDs.Count) do
    begin
      if Obs.FBeforeTime = Obs.FAfterTime then
      begin
        InterpolatedValues[CellIndex] := Obs.FBeforeValues[CellIndex];
      end
      else
      begin
        if (Obs.FBeforeValues[CellIndex] = MissingValue)
          or (Obs.FAfterValues[CellIndex] = MissingValue) then
        begin
          InterpolatedValues[CellIndex] := MissingValue;
        end
        else
        begin
          InterpolatedValues[CellIndex] := Obs.FBeforeValues[CellIndex]
            + (Obs.ObsTime - Obs.FBeforeTime)/(Obs.FAfterTime - Obs.FBeforeTime)
            * (Obs.FAfterValues[CellIndex] - Obs.FBeforeValues[CellIndex]);
        end;
      end;
    end;
    if InterpolatedValues[0] = MissingValue then
    begin
      Obs.SimulatedValue := MissingValue
    end
    else
    begin
      SumWeights := 0;
      SumWeightedValues := 0;
      for CellIndex := 0 to Pred(Obs.FCellIDs.Count) do
      begin
        ACell := Obs.FCellIDs[CellIndex];
        if InterpolatedValues[CellIndex] <> MissingValue then
        begin
          SumWeights := SumWeights + ACell.Fraction;
          SumWeightedValues := SumWeightedValues
            + ACell.Fraction * InterpolatedValues[CellIndex];
        end;
      end;
      Obs.SimulatedValue := SumWeightedValues/SumWeights;
    end;
  end;
end;

{ TSubsidenceObsValue }

constructor TSubsidenceObsValue.Create;
begin
  FCellIDs := TCellID_List.Create;
end;

destructor TSubsidenceObsValue.Destroy;
begin
  FCellIDs.Free;
  inherited Destroy;
end;

procedure TSubsidenceObsValue.AddCellID(ACellID: TCellID);
begin
  FCellIDs.Add(ACellID);
end;

initialization;
  InitializeSubsidenceTypes

finalization
  SubsidenceTypes.Free;

end.

