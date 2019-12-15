unit InputFileReader;

{$mode objfpc}{$H+}
{$ASSERTIONS ON}

interface

uses
  Classes, SysUtils, Math, Generics.Collections, Generics.Defaults,
  OutputFileReader, RbwParser, FastGeo;

type

  TProcessStatus = (psNone, psOptions, psObsFiles, psIdentifiers, psDerivedObs);
  TIdStatus = (isNone, isID, isLocation, isTime);
  TDerivedObsStatus = (dosNone, dosObsName, dosInterpolate, dosFormula);

  TLocationID = record
    ID: string;
    APoint: TPoint2D;
  end;

  TLocationList = specialize TList<TLocationID>;
  TLocationDictionary = specialize TDictionary<string, TLocationID>;

  TDerivedObs = class
    ID: string;
    Obsname: string;
    Time: double;
    Print: boolean;
    Value: double;
    TimeAssigned: Boolean;
  end;

  TDerivedObsList = specialize TList<TDerivedObs>;

  { TDerivedObsCompare }

  TDerivedObsCompare = class(specialize TComparer<TDerivedObs>)
    function Compare(constref Left, Right: TDerivedObs): Integer; override;
  end;

  { TDerivedObsObjectList }

  TDerivedObsObjectList = class(specialize TObjectList<TDerivedObs>)
  public
    procedure Sort;
  end;

  TDerivedObsDictionary = specialize TDictionary<string, TDerivedObs>;

  { TInputHandler }

  TInputHandler = class(TObject)
  private
    FInputFileLines: TStringList;
    FCurrentProcessStatus: TProcessStatus;
    FPriorProcessStatus: TProcessStatus;
    FIdStatus: TIdStatus;
    FSplitter: TStringList;
    FOutputFile: TStringList;
    FOutputFileName: string;
    FLineIndex: Integer;
    FListingFileName: string;
    FListingFile: TStringList;
    FInstructionFileName: string;
    FInstructionFile: TStringList;
    FObsFileList: TOutputFileObjectList;
    FObservationDictionary: TObservationDictionary;
    FParser: TRbwParser;
    FID: string;
    FLocationDictionary: TLocationDictionary;
    FDerivedObsList: TDerivedObsObjectList;
    FDerivedObsDictionary: TDerivedObsDictionary;
    FPriorDerivedObsStatus: TDerivedObsStatus;
    FObsName: string;
    FPrint: Boolean;
    FIdentifiersRead: Boolean;
    procedure AssignInactiveObs(var NewLocation: TLocationID);
    procedure HandleOption;
    procedure HandleObservationFiles;
    procedure ClearAllObservations;
    procedure InitializeDerivedObs;
    procedure InitializeIdentifiers;
    procedure InitializeObsFiles;
    procedure HandleIdentifiers;
    procedure InterpolateInTime;
    procedure HandleDerivedObs;
    procedure InterpOnePoint(DerivedObs: TDerivedObs; NewLocation: TLocationID);
    procedure InterpTwoPoints(DerivedObss: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure InterpThreePoints(DerivedObservations: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure InterpFourPoints(DerivedObservations: TDerivedObsList;
      Locations: TLocationList; NewLocation: TLocationID);
    procedure PrintToOutputFile(const AnObs: TDerivedObs);
    procedure RecordObs(const AnObs: TDerivedObs);
    procedure AddLocationToDictionary(Location: TLocationID);
    procedure AddObsToDictionary(AnObs: TDerivedObs);
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadAndProcessInputFile(const FileName: string);
  end;

  EInputException = class(Exception);

implementation

uses SubPolygonUnit, BasisFunctionUnit;

const
  InactiveValue = -1E30;
  Epsilon = 1E-6;

resourceString
  rsBEGIN = 'BEGIN';
  rsOPTIONS = 'OPTIONS';
  rsOBSERVATION_Files = 'OBSERVATION_FILES';
  rsIDENTIFIERS = 'IDENTIFIERS';
  rsDERIVED_OBSE = 'DERIVED_OBSERVATIONS';
  rsFORMULA = 'FORMULA';

function NearlyTheSame(A, B, Epsilon: double): boolean;
begin
  result := (A = B)
    or (Abs(A-B)/Abs(A + B) < Epsilon)
end;

{ TDerivedObsCompare }

function TDerivedObsCompare.Compare(constref Left, Right: TDerivedObs
  ): Integer;
begin
  Result := Sign(Left.Time - Right.Time);
end;

{ TDerivedObsObjectList }

procedure TDerivedObsObjectList.Sort;
begin
  inherited Sort(TDerivedObsCompare.Create);
end;

{ TInputHandler }

procedure TInputHandler.HandleOption;

  procedure RecordOutputFile;
  begin
    if (FOutputFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format('Output file = %s', [FOutputFileName]));
    end;
  end;

  procedure RecordInstructionFile;
  begin
    if (FInstructionFileName <> '') and (FListingFile <> nil) then
    begin
      FListingFile.Add(Format('Instruction file = %s', [FInstructionFileName]));
    end;
  end;

begin
  Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there were not exactly two items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
  if UpperCase(FSplitter[0]) = 'LISTING' then
  begin
    Assert(FListingFileName = '', Format('The listing file was already set to "0:s".', [FListingFileName]));
    FListingFileName := FSplitter[1];
    FListingFile := TStringList.Create;
    FListingFile.Add('MODFLOW 6 Observation Extractor');
    FListingFile.Add('');
    FListingFile.Add(Format('Listing file = %s', [FListingFileName]));
    RecordOutputFile;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = 'OUTPUT' then
  begin
    Assert(FOutputFileName = '', Format('The output file was already set to "0:s".', [FOutputFileName]));
    FOutputFileName := FSplitter[1];
    FOutputFile := TStringList.Create;
    RecordOutputFile;
  end
  else if UpperCase(FSplitter[0]) = 'INSTRUCTION' then
  begin
    Assert(FInstructionFileName = '', Format('The instruction file was already set to "0:s".', [FInstructionFileName]));
    FInstructionFileName := FSplitter[1];
    FInstructionFile := TStringList.Create;
    RecordInstructionFile;
  end
  else if UpperCase(FSplitter[0]) = 'END' then
  begin
    Assert(UpperCase(FSplitter[1]) = 'OPTIONS', Format('BEGIN OPTIONS must be paired with END OPTIONS in line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psOptions;
  end
  else
  begin
    Assert(False, Format('Unrecognized option in line %0:d, ""%1:s.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
  end;
end;

procedure TInputHandler.AssignInactiveObs(var NewLocation: TLocationID);
var
  AnObs: TDerivedObs;
begin
  AnObs := TDerivedObs.Create;
  AnObs.ID := '';
  AnObs.Obsname := FObsName;
  AnObs.Print := FPrint;
  AnObs.Value := InactiveValue;
  AnObs.Time := 0;
  AnObs.TimeAssigned := False;
  AddLocationToDictionary(NewLocation);
  //FLocationDictionary.Add(UpperCase(NewLocation.ID), NewLocation);
  RecordObs(AnObs);
end;

procedure TInputHandler.HandleObservationFiles;
var
  FileName: string;
  FileType: TFileType;
  ObsFile: TOutputFile;
begin
  Assert(FSplitter.Count in [2, 3], Format('In line %0:d, "%1:s", there must be exactly two or three items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
  if UpperCase(FSplitter[0]) = 'FILENAME' then
  begin
    FileName := FSplitter[1];
    Assert(FileExists(FileName), Format('The observation file "%s" does not exist', [FileName]));
    if FSplitter.Count = 3 then
    begin
      if UpperCase(FSplitter[2]) = 'BINARY' then
      begin
        FileType := ftBinary
      end
      else if UpperCase(FSplitter[2]) = 'TEXT' then
      begin
        FileType := ftText
      end
      else
      begin
        Assert(False, Format('The file format in line %0:d, "%1:s" must be "BINARY" or "TEXT".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
      end;
    end
    else
    begin
      FileType := ftBinary
    end;
    ObsFile := TOutputFile.Create(FileName, FileType, FObservationDictionary);
    FObsFileList.Add(ObsFile);
  end
  else if UpperCase(FSplitter[0]) = 'END' then
  begin
    Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
    Assert(UpperCase(FSplitter[1]) = 'OBSERVATION_FILES', Format('BEGIN OBSERVATION_FILES must be paired with END OBSERVATION_FILES in line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
    FCurrentProcessStatus := psNone;
    FPriorProcessStatus := psObsFiles;
  end
  else
  begin
    Assert(False, Format('Unrecognized option in line %0:d, ""%1:s.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
  end;
end;

procedure TInputHandler.ClearAllObservations;
begin
  FParser.ClearExpressions;
  FParser.ClearVariables;
  FObsFileList.Clear;
  FObservationDictionary.Clear;
  FLocationDictionary.Clear;
  FDerivedObsList.Clear;
  FDerivedObsDictionary.Clear;
end;

procedure TInputHandler.InitializeDerivedObs;
begin
  FCurrentProcessStatus := psDerivedObs;
  FPriorDerivedObsStatus := dosNone;
end;

procedure TInputHandler.InitializeIdentifiers;
begin
  Assert(not FIdentifiersRead, Format('In line %0:d, "%1:s", a second IDENTIFIERS block was started without being preceeded by an OBSERVATION_FILES block.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
  FCurrentProcessStatus := psIdentifiers;
  FIdStatus := isNone;
  FIdentifiersRead := True;
end;

procedure TInputHandler.InitializeObsFiles;
begin
  FIdentifiersRead := False;
  FCurrentProcessStatus := psObsFiles;
  ClearAllObservations;
end;

procedure TInputHandler.HandleIdentifiers;
var
  LocationID: TLocationID;
  DerivedObs: TDerivedObs;
begin
  case FIdStatus of
    isNone:
      begin
        if UpperCase(FSplitter[0]) = 'ID' then
        begin
          FIdStatus := isID
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "ID".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
    isID:
      begin
        if UpperCase(FSplitter[0]) = 'LOCATION' then
        begin
          FIdStatus := isLocation
        end
        else if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "LOCATION" or "OBSNAME".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
    isLocation:
      begin
        if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "OBSNAME".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
    isTime:
      begin
        if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          FIdStatus := isTime
        end
        else if UpperCase(FSplitter[0]) = 'ID' then
        begin
          FIdStatus := isID
        end
        else if UpperCase(FSplitter[0]) = 'END' then
        begin
          FIdStatus := isNone;
          FPriorProcessStatus := psIdentifiers;
          FCurrentProcessStatus := psNone;
          Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          Assert(UpperCase(FSplitter[1]) = 'IDENTIFIERS', Format('BEGIN IDENTIFIERS must be paired with END IDENTIFIERS in line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          InterpolateInTime;
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s" must start with "OBSNAME" or "END".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
  else Assert(False);
  end;
  case FIdStatus of
    isNone:
      begin
      end;
    isID:
      begin
        Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there must be exactly two items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        FID := FSplitter[1];
      end;
    isLocation:
      begin
        Assert(FSplitter.Count = 3, Format('In line %0:d, "%1:s", there must be exactly three items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        LocationID.ID := FID;
        try
          LocationID.APoint.X := StrToFloat(FSplitter[1]);
          LocationID.APoint.Y := StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            raise EInputException.Create(Format('Error converting X or Y coordinate on line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          end;
        end;
        AddLocationToDictionary(LocationID);
        //FLocationDictionary.Add(LocationID.ID, LocationID);
      end;
    isTime:
      begin
        Assert(FSplitter.Count in [3, 4], Format('In line %0:d, "%1:s", there must be exactly three or four items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        DerivedObs := TDerivedObs.Create;
        DerivedObs.ID := FID;
        DerivedObs.Obsname := FSplitter[1];
        try
          DerivedObs.Time:= StrToFloat(FSplitter[2]);
        except on EConvertError do
          begin
            DerivedObs.Free;
            raise EInputException.Create(Format('Error converting time on line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          end;
        end;
        DerivedObs.TimeAssigned := True;
        if FSplitter.Count = 4 then
        begin
          Assert(UpperCase(FSplitter[3]) = 'PRINT', Format('In line %0:d, "%1:s", the fourth item, if present, must be "PRINT".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          DerivedObs.Print := True;
        end
        else
        begin
          DerivedObs.Print := False;
        end;
        DerivedObs.Value := 0;
        FDerivedObsList.Add(DerivedObs);
      end;
  else Assert(False, 'Programming error in TInputHandler.HandleIdentifiers');
  end;
end;

procedure TInputHandler.InterpolateInTime;
var
  ObsIndex: Integer;
  AnObs: TDerivedObs;
  ObsFile : TFileId;
  FirstValue: double;
  SecondValue: double;
  FirstTime: double;
  SecondTime: double;
begin
  FDerivedObsList.Sort;
  for ObsIndex := 0 to Pred(FDerivedObsList.Count) do
  begin
    AnObs := FDerivedObsList[ObsIndex];
    Assert(FObservationDictionary.TryGetValue(UpperCase(AnObs.ID), ObsFile), Format('The observation %s is not found in any of the observation output files.', [AnObs.ID]));
    if  ObsFile.OutputFile.FirstTime < ObsFile.OutputFile.SecondTime then
    begin
      while AnObs.Time > ObsFile.OutputFile.SecondTime do
      begin
        ObsFile.OutputFile.ReadTimeAndValues;
        if ObsFile.OutputFile.FirstTime > ObsFile.OutputFile.SecondTime then
        begin
          break;
        end;
      end;
      if ObsFile.OutputFile.FirstTime > ObsFile.OutputFile.SecondTime then
      begin
        AnObs.Value := ObsFile.OutputFile.FirstValue[ObsFile.Position];
      end
      else
      begin
        FirstValue := ObsFile.OutputFile.FirstValue[ObsFile.Position];
        SecondValue := ObsFile.OutputFile.SecondValue[ObsFile.Position];
        FirstTime := ObsFile.OutputFile.FirstTime;
        SecondTime := ObsFile.OutputFile.SecondTime;
        AnObs.Value := FirstValue + (SecondValue-FirstValue)
          * (AnObs.Time - FirstTime)
          / (SecondTime - FirstTime);
      end;
      AnObs.TimeAssigned := True;
      AddObsToDictionary(AnObs);
      //FDerivedObsDictionary.Add(UpperCase(AnObs.Obsname), AnObs);
      FParser.CreateVariable(AnObs.ObsName, '', AnObs.Value, AnObs.ObsName);
      PrintToOutputFile(AnObs);
    end;
  end;
end;

procedure TInputHandler.InterpOnePoint(DerivedObs: TDerivedObs; NewLocation: TLocationID);
var
  AnObs: TDerivedObs;
begin
  AnObs := TDerivedObs.Create;
  AnObs.ID := '';
  AnObs.Obsname := FObsName;
  AnObs.Print := FPrint;
  AnObs.Value := DerivedObs.Value;
  AnObs.Time := DerivedObs.Time;
  AnObs.TimeAssigned := DerivedObs.TimeAssigned;
  AddLocationToDictionary(NewLocation);
  //FLocationDictionary.Add(UpperCase(NewLocation.ID), NewLocation);
  RecordObs(AnObs);
end;

procedure TInputHandler.InterpTwoPoints(DerivedObss: TDerivedObsList;
  Locations: TLocationList; NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  ClosestPoint: TPoint2D;
  SegmentLength: double;
  PointDistance: double;
  AnObs: TDerivedObs;
begin
  Assert(DerivedObss.Count = 2, 'Programming error in TInputHandler.InterpTwoPoints');
  Assert(Locations.Count = 2, 'Programming error in TInputHandler.InterpTwoPoints');
  DerivedObs1 := DerivedObss[0];
  DerivedObs2 := DerivedObss[1];
  Location1 := Locations[0];
  Location2 := Locations[1];
  ClosestPoint := ClosestPointOnSegmentFromPoint(
    EquateSegment(Location1.APoint, Location2.APoint),
    NewLocation.APoint);
  SegmentLength := Distance(Location1.APoint, Location2.APoint);
  if SegmentLength > 0 then
  begin
    PointDistance := Distance(Location1.APoint, ClosestPoint);

    AnObs := TDerivedObs.Create;
    AnObs.ID := '';
    AnObs.Obsname := FObsName;
    AnObs.Print := FPrint;
    AnObs.Time := DerivedObs1.Time;
    AnObs.TimeAssigned := DerivedObs1.TimeAssigned and DerivedObs2.TimeAssigned and (DerivedObs1.Time = DerivedObs2.Time);

    AnObs.Value := DerivedObs1.Value
      + (DerivedObs2.Value - DerivedObs1.Value)
      * PointDistance/SegmentLength;

    AddLocationToDictionary(NewLocation);
    //FLocationDictionary.Add(UpperCase(NewLocation.ID), NewLocation);
    RecordObs(AnObs);
  end
  else
  begin
    Assert(False, Format('In line %0:d, "%1:s", the distance between %2:s and %3:s is zero.', [FLineIndex+1, FInputFileLines[FLineIndex], DerivedObs1.Obsname, DerivedObs2.Obsname]));
  end;
end;

procedure TInputHandler.InterpThreePoints(DerivedObservations: TDerivedObsList;
  Locations: TLocationList; NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  DerivedObs3: TDerivedObs;
  Location3: TLocationID;
  ClosestPoint: TPoint2D;
  AnObs: TDerivedObs;
  Corners: TTriangularElement;
  Triangle: TSimplePolygon;
  NodeValues: TTriangularNodeValues;
  Distance1: double;
  Distance2: double;
  Distance3: double;
begin
  Assert(DerivedObservations.Count = 3, 'Programming error in TInputHandler.InterpThreePoints');
  Assert(Locations.Count = 3, 'Programming error in TInputHandler.InterpThreePoints');
  DerivedObs1 := DerivedObservations[0];
  DerivedObs2 := DerivedObservations[1];
  DerivedObs3 := DerivedObservations[2];
  Location1 := Locations[0];
  Location2 := Locations[1];
  Location3 := Locations[2];
  SetLength(Corners, 4);
  Corners[0] := Location1.APoint;
  Corners[2] := Location2.APoint;
  Corners[3] := Location3.APoint;
  Corners[4] := Location1.APoint;
  Triangle := TSimplePolygon.Create(Corners);
  try
    if Triangle.PointInside(NewLocation.APoint) then
    begin
      AnObs := TDerivedObs.Create;
      AnObs.ID := '';
      AnObs.Obsname := FObsName;
      AnObs.Print := FPrint;
      AnObs.Time := DerivedObs1.Time;
      AnObs.TimeAssigned := DerivedObs1.TimeAssigned
        and DerivedObs2.TimeAssigned and DerivedObs3.TimeAssigned
        and (DerivedObs1.Time = DerivedObs2.Time)
        and (DerivedObs1.Time = DerivedObs3.Time);

      SetLength(Corners, 3);
      SetLength(NodeValues, 3);
      NodeValues[0] := DerivedObs1.Value;
      NodeValues[1] := DerivedObs2.Value;
      NodeValues[2] := DerivedObs3.Value;
      AnObs.Value := TriangularBasisFunction(Corners, NodeValues, NewLocation.APoint);

      AddLocationToDictionary(NewLocation);
      //FLocationDictionary.Add(UpperCase(NewLocation.ID), NewLocation);
      RecordObs(AnObs);
  end
    else
    begin
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location2.APoint),
        NewLocation.APoint);
      Distance1 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance2 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location2.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance3 := Distance(ClosestPoint, NewLocation.APoint);
      if (Distance1 <= Distance2) and (Distance1 <= Distance3) then
      begin
        DerivedObservations.Delete(2);
        Locations.Delete(2);
      end
      else if (Distance2 <= Distance1) and (Distance2 <= Distance3) then
      begin
        DerivedObservations.Delete(1);
        Locations.Delete(1);
      end
      else if (Distance3 <= Distance1) and (Distance3 <= Distance2) then
      begin
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else
      begin
        Assert(False, 'Programming error in TInputHandler.InterpThreePoints');
      end;
      InterpTwoPoints(DerivedObservations, Locations, NewLocation);
    end;
  finally
    Triangle.Free;
  end;
end;

procedure TInputHandler.InterpFourPoints(DerivedObservations: TDerivedObsList;
  Locations: TLocationList; NewLocation: TLocationID);
var
  DerivedObs1: TDerivedObs;
  Location1: TLocationID;
  DerivedObs2: TDerivedObs;
  Location2: TLocationID;
  DerivedObs3: TDerivedObs;
  Location3: TLocationID;
  DerivedObs4: TDerivedObs;
  Location4: TLocationID;
  ClosestPoint: TPoint2D;
  AnObs: TDerivedObs;
  Corners: TTriangularElement;
  Quadrilateral: TSimplePolygon;
  NodeValues: TTriangularNodeValues;
  Distance1: double;
  Distance2: double;
  Distance3: double;
  Distance4: double;
begin
  Assert(DerivedObservations.Count = 4, 'Programming error in TInputHandler.InterpFourPoints');
  Assert(Locations.Count = 4, 'Programming error in TInputHandler.InterpFourPoints');
  DerivedObs1 := DerivedObservations[0];
  DerivedObs2 := DerivedObservations[1];
  DerivedObs3 := DerivedObservations[2];
  DerivedObs4 := DerivedObservations[3];
  Location1 := Locations[0];
  Location2 := Locations[1];
  Location3 := Locations[2];
  Location4 := Locations[3];
  SetLength(Corners, 5);
  Corners[0] := Location1.APoint;
  Corners[2] := Location2.APoint;
  Corners[3] := Location3.APoint;
  Corners[4] := Location4.APoint;
  Corners[5] := Location1.APoint;
  Quadrilateral := TSimplePolygon.Create(Corners);
  try
    if Quadrilateral.PointInside(NewLocation.APoint) then
    begin
      AnObs := TDerivedObs.Create;
      AnObs.ID := '';
      AnObs.Obsname := FObsName;
      AnObs.Print := FPrint;
      AnObs.Time := DerivedObs1.Time;
      AnObs.TimeAssigned := DerivedObs1.TimeAssigned
        and DerivedObs2.TimeAssigned and DerivedObs3.TimeAssigned
        and DerivedObs4.TimeAssigned
        and (DerivedObs1.Time = DerivedObs2.Time)
        and (DerivedObs1.Time = DerivedObs3.Time)
        and (DerivedObs4.Time = DerivedObs3.Time);

      SetLength(Corners, 4);
      SetLength(NodeValues, 4);
      NodeValues[0] := DerivedObs1.Value;
      NodeValues[1] := DerivedObs2.Value;
      NodeValues[2] := DerivedObs3.Value;
      NodeValues[3] := DerivedObs4.Value;
      AnObs.Value := QuadrilateralBasisFunction(Corners, NodeValues, NewLocation.APoint);

      AddLocationToDictionary(NewLocation);
      //FLocationDictionary.Add(UpperCase(NewLocation.ID), NewLocation);
      RecordObs(AnObs);
    end
    else
    begin
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location1.APoint, Location2.APoint),
        NewLocation.APoint);
      Distance1 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location2.APoint, Location3.APoint),
        NewLocation.APoint);
      Distance2 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location3.APoint, Location4.APoint),
        NewLocation.APoint);
      Distance3 := Distance(ClosestPoint, NewLocation.APoint);
      ClosestPoint := ClosestPointOnSegmentFromPoint(
        EquateSegment(Location4.APoint, Location1.APoint),
        NewLocation.APoint);
      Distance4 := Distance(ClosestPoint, NewLocation.APoint);
      if (Distance1 <= Distance2) and (Distance1 <= Distance3) and (Distance1 <= Distance4) then
      begin
        DerivedObservations.Delete(3);
        Locations.Delete(3);
        DerivedObservations.Delete(2);
        Locations.Delete(2);
      end
      else if (Distance2 <= Distance1) and (Distance2 <= Distance3) and (Distance2 <= Distance4) then
      begin
        DerivedObservations.Delete(3);
        Locations.Delete(3);
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else if (Distance3 <= Distance1) and (Distance3 <= Distance2) and (Distance3 <= Distance4) then
      begin
        DerivedObservations.Delete(1);
        Locations.Delete(1);
        DerivedObservations.Delete(0);
        Locations.Delete(0);
      end
      else if (Distance4 <= Distance1) and (Distance4 <= Distance2) and (Distance4 <= Distance3) then
      begin
        DerivedObservations.Delete(2);
        Locations.Delete(2);
        DerivedObservations.Delete(1);
        Locations.Delete(1);
      end
      else
      begin
        Assert(False, 'Programming error in TInputHandler.InterpFourPoints');
      end;
      InterpTwoPoints(DerivedObservations, Locations, NewLocation);
    end;
  finally
    Quadrilateral.Free;
  end;
end;

procedure TInputHandler.PrintToOutputFile(const AnObs: TDerivedObs);
begin
  if AnObs.Print then
  begin
    FOutputFile.Add(Format('%0:s %1:g', [AnObs.ObsName, AnObs.Value]));
  end;
end;

procedure TInputHandler.RecordObs(const AnObs: TDerivedObs);
begin
  AddObsToDictionary(AnObs);
  //FDerivedObsDictionary.Add(UpperCase(AnObs.Obsname), AnObs);
  FDerivedObsList.Add(AnObs);
  FParser.CreateVariable(AnObs.ObsName, '', AnObs.Value, AnObs.ObsName);
  PrintToOutputFile(AnObs);
end;

procedure TInputHandler.AddLocationToDictionary(
  Location: TLocationID);
begin
  try
    FLocationDictionary.Add(UpperCase(Location.ID), Location);

  except on EListError do
    begin
      Assert(False, Format('The identifier "%s" is a duplicate of an earlier identifier', [Location.ID]));
    end;
  end;

end;

procedure TInputHandler.AddObsToDictionary(AnObs: TDerivedObs);
begin
  try
    FDerivedObsDictionary.Add(UpperCase(AnObs.Obsname), AnObs);

  except on EListError do
    begin
      Assert(False, Format('The identifier "%s" is a duplicate of an earlier oservation name', [AnObs.Obsname]));
    end;

  end;
end;

procedure TInputHandler.HandleDerivedObs;
var
  CurrentStatus: TDerivedObsStatus;
  //DerivedObs1: TDerivedObs;
  DerivedObs: TDerivedObs;
  NewLocation: TLocationID;
  LocationList: TLocationList;
  DerivedObsList: TDerivedObsList;
  LocationIndex: Integer;
  ALine: String;
  Formula: string;
  AnObs: TDerivedObs;
  function GetLocation(DerivedObs1: TDerivedObs): TLocationID;
  var
    ALocationID: TLocationID;
  begin
    if FLocationDictionary.TryGetValue(UpperCase(DerivedObs1.ID), ALocationID)then
    begin
      result := ALocationID
    end
    else if FLocationDictionary.TryGetValue(UpperCase(DerivedObs1.ID), ALocationID)then
    begin
      result := ALocationID
    end
    else
    begin
      Assert(False, Format('In line %0:d, "%1:s", there is no location associated with %2:s.', [FLineIndex+1, FInputFileLines[FLineIndex], DerivedObs1.ID]));
    end;
  end;
  function GetObs(ObsName: string): TDerivedObs;
  var
    ADerivedObs: TDerivedObs;
  begin
    if FDerivedObsDictionary.TryGetValue(UpperCase(ObsName), ADerivedObs) then
    begin
      result := ADerivedObs;
    end
    else
    begin
      Assert(False, Format('In line %0:d, "%1:s", there is no observation associated with %2:s.', [FLineIndex+1, FInputFileLines[FLineIndex], ObsName]));
    end;
  end;

begin
  LocationList := TLocationList.Create;
  DerivedObsList := TDerivedObsList.Create;
  try
    case FPriorDerivedObsStatus of
      dosNone:
      begin
        if UpperCase(FSplitter[0]) = 'OBSNAME' then
        begin
          CurrentStatus := dosObsName;
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s", the line does not begin with "OBSNAME".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
      dosObsName:
      begin
        if UpperCase(FSplitter[0]) = 'INTERPOLATE' then
        begin
          CurrentStatus := dosInterpolate;
        end
        else if UpperCase(FSplitter[0]) = rsFORMULA then
        begin
          CurrentStatus := dosFormula;
        end
        else
        begin
          Assert(False, Format('In line %0:d, "%1:s", the line does not begin with "FORMULA" OR "INTERPOLATE".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
        end;
      end;
      dosInterpolate, dosFormula:
      begin
        if UpperCase(FSplitter[0]) = 'END' then
        begin
          Assert(UpperCase(FSplitter[1]) = 'DERIVED_OBSERVATIONS', Format('In line %0:d, "%1:s", the DERIVED_OBSERVATIONS block must end with "END DERIVED_OBSERVATIONS".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          CurrentStatus := dosNone;
          FCurrentProcessStatus := psNone;
        end
        else
        begin
          CurrentStatus := dosObsName
        end;
      end;
    else Assert(False);
    end;
    FPriorDerivedObsStatus := CurrentStatus;
    case CurrentStatus of
      dosNone:
        begin
        end;
      dosObsName:
        begin
          Assert(FSplitter.Count in [2,3], Format('In line %0:d, "%1:s", there must be either two or three items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          FObsName := FSplitter[1];
          if FSplitter.Count = 2 then
          begin
            FPrint := False;
          end
          else if UpperCase(FSplitter[2]) = 'PRINT' then
          begin
            FPrint := True;
          end
          else
          begin
            Assert(False, Format('In line %0:d, "%1:s", if a third item is listed, it must be "PRINT".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          end;
        end;
      dosInterpolate:
        begin
          NewLocation.ID := FObsName;
          Assert(FSplitter.Count in [4..7], Format('In line %0:d, "%1:s", there must be from four to seven items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
          try
            NewLocation.APoint.X := StrToFloat(FSplitter[1]);
            NewLocation.APoint.Y := StrToFloat(FSplitter[2]);
          except on EConvertError do
            begin
              raise EInputException.Create(Format('Error converting X or Y coordinate on line %0:d, "%1:s".', [FLineIndex+1, FInputFileLines[FLineIndex]]));
            end;
          end;
          for LocationIndex := 3 to Pred(FSplitter.Count) do
          begin
            DerivedObs := GetObs(UpperCase(FSplitter[LocationIndex]));
            if not NearlyTheSame(DerivedObs.Value, InactiveValue, Epsilon) then
            begin
              DerivedObsList.Add(DerivedObs);
              LocationList.Add(GetLocation(DerivedObs));
            end
            else
            begin
              // Add a warning message.
            end;
          end;
          case DerivedObsList.Count of
            0:
              begin
                AssignInactiveObs(NewLocation);
              end;
            1:
              begin
                InterpOnePoint(DerivedObsList[0], NewLocation);
              end;
            2:
              begin
                InterpTwoPoints(DerivedObsList, LocationList, NewLocation);
              end;
            3:
              begin
                InterpThreePoints(DerivedObsList, LocationList, NewLocation);
              end;
            4:
              begin
                InterpFourPoints(DerivedObsList, LocationList, NewLocation);
              end;
            else Assert(False, 'Programming error in TInputHandler.HandleDerivedObs');
          end;
        end;
      dosFormula:
        begin
          ALine := FInputFileLines[FLineIndex];
          ALine := Trim(ALine);
          Formula := Copy(ALine, Length(rsFORMULA)+1, MAXINT);
          Formula := Trim(Formula);
          try
            FParser.Compile(Formula);
            if not (FParser.CurrentExpression.ResultType in [rdtDouble, rdtInteger]) then
            begin
              Assert(False, Format('In line %0:d, "%1:s", The formula "%2:s" is invalid because it doesn''t evaluate to a number.', [FLineIndex+1, FInputFileLines[FLineIndex], Formula]));
            end;

            AnObs := TDerivedObs.Create;
            AnObs.ID := '';
            AnObs.Obsname := FObsName;
            AnObs.Print := FPrint;
            FParser.CurrentExpression.Evaluate;
            AnObs.Value := FParser.CurrentExpression.DoubleResult;
            AnObs.Time := 0;
            AnObs.TimeAssigned := False;
            RecordObs(AnObs);
          except on E: ERbwParserError do
            begin
              raise EInputException.Create(Format('In line %0:d, "%1:s", the formula "%2:s" is invalid. The error message is "%3:s".', [FLineIndex+1, FInputFileLines[FLineIndex], Formula, E.Message]));
            end;
          end;
        end;
    else
      Assert(False, Format('In line %0:d, "%1:s", Error', [FLineIndex+1, FInputFileLines[FLineIndex], Formula]));
    end;

  finally
    LocationList.Free;
    DerivedObsList.Free;
  end;
end;

constructor TInputHandler.Create;
begin
  FCurrentProcessStatus := psNone;
  FPriorProcessStatus := psNone;
  FInputFileLines := TStringList.Create;
  FSplitter := TStringList.Create;
  FObsFileList := TOutputFileObjectList.Create;
  FObservationDictionary := TObservationDictionary.Create;
  FParser := TRbwParser.Create(nil);
  FLocationDictionary := TLocationDictionary.Create;
  FDerivedObsList := TDerivedObsObjectList.Create;
  FDerivedObsDictionary := TDerivedObsDictionary.Create;

  FListingFileName := '';
  FOutputFileName := '';
  FInstructionFileName := '';
end;

destructor TInputHandler.Destroy;
begin
  if FListingFileName <> '' then
  begin
    FListingFile.SaveToFile(FListingFileName);
  end;
  if FOutputFileName <> '' then
  begin
    FOutputFile.SaveToFile(FOutputFileName);
  end;
  FLocationDictionary.Free;
  FParser.Free;
  FObservationDictionary.Free;
  FObsFileList.Free;
  FInstructionFile.Free;
  FListingFile.Free;
  FOutputFile.Free;
  FSplitter.Free;
  FInputFileLines.Free;
  inherited Destroy;
end;

procedure TInputHandler.ReadAndProcessInputFile(const FileName: string);
var
  Index: Integer;
  ALine: String;
begin
  try
    try
      FInputFileLines.LoadFromFile(FileName);
      for Index := 0 to Pred(FInputFileLines.Count) do
      begin
        FLineIndex := Index;
        ALine := FInputFileLines[Index];
        ALine := Trim(ALine);
        if ALine = '' then
        begin
          Continue;
        end;
        if ALine[1] = '#' then
        begin
          Continue;
        end;
        FSplitter.DelimitedText := ALine;
        case FCurrentProcessStatus of
          psNone:
          begin
            Assert(FSplitter.Count = 2, Format('In line %0:d, "%1:s", there were not exactly two items listed.', [FLineIndex+1, FInputFileLines[FLineIndex]]));
            Assert(UpperCase(FSplitter[0]) = rsBEGIN);
            case FPriorProcessStatus of
              psNone:
              begin
                Assert(UpperCase(FSplitter[1]) = rsOPTIONS);
                FCurrentProcessStatus := psOptions;
              end;
              psOptions:
              begin
                Assert(UpperCase(FSplitter[1]) = rsOBSERVATION_Files);
                InitializeObsFiles;
              end;
              psObsFiles:
              begin
                Assert(UpperCase(FSplitter[1]) = rsIDENTIFIERS);
                InitializeIdentifiers;
              end;
              psIdentifiers, psDerivedObs:
              begin
                if UpperCase(FSplitter[1]) = rsDERIVED_OBSE then
                begin
                  InitializeDerivedObs;
                end
                else if UpperCase(FSplitter[1]) = rsOBSERVATION_Files then
                begin
                  InitializeObsFiles
                end
                else if UpperCase(FSplitter[1]) = rsIDENTIFIERS then
                begin
                  InitializeIdentifiers;
                end
                else
                begin
                  Assert(False);
                end;
              end;
              else
                Assert(False);
            end;
          end;
          psOptions:
          begin
            HandleOption;
          end;
          psObsFiles:
          begin
            HandleObservationFiles;
          end;
          psIdentifiers:
          begin
            HandleIdentifiers;
          end;
          psDerivedObs:
          begin
            HandleDerivedObs
          end;
          else
            Assert(False);
        end;
      end;
    except on E: Exception do
      begin
        if FListingFile <> nil then
        begin
          FListingFile.Add(E.Message);
        end;
        raise;
      end;
    end;
  finally
  end;
end;

end.
psOptions, psObsFiles, psIdentifiers, psDerivedObs
