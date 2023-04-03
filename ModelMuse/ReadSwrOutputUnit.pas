unit ReadSwrOutputUnit;

interface

uses
  System.UITypes, Windows, Types, Generics.Collections, Classes, SysUtils,
  GoPhastTypes;

type
  TSwrFileType = (srtAscii, srtBinary);

  TCustomSwrResult = class(TObject)
  public
    TotalTime: Double;
    SwrTimeStepLength: double;
    StressPeriod: integer;
    ModflowTimeStep: integer;
    SwrTimeStep: Integer;
  end;

  TSwrTimeStage = class(TCustomSwrResult)
    Stages: TDoubleDynArray;
  end;

  TSwrTimeStages = class(TObjectList<TSwrTimeStage>)
  private
    FNumberOfReaches: Integer;
  public
    property NumberOfReaches: Integer read FNumberOfReaches
      write FNumberOfReaches;
  end;

  TReachExchange = class(TCustomSwrResult)
  private
    BottomElevations: TList<double>;
    ReachStages: TList<double>;
    ReachDepths: TList<double>;
    GroundwaterHead: TList<double>;
    WettedPerimeter: TList<double>;
    Conductance: TList<double>;
    HeadDifference: TList<double>;
    Aquifer_ReachFlow: TList<double>;
    function GetValue(SwrDataType, ReachIndex: Integer): double;
  public
    ReachNumbers: TGenericIntegerList;
    ModelLayers: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    property Value[SwrDataType, ReachIndex: Integer]: double read GetValue;
  end;

  TReachExchanges = TObjectList<TReachExchange>;

  TReachGroupWaterBudget = class(TCustomSwrResult)
  private
    ReachGroupStages: TList<double>;
    InflowToConnected: TList<double>;
    LateralFlow: TList<double>;
    UzfInflow: TList<double>;
    Rain: TList<double>;
    Evaporation: TList<double>;
    AquiferReachFlow: TList<double>;
    OutflowToConnected: TList<double>;
    FlowFromExternalSources: TList<double>;
    StructureFlow: TList<double>;
    ConstantReachFlow: TList<double>;
    VolumeChange: TList<double>;
    Discrepancy: TList<double>;
    Volume: TList<double>;
    function GetValue(SwrDataType, ReachGroupIndex: Integer): double;
  public
    ReachGroupNumbers: TGenericIntegerList;
    constructor Create;
    destructor Destroy; override;
    property Value[SwrDataType, ReachGroupIndex: Integer]: double read GetValue;
  end;

  TReachGroupWaterBudgets = TObjectList<TReachGroupWaterBudget>;

  TObsData = class(TObject)
  private
    FTime: Double;
    FValues: TList<Double>;
    function GetValue(Index: Integer): Double;
  public
    constructor Create;
    destructor Destroy; override;
    property Time: Double read FTime;
    property Values[Index: Integer]: Double read GetValue; default;
  end;

  TObsDataList = class(TObjectList<TObsData>)
  private
    FObsNames: TStringList;
    function GetObsName(Index: Integer): string;
    function GObsNameCount: Integer;
  public
    procedure Clear;
    constructor Create;
    destructor Destroy; override;
    property ObsNameCount: Integer read GObsNameCount;
    property ObsNames[Index: Integer]: string read GetObsName;
    function IndexOfName(AnObsName: string): Integer;
  end;

procedure ReadSwrTimeStageData(const FileName: string; FileType: TSwrFileType;
  Data: TSwrTimeStages);

procedure ReadSwrReachExchangeData(const FileName: string;
  FileType: TSwrFileType; Data: TReachExchanges);

procedure ReadSwrReachGroupWaterBudgetData(const FileName: string;
  FileType: TSwrFileType; Data: TReachGroupWaterBudgets);

procedure ReadSwrObsData(const FileName: string;
  FileType: TSwrFileType; Data: TObsDataList);

implementation

uses
  IOUtils, Dialogs;

resourcestring
  StrTheFileContainsNo = 'The file contains no observations';

procedure ReadSwrTimeStageData(const FileName: string; FileType: TSwrFileType;
  Data: TSwrTimeStages);
const
  NumberOfNonStageItems = 5;
var
  TextReader: TStreamReader;
  BinaryReader: TFileStream;
  Splitter: TStringList;
  StageData: TSwrTimeStage;
  ReaderFileSize: Int64;
  ReachIndex: Integer;
  NumberOfReaches: integer;
begin
  Data.Clear;
  case FileType of
    srtAscii:
      begin
        TextReader := TFile.OpenText(FileName);
        Splitter := TStringList.Create;
        try
          Splitter.CommaText := TextReader.ReadLine;
          while Splitter[Splitter.Count-1] = '' do
          begin
            Splitter.Delete(Splitter.Count-1);
          end;
          Data.NumberOfReaches := Splitter.Count - NumberOfNonStageItems;
          while not TextReader.EndOfStream do
          begin
            Splitter.CommaText := TextReader.ReadLine;
            if Splitter.Count = 0 then
            begin
              Exit;
            end;
            Assert(Splitter.Count >= Data.NumberOfReaches + NumberOfNonStageItems);
            StageData := TSwrTimeStage.Create;
            SetLength(StageData.Stages,Data.NumberOfReaches);
            StageData.TotalTime := FortranStrToFloat(Splitter[0]);
            StageData.SwrTimeStepLength := FortranStrToFloat(Splitter[1]);
            StageData.StressPeriod := StrToInt(Splitter[2]);
            StageData.ModflowTimeStep := StrToInt(Splitter[3]);
            StageData.SwrTimeStep := StrToInt(Splitter[4]);
            for ReachIndex := 0 to Data.NumberOfReaches - 1 do
            begin
              StageData.Stages[ReachIndex] :=
                FortranStrToFloat(Splitter[ReachIndex+NumberOfNonStageItems]);
            end;
            Data.Add(StageData);
          end;
        finally
          TextReader.Free;
          Splitter.Free;
        end;
      end;
    srtBinary:
      begin
        BinaryReader := TFile.OpenRead(FileName);
        try
          ReaderFileSize := BinaryReader.Size;

          BinaryReader.Read(NumberOfReaches, SizeOf(NumberOfReaches));
          Data.NumberOfReaches := NumberOfReaches;
          while BinaryReader.Position < ReaderFileSize do
          begin
            StageData := TSwrTimeStage.Create;
            SetLength(StageData.Stages,Data.NumberOfReaches);
            BinaryReader.Read(StageData.TotalTime, SizeOf(StageData.TotalTime));
            BinaryReader.Read(StageData.SwrTimeStepLength, SizeOf(StageData.SwrTimeStepLength));
            BinaryReader.Read(StageData.StressPeriod, SizeOf(StageData.StressPeriod));
            BinaryReader.Read(StageData.ModflowTimeStep, SizeOf(StageData.ModflowTimeStep));
            BinaryReader.Read(StageData.SwrTimeStep, SizeOf(StageData.SwrTimeStep));
            for ReachIndex := 0 to Data.NumberOfReaches - 1 do
            begin
              BinaryReader.Read(StageData.Stages[ReachIndex], SizeOf(double));
            end;
            Data.Add(StageData);
          end;
        finally
          BinaryReader.Free;
        end;
      end;
    else
      Assert(False);
  end;
end;

procedure ReadSwrReachExchangeData(const FileName: string;
  FileType: TSwrFileType; Data: TReachExchanges);
var
  TextReader: TStreamReader;
  Splitter: TStringList;
  ReachExchange: TReachExchange;
  StressPeriod: Integer;
  ModflowTimeStep: Integer;
  SwrTimeStep: Integer;
  ReachIndex: Integer;
  PriorStressPeriod: Integer;
  PriorModflowTimeStep: Integer;
  PriorSwrTimeStep: Integer;
  NumberOfReaches: Integer;
  NumberOfLayers: TIntegerDynArray;
  BinaryReader: TFileStream;
  ReaderFileSize: Int64;
  LayerIndex: Integer;
  AnInt: Integer;
  ADouble: double;
begin
  Data.Clear;
  case FileType of
    srtAscii:
      begin
        Splitter := TStringList.Create;
        try
          PriorStressPeriod := 0;
          PriorModflowTimeStep := 0;
          PriorSwrTimeStep := 0;
          TextReader := TFile.OpenText(FileName);
          try
            TextReader.ReadLine;
            ReachExchange := nil;
            while not TextReader.EndOfStream do
            begin
              Splitter.CommaText := TextReader.ReadLine;
              StressPeriod := StrToInt(Splitter[2]);
              ModflowTimeStep := StrToInt(Splitter[3]);
              SwrTimeStep := StrToInt(Splitter[4]);
              if (PriorStressPeriod <> StressPeriod)
                or (PriorModflowTimeStep <> ModflowTimeStep)
                or (PriorSwrTimeStep <> SwrTimeStep) then
              begin
                PriorStressPeriod := StressPeriod;
                PriorModflowTimeStep := ModflowTimeStep;
                PriorSwrTimeStep := SwrTimeStep;
                ReachExchange := TReachExchange.Create;
                Data.Add(ReachExchange);
                ReachExchange.TotalTime := FortranStrToFloat(Splitter[0]);
                ReachExchange.SwrTimeStepLength := FortranStrToFloat(Splitter[1]);
                ReachExchange.StressPeriod := StrToInt(Splitter[2]);
                ReachExchange.ModflowTimeStep := StrToInt(Splitter[3]);
                ReachExchange.SwrTimeStep := StrToInt(Splitter[4]);
              end
              else
              begin
                ADouble := FortranStrToFloat(Splitter[0]);
                Assert(ReachExchange.TotalTime = ADouble);
                ADouble := FortranStrToFloat(Splitter[1]);
                Assert(ReachExchange.SwrTimeStepLength = ADouble);
                Assert(ReachExchange.StressPeriod = StrToInt(Splitter[2]));
                Assert(ReachExchange.ModflowTimeStep = StrToInt(Splitter[3]));
                Assert(ReachExchange.SwrTimeStep = StrToInt(Splitter[4]));
              end;

              ReachExchange.ReachNumbers.Add(StrToInt(Splitter[5])-1);
              ReachExchange.ModelLayers.Add(StrToInt(Splitter[6]));
              ReachExchange.BottomElevations.Add(FortranStrToFloat(Splitter[7]));
              ReachExchange.ReachStages.Add(FortranStrToFloat(Splitter[8]));
              ReachExchange.ReachDepths.Add(FortranStrToFloat(Splitter[9]));
              ReachExchange.GroundwaterHead.Add(FortranStrToFloat(Splitter[10]));
              ReachExchange.WettedPerimeter.Add(FortranStrToFloat(Splitter[11]));
              ReachExchange.Conductance.Add(FortranStrToFloat(Splitter[12]));
              ReachExchange.HeadDifference.Add(FortranStrToFloat(Splitter[13]));
              ReachExchange.Aquifer_ReachFlow.Add(FortranStrToFloat(Splitter[14]));
            end;
          finally
            TextReader.Free;
          end;
        finally
          Splitter.Free;
        end;
      end;
    srtBinary:
      begin
        BinaryReader := TFile.OpenRead(FileName);
        try
          ReaderFileSize := BinaryReader.Size;

          BinaryReader.Read(NumberOfReaches, SizeOf(NumberOfReaches));

          while BinaryReader.Position < ReaderFileSize do
          begin
            SetLength(NumberOfLayers, NumberOfReaches);
            for ReachIndex := 0 to NumberOfReaches - 1 do
            begin
              BinaryReader.Read(NumberOfLayers[ReachIndex], SizeOf(integer));
            end;
            ReachExchange := TReachExchange.Create;
            Data.Add(ReachExchange);

            BinaryReader.Read(ReachExchange.TotalTime, SizeOf(ReachExchange.TotalTime));
            BinaryReader.Read(ReachExchange.SwrTimeStepLength, SizeOf(ReachExchange.SwrTimeStepLength));
            BinaryReader.Read(ReachExchange.StressPeriod, SizeOf(ReachExchange.StressPeriod));
            BinaryReader.Read(ReachExchange.ModflowTimeStep, SizeOf(ReachExchange.ModflowTimeStep));
            BinaryReader.Read(ReachExchange.SwrTimeStep, SizeOf(ReachExchange.SwrTimeStep));

            for ReachIndex := 0 to NumberOfReaches - 1 do
            begin
              for LayerIndex := 0 to NumberOfLayers[ReachIndex] - 1 do
              begin
                ReachExchange.ReachNumbers.Add(ReachIndex);

                BinaryReader.Read(AnInt, SizeOf(AnInt));
                ReachExchange.ModelLayers.Add(AnInt);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.BottomElevations.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.ReachStages.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.ReachDepths.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.GroundwaterHead.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.WettedPerimeter.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.Conductance.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.HeadDifference.Add(ADouble);

                BinaryReader.Read(ADouble, SizeOf(ADouble));
                ReachExchange.Aquifer_ReachFlow.Add(ADouble);
              end;
            end;
          end;
        finally
          BinaryReader.Free;
        end;
      end;
    else Assert(False);
  end;
end;

{ TReachExchange }

constructor TReachExchange.Create;
begin
  ReachNumbers := TGenericIntegerList.Create;
  ModelLayers := TGenericIntegerList.Create;
  BottomElevations := TList<double>.Create;
  ReachStages := TList<double>.Create;
  ReachDepths := TList<double>.Create;
  GroundwaterHead := TList<double>.Create;
  WettedPerimeter := TList<double>.Create;
  Conductance := TList<double>.Create;
  HeadDifference := TList<double>.Create;
  Aquifer_ReachFlow := TList<double>.Create;
end;

destructor TReachExchange.Destroy;
begin
  ReachNumbers.Free;
  ModelLayers.Free;
  BottomElevations.Free;
  ReachStages.Free;
  ReachDepths.Free;
  GroundwaterHead.Free;
  WettedPerimeter.Free;
  Conductance.Free;
  HeadDifference.Free;
  Aquifer_ReachFlow.Free;
  inherited;
end;

function TReachExchange.GetValue(SwrDataType, ReachIndex: Integer): double;
begin
  result := 0;
  case SwrDataType of
    0:
      begin
        result := BottomElevations[ReachIndex];
      end;
    1:
      begin
        result := ReachStages[ReachIndex];
      end;
    2:
      begin
        result := ReachDepths[ReachIndex];
      end;
    3:
      begin
        result := GroundwaterHead[ReachIndex];
      end;
    4:
      begin
        result := WettedPerimeter[ReachIndex];
      end;
    5:
      begin
        result := Conductance[ReachIndex];
      end;
    6:
      begin
        result := HeadDifference[ReachIndex];
      end;
    7:
      begin
        result := Aquifer_ReachFlow[ReachIndex];
      end;
    else
      Assert(False);
  end;
end;

{ TReachGroupWaterBudget }

constructor TReachGroupWaterBudget.Create;
begin
  ReachGroupStages := TList<double>.Create;
  InflowToConnected := TList<double>.Create;
  LateralFlow := TList<double>.Create;
  UzfInflow := TList<double>.Create;
  Rain := TList<double>.Create;
  Evaporation := TList<double>.Create;
  AquiferReachFlow := TList<double>.Create;
  OutflowToConnected := TList<double>.Create;
  FlowFromExternalSources := TList<double>.Create;
  StructureFlow := TList<double>.Create;
  ConstantReachFlow := TList<double>.Create;
  VolumeChange := TList<double>.Create;
  Discrepancy := TList<double>.Create;
  Volume := TList<double>.Create;
  ReachGroupNumbers := TGenericIntegerList.Create;

end;

destructor TReachGroupWaterBudget.Destroy;
begin
  ReachGroupStages.Free;
  InflowToConnected.Free;
  LateralFlow.Free;
  UzfInflow.Free;
  Rain.Free;
  Evaporation.Free;
  AquiferReachFlow.Free;
  OutflowToConnected.Free;
  FlowFromExternalSources.Free;
  StructureFlow.Free;
  ConstantReachFlow.Free;
  VolumeChange.Free;
  Discrepancy.Free;
  Volume.Free;
  ReachGroupNumbers.Free;

  inherited;
end;

function TReachGroupWaterBudget.GetValue(SwrDataType,
  ReachGroupIndex: Integer): double;
begin
  result := 0;
  case SwrDataType of
    0: result := ReachGroupStages[ReachGroupIndex];
    1: result := InflowToConnected[ReachGroupIndex];
    2: result := LateralFlow[ReachGroupIndex];
    3: result := UzfInflow[ReachGroupIndex];
    4: result := Rain[ReachGroupIndex];
    5: result := Evaporation[ReachGroupIndex];
    6: result := AquiferReachFlow[ReachGroupIndex];
    7: result := OutflowToConnected[ReachGroupIndex];
    8: result := FlowFromExternalSources[ReachGroupIndex];
    9: result := StructureFlow[ReachGroupIndex];
    10: result := ConstantReachFlow[ReachGroupIndex];
    11: result := VolumeChange[ReachGroupIndex];
    12: result := Discrepancy[ReachGroupIndex];
    13: result := Volume[ReachGroupIndex];
    else Assert(False);
  end;
end;

procedure ReadSwrReachGroupWaterBudgetData(const FileName: string;
  FileType: TSwrFileType; Data: TReachGroupWaterBudgets);
var
  Splitter: TStringList;
  TextReader: TStreamReader;
  PriorStressPeriod: Integer;
  PriorModflowTimeStep: Integer;
  PriorSwrTimeStep: Integer;
  StressPeriod: integer;
  ModflowTimeStep: integer;
  SwrTimeStep: integer;
  ReachGroupWaterBudget: TReachGroupWaterBudget;
  ADouble: double;
  BinaryReader: TFileStream;
  ReaderFileSize: Int64;
  NumberOfReachGroups: Integer;
  ReachGroupIndex: Integer;
begin
  Data.Clear;
  case FileType of
    srtAscii:
      begin
        Splitter := TStringList.Create;
        try
          PriorStressPeriod := 0;
          PriorModflowTimeStep := 0;
          PriorSwrTimeStep := 0;
          TextReader := TFile.OpenText(FileName);
          try
            TextReader.ReadLine;
            ReachGroupWaterBudget := nil;
            while not TextReader.EndOfStream do
            begin
              Splitter.CommaText := TextReader.ReadLine;
              StressPeriod := StrToInt(Splitter[2]);
              ModflowTimeStep := StrToInt(Splitter[3]);
              SwrTimeStep := StrToInt(Splitter[4]);
              if (PriorStressPeriod <> StressPeriod)
                or (PriorModflowTimeStep <> ModflowTimeStep)
                or (PriorSwrTimeStep <> SwrTimeStep) then
              begin
                PriorStressPeriod := StressPeriod;
                PriorModflowTimeStep := ModflowTimeStep;
                PriorSwrTimeStep := SwrTimeStep;
                ReachGroupWaterBudget := TReachGroupWaterBudget.Create;
                Data.Add(ReachGroupWaterBudget);
                ReachGroupWaterBudget.TotalTime := FortranStrToFloat(Splitter[0]);
                ReachGroupWaterBudget.SwrTimeStepLength := FortranStrToFloat(Splitter[1]);
                ReachGroupWaterBudget.StressPeriod := StrToInt(Splitter[2]);
                ReachGroupWaterBudget.ModflowTimeStep := StrToInt(Splitter[3]);
                ReachGroupWaterBudget.SwrTimeStep := StrToInt(Splitter[4]);
              end
              else
              begin
                ADouble := FortranStrToFloat(Splitter[0]);
                Assert(ReachGroupWaterBudget.TotalTime = ADouble);

                ADouble := FortranStrToFloat(Splitter[1]);
                Assert(ReachGroupWaterBudget.SwrTimeStepLength = ADouble);

                Assert(ReachGroupWaterBudget.StressPeriod = StrToInt(Splitter[2]));
                Assert(ReachGroupWaterBudget.ModflowTimeStep = StrToInt(Splitter[3]));
                Assert(ReachGroupWaterBudget.SwrTimeStep = StrToInt(Splitter[4]));
              end;
              ReachGroupWaterBudget.ReachGroupNumbers.Add(StrToInt(Splitter[5]));
              ReachGroupWaterBudget.ReachGroupStages.Add(FortranStrToFloat(Splitter[6]));
              ReachGroupWaterBudget.InflowToConnected.Add(FortranStrToFloat(Splitter[7]));
              ReachGroupWaterBudget.LateralFlow.Add(FortranStrToFloat(Splitter[8]));
              ReachGroupWaterBudget.UzfInflow.Add(FortranStrToFloat(Splitter[9]));
              ReachGroupWaterBudget.Rain.Add(FortranStrToFloat(Splitter[10]));
              ReachGroupWaterBudget.Evaporation.Add(FortranStrToFloat(Splitter[11]));
              ReachGroupWaterBudget.AquiferReachFlow.Add(FortranStrToFloat(Splitter[12]));
              ReachGroupWaterBudget.OutflowToConnected.Add(FortranStrToFloat(Splitter[13]));
              ReachGroupWaterBudget.FlowFromExternalSources.Add(FortranStrToFloat(Splitter[14]));
              ReachGroupWaterBudget.StructureFlow.Add(FortranStrToFloat(Splitter[15]));
              ReachGroupWaterBudget.ConstantReachFlow.Add(FortranStrToFloat(Splitter[16]));
              ReachGroupWaterBudget.VolumeChange.Add(FortranStrToFloat(Splitter[17]));
              ReachGroupWaterBudget.Discrepancy.Add(FortranStrToFloat(Splitter[18]));
              ReachGroupWaterBudget.Volume.Add(FortranStrToFloat(Splitter[19]));
            end;
          finally
            TextReader.Free;
          end;
        finally
          Splitter.Free;
        end;
      end;
    srtBinary:
      begin
        BinaryReader := TFile.OpenRead(FileName);
        try
          ReaderFileSize := BinaryReader.Size;

          BinaryReader.Read(NumberOfReachGroups, SizeOf(NumberOfReachGroups));

          while BinaryReader.Position < ReaderFileSize do
          begin
            ReachGroupWaterBudget := TReachGroupWaterBudget.Create;
            Data.Add(ReachGroupWaterBudget);

            BinaryReader.Read(ReachGroupWaterBudget.TotalTime, SizeOf(ReachGroupWaterBudget.TotalTime));
            BinaryReader.Read(ReachGroupWaterBudget.SwrTimeStepLength, SizeOf(ReachGroupWaterBudget.SwrTimeStepLength));
            BinaryReader.Read(ReachGroupWaterBudget.StressPeriod, SizeOf(ReachGroupWaterBudget.StressPeriod));
            BinaryReader.Read(ReachGroupWaterBudget.ModflowTimeStep, SizeOf(ReachGroupWaterBudget.ModflowTimeStep));
            BinaryReader.Read(ReachGroupWaterBudget.SwrTimeStep, SizeOf(ReachGroupWaterBudget.SwrTimeStep));

            ReachGroupWaterBudget.ReachGroupStages.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.InflowToConnected.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.LateralFlow.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.UzfInflow.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.Rain.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.Evaporation.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.AquiferReachFlow.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.OutflowToConnected.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.FlowFromExternalSources.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.StructureFlow.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.ConstantReachFlow.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.VolumeChange.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.Discrepancy.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.Volume.Capacity := NumberOfReachGroups;
            ReachGroupWaterBudget.ReachGroupNumbers.Capacity := NumberOfReachGroups;

            for ReachGroupIndex := 1 to NumberOfReachGroups do
            begin
              ReachGroupWaterBudget.ReachGroupNumbers.Add(ReachGroupIndex);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.ReachGroupStages.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.InflowToConnected.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.LateralFlow.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.UzfInflow.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.Rain.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.Evaporation.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.AquiferReachFlow.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.OutflowToConnected.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.FlowFromExternalSources.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.StructureFlow.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.ConstantReachFlow.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.VolumeChange.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.Discrepancy.Add(ADouble);

              BinaryReader.Read(ADouble, SizeOf(ADouble));
              ReachGroupWaterBudget.Volume.Add(ADouble);
            end;
          end;
        finally
          BinaryReader.Free;
        end;
      end;
    else
      Assert(False);
  end;
end;

{ TObsData }

constructor TObsData.Create;
begin
  FValues := TList<Double>.Create;
end;

destructor TObsData.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TObsData.GetValue(Index: Integer): Double;
begin
  result := FValues[Index];
end;

{ TObsDataList }

procedure TObsDataList.Clear;
begin
  FObsNames.Clear;
  inherited;
end;

constructor TObsDataList.Create;
begin
  inherited Create(True);
  FObsNames := TStringList.Create;
end;

destructor TObsDataList.Destroy;
begin
  FObsNames.Free;
  inherited;
end;

function TObsDataList.GetObsName(Index: Integer): string;
begin
  result := FObsNames[Index];
end;

function TObsDataList.GObsNameCount: Integer;
begin
  result := FObsNames.Count;
end;

function TObsDataList.IndexOfName(AnObsName: string): Integer;
begin
  result := FObsNames.IndexOf(AnObsName);
end;

procedure ReadSwrObsData(const FileName: string;
  FileType: TSwrFileType; Data: TObsDataList);
var
  Splitter: TStringList;
  TextReader: TStreamReader;
  BinaryReader: TFileStream;
  ReaderFileSize: Int64;
  ObsData: TObsData;
  NameIndex: integer;
  NumberOfObservations: Integer;
  ObsName: array[0..19] of AnsiChar;
  AValue: double;
begin
  Data.Clear;
  case FileType of
    srtAscii:
      begin
        Splitter := TStringList.Create;
        try
          TextReader := TFile.OpenText(FileName);
          try
            Splitter.CommaText := TextReader.ReadLine;
            while (Splitter.Count > 0) and (Trim(Splitter.Strings[Splitter.Count-1]) = '') do
            begin
              Splitter.Delete(Splitter.Count-1);
            end;
            if Splitter.Count = 0 then
            begin
              Beep;
              MessageDlg(StrTheFileContainsNo, mtError, [mbOK], 0);
              Exit;
            end;
            Data.FObsNames.Capacity := Splitter.Count - 1;
            for NameIndex := 1 to Splitter.Count - 1 do
            begin
              Data.FObsNames.Add(Splitter[NameIndex]);
            end;
            while not TextReader.EndOfStream do
            begin
              Splitter.CommaText := TextReader.ReadLine;
              ObsData := TObsData.Create;
              Data.Add(ObsData);
              ObsData.FValues.Capacity := Data.FObsNames.Count;
              ObsData.FTime := FortranStrToFloat(Splitter[0]);
              for NameIndex := 0 to Data.FObsNames.Count - 1 do
              begin
                ObsData.FValues.Add(FortranStrToFloat(Splitter[NameIndex+1]));
              end;
            end;
          finally
            TextReader.Free
          end;
        finally
          Splitter.Free;
        end;
      end;
    srtBinary:
      begin
        BinaryReader := TFile.OpenRead(FileName);
        try
          ReaderFileSize := BinaryReader.Size;
          BinaryReader.Read(NumberOfObservations, SizeOf(NumberOfObservations));
          if NumberOfObservations = 0 then
          begin
            Beep;
            MessageDlg(StrTheFileContainsNo, mtError, [mbOK], 0);
            Exit;
          end;
          Data.FObsNames.Capacity := NumberOfObservations;
          for NameIndex := 0 to NumberOfObservations - 1 do
          begin
            BinaryReader.Read(ObsName[0], SizeOf(ObsName));
            Data.FObsNames.Add(Trim(string(ObsName)));
          end;
          while BinaryReader.Position < ReaderFileSize do
          begin
            ObsData := TObsData.Create;
            Data.Add(ObsData);
            BinaryReader.Read(AValue, SizeOf(AValue));
            ObsData.FTime := AValue;
            ObsData.FValues.Capacity := NumberOfObservations;
            for NameIndex := 0 to NumberOfObservations - 1 do
            begin
              BinaryReader.Read(AValue, SizeOf(AValue));
              ObsData.FValues.Add(AValue);
            end;
          end;
        finally
          BinaryReader.Free;
        end;
      end;
  end;

end;

end.
