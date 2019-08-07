{#BACKUP WellFootprint.ico}

unit FootprintUnit;

interface

uses
  System.Classes, FootprintFileUnit, FootPrintUtilities,
  System.Generics.Collections, System.Generics.Defaults, System.SysUtils;

type
  TCellItem = class
    Col: Integer;
    Row: Integer;
    Distance: double;
    Weight: double;
  end;

  TCellItemObjectList = TObjectList<TCellItem>;

  TCellItemComparer = class(TComparer<TCellItem>)
    function Compare(const Left, Right: TCellItem): Integer; override;
  end;

  EFootprintGenTooMuchWithdrawal = class(Exception);
  EFootprintGenNoListingFile = class(Exception);

  TConsolWriteProcedure = procedure (const ALine: string);

  TFootPrintGenerator = class(TObject)
  private
    FSettings: TFootPrintFile;
    FCapacity: TTwoDRealArray;
    FNeighborCount: TTwoDIntArray;
    FActive: TTwoDBooleanArray;
    FCode: TTwoDIntArray;
    FListingFile: TStreamWriter;
    FConsolWriteProcedure: TConsolWriteProcedure;
    procedure CalculateCapacity;
    function GetCapacity: TTwoDRealArray;
    function GetWithdrawals: TTwoDRealArray;
    function GetNeighborCount: TTwoDIntArray;
    procedure SetUpNeighborCount;
    procedure IdentifyGroups(const PriorValues: TTwoDRealArray;
      var Groups: TTwoDIntArray; out GroupCount: Integer);
    procedure WriteArrayColTitles(MaxSpaceCount, ColumnCount: Integer;
      IndentSpaces: string; Writer: TStreamWriter);
    function GetFormatedNumber(Value,
      MaxSpaceCount, MaxNumberSize: Integer): string;
    function Indent(MaxSpaceCount: integer): string;
    procedure WriteStringArray(AnArray: TTwoDStringArray; Writer: TStreamWriter);
    procedure SaveAsciiResults(NewValues: TTwoDRealArray);
    procedure SaveBinaryResults(NewValues: TTwoDRealArray);
    procedure OnReadListingFileName(Sender: TObject);
    procedure DoReadInput(Sender: TObject; const Text: string);
    procedure WriteOutline;
    procedure DoWriteOutline(Sender: TObject);
    function SumArray(Values: TTwoDRealArray): double;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: string);
//    procedure SaveABinaryFile(FileName: string);

    procedure DoInitialRedistribution(var PriorValues: TTwoDRealArray);
    procedure RedistributeToNeighbors(IterationLimit: Integer;
      PriorValues: TTwoDRealArray; var NewValues: TTwoDRealArray;
      var MaxExcessFraction: Double; var IterationCount, TotalIterationCount: Integer);
    procedure RedistributeToPerimeter(ColLimit: Integer; RowLimit: Integer;
      PriorValues: TTwoDRealArray; var NewValues: TTwoDRealArray);
    procedure WriteRealArray(AnArray: TTwoDRealArray; Writer: TStreamWriter);
    procedure WriteIntegerArray(AnArray: TTwoDIntArray; Writer: TStreamWriter);
    function GenerateResults: Boolean;

    procedure SaveTextRealArray(AnArray: TTwoDRealArray; Writer: TStreamWriter; ArrayName: string);
    procedure SaveTextIntegerArray(AnArray: TTwoDIntArray; Writer: TStreamWriter; ArrayName: string);
    procedure SaveBinaryRealArray(AnArray: TTwoDRealArray; Writer: TFileStream; ArrayName: string);
    procedure SaveBinaryIntegerArray(AnArray: TTwoDIntArray; Writer: TFileStream; ArrayName: string);

    property Settings: TFootPrintFile read FSettings;
    property Capacity : TTwoDRealArray read GetCapacity;
    property Withdrawals : TTwoDRealArray read GetWithdrawals;
    property NeighborCount: TTwoDIntArray read GetNeighborCount;

    property ConsolWriteProcedure: TConsolWriteProcedure read FConsolWriteProcedure
      write FConsolWriteProcedure;
  end;

const
  Version = '1.0.1.0';

implementation

uses
  System.IOUtils, QuadtreeClass, System.Math, DisclaimerTextUnit;

resourcestring
  StrWarningTheFollowi = 'Warning: The following cells have Depth-Rate Indic' +
  'es less than or equal to zero. They will be treated as inactive.';
  StrRowCol0d1 = 'Row, Col = %0:d, %1:d';

{ TFootPrintGenerator }

procedure TFootPrintGenerator.CalculateCapacity;
var
  RowIndex: Integer;
  ColIndex: Integer;
  BadCapacity: Boolean;
begin
  BadCapacity := False;
  if FCapacity = nil then
  begin
    FCapacity := FSettings.DepthRateIndexArray;
    FActive := FSettings.ActiveArray;
    for RowIndex := 0 to FSettings.NumberOfRows - 1 do
    begin
      for ColIndex := 0 to FSettings.NumberOfColumns - 1 do
      begin
        if FActive[RowIndex, ColIndex] then
        begin
          if FCapacity[RowIndex, ColIndex] <= 0 then
          begin
            if not BadCapacity then
            begin
              BadCapacity := True;
              if FListingFile <> nil then
              begin
                FListingFile.WriteLine;
                FListingFile.WriteLine(StrWarningTheFollowi);
              end;
            end;
            if FListingFile <> nil then
            begin
              FListingFile.WriteLine(Format(StrRowCol0d1, [RowIndex+1, ColIndex+1]));
            end;
          end;
          FCapacity[RowIndex, ColIndex] :=
            FCapacity[RowIndex, ColIndex] * FSettings.CellSize;
        end
        else
        begin
          FCapacity[RowIndex, ColIndex] := 0;
        end;
      end;
    end;
    if BadCapacity then
    begin
      if FListingFile <> nil then
      begin
        FListingFile.WriteLine;
      end;
    end;
  end;
end;

constructor TFootPrintGenerator.Create;
begin
  FSettings := TFootPrintFile.Create;
  FSettings.OnReadListingFileName := OnReadListingFileName;
  FSettings.OnReadInput := DoReadInput;
  FSettings.OnReadOutline := DoWriteOutline;
end;

destructor TFootPrintGenerator.Destroy;
begin
  FSettings.Free;
  FListingFile.Free;
  inherited;
end;

procedure TFootPrintGenerator.DoInitialRedistribution(var PriorValues: TTwoDRealArray);
var
  RowCount: Integer;
  ColCount: Integer;
  QuadTree: TRbwQuadTree;
  Cells: TList<TCellItem>;
  RowIndex: Integer;
  ACellItem: TCellItem;
  ColIndex: Integer;
  NeighborHoods: TObjectList<TCellItemObjectList>;
  ANeighborhood: TCellItemObjectList;
  CellIndex: Integer;
  Points: TQuadPointArray;
  TotalWithdrawal: Double;
  SearchDistance: double;
  ItemIndex: Integer;
  NeighborCell: TCellItem;
  Neighbors: array of array of Boolean;
  NeighborIndex: Integer;
  TotalCapacity: double;
  Excess: Extended;
  EdgeDistance: Double;
  EdgeCount: Integer;
  type
    TGroupMethod = (gmInitial, gmCheck);
  function ShouldBeGrouped(RowIndex, ColIndex: integer; GroupMethod: TGroupMethod): Boolean;
  begin
    if GroupMethod = gmInitial then
    begin
      result := not Neighbors[RowIndex,ColIndex]
        and (FCapacity[RowIndex,ColIndex] > 0)
        and (SearchDistance > Sqrt(Sqr(RowIndex-ACellItem.Row)
          + Sqr(ColIndex-ACellItem.Col)));
    end
    else
    begin
      result := Neighbors[RowIndex,ColIndex]
//        and (FCapacity[RowIndex,ColIndex] > 0)
//        and (SearchDistance > Sqrt(Sqr(NeighborCell.Row-ACellItem.Row)
//          + Sqr(NeighborCell.Col-ACellItem.Col)));
    end;
  end;
  procedure InitializeNeighbors;
  var
    RowIndex: integer;
    ColIndex: Integer;
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColCount - 1 do
      begin
        Neighbors[RowIndex, ColIndex] := False;
      end;
    end;
    Neighbors[ACellItem.Row, ACellItem.Col] := True;
  end;
  procedure AddNeighbors(GroupMethod: TGroupMethod);
  var
    TotalCapacity: double;
    NewNeighbor: TCellItem;
    Comparer: TCellItemComparer;
    TestDistance: Double;
    CellIndex: Integer;
    DeleteIndex: Integer;
//    NextNeighbor: TCellItem;
  begin

    While ItemIndex < ANeighborhood.Count do
    begin
      NeighborCell := ANeighborhood[ItemIndex];

      if (NeighborCell.Row > 0)
        and ShouldBeGrouped(NeighborCell.Row-1, NeighborCell.Col, GroupMethod) then
      begin
        NewNeighbor := TCellItem.Create;
        NewNeighbor.Col := NeighborCell.Col;
        NewNeighbor.Row := NeighborCell.Row-1;
        NewNeighbor.Distance := Sqrt(Sqr(NewNeighbor.Row-ACellItem.Row)
          + Sqr(NewNeighbor.Col-ACellItem.Col));
        ANeighborhood.Add(NewNeighbor);
        Neighbors[NewNeighbor.Row, NewNeighbor.Col] := GroupMethod = gmInitial;
      end;
      if (NeighborCell.Col > 0)
         and ShouldBeGrouped(NeighborCell.Row, NeighborCell.Col-1, GroupMethod) then
      begin
        NewNeighbor := TCellItem.Create;
        NewNeighbor.Col := NeighborCell.Col-1;
        NewNeighbor.Row := NeighborCell.Row;
        NewNeighbor.Distance := Sqrt(Sqr(NewNeighbor.Row-ACellItem.Row)
          + Sqr(NewNeighbor.Col-ACellItem.Col));
        ANeighborhood.Add(NewNeighbor);
        Neighbors[NewNeighbor.Row, NewNeighbor.Col] := GroupMethod = gmInitial;
      end;
      if (NeighborCell.Row < RowCount-1)
        and ShouldBeGrouped(NeighborCell.Row+1, NeighborCell.Col, GroupMethod) then
      begin
        NewNeighbor := TCellItem.Create;
        NewNeighbor.Col := NeighborCell.Col;
        NewNeighbor.Row := NeighborCell.Row+1;
        NewNeighbor.Distance := Sqrt(Sqr(NewNeighbor.Row-ACellItem.Row)
          + Sqr(NewNeighbor.Col-ACellItem.Col));
        ANeighborhood.Add(NewNeighbor);
        Neighbors[NewNeighbor.Row, NewNeighbor.Col] := GroupMethod = gmInitial;
      end;
      if (NeighborCell.Col < ColCount-1)
        and ShouldBeGrouped(NeighborCell.Row, NeighborCell.Col+1, GroupMethod) then
      begin
        NewNeighbor := TCellItem.Create;
        NewNeighbor.Col := NeighborCell.Col+1;
        NewNeighbor.Row := NeighborCell.Row;
        NewNeighbor.Distance := Sqrt(Sqr(NewNeighbor.Row-ACellItem.Row)
          + Sqr(NewNeighbor.Col-ACellItem.Col));
        ANeighborhood.Add(NewNeighbor);
        Neighbors[NewNeighbor.Row, NewNeighbor.Col] := GroupMethod = gmInitial;
      end;
      Inc(ItemIndex);
    end;

    Comparer := TCellItemComparer.Create;
    try
      ANeighborhood.Sort(Comparer);
    finally
      Comparer.Free;
    end;

    TotalCapacity := 0;
    for CellIndex := 0 to ANeighborhood.Count - 1 do
    begin
      NeighborCell := ANeighborhood[CellIndex];
      TotalCapacity := TotalCapacity +
        FCapacity[NeighborCell.Row, NeighborCell.Col];
      if TotalCapacity > TotalWithdrawal then
      begin
        TestDistance := NeighborCell.Distance;
        for DeleteIndex := ANeighborhood.Count - 1 downto Max(CellIndex,1) do
        begin
          ANeighborhood.Delete(DeleteIndex);
        end;
        while (ANeighborhood.Count > 1)
          and (ANeighborhood.Last.Distance >= TestDistance) do
        begin
          ANeighborhood.Delete(ANeighborhood.Count-1);
        end;
        Break;
      end;
    end;
  end;
  procedure CheckNeighbors;
  var
    RowIndex: integer;
    ColIndex: Integer;
    CellIndex: integer;
    TestCellItem: TCellItem;
    FirstItem: TCellItem;
  begin
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColCount - 1 do
      begin
        Neighbors[RowIndex, ColIndex] := False;
      end;
    end;
    for CellIndex := 1 to ANeighborhood.Count - 1 do
    begin
      TestCellItem := ANeighborhood[CellIndex];
      Neighbors[TestCellItem.Row, TestCellItem.Col] := True;
    end;
    FirstItem := ANeighborhood[0];
    ANeighborhood.Extract(FirstItem);
    ANeighborhood.Clear;
    ANeighborhood.Add(FirstItem);
    ItemIndex := 0;
    AddNeighbors(gmCheck);
  end;
begin
  Capacity;
  RowCount := Length(PriorValues);
  ColCount := Length(PriorValues[0]);
  Assert(RowCount = Settings.NumberOfRows);
  Assert(ColCount = Settings.NumberOfColumns);
  TotalCapacity := 0;
  TotalWithdrawal := 0;
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      Assert(PriorValues[RowIndex,ColIndex] >= 0);
      Assert(FCapacity[RowIndex,ColIndex] >= 0);
      TotalCapacity := TotalCapacity + FCapacity[RowIndex,ColIndex];
      TotalWithdrawal := TotalWithdrawal + PriorValues[RowIndex,ColIndex];
    end;
  end;
  if TotalWithdrawal > TotalCapacity then
  begin
    raise EFootprintGenTooMuchWithdrawal.Create(Format(
      'The total withdrawals (%0:g) exceed the total capacity (%1:g)',
      [TotalWithdrawal, TotalCapacity]));
  end;
  if not FSettings.InitialDistribution then
  begin
    Exit;
  end;
  SetLength(Neighbors, RowCount, ColCount);
  QuadTree := TRbwQuadTree.Create(nil);
  Cells := TList<TCellItem>.Create;
  NeighborHoods := TObjectList<TCellItemObjectList>.Create;
  try
    QuadTree.XMin := 0;
    QuadTree.XMax := ColCount - 1;
    QuadTree.YMin := 0;
    QuadTree.YMax := RowCount - 1;
    for RowIndex := 0 to RowCount - 1 do
    begin
      for ColIndex := 0 to ColCount - 1 do
      begin
        if PriorValues[RowIndex,ColIndex] > 0 then
        begin
          ACellItem := TCellItem.Create;
          ACellItem.Row := RowIndex;
          ACellItem.Col := ColIndex;
          ANeighborhood := TCellItemObjectList.Create;
          ANeighborhood.Add(ACellItem);
          NeighborHoods.Add(ANeighborhood);
          Cells.Add(ACellItem);
          QuadTree.AddPoint(ACellItem.Col, ACellItem.Row, ACellItem);
        end;
      end;
    end;
    for CellIndex := 0 to Cells.Count - 1 do
    begin
      ACellItem := Cells[CellIndex];
      QuadTree.FindNearestPoints(ACellItem.Col, ACellItem.Row, 2, Points);
      if Length(Points) >= 2 then
      begin
        SearchDistance := Points[1].Distance/2;
      end
      else
      begin
        SearchDistance := MaxInt;
      end;
//      ACellItem.Generation := 1;
      TotalWithdrawal := PriorValues[ACellItem.Row,ACellItem.Col];
      ItemIndex := 0;
      ANeighborhood := Neighborhoods[CellIndex];
      InitializeNeighbors;
      AddNeighbors(gmInitial);
      CheckNeighbors;
      if ANeighborhood.Count > 1 then
      begin
        TotalCapacity := 0;
        EdgeDistance := ANeighborhood.Last.Distance - 1;
        EdgeCount := 0;
        for NeighborIndex := 0 to ANeighborhood.Count - 1 do
        begin
          NeighborCell := ANeighborhood[NeighborIndex];
          TotalCapacity := TotalCapacity +
            FCapacity[NeighborCell.Row, NeighborCell.Col];
          if NeighborCell.Distance > EdgeDistance then
          begin
            Inc(EdgeCount);
          end;
        end;
        Excess := (TotalWithdrawal-TotalCapacity)/EdgeCount;
        if EdgeCount = ANeighborhood.Count then
        begin
          PriorValues[ACellItem.Row,ACellItem.Col] :=
            FCapacity[ACellItem.Row, ACellItem.Col] + Excess;
          for NeighborIndex := 1 to ANeighborhood.Count - 1 do
          begin
            NeighborCell := ANeighborhood[NeighborIndex];
            PriorValues[NeighborCell.Row,NeighborCell.Col] :=
              PriorValues[NeighborCell.Row,NeighborCell.Col] +
              FCapacity[NeighborCell.Row, NeighborCell.Col] + Excess;
          end;
        end
        else
        begin
          PriorValues[ACellItem.Row,ACellItem.Col] :=
            FCapacity[ACellItem.Row, ACellItem.Col];
          for NeighborIndex := ANeighborhood.Count - 1 downto 1 do
          begin
            NeighborCell := ANeighborhood[NeighborIndex];
            if NeighborCell.Distance > EdgeDistance  then
            begin
              PriorValues[NeighborCell.Row,NeighborCell.Col] :=
                PriorValues[NeighborCell.Row,NeighborCell.Col] +
                FCapacity[NeighborCell.Row, NeighborCell.Col] + Excess;
            end
            else
            begin
              PriorValues[NeighborCell.Row,NeighborCell.Col] :=
                PriorValues[NeighborCell.Row,NeighborCell.Col] +
                FCapacity[NeighborCell.Row, NeighborCell.Col];
            end;
          end;
        end;
      end;
    end;
  finally
    Cells.Free;
    QuadTree.Free;
    NeighborHoods.Free;
  end;
end;

procedure TFootPrintGenerator.DoReadInput(Sender: TObject; const Text: string);
begin
  Assert(FListingFile <> nil);
  FListingFile.WriteLine(Text);
end;

procedure TFootPrintGenerator.DoWriteOutline(Sender: TObject);
begin
  WriteOutline;
end;

function TFootPrintGenerator.GenerateResults: Boolean;
var
  PriorValues: TTwoDRealArray;
  IterationLimit: Integer;
  NewValues: TTwoDRealArray;
  MaxExcessFraction: Double;
  IterationCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  TotalIterationCount: Integer;
begin
  result := False;
  if Settings.ListingFileName = '' then
  begin
     raise EFootprintGenNoListingFile.Create('Error: no Listing file specified.');
  end;
  try
    try
      FListingFile.WriteLine();

      PriorValues := Withdrawals;

      FListingFile.WriteLine('Withdrawals');
      WriteRealArray(PriorValues, FListingFile);
      FListingFile.WriteLine();

      FListingFile.WriteLine('Depth Rate Index');
      WriteRealArray(Settings.DepthRateIndexArray, FListingFile);
      FListingFile.WriteLine();

      Capacity;
      FListingFile.WriteLine('Capacity');
      WriteRealArray(Capacity, FListingFile);
      FListingFile.WriteLine();

      SetUpNeighborCount;
      FListingFile.WriteLine('Neighbor Count');
      WriteIntegerArray(FNeighborCount, FListingFile);
      FListingFile.WriteLine();


      DoInitialRedistribution(PriorValues);
      IterationLimit := Settings.MaxIterations;

      TotalIterationCount := 0;
      repeat
        RedistributeToNeighbors(IterationLimit,
          PriorValues, NewValues, MaxExcessFraction, IterationCount, TotalIterationCount);

        TotalIterationCount := TotalIterationCount + IterationCount;

        if MaxExcessFraction > Settings.ClosureCriterion then
        begin
          FListingFile.WriteLine('WARNING: FAILED TO MEET CLOSURE CRITERION');
          Writeln('WARNING: FAILED TO MEET CLOSURE CRITERION');
          WriteLn('Maximum excess fraction = ', MaxExcessFraction);
          WriteLn('If you want to continue, specify the number of additional iterations to try. Otherwise enter zero');
          Readln(IterationLimit);
          if IterationLimit <= 0 then
          begin
            Break;
          end;

          PriorValues := NewValues;
        end;
      until MaxExcessFraction <= Settings.ClosureCriterion;

      FListingFile.WriteLine('');

      FListingFile.Write('Number of iterations = ');
      FListingFile.WriteLine(TotalIterationCount);

      FListingFile.Write('Maximum excess fraction = ');
      FListingFile.WriteLine(MaxExcessFraction);

      SetLength(FCode, Settings.NumberOfRows, Settings.NumberOfColumns);
      for RowIndex := 0 to Settings.NumberOfRows - 1 do
      begin
        for ColIndex := 0 to Settings.NumberOfColumns - 1 do
        begin
          if not FActive[RowIndex, ColIndex] then
          begin
            FCode[RowIndex, ColIndex] := 0;
          end
          else if NewValues[RowIndex, ColIndex] = 0 then
          begin
            FCode[RowIndex, ColIndex] := 1;
          end
          else if NewValues[RowIndex, ColIndex] < FCapacity[RowIndex, ColIndex] then
          begin
            FCode[RowIndex, ColIndex] := 2;
          end
          else if NewValues[RowIndex, ColIndex] < FCapacity[RowIndex, ColIndex]
            * (1+Settings.ClosureCriterion) then
          begin
            FCode[RowIndex, ColIndex] := 3;
          end
          else
          begin
            FCode[RowIndex, ColIndex] := 4;
          end;
        end;
      end;


      if (Settings.AsciiFileName <> '') or (Settings.BinaryFileName <> '') then
      begin
        SaveAsciiResults(NewValues);
        SaveBinaryResults(NewValues);
      end
      else
      begin
        FListingFile.WriteLine();
        FListingFile.WriteLine('Distributed Withdrawals');
        WriteRealArray(NewValues, FListingFile);
        FListingFile.WriteLine();

        FListingFile.WriteLine('Footprint code');
        WriteIntegerArray(FCode, FListingFile);
        FListingFile.WriteLine();
      end;
    except on E: Exception do
      begin
        FListingFile.WriteLine('AN ERROR OCCURRED.');
        FListingFile.Write(E.ClassName);
        FListingFile.Write(': ');
        FListingFile.WriteLine(E.Message);
        raise;
      end;
    end;

    if MaxExcessFraction <= Settings.ClosureCriterion then
    begin
      FListingFile.WriteLine('normal termination');
      result := True;
    end

  finally
  end;
end;

function TFootPrintGenerator.GetCapacity: TTwoDRealArray;
begin
  CalculateCapacity;
  result := FCapacity;
end;

function TFootPrintGenerator.GetNeighborCount: TTwoDIntArray;
begin
  SetUpNeighborCount;
  result := FNeighborCount;
end;

function TFootPrintGenerator.GetWithdrawals: TTwoDRealArray;
begin
  Result := Settings.WithdrawalsArray;
end;

procedure TFootPrintGenerator.IdentifyGroups(const PriorValues: TTwoDRealArray;
  var Groups: TTwoDIntArray; out GroupCount: Integer);
var
  RowIndex: Integer;
  ColIndex: integer;
  CellList: TObjectQueue<TCellItem>;
  RowCount: Integer;
  ColCount: Integer;
  ACellItem: TCellItem;
  ARow: Integer;
  ACol: Integer;
  function ShouldBeGrouped(RowIndex, ColIndex: integer): Boolean;
  begin
    result := (Groups[RowIndex,ColIndex] = 0)
      and (PriorValues[RowIndex, ColIndex] >= FCapacity[RowIndex, ColIndex]);
  end;
  procedure GroupNeighbors(RowIndex, ColIndex: integer);
  begin
    if (RowIndex > 0) and ShouldBeGrouped(RowIndex-1, ColIndex) then
    begin
      ACellItem := TCellItem.Create;
      ACellItem.Col := ColIndex;
      ACellItem.Row := RowIndex-1;
      CellList.Enqueue(ACellItem);
      Groups[RowIndex-1,ColIndex] := GroupCount;
    end;
    if (ColIndex > 0) and ShouldBeGrouped(RowIndex, ColIndex-1) then
    begin
      ACellItem := TCellItem.Create;
      ACellItem.Col := ColIndex-1;
      ACellItem.Row := RowIndex;
      CellList.Enqueue(ACellItem);
      Groups[RowIndex,ColIndex-1] := GroupCount;
    end;
    if (RowIndex < RowCount-1) and ShouldBeGrouped(RowIndex+1, ColIndex) then
    begin
      ACellItem := TCellItem.Create;
      ACellItem.Col := ColIndex;
      ACellItem.Row := RowIndex+1;
      CellList.Enqueue(ACellItem);
      Groups[RowIndex+1,ColIndex] := GroupCount;
    end;
    if (ColIndex < ColCount-1) and ShouldBeGrouped(RowIndex, ColIndex+1) then
    begin
      ACellItem := TCellItem.Create;
      ACellItem.Col := ColIndex+1;
      ACellItem.Row := RowIndex;
      CellList.Enqueue(ACellItem);
      Groups[RowIndex,ColIndex+1] := GroupCount;
    end;
  end;
begin
  GroupCount := 0;
  RowCount := Length(PriorValues);
  ColCount := Length(PriorValues[0]);
  SetLength(Groups, RowCount, ColCount);
  for RowIndex := 0 to RowCount -1 do
  begin
    for ColIndex := 0 to ColCount - 1 do
    begin
      if FCapacity[RowIndex, ColIndex] > 0 then
      begin
        Groups[RowIndex,ColIndex] := 0;
      end
      else
      begin
        Groups[RowIndex,ColIndex] := -1;
      end;
    end;
  end;
  GroupCount := 1;
  CellList := TObjectQueue<TCellItem>.Create;
  try
    for RowIndex := 0 to Length(Groups) -1 do
    begin
      for ColIndex := 0 to Length(Groups[0]) - 1 do
      begin
        if ShouldBeGrouped(RowIndex, ColIndex) then
        begin
          Groups[RowIndex,ColIndex] := GroupCount;
          GroupNeighbors(RowIndex,ColIndex);
          while CellList.Count > 0 do
          begin
            ACellItem := CellList.Peek;
            ARow := ACellItem.Row;
            ACol := ACellItem.Col;
            CellList.Dequeue;
            GroupNeighbors(ARow, ACol);
          end;
          Inc(GroupCount);
        end;
      end;
    end;
  finally
    CellList.Free;
  end;
end;

procedure TFootPrintGenerator.LoadFromFile(FileName: string);
begin
  FSettings.LoadFromFile(FileName);
end;

procedure TFootPrintGenerator.OnReadListingFileName(Sender: TObject);
var
  LineIndex: Integer;
begin
  if Settings.ListingFileName = '' then
  begin
     raise EFootprintGenNoListingFile.Create('Error: no Listing file specified.');
  end;
  FListingFile := TFile.CreateText(Settings.ListingFileFullPath);
  FListingFile.WriteLine('WellFootprint');
  FListingFile.WriteLine('by Richard B. Winston and Daniel J. Goode');
  FListingFile.Write('Version = ');
  FListingFile.WriteLine(Version);
  FListingFile.Write('Date and Time = ');
  FListingFile.WriteLine(DateTimeToStr(Now));

  FListingFile.WriteLine();
  for LineIndex := 0 to Disclaimer.Count - 1 do
  begin
    FListingFile.WriteLine(Disclaimer[LineIndex]);
  end;
  FListingFile.WriteLine();

end;

procedure TFootPrintGenerator.RedistributeToNeighbors(IterationLimit: Integer;
  PriorValues: TTwoDRealArray; var NewValues: TTwoDRealArray;
  var MaxExcessFraction: Double; var IterationCount, TotalIterationCount: Integer);
var
  Excess: Double;
  DistributionCoefficient: Double;
  ExcessFraction: Double;
  IterationIndex: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  NewCellAdded: Boolean;
  NoCellsChangedCount: Integer;
  RowLimit: Integer;
  ColLimit: Integer;
begin
  MaxExcessFraction := 0;
  NoCellsChangedCount := 0;
  RowLimit := Settings.NumberOfRows -1;
  ColLimit := Settings.NumberOfColumns -1;
  Assert(Length(PriorValues) = Settings.NumberOfRows);
  Assert(Length(PriorValues[0]) = Settings.NumberOfColumns);
  NewValues := PriorValues;
  SetLength(NewValues, Settings.NumberOfRows, Settings.NumberOfColumns);
  SetUpNeighborCount;
  IterationCount := 0;
  for IterationIndex := 0 to IterationLimit - 1 do
  begin
    NewCellAdded := False;
    Inc(IterationCount);

    DistributionCoefficient := 1;
    MaxExcessFraction := 0;
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        if (FNeighborCount[RowIndex, ColIndex] > 0) then
        begin
          Excess := PriorValues[RowIndex, ColIndex]
            - FCapacity[RowIndex, ColIndex];
          if Excess > 0 then
          begin
            if FCapacity[RowIndex, ColIndex] > 0 then
            begin
              ExcessFraction := Excess / FCapacity[RowIndex, ColIndex];
              if ExcessFraction > MaxExcessFraction then
              begin
                MaxExcessFraction := ExcessFraction;
              end;
            end;
            Excess := Excess
              / (FNeighborCount[RowIndex, ColIndex] + DistributionCoefficient);
            NewValues[RowIndex, ColIndex] :=
              NewValues[RowIndex, ColIndex]
              - Excess * FNeighborCount[RowIndex, ColIndex];
            if (RowIndex > 0)
              and (FCapacity[RowIndex - 1, ColIndex] > 0) then
            begin
              if NewValues[RowIndex - 1, ColIndex] = 0 then
              begin
                NewCellAdded := True;
              end;
              NewValues[RowIndex - 1, ColIndex] :=
                NewValues[RowIndex - 1, ColIndex] + Excess;
            end;
            if (ColIndex > 0)
              and (FCapacity[RowIndex, ColIndex - 1] > 0) then
            begin
              if NewValues[RowIndex, ColIndex - 1] = 0 then
              begin
                NewCellAdded := True;
              end;
              NewValues[RowIndex, ColIndex - 1] :=
                NewValues[RowIndex, ColIndex - 1] + Excess;
            end;
            if (RowIndex < RowLimit)
              and (FCapacity[RowIndex + 1, ColIndex] > 0) then
            begin
              if NewValues[RowIndex + 1, ColIndex] = 0 then
              begin
                NewCellAdded := True;
              end;
              NewValues[RowIndex + 1, ColIndex] :=
                NewValues[RowIndex + 1, ColIndex] + Excess;
            end;
            if (ColIndex < ColLimit)
              and (FCapacity[RowIndex, ColIndex + 1] > 0) then
            begin
              if NewValues[RowIndex, ColIndex + 1] = 0 then
              begin
                NewCellAdded := True;
              end;
              NewValues[RowIndex, ColIndex + 1] :=
                NewValues[RowIndex, ColIndex + 1] + Excess;
            end;
          end;
        end;
      end;
    end;

    if NewCellAdded then
    begin
      NoCellsChangedCount := 0;
    end
    else
    begin
      Inc(NoCellsChangedCount);
    end;


    if (Settings.RedistributionCriterion > 0)
      and (NoCellsChangedCount >= Settings.RedistributionCriterion)
      and (MaxExcessFraction > Settings.ClosureCriterion) then
    begin
      if FListingFile <> nil then
      begin
        FListingFile.WriteLine('Redistributing excess pumpage to edges of groups on iteration '
           + IntToStr(IterationIndex+1 + TotalIterationCount));
      end;
      RedistributeToPerimeter(ColLimit, RowLimit, PriorValues, NewValues);
    end;

    PriorValues := NewValues;
    SetLength(NewValues, RowLimit + 1, ColLimit + 1);
    if MaxExcessFraction <= Settings.ClosureCriterion then
    begin
      break;
    end;
    if Assigned(FConsolWriteProcedure) then
    begin
      FConsolWriteProcedure(Format( 'Iteration %d'#13, [IterationIndex+1 + TotalIterationCount] ));
    end;
  end;
end;

//procedure TFootPrintGenerator.SaveABinaryFile(FileName: string);
//var
//  NumberOfRows: Integer;
//  NumberOfColumns: Integer;
//  RowIndex: Integer;
//  index: Integer;
//  ColIndex: integer;
//  FloatIndex: Double;
//  BinaryFile: TFileStream;
//begin
//  BinaryFile := TFile.Create(FileName);
//  try
//    NumberOfRows := 5;
//    NumberOfColumns := 6;
//    index := 101;
//    for RowIndex := 0 to NumberOfRows - 1 do
//    begin
//      for ColIndex := 0 to NumberOfColumns - 1 do
//      begin
//        FloatIndex := index;
//        BinaryFile.Write(FloatIndex, SizeOf(FloatIndex));
//        Inc(index);
//      end;
//    end;
//  finally
//    BinaryFile.Free;
//  end;
//end;

procedure TFootPrintGenerator.SaveBinaryIntegerArray(AnArray: TTwoDIntArray;
  Writer: TFileStream; ArrayName: string);
var
  NameLength: longint;
  ACount: LongInt;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  NameLength := Length(ArrayName);
  Writer.Write(NameLength, SizeOf(NameLength));
  Writer.Write(ArrayName[1], NameLength*SizeOf(Char));
  ACount := Length(AnArray);
  Writer.Write(ACount, SizeOf(ACount));
  if ACount > 0 then
  begin
    ACount := Length(AnArray[0]);
  end
  else
  begin
    ACount := 0;
  end;
  Writer.Write(ACount, SizeOf(ACount));
  for RowIndex := 0 to Length(AnArray) - 1 do
  begin
    for ColIndex := 0 to Length(AnArray[0]) - 1 do
    begin
      Writer.Write(AnArray[RowIndex, ColIndex], SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintGenerator.WriteOutline;
var
  OutlineStringList: TStringList;
  ANumber: string;
  PointIndex: Integer;
  MaxPointLength: Integer;
  NumIndex: Integer;
begin
  if Length(Settings.Outline) > 0 then
  begin
    OutlineStringList := TStringList.Create;
    try
      for PointIndex := 0 to Length(Settings.Outline) - 1 do
      begin
        OutlineStringList.Add(Format('%g', [Settings.Outline[PointIndex].x]));
        OutlineStringList.Add(Format('%g', [Settings.Outline[PointIndex].y]));
      end;
      MaxPointLength := 0;
      for NumIndex := 0 to OutlineStringList.Count - 1 do
      begin
        if Length(OutlineStringList[NumIndex]) > MaxPointLength then
        begin
          MaxPointLength := Length(OutlineStringList[NumIndex]);
        end;
      end;
      FListingFile.WriteLine;
      FListingFile.WriteLine('Model outline');
      FListingFile.Write(StringOfChar(' ', MaxPointLength div 2));
      FListingFile.Write('X');
      FListingFile.Write(StringOfChar(' ', MaxPointLength - ((MaxPointLength div 2) + 1)));
      FListingFile.Write(' ');
      FListingFile.Write(StringOfChar(' ', MaxPointLength div 2));
      FListingFile.Write('Y');
      FListingFile.Write(StringOfChar(' ', MaxPointLength - ((MaxPointLength div 2) + 1)));
      FListingFile.WriteLine;
      FListingFile.Write(StringOfChar('_', MaxPointLength));
      FListingFile.Write(' ');
      FListingFile.Write(StringOfChar('_', MaxPointLength));
      FListingFile.WriteLine;
      for NumIndex := 0 to OutlineStringList.Count - 1 do
      begin
        ANumber := OutlineStringList[NumIndex];
        if Odd(NumIndex) then
        begin
          // Y
          FListingFile.Write(' ');
        end;
        FListingFile.Write(StringOfChar(' ', MaxPointLength - Length(ANumber)));
        FListingFile.Write(ANumber);
        if Odd(NumIndex) then
        begin
          // Y
          FListingFile.WriteLine;
        end;
      end;
      FListingFile.WriteLine;
    finally
      OutlineStringList.Free;
    end;
  end;
end;

procedure TFootPrintGenerator.RedistributeToPerimeter(
  ColLimit: Integer; RowLimit: Integer;
  PriorValues: TTwoDRealArray; var NewValues: TTwoDRealArray);
const
  Epsilon = 1e-6;
var
  InternalCell: Boolean;
  EdgeCells: TCellItemObjectList;
  GroupIndex: Integer;
  InnerCells: TCellItemObjectList;
  AGroup: TCellItemObjectList;
  InternalCells: System.Generics.Collections.TObjectList<TCellItemObjectList>;
  Groups: TTwoDIntArray;
  CellIndex: Integer;
  ACellItem: TCellItem;
  ExternalCells: System.Generics.Collections.TObjectList<TCellItemObjectList>;
  TotalWeight: Double;
  EdgeCapacity: Double;
  GroupCount: Integer;
  Weight: Double;
  GroupExcesses: TOneDRealArray;
  RowIndex: Integer;
  ColIndex: Integer;
  Excess: Double;
  PriorSum: Double;
  FinalSum: Double;
begin
  PriorSum := SumArray(PriorValues);
  IdentifyGroups(NewValues, Groups, GroupCount);
  InternalCells := TObjectList<TCellItemObjectList>.Create;
  ExternalCells := TObjectList<TCellItemObjectList>.Create;
  try
    SetLength(GroupExcesses, GroupCount);
    for GroupIndex := 0 to GroupCount - 1 do
    begin
      InternalCells.Add(TCellItemObjectList.Create);
      ExternalCells.Add(TCellItemObjectList.Create);
      GroupExcesses[GroupIndex] := 0;
    end;
    for RowIndex := 0 to RowLimit do
    begin
      for ColIndex := 0 to ColLimit do
      begin
        GroupIndex := Groups[RowIndex, ColIndex];
        if GroupIndex > 0 then
        begin
          Excess := NewValues[RowIndex, ColIndex] - FCapacity[RowIndex, ColIndex];
          Assert(Excess >= 0);
          GroupExcesses[GroupIndex] := GroupExcesses[GroupIndex] + Excess;
          InternalCell := True;
          if (RowIndex > 0) and (Groups[RowIndex - 1, ColIndex] = 0) then
          begin
            InternalCell := False;
          end;
          if (ColIndex > 0) and (Groups[RowIndex, ColIndex - 1] = 0) then
          begin
            InternalCell := False;
          end;
          if (RowIndex < RowLimit) and (Groups[RowIndex + 1, ColIndex] = 0) then
          begin
            InternalCell := False;
          end;
          if (ColIndex < ColLimit) and (Groups[RowIndex, ColIndex + 1] = 0) then
          begin
            InternalCell := False;
          end;
          if InternalCell then
          begin
            AGroup := InternalCells[GroupIndex];
          end
          else
          begin
            AGroup := ExternalCells[GroupIndex];
          end;
          ACellItem := TCellItem.Create;
          ACellItem.Row := RowIndex;
          ACellItem.Col := ColIndex;
          AGroup.Add(ACellItem);
        end;
      end;
    end;
    for GroupIndex := 1 to GroupCount - 1 do
    begin
      Excess := GroupExcesses[GroupIndex];
      if Excess > 0 then
      begin
        EdgeCapacity := 0;
        TotalWeight := 0;
        EdgeCells := ExternalCells[GroupIndex];
        for CellIndex := 0 to EdgeCells.Count - 1 do
        begin
          ACellItem := EdgeCells[CellIndex];
          EdgeCapacity := EdgeCapacity + FCapacity[ACellItem.Row, ACellItem.Col];
          ACellItem.Weight := 0;
          if (ACellItem.Row > 0) and //                and ((Groups[ACellItem.Row - 1, ACellItem.Col] = GroupIndex)
          //                or (Groups[ACellItem.Row - 1, ACellItem.Col] = 0))
          //                and (FCapacity[ACellItem.Row-1,ACellItem.Col] > 0) then
          (Groups[ACellItem.Row - 1, ACellItem.Col] = GroupIndex) then
          begin
            Weight := PriorValues[ACellItem.Row - 1, ACellItem.Col] - PriorValues[ACellItem.Row, ACellItem.Col] + FCapacity[ACellItem.Row - 1, ACellItem.Col] - FCapacity[ACellItem.Row, ACellItem.Col];
//            if Weight > 0 then
            begin
              ACellItem.Weight := ACellItem.Weight + Weight;
            end;
            end;
          if (ACellItem.Col > 0) and //                and ((Groups[ACellItem.Row, ACellItem.Col - 1] = GroupIndex)
          //                or (Groups[ACellItem.Row, ACellItem.Col - 1] = 0))
          //                and (FCapacity[ACellItem.Row,ACellItem.Col-1] > 0) then
          (Groups[ACellItem.Row, ACellItem.Col - 1] = GroupIndex) then
          begin
            Weight := PriorValues[ACellItem.Row, ACellItem.Col - 1] - PriorValues[ACellItem.Row, ACellItem.Col] + FCapacity[ACellItem.Row, ACellItem.Col - 1] - FCapacity[ACellItem.Row, ACellItem.Col];
//            if Weight > 0 then
            begin
              ACellItem.Weight := ACellItem.Weight + Weight;
            end;
          end;
          if (ACellItem.Row < RowLimit) and //                and ((Groups[ACellItem.Row + 1, ACellItem.Col] = GroupIndex)
          //                or (Groups[ACellItem.Row + 1, ACellItem.Col] = 0))
          //                and (FCapacity[ACellItem.Row+1,ACellItem.Col] > 0) then
          (Groups[ACellItem.Row + 1, ACellItem.Col] = GroupIndex) then
          begin
            Weight := PriorValues[ACellItem.Row + 1, ACellItem.Col] - PriorValues[ACellItem.Row, ACellItem.Col] + FCapacity[ACellItem.Row + 1, ACellItem.Col] - FCapacity[ACellItem.Row, ACellItem.Col];
//            if Weight > 0 then
            begin
              ACellItem.Weight := ACellItem.Weight + Weight;
            end;
          end;
          if (ACellItem.Col < ColLimit) and //                and ((Groups[ACellItem.Row, ACellItem.Col + 1] = GroupIndex)
          //                or (Groups[ACellItem.Row, ACellItem.Col + 1] = 0))
          //                and (FCapacity[ACellItem.Row,ACellItem.Col+1] > 0) then
          (Groups[ACellItem.Row, ACellItem.Col + 1] = GroupIndex) then
          begin
            Weight := PriorValues[ACellItem.Row, ACellItem.Col + 1] - PriorValues[ACellItem.Row, ACellItem.Col] + FCapacity[ACellItem.Row, ACellItem.Col + 1] - FCapacity[ACellItem.Row, ACellItem.Col];
//            if Weight > 0 then
            begin
              ACellItem.Weight := ACellItem.Weight + Weight;
            end;
          end;
          if ACellItem.Weight < 0 then
          begin
            ACellItem.Weight := 0;
          end;
          TotalWeight := TotalWeight + ACellItem.Weight;
        end;
        Assert(TotalWeight >= 0);
        if TotalWeight <> 0 then
        begin
          for CellIndex := 0 to EdgeCells.Count - 1 do
          begin
            ACellItem := EdgeCells[CellIndex];
            //                if ACellItem.Weight > 0 then
            //                begin
            NewValues[ACellItem.Row, ACellItem.Col] := FCapacity[ACellItem.Row, ACellItem.Col] +
            //                + Excess * (FCapacity[ACellItem.Row,ACellItem.Col]/EdgeCapacity);
            //                + Excess * (FCapacity[ACellItem.Row,ACellItem.Col]/EdgeCapacity);
              Excess * (ACellItem.Weight / TotalWeight);
          end;
          InnerCells := InternalCells[GroupIndex];
          for CellIndex := 0 to InnerCells.Count - 1 do
          begin
            ACellItem := InnerCells[CellIndex];
            NewValues[ACellItem.Row, ACellItem.Col] := FCapacity[ACellItem.Row, ACellItem.Col];
          end;
        end;
      end;
    end;
    FinalSum := SumArray(NewValues);
    Assert(Abs(FinalSum-PriorSum)/(Abs(FinalSum)+Abs(PriorSum)) < Epsilon);

  finally
    ExternalCells.Free;
    InternalCells.Free;
  end;
end;

procedure TFootPrintGenerator.SaveBinaryResults(NewValues: TTwoDRealArray);
var
  Writer: TFileStream;
  NameLength: longint;
  ID: string;
  RealNumberLength: longint;
begin
  if Settings.BinaryFileName <> '' then
  begin
    Writer := TFile.Create(Settings.BinaryFileFullPath);
    try
      ID := StrFootprintBinaryFile;
      NameLength := Length(ID);
//      Writer.Write(NameLength, SizeOf(NameLength));
      Writer.Write(ID[1], NameLength*SizeOf(Char));

      RealNumberLength := SizeOf(Double);
      Writer.Write(RealNumberLength, SizeOf(RealNumberLength));

      SaveBinaryRealArray(NewValues, Writer, StrDistributedWithdraw);
      SaveBinaryIntegerArray(FCode, Writer, StrFootprintCode);
    finally
      Writer.Free;
    end;
  end;
end;

procedure TFootPrintGenerator.SaveAsciiResults(NewValues: TTwoDRealArray);
var
  Writer: TStreamWriter;
begin
  if Settings.AsciiFileName <> '' then
  begin
    Writer := TFile.CreateText(Settings.AsciiFileFullPath);
    try
      SaveTextRealArray(NewValues, Writer, StrDistributedWithdraw);
      SaveTextIntegerArray(FCode, Writer, StrFootprintCode);
    finally
      Writer.Free;
    end;
  end;
end;

procedure TFootPrintGenerator.SaveBinaryRealArray(AnArray: TTwoDRealArray;
  Writer: TFileStream; ArrayName: string);
var
  NameLength: longint;
  ACount: LongInt;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  NameLength := Length(ArrayName);
  Writer.Write(NameLength, SizeOf(NameLength));
  Writer.Write(ArrayName[1], NameLength*SizeOf(Char));
  ACount := Length(AnArray);
  Writer.Write(ACount, SizeOf(ACount));
  if ACount > 0 then
  begin
    ACount := Length(AnArray[0]);
  end
  else
  begin
    ACount := 0;
  end;
  Writer.Write(ACount, SizeOf(ACount));
  for RowIndex := 0 to Length(AnArray) - 1 do
  begin
    for ColIndex := 0 to Length(AnArray[0]) - 1 do
    begin
      Writer.Write(AnArray[RowIndex, ColIndex], SizeOf(AnArray[RowIndex, ColIndex]));
    end;
  end;
end;

procedure TFootPrintGenerator.SaveTextIntegerArray(AnArray: TTwoDIntArray;
  Writer: TStreamWriter; ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Writer.WriteLine(ArrayName);
  Writer.WriteLine(Length(AnArray));
  Writer.WriteLine(Length(AnArray[0]));
  for RowIndex := 0 to Length(AnArray) - 1 do
  begin
    for ColIndex := 0 to Length(AnArray[RowIndex]) - 1 do
    begin
      Writer.write(AnArray[RowIndex,ColIndex]);
      if ColIndex < Length(AnArray[RowIndex]) - 1 then
      begin
        Writer.write(', ');
      end;
    end;
    Writer.WriteLine;
  end;
end;
procedure TFootPrintGenerator.SaveTextRealArray(AnArray: TTwoDRealArray;
  Writer: TStreamWriter; ArrayName: string);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  Writer.WriteLine(ArrayName);
  Writer.WriteLine(Length(AnArray));
  Writer.WriteLine(Length(AnArray[0]));
  for RowIndex := 0 to Length(AnArray) - 1 do
  begin
    for ColIndex := 0 to Length(AnArray[RowIndex]) - 1 do
    begin
      Writer.write(AnArray[RowIndex,ColIndex]);
      if ColIndex < Length(AnArray[RowIndex]) - 1 then
      begin
        Writer.write(', ');
      end;
    end;
    Writer.WriteLine;
  end;
end;

procedure TFootPrintGenerator.SetUpNeighborCount;
var
  RowLimit: Integer;
  ColLimit: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  RowLimit := Settings.NumberOfRows -1;
  ColLimit := Settings.NumberOfColumns -1;
  Capacity;
  SetLength(FNeighborCount, RowLimit+1, ColLimit+1);
  for RowIndex := 0 to RowLimit do
  begin
    for ColIndex := 0 to ColLimit do
    begin
      FNeighborCount[RowIndex,ColIndex] := 0;
      if FCapacity[RowIndex,ColIndex] > 0 then
      begin
        if (RowIndex > 0)
          and (FCapacity[RowIndex-1,ColIndex] > 0) then
        begin
          FNeighborCount[RowIndex,ColIndex] :=
            FNeighborCount[RowIndex,ColIndex] + 1;
        end;
        if (ColIndex > 0)
          and (FCapacity[RowIndex,ColIndex-1] > 0) then
        begin
          FNeighborCount[RowIndex,ColIndex] :=
            FNeighborCount[RowIndex,ColIndex] + 1;
        end;
        if (RowIndex < RowLimit)
          and (FCapacity[RowIndex+1,ColIndex] > 0) then
        begin
          FNeighborCount[RowIndex,ColIndex] :=
            FNeighborCount[RowIndex,ColIndex] + 1;
        end;
        if (ColIndex < ColLimit)
          and (FCapacity[RowIndex,ColIndex+1] > 0) then
        begin
          FNeighborCount[RowIndex,ColIndex] :=
            FNeighborCount[RowIndex,ColIndex] + 1;
        end;
      end;
    end;
  end;
end;

function TFootPrintGenerator.SumArray(Values: TTwoDRealArray): double;
var
  OuterIndex: Integer;
  InnerIndex: Integer;
begin
  result := 0;
  for OuterIndex := 0 to Length(Values) - 1 do
  begin
    for InnerIndex := 0 to Length(Values[0]) - 1 do
    begin
      result := result + Values[OuterIndex,InnerIndex];
    end;
  end;
end;

procedure TFootPrintGenerator.WriteIntegerArray(AnArray: TTwoDIntArray;
  Writer: TStreamWriter);
var
  RowCount: Integer;
  ColumnCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  StringArray: TTwoDStringArray;
begin
  RowCount := Length(AnArray);
  ColumnCount := Length(AnArray[0]);
  SetLength(StringArray, RowCount, ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      StringArray[RowIndex,ColIndex] :=
        Format('%d', [AnArray[RowIndex,ColIndex]]);
    end;
  end;
  WriteStringArray(StringArray, Writer);
end;

function TFootPrintGenerator.GetFormatedNumber(
  Value, MaxSpaceCount, MaxNumberSize: Integer): string;
var
  NumberLine: TStringBuilder;
  PriorSpaceCount: Integer;
begin
  result := Format('%d', [Value]);
  NumberLine := TStringBuilder.Create;
  PriorSpaceCount := (MaxSpaceCount + (MaxNumberSize - Length(result))) div 2;
  try
    NumberLine.Append(' ', PriorSpaceCount);// - Length(result));
    NumberLine.Append(result);
    NumberLine.Append(' ', MaxSpaceCount-PriorSpaceCount - Length(result));
    result := NumberLine.ToString;
  finally
    NumberLine.Free;
  end;
end;

function TFootPrintGenerator.Indent(MaxSpaceCount: integer): string;
begin
  result := StringOfChar(' ', MaxSpaceCount);
end;

procedure TFootPrintGenerator.WriteArrayColTitles(MaxSpaceCount,
  ColumnCount: Integer; IndentSpaces: string; Writer: TStreamWriter);
var
  ColIndex: Integer;
  ANumber: string;
  NumberCount: Integer;
begin
  Writer.Write(Indent(MaxSpaceCount));
  ANumber := Format('%d', [ColumnCount]);
  NumberCount := Length(ANumber);
  for ColIndex := 1 to ColumnCount do
  begin
    ANumber := GetFormatedNumber(ColIndex, MaxSpaceCount, NumberCount);
    if (Odd(NumberCount)) and (ANumber[Length(ANumber)] = ' ') then
    begin
      ANumber := ' ' + Copy(ANumber,1,Length(ANumber)-1);
    end;
    Writer.Write(ANumber);
    if ((ColIndex mod 10) = 0) or (ColIndex = ColumnCount) then
    begin
      Writer.WriteLine;
      if ColIndex <> ColumnCount then
      begin
        Writer.Write(IndentSpaces);
      end;
    end;
  end;
  Writer.Write(IndentSpaces);
  for ColIndex := 1 to Min(ColumnCount, 10) do
  begin
    Writer.Write(' ');
    Writer.Write(StringOfChar('_', Length(IndentSpaces) - 1));
  end;
  Writer.WriteLine;
end;

procedure TFootPrintGenerator.WriteRealArray(AnArray: TTwoDRealArray;
  Writer: TStreamWriter);
var
  RowCount: Integer;
  ColumnCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  StringArray: TTwoDStringArray;
begin
  RowCount := Length(AnArray);
  ColumnCount := Length(AnArray[0]);
  SetLength(StringArray, RowCount, ColumnCount);
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      StringArray[RowIndex,ColIndex] :=
        Format('%g', [AnArray[RowIndex,ColIndex]]);
    end;
  end;
  WriteStringArray(StringArray, Writer);
end;

procedure TFootPrintGenerator.WriteStringArray(AnArray: TTwoDStringArray;
  Writer: TStreamWriter);
var
  MaxSpaceCount: Integer;
  RowCount: Integer;
  ColumnCount: Integer;
  RowIndex: Integer;
  ColIndex: Integer;
  ANumber: string;
  NumberWriter: TStringBuilder;
  IndentSpaces: string;
  NumberCount: Integer;
  ValueLength: Integer;
begin
  RowCount := Length(AnArray);
  ColumnCount := Length(AnArray[0]);
  ANumber := Format('%d', [RowCount]);
  NumberCount := Length(ANumber);
  MaxSpaceCount := NumberCount;
  for RowIndex := 0 to RowCount - 1 do
  begin
    for ColIndex := 0 to ColumnCount - 1 do
    begin
      ValueLength := Length(AnArray[RowIndex,ColIndex]);
      if ValueLength > MaxSpaceCount then
      begin
        MaxSpaceCount := ValueLength;
      end;
    end;
  end;
  Inc(MaxSpaceCount);
  NumberWriter := TStringBuilder.Create;
  IndentSpaces := Indent(MaxSpaceCount);
  WriteArrayColTitles(MaxSpaceCount, ColumnCount, IndentSpaces, Writer);
  try
    for RowIndex := 1 to RowCount do
    begin
      ANumber := GetFormatedNumber(RowIndex, MaxSpaceCount, NumberCount);
      Writer.Write(ANumber);
      for ColIndex := 1 to ColumnCount do
      begin
        ANumber := AnArray[RowIndex-1,ColIndex-1];
        NumberWriter.Append(' ', MaxSpaceCount - Length(ANumber));
        NumberWriter.Append(ANumber);
        Writer.Write(NumberWriter.ToString);
        NumberWriter.Clear;
        if ((ColIndex mod 10) = 0) or (ColIndex = ColumnCount) then
        begin
          Writer.WriteLine;
          if ColIndex <> ColumnCount then
          begin
            Writer.Write(IndentSpaces);
          end;
        end;
      end;
    end;
  finally
    NumberWriter.Free;
  end;
end;

{ TCellItemComparer }

function TCellItemComparer.Compare(const Left, Right: TCellItem): Integer;
begin
  result := Sign(Left.Distance - Right.Distance);
end;

end.
