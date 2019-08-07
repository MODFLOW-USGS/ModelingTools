unit FlowObs;

interface

uses
  System.Generics.Collections, System.Classes, System.SysUtils;

type
  EFlowReader = class(Exception);

  TCustomCell = class(TObject)
  private
    FOrder: Integer;
    FFactor: double;
  public
    property Order: Integer read FOrder write FOrder;
    property Factor: double read FFactor write FFactor;
  end;

  TStructuredCell = class(TCustomCell)
  private
    // A value of -1 @name indicates that the cell can be on any layer.
    // This is useful for the SFR package where the actual layer can be the
    // highest active layer rather than the specified layer.
    FLayer: Integer;
    FRow: Integer;
    FColumn: Integer;
  public
    property Layer: Integer read FLayer write FLayer;
    property Row: Integer read FRow write FRow;
    property Column: Integer read FColumn write FColumn;
  end;

  TUnstructuredCell = class(TCustomCell)
    FCellNumber: Integer;
  public
    property CellNumber: Integer read FCellNumber write FCellNumber;
  end;

  TObservation = class(TObject)
    OBSNAM: string;
    ReferenceStressPeriod: Integer;
    TimeOffset: Double;
    ObservedValue: double;
    Time: double;
    SimulatedValue: double;

  end;

  TCustomFlowGroup = class(TObject)
  private
    FObservationTimes: TObjectList<TObservation>;
    FUniformFactor: Boolean;
    FObsLabel: string;
    function GetCellCapacity: Integer; virtual; abstract;
    procedure SetCellCapacity(Value: Integer); virtual; abstract;
  public
    FTimeIndex: integer;
    constructor Create;
    destructor Destroy; override;
    procedure ReadCells(InputFile: TStreamReader;
      LineSplitter: TStringList; OutputFile: TStreamWriter); virtual; abstract;
    property ObservationTimes: TObjectList<TObservation> read FObservationTimes;
    property CellCapacity: Integer read GetCellCapacity write SetCellCapacity;
    property UniformFactor: Boolean read FUniformFactor write FUniformFactor;
    // @name is the label in the cell-by-cell flow file for this type of
    // observation.
    property ObsLabel: string read FObsLabel write FObsLabel;
  end;

  TFlowList = class(TList<TCustomFlowGroup>)
    function ObsLabel: string;
  end;

  TFlowLists = class(TObjectList<TFlowList>)
    function IndexOfLabel(const ALabel: string): integer;
  end;

  TStructuredFlowGroup = class(TCustomFlowGroup)
  private
    FCells: TObjectList<TStructuredCell>;
    function GetCellCapacity: Integer; override;
    procedure SetCellCapacity(Value: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadCells(InputFile: TStreamReader;
      LineSplitter: TStringList; OutputFile: TStreamWriter); override;
    property Cells: TObjectList<TStructuredCell> read FCells;
  end;

  TUnstructuredFlowGroup = class(TCustomFlowGroup)
  private
    FCells: TObjectList<TUnstructuredCell>;
    function GetCellCapacity: Integer; override;
    procedure SetCellCapacity(Value: Integer); override;
  public
    constructor Create;
    destructor Destroy; override;
    procedure ReadCells(InputFile: TStreamReader;
      LineSplitter: TStringList; OutputFile: TStreamWriter); override;
    property Cells: TObjectList<TUnstructuredCell> read FCells;
  end;

implementation

{ TCustomFlowGroup }

constructor TCustomFlowGroup.Create;
begin
  FObservationTimes := TObjectList<TObservation>.Create;
end;

destructor TCustomFlowGroup.Destroy;
begin
  FObservationTimes.Free;
  inherited;
end;

{ TStructuredFlowGroup }

constructor TStructuredFlowGroup.Create;
begin
  inherited;
  FCells := TObjectList<TStructuredCell>.Create;
end;

destructor TStructuredFlowGroup.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TStructuredFlowGroup.GetCellCapacity: Integer;
begin
  result := FCells.Capacity;
end;

procedure TStructuredFlowGroup.ReadCells(InputFile: TStreamReader;
  LineSplitter: TStringList; OutputFile: TStreamWriter);
var
  CellIndex: Integer;
  Layer: Integer;
  Row: integer;
  ACell: TStructuredCell;
  Column: Integer;
  Order: Integer;
  Factor: double;
  OutputLine: TStringBuilder;
  Blanks: TCharArray;
  Index: Integer;
begin
  SetLength(Blanks, 10);
  for Index := 0 to Length(Blanks) - 1 do
  begin
    Blanks[Index] := ' ';
  end;
  OutputFile.WriteLine('Layer     Row       Column    Order     Factor');
  OutputFile.WriteLine('--------  --------  --------  --------  --------');

  OutputLine := TStringBuilder.Create;
  try
    for CellIndex := 0 to Cells.Capacity - 1 do
    begin
      OutputLine.Clear;
      LineSplitter.DelimitedText := InputFile.ReadLine;
      if FUniformFactor then
      begin
        if LineSplitter.Count < 4 then
        begin
          raise EFlowReader.Create(
            'Error reading data set 5. Too few input variables.');
        end;
      end
      else
      begin
        if LineSplitter.Count < 5 then
        begin
          raise EFlowReader.Create(
            'Error reading data set 5. Too few input variables.');
        end;
      end;
      ACell := TStructuredCell.Create;
      Cells.Add(ACell);

      if TryStrToInt(LineSplitter[0], Layer) then
      begin
        ACell.Layer := Layer;
        OutputLine.Append(Layer);
        OutputLine.Append(Blanks, 0, 10-OutputLine.Length);
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading Layer in data set 5: %s.', [LineSplitter[0]]));
      end;

      if TryStrToInt(LineSplitter[1], Row) then
      begin
        ACell.Row := Row;
        OutputLine.Append(Row);
        OutputLine.Append(Blanks, 0, 20-OutputLine.Length);
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading Row in data set 5: %s.', [LineSplitter[1]]));
      end;

      if TryStrToInt(LineSplitter[2], Column) then
      begin
        ACell.Column := Column;
        OutputLine.Append(Column);
        OutputLine.Append(Blanks, 0, 30-OutputLine.Length);
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading Column in data set 5: %s.', [LineSplitter[2]]));
      end;

      if TryStrToInt(LineSplitter[3], Order) then
      begin
        ACell.Order := Order;
        OutputLine.Append(Order);
        OutputLine.Append(Blanks, 0, 40-OutputLine.Length);
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading Order in data set 5: %s.', [LineSplitter[3]]));
      end;

      if FUniformFactor then
      begin
        ACell.Factor := 1;
      end
      else
      begin
        if TryStrToFloat(LineSplitter[4], Factor) then
        begin
          ACell.Factor := Factor
        end
        else
        begin
          raise EFlowReader.Create(Format(
            'Error reading Factor in data set 5: %s.', [LineSplitter[4]]));
        end;
      end;
      OutputLine.Append(ACell.Factor);
      OutputLine.Append(Blanks, 0, 50-OutputLine.Length);
      OutputFile.WriteLine(OutputLine.ToString);
    end;
  finally
    OutputLine.Free;
  end;
  OutputFile.WriteLine('');
end;

procedure TStructuredFlowGroup.SetCellCapacity(Value: Integer);
begin
  FCells.Capacity := Value;
end;

{ TUnstructuredFlowGroup }

constructor TUnstructuredFlowGroup.Create;
begin
  inherited;
  FCells := TObjectList<TUnstructuredCell>.Create;
end;

destructor TUnstructuredFlowGroup.Destroy;
begin
  FCells.Free;
  inherited;
end;

function TUnstructuredFlowGroup.GetCellCapacity: Integer;
begin
  result := FCells.Capacity;
end;

procedure TUnstructuredFlowGroup.ReadCells(InputFile: TStreamReader;
  LineSplitter: TStringList; OutputFile: TStreamWriter);
var
  CellIndex: Integer;
  ACell: TUnstructuredCell;
  Order: Integer;
  Factor: double;
  CellNumber: Integer;
begin
  for CellIndex := 0 to Cells.Capacity - 1 do
  begin
    LineSplitter.DelimitedText := InputFile.ReadLine;
    if FUniformFactor then
    begin
      if LineSplitter.Count < 2 then
      begin
        raise EFlowReader.Create(
          'Error reading data set 6. Too few input variables.');
      end;
    end
    else
    begin
      if LineSplitter.Count < 3 then
      begin
        raise EFlowReader.Create(
          'Error reading data set 6. Too few input variables.');
      end;
    end;
    ACell := TUnstructuredCell.Create;
    Cells.Add(ACell);

    if TryStrToInt(LineSplitter[0], CellNumber) then
    begin
      ACell.CellNumber := CellNumber
    end
    else
    begin
      raise EFlowReader.Create(Format(
        'Error reading Layer in data set 6: %s.', [LineSplitter[0]]));
    end;

    if TryStrToInt(LineSplitter[1], Order) then
    begin
      ACell.Order := Order
    end
    else
    begin
      raise EFlowReader.Create(Format(
        'Error reading Order in data set 6: %s.', [LineSplitter[1]]));
    end;

    if FUniformFactor then
    begin
      ACell.Factor := 1;
    end
    else
    begin
      if TryStrToFloat(LineSplitter[2], Factor) then
      begin
        ACell.Factor := Factor
      end
      else
      begin
        raise EFlowReader.Create(Format(
          'Error reading Factor in data set 6: %s.', [LineSplitter[2]]));
      end;
    end;
  end;
end;

procedure TUnstructuredFlowGroup.SetCellCapacity(Value: Integer);
begin
  FCells.Capacity := Value;
end;

{ TFlowList }

function TFlowList.ObsLabel: string;
begin
  if Count > 0 then
  begin
    Result := Items[0].ObsLabel
  end
  else
  begin
    Result := '';
  end;
end;

{ TFlowLists }

function TFlowLists.IndexOfLabel(const ALabel: string): integer;
var
  index: Integer;
begin
  Result := -1;
  for index := 0 to Count - 1 do
  begin
    if Items[index].ObsLabel = ALabel then
    begin
      Result := index;
      Exit;
    end;
  end;
end;

end.
