unit PhastZoneUnit;

interface

uses Classes, BasicTypes, DataSetUnit, PhastDataSets;

Type
  TPropertyType = (ptReal, ptInteger, ptBoolean);

  TProperty = class(TObject)
  private
    FPhastInterpolationValues: TPhastInterpolationValues;
    function GetIValue: integer; virtual;
    function GetRValue: double; virtual;
    procedure SetIValue(const Value: integer); virtual;
    procedure SetRValue(const Value: double); virtual;
    function GetBValue: boolean; virtual;
    procedure SetBValue(const Value: boolean); virtual;
    procedure SetPhastInterpolationValues(
      const Value: TPhastInterpolationValues);
  public
    function DataType: TPropertyType; virtual; abstract;
    Property IValue: integer read GetIValue write SetIValue;
    Property RValue: double read GetRValue write SetRValue;
    property BValue: boolean read GetBValue write SetBValue;                                                                   
    property Interpolation: TPhastInterpolationValues
      read FPhastInterpolationValues write SetPhastInterpolationValues;
    Function IsIdentical(AProperty: TProperty): boolean; virtual; abstract;
    constructor Create;
    destructor Destroy; override;
  end;

  TIProperty = class(TProperty)
  private
    FIValue: integer;
    function GetIValue: integer; Override;
    procedure SetIValue(const Value: integer); Override;
    procedure Assign(const DataSet: TDataSet; const LayerIndex, RowIndex, ColIndex: integer);
  public
    function DataType: TPropertyType; Override;
    Function IsIdentical(AProperty: TProperty): boolean; Override;
  end;

  TRProperty = class(TProperty)
  private
    FRValue: double;
    function GetRValue: double; Override;
    procedure SetRValue(const Value: double); Override;
    procedure Assign(const DataSet: TDataSet; const LayerIndex, RowIndex, ColIndex: integer);
  public
    function DataType: TPropertyType; Override;
    Function IsIdentical(AProperty: TProperty): boolean; Override;
  end;

  TBProperty = class(TProperty)
  private
    FBValue: boolean;
    function GetBValue: boolean; Override;
    procedure SetBValue(const Value: boolean); Override;
    procedure Assign(const DataSet: TDataSet; const LayerIndex, RowIndex, ColIndex: integer);
  public
    function DataType: TPropertyType; Override;
    Function IsIdentical(AProperty: TProperty): boolean; Override;
  end;

  TPhastZone = class;
  TZoneClass = class of TPhastZone;

  TDataSets = class(TObject)
  private
    EvalAt: TEvaluatedAt;
    ZoneClass: TZoneClass;
  end;

  TMediaDataSets = class(TDataSets)
  private
    Kx: TDataSet;
    Ky: TDataSet;
    Kz: TDataSet;
    Porosity: TDataSet;
    SpecificStorage: TDataSet;
    LongDisp: TDataSet;
    HorzTransDisp: TDataSet;
    VertTransDisp: TDataSet;
  public
    Constructor Create;
  end;

  TActiveDataSets = class(TDataSets)
  private
    Active: TDataSet;
  public
    Constructor Create;
  end;

  TInitialHeadDataSets = class(TDataSets)
  private
    InitialHead: TDataSet;
  public
    Constructor Create;
  end;

  TPhastZone = class(TObject)
  protected
    PropertyList: TList;
    Function PropertiesAreIdentical(AZone: TPhastZone): boolean;
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TDataSets); virtual;
  public
    X1, X2: double;
    Y1, Y2: double;
    Z1, Z2: double;
    Constructor Create; virtual;
    Destructor Destroy; override;
  end;

  TMediaZone = class(TPhastZone)
  protected
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TDataSets); override;
  public
    Kx: TRProperty;
    Ky: TRProperty;
    Kz: TRProperty;
    Porosity: TRProperty;
    SpecificStorage: TRProperty;
    LongDisp: TRProperty;
    HorzTransDisp: TRProperty;
    VertTransDisp: TRProperty;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TActiveZone = class(TPhastZone)
  protected
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TDataSets); override;
  public
    Active: TBProperty;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TInitialHeadZone = class(TPhastZone)
  protected
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex: integer;
      const DataSets: TDataSets); override;
  public
    InitialHead: TRProperty;
    Constructor Create; override;
    Destructor Destroy; override;
  end;

  TZoneGroup = class(TObject)
  private
    FinalZones: TList;
    function GetZone(Index: integer): TPhastZone;
  public
    Constructor Create(const DataSets: TDataSets);
    Destructor Destroy; override;
    property Zones[Index: integer]: TPhastZone read GetZone;
    function ZoneCount: integer;
  end;

  TMediaZoneGroup = class(TZoneGroup)
  private
    function GetZone(Index: integer): TMediaZone;
  public
    Constructor Create;
    property Zones[Index: integer]: TMediaZone read GetZone;
  end;

  TActiveZoneGroup = class(TZoneGroup)
  private
    function GetZone(Index: integer): TActiveZone;
  public
    Constructor Create;
    property Zones[Index: integer]: TActiveZone read GetZone;
  end;

  TInitialHeadZoneGroup = class(TZoneGroup)
  private
    function GetZone(Index: integer): TInitialHeadZone;
  public
    Constructor Create;
    property Zones[Index: integer]: TInitialHeadZone read GetZone;
  end;

implementation

uses Contnrs, frmGoPhastUnit, ModelUnit;

{ TIProperty }

procedure TIProperty.Assign(const DataSet: TDataSet; const LayerIndex,
  RowIndex, ColIndex: integer);
var
  PhastDataSet: TIntegerPhastDataSet;
begin
  PhastDataSet := DataSet as TIntegerPhastDataSet;
  Interpolation.UsePHAST_Interpolation := PhastDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
  if not Interpolation.UsePHAST_Interpolation then
  begin
    IValue := DataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
  end
  else
  begin
    Interpolation.Distance1 := PhastDataSet.CellDistance1[LayerIndex, RowIndex, ColIndex];
    Interpolation.Distance2 := PhastDataSet.CellDistance2[LayerIndex, RowIndex, ColIndex];
    Interpolation.IntValue1 := PhastDataSet.CellValue1[LayerIndex, RowIndex, ColIndex];
    Interpolation.IntValue2 := PhastDataSet.CellValue2[LayerIndex, RowIndex, ColIndex];
    Interpolation.InterpolationDirection := PhastDataSet.CellInterpolationDirection[LayerIndex, RowIndex, ColIndex];
  end;
end;

function TIProperty.DataType: TPropertyType;
begin
  result := ptInteger;
end;

function TIProperty.GetIValue: integer;
begin
  result := FIValue;
end;

function TIProperty.IsIdentical(AProperty: TProperty): boolean;
begin
  with (AProperty as TIProperty) do
  begin
    if self.Interpolation.UsePHAST_Interpolation then
    begin
      result := Interpolation.UsePHAST_Interpolation
        and (self.Interpolation.Distance1 = Interpolation.Distance1)
        and (self.Interpolation.Distance2 = Interpolation.Distance2)
        and (self.Interpolation.IntValue1 = Interpolation.IntValue1)
        and (self.Interpolation.IntValue2 = Interpolation.IntValue2)
        and (self.Interpolation.InterpolationDirection = Interpolation.InterpolationDirection);
    end
    else
    begin
      result := not Interpolation.UsePHAST_Interpolation
        and (self.IValue = IValue);
    end;
  end;
end;

procedure TIProperty.SetIValue(const Value: integer);
begin
  FIValue := Value;
end;

{ TRProperty }

procedure TRProperty.Assign(const DataSet: TDataSet; const LayerIndex, RowIndex, ColIndex: integer);
var
  PhastDataSet: TRealPhastDataSet;
begin
  PhastDataSet := DataSet as TRealPhastDataSet;
  Interpolation.UsePHAST_Interpolation := PhastDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
  if not Interpolation.UsePHAST_Interpolation then
  begin
    RVAlue := DataSet.RealData[LayerIndex, RowIndex, ColIndex];
  end
  else
  begin
    Interpolation.Distance1 := PhastDataSet.CellDistance1[LayerIndex, RowIndex, ColIndex];
    Interpolation.Distance2 := PhastDataSet.CellDistance2[LayerIndex, RowIndex, ColIndex];
    Interpolation.RealValue1 := PhastDataSet.CellValue1[LayerIndex, RowIndex, ColIndex];
    Interpolation.RealValue2 := PhastDataSet.CellValue2[LayerIndex, RowIndex, ColIndex];
    Interpolation.InterpolationDirection := PhastDataSet.CellInterpolationDirection[LayerIndex, RowIndex, ColIndex];
  end;
end;

function TRProperty.DataType: TPropertyType;
begin
  result := ptReal;
end;

function TRProperty.GetRValue: double;
begin
  result := FRValue;
end;

function TRProperty.IsIdentical(AProperty: TProperty): boolean;
begin
  with (AProperty as TRProperty) do
  begin
    if self.Interpolation.UsePHAST_Interpolation then
    begin
      result := Interpolation.UsePHAST_Interpolation
        and (self.Interpolation.Distance1 = Interpolation.Distance1)
        and (self.Interpolation.Distance2 = Interpolation.Distance2)
        and (self.Interpolation.RealValue1 = Interpolation.RealValue1)
        and (self.Interpolation.RealValue2 = Interpolation.RealValue2)
        and (self.Interpolation.InterpolationDirection = Interpolation.InterpolationDirection);
    end
    else
    begin
      result := not Interpolation.UsePHAST_Interpolation
        and (self.RValue = RValue);
    end;
  end;
end;

procedure TRProperty.SetRValue(const Value: double);
begin
  FRValue := Value;
end;

{ TProperty }

constructor TProperty.Create;
begin
  FPhastInterpolationValues:= TPhastInterpolationValues.Create;
end;

destructor TProperty.Destroy;
begin
  FPhastInterpolationValues.Free;
  inherited;
end;

function TProperty.GetBValue: boolean;
begin
  result := False;
end;

function TProperty.GetIValue: integer;
begin
  result := 0;
end;

function TProperty.GetRValue: double;
begin
  result := 0;
end;

procedure TProperty.SetBValue(const Value: boolean);
begin
  // do nothing
end;

procedure TProperty.SetIValue(const Value: integer);
begin
  // do nothing
end;

procedure TProperty.SetPhastInterpolationValues(
  const Value: TPhastInterpolationValues);
begin
  FPhastInterpolationValues.Assign(Value);
end;

procedure TProperty.SetRValue(const Value: double);
begin
  // do nothing
end;

{ TMediaDataSets }

constructor TMediaDataSets.Create;
begin
  EvalAt := eaBlocks;
  ZoneClass := TMediaZone;
  Kx := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Kx)];
  Ky := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Ky)];
  Kz := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Kz)];
  Porosity := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Porosity)];
  SpecificStorage := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Specific_Storage)];
  LongDisp := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Long_Dispersivity)];
  HorzTransDisp := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Horizontal_Transv_Dispersivity)];
  VertTransDisp := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Vertical_Transv_Dispersivity)];

  Kx.Initialize;
  Ky.Initialize;
  Kz.Initialize;
  Porosity.Initialize;
  SpecificStorage.Initialize;
  LongDisp.Initialize;
  HorzTransDisp.Initialize;
  VertTransDisp.Initialize;

end;

{ TPhastZone }

procedure TPhastZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TDataSets);
begin
  case DataSets.EvalAt of
    eaBlocks:
      begin
        with frmGoPhast.PhastGrid do
        begin
          X1 := ColumnPosition[ColIndex];
          X2 := ColumnPosition[ColIndex+1];
          Y1 := RowPosition[RowIndex];
          Y2 := RowPosition[RowIndex+1];
          Z1 := LayerElevation[LayerIndex];
          Z2 := LayerElevation[LayerIndex+1];
        end;
      end;
    eaNodes:
      begin
        with frmGoPhast.PhastGrid do
        begin
          X1 := ColumnPosition[ColIndex];
          X2 := X1;
          Y1 := RowPosition[RowIndex];
          Y2 := Y1;
          Z1 := LayerElevation[LayerIndex];
          Z2 := Z1;
        end;
      end;
  else Assert(False);
  end;
end;

constructor TPhastZone.Create;
begin
  PropertyList:= TList.Create;
end;

destructor TPhastZone.Destroy;
begin
  PropertyList.Free;
  inherited;
end;

function TPhastZone.PropertiesAreIdentical(AZone: TPhastZone): boolean;
var
  Index: integer;
  Property1, Property2: TProperty;
begin
  result := True;
  Assert(PropertyList.Count = AZone.PropertyList.Count);
  for Index := 0 to PropertyList.Count -1 do
  begin
    Property1 := PropertyList[Index];
    Property2 := AZone.PropertyList[Index];
    result := Property1.IsIdentical(Property2);
    if not result then Exit;
  end;
end;

{ TMediaZone }

procedure TMediaZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TDataSets);
var
  MediaDataSets: TMediaDataSets;
begin
  inherited;
  MediaDataSets := DataSets as TMediaDataSets;
  Kx.Assign(MediaDataSets.Kx, LayerIndex, RowIndex, ColIndex);
  Ky.Assign(MediaDataSets.Ky, LayerIndex, RowIndex, ColIndex);
  Kz.Assign(MediaDataSets.Kz, LayerIndex, RowIndex, ColIndex);
  Porosity.Assign(MediaDataSets.Porosity, LayerIndex, RowIndex, ColIndex);
  SpecificStorage.Assign(MediaDataSets.SpecificStorage, LayerIndex, RowIndex, ColIndex);
  LongDisp.Assign(MediaDataSets.LongDisp, LayerIndex, RowIndex, ColIndex);
  HorzTransDisp.Assign(MediaDataSets.HorzTransDisp, LayerIndex, RowIndex, ColIndex);
  VertTransDisp.Assign(MediaDataSets.VertTransDisp, LayerIndex, RowIndex, ColIndex);
end;

constructor TMediaZone.Create;
begin
  inherited;
  Kx:= TRProperty.Create;
  Ky:= TRProperty.Create;
  Kz:= TRProperty.Create;
  Porosity:= TRProperty.Create;
  SpecificStorage:= TRProperty.Create;
  LongDisp:= TRProperty.Create;
  HorzTransDisp:= TRProperty.Create;
  VertTransDisp:= TRProperty.Create;
  PropertyList.Add(Kx);
  PropertyList.Add(Ky);
  PropertyList.Add(Kz);
  PropertyList.Add(Porosity);
  PropertyList.Add(SpecificStorage);
  PropertyList.Add(LongDisp);
  PropertyList.Add(HorzTransDisp);
  PropertyList.Add(VertTransDisp);
end;

destructor TMediaZone.Destroy;
begin
  Kx.Free;
  Ky.Free;
  Kz.Free;
  Porosity.Free;
  SpecificStorage.Free;
  LongDisp.Free;
  HorzTransDisp.Free;
  VertTransDisp.Free;
  inherited;
end;

{ TZoneGroup }

constructor TZoneGroup.Create(const DataSets: TDataSets);
var
  LayerIndex, RowIndex, ColIndex: integer;
  LayerLimit, RowLimit, ColLimit: integer;
  RowList: TList;
  ColList: TList;
  PriorZone, CurrentZone: TPhastZone;
  Layers: TList;
  PriorColIndex: integer;
  PriorColList, PriorRowList: TList;
  PriorCol: integer;
  ZoneCount: integer;
begin
  LayerLimit := 0;
  RowLimit := 0;
  ColLimit := 0;
  ZoneCount := 0;
  FinalZones:= TObjectList.Create;
  Layers:= TObjectList.Create;
  try
    case DataSets.EvalAt of
      eaBlocks:
        begin
          LayerLimit := frmGoPhast.PhastGrid.LayerCount;
          RowLimit := frmGoPhast.PhastGrid.RowCount;
          ColLimit := frmGoPhast.PhastGrid.ColumnCount;
        end;
      eaNodes:
        begin
          LayerLimit := frmGoPhast.PhastGrid.LayerCount + 1;
          RowLimit := frmGoPhast.PhastGrid.RowCount + 1;
          ColLimit := frmGoPhast.PhastGrid.ColumnCount + 1;
        end;
    else Assert(False);
    end;

    Layers.Capacity := LayerLimit;

    for LayerIndex := 0 to LayerLimit -1  do
    begin
      RowList := TObjectList.Create;
      Layers.Add(RowList);
      RowList.Capacity := RowLimit;
      for RowIndex := 0 to RowLimit -1 do
      begin
        ColList := TList.Create;
        RowList.Add(ColList);
        ColList.Capacity := ColLimit;
        PriorZone := nil;
        for ColIndex := 0 to ColLimit -1 do
        begin
          CurrentZone := DataSets.ZoneClass.Create;
          Inc(ZoneCount);
          CurrentZone.AssignProperties(LayerIndex, RowIndex, ColIndex, DataSets);
          if (PriorZone = nil) or not PriorZone.PropertiesAreIdentical(CurrentZone) then
          begin
            ColList.Add(CurrentZone);
            PriorZone := CurrentZone;
          end
          else
          begin
            PriorZone.X2 := CurrentZone.X2;
            CurrentZone.Free;
            Dec(ZoneCount);
          end;
        end;
        if RowIndex > 0 then
        begin
          PriorColList := RowList[RowIndex-1];
          PriorCol := PriorColList.Count -1;
          for ColIndex := ColList.Count -1 downto 0 do
          begin
            CurrentZone := ColList[ColIndex];
            for PriorColIndex := PriorCol downto 0 do
            begin
              PriorZone := PriorColList[PriorColIndex];
              if PriorZone.X1 > CurrentZone.X1 then
              begin
                PriorCol := PriorColIndex;
                Continue;
              end;

              if PriorZone.X1 < CurrentZone.X1 then
              begin
                break;
              end;

              // PriorZone.X1 = CurrentZone.X1
              PriorCol := PriorCol -1;

              if PriorZone.X2 <> CurrentZone.X2 then
              begin
                break;
              end;

              if PriorZone.PropertiesAreIdentical(CurrentZone) then
              begin
                PriorColList.Delete(PriorCol+1);
                CurrentZone.Y1 := PriorZone.Y1;
                PriorZone.Free;
                Dec(ZoneCount);
              end;
              break;
            end;
          end;
        end;
        ColList.Capacity := ColList.Count;
      end;
      if LayerIndex > 0 then
      begin
        PriorRowList := Layers[LayerIndex -1];
        Assert(PriorRowList.Count = RowList.Count);
        for RowIndex := 0 to RowList.Count -1 do
        begin
          PriorColList := PriorRowList[RowIndex];
          ColList := RowList[RowIndex];
          PriorCol := PriorColList.Count -1;

          for ColIndex := ColList.Count -1 downto 0 do
          begin
            CurrentZone := ColList[ColIndex];
            for PriorColIndex := PriorCol downto 0 do
            begin
              PriorZone := PriorColList[PriorColIndex];
              if PriorZone.X1 > CurrentZone.X1 then
              begin
                PriorCol := PriorColIndex;
                Continue;
              end;

              if PriorZone.X1 < CurrentZone.X1 then
              begin
                break;
              end;

              // PriorZone.X1 = CurrentZone.X1
              PriorCol := PriorCol -1;

              if (PriorZone.X2 <> CurrentZone.X2)
                or (PriorZone.Y1 <> CurrentZone.Y1)
                or (PriorZone.Y2 <> CurrentZone.Y2) then
              begin
                break;
              end;

              if PriorZone.PropertiesAreIdentical(CurrentZone) then
              begin
                PriorColList.Delete(PriorCol+1);
                CurrentZone.Z1 := PriorZone.Z1;
                PriorZone.Free;
                Dec(ZoneCount);
              end;
              break;
            end;
          end;
        end;
      end;
    end;
    FinalZones.Capacity := ZoneCount;
    for LayerIndex := 0 to Layers.Count -1 do
    begin
      RowList := Layers[LayerIndex];
      for RowIndex := 0 to RowList.Count -1 do
      begin
        ColList := RowList[RowIndex];
        for ColIndex := 0 to ColList.Count -1 do
        begin
          CurrentZone := ColList[ColIndex];
          FinalZones.Add(CurrentZone);
        end;
      end;
    end;
  finally
    Layers.Free;
  end;
end;

destructor TZoneGroup.Destroy;
begin
  FinalZones.Free;
  inherited;
end;

function TZoneGroup.GetZone(Index: integer): TPhastZone;
begin
  result := FinalZones[Index];
end;

function TZoneGroup.ZoneCount: integer;
begin
  result := FinalZones.Count;
end;

{ TMediaZoneGroup }

constructor TMediaZoneGroup.Create;
var
  DataSets: TMediaDataSets;
begin
  DataSets := TMediaDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TMediaZoneGroup.GetZone(Index: integer): TMediaZone;
begin
  result := inherited Zones[Index] as TMediaZone;
end;

{ TBProperty }

procedure TBProperty.Assign(const DataSet: TDataSet; const LayerIndex,
  RowIndex, ColIndex: integer);
begin
  BVAlue := DataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
end;

function TBProperty.DataType: TPropertyType;
begin
  result := ptBoolean;
end;

function TBProperty.GetBValue: boolean;
begin
  result := FBValue;
end;

function TBProperty.IsIdentical(AProperty: TProperty): boolean;
begin
  result := BValue = (AProperty as TBProperty).BValue;
end;

procedure TBProperty.SetBValue(const Value: boolean);
begin
  FBValue := Value;
end;

{ TActiveDataSets }

constructor TActiveDataSets.Create;
begin
  EvalAt := eaBlocks;
  ZoneClass := TActiveZone;
  Active := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Active)];
  Active.Initialize;
end;

{ TActiveZone }

procedure TActiveZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TDataSets);
var
  ActiveDataSets: TActiveDataSets;
begin
  inherited;
  ActiveDataSets := DataSets as TActiveDataSets;
  Active.Assign(ActiveDataSets.Active, LayerIndex, RowIndex, ColIndex);
end;

constructor TActiveZone.Create;
begin
  inherited;
  Active:= TBProperty.Create;
  PropertyList.Add(Active);
end;

destructor TActiveZone.Destroy;
begin
  Active.Free;
  inherited;
end;

{ TActiveZoneGroup }

constructor TActiveZoneGroup.Create;
var
  DataSets: TActiveDataSets;
begin
  DataSets := TActiveDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TActiveZoneGroup.GetZone(Index: integer): TActiveZone;
begin
  result := inherited Zones[Index] as TActiveZone;
end;

{ TInitialHeadDataSets }

constructor TInitialHeadDataSets.Create;
begin
  EvalAt := eaNodes;
  ZoneClass := TInitialHeadZone;
  InitialHead := frmGoPhast.Model.DataSets[frmGoPhast.Model.
    IndexOfDataSet(ModelUnit.Initial_Head)];
  InitialHead.Initialize;
end;

{ TInitialHeadZone }

procedure TInitialHeadZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TDataSets);
var
  InitialHeadDataSets: TInitialHeadDataSets;
begin
  inherited;
  InitialHeadDataSets := DataSets as TInitialHeadDataSets;
  InitialHead.Assign(InitialHeadDataSets.InitialHead, LayerIndex, RowIndex, ColIndex);
end;

constructor TInitialHeadZone.Create;
begin
  inherited;
  InitialHead:= TRProperty.Create;
  PropertyList.Add(InitialHead);
end;

destructor TInitialHeadZone.Destroy;
begin
  InitialHead.Free;
  inherited;
end;

{ TInitialHeadZoneGroup }

constructor TInitialHeadZoneGroup.Create;
var
  DataSets: TInitialHeadDataSets;
begin
  DataSets := TInitialHeadDataSets.Create;
  try
    inherited Create(DataSets);
  finally
    DataSets.Free;
  end;
end;

function TInitialHeadZoneGroup.GetZone(Index: integer): TInitialHeadZone;
begin
  result := inherited Zones[Index] as TInitialHeadZone;
end;

end.
