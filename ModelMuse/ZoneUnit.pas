{
 @abstract(@name is used to define the base classes that are used to define
 property zones in PHAST.  These classes are @link(TCustomProperty),
 @link(TIProperty), @link(TRProperty), @link(TBProperty),
 @link(TCustomDataSets), @link(TCustomPhastZone), and @link(TCustomZoneGroup).)
 Each zone is
 represented by a @link(TCustomPhastZone).
 Each zones has a series of properties
 (@link(TCustomProperty) that are constant
 within it.  @link(TCustomPhastZone)s are
 grouped together in @link(TCustomZoneGroup)s.
 For instance, all the @link(TCustomPhastZone)s
 related to hydraulic conductivity
 and other aquifer properties
 are in a @link(TMediaZoneGroup) which descends from @link(TCustomZoneGroup).

 Helper classes include @link(TCustomDataSets) which is used to define which
 @link(TDataArray)s are used to set the @link(TCustomProperty)s
 and TZoneList (defined
 in the implementation section)
 which is used to store @link(TCustomPhastZone)s.

 Generally, what happens is that a descendant of @link(TCustomZoneGroup)
 is created.
 It, in turn creates a descendant of @link(TCustomDataSets) in its constructor
 which is then passed to the inherited
 TCustomZoneGroup.@link(TCustomZoneGroup.Create).
 It then creates a @link(TCustomPhastZone) for each cell or element.
 @link(TCustomPhastZone.AssignProperties) is used to set the values in
 the @link(TCustomPhastZone).  If possible, a zone will be merged with
 its neighbors.  The properties of the zones must be the same for them
 to be merged. The merged zone must also be rectangular.
 PHAST does not allow some zones to have more than two dimensions so
 @link(TCustomZoneGroup.MergeInXDirection),
 @link(TCustomZoneGroup.MergeInYDirection), and
 @link(TCustomZoneGroup.MergeInZDirection) provide an additional constraint
 on merging.  Finally boundary conditions zones are only defined
 where the boundary condition is present so zones representing
 the absence of a boundary condition are eliminated.
}
unit ZoneUnit;

interface

uses SysUtils, Classes, GoPhastTypes, DataSetUnit, PhastDataSets;

type
  // @name is used to specify what kind of data is stored in a
  // @link(TCustomProperty). See @link(TCustomProperty.DataType).
  TPropertyType = (ptReal, ptInteger, ptBoolean);

  {@abstract(@name is the abstract ancestor of real number, integer,
   and boolean properties.)  (See @link(TRProperty), @link(TIProperty),
   and @link(TBProperty).)  A key method is @link(TCustomProperty.IsIdentical)
   which tests whether to @link(TCustomProperty)s are identical.
   Another is @link(TCustomProperty.Assign) which assigns the value
   of a data set at a particular location to the @link(TCustomProperty).
  }
  TCustomProperty = class(TObject)
  private
    // @name: TPhastInterpolationValues;
    // See @link(Interpolation)
    FPhastInterpolationValues: TPhastInterpolationValues;
    // @name returns @False. In descendants it returns an actual value.
    function GetBValue: boolean; virtual;
    // @name returns 0. In descendants it returns an actual value.
    function GetIValue: integer; virtual;
    // @name returns 0.0.  In descendants it returns an actual value.
    function GetRValue: double; virtual;
    // @name does nothing.  In descendants it sets a value.
    procedure SetBValue(const Value: boolean); virtual;
    // @name does nothing.  In descendants it sets a value.
    procedure SetIValue(const Value: integer); virtual;
    // See @link(Interpolation)
    procedure SetPhastInterpolationValues(
      const Value: TPhastInterpolationValues);
    // @name does nothing.  In descendants it sets a value.
    procedure SetRValue(const Value: double); virtual;
    // See @link(Interpolation)
  protected
    // @name is used to say whether the zone contains a value or not.
    FIsValue: boolean;
  public
    // @name copies the value of DataSet at LayerIndex, RowIndex,
    //  ColIndex to the @classname.  In @classname, all it does is set
    // @link(FIsValue).
    procedure Assign(const DataSet: TDataArray; const LayerIndex, RowIndex,
      ColIndex: integer); virtual;
    // @name is an boolean value in the @classname.
    property BValue: boolean read GetBValue write SetBValue;
    // @name is used to specify what sort of data is stored in the @classname.
    function DataType: TPropertyType; virtual; abstract;
    // @name is an integer value in the @classname.
    property IValue: integer read GetIValue write SetIValue;
    // @name is an real-number value in the @classname.
    property RValue: double read GetRValue write SetRValue;
    // If PHAST-style interpolation is used, @name is used to save
    // the interpolation parameters.
    property Interpolation: TPhastInterpolationValues
      read FPhastInterpolationValues write SetPhastInterpolationValues;
    // @name returns true if the @classname's are the same.
    function IsIdentical(AProperty: TCustomProperty): boolean;
      virtual; abstract;
    // @name creates an instance of @classname.
    constructor Create;
    // @name is used to tell whether or not the property has a value.
    // In boundary conditions, zones often don't have a value.
    property IsValue: boolean read FIsValue;
    // @name destroys the current instance.  Do not call Destroy directly.
    // Call Free instead.
    destructor Destroy; override;
  end;

  // @abstract(@name represents an integer value.)
  TIProperty = class(TCustomProperty)
  private
    // @name: integer;
    // @name is the value stored in the @classname.
    FIValue: integer;
    // See @link(TCustomProperty.IValue).
    function GetIValue: integer; override;
    // See @link(TCustomProperty.IValue).
    procedure SetIValue(const Value: integer); override;
  public
    // @name assigns the integer value and PHAST-Interpolation
    // parameters of DataSet at LayerIndex, RowIndex,
    // ColIndex to the @classname.
    procedure Assign(const DataSet: TDataArray; const LayerIndex, RowIndex,
      ColIndex: integer); override;
    // @name identifies @classname as holding an integer value.
    function DataType: TPropertyType; override;
    // @name determines whether two @classname's have the same values.
    function IsIdentical(AProperty: TCustomProperty): boolean; override;
  end;

  TIPropertyList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TIProperty;
  public
    function Add(IProperty: TIProperty): integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: TIProperty read GetItem; default;
    Constructor Create;
    destructor Destroy; override;
  end;

  // @abstract(@name represents an real-number value.)
  TRProperty = class(TCustomProperty)
  private
    // @name: double;
    // @name is the value stored in the @classname.
    FRValue: double;
    // See @link(TCustomProperty.RValue).
    function GetRValue: double; override;
    // See @link(TCustomProperty.RValue).
    procedure SetRValue(const Value: double); override;
  public
    // @name assigns the integer value and PHAST-Interpolation
    // parameters of DataSet at LayerIndex, RowIndex,
    // ColIndex to the @classname.
    procedure Assign(const DataSet: TDataArray; const LayerIndex, RowIndex,
      ColIndex: integer); override;
    // @name identifies @classname as holding a real-number value.
    function DataType: TPropertyType; override;
    // @name determines whether two @classname's have the same values.
    function IsIdentical(AProperty: TCustomProperty): boolean; override;
  end;

  TRPropertyList = class(TObject)
  private
    FList: TList;
    function GetCount: integer;
    function GetItem(Index: integer): TRProperty;
  public
    function Add(RProperty: TRProperty): integer;
    property Count: integer read GetCount;
    property Items[Index: integer]: TRProperty read GetItem; default;
    Constructor Create;
    destructor Destroy; override;
  end;


  // @abstract(@name represents an boolean value.)
  TBProperty = class(TCustomProperty)
  private
    // @name: boolean;
    // @name is the value stored in the @classname.
    FBValue: boolean;
    // See @link(TCustomProperty.BValue).
    function GetBValue: boolean; override;
    // See @link(TCustomProperty.BValue).
    procedure SetBValue(const Value: boolean); override;
  public
    // @name assigns the integer value of DataSet at LayerIndex, RowIndex,
    // ColIndex to the @classname.
    procedure Assign(const DataSet: TDataArray; const LayerIndex, RowIndex,
      ColIndex: integer); override;
    // @name identifies @classname as holding a boolean value.
    function DataType: TPropertyType; override;
    // @name determines whether two @classname's have the same values.
    function IsIdentical(AProperty: TCustomProperty): boolean; override;
  end;

  TCustomPhastZone = class;

  // @name is used in @link(TCustomDataSets) to create descendants of
  // @link(TCustomPhastZone).
  TZoneClass = class of TCustomPhastZone;

  {@abstract(Descendants of @name store the @link(TDataArray)s
  used to assign values to
  @link(TCustomProperty)s.)  an instance of @classname is passed to
  @link(TCustomZoneGroup) in its constuctor.}
  TCustomDataSets = class(TObject)
  protected
    // @name: @link(TEvaluatedAt).
    // @name specifies where the @link(TDataArray)s
    // for this @classname are evaluated.
    FEvalAt: TEvaluatedAt;
    // @name @link(TZoneClass) is used to specify which descendant of
    // @link(TCustomPhastZone) will be created in a @link(TCustomPhastZone).
    FZoneClass: TZoneClass;
  end;

  { @abstract(@name represents a PHAST zone.)}
  TCustomPhastZone = class(TObject)
  private
    // @name: boolean;
    // See @link(MergedX).
    FMergedX: boolean;
    // @name: boolean;
    // See @link(MergedY).
    FMergedY: boolean;
    // @name: boolean;
    // See @link(MergedZ).
    FMergedZ: boolean;
  protected
    // @name: TList;
    // @name contains @link(TCustomProperty)s that define the zone.
    FPropertyList: TList;
    // @name  assigns the limits of the zone. (See @link(FX1), @link(FX2),
    // @link(FY1), @link(FY2), @link(FZ1), and @link(FZ2).)  In descendants
    // each @link(TCustomProperty) in @link(FPropertyList) has a value
    // assigned to it using the @link(TDataArray)s in DataSets.
    // Some zones can not be merged in the X, Y, or Z direction.
    // CanMergeX, CanMergeY, and CanMergeZ are used to specify whether
    // or not they can be merged in a particular direction.
    procedure AssignProperties(const LayerIndex, RowIndex, ColIndex : integer;
      const DataSets: TCustomDataSets; const CanMergeX, CanMergeY,
      CanMergeZ: boolean); virtual;
    // @name returns true if AZone and self have identical
    // @link(TCustomProperty)s.
    function PropertiesAreIdentical(AZone: TCustomPhastZone): boolean;
  public
    // @name: double;
    // @name is the lower X coordinate of the zone.
    FX1: double;
    // @name: double;
    // @name is the upper X coordinate of the zone.
    FX2: double;
    // @name: double;
    // @name is the lower Y coordinate of the zone.
    FY1: double;
    // @name: double;
    // @name is the upper Y coordinate of the zone.
    FY2: double;
    // @name: double;
    // @name is the lower Z coordinate of the zone.
    FZ1: double;
    // @name: double;
    // @name is the upper Z coordinate of the zone.
    FZ2: double;
    // @name creates an instance of @classname.
    constructor Create; virtual;
    // @name destroys the current instance of @classname.
    // Do not call @name. Call Free instead.
    destructor Destroy; override;
    // @name specifies whether or not the zone has been merged with another
    // in the X direction.
    property MergedX: boolean read FMergedX;
    // @name specifies whether or not the zone has been merged with another
    // in the Y direction.
    property MergedY: boolean read FMergedY;
    // @name specifies whether or not the zone has been merged with another
    // in the Z direction.
    property MergedZ: boolean read FMergedZ;
    // @name writes the limits of the zone to Stream.
    procedure WriteZone(const Stream: TStringStream);
  end;

  // @abstract(@name creates and stores series of @link(TCustomPhastZone)s.)
  TCustomZoneGroup = class(TObject)
  private
    // See @link(Zones).
    function GetZone(Index: integer): TCustomPhastZone;
  protected
    // @name: TList.
    // @name is instantiated as TObjectList;
    // It is used to store the @link(TCustomPhastZone)s.
    FFinalZones: TList;
    // See @link(MergeInXDirection).
    function GetMergeInXDirection: boolean; virtual;
    // See @link(MergeInYDirection).
    function GetMergeInYDirection: boolean; virtual;
    // See @link(MergeInZDirection).
    function GetMergeInZDirection: boolean; virtual;
  public
    // @name creates and instance of @classname and creates the
    // @link(TCustomPhastZone)s specified using DataSets.
    constructor Create(const DataSets: TCustomDataSets);
    // @name destroys the current instance of @classname and all its
    // @link(TCustomPhastZone)s.
    // Do not call @name. Call Free instead.
    destructor Destroy; override;
    // @name is used to access the @link(TCustomPhastZone)s stored by
    // @classname.
    property Zones[Index: integer]: TCustomPhastZone read GetZone;
    // @name is the number of @link(TCustomPhastZone)s stored by @classname.
    function ZoneCount: integer;
    // @name specifies whether the zones can merge in the X direction.
    property MergeInXDirection: boolean read GetMergeInXDirection;
    // @name specifies whether the zones can merge in the Y direction.
    property MergeInYDirection: boolean read GetMergeInYDirection;
    // @name specifies whether the zones can merge in the Z direction.
    property MergeInZDirection: boolean read GetMergeInZDirection;
  end;

implementation

uses Contnrs, frmGoPhastUnit;

type
  // TZoneList is an expandable array of TCustomPhastZones.
  TZoneList = class(TObject)
  private
    FList: TList;
    function GetItems(Index: integer): TCustomPhastZone;
    procedure SetItems(Index: integer; const Value: TCustomPhastZone);
    procedure SetCapacity(const Value: integer);
    function GetCapacity: integer;
    function GetCount: integer;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(AZone: TCustomPhastZone): integer;
    property Items[Index: integer]: TCustomPhastZone read GetItems write SetItems;
      default;
    procedure Delete(Index: integer);
    property Capacity: integer read GetCapacity write SetCapacity;
    property Count: integer read GetCount;
    function IndexOf(const AZone: TCustomPhastZone): integer;
  end;

  { TIProperty }

procedure TIProperty.Assign(const DataSet: TDataArray; const LayerIndex,
  RowIndex, ColIndex: integer);
var
  PhastDataSet: TIntegerPhastDataSet;
  SparseDataSet: TSparseIntegerPhastDataSet;
begin
  inherited Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  if IsValue then
  begin
    if DataSet is TIntegerPhastDataSet then
    begin
      PhastDataSet := DataSet as TIntegerPhastDataSet;
      Interpolation.UsePHAST_Interpolation :=
        PhastDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
      if not Interpolation.UsePHAST_Interpolation then
      begin
        IValue := DataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        Interpolation.Distance1 := PhastDataSet.CellDistance1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.Distance2 := PhastDataSet.CellDistance2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.IntValue1 := PhastDataSet.CellValue1[LayerIndex, RowIndex,
          ColIndex];
        Interpolation.IntValue2 := PhastDataSet.CellValue2[LayerIndex, RowIndex,
          ColIndex];
        Interpolation.InterpolationDirection :=
          PhastDataSet.CellInterpolationDirection[LayerIndex, RowIndex,
            ColIndex];
      end;
    end
    else if DataSet is TSparseIntegerPhastDataSet then
    begin
      SparseDataSet := DataSet as TSparseIntegerPhastDataSet;
      Interpolation.UsePHAST_Interpolation :=
        SparseDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
      if not Interpolation.UsePHAST_Interpolation then
      begin
        IValue := DataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        Interpolation.Distance1 := SparseDataSet.CellDistance1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.Distance2 := SparseDataSet.CellDistance2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.IntValue1 := SparseDataSet.CellValue1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.IntValue2 := SparseDataSet.CellValue2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.InterpolationDirection :=
          SparseDataSet.CellInterpolationDirection[LayerIndex, RowIndex,
            ColIndex];
      end;
    end
    else
    begin
      Interpolation.UsePHAST_Interpolation := False;
      IValue := DataSet.IntegerData[LayerIndex, RowIndex, ColIndex];
    end;
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

function TIProperty.IsIdentical(AProperty: TCustomProperty): boolean;
begin
  with (AProperty as TIProperty) do
  begin
    result := self.IsValue = IsValue;
    if result and self.IsValue then
    begin
      if self.Interpolation.UsePHAST_Interpolation then
      begin
        result := Interpolation.UsePHAST_Interpolation
          and (self.Interpolation.Distance1 = Interpolation.Distance1)
          and (self.Interpolation.Distance2 = Interpolation.Distance2)
          and (self.Interpolation.IntValue1 = Interpolation.IntValue1)
          and (self.Interpolation.IntValue2 = Interpolation.IntValue2)
          and (self.Interpolation.InterpolationDirection =
          Interpolation.InterpolationDirection);
      end
      else
      begin
        result := not Interpolation.UsePHAST_Interpolation
          and (self.IValue = IValue);
      end;
    end;
  end;
end;

procedure TIProperty.SetIValue(const Value: integer);
begin
  FIValue := Value;
end;

{ TRProperty }

procedure TRProperty.Assign(const DataSet: TDataArray; const LayerIndex, RowIndex,
  ColIndex: integer);
var
  PhastDataSet: TRealPhastDataSet;
  SparseDataSet: TSparseRealPhastDataSet;
begin
  inherited Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  if IsValue then
  begin
    if DataSet is TRealPhastDataSet then
    begin
      PhastDataSet := TRealPhastDataSet(DataSet);
      Interpolation.UsePHAST_Interpolation :=
        PhastDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
      if not Interpolation.UsePHAST_Interpolation then
      begin
        RVAlue := DataSet.RealData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        Interpolation.Distance1 := PhastDataSet.CellDistance1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.Distance2 := PhastDataSet.CellDistance2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.RealValue1 := PhastDataSet.CellValue1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.RealValue2 := PhastDataSet.CellValue2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.InterpolationDirection :=
          PhastDataSet.CellInterpolationDirection[LayerIndex, RowIndex,
            ColIndex];
      end;
    end
    else if DataSet is TSparseRealPhastDataSet then
    begin
      SparseDataSet := TSparseRealPhastDataSet(DataSet);
      Interpolation.UsePHAST_Interpolation :=
        SparseDataSet.IsInterpolatedCell[LayerIndex, RowIndex, ColIndex];
      if not Interpolation.UsePHAST_Interpolation then
      begin
        RVAlue := DataSet.RealData[LayerIndex, RowIndex, ColIndex];
      end
      else
      begin
        Interpolation.Distance1 := SparseDataSet.CellDistance1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.Distance2 := SparseDataSet.CellDistance2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.RealValue1 := SparseDataSet.CellValue1[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.RealValue2 := SparseDataSet.CellValue2[LayerIndex,
          RowIndex,
          ColIndex];
        Interpolation.InterpolationDirection :=
          SparseDataSet.CellInterpolationDirection[LayerIndex, RowIndex,
            ColIndex];
      end;
    end
    else
    begin
      Interpolation.UsePHAST_Interpolation := False;
      RVAlue := DataSet.RealData[LayerIndex, RowIndex, ColIndex];
    end
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

function TRProperty.IsIdentical(AProperty: TCustomProperty): boolean;
begin
  with (AProperty as TRProperty) do
  begin
    result := self.IsValue = IsValue;
    if result and self.IsValue then
    begin
      if self.Interpolation.UsePHAST_Interpolation then
      begin
        result := Interpolation.UsePHAST_Interpolation
          and (self.Interpolation.Distance1 = Interpolation.Distance1)
          and (self.Interpolation.Distance2 = Interpolation.Distance2)
          and (self.Interpolation.RealValue1 = Interpolation.RealValue1)
          and (self.Interpolation.RealValue2 = Interpolation.RealValue2)
          and (self.Interpolation.InterpolationDirection =
          Interpolation.InterpolationDirection);
      end
      else
      begin
        result := not Interpolation.UsePHAST_Interpolation
          and (self.RValue = RValue);
      end;
    end;
  end;
end;

procedure TRProperty.SetRValue(const Value: double);
begin
  FRValue := Value;
end;

{ TCustomProperty }

procedure TCustomProperty.Assign(const DataSet: TDataArray; const LayerIndex,
  RowIndex, ColIndex: integer);
begin
  FIsValue := DataSet.IsValue[LayerIndex, RowIndex, ColIndex];
end;

constructor TCustomProperty.Create;
begin
  FPhastInterpolationValues := TPhastInterpolationValues.Create;
end;

destructor TCustomProperty.Destroy;
begin
  FPhastInterpolationValues.Free;
  inherited;
end;

function TCustomProperty.GetBValue: boolean;
begin
  result := False;
end;

function TCustomProperty.GetIValue: integer;
begin
  result := 0;
end;

function TCustomProperty.GetRValue: double;
begin
  result := 0;
end;

procedure TCustomProperty.SetBValue(const Value: boolean);
begin
  // do nothing
end;

procedure TCustomProperty.SetIValue(const Value: integer);
begin
  // do nothing
end;

procedure TCustomProperty.SetPhastInterpolationValues(
  const Value: TPhastInterpolationValues);
begin
  FPhastInterpolationValues.Assign(Value);
end;

procedure TCustomProperty.SetRValue(const Value: double);
begin
  // do nothing
end;

{ TCustomPhastZone }

procedure TCustomPhastZone.AssignProperties(const LayerIndex, RowIndex,
  ColIndex: integer; const DataSets: TCustomDataSets;
  const CanMergeX, CanMergeY, CanMergeZ: boolean);
begin
  case DataSets.FEvalAt of
    eaBlocks:
      begin
        with frmGoPhast.PhastGrid do
        begin
          FX1 := ColumnPosition[ColIndex];
          FX2 := ColumnPosition[ColIndex + 1];
          FY1 := RowPosition[RowIndex];
          FY2 := RowPosition[RowIndex + 1];
          FZ1 := LayerElevation[LayerIndex];
          FZ2 := LayerElevation[LayerIndex + 1];
        end;
      end;
    eaNodes:
      begin
        with frmGoPhast.PhastGrid do
        begin
          if (ColIndex > 0) and CanMergeX then
          begin
            FX1 := (ColumnPosition[ColIndex - 1]
              + ColumnPosition[ColIndex]) / 2;
          end
          else
          begin
            FX1 := ColumnPosition[ColIndex];
          end;

          if (ColIndex < ColumnCount) and CanMergeX then
          begin
            FX2 := (ColumnPosition[ColIndex + 1]
              + ColumnPosition[ColIndex]) / 2;
          end
          else
          begin
            FX2 := ColumnPosition[ColIndex];
          end;

          if (RowIndex > 0) and CanMergeY then
          begin
            FY1 := (RowPosition[RowIndex - 1]
              + RowPosition[RowIndex]) / 2;
          end
          else
          begin
            FY1 := RowPosition[RowIndex];
          end;

          if (RowIndex < RowCount) and CanMergeY then
          begin
            FY2 := (RowPosition[RowIndex + 1]
              + RowPosition[RowIndex]) / 2;
          end
          else
          begin
            FY2 := RowPosition[RowIndex];
          end;

          if (LayerIndex > 0) and CanMergeZ then
          begin
            FZ1 := (LayerElevation[LayerIndex - 1]
              + LayerElevation[LayerIndex]) / 2;
          end
          else
          begin
            FZ1 := LayerElevation[LayerIndex];
          end;

          if (LayerIndex < LayerCount) and CanMergeZ then
          begin
            FZ2 := (LayerElevation[LayerIndex + 1]
              + LayerElevation[LayerIndex]) / 2;
          end
          else
          begin
            FZ2 := LayerElevation[LayerIndex];
          end;
        end;
      end;
  else
    Assert(False);
  end;
end;

constructor TCustomPhastZone.Create;
begin
  FPropertyList := TList.Create;
  FMergedX := False;
  FMergedY := False;
  FMergedZ := False;
end;

destructor TCustomPhastZone.Destroy;
begin
  FPropertyList.Free;
  inherited;
end;

function TCustomPhastZone.PropertiesAreIdentical(AZone: TCustomPhastZone): boolean;
var
  Index: integer;
  Property1, Property2: TCustomProperty;
begin
  result := True;
  Assert(FPropertyList.Count = AZone.FPropertyList.Count);
  for Index := 0 to FPropertyList.Count - 1 do
  begin
    Property1 := FPropertyList[Index];
    Property2 := AZone.FPropertyList[Index];
    result := Property1.IsIdentical(Property2);
    if not result then
      Exit;
  end;
end;

procedure TCustomPhastZone.WriteZone(const Stream: TStringStream);
begin
  Stream.WriteString(BlankSpaces + '-zone '
    + FloatToStr(FX1) + ' '
    + FloatToStr(FY1) + ' '
    + FloatToStr(FZ1) + ' '
    + FloatToStr(FX2) + ' '
    + FloatToStr(FY2) + ' '
    + FloatToStr(FZ2) + EndOfLine);
end;

{ TCustomZoneGroup }

constructor TCustomZoneGroup.Create(const DataSets: TCustomDataSets);
var
  LayerIndex, RowIndex, ColIndex: integer;
  LayerLimit, RowLimit, ColLimit: integer;
  RowList: TList;
  ColList: TZoneList;
  PriorZone, CurrentZone, TempZone: TCustomPhastZone;
  Layers: TList;
  PriorColIndex: integer;
  PriorColList: TZoneList;
  PriorRowList: TList;
  PriorCol: integer;
  ZoneCount: integer;
begin
  LayerLimit := 0;
  RowLimit := 0;
  ColLimit := 0;
  ZoneCount := 0;
  FFinalZones := TObjectList.Create;
  Layers := TObjectList.Create;
  try
    case DataSets.FEvalAt of
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
    else
      Assert(False);
    end;

    Layers.Capacity := LayerLimit;

    TempZone := DataSets.FZoneClass.Create;
    for LayerIndex := 0 to LayerLimit - 1 do
    begin
      RowList := TObjectList.Create;
      Layers.Add(RowList);
      RowList.Capacity := RowLimit;
      for RowIndex := 0 to RowLimit - 1 do
      begin
        ColList := TZoneList.Create;
        RowList.Add(ColList);
        ColList.Capacity := ColLimit;
        PriorZone := nil;
        for ColIndex := 0 to ColLimit - 1 do
        begin
          CurrentZone := TempZone;
          Inc(ZoneCount);
          try
            CurrentZone.AssignProperties(LayerIndex, RowIndex, ColIndex,
              DataSets, MergeInXDirection, MergeInYDirection,
                MergeInZDirection);
          except
            CurrentZone.Free;
            raise;
          end;
          if not MergeInXDirection or (PriorZone = nil) or not
            PriorZone.PropertiesAreIdentical(CurrentZone) then
          begin
            Assert(CurrentZone is DataSets.FZoneClass);
            ColList.Add(CurrentZone);
            TempZone := DataSets.FZoneClass.Create;
            PriorZone := CurrentZone;
          end
          else
          begin
            PriorZone.FX2 := CurrentZone.FX2;
            PriorZone.FMergedX := True;
            Dec(ZoneCount);
          end;
        end;
        if MergeInYDirection and (RowIndex > 0) then
        begin
          PriorColList := RowList[RowIndex - 1];
          PriorCol := PriorColList.Count - 1;
          for ColIndex := ColList.Count - 1 downto 0 do
          begin
            CurrentZone := ColList[ColIndex];
            Assert(CurrentZone is DataSets.FZoneClass);
            for PriorColIndex := PriorCol downto 0 do
            begin
              PriorZone := PriorColList[PriorColIndex];
              if PriorZone.FX1 > CurrentZone.FX1 then
              begin
                PriorCol := PriorColIndex;
                Continue;
              end;

              if PriorZone.FX1 < CurrentZone.FX1 then
              begin
                break;
              end;

              PriorCol := PriorCol - 1;

              if PriorZone.FX2 <> CurrentZone.FX2 then
              begin
                break;
              end;

              if PriorZone.PropertiesAreIdentical(CurrentZone) then
              begin
                //                Assert(PriorColList.IndexOf(PriorZone) = PriorCol + 1);
                PriorColList.Delete(PriorColIndex);
                CurrentZone.FY1 := PriorZone.FY1;
                CurrentZone.FMergedY := True;
                PriorZone.Free;
                Dec(ZoneCount);
              end;
              break;
            end;
          end;
        end;
        ColList.Capacity := ColList.Count;
      end;
      if MergeInZDirection and (LayerIndex > 0) then
      begin
        PriorRowList := Layers[LayerIndex - 1];
        Assert(PriorRowList.Count = RowList.Count);
        for RowIndex := 0 to RowList.Count - 1 do
        begin
          PriorColList := PriorRowList[RowIndex];
          ColList := RowList[RowIndex];
          PriorCol := PriorColList.Count - 1;

          for ColIndex := ColList.Count - 1 downto 0 do
          begin
            CurrentZone := ColList[ColIndex];
            Assert(CurrentZone is DataSets.FZoneClass);
            for PriorColIndex := PriorCol downto 0 do
            begin
              PriorZone := PriorColList[PriorColIndex];
              Assert(PriorZone is DataSets.FZoneClass);
              if PriorZone.FX1 > CurrentZone.FX1 then
              begin
                PriorCol := PriorColIndex;
                Continue;
              end;

              if PriorZone.FX1 < CurrentZone.FX1 then
              begin
                break;
              end;

              // PriorZone.X1 = CurrentZone.X1
              PriorCol := PriorCol - 1;

              if (PriorZone.FX2 <> CurrentZone.FX2)
                or (PriorZone.FY1 <> CurrentZone.FY1)
                or (PriorZone.FY2 <> CurrentZone.FY2) then
              begin
                break;
              end;

              if PriorZone.PropertiesAreIdentical(CurrentZone) then
              begin
                //                Assert(PriorColList.IndexOf(PriorZone) = PriorCol + 1);
                PriorColList.Delete(PriorColIndex);
                CurrentZone.FZ1 := PriorZone.FZ1;
                CurrentZone.FMergedZ := True;
                PriorZone.Free;
                Dec(ZoneCount);
              end;
              break;
            end;
          end;
        end;
      end;
    end;
    TempZone.Free;
    FFinalZones.Capacity := ZoneCount;
    for LayerIndex := 0 to Layers.Count - 1 do
    begin
      RowList := Layers[LayerIndex];
      for RowIndex := 0 to RowList.Count - 1 do
      begin
        ColList := RowList[RowIndex];
        for ColIndex := 0 to ColList.Count - 1 do
        begin
          CurrentZone := ColList[ColIndex];
          Assert(CurrentZone is DataSets.FZoneClass);
          FFinalZones.Add(CurrentZone);
        end;
      end;
    end;
  finally
    Layers.Free;
  end;
end;

destructor TCustomZoneGroup.Destroy;
begin
  FFinalZones.Free;
  inherited;
end;

function TCustomZoneGroup.GetMergeInXDirection: boolean;
begin
  result := True;
end;

function TCustomZoneGroup.GetMergeInYDirection: boolean;
begin
  result := True;
end;

function TCustomZoneGroup.GetMergeInZDirection: boolean;
begin
  result := True;
end;

function TCustomZoneGroup.GetZone(Index: integer): TCustomPhastZone;
begin
  result := FFinalZones[Index];
end;

function TCustomZoneGroup.ZoneCount: integer;
begin
  result := FFinalZones.Count;
end;

{ TBProperty }

procedure TBProperty.Assign(const DataSet: TDataArray; const LayerIndex,
  RowIndex, ColIndex: integer);
begin
  inherited Assign(DataSet, LayerIndex, RowIndex, ColIndex);
  if IsValue then
  begin
    BVAlue := DataSet.BooleanData[LayerIndex, RowIndex, ColIndex];
  end;
end;

function TBProperty.DataType: TPropertyType;
begin
  result := ptBoolean;
end;

function TBProperty.GetBValue: boolean;
begin
  result := FBValue;
end;

function TBProperty.IsIdentical(AProperty: TCustomProperty): boolean;
begin
  with (AProperty as TBProperty) do
  begin
    result := self.IsValue = IsValue;
    if result and self.IsValue then
    begin
      result := self.BValue = BValue;
    end
  end;
end;

procedure TBProperty.SetBValue(const Value: boolean);
begin
  FBValue := Value;
end;

{ TZoneList }

function TZoneList.Add(AZone: TCustomPhastZone): integer;
begin
  result := FList.Add(AZone);
end;

constructor TZoneList.Create;
begin
  inherited;
  FList := TList.Create;
end;

procedure TZoneList.Delete(Index: integer);
begin
  FList.Delete(Index);
end;

destructor TZoneList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TZoneList.GetCapacity: integer;
begin
  result := FList.Capacity;
end;

function TZoneList.GetCount: integer;
begin
  result := FList.Count;
end;

function TZoneList.GetItems(Index: integer): TCustomPhastZone;
begin
  result := FList[Index];
end;

function TZoneList.IndexOf(const AZone: TCustomPhastZone): integer;
begin
  result := FList.IndexOf(AZone)
end;

procedure TZoneList.SetCapacity(const Value: integer);
begin
  FList.Capacity := Value;
end;

procedure TZoneList.SetItems(Index: integer; const Value: TCustomPhastZone);
begin
  FList[Index] := Value;
end;

{ TIPropertyList }

function TIPropertyList.Add(IProperty: TIProperty): integer;
begin
  result := FList.Add(IProperty);
end;

constructor TIPropertyList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TIPropertyList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TIPropertyList.GetCount: integer;
begin
  result := FList.Count;
end;

function TIPropertyList.GetItem(Index: integer): TIProperty;
begin
  result := FList[Index];
end;

{ TRPropertyList }

function TRPropertyList.Add(RProperty: TRProperty): integer;
begin
  result := FList.Add(RProperty)
end;

constructor TRPropertyList.Create;
begin
  FList := TObjectList.Create;
end;

destructor TRPropertyList.Destroy;
begin
  FList.Free;
  inherited;
end;

function TRPropertyList.GetCount: integer;
begin
  result := FList.Count;
end;

function TRPropertyList.GetItem(Index: integer): TRProperty;
begin
  result := FList[Index];
end;

end.


