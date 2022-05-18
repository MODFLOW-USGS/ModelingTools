unit GeoRefUnit;

interface

uses
  Classes, GoPhastTypes{, CustomModflowWriterUnit};

type
  TProjectionType = (ptEpsg, ptProj4);

  TGeoRef = class(TGoPhastPersistent)
  private
    FStartTime: TDateTime;
    FProjection: string;
    FStartDate: TDateTime;
    FModel: TBaseModel;
    FPhastTimeUnits: TTimeUnits;
    FPhastLengthUnits: TLengthUnits;
    FOtherTimeUnits: string;
    FModflowTimeUnits: Integer;
    FOtherLengthUnits: string;
    FModflowLengthUnits: Integer;
    FRotation: Double;
    FModelType: TModelSelection;
    FUpperLeftX: Double;
    FUpperLeftY: Double;
    FProjectionType: TProjectionType;
    function GetModeltype: TModelSelection;
    function GetUpperLeftX: Double;
    function GetUpperLeftY: Double;
    function GetModflowLengthUnits: Integer;
    function GetModflowTimeUnits: Integer;
    function GetPhastLengthUnits: TLengthUnits;
    function GetPhastTimeUnits: TTimeUnits;
    function GetRotation: Double;
    procedure SetPhastLengthUnits(const Value: TLengthUnits);
    procedure SetProjection(const Value: string);
    procedure SetStartDate(const Value: TDateTime);
    procedure SetStartTime(const Value: TDateTime);
    procedure SetModflowLengthUnits(const Value: Integer);
    procedure SetModflowTimeUnits(const Value: Integer);
    procedure SetOtherLengthUnits(const Value: string);
    procedure SetOtherTimeUnits(const Value: string);
    procedure SetPhastTimeUnits(const Value: TTimeUnits);
    procedure SetModeltype(const Value: TModelSelection);
    procedure SetRotation(const Value: Double);
    procedure SetUpperLeftX(const Value: Double);
    procedure SetUpperLeftY(const Value: Double);
    procedure SetProjectionType(const Value: TProjectionType);
  public
    constructor Create(Model: TBaseModel);
    procedure Assign(Source: TPersistent); override;
    procedure Initialize;
    property UpperLeftX: Double read GetUpperLeftX write SetUpperLeftX;
    property UpperLeftY: Double read GetUpperLeftY write SetUpperLeftY;
    // @name is measured in radians.
    property Rotation: Double read GetRotation write SetRotation;
    property Modeltype: TModelSelection read GetModeltype write SetModeltype;
    property PhastLengthUnits: TLengthUnits read GetPhastLengthUnits write SetPhastLengthUnits;
    property PhastTimeUnits: TTimeUnits read GetPhastTimeUnits write SetPhastTimeUnits;
    property ModflowLengthUnits: Integer read GetModflowLengthUnits write SetModflowLengthUnits;
    property ModflowTimeUnits: Integer read GetModflowTimeUnits write SetModflowTimeUnits;
    function LengthUnit: string;
    function TimeUnit: string;
    function ModelProgramName: string;
  published
    property OtherLengthUnits: string read FOtherLengthUnits write SetOtherLengthUnits;
    property OtherTimeUnits: string read FOtherTimeUnits write SetOtherTimeUnits;
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property Projection: string read FProjection write SetProjection;
    property ProjectionType: TProjectionType read FProjectionType write SetProjectionType;

//xul 1157053.959            # x-coordinate of the upper left of the model grid
//yul 405727.084             # y-coordinate of the upper left of the model grid
//rotation -45.957964853112  # rotation of the model grid in degrees
//length_units feet          # length unit of the model grid (feet, meters, etc.)
//time_units days            # time unit of the model grid (days, years, etc.)
//start_date 1/1/1900        # state date of the model
//start_time 00:00:00        # start time of the model
//model modflow-2000         # MODFLOW model type (MODFLOW-NWT, etc.)
//epsg 102733                # or proj4 string instead of epsg code
//# proj4 'proj4 string'     # or epsg code instead of proj4 string

  end;

implementation

uses
  PhastModelUnit, SutraMeshUnit, AbstractGridUnit;

{ TGeoRef }

procedure TGeoRef.Assign(Source: TPersistent);
var
  GeoRefSource: TGeoRef;
begin
  if Source is TGeoRef then
  begin
    GeoRefSource := TGeoRef(Source);
    UpperLeftX := GeoRefSource.UpperLeftX;
    UpperLeftY := GeoRefSource.UpperLeftY;
    Rotation := GeoRefSource.Rotation;
    Modeltype := GeoRefSource.Modeltype;
    PhastLengthUnits := GeoRefSource.PhastLengthUnits;
    PhastTimeUnits := GeoRefSource.PhastTimeUnits;
    ModflowLengthUnits := GeoRefSource.ModflowLengthUnits;
    ModflowTimeUnits := GeoRefSource.ModflowTimeUnits;
    OtherLengthUnits := GeoRefSource.OtherLengthUnits;
    OtherTimeUnits := GeoRefSource.OtherTimeUnits;
    StartDate := GeoRefSource.StartDate;
    StartTime := GeoRefSource.StartTime;
    Projection := GeoRefSource.Projection;
    ProjectionType := GeoRefSource.ProjectionType;
  end
  else
  begin
    inherited;
  end;
end;

constructor TGeoRef.Create(Model: TBaseModel);
var
  OnChange: TNotifyEvent;
begin
  FModel := Model;
  if FModel = nil then
  begin
    OnChange := nil;
  end
  else
  begin
    OnChange := FModel.Invalidate;
  end;
  inherited Create(OnChange);
end;


function TGeoRef.GetModeltype: TModelSelection;
begin
  if FModel = nil then
  begin
    result := FModeltype
  end
  else
  begin
    result := FModel.ModelSelection;
  end;
end;

function TGeoRef.GetModflowLengthUnits: Integer;
begin
  if FModel = nil then
  begin
    result := FModflowLengthUnits;
  end
  else
  begin
    result := (FModel as TCustomModel).ModflowOptions.LengthUnit;
  end;
end;

function TGeoRef.GetModflowTimeUnits: Integer;
begin
  if FModel = nil then
  begin
    result := FModflowTimeUnits;
  end
  else
  begin
    result := (FModel as TCustomModel).ModflowOptions.TimeUnit;
  end;
end;

function TGeoRef.GetPhastLengthUnits: TLengthUnits;
begin
  if FModel = nil then
  begin
    result := FPhastLengthUnits;
  end
  else
  begin
    result := ((FModel as TCustomModel).ParentModel
      as TPhastModel).Units.DefaultHorizontalGridUnits;
  end;
end;

function TGeoRef.GetPhastTimeUnits: TTimeUnits;
begin
  if FModel = nil then
  begin
    result := FPhastTimeUnits;
  end
  else
  begin
    result := ((FModel as TCustomModel).ParentModel
      as TPhastModel).Units.DefaultTimeUnits;
  end;
end;

function TGeoRef.GetRotation: Double;
begin
  if FModel = nil then
  begin
    result := FRotation;
  end
  else
  begin
    if (FModel as TCustomModel).Grid = nil then
    begin
      Result := 0;
    end
    else
    begin
      result := (FModel as TCustomModel).Grid.GridAngle;
    end;
  end;
end;

function TGeoRef.GetUpperLeftX: Double;
var
//  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
begin
  if FModel = nil then
  begin
    result := FUpperLeftX;
  end
  else
  begin
    if (FModel as TCustomModel).Grid = nil then
    begin
      result := 0;
//      Mesh := (FModel as TCustomModel).Mesh;
//      result := Mesh.MeshLimits(vdTop, 0).MinX;
    end
    else
    begin
      Grid := (FModel as TCustomModel).Grid;
      if (Grid.ColumnCount > 0) and (Grid.RowCount > 0) then
      begin
        if Modeltype = msPhast then
        begin
          result := Grid.TwoDCellCorner(0, Grid.RowCount).x;
        end
        else
        begin
          result := Grid.TwoDCellCorner(0, 0).x;
        end;
      end
      else
      begin
        Result := 0;
      end;
    end;
  end;
end;

function TGeoRef.GetUpperLeftY: Double;
var
//  Mesh: TSutraMesh3D;
  Grid: TCustomModelGrid;
begin
  if FModel = nil then
  begin
    result := FUpperLeftY;
  end
  else
  begin
    if (FModel as TCustomModel).Grid = nil then
    begin
      result := 0;
//      Mesh := (FModel as TCustomModel).Mesh;
//      result := Mesh.MeshLimits(vdTop, 0).MinX;
    end
    else
    begin
      Grid := (FModel as TCustomModel).Grid;
      if (Grid.ColumnCount > 0) and (Grid.RowCount > 0) then
      begin
        if Modeltype = msPhast then
        begin
          result := Grid.TwoDCellCorner(0, Grid.RowCount).y;
        end
        else
        begin
          result := Grid.TwoDCellCorner(0, 0).y;
        end;
      end
      else
      begin
        result := 0;
      end;
    end;
  end;
end;

procedure TGeoRef.Initialize;
begin
  FStartTime := 0;
  FProjection := '';
  FStartDate := 0;
  FOtherTimeUnits := '';
  FOtherLengthUnits := '';
  FProjectionType := ptEpsg;
  if FModel = nil then
  begin
    FPhastTimeUnits := tuSeconds;
    FPhastLengthUnits := luMeters;
    FModflowTimeUnits := 0;
    FModflowLengthUnits := 0;
    FRotation := 0;
    FModelType := msModflow;
    FUpperLeftX := 0;
    FUpperLeftY := 0;
  end;

end;

function TGeoRef.LengthUnit: string;
begin
  case Modeltype of
    msPhast:
      begin
        case PhastLengthUnits of
          luInches: result := 'inch';
          luFeet: result := 'feet';
          luMiles: result := 'miles';
          luMillimeters: result := 'millimeters';
          luCentimeters: result := 'centimeters';
          luMeters: result := 'meters';
          luKilometers: result := 'kilometers';
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflow2015:
      begin
        case ModflowLengthUnits of
          0:
            begin
              result := 'undefined';
            end;
          1:
            begin
              result := 'feet';
            end;
          2:
            begin
              result := 'meters';
            end;
          3:
            begin
              result := 'centimeters';
            end;
          else
            begin
              result := 'undefined';
            end;
        end;
      end;
    else
      begin
        result := OtherLengthUnits;
      end;
  end;
end;

function TGeoRef.ModelProgramName: string;
begin
  case Modeltype of
    msPhast: result := 'PHAST';
    msModflow: Result := 'MODFLOW-2005';
    msModflowLGR: Result := 'MODFLOW-LGR version 1';
    msModflowLGR2: Result := 'MODFLOW-LGR version 2';
    msModflowNWT: Result := 'MODFLOW-NWT';
    msModflowFmp: Result := 'MODFLOW-OWM';
    msModflowCfp: Result := 'MODFLOW-CFP';
    msSutra22: Result := 'SUTRA 2.2';
    msSutra30: Result := 'SUTRA 3.0';
    msSutra40: Result := 'SUTRA 4.0';
    msFootPrint: Result := 'WellFootprint';
    msModflow2015: Result := 'MODFLOW 6';
    else
      Assert(False);
  end;
end;

procedure TGeoRef.SetModeltype(const Value: TModelSelection);
begin
  FModelType := Value;
end;

procedure TGeoRef.SetModflowLengthUnits(const Value: Integer);
begin
  FModflowLengthUnits := Value;
end;

procedure TGeoRef.SetModflowTimeUnits(const Value: Integer);
begin
  FModflowTimeUnits := Value;
end;

procedure TGeoRef.SetOtherLengthUnits(const Value: string);
begin
  SetStringProperty(FOtherLengthUnits, Value);
end;

procedure TGeoRef.SetOtherTimeUnits(const Value: string);
begin
  SetStringProperty(FOtherTimeUnits, Value);
end;

procedure TGeoRef.SetPhastLengthUnits(const Value: TLengthUnits);
begin
  FPhastLengthUnits := Value;
end;

procedure TGeoRef.SetPhastTimeUnits(const Value: TTimeUnits);
begin
  FPhastTimeUnits := Value;
end;

procedure TGeoRef.SetProjection(const Value: string);
begin
  SetStringProperty(FProjection, Value);
end;

procedure TGeoRef.SetProjectionType(const Value: TProjectionType);
begin
  if FProjectionType <> Value then
  begin
    FProjectionType := Value;
    InvalidateModel;
  end;
end;

procedure TGeoRef.SetRotation(const Value: Double);
begin
  FRotation := Value;
end;

procedure TGeoRef.SetStartDate(const Value: TDateTime);
begin
  SetDataTimeProperty(FStartDate, Value);
end;

procedure TGeoRef.SetStartTime(const Value: TDateTime);
begin
  SetDataTimeProperty(FStartTime, Value)
end;

procedure TGeoRef.SetUpperLeftX(const Value: Double);
begin
  FUpperLeftX := Value;
end;

procedure TGeoRef.SetUpperLeftY(const Value: Double);
begin
  FUpperLeftY := Value;
end;

function TGeoRef.TimeUnit: string;
begin
  case Modeltype of
    msPhast:
      begin
        case PhastTimeUnits of
          tuSeconds: result := 'seconds';
          tuMinutes: result := 'minutes';
          tuHours: result := 'hours';
          tuDays: result := 'days';
          tuYears: result := 'years';
        end;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT, msModflowFmp,
      msModflowCfp, msModflow2015:
      begin
        case ModflowTimeUnits of
          0:
            begin
              result := 'undefined';
            end;
          1:
            begin
              result := 'seconds';
            end;
          2:
            begin
              result := 'minutes';
            end;
          3:
            begin
              result := 'hours';
            end;
          4:
            begin
              result := 'days';
            end;
          5:
            begin
              result := 'years';
            end;
          else
            begin
              result := 'undefined';
            end;
        end;
      end;
    else
      begin
        result := OtherTimeUnits;
      end;
  end;
end;

{ TGeoRefWriter }

end.
