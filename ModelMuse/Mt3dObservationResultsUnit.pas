unit Mt3dObservationResultsUnit;

interface

uses
  System.SysUtils, System.Classes, Vcl.Dialogs, System.Generics.Collections,
  Graphics, GR32, GR32_Polygons, ZoomBox2, GoPhastTypes, DataSetUnit;

type
  TMt3dDrawChoice = (dcmResidual, dcmWeightedResidual);

  TObsNameDictionary = TDictionary<string, string>;

  TMt3dObsCollection = class;

  TMt3dObsResult = class(TPhastCollectionItem)
  private
    FName: string;
    FStoredSimulatedValue: TRealStorage;
    FStoredTime: TRealStorage;
    FMt3dObsCollection: TMt3dObsCollection;
    FScreenObjectName: string;
    FWeightedResidualText: string;
    FPlotLabel: Boolean;
    FWeightedSimulatedText: string;
    FWeightText: string;
    FWeightedMeasuredText: string;
    FVisible: Boolean;
    FResidualText: string;
    FStoredWeightedSimulated: TRealStorage;
    FStoredWeight: TRealStorage;
    FStoredWeightedMeasured: TRealStorage;
    FStoredResidual: TRealStorage;
    FStoredMeasured: TRealStorage;
    FStoredWeightedResidual: TRealStorage;
    FScreenObject: TObject;
    Fx: double;
    Fy: double;
    procedure SetName(const Value: string);
    procedure SetSimulatedValue(const Value: double);
    procedure SetStoredSimulatedValue(const Value: TRealStorage);
    procedure SetStoredTime(const Value: TRealStorage);
    procedure SetTime(const Value: double);
    function GetSimulatedValue: double;
    function GetTime: double;
    procedure SetScreenObjectName(const Value: string);
    procedure SetPlotLabel(const Value: Boolean);
    procedure SetResidualText(const Value: string);
    procedure SetVisible(const Value: Boolean);
    procedure SetWeightedMeasuredText(const Value: string);
    procedure SetWeightedSimulatedText(const Value: string);
    procedure SetWeightedResidualText(const Value: string);
    procedure SetWeightText(const Value: string);
    function GetMeasured: double;
    function GetResidual: double;
    function GetWeight: double;
    function GetWeightedMeasured: double;
    function GetWeightedSimulated: double;
    function GetWeightedResidual: double;
    procedure SetMeasured(const Value: double);
    procedure SetResidual(const Value: double);
    procedure SetStoredMeasured(const Value: TRealStorage);
    procedure SetStoredResidual(const Value: TRealStorage);
    procedure SetStoredWeight(const Value: TRealStorage);
    procedure SetStoredWeightedMeasured(const Value: TRealStorage);
    procedure SetStoredWeightedSimulated(const Value: TRealStorage);
    procedure SetStoredWeightedResidual(const Value: TRealStorage);
    procedure SetWeight(const Value: double);
    procedure SetWeightedMeasured(const Value: double);
    procedure SetWeightedSimulated(const Value: double);
    procedure SetWeightedResidual(const Value: double);
    procedure Draw(const BitMap: TPersistent; const ZoomBox: TQrbwZoomBox2);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    property Time: double read GetTime write SetTime;
    property SimulatedValue: double read GetSimulatedValue write SetSimulatedValue;
    property Measured: double read GetMeasured write SetMeasured;
    property Residual: double read GetResidual write SetResidual;
    property Weight: double read GetWeight write SetWeight;
    property WeightedMeasured: double read GetWeightedMeasured write SetWeightedMeasured;
    property WeightedSimulated: double read GetWeightedSimulated write SetWeightedSimulated;
    property WeightedResidual: double read GetWeightedResidual write SetWeightedResidual;
    property X: double read Fx;
    property Y: double read Fy;
  published
    property Name: string read FName write SetName;
    property ScreenObjectName: string read FScreenObjectName write SetScreenObjectName;
    property ScreenObject: TObject read FScreenObject;
    property StoredTime: TRealStorage read FStoredTime write SetStoredTime;
    property StoredSimulatedValue: TRealStorage read FStoredSimulatedValue
      write SetStoredSimulatedValue;
    property StoredMeasured: TRealStorage read FStoredMeasured write SetStoredMeasured;
    property StoredResidual: TRealStorage read FStoredResidual write SetStoredResidual;
    property StoredWeight: TRealStorage read FStoredWeight write SetStoredWeight;
    property StoredWeightedMeasured: TRealStorage read FStoredWeightedMeasured write SetStoredWeightedMeasured;
    property StoredWeightedSimulated: TRealStorage read FStoredWeightedSimulated write SetStoredWeightedSimulated;
    property StoredWeightedResidual: TRealStorage read FStoredWeightedResidual write SetStoredWeightedResidual;
    property ResidualText: string read FResidualText write SetResidualText;
    property WeightText: string read FWeightText write SetWeightText;
    property WeightedMeasuredText: string read FWeightedMeasuredText write SetWeightedMeasuredText;
    property WeightedSimulatedText: string read FWeightedSimulatedText write SetWeightedSimulatedText;
    property WeightedResidualText: string read FWeightedResidualText write SetWeightedResidualText;
    property Visible: Boolean read FVisible write SetVisible;
    property PlotLabel: Boolean read FPlotLabel write SetPlotLabel stored True;
  end;

  TMt3dObsCollection = class(TPhastCollection)
  private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
    FFileName: string;
    FFileDate: TDateTime;
    FMaxObjectWeightedResidual: double;
    FMaxObjectResidual: double;
    FMaxTimeLimit: TColoringLimit;
    FMinWeightedResidualLimit: TColoringLimit;
    FMaxResidualLimit: TColoringLimit;
    FMinTimeLimit: TColoringLimit;
    FMinResidualLimit: TColoringLimit;
    FMaxWeightedResidualLimit: TColoringLimit;
    FPositiveColor: TColor;
    FNegativeColor: TColor;
    FPositiveColor32: TColor32;
    FNegativeColor32: TColor32;
    FMaxSymbolSize: integer;
    FDrawChoice: TMt3dDrawChoice;
    FVisible: boolean;
    procedure SetMaxResidualLimit(const Value: TColoringLimit);
    procedure SetMaxTimeLimit(const Value: TColoringLimit);
    procedure SetMaxWeightedResidualLimit(const Value: TColoringLimit);
    procedure SetMinResidualLimit(const Value: TColoringLimit);
    procedure SetMinTimeLimit(const Value: TColoringLimit);
    procedure SetMinWeightedResidualLimit(const Value: TColoringLimit);
    procedure InitializeVariables;
    procedure SetNegativeColor(const Value: TColor);
    procedure SetPositiveColor(const Value: TColor);
    procedure SetMaxSymbolSize(const Value: integer);
    procedure SetDrawChoice(const Value: TMt3dDrawChoice);
    procedure SetVisible(const Value: boolean);
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ReadFromFile(const AFileName: string; ScreenObjectNameDictionary: TObsNameDictionary): boolean;
    function Add: TMt3dObsResult;
    function GetItem(Index: Integer): TMt3dObsResult;
    procedure SetItem(Index: Integer; const Value: TMt3dObsResult);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetFileName(const Value: string);
    property Items[Index: Integer]: TMt3dObsResult read GetItem
      write SetItem; default;
    procedure Draw(const BitMap: TPersistent; const ZoomBox: TQrbwZoomBox2);
    function RootMeanSquareResidual: double;
    function RootMeanSquareWeightedResidual: double;
    property MaxObjectResidual: double read FMaxObjectResidual;
    property MaxObjectWeightedResidual: double read FMaxObjectWeightedResidual;
    procedure CalculateMaxValues;
    procedure Loaded;
  published
    property FileName: string read FFileName write SetFileName;
    property FileDate: TDateTime read FFileDate write SetFileDate;
    property MaxResidualLimit: TColoringLimit read FMaxResidualLimit
      write SetMaxResidualLimit;
    property MinResidualLimit: TColoringLimit read FMinResidualLimit
      write SetMinResidualLimit;
    property MaxWeightedResidualLimit: TColoringLimit read FMaxWeightedResidualLimit
      write SetMaxWeightedResidualLimit;
    property MinWeightedResidualLimit: TColoringLimit read FMinWeightedResidualLimit
      write SetMinWeightedResidualLimit;
    property MaxTimeLimit: TColoringLimit read FMaxTimeLimit
      write SetMaxTimeLimit;
    property MinTimeLimit: TColoringLimit read FMinTimeLimit
      write SetMinTimeLimit;
    property NegativeColor: TColor read FNegativeColor
      write SetNegativeColor default clRed;
    property PositiveColor: TColor read FPositiveColor
      write SetPositiveColor default clBlue;
    property MaxSymbolSize: integer read FMaxSymbolSize
      write SetMaxSymbolSize default 20;
    property Visible: boolean read FVisible write SetVisible default True;
    property DrawChoice: TMt3dDrawChoice read FDrawChoice
      write SetDrawChoice default dcmWeightedResidual;
  end;


implementation

uses
  System.IOUtils, PhastModelUnit, ScreenObjectUnit, Mt3dmsTobUnit,
  BigCanvasMethods, frmErrorsAndWarningsUnit;

resourcestring
  StrTheFileFromWhich = 'The file from which you are attempting to read ' +
  'MT3D bservation results, %s, does not exist.';
  StrNotRecorded = 'Not recorded';
  StrNoObservationsReco = 'No observations recorded';
  StrSIsEmptyExceptF = '%s is empty except for a header.';

{ TMt3dObsResult }

procedure TMt3dObsResult.Assign(Source: TPersistent);
var
  ObsSource: TMt3dObsResult;
begin
  if Source is TMt3dObsResult then
  begin
    ObsSource := TMt3dObsResult(Source);
    Name := ObsSource.Name;
    Time := ObsSource.Time;
    SimulatedValue := ObsSource.SimulatedValue;
    Measured := ObsSource.Measured;
    Residual := ObsSource.Residual;
    Weight := ObsSource.Weight;
    WeightedMeasured := ObsSource.WeightedMeasured;
    WeightedSimulated := ObsSource.WeightedSimulated;
    WeightedResidual := ObsSource.WeightedResidual;

    ScreenObjectName := ObsSource.ScreenObjectName;

    ResidualText := ObsSource.ResidualText;
    WeightText := ObsSource.WeightText;
    WeightedMeasuredText := ObsSource.WeightedMeasuredText;
    WeightedSimulatedText := ObsSource.WeightedSimulatedText;
    WeightedResidualText := ObsSource.WeightedResidualText;
    Visible := ObsSource.Visible;
    PlotLabel := ObsSource.PlotLabel;

  end
  else
  begin
    inherited;
  end;
end;

constructor TMt3dObsResult.Create(Collection: TCollection);
var
  InvalidateModelEvent: TNotifyEvent;
  LocalModel: TBaseModel;
begin
  FMt3dObsCollection := Collection as TMt3dObsCollection;
  LocalModel := FMt3dObsCollection.FModel;
  if LocalModel = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := LocalModel.DoInvalidate;
  end;
  FStoredSimulatedValue := TRealStorage.Create(InvalidateModelEvent);
  FStoredTime := TRealStorage.Create(InvalidateModelEvent);

  FStoredWeightedSimulated := TRealStorage.Create(InvalidateModelEvent);
  FStoredWeight := TRealStorage.Create(InvalidateModelEvent);
  FStoredWeightedMeasured := TRealStorage.Create(InvalidateModelEvent);
  FStoredResidual := TRealStorage.Create(InvalidateModelEvent);
  FStoredMeasured := TRealStorage.Create(InvalidateModelEvent);
  FStoredWeightedResidual := TRealStorage.Create(InvalidateModelEvent);

  inherited;

end;

destructor TMt3dObsResult.Destroy;
begin
  FStoredTime.Free;
  FStoredSimulatedValue.Free;

  FStoredWeightedSimulated.Free;
  FStoredWeight.Free;
  FStoredWeightedMeasured.Free;
  FStoredResidual.Free;
  FStoredMeasured.Free;
  FStoredWeightedResidual.Free;

  inherited;
end;

procedure TMt3dObsResult.Draw(const BitMap: TPersistent;
  const ZoomBox: TQrbwZoomBox2);
const
  MaxPoints = 12;
  PointsPerHalfCircle = MaxPoints div 2;
var
  XCenter: Integer;
  YCenter: Integer;
  Radius: Double;
  Points: TPointArray;
  PointIndex: Integer;
  Angle: double;
  Color: TColor32;
  APolygon: TPolygon32;
  ClipRect: TRect;
  MaxValue: double;
  Value: Double;
  function GetClipRect(Graphic: TPersistent): TRect;
  begin
    if Graphic is TBitmap32 then
    begin
      result := TBitmap32(Graphic).Canvas.ClipRect;
    end
    else
    begin
      result := (Graphic as TCanvas).ClipRect;
    end;
  end;
begin
  if not Visible then
  begin
    Exit;
  end;
  XCenter := ZoomBox.XCoord(X);
  YCenter := ZoomBox.YCoord(Y);
  if Residual > 0 then
  begin
    Color := FMt3dObsCollection.FPositiveColor32;
  end
  else
  begin
    Color := FMt3dObsCollection.FNegativeColor32;
  end;
  MaxValue := 1;
  Value := 0;
  case FMt3dObsCollection.DrawChoice of
    dcmResidual:
      begin
        MaxValue := FMt3dObsCollection.MaxObjectResidual;
        Value := Residual;
      end;
    dcmWeightedResidual:
      begin
        MaxValue := FMt3dObsCollection.MaxObjectWeightedResidual;
        Value := WeightedResidual;
      end;
    else
      begin
        Assert(False);
      end;
  end;
  Radius :=
    Sqrt(Abs(Value)/MaxValue)
    * (FMt3dObsCollection.MaxSymbolSize / 2);

  ClipRect := GetClipRect(BitMap);
  if XCenter + Radius < ClipRect.Left then
  begin
    Exit;
  end;
  if XCenter - Radius > ClipRect.Right then
  begin
    Exit;
  end;
  if YCenter + Radius < ClipRect.Top then
  begin
    Exit;
  end;
  if YCenter - Radius > ClipRect.Bottom then
  begin
    Exit;
  end;

  SetLength(Points, MaxPoints);
  for PointIndex := 0 to MaxPoints - 1 do
  begin
    Angle := PointIndex * Pi / PointsPerHalfCircle;
    Points[PointIndex].X := Round(XCenter + Cos(Angle)*Radius);
    Points[PointIndex].Y := Round(YCenter + Sin(Angle)*Radius);
  end;
  APolygon := nil;
  DrawBigPolygon32(BitMap, Color, Color, 0.1, Points, APolygon, False, True);
  if PlotLabel then
  begin
    DrawBigText(BitMap, Point(XCenter, YCenter), Name);
  end;
end;

function TMt3dObsResult.GetMeasured: double;
begin
  result := StoredMeasured.Value;
end;

function TMt3dObsResult.GetResidual: double;
begin
  result := StoredResidual.Value;
end;

function TMt3dObsResult.GetSimulatedValue: double;
begin
  result := FStoredSimulatedValue.Value;
end;

function TMt3dObsResult.GetTime: double;
begin
  result := FStoredTime.Value;
end;

function TMt3dObsResult.GetWeight: double;
begin
  result := StoredWeight.Value;
end;

function TMt3dObsResult.GetWeightedMeasured: double;
begin
  result := StoredWeightedMeasured.Value;
end;

function TMt3dObsResult.GetWeightedSimulated: double;
begin
  result := StoredWeightedSimulated.Value;
end;

function TMt3dObsResult.GetWeightedResidual: double;
begin
  result := StoredWeightedResidual.Value;
end;

procedure TMt3dObsResult.SetMeasured(const Value: double);
begin
  StoredMeasured.Value := Value;
end;

procedure TMt3dObsResult.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

procedure TMt3dObsResult.SetPlotLabel(const Value: Boolean);
begin
  FPlotLabel := Value;
end;

procedure TMt3dObsResult.SetResidual(const Value: double);
begin
  StoredResidual.Value := Value;
end;

procedure TMt3dObsResult.SetResidualText(const Value: string);
begin
  FResidualText := Value;
end;

procedure TMt3dObsResult.SetScreenObjectName(const Value: string);
begin
  SetStringProperty(FScreenObjectName, Value);
end;

procedure TMt3dObsResult.SetSimulatedValue(const Value: double);
begin
  FStoredSimulatedValue.Value := Value;
end;

procedure TMt3dObsResult.SetStoredMeasured(const Value: TRealStorage);
begin
  FStoredMeasured.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredResidual(const Value: TRealStorage);
begin
  FStoredResidual.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredSimulatedValue(const Value: TRealStorage);
begin
  FStoredSimulatedValue.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredTime(const Value: TRealStorage);
begin
  FStoredTime.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredWeight(const Value: TRealStorage);
begin
  FStoredWeight.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredWeightedMeasured(const Value: TRealStorage);
begin
  FStoredWeightedMeasured.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredWeightedSimulated(const Value: TRealStorage);
begin
  FStoredWeightedSimulated.Assign(Value);
end;

procedure TMt3dObsResult.SetStoredWeightedResidual(const Value: TRealStorage);
begin
  FStoredWeightedResidual.Assign(Value);
end;

procedure TMt3dObsResult.SetTime(const Value: double);
begin
  FStoredTime.Value := Value;
end;

procedure TMt3dObsResult.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TMt3dObsResult.SetWeight(const Value: double);
begin
  FStoredWeight.Value := Value;
end;

procedure TMt3dObsResult.SetWeightedMeasured(const Value: double);
begin
  FStoredWeightedMeasured.Value := Value;
end;

procedure TMt3dObsResult.SetWeightedMeasuredText(const Value: string);
begin
  FWeightedMeasuredText := Value;
end;

procedure TMt3dObsResult.SetWeightedSimulated(const Value: double);
begin
  FStoredWeightedSimulated.Value := Value;
end;

procedure TMt3dObsResult.SetWeightedSimulatedText(const Value: string);
begin
  FWeightedSimulatedText := Value;
end;

procedure TMt3dObsResult.SetWeightedResidual(const Value: double);
begin
  FStoredWeightedResidual.Value := Value;
end;

procedure TMt3dObsResult.SetWeightedResidualText(const Value: string);
begin
  FWeightedResidualText := Value;
end;

procedure TMt3dObsResult.SetWeightText(const Value: string);
begin
  FWeightText := Value;
end;

{ TMt3dObsCollection }

function TMt3dObsCollection.Add: TMt3dObsResult;
begin
  result := inherited Add as TMt3dObsResult
end;

procedure TMt3dObsCollection.Assign(Source: TPersistent);
var
  SourceCollection: TMt3dObsCollection;
begin
  if Source is TMt3dObsCollection then
  begin
    SourceCollection := TMt3dObsCollection(Source);
    FileName := SourceCollection.FileName;
    FileDate := SourceCollection.FileDate;

    MaxResidualLimit := SourceCollection.MaxResidualLimit;
    MinResidualLimit := SourceCollection.MinResidualLimit;
    MaxWeightedResidualLimit := SourceCollection.MaxWeightedResidualLimit;
    MinWeightedResidualLimit := SourceCollection.MinWeightedResidualLimit;
    MaxTimeLimit := SourceCollection.MaxTimeLimit;
    MinTimeLimit := SourceCollection.MinTimeLimit;
    NegativeColor := SourceCollection.NegativeColor;
    PositiveColor := SourceCollection.PositiveColor;
    MaxSymbolSize := SourceCollection.MaxSymbolSize;
    Visible := SourceCollection.Visible;
    DrawChoice := SourceCollection.DrawChoice;
  end;
  inherited;
  CalculateMaxValues;
end;


procedure TMt3dObsCollection.CalculateMaxValues;
var
  Obs: TMt3dObsResult;
  ObsIndex: Integer;
  ScreenObject: TScreenObject;
begin
  FMaxObjectResidual := 0;
  FMaxObjectWeightedResidual := 0;
  try
    for ObsIndex := 0 to Count - 1 do
    begin
      Obs := Items[ObsIndex];
      if Obs.ScreenObjectName <> '' then
      begin
        ScreenObject := (FModel as TCustomModel).GetScreenObjectByName(Obs.ScreenObjectName);
        Obs.FScreenObject := ScreenObject;
      end;

      if (Obs.ScreenObject <> nil) then
      begin
        ScreenObject := Obs.ScreenObject as TScreenObject;
        if (ScreenObject.Count = 1) then
        begin
          Obs.Visible := True;
          Obs.FX := ScreenObject.Points[0].X;
          Obs.Fy := ScreenObject.Points[0].Y;
          if MaxResidualLimit.UseLimit
            and (Obs.Residual > MaxResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinResidualLimit.UseLimit
            and (Obs.Residual < MinResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MaxWeightedResidualLimit.UseLimit
            and (Obs.WeightedResidual > MaxWeightedResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinWeightedResidualLimit.UseLimit
            and (Obs.WeightedResidual < MinWeightedResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MaxTimeLimit.UseLimit
            and (Obs.Time > MaxTimeLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinTimeLimit.UseLimit
            and (Obs.Time < MinTimeLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end;
          if Obs.Visible then
          begin
  //          UsedObs.Add(Obs);
            if Abs(Obs.Residual) > FMaxObjectResidual then
            begin
              FMaxObjectResidual := Abs(Obs.Residual);
            end;
            if Abs(Obs.WeightedResidual) > FMaxObjectWeightedResidual then
            begin
              FMaxObjectWeightedResidual := Abs(Obs.WeightedResidual);
            end;
          end;
        end;
      end;
    end;
//    for ObsIndex := 0 to UsedObs.Count - 1 do
//    begin
//      Obs := UsedObs[ObsIndex];
//      Obs.Draw(BitMap, ZoomBox);
//    end;
  finally
//    UsedObs.Free;
  end;
end;

constructor TMt3dObsCollection.Create(Model: TBaseModel);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  FModel := Model;
  if Model = nil then
  begin
    InvalidateModelEvent := nil;
  end
  else
  begin
    InvalidateModelEvent := Model.DoInvalidate;
  end;
  inherited Create(TMt3dObsResult, InvalidateModelEvent);
  FMaxTimeLimit := TColoringLimit.Create;
  FMaxResidualLimit := TColoringLimit.Create;
  FMinTimeLimit := TColoringLimit.Create;
  FMinResidualLimit := TColoringLimit.Create;
  FMaxWeightedResidualLimit := TColoringLimit.Create;
  FMinWeightedResidualLimit := TColoringLimit.Create;

  InitializeVariables;
end;

destructor TMt3dObsCollection.Destroy;
begin
  FMinWeightedResidualLimit.Free;
  FMaxWeightedResidualLimit.Free;
  FMaxTimeLimit.Free;
  FMaxResidualLimit.Free;
  FMinTimeLimit.Free;
  FMinResidualLimit.Free;

  inherited;
end;

procedure TMt3dObsCollection.Draw(const BitMap: TPersistent;
  const ZoomBox: TQrbwZoomBox2);
var
  Obs: TMt3dObsResult;
  UsedObs: TList<TMt3dObsResult>;
  ObsIndex: Integer;
  ScreenObject: TScreenObject;
begin

  FMaxObjectResidual := 0;
  FMaxObjectWeightedResidual := 0;
  UsedObs := TList<TMt3dObsResult>.Create;
  try
    for ObsIndex := 0 to Count - 1 do
    begin
      Obs := Items[ObsIndex];
      if (Obs.ScreenObject <> nil) then
      begin
        ScreenObject := Obs.ScreenObject as TScreenObject;
        if (ScreenObject.Count = 1) then
        begin
          Obs.Visible := True;
          Obs.FX := ScreenObject.Points[0].X;
          Obs.Fy := ScreenObject.Points[0].Y;
          if MaxResidualLimit.UseLimit
            and (Obs.Residual > MaxResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinResidualLimit.UseLimit
            and (Obs.Residual < MinResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MaxWeightedResidualLimit.UseLimit
            and (Obs.WeightedResidual > MaxWeightedResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinWeightedResidualLimit.UseLimit
            and (Obs.WeightedResidual < MinWeightedResidualLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MaxTimeLimit.UseLimit
            and (Obs.Time > MaxTimeLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end
          else if MinTimeLimit.UseLimit
            and (Obs.Time < MinTimeLimit.RealLimitValue) then
          begin
            Obs.Visible := False;
          end;
          if Obs.Visible then
          begin
            UsedObs.Add(Obs);
            if Abs(Obs.Residual) > FMaxObjectResidual then
            begin
              FMaxObjectResidual := Abs(Obs.Residual);
            end;
            if Abs(Obs.WeightedResidual) > FMaxObjectWeightedResidual then
            begin
              FMaxObjectWeightedResidual := Abs(Obs.WeightedResidual);
            end;
          end;
        end;
      end;
    end;
    for ObsIndex := 0 to UsedObs.Count - 1 do
    begin
      Obs := UsedObs[ObsIndex];
      Obs.Draw(BitMap, ZoomBox);
    end;
  finally
    UsedObs.Free;
  end;
end;

function TMt3dObsCollection.GetItem(Index: Integer): TMt3dObsResult;
begin
  result := inherited Items[Index] as TMt3dObsResult
end;

procedure TMt3dObsCollection.InitializeVariables;
begin
  NegativeColor := clRed;
  PositiveColor := clBlue;
  FMaxSymbolSize := 20;
  FVisible := True;
  FFileName := '';
  FFileDate := 0;
  FMaxResidualLimit.UseLimit := False;
  FMinResidualLimit.UseLimit := False;
  FMaxWeightedResidualLimit.UseLimit := False;
  FMinWeightedResidualLimit.UseLimit := False;
  FMaxTimeLimit.UseLimit := False;
  FMinTimeLimit.UseLimit := False;
  FDrawChoice := dcmWeightedResidual;
end;

procedure TMt3dObsCollection.Loaded;
begin
  CalculateMaxValues;
end;

function TMt3dObsCollection.ReadFromFile(const AFileName: string; ScreenObjectNameDictionary: TObsNameDictionary): boolean;
const
  NameLength = 12;
  HeaderLength = 15;
var
  FileReader: TFileStream;
  Header: array[0..HeaderLength-1] of AnsiChar;
  ANameArray: array[0..NameLength-1] of AnsiChar;
  AName: string;
  Time: single;
  Value: Single;
  AnItem: TMt3dObsResult;
  ScreenObjectName: string;
  ScreenObject: TScreenObject;
  TransObservations: TMt3dmsTransObservations;
  ObservationName: string;
  MtObs: TMt3dmsTobItem;
  ItemIndex: Integer;
  ItemString: string;
  ItemValuesAssigned: Boolean;
begin
  result := False;
  if not TFile.Exists(AFileName) then
  begin
    Beep;
    MessageDlg(Format(StrTheFileFromWhich, [AFileName]), mtError, [mbOK], 0);
    Exit;
  end;

  Clear;
  Assert(ScreenObjectNameDictionary <> nil);
  FileReader := TFile.OpenRead(AFileName);
  try
    FileReader.Read(Header[0], SizeOf(AnsiChar)*HeaderLength);
    if FileReader.Position = FileReader.Size then
    begin
      Beep;
      MessageDlg(Format(StrSIsEmptyExceptF, [AFileName]), mtError, [mbOK], 0);
      Exit;
    end;
    repeat
      FileReader.Read(ANameArray[0], SizeOf(AnsiChar)*NameLength);
      AName := Trim(String(ANameArray));
      if AName = '' then
      begin
        break;
      end;
      FileReader.Read(Time, SizeOf(Time));
      FileReader.Read(Value, SizeOf(Value));
      AnItem := Add;
      AnItem.Name := AName;
      AnItem.Time := Time;
      AnItem.SimulatedValue := Value;

      if not ScreenObjectNameDictionary.TryGetValue(AName, ScreenObjectName) then
      begin
        ScreenObjectName := ''
      end;

      AnItem.ScreenObjectName := ScreenObjectName;
      ScreenObject := nil;
      if ScreenObjectName <> '' then
      begin
        ScreenObject := (FModel as TCustomModel).GetScreenObjectByName(ScreenObjectName);
        AnItem.FScreenObject := ScreenObject;
      end;

      ItemValuesAssigned := False;
      if ScreenObject <> nil then
      begin
        TransObservations := ScreenObject.Mt3dmsTransObservations;
        if TransObservations <> nil then
        begin
          ObservationName := TransObservations.ObservationName;
          if AnsiSameText(AnItem.Name, ObservationName) then
          begin
            ItemIndex := 0;
          end
          else
          begin
            if Pos(ObservationName + '_', AnItem.Name) = 1 then
            begin
              ItemString := Copy(AnItem.Name, Length(ObservationName)+2, MaxInt);
              if TryStrToInt(ItemString, ItemIndex) then
              begin
                Dec(ItemIndex);
              end
              else
              begin
                ItemIndex := -1;
              end;
            end;
          end;

          if (ItemIndex >= 0) and (ItemIndex < TransObservations.Values.Count) then
          begin
            MtObs := TransObservations.Values.TobItems[ItemIndex];
            AnItem.Measured := MtObs.Concentration;
            AnItem.Weight := MtObs.Weight;
            AnItem.Residual := AnItem.Measured - AnItem.SimulatedValue;
            AnItem.WeightedMeasured := AnItem.Measured * AnItem.Weight;
            AnItem.WeightedSimulated := AnItem.SimulatedValue * AnItem.Weight;
            AnItem.WeightedResidual := AnItem.Residual * AnItem.Weight;
            ItemValuesAssigned := True;
          end;
        end;
      end;

      if not ItemValuesAssigned then
      begin
        AnItem.ResidualText := StrNotRecorded;
        AnItem.WeightText := StrNotRecorded;
        AnItem.WeightedMeasuredText := StrNotRecorded;
        AnItem.WeightedSimulatedText := StrNotRecorded;
        AnItem.WeightedResidualText := StrNotRecorded;
      end;

    until FileReader.Position = FileReader.Size;
  finally
    FileReader.Free;
  end;

  FileName := AFileName;
  FileDate := TFile.GetLastWriteTime(FileName);


end;

function TMt3dObsCollection.RootMeanSquareResidual: double;
var
  ItemIndex: Integer;
  Item: TMt3dObsResult;
begin
  result := 0;
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex];
    result := result + Sqr(Item.Residual);
  end;
  Result := Sqrt(result/Count);
end;

function TMt3dObsCollection.RootMeanSquareWeightedResidual: double;
var
  ItemIndex: Integer;
  Item: TMt3dObsResult;
begin
  result := 0;
  for ItemIndex := 0 to Count - 1 do
  begin
    Item := Items[ItemIndex];
    result := result + Sqr(Item.WeightedResidual);
  end;
  Result := Sqrt(result/Count);
end;

procedure TMt3dObsCollection.SetDrawChoice(const Value: TMt3dDrawChoice);
begin
  if FDrawChoice <> Value then
  begin
    FDrawChoice := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dObsCollection.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TMt3dObsCollection.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TMt3dObsCollection.SetItem(Index: Integer;
  const Value: TMt3dObsResult);
begin
  inherited Items[Index] := Value;
end;

procedure TMt3dObsCollection.SetMaxResidualLimit(const Value: TColoringLimit);
begin
  FMaxResidualLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetMaxSymbolSize(const Value: integer);
begin
  if FMaxSymbolSize <> Value then
  begin
    FMaxSymbolSize := Value;
    InvalidateModel;
  end;
end;

procedure TMt3dObsCollection.SetMaxTimeLimit(const Value: TColoringLimit);
begin
  FMaxTimeLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetMaxWeightedResidualLimit(
  const Value: TColoringLimit);
begin
  FMaxWeightedResidualLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetMinResidualLimit(const Value: TColoringLimit);
begin
  FMinResidualLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetMinTimeLimit(const Value: TColoringLimit);
begin
  FMinTimeLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetMinWeightedResidualLimit(
  const Value: TColoringLimit);
begin
  FMinWeightedResidualLimit.Assign(Value);
end;

procedure TMt3dObsCollection.SetNegativeColor(const Value: TColor);
begin
  if FNegativeColor <> Value then
  begin
    FNegativeColor := Value;
    FNegativeColor32 := Color32(FNegativeColor);
    InvalidateModel;
  end;
end;

procedure TMt3dObsCollection.SetPositiveColor(const Value: TColor);
begin
  if FPositiveColor <> Value then
  begin
    FPositiveColor := Value;
    FPositiveColor32 := Color32(FPositiveColor);
    InvalidateModel;
  end;
end;

procedure TMt3dObsCollection.SetVisible(const Value: boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    InvalidateModel;
  end;
end;

end.
