unit PestObservationResults;

interface

uses GoPhastTypes, DataSetUnit, Graphics, GR32, Classes, System.SysUtils,
  Vcl.Dialogs;

type
  TPestObsResult = class(TPhastCollectionItem)
  private
    FName: string;
    FNaturalWeight: double;
    FMeasured: double;
    FOriginalOrder: Integer;
    FWeightedResidual: double;
    FModelled: double;
    FWeight: double;
    FMeasurementStdDeviation: double;
    FWeightedMeasured: double;
    FResidual: double;
    FWeightedModelled: double;
    FGroupName: string;
    procedure SetGroupName(const Value: string);
    procedure SetMeasured(const Value: double);
    procedure SetMeasurementStdDeviation(const Value: double);
    procedure SetModelled(const Value: double);
    procedure SetName(const Value: string);
    procedure SetNaturalWeight(const Value: double);
    procedure SetOriginalOrder(const Value: Integer);
    procedure SetResidual(const Value: double);
    procedure SetWeight(const Value: double);
    procedure SetWeightedMeasured(const Value: double);
    procedure SetWeightedModelled(const Value: double);
    procedure SetWeightedResidual(const Value: double);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
  published
    property Name: string read FName write SetName;
    property GroupName: string read FGroupName write SetGroupName;
    property Measured: double read FMeasured write SetMeasured;
    property Modelled: double read FModelled write SetModelled;
    property Residual: double read FResidual write SetResidual;
    property Weight: double read FWeight write SetWeight;
    property WeightedMeasured: double read FWeightedMeasured write SetWeightedMeasured;
    property WeightedModelled: double read FWeightedModelled write SetWeightedModelled;
    property WeightedResidual: double read FWeightedResidual write SetWeightedResidual;
    property MeasurementStdDeviation: double read FMeasurementStdDeviation write SetMeasurementStdDeviation;
    property NaturalWeight: double read FNaturalWeight write SetNaturalWeight;
    property OriginalOrder: Integer read FOriginalOrder write SetOriginalOrder;
  end;

  TPestObsCollection = class(TPhastCollection)
  strict private
    { TODO -cRefactor : Consider replacing FModel with a TNotifyEvent or interface. }
    //
    FModel: TBaseModel;
  private
    FMaxTimeLimit: TColoringLimit;
    FMinWeightedResidualLimit: TColoringLimit;
    FFileName: string;
    FFileDate: TDateTime;
    FMaxResidualLimit: TColoringLimit;
    FMinTimeLimit: TColoringLimit;
    FPositiveColor: TColor;
    FMinResidualLimit: TColoringLimit;
    FDisplayResiduals: boolean;
    FMaxLayerLimit: TColoringLimit;
    FMaxWeightedResidualLimit: TColoringLimit;
    FMaxSymbolSize: integer;
    FNegativeColor: TColor;
    FMinLayerLimit: TColoringLimit;
    FPositiveColor32: TColor32;
    FNegativeColor32: TColor32;
    procedure SetDisplayResiduals(const Value: boolean);
    procedure SetFileDate(const Value: TDateTime);
    procedure SetFileName(const Value: string);
    procedure SetMaxLayerLimit(const Value: TColoringLimit);
    procedure SetMaxResidualLimit(const Value: TColoringLimit);
    procedure SetMaxSymbolSize(const Value: integer);
    procedure SetMaxTimeLimit(const Value: TColoringLimit);
    procedure SetMaxWeightedResidualLimit(const Value: TColoringLimit);
    procedure SetMinLayerLimit(const Value: TColoringLimit);
    procedure SetMinResidualLimit(const Value: TColoringLimit);
    procedure SetMinTimeLimit(const Value: TColoringLimit);
    procedure SetMinWeightedResidualLimit(const Value: TColoringLimit);
    procedure SetNegativeColor(const Value: TColor);
    procedure SetPositiveColor(const Value: TColor);
  public
    constructor Create(Model: TBaseModel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function ReadFromFile(AModel: TBaseModel): boolean;
    function Add: TPestObsResult;
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
    property MaxLayerLimit: TColoringLimit read FMaxLayerLimit write SetMaxLayerLimit;
    property MinLayerLimit: TColoringLimit read FMinLayerLimit write SetMinLayerLimit;
    property NegativeColor: TColor read FNegativeColor write SetNegativeColor default clRed;
    property PositiveColor: TColor read FPositiveColor write SetPositiveColor default clBlue;
    property MaxSymbolSize: integer read FMaxSymbolSize write SetMaxSymbolSize default 20;
    property Visible: boolean read FDisplayResiduals write SetDisplayResiduals default True;
  end;

implementation

uses RbwParser, System.IOUtils, frmErrorsAndWarningsUnit, ModelMuseUtilities;

resourcestring
  StrTheFileFromWhich = 'The file from which you are attempting to read ' +
  'residuals, %s, does not exist.';

{ TPestObsResult }

procedure TPestObsResult.Assign(Source: TPersistent);
var
  ObsSource: TPestObsResult;
begin
  if Source is TPestObsResult then
  begin
    ObsSource := TPestObsResult(Source);
    Name := ObsSource.Name;
    GroupName := ObsSource.GroupName;
    Measured := ObsSource.Measured;
    Modelled := ObsSource.Modelled;
    Residual := ObsSource.Residual;
    Weight := ObsSource.Weight;
    WeightedMeasured := ObsSource.WeightedMeasured;
    WeightedResidual := ObsSource.WeightedResidual;
    MeasurementStdDeviation := ObsSource.MeasurementStdDeviation;
    NaturalWeight := ObsSource.NaturalWeight;
    OriginalOrder := ObsSource.OriginalOrder;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestObsResult.Create(Collection: TCollection);
begin
  inherited;

end;

procedure TPestObsResult.SetGroupName(const Value: string);
begin
  SetStringProperty(FGroupName, Value);
end;

procedure TPestObsResult.SetMeasured(const Value: double);
begin
  SetRealProperty(FMeasured, Value);
end;

procedure TPestObsResult.SetMeasurementStdDeviation(const Value: double);
begin
  SetRealProperty(FMeasurementStdDeviation, Value);
end;

procedure TPestObsResult.SetModelled(const Value: double);
begin
  SetRealProperty(FModelled, Value);
end;

procedure TPestObsResult.SetName(const Value: string);
begin
  SetStringProperty(FName, Value);
end;

procedure TPestObsResult.SetNaturalWeight(const Value: double);
begin
  SetRealProperty(FNaturalWeight, Value);
end;

procedure TPestObsResult.SetOriginalOrder(const Value: Integer);
begin
  SetIntegerProperty(FOriginalOrder, Value);
end;

procedure TPestObsResult.SetResidual(const Value: double);
begin
  SetRealProperty(FResidual, Value);
end;

procedure TPestObsResult.SetWeight(const Value: double);
begin
  SetRealProperty(FWeight, Value);
end;

procedure TPestObsResult.SetWeightedMeasured(const Value: double);
begin
  SetRealProperty(FWeightedMeasured, Value);
end;

procedure TPestObsResult.SetWeightedModelled(const Value: double);
begin
  SetRealProperty(FWeightedModelled, Value);
end;

procedure TPestObsResult.SetWeightedResidual(const Value: double);
begin
  SetRealProperty(FWeightedResidual, Value);
end;

{ TPestObsCollection }

function TPestObsCollection.Add: TPestObsResult;
begin
  result := inherited Add as TPestObsResult;
end;

procedure TPestObsCollection.Assign(Source: TPersistent);
begin
  inherited;

end;

constructor TPestObsCollection.Create(Model: TBaseModel);
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
    InvalidateModelEvent := Model.Invalidate;
  end;
  inherited Create(TPestObsResult, InvalidateModelEvent);
  FMaxTimeLimit := TColoringLimit.Create;
  FMaxResidualLimit := TColoringLimit.Create;
  FMinTimeLimit := TColoringLimit.Create;
  FMinResidualLimit := TColoringLimit.Create;
  FMaxLayerLimit := TColoringLimit.Create;
  FMinLayerLimit := TColoringLimit.Create;
  FMaxWeightedResidualLimit := TColoringLimit.Create;
  FMinWeightedResidualLimit := TColoringLimit.Create;

  FMaxLayerLimit.DataType := rdtInteger;
  FMinLayerLimit.DataType := rdtInteger;

  NegativeColor := clRed;
  PositiveColor := clBlue;

  FMaxSymbolSize := 20;
  FDisplayResiduals := True;
{
    FMaxTimeLimit: TColoringLimit;
    FMinWeightedResidualLimit: TColoringLimit;
    FFileName: string;
    FFileDate: TDateTime;
    FMaxResidualLimit: TColoringLimit;
    FMinTimeLimit: TColoringLimit;
    FPositiveColor: TColor;
    FMinResidualLimit: TColoringLimit;
    FDisplayResiduals: boolean;
    FMaxLayerLimit: TColoringLimit;
    FMaxWeightedResidualLimit: TColoringLimit;
    FMaxSymbolSize: integer;
    FNegativeColor: TColor;
    FMinLayerLimit: TColoringLimit;
    FPositiveColor32: TColor32;
    FNegativeColor32: TColor32;
}
end;

destructor TPestObsCollection.Destroy;
begin
  FMinWeightedResidualLimit.Free;
  FMaxWeightedResidualLimit.Free;

  FMinLayerLimit.Free;
  FMaxLayerLimit.Free;
  FMaxTimeLimit.Free;
  FMaxResidualLimit.Free;
  FMinTimeLimit.Free;
  FMinResidualLimit.Free;

  inherited;
end;

function TPestObsCollection.ReadFromFile(AModel: TBaseModel): boolean;
var
  ShowErrors: Boolean;
  ResidualsFile: TStringList;
  LineIndex: Integer;
  Splitter: TStringList;
  Item: TPestObsResult;
begin
  result := False;
  ShowErrors := False;
  try
    if not TFile.Exists(FileName) then
    begin
      Beep;
      MessageDlg(Format(StrTheFileFromWhich, [FileName]), mtError, [mbOK], 0);
      Exit;
    end;
    ResidualsFile := TStringList.Create;
    Splitter := TStringList.Create;
    try
      Clear;
      ResidualsFile.LoadFromFile(FileName);
      for LineIndex := 1 to ResidualsFile.Count - 1 do
      begin
        Splitter.DelimitedText := ResidualsFile[LineIndex];
        if Splitter.Count > 0 then
        begin
          Assert(Splitter.Count >= 11);
          Item := Add;
          Item.Name := Splitter[0];
          Item.GroupName := Splitter[1];
          Item.Measured := FortranStrToFloat(Splitter[2]);
          Item.Modelled := FortranStrToFloat(Splitter[3]);
          Item.Residual := FortranStrToFloat(Splitter[4]);
          Item.Weight := FortranStrToFloat(Splitter[5]);
          Item.WeightedMeasured := FortranStrToFloat(Splitter[6]);
          Item.WeightedModelled := FortranStrToFloat(Splitter[7]);
          Item.WeightedResidual := FortranStrToFloat(Splitter[8]);
          Item.MeasurementStdDeviation := FortranStrToFloat(Splitter[9]);
          Item.NaturalWeight := FortranStrToFloat(Splitter[10]);
          Item.OriginalOrder := LineIndex-1;
        end;
      end;
    finally
      Splitter.Free;
      ResidualsFile.Free;
    end;
  finally
    if ShowErrors then
    begin
      frmErrorsAndWarnings.ShowAfterDelay;
    end;
  end;
  result := True;
end;

procedure TPestObsCollection.SetDisplayResiduals(const Value: boolean);
begin
  if FDisplayResiduals <> Value then
  begin
    FDisplayResiduals := Value;
    InvalidateModel;
  end;
end;

procedure TPestObsCollection.SetFileDate(const Value: TDateTime);
begin
  FFileDate := Value;
end;

procedure TPestObsCollection.SetFileName(const Value: string);
begin
  if FFileName <> Value then
  begin
    FFileName := Value;
    InvalidateModel;
  end;
end;

procedure TPestObsCollection.SetMaxLayerLimit(const Value: TColoringLimit);
begin
  FMaxLayerLimit.Assign(Value);
end;

procedure TPestObsCollection.SetMaxResidualLimit(const Value: TColoringLimit);
begin
  FMaxResidualLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMaxSymbolSize(const Value: integer);
begin
  if FMaxSymbolSize <> Value then
  begin
    FMaxSymbolSize := Value;
    InvalidateModel;
  end;
end;

procedure TPestObsCollection.SetMaxTimeLimit(const Value: TColoringLimit);
begin
  FMaxTimeLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMaxWeightedResidualLimit(
  const Value: TColoringLimit);
begin
  FMaxWeightedResidualLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMinLayerLimit(const Value: TColoringLimit);
begin
  FMinLayerLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMinResidualLimit(const Value: TColoringLimit);
begin
  FMinResidualLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMinTimeLimit(const Value: TColoringLimit);
begin
  FMinTimeLimit.Assign(Value)
end;

procedure TPestObsCollection.SetMinWeightedResidualLimit(
  const Value: TColoringLimit);
begin
  FMinWeightedResidualLimit.Assign(Value)
end;

procedure TPestObsCollection.SetNegativeColor(const Value: TColor);
begin
  if FNegativeColor <> Value then
  begin
    FNegativeColor := Value;
    FNegativeColor32 := Color32(FNegativeColor);
    InvalidateModel;
  end;
end;

procedure TPestObsCollection.SetPositiveColor(const Value: TColor);
begin
  if FPositiveColor <> Value then
  begin
    FPositiveColor := Value;
    FPositiveColor32 := Color32(FPositiveColor);
    InvalidateModel;
  end;
end;

end.
