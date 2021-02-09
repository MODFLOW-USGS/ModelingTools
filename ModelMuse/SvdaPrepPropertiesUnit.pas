unit SvdaPrepPropertiesUnit;

interface

uses
  System.Classes, GoPhastTypes;

type
  TSvdaMethod = (smSvdQSqrRt, smXtQX, smLsqrNoOrtho, smLsqrOrtho);

  TSvdaPrepProperties = class(TGoPhastPersistent)
  private
    FMethod: TSvdaMethod;
    FNumberOfSuperParameters: Integer;
    FRunPest: Boolean;
    FRunSvdaPrep: Boolean;
    FFileName: string;
    procedure SetMethod(const Value: TSvdaMethod);
    procedure SetNumberOfSuperParameters(const Value: Integer);
    procedure SetRunPest(const Value: Boolean);
    procedure SetRunSvdaPrep(const Value: Boolean);
    procedure SetFileName(const Value: string);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;
    Procedure InitializeVariables;
  published
    // PEST control file name
    property FileName: string read FFileName write SetFileName;
    property Method: TSvdaMethod read FMethod write SetMethod;
    Property NumberOfSuperParameters: Integer read FNumberOfSuperParameters
      write SetNumberOfSuperParameters;
    property RunSvdaPrep: Boolean read FRunSvdaPrep write SetRunSvdaPrep;
    property RunPest: Boolean read FRunPest write SetRunPest;
  end;

  TSupCalcMethod = (scSvdQSqrRt, scXtQX);

  TSupCalcProperties = class(TGoPhastPersistent)
  private
    FFileName: string;
    FMethod: TSupCalcMethod;
    FRunSupCalc: boolean;
    FStoredExpectedValue: TRealStorage;
    FRunPest: boolean;
    function GetExpectedValue: double;
    procedure SetExpectedValue(const Value: double);
    procedure SetFileName(const Value: string);
    procedure SetMethod(const Value: TSupCalcMethod);
    procedure SetRunPest(const Value: boolean);
    procedure SetRunSupCalc(const Value: boolean);
    procedure SetStoredExpectedValue(const Value: TRealStorage);
  public
    Constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Procedure InitializeVariables;
    property ExpectedValue: double read GetExpectedValue write SetExpectedValue;
  published
    // PEST control file name
    property FileName: string read FFileName write SetFileName;
    property Method: TSupCalcMethod read FMethod write SetMethod;
    property StoredExpectedValue: TRealStorage read FStoredExpectedValue write SetStoredExpectedValue;
    property RunPest: boolean read FRunPest write SetRunPest;
    property RunSupCalc: boolean read FRunSupCalc write SetRunSupCalc;
  end;

implementation

{ TSvdaPrepProperties }

procedure TSvdaPrepProperties.Assign(Source: TPersistent);
var
  SvdaPrepSource: TSvdaPrepProperties;
begin
  if Source is TSvdaPrepProperties then
  begin
    SvdaPrepSource := TSvdaPrepProperties(Source);
    FileName := SvdaPrepSource.FileName;
    Method := SvdaPrepSource.Method;
    NumberOfSuperParameters := SvdaPrepSource.NumberOfSuperParameters;
    RunSvdaPrep := SvdaPrepSource.RunSvdaPrep;
    RunPest := SvdaPrepSource.RunPest;
  end
  else
  begin
    inherited;
  end;
end;

constructor TSvdaPrepProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  InitializeVariables;
end;

procedure TSvdaPrepProperties.InitializeVariables;
begin
  FFileName := '';
  FMethod := smSvdQSqrRt;
  FNumberOfSuperParameters := 1;
  FRunPest := True;
  FRunSvdaPrep := True;
end;

procedure TSvdaPrepProperties.SetFileName(const Value: string);
begin
  SetStringProperty(FFileName, Value);
end;

procedure TSvdaPrepProperties.SetMethod(const Value: TSvdaMethod);
begin
  if FMethod <> Value then
  begin
    FMethod := Value;
    InvalidateModel;
  end;
end;

procedure TSvdaPrepProperties.SetNumberOfSuperParameters(const Value: Integer);
begin
  SetIntegerProperty(FNumberOfSuperParameters, Value);
end;

procedure TSvdaPrepProperties.SetRunPest(const Value: Boolean);
begin
  SetBooleanProperty(FRunPest, Value);
end;

procedure TSvdaPrepProperties.SetRunSvdaPrep(const Value: Boolean);
begin
  SetBooleanProperty(FRunSvdaPrep, Value);
end;

{ TSupCalcProperties }

procedure TSupCalcProperties.Assign(Source: TPersistent);
var
  SupCalcSource: TSupCalcProperties;
begin
  if Source is TSupCalcProperties then
  begin
    SupCalcSource := TSupCalcProperties(Source);
    FileName := SupCalcSource.FileName;
    Method := SupCalcSource.Method;
    ExpectedValue := SupCalcSource.ExpectedValue;
    RunPest := SupCalcSource.RunPest;
    RunSupCalc := SupCalcSource.RunSupCalc;
  end
  else
  begin
    inherited
  end;
end;

constructor TSupCalcProperties.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited;
  FStoredExpectedValue := TRealStorage.Create;
  FStoredExpectedValue.OnChange := InvalidateModelEvent;
  InitializeVariables;
end;

destructor TSupCalcProperties.Destroy;
begin
  FStoredExpectedValue.Free;
  inherited;
end;

function TSupCalcProperties.GetExpectedValue: double;
begin
  result := StoredExpectedValue.Value;
end;

procedure TSupCalcProperties.InitializeVariables;
begin
  FFileName := '';
  FMethod := FMethod;
  FRunSupCalc := True;
  FRunPest := True;
  ExpectedValue := 0;
end;

procedure TSupCalcProperties.SetExpectedValue(const Value: double);
begin
  StoredExpectedValue.Value := Value;
end;

procedure TSupCalcProperties.SetFileName(const Value: string);
begin
  SetStringProperty(FFileName, Value);
end;

procedure TSupCalcProperties.SetMethod(const Value: TSupCalcMethod);
begin
  if FMethod <> Value then
  begin
    FMethod := Value;
    InvalidateModel;
  end;

end;

procedure TSupCalcProperties.SetRunPest(const Value: boolean);
begin
  SetBooleanProperty(FRunPest, Value);
end;

procedure TSupCalcProperties.SetRunSupCalc(const Value: boolean);
begin
  SetBooleanProperty(FRunSupCalc, Value);
end;

procedure TSupCalcProperties.SetStoredExpectedValue(const Value: TRealStorage);
begin
  FStoredExpectedValue.Assign(Value);
end;

end.
