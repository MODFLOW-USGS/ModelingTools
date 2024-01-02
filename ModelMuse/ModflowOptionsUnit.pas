unit ModflowOptionsUnit;

interface

uses SysUtils, Classes, GoPhastTypes;

Type
  TModflowOptions = class(TPersistent)
  private
    FOnInvalidateModel: TNotifyEvent;
    FLengthUnit: integer;
    FPrintTime: boolean;
    FProjectName: string;
    FProjectDate: string;
    FModeler: string;
    FComputeFluxesBetweenConstantHeadCells: boolean;
    FTimeUnit: integer;
    FDescription: TStrings;
    FOpenInTextEditor: boolean;
    FInitialHeadFileName: string;
    FStoredStopErrorCriterion: TRealStorage;
    FStopError: Boolean;
    FUnderRelaxationMF6: Boolean;
    FNewtonMF6: Boolean;
    FWriteBinaryGridFile: Boolean;
    FStoredHDry: TRealStorage;
    FStoredHNoFlow: TRealStorage;
    procedure InvalidateModel;
    procedure SetComputeFluxesBetweenConstantHeadCells(const Value: boolean);
    procedure SetDescription(const Value: TStrings);
    procedure SetHNoFlow(const Value: real);
    procedure SetLengthUnit(const Value: integer);
    procedure SetModeler(const Value: string);
    procedure SetPrintTime(const Value: boolean);
    procedure SetProjectDate(const Value: string);
    procedure SetProjectName(const Value: string);
    procedure SetTimeUnit(const Value: integer);
    procedure SetHDry(const Value: real);
    procedure SetOpenInTextEditor(const Value: boolean);
    procedure SetInitialHeadFileName(const Value: string);
    procedure SetStopError(const Value: Boolean);
    procedure SetStopErrorCriterion(const Value: double);
    procedure SetStoredStopErrorCriterion(const Value: TRealStorage);
    function GetStopErrorCriterion: double;
    procedure SetNewtonMF6(const Value: Boolean);
    procedure SetUnderRelaxationMF6(const Value: Boolean);
    function StoreMf6Properties: Boolean;
    procedure SetWriteBinaryGridFile(const Value: Boolean);
    procedure SetStoredHDry(const Value: TRealStorage);
    procedure SetStoredHNoFlow(const Value: TRealStorage);
    function GetHDry: real;
    function GetHNoFlow: real;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    destructor Destroy; override;
    procedure Clear;
    property StopErrorCriterion: double read GetStopErrorCriterion write SetStopErrorCriterion;
    Function StreamConstant(Model: TBaseModel): double;
  published
    property ComputeFluxesBetweenConstantHeadCells: boolean
      read FComputeFluxesBetweenConstantHeadCells
      write SetComputeFluxesBetweenConstantHeadCells default True;
    property Description: TStrings read FDescription write SetDescription;
    property HDry: real read GetHDry write SetHDry stored False;
    property HNoFlow: real read GetHNoFlow write SetHNoFlow stored False;
    property StoredHDry: TRealStorage read FStoredHDry write SetStoredHDry;
    property StoredHNoFlow: TRealStorage read FStoredHNoFlow write SetStoredHNoFlow;
    {
      0 - undefined
      1 - feet
      2 - meters
      3 - centimeters
    }
    property LengthUnit: integer read FLengthUnit write SetLengthUnit default 2;
    property Modeler: string read FModeler write SetModeler;
    property PrintTime: boolean read FPrintTime write SetPrintTime default True;
    property ProjectName: string read FProjectName write SetProjectName;
    property ProjectDate: string read FProjectDate write SetProjectDate;
    {
      0 - undefined
      1 - seconds
      2 - minutes
      3 - hours
      4 - days
      5 - years
    }
    property TimeUnit: integer read FTimeUnit write SetTimeUnit default 1;
    property OpenInTextEditor: boolean read FOpenInTextEditor
      write SetOpenInTextEditor default True;
    property InitialHeadFileName: string read FInitialHeadFileName
      write SetInitialHeadFileName;
    property StopError: Boolean read FStopError write SetStopError;
    property StoredStopErrorCriterion: TRealStorage
      read FStoredStopErrorCriterion write SetStoredStopErrorCriterion;
    // NEWTON option in MF6 name file
    property NewtonMF6: Boolean read FNewtonMF6 write SetNewtonMF6
      stored StoreMf6Properties;
    // UNDER_RELAXATION option in MF6 name file
    property UnderRelaxationMF6: Boolean read FUnderRelaxationMF6
      write SetUnderRelaxationMF6 stored StoreMf6Properties;
    // Inverse of NOGRB option in MF6 DIS file
    property WriteBinaryGridFile: Boolean read FWriteBinaryGridFile
      write SetWriteBinaryGridFile stored StoreMf6Properties;
  end;

  TWettingOptions = class(TPersistent)
  private
    FOnInvalidateModel: TNotifyEvent;
    FWettingFactor: real;
    FWettingEquation: integer;
    FWettingActive: boolean;
    FWettingIterations: integer;
    procedure SetWettingActive(const Value: boolean);
    procedure SetWettingEquation(const Value: integer);
    procedure SetWettingFactor(const Value: real);
    procedure InvalidateModel;
    procedure SetWettingIterations(Value: integer);
  published
    procedure Assign(Source: TPersistent); override;
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    // REWET
    property WettingActive: boolean read FWettingActive write SetWettingActive;
    // WETFCT
    property WettingFactor: real read FWettingFactor write SetWettingFactor;
    // IWETIT
    property WettingIterations: integer read FWettingIterations
      write SetWettingIterations default 1;
      // IHDWET
    property WettingEquation: integer read FWettingEquation write SetWettingEquation;
  end;

implementation

uses
  frmErrorsAndWarningsUnit, System.Math, frmGoPhastUnit;

const
  DefaultHNoFlow: real = -1e20;
  DefaultHDry: real = -2e20;

resourcestring
  SfrError = 'SFR Error';
  StrLengthUnitsForMod = 'Length units for model are undefined';
  StrTimeUnitsForModel = 'Time units for model are undefined';

{ TModflowOptions }

procedure TModflowOptions.Assign(Source: TPersistent);
var
  SourceModel: TModflowOptions;
begin
  if Source is TModflowOptions then
  begin
    SourceModel := TModflowOptions(Source);
    ComputeFluxesBetweenConstantHeadCells := SourceModel.ComputeFluxesBetweenConstantHeadCells;
    Description := SourceModel.Description;
    LengthUnit := SourceModel.LengthUnit;
    StoredHDry := SourceModel.StoredHDry;
    StoredHNoFlow := SourceModel.StoredHNoFlow;
    Modeler := SourceModel.Modeler;
    PrintTime := SourceModel.PrintTime;
    ProjectName := SourceModel.ProjectName;
    ProjectDate := SourceModel.ProjectDate;
//    ShowProgress := SourceModel.ShowProgress;
    TimeUnit := SourceModel.TimeUnit;
    OpenInTextEditor := SourceModel.OpenInTextEditor;
    InitialHeadFileName := SourceModel.InitialHeadFileName;
    StopError := SourceModel.StopError;
    StopErrorCriterion := SourceModel.StopErrorCriterion;
  end
  else
  begin
    inherited;
  end;
end;

constructor TModflowOptions.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create;
  FDescription := TStringList.Create;
  FStoredStopErrorCriterion := TRealStorage.Create;
  FStoredHDry := TRealStorage.Create;
  FStoredHNoFlow := TRealStorage.Create;
  Clear;
  FOnInvalidateModel := InvalidateModelEvent;
  FStoredStopErrorCriterion.OnChange := FOnInvalidateModel;
  FStoredHDry.OnChange := FOnInvalidateModel;
  FStoredHNoFlow.OnChange := FOnInvalidateModel;
  FProjectDate := DateTimeToStr(Trunc(Now));
end;

destructor TModflowOptions.Destroy;
begin
  FStoredHDry.Free;
  FStoredHNoFlow.Free;
  FStoredStopErrorCriterion.Free;
  FDescription.Free;
  inherited;
end;

function TModflowOptions.GetHDry: real;
const
  MF6HDry = -1E30;
begin
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    result := MF6HDry;
  end
  else
  begin
    result := StoredHDry.Value;
  end;
end;

function TModflowOptions.GetHNoFlow: real;
const
  MF6NoFlow = 1E30;
begin
  if frmGoPhast.ModelSelection = msModflow2015 then
  begin
    result := MF6NoFlow;
  end
  else
  begin
    result := StoredHNoFlow.Value;
  end;
end;

function TModflowOptions.GetStopErrorCriterion: double;
begin
  result := FStoredStopErrorCriterion.Value;
end;

procedure TModflowOptions.Clear;
begin
  FDescription.Clear;
  StoredHDry.Value := DefaultHDry;
  StoredHNoFlow.Value := DefaultHNoFlow;
  FTimeUnit := 1;
  FLengthUnit := 2;
  FPrintTime := True;
  FProjectName := '';
  FProjectDate := '';
  FModeler := '';
  FComputeFluxesBetweenConstantHeadCells := True;
  FOpenInTextEditor := True;
  FInitialHeadFileName := '';
  StopError := False;
  StopErrorCriterion := 1;
  FNewtonMF6 := False;
  FUnderRelaxationMF6 := False;
  FWriteBinaryGridFile := True;
end;

procedure TModflowOptions.InvalidateModel;
begin
  if Assigned(FOnInvalidateModel) then
  begin
    FOnInvalidateModel(self);
  end;
end;

procedure TModflowOptions.SetComputeFluxesBetweenConstantHeadCells(
  const Value: boolean);
begin
  if FComputeFluxesBetweenConstantHeadCells <> Value then
  begin
    FComputeFluxesBetweenConstantHeadCells := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetDescription(const Value: TStrings);
begin
  Assert(Value <> nil);
  if not FDescription.Equals(Value) then
  begin
    FDescription.Assign(Value);
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetHDry(const Value: real);
begin
  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    StoredHDry.Value := Value;
  end;
end;

procedure TModflowOptions.SetHNoFlow(const Value: real);
begin
  if frmGoPhast.ModelSelection <> msModflow2015 then
  begin
    StoredHNoFlow.Value := Value;
  end;
end;

procedure TModflowOptions.SetInitialHeadFileName(const Value: string);
begin
  if FInitialHeadFileName <> Value then
  begin
    FInitialHeadFileName := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetLengthUnit(const Value: integer);
begin
  if FLengthUnit <> Value then
  begin
    FLengthUnit := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetModeler(const Value: string);
begin
  if FModeler <> Value then
  begin
    FModeler := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetNewtonMF6(const Value: Boolean);
begin
  if FNewtonMF6 <> Value then
  begin
    FNewtonMF6 := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetOpenInTextEditor(const Value: boolean);
begin
  if FOpenInTextEditor <> Value then
  begin
    FOpenInTextEditor := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetPrintTime(const Value: boolean);
begin
  if FPrintTime <> Value then
  begin
    FPrintTime := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetProjectDate(const Value: string);
begin
  if FProjectDate <> Value then
  begin
    FProjectDate := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetProjectName(const Value: string);
begin
  if FProjectName <> Value then
  begin
    FProjectName := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetStopError(const Value: Boolean);
begin
  if FStopError <> Value then
  begin
    FStopError := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetStopErrorCriterion(const Value: double);
begin
  FStoredStopErrorCriterion.Value := Value;
end;

procedure TModflowOptions.SetStoredHDry(const Value: TRealStorage);
begin
  FStoredHDry.Assign(Value);
end;

procedure TModflowOptions.SetStoredHNoFlow(const Value: TRealStorage);
begin
  FStoredHNoFlow.Assign(Value);
end;

procedure TModflowOptions.SetStoredStopErrorCriterion(
  const Value: TRealStorage);
begin
  FStoredStopErrorCriterion.Assign(Value);
end;

procedure TModflowOptions.SetTimeUnit(const Value: integer);
begin
  if FTimeUnit <> Value then
  begin
    FTimeUnit := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetUnderRelaxationMF6(const Value: Boolean);
begin
  if FUnderRelaxationMF6 <> Value then
  begin
    FUnderRelaxationMF6 := Value;
    InvalidateModel;
  end;
end;

procedure TModflowOptions.SetWriteBinaryGridFile(const Value: Boolean);
begin
  if FWriteBinaryGridFile <> Value then
  begin
    FWriteBinaryGridFile := Value;
    InvalidateModel;
  end;
end;

function TModflowOptions.StoreMf6Properties: Boolean;
begin
  Result := True;
end;

function TModflowOptions.StreamConstant(Model: TBaseModel): double;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(Model, SfrError);
  result := 1;
  case LengthUnit of
    0: // undefined
      begin
        frmErrorsAndWarnings.AddError(Model, SfrError, StrLengthUnitsForMod);
      end;
    1: // feet
      begin
        result := 1 / 0.3048;
      end;
    2: // m
      begin
      end;
    3: // cm
      begin
        result := 1 / 100;
      end;
  else
    Assert(False);
  end;
  if result <> 1 then
  begin
    result := Power(result, 1 / 3);
  end;
  case TimeUnit of
    0: // Undefined
      begin
//        ErrorMessage :=
//          StrTimeUnitsForModel;
        frmErrorsAndWarnings.AddError(Model, SfrError, StrTimeUnitsForModel);
      end;
    1: // Seconds
      begin
      end;
    2: // Minutes
      begin
        result := result * 60;
      end;
    3: // Hours
      begin
        result := result * 60 * 60;
      end;
    4: // Days
      begin
        result := result * 60 * 60 * 24;
      end;
    5: // Years
      begin
        result := result * 60 * 60 * 24 * 365.25;
      end;
  else
    Assert(False);
  end;
end;

{ TWettingOptions }

procedure TWettingOptions.Assign(Source: TPersistent);
var
  WettingOptions: TWettingOptions;
begin
  if Source is TWettingOptions then
  begin
    WettingOptions := TWettingOptions(Source);
    WettingActive := WettingOptions.WettingActive;
    WettingFactor := WettingOptions.WettingFactor;
    WettingIterations := WettingOptions.WettingIterations;
    WettingEquation := WettingOptions.WettingEquation;
  end
  else
  begin
    inherited;
  end;
end;

constructor TWettingOptions.Create(InvalidateModelEvent: TNotifyEvent);
begin
  FOnInvalidateModel := InvalidateModelEvent;
  FWettingFactor := 1;
  FWettingIterations := 1;
end;

procedure TWettingOptions.InvalidateModel;
begin
  if Assigned(FOnInvalidateModel) then
  begin
    FOnInvalidateModel(self);
  end;
end;

procedure TWettingOptions.SetWettingActive(const Value: boolean);
begin
  if FWettingActive <> Value then
  begin
    FWettingActive := Value;
    InvalidateModel;
  end;
end;

procedure TWettingOptions.SetWettingEquation(const Value: integer);
begin
  if FWettingEquation <> Value then
  begin
    FWettingEquation := Value;
    InvalidateModel;
  end;
end;

procedure TWettingOptions.SetWettingFactor(const Value: real);
begin
  if FWettingFactor <> Value then
  begin
    FWettingFactor := Value;
    InvalidateModel;
  end;
end;

procedure TWettingOptions.SetWettingIterations(Value: integer);
begin
  if Value <= 0 then
  begin
    Value := 1;
  end;
  if FWettingIterations <> Value then
  begin
    FWettingIterations := Value;
    InvalidateModel;
  end;
end;

end.
