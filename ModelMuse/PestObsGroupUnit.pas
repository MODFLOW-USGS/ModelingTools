unit PestObsGroupUnit;

interface

uses
  GoPhastTypes, System.Classes, System.SysUtils;

type
  TPestObservationGroup = class(TPhastCollectionItem)
  private
    FObsGroupName: string;
    FUseGroupTarget: Boolean;
    FAbsoluteCorrelationFileName: string;
    FStoredGroupTarget: TRealStorage;
    function GetRelativCorrelationFileName: string;
    procedure SetAbsoluteCorrelationFileName(const Value: string);
    procedure SetObsGroupName(const Value: string);
    procedure SetRelativCorrelationFileName(const Value: string);
    procedure SetStoredGroupTarget(const Value: TRealStorage);
    procedure SetUseGroupTarget(const Value: Boolean);
    function GetGroupTarget: Double;
    procedure SetGroupTarget(const Value: Double);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AbsoluteCorrelationFileName: string
      read FAbsoluteCorrelationFileName write SetAbsoluteCorrelationFileName;
    property GroupTarget: Double read GetGroupTarget write SetGroupTarget;
  published
    property ObsGroupName: string read FObsGroupName write SetObsGroupName;
    property UseGroupTarget: Boolean read FUseGroupTarget
      write SetUseGroupTarget;
    property StoredGroupTarget: TRealStorage read FStoredGroupTarget
      write SetStoredGroupTarget;
    property RelativCorrelationFileName: string
      read GetRelativCorrelationFileName write SetRelativCorrelationFileName;
  end;

  TPestObservationGroups = class(TPhastCollection)
  private
    function GetParamGroup(Index: Integer): TPestObservationGroup;
    procedure SetParamGroup(Index: Integer; const Value: TPestObservationGroup);
  public
    constructor Create(InvalidateModelEvent: TNotifyEvent);
    function Add: TPestObservationGroup;
    property Items[Index: Integer]: TPestObservationGroup read GetParamGroup
      write SetParamGroup; default;
  end;

implementation

uses
  frmGoPhastUnit;

{ TPestObservationGroup }

procedure TPestObservationGroup.Assign(Source: TPersistent);
var
  SourceGroup: TPestObservationGroup;
begin
  if Source is TPestObservationGroup then
  begin
    SourceGroup := TPestObservationGroup(Source);
    ObsGroupName := SourceGroup.ObsGroupName;
    UseGroupTarget := SourceGroup.UseGroupTarget;
    GroupTarget := SourceGroup.GroupTarget;
    AbsoluteCorrelationFileName := SourceGroup.AbsoluteCorrelationFileName;
  end
  else
  begin
    inherited;
  end;
end;

constructor TPestObservationGroup.Create(Collection: TCollection);
begin
  inherited;
  FStoredGroupTarget := TRealStorage.Create;
  FStoredGroupTarget.OnChange := OnInvalidateModel;
end;

destructor TPestObservationGroup.Destroy;
begin
  FStoredGroupTarget.Free;
  inherited;
end;

function TPestObservationGroup.GetGroupTarget: Double;
begin
  result := FStoredGroupTarget.Value;
end;

function TPestObservationGroup.GetRelativCorrelationFileName: string;
begin
  result := ExtractRelativePath(frmGoPhast.PhastModel.ModelFileName,
    AbsoluteCorrelationFileName);
end;

procedure TPestObservationGroup.SetAbsoluteCorrelationFileName(
  const Value: string);
begin
  SetStringProperty(FAbsoluteCorrelationFileName, Value);
end;

procedure TPestObservationGroup.SetGroupTarget(const Value: Double);
begin
  FStoredGroupTarget.Value := Value;
end;

procedure TPestObservationGroup.SetObsGroupName(const Value: string);
begin
  SetStringProperty(FObsGroupName, Value);
end;

procedure TPestObservationGroup.SetRelativCorrelationFileName(
  const Value: string);
begin
  AbsoluteCorrelationFileName := ExpandFileName(Value)
end;

procedure TPestObservationGroup.SetStoredGroupTarget(const Value: TRealStorage);
begin
  FStoredGroupTarget.Assign(Value);
end;

procedure TPestObservationGroup.SetUseGroupTarget(const Value: Boolean);
begin
  SetBooleanProperty(FUseGroupTarget, Value);
end;

{ TPestObservationGroups }

function TPestObservationGroups.Add: TPestObservationGroup;
begin
  result := inherited Add as TPestObservationGroup
end;

constructor TPestObservationGroups.Create(InvalidateModelEvent: TNotifyEvent);
begin
  inherited Create(TPestObservationGroup, InvalidateModelEvent);
end;

function TPestObservationGroups.GetParamGroup(
  Index: Integer): TPestObservationGroup;
begin
  result := inherited Items[Index] as TPestObservationGroup;
end;

procedure TPestObservationGroups.SetParamGroup(Index: Integer;
  const Value: TPestObservationGroup);
begin
  inherited Items[Index] := Value;
end;

end.
