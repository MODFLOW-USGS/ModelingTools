unit frameSubPestObsUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePestObsUnit, frameGridUnit,
  Vcl.StdCtrls, Vcl.ExtCtrls, PestObsUnit;

type
  TSubObsColumns = (socName, socType, socTime, socValue, socWeight,
    socInterbedSystem, socComment);

  TframeSubPestObs = class(TframePestObs)
    pnlCaption: TPanel;
    procedure frameObservationsGridSelectCell(Sender: TObject; ACol,
      ARow: Integer; var CanSelect: Boolean);
  private
    FNoDelayInterbeds: TStringList;
    FDelayInterbeds: TStringList;
    FWaterTableInterbeds: TStringList;
    procedure GetInterbeds;
    { Private declarations }
  protected
    procedure SetObsColumnCaptions; override;
    procedure GetDirectObs(Observations: TCustomComparisonCollection); override;
    procedure SetDirectObs(Observations: TCustomComparisonCollection); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Public declarations }
  end;

//var
//  frameSubPestObs: TframeSubPestObs;

implementation

uses
  LayerStructureUnit, frmGoPhastUnit, ModflowSubsidenceDefUnit;

resourcestring
  StrInterbedSystem = 'Interbed System';

{$R *.dfm}

{ TframeSubPestObs }

constructor TframeSubPestObs.Create(AOwner: TComponent);
begin
  inherited;
  FNoDelayInterbeds := TStringList.Create;
  FDelayInterbeds := TStringList.Create;
  FWaterTableInterbeds := TStringList.Create;
end;

destructor TframeSubPestObs.Destroy;
begin
  FNoDelayInterbeds.Free;
  FDelayInterbeds.Free;
  FWaterTableInterbeds.Free;
  inherited;
end;

procedure TframeSubPestObs.frameObservationsGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if ACol = Ord(socInterbedSystem) then
  begin
    if (frameObservations.Grid.Cells[Ord(socType),ARow] = rsNDSYSCOMPACT) then
    begin
      CanSelect := True;
      if not frameObservations.Grid.Drawing then
      begin
        frameObservations.Grid.Columns[Ord(socInterbedSystem)].Picklist := FNoDelayInterbeds;
      end;
    end
    else if (frameObservations.Grid.Cells[Ord(socType),ARow] = rsDSYSCOMPACTI) then
    begin
      CanSelect := True;
      if not frameObservations.Grid.Drawing then
      begin
        frameObservations.Grid.Columns[Ord(socInterbedSystem)].Picklist := FDelayInterbeds;
      end;
    end
    else if (frameObservations.Grid.Cells[Ord(socType),ARow] = rsSYSTMCOMPACT)
      or (frameObservations.Grid.Cells[Ord(socType),ARow] = rsVOIDRATIO)
      or (frameObservations.Grid.Cells[Ord(socType),ARow] = rsTHICKNESS)
      then
    begin
      CanSelect := True;
      if not frameObservations.Grid.Drawing then
      begin
        frameObservations.Grid.Columns[Ord(socInterbedSystem)].Picklist := FWaterTableInterbeds;
      end;
    end
    else
    begin
      CanSelect := False;
    end;
  end;
end;

procedure TframeSubPestObs.GetDirectObs(
  Observations: TCustomComparisonCollection);
var
  ItemIndex: Integer;
  Obs: TCustomSubObsItem;
begin
  for ItemIndex := 0 to Observations.Count - 1 do
  begin
    Obs := Observations[ItemIndex] as TCustomSubObsItem;
    frameObservations.Grid.Cells[Ord(socName), ItemIndex + 1] := Obs.Name;
    frameObservations.Grid.Objects[Ord(socName), ItemIndex + 1] := Obs;
    frameObservations.Grid.Cells[Ord(socType), ItemIndex + 1] := Obs.ObsTypeString;
    frameObservations.Grid.RealValue[Ord(socTime), ItemIndex + 1] := Obs.Time;
    frameObservations.Grid.RealValue[Ord(socValue), ItemIndex + 1] := Obs.ObservedValue;
    frameObservations.Grid.RealValue[Ord(socWeight), ItemIndex + 1] := Obs.Weight;
    frameObservations.Grid.Cells[Ord(socInterbedSystem), ItemIndex + 1] := Obs.InterbedSystem;
    frameObservations.Grid.Objects[Ord(socInterbedSystem), ItemIndex + 1] := Obs.Interbed;
    frameObservations.Grid.Cells[Ord(socComment), ItemIndex + 1] := Obs.Comment;
  end;
end;

procedure TframeSubPestObs.GetInterbeds;
var
  LayerGroups: TLayerStructure;
  LayerGroupIndex: Integer;
  LayerGroup: TLayerGroup;
  InterbedIndex: Integer;
  Interbed: TSubNoDelayBedLayerItem;
  DelayInterbed: TSubDelayBedLayerItem;
  WtInterbed: TSwtWaterTableItem;
begin
  FNoDelayInterbeds.Clear;
  FDelayInterbeds.Clear;
  FWaterTableInterbeds.Clear;
  LayerGroups := frmGoPhast.PhastModel.LayerStructure;
  for LayerGroupIndex := 0 to LayerGroups.Count - 1 do
  begin
    LayerGroup := LayerGroups[LayerGroupIndex];
    for InterbedIndex := 0 to LayerGroup.SubNoDelayBedLayers.Count - 1 do
    begin
      Interbed := LayerGroup.SubNoDelayBedLayers[InterbedIndex];
      FNoDelayInterbeds.AddObject(Interbed.Name, Interbed);
    end;
    for InterbedIndex := 0 to LayerGroup.SubDelayBedLayers.Count - 1 do
    begin
      DelayInterbed := LayerGroup.SubDelayBedLayers[InterbedIndex];
      FDelayInterbeds.AddObject(DelayInterbed.Name, DelayInterbed);
    end;
    for InterbedIndex := 0 to LayerGroup.WaterTableLayers.Count - 1 do
    begin
      WtInterbed := LayerGroup.WaterTableLayers[InterbedIndex];
      FWaterTableInterbeds.AddObject(WtInterbed.Name, WtInterbed);
    end;
  end;
end;

procedure TframeSubPestObs.SetDirectObs(
  Observations: TCustomComparisonCollection);
var
  ObsCount: Integer;
  RowIndex: Integer;
  RowOK: Boolean;
  ColIndex: Integer;
  Obs: TCustomSubObsItem;
  OtherObs: TCustomSubObsItem;
  MyGuid: TGUID;
begin
  ObsCount := 0;
  for RowIndex := 1 to frameObservations.seNumber.AsInteger do
  begin
    RowOK := True;
    for ColIndex := 0 to Ord(socWeight) do
    begin
      if frameObservations.Grid.Cells[ColIndex,RowIndex] = '' then
      begin
        RowOK := False;
        Break;
      end;
    end;
    if RowOK then
    begin
      if (frameObservations.Grid.Cells[Ord(socType),RowIndex] = rsNDSYSCOMPACT)
        or (frameObservations.Grid.Cells[Ord(socType),RowIndex] = rsDSYSCOMPACTI) then
      begin
        if frameObservations.Grid.Cells[Ord(socInterbedSystem),RowIndex] = '' then
        begin
          RowOK := False;
        end;
      end;
    end;
    if RowOK then
    begin
      if ObsCount < Observations.Count then
      begin
        Obs := Observations[ObsCount] as TCustomSubObsItem;
      end
      else
      begin
        Obs := Observations.Add as TCustomSubObsItem;
      end;
      Inc(ObsCount);
      Obs.Name := frameObservations.Grid.Cells[Ord(pocName), RowIndex];
      if frameObservations.Grid.Objects[Ord(pocName), RowIndex] <> nil then
      begin
        OtherObs := frameObservations.Grid.Objects[Ord(pocName), RowIndex] as TCustomSubObsItem;
        Obs.GUID  := OtherObs.GUID;
      end
      else
      begin
        if CreateGUID(MyGuid) = 0 then
        begin
          Obs.GUID := GUIDToString(MyGuid);
        end;
      end;
      Obs.ObsTypeString := frameObservations.Grid.Cells[Ord(socType), RowIndex];
      Obs.Time := frameObservations.Grid.RealValue[Ord(socTime), RowIndex];
      Obs.ObservedValue := frameObservations.Grid.RealValue[Ord(socValue), RowIndex];
      Obs.Weight := frameObservations.Grid.RealValue[Ord(socWeight), RowIndex];
      Obs.InterbedSystem := frameObservations.Grid.Cells[Ord(socInterbedSystem), RowIndex];
      if frameObservations.Grid.Objects[Ord(socInterbedSystem), RowIndex] <> nil then
      begin
        Obs.Interbed := frameObservations.Grid.Objects
          [Ord(socInterbedSystem), RowIndex] as TCustomSubLayerItem
      end;
      Obs.Comment := frameObservations.Grid.Cells[Ord(socComment), RowIndex];
    end;
  end;
  Observations.Count := ObsCount;
end;

procedure TframeSubPestObs.SetObsColumnCaptions;
begin
  GetInterbeds;
  frameObservations.Grid.BeginUpdate;
  try
    frameObservations.Grid.Cells[Ord(socName), 0] := StrObservationName;
    frameObservations.Grid.Cells[Ord(socType), 0] := StrObservationType;
    frameObservations.Grid.Cells[Ord(socTime), 0] := StrObservationTime;
    frameObservations.Grid.Cells[Ord(socValue), 0] := StrObservationValue;
    frameObservations.Grid.Cells[Ord(socWeight), 0] := StrObservationWeight;
    frameObservations.Grid.Cells[Ord(socInterbedSystem), 0] := StrInterbedSystem;
    frameObservations.Grid.Cells[Ord(socComment), 0] := StrComment;
  finally
    frameObservations.Grid.EndUpdate;
  end;
end;

end.
