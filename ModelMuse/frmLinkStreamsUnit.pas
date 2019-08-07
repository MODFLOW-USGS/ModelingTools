unit frmLinkStreamsUnit;

interface

uses System.UITypes,
  Windows, Messages, Types, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, frmCustomGoPhastUnit, StdCtrls, Buttons, ArgusDataEntry, ExtCtrls,
  UndoItems, ScreenObjectUnit, OrderedCollectionUnit,
  Generics.Collections, ModflowSwrReachUnit, QuadTreeClass, IntListUnit;

type
  TLinkType = (ltSFR, ltSTR, ltSWR, ltSFR6);

  TStreamLinkageChangeItem = class(TCollectionItem)
  private
    FNewOutFlowSegment: integer;
    FOldOutFlowSegments: TIntegerDynArray;
    FScreenObject: TScreenObject;
    FNewSwrReachConnections: TSwrConnections;
    FOldSwrReachConnections: TSwrConnections;
    procedure SetNewOutFlowSegment(const Value: integer);
    procedure SetScreenObject(const Value: TScreenObject);
    function GetOutFlowSegment(Index: integer): integer;
    procedure SetNewSwrReachConnections(const Value: TSwrConnections);
    procedure SetOldSwrReachConnections(const Value: TSwrConnections);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property ScreenObject: TScreenObject read FScreenObject
      write SetScreenObject;
    property NewOutFlowSegment: integer read FNewOutFlowSegment
      write SetNewOutFlowSegment;
    property OldOutFlowSegments[Index: integer]: integer read GetOutFlowSegment;
    property NewSwrReachConnections: TSwrConnections
      read FNewSwrReachConnections write SetNewSwrReachConnections;
    property OldSwrReachConnections: TSwrConnections
      read FOldSwrReachConnections write SetOldSwrReachConnections;
  end;

  TStreamLinkageChangeCollection = class(TCollection)
  private
    FLinkType: TLinkType;
    function GetItems(Index: integer): TStreamLinkageChangeItem;
    procedure SetItems(Index: integer; const Value: TStreamLinkageChangeItem);
  public
    Constructor Create(LinkType: TLinkType);
    property Items[Index: integer]: TStreamLinkageChangeItem read GetItems
      write SetItems;
    function Add: TStreamLinkageChangeItem;
  end;

  TUndoChangeStreamLinkages = class(TCustomUndo)
  private
    FLinkages: TStreamLinkageChangeCollection;
    procedure UpdateStreamLinkDisplay;
  public
    function Description: string; override;
    Constructor Create(Linkages: TStreamLinkageChangeCollection);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmLinkStreams = class(TfrmCustomGoPhast)
    rgWhatToLink: TRadioGroup;
    lblTolerance: TLabel;
    rdeTolerance: TRbwDataEntry;
    cbKeepExistingLinkages: TCheckBox;
    btnApply: TBitBtn;
    btnClose: TBitBtn;
    btnHelp: TBitBtn;
    rgStreamtype: TRadioGroup;
    quadtreeStream: TRbwQuadTree;
    procedure GetLinkTolerance;
    procedure btnApplyClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure rgStreamtypeClick(Sender: TObject);
  private
    FTolerance: double;
    FLinkType: TLinkType;
    FScreenObjects: TList<TScreenObject>;
    FLakeObjects: TList<TScreenObject>;
    FSwrObjects: TList<TScreenObject>;
    FSegmentNumbers: TIntegerList;
    procedure FillSegmentsAndSegmentNumbersLists(var DuplicateNumbers: Boolean);
    procedure RenumberSegments;
    procedure FillQuadTree;
    procedure FillLakeObjects;
    procedure FillSwrObjects;
    procedure LocateNearestStream(TestScreenObject: TScreenObject;
      var NearestStream: TScreenObject);
    procedure LocateNearestLake(TestScreenObject: TScreenObject;
      var NearestLake: TScreenObject);
    procedure LocateNearestLakeOrStream(TestScreenObject: TScreenObject;
      var NearestLake, NearestStream: TScreenObject);
    procedure LocateNearestSwrReachObjects(TestScreenObject: TScreenObject;
      var NearestReachObjects: TList<TScreenObject>; Tolerance: double);
    procedure LocateNearestModflow6LakeStream(TestScreenObject: TScreenObject;
      var NearestLake, NearestStream: TScreenObject);
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmLinkStreams: TfrmLinkStreams = nil;

implementation

uses frmGoPhastUnit, ModflowSfrParamIcalcUnit, ModflowStrUnit,
  ModflowBoundaryUnit, DataSetUnit, PhastModelUnit,
  DisplaySettingsUnit, FastGEO, GoPhastTypes;

resourcestring
  StrAssignStreamLinkag = 'assign stream linkages';
  StrNoOutflowSegmentN = 'No outflow segment numbers were changed.';
  StrSomeSegmentNumbers = 'Some segment numbers are duplicated. Do you want ' +
  'to renumber the segments with duplicate numbers? This can not be undone.';

{$R *.dfm}

function Point2D(X, Y: double): TPoint2D;
begin
  result.x := X;
  result.y := Y;
end;

{ TStreamLinkageChangeItem }

procedure TStreamLinkageChangeItem.Assign(Source: TPersistent);
var
  SourceItem: TStreamLinkageChangeItem;
begin
  SourceItem := Source as TStreamLinkageChangeItem;
  ScreenObject := SourceItem.ScreenObject;
  NewOutFlowSegment := SourceItem.NewOutFlowSegment;
  OldSwrReachConnections.Assign(SourceItem.OldSwrReachConnections);
  NewSwrReachConnections.Assign(SourceItem.NewSwrReachConnections);
end;

constructor TStreamLinkageChangeItem.Create(Collection: TCollection);
begin
  inherited;
  FNewSwrReachConnections := TSwrConnections.Create(nil);
  FOldSwrReachConnections := TSwrConnections.Create(nil);
end;

destructor TStreamLinkageChangeItem.Destroy;
begin
  FOldSwrReachConnections.Free;
  FNewSwrReachConnections.Free;
  inherited;
end;

function TStreamLinkageChangeItem.GetOutFlowSegment(Index: integer): integer;
begin
  result := FOldOutFlowSegments[Index];
end;

procedure TStreamLinkageChangeItem.SetNewOutFlowSegment(const Value: integer);
begin
  FNewOutFlowSegment := Value;
end;

procedure TStreamLinkageChangeItem.SetNewSwrReachConnections(
  const Value: TSwrConnections);
begin
  FNewSwrReachConnections.Assign(Value);
end;

procedure TStreamLinkageChangeItem.SetOldSwrReachConnections(
  const Value: TSwrConnections);
begin
  FOldSwrReachConnections.Assign(Value);
end;

procedure TStreamLinkageChangeItem.SetScreenObject(const Value: TScreenObject);
var
  ParamIcalc: TSfrParamIcalcCollection;
  Index: integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
  DownSegments: TIntegerCollection;
begin
  FScreenObject := Value;
  case (Collection as TStreamLinkageChangeCollection).FLinkType of
    ltSFR:
      begin
        Assert((FScreenObject.ModflowSfrBoundary <> nil)
          and FScreenObject.ModflowSfrBoundary.Used);
        ParamIcalc := FScreenObject.ModflowSfrBoundary.ParamIcalc;
        SetLength(FOldOutFlowSegments, ParamIcalc.Count);
        for Index := 0 to ParamIcalc.Count - 1 do
        begin
          FOldOutFlowSegments[Index] := ParamIcalc.Items[Index].OutflowSegment;
        end;
      end;
    ltSTR:
      begin
        Assert((FScreenObject.ModflowStrBoundary <> nil)
          and FScreenObject.ModflowStrBoundary.Used);

        if FScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
        begin
          ParamItem := FScreenObject.ModflowStrBoundary.Parameters[0];
          Values := ParamItem.Param as TStrCollection;
        end
        else
        begin
          Values := FScreenObject.ModflowStrBoundary.Values as TStrCollection;
        end;

        SetLength(FOldOutFlowSegments, Values.Count);
        for Index := 0 to Values.Count - 1 do
        begin
          FOldOutFlowSegments[Index] := (Values[Index] as TStrItem).OutflowSegment;
        end;
      end;
    ltSWR:
      begin
        Assert((FScreenObject.ModflowSwrReaches <> nil)
          and FScreenObject.ModflowSwrReaches.Used);
        FOldSwrReachConnections.Assign(FScreenObject.ModflowSwrReaches.Connections);
      end;
    ltSFR6:
      begin
        Assert((FScreenObject.ModflowSfr6Boundary <> nil)
          and FScreenObject.ModflowSfr6Boundary.Used);
        DownSegments := FScreenObject.ModflowSfr6Boundary.DownstreamSegments;
        SetLength(FOldOutFlowSegments, DownSegments.Count);
        for Index := 0 to DownSegments.Count - 1 do
        begin
          FOldOutFlowSegments[Index] := DownSegments[Index].Value;
        end;
      end;
    else
      Assert(False)
  end;
end;

{ TStreamLinkageChangeCollection }

function TStreamLinkageChangeCollection.Add: TStreamLinkageChangeItem;
begin
  result := inherited Add as TStreamLinkageChangeItem;
end;

constructor TStreamLinkageChangeCollection.Create(LinkType: TLinkType);
begin
  inherited Create(TStreamLinkageChangeItem);
  Assert(LinkType in [ltSFR, ltSTR, ltSWR, ltSFR6]);
  FLinkType := LinkType;
end;

function TStreamLinkageChangeCollection.GetItems(
  Index: integer): TStreamLinkageChangeItem;
begin
  result := inherited Items[Index] as TStreamLinkageChangeItem;
end;

procedure TStreamLinkageChangeCollection.SetItems(Index: integer;
  const Value: TStreamLinkageChangeItem);
begin
  inherited Items[Index] := Value;
end;

{ TUndoChangeStreamLinkages }

constructor TUndoChangeStreamLinkages.Create(
  Linkages: TStreamLinkageChangeCollection);
begin
  inherited Create;
  FLinkages:= TStreamLinkageChangeCollection.Create(Linkages.FLinkType);
  FLinkages.Assign(Linkages);
end;

function TUndoChangeStreamLinkages.Description: string;
begin
  result := StrAssignStreamLinkag;
end;

destructor TUndoChangeStreamLinkages.Destroy;
begin
  FLinkages.Free;
  inherited;
end;

procedure TUndoChangeStreamLinkages.DoCommand;
var
  Index: Integer;
  Item: TStreamLinkageChangeItem;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
  ItemIndex: Integer;
  ADataArray: TDataArray;
  DownSegments: TIntegerCollection;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FLinkType of
      ltSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment := Item.NewOutFlowSegment;
          end;
        end;
      ltSTR:
        begin
          if Item.ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
          begin
            ParamItem := Item.ScreenObject.ModflowStrBoundary.Parameters[0];
            Values := ParamItem.Param as TStrCollection;
          end
          else
          begin
            Values := Item.ScreenObject.ModflowStrBoundary.Values as TStrCollection;
          end;

          for ItemIndex := 0 to Values.Count - 1 do
          begin
            (Values[ItemIndex] as TStrItem).OutflowSegment := Item.NewOutFlowSegment;
          end;
        end;
      ltSWR:
        begin
          Item.ScreenObject.ModflowSwrReaches.Connections := Item.NewSwrReachConnections;
          ADataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSwrReach);
          ADataArray.Invalidate;
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end;
      ltSFR6:
        begin
          DownSegments := Item.ScreenObject.ModflowSfr6Boundary.DownstreamSegments;
          DownSegments.Clear;
          if Item.NewOutFlowSegment <> 0 then
          begin
            DownSegments.Add.Value := Item.NewOutFlowSegment;
          end;
        end
      else
        Assert(False);
    end;
  end;
  case FLinkages.FLinkType of
    ltSFR:
      begin
        if frmGoPhast.PhastModel.SfrStreamLinkPlot.StreamsToPlot <> stpNone then
        begin
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end;
      end;
    ltSTR:
      begin
        if frmGoPhast.PhastModel.StrStreamLinkPlot.StreamsToPlot <> stpNone then
        begin
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end;
      end;
    ltSWR:
      begin
        // do nothing
      end;
    ltSFR6:
      begin
        // do nothing
      end
    else
      Assert(False);
  end;
  UpdateStreamLinkDisplay;
end;

procedure TUndoChangeStreamLinkages.Undo;
var
  Index: Integer;
  Item: TStreamLinkageChangeItem;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  ParamItem: TModflowParamItem;
  Values: TStrCollection;
  ItemIndex: Integer;
  ADataArray: TDataArray;
  DownSegments: TIntegerCollection;
begin
  for Index := 0 to FLinkages.Count - 1 do
  begin
    Item := FLinkages.Items[Index];
    case FLinkages.FLinkType of
      ltSFR:
        begin
          ParamIcalc := Item.ScreenObject.ModflowSfrBoundary.ParamIcalc;
          for PI_Index := 0 to ParamIcalc.Count - 1 do
          begin
            ParamIcalc.Items[PI_Index].OutflowSegment :=
              Item.OldOutFlowSegments[PI_Index];
          end;
        end;
      ltSTR:
        begin
          if Item.ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
          begin
            ParamItem := Item.ScreenObject.ModflowStrBoundary.Parameters[0];
            Values := ParamItem.Param as TStrCollection;
          end
          else
          begin
            Values := Item.ScreenObject.ModflowStrBoundary.Values as TStrCollection;
          end;

          for ItemIndex := 0 to Values.Count - 1 do
          begin
            (Values[ItemIndex] as TStrItem).OutflowSegment := Item.OldOutFlowSegments[ItemIndex];
          end;
        end;
      ltSWR:
        begin
          Item.ScreenObject.ModflowSwrReaches.Connections := Item.OldSwrReachConnections;
          ADataArray := frmGoPhast.PhastModel.DataArrayManager.GetDataSetByName(KSwrReach);
          ADataArray.Invalidate;
          frmGoPhast.frameTopView.ModelChanged := True;
          frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
        end;
      ltSFR6:
        begin
          DownSegments := Item.ScreenObject.ModflowSfr6Boundary.DownstreamSegments;
          DownSegments.Clear;
          for PI_Index := 0 to Length(Item.FOldOutFlowSegments) - 1 do
          begin
            DownSegments.Add.Value := Item.OldOutFlowSegments[PI_Index];
          end;
        end
      else
        Assert(False);
    end;
  end;
  UpdateStreamLinkDisplay;
end;

procedure TUndoChangeStreamLinkages.UpdateStreamLinkDisplay;
begin
  if (FLinkages.Count > 0) then
  begin
    case FLinkages.FLinkType of
      ltSFR:
        begin
          if frmGoPhast.PhastModel.SfrStreamLinkPlot.PlotStreamConnections then
          begin
            frmGoPhast.frameTopView.ModelChanged := True;
            frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
          end;
        end;
      ltSTR:
        begin
          if frmGoPhast.PhastModel.StrStreamLinkPlot.PlotStreamConnections then
          begin
            frmGoPhast.frameTopView.ModelChanged := True;
            frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
          end;
        end;
      ltSWR:
        begin
          if frmGoPhast.PhastModel.SwrReachConnectionsPlot.PlotReachConnections then
          begin
            frmGoPhast.frameTopView.ModelChanged := True;
            frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
          end;
        end;
      ltSFR6:
        begin
          if frmGoPhast.PhastModel.SfrMf6StreamLinkPlot.PlotStreamConnections then
          begin
            frmGoPhast.frameTopView.ModelChanged := True;
            frmGoPhast.frameTopView.ZoomBox.InvalidateImage32;
          end;
        end;
    end;
  end;
end;

procedure TfrmLinkStreams.SetData;
var
  Linkages: TStreamLinkageChangeCollection;
  ScreenObjectIndex: Integer;
  ScreenObject: TScreenObject;
  NearestLake, NearestStream: TScreenObject;
  Linkage: TStreamLinkageChangeItem;
  OutflowSegmentAssigned: boolean;
  ParamIcalc: TSfrParamIcalcCollection;
  PI_Index: Integer;
  OutFlowSegmentNeedsToChange: Boolean;
  Values: TStrCollection;
  ParamItem: TModflowParamItem;
  ItemIndex: Integer;
  StrItem: TStrItem;
  NearbyReachObjects: TList<TScreenObject>;
  ConnectionIndex: Integer;
  AConnection: TSwrConnectionItem;
  ObjectIndex: Integer;
  DuplicateNumbers: Boolean;
  DownSegments: TIntegerCollection;
begin
  FTolerance := StrToFloat(rdeTolerance.Text);
  FLinkType := TLinkType(rgStreamtype.ItemIndex);

  FScreenObjects := TList<TScreenObject>.Create;
  FLakeObjects := TList<TScreenObject>.Create;
  FSwrObjects := TList<TScreenObject>.Create;
  FSegmentNumbers := TIntegerList.Create;
  try
    FillSegmentsAndSegmentNumbersLists(DuplicateNumbers);
    if DuplicateNumbers then
    begin
      RenumberSegments;
    end;

    FillQuadTree;
    FillLakeObjects;
    FillSwrObjects;

    NearbyReachObjects := TList<TScreenObject>.Create;
    Linkages := TStreamLinkageChangeCollection.Create(FLinkType);
    try
      for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
      begin
        ScreenObject := FScreenObjects[ScreenObjectIndex];
        if (rgWhatToLink.ItemIndex = 1) and not ScreenObject.Selected then
        begin
          Continue;
        end;
        if cbKeepExistingLinkages.Checked then
        begin
          OutflowSegmentAssigned := False;
          case FLinkType of
            ltSFR:
              begin
                ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
                for PI_Index := 0 to ParamIcalc.Count - 1 do
                begin
                  if ParamIcalc.Items[PI_Index].OutflowSegment <> 0 then
                  begin
                    OutflowSegmentAssigned := True;
                    break;
                  end;
                end;
              end;
            ltSTR:
              begin
                if ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
                begin
                  ParamItem := ScreenObject.ModflowStrBoundary.Parameters[0];
                  Values := ParamItem.Param as TStrCollection;
                end
                else
                begin
                  Values := ScreenObject.ModflowStrBoundary.Values as TStrCollection;
                end;
                for ItemIndex := 0 to Values.Count - 1 do
                begin
                  StrItem := Values[ItemIndex] as TStrItem;
                  if StrItem.OutflowSegment > 0 then
                  begin
                    OutflowSegmentAssigned := True;
                    break;
                  end;
                end;
              end;
            ltSWR:
              begin
                OutflowSegmentAssigned := False;
                // do nothing.
              end;
            ltSFR6:
              begin
                DownSegments := ScreenObject.ModflowSfr6Boundary.DownstreamSegments;
                OutflowSegmentAssigned := DownSegments.Count > 0
              end
            else Assert(False);
          end;
          if OutflowSegmentAssigned then
          begin
            Continue;
          end;
        end;
        NearestStream := nil;
        NearestLake := nil;
        case FLinkType of
          ltSFR:
            begin
              LocateNearestLakeOrStream(ScreenObject, NearestLake,
                NearestStream);
            end;
          ltSTR:
            begin
              LocateNearestStream(ScreenObject, NearestStream);
            end;
          ltSWR:
            begin
              LocateNearestSwrReachObjects(ScreenObject,
                NearbyReachObjects, FTolerance);
              if rgWhatToLink.ItemIndex = 1 then
              begin
                for ObjectIndex := NearbyReachObjects.Count - 1 downto 0 do
                begin
                  if not NearbyReachObjects[ObjectIndex].Selected then
                  begin
                    NearbyReachObjects.Delete(ObjectIndex);
                  end;
                end;
              end;
            end;
          ltSFR6:
            begin
              LocateNearestModflow6LakeStream(ScreenObject, NearestLake,
                NearestStream);
            end
          else Assert(False);
        end;
        if (NearestStream = nil) and (NearestLake = nil)
          and (NearbyReachObjects.Count = 0) then
        begin
          if not cbKeepExistingLinkages.Checked then
          begin
            case FLinkType of
              ltSFR:
                begin
                  OutFlowSegmentNeedsToChange := False;
                  ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
                  for PI_index := 0 to ParamIcalc.Count - 1 do
                  begin
                    if ParamIcalc.Items[PI_index].OutflowSegment <> 0 then
                    begin
                      OutFlowSegmentNeedsToChange := True;
                      break;
                    end;
                  end;
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment := 0;
                  end;
                end;
              ltSTR:
                begin
                  if ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
                  begin
                    ParamItem := ScreenObject.ModflowStrBoundary.Parameters[0];
                    Values := ParamItem.Param as TStrCollection;
                  end
                  else
                  begin
                    Values := ScreenObject.ModflowStrBoundary.Values as TStrCollection;
                  end;
                  OutFlowSegmentNeedsToChange := False;
                  for ItemIndex := 0 to Values.Count - 1 do
                  begin
                    if (Values[ItemIndex] as TStrItem).OutflowSegment <> 0 then
                    begin
                      OutFlowSegmentNeedsToChange := True;
                      break;
                    end;
                  end;
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment := 0;
                  end;
                end;
              ltSWR:
                begin
                  if ScreenObject.ModflowSwrReaches.Connections.Count > 0 then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewSwrReachConnections.Clear;
                  end;
                end;
              ltSFR6:
                begin
//                  OutFlowSegmentNeedsToChange := False;
                  DownSegments := ScreenObject.ModflowSfr6Boundary.DownstreamSegments;
                  OutFlowSegmentNeedsToChange := DownSegments.Count > 0;
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment := 0;
                  end;
                end;
            end;
          end;
          Continue;
        end
        else
        begin
          if NearestLake = nil then
          begin
            case FLinkType of
              ltSFR:
                begin
                  Assert(NearestStream.ModflowSfrBoundary <> nil);
                  OutFlowSegmentNeedsToChange := False;
                  ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
                  for PI_index := 0 to ParamIcalc.Count - 1 do
                  begin
                    if ParamIcalc.Items[PI_index].OutflowSegment <>
                      NearestStream.ModflowSfrBoundary.SegmentNumber then
                    begin
                      OutFlowSegmentNeedsToChange := True;
                      break;
                    end;
                  end;
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment :=
                      NearestStream.ModflowSfrBoundary.SegmentNumber;
                  end;
                end;
              ltSTR:
                begin
                  Assert(NearestStream.ModflowStrBoundary <> nil);
                  OutFlowSegmentNeedsToChange := False;

                  if ScreenObject.ModflowStrBoundary.Parameters.Count > 0 then
                  begin
                    ParamItem := ScreenObject.ModflowStrBoundary.Parameters[0];
                    Values := ParamItem.Param as TStrCollection;
                  end
                  else
                  begin
                    Values := ScreenObject.ModflowStrBoundary.Values as TStrCollection;
                  end;

                  for ItemIndex := 0 to Values.Count - 1 do
                  begin
                    if (Values[ItemIndex] as TStrItem).OutflowSegment <>
                      NearestStream.ModflowStrBoundary.SegmentNumber then
                    begin
                      OutFlowSegmentNeedsToChange := True;
                      break;
                    end;
                  end;
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment :=
                      NearestStream.ModflowStrBoundary.SegmentNumber;
                  end;
                end;
              ltSWR:
                begin
                  if cbKeepExistingLinkages.Checked then
                  begin
                    for ConnectionIndex := 0 to ScreenObject.ModflowSwrReaches.Connections.Count - 1 do
                    begin
                      AConnection := ScreenObject.ModflowSwrReaches.Connections[ConnectionIndex];
                      if AConnection.Method in [scmObject, scmSameCell] then
                      begin
                        for ObjectIndex := NearbyReachObjects.Count - 1 downto 0 do
                        begin
                          if NearbyReachObjects[ObjectIndex].Name = AConnection.ScreenObjectName then
                          begin
                            NearbyReachObjects.Delete(ObjectIndex);
                          end;
                        end;
                      end;
                    end;
                  end;
                  if NearbyReachObjects.Count > 0 then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewSwrReachConnections := ScreenObject.ModflowSwrReaches.Connections;
                    if not cbKeepExistingLinkages.Checked then
                    begin
                      Linkage.NewSwrReachConnections.Clear;
                    end;
                    for ObjectIndex := 0 to NearbyReachObjects.Count - 1 do
                    begin
                      AConnection := Linkage.NewSwrReachConnections.Add;
                      AConnection.Method := scmObject;
                      AConnection.ScreenObject := NearbyReachObjects[ObjectIndex];
                    end;
                  end;
                end;
              ltSFR6:
                begin
                  Assert(NearestStream.ModflowSfr6Boundary <> nil);
//                  OutFlowSegmentNeedsToChange := False;
                  DownSegments := ScreenObject.ModflowSfr6Boundary.DownstreamSegments;
                  OutFlowSegmentNeedsToChange := (DownSegments.Count <> 1)
                    or (DownSegments[0].Value <> NearestStream.ModflowSfr6Boundary.SegmentNumber);
                  if OutFlowSegmentNeedsToChange then
                  begin
                    Linkage := Linkages.Add;
                    Linkage.ScreenObject := ScreenObject;
                    Linkage.NewOutFlowSegment :=
                      NearestStream.ModflowSfr6Boundary.SegmentNumber;
                  end;
                end
              else Assert(False);
            end;
          end
          else
          begin
            Assert(NearestLake.ModflowLakBoundary <> nil);
            OutFlowSegmentNeedsToChange := False;
            ParamIcalc := ScreenObject.ModflowSfrBoundary.ParamIcalc;
            for PI_index := 0 to ParamIcalc.Count - 1 do
            begin
              if ParamIcalc.Items[PI_index].OutflowSegment <>
                -NearestLake.ModflowLakBoundary.LakeID then
              begin
                OutFlowSegmentNeedsToChange := True;
                break;
              end;
            end;
            if OutFlowSegmentNeedsToChange then
            begin
              Linkage := Linkages.Add;
              Linkage.ScreenObject := ScreenObject;
              Linkage.NewOutFlowSegment :=
                -NearestLake.ModflowLakBoundary.LakeID;
            end;
          end;
        end;
      end;
      if Linkages.Count > 0 then
      begin
        frmGoPhast.UndoStack.Submit(TUndoChangeStreamLinkages.Create(Linkages));
      end
      else
      begin
        Beep;
        MessageDlg(StrNoOutflowSegmentN, mtInformation,
          [mbOK], 0);
      end;
    finally
      NearbyReachObjects.Free;
      Linkages.Free;
    end;
  finally
    FScreenObjects.Free;
    FSegmentNumbers.Free;
    FLakeObjects.Free;
    FSwrObjects.Free;
  end;
end;

procedure TfrmLinkStreams.LocateNearestStream(TestScreenObject: TScreenObject;
  var NearestStream: TScreenObject);
var
  TestDistance: Double;
  MinDistance: Double;
  NearbyStreams: TQuadPointInRegionArray;
  PointIndex: Integer;
  FirstFound: Boolean;
  ObjectIndex: Integer;
  TestObject: TScreenObject;
  OutFlowLocation: TPoint2D;
begin
  NearestStream := nil;
  MinDistance := 0;
  OutFlowLocation := TestScreenObject.Points[TestScreenObject.Count - 1];
  if quadtreeStream.Count > 0 then
  begin
    quadtreeStream.FindPointsInCircle(OutFlowLocation.x, OutFlowLocation.y, FTolerance, NearbyStreams);
    if Length(NearbyStreams) > 0 then
    begin
      FirstFound := False;
      for PointIndex := 0 to Length(NearbyStreams) - 1 do
      begin
        if FirstFound then
        begin
          TestDistance := Distance(OutFlowLocation, Point2D(NearbyStreams[PointIndex].x, NearbyStreams[PointIndex].y));
          if TestDistance < MinDistance then
          begin
            for ObjectIndex := 0 to Length(NearbyStreams[PointIndex].Data) - 1 do
            begin
              TestObject := NearbyStreams[PointIndex].Data[ObjectIndex];
              if TestObject <> TestScreenObject then
              begin
                NearestStream := TestObject;
                MinDistance := TestDistance;
                break;
              end;
            end;
          end;
        end
        else
        begin
          MinDistance := Distance(OutFlowLocation, Point2D(NearbyStreams[PointIndex].x, NearbyStreams[PointIndex].y));
          for ObjectIndex := 0 to Length(NearbyStreams[PointIndex].Data) - 1 do
          begin
            TestObject := NearbyStreams[PointIndex].Data[ObjectIndex];
            if TestObject <> TestScreenObject then
            begin
              NearestStream := TestObject;
              FirstFound := True;
              break;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TfrmLinkStreams.LocateNearestSwrReachObjects(
  TestScreenObject: TScreenObject;
  var NearestReachObjects: TList<TScreenObject>; Tolerance: double);
var
  ReferenceLocation: TPoint2D;
  Index: Integer;
  AScreenObject: TScreenObject;
  TestLocation: TPoint2D;
  TestDist: double;
begin
  NearestReachObjects.Clear;
  for Index := 0 to FSwrObjects.Count - 1 do
    begin
      AScreenObject := FSwrObjects[Index];
      if AScreenObject <> TestScreenObject then
      begin
        TestLocation := AScreenObject.Points[0];
        ReferenceLocation := TestScreenObject.Points[0];
        TestDist := Distance(TestLocation, ReferenceLocation);
        if (TestDist < Tolerance) then
        begin
          NearestReachObjects.Add(AScreenObject);
        end
        else if TestScreenObject.Count > 1 then
        begin
          ReferenceLocation := TestScreenObject.Points[TestScreenObject.Count-1];
          TestDist := Distance(TestLocation, ReferenceLocation);
          if (TestDist < Tolerance) then
          begin
            NearestReachObjects.Add(AScreenObject);
          end
        end;
      end;
    end;

end;

procedure TfrmLinkStreams.LocateNearestLake(TestScreenObject: TScreenObject;
  var NearestLake: TScreenObject);
var
  MinDist: Real;
  TestDist: Real;
  TestLocation: TPoint2D;
  OutFlowLocation: TPoint2D;
  ALake: TScreenObject;
  LakeIndex: Integer;
  SectionIndex: Integer;
begin
  NearestLake := nil;
  OutFlowLocation := TestScreenObject.Points[TestScreenObject.Count - 1];
  MinDist := MaxInt;
  for LakeIndex := 0 to FLakeObjects.Count - 1 do
  begin
    ALake := FLakeObjects[LakeIndex];
    if ALake = TestScreenObject then
    begin
      Continue;
    end;
    if (NearestLake = nil) then
    begin
      MinDist := ALake.DistanceToScreenObject(OutFlowLocation, TestLocation, 1, SectionIndex);
      if MinDist <= FTolerance then
      begin
        NearestLake := ALake;
        if MinDist = 0 then
        begin
          Break;
        end;
      end;
    end
    else
    begin
      TestDist := ALake.DistanceToScreenObject(OutFlowLocation, TestLocation, 1, SectionIndex);
      if TestDist <= MinDist then
      begin
        NearestLake := ALake;
        MinDist := TestDist;
        if MinDist = 0 then
        begin
          Break;
        end;
      end;
    end;
  end;
end;

procedure TfrmLinkStreams.FillLakeObjects;
var
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
begin
  FLakeObjects.Clear;
  if (FLinkType = ltSFR) and frmGoPhast.PhastModel.LakePackageUsed(nil) then
  begin
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if (ScreenObject.ModflowLakBoundary <> nil)
        and ScreenObject.ModflowLakBoundary.Used  then
      begin
        FLakeObjects.Add(ScreenObject);
      end;
    end;
  end;

end;

procedure TfrmLinkStreams.FillQuadTree;
var
  ObjectIndex: Integer;
  AScreenObject: TScreenObject;
  APoint: TPoint2D;
begin
  quadtreeStream.Clear;
  if FLinkType in [ltSFR, ltSTR, ltSFR6] then
  begin
    for ObjectIndex := 0 to FScreenObjects.Count - 1 do
    begin
      AScreenObject := FScreenObjects[ObjectIndex];
      APoint := AScreenObject.Points[0];

      quadtreeStream.AddPoint(APoint.x, APoint.y, AScreenObject);
    end;
  end;
end;

procedure TfrmLinkStreams.FillSegmentsAndSegmentNumbersLists(var DuplicateNumbers: Boolean);
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  SegmentNumber: Integer;
begin
  FSegmentNumbers.Sorted := True;
  DuplicateNumbers := False;
  for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
    if ScreenObject.Deleted then
    begin
      Continue;
    end;
    SegmentNumber := 0;
    case FLinkType of
      ltSFR:
        begin
          if (ScreenObject.ModflowSfrBoundary = nil) or not ScreenObject.ModflowSfrBoundary.Used then
          begin
            Continue;
          end
          else
          begin
            SegmentNumber := ScreenObject.ModflowSfrBoundary.SegmentNumber;
          end;
        end;
      ltSTR:
        begin
          if (ScreenObject.ModflowStrBoundary = nil) or not ScreenObject.ModflowStrBoundary.Used then
          begin
            Continue;
          end
          else
          begin
            SegmentNumber := ScreenObject.ModflowStrBoundary.SegmentNumber;
          end;
        end;
      ltSWR:
        begin
          if (ScreenObject.ModflowSwrReaches = nil) or not ScreenObject.ModflowSwrReaches.Used then
          begin
            Continue;
          end;
        end;
      ltSFR6:
        begin
          if (ScreenObject.ModflowSfr6Boundary = nil) or not ScreenObject.ModflowSfr6Boundary.Used then
          begin
            Continue;
          end
          else
          begin
            SegmentNumber := ScreenObject.ModflowSfr6Boundary.SegmentNumber;
          end;
        end
    else
      Assert(False);
    end;
    FScreenObjects.Add(ScreenObject);
    if FLinkType in [ltSFR, ltSTR, ltSFR6] then
    begin
      if FSegmentNumbers.IndexOf(SegmentNumber) >= 0 then
      begin
        DuplicateNumbers := True;
      end;
      FSegmentNumbers.Add(SegmentNumber);
    end;
  end;
end;

procedure TfrmLinkStreams.FillSwrObjects;
var
  ScreenObjectIndex: integer;
  ScreenObject: TScreenObject;
begin
  FSwrObjects.Clear;
  if FLinkType = ltSWR then
  begin
    for ScreenObjectIndex := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
    begin
      ScreenObject := frmGoPhast.PhastModel.ScreenObjects[ScreenObjectIndex];
      if ScreenObject.Deleted then
      begin
        Continue;
      end;
      if (ScreenObject.ModflowSwrReaches <> nil)
        and ScreenObject.ModflowSwrReaches.Used then
      begin
        FSwrObjects.Add(ScreenObject);
      end;
    end;
  end;
end;

procedure TfrmLinkStreams.RenumberSegments;
var
  ScreenObject: TScreenObject;
  NextNumber: Integer;
  SegmentNumber: Integer;
  ScreenObjectIndex: Integer;
begin
  Beep;
  if (MessageDlg(StrSomeSegmentNumbers, mtWarning, [mbYes, mbNo], 0) = mrYes) then
  begin
    NextNumber := FSegmentNumbers.Last + 2;
    FSegmentNumbers.Clear;
    FSegmentNumbers.Sorted := True;
    for ScreenObjectIndex := 0 to FScreenObjects.Count - 1 do
    begin
      ScreenObject := FScreenObjects[ScreenObjectIndex];
      SegmentNumber := 0;
      case FLinkType of
        ltSFR:
          begin
            SegmentNumber := ScreenObject.ModflowSfrBoundary.SegmentNumber;
            if FSegmentNumbers.IndexOf(SegmentNumber) >= 0 then
            begin
              ScreenObject.ModflowSfrBoundary.SegmentNumber := NextNumber;
              Inc(NextNumber);
            end;
          end;
        ltSTR:
          begin
            SegmentNumber := ScreenObject.ModflowStrBoundary.SegmentNumber;
            if FSegmentNumbers.IndexOf(SegmentNumber) >= 0 then
            begin
              ScreenObject.ModflowStrBoundary.SegmentNumber := NextNumber;
              Inc(NextNumber);
            end;
          end;
        ltSFR6:
          begin
            SegmentNumber := ScreenObject.ModflowSfr6Boundary.SegmentNumber;
            if FSegmentNumbers.IndexOf(SegmentNumber) >= 0 then
            begin
              ScreenObject.ModflowSfr6Boundary.SegmentNumber := NextNumber;
              Inc(NextNumber);
            end;
          end;
      else
        Assert(False);
      end;
      FSegmentNumbers.Add(SegmentNumber);
    end;
  end;
end;

procedure TfrmLinkStreams.btnApplyClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmLinkStreams.FormShow(Sender: TObject);
begin
  inherited;
  GetData;
  OnShow := nil;
end;

procedure TfrmLinkStreams.GetData;
begin
  GetLinkTolerance;
  rgStreamtype.Buttons[0].Enabled := frmGoPhast.PhastModel.SfrIsSelected;
  rgStreamtype.Buttons[1].Enabled := frmGoPhast.PhastModel.StrIsSelected;
  rgStreamtype.Buttons[2].Enabled := frmGoPhast.PhastModel.SwrIsSelected;
  rgStreamtype.Buttons[3].Enabled := frmGoPhast.PhastModel.Sfr6IsSelected;
  if not rgStreamtype.Buttons[0].Enabled then
  begin
    if rgStreamtype.Buttons[1].Enabled then
    begin
      rgStreamtype.ItemIndex := 1;
    end
    else if rgStreamtype.Buttons[2].Enabled then
    begin
      rgStreamtype.ItemIndex := 2;
    end
    else
    begin
      rgStreamtype.ItemIndex := 3;
    end;
  end;
end;

procedure TfrmLinkStreams.GetLinkTolerance;
var
  Tolerance: double;
  Index: Integer;
begin
  if frmGoPhast.PhastModel.ModflowGrid.RowCount > 0 then
  begin
    Tolerance := frmGoPhast.PhastModel.ModflowGrid.RowWidth[0];
    for Index := 0 to frmGoPhast.PhastModel.ModflowGrid.RowCount - 1 do
    begin
      if frmGoPhast.PhastModel.ModflowGrid.RowWidth[Index] < Tolerance then
      begin
        Tolerance := frmGoPhast.PhastModel.ModflowGrid.RowWidth[Index];
      end;
    end;
    for Index := 0 to frmGoPhast.PhastModel.ModflowGrid.ColumnCount - 1 do
    begin
      if frmGoPhast.PhastModel.ModflowGrid.ColumnWidth[Index] < Tolerance then
      begin
        Tolerance := frmGoPhast.PhastModel.ModflowGrid.ColumnWidth[Index];
      end;
    end;
    Tolerance := Tolerance /2;
  end
  else
  begin
    Tolerance := -1;
  end;
  rdeTolerance.Text := FloatToStr(Tolerance);
end;

procedure TfrmLinkStreams.LocateNearestLakeOrStream(
  TestScreenObject: TScreenObject; var NearestLake,
  NearestStream: TScreenObject);
begin
  LocateNearestLake(TestScreenObject, NearestLake);

  if NearestLake = nil then
  begin
    LocateNearestStream(TestScreenObject, NearestStream);
  end
  else
  begin
    NearestStream := nil;
  end;
end;

procedure TfrmLinkStreams.LocateNearestModflow6LakeStream(
  TestScreenObject: TScreenObject; var NearestLake,
  NearestStream: TScreenObject);
begin
  LocateNearestLake(TestScreenObject, NearestLake);

  if NearestLake = nil then
  begin
    LocateNearestStream(TestScreenObject, NearestStream);
  end
  else
  begin
    NearestStream := nil;
  end;
end;

procedure TfrmLinkStreams.rgStreamtypeClick(Sender: TObject);
begin
  inherited;
  case TLinkType(rgStreamtype.ItemIndex) of
    ltSFR, ltSTR, ltSFR6:
      begin
        rgWhatToLink.Items[0] := 'All streams';
        rgWhatToLink.Items[1] := 'Selected streams';
      end;
    ltSWR:
      begin
        rgWhatToLink.Items[0] := 'All reach objects';
        rgWhatToLink.Items[1] := 'Selected reach objects';
      end;
  end;
end;

end.
