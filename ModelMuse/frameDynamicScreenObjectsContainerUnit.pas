unit frameDynamicScreenObjectsContainerUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls,
  JvExComCtrls, JvPageListTreeView, Vcl.StdCtrls, Vcl.ExtCtrls, JvExControls,
  JvPageList, UndoItemsScreenObjects, Modflow6DynamicTimeSeriesUnit,
  frameModflow6DynamicTimeSeriesUnit, Modflow6DynamicTimeSeriesInterfaceUnit,
  System.Math, RbwDataGrid4;

type
  TframeDynamicScreenObjectsContainer = class(TFrame)
    plTimeSeries: TJvPageList;
    pnlBottom: TPanel;
    btnAddGroup: TButton;
    btnDeleteGroup: TButton;
    tvTimeSeries: TJvPageListTreeView;
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
    procedure tvTimeSeriesChange(Sender: TObject; Node: TTreeNode);
  private
    FShouldSetData: Boolean;
    FTimesSeries: TDynamicTimesSeriesCollections;
    FPestNames: TStringList;
    FGridButtonEvent: TGridButtonEvent;
    function AddNewFrame(NewName: string): TframeModflow6DynamicTimeSeries;
    procedure edGroupNameChange(Sender: TObject);
    procedure AssignProperties;
    { Private declarations }
  public
    destructor Destroy; override;
    procedure SetGridButtonEvent(const Value: TGridButtonEvent);
    property GridButtonEvent: TGridButtonEvent read FGridButtonEvent write SetGridButtonEvent;
    function GetData(ScreenObjectList: TScreenObjectEditCollection): Boolean;
    procedure SetData(ScreenObjectList: TScreenObjectEditCollection);
    function DynamicTimeSeriesNames: TStringList;
    function GetTimeSeriesByName(ASeriesName: String): TDynamicTimeSeries;
    { Public declarations }
  end;

implementation

uses
  PhastModelInterfaceUnit, GoPhastTypes, ScreenObjectUnit, PhastModelUnit,
  ModflowParameterUnit, Modflow6TimeSeriesCollectionsInterfaceUnit,
  Modflow6TimeSeriesInterfaceUnit;

{$R *.dfm}

{ TframeDynamicScreenObjectsContainer }

function TframeDynamicScreenObjectsContainer.AddNewFrame(
  NewName: string): TframeModflow6DynamicTimeSeries;
var
  NewPage: TJvCustomPage;
  NewNode: TJvPageIndexNode;
begin
  NewPage := TJvStandardPage.Create(plTimeSeries);
  NewPage.PageList := plTimeSeries;
  NewNode := tvTimeSeries.Items.AddChild(nil, NewName) as TJvPageIndexNode;
  NewNode.PageIndex := NewPage.PageIndex;
  result := TframeModflow6DynamicTimeSeries.Create(NewPage);
  result.Parent := NewPage;
  result.Align := AlClient;
  result.edGroupName.Tag := plTimeSeries.PageCount - 1;
  result.edGroupName.OnChange := edGroupNameChange;
  result.edGroupName.Text := NewName;
  result.InitializeGrid;
  result.rrdgTimeSeries.OnButtonClick := GridButtonEvent;
  plTimeSeries.ActivePage := NewPage;
  tvTimeSeries.Selected := NewNode;
end;

procedure TframeDynamicScreenObjectsContainer.AssignProperties;
var
  AFrame: TframeModflow6DynamicTimeSeries;
  SeriesIndex: Integer;
begin
  for SeriesIndex := 0 to plTimeSeries.PageCount - 1 do
  begin
    AFrame := plTimeSeries.Pages[SeriesIndex].Controls[0] as TframeModflow6DynamicTimeSeries;
    AFrame.SetData;
  end;
end;

procedure TframeDynamicScreenObjectsContainer.btnAddGroupClick(Sender: TObject);
var
  NewName: String;
  NewFrame: TframeModflow6DynamicTimeSeries;
  NewItem: ITimeSeriesCollectionItem;
  NewerItem: IDyanmicTimesSeriesCollection;
begin
  inherited;

  NewName := 'NewGroup' + IntToStr(tvTimeSeries.Items.Count + 1);
  NewFrame := AddNewFrame(NewName);
  NewItem := FTimesSeries.AddI;
  NewItem.TimesSeriesCollectionI.GroupName := AnsiString(NewName);

  // IDyanmicTimesSeriesCollection
  if NewItem.TimesSeriesCollectionI.QueryInterface(IDyanmicTimesSeriesCollection, NewerItem) <> 0 then
  begin
    Assert(False);
  end;

  NewFrame.GetData(NewerItem, FPestNames);
  tvTimeSeries.Selected.Data := NewerItem;
end;

procedure TframeDynamicScreenObjectsContainer.btnDeleteGroupClick(
  Sender: TObject);
var
  PageIndex: Integer;
  AFrame: TframeModflow6DynamicTimeSeries;
  APage: TJvCustomPage;
  NewActivePageIndex: Integer;
  TimesSeriesCollection: IDyanmicTimesSeriesCollection;
//  TimeSeries: TMf6TimeSeries;
  SeriesIndex: Integer;
  TimeSeries: ITimeSeries;
  Collection: TPhastCollection;
begin
  inherited;
  if tvTimeSeries.Selected <> nil then
  begin
    Collection := tvTimeSeries.Selected.Data;
    if Collection.QueryInterface(IDyanmicTimesSeriesCollection, TimesSeriesCollection) <> 0 then
    begin
      Assert(False)
    end;
//    TimesSeriesCollection := tvTimeSeries.Selected.Data;
    if TimesSeriesCollection <> nil then
    begin
      TimesSeriesCollection.Deleted := True;
      for SeriesIndex := 0 to TimesSeriesCollection.Count - 1 do
      begin
        TimeSeries := TimesSeriesCollection.ItemsI[SeriesIndex].TimeSeriesI;
        TimeSeries.Deleted := True;
      end;
    end;
    NewActivePageIndex := Max(plTimeSeries.ActivePageIndex-1,0);
    plTimeSeries.ActivePage.Free;
    tvTimeSeries.Selected.Free;
    if tvTimeSeries.Items.Count > 0 then
    begin
      tvTimeSeries.Selected := tvTimeSeries.Items[NewActivePageIndex];
      plTimeSeries.ActivePageIndex := NewActivePageIndex;
      for PageIndex := NewActivePageIndex to plTimeSeries.PageCount - 1 do
      begin
        APage := plTimeSeries.Pages[PageIndex];
        AFrame := APage.Controls[0] as TframeModflow6DynamicTimeSeries;
        AFrame.edGroupName.Tag := PageIndex;
      end;
    end
    else
    begin
      plTimeSeries.ActivePageIndex := -1;
    end;
  end;
end;

destructor TframeDynamicScreenObjectsContainer.Destroy;
begin
  FTimesSeries.Free;
  FPestNames.Free;
  inherited;
end;

function TframeDynamicScreenObjectsContainer.DynamicTimeSeriesNames: TStringList;
begin
  AssignProperties;
  result := FTimesSeries.TimeSeriesNames;
end;

procedure TframeDynamicScreenObjectsContainer.edGroupNameChange(
  Sender: TObject);
var
  Edit: TEdit;
begin
  Edit := Sender as TEdit;
  tvTimeSeries.Items[Edit.Tag].Text := Edit.Text;
end;

function TframeDynamicScreenObjectsContainer.GetData(
  ScreenObjectList: TScreenObjectEditCollection): Boolean;
var
  AScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  SteadyParams: TModflowSteadyParameters;
  ParamIndex: Integer;
  AParam: TModflowSteadyParameter;
  SeriesIndex: Integer;
  ASeriesItem: TDynamicTimeSeriesCollectionItem;
  ANewFrame: TframeModflow6DynamicTimeSeries;
  PageIndex: Integer;
begin
  result := IGlobalModel.ModelSelection = msModflow2015;
  try
    if not result then
    begin
      Exit;
    end;

    tvTimeSeries.Items.Clear;
    for PageIndex := plTimeSeries.PageCount - 1 downto 0 do
    begin
      plTimeSeries.Pages[PageIndex].Free;
    end;

    FreeAndNil(FTimesSeries);
    FTimesSeries := TDynamicTimesSeriesCollections.Create(nil, nil);

    Assert(ScreenObjectList.Count > 0);
    AScreenObject := ScreenObjectList[0].ScreenObject;
    FTimesSeries.Assign(AScreenObject.DyanmicTimesSeriesCollections);

    for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
    begin
      AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
      Result := FTimesSeries.IsSame(AScreenObject.DyanmicTimesSeriesCollections);
      if not Result then
      begin
        Exit;
      end;
    end;

    FreeAndNil(FPestNames);
    FPestNames := TStringList.Create;

    SteadyParams := (IGlobalModel as TCustomModel).ModflowSteadyParameters;
    for ParamIndex := 0 to SteadyParams.Count - 1 do
    begin
      AParam := SteadyParams[ParamIndex];
      if AParam.ParameterType = ptPEST then
      begin
        FPestNames.AddObject(AParam.ParameterName, AParam);
      end;
    end;
    FPestNames.Sorted := True;
    FPestNames.Sorted := False;
    FPestNames.Insert(0, 'none');

    for SeriesIndex := 0 to FTimesSeries.Count - 1 do
    begin
      ASeriesItem := FTimesSeries[SeriesIndex];
      if ASeriesItem.TimesSeriesCollection.Deleted then
      begin
        Continue;
      end;
      ANewFrame := AddNewFrame(String(ASeriesItem.TimesSeriesCollection.GroupName));
      tvTimeSeries.Selected.Data := ASeriesItem.TimesSeriesCollection;
      ANewFrame.GetData(ASeriesItem.TimesSeriesCollection, FPestNames);
    end;

  finally
    FShouldSetData := result;
  end;
end;

function TframeDynamicScreenObjectsContainer.GetTimeSeriesByName(
  ASeriesName: String): TDynamicTimeSeries;
begin
  AssignProperties;
  if FTimesSeries <> nil then
  begin
    result := FTimesSeries.GetTimeSeriesByName(ASeriesName);
  end
  else
  begin
    result := nil;
  end;
end;

procedure TframeDynamicScreenObjectsContainer.SetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  AScreenObject: TScreenObject;
begin
  if not FShouldSetData then
  begin
    Exit;
  end;
  AssignProperties;
  for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
  begin
    AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
    AScreenObject.DyanmicTimesSeriesCollections := FTimesSeries;
  end;
end;

procedure TframeDynamicScreenObjectsContainer.SetGridButtonEvent(
  const Value: TGridButtonEvent);
begin
  FGridButtonEvent := Value;
end;

procedure TframeDynamicScreenObjectsContainer.tvTimeSeriesChange(
  Sender: TObject; Node: TTreeNode);
begin
  plTimeSeries.ActivePageIndex := (Node as TJvPageIndexNode).PageIndex;
end;

end.
