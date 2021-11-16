unit frmTimeSeriesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, JvPageList,
  Vcl.ExtCtrls, JvExControls, Vcl.ComCtrls, JvExComCtrls, JvPageListTreeView,
  frameModflow6TimeSeriesUnit, Vcl.StdCtrls, Vcl.Buttons, Modflow6TimeSeriesUnit,
  UndoItems, Modflow6TimeSeriesCollectionsUnit;

type
  TUndoChangeTimeSeries = class(TCustomUndo)
  private
    FOldTimesSeries: TTimesSeriesCollections;
    FNewTimesSeries: TTimesSeriesCollections;
  protected
    // See TCustomUndo.@link(TCustomUndo.Description).
    function Description: string; override;
  public
    constructor Create(var TimesSeries: TTimesSeriesCollections);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;

  end;

  TfrmTimeSeries = class(TfrmCustomGoPhast)
    tvTimeSeries: TJvPageListTreeView;
    plTimeSeries: TJvPageList;
    pnlBottom: TPanel;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    btnAddGroup: TButton;
    btnDeleteGroup: TButton;
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
    procedure tvTimeSeriesChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
  private
    FTimesSeries: TTimesSeriesCollections;
    FPestNames: TStringList;
    procedure edGroupNameChange(Sender: TObject);
    function AddNewFrame(NewName: string): TframeModflow6TimeSeries;
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

var
  frmTimeSeries: TfrmTimeSeries;

implementation

uses
  System.Math, frmGoPhastUnit, ModflowParameterUnit, OrderedCollectionUnit;

{$R *.dfm}

procedure TfrmTimeSeries.btnAddGroupClick(Sender: TObject);
var
  NewName: string;
  NewFrame: TframeModflow6TimeSeries;
  NewItem: TimeSeriesCollectionItem;
begin
  inherited;

  NewName := 'NewGroup' + IntToStr(tvTimeSeries.Items.Count + 1);
  NewFrame := AddNewFrame(NewName);
  NewItem := FTimesSeries.Add;
  NewItem.TimesSeriesCollection.GroupName := NewName;
  NewFrame.GetData(NewItem, FPestNames);
end;

procedure TfrmTimeSeries.btnDeleteGroupClick(Sender: TObject);
var
  PageIndex: Integer;
  AFrame: TframeModflow6TimeSeries;
  APage: TJvCustomPage;
  NewActivePageIndex: Integer;
begin
  inherited;
  if tvTimeSeries.Selected <> nil then
  begin
    FTimesSeries.Items[plTimeSeries.ActivePageIndex].Free;
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
        AFrame := APage.Controls[0] as TframeModflow6TimeSeries;
        AFrame.edGroupName.Tag := PageIndex;
      end;
    end
    else
    begin
      plTimeSeries.ActivePageIndex := -1;
    end;
  end;
end;

procedure TfrmTimeSeries.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

procedure TfrmTimeSeries.edGroupNameChange(Sender: TObject);
var
  Edit: TEdit;
begin
  Edit := Sender as TEdit;
  tvTimeSeries.Items[Edit.Tag].Text := Edit.Text;
end;

procedure TfrmTimeSeries.FormCreate(Sender: TObject);
begin
  inherited;
  FPestNames := TStringList.Create;
  GetData;
end;

procedure TfrmTimeSeries.FormDestroy(Sender: TObject);
begin
  inherited;
  FPestNames.Free;
  FTimesSeries.Free;
end;

procedure TfrmTimeSeries.GetData;
var
  SeriesIndex: Integer;
  ASeriesItem: TimeSeriesCollectionItem;
  NewFrame: TframeModflow6TimeSeries;
  ParamIndex: Integer;
  SteadyParams: TModflowSteadyParameters;
  AParam: TModflowSteadyParameter;
begin
  SteadyParams := frmGoPhast.PhastModel.ModflowSteadyParameters;
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

  FTimesSeries := TTimesSeriesCollections.Create(nil);
  FTimesSeries.Assign(frmGoPhast.PhastModel.TimesSeries);
  for SeriesIndex := 0 to FTimesSeries.Count - 1 do
  begin
    ASeriesItem := FTimesSeries[SeriesIndex];
    NewFrame := AddNewFrame(ASeriesItem.TimesSeriesCollection.GroupName);
    NewFrame.GetData(ASeriesItem, FPestNames);
  end;
end;

procedure TfrmTimeSeries.SetData;
var
  AFrame: TframeModflow6TimeSeries;
  SeriesIndex: Integer;
begin
  Assert(plTimeSeries.PageCount = FTimesSeries.Count);
  for SeriesIndex := 0 to FTimesSeries.Count - 1 do
  begin
    AFrame := plTimeSeries.Pages[SeriesIndex].Controls[0] as TframeModflow6TimeSeries;
    AFrame.SetData;
  end;

  frmGoPhast.UndoStack.Submit(TUndoChangeTimeSeries.Create(FTimesSeries));
end;

function TfrmTimeSeries.AddNewFrame(NewName: string): TframeModflow6TimeSeries;
var
  NewPage: TJvCustomPage;
  NewNode: TJvPageIndexNode;
begin
  NewPage := TJvStandardPage.Create(plTimeSeries);
  NewPage.PageList := plTimeSeries;
  NewNode := tvTimeSeries.Items.AddChild(nil, NewName) as TJvPageIndexNode;
  NewNode.PageIndex := NewPage.PageIndex;
  result := TframeModflow6TimeSeries.Create(NewPage);
  result.Parent := NewPage;
  result.Align := AlClient;
  result.edGroupName.Tag := plTimeSeries.PageCount - 1;
  result.edGroupName.OnChange := edGroupNameChange;
  result.edGroupName.Text := NewName;
  result.InitializeGrid;
  plTimeSeries.ActivePage := NewPage;
end;

procedure TfrmTimeSeries.tvTimeSeriesChange(Sender: TObject; Node: TTreeNode);
begin
  inherited;
  plTimeSeries.ActivePageIndex := (Node as TJvPageIndexNode).PageIndex;
end;

{ TUndoChangeTimeSeries }

constructor TUndoChangeTimeSeries.Create(
  var TimesSeries: TTimesSeriesCollections);
begin
  FOldTimesSeries := TTimesSeriesCollections.Create(nil);
  FOldTimesSeries.Assign(frmGoPhast.PhastModel.TimesSeries);
  FNewTimesSeries := TimesSeries;
  TimesSeries := nil;
end;

function TUndoChangeTimeSeries.Description: string;
begin
  result := 'edit time series';
end;

destructor TUndoChangeTimeSeries.Destroy;
begin
  FOldTimesSeries.Free;
  FNewTimesSeries.Free;
  inherited;
end;

procedure TUndoChangeTimeSeries.DoCommand;
begin
  frmGoPhast.PhastModel.TimesSeries.Assign(FNewTimesSeries);
end;

procedure TUndoChangeTimeSeries.Undo;
begin
  frmGoPhast.PhastModel.TimesSeries.Assign(FOldTimesSeries);
end;

end.
