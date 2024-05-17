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
    spl1: TSplitter;
    procedure btnAddGroupClick(Sender: TObject);
    procedure btnDeleteGroupClick(Sender: TObject);
    procedure tvTimeSeriesChange(Sender: TObject; Node: TTreeNode);
    procedure FormCreate(Sender: TObject); override;
    procedure FormDestroy(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    function GroupNamesOK: Boolean;
    function SeriesNamesOK: Boolean;
    function TimesOK: Boolean;
  private
    FTimesSeries: TTimesSeriesCollections;
    FPestNames: TStringList;
    FPageCount: integer;
    procedure edGroupNameChange(Sender: TObject);
    function AddNewFrame(NewName: string): TframeModflow6TimeSeries;
    procedure AssignProperties;
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
  System.Math, frmGoPhastUnit, ModflowParameterUnit, OrderedCollectionUnit,
  System.UITypes, ModflowTimeUnit, GoPhastTypes;

resourcestring
  StrTimeSeriesGroupNa = 'Time series group names must be unique. The follow' +
  'ing group names are duplicates.';
  StrTimeSeriesNamesMu = 'Time series names must be unique. The following na' +
  'mes are duplicates.';
  StrTimeSeriesNamesMuDataSets = 'Time series names must be different from t' +
  'he names of data sets or global variables. The following names are duplic' +
  'ates.';
  StrTimeSeriesNamesMuPest = 'Time series names must be different from the n' +
  'ames of any PEST parameters. The following names are duplicates.';
  StrTheTimesInSDoN = 'The times in %s do not encompass all the times in the' +
  ' model. This can cause an error if one of its time series is used for a ti' +
  'me step in which its times are not defined. Do you want to fix this?';

{$R *.dfm}

procedure TfrmTimeSeries.btnAddGroupClick(Sender: TObject);
var
  NewName: String;
  NewFrame: TframeModflow6TimeSeries;
  NewItem: TTimeSeriesCollectionItem;
begin
  inherited;

  NewName := 'NewGroup' + IntToStr(tvTimeSeries.Items.Count + 1);
  NewFrame := AddNewFrame(NewName);
  NewItem := FTimesSeries.Add;
  NewItem.TimesSeriesCollection.GroupName := AnsiString(NewName);
  NewFrame.GetData(NewItem, FPestNames);
  tvTimeSeries.Selected.Data := NewItem.TimesSeriesCollection;
end;

procedure TfrmTimeSeries.btnDeleteGroupClick(Sender: TObject);
var
  PageIndex: Integer;
  AFrame: TframeModflow6TimeSeries;
  APage: TJvCustomPage;
  NewActivePageIndex: Integer;
  TimesSeriesCollection: TTimesSeriesCollection;
  TimeSeries: TMf6TimeSeries;
  SeriesIndex: Integer;
begin
  inherited;
  if tvTimeSeries.Selected <> nil then
  begin
    TimesSeriesCollection := tvTimeSeries.Selected.Data;
    if TimesSeriesCollection <> nil then
    begin
      TimesSeriesCollection.Deleted := True;
      for SeriesIndex := 0 to TimesSeriesCollection.Count - 1 do
      begin
        TimeSeries := TimesSeriesCollection[SeriesIndex].TimeSeries;
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
  AssignProperties;
  if not GroupNamesOK then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  if not SeriesNamesOK then
  begin
    ModalResult := mrNone;
    Exit;
  end;
  if not TimesOK then
  begin
    ModalResult := mrNone;
    Exit;
  end;
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
  ASeriesItem: TTimeSeriesCollectionItem;
  NewFrame: TframeModflow6TimeSeries;
  ParamIndex: Integer;
  SteadyParams: TModflowSteadyParameters;
  AParam: TModflowSteadyParameter;
begin
  FPageCount := 0;
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
  FTimesSeries.Assign(frmGoPhast.PhastModel.Mf6TimesSeries);
  for SeriesIndex := 0 to FTimesSeries.Count - 1 do
  begin
    ASeriesItem := FTimesSeries[SeriesIndex];
    if ASeriesItem.TimesSeriesCollection.Deleted then
    begin
      Continue;
    end;
    NewFrame := AddNewFrame(String(ASeriesItem.TimesSeriesCollection.GroupName));
    tvTimeSeries.Selected.Data := ASeriesItem.TimesSeriesCollection;
    NewFrame.GetData(ASeriesItem, FPestNames);
  end;
end;

function TfrmTimeSeries.GroupNamesOK: Boolean;
var
  GroupNameList: TStringList;
  DuplicateNames: TStringList;
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  ErrorMessage: string;
begin
  DuplicateNames := TStringList.Create;
  GroupNameList := TStringList.Create;
  try
    GroupNameList.CaseSensitive := False;
    GroupNameList.Sorted := True;
    DuplicateNames.Duplicates := dupIgnore;
    DuplicateNames.Sorted := True;
    for GroupIndex := 0 to FTimesSeries. Count - 1 do
    begin
      AGroup := FTimesSeries.Items[GroupIndex].TimesSeriesCollection;
      if AGroup.Deleted then
      begin
        Continue;
      end;
      if GroupNameList.IndexOf(String(AGroup.GroupName)) >= 0 then
      begin
        DuplicateNames.Add(String(AGroup.GroupName));
      end
      else
      begin
        GroupNameList.Add(String(AGroup.GroupName));
      end;
    end;
    result := DuplicateNames.Count = 0;
    if not result then
    begin
      ErrorMessage := StrTimeSeriesGroupNa + sLineBreak + DuplicateNames.Text;
      MessageDlg(ErrorMessage, mtError, [mbOK], 0);
    end;

  finally
    GroupNameList.Free;
    DuplicateNames.Free;
  end;
end;

procedure TfrmTimeSeries.AssignProperties;
var
  AFrame: TframeModflow6TimeSeries;
  SeriesIndex: Integer;
begin
  for SeriesIndex := 0 to plTimeSeries.PageCount - 1 do
  begin
    AFrame := plTimeSeries.Pages[SeriesIndex].Controls[0] as TframeModflow6TimeSeries;
    AFrame.SetData;
  end;
end;

function TfrmTimeSeries.SeriesNamesOK: Boolean;
var
  SeriesNameList: TStringList;
  DuplicateNames: TStringList;
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  ErrorMessage: string;
  SeriesIndex: Integer;
  ASeries: TMf6TimeSeries;
  ObserverDuplicates: TStringList;
  Observer: TObject;
  ParameterDuplicates: TStringList;
  Parameter: TObject;
begin
  DuplicateNames := TStringList.Create;
  SeriesNameList := TStringList.Create;
  ObserverDuplicates := TStringList.Create;
  ParameterDuplicates := TStringList.Create;
  try
    SeriesNameList.CaseSensitive := False;
    SeriesNameList.Sorted := True;
    DuplicateNames.Duplicates := dupIgnore;
    DuplicateNames.Sorted := True;
    ObserverDuplicates.Sorted := True;
    for GroupIndex := 0 to FTimesSeries. Count - 1 do
    begin
      AGroup := FTimesSeries.Items[GroupIndex].TimesSeriesCollection;
      if AGroup.Deleted then
      begin
        Continue;
      end;
      for SeriesIndex := 0 to AGroup.Count - 1 do
      begin
        ASeries := AGroup[SeriesIndex].TimeSeries;
        if ASeries.Deleted then
        begin
          Continue;
        end;
        if SeriesNameList.IndexOf(String(ASeries.SeriesName)) >= 0 then
        begin
          DuplicateNames.Add(String(ASeries.SeriesName));
        end
        else
        begin
          Observer := frmGoPhast.PhastModel.GetObserverByName(
            String(ASeries.SeriesName));
          if Observer <> nil then
          begin
            ObserverDuplicates.Add(String(ASeries.SeriesName));
          end
          else
          begin
            Parameter := frmGoPhast.PhastModel.GetPestParameterByName(
              string(ASeries.SeriesName));
            if Parameter <> nil then
            begin
              ParameterDuplicates.Add(String(ASeries.SeriesName));
            end
            else
            begin
              SeriesNameList.Add(String(ASeries.SeriesName));
            end;
          end;
        end;
      end;
    end;
    result := (DuplicateNames.Count = 0)
      and (ObserverDuplicates.Count = 0)
      and (ParameterDuplicates.Count = 0);
    if not result then
    begin
      if DuplicateNames.Count > 0 then
      begin
        ErrorMessage := StrTimeSeriesNamesMu + sLineBreak + DuplicateNames.Text;
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
      end;
      if ObserverDuplicates.Count > 0 then
      begin
        ErrorMessage := StrTimeSeriesNamesMuDataSets + sLineBreak
          + ObserverDuplicates.Text;
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
      end;
      if ParameterDuplicates.Count > 0 then
      begin
        ErrorMessage := StrTimeSeriesNamesMuPest + sLineBreak
          + ParameterDuplicates.Text;
        MessageDlg(ErrorMessage, mtError, [mbOK], 0);
      end;
    end;

  finally
    ParameterDuplicates.Free;
    ObserverDuplicates.Free;
    SeriesNameList.Free;
    DuplicateNames.Free;
  end;
end;

procedure TfrmTimeSeries.SetData;
begin
  frmGoPhast.UndoStack.Submit(TUndoChangeTimeSeries.Create(FTimesSeries));
end;

function TfrmTimeSeries.TimesOK: Boolean;
var
  GroupIndex: Integer;
  AGroup: TTimesSeriesCollection;
  StressPeriods: TModflowStressPeriods;
  StartTime: Double;
  EndTime: Double;
begin
  result := True;
  StressPeriods := frmGoPhast.PhastModel.ModflowStressPeriods;
  StartTime := StressPeriods.First.StartTime;
  EndTime := StressPeriods.Last.EndTime;

  for GroupIndex := 0 to FTimesSeries. Count - 1 do
  begin
    AGroup := FTimesSeries.Items[GroupIndex].TimesSeriesCollection;
    if AGroup.Deleted then
    begin
      Continue;
    end;
    result := (AGroup.Times.First.Value <= StartTime)
      and (AGroup.Times.Last.Value >= EndTime);
    if not result then
    begin
      if (MessageDlg(Format(StrTheTimesInSDoN, [AGroup.GroupName]), mtWarning,
       [mbYes, mbNo, mbCancel], 0) = mrNo) then
      begin
        result := True;
      end;
    end;
  end;

end;

function TfrmTimeSeries.AddNewFrame(NewName: string): TframeModflow6TimeSeries;
var
  NewPage: TJvCustomPage;
  NewNode: TJvPageIndexNode;
begin
  Inc(FPageCount);
  NewPage := TJvStandardPage.Create(self);
  NewPage.Name := 'APage_' + FPageCount.ToString;
  NewPage.PageList := plTimeSeries;
  NewNode := tvTimeSeries.Items.AddChild(nil, NewName) as TJvPageIndexNode;
  NewNode.PageIndex := NewPage.PageIndex;
  result := TframeModflow6TimeSeries.Create(self);
  result.Name := 'AFrame_' + FPageCount.ToString;
  result.Parent := NewPage;
  result.Align := AlClient;
  result.edGroupName.Tag := plTimeSeries.PageCount - 1;
  result.edGroupName.OnChange := edGroupNameChange;
  result.edGroupName.Text := NewName;
  result.InitializeGrid;
  plTimeSeries.ActivePage := NewPage;
  tvTimeSeries.Selected := NewNode;
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
  FOldTimesSeries.Assign(frmGoPhast.PhastModel.Mf6TimesSeries);
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
  frmGoPhast.PhastModel.Mf6TimesSeries.Assign(FNewTimesSeries);
end;

procedure TUndoChangeTimeSeries.Undo;
begin
  frmGoPhast.PhastModel.Mf6TimesSeries.Assign(FOldTimesSeries);
end;

end.
