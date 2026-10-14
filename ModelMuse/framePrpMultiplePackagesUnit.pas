unit framePrpMultiplePackagesUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, framePackageUnit, RbwController,
  Vcl.StdCtrls, Vcl.ExtCtrls, frameGridUnit, Vcl.CheckLst, JvExCheckLst,
  JvCheckListBox, Vcl.ComCtrls, JvPageList, FramePackageNodeLinkUnit,
  framePackagePrpUnit, ModflowPackageSelectionUnit, frmCustomGoPhastUnit,
  GoPhastTypes;

type
  // frame should be a @link(TframePackagePrp).
  TPrpPackagedDeletedEvent = procedure (Sender: TObject; frame: TFrame) of Object;

  TframePrpMultiplePackages = class(TframePackage)
    pcPrt: TPageControl;
    tabOptions: TTabSheet;
    grpMIP: TGroupBox;
    cbRetardationFactor: TCheckBox;
    cbUseParticleStopZones: TCheckBox;
    grpOutputControl: TGroupBox;
    lblOutputFiles: TLabel;
    lblTrackEvents: TLabel;
    chklstOutputFiles: TJvCheckListBox;
    chklstTrackEvents: TJvCheckListBox;
    tabPrpPackages: TTabSheet;
    framePrpPackages: TframeGrid;
    tabTrackTimes: TTabSheet;
    frameTrackTimes: TframeGrid;
    pnlTrackTime: TPanel;
    lblTrackTime: TLabel;
    tabTrackByStressPeriod: TTabSheet;
    pnlTrackPeriodData: TPanel;
    lblTrackPeriodData: TLabel;
    frameReleasePeriodData: TframeGrid;
    cbRunAsSeparateSimulation: TCheckBox;
    gbSimulation: TGroupBox;
    cbScratchBuffer: TCheckBox;
    procedure framePrpPackagesGridBeforeDrawCell(Sender: TObject; ACol, ARow:
        LongInt);
    procedure framePrpPackagesGridEndUpdate(Sender: TObject);
    procedure framePrpPackagesGridSelectCell(Sender: TObject; ACol, ARow: LongInt;
        var CanSelect: Boolean);
    procedure framePrpPackagesGridSetEditText(Sender: TObject; ACol, ARow: LongInt;
        const Value: string);
    procedure framePrpPackagesGridStateChange(Sender: TObject; ACol, ARow: LongInt;
        const Value: TCheckBoxState);
    procedure framePrpPackagesseNumberChange(Sender: TObject);
    procedure frameReleasePeriodDataGridEndUpdate(Sender: TObject);
    procedure frameReleasePeriodDataGridEnter(Sender: TObject);
    procedure frameReleasePeriodDataGridExit(Sender: TObject);
    procedure frameTrackTimesGridEndUpdate(Sender: TObject);
    procedure frameTrackTimesGridExit(Sender: TObject);
  private
    FOnPrpPackageDeleted: TPrpPackagedDeletedEvent;
    FOnPrpPackageAdded: TNotifyEvent;
    FPackageTreeView: TTreeView;
    FNode: TTreeNode;
    FPageList: TJvPageList;
    FLinkDictionary: TLinkDictionary;
    FFrameNodeLinks: TLinkObjectList;
    FMemoWidthDelta: Integer;
    FInitializing: Boolean;
    FGettingData: Boolean;
    function CreateNewPrpFrame: TframePackagePrp;
    procedure SetMemoWidthDelta(const Value: Integer);
    Procedure SelectedChange(Sender: TObject);
    { Private declarations }
  public
    procedure ClearFrames;
    Procedure Initialize(PackageTreeView: TTreeView;
      Node: TTreeNode; PageList: TJvPageList; LinkDictionary: TLinkDictionary);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure GetData(PrtModel: TPrtModel); reintroduce;
    procedure SetData(PrtModel: TPrtModel); reintroduce;
    property OnPrpPackageAdded: TNotifyEvent read FOnPrpPackageAdded write FOnPrpPackageAdded;
    property OnPrpPackageDeleted: TPrpPackagedDeletedEvent read FOnPrpPackageDeleted write FOnPrpPackageDeleted;
    property MemoWidthDelta: Integer read FMemoWidthDelta write SetMemoWidthDelta;
    procedure ResizeFrame(Sender: TObject);
    { Public declarations }
  end;

var
  framePrpMultiplePackages: TframePrpMultiplePackages;

implementation

{$R *.dfm}

uses frmGoPhastUnit;

resourcestring
  SPrintOrSave = 'Print or Save';
  SParticleReleasePointPRPPackage = 'Particle Release Point (PRP) Package';
  StrPackageName = 'Package Name';
  StrPackageUsed = 'Package Used';
  StrAllSteps = 'All Steps';
  StrFirstSteps = 'First Step';
  StrLastSteps = 'Last Step';
  StrStepFrequency = 'Step frequency';
  StrSteps = 'Steps';
  StrTrackingTime = 'Tracking Time';

type
  TPrpColumns = (pcName, pcUsed);

  TPackagePeriodDataColumns = (ppdcStartTime, ppdcEndTime, ppdcPrintSave, ppdcAll, ppdcFirst, ppdcLast,
    ppdcFrequency, ppdcSteps);


{ TframePrpMultiplePackages }

procedure TframePrpMultiplePackages.ClearFrames;
var
  ALink: TFrameNodeLink;
  Page: TJvStandardPage;
begin
  for var Index := 0 to FFrameNodeLinks.Count - 1 do
  begin
     ALink := FFrameNodeLinks[Index];
     FLinkDictionary.Remove(ALink.Frame);
     Page := ALink.Frame.Parent as TJvStandardPage;
     Page.Free;
     ALink.Node.Free;
  end;
  FFrameNodeLinks.Clear;
end;

constructor TframePrpMultiplePackages.Create(AOwner: TComponent);
begin
  inherited;
  FFrameNodeLinks := TLinkObjectList.Create;
  OnSelectedChange := SelectedChange;
end;

function TframePrpMultiplePackages.CreateNewPrpFrame: TframePackagePrp;
var
  NewPage: TJvStandardPage;
  ChildNode: TTreeNode;
  Link: TFrameNodeLink;
begin
  NewPage := TJvStandardPage.Create(nil);
  Result := TframePackagePrp.Create(NewPage);
  NewPage.Name := '';
  NewPage.HelpKeyword := 'PRP-Particle-Release-Point-Pac';
  NewPage.PageList := FPageList;
//  NewPage.OnShow := ShowImsPage;
  result.MemoWidthDelta := result.Width - result.MemoComments.Width;
  result.Parent := NewPage;
  result.Align := alClient;
  Result.lblPackage.Caption := SParticleReleasePointPRPPackage;
  Result.InitializeGrids;

  ChildNode := FPackageTreeView.Items.AddChild(FNode, SParticleReleasePointPRPPackage);

  ChildNode.Data := NewPage;

  Link := TFrameNodeLink.Create;
  Link.Frame := Result;
  Link.Node := ChildNode;
  Link.AlternateNode := ChildNode;
  FFrameNodeLinks.Add(Link);
  FLinkDictionary.Add(Link.Frame, Link);
end;

destructor TframePrpMultiplePackages.Destroy;
begin
  ClearFrames;
  FFrameNodeLinks.Free;
  inherited;
end;

procedure TframePrpMultiplePackages.framePrpPackagesGridBeforeDrawCell(Sender:
    TObject; ACol, ARow: LongInt);
var
  AName: string;
begin
  inherited;
  if framePrpPackages.seNumber.AsInteger <= 0 then
  begin
    framePrpPackages.Grid.Canvas.Brush.Color := clBtnFace;
    Exit;
  end;
  if (ACol = Ord(pcName)) and (ARow >= 1) then
  begin
    AName := framePrpPackages.Grid.Cells[ACol, ARow];
    for var RowIndex := ARow - 1 downto 1 do
    begin
      if SameText(AName,framePrpPackages.Grid.Cells[ACol, RowIndex]) then
      begin
        framePrpPackages.Grid.Canvas.Brush.Color := clRed;
        Exit;
      end;
    end;
  end;
end;

procedure TframePrpMultiplePackages.framePrpPackagesGridEndUpdate(Sender:
    TObject);
begin
  inherited;
  if FInitializing or FGettingData or (ComponentState <> []) then
  begin
    Exit;
  end;

  framePrpPackages.GridEndUpdate(Sender);
end;

procedure TframePrpMultiplePackages.framePrpPackagesGridSelectCell(Sender:
    TObject; ACol, ARow: LongInt; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := framePrpPackages.seNumber.AsInteger >= 1;
end;

procedure TframePrpMultiplePackages.framePrpPackagesGridSetEditText(Sender:
    TObject; ACol, ARow: LongInt; const Value: string);
var
  Frame: TframePackagePrp;
  ALink: TFrameNodeLink;
begin
  if (ACol = Ord(pcName)) and (ARow >= 1) then
  begin
    if framePrpPackages.Grid.Objects[Ord(pcUsed), ARow] <> nil then
    begin
      Frame := framePrpPackages.Grid.Objects[Ord(pcUsed), ARow] as TframePackagePrp;
      if FLinkDictionary.TryGetValue(Frame, ALink) then
      begin
        ALink.Node.Text := Value + ': (Particle Release Point (PRP) Package)';
      end;
    end;
  end;
end;

procedure TframePrpMultiplePackages.framePrpPackagesGridStateChange(Sender:
    TObject; ACol, ARow: LongInt; const Value: TCheckBoxState);
var
  PrpFrame: TframePackagePrp;
begin
  inherited;
  PrpFrame := framePrpPackages.Grid.Objects[Ord(pcUsed), ARow] as TframePackagePrp;
  if PrpFrame <> nil then
  begin
    PrpFrame.Selected := Selected and framePrpPackages.Grid.Checked[ACol, ARow];
  end;
end;

procedure TframePrpMultiplePackages.framePrpPackagesseNumberChange(Sender:
    TObject);
var
  NewFrame: TframePackagePrp;
begin
  framePrpPackages.seNumberChange(Sender);
  if FGettingData or (ControlState <> []) then
  begin
    Exit;
  end;

  for var Index := 0 to framePrpPackages.seNumber.AsInteger - 1 do
  begin
    if framePrpPackages.Grid.Objects[Ord(pcUsed), Index+1] = nil then
    begin
      NewFrame := CreateNewPrpFrame;
      framePrpPackages.Grid.Objects[Ord(pcUsed), Index+1] := NewFrame;

      FNode.Expanded := True;
      Assert(FPackageTreeView <> nil);
      Assert(Assigned(FPackageTreeView.OnChange));
      FPackageTreeView.OnChange(FPackageTreeView, FNode);

      framePrpPackages.Grid.Cells[Ord(pcName), Index+1] := Format('PRP%d', [Index+1]);
      framePrpPackages.Grid.Checked[Ord(pcUsed), Index+1] := True;

      if Assigned(framePrpPackages.Grid.OnSetEditText) then
      begin
        framePrpPackages.Grid.OnSetEditText(nil, Ord(pcName), Index+1,
          framePrpPackages.Grid.Cells[Ord(pcName), Index+1]);
      end;
    end;
  end;
end;

procedure TframePrpMultiplePackages.frameReleasePeriodDataGridEndUpdate(Sender:
    TObject);
begin
  inherited;
  if FInitializing or FGettingData or (ControlState <> []) then
  begin
    Exit;
  end;

  frameReleasePeriodData.GridEndUpdate(Sender);
end;

procedure TframePrpMultiplePackages.frameReleasePeriodDataGridEnter(Sender:
    TObject);
begin
  inherited;
  frameReleasePeriodData.Grid.BeginUpdate;
end;

procedure TframePrpMultiplePackages.frameReleasePeriodDataGridExit(Sender:
    TObject);
begin
  inherited;
  frameReleasePeriodData.Grid.EndUpdate;
end;

procedure TframePrpMultiplePackages.frameTrackTimesGridEndUpdate(Sender:
    TObject);
begin
  inherited;
  if FInitializing or FGettingData or (ComponentState <> []) then
  begin
    Exit;
  end;

  frameTrackTimes.GridEndUpdate(Sender);
end;

procedure TframePrpMultiplePackages.frameTrackTimesGridExit(Sender: TObject);
begin
  inherited;
  if (frameTrackTimes.Grid.RowCount > 2) or (frameTrackTimes.Grid.Cells[0,1] <> '') then
  begin
    frameTrackTimes.seNumber.AsInteger := frameTrackTimes.Grid.RowCount -1;
  end;
end;

procedure TframePrpMultiplePackages.GetData(PrtModel: TPrtModel);
var
  FileItem: TPrtOutputFile;
  TrackingOption: TPrtTrackingOption;
  PrpPackage: TPrpPackage;
  PeriodData: TPrpPeriodDataItem;
  StepList: TStringList;
  NewPrpFrame: TframePackagePrp;
begin
  FGettingData := True;
  try
    cbRetardationFactor.Checked := PrtModel.RetardationFactorUsed;
    cbUseParticleStopZones.Checked := PrtModel.ZoneUsed;
    cbRunAsSeparateSimulation.Checked := PrtModel.RunAsSeparateSimulation;
    cbScratchBuffer.Checked := PrtModel.ScratchBuffer;

    chklstOutputFiles.Checked[Ord(pofBinaryTrack)] := pofBinaryTrack in PrtModel.PrtOutputFiles;
    chklstOutputFiles.Checked[Ord(pofCsvTrack)] := pofCsvTrack in PrtModel.PrtOutputFiles;
    chklstOutputFiles.Checked[Ord(pofBinaryBudget)] := pofBinaryBudget in PrtModel.PrtOutputFiles;
    chklstOutputFiles.Checked[Ord(pofoCsvBudget)] := pofoCsvBudget in PrtModel.PrtOutputFiles;

    for TrackingOption in PrtModel.PrtTrackingOptions do
    begin
      chklstTrackEvents.Checked[Ord(TrackingOption)] := True;
    end;

    framePrpPackages.Grid.BeginUpdate;
    try
      framePrpPackages.seNumber.AsInteger := PrtModel.Count;
      for var Index := 0 to PrtModel.Count - 1 do
      begin
        PrpPackage := PrtModel[Index].PrpPackage;
        framePrpPackages.Grid.Cells[Ord(pcName), Index+1] := PrpPackage.PackageName;
        framePrpPackages.Grid.Checked[Ord(pcUsed), Index+1] := PrpPackage.isSelected;
        framePrpPackages.Grid.Objects[Ord(pcName), Index+1] := PrpPackage;

        NewPrpFrame := CreateNewPrpFrame;
        NewPrpFrame.GetData(PrpPackage);
        framePrpPackages.Grid.Objects[Ord(pcUsed), Index+1] := NewPrpFrame;
        framePrpPackagesGridSetEditText(framePrpPackages.Grid, Ord(pcName), Index+1, PrpPackage.PackageName);
      end;
    finally
      framePrpPackages.Grid.EndUpdate;
    end;

    frameTrackTimes.Grid.BeginUpdate;
    try
      frameTrackTimes.seNumber.AsInteger := PrtModel.TrackTimes.Count;
      for var Index := 0 to PrtModel.TrackTimes.Count - 1 do
      begin
        frameTrackTimes.Grid.RealValue[0, Index+1] := PrtModel.TrackTimes[Index].Value;
      end;
    finally
      frameTrackTimes.Grid.EndUpdate;
    end;

    frameReleasePeriodData.Grid.BeginUpdate;
    try
      frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes(frameReleasePeriodData.Grid, Ord(ppdcStartTime));
      frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes(frameReleasePeriodData.Grid, Ord(ppdcEndTime));

      frameReleasePeriodData.seNumber.AsInteger := PrtModel.PeriodData.Count;
      for var Index := 0 to PrtModel.PeriodData.Count - 1 do
      begin
        PeriodData := PrtModel.PeriodData[Index];
        frameReleasePeriodData.Grid.RealValue[Ord(ppdcStartTime), Index+1] := PeriodData.StartTime;
        frameReleasePeriodData.Grid.RealValue[Ord(ppdcEndTime), Index+1] := PeriodData.EndTime;
        frameReleasePeriodData.Grid.ItemIndex[Ord(ppdcPrintSave), Index+1] := Ord(PeriodData.OCMethod);
        frameReleasePeriodData.Grid.Checked[Ord(ppdcAll), Index+1] := PeriodData.All;
        frameReleasePeriodData.Grid.Checked[Ord(ppdcFirst), Index+1] := PeriodData.First;
        frameReleasePeriodData.Grid.Checked[Ord(ppdcLast), Index+1] := PeriodData.Last;
        frameReleasePeriodData.Grid.IntegerValue[Ord(ppdcFrequency), Index+1] := PeriodData.Frequency;
        StepList := TStringList.Create;
        try
          for var StepIndex := 0 to PeriodData.Steps.Count - 1 do
          begin
            StepList.Add(PeriodData.Steps[StepIndex].Value.ToString);
          end;
          frameReleasePeriodData.Grid.Cells[Ord(ppdcSteps), Index+1] := StepList.commaText;
        finally
          StepList.Free;
        end;
      end;
    finally
      frameReleasePeriodData.Grid.EndUpdate;
    end;
  finally
    FGettingData := False;
  end;
  framePrpPackagesseNumberChange(self);
  SelectedChange(self);
end;

procedure TframePrpMultiplePackages.Initialize(PackageTreeView: TTreeView;
      Node: TTreeNode; PageList: TJvPageList; LinkDictionary: TLinkDictionary);
begin
  FInitializing := True;
  try
    FPackageTreeView := PackageTreeView;
    FNode := Node;
    FPageList := PageList;
    FLinkDictionary := LinkDictionary;

    pcPrt.ActivePageIndex := 0;

    framePrpPackages.Grid.BeginUpdate;
    try
      ClearGrid(framePrpPackages.Grid);
      framePrpPackages.Grid.Cells[Ord(pcName ), 0] := StrPackageName;
      framePrpPackages.Grid.Cells[Ord(pcUsed ), 0] := StrPackageUsed;
      framePrpPackages.seNumber.AsInteger := 0;
    finally
      framePrpPackages.Grid.EndUpdate;
    end;

    frameReleasePeriodData.Grid.BeginUpdate;
    try
      ClearGrid(frameReleasePeriodData.Grid);
      frameReleasePeriodData.Grid.Cells[Ord(ppdcStartTime), 0] := StrStartingTime;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcEndTime), 0] := StrEndingTime;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcPrintSave), 0] := SPrintOrSave;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcAll), 0] := StrAllSteps;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcFirst), 0] := StrFirstSteps;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcLast), 0] := StrLastSteps;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcFrequency), 0] := StrStepFrequency;
      frameReleasePeriodData.Grid.Cells[Ord(ppdcSteps), 0] := StrSteps;
    finally
      frameReleasePeriodData.Grid.EndUpdate;
    end;

    frameTrackTimes.Grid.BeginUpdate;
    try
      ClearGrid(frameTrackTimes.Grid);
      frameTrackTimes.Grid.Cells[0,0] := StrTrackingTime
    finally
      frameTrackTimes.Grid.EndUpdate;
    end;

    chklstOutputFiles.UnCheckAll;
    chklstTrackEvents.UnCheckAll;

    chklstOutputFiles.Checked[Ord(pofBinaryTrack)] := True;

    ClearFrames;

  finally
    FInitializing := False;
  end;
end;

procedure TframePrpMultiplePackages.ResizeFrame(Sender: TObject);
begin
  MemoComments.Width := Width - FMemoWidthDelta;
end;

procedure TframePrpMultiplePackages.SelectedChange(Sender: TObject);
begin
  for var RowIndex := 1 to framePrpPackages.seNumber.AsInteger do
  begin
    framePrpPackagesGridStateChange(framePrpPackages.Grid, Ord(pcUsed), RowIndex,
      framePrpPackages.Grid.CheckState[Ord(pcUsed), RowIndex]);
  end;
end;

procedure TframePrpMultiplePackages.SetData(PrtModel: TPrtModel);
var
  PeriodData: TPrpPeriodDataItem;
  StepList: TStringList;
  PrtOutputFiles: TPrtOutputFiles;
  StartTime: Extended;
  EndTime: Extended;
  PeriodIndex: Integer;
  Steps: TIntegerCollection;
  AnInt: Integer;
  StepCount: Integer;
  PrtTrackingOptions: TPrtTrackingOptions;
  PackageColumn: TStrings;
  PrpPackage: TPrpPackage;
  PrpFrame: TframePackagePrp;
  PackageItem: TPrpPackageItem;
  PackageFound: Boolean;
  AnObject: TObject;
  StoredPackage: TPrpPackage;
  AnInteger: Integer;
begin
  PrtModel.RetardationFactorUsed := cbRetardationFactor.Checked;
  PrtModel.ZoneUsed := cbUseParticleStopZones.Checked;
  PrtModel.RunAsSeparateSimulation := cbRunAsSeparateSimulation.Checked;
  PrtModel.ScratchBuffer := cbScratchBuffer.Checked;

  PrtOutputFiles := [];
  if chklstOutputFiles.Checked[Ord(pofBinaryTrack)] then
  begin
    Include(PrtOutputFiles, pofBinaryTrack);
  end;
  if chklstOutputFiles.Checked[Ord(pofCsvTrack)] then
  begin
    Include(PrtOutputFiles, pofCsvTrack);
  end;
  if chklstOutputFiles.Checked[Ord(pofBinaryBudget)] then
  begin
    Include(PrtOutputFiles, pofBinaryBudget);
  end;
  if chklstOutputFiles.Checked[Ord(pofoCsvBudget)] then
  begin
    Include(PrtOutputFiles, pofoCsvBudget);
  end;


  PrtTrackingOptions := [];
  for var Index := 0 to chklstTrackEvents.Items.Count -1 do
  begin
    if chklstTrackEvents.Checked[Index] then
    begin
      Include(PrtTrackingOptions, TPrtTrackingOption(Index));
    end;
  end;
  PrtModel.PrtTrackingOptions := PrtTrackingOptions;

  PackageColumn := framePrpPackages.Grid.Cols[Ord(pcName)];
  for var Index := PrtModel.Count - 1 downto 0 do
  begin
    PrpPackage := PrtModel[Index].PrpPackage;
    PackageFound := False;
    for var PackageIndex := 1 to PackageColumn.Count - 1 do
    begin
      AnObject := PackageColumn.Objects[PackageIndex];
      if AnObject <> nil then
      begin
        StoredPackage := AnObject as TPrpPackage;
        if PrpPackage.OriginalId = StoredPackage.OriginalId then
        begin
          PackageFound := True;
          PackageColumn.Objects[PackageIndex] := PrpPackage;
          Break;
        end;
      end;
    end;
    if not PackageFound then
    begin
      PrtModel.Delete(Index);
    end;
  end;
  for var Index := 0 to framePrpPackages.seNumber.AsInteger - 1 do
  begin
    PrpPackage := framePrpPackages.Grid.Objects[Ord(pcName), Index + 1] as TPrpPackage;
    If PrpPackage = nil then
    begin
      PackageItem := PrtModel.Add as TPrpPackageItem;
      PrpPackage := PackageItem.PrpPackage;
      PackageItem.Index := Index;
    end;
    PrpFrame := framePrpPackages.Grid.Objects[Ord(pcUsed), Index + 1] as TframePackagePrp;
    Assert(PrpFrame <> nil);
    PrpFrame.SetData(PrpPackage);
    PrpPackage.PackageName := framePrpPackages.Grid.Cells[Ord(pcName), Index + 1];
    PrpPackage.IsSelected := framePrpPackages.Grid.Checked[Ord(pcUsed), Index + 1];
  end;

  PrtModel.TrackTimes.Count := frameTrackTimes.seNumber.AsInteger;
  for var Index := 0 to frameTrackTimes.seNumber.AsInteger - 1 do
  begin
    PrtModel.TrackTimes[Index].Value := frameTrackTimes.Grid.RealValueDefault[0, Index+1, 0];
  end;

  PeriodIndex := 0;
  if PrtModel.PeriodData.Count > frameReleasePeriodData.seNumber.AsInteger then
  begin
    PrtModel.PeriodData.Count := frameReleasePeriodData.seNumber.AsInteger;
  end;
  for var Index := 0 to frameReleasePeriodData.seNumber.AsInteger  - 1 do
  begin
    if TryStrToFloat(frameReleasePeriodData.Grid.Cells[Ord(ppdcStartTime), Index+1], StartTime)
      and TryStrToFloat(frameReleasePeriodData.Grid.Cells[Ord(ppdcEndTime), Index+1], EndTime) then
    begin
      if PeriodIndex < PrtModel.PeriodData.Count then
      begin
        PeriodData := PrtModel.PeriodData[PeriodIndex];
      end
      else
      begin
        PeriodData := PrtModel.PeriodData.Add;
      end;
      PeriodData.StartTime := StartTime;
      PeriodData.EndTime := EndTime;

      if frameReleasePeriodData.Grid.ItemIndex[Ord(ppdcPrintSave), Index+1] >= 0 then
      begin
        PeriodData.OCMethod := TPrtOCMethod(frameReleasePeriodData.Grid.ItemIndex[Ord(ppdcPrintSave), Index+1]);
        case PeriodData.OCMethod of
          pomBoth:
            begin
              Include(PrtOutputFiles, pofBinaryBudget);
              Include(PrtOutputFiles, pofoCsvBudget);
            end;
          pomPrint:
            begin
              Include(PrtOutputFiles, pofoCsvBudget);
            end;
          pomSave:
            begin
              Include(PrtOutputFiles, pofBinaryBudget);
            end;
          else Assert(False)
        end;
      end;

      PeriodData.All := frameReleasePeriodData.Grid.Checked[Ord(ppdcAll), Index+1];
      PeriodData.First := frameReleasePeriodData.Grid.Checked[Ord(ppdcFirst), Index+1];
      PeriodData.Last := frameReleasePeriodData.Grid.Checked[Ord(ppdcLast), Index+1];
      if TryStrToInt(frameReleasePeriodData.Grid.Cells[Ord(ppdcFrequency), Index+1], AnInteger) then
      begin
        PeriodData.Frequency := AnInteger;
      end;
      StepList := TStringList.Create;
      try
        StepList.commaText := frameReleasePeriodData.Grid.Cells[Ord(ppdcSteps), Index+1];
        Steps := PeriodData.Steps;
        StepCount := 0;
        Steps.Capacity := StepList.Count;
        for var StepIndex := 0 to StepList.Count - 1 do
        begin
          if TryStrToInt(StepList[StepIndex], AnInt) then
          begin
            if StepCount < Steps.Count then
            begin
               Steps[StepCount].Value := AnInt;
            end
            else
            begin
              Steps.Add.Value := AnInt;
            end;
            Inc(StepCount);
          end;
        end;
        Steps.Count := StepCount;
      finally
        StepList.Free;
      end;
      Inc(PeriodIndex);
    end;
  end;
  PrtModel.PrtOutputFiles := PrtOutputFiles;
end;

procedure TframePrpMultiplePackages.SetMemoWidthDelta(const Value: Integer);
begin
  FMemoWidthDelta := Value;
end;

end.
