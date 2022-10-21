unit frameScreenObjectMvrUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, Vcl.ComCtrls, frameGridUnit,
  ModflowMvrUnit;

type
  TReceiverColumn = (rcPackage, rcSfrChoice, rcLakeOutlet, rcObject);

  TGetSourcesEvent = procedure (Sender: TObject;
      var PotentialSources: TSourcePackageChoices) of object;

  TframeScreenObjectMvr = class(TframeScreenObjectNoParam)
    comboSourcePackage: TComboBox;
    lblSourcePackage: TLabel;
    pcMain: TPageControl;
    tabTime: TTabSheet;
    tabConnections: TTabSheet;
    frameReceivers: TframeGrid;
    lblMvrType: TLabel;
    comboMvrType: TComboBox;
    procedure comboSourcePackageChange(Sender: TObject);
    procedure rdgModflowBoundarySetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure seNumberOfTimesChange(Sender: TObject);
    procedure frameReceiversGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure frameReceiversseNumberChange(Sender: TObject);
    procedure frameReceiversGridSetEditText(Sender: TObject; ACol,
      ARow: Integer; const Value: string);
    procedure frameReceiverssbDeleteClick(Sender: TObject);
    procedure frameReceiverssbInsertClick(Sender: TObject);
    procedure rdeFormulaChange(Sender: TObject);
    procedure rdgModflowBoundaryMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure comboMvrTypeChange(Sender: TObject);
    procedure frameReceiversGridBeforeDrawCell(Sender: TObject; ACol,
      ARow: Integer);
    procedure comboSourcePackageDropDown(Sender: TObject);
    procedure rdgModflowBoundarySelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
  private
    FLakObjects: TStringList;
    FMawObjects: TStringList;
    FSfrObjects: TStringList;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FOnGetSources: TGetSourcesEvent;
    FUzfObjects: TStringList;
//    FMoverChoices: TStringList;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetChanging(const Value: Boolean);
    procedure InitializeGrids;
    function GetLakObjects: TStringList;
    function GetMawObjects: TStringList;
    function GetSfrObjects: TStringList;
    function GetUzfObjects: TStringList;
    { Private declarations }
  protected
    procedure LayoutMultiRowEditControls; override;
  public
//    constructor Create(AOwner: TComponent); override;
//    destructor Destroy; override;
    property LakObjects: TStringList read GetLakObjects;
    property MawObjects: TStringList read GetMawObjects;
    property SfrObjects: TStringList read GetSfrObjects;
    property UzfObjects: TStringList read GetUzfObjects;
    property Changing: Boolean read FChanging write SetChanging;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnGetSources: TGetSourcesEvent read FOnGetSources write FOnGetSources;
    { Public declarations }
  end;

  TMvrTimeColumns = (mtcStartTime, mtcEndTime, mtcValue, mtcType);

var
  frameScreenObjectMvr: TframeScreenObjectMvr;

implementation

{$R *.dfm}

uses frmGoPhastUnit, ScreenObjectUnit, GoPhastTypes,
  System.Math, frmCustomGoPhastUnit, frmErrorsAndWarningsUnit,
  ModflowLakMf6Unit;

resourcestring
  StrValue = ' Value';
  StrMoverType = ' Mover Type';
  StrSourceLakeOutlet = 'Source Lake Outlet';
  StrReceiverPackage = 'Receiver Package';
  StrSFRReceiverReach = 'SFR Receiver Reach';
  StrReceiverObject = 'Receiver Object';
  StrReceiver1 = 'Receiver 1';
  StrReceiver = 'Receiver ';
  StrInvalidLakeSource = 'Invalid Lake source in MVR Package';
  StrInvalidLakeOutlet = 'Invalid Lake outlet in MVR Package';
  StrInvalidLakeSourceExplanation = 'The object "%s" defines a Lake MVR sour' +
  'ce but the object does not define a lake.';
  StrInvalidLakeOutletExplanation = 'The object "%s" defines a Lake MVR sour' +
  'ce but the lake outlet number is invalid.';

{ TframeScreenObjectMvr }

procedure TframeScreenObjectMvr.Changed;
begin
  if Assigned(OnChange) then
  begin
    if Changing then
      Exit;
    Changing := True;
    try
      OnChange(self);
    finally
      Changing := False;
    end;
  end;
end;

procedure TframeScreenObjectMvr.comboMvrTypeChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := FLastTimeColumn+1 to rdgModflowBoundary.ColCount - 1 do
      begin
        if not Odd(ColIndex) then
        begin
          Continue;
        end;
        if rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex) then
        begin
          rdgModflowBoundary.Cells[ColIndex, RowIndex] := comboMvrType.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,ColIndex,RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMvr.comboSourcePackageChange(Sender: TObject);
var
  PotentialSources: TSourcePackageChoices;
  ASource: TSourcePackageChoice;
begin
  inherited;
  Changed;

  if Assigned(OnGetSources) then
  begin
    OnGetSources(self, PotentialSources);
    if (comboSourcePackage.ItemIndex >= 0) then
    begin
      ASource := TSourcePackageChoice(comboSourcePackage.ItemIndex);
      if ASource in PotentialSources then
      begin
        comboSourcePackage.Color := clWindow
      end
      else
      begin
        comboSourcePackage.Color := clRed
      end;
    end
    else if PotentialSources <> [] then
    begin
      comboSourcePackage.Color := clWindow
    end
    else
    begin
      comboSourcePackage.Color := clRed
    end;
  end;

  frameReceivers.Grid.Invalidate;
end;

procedure TframeScreenObjectMvr.comboSourcePackageDropDown(Sender: TObject);
begin
  inherited;
  comboSourcePackage.Color := clWindow
end;

//constructor TframeScreenObjectMvr.Create(AOwner: TComponent);
//begin
//  inherited;
//  FMoverChoices := TStringList.Create;
//  FMoverChoices.Add('Factor');
//  FMoverChoices.Add('Excess');
//  FMoverChoices.Add('Threshold');
//  FMoverChoices.Add('Up To');
//end;

//destructor TframeScreenObjectMvr.Destroy;
//begin
//  FMoverChoices.Free;
//  inherited;
//end;

procedure TframeScreenObjectMvr.frameReceiversGridBeforeDrawCell(
  Sender: TObject; ACol, ARow: Integer);
var
  ReceiverIndex: Integer;
  ReceiverPackage: TReceiverPackageChoice;
begin
  inherited;
  if (ACol = Ord(rcPackage)) and (ARow >= frameReceivers.Grid.FixedRows)
    and Assigned(OnGetSources) then
  begin
    ReceiverIndex := frameReceivers.Grid.ItemIndex[ACol, ARow];
    if ReceiverIndex >= 0 then
    begin
      ReceiverPackage := TReceiverPackageChoice(ReceiverIndex);
      case ReceiverPackage of
        rpcLak:
          begin
            if not frmGoPhast.PhastModel.LakMf6IsSelected then
            begin
              frameReceivers.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        rpcMaw:
          begin
            if not frmGoPhast.PhastModel.MawIsSelected then
            begin
              frameReceivers.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        rpcSfr:
          begin
            if not frmGoPhast.PhastModel.Sfr6IsSelected then
            begin
              frameReceivers.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        rpcUzf:
          begin
            if not frmGoPhast.PhastModel.UzfMf6IsSelected then
            begin
              frameReceivers.Grid.Canvas.Brush.Color := clRed;
            end;
          end;
        else
          Assert(False);
      end;
    end
    else
    begin
      frameReceivers.Grid.Canvas.Brush.Color := clRed;
    end;
  end;
end;

procedure TframeScreenObjectMvr.frameReceiversGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  ReceiverPkgIndex: Integer;
  ReceiverColumn: TReceiverColumn;
begin

  if ARow > 0 then
  begin
    ReceiverPkgIndex := frameReceivers.Grid.ItemIndex[Ord(rcPackage), ARow];
    if (ACol >= 0) then
    begin
      ReceiverColumn := TReceiverColumn(ACol);
      if (ReceiverPkgIndex >= 0) then
      begin
        case ReceiverColumn of
          rcPackage: ;
          rcSfrChoice:
            begin
              CanSelect := TReceiverPackageChoice(ReceiverPkgIndex) = rpcSfr;
            end;
          rcLakeOutlet:
            begin
              CanSelect := (comboSourcePackage.ItemIndex >= 0)
                and (TSourcePackageChoice(comboSourcePackage.ItemIndex) = spcLak);
            end;
          rcObject: ;
        end;
      end
      else
      begin
        CanSelect := ReceiverColumn = rcPackage;
      end;
    end;
    if frameReceivers.Grid.Drawing then
    begin
      Exit;
    end;
    if ReceiverPkgIndex >= 0 then
    begin
      case TReceiverPackageChoice(ReceiverPkgIndex) of
        rpcLak:
          begin
            frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Assign(LakObjects);
          end;
        rpcMaw:
          begin
            frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Assign(MawObjects);
          end;
        rpcSfr:
          begin
            frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Assign(SfrObjects);
          end;
        rpcUzf:
          begin
            frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Assign(UzfObjects);
          end;
        else Assert(False);
      end;
    end
    else
    begin
      frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Clear;
    end;
  end
  else
  begin
    frameReceivers.Grid.Columns[Ord(rcObject)].Picklist.Clear;
  end;
end;

procedure TframeScreenObjectMvr.frameReceiversGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Changed;
  frameReceivers.Grid.Invalidate;
end;

procedure TframeScreenObjectMvr.frameReceiverssbDeleteClick(Sender: TObject);
var
  ColToDelete: Integer;
begin
  inherited;
  if frameReceivers.Grid.SelectedRow >= frameReceivers.Grid.FixedRows  then
  begin
    ColToDelete := frameReceivers.Grid.SelectedRow * 2;
    rdgModflowBoundary.DeleteColumn(ColToDelete);
    rdgModflowBoundary.DeleteColumn(ColToDelete);
  end;
  frameReceivers.sbDeleteClick(Sender);

end;

procedure TframeScreenObjectMvr.frameReceiverssbInsertClick(Sender: TObject);
var
  ColToInsert: Integer;
begin
  inherited;
  if frameReceivers.Grid.SelectedRow >= frameReceivers.Grid.FixedRows  then
  begin
    ColToInsert := frameReceivers.Grid.SelectedRow * 2;
    rdgModflowBoundary.InsertColumn(ColToInsert);
    rdgModflowBoundary.InsertColumn(ColToInsert);
  end;
  frameReceivers.sbInsertClick(Sender);

end;

procedure TframeScreenObjectMvr.frameReceiversseNumberChange(Sender: TObject);
var
  ReceiverIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  rdgModflowBoundary.BeginUpdate;
  try
    if frameReceivers.seNumber.AsInteger <= 0 then
    begin
      frameReceivers.seNumber.AsInteger:= 1;
    end;
    frameReceivers.seNumberChange(Sender);
    rdgModflowBoundary.ColCount := 2 * (frameReceivers.seNumber.AsInteger) + 2;

    for ReceiverIndex := 1 to frameReceivers.seNumber.AsInteger do
    begin
      ColIndex := ReceiverIndex* 2;
      rdgModflowBoundary.Cells[ColIndex, 0] :=
        StrReceiver + ReceiverIndex.ToString + StrValue;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonUsed := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonWidth := 35;
      rdgModflowBoundary.Columns[ColIndex].ButtonCaption := 'F()';
      Inc(ColIndex);
      rdgModflowBoundary.Cells[ColIndex, 0] :=
        StrReceiver + ReceiverIndex.ToString + StrMoverType;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
      rdgModflowBoundary.Columns[ColIndex].PickList.Assign(comboMvrType.Items);
      rdgModflowBoundary.Columns[ColIndex].ComboUsed := True;
      rdgModflowBoundary.Columns[ColIndex].LimitToList := True;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
end;

procedure TframeScreenObjectMvr.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ScreenObjectIndex: Integer;
  FoundFirst: Boolean;
  AScreenObject: TScreenObject;
  TimeIndex: Integer;
  MvrItem: TMvrItem;
  ModflowMvr: TMvrBoundary;
  FirstModflowMvr: TMvrBoundary;
  Receivers: TReceiverCollection;
  ReceiverIndex: Integer;
  ReceiverItem: TReceiverItem;
  FirstReceivers: TReceiverCollection;
  AReceiverItem: TIndividualMvrItem;
  ColIndex: Integer;
begin
  rdgModflowBoundary.Handle;
  seNumberOfTimes.Handle;
  pcMain.ActivePageIndex := 0;
  InitializeGrids;
//  FMoverChoices.Assign(rdgModflowBoundary.Columns[3].PickList);

  MoveGridToTabSheet(tabTime);
//  pnlGrid.Parent := tabTime;
//  pnlBottom.Parent := tabTime;
//  pnlGrid.Align := alClient;

  FLakObjects := nil;
  FMawObjects := nil;
  FSfrObjects := nil;
  FUzfObjects := nil;
  FirstReceivers := nil;

  Changing := True;
  try
    comboSourcePackage.ItemIndex := -1;

    FoundFirst := False;
    FirstModflowMvr := nil;
    for ScreenObjectIndex := 0 to ScreenObjectList.Count - 1 do
    begin
      AScreenObject := ScreenObjectList[ScreenObjectIndex].ScreenObject;
      if (AScreenObject.ModflowMvr <> nil) and AScreenObject.ModflowMvr.Used then
      begin
        ModflowMvr := AScreenObject.ModflowMvr;
        if not FoundFirst then
        begin
          FoundFirst := True;
          FirstModflowMvr := ModflowMvr;
          comboSourcePackage.ItemIndex :=
            Ord(ModflowMvr.SourcePackageChoice);
          Receivers := ModflowMvr.Receivers;
          frameReceivers.seNumber.AsInteger := Receivers.Count;
          for ReceiverIndex := 0 to Receivers.Count - 1 do
          begin
            ReceiverItem := Receivers[ReceiverIndex];
            frameReceivers.Grid.ItemIndex[Ord(rcPackage), ReceiverIndex+1]
              := Ord(ReceiverItem.ReceiverPackage);
            frameReceivers.Grid.IntegerValue[Ord(rcLakeOutlet), ReceiverIndex+1]
             := ReceiverItem.LakeOutlet;
            frameReceivers.Grid.ItemIndex[Ord(rcSfrChoice), ReceiverIndex+1]
              := Ord(ReceiverItem.SfrReceiverChoice);
            frameReceivers.Grid.Cells[Ord(rcObject), ReceiverIndex+1]
              := ReceiverItem.ReceiverObjectName;
          end;
          FirstReceivers := Receivers;

          seNumberOfTimes.AsInteger := ModflowMvr.Values.Count;

          for TimeIndex := 0 to ModflowMvr.Values.Count - 1 do
          begin
            MvrItem := ModflowMvr.Values[TimeIndex] as TMvrItem;
            rdgModflowBoundary.RealValue[Ord(mtcStartTime), TimeIndex+1+PestRowOffset] :=
               MvrItem.StartTime;
            rdgModflowBoundary.RealValue[Ord(mtcEndTime), TimeIndex+1+PestRowOffset] :=
              MvrItem.EndTime;

            for ReceiverIndex := 0 to MvrItem.Items.Count - 1 do
            begin
              AReceiverItem := MvrItem.Items[ReceiverIndex];
              ColIndex := ReceiverIndex*2+2;
              rdgModflowBoundary.Cells[ColIndex, TimeIndex+1+PestRowOffset] :=
                AReceiverItem.Value;
              rdgModflowBoundary.ItemIndex[ColIndex+1, TimeIndex+1+PestRowOffset] :=
                Ord(AReceiverItem.MvrType);
            end;
          end;
        end
        else
        begin
          Receivers := ModflowMvr.Receivers;
          if not Receivers.IsSame(FirstReceivers) then
          begin
            ClearGrid(frameReceivers.Grid);
            if (Receivers.Count <> FirstReceivers.Count) then
            begin
              ClearGrid(rdgModflowBoundary);
            end;
          end;

          if ModflowMvr.SourcePackageChoice <> FirstModflowMvr.SourcePackageChoice then
          begin
            comboSourcePackage.ItemIndex := -1;
          end;

          if not ModflowMvr.Values.IsSame(FirstModflowMvr.Values) then
          begin
            ClearGrid(rdgModflowBoundary);
          end;
        end;
      end;
    end;
  finally
    Changing := False;
  end;
  frameReceivers.Grid.HideEditor;
end;

function TframeScreenObjectMvr.GetLakObjects: TStringList;
begin
  if FLakObjects = nil then
  begin
    FLakObjects := frmGoPhast.PhastModel.LakScreenObjects;
  end;
  result := FLakObjects;
end;

function TframeScreenObjectMvr.GetMawObjects: TStringList;
begin
  if FMawObjects = nil then
  begin
    FMawObjects := frmGoPhast.PhastModel.MawScreenObjects;
  end;
  result := FMawObjects
end;

function TframeScreenObjectMvr.GetSfrObjects: TStringList;
begin
  if FSfrObjects = nil then
  begin
    FSfrObjects := frmGoPhast.PhastModel.SfrMf6ScreenObjects;
  end;
  result := FSfrObjects;
end;

function TframeScreenObjectMvr.GetUzfObjects: TStringList;
begin
  if FUzfObjects = nil then
  begin
    FUzfObjects := frmGoPhast.PhastModel.UzfMf6ScreenObjects;
  end;
  result := FUzfObjects;
end;

procedure TframeScreenObjectMvr.InitializeGrids;
begin
  Changing := True;

  frameReceivers.Grid.BeginUpdate;
  try
    ClearGrid(frameReceivers.Grid);
    frameReceivers.seNumber.AsInteger := 1;

    frameReceivers.Grid.Cells[Ord(rcLakeOutlet), 0] := StrSourceLakeOutlet;
    frameReceivers.Grid.Cells[Ord(rcPackage), 0] := StrReceiverPackage;
    frameReceivers.Grid.Cells[Ord(rcSfrChoice), 0] := StrSFRReceiverReach;
    frameReceivers.Grid.Cells[Ord(rcObject), 0] := StrReceiverObject;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;
  finally
    frameReceivers.Grid.EndUpdate;
  end;

  rdgModflowBoundary.BeginUpdate;
  try
    ClearGrid(rdgModflowBoundary);

    seNumberOfTimes.AsInteger := 0;
    seNumberOfTimesChange(seNumberOfTimes);

    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithStartTimes
      (rdgModflowBoundary, Ord(mtcStartTime));
    frmGoPhast.PhastModel.ModflowStressPeriods.FillPickListWithEndTimes
      (rdgModflowBoundary, Ord(mtcEndTime));

    rdgModflowBoundary.Cells[Ord(mtcStartTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(mtcEndTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(mtcValue), 0] := StrReceiver1 + StrValue;
    rdgModflowBoundary.Cells[Ord(mtcType), 0] := StrReceiver1 + StrMoverType;
  finally
    rdgModflowBoundary.EndUpdate;
    Changing := False;
  end;
end;

procedure TframeScreenObjectMvr.LayoutMultiRowEditControls;
var
  AColumn: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;
  AColumn := Max(rdgModflowBoundary.LeftCol, FLastTimeColumn+1);
  if Odd(AColumn) then
  begin
    Inc(AColumn);
  end;
  LayoutControls(rdgModflowBoundary, rdeFormula, lblFormula,
    AColumn);

  AColumn := Max(rdgModflowBoundary.LeftCol, FLastTimeColumn+1);
  if not Odd(AColumn) then
  begin
    Inc(AColumn);
  end;
  LayoutControls(rdgModflowBoundary, comboMvrType, lblMvrType,
    AColumn);

end;

procedure TframeScreenObjectMvr.rdeFormulaChange(Sender: TObject);
var
  ColIndex: Integer;
  RowIndex: Integer;
  TempOptions: TGridOptions;
begin
  rdgModflowBoundary.BeginUpdate;
  try
    for RowIndex := rdgModflowBoundary.FixedRows to
      rdgModflowBoundary.RowCount - 1 do
    begin
      for ColIndex := FLastTimeColumn+1 to rdgModflowBoundary.ColCount - 1 do
      begin
        if Odd(ColIndex) then
        begin
          Continue;
        end;
        if rdgModflowBoundary.IsSelectedCell(ColIndex, RowIndex) then
        begin
          rdgModflowBoundary.Cells[ColIndex, RowIndex] := rdeFormula.Text;
          if Assigned(rdgModflowBoundary.OnSetEditText) then
          begin
            rdgModflowBoundary.OnSetEditText(
              rdgModflowBoundary,ColIndex,RowIndex, rdeFormula.Text);
          end;
        end;
      end;
    end;
  finally
    rdgModflowBoundary.EndUpdate;
  end;
  TempOptions := rdgModflowBoundary.Options;
  try
    rdgModflowBoundary.Options := [goEditing, goAlwaysShowEditor];
    rdgModflowBoundary.UpdateEditor;
  finally
    rdgModflowBoundary.Options := TempOptions;
  end;
end;

procedure TframeScreenObjectMvr.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  comboMvrType.Enabled := ShouldEnableMultisetControls;
end;

procedure TframeScreenObjectMvr.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  if ARow <= PestRowOffset then
  begin
    CanSelect := False;
  end;
end;

procedure TframeScreenObjectMvr.rdgModflowBoundarySetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
begin
  inherited;
  Changed;

end;

procedure TframeScreenObjectMvr.seNumberOfTimesChange(Sender: TObject);
begin
  inherited;
  Changed;

end;

procedure TframeScreenObjectMvr.SetChanging(const Value: Boolean);
begin
  FChanging := Value;
end;

procedure TframeScreenObjectMvr.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TMvrBoundary;
  BoundaryUsed: Boolean;
  RowIndex: Integer;
  RowOk: Boolean;
  ColumnIndex: Integer;
  MvrItem: TMvrItem;
  TimeValues: TMvrItems;
  ItemCount: Integer;
  IndvidualItem: TIndividualMvrItem;
  ItemIndex: Integer;
  Receivers: TReceiverCollection;
  ReceiverItem: TReceiverItem;
  ModflowLak6: TLakeMf6;
begin
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, StrInvalidLakeSource);
  frmErrorsAndWarnings.RemoveErrorGroup(frmGoPhast.PhastModel, StrInvalidLakeOutlet);
  TimeValues := nil;
  try
    if not ClearAll then
    begin
      TimeValues := TMvrItems.Create(nil, nil, nil);
      ItemCount := frameReceivers.seNumber.AsInteger;
      for RowIndex := 1+PestRowOffset to rdgModflowBoundary.RowCount - 1 do
      begin
        RowOk := True;

        for ColumnIndex := 0 to rdgModflowBoundary.ColCount - 1 do
        begin
          if rdgModflowBoundary.Cells[ColumnIndex,RowIndex] = '' then
          begin
            RowOk := False;
            break;
          end;
        end;
        if RowOk then
        begin
          MvrItem := TimeValues.Add as TMvrItem;
          MvrItem.StartTime := rdgModflowBoundary.RealValue[Ord(mtcStartTime),RowIndex];
          MvrItem.EndTime := rdgModflowBoundary.RealValue[Ord(mtcEndTime),RowIndex];
          MvrItem.Items.Count := ItemCount;
          for ColumnIndex := 2 to rdgModflowBoundary.ColCount - 1 do
          begin
            ItemIndex := ColumnIndex div 2 - 1;
            IndvidualItem := MvrItem.Items[ItemIndex];
            if Odd(ColumnIndex) then
            begin
              IndvidualItem.MvrType :=
                TMvrType(rdgModflowBoundary.ItemIndex[ColumnIndex,RowIndex]);
            end
            else
            begin
              IndvidualItem.Value :=
                rdgModflowBoundary.Cells[ColumnIndex,RowIndex]
            end;
          end;
        end;
      end;
    end;

    Receivers := TReceiverCollection.Create(nil);
    try
      for RowIndex := 1 to frameReceivers.Grid.RowCount - 1 do
      begin
        RowOk := True;

        if (frameReceivers.Grid.Cells[Ord(rcPackage), RowIndex] = '')
          or (frameReceivers.Grid.Cells[Ord(rcObject), RowIndex] = '') then
        begin
          RowOk := False;
        end
        else if ((comboSourcePackage.ItemIndex < 0)
          or (TSourcePackageChoice(comboSourcePackage.ItemIndex) = spcLak))
          and (frameReceivers.Grid.Cells[Ord(rcLakeOutlet),RowIndex] = '') then
        begin
          RowOk := False;
        end
        else if (TReceiverPackageChoice(frameReceivers.Grid.ItemIndex[Ord(rcPackage), RowIndex]) = rpcSfr)
          and (frameReceivers.Grid.Cells[Ord(rcSfrChoice), RowIndex] = '') then
        begin
          RowOk := False;
        end;

        if RowOk then
        begin
          ReceiverItem := Receivers.Add;
          ReceiverItem.ReceiverPackage := TReceiverPackageChoice(
            frameReceivers.Grid.ItemIndex[Ord(rcPackage), RowIndex]);
          if ReceiverItem.ReceiverPackage = rpcSfr then
          begin
            ReceiverItem.SfrReceiverChoice := TSfrReceiverChoice(
              frameReceivers.Grid.ItemIndex[Ord(rcSfrChoice), RowIndex]);
          end
          else
          begin
            ReceiverItem.SfrReceiverChoice := srcFirst;
          end;

          ReceiverItem.LakeOutlet := StrToIntDef(
            frameReceivers.Grid.Cells[Ord(rcLakeOutlet), RowIndex], 1);
          ReceiverItem.ReceiverObjectName :=
            frameReceivers.Grid.Cells[Ord(rcObject), RowIndex];
        end;
      end;

      for Index := 0 to List.Count - 1 do
      begin
        Item := List.Items[Index];
    //    ScreenObject := Item.ScreenObject;
        Boundary := Item.ScreenObject.ModflowMvr;
        BoundaryUsed := (Boundary <> nil) and Boundary.Used;
        { TODO -cBUG : If multiple items are edited simultaneously, all of them may have MVR boundaries defined even if some should not. }
        if ClearAll then
        begin
          if BoundaryUsed then
          begin
            Boundary.Clear;
          end;
        end
        else if SetAll or BoundaryUsed then
        begin
          if Boundary = nil then
          begin
            Item.ScreenObject.CreateModflowMvr;
            Boundary := Item.ScreenObject.ModflowMvr;
          end;
        end;

        if (Boundary = nil) then
        begin
          Continue;
        end;

        if Receivers.Count > 0 then
        begin
          Boundary.Receivers := Receivers;
        end;

        if comboSourcePackage.ItemIndex >= 0 then
        begin
          Boundary.SourcePackageChoice :=
            TSourcePackageChoice(comboSourcePackage.ItemIndex)
        end;

        if (TimeValues <> nil) and (TimeValues.Count > 0) then
        begin
          Boundary.Values.Assign(TimeValues);
        end;

        if Boundary.SourcePackageChoice = spcLak then
        begin
          ModflowLak6 := Item.ScreenObject.ModflowLak6;
          if ModflowLak6 = nil then
          begin
            frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
              StrInvalidLakeSource, Format(StrInvalidLakeSourceExplanation,
              [Item.ScreenObject.Name]));
          end
          else
          begin
            for ItemIndex := 0 to Boundary.Receivers.Count - 1 do
            begin
              ReceiverItem := Boundary.Receivers[ItemIndex];
              if ReceiverItem.LakeOutlet > ModflowLak6.Outlets.Count then
              begin
                frmErrorsAndWarnings.AddError(frmGoPhast.PhastModel,
                  StrInvalidLakeOutlet, Format(StrInvalidLakeOutletExplanation,
                  [Item.ScreenObject.Name]));
              end;
            end;
          end;
        end;
      end;
    finally
      Receivers.Free;
    end;
  finally
    TimeValues.Free;
  end;

end;

procedure TframeScreenObjectMvr.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

end.
