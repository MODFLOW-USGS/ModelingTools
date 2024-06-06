unit frameScreenObjectMvrUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectNoParamUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, UndoItemsScreenObjects, Vcl.ComCtrls, frameGridUnit,
  ModflowMvrUnit, JvExExtCtrls, JvNetscapeSplitter;

type
  TReceiverColumn = (rcPackage, rcDivide, rcSfrChoice, rcLakeOutlet, rcObject);
  TMapColumn = (mcSource, mcReceiver, mcValue);
  TReceiverItemColumn = (ricValue, ricType, ricMapName);

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
    tabMvrMap: TTabSheet;
    JvNetscapeSplitter1: TJvNetscapeSplitter;
    rdgMap: TRbwDataGrid4;
    frameMapNames: TframeGrid;
    pnl1: TPanel;
    pnl2: TPanel;
    seReceiverNumber: TJvSpinEdit;
    lblReceiverNumber: TLabel;
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
    procedure frameMapNamesseNumberChange(Sender: TObject);
    procedure frameMapNamesGridSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure frameMapNamessbAddClick(Sender: TObject);
    procedure frameMapNamessbInsertClick(Sender: TObject);
    procedure frameMapNamessbDeleteClick(Sender: TObject);
    procedure frameMapNamesGridSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure rdgMapExit(Sender: TObject);
    procedure rdgMapSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seReceiverNumberChange(Sender: TObject);
    procedure rdgMapSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
  private
    FLakObjects: TStringList;
    FMawObjects: TStringList;
    FSfrObjects: TStringList;
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    FOnGetSources: TGetSourcesEvent;
    FUzfObjects: TStringList;
    FMvrMaps: TSectionMaps;
    FMapItem: TSectionMap;
    FValuesChanged: Boolean;
    FRow: Integer;
    procedure Changed;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetChanging(const Value: Boolean);
    procedure InitializeGrids;
    function GetLakObjects: TStringList;
    function GetMawObjects: TStringList;
    function GetSfrObjects: TStringList;
    function GetUzfObjects: TStringList;
    procedure SetMapItem(const Value: TSectionMap);
    property MapItem: TSectionMap read FMapItem write SetMapItem;
    procedure ColumnToColumnType(ColumnIndex: Integer; var ReceiverIndex: Integer;
      var ColumnType: TReceiverItemColumn);
    { Private declarations }
  protected
    procedure LayoutMultiRowEditControls; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
  ModflowLakMf6Unit, System.Generics.Collections;

const
  NumberOfColumnsPerReciever = 3;
  NumberOfTimeColumns = 2;

resourcestring
  StrValue = ' value';
  StrMoverType = ' mover type';
  StrSourceLakeOutlet = 'Source lake outlet';
  StrReceiverPackage = 'Receiver package';
  StrSFRReceiverReach = 'SFR receiver reach';
  StrReceiverObject = 'Receiver object';
  StrReceiver1 = 'Receiver 1';
  StrReceiver = 'Receiver ';
  StrInvalidLakeSource = 'Invalid Lake source in MVR Package';
  StrInvalidLakeOutlet = 'Invalid Lake outlet in MVR Package';
  StrInvalidLakeSourceExplanation = 'The object "%s" defines a Lake MVR sour' +
  'ce but the object does not define a lake.';
  StrInvalidLakeOutletExplanation = 'The object "%s" defines a Lake MVR sour' +
  'ce but the lake outlet number is invalid.';
  StrDivideFlowEqually = 'Divide flow equally among UZF receiver cells';
  StrMapNameDOptiona = 'UZF receiver map name %d (optional)';
  StrMapName = 'UZF receiver map names';
  StrSourceSection = 'Source section';
  StrReceiverSections = 'Receiver section %d';
  StrReceiverValue = 'Value';

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

constructor TframeScreenObjectMvr.Create(AOwner: TComponent);
begin
  inherited;
  FMvrMaps := TSectionMaps.Create(nil);
end;

destructor TframeScreenObjectMvr.Destroy;
begin
  FMvrMaps.Free;
  inherited;
end;

procedure TframeScreenObjectMvr.frameMapNamesGridSelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  AMapItem: TSectionMap;
  RowIndex: Integer;
  ItemIndex: Integer;
  AnItem: TSectionMapItem;
  ColIndex: Integer;
  MaxReceivers: Integer;
  ReceiverIndex: Integer;
  ReceiverItem: TReceiverSectionValue;
begin
  inherited;
  FRow := ARow;
  if Changing or frameMapNames.Grid.Drawing or FValuesChanged then
  begin
    Exit;
  end;
  if ARow >= 1 then
  begin
    if frameMapNames.Grid.Objects[ACol, ARow] <> nil then
    begin
      AMapItem := frameMapNames.Grid.Objects[ACol, ARow] as TSectionMap;
      MapItem := AMapItem;
      for RowIndex := 1 to rdgMap.RowCount - 1 do
      begin
        for ColIndex := Ord(mcReceiver) to rdgMap.ColCount - 1 do
        begin
          rdgMap.Cells[ColIndex, RowIndex] := '';
        end;
      end;
      MaxReceivers := 1;
      for ItemIndex := 0 to AMapItem.MvrMap.Count - 1 do
      begin
        AnItem := AMapItem.MvrMap[ItemIndex];
        if (AnItem.SourceSection >= 1)
          and (AnItem.SourceSection < rdgMap.RowCount) then
        begin
          if AnItem.ReceiverSectionsValues.Count > MaxReceivers then
          begin
            MaxReceivers := AnItem.ReceiverSectionsValues.Count;
          end;
//          rdgMap.Cells[Ord(mcReceiver), ItemIndex+1] := AnItem.ReceiverSections.CommaSeparatedText;
        end;
      end;
      seReceiverNumber.AsInteger := MaxReceivers;
      seReceiverNumberChange(nil);
      for ItemIndex := 0 to AMapItem.MvrMap.Count - 1 do
      begin
        AnItem := AMapItem.MvrMap[ItemIndex];
        if (AnItem.SourceSection >= 1)
          and (AnItem.SourceSection < rdgMap.RowCount) then
        begin
          for ReceiverIndex := 0 to AnItem.ReceiverSectionsValues.Count - 1 do
          begin
            ReceiverItem := AnItem.ReceiverSectionsValues[ReceiverIndex];
            ColIndex := ReceiverIndex*2 + 1;
            rdgMap.IntegerValue[ColIndex, AnItem.SourceSection] := ReceiverItem.SectionNumber;
            Inc(ColIndex);
            rdgMap.RealValue[ColIndex, AnItem.SourceSection] := ReceiverItem.Value;
          end;
        end;
      end;
    end
    else
    begin
      MapItem := nil;
    end;
  end
  else
  begin
    MapItem := nil;
  end;
end;

procedure TframeScreenObjectMvr.frameMapNamesGridSetEditText(Sender: TObject;
  ACol, ARow: Integer; const Value: string);
var
  Dummy: Boolean;
  AMapItem: TSectionMap;
begin
  inherited;
  if Changing then
  begin
    Exit;
  end;
  if ARow >= 1 then
  begin
    if Value <> '' then
    begin
      if frameMapNames.Grid.Objects[ACol, ARow] = nil then
      begin
        AMapItem := FMvrMaps.Add;
        frameMapNames.Grid.Objects[ACol, ARow] := AMapItem;
      end
      else
      begin
        AMapItem := frameMapNames.Grid.Objects[ACol, ARow] as TSectionMap;
      end;
      AMapItem.MvrMap.MapName := Value;
    end;
    Dummy := True;
    frameMapNamesGridSelectCell(self, ACol, ARow, Dummy)
  end;
end;

procedure TframeScreenObjectMvr.frameMapNamessbAddClick(Sender: TObject);
begin
  inherited;
  frameMapNames.sbAddClick(Sender);

end;

procedure TframeScreenObjectMvr.frameMapNamessbDeleteClick(Sender: TObject);
var
  FirstValidRow: Integer;
  SelectedRow: Integer;
begin
  inherited;
  FirstValidRow := frameMapNames.Grid.FixedRows;
  SelectedRow := frameMapNames.Grid.SelectedRow;
  frameMapNames.sbDeleteClick(Sender);
  if SelectedRow >= FirstValidRow then
  begin
    frameMapNames.Grid.Objects[0, SelectedRow].Free;
  end;

end;

procedure TframeScreenObjectMvr.frameMapNamessbInsertClick(Sender: TObject);
var
  FirstValidRow: Integer;
  SelectedRow: Integer;
begin
  inherited;
  FirstValidRow := frameMapNames.Grid.FixedRows;
  SelectedRow := frameMapNames.Grid.SelectedRow;
  frameMapNames.sbInsertClick(Sender);
  if SelectedRow >= FirstValidRow then
  begin
    frameMapNames.Grid.Objects[0, SelectedRow] := FMvrMaps.Insert(SelectedRow-1);
  end;
end;

procedure TframeScreenObjectMvr.frameMapNamesseNumberChange(Sender: TObject);
var
  OldRowCount: Integer;
  NewRowCount: Integer;
  RowIndex: Integer;
begin
  inherited;
  OldRowCount := frameMapNames.Grid.RowCount;
  NewRowCount := frameMapNames.seNumber.AsInteger+1;
  frameMapNames.Grid.BeginUpdate;
  try
    if (NewRowCount < frameMapNames.Grid.RowCount) then
    begin
      for RowIndex := NewRowCount to frameMapNames.Grid.RowCount - 1 do
      begin
        frameMapNames.Grid.Objects[0, RowIndex].Free;
        frameMapNames.Grid.Objects[0, RowIndex] := nil;
      end;
    end;

    frameMapNames.seNumberChange(Sender);

    if NewRowCount > OldRowCount then
    begin
      for RowIndex := OldRowCount to NewRowCount - 1 do
      begin
        frameMapNames.Grid.Objects[0, RowIndex] := FMvrMaps.Add;
      end;
    end;

  finally
    frameMapNames.Grid.EndUpdate;
  end;

end;

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
          rcDivide:
            begin
              CanSelect := TReceiverPackageChoice(ReceiverPkgIndex) = rpcUzf;
            end;
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
  RowIndex: Integer;
begin
  inherited;
  rdgModflowBoundary.BeginUpdate;
  try
    if frameReceivers.seNumber.AsInteger <= 0 then
    begin
      frameReceivers.seNumber.AsInteger:= 1;
    end;
    frameReceivers.seNumberChange(Sender);
    rdgModflowBoundary.ColCount := NumberOfColumnsPerReciever
      * (frameReceivers.seNumber.AsInteger) + NumberOfTimeColumns;

    for RowIndex := 1 to frameReceivers.Grid.RowCount - 1 do
    begin
      if frameReceivers.Grid.Cells[Ord(rcPackage), RowIndex] = '' then
      begin
        frameReceivers.Grid.Checked[Ord(rcDivide), RowIndex] := True;
      end;
    end;

    for ReceiverIndex := 0 to frameReceivers.seNumber.AsInteger -1 do
    begin
      ColIndex := NumberOfTimeColumns + ReceiverIndex* NumberOfColumnsPerReciever
        + Ord(ricValue);
      rdgModflowBoundary.Cells[ColIndex, 0] :=
        StrReceiver + (ReceiverIndex+1).ToString + StrValue;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonUsed := True;
      rdgModflowBoundary.Columns[ColIndex].ButtonWidth := 35;
      rdgModflowBoundary.Columns[ColIndex].ButtonCaption := 'F()';

      ColIndex := NumberOfTimeColumns + ReceiverIndex* NumberOfColumnsPerReciever
        + Ord(ricType);

      rdgModflowBoundary.Cells[ColIndex, 0] :=
        StrReceiver + (ReceiverIndex+1).ToString + StrMoverType;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
      rdgModflowBoundary.Columns[ColIndex].PickList.Assign(comboMvrType.Items);
      rdgModflowBoundary.Columns[ColIndex].ComboUsed := True;
      rdgModflowBoundary.Columns[ColIndex].LimitToList := True;

      ColIndex := NumberOfTimeColumns + ReceiverIndex* NumberOfColumnsPerReciever
        + Ord(ricMapName);

      rdgModflowBoundary.Cells[ColIndex, 0] := Format(StrMapNameDOptiona, [ReceiverIndex+1]) ;
      rdgModflowBoundary.Columns[ColIndex].WordWrapCaptions := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustRowHeights := True;
      rdgModflowBoundary.Columns[ColIndex].AutoAdjustColWidths := True;
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
  RowIndex: Integer;
  MapIndex: Integer;
  AMap: TSectionMap;
  TimeRowIndex: Integer;
  Dummy: Boolean;
begin
  rdgModflowBoundary.Handle;
  seNumberOfTimes.Handle;
  InitializeGrids;

  MoveGridToTabSheet(tabTime);
  pcMain.ActivePageIndex := 0;

  FLakObjects := nil;
  FMawObjects := nil;
  FSfrObjects := nil;
  FUzfObjects := nil;
  FirstReceivers := nil;

  Changing := True;
  try
    comboSourcePackage.ItemIndex := -1;
  {$IFDEF ImportMF6}
    if ScreenObjectList.Count = 1 then
    begin
      AScreenObject := ScreenObjectList[0].ScreenObject;
      if AScreenObject.SectionCount > 1 then
      begin
        tabMvrMap.Visible := True;
        rdgMap.RowCount := AScreenObject.SectionCount + 1;
        for RowIndex := 1 to rdgMap.RowCount - 1 do
        begin
          rdgMap.Cells[Ord(mcSource), RowIndex] := IntToStr(RowIndex);
        end;
      end;
    end;
  {$ELSE}
    tabMvrMap.Visible := False;
  {$ENDIF}

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
          frameReceiversseNumberChange(nil);
          for ReceiverIndex := 0 to Receivers.Count - 1 do
          begin
            ReceiverItem := Receivers[ReceiverIndex];
            frameReceivers.Grid.ItemIndex[Ord(rcPackage), ReceiverIndex+1]
              := Ord(ReceiverItem.ReceiverPackage);
            frameReceivers.Grid.Checked[Ord(rcDivide), ReceiverIndex+1]
              := ReceiverItem.DivisionChoice = dcDivide;
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


            if frameReceivers.seNumber.AsInteger < MvrItem.Items.Count then
            begin
              MvrItem.Items.Count := frameReceivers.seNumber.AsInteger;
            end;

            for ReceiverIndex := 0 to MvrItem.Items.Count - 1 do
            begin
              AReceiverItem := MvrItem.Items[ReceiverIndex];
              TimeRowIndex := TimeIndex+1+PestRowOffset;
              ColIndex := ReceiverIndex*NumberOfColumnsPerReciever
                + NumberOfTimeColumns + Ord(ricValue);
              rdgModflowBoundary.Cells[ColIndex, TimeRowIndex] :=
                AReceiverItem.Value;

              ColIndex := ReceiverIndex*NumberOfColumnsPerReciever
                + NumberOfTimeColumns + Ord(ricType);
              rdgModflowBoundary.ItemIndex[ColIndex, TimeRowIndex] :=
                Ord(AReceiverItem.MvrType);

              ColIndex := ReceiverIndex*NumberOfColumnsPerReciever
                + NumberOfTimeColumns + Ord(ricMapName);
              rdgModflowBoundary.Cells[ColIndex, TimeRowIndex] :=
                AReceiverItem.MapName;
            end;
          end;

          if tabMvrMap.Visible then
          begin
            FMvrMaps.Assign(ModflowMvr.MvrMaps);
            rdgMap.RowCount := AScreenObject.SectionCount + 1;
            frameMapNames.Grid.BeginUpdate;
            try
              frameMapNames.Grid.RowCount := Max(FMvrMaps.Count, 1)+1;
              frameMapNames.Grid.Cells[0,1] := '';
              for MapIndex := 0 to FMvrMaps.Count - 1 do
              begin
                AMap := FMvrMaps[MapIndex];
                frameMapNames.Grid.Cells[0, MapIndex+1] := AMap.MvrMap.MapName;
                frameMapNames.Grid.Objects[0, MapIndex+1] := AMap;
              end;
            finally
              frameMapNames.Grid.EndUpdate;
              frameMapNames.Width := Max(110, frameMapNames.Grid.ColWidths[0] + 16);
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
    FValuesChanged := False;
  end;
  Dummy := True;
  frameMapNamesGridSelectCell(frameMapNames.Grid, 0, 1, Dummy);
  frameReceivers.Grid.HideEditor;
  frameMapNames.Grid.HideEditor;
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
var
  RowIndex: Integer;
begin
  Changing := True;
  rdgMap.FixedCols := 1;

  frameMapNames.Grid.BeginUpdate;
  try
    ClearGrid(frameMapNames.Grid);
    frameMapNames.Grid.Cells[0,0] := StrMapName;
  finally
    frameMapNames.Grid.EndUpdate;
  end;

  rdgMap.BeginUpdate;
  try
    ClearGrid(rdgMap);
    rdgMap.Cells[Ord(mcSource),0] := StrSourceSection;
    rdgMap.Cells[Ord(mcReceiver),0] := Format(StrReceiverSections, [1]);
    rdgMap.Cells[Ord(mcValue),0] := StrReceiverValue;
    rdgMap.RowCount := 2;
  finally
    rdgMap.EndUpdate;
  end;

  frameReceivers.Grid.BeginUpdate;
  try
    ClearGrid(frameReceivers.Grid);
    frameReceivers.seNumber.AsInteger := 1;

    frameReceivers.Grid.Cells[Ord(rcPackage), 0] := StrReceiverPackage;
    frameReceivers.Grid.Cells[Ord(rcDivide), 0] := StrDivideFlowEqually;
    frameReceivers.Grid.Cells[Ord(rcSfrChoice), 0] := StrSFRReceiverReach;
    frameReceivers.Grid.Cells[Ord(rcLakeOutlet), 0] := StrSourceLakeOutlet;
    frameReceivers.Grid.Cells[Ord(rcObject), 0] := StrReceiverObject;
    rdgModflowBoundary.Cells[0, PestModifierRow] := StrPestModifier;
    rdgModflowBoundary.Cells[0, PestMethodRow] := StrModificationMethod;

    for RowIndex := 1 to frameReceivers.Grid.RowCount - 1 do
    begin
      frameReceivers.Grid.Checked[Ord(rcDivide), RowIndex] := True;
    end;
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

procedure TframeScreenObjectMvr.rdgMapExit(Sender: TObject);
var
  Map: TSectionMapItemCollection;
  RowIndex: Integer;
  ItemIndex: Integer;
  AnItem: TSectionMapItem;
  MaxReceivers: Int64;
  ReceiverIndex: Integer;
  ReceiverCount: Integer;
  ColIndex: Integer;
  SectionNumber: Integer;
  AValue: double;
  ReceiverItem: TReceiverSectionValue;
  Dummy: Boolean;
begin
  inherited;
  if MapItem <> nil then
  begin
    Map := MapItem.MvrMap;
    for RowIndex := 1 to rdgMap.RowCount - 1 do
    begin
      rdgMap.Objects[Ord(mcReceiver), RowIndex] := nil;
    end;
    for ItemIndex := Map.Count - 1 downto 0 do
    begin
      AnItem := Map[ItemIndex];
      if (AnItem.SourceSection >= 1)
        and (AnItem.SourceSection <= rdgMap.RowCount-1) then
      begin
        if rdgMap.Objects[Ord(mcReceiver), AnItem.SourceSection] <> nil then
        begin
          rdgMap.Objects[Ord(mcReceiver), AnItem.SourceSection].Free;
        end;
        rdgMap.Objects[Ord(mcReceiver), ItemIndex+1] := AnItem;
      end
      else
      begin
        AnItem.Free;
      end;
    end;
    MaxReceivers := seReceiverNumber.AsInteger;
    for RowIndex := 1 to rdgMap.RowCount - 1  do
    begin
      if rdgMap.Cells[Ord(mcReceiver), RowIndex] <> '' then
      begin
        AnItem := rdgMap.Objects[Ord(mcReceiver), RowIndex] as TSectionMapItem;
        if AnItem = nil then
        begin
          AnItem := Map.Add;
          AnItem.SourceSection := RowIndex;
        end;

        ReceiverCount := 0;
        while AnItem.ReceiverSectionsValues.Count > MaxReceivers do
        begin
          AnItem.ReceiverSectionsValues.Last.Free;
        end;
        for ReceiverIndex := 0 to MaxReceivers - 1 do
        begin
          ColIndex := ReceiverIndex*2 + 1;
          if TryStrToInt(rdgMap.Cells[ColIndex, RowIndex], SectionNumber)
            and TryStrToFloat(rdgMap.Cells[ColIndex+1, RowIndex], AValue) then
          begin
            if ReceiverCount < AnItem.ReceiverSectionsValues.Count then
            begin
              ReceiverItem := AnItem.ReceiverSectionsValues[ReceiverCount];
            end
            else
            begin
              ReceiverItem := AnItem.ReceiverSectionsValues.Add;
            end;
            ReceiverItem.SectionNumber := SectionNumber;
            ReceiverItem.Value := AValue;
            Inc(ReceiverCount);
          end;
        end;
        AnItem.ReceiverSectionsValues.Count := ReceiverCount;
        if AnItem.ReceiverSectionsValues.Count = 0 then
        begin
          AnItem.Free;
          rdgMap.Objects[Ord(mcReceiver), RowIndex] := nil;
        end;
      end
      else
      begin
        rdgMap.Objects[Ord(mcReceiver), RowIndex].Free;
        rdgMap.Objects[Ord(mcReceiver), RowIndex] := nil;
      end;
    end;
  end;
  if FValuesChanged then
  begin
    FValuesChanged := False;
    Dummy := True;
    frameMapNamesGridSelectCell(frameMapNames.Grid, 0, FRow, Dummy);
  end;
end;

procedure TframeScreenObjectMvr.rdgMapSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  inherited;
  CanSelect := MapItem <> nil;
end;

procedure TframeScreenObjectMvr.rdgMapSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  FValuesChanged := True;
end;

procedure TframeScreenObjectMvr.rdgModflowBoundaryMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  comboMvrType.Enabled := ShouldEnableMultisetControls;
end;

procedure TframeScreenObjectMvr.rdgModflowBoundarySelectCell(Sender: TObject;
  ACol, ARow: Integer; var CanSelect: Boolean);
var
  ItemIndex: Integer;
  ColumnType: TReceiverItemColumn;
  ReceiverIndex: Integer;
begin
  inherited;
  if ARow <= PestRowOffset then
  begin
    CanSelect := False;
  end;
  if (ACol >= NumberOfTimeColumns)
    and (ARow >= rdgModflowBoundary.FixedRows + PestRowOffset) then
  begin
    ColumnToColumnType(ACol, ItemIndex, ColumnType);
    if ColumnType = ricMapName then
    begin
      ReceiverIndex := frameReceivers.Grid.ItemIndex[Ord(rcPackage), ItemIndex+1];
      if ReceiverIndex >= 0 then
      begin
        CanSelect := TReceiverPackageChoice(ReceiverIndex) in [rpcSfr, rpcUzf];
      end;
    end;
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

procedure TframeScreenObjectMvr.seReceiverNumberChange(Sender: TObject);
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
begin
  inherited;
  rdgMap.BeginUpdate;
  try
    rdgMap.ColCount := seReceiverNumber.AsInteger * 2 + 1;
    for ColIndex := Ord(mcReceiver) to rdgMap.ColCount - 1 do
    begin
      AColumn := rdgMap.Columns[ColIndex];
      if Odd(ColIndex) then
      begin
        rdgMap.Cells[ColIndex,0] := Format(StrReceiverSections, [(ColIndex + 1) div 2]);
        AColumn.Format := rcf4Integer;
      end
      else
      begin
        rdgMap.Cells[ColIndex,0] := StrReceiverValue;
        AColumn.Format := rcf4Real;
      end;
      AColumn.AutoAdjustColWidths := True;
      AColumn.AutoAdjustCaptionRowHeights := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.WordWrapCaptions := True;
    end;
  finally
    rdgMap.EndUpdate;
  end;
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
  ColumnType: TReceiverItemColumn;
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
          if ColumnIndex >= NumberOfTimeColumns then
          begin
            if (ColumnIndex - NumberOfTimeColumns) mod NumberOfColumnsPerReciever
              = Ord(ricMapName) then
            begin
              Continue;
            end;
          end;
          if (rdgModflowBoundary.Cells[ColumnIndex,RowIndex] = '') then
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
          for ColumnIndex := NumberOfTimeColumns to rdgModflowBoundary.ColCount - 1 do
          begin
            ColumnToColumnType(ColumnIndex, ItemIndex, ColumnType);
            IndvidualItem := MvrItem.Items[ItemIndex];
            case ColumnType of
              ricValue:
                begin
                  IndvidualItem.Value :=
                    rdgModflowBoundary.Cells[ColumnIndex,RowIndex]
                end;
              ricType:
                begin
                  IndvidualItem.MvrType :=
                    TMvrType(rdgModflowBoundary.ItemIndex[ColumnIndex,RowIndex]);
                end;
              ricMapName:
                begin
                  IndvidualItem.MapName := rdgModflowBoundary.Cells[ColumnIndex,RowIndex];
                end;
              else
                begin
                  Assert(False);
                end;
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

          if frameReceivers.Grid.Checked[Ord(rcDivide), RowIndex] then
          begin
            ReceiverItem.DivisionChoice := dcDivide;
          end
          else
          begin
            ReceiverItem.DivisionChoice := dcDoNotDivide;
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
        Boundary := Item.ScreenObject.ModflowMvr;
        BoundaryUsed := (Boundary <> nil) and Boundary.Used;
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

        if ((Boundary = nil) or not Boundary.Used) and (not SetAll) then
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

        if tabMvrMap.Visible then
        begin
          Boundary.MvrMaps := FMvrMaps;
        end;
      end;
    finally
      Receivers.Free;
    end;
  finally
    TimeValues.Free;
  end;

end;

procedure TframeScreenObjectMvr.ColumnToColumnType(ColumnIndex: Integer;
  var ReceiverIndex: Integer; var ColumnType: TReceiverItemColumn);
begin
  Assert(ColumnIndex >= NumberOfTimeColumns);
  ColumnIndex := ColumnIndex - NumberOfTimeColumns;
  ReceiverIndex := ColumnIndex div NumberOfColumnsPerReciever;
  ColumnType := TReceiverItemColumn(ColumnIndex mod NumberOfColumnsPerReciever);
end;

procedure TframeScreenObjectMvr.SetMapItem(const Value: TSectionMap);
var
  RowIndex: Integer;
begin
  FMapItem := Value;
  rdgMap.BeginUpdate;
  try
    for RowIndex := 1 to rdgMap.RowCount - 1 do
    begin
      rdgMap.Cells[Ord(mcReceiver), RowIndex] := ''
    end;
  finally
    rdgMap.EndUpdate;
  end;
end;

procedure TframeScreenObjectMvr.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

end.
