unit frameScreenObjectCSubUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frameScreenObjectTabbedUnit, Vcl.Grids,
  RbwDataGrid4, Vcl.StdCtrls, ArgusDataEntry, Vcl.Buttons, Vcl.Mask, JvExMask,
  JvSpin, Vcl.ExtCtrls, Vcl.ComCtrls, UndoItemsScreenObjects;

type
  TInterbedColumns = (icName, icUsed, icInitialOffset, icThickness,
    icEquivInterbedNumber, icInitialInelasticSpecificStorage,
    icInitialElasticSpecificStorage, icInitialPorosity, icDelayKv,
    icInitialDelayHeadOffset);

  TStressColumns = (scStartingTime, scEndingTime, scStressOffset);

  TframeScreenObjectCSub = class(TframeScreenObjectTabbed)
    tabInterbedSystems: TTabSheet;
    rdgSubGroups: TRbwDataGrid4;
    pnlEditPkgProp: TPanel;
    lblFormulaPkgProp: TLabel;
    rdeFormulaPkgProp: TRbwDataEntry;
    cbUsedPkgProp: TCheckBox;
    procedure rdgSubGroupsSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgSubGroupsStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
    procedure rdgSubGroupsColSize(Sender: TObject; ACol, PriorWidth: Integer);
    procedure rdgSubGroupsHorizontalScroll(Sender: TObject);
    procedure rdgSubGroupsMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure cbUsedPkgPropClick(Sender: TObject);
    procedure rdeFormulaPkgPropChange(Sender: TObject);
  private
    FPackageDataCleared: Boolean;
    FTimeDataCleared: Boolean;
    procedure InitializeControls;
    procedure LayoutMultiRowEditControlsPkgProp;
    { Private declarations }
  public
    procedure GetData(const List: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    { Public declarations }
  end;

var
  frameScreenObjectCSub: TframeScreenObjectCSub;

implementation

uses
  ModflowPackageSelectionUnit, frmGoPhastUnit, ScreenObjectUnit,
  ModflowCsubUnit, GoPhastTypes, System.Math, frmCustomGoPhastUnit,
  ModflowCSubInterbed;

resourcestring
  StrInterbed = 'Interbed';
  StrUsed = 'Used';
  StrInitialOffset = 'Initial Offset';
  StrThickness = 'Thickness';
  StrEquivalentInterbed = 'Equivalent Interbed Number';
  StrInitialInelasticS = ' Initial Inelastic Specific Storage';
  StrInitialElasticSpec = 'Initial Elastic Specific Storage';
  StrInitialPorosity = 'Initial Porosity';
  StrDelayKv = 'Delay Kv';
  StrInitialDelayHeadO = 'Initial Delay Head Offset';
  StrStressOffset = 'Stress Offset';

{$R *.dfm}

{ TframeScreenObjectCSub }

procedure TframeScreenObjectCSub.cbUsedPkgPropClick(Sender: TObject);
var
  RowIndex: Integer;
begin
  inherited;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    if rdgSubGroups.IsSelectedCell(Ord(icUsed),RowIndex) then
    begin
      rdgSubGroups.Checked[Ord(icUsed),RowIndex] := cbUsedPkgProp.Checked;
    end;
  end;
end;

procedure TframeScreenObjectCSub.GetData(
  const List: TScreenObjectEditCollection);
var
  ScreenObject: TScreenObject;
  ScreenObjectIndex: Integer;
  ModflowCSub: TCSubBoundary;
  FoundFirst: Boolean;
  InterbedIndex: Integer;
  Interbed: TCSubPackageData;
  RowIndex: Integer;
  FirstCSub: TCSubBoundary;
  Item: TCSubItem;
begin
  FGettingData := True;
  try
    InitializeControls;

    FPackageDataCleared := False;
    FTimeDataCleared := False;
    FoundFirst := False;
    FirstCSub := nil;
    for ScreenObjectIndex := 0 to List.Count - 1 do
    begin
      ScreenObject := List[ScreenObjectIndex].ScreenObject;
      ModflowCSub := ScreenObject.ModflowCSub;
      if (ModflowCSub <> nil) and ModflowCSub.Used then
      begin
        if not FoundFirst then
        begin
          FoundFirst := True;
          FirstCSub := ModflowCSub;
          rdgSubGroups.BeginUpdate;
          try
            for InterbedIndex := 0 to ModflowCSub.CSubPackageData.Count -1 do
            begin
              Interbed := ModflowCSub.CSubPackageData[InterbedIndex];
              RowIndex := rdgSubGroups.Cols[Ord(icName)].IndexOf(Interbed.InterbedSystemName);
              if RowIndex >= 1 then
              begin
                rdgSubGroups.Checked[Ord(icUsed), RowIndex] := Interbed.Used;
                rdgSubGroups.Cells[Ord(icInitialOffset), RowIndex] :=
                  Interbed.InitialOffset;
                rdgSubGroups.Cells[Ord(icThickness), RowIndex] :=
                  Interbed.Thickness;
                rdgSubGroups.Cells[Ord(icEquivInterbedNumber), RowIndex] :=
                  Interbed.EquivInterbedNumber;
                rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), RowIndex] :=
                  Interbed.InitialInelasticSpecificStorage;
                rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), RowIndex] :=
                  Interbed.InitialElasticSpecificStorage;
                rdgSubGroups.Cells[Ord(icInitialPorosity), RowIndex] :=
                  Interbed.InitialPorosity;
                rdgSubGroups.Cells[Ord(icDelayKv), RowIndex] := Interbed.DelayKv;
                rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), RowIndex] :=
                  Interbed.InitialDelayHeadOffset;
              end;
            end;
          finally
            rdgSubGroups.EndUpdate;
          end;

          rdgModflowBoundary.BeginUpdate;
          try
            seNumberOfTimes.AsInteger := ModflowCSub.Values.Count;
            for RowIndex := 1 to ModflowCSub.Values.Count do
            begin
              Item := ModflowCSub.Values[RowIndex-1] as TCSubItem;
              rdgModflowBoundary.Cells[Ord(scStartingTime), RowIndex] :=
                FloatToStr(Item.StartTime);
              rdgModflowBoundary.Cells[Ord(scEndingTime), RowIndex] :=
                FloatToStr(Item.EndTime);
              rdgModflowBoundary.Cells[Ord(scStressOffset), RowIndex] :=
                Item.StressOffset;
            end;
          finally
            rdgModflowBoundary.EndUpdate;
          end;
        end
        else
        begin
          if not ModflowCSub.CSubPackageData.IsSame(FirstCSub.CSubPackageData) then
          begin
            ClearGrid(rdgSubGroups);
            FPackageDataCleared := True;
          end;
          if not ModflowCSub.Values.IsSame(FirstCSub.Values) then
          begin
            ClearGrid(rdgModflowBoundary);
            FTimeDataCleared := True;
          end;
        end;
      end;
    end;
  finally
    FGettingData := False;
  end;

end;

procedure TframeScreenObjectCSub.InitializeControls;
var
  Interbeds: TCSubInterbeds;
  ColIndex: Integer;
  RowIndex: Integer;
begin
  pcMain.ActivePageIndex := 0;
  Interbeds := frmGoPhast.PhastModel.ModflowPackages.CSubPackage.Interbeds;
  ClearGrid(rdgSubGroups);
  rdgSubGroups.BeginUpdate;
  try
    for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
    begin
      rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := True;
    end;
    rdgSubGroups.RowCount := Interbeds.Count + 1;
    for RowIndex := 1 to Interbeds.Count do
    begin
      rdgSubGroups.Cells[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1].Name;
      rdgSubGroups.Objects[Ord(icName), RowIndex] :=
        Interbeds[RowIndex-1];
    end;

    rdgSubGroups.Cells[Ord(icName), 0] := StrInterbed;
    rdgSubGroups.Cells[Ord(icUsed), 0] := StrUsed;
    rdgSubGroups.Cells[Ord(icInitialOffset), 0] := StrInitialOffset;
    rdgSubGroups.Cells[Ord(icThickness), 0] := StrThickness;
    rdgSubGroups.Cells[Ord(icEquivInterbedNumber), 0] := StrEquivalentInterbed;
    rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), 0] := StrInitialInelasticS;
    rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), 0] := StrInitialElasticSpec;
    rdgSubGroups.Cells[Ord(icInitialPorosity), 0] := StrInitialPorosity;
    rdgSubGroups.Cells[Ord(icDelayKv), 0] := StrDelayKv;
    rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), 0] := StrInitialDelayHeadO;
  finally
    rdgSubGroups.EndUpdate;
  end;

  for ColIndex := 0 to rdgSubGroups.ColCount - 1 do
  begin
    rdgSubGroups.Columns[ColIndex].AutoAdjustColWidths := False;
  end;

  ClearGrid(rdgModflowBoundary);

  rdgModflowBoundary.BeginUpdate;
  try
    rdgModflowBoundary.Cells[Ord(scStartingTime), 0] := StrStartingTime;
    rdgModflowBoundary.Cells[Ord(scEndingTime), 0] := StrEndingTime;
    rdgModflowBoundary.Cells[Ord(scStressOffset), 0] := StrStressOffset;
    FillPicklistsWithStartTimes;
  finally
    rdgModflowBoundary.EndUpdate;
  end;

  LayoutMultiRowEditControls
end;

procedure TframeScreenObjectCSub.LayoutMultiRowEditControlsPkgProp;
var
  FormulaColumn: Integer;
begin
  if [csLoading, csReading] * ComponentState <> [] then
  begin
    Exit
  end;

  FormulaColumn := Max(Ord(icInitialOffset),rdgModflowBoundary.LeftCol);
  LayoutControls(rdgSubGroups, rdeFormulaPkgProp, lblFormulaPkgProp, FormulaColumn);

  FormulaColumn := Ord(icUsed);
  LayoutControls(rdgSubGroups, cbUsedPkgProp, nil, FormulaColumn);
end;

procedure TframeScreenObjectCSub.rdeFormulaPkgPropChange(Sender: TObject);
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    for ColIndex := Ord(icInitialOffset) to rdgSubGroups.ColCount do
    begin
      if rdgSubGroups.IsSelectedCell(ColIndex,RowIndex) then
      begin
        rdgSubGroups.Cells[ColIndex,RowIndex] := rdeFormulaPkgProp.Text;
      end;
    end;
  end;

end;

procedure TframeScreenObjectCSub.rdgSubGroupsColSize(Sender: TObject; ACol,
  PriorWidth: Integer);
begin
  inherited;
  LayoutMultiRowEditControlsPkgProp;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsHorizontalScroll(Sender: TObject);
begin
  inherited;
  LayoutMultiRowEditControlsPkgProp;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  ShouldEnable: Boolean;
  RowIndex: Integer;
  ColIndex: Integer;
begin
  inherited;
  ShouldEnable := False;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    for ColIndex := Ord(icInitialOffset) to rdgSubGroups.ColCount do
    begin
      ShouldEnable := rdgSubGroups.IsSelectedCell(ColIndex,RowIndex);
      if ShouldEnable then
      begin
        break;
      end;
    end;
    if ShouldEnable then
    begin
      break;
    end;
  end;
  rdeFormulaPkgProp.Enabled := ShouldEnable;

  ShouldEnable := False;
  for RowIndex := rdgSubGroups.FixedRows to rdgSubGroups.RowCount -1 do
  begin
    ShouldEnable := rdgSubGroups.IsSelectedCell(Ord(icUsed),RowIndex);
    if ShouldEnable then
    begin
      break;
    end;
  end;
  cbUsedPkgProp.Enabled := ShouldEnable;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCSub.rdgSubGroupsStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  inherited;
  Edited;
end;

procedure TframeScreenObjectCSub.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  Boundary: TCSubBoundary;
  BoundaryUsed: Boolean;
  Interbeds: TCSubInterbeds;
  CSubPackageData: TCSubPackageDataCollection;
  CSubValues: TCSubCollection;
  AnInterBed: TCSubInterbed;
  InterBedSystem: TCSubPackageData;
  Formula: string;
  TimeIndex: Integer;
  StartTime: double;
  EndTime: double;
  CSubItem: TCSubItem;
begin
  Interbeds := frmGoPhast.PhastModel.ModflowPackages.CSubPackage.Interbeds;
  CSubPackageData := TCSubPackageDataCollection.Create(nil);
  CSubValues := TCSubCollection.Create(nil, nil, nil);
  try
  for Index := 0 to Interbeds.Count -1 do
  begin
    AnInterBed := Interbeds[Index];
    InterBedSystem := CSubPackageData.Add;
    InterBedSystem.Interbed := AnInterBed;
    InterBedSystem.Used := rdgSubGroups.Checked[Ord(icUsed), Index+1];
    if InterBedSystem.Used then
    begin
      FPackageDataCleared := False;
    end;

    Formula := rdgSubGroups.Cells[Ord(icInitialOffset), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.InitialOffset := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icThickness), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.Thickness := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icEquivInterbedNumber), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.EquivInterbedNumber := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialInelasticSpecificStorage), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.InitialInelasticSpecificStorage := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialElasticSpecificStorage), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.InitialElasticSpecificStorage := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialPorosity), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.InitialPorosity := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icDelayKv), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.DelayKv := Formula;
    end;
    Formula := rdgSubGroups.Cells[Ord(icInitialDelayHeadOffset), Index+1];
    if Formula <> '' then
    begin
      InterBedSystem.InitialDelayHeadOffset := Formula;
    end;
  end;

  for TimeIndex := 0 to seNumberOfTimes.AsInteger -1 do
  begin
    if TryStrToFloat(rdgModflowBoundary.Cells[Ord(scStartingTime), TimeIndex+1], StartTime)
      and TryStrToFloat(rdgModflowBoundary.Cells[Ord(scEndingTime), TimeIndex+1], EndTime)
      and (rdgModflowBoundary.Cells[Ord(scStressOffset), TimeIndex+1] <> '') then
    begin
      CSubItem := CSubValues.Add;
      CSubItem.StartTime := StartTime;
      CSubItem.EndTime := EndTime;
      CSubItem.StressOffset := rdgModflowBoundary.Cells[Ord(scStressOffset), TimeIndex+1];
    end;
  end;

//  TStressColumns = (scStartingTime, scEndingTime, scStressOffset);
  
  for Index := 0 to List.Count - 1 do
  begin
    Item := List.Items[Index];
    Boundary := Item.ScreenObject.ModflowCSub;
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
      if SetAll and (Boundary = nil) then
      begin
        Item.ScreenObject.CreateCSubBoundary;
        Boundary := Item.ScreenObject.ModflowCSub;
        Boundary.CSubPackageData := CSubPackageData;
      end;
      
      if (Boundary <> nil) then 
      begin
        if not FPackageDataCleared then
        begin
          Boundary.CSubPackageData := CSubPackageData;
        end;
        
        if (not FTimeDataCleared) or (CSubValues.Count > 0) then
        begin
          Boundary.Values := CSubValues;
        end;
      end;
    end;
  end;
  finally
    CSubPackageData.Free;
    CSubValues.Free;
  end;
end;

end.
