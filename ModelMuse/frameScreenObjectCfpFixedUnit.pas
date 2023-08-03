unit frameScreenObjectCfpFixedUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, frameScreenObjectUnit, StdCtrls,
  ExtCtrls, UndoItemsScreenObjects, Vcl.Mask, Vcl.Grids, Vcl.ComCtrls,
  frameGridUnit, RbwDataGrid4, PhastModelInterfaceUnit;

type
  TCfpColumns = (ccTime, ccHead, ccValue1, ccValue2);

  TframeScreenObjectCfpFixed = class(TframeScreenObject)
    pnlCaption: TPanel;
    pcCfp: TPageControl;
    tabSteady: TTabSheet;
    lblValue2: TLabel;
    lblValue3: TLabel;
    edValue2: TEdit;
    btnValue2: TButton;
    edValue3: TEdit;
    btnValue3: TButton;
    tabTransient: TTabSheet;
    frameTimeDependent: TframeGrid;
    lblHint: TLabel;
    btnFixedHead: TButton;
    edFixedHead: TLabeledEdit;
    cbTimeDependent: TCheckBox;
    rgBoundaryType: TRadioGroup;
    procedure cbTimeDependentClick(Sender: TObject);
    procedure edFixedHeadChange(Sender: TObject);
    procedure edValue2Change(Sender: TObject);
    procedure edValue3Change(Sender: TObject);
    procedure frameTimeDependentGridEndUpdate(Sender: TObject);
    procedure frameTimeDependentGridSetEditText(Sender: TObject; ACol, ARow:
        Integer; const Value: string);
    procedure rgBoundaryTypeClick(Sender: TObject);
    procedure StringGrid1SetEditText(Sender: TObject; ACol, ARow: Integer; const
        Value: string);
  private
    FChanging: Boolean;
    FOnChange: TNotifyEvent;
    FValue2Set: Boolean;
    FValue3Set: Boolean;
    FTransientGridCleared: Boolean;
    property Changing: Boolean read FChanging write FChanging;
    procedure DoChange;
    procedure InitializeControls;
    { Private declarations }
  public
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll: boolean;
      ClearAll: boolean);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    { Public declarations }
  end;

var
  frameScreenObjectCfpFixed: TframeScreenObjectCfpFixed;

implementation

uses
  ScreenObjectUnit, ModflowCfpFixedUnit, GoPhastTypes, ModflowBoundaryUnit;

resourcestring
  StrDefinedHead = 'Defined Head';
  StrTime = 'Time';
  StrLimitedFlowValue = 'Limited flow value (optional)';
  StrDefinedDischarge = 'Defined Discharge';
  StrCellToWellConduct = 'Cell to well conductance (optional)';
  StrRobinCauchyHead = 'Robin (Cauchy) Head';
  StrRobinCauchyCondu = 'Robin (Cauchy) conductivity';
  StrCauchyLimitedInflo = 'Cauchy limited inflow';
  StrLimitedHead = 'Limited Head';

{$R *.dfm}

procedure TframeScreenObjectCfpFixed.cbTimeDependentClick(Sender: TObject);
begin
  inherited;
  tabTransient.TabVisible := cbTimeDependent.State in [cbChecked, cbGrayed];
  DoChange;

  if not Changing then
  begin
    cbTimeDependent.AllowGrayed := False;
  end;
end;

{ TframeScreenObjectCfpFixed }

procedure TframeScreenObjectCfpFixed.DoChange;
begin
  if Changing then
  begin
    Exit;
  end;
  if Assigned(OnChange) then
  begin
    OnChange(Self);
  end;
end;

procedure TframeScreenObjectCfpFixed.edFixedHeadChange(Sender: TObject);
begin
  inherited;
  DoChange;
end;

procedure TframeScreenObjectCfpFixed.edValue2Change(Sender: TObject);
begin
  inherited;
  DoChange;
  if not Changing then
  begin
    FValue2Set := True;
  end;
end;

procedure TframeScreenObjectCfpFixed.edValue3Change(Sender: TObject);
begin
  inherited;
  DoChange;
  if not Changing then
  begin
    FValue3Set := True;
  end;
end;

procedure TframeScreenObjectCfpFixed.frameTimeDependentGridEndUpdate(Sender:
    TObject);
begin
  inherited;
  frameTimeDependent.GridEndUpdate(Sender);
end;

procedure TframeScreenObjectCfpFixed.frameTimeDependentGridSetEditText(Sender:
    TObject; ACol, ARow: Integer; const Value: string);
begin
  inherited;
  FTransientGridCleared := False;
end;

procedure TframeScreenObjectCfpFixed.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  ListOfScreenObjects: TScreenObjectList;
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  ABoundary: TCfpFixedBoundary;
  ScreenObjectIndex: Integer;
  FirstTimeDependentBoundary: TCfpFixedBoundary;
  TimeIndex: Integer;
  AnItem: TCustomBoundaryItem;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundIndex: Integer;
begin
  Assert(ScreenObjectList.Count >= 1);
  Changing := True;
  try
    InitializeControls;
    FirstTimeDependentBoundary := nil;
    ListOfScreenObjects := TScreenObjectList.Create;
    try
      FValue2Set := True;
      FValue3Set := True;
      for Index := 0 to ScreenObjectList.Count - 1 do
      begin
        Item := ScreenObjectList[Index];
        AScreenObject := Item.ScreenObject;
        ABoundary := AScreenObject.ModflowCfpFixedHeads;
        if (ABoundary <> nil) and ABoundary.Used then
        begin
          ListOfScreenObjects.Add(AScreenObject);
        end;
      end;
      FTransientGridCleared := False;
      if ListOfScreenObjects.Count > 0 then
      begin
        ABoundary := ListOfScreenObjects[0].ModflowCfpFixedHeads;
        edFixedHead.Text := ABoundary.FixedHead;
        edValue2.Text := ABoundary.Value2;
        edValue3.Text := ABoundary.Value3;
        rgBoundaryType.ItemIndex := Ord(ABoundary.BoundaryType);
        cbTimeDependent.Checked := ABoundary.TimeDependent;
        FirstTimeDependentBoundary := ABoundary;
        if ABoundary.TimeDependent then
        begin
          frameTimeDependent.seNumber.AsInteger :=
            FirstTimeDependentBoundary.Values.Count;
          frameTimeDependent.seNumber.OnChange(nil);
          for TimeIndex := 0 to FirstTimeDependentBoundary.Values.Count - 1 do
          begin
            AnItem := FirstTimeDependentBoundary.Values[TimeIndex];
            RowIndex := TimeIndex + 1;
            frameTimeDependent.Grid.Cells[Ord(ccTime), RowIndex] := FloatToStr(AnItem.StartTime);
            for ColIndex := 1 to frameTimeDependent.Grid.ColCount - 1 do
            begin
              BoundIndex := ColIndex -1;
              frameTimeDependent.Grid.Cells[ColIndex, RowIndex] :=
                AnItem.BoundaryFormula[BoundIndex];
            end;
          end;
        end
        else
        begin
          frameTimeDependent.seNumber.AsInteger := 0;
        end;
      end;
      for ScreenObjectIndex := 1 to ListOfScreenObjects.Count - 1 do
      begin
        ABoundary := ListOfScreenObjects[ScreenObjectIndex].ModflowCfpFixedHeads;
        if edFixedHead.Text <> ABoundary.FixedHead then
        begin
          edFixedHead.Text := ''
        end;
        if edValue2.Text <> ABoundary.Value2 then
        begin
          edValue2.Text := '';
          FValue2Set := False;
        end;
        if edValue3.Text <> ABoundary.Value3 then
        begin
          edValue3.Text := '';
          FValue3Set := False;
        end;
        if rgBoundaryType.ItemIndex <> Ord(ABoundary.BoundaryType) then
        begin
          rgBoundaryType.ItemIndex := -1;
        end;
        if cbTimeDependent.State <> cbGrayed then
        begin
          if ABoundary.TimeDependent then
          begin
            if not FirstTimeDependentBoundary.Values.IsSame(ABoundary.Values) then
            begin
              ClearGrid(frameTimeDependent.Grid);
              FTransientGridCleared := True;
            end;
          end;
          if cbTimeDependent.Checked <> ABoundary.TimeDependent then
          begin
            cbTimeDependent.AllowGrayed := True;
            cbTimeDependent.State := cbGrayed
          end;
          if not FTransientGridCleared and not ABoundary.TimeDependent then
          begin
            frameTimeDependent.seNumber.AsInteger := 0;
          end;
        end;
      end;
    finally
      ListOfScreenObjects.Free;
    end;
  finally
    Changing := False;
  end;
end;

procedure TframeScreenObjectCfpFixed.InitializeControls;
begin
  edFixedHead.Text := '';
{$IFDEF OWHMV2}
  ClearGrid(frameTimeDependent.Grid);
  frameTimeDependent.seNumber.AsInteger := 0;
  if IGlobalModel.ModelSelection = msModflowOwhm2 then
  begin
    rgBoundaryType.Enabled := True;
    pcCfp.Enabled := True;
    cbTimeDependent.Enabled := True;
  end
  else
{$ENDIF}
  begin
    rgBoundaryType.Enabled := False;
    pcCfp.Enabled := False;
    cbTimeDependent.Enabled := False;
  end;
  cbTimeDependent.Checked := False;
  cbTimeDependentClick(nil);
  rgBoundaryType.ItemIndex := 0;
end;

procedure TframeScreenObjectCfpFixed.rgBoundaryTypeClick(Sender: TObject);
var
  ColIndex: Integer;
  AColumn: TRbwColumn4;
begin
  frameTimeDependent.Grid.BeginUpdate;
  try
    if rgBoundaryType.ItemIndex >= 0 then
    begin
      pcCfp.Enabled := True;
      case rgBoundaryType.ItemIndex of
        0:  // fixed head
          begin
            edFixedHead.EditLabel.Caption := StrDefinedHead;
            edFixedHead.Enabled := True;
            btnFixedHead.Enabled := True;

            lblValue2.Enabled := True;
            edValue2.Enabled := True;
            btnValue2.Enabled := True;
            lblValue2.Caption := StrLimitedFlowValue;

            lblValue3.Enabled := False;
            edValue3.Enabled := False;
            btnValue3.Enabled := False;

            frameTimeDependent.Grid.ColCount := 2;
            frameTimeDependent.Grid.Cells[Ord(ccTime),0] := StrTime;
            frameTimeDependent.Grid.Cells[Ord(ccHead),0] := StrDefinedHead;
//            frameTimeDependent.Grid.Cells[Ord(ccValue1),0] := StrLimitedFlowValue;
          end;
        1:  // well
          begin
            edFixedHead.EditLabel.Caption := StrDefinedDischarge;
            edFixedHead.Enabled := True;
            btnFixedHead.Enabled := True;

//            lblValue2.Caption := StrCellToWellConduct;
            lblValue2.Enabled := False;
            edValue2.Enabled := False;
            btnValue2.Enabled := False;

            lblValue3.Enabled := False;
            edValue3.Enabled := False;
            btnValue3.Enabled := False;

            frameTimeDependent.Grid.ColCount := 2;
            frameTimeDependent.Grid.Cells[Ord(ccTime),0] := StrTime;
            frameTimeDependent.Grid.Cells[Ord(ccHead),0] := StrDefinedDischarge;
//            frameTimeDependent.Grid.Cells[Ord(ccValue1),0] := StrCellToWellConduct;
          end;
        2:  // Cauchy
          begin
            edFixedHead.EditLabel.Caption := StrRobinCauchyHead;
            edFixedHead.Enabled := True;
            btnFixedHead.Enabled := True;

            lblValue2.Enabled := True;
            edValue2.Enabled := True;
            btnValue2.Enabled := True;
            lblValue2.Caption := StrRobinCauchyCondu;

            lblValue3.Enabled := True;
            edValue3.Enabled := True;
            btnValue3.Enabled := True;

            frameTimeDependent.Grid.ColCount := 2;
            frameTimeDependent.Grid.Cells[Ord(ccTime),0] := StrTime;
            frameTimeDependent.Grid.Cells[Ord(ccHead),0] := StrRobinCauchyHead;
//            frameTimeDependent.Grid.Cells[Ord(ccValue1),0] := StrCellToWellConduct;
//            frameTimeDependent.Grid.Cells[Ord(ccValue2),0] := StrCauchyLimitedInflo;
          end;
        3:  // limited head
          begin
            edFixedHead.EditLabel.Caption := StrLimitedHead;
            edFixedHead.Enabled := True;
            btnFixedHead.Enabled := True;

            lblValue2.Enabled := False;
            edValue2.Enabled := False;
            btnValue2.Enabled := False;

            lblValue3.Enabled := False;
            edValue3.Enabled := False;
            btnValue3.Enabled := False;

            frameTimeDependent.Grid.ColCount := 2;
            frameTimeDependent.Grid.Cells[Ord(ccTime),0] := StrTime;
            frameTimeDependent.Grid.Cells[Ord(ccHead),0] := StrLimitedHead;
          end;
        else
          begin
            ClearGrid(frameTimeDependent.Grid);
            FTransientGridCleared := True;
          end;
       end;
    end
    else
    begin
      pcCfp.Enabled := False;
      edFixedHead.Enabled := False;
      btnFixedHead.Enabled := False;
    end;

    AColumn := frameTimeDependent.Grid.Columns[Ord(ccTime)];
    AColumn.Format := rcf4Real;

    for ColIndex := Ord(ccHead) to frameTimeDependent.Grid.ColCount - 1 do
    begin
      AColumn := frameTimeDependent.Grid.Columns[ColIndex];
      AColumn.ButtonUsed := True;
      AColumn.ButtonCaption := 'F()';
      AColumn.ButtonWidth := 35;
      AColumn.WordWrapCaptions := True;
      AColumn.AutoAdjustCaptionRowHeights := True;
      AColumn.AutoAdjustRowHeights := True;
      AColumn.AutoAdjustColWidths := True;
      AColumn.Format := rcf4String;
    end;
//  TCfpColumns = (ccTime, ccHead, ccValue1, ccValue2);


  finally
    frameTimeDependent.Grid.EndUpdate;
  end;
  DoChange;
end;

procedure TframeScreenObjectCfpFixed.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  ScreenObjectIndex: Integer;
  Item: TScreenObjectEditItem;
  ScreenObject: TScreenObject;
  Boundary: TCfpFixedBoundary;
  BoundaryUsed: Boolean;
  TransientData: TCustomMF_BoundColl;
  TimeIndex: Integer;
  AnItem: TCustomBoundaryItem;
  RowIndex: Integer;
  ColIndex: Integer;
  BoundIndex: Integer;
begin
  for ScreenObjectIndex := 0 to List.Count - 1 do
  begin
    Item := List[ScreenObjectIndex];
    ScreenObject := Item.ScreenObject;
    Boundary := ScreenObject.ModflowCfpFixedHeads;
    BoundaryUsed := (Boundary <> nil) and Boundary.Used;
    if ClearAll then
    begin
      if BoundaryUsed then
      begin
        Boundary.IsUsed := False;
      end;
    end
    else if SetAll or BoundaryUsed then
    begin
      if Boundary = nil then
      begin
        ScreenObject.CreateCfpFixedHeads;
        Boundary := ScreenObject.ModflowCfpFixedHeads;
      end;
      if Boundary <> nil then
      begin
        Boundary.IsUsed := True;
        if edFixedHead.Text <> '' then
        begin
          Boundary.FixedHead := edFixedHead.Text;
        end;
        if FValue2Set then
        begin
          Boundary.Value2 := edValue2.Text;
        end;
        if FValue3Set then
        begin
          Boundary.Value3 := edValue3.Text;
        end;
        if rgBoundaryType.ItemIndex >= 0 then
        Begin
          Boundary.BoundaryType := TCfpBoundaryType(rgBoundaryType.ItemIndex);
        end;
        if cbTimeDependent.State <> cbGrayed then
        begin
          Boundary.TimeDependent := cbTimeDependent.Checked;
          if not FTransientGridCleared then
          begin
            TransientData := Boundary.Values;
            TransientData.Count := frameTimeDependent.seNumber.AsInteger;
            for TimeIndex := 0 to frameTimeDependent.seNumber.AsInteger - 1 do
            begin
              RowIndex := TimeIndex + 1;
              AnItem := Boundary.Values[TimeIndex];
              AnItem.StartTime := frameTimeDependent.Grid.RealValueDefault[Ord(ccTime), RowIndex, 0];
              for ColIndex := 1 to frameTimeDependent.Grid.ColCount - 1 do
              begin
                BoundIndex := ColIndex -1;
                AnItem.BoundaryFormula[BoundIndex] :=
                  frameTimeDependent.Grid.Cells[ColIndex, RowIndex];
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TframeScreenObjectCfpFixed.StringGrid1SetEditText(Sender: TObject;
    ACol, ARow: Integer; const Value: string);
begin
  inherited;
  DoChange;
end;

end.
