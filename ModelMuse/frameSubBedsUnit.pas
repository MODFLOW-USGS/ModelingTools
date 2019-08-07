unit frameSubBedsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, Buttons, ExtCtrls, StdCtrls, Mask, JvExMask, JvSpin, Grids,
  RbwDataGrid4, ModflowSubsidenceDefUnit;

type
  TSubColumns = (scName, scUseAll, fcFirst);

  TGetSelectedSubLayers = procedure (Sender: TObject;
    var SubLayers: TCustomSubLayer) of object;

  TGetNewName = procedure (Sender: TObject;
    var NewLayerName: string) of object;

  TframeSubBeds = class(TFrame)
    rdgSubBed: TRbwDataGrid4;
    seCount: TJvSpinEdit;
    lblCount: TLabel;
    sbAdd: TSpeedButton;
    sbInsert: TSpeedButton;
    sbDelete: TSpeedButton;
    procedure seCountChange(Sender: TObject);
    procedure sbAddClick(Sender: TObject);
    procedure sbInsertClick(Sender: TObject);
    procedure sbDeleteClick(Sender: TObject);
    procedure rdgSubBedSelectCell(Sender: TObject; ACol, ARow: Integer;
      var CanSelect: Boolean);
    procedure seCountEnter(Sender: TObject);
    procedure rdgSubBedSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure rdgSubBedStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
  private
    FOldCount: Integer;
    FOnGetSelectedSubLayers: TGetSelectedSubLayers;
    FOnGetNewName: TGetNewName;
    FSettingUseAll: Boolean;
    procedure AssignBooleanProperties(const Value: TCheckBoxState;
      SubLayers: TCustomSubLayer; ACol: Integer; ARow: Integer);
    { Private declarations }
    procedure AssignSubLayerName(SubLayers: TCustomSubLayer;
      const Value: string; ARow: Integer);
  public
    property OnGetSelectedSubLayers: TGetSelectedSubLayers
      read FOnGetSelectedSubLayers write FOnGetSelectedSubLayers;
    property OnGetNewName: TGetNewName read FOnGetNewName
      write FOnGetNewName;
    { Public declarations }
  end;

implementation

{$R *.dfm}

procedure TframeSubBeds.AssignBooleanProperties(const Value: TCheckBoxState;
  SubLayers: TCustomSubLayer; ACol, ARow: Integer);
var
  UseLayerItem: TUseLayerNumberItem;
  ColIndex: Integer;
  Item: TCustomSubLayerItem;
begin
  if SubLayers.Count = 0 then
  begin
    Exit;
  end;
  Item := SubLayers.Items[ARow - 1] as TCustomSubLayerItem;
  if ACol = Ord(scUseAll) then
  begin
    FSettingUseAll := True;
    try
      Item.UseInAllLayers := Value = cbChecked;
      if Item.UseInAllLayers then
      begin
        for ColIndex := Ord(fcFirst) to rdgSubBed.ColCount - 1 do
        begin
          rdgSubBed.State[ColIndex, ARow] := cbChecked;
        end;
      end;
      for ColIndex := Ord(fcFirst) to rdgSubBed.ColCount - 1 do
      begin
        if rdgSubBed.Checked[ColIndex, ARow] then
        begin
          UseLayerItem := Item.UsedLayers.GetItemByLayerNumber(ColIndex - 1);
          if UseLayerItem = nil then
          begin
            UseLayerItem := Item.UsedLayers.Add;
            UseLayerItem.LayerNumber := ColIndex - 1;
          end;
        end
        else
        begin
          UseLayerItem := Item.UsedLayers.GetItemByLayerNumber(ColIndex - 1);
          UseLayerItem.Free;
        end;
      end;
    finally
      FSettingUseAll := False;
    end;
  end
  else
  begin
    if Value = cbChecked then
    begin
      UseLayerItem := Item.UsedLayers.GetItemByLayerNumber(ACol - 1);
      if UseLayerItem = nil then
      begin
        UseLayerItem := Item.UsedLayers.Add;
        UseLayerItem.LayerNumber := ACol - 1;
      end;
      for ColIndex := Ord(fcFirst) to rdgSubBed.ColCount - 1 do
      begin
        UseLayerItem := Item.UsedLayers.GetItemByLayerNumber(ColIndex - 1);
        if UseLayerItem = nil then
        begin
          break;
        end;
      end;
      if not FSettingUseAll then
      begin
        if UseLayerItem = nil then
        begin
          rdgSubBed.State[Ord(scUseAll), ARow] := cbUnchecked;
        end
        else
        begin
          rdgSubBed.State[Ord(scUseAll), ARow] := cbChecked;
        end;
      end;
    end
    else
    begin
      UseLayerItem := Item.UsedLayers.GetItemByLayerNumber(ACol - 1);
      UseLayerItem.Free;
      if not FSettingUseAll then
      begin
        rdgSubBed.State[Ord(scUseAll), ARow] := cbUnchecked;
      end;
    end;
  end;
end;

procedure TframeSubBeds.AssignSubLayerName(SubLayers: TCustomSubLayer;
  const Value: string; ARow: Integer);
var
  Item: TCustomSubLayerItem;
begin
  if ARow - 1 < SubLayers.Count then
  begin
    Item := SubLayers.Items[ARow - 1] as TCustomSubLayerItem;
    Item.Name := Value;
    if rdgSubBed.ColCount = 1 then
    begin
      Item.UseInAllLayers := True;
    end;
  end;
end;

procedure TframeSubBeds.rdgSubBedSelectCell(Sender: TObject; ACol,
  ARow: Integer; var CanSelect: Boolean);
begin
  if ARow >= rdgSubBed.FixedRows then
  begin
    CanSelect := seCount.AsInteger >= 1
  end;
end;

procedure TframeSubBeds.rdgSubBedSetEditText(Sender: TObject; ACol,
  ARow: Integer; const Value: string);
var
  SubLayers: TCustomSubLayer;
begin
  if (ARow >= 1) and (ACol = Ord(scName)) then
  begin
    if Assigned(OnGetSelectedSubLayers) then
    begin
      OnGetSelectedSubLayers(self, SubLayers);
      if SubLayers <> nil then
      begin
        if (ARow  = 1) and (Value <> '') and (seCount.AsInteger = 0) then
        begin
          seCount.AsInteger := 1;
          seCountChange(nil);
        end;
        AssignSubLayerName(SubLayers, Value, ARow);
      end;
    end;
  end;
end;

procedure TframeSubBeds.rdgSubBedStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
var
  SubLayers: TCustomSubLayer;
begin
  if (ARow >= 1) and (ACol >= Ord(scUseAll)) then
  begin
    if Assigned(OnGetSelectedSubLayers) then
    begin
      OnGetSelectedSubLayers(self, SubLayers);
      if SubLayers <> nil then
      begin
        AssignBooleanProperties(Value, SubLayers, ACol, ARow);
      end;
    end;
  end;
end;

procedure TframeSubBeds.sbAddClick(Sender: TObject);
var
  SubLayers: TCustomSubLayer;
  Item: TCollectionItem;
begin
  seCount.AsInteger := seCount.AsInteger+1;
  seCountChange(Sender);
  if Assigned(OnGetSelectedSubLayers) then
  begin
    OnGetSelectedSubLayers(self, SubLayers);
    if SubLayers <> nil then
    begin
      Item := SubLayers.Add;
      Item.Index := rdgSubBed.RowCount-2;
    end;
  end;
end;

procedure TframeSubBeds.sbDeleteClick(Sender: TObject);
var
  SubLayers: TCustomSubLayer;
begin
  Assert(seCount.AsInteger > 0);
  if seCount.AsInteger = 1 then
  begin
    seCount.AsInteger := 0;
    seCountChange(Sender);
  end
  else
  begin
    if (rdgSubBed.Row >= 1) and (rdgSubBed.Row < rdgSubBed.RowCount) then
    begin
      rdgSubBed.DeleteRow(rdgSubBed.Row);
      seCount.AsInteger := seCount.AsInteger - 1;
      seCountChange(nil);

      if Assigned(OnGetSelectedSubLayers) then
      begin
        OnGetSelectedSubLayers(self, SubLayers);
        if SubLayers <> nil then
        begin
          SubLayers.Delete(rdgSubBed.Row-1);
        end;
      end;
    end;
  end;
end;

procedure TframeSubBeds.sbInsertClick(Sender: TObject);
var
  SubLayers: TCustomSubLayer;
  Item: TCollectionItem;
begin
  if seCount.AsInteger = 0 then
  begin
    sbAddClick(nil);
  end
  else
  begin
    if (rdgSubBed.Row >= 1) and (rdgSubBed.Row < rdgSubBed.RowCount) then
    begin
      rdgSubBed.InsertRow(rdgSubBed.Row);
      seCount.AsInteger := seCount.AsInteger + 1;
      seCountChange(Sender);
      if Assigned(OnGetSelectedSubLayers) then
      begin
        OnGetSelectedSubLayers(self, SubLayers);
        if SubLayers <> nil then
        begin
          Item := SubLayers.Add;
          Item.Index := rdgSubBed.Row-1;
        end;
      end;
    end;
  end;

end;

procedure TframeSubBeds.seCountChange(Sender: TObject);
var
  NumRows: Integer;
  ColIndex: Integer;
  SubLayers: TCustomSubLayer;
  Item: TCustomSubLayerItem;
  NewName: string;
begin
  NumRows := seCount.AsInteger + 1;
  if NumRows = 1 then
  begin
    NumRows := 2;
  end;
  rdgSubBed.BeginUpdate;
  try
    rdgSubBed.RowCount := NumRows;
    if Sender = seCount then
    begin
      if seCount.AsInteger > FOldCount then
      begin
        While seCount.AsInteger > FOldCount do
        begin
          if Assigned(OnGetSelectedSubLayers) then
          begin
            OnGetSelectedSubLayers(self, SubLayers);
            if SubLayers <> nil then
            begin
              Item := SubLayers.Add as TCustomSubLayerItem;
              Item.Index := FOldCount;
              if Assigned(OnGetNewName) then
              begin
                OnGetNewName(self, NewName);
                Item.Name := NewName;
                rdgSubBed.Cells[Ord(scName), FOldCount+1] := NewName;
              end;
            end;
          end;
          Inc(FOldCount);
        end;
      end
      else
      begin
        while (seCount.AsInteger < FOldCount) do
        begin
          if Assigned(OnGetSelectedSubLayers) then
          begin
            OnGetSelectedSubLayers(self, SubLayers);
            if SubLayers <> nil then
            begin
              SubLayers.Delete(FOldCount-1);
            end;
          end;
          Dec(FOldCount);
        end;
      end;
    end;
    if seCount.AsInteger = 0 then
    begin
      rdgSubBed.Cells[0,1] := '';
      for ColIndex := 1 to rdgSubBed.ColCount - 1 do
      begin
        rdgSubBed.Checked[ColIndex,1] := False;
      end;
    end;
    FOldCount := seCount.AsInteger;

    rdgSubBed.Invalidate;
  finally
    rdgSubBed.EndUpdate;
  end;
end;

procedure TframeSubBeds.seCountEnter(Sender: TObject);
begin
  FOldCount := seCount.AsInteger;
end;

end.
