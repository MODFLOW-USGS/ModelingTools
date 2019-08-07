unit frameLocationMethodUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, 
  Dialogs, ArgusDataEntry, ComCtrls, JvExComCtrls, JvComCtrls, StdCtrls,
  JvExStdCtrls, JvCombobox, JvListComb, UndoItemsScreenObjects;

type
  TframeLocationMethod = class(TFrame)
    lblLocationMethod: TLabel;
    pcLocationChoice: TJvPageControl;
    tabNone: TTabSheet;
    tabObject: TTabSheet;
    comboObject: TComboBox;
    tabLocation: TTabSheet;
    lblX: TLabel;
    lblY: TLabel;
    lblZ: TLabel;
    rdeX: TRbwDataEntry;
    rdeY: TRbwDataEntry;
    rdeZ: TRbwDataEntry;
    tabCell: TTabSheet;
    lblCol: TLabel;
    lblRow: TLabel;
    lblLay: TLabel;
    rdeLay: TRbwDataEntry;
    rdeRow: TRbwDataEntry;
    rdeCol: TRbwDataEntry;
    comboLocationChoice: TJvImageComboBox;
    procedure comboLocationChoiceChange(Sender: TObject);
    procedure ControlChange(Sender: TObject);
  private
    FOnChange: TNotifyEvent;
    FChanging: Boolean;
    procedure Changed;
    procedure InitializeControls(var TargetObjects: TStringList);
    procedure FillListWithMnwBoundaries(ScreenObjectList: TScreenObjectEditCollection; LocalList: TList);
  protected
    function GetEnabled: boolean; override;
    procedure SetEnabled(Value: boolean); override;
    { Private declarations }
  public
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    procedure GetData(ScreenObjectList: TScreenObjectEditCollection);
    procedure SetData(List: TScreenObjectEditCollection; SetAll,
      ClearAll: boolean);
    property Changing: Boolean read FChanging write FChanging;
    { Public declarations }
  end;

implementation

uses
  ScreenObjectUnit, ModflowMNW2_WriterUnit, ModflowMnw2Unit, frmGoPhastUnit,
  GoPhastTypes;

{$R *.dfm}

procedure TframeLocationMethod.Changed;
begin
  if FChanging then
  begin
    Exit;
  end;
  FChanging := True;
  try
    if Assigned(OnChange) then
    begin
      OnChange(self);
    end;
  finally
    FChanging := False;
  end;
end;

procedure TframeLocationMethod.comboLocationChoiceChange(Sender: TObject);
begin
  if comboLocationChoice.ItemIndex >= 0 then
  begin
    pcLocationChoice.ActivePageIndex :=
      comboLocationChoice.ItemIndex;
  end
  else
  begin
    pcLocationChoice.ActivePageIndex := 0;
  end;
  Changed;
end;

procedure TframeLocationMethod.ControlChange(Sender: TObject);
begin
  Changed;
end;

procedure TframeLocationMethod.GetData(
  ScreenObjectList: TScreenObjectEditCollection);
var
  LocalList: TList;
  Index: Integer;
  Mnw2Boundary: TMnw2Boundary;
  TargetCell: TTargetCell;
  TargetLocation: TTargetLocation;
  TargetObject: TTargetObject;
  TargetObjects: TStringList;
begin
  TargetObjects:= TStringList.Create;
  try
    InitializeControls(TargetObjects);

    LocalList := TList.Create;
    try
      FillListWithMnwBoundaries(ScreenObjectList, LocalList);

      if LocalList.Count = 0 then
      begin
        comboLocationChoice.ItemIndex := 0;
      end
      else
      begin
        Mnw2Boundary := LocalList[0];

        comboLocationChoice.ItemIndex :=
          Ord(Mnw2Boundary.PumpCellTarget.TargetType);
        comboLocationChoiceChange(nil);
        case Mnw2Boundary.PumpCellTarget.TargetType of
          ttNone: ; // do nothing
          ttCell:
            begin
              TargetCell := Mnw2Boundary.PumpCellTarget.TargetCell;
              rdeCol.Text := IntToStr(TargetCell.Col);
              rdeRow.Text := IntToStr(TargetCell.Row);
              rdeLay.Text := IntToStr(TargetCell.Lay);
            end;
          ttLocation:
            begin
              TargetLocation := Mnw2Boundary.PumpCellTarget.TargetLocation;
              rdeX.Text := FloatToStr(TargetLocation.X);
              rdeY.Text := FloatToStr(TargetLocation.Y);
              rdeZ.Text := FloatToStr(TargetLocation.Z);
            end;
          ttObject:
            begin
              TargetObject := Mnw2Boundary.PumpCellTarget.TargetObject;
              comboObject.ItemIndex :=
                TargetObjects.IndexOf(TargetObject.ObjectName)
            end;
          else Assert(False);
        end;
        for Index := 1 to LocalList.Count - 1 do
        begin
          Mnw2Boundary := LocalList[Index];

          if comboLocationChoice.ItemIndex <>
            Ord(Mnw2Boundary.PumpCellTarget.TargetType) then
          begin
            comboLocationChoice.ItemIndex := -1;
            comboLocationChoiceChange(nil);
            break;
          end;

          case Mnw2Boundary.PumpCellTarget.TargetType of
            ttNone: ; // do nothing
            ttCell:
              begin
                TargetCell := Mnw2Boundary.PumpCellTarget.TargetCell;
                if rdeCol.Text <> IntToStr(TargetCell.Col) then
                begin
                  rdeCol.Text := '';
                end;
                if rdeRow.Text <> IntToStr(TargetCell.Row) then
                begin
                  rdeRow.Text := '';
                end;
                if rdeLay.Text <> IntToStr(TargetCell.Lay) then
                begin
                  rdeLay.Text := '';
                end;
              end;
            ttLocation:
              begin
                TargetLocation := Mnw2Boundary.PumpCellTarget.TargetLocation;
                if rdeX.Text <> FloatToStr(TargetLocation.X) then
                begin
                  rdeX.Text := '';
                end;
                if rdeY.Text <> FloatToStr(TargetLocation.Y) then
                begin
                  rdeY.Text := '';
                end;
                if rdeZ.Text <> FloatToStr(TargetLocation.Z) then
                begin
                  rdeZ.Text := '';
                end;
              end;
            ttObject:
              begin
                TargetObject := Mnw2Boundary.PumpCellTarget.TargetObject;
                if comboObject.ItemIndex <>
                  TargetObjects.IndexOf(TargetObject.ObjectName) then
                begin
                  comboObject.ItemIndex := -1;
                end;
              end;
            else Assert(False);
          end;
        end;
      end;
    finally
      LocalList.Free
    end;
  finally
    TargetObjects.Free;
  end;
end;

function TframeLocationMethod.GetEnabled: boolean;
begin
  result := inherited GetEnabled;
end;

procedure TframeLocationMethod.SetData(List: TScreenObjectEditCollection;
  SetAll, ClearAll: boolean);
var
  Index: Integer;
  Item: TScreenObjectEditItem;
  AScreenObject: TScreenObject;
  LocalList: TList;
  Mnw2Boundary: TMnw2Boundary;
  TargetCell: TTargetCell;
  TargetLocation: TTargetLocation;
  TargetObject: TTargetObject;
begin
  if comboLocationChoice.ItemIndex <= 0 then
  begin
    Exit;
  end;
  LocalList := TList.Create;
  try

    for Index := 0 to List.Count - 1 do
    begin
      Item := List[Index];
      AScreenObject := Item.ScreenObject;
      if not TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
      begin
        if SetAll and (AScreenObject.ModflowMnw2Boundary = nil) then
        begin
          AScreenObject.CreateMnw2Boundary;
        end;
        if (AScreenObject.ModflowMnw2Boundary <> nil) then
        begin
          LocalList.Add(AScreenObject.ModflowMnw2Boundary);
        end;
      end;
    end;
    for Index := 0 to LocalList.Count - 1 do
    begin
      Mnw2Boundary := LocalList[Index];
      Mnw2Boundary.PumpCellTarget.TargetType :=
        TTargetType(comboLocationChoice.ItemIndex);
      case Mnw2Boundary.PumpCellTarget.TargetType of
        ttNone: Assert(False);
        ttCell:
          begin
            TargetCell := Mnw2Boundary.PumpCellTarget.TargetCell;
            if rdeCol.Text <> '' then
            begin
              TargetCell.Col := StrToInt(rdeCol.Text);
            end;
            if rdeRow.Text <> '' then
            begin
              TargetCell.Row := StrToInt(rdeRow.Text);
            end;
            if rdeLay.Text <> '' then
            begin
              TargetCell.Lay := StrToInt(rdeLay.Text);
            end;
          end;
        ttLocation:
          begin
            TargetLocation := Mnw2Boundary.PumpCellTarget.TargetLocation;
            if rdeX.Text <> '' then
            begin
              TargetLocation.X := StrToFloat(rdeX.Text);
            end;
            if rdeY.Text <> '' then
            begin
              TargetLocation.Y := StrToFloat(rdeY.Text);
            end;
            if rdeZ.Text <> '' then
            begin
              TargetLocation.Z := StrToFloat(rdeZ.Text);
            end;
          end;
        ttObject:
          begin
            TargetObject := Mnw2Boundary.PumpCellTarget.TargetObject;
            if comboObject.ItemIndex >= 0 then
            begin
              TargetObject.ScreenObject :=
                comboObject.Items.Objects[comboObject.ItemIndex];
            end;
          end;
        else Assert(False);
      end;
    end;
  finally
    LocalList.Free;
  end;
end;

procedure TframeLocationMethod.FillListWithMnwBoundaries(ScreenObjectList: TScreenObjectEditCollection; LocalList: TList);
var
  AScreenObject: TScreenObject;
  Item: TScreenObjectEditItem;
  Index: Integer;
begin
  for Index := 0 to ScreenObjectList.Count - 1 do
  begin
    Item := ScreenObjectList[Index];
    AScreenObject := Item.ScreenObject;
    if (AScreenObject.ModflowMnw2Boundary <> nil)
      and (AScreenObject.ModflowMnw2Boundary.Used)
      and not TMultinodeWell.IsScreenObjectVertical(AScreenObject) then
    begin
      LocalList.Add(AScreenObject.ModflowMnw2Boundary);
    end;
  end;
end;

procedure TframeLocationMethod.InitializeControls(var TargetObjects: TStringList);
var
  AScreenObject: TScreenObject;
  Index: Integer;
begin
  comboObject.Items.Clear;
  for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
  begin
    AScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
    if not AScreenObject.Deleted
      and (AScreenObject.Count = 1)
      and (AScreenObject.ElevationCount = ecOne) then
    begin
      comboObject.Items.AddObject(AScreenObject.Name, AScreenObject);
    end;
  end;
  TargetObjects.Assign(comboObject.Items);
  TargetObjects.CaseSensitive := False;
  comboLocationChoice.ItemIndex := 0;
  comboObject.ItemIndex := -1;
  rdeCol.Text := '';
  rdeRow.Text := '';
  rdeLay.Text := '';
  rdeX.Text := '';
  rdeY.Text := '';
  rdeZ.Text := '';
end;

procedure TframeLocationMethod.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  if comboLocationChoice = nil then
  begin
    Exit;
  end;
  comboLocationChoice.Enabled := Value;
  if Value then
  begin
    comboLocationChoice.Font.Color := clMenuText;
  end
  else
  begin
    comboLocationChoice.Font.Color := clScrollBar;
    if comboLocationChoice.ItemIndex <> 0 then
    begin
      FChanging := True;
      try
        comboLocationChoice.ItemIndex := 0;
        pcLocationChoice.ActivePageIndex := 0;
      finally
        FChanging := False;
      end;
    end;
  end;
end;

end.
