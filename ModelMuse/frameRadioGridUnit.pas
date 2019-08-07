unit frameRadioGridUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, Grids, RbwDataGrid4,
  StdCtrls, MMJLabel;

type
  TframeRadioGrid = class(TFrame)
    grpDescription: TGroupBox;
    rdgGrid: TRbwDataGrid4;
    lblTop: TLabel;
    lblLeft: TMMJLabel;
    procedure rdgGridStateChange(Sender: TObject; ACol, ARow: Integer;
      const Value: TCheckBoxState);
  private
    FColItemIndex: Integer;
    FRowItemIndex: Integer;
    FChanging: boolean;
    procedure SetColItemIndex(const Value: Integer);
    procedure SetRowItemIndex(const Value: Integer);
    procedure SetChecked;
    { Private declarations }
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    property RowItemIndex: Integer read FRowItemIndex write SetRowItemIndex;
    property ColItemIndex: Integer read FColItemIndex write SetColItemIndex;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrame3 }

procedure TframeRadioGrid.rdgGridStateChange(Sender: TObject; ACol,
  ARow: Integer; const Value: TCheckBoxState);
begin
  if Value = cbChecked then
  begin
    FChanging := True;
    try
      ColItemIndex := ACol;
      RowItemIndex := ARow;
    finally
      FChanging := False;
    end;
    SetChecked;
  end;
end;

procedure TframeRadioGrid.SetChecked;
var
  RowIndex: Integer;
  ColIndex: Integer;
begin
  if FChanging then
  begin
    Exit;
  end;
  FChanging := true;
  try
    for RowIndex := 0 to rdgGrid.RowCount - 1 do
    begin
      for ColIndex := 0 to rdgGrid.ColCount - 1 do
      begin
        rdgGrid.Checked[ColIndex,RowIndex] :=
          (ColIndex = FColItemIndex) and (RowIndex = FRowItemIndex);
      end;
    end;
  finally
    FChanging := False;
  end;
end;

procedure TframeRadioGrid.SetColItemIndex(const Value: Integer);
begin
  FColItemIndex := Value;
  SetChecked;
end;

procedure TframeRadioGrid.SetEnabled(Value: Boolean);
begin
  inherited;// Enabled := Value;
  if grpDescription <> nil then
  begin
    grpDescription.Enabled := Value;
  end;
  if rdgGrid <> nil then
  begin
    rdgGrid.Enabled := Value;
  end;
end;

procedure TframeRadioGrid.SetRowItemIndex(const Value: Integer);
begin
  FRowItemIndex := Value;
  SetChecked;
end;

end.
