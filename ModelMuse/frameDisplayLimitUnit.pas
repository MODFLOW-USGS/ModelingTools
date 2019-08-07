{@abstract(@name is used to define
  @link(TframeDisplayLimit) which is used to edit
   TDataArray.Limits.@link(DataSetUnit.TColoringLimits.LowerLimit)
   or @link(DataSetUnit.TColoringLimits.UpperLimit).)
   See @link(frmDisplayDataUnit).}

unit frameDisplayLimitUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, RbwParser, DataSetUnit, ArgusDataEntry;

type
  {@abstract(@name is used to edit
   TDataArray.Limits.@link(DataSetUnit.TColoringLimits.LowerLimit)
   or @link(DataSetUnit.TColoringLimits.UpperLimit).)
   See @link(frmDisplayDataUnit).}
  TframeDisplayLimit = class(TFrame)
    // @name is used to edit TColoringLimit.@link(TColoringLimit.UseLimit).
    cbCheck: TCheckBox;
    // @name is used to edit
    // @link(TColoringLimit.RealLimitValue),
    // @link(TColoringLimit.IntegerLimitValue), or
    // @link(TColoringLimit.StringLimitValue).
    rdeLimit: TRbwDataEntry;
    // @name is used to edit
    // @link(TColoringLimit.BooleanLimitValue),
    comboBoolLimit: TComboBox;
    // @name is used to set
    // @link(TColoringLimit.RealLimitValue),
    // @link(TColoringLimit.IntegerLimitValue), or
    // @link(TColoringLimit.StringLimitValue) when exiting from the
    // @link(rdeLimit) control.
    procedure rdeLimitExit(Sender: TObject);
    // @name is used to respond to the user clicking on the @link(cbCheck)
    // check box.
    procedure cbCheckClick(Sender: TObject);
    // @name is used to set
    // @link(TColoringLimit.BooleanLimitValue),
    // when exiting from the
    // @link(comboBoolLimit) control.
    procedure comboBoolLimitChange(Sender: TObject);
  private
    // See @link(DataType).
    FDataType: TRbwDataType;
    // See @link(Limit).
    FLimit: TColoringLimit;
    FAssigningLimit: Boolean;
    // See @link(DataType).
    procedure SetDataType(const Value: TRbwDataType);
    // See @link(Limit).
    procedure SetLimit(const Value: TColoringLimit);
    { Private declarations }
  protected
    // @name sets the enabled property of @link(cbCheck)
    // and calls @link(cbCheckClick).
    procedure SetEnabled(Value: Boolean); override;
  public
    function OkLogLimit: boolean;
    // @name creates an instance of @classname.
    constructor Create(AOwner: TComponent); override;
    // @name specifies the type of data limited by whether @link(FLimit).
    property DataType: TRbwDataType read FDataType write SetDataType;
    // @name destroys the current instance of @classname.
    // Do not call @name directly.  Call Free instead.
    destructor Destroy; override;
    // @name is the @link(TColoringLimit) that this @classname is being
    // used to edit.
    property Limit: TColoringLimit read FLimit write SetLimit;
    { Public declarations }
  end;

implementation

{$R *.dfm}

{ TFrame1 }

procedure TframeDisplayLimit.SetDataType(const Value: TRbwDataType);
begin
  FLimit.DataType := Value;
  FDataType := Value;
  // The following need to be set even if FDataType has not changed.
  case FDataType of
    rdtDouble:
      begin
        rdeLimit.DataType := dtReal;
        comboBoolLimit.Visible := False;
        rdeLimit.Visible := True;
      end;
    rdtInteger:
      begin
        rdeLimit.DataType := dtInteger;
        comboBoolLimit.Visible := False;
        rdeLimit.Visible := True;
      end;
    rdtBoolean:
      begin
        rdeLimit.Visible := False;
        comboBoolLimit.Visible := True;
      end;
    rdtString:
      begin
        rdeLimit.DataType := dtString;
        comboBoolLimit.Visible := False;
        rdeLimit.Visible := True;
      end;
  else
    Assert(False);
  end;
end;

procedure TframeDisplayLimit.cbCheckClick(Sender: TObject);
begin
  FLimit.UseLimit := cbCheck.Checked and cbCheck.Enabled;
  rdeLimit.Enabled := rdeLimit.Visible and cbCheck.Checked and cbCheck.Enabled;
  comboBoolLimit.Enabled := comboBoolLimit.Visible and cbCheck.Checked
    and cbCheck.Enabled;
  if FLimit.UseLimit then
  begin
    if rdeLimit.Enabled then
    begin
      rdeLimitExit(rdeLimit);
    end
    else if comboBoolLimit.Enabled then
    begin
      comboBoolLimitChange(comboBoolLimit);
    end;
  end;
end;

procedure TframeDisplayLimit.SetEnabled(Value: Boolean);
begin
  inherited;
  cbCheck.Enabled := Value;
  cbCheckClick(nil);
  if not cbCheck.Enabled then
  begin
    rdeLimit.Visible := True;
    comboBoolLimit.Visible := False;
  end;
end;

procedure TframeDisplayLimit.SetLimit(const Value: TColoringLimit);
begin
  FAssigningLimit := True;
  try
    FLimit.Assign(Value);
    DataType := Value.DataType;
    cbCheck.Checked := Value.UseLimit;
    case DataType of
      rdtDouble:
        begin
          rdeLimit.Text := FloatToStr(Value.RealLimitValue);
        end;
      rdtInteger:
        begin
          rdeLimit.Text := IntToStr(Value.IntegerLimitValue);
        end;
      rdtBoolean:
        begin
          if Value.BooleanLimitValue then
          begin
            comboBoolLimit.ItemIndex := 1;
          end
          else
          begin
            comboBoolLimit.ItemIndex := 0;
          end;
        end;
      rdtString:
        begin
          rdeLimit.Text := Value.StringLimitValue;
        end;
    else
      Assert(False);
    end;
  finally
    FAssigningLimit := False;
  end;
end;

constructor TframeDisplayLimit.Create(AOwner: TComponent);
begin
  inherited;
  FLimit := TColoringLimit.Create;
end;

destructor TframeDisplayLimit.Destroy;
begin
  FLimit.Free;
  inherited;
end;

function TframeDisplayLimit.OkLogLimit: boolean;
begin
  result := FLimit.OkLogLimit;
end;

procedure TframeDisplayLimit.rdeLimitExit(Sender: TObject);
var
  RealValue: double;
  IntValue: Integer;
begin
  if not FLimit.UseLimit or FAssigningLimit then
  begin
    Exit;
  end;
  case Limit.DataType of
    rdtDouble:
      begin
        if TryStrToFloat(rdeLimit.Text, RealValue) then
        begin
          Limit.RealLimitValue := RealValue
        end
        else
        begin
          FLimit.UseLimit := false;
        end;
      end;
    rdtInteger:
      begin
        if TryStrToInt(rdeLimit.Text, IntValue) then
        begin
          Limit.IntegerLimitValue := IntValue;
        end
        else
        begin
          FLimit.UseLimit := false;
        end;
      end;
    rdtString:
      begin
        Limit.StringLimitValue := rdeLimit.Text
      end;
    rdtBoolean:
      begin
        // do nothing
      end;
  else
    Assert(False);
  end;
end;

procedure TframeDisplayLimit.comboBoolLimitChange(Sender: TObject);
begin
  if  FAssigningLimit then
  begin
    Exit;
  end;
  Limit.BooleanLimitValue := (comboBoolLimit.ItemIndex = 1);
end;

end.
