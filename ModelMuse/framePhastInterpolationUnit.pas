{@abstract(@name is used to define @link(TframePhastInterpolation)
  which is used to provide the user with a standardized method
  of specifying the parameters needed for PHAST-style interpolation.)
  See @link(TPhastInterpolationValues).}
unit framePhastInterpolationUnit;

interface

uses
  SysUtils, Types, Classes, Variants, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, PhastDataSets, ArgusDataEntry, RbwEdit,
  JvExStdCtrls, JvCheckBox;

type
  {@abstract(@name is used to provide the user with a standardized method
    of specifying the parameters needed for PHAST-style interpolation.)
    See @link(TPhastInterpolationValues).}
  TframePhastInterpolation = class(TFrame)
    // @name allows the user to edit the first distance
    // in the PHAST-style interpolation.
    // See @link(TPhastInterpolationValues).
    rdeDistance1: TRbwDataEntry;
    // @name allows the user to edit the second distance
    // in the PHAST-style interpolation.
    // See @link(TPhastInterpolationValues).
    rdeDistance2: TRbwDataEntry;
    // @name allows the user to edit the first value
    // in the PHAST-style interpolation.
    // See @link(TPhastInterpolationValues).
    rdeValue1: TRbwDataEntry;
    // @name allows the user to edit the second value
    // in the PHAST-style interpolation.
    // See @link(TPhastInterpolationValues).
    rdeValue2: TRbwDataEntry;
    // @name is designed to be used to allow the user to edit the
    // formula for mixtures in @link(edMixFormula).  However, no event-handler
    // is assigned to it OnClick event in the @classname.  The event-handler
    // must be assigned where the @classname is used.
    // See @link(TPhastInterpolationValues).
    btnEditMixtureFormula: TButton;
    cbPhastInterpolation: TJvCheckBox;
    // @name allows the user to edit the formula for mixtures.
    // No event-handler is assigned to its OnChange event.  The event-handler
    // must be assigned where the @classname is used.
    // See @link(TPhastInterpolationValues).
    edMixFormula: TRbwEdit;
    // @name labels @link(rdeDistance1).
    lblDistance1: TLabel;
    // @name labels @link(rdeDistance2).
    lblDistance2: TLabel;
    // @name labels @link(edMixFormula).
    lblMixtureFormula: TLabel;
    // @name labels @link(rdeValue1).
    lblValue1: TLabel;
    // @name labels @link(rdeValue2).
    lblValue2: TLabel;
    // @name allows the user to edit the direction used for
    // PHAST-style interpolation or to use a mixture.
    // See @link(TPhastInterpolationValues).
    // See @link(rgInterpolationDirectionClick).
    rgInterpolationDirection: TRadioGroup;
    // @name enables or disables controls depending on whether
    // @link(cbPhastInterpolation) is checked. If the controls are enabled
    // and their OnChange events are assigned, the OnChange events are called.
    procedure cbPhastInterpolationClick(Sender: TObject);
    // @name enables or disables controls in response to the user
    // choosing one of the radio buttons in @link(rgInterpolationDirection).
    procedure rgInterpolationDirectionClick(Sender: TObject);
  private
    // @name stores the value specified in @link(SetMixtureAllowed)
    FMixtureAllowed: boolean;
    // See @link(AssigningValues).
    FAssigningValues: boolean;
    // @name fills the controls on the @classname
    // with the values in InterpValues
    procedure AssignData(const InterpValues: TPhastInterpolationValues);
  protected
    // @name enables or disables the controls on the @classname based
    // on Value and calls inherited SetEnabled.
    procedure SetEnabled(Value: boolean); override;
    { Private declarations }
  public
    // @name should be set to @true when the values in the @classname are
    // first being set to prevent OnChange events from being called when they
    // shouldn't.
    property AssigningValues: boolean read FAssigningValues
      write FAssigningValues;
    // @name copies the data in InterpValues to the controls in @classname
    // and enables or disables controls appropriately.
    procedure GetFirstData(const InterpValues: TPhastInterpolationValues);
    // @name is used to enable or disable the radiobutton for mixtures.
    procedure SetMixtureAllowed(const AValue: boolean);
    // @name should only be called after @link(GetFirstData) has already
    // been called.  It checks whether the data in InterpValues agrees
    // with the data that has already been copied to the controls.  If
    // not, it puts the control in an indeterminate state.
    procedure GetMoreData(const InterpValues: TPhastInterpolationValues);
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses RbwParser, GoPhastTypes;

procedure TframePhastInterpolation.AssignData(
  const InterpValues: TPhastInterpolationValues);
begin
  FAssigningValues := True;
  try
    with InterpValues do
    begin
      rdeDistance1.Text := FloatToStr(Distance1);
      rdeDistance2.Text := FloatToStr(Distance2);
      rgInterpolationDirection.ItemIndex := Ord(InterpolationDirection);
      Assert(DataSet <> nil);
      case DataSet.DataType of
        rdtDouble:
          begin
            rdeValue1.DataType := dtReal;
            rdeValue1.Text := FloatToStr(RealValue1);
            rdeValue2.DataType := dtReal;
            rdeValue2.Text := FloatToStr(RealValue2);
          end;
        rdtInteger:
          begin
            rdeValue1.Text := IntToStr(IntValue1);
            rdeValue1.DataType := dtInteger;
            rdeValue2.Text := IntToStr(IntValue2);
            rdeValue2.DataType := dtInteger;
          end;
      else
        Assert(False);
      end;
      case InterpolationDirection of
        pidX, pidY, pidZ:
          begin
          end;
        pidMix:
          begin
            if (DataSet is TIntegerPhastDataSet)
              or (DataSet is TSparseIntegerPhastDataSet) then
            begin
              edMixFormula.Text := InterpValues.MixtureFormula;
            end
            else
            begin
              Assert(False);
            end;
          end;
      else
        begin
          Assert(False);
        end;
      end;
    end;
  finally
    FAssigningValues := False;
  end;
end;

procedure TframePhastInterpolation.cbPhastInterpolationClick(
  Sender: TObject);
var
  ShouldEnable: boolean;
begin
  ShouldEnable := Enabled and cbPhastInterpolation.Enabled
    and (cbPhastInterpolation.State <> cbUnchecked);
  rdeValue1.Enabled := ShouldEnable;
  rdeValue2.Enabled := ShouldEnable;
  rgInterpolationDirection.Enabled := ShouldEnable;
  lblDistance1.Enabled := ShouldEnable;
  lblDistance2.Enabled := ShouldEnable;
  lblValue1.Enabled := ShouldEnable;
  lblValue2.Enabled := ShouldEnable;
  rgInterpolationDirection.Handle;
  rgInterpolationDirection.Buttons[3].Enabled := FMixtureAllowed
    and Enabled and cbPhastInterpolation.Checked;
//  rgInterpolationDirection.Controls[3].Enabled := FMixtureAllowed
//    and Enabled and cbPhastInterpolation.Checked;
  rgInterpolationDirectionClick(nil);

  if rdeValue1.Enabled and Assigned(rdeValue1.OnChange) then
  begin
    rdeValue1.OnChange(rdeValue1);
  end;
  if rdeValue2.Enabled and Assigned(rdeValue2.OnChange) then
  begin
    rdeValue2.OnChange(rdeValue2);
  end;
  if rgInterpolationDirection.Enabled
    and Assigned(rgInterpolationDirection.OnClick) then
  begin
    rgInterpolationDirection.OnClick(rgInterpolationDirection);
  end;
  if rdeDistance1.Enabled and Assigned(rdeDistance1.OnChange) then
  begin
    rdeDistance1.OnChange(rdeDistance1);
  end;
  if rdeDistance2.Enabled and Assigned(rdeDistance2.OnChange) then
  begin
    rdeDistance2.OnChange(rdeDistance2);
  end;
  if edMixFormula.Enabled and Assigned(edMixFormula.OnChange) then
  begin
    edMixFormula.OnChange(edMixFormula);
  end;
end;

procedure TframePhastInterpolation.GetFirstData(
  const InterpValues: TPhastInterpolationValues);
begin
  if InterpValues = nil then
  begin
    cbPhastInterpolation.Enabled := False;
  end
  else
  begin
    cbPhastInterpolation.Enabled := True;

    cbPhastInterpolation.Checked :=
      InterpValues.UsePHAST_Interpolation;
    if InterpValues.UsePHAST_Interpolation then
    begin
      AssignData(InterpValues);
    end;
  end;
  FAssigningValues := True;
  cbPhastInterpolationClick(nil);
  FAssigningValues := False;
end;

procedure TframePhastInterpolation.GetMoreData(
  const InterpValues: TPhastInterpolationValues);
var
  First: boolean;
  NewValue: string;
  NewItemIndex: integer;
begin
  if InterpValues = nil then
  begin
    Exit;
  end
  else
  begin
    cbPhastInterpolation.Enabled := True;
    First := False;
    with InterpValues do
    begin
      if not UsePHAST_Interpolation then
      begin
        if cbPhastInterpolation.State = cbChecked then
        begin
          cbPhastInterpolation.State := cbGrayed;
        end;
      end
      else
      begin
        if cbPhastInterpolation.State = cbUnchecked then
        begin
          cbPhastInterpolation.State := cbGrayed;
          First := True;
        end;

        if First then
        begin
          AssignData(InterpValues);
        end
        else
        begin
          NewValue := FloatToStr(Distance1);

          if NewValue <> rdeDistance1.Text then
          begin
            rdeDistance1.Text := '';
          end;

          NewValue := FloatToStr(Distance2);
          if NewValue <> rdeDistance2.Text then
          begin
            rdeDistance2.Text := '';
          end;

          NewItemIndex := Ord(InterpolationDirection);

          if NewItemIndex <> rgInterpolationDirection.ItemIndex then
          begin
            rgInterpolationDirection.ItemIndex := -1;
          end;

          case DataSet.DataType of
            rdtDouble:
              begin
                NewValue := FloatToStr(RealValue1);
                if NewValue <> rdeValue1.Text then
                begin
                  rdeValue1.Text := '';
                end;
                rdeValue1.DataType := dtReal;

                NewValue := FloatToStr(RealValue2);
                if NewValue <> rdeValue2.Text then
                begin
                  rdeValue2.Text := '';
                end;
                rdeValue2.DataType := dtReal;
              end;
            rdtInteger:
              begin
                rdeValue1.DataType := dtInteger;
                NewValue := IntToStr(IntValue1);
                if NewValue <> rdeValue1.Text then
                begin
                  rdeValue1.Text := '';
                end;

                rdeValue2.DataType := dtInteger;
                NewValue := IntToStr(IntValue2);
                if NewValue <> rdeValue2.Text then
                begin
                  rdeValue2.Text := '';
                end;
              end;
          else
            Assert(False);
          end;
        end;
      end;
    end;
  end;
  cbPhastInterpolationClick(nil);
end;

procedure TframePhastInterpolation.SetEnabled(Value: boolean);
begin
  inherited SetEnabled(Value);
  cbPhastInterpolation.Enabled := Value;
  rdeDistance1.Enabled := Value and cbPhastInterpolation.Checked;
  rdeDistance2.Enabled := Value and cbPhastInterpolation.Checked;
  rdeValue1.Enabled := Value and cbPhastInterpolation.Checked;
  rdeValue2.Enabled := Value and cbPhastInterpolation.Checked;
  rgInterpolationDirection.Enabled := Value and cbPhastInterpolation.Checked;
  rgInterpolationDirectionClick(nil);

  // This will cause TCustomRadioGroup.UpdateButtons to be called.
  rgInterpolationDirection.WordWrap := not rgInterpolationDirection.WordWrap;
  rgInterpolationDirection.WordWrap := not rgInterpolationDirection.WordWrap;

  rgInterpolationDirection.Handle;
  rgInterpolationDirection.Buttons[3].Enabled := FMixtureAllowed
    and Enabled and cbPhastInterpolation.Checked;
//  rgInterpolationDirection.Controls[3].Enabled := FMixtureAllowed
//    and Enabled and cbPhastInterpolation.Checked;
end;

procedure TframePhastInterpolation.SetMixtureAllowed(
  const AValue: boolean);
begin
  FMixtureAllowed := AValue;
  rgInterpolationDirection.Handle;
//  rgInterpolationDirection.Controls[3].Enabled := FMixtureAllowed
//    and Enabled and cbPhastInterpolation.Checked;
  rgInterpolationDirection.Buttons[3].Enabled := FMixtureAllowed
    and Enabled and cbPhastInterpolation.Checked;
end;

procedure TframePhastInterpolation.rgInterpolationDirectionClick(
  Sender: TObject);
var
  ShouldEnable: boolean;
begin
  ShouldEnable := Enabled and cbPhastInterpolation.Enabled
    and (cbPhastInterpolation.State <> cbUnchecked)
    and (rgInterpolationDirection.ItemIndex = 3);
  edMixFormula.Enabled := ShouldEnable;
  btnEditMixtureFormula.Enabled := ShouldEnable;
  if ShouldEnable then
  begin
    edMixFormula.Color := clWindow;
  end
  else
  begin
    edMixFormula.Color := clBtnFace;
  end;

  ShouldEnable := Enabled and cbPhastInterpolation.Enabled
    and (cbPhastInterpolation.State <> cbUnchecked)
    and (rgInterpolationDirection.ItemIndex < 3);
  rdeDistance1.Enabled := ShouldEnable;
  rdeDistance2.Enabled := ShouldEnable;
end;

end.

