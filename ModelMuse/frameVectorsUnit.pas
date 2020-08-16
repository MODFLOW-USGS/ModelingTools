unit frameVectorsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  ArgusDataEntry, VectorDisplayUnit, ExtCtrls, ComCtrls, JvExComCtrls,
  JvUpDown, Mask, JvExMask, JvSpin;

type
  TframeVectors = class(TFrame)
    rdeScale: TRbwDataEntry;
    lblScale: TLabel;
    comboVectorSource: TComboBox;
    clrbxMax: TColorBox;
    clrbxMid: TColorBox;
    clrbxMin: TColorBox;
    clrbxVelocity: TColorBox;
    lblMaxColor: TLabel;
    lblMidColor: TLabel;
    lblMinColor: TLabel;
    lblVelocityColor: TLabel;
    lblVectorSource: TLabel;
    lblScale3D: TLabel;
    rdeScale3D: TRbwDataEntry;
    udVectors: TJvUpDown;
    lblMinSpacing2D: TLabel;
    seMinSpacing2D: TJvSpinEdit;
    cbMaxVisible: TCheckBox;
    cbMidVisible: TCheckBox;
    cbMinVisible: TCheckBox;
    rdeMinHorizontalSpacing3D: TRbwDataEntry;
    rdeMinVerticalSpacing3D: TRbwDataEntry;
    lblMinHorizontalSpacing3D: TLabel;
    lblMinVerticalSpacing3D: TLabel;
    cbLogTransform: TCheckBox;
    procedure comboVectorSourceChange(Sender: TObject);
    procedure udVectorsChangingEx(Sender: TObject; var AllowChange: Boolean;
      NewValue: Integer; Direction: TUpDownDirection);
  private
    FFirstVectorItem: integer;
    { Private declarations }
  public
    procedure GetData;
    procedure SetData;
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses frmGoPhastUnit, SutraMeshUnit, SutraOptionsUnit, PhastModelUnit,
  GoPhastTypes;

resourcestring
  StrNone = 'none';
  StrHydraulicConductivi = 'Hydraulic Conductivity';
  StrPermeability = 'Permeability';
  StrLongitudinalDispers = 'Longitudinal Dispersivity';
  StrTransverseDispersiv = 'Transverse Dispersivity';

{ TframeVectors }

type
  TControlCrack = class(TControl);

procedure TframeVectors.comboVectorSourceChange(Sender: TObject);
  procedure SetEnabledAppearance(Box: TControl);
  begin
    if Box.Enabled then
    begin
      TControlCrack(Box).Color := clWindow;
    end
    else
    begin
      TControlCrack(Box).Color := clBtnFace;
    end;
  end;
var
  LocalModel: TPhastModel;
  VItem: TVectorItem;
begin
  seMinSpacing2D.Enabled := (comboVectorSource.ItemIndex > 0);
  rdeMinHorizontalSpacing3D.Enabled := seMinSpacing2D.Enabled;
  rdeMinVerticalSpacing3D.Enabled := seMinSpacing2D.Enabled;
  rdeScale.Enabled := seMinSpacing2D.Enabled;
  rdeScale3D.Enabled := seMinSpacing2D.Enabled;

  clrbxMax.Enabled := (comboVectorSource.ItemIndex > 0)
    and (comboVectorSource.ItemIndex < FFirstVectorItem);
  clrbxMid.Enabled := clrbxMax.Enabled;
  clrbxMin.Enabled := clrbxMax.Enabled;
  lblMaxColor.Enabled := clrbxMax.Enabled;
  lblMidColor.Enabled := clrbxMid.Enabled;
  lblMinColor.Enabled := clrbxMin.Enabled;

  cbMaxVisible.Enabled := clrbxMax.Enabled;
  cbMidVisible.Enabled := clrbxMid.Enabled;
  cbMinVisible.Enabled := lblMinColor.Enabled;

  clrbxVelocity.Enabled := comboVectorSource.ItemIndex >= FFirstVectorItem;
  lblVelocityColor.Enabled := clrbxVelocity.Enabled;

  SetEnabledAppearance(clrbxMax);
  SetEnabledAppearance(clrbxMid);
  SetEnabledAppearance(clrbxMin);
  SetEnabledAppearance(clrbxVelocity);

  if clrbxMax.Enabled then
  begin
    LocalModel := frmGoPhast.PhastModel;
    clrbxMax.Selected := LocalModel.MaxVectors.Color;
    clrbxMid.Selected := LocalModel.MidVectors.Color;
    clrbxMin.Selected := LocalModel.MinVectors.Color;
  end;

  if clrbxVelocity.Enabled then
  begin
    VItem := comboVectorSource.Items.Objects[
      comboVectorSource.ItemIndex] as TVectorItem;
    clrbxVelocity.Selected := VItem.Vectors.Color;
  end;

  udVectors.Position := comboVectorSource.ItemIndex;
end;

procedure TframeVectors.GetData;
var
  LocalModel: TPhastModel;
  NewItemIndex: Integer;
  VItem: TVectorItem;
  ItemIndex: Integer;
  VelocityVectors: TVectorCollection;
begin
  comboVectorSource.Clear;
  comboVectorSource.Items.Add(StrNone);
  if frmGoPhast.ModelSelection in SutraSelection then
  begin
    if frmGoPhast.PhastModel.SutraOptions.TransportChoice = tcSoluteHead then
    begin
      comboVectorSource.Items.Add(StrHydraulicConductivi);
    end
    else
    begin
      comboVectorSource.Items.Add(StrPermeability);
    end;
    comboVectorSource.Items.Add(StrLongitudinalDispers);
    comboVectorSource.Items.Add(StrTransverseDispersiv);
  end;
  FFirstVectorItem := comboVectorSource.Items.Count;

  VelocityVectors := frmGoPhast.PhastModel.VelocityVectors;
  for ItemIndex := 0 to VelocityVectors.Count - 1 do
  begin
    VItem := VelocityVectors.Items[ItemIndex] as TVectorItem;
    if VItem.IsValid then
    begin
      comboVectorSource.Items.AddObject(VItem.Description, VItem);
    end;

  end;

  LocalModel := frmGoPhast.PhastModel;

  seMinSpacing2D.AsInteger := LocalModel.MaxVectors.MinimumSeparation2D;
  rdeMinHorizontalSpacing3D.RealValue := LocalModel.MaxVectors.MinSeparationHorizontal3D;
  rdeMinVerticalSpacing3D.RealValue := LocalModel.MaxVectors.MinSeparationVertical3D;

  if (not LocalModel.MaxVectors.Visible) and (VelocityVectors.SelectedItem < 0) then
  begin
    comboVectorSource.ItemIndex := 0;
    rdeScale.Text := '1';
  end
  else
  begin
    if LocalModel.MaxVectors.Visible then
    begin
      NewItemIndex := -1;
      rdeScale.Text := FloatToStr(LocalModel.MaxVectors.Scale);
      rdeScale3D.Text := FloatToStr(LocalModel.MaxVectors.Scale3D);
      cbLogTransform.Checked := LocalModel.MaxVectors.LogScaled;
      case LocalModel.MaxVectors.VectorType of
        pvtPermeability:
          begin
            NewItemIndex := comboVectorSource.Items.IndexOf(StrPermeability);
            if NewItemIndex < 0 then
            begin
              NewItemIndex := comboVectorSource.Items.IndexOf(StrHydraulicConductivi)
            end;
          end;
        pvtConductivity:
          begin
            NewItemIndex := comboVectorSource.Items.IndexOf(StrHydraulicConductivi);
            if NewItemIndex < 0 then
            begin
              NewItemIndex := comboVectorSource.Items.IndexOf(StrPermeability)
            end;
          end;
        pvtLongitudianalDispersivity:
          begin
            NewItemIndex := comboVectorSource.Items.IndexOf(StrLongitudinalDispers)
          end;
        pvtTransverseDispersivity:
          begin
            NewItemIndex := comboVectorSource.Items.IndexOf(StrTransverseDispersiv)
          end;
        else
          Assert(False);
      end;
      if NewItemIndex < 0 then
      begin
        NewItemIndex := 0;
      end;
    end
    else
    begin
      VItem := VelocityVectors.Items[VelocityVectors.SelectedItem] as TVectorItem;
      NewItemIndex := comboVectorSource.Items.IndexOfObject(VItem);

      rdeScale.Text := FloatToStr(VItem.Vectors.Scale);
      rdeScale3D.Text := FloatToStr(VItem.Vectors.Scale3D);
      clrbxVelocity.Selected := VItem.Vectors.Color;
    end;
    comboVectorSource.ItemIndex := NewItemIndex;
  end;
  udVectors.Position := comboVectorSource.ItemIndex;
  comboVectorSourceChange(nil);
end;

procedure TframeVectors.SetData;
var
  LocalModel: TPhastModel;
  SourceText: string;
  PredefinedVisible: boolean;
  AValue: double;
  Is3D: Boolean;
  VItem: TVectorItem;
begin
  LocalModel := frmGoPhast.PhastModel;
  if LocalModel.ModelSelection in SutraSelection then
  begin
    Is3D := LocalModel.SutraMesh.MeshType = mt3D;
  end
  else
  begin
    Is3D := True;
  end;
  SourceText := comboVectorSource.Text;

  PredefinedVisible := (comboVectorSource.ItemIndex > 0) and
    (comboVectorSource.ItemIndex < FFirstVectorItem);

  LocalModel.MaxVectors.Visible := PredefinedVisible and cbMaxVisible.Checked;
  LocalModel.MidVectors.Visible := PredefinedVisible and Is3D and cbMidVisible.Checked;
  LocalModel.MinVectors.Visible := PredefinedVisible and cbMinVisible.Checked;

  LocalModel.MaxVectors.MinimumSeparation2D := seMinSpacing2D.AsInteger;
  LocalModel.MidVectors.MinimumSeparation2D := seMinSpacing2D.AsInteger;
  LocalModel.MinVectors.MinimumSeparation2D := seMinSpacing2D.AsInteger;

  LocalModel.MaxVectors.MinSeparationHorizontal3D := rdeMinHorizontalSpacing3D.RealValue;
  LocalModel.MidVectors.MinSeparationHorizontal3D := rdeMinHorizontalSpacing3D.RealValue;
  LocalModel.MinVectors.MinSeparationHorizontal3D := rdeMinHorizontalSpacing3D.RealValue;

  LocalModel.MaxVectors.MinSeparationVertical3D := rdeMinVerticalSpacing3D.RealValue;
  LocalModel.MidVectors.MinSeparationVertical3D := rdeMinVerticalSpacing3D.RealValue;
  LocalModel.MinVectors.MinSeparationVertical3D := rdeMinVerticalSpacing3D.RealValue;

  LocalModel.MaxVectors.LogScaled := cbLogTransform.Checked;
  LocalModel.MidVectors.LogScaled := cbLogTransform.Checked;
  LocalModel.MinVectors.LogScaled := cbLogTransform.Checked;

  if PredefinedVisible then
  begin
    LocalModel.VelocityVectors.SelectedItem := -1;

    LocalModel.MaxVectors.Color := clrbxMax.Selected;
    LocalModel.MidVectors.Color := clrbxMid.Selected;
    LocalModel.MinVectors.Color := clrbxMin.Selected;

    if SourceText = StrHydraulicConductivi then
    begin
      LocalModel.MaxVectors.VectorType := pvtConductivity;
      LocalModel.MidVectors.VectorType := pvtConductivity;
      LocalModel.MinVectors.VectorType := pvtConductivity;
    end
    else if SourceText = StrPermeability then
    begin
      LocalModel.MaxVectors.VectorType := pvtPermeability;
      LocalModel.MidVectors.VectorType := pvtPermeability;
      LocalModel.MinVectors.VectorType := pvtPermeability;
    end
    else if SourceText = StrLongitudinalDispers then
    begin
      LocalModel.MaxVectors.VectorType := pvtLongitudianalDispersivity;
      LocalModel.MidVectors.VectorType := pvtLongitudianalDispersivity;
      LocalModel.MinVectors.VectorType := pvtLongitudianalDispersivity;
    end
    else if SourceText = StrTransverseDispersiv then
    begin
      LocalModel.MaxVectors.VectorType := pvtTransverseDispersivity;
      LocalModel.MidVectors.VectorType := pvtTransverseDispersivity;
      LocalModel.MinVectors.VectorType := pvtTransverseDispersivity;
    end;

    if TryStrToFloat(rdeScale.Text, AValue) then
    begin
      LocalModel.MaxVectors.Scale := AValue;
      LocalModel.MidVectors.Scale := AValue;
      LocalModel.MinVectors.Scale := AValue;
    end;

    if TryStrToFloat(rdeScale3D.Text, AValue) then
    begin
      LocalModel.MaxVectors.Scale3D := AValue;
      LocalModel.MidVectors.Scale3D := AValue;
      LocalModel.MinVectors.Scale3D := AValue;
    end;
  end
  else
  begin
    VItem := comboVectorSource.Items.Objects[comboVectorSource.ItemIndex] as TVectorItem;
    if VItem = nil then
    begin
      LocalModel.VelocityVectors.SelectedItem := -1;
    end
    else
    begin
      LocalModel.VelocityVectors.SelectedItem := VItem.Index;
      if TryStrToFloat(rdeScale.Text, AValue) then
      begin
        VItem.Vectors.Scale := AValue;
      end;
      if TryStrToFloat(rdeScale3D.Text, AValue) then
      begin
        VItem.Vectors.Scale3D := AValue;
      end;
      VItem.Vectors.Color := clrbxVelocity.Selected;
      VItem.Vectors.MinimumSeparation2D := seMinSpacing2D.AsInteger;
      VItem.Vectors.MinSeparationVertical3D := rdeMinVerticalSpacing3D.RealValue;
      VItem.Vectors.MinSeparationHorizontal3D := rdeMinHorizontalSpacing3D.RealValue;
      VItem.Vectors.LogScaled := cbLogTransform.Checked;
    end;
  end;
end;

procedure TframeVectors.udVectorsChangingEx(Sender: TObject;
  var AllowChange: Boolean; NewValue: Integer; Direction: TUpDownDirection);
var
  NewIndex: Integer;
begin
  NewIndex := comboVectorSource.ItemIndex;
  case Direction of
    updNone:
      begin

      end;
    updUp:
      begin
        Dec(NewIndex);
      end;
    updDown:
      begin
        Inc(NewIndex);
      end;
  end;
  if (NewIndex >= 0) and (NewIndex < comboVectorSource.Items.Count) then
  begin
    comboVectorSource.ItemIndex := NewIndex;
    SetData;
    udVectors.Position := comboVectorSource.ItemIndex;
  end
  else
  begin
    AllowChange := False;
  end;
end;

end.
