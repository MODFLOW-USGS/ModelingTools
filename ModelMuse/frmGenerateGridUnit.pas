{@abstract(The main purpose of @name is to define @link(TfrmGenerateGrid) which
  is used to specify the parameters for @link(GenerateGrid)
  which is then used to generate a grid based on
  @link(ScreenObjectUnit.TScreenObject)s.)}
unit frmGenerateGridUnit;

interface

uses System.UITypes,
  Windows, SysUtils, Types, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, frmCustomGoPhastUnit, Buttons, ArgusDataEntry;

type
  {@abstract(@name is used to specify the parameters for @link(GenerateGrid)
    which is then used to generate a grid based on
    @link(ScreenObjectUnit.TScreenObject)s.)}
  TfrmGenerateGrid = class(TfrmCustomGoPhast)
    // @name: TBitBtn;
    // @name closes the @classname without making any changes.
    btnCancel: TBitBtn;
    // @name: TBitBtn;
    // @name displays help for the @classname.
    btnHelp: TBitBtn;
    // @name: TBitBtn;
    // See @link(btnOKClick).
    btnOK: TBitBtn;
    // @name: TCheckBox;
    // @name indicates whether the columns should be "smoothed".
    cbColumns: TCheckBox;
    // @name: TCheckBox;
    // @name indicates whether the layers should be "smoothed".
    cbLayers: TCheckBox;
    // @name: TCheckBox;
    // @name indicates whether the rows should be "smoothed".
    cbRows: TCheckBox;
    // @name: TCheckBox;
    // @name indicates whether any grid "smoothing" should occur.
    cbSmoothGrid: TCheckBox;
    // @name: TCheckBox;
    // @name indicates whether the grid angle should be determined automatically
    // or specified by the user.
    cbSpecifyGridAngle: TCheckBox;
    // @name holds the controls related to grid smoothing.
    gbGridSmoothing: TGroupBox;
    // @name: TLabel;
    // @name labels the edit box for the grid smoothing criterion.
    lblCriterion: TLabel;
    // @name: TLabel;
    // @name labels the edit box for the grid angle.
    lblGridAngle: TLabel;
    // @name: TRbwDataEntry;
    // @name is the edit box for the grid smoothing criterion.
    rdeCriterion: TRbwDataEntry;
    // @name: TRbwDataEntry;
    // @name is the edit box for the grid angle.
    rdeGridAngle: TRbwDataEntry;
    // @name calls @link(GenerateGrid) using the values displayed in the
    // @classname.
    procedure btnOKClick(Sender: TObject);
    // @name enables or disables controls based on whether
    // grid smoothing will be used.
    procedure cbSmoothGridClick(Sender: TObject);
    // @name enables or disables @link(rdeGridAngle) depending on whether
    // or not the user will specify it.
    procedure cbSpecifyGridAngleClick(Sender: TObject);
    // name reads the grid angle and puts it in @link(rdeGridAngle).
    procedure FormCreate(Sender: TObject); override;
  private
    FDisvUsed: Boolean;
    procedure GetData;
    procedure SetData;
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.dfm}

uses Math, frmGoPhastUnit, GridGeneration, GoPhastTypes, ScreenObjectUnit;

procedure TfrmGenerateGrid.cbSpecifyGridAngleClick(Sender: TObject);
begin
  inherited;
  rdeGridAngle.Enabled := not cbSpecifyGridAngle.Checked;
end;

procedure TfrmGenerateGrid.FormCreate(Sender: TObject);
begin
  inherited;
  GetData
end;

procedure TfrmGenerateGrid.GetData;
var
  Index: Integer;
  Count: integer;
  ScreenObject: TScreenObject;
begin
  FDisvUsed := frmGoPhast.DisvUsed;
  if FDisvUsed then
  begin
    rdeGridAngle.Text := FloatToStr(RadToDeg(frmGoPhast.PhastModel.ModflowGrid.GridAngle));
  end
  else if frmGoPhast.Grid <> nil then
  begin
    rdeGridAngle.Text := FloatToStr(RadToDeg(frmGoPhast.Grid.GridAngle));
  end
  else
  begin
    rdeGridAngle.Text := '0';
  end;
  case frmGoPhast.ModelSelection of
    msUndefined: Assert(False);
    msPhast:
      begin
        cbSmoothGrid.Checked := False;
      end;
    msModflow, msModflowLGR, msModflowLGR2, msModflowNWT,
      msModflowFmp, msModflowCfp, msFootPrint, msModflow2015
      {$IFDEF OWHMV2}
      , msModflowOwhm2
      {$ENDIF}
      :
      begin
        Count := 0;
        for Index := 0 to frmGoPhast.PhastModel.ScreenObjectCount - 1 do
        begin
          ScreenObject := frmGoPhast.PhastModel.ScreenObjects[Index];
          if not ScreenObject.Deleted and ScreenObject.CellSizeUsed then
          begin
            Inc(Count);
            if Count > 1 then
            begin
              break;
            end;
          end;
        end;
        cbSpecifyGridAngle.Checked := (frmGoPhast.ModelSelection <> msModflow2015);
        cbSmoothGrid.Checked := (Count > 1);
        if Count = 0 then
        begin
          Beep;
          MessageDlg(GenGridErrorMessage, mtError, [mbOK], 0);
          ModalResult := mrCancel;
        end;
      end;
    else Assert(False);
  end;
end;

procedure TfrmGenerateGrid.SetData;
var
  ErrorMessage: string;
  WarningMessage: string;
//  PriorDisVUsed: Boolean;
//  PriorLayerType: TMf6GridType;
begin
//  PriorDisVUsed := frmGoPhast.DisvUsed;
//  if PriorDisVUsed then
//  begin
//    frmGoPhast.UndoStack.Submit(TUndoChangeGridType.Create(mgtStructured));
//  end;
  if GenerateGrid(ErrorMessage, WarningMessage, not cbSpecifyGridAngle.Checked,
    DegToRad(strToFloat(rdeGridAngle.Text)),
    cbSmoothGrid.Checked and cbColumns.Checked,
    cbSmoothGrid.Checked and cbRows.Checked,
    cbSmoothGrid.Checked and cbLayers.Checked,
    StrToFloat(rdeCriterion.Text)) then
  begin
//    if PriorDisVUsed then
//    begin
//      frmGoPhast.UndoStack.Submit(TUndoChangeGridType.Create(mgtLayered, True));
//    end;
//    if frmGoPhast.DisvUsed then
//    begin
//      frmGoPhast.acQuadmeshExecute(nil);
//    end;
    with frmGoPhast.frame3DView do
    begin
      SetDefaultOrientation;
      glWidModelView.Invalidate;
    end;
    if WarningMessage <> '' then
    begin
      Beep;
      MessageDlg(WarningMessage, mtWarning, [mbOK], 0);
    end;
  end
  else
  begin
    if ErrorMessage <> '' then
    begin
      Beep;
      MessageDlg(ErrorMessage, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfrmGenerateGrid.cbSmoothGridClick(Sender: TObject);
begin
  inherited;
  cbColumns.Enabled := cbSmoothGrid.Checked;
  cbRows.Enabled := cbSmoothGrid.Checked;
  cbLayers.Enabled := cbSmoothGrid.Checked
    and (frmGoPhast.PhastModel.ModelSelection = msPhast);
  rdeCriterion.Enabled := cbSmoothGrid.Checked;
  lblCriterion.Enabled := cbSmoothGrid.Checked;
end;

procedure TfrmGenerateGrid.btnOKClick(Sender: TObject);
begin
  inherited;
  SetData;
end;

end.

