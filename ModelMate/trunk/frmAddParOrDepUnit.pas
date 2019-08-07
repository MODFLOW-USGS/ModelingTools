unit frmAddParOrDepUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  GlobalData, GlobalTypesUnit, JupiterUnit,
  ModelMateClassesUnit, ModelMateUtilities,
  DependentsUnit, PriorInfoUnit, Utilities,
  GlobalBasicData, Buttons, frmNamingConvention;

type
  { Enumerations }
  TParOrDepUse = (pduParameter, pduObservation, pduPrediction,
                  pduPrior, pduUnknown);

  TfrmAddParOrDep = class(TForm)
    edtParOrDepName: TEdit;
    lblName: TLabel;
    lblGroup: TLabel;
    cmbGroup: TComboBox;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    PDUse: TParOrDepUse;
  end;

var
  frmAddParOrDep: TfrmAddParOrDep;

implementation

{$R *.dfm}

procedure TfrmAddParOrDep.btnOKClick(Sender: TObject);
var
  Msg: string;
  I, IErr: integer;
begin
  ModalResult := mrNone;
  if edtParOrDepName.Text <> '' then
    begin
      IErr := 0;
      if J_Valid_Name(edtParOrDepName.Text,MaxLenDepName) then
        begin
          // Check for uniqueness of parameter or dependent name.
          case PDUse of
            pduParameter:
              for I := 0 to ParamSetCurrent.Count - 1 do
                begin
                  if AnsiSameText(edtParOrDepName.Text,ConvertString(ParamSetCurrent.Items[I].Name)) then
                    IErr := 2;
                end;
            pduObservation:
              for I := 0 to DepSetTemp.Count - 1 do
                begin
                  if AnsiSameText(edtParOrDepName.Text,ConvertString(DepSetTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            pduPrediction:
              for I := 0 to DepSetTemp.Count - 1 do
                begin
                  if AnsiSameText(edtParOrDepName.Text,ConvertString(DepSetTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            pduPrior:
              for I := 0 to PriSetTemp.Count - 1 do
                begin
                  if AnsiSameText(edtParOrDepName.Text,ConvertString(PriSetTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            pduUnknown: ShowMessage('Prog error in TfrmAddParOrDep.btnSaveClick');
            else  ShowMessage('Prog error in TfrmAddParOrDep.btnSaveClick');
          end;
        end
      else
        // Name is invalid.
        begin
          IErr := 1;
        end;
      if IErr = 0 then
        begin
          case PDUse of
            pduParameter:
              begin
                ParamSetCurrent.Add;
                I := ParamSetCurrent.Count - 1;
                ParamSetCurrent.Items[I].Initialize(ConvertString12(edtParOrDepName.Text),
                                                    ConvertString12(cmbGroup.Text));
              end;
            pduObservation:
              begin
                DepSetTemp.Add;
                I := DepSetTemp.Count - 1;
                DepSetTemp.Items[I].Initialize(ConvertString20(edtParOrDepName.Text),
                                                  ConvertString12(cmbGroup.Text), dcObs);
//                ObservationsChanged := True;
              end;
            pduPrediction:
              begin
                DepSetTemp.Add;
                I := DepSetTemp.Count - 1;
                DepSetTemp.Items[I].Initialize(ConvertString20(edtParOrDepName.Text),
                                                   ConvertString12(cmbGroup.Text), dcPred);
              end;
            pduPrior:
              begin
                PriSetTemp.Add;
                I := PriSetTemp.Count - 1;
                PriSetTemp.Items[I].Initialize(ConvertString20(edtParOrDepName.Text),
                                                   ConvertString12(cmbGroup.Text));
              end;
          end;
          ModalResult := mrOK;
          Close;
          ModalResult := mrOK;
        end
      else
        begin
          case IErr of
            //1: Msg := 'Error: "' + edtParOrDepName.Text + '" is not a valid name.';
            1:
              begin
                Msg := 'Error: "' + edtParOrDepName.Text + '" is not a valid name.';
                ShowMessage(Msg);
                FormNamingConvention.ShowModal;
              end;
            2:
              begin
                Msg := 'Error: Name "' + edtParOrDepName.Text + '" is already assigned.';
                ShowMessage(Msg);
              end;
          end;
        end;
    end
  else
    Close;
end;

procedure TfrmAddParOrDep.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  PDUse := pduUnknown;
end;

procedure TfrmAddParOrDep.FormShow(Sender: TObject);
var
  I: integer;
  GroupList: TStringList;
  Str: string;
begin
  // Populate edtGroup drop down list with list of groups
  GroupList := TStringList.Create;
  try
    case PDUse of
      pduParameter:
        begin
          Caption := 'Add Parameter';
          lblName.Caption := 'Name of new parameter:';
          lblGroup.Caption := 'Parameter group:';
          for I := 0 to ParamGpsCurrent.Count - 1 do
            begin
              Str := ConvertString(ParamGpsCurrent.Items[I].Name);
              GroupList.Add(Str);
            end;
          cmbGroup.Items := GroupList;
          cmbGroup.ItemIndex := 0;
          edtParOrDepName.MaxLength := 12;
        end;
      pduObservation:
        begin
          Caption := 'Add Observation';
          lblName.Caption := 'Name of new observation:';
          lblGroup.Caption := 'Observation group:';
          for I := 0 to DepGpsTemp.Count - 1 do
            begin
              Str := ConvertString(DepGpsTemp.Items[I].Name);
              GroupList.Add(Str);
            end;
          cmbGroup.Items := GroupList;
          cmbGroup.ItemIndex := 0;
          edtParOrDepName.MaxLength := 20;
        end;
      pduPrediction:
        begin
          Caption := 'Add Prediction';
          lblName.Caption := 'Name of new prediction:';
          lblGroup.Caption := 'Prediction group:';
          for I := 0 to DepGpsTemp.Count - 1 do
            begin
              Str := ConvertString(DepGpsTemp.Items[I].Name);
              GroupList.Add(Str);
            end;
          cmbGroup.Items := GroupList;
          cmbGroup.ItemIndex := 0;
          edtParOrDepName.MaxLength := 20;
        end;
      pduPrior:
        begin
          Caption := 'Add Prior-Information Item';
          lblName.Caption := 'Name of new prior info item:';
          lblGroup.Caption := 'Prior-information group:';
          for I := 0 to PriGpsTemp.Count - 1 do
            begin
              Str := ConvertString(PriGpsTemp.Items[I].Name);
              GroupList.Add(Str);
            end;
          cmbGroup.Items := GroupList;
          cmbGroup.ItemIndex := 0;
          edtParOrDepName.MaxLength := 20;
        end;
      pduUnknown:
        begin
          Caption := 'Add unknown thing (TfrmAddParOrDep)';
          lblName.Caption := 'Name of new thing:';
          lblGroup.Caption := '??? group:';
          ShowMessage('Prog error: PDUse not set');
        end
      else
        begin
          Caption := 'Add unknown thing (TfrmAddParOrDep)';
          lblName.Caption := 'Name of new thing:';
          lblGroup.Caption := '??? group:';
          ShowMessage('PDUse unrecognized in TfrmAddParOrDep.FormShow');
        end;
    end;
  finally
    FreeAndNil(GroupList);
  end;
end;

end.
