unit frmAddGroupUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes,
  Graphics, Controls, Forms, Dialogs, StdCtrls,
  DependentsUnit, GlobalData, JupiterUnit, Utilities,
  PriorInfoUnit, GlobalBasicData, GlobalTypesUnit, Buttons,
  ModelMateUtilities, frmNamingConvention;

type

  TfrmAddGroup = class(TForm)
    edtGPName: TEdit;
    lblName: TLabel;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    GpUse: TGrpUse;
  end;

var
  frmAddGroup: TfrmAddGroup;

implementation

{$R *.dfm}

procedure TfrmAddGroup.btnOKClick(Sender: TObject);
var
  GroupName, Msg: string;
  I, IErr: integer;
begin
  ModalResult := mrNone;
  GroupName := edtGpName.Text;
  if GroupName <> '' then
    begin
      IErr := 0;
      if J_Valid_Name(GroupName,MaxLenGpName) then
        begin
          // Ensure that group name is unique in group GpUse.
          if not UniqueGroupName(GroupName,GpUse) then IErr := 2;
        end
      else
        //Name is invalid
        IErr := 1;
      if IErr = 0 then
        begin
          case GpUse of
            guParGroup:
              begin
                ParamGpsCurrent.Add;
                I := ParamGpsCurrent.Count - 1;
                ParamGpsCurrent.Items[I].Initialize(ConvertString12(GroupName), ConvertString12(GroupName));
                Close;
              end;
            guObsGroup:
              begin
                DepGpsTemp.Add;
                I := DepGpsTemp.Count - 1;
                DepGpsTemp.Items[I].Initialize(ConvertString20(GroupName), ConvertString12(GroupName), dcObs);
                Close;
              end;
            guPredGroup:
              begin
                DepGpsTemp.Add;
                I := DepGpsTemp.Count - 1;
                DepGpsTemp.Items[I].Initialize(ConvertString20(GroupName), ConvertString12(GroupName), dcPred);
                Close;
              end;
            guPriGroup:
              begin
                PriGpsTemp.Add;
                I := PriGpsTemp.Count - 1;
                PriGpsTemp.Items[I].Initialize(ConvertString20(GroupName), ConvertString12(GroupName));
                Close;
              end;
            guUnknown: ;
          end;
          ModalResult := mrOK;
        end
      else
        begin
          case IErr of
            1: Msg := 'Error: "' + GroupName + '" is not a valid name.';
            2: Msg := 'Error: Name "' + GroupName + '" is already assigned.';
          end;
          ShowMessage(Msg);
          if IErr = 1 then FormNamingConvention.ShowModal;
        end;
    end;
end;

procedure TfrmAddGroup.btnSaveClick(Sender: TObject);
var
  Msg: string;
  I, IErr: integer;
begin
  if edtGpName.Text <> '' then
    begin
      IErr := 0;
      if J_Valid_Name(edtGpName.Text,MaxLenGpName) then
        case GpUse of
          guParGroup:
            begin
              for I := 0 to ParamGpsCurrent.Count - 1 do
                begin
                  if AnsiSameText(edtGpName.Text,ConvertString(ParamGpsCurrent.Items[I].Name)) then
                    IErr := 2;
                end;
            end;
          guObsGroup:
            begin
              for I := 0  to DepGpsTemp.Count - 1 do
                begin
                  if AnsiSameText(edtGpName.Text,ConvertString(DepGpsTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            end;
          guPredGroup:
            begin
              for I := 0  to DepGpsTemp.Count - 1 do
                begin
                  if AnsiSameText(edtGpName.Text,ConvertString(DepGpsTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            end;
          guPriGroup:
            begin
              for I := 0  to PriGpsTemp.Count - 1 do
                begin
                  if AnsiSameText(edtGpName.Text,ConvertString(PriGpsTemp.Items[I].Name)) then
                    IErr := 2;
                end;
            end;
          guUnknown: ;
        end
      else
        //Name is invalid
        IErr := 1;
      if IErr = 0 then
        case GpUse of
          guParGroup:
            begin
              ParamGpsCurrent.Add;
              I := ParamGpsCurrent.Count - 1;
              ParamGpsCurrent.Items[I].Initialize(ConvertString12(edtGpName.Text), ConvertString12(edtGpName.Text));
              Close;
            end;
          guObsGroup:
            begin
              DepGpsTemp.Add;
              I := DepGpsTemp.Count - 1;
              DepGpsTemp.Items[I].Initialize(ConvertString20(edtGpName.Text), ConvertString12(edtGpName.Text), dcObs);
              Close;
            end;
          guPredGroup:
            begin
              DepGpsTemp.Add;
              I := DepGpsTemp.Count - 1;
              DepGpsTemp.Items[I].Initialize(ConvertString20(edtGpName.Text), ConvertString12(edtGpName.Text), dcPred);
              Close;
            end;
          guPriGroup:
            begin
              PriGpsTemp.Add;
              I := PriGpsTemp.Count - 1;
              PriGpsTemp.Items[I].Initialize(ConvertString20(edtGpName.Text), ConvertString12(edtGpName.Text));
              Close;
            end;
          guUnknown: ;
        end
      else
        begin
          case IErr of
            1: Msg := 'Error: "' + edtGpName.Text + '" is not a valid name.';
            2: Msg := 'Error: Name "' + edtGpName.Text + '" is already assigned.';
          end;
          ShowMessage(Msg);
        end;
    end;
  Close;
end;

procedure TfrmAddGroup.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  GpUse := guUnknown;
end;

procedure TfrmAddGroup.FormShow(Sender: TObject);
begin
  //
 // Populate edtGroup drop down list with list of groups
 lblName.Caption := 'Name of new group:';
 case GpUse of
   guParGroup:
     begin
       Caption := 'Add Parameter Group';
     end;
   guObsGroup:
     begin
       Caption := 'Add Observation Group';
     end;
   guPredGroup:
     begin
       Caption := 'Add Prediction Group';
     end;
   guPriGroup:
     begin
       Caption := 'Add Prior-Information Group';
     end;
   guUnknown:
     begin
       Caption := 'Add unknown thing (TfrmAddGroup)';
       ShowMessage('Prog error: GpUse not set');
     end
   else
     begin
       Caption := 'Add unknown thing (TfrmAddGroup)';
       lblName.Caption := 'Name of new thing:';
       ShowMessage('GpUse unrecognized in TfrmAddGroup.FormShow');
     end;
 end;
end;

end.
