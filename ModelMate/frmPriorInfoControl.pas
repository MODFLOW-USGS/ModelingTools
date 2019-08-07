unit frmPriorInfoControl;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,
  JvCheckBox, StdCtrls, Grids, DataGrid, JvExStdCtrls, JvMemo, frmPriorInfo,
  ModelMateClassesUnit, GlobalData, GlobalBasicData, PriorInfoUnit,
  GlobalTypesUnit;

type
  TFormPriorInfoControl = class(TForm)
    cbUsePriorInfo: TJvCheckBox;
    Label12: TLabel;
    Label3: TLabel;
    lblNumPri: TLabel;
    jvmNumPri: TJvMemo;
    jvmNumPriGps: TJvMemo;
    lblNumPriGps: TLabel;
    dgPriGpSummary: TEcDataGrid;
    btnPriorForm: TButton;
    btnClose: TButton;
    procedure btnPriorFormClick(Sender: TObject);
    procedure ShowPriForm;
    procedure btnCloseClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure cbUsePriorInfoClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    procedure PopulateForm;
  public
    { Public declarations }
    PriSetControl: TPriSet;
    PriGpsControl: TPriSet;
    PriorSetupControl: TPriorSetup;
    PriorDataControlChanged, UsePriorInfoControl: boolean;
  end;

var
  FormPriorInfoControl: TFormPriorInfoControl;

implementation

{$R *.dfm}

procedure TFormPriorInfoControl.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrOK;
  Close;
  ModalResult := mrOK;
end;

procedure TFormPriorInfoControl.btnPriorFormClick(Sender: TObject);
begin
  FormPriorInfo.PriSetLocal.Assign(PriSetControl);
  FormPriorInfo.PriGpsLocal.Assign(PriGpsControl);
  FormPriorInfo.PriorSetupLocal.Assign(PriorSetupControl);
  ShowPriForm;
  PopulateForm;
end;

procedure TFormPriorInfoControl.cbUsePriorInfoClick(Sender: TObject);
begin
  UsePriorInfoControl := cbUsePriorInfo.Checked;
  PriorDataControlChanged := True;
end;

procedure TFormPriorInfoControl.FormCreate(Sender: TObject);
begin
    PriSetControl := TPriSet.Create;
    PriGpsControl := TPriSet.Create;
    PriorSetupControl := TPriorSetup.Create(Self);
end;

procedure TFormPriorInfoControl.FormDestroy(Sender: TObject);
begin
    PriSetControl.Free;
    PriGpsControl.Free;
    PriorSetupControl.Free;
end;

procedure TFormPriorInfoControl.FormShow(Sender: TObject);
begin
  PopulateForm;
  PriorDataControlChanged := False;
end;

procedure TFormPriorInfoControl.PopulateForm;
var
  AttPos, I, J, K, NPri, NPriGps: integer;
  GName: string;
begin
  cbUsePriorInfo.Checked := UsePriorInfoControl;
  AttPos := PriAttPos(piatGroupName);
  NPri := PriSetControl.Count;
  NPriGps := PriGpsControl.Count;
  jvmNumPri.Lines.Clear;
  jvmNumPri.Lines.Add(IntToStr(NPri));
  jvmNumPri.CurrentLine := 0;
  jvmNumPriGps.Lines.Clear;
  jvmNumPriGps.Lines.Add(IntToStr(NPriGps));
  jvmNumPriGps.CurrentLine := 0;
  dgPriGpSummary.RowCount := 1 + NPriGps;
  if NPriGps > 0 then
    begin
      // Loop through prior info groups
      for I := 1 to NPriGps do
        begin
          GName := ConvertString(PriGpsControl.Items[I-1].Name);
          dgPriGpSummary.Cells[0,I] := GName;
          // Count number of prior info items in this group.
          K := 0;
          for J := 0 to NPri - 1 do
            begin
              if AnsiSameText(PriSetControl.Items[J].AllAtts.Items[AttPos].Text, GName) then
                begin
                  K := K + 1;
                end;
            end;
          dgPriGpSummary.Cells[1,I] := IntToStr(K);
        end;
    end;
end;

procedure TFormPriorInfoControl.ShowPriForm;
var ModRes: integer;
begin
  FormPriorInfo.PriSetLocal.Assign(PriSetControl);
  FormPriorInfo.PriGpsLocal.Assign(PriGpsControl);
  FormPriorInfo.PriorSetupLocal.Assign(PriorSetupControl);
  ModRes := FormPriorInfo.ShowModal;
  if (ModRes = mrOK) and (FormPriorInfo.PriorDataChanged) then
  begin
    PriSetControl.Assign(FormPriorInfo.PriSetLocal);
    PriGpsControl.Assign(FormPriorInfo.PriGpsLocal);
    PriorSetupControl.Assign(FormPriorInfo.PriorSetupLocal);
    PriorDataControlChanged := True;
  end;
end;

end.
