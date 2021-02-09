unit frmSvdaPrepInputUnit;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, frmCustomGoPhastUnit, Vcl.StdCtrls,
  Vcl.Mask, JvExMask, JvSpin, Vcl.ExtCtrls, Vcl.Buttons, UndoItems, SvdaPrepPropertiesUnit,
  JvToolEdit;

type
  TUndoSvdaPrepProperties = class(TCustomUndo)
  private 
    FOldSvdaPrep: TSvdaPrepProperties;
    FNewSvdaPrep: TSvdaPrepProperties;
    procedure AssignSvdaPrep(SvdaPrep: TSvdaPrepProperties);
  protected
    function Description: string; override;
  public  
    Constructor Create(var NewSvdaPrep: TSvdaPrepProperties);
    destructor Destroy; override;
    procedure DoCommand; override;
    procedure Undo; override;
  end;

  TfrmSvdaPrepInput = class(TfrmCustomGoPhast)
    rgSuperParameterCalculation: TRadioGroup;
    seNumSupParameters: TJvSpinEdit;
    lblNumSupParameters: TLabel;
    cbRunSvdaPrep: TCheckBox;
    cbRunPest: TCheckBox;
    btnHelp: TBitBtn;
    btnOK: TBitBtn;
    btnCancel: TBitBtn;
    fedPestControlFile: TJvFilenameEdit;
    lblPestFileName: TLabel;
    procedure FormCreate(Sender: TObject); override;
    procedure btnOKClick(Sender: TObject);
    procedure fedPestControlFileChange(Sender: TObject);
  private
    procedure GetData;
    procedure SetData; 
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSvdaPrepInput: TfrmSvdaPrepInput;

implementation

uses
  frmGoPhastUnit, System.IOUtils;

resourcestring
  StrYouMustSelectAnE = 'You must select an existing PEST control file.';

{$R *.dfm}

{ TfrmSvdaPrepInput }

procedure TfrmSvdaPrepInput.btnOKClick(Sender: TObject);
begin
  inherited;
  if not (TFile.Exists(fedPestControlFile.FileName)) then
  begin
    Beep;
    MessageDlg(StrYouMustSelectAnE, mtError, [mbOK], 0);
    ModalResult := mrNone;
    Exit;
  end;
  SetData;
end;

procedure TfrmSvdaPrepInput.fedPestControlFileChange(Sender: TObject);
begin
  inherited;
  if not (TFile.Exists(fedPestControlFile.FileName)) then
  begin
    fedPestControlFile.Color := clRed;
  end
  else
  begin
    fedPestControlFile.Color := clWindow;
  end;
end;

procedure TfrmSvdaPrepInput.FormCreate(Sender: TObject);
begin
  inherited;
  GetData;
end;

procedure TfrmSvdaPrepInput.GetData;
var
  SvdaPrep: TSvdaPrepProperties;
begin
  SvdaPrep := frmGoPhast.PhastModel.SvdaPrepProperties;
  fedPestControlFile.FileName := SvdaPrep.FileName;
  rgSuperParameterCalculation.ItemIndex := Ord(SvdaPrep.Method);
  seNumSupParameters.AsInteger := SvdaPrep.NumberOfSuperParameters;
  cbRunSvdaPrep.Checked := SvdaPrep.RunSvdaPrep;
  cbRunPest.Checked := SvdaPrep.RunPest;
end;

procedure TfrmSvdaPrepInput.SetData;
var
  InvalidateModelEvent: TNotifyEvent;
  SvdaPrep: TSvdaPrepProperties;
begin
  InvalidateModelEvent := nil;
  SvdaPrep := TSvdaPrepProperties.Create(InvalidateModelEvent);
  try
    SvdaPrep.FileName := fedPestControlFile.FileName;
    SvdaPrep.Method := TSvdaMethod(rgSuperParameterCalculation.ItemIndex);
    SvdaPrep.NumberOfSuperParameters := seNumSupParameters.AsInteger;
    SvdaPrep.RunSvdaPrep := cbRunSvdaPrep.Checked;
    SvdaPrep.RunPest := cbRunPest.Checked;
    frmGoPhast.UndoStack.Submit(TUndoSvdaPrepProperties.Create(SvdaPrep));
  finally
    SvdaPrep.Free;
  end;
end;

{ TUndoSvdaPrepProperties }

procedure TUndoSvdaPrepProperties.AssignSvdaPrep(SvdaPrep: TSvdaPrepProperties);
begin
  frmGoPhast.PhastModel.SvdaPrepProperties := SvdaPrep;
end;

constructor TUndoSvdaPrepProperties.Create(
  var NewSvdaPrep: TSvdaPrepProperties);
var
  InvalidateModelEvent: TNotifyEvent;
begin
  InvalidateModelEvent := nil;
  FOldSvdaPrep := TSvdaPrepProperties.Create(InvalidateModelEvent);
  FOldSvdaPrep.Assign(frmGoPhast.PhastModel.SvdaPrepProperties);
  FNewSvdaPrep := NewSvdaPrep;
  NewSvdaPrep := nil;
end;

function TUndoSvdaPrepProperties.Description: string;
begin
  result := 'change SvdaPrep options'
end;

destructor TUndoSvdaPrepProperties.Destroy;
begin
  FOldSvdaPrep.Free;
  FNewSvdaPrep.Free;
  inherited;
end;

procedure TUndoSvdaPrepProperties.DoCommand;
begin
  inherited;
  AssignSvdaPrep(FNewSvdaPrep);
end;

procedure TUndoSvdaPrepProperties.Undo;
begin
  inherited;
  AssignSvdaPrep(FOldSvdaPrep);
end;

end.
