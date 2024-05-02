{ @abstract(The main purpose of @name is to define @link(TfrmProgressMM) which
  is used to display a progress bar to the user with extra
  information displayed in @link(TfrmProgressMM.memoMessages)
  about what is happening.)}
unit frmProgressUnit;

interface

uses
  SysUtils, Types, Winapi.Windows, Winapi.Messages, Classes, Variants,
  Graphics, Controls, Forms,
  Dialogs, StdCtrls, frmCustomGoPhastUnit, ComCtrls,
  ExtCtrls, Buttons;

type
  { @abstract(@name is used to display a progress bar to the user with extra
    information displayed in @link(memoMessages) about what is happening.)}
  TfrmProgressMM = class(TfrmCustomGoPhast)
    // @name: TLabel;
    // @name displays the amount of progress.
    // See @link(StepIt) and @link(Prefix).
    lblProgress: TLabel;
    // @name: TMemo;
    // @name displays messages to the user.
    memoMessages: TMemo;
    // @name: TProgressBar;
    // @name displays the amount of progress.
    // See @link(StepIt).
    pbProgress: TProgressBar;
    // @name: TPanel;
    // @name holds the controls at the top of @classname.
    pnlTop: TPanel;
    btnAbort: TBitBtn;
    // @name initializes @classname.
    procedure FormShow(Sender: TObject);
    procedure btnAbortClick(Sender: TObject);
    procedure memoMessagesChange(Sender: TObject);
  private
    // @name: string;
    // See @link(Prefix).
    FPrefix: string;
    LastTime: TDateTime;
    LastMessageTime: TDateTime;
    FShouldContinue: Boolean;
    // See @link(Prefix).
    procedure SetPrefix(const Value: string);
    procedure SetProgressLabelCaption(const Value: string);
    function GetProgressLabelCaption: string;
    function GetShouldContinue: Boolean;
    { Private declarations }
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    procedure BeginUpdate;
    procedure EndUpdate;
    property ShouldContinue: Boolean read GetShouldContinue write FShouldContinue;
    property ProgressLabelCaption: string read GetProgressLabelCaption
      write SetProgressLabelCaption;
    // @name is used in @link(StepIt) to help set the caption of
    // @link(lblProgress).
    // See @link(StepIt).
    property Prefix: string read FPrefix write SetPrefix;
    // @name advances @link(pbProgress) and sets the caption of
    // @link(lblProgress) based on @link(Prefix) and the amount of progress.
    procedure StepIt;
    procedure AddMessage(Const AMessage: string; AllowUpdate: boolean = True);
    { Public declarations }
  end;

var
  // @name is the instance of @link(TfrmProgressMM).
  frmProgressMM: TfrmProgressMM = nil;
  frmFileProgress: TfrmProgressMM;

implementation

resourcestring
  ProgressCaption = '%0:s%1:d out of %2:d.';
  StrAttemptingToAbort = 'Attempting to abort; please wait.';

{$R *.dfm}

const
  HalfSecond = 1/24/3600/2;

{ TfrmProgressMM }

procedure TfrmProgressMM.StepIt;
begin
  pbProgress.StepIt;
  ProgressLabelCaption := Format(ProgressCaption,
    [Prefix, pbProgress.Position, pbProgress.Max]);
end;

procedure TfrmProgressMM.AddMessage(const AMessage: string;
  AllowUpdate: boolean = True);
begin
  memoMessages.Lines.Add(AMessage);
  if AllowUpdate and (Now - LastMessageTime > HalfSecond) then
  begin
  // Don't call Application.ProcessMessages.
  // Doing so can cause access violations due to
  // TPhastModel.UpdateModflowFullStressPeriods being called from
  // within itself.
  // The basic problem is that only one view should be updated at a time.
  //
//  Application.ProcessMessages;
    LastMessageTime := Now;
  end;
end;

procedure TfrmProgressMM.BeginUpdate;
begin
  memoMessages.Lines.BeginUpdate;
end;

procedure TfrmProgressMM.btnAbortClick(Sender: TObject);
begin
  inherited;
  FShouldContinue := False;
  memoMessages.Lines.Add(StrAttemptingToAbort);
end;

procedure TfrmProgressMM.CreateParams(var Params: TCreateParams);
begin
  inherited;
  // Ensure that this dialog box has a link on the taskbar.
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
  Params.WndParent := 0;
end;

procedure TfrmProgressMM.EndUpdate;
begin
  memoMessages.Lines.EndUpdate;
end;

procedure TfrmProgressMM.FormShow(Sender: TObject);
begin
  inherited;
  LastTime := Now;
  LastMessageTime := Now;
  memoMessages.Clear;
  pbProgress.Position := 0;
  lblProgress.Caption := '';
end;

function TfrmProgressMM.GetProgressLabelCaption: string;
begin
  result := lblProgress.Caption;
end;

function TfrmProgressMM.GetShouldContinue: Boolean;
begin
  // Don't call Application.ProcessMessages.
  // Doing so can cause access violations due to
  // TPhastModel.UpdateModflowFullStressPeriods being called from
  // within itself.
  // The basic problem is that only one view should be updated at a time.
  //
//  Application.ProcessMessages;
  Result := FShouldContinue;
end;

procedure TfrmProgressMM.memoMessagesChange(Sender: TObject);
begin
  inherited;
  SendMessage(memoMessages.Handle, EM_LINESCROLL, 0,memoMessages.Lines.Count);
end;

procedure TfrmProgressMM.SetPrefix(const Value: string);
begin
  FPrefix := Value;
end;

procedure TfrmProgressMM.SetProgressLabelCaption(const Value: string);
begin
  if Now - LastTime > HalfSecond then
  begin
    lblProgress.Caption := Value;
    LastTime := Now;
  end;
end;

end.
