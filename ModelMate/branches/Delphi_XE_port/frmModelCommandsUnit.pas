unit frmModelCommandsUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Buttons,
  GlobalBasicData, GlobalData, GlobalTypesUnit, Utilities, ModelMateUtilities;

type
  TFormModelCommands = class(TForm)
    edtMCLForward: TEdit;
    odModelCommands: TOpenDialog;
    edtMCLFwdDeriv: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    sbForward: TSpeedButton;
    sbFwdDeriv: TSpeedButton;
    Label7: TLabel;
    Label3: TLabel;
    btnGenModflowForwardCommand: TButton;
    Label4: TLabel;
    Label5: TLabel;
    edtMCLPred: TEdit;
    edtMCLPredDeriv: TEdit;
    sbPred: TSpeedButton;
    spPredDeriv: TSpeedButton;
    btnGenModflowPredCommand: TButton;
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    procedure btnCancelClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure sbForwardClick(Sender: TObject);
    procedure sbFwdDerivClick(Sender: TObject);
    procedure btnGenModflowForwardCommandClick(Sender: TObject);
    procedure btnGenModflowPredCommandClick(Sender: TObject);
    procedure sbPredClick(Sender: TObject);
    procedure spPredDerivClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnOKClick(Sender: TObject);
  private
    procedure StoreCommands;
    { Private declarations }
  public
    { Public declarations }
    NeedProgLoc: boolean;
  end;

var
  FormModelCommands: TFormModelCommands;

implementation

{$R *.dfm}

procedure TFormModelCommands.btnOKClick(Sender: TObject);
begin
  StoreCommands;
end;

procedure TFormModelCommands.btnCancelClick(Sender: TObject);
begin
  Close;
end;

procedure TFormModelCommands.btnGenModflowForwardCommandClick(Sender: TObject);
var
  ProgLoc, NamFile: TFileName;
  Messg: string;
begin
  ProgLoc := '';
  case PCurrent.ModelID of
    midModflow2005: ProgLoc := GlobalProgramLocations.Modflow2005Location;
    midModflow2000: ProgLoc := GlobalProgramLocations.Modflow2000Location;
  end;
  if FileExists(ProgLoc) then
    begin
      NeedProgLoc := False;
      NamFile := PathToAbsPath(ProjectDirectory, PCurrent.ModflowNameFile);
      if FileExists(NamFile) then
        begin
          edtMCLForward.Text := BuildCommand(ProgLoc, NamFile, True);
        end
      else
        begin
          Messg := 'Unable to generate command.  Check that ' +
                   'Model...Model Settings identifies a valid MODFLOW Name File';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      NeedProgLoc := True;
      Messg := 'Location for MODFLOW executable needs to be defined';
      ShowMessage(Messg);
//      StoreCommands;
      Close;
    end;
end;

procedure TFormModelCommands.btnGenModflowPredCommandClick(Sender: TObject);
var
  ProgLoc, NamFile: TFileName;
  Messg: string;
begin
  ProgLoc := '';
  case PCurrent.ModelID of
    midModflow2005: ProgLoc := GlobalProgramLocations.Modflow2005Location;
    midModflow2000: ProgLoc := GlobalProgramLocations.Modflow2000Location;
  end;
  if FileExists(ProgLoc) then
    begin
      NeedProgLoc := False;
      NamFile := PathToAbsPath(ProjectDirectory, PCurrent.ModflowNameFilePred);
      if FileExists(NamFile) then
        begin
          edtMCLPred.Text := BuildCommand(ProgLoc, NamFile, True);
        end
      else
        begin
          Messg := 'Unable to generate command.  Check that ' +
                   'Model...Model Settings identifies a valid MODFLOW Name File';
          ShowMessage(Messg);
        end;
    end
  else
    begin
      NeedProgLoc := True;
      Messg := 'Location for MODFLOW executable needs to be defined';
      ShowMessage(Messg);
      StoreCommands;
      Close;
    end;
end;

procedure TFormModelCommands.btnSaveClick(Sender: TObject);
begin
  StoreCommands;
  Close;
end;

procedure TFormModelCommands.FormCreate(Sender: TObject);
begin
  CenterForm(self);
  NeedProgLoc := False;
end;

procedure TFormModelCommands.StoreCommands;
begin
  PCurrent.MCLForward := edtMCLForward.Text;
  PCurrent.MCLFwdDeriv := edtMCLFwdDeriv.Text;
  PCurrent.MCLPred := edtMCLPred.Text;
  PCurrent.MCLPredDeriv := edtMCLPredDeriv.Text;
end;

procedure TFormModelCommands.FormShow(Sender: TObject);
begin
  case PCurrent.ModelID of
    midModflow2005:
      begin
        btnGenModflowForwardCommand.Enabled := True;
        btnGenModflowPredCommand.Enabled := True;
      end;
    midModflow2000: ;
    midGeneric:
      begin
        btnGenModflowForwardCommand.Enabled := False;
        btnGenModflowPredCommand.Enabled := False;
      end;
  end;
  edtMCLForward.Text := PCurrent.MCLForward;
  edtMCLFwdDeriv.Text := PCurrent.MCLFwdDeriv;
  edtMCLPred.Text := PCurrent.MCLPred;
  edtMCLPredDeriv.Text := PCurrent.MCLPredDeriv;
end;

procedure TFormModelCommands.sbForwardClick(Sender: TObject);
var
  Path, Dir: string;
begin
  if edtMCLForward.Text <> '' then
    begin
      Path := GetFirstWord(edtMCLForward.Text); // Eliminate arguments, if any
      Dir := ExtractFileDir(Path);
      odModelCommands.InitialDir := Dir;
    end;
  if odModelCommands.Execute then
    begin
      edtMCLForward.Text := PathWithoutSpaces(odModelCommands.FileName);
    end;
end;

procedure TFormModelCommands.sbFwdDerivClick(Sender: TObject);
var
  Path, Dir: string;
begin
  if edtMCLFwdDeriv.Text <> '' then
    begin
      Path := GetFirstWord(edtMCLFwdDeriv.Text); // Eliminate arguments, if any.
      Dir := ExtractFileDir(Path);
      odModelCommands.InitialDir := Dir;
    end;
  if odModelCommands.Execute then
    begin
      edtMCLFwdDeriv.Text := PathWithoutSpaces(odModelCommands.FileName);
    end;
end;

procedure TFormModelCommands.sbPredClick(Sender: TObject);
var
  Path, Dir: string;
begin
  if edtMCLPred.Text <> '' then
    begin
      Path := GetFirstWord(edtMCLPred.Text); // Eliminate arguments, if any.
      Dir := ExtractFileDir(Path);
      odModelCommands.InitialDir := Dir;
    end;
  if odModelCommands.Execute then
    begin
      edtMCLPred.Text := PathWithoutSpaces(odModelCommands.FileName);
    end;
end;

procedure TFormModelCommands.spPredDerivClick(Sender: TObject);
var
  Path, Dir: string;
begin
  if edtMCLPredDeriv.Text <> '' then
    begin
      Path := GetFirstWord(edtMCLPredDeriv.Text); // Eliminate arguments, if any.
      Dir := ExtractFileDir(Path);
      odModelCommands.InitialDir := Dir;
    end;
  if odModelCommands.Execute then
    begin
      edtMCLPredDeriv.Text := PathWithoutSpaces(odModelCommands.FileName);
    end;
end;


end.
