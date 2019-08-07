unit ActivApp;
{ Allows switching between open delphi applications and/or lauching (any) application
  Note: App to Switch too must have TActivateApp component
  two Methods:        1) ActivateApp - Switch to open App, if App Closed Then Launch It
                      2) ExecuteApp - Launch App
  one Event:          1) BeforeLaunchApp - Allows one to discontinue Lauching of app when
                         ActivateApp senses app to switch to is not open, does not stop
                         launching of app when execute method used.
      Properties:    1) MainFormTitle - Title On Main form of Application to Activate when
                        using AppActivate Method only. IF An MDI Application then included the
                        FULL title displayed on the titlebar of the main form
                     2) ExePath - Full path to executable including Exe name and any parameters
                        Used by both ActivateApp & Execute App
  Freeware Use & Abuse
  Author: Edward de la Rey
          edwardr@mailbox.ru.ac.za
          USE AT OWN RISK
  For Delphi 1,2,3 Will Automatically pickup the correct DCR File,
  D16 is for Delphi1, D32 for Delphi 2&3, Don't rename them.}
 

interface

{$IFDEF WIN32}
uses
  Windows, Messages, SysUtils, Classes, Forms;
{$ELSE}
uses
  SysUtils, WinTypes, WinProcs, Messages, Classes, Forms;
{$ENDIF}

const
WM_ShowYourSelf = WM_USER + 402;

type
  TMYParamEvent = Procedure (Sender:TObject;var Continue:Boolean) of object;
  TSuccess = Procedure (Sender:TObject;Result:Boolean) of object;
  TActivApp = class(Tcomponent)
  private
  GetAppToActivate: AnsiString;
  GetExePath: AnsiString;
  EBeforeLaunchApp:TMYParamEvent;
  Procedure SetAppToActivate(Value:AnsiString);
  Procedure SetExePath(Value:AnsiString);
  protected
    { Protected declarations }
  procedure ShowYourSelf(var Msg: TMsg; var Handled: Boolean);
  public
    { Public declarations }
  published
  Constructor Create(AOwner: TComponent); override;
  Procedure ActivateApp;
  Procedure ExecuteApp (var Success:Boolean);
  Property MainFormTitle:AnsiString Read GetAppToActivate Write SetAppToActivate;
  Property ExePath:AnsiString Read GetExePath Write SetExePath;
  Property BeforeLaunchApp:TMYParamEvent Read EBeforeLaunchApp Write EBeforeLaunchApp;
  end;

procedure Register;

implementation

{$IFDEF WIN32}
{$R *.D32}
{$ELSE}
{$R *.D16}
{$ENDIF}

constructor TActivApp.Create(AOwner: TComponent);
begin
   INHERITED CREATE(Aowner);
   GetAppToActivate:='MyOtherApp';
   GetExePath:=ExtractFilePath(application.ExeName);
   if not (csDesigning in ComponentState) then
   Application.OnMessage:=ShowYourSelf;
End;

procedure TActivApp.ShowYourSelf(var Msg: TMsg; var Handled: Boolean);
begin
 {This procedure handles messages sent from other Apps}
    Handled:=false;
    if msg.message = WM_ShowYourSelf then
    begin
      Application.Restore;
      application.BringToFront;
      Handled:=true;
    end;
End;

Procedure TActivApp.ExecuteApp(var Success:Boolean);
Var
//It:PChar;
It:AnsiString;
Ans:integer;
begin
//    it:=Stralloc ((length(GetExePath))+2);
    it:=GetExePath;
//    strPCopy(It,GetExePath);
//    Ans:=WinExec(It,SW_SHOW);
    Ans:=WinExec(PAnsiChar(It),SW_SHOW);
//    strDispose(it);
    Success:= Ans > 31;
end;

procedure TActivApp.ActivateApp;
Var
// it,AppToActiv:PChar;
 it,AppToActiv:AnsiString;
 MyHandle:HWnd;
 Success:Integer;
 Continue:Boolean;
begin
//  AppToActiv:=Stralloc ((length(GetAppToActivate))+2);
  AppToActiv:=GetAppToActivate;
//  strPCopy(AppToActiv,GetAppToActivate);
  MyHandle := FindWindowA(nil,PAnsiChar(AppToActiv));
//  strDispose(ApptoActiv);
  if MyHandle <> 0 then begin
   PostMessage(MyHandle,WM_ShowYourSelf,0,0); {Unminimize and bring to front}
   {nb Must use PostMessage Only}
  end {My Handle <> 0}
  else begin
    Continue:=true;
    If Assigned(EBeforeLaunchApp) Then EBeforeLaunchApp(Self,Continue);
    if Continue Then begin
//    it:=Stralloc ((length(GetExePath))+2);
    it:=GetExePath;
//    strPCopy(It,GetExePath);
    Success:=WinExec(PAnsiChar(It),SW_SHOW);
//    strDispose(it);
    end; {Continue}
  end;{Else MyHAndle = 0}
 end;

procedure TActivApp.SetAppToActivate (Value:AnsiString);
begin
     GetAppToActivate:=Value;
end;

procedure TActivApp.SetExePath (Value:AnsiString);
begin
     GetExePath:=Value;
end;

procedure Register;
begin
  RegisterComponents('Freeware', [TActivApp]);
end;

end.
