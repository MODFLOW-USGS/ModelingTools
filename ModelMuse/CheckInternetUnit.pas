{@abstract(@name defines @link(TCheckInternetThread). @link(TCheckInternetThread)
is started when ModelMuse starts.  It checks a file on the Internet to
see if ModelMuse has been updated.  It also downloads a list of available
videos about ModelMuse and will start one if appropriate.)
@author(Richard B. Winston <rbwinst@usgs.gov>) 
}
unit CheckInternetUnit;

interface

uses System.UITypes, Windows, SysUtils, Classes, Dialogs, Forms, IniFiles,
  JvExStdCtrls, JvHtControls;

type
  TVersionCompare = (vcUnknown, vcSame, vcExternalOlder, vcExternalNewer);

  TCheckInternetThread = class(TThread)
  private
    FModelVersion: string;
    FIniFile: TMemInifile;
    FShowVideos: Boolean;
    FBrowser: string;
    FAppName: string;
    FVideoURLs: TStringList;
    FUpdateText: TStringList;
    FLastTipDate: Extended;
    FCurrentURL: String;
    FCurrentUrlHasBeenDisplayed: Boolean;
    FLastCheckInternetDate: TDateTime;
    FNewVideoCount: Integer;
    FVersionOnWeb: string;
    procedure CheckWeb;
    procedure ReadIniFile;
    function CheckVersion(const ExternalVersionString: string): TVersionCompare;
    procedure ShowNewVersionMessage;
    procedure GetAppName;
    procedure CheckCurrentUrl;
    procedure UpdateIniFile;
    procedure DestroyIniFile;
    procedure NewVideoMessage;
  public
    Constructor Create(ModelVersion: string; IniFile: TMemInifile; ShowTips: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

const
  StrVideoDisplayed = 'VideoDisplayed';
  StrTipDate = 'TipDate';
  StrInternetCheckDate = 'InternetCheckDate';
  
implementation

uses
  Math, RbwInternetUtilities, frmGoPhastUnit, IniFileUtilities, GoPhastTypes, 
  StdCtrls, frmNewVersionUnit, System.IOUtils, Xml.VerySimple;

resourcestring
  StrYourVersionS = 'Your version: %s';
  StrNewVersionS = 'New version: %s';
  StrClickHereForModel = 'Click here for ModelMuse Videos';
  StrThereIsANewVideo = 'There is a new video on the ModelMuse web site.';


const
  UpdateURL = 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseInternetUpdate.txt';
  VideoUpdateURL = 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/Videos.xml';

{ TCheckInternetThread }

function TCheckInternetThread.CheckVersion(
  const ExternalVersionString: string): TVersionCompare;
var
  LocalVersionList: TStringList;
  ExternalVersionList: TStringList;
  Index: Integer;
  LocalVersion, ExternalVersion: integer;
  CasVar: Integer;
begin
  result := vcUnknown;
  LocalVersionList := TStringList.Create;
  ExternalVersionList := TStringList.Create;
  try
    LocalVersionList.Delimiter := '.';
    ExternalVersionList.Delimiter := '.';
    LocalVersionList.DelimitedText := FModelVersion;
    ExternalVersionList.DelimitedText := ExternalVersionString;
    if ExternalVersionList.Count = LocalVersionList.Count then
    begin
      for Index := 0 to LocalVersionList.Count - 1 do
      begin
        LocalVersion := StrToInt(LocalVersionList[Index]);
        ExternalVersion := StrToInt(ExternalVersionList[Index]);
        CasVar := Sign(LocalVersion - ExternalVersion);
        if CasVar < 0 then
        begin
          result := vcExternalNewer;
          Exit;
        end
        else if CasVar > 0 then
        begin
          result := vcExternalOlder;
          Exit;
        end
        else
        begin
          result := vcSame;
        end;
      end;
    end;
  finally
    ExternalVersionList.Free;
    LocalVersionList.Free;
  end;
end;

procedure TCheckInternetThread.CheckCurrentUrl;
begin
  FCurrentUrlHasBeenDisplayed := FIniFile.ReadBool(StrVideoDisplayed, FCurrentURL, False)
end;

procedure TCheckInternetThread.CheckWeb;
var
//  VersionOnWeb: string;
  VerCompar: TVersionCompare;
  Index: Integer;
  MemStream: TMemoryStream;
begin
  try
    try
      Synchronize(GetAppName);
      Synchronize(ReadIniFile);

      if (Now - FLastCheckInternetDate) > 0.95 then
      begin
        if ReadInternetFile(UpdateURL, FUpdateText, FAppName) then
        begin
          if FUpdateText.Count > 0 then
          begin
            FVersionOnWeb := FUpdateText[0];
            VerCompar := CheckVersion(FVersionOnWeb);
            case VerCompar of
              vcUnknown, vcSame, vcExternalOlder: ; // do nothing
              vcExternalNewer:
                begin
                  Synchronize(ShowNewVersionMessage);
                end
              else Assert(False)
            end;
            FUpdateText.Delete(0);
            if FUpdateText.Count > 0 then
            begin
              Synchronize(ReadIniFile);
            end;
          end;
          if FShowVideos then
          begin
            if (Now - FLastTipDate) > 0.95 then
            begin
              for Index := 0 to FVideoURLs.Count - 1 do
              begin
                FCurrentURL := FVideoURLs[Index];
                Synchronize(CheckCurrentUrl);
                if not FCurrentUrlHasBeenDisplayed then
                begin
                  LaunchURL(FBrowser, FCurrentURL);
                  Synchronize(UpdateIniFile);
                  break;
                end;
              end;
            end;
          end
          else if (FNewVideoCount > 0) then
          begin
            Synchronize(NewVideoMessage);
          end;

          if ReadInternetFile(VideoUpdateURL, FUpdateText, FAppName) then
          begin
            MemStream := TMemoryStream.Create;
            try
              FUpdateText.SaveToStream(MemStream);
              MemStream.Position := 0;
              Videos := TXmlVerySimple.Create;
              try
                try
                  Videos.LoadFromStream(MemStream);
                except
                  Exit;
                end;
              finally
                Videos.Free;
              end;
            finally
              MemStream.Free;
            end;
          end;

        end;



        Synchronize(UpdateIniFile);

      end;
    except on E: EInternetConnectionError do
      begin
        Terminate;
      end;
    end
  finally
    Synchronize(DestroyIniFile);
  end;
end;

constructor TCheckInternetThread.Create(ModelVersion: string;
  IniFile: TMemInifile; ShowTips: boolean);
begin
  inherited Create(False);
  FModelVersion := ModelVersion;
  FIniFile := IniFile;
  FVideoURLs := TStringList.Create;
  FUpdateText := TStringList.Create;
  FreeOnTerminate := True;
  FShowVideos := ShowTips;
end;

procedure TCheckInternetThread.DestroyIniFile;
begin
  FIniFile.Free;
end;

destructor TCheckInternetThread.Destroy;
begin
  FVideoURLs.Free;
  FUpdateText.Free;
  inherited;
end;

procedure TCheckInternetThread.Execute;
begin
  CheckWeb;
end;

procedure TCheckInternetThread.UpdateIniFile;
const
  BackupExtension = '.webinibak';
var
  BackupFilename: string;
begin
  if FCurrentURL <> '' then
  begin
    FIniFile.WriteBool(StrVideoDisplayed, FCurrentURL, True);
  end;
  FIniFile.WriteDateTime(StrCustomization, StrTipDate, Now);
  FIniFile.WriteDateTime(StrCustomization, StrInternetCheckDate, Now);
  BackupFilename := ChangeFileExt(FIniFile.FileName, BackupExtension);
//  if TFile.Exists(BackupFilename) then
//  begin
//    TFile.Delete(BackupFilename);
//  end;
  if TFile.Exists(FIniFile.FileName) then
  begin
    try
      TFile.Copy(FIniFile.FileName, BackupFilename, True);
    except on EInOutError do
      begin
        Exit;
      end;
    end;
  end;
  try
    FIniFile.UpdateFile;
  except on EFCreateError do
    begin
      Sleep(100);
      try
        FIniFile.UpdateFile;
      except on E: EFCreateError do
        begin
          Beep;
          MessageDlg(E.message, mtWarning, [mbOK], 0);
          if TFile.Exists(FIniFile.FileName)
            and TFile.Exists(BackupFilename) then
          begin
            TFile.Delete(FIniFile.FileName);
            TFile.Move(BackupFilename, FIniFile.FileName);
          end;
        end;
      end;
    end;
  end;
end;

procedure TCheckInternetThread.GetAppName;
begin
  FAppName := ExtractFileName(Application.Name);
end;

procedure TCheckInternetThread.NewVideoMessage;
var
  Lbl: TJvHTLabel;
  AForm: TForm;
  AMessage: string;
begin
  if FNewVideoCount = 1 then
  begin
    AMessage := StrThereIsANewVideo;
  end
  else
  begin
    AMessage := Format('There are  %d new videos on the ModelMuse web site.', [FNewVideoCount]);
  end;
  AForm := CreateMessageDialog(AMessage, mtInformation, [mbOK]);
  try
    Lbl := TJvHTLabel.Create(AForm);
    Lbl.Parent := AForm;
//    Lbl.Caption := '<u><a href="http://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseVideos.html">'
//      + StrClickHereForModel+'</a></u>';
    Lbl.Caption := Format('<u><a href="https://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseVideos.html">%s</a></u>', [StrClickHereForModel]);
    Lbl.Left := (AForm.ClientWidth - Lbl.Width) div 2;
    Lbl.Top := 40;
    AForm.ShowModal;
  finally
    AForm.Free;
  end;
end;

procedure TCheckInternetThread.ShowNewVersionMessage;
begin
  Beep;

  with TfrmNewVersion.Create(nil) do
  begin
    try
      lblYourVersion.Caption := Format(StrYourVersionS, [FModelVersion]);
      lblVersionOnWeb.Caption := Format(StrNewVersionS, [FVersionOnWeb]);
      ShowModal;
    finally
      Free;
    end;
  end;
//  ShowMessage('A newer version of ModelMuse is ' + 'now available on the ModelMuse web site.');
end;

procedure TCheckInternetThread.ReadIniFile;
var
  Index: Integer;
  OldURL: string;
  NewURL: string;
  HasDisplayed: Boolean;
begin
  FNewVideoCount := 0;
//  FShowVideos := FIniFile.ReadBool(StrCustomization, StrShowTips, True);
  FIniFile.ReadSection(StrVideoDisplayed, FVideoURLs);
  if FUpdateText.Count > 0 then
  begin
    for Index := 0 to FVideoURLs.Count - 1 do
    begin
      OldURL := FVideoURLs[Index];
      if FUpdateText.IndexOf(OldURL) < 0 then
      begin
        FIniFile.DeleteKey(StrVideoDisplayed, OldURL);
      end;
    end;
  end;
  for Index := 0 to FUpdateText.Count - 1 do
  begin
    NewURL := FUpdateText[Index];
    HasDisplayed := FIniFile.ReadBool(StrVideoDisplayed, NewURL, False);
    FIniFile.WriteBool(StrVideoDisplayed, NewURL, HasDisplayed);
    if FVideoURLs.IndexOf(NewURL) < 0 then
    begin
      FVideoURLs.Add(NewURL);
      Inc(FNewVideoCount);
    end;
  end;
//  if FShowVideos then
  begin
    try
      FLastTipDate := FIniFile.ReadDateTime(StrCustomization, StrTipDate, 0);
    except on EConvertError do
      FLastTipDate := Now;
    end;
  end;
  try
    FLastCheckInternetDate := FIniFile.ReadDateTime(StrCustomization, StrInternetCheckDate, FLastTipDate);
  except on EConvertError do
    FLastCheckInternetDate := Now;
  end;
end;

end.
