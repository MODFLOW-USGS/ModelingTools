{@abstract(@name defines @link(TCheckInternetThread). @link(TCheckInternetThread)
is started when ModelMuse starts.  It checks a file on the Internet to
see if ModelMuse has been updated.  It also downloads a list of available
videos about ModelMuse and will start one if appropriate.)
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit CheckInternetUnit;

interface

uses System.UITypes, Windows, SysUtils, Classes, Dialogs, Forms, IniFiles,
  JvExStdCtrls, JvHtControls, System.Generics.Collections, Xml.VerySimple;

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
    FOldToNewDictionary: TDictionary<string, string>;
    FNewToOldDictionary: TDictionary<string, string>;
    FVideos: TXmlVerySimple;
    procedure CheckWeb;
    procedure ReadIniFile;
    function CheckVersion(const ExternalVersionString: string): TVersionCompare;
    procedure ShowNewVersionMessage;
    procedure GetAppName;
    procedure CheckCurrentUrl;
    procedure UpdateIniFile;
    procedure DestroyIniFile;
    procedure NewVideoMessage;
    procedure GetLastDates;
  public
    Constructor Create(ModelVersion: string; IniFile: TMemInifile; ShowTips: boolean);
    destructor Destroy; override;
    procedure Execute; override;
  end;

const
  StrVideoDisplayed = 'VideoDisplayed';
  StrTipDate = 'TipDate';
  StrInternetCheckDate = 'InternetCheckDate';
  StrIntroVideoURL = 'https://www.usgs.gov/mission-areas/water-resources/mod' +
  'elmuse-introductory-video';


implementation

uses
  Math, RbwInternetUtilities, frmGoPhastUnit, IniFileUtilities, GoPhastTypes,
  StdCtrls, frmNewVersionUnit, System.IOUtils;

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
  VerCompar: TVersionCompare;
  Index: Integer;
  XmlText: TStringList;
  MemStream: TMemoryStream;
  NodeIndex: Integer;
  ANode: TXmlNode;
  NewUrl: string;
  InnerNodeIndex: Integer;
  ChildNode: TXmlNode;
  OldUrl: string;
  ErrorUrl: Boolean;
begin
  XmlText := TStringList.Create;
  try
    try
      ErrorUrl := True;
      if ReadInternetFile(VideoUpdateURL, XmlText, FAppName) then
      begin
        if (XmlText.Count >= 2) and (XmlText[1] = '<html>') then
        begin
          ErrorUrl := True;
        end
        else
        begin
          ErrorUrl := False;
        end;
      end;
      if not ErrorUrl then
      begin
        MemStream := TMemoryStream.Create;
        try
          XmlText.SaveToStream(MemStream);
          MemStream.Position := 0;
          FVideos.LoadFromStream(MemStream);
        finally
          MemStream.Free;
        end;
        for NodeIndex := 1 to FVideos.ChildNodes.Count - 1 do
        begin
          ANode := FVideos.ChildNodes[NodeIndex];
          NewUrl := Trim(ANode.Name);
          if Pos('https:', NewUrl) <> 1 then
          begin
            raise Exception.Create('Invalid URL');
          end;
          if NewUrl = '' then
          begin
            Continue;
          end;
          for InnerNodeIndex := 0 to ANode.ChildNodes.Count - 1 do
          begin
            ChildNode := ANode.ChildNodes[InnerNodeIndex];
            OldUrl := ChildNode.Name;
            if Pos('https:', OldUrl) <> 1 then
            begin
              raise Exception.Create('Invalid URL');
            end;
            FOldToNewDictionary.Add(OldUrl, NewUrl);
            FNewToOldDictionary.Add(NewUrl, OldUrl);
          end;
        end;
      end;
    except
      FVideos.Clear;
      FOldToNewDictionary.Clear;
      FNewToOldDictionary.Clear;
    end;
  finally
    XmlText.Free;
  end;

  try
    try
      Synchronize(GetAppName);
      Synchronize(GetLastDates);

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
              FCurrentURL := StrIntroVideoURL;
              Synchronize(CheckCurrentUrl);
              if not FCurrentUrlHasBeenDisplayed then
              begin
                LaunchURL(FBrowser, FCurrentURL);
                Synchronize(UpdateIniFile);
              end
              else
              begin
                for Index := 0 to FVideoURLs.Count - 1 do
                begin
                  FCurrentURL := FVideoURLs[Index];
                  if FNewToOldDictionary.ContainsKey(FCurrentURL) then
                  begin
                    Synchronize(CheckCurrentUrl);
                    if not FCurrentUrlHasBeenDisplayed then
                    begin
                      LaunchURL(FBrowser, FCurrentURL);
                      Synchronize(UpdateIniFile);
                      break;
                    end;
                  end;
                end;
              end;
            end;
          end
          else if (FNewVideoCount > 0) then
          begin
            Synchronize(NewVideoMessage);
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
  FOldToNewDictionary := TDictionary<string, string>.Create;
  FNewToOldDictionary := TDictionary<string, string>.Create;
  FVideos := TXmlVerySimple.Create;
end;

procedure TCheckInternetThread.DestroyIniFile;
begin
  FVideos.Free;
  FIniFile.Free;
end;

destructor TCheckInternetThread.Destroy;
begin
  FNewToOldDictionary.Free;
  FOldToNewDictionary.Free;
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
    Lbl.Caption := Format('<u><a href="https://www.usgs.gov/mission-areas/water-resources/modelmuse-tutorial-videos-0">%s</a></u>', [StrClickHereForModel]);
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
  NewerUrl: string;
  OlderURL: string;
  OldPos: Integer;
begin
  FNewVideoCount := 0;
//  FShowVideos := FIniFile.ReadBool(StrCustomization, StrShowTips, True);
  FIniFile.ReadSection(StrVideoDisplayed, FVideoURLs);
  // delete videos that are no longer available.
  // update file names that have been replaced.
  if FUpdateText.Count > 0 then
  begin
    for Index := 1 to FVideoURLs.Count - 1 do
    begin
      OldURL := FVideoURLs[Index];
      NewUrl := OldURL;
      if FOldToNewDictionary.TryGetValue(OldURL, NewerUrl) then
      begin
        HasDisplayed := FIniFile.ReadBool(StrVideoDisplayed, OldUrl, False);
        FIniFile.DeleteKey(StrVideoDisplayed, OldURL);
        FIniFile.WriteBool(StrVideoDisplayed, NewerUrl, HasDisplayed);
        FVideoURLs[Index] := NewerUrl;
        OldPos := FUpdateText.IndexOf(OldURL);
        if OldPos >= 0 then
        begin
          FUpdateText[OldPos] := NewerUrl
        end;
      end
      else if FNewToOldDictionary.TryGetValue(NewUrl, OlderURL) then
      begin
        OldPos := FUpdateText.IndexOf(OlderURL);
        if OldPos >= 0 then
        begin
          FUpdateText[OldPos] := NewUrl;
          FIniFile.DeleteKey(StrVideoDisplayed, OlderURL);
        end;
      end
      else if FUpdateText.IndexOf(OldURL) < 0 then
      begin
        FIniFile.DeleteKey(StrVideoDisplayed, OldURL);
      end;
    end;
    // Add new files from XML file.
    for Index := 1 to FVideos.ChildNodes.Count - 1 do
    begin
      NewUrl := Trim(FVideos.ChildNodes[Index].Name);
      if (NewUrl <> '') and (FUpdateText.IndexOf(NewUrl) < 0) then
      begin
        FUpdateText.Add(NewUrl);
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
end;

procedure TCheckInternetThread.GetLastDates;
begin
  try
    FLastTipDate := FIniFile.ReadDateTime(StrCustomization, StrTipDate, 0);
  except on EConvertError do
    FLastTipDate := Now;
  end;

  try
    FLastCheckInternetDate := FIniFile.ReadDateTime(StrCustomization, StrInternetCheckDate, FLastTipDate);
  except on EConvertError do
    FLastCheckInternetDate := Now;
  end;
end;


end.
