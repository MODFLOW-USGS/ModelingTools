{@abstract(@name defines @link(TCheckInternetThread). @link(TCheckInternetThread)
is started when ModelMuse starts.  It checks a file on the Internet to
see if ModelMuse has been updated.  It also downloads a list of available
videos about ModelMuse and will start one if appropriate.)
@author(Richard B. Winston <rbwinst@usgs.gov>)
}
unit CheckInternetUnit;

interface

uses System.UITypes, Windows, SysUtils, Classes, Dialogs, Forms, IniFiles,
  JvExStdCtrls, JvHtControls, System.Generics.Collections, Xml.VerySimple,
  System.StrUtils;

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
    FUrlToXmlNodeDictionary: TDictionary<string, TXmlNode>;
    FNewVideos: TList<TXmlNode>;
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
  StrUpdated = 'Updated';
  StrTrue = 'True';
  StrFalse = 'False';


implementation

uses
  Math, RbwInternetUtilities, frmGoPhastUnit, IniFileUtilities, GoPhastTypes,
  StdCtrls, frmNewVersionUnit, System.IOUtils, frmNewVideoUnit;

resourcestring
  StrYourVersionS = 'Your version: %s';
  StrNewVersionS = 'New version: %s';
  StrClickHereForModel = 'Click here for ModelMuse Videos';
  StrThereIsANewVideo = 'There is a new video on the ModelMuse web site.';
  StrThereAreDNewVi = 'There are  %d new videos on the ModelMuse web site.';


const
  UpdateURL =      'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/ModelMuseInternetUpdate.txt';
  VideoUpdateURL = 'https://water.usgs.gov/nrp/gwsoftware/ModelMuse/Videos.xml';
  StrTopic = 'Topic';

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
  XmlText: TStringList;
  MemStream: TMemoryStream;
  NodeIndex: Integer;
  ANode: TXmlNode;
  NewUrl: string;
  InnerNodeIndex: Integer;
  ChildNode: TXmlNode;
  OldUrl: string;
  ErrorUrl: Boolean;
  AValue: string;
  Updated: Boolean;
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
        // The first child node that has URLS is #1 not #0
        for NodeIndex := 1 to FVideos.ChildNodes.Count - 1 do
        begin
          ANode := FVideos.ChildNodes[NodeIndex];
          NewUrl := Trim(ANode.Name);
          if NewUrl = '' then
          begin
            Continue;
          end;
          if Pos('https:', NewUrl) <> 1 then
          begin
            raise Exception.Create('Invalid URL');
          end;
          FUrlToXmlNodeDictionary.Add(NewUrl,ANode);
          if ANode.HasAttribute(StrUpdated) then
          begin
            AValue := ANode.Attributes[StrUpdated];
            Updated := AValue = StrTrue;
          end
          else
          begin
            Updated := False;
          end;
          if Updated then
          begin
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
              FUpdateText.Clear;
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
                for NodeIndex := 1 to FVideos.ChildNodes.Count - 1 do
                begin
                  ANode := FVideos.ChildNodes[NodeIndex];
                  if ANode.HasAttribute(StrUpdated) then
                  begin
                    AValue := ANode.Attributes[StrUpdated];
                    Updated := AValue = StrTrue;
                  end
                  else
                  begin
                    Updated := False;
                  end;
                  if Updated then
                  begin
                    FCurrentURL := Trim(ANode.Name);
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
  FUrlToXmlNodeDictionary := TDictionary<string, TXmlNode>.Create;
  FNewVideos := TList<TXmlNode>.Create;

  FVideos := TXmlVerySimple.Create;
end;

procedure TCheckInternetThread.DestroyIniFile;
begin
  FIniFile.Free;
end;

destructor TCheckInternetThread.Destroy;
begin
  FVideos.Free;
  FNewVideos.FreeInstance;
  FUrlToXmlNodeDictionary.Free;
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
//  FIniFile.WriteSection(StrVideoDisplayed, FVideoURLs);

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
  index: Integer;
  ANode: TXmlNode;
  TopicName: string;
begin
  Application.CreateForm(TfrmNewVideos, frmNewVideos);
  try
    frmNewVideos.memoNewVideos.Lines.BeginUpdate;
    try
      for index := 0 to FNewVideos.Count - 1 do
      begin
        ANode := FNewVideos[index];
        if ANode.HasAttribute(StrTopic) then
        begin
          TopicName := ANode.Attributes[StrTopic];
          if TopicName <> '' then
          begin
            frmNewVideos.memoNewVideos.Lines.Add(TopicName);
          end;
        end;
      end;
    finally
      frmNewVideos.memoNewVideos.Lines.EndUpdate;
    end;
    frmNewVideos.ShowModal;
  finally
    frmNewVideos.Free
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
  UpdatedOldURL: string;
  ANode: TXmlNode;
begin
  FNewVideoCount := 0;
  FIniFile.ReadSection(StrVideoDisplayed, FVideoURLs);
  for Index := 0 to FVideoURLs.Count - 1 do
  begin
    OldURL := FVideoURLs[Index];
    if Pos('http:', OldURL) = 1 then
    begin
      UpdatedOldURL := ReplaceStr(OldURL, 'http:', 'https:');
      HasDisplayed := FIniFile.ReadBool(StrVideoDisplayed, OldURL, False);
      FIniFile.DeleteKey(StrVideoDisplayed, OldURL);
      FIniFile.WriteBool(StrVideoDisplayed, UpdatedOldURL, HasDisplayed);
      FVideoURLs[Index] := UpdatedOldURL;
    end;

  end;
  // delete videos that are no longer available.
  // update file names that have been replaced.
  for Index := 0 to FVideoURLs.Count - 1 do
  begin
    OldURL := FVideoURLs[Index];
    NewUrl := OldURL;
    if FOldToNewDictionary.TryGetValue(OldURL, NewerUrl) then
    begin
      HasDisplayed := FIniFile.ReadBool(StrVideoDisplayed, OldUrl, False);
      FIniFile.DeleteKey(StrVideoDisplayed, OldURL);
      FIniFile.WriteBool(StrVideoDisplayed, NewerUrl, HasDisplayed);
      FVideoURLs[Index] := NewerUrl;
    end
    else if FNewToOldDictionary.TryGetValue(NewUrl, OlderURL) then
    begin
      HasDisplayed := FIniFile.ReadBool(StrVideoDisplayed, OlderURL, False)
        or FIniFile.ReadBool(StrVideoDisplayed, NewUrl, False);
      FIniFile.DeleteKey(StrVideoDisplayed, OlderURL);
      FIniFile.WriteBool(StrVideoDisplayed, NewUrl, HasDisplayed);
    end;
  end;
  // Add new files from XML file.
  // The first child node that has URLS is #1 not #0
  for Index := 1 to FVideos.ChildNodes.Count - 1 do
  begin
    NewUrl := Trim(FVideos.ChildNodes[Index].Name);
    if (NewUrl <> '') and (FUpdateText.IndexOf(NewUrl) < 0) then
    begin
      FUpdateText.Add(NewUrl);
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
      if FUrlToXmlNodeDictionary.TryGetValue(NewURL, ANode) then
      begin
        FNewVideos.Add(ANode);
      end;
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
