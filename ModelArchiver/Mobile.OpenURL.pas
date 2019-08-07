unit Mobile.OpenURL;

(***
 *
 * Universal code to open URL at mobile for Delphi XE5.
 *
 * written by P.J.Strnad.
 *
 * Based on solutions from: http://stackoverflow.com/questions/16354876/opening-url-within-ios-application
 * and http://stackoverflow.com/questions/18873699/how-do-i-open-urls-pdfs-etc-with-the-default-apps
 *
 **)

interface

procedure openURL(url:String);

implementation

uses
{$IF defined(IOS)} //  and NOT defined(CPUARM)
 FMX.Helpers.iOS, iOSapi.Foundation;
{$ELSEIF defined(ANDROID)}
 Androidapi.JNI.GraphicsContentViewText,
 FMX.Helpers.Android;
{$ELSE}
 Winapi.Windows, Winapi.ShellAPI;
{$ENDIF}

{$IF defined(IOS)} //  and NOT defined(CPUARM)
procedure openURL(url:String);
begin
 SharedApplication.openURL(TNSURL.Wrap(TNSURL.OCClass.URLWithString(NSSTR(PChar(String(url))))));
end;

{$ELSEIF defined(ANDROID)}

procedure openURL(url:String);
var
  Intent: JIntent;
begin
 Intent := TJIntent.Create;
 Intent.setAction(TJIntent.JavaClass.ACTION_VIEW);
 Intent.setData(StrToJURI(url));
 SharedActivity.startActivity(Intent);
end;

{$ELSE}

procedure openURL(url:String);
begin
 shellExecute(0, 'open', PWideChar(url), nil, nil, SW_SHOW);
end;
{$ENDIF}

end.
