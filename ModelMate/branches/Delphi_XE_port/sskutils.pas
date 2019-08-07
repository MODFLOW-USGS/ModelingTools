{**************************************************
   sskutils.pas -- Various utility functions

   Copyright (C) 1999-2001, Sanjay Kanade
   http://kanadepro.com/delphistuff/
**************************************************}

unit sskutils;

interface

uses windows, classes;

//shows help topic in a secondary window
//pass 'main' or '' for main window
procedure showHelpInWindow(helpContextNum: integer; winname: string);

//shows help topic in a popup window
procedure showHelpInPopup(helpContextNum: integer);

//gets the file version from version info set in the project options
function getMyFileVersion: string;

function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;

implementation

uses sysutils, forms, shellapi;

//*******************************************
//pass 'main' or '' for main window
procedure showHelpInWindow(helpContextNum: integer; winname: string);
var
  helpFilePlusWindowName: string;
begin
  helpFilePlusWindowName := application.HelpFile;
  if winname <> '' then
    helpFilePlusWindowName := helpFilePlusWindowName + '>' + winname;
  WinHelp(application.mainForm.Handle, PChar(helpFilePlusWindowName), HELP_CONTEXT, helpContextNum);
end;

//*******************************************
procedure showHelpInPopup(helpContextNum: integer);
begin
  application.helpcommand(HELP_CONTEXTPOPUP, helpContextNum);
end;

//*****************************************
//gets the file version for display on the about
//box.
function getMyFileVersion: string;
const
  vqvFmt = '\StringFileInfo\%4.4x%4.4x\%s';
var
  size: longint;
  vlen: DWord;
  FInfo: pointer;
  FLang: PInteger;
  vptr: pchar;
  sval: string;
  //slen: integer;
begin
  result := '';
  size := GetFileVersionInfoSize(pchar(application.ExeName), vlen);
  if size > 0 then
  begin
    GetMem(FInfo, size);
    if GetFileVersionInfo(pchar(application.ExeName), vlen, size, FInfo) then
    begin
      // get languages
      VerQueryValue(FInfo, '\VarFileInfo\Translation', pointer(FLang), vlen);
      if VerQueryValue(FInfo, pchar(Format(vqvFmt, [LoWord(FLang^), HiWord(FLang^), 'FileVersion'])), pointer(vptr), vlen) then
      begin
        //The returned value is x.x.x.x
        //We drop the last .x and join 2nd and 3rd
        //We also assume that each is 1 character only
        // ERB: Edited 4/16/08 to comment-out code that shortens the version string
        sval := vptr;
        {
        slen := length(sval);
        while (slen > 3) and ((sval[slen]='0') or (sval[slen]='.')) do
        begin
          Delete(sval,slen,1);
          dec(slen);
        end;
        }
        result := sval;
      end;
    end;    
    FreeMem(FInfo, size);
  end;
end;

//*****************************************
//This function is taken from Borland's fmxutils.pas unit
function ExecuteFile(const FileName, Params, DefaultDir: string;
  ShowCmd: Integer): THandle;
var
  zFileName, zParams, zDir: array[0..79] of Char;
begin
  Result := ShellExecute(Application.MainForm.Handle, nil,
    StrPCopy(zFileName, FileName), StrPCopy(zParams, Params),
    StrPCopy(zDir, DefaultDir), ShowCmd);
end;

end.
