{

Fast Memory Manager: FullDebugMode Support DLL 1.60

Description:
 Support DLL for FastMM. With this DLL available, FastMM will report debug info
 (unit name, line numbers, etc.) for stack traces.

Usage:
 1) To compile you will need the JCL library (http://sourceforge.net/projects/jcl/)
 2) Place in the same location as the replacement borlndmm.dll or your
 application's executable module.

Change log:
 Version 1.00 (9 July 2005):
  - Initial release.
 Version 1.01 (13 July 2005):
  - Added the option to use madExcept instead of the JCL Debug library. (Thanks
    to Martin Aignesberger.)
 Version 1.02 (30 September 2005):
  - Changed options to display detail for addresses inside libraries as well.
 Version 1.03 (13 October 2005):
  - Added a raw stack trace procedure that implements raw stack traces.
 Version 1.10 (14 October 2005):
  - Improved the program logic behind the skipping of stack levels to cause
    less incorrect entries in raw stack traces. (Thanks to Craig Peterson.)
 Version 1.20 (17 October 2005):
  - Improved support for madExcept stack traces. (Thanks to Mathias Rauen.)
 Version 1.30 (26 October 2005):
  - Changed name to FastMM_FullDebugMode to reflect the fact that there is now
    a static dependency on this DLL for FullDebugMode. The static dependency
    solves a DLL unload order issue. (Thanks to Bart van der Werf.)
 Version 1.40 (31 October 2005):
  - Added support for EurekaLog. (Thanks to Fabio Dell'Aria.)
 Version 1.42 (23 June 2006):
  - Fixed a bug in the RawStackTraces code that may have caused an A/V in some
    rare circumstances. (Thanks to Primoz Gabrijelcic.)
 Version 1.44 (16 November 2006):
  - Changed the RawStackTraces code to prevent it from modifying the Windows
    "GetLastError" error code. (Thanks to Primoz Gabrijelcic.)
 Version 1.50 (14 August 2008):
  - Added support for Delphi 2009. (Thanks to Mark Edington.)
 Version 1.60 (5 May 2009):
  - Improved the code used to identify call instructions in the stack trace
    code. (Thanks to the JCL team.)
 Version 1.61 (5 September 2010):
  - Recompiled using the latest JCL in order to fix a possible crash on shutdown
    when the executable contains no debug information. (Thanks to Hanspeter
    Widmer.)

}

{--------------------Start of options block-------------------------}

{Select the stack tracing library to use. The JCL, madExcept and EurekaLog are
 supported. Only one can be used at a time.}
{$define JCLDebug}
{.$define madExcept}
{.$define EurekaLog}

{--------------------End of options block-------------------------}

// JCL_DEBUG_EXPERT_INSERTJDBG OFF
library FastMM_FullDebugMode;

uses
  {$ifdef JCLDebug}JCLDebug{$endif}
  {$ifdef madExcept}madStackTrace{$endif}
  {$ifdef EurekaLog}ExceptionLog{$endif},
  SysUtils, Windows;

{$R *.res}

{$STACKFRAMES ON}

{--------------------------Frame Based Stack Tracing--------------------------}

{Dumps the call stack trace to the given address. Fills the list with the
 addresses where the called addresses can be found. This is the fast stack
 frame based tracing routine.}
procedure GetFrameBasedStackTrace(AReturnAddresses: PCardinal; AMaxDepth, ASkipFrames: Cardinal);
var
  LStackTop, LStackBottom, LCurrentFrame: Cardinal;
begin
  {Get the call stack top and current bottom}
  asm
    mov eax, FS:[4]
    sub eax, 3
    mov LStackTop, eax
    mov LStackBottom, ebp
  end;
  {Get the current frame start}
  LCurrentFrame := LStackBottom;
  {Fill the call stack}
  while (AMaxDepth > 0)
    and (LCurrentFrame >= LStackBottom)
    and (LCurrentFrame < LStackTop) do
  begin
    {Ignore the requested number of levels}
    if ASkipFrames = 0 then
    begin
      AReturnAddresses^ := PCardinal(LCurrentFrame + 4)^;
      Inc(AReturnAddresses);
      Dec(AMaxDepth);
    end
    else
      Dec(ASkipFrames);
    {Get the next frame}
    LCurrentFrame := PCardinal(LCurrentFrame)^;
  end;
  {Clear the remaining dwords}
  while (AMaxDepth > 0) do
  begin
    AReturnAddresses^ := 0;
    Inc(AReturnAddresses);
    Dec(AMaxDepth);
  end;
end;

{-----------------------------Raw Stack Tracing-----------------------------}

const
  {Hexadecimal characters}
  HexTable: array[0..15] of AnsiChar = '0123456789ABCDEF';

type
  {The state of a memory page. Used by the raw stack tracing mechanism to
   determine whether an address is a valid call site or not.}
  TMemoryPageAccess = (mpaUnknown, mpaNotExecutable, mpaExecutable);

var
  {There are a total of 1M x 4K pages in the 4GB address space}
  MemoryPageAccessMap: array[0..1024 * 1024 - 1] of TMemoryPageAccess;

{Updates the memory page}
procedure UpdateMemoryPageAccessMap(AAddress: Cardinal);
var
  LMemInfo: TMemoryBasicInformation;
  LAccess: TMemoryPageAccess;
  LStartPage, LPageCount: Cardinal;
begin
  {Query the page}
  if VirtualQuery(Pointer(AAddress), LMemInfo, SizeOf(LMemInfo)) <> 0 then
  begin
    {Get access type}
    if (LMemInfo.State = MEM_COMMIT)
      and (LMemInfo.Protect and (PAGE_EXECUTE_READ or PAGE_EXECUTE_READWRITE
        or PAGE_EXECUTE_WRITECOPY or PAGE_EXECUTE) <> 0)
      and (LMemInfo.Protect and PAGE_GUARD = 0) then
    begin
      LAccess := mpaExecutable
    end
    else
      LAccess := mpaNotExecutable;
    {Update the map}
    LStartPage := Cardinal(LMemInfo.BaseAddress) div 4096;
    LPageCount := LMemInfo.RegionSize div 4096;
    if (LStartPage + LPageCount) < Cardinal(Length(MemoryPageAccessMap)) then
      FillChar(MemoryPageAccessMap[LStartPage], LPageCount, Ord(LAccess));
  end
  else
  begin
    {Invalid address}
    MemoryPageAccessMap[AAddress div 4096] := mpaNotExecutable;
  end;
end;

{Returns true if the return address is a valid call site. This function is only
 safe to call while exceptions are being handled.}
function IsValidCallSite(AReturnAddress: Cardinal): boolean;
var
  LCallAddress, LCode8Back, LCode4Back, LTemp: Cardinal;
begin
  if AReturnAddress > $ffff then
  begin
    {The call address is up to 8 bytes before the return address}
    LCallAddress := AReturnAddress - 8;
    {Update the page map}
    if MemoryPageAccessMap[LCallAddress div 4096] = mpaUnknown then
      UpdateMemoryPageAccessMap(LCallAddress);
    {Check the page access}
    if (MemoryPageAccessMap[LCallAddress div 4096] = mpaExecutable)
      and (MemoryPageAccessMap[(LCallAddress + 8) div 4096] = mpaExecutable) then
    begin
      {Try to determine what kind of call it is (if any), more or less in order
       of frequency of occurrence. (Code below taken from the Jedi Code Library
       (jcl.sourceforge.net).)}
      try
        {5 bytes, CALL NEAR REL32}
        if PByteArray(LCallAddress)[3] = $E8 then
        begin
          Result := True;
          Exit;
        end;
        {Get the 4 bytes before the return address}
        LCode4Back := PCardinal(LCallAddress + 4)^;
        {2 byte call?}
        LTemp := LCode4Back and $F8FF0000;
        {2 bytes, CALL NEAR EAX}
        if LTemp = $D0FF0000 then
        begin
          Result := True;
          Exit;
        end;
        {2 bytes, CALL NEAR [EAX]}
        if LTemp = $10FF0000 then
        begin
          LTemp := LCode4Back - LTemp;
          if (LTemp <> $04000000) and (LTemp <> $05000000) then
          begin
            Result := True;
            Exit;
          end;
        end;
        {3 bytes, CALL NEAR [EAX+EAX*i]}
        if (LCode4Back and $00FFFF00) = $0014FF00 then
        begin
          Result := True;
          Exit;
        end;
        {3 bytes, CALL NEAR [EAX+$12]}
        if ((LCode4Back and $00F8FF00) = $0050FF00)
          and ((LCode4Back and $00070000) <> $00040000) then
        begin
          Result := True;
          Exit;
        end;
        {4 bytes, CALL NEAR [EAX+EAX+$12]}
        if Word(LCode4Back) = $54FF then
        begin
          Result := True;
          Exit;
        end;
        {6 bytes, CALL NEAR [$12345678]}
        LCode8Back := PCardinal(LCallAddress)^;
        if (LCode8Back and $FFFF0000) = $15FF0000 then
        begin
          Result := True;
          Exit;
        end;
        {6 bytes, CALL NEAR [EAX+$12345678]}
        if ((LCode8Back and $F8FF0000) = $90FF0000)
          and ((LCode8Back and $07000000) <> $04000000) then
        begin
          Result := True;
          Exit;
        end;
        {7 bytes, CALL NEAR [EAX+EAX+$1234567]}
        if (LCode8Back and $00FFFF00) = $0094FF00 then
        begin
          Result := True;
          Exit;
        end;
        {7 bytes, CALL FAR $1234:12345678}
        if (LCode8Back and $0000FF00) = $00009A00 then
        begin
          Result := True;
          Exit;
        end;
        {Not a valid call site}
        Result := False;
      except
        {The access has changed}
        UpdateMemoryPageAccessMap(LCallAddress);
        {Not executable}
        Result := False;
      end;
    end
    else
      Result := False;
  end
  else
    Result := False;
end;

{Dumps the call stack trace to the given address. Fills the list with the
 addresses where the called addresses can be found. This is the "raw" stack
 tracing routine.}
procedure GetRawStackTrace(AReturnAddresses: PCardinal; AMaxDepth, ASkipFrames: Cardinal);
var
  LStackTop, LStackBottom, LCurrentFrame, LNextFrame, LReturnAddress,
    LStackAddress, LLastOSError: Cardinal;
begin
  {Are exceptions being handled? Can only do a raw stack trace if the possible
   access violations are going to be handled.}
  if Assigned(ExceptObjProc) then
  begin
    {Save the last Windows error code}
    LLastOSError := GetLastError;
    {Get the call stack top and current bottom}
    asm
      mov eax, FS:[4]
      sub eax, 3
      mov LStackTop, eax
      mov LStackBottom, ebp
    end;
    {Get the current frame start}
    LCurrentFrame := LStackBottom;
    {Fill the call stack}
    while (AMaxDepth > 0)
      and (LCurrentFrame < LStackTop) do
    begin
      {Get the next frame}
      LNextFrame := PCardinal(LCurrentFrame)^;
      {Is it a valid stack frame address?}
      if (LNextFrame < LStackTop)
        and (LNextFrame > LCurrentFrame) then
      begin
        {The pointer to the next stack frame appears valid: Get the return
         address of the current frame}
        LReturnAddress := PCardinal(LCurrentFrame + 4)^;
        {Does this appear to be a valid return address}
        if (LReturnAddress and $ffff0000) <> 0 then
        begin
          {Is the map for this return address incorrect? It may be unknown or marked
           as non-executable because a library was previously not yet loaded, or
           perhaps this is not a valid stack frame.}
          if MemoryPageAccessMap[(LReturnAddress - 8) div 4096] <> mpaExecutable then
            UpdateMemoryPageAccessMap(LReturnAddress - 8);
          {Is this return address actually valid?}
          if IsValidCallSite(LReturnAddress) then
          begin
            {Ignore the requested number of levels}
            if ASkipFrames = 0 then
            begin
              AReturnAddresses^ := LReturnAddress;
              Inc(AReturnAddresses);
              Dec(AMaxDepth);
            end;
          end
          else
          begin
            {If the return address is invalid it implies this stack frame is
             invalid after all.}
            LNextFrame := LStackTop;
          end;
        end
        else
        begin
          {The return address is bad - this is not a valid stack frame}
          LNextFrame := LStackTop;
        end;
      end
      else
      begin
        {This is not a valid stack frame}
        LNextFrame := LStackTop;
      end;
      {Do not check intermediate entries if there are still frames to skip}
      if ASkipFrames <> 0 then
      begin
        Dec(ASkipFrames);
      end
      else
      begin
        {Check all stack entries up to the next stack frame}
        LStackAddress := LCurrentFrame + 8;
        while (AMaxDepth > 0)
          and (LStackAddress < LNextFrame) do
        begin
          {Get the return address}
          LReturnAddress := PCardinal(LStackAddress)^;
          {Is this a valid call site?}
          if IsValidCallSite(LReturnAddress) then
          begin
            AReturnAddresses^ := LReturnAddress;
            Inc(AReturnAddresses);
            Dec(AMaxDepth);
          end;
          {Check the next stack address}
          Inc(LStackAddress, 4);
        end;
      end;
      {Do the next stack frame}
      LCurrentFrame := LNextFrame;
    end;
    {Clear the remaining dwords}
    while (AMaxDepth > 0) do
    begin
      AReturnAddresses^ := 0;
      Inc(AReturnAddresses);
      Dec(AMaxDepth);
    end;
    {Restore the last Windows error code, since a VirtualQuery call may have
     modified it.}
    SetLastError(LLastOSError);
  end
  else
  begin
    {Exception handling is not available - do a frame based stack trace}
    GetFrameBasedStackTrace(AReturnAddresses, AMaxDepth, ASkipFrames);
  end;
end;

{-----------------------------Stack Trace Logging----------------------------}

{Gets the textual representation of the stack trace into ABuffer and returns
 a pointer to the position just after the last character.}
{$ifdef JCLDebug}
{Converts a cardinal to a hexadecimal string at the buffer location, returning
 the new buffer position.}
function CardinalToHexBuf(ACardinal: integer; ABuffer: PAnsiChar): PAnsiChar;
asm
  {On entry:
    eax = ACardinal
    edx = ABuffer}
  push ebx
  push edi
  {Save ACardinal in ebx}
  mov ebx, eax
  {Get a pointer to the first character in edi}
  mov edi, edx
  {Get the number in ecx as well}
  mov ecx, eax
  {Keep the low nibbles in ebx and the high nibbles in ecx}
  and ebx, $0f0f0f0f
  and ecx, $f0f0f0f0
  {Swap the bytes into the right order}
  ror ebx, 16
  ror ecx, 20
  {Get nibble 7}
  movzx eax, ch
  mov dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 6}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 5}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 4}
  movzx eax, bl
  or dl, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Rotate ecx and ebx so we get access to the rest}
  shr ebx, 16
  shr ecx, 16
  {Get nibble 3}
  movzx eax, ch
  or dl, ch
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 2}
  movzx eax, bh
  or dl, bh
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 1}
  movzx eax, cl
  or dl, cl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  cmp dl, 1
  sbb edi, -1
  {Get nibble 0}
  movzx eax, bl
  mov al, byte ptr HexTable[eax]
  mov [edi], al
  {Return a pointer to the end of the string}
  lea eax, [edi + 1]
  {Restore registers}
  pop edi
  pop ebx
end;

{Subroutine used by LogStackTrace}
procedure AppendInfoToString(var AString: string; const AInfo: string);
begin
  if AInfo <> '' then
    AString := Format('%s[%s]', [AString, AInfo]);
end;

function LogStackTrace(AReturnAddresses: PCardinal;
  AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
var
  LInd, LAddress: Cardinal;
  LNumChars: Integer;
  LInfo: TJCLLocationInfo;
  LTempStr: string;
begin
  Result := ABuffer;
  for LInd := 0 to AMaxDepth - 1 do
  begin
    LAddress := AReturnAddresses^;
    if LAddress = 0 then
      Exit;
    Result^ := #13;
    Inc(Result);
    Result^ := #10;
    Inc(Result);
    Result := CardinalToHexBuf(LAddress, Result);
    {Get location info for the caller (at least one byte before the return
     address).}
    GetLocationInfo(Pointer(Cardinal(LAddress) - 1), LInfo);
    {Build the result string}
    LTempStr := ' ';
    AppendInfoToString(LTempStr, LInfo.SourceName);
    AppendInfoToString(LTempStr, LInfo.UnitName);
    AppendInfoToString(LTempStr, LInfo.ProcedureName);
    if LInfo.LineNumber <> 0 then
      AppendInfoToString(LTempStr, IntToStr(LInfo.LineNumber));
    {Return the result}
    if Length(LTempStr) < 256 then
      LNumChars := Length(LTempStr)
    else
      LNumChars := 255;
    StrLCopy(Result, PAnsiChar(AnsiString(LTempStr)), LNumChars);
    Inc(Result, LNumChars);
    {Next address}
    Inc(AReturnAddresses);
  end;
end;
{$endif}

{$ifdef madExcept}
function LogStackTrace(AReturnAddresses: PCardinal;
  AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
begin
  {Needs madExcept 2.7i or madExcept 3.0a or a newer build}
  Result := madStackTrace.FastMM_LogStackTrace(
    AReturnAddresses,
    AMaxDepth,
    ABuffer,
    {madExcept stack trace fine tuning}
    false, //hide items which have no line number information?
    true,  //show relative address offset to procedure entrypoint?
    true,  //show relative line number offset to procedure entry point?
    false  //skip special noise reduction processing?
    );
end;
{$endif}

{$ifdef EurekaLog}
function LogStackTrace(AReturnAddresses: PCardinal; AMaxDepth: Cardinal; ABuffer: PAnsiChar): PAnsiChar;
begin
  {Needs EurekaLog 5.0.5 or a newer build}
  Result := ExceptionLog.FastMM_LogStackTrace(
    AReturnAddresses, AMaxDepth, ABuffer,
    {EurekaLog stack trace fine tuning}
    False, // Show the DLLs functions call.  <--|
           //                                   |-- See the note below!
    False, // Show the BPLs functions call.  <--|
    True  // Show relative line no. offset to procedure start point.
    );
// NOTE:
// -----
// With these values set both to "False", EurekaLog try to returns the best
// call-stack available.
//
// To do this EurekaLog execute the following points:
// --------------------------------------------------
// 1)...try to fill all call-stack items using only debug data with line no.
// 2)...if remains some empty call-stack items from the previous process (1),
//      EurekaLog try to fill these with the BPLs functions calls;
// 3)...if remains some empty call-stack items from the previous process (2),
//      EurekaLog try to fill these with the DLLs functions calls;
end;
{$endif}

{-----------------------------Exported Functions----------------------------}

exports
  GetFrameBasedStackTrace,
  GetRawStackTrace,
  LogStackTrace;

begin
{$ifdef JCLDebug}
  JclStackTrackingOptions := JclStackTrackingOptions + [stAllModules];
{$endif}
end.
