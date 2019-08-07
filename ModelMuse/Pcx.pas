// Davie Reed, January 1999
// E-Mail:  davie@smatters.com

unit PCX;

interface

Uses
   Windows, Classes, Graphics;
{
{ Setup the following variable before calling LoadFromFileX
{
{ Global_KeepTrueFormat:Word
{ 0 = Use the files native bits per pixel for the TBitMap
{ 1 = Force TBitMap of 256 colors and use gray it file was 24bit
{ 2 = Force TBitMap to 24bit
{
{ SAVETOFILEX(parm1,parm2,parm3);
{    Parm1=Filename
{    Parm2=TBitMap to save
{    Parm3=Type of PCX file to create
{           1 = Save as 256 Color file
{           2 = Save as 16M file
{
{ ****************** ERROR HANDLING ******************
{ If you want a special message displayed if there is an error
{ while saving a PCX, then search for    ###ERROR###    and you will
{ find the block of code that has nothing in it currently. Just put in
{ whatever logic you like. For example: An error message :)
}

{$J+}
{$R-}

Procedure LoadFromFileX(FileName:String;Const BitMap:TBitMap);
Procedure SaveToFileX(FileName:String;Const BitMap:TBitMap;PcxType:Byte);

implementation

Type
   TypeRegVer=Set Of (Non_Registered,Registered,OEM,PRO,SYSOP);
   DataLineArray=Array[0..65535] Of Byte;
   DataWordArray=Array[0..65535] Of SmallInt;
   FakePalette= Packed Record
      LPal : TLogPalette;
      Dummy:Array[1..255] of TPaletteEntry;
      End;

   TypeEgaPalette=Array[0..16] Of Byte;
   TypePalette=Array[0..255,1..3] Of Byte;

Const
   Global_HiColor=3;
   Global_KeepTrueFormat:Word=0;

   Global_PaletteDef:Array[0..15,1..3] Of Byte = (
{Black}                                       (0  ,0  ,0 ),
{Blue}                                        (0  ,0  ,32),
{Green}                                       (0  ,32 ,0 ),
{Cyan}                                        (0  ,32 ,32),
{Red}                                         (32 ,0  ,0 ),
{Magenta}                                     (32 ,0  ,32),
{Brown}                                       (32 ,32 ,0 ),
{Light Gray}                                  (42 ,42 ,42),
{Dark Gray}                                   (21 ,21 ,21),
{Light Blue}                                  (0  ,0  ,63),
{Light Green}                                 (0  ,63 ,0 ),
{Light Cyan}                                  (0  ,63 ,63),
{Light Red}                                   (63 ,0  ,0 ),
{Light Magenta}                               (63 ,0  ,63),
{Yellow}                                      (63 ,63 ,0 ),
{Bright White}                                (63 ,63 ,63)
                                              );

Var
  PictureFile:File;
  PaletteVGA:TypePalette;
  SysPal:FakePalette;
  TempArrayD:^DataLineArray;
  TempArrayD2:^DataLineArray;
  TempArrayDBIg,TempArrayDBig16:^DataLineArray;
  ErrorString:ShortString;
  Width:Word;
  Height:Word;
  BitsPerPixel:SmallInt;
  MyKeepTrueFormat:Boolean;
  MyKeepTrueBits:Word;
Var
  PcxVersion:Word;
  PcxColorPlanes:Byte;
  PcxEncoding:Word;
  PcxBytesPerLine:Word;
  PcxPaletteType:Word;

Const
  Index1:Word=0;
  Index2:Word=0;
  Const4096=8*1024;
Var
  IndexData:Array[0..Const4096-1] Of Byte;
Procedure FileGetMore;
Var
  NumRead:Integer;
Begin
FillChar(IndexData,Const4096,0);
BlockRead(PictureFile,IndexData,Const4096,NumRead);
Index1:=Const4096;
Index2:=0;
End;

Procedure FastGetBytes(Var Ptr1;NumBytes:Word);
Var
  X:Integer;
Begin
{
{ If we have enough the block it!
{ Otherwise do one at a time!
}
If Index1<NumBytes Then
   Begin
   If Index1=0 Then
      Begin
      FileGetMore;
      End;
   For X:=0 To NumBytes-1 Do
       Begin
       DataLineArray(Ptr1)[X]:=IndexData[Index2];
       Inc(Index2);
       Dec(Index1);
       If Index1=0 Then
          FileGetMore;
       End;
   End
Else
   Begin
   {
   { Block it fast!
   }
   Move(IndexData[Index2],DataLineArray(Ptr1)[0],NumBytes);
   Index2:=Index2+Numbytes;
   Index1:=Index1-NumBytes;
   End;
End;

Function FastGetByte:Byte;
Begin
If Index1=0 Then
   Begin
   FileGetMore;
   End;
FastGetByte:=IndexData[Index2];
Inc(Index2);
Dec(Index1);
End;

Function FastGetWord:Word;
Begin
FastGetWord:=Word(FastGetByte)+Word(FastGetByte)*256;
End;

Procedure FileIoReset;
Begin
Index1:=0;
Index2:=0;
End;

Procedure OpenFile(Var FileName:String;Var FileOk:Boolean);
Var
  Io:Integer;
  OldFileMode:Word;
Begin
FileIoReset;
OldFileMode:=FileMode;
FileMode:=0;
AssignFile(PictureFile,FileName);
ReSet(PictureFile,1);
Io:=IoResult;
If Io<>0 Then
   Begin
   FileOk:=False;
   End;
FileMode:=OldFileMode;
End;

Procedure FillerUp(Var TempArrayD;Size:Word;B1:Byte);
Begin
FillChar(TempArrayD,Size,B1);
End;

Procedure ConvertXBitsToYBits(Var Input,Output:DataLineArray;Xbits,Ybits,Width:Word);
Var
  X,Z:Word;
  B1:Byte;
Begin
{
{ Generic converter to a single data line :)
{ Can go only from smaller bits to larger bits, otherwise you need to
{     dither down!
{ PaletteVGA MUST be setup already!
}
Case Xbits Of
     1:Begin
       Case Ybits Of
            4:Begin
              {
              { From 1 bit to 4 bit, hmmmmm EZ :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 3] Shr (7-(X Mod 8))) And 1;
                  OutPut[X Shr 1]:=OutPut[X Shr 1] Or (B1 Shl ((1-(X Mod 2))*4));
                  End;
              End;
            8:Begin
              {
              { From 1 bit to 8 bit, hmmmmm EZ :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 3] Shr (7-(X Mod 8) )) And 1;
                  OutPut[X]:=B1;
                  End;
              End;
           24:Begin
              {
              { From 1 bit to 8 bit, hmmmmm EZ :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=((Input[X Shr 3] Shr (7-(X Mod 8))) And 1)*255;
                  OutPut[Z+0]:=B1;
                  OutPut[Z+1]:=B1;
                  OutPut[Z+2]:=B1;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
     4:Begin
       Case Ybits Of
            4:Begin
              Move(Input[0],Output[0],Width);
              End;
            8:Begin
              {
              { Go from 4 bits to 8 bit :)
              }
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 1] Shr ((1-(X Mod 2))*4)) And $0F;
                  OutPut[X]:=B1;
                  End;
              End;
           24:Begin
              {
              { Go from 4 bits to 24 bit :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=(Input[X Shr 1] Shr ((1-(X Mod 2))*4)) And $0F;
                  OutPut[Z+0]:=(PaletteVGA[B1,3]*255) Div 63;
                  OutPut[Z+1]:=(PaletteVGA[B1,2]*255) Div 63;
                  OutPut[Z+2]:=(PaletteVGA[B1,1]*255) Div 63;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
     8:Begin
       Case Ybits Of
            1:Begin
              For X:=0 To Width-1 Do
                  OutPut[X Shr 3]:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=InPut[X];
                  OutPut[X Shr 3]:=OutPut[X Shr 3] Or (B1 Shl (7-(X Mod 8)));
                  End;
              End;
            8:Begin
              Move(Input[0],Output[0],Width);
              End;
           24:Begin
              {
              { From 8 bit to 24 bit, hmmmmm 2EZ :)
              }
              Z:=0;
              For X:=0 To Width-1 Do
                  Begin
                  B1:=Input[X];
                  OutPut[Z+0]:=(PaletteVGA[B1,3]*255) Div 63;
                  OutPut[Z+1]:=(PaletteVGA[B1,2]*255) Div 63;
                  OutPut[Z+2]:=(PaletteVGA[B1,1]*255) Div 63;
                  Z:=Z+3;
                  End;
              End;
           End;
       End;
    24:Begin
       Case Ybits Of
            24:Begin
               Move(Input[0],Output[0],Width*3);
               End;
            End;
       End;
    End;
End;


Procedure SetUpMaskGrayPalette;
Var
  I,J:Word;
Begin
For J:=0 To 255 Do
    Begin
    For I:=1 To 3 Do
        Begin
        PaletteVga[J,I]:=J*63 Div 255;
        End;
    End;
End;

Function PCXGrayValue(R,G,B:Word):Word;
Begin
PCXGrayValue:=((R Shl 5)+(G Shl 6)+(B*12)) Div 108;
End;

Procedure MakePalBW(Const BitMap:TBitMap);
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=2;
Syspal.LPal.PalPalEntry[0].peRed:=0;
Syspal.LPal.PalPalEntry[0].peGreen:=0;
Syspal.LPal.PalPalEntry[0].peBlue:=0;
Syspal.LPal.PalPalEntry[0].peFlags:=0;
Syspal.Dummy[1].peRed:=255;
Syspal.Dummy[1].peGreen:=255;
Syspal.Dummy[1].peBlue:=255;
Syspal.Dummy[1].peFlags:=0;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure MakePalPalette(Const BitMap:TBitMap);
Var
  I:Word;
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=256;
For I:=0 To 255 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=  (PaletteVga[I,1])*4;
    Syspal.LPal.PalPalEntry[I].peGreen:=(PaletteVga[I,2])*4;
    Syspal.LPal.PalPalEntry[I].peBlue:= (PaletteVga[I,3])*4;
    Syspal.LPal.PalPalEntry[I].peFlags:= 0;
    End;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure MakePalPaletteX(Const BitMap:TBitMap;HowMany:Word);
Var
  I:Word;
Begin
SysPal.LPal.palVersion:=$300;
SysPal.LPal.palNumEntries:=HowMany;
For I:=0 To HowMany-1 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=  (PaletteVga[I,1])*4;
    Syspal.LPal.PalPalEntry[I].peGreen:=(PaletteVga[I,2])*4;
    Syspal.LPal.PalPalEntry[I].peBlue:= (PaletteVga[I,3])*4;
    Syspal.LPal.PalPalEntry[I].peFlags:= 0;
    End;
Bitmap.Palette:= CreatePalette(Syspal.LPal);
End;

Procedure SaveThePalette(Const HPal:HPalette;Var SavePal:TypePalette);
Var
  I:Word;
Begin
For I:=0 To 255 Do
    Begin
    Syspal.LPal.PalPalEntry[I].peRed:=0;
    Syspal.LPal.PalPalEntry[I].peGreen:=0;
    Syspal.LPal.PalPalEntry[I].peBlue:=0;
    End;
GetPaletteEntries(HPal,0,256,SysPal.LPal.PalPalEntry[0]);
For I:=0 To 255 Do
    Begin
    SavePal[I,1]:=(((Syspal.LPal.PalPalEntry[I].peRed)) Div 4);
    SavePal[I,2]:=(((Syspal.LPal.PalPalEntry[I].peGreen)) Div 4);
    SavePal[I,3]:=(((Syspal.LPal.PalPalEntry[I].peBlue)) Div 4);
    End;
End;

Procedure MakeGenPalette;
Var
  X:Word;
  R,G,B:Word;
Begin
X:=0;
For R:=0 To 7 Do
    Begin
    For G:=0 To 7 Do
        Begin
        For B:=0 To 3 Do
            Begin
            PaletteVga[X,1]:=(R+1)*8-1;
            PaletteVga[X,2]:=(G+1)*8-1;
            PaletteVga[X,3]:=(B+1)*16-1;
            Inc(X);
            End;
        End;
    End;
End;

Function  ShouldIKeepTrueFormat(Var BPP:Word):Boolean;
Begin
{
{ Choices
{    Use File Colors
{    Force 256 Colors
{    Force 16M Colors
}
If Global_KeepTrueFormat=0 Then
   ShouldIKeepTrueFormat:=True
Else
   ShouldIKeepTrueFormat:=False;
If Global_KeepTrueFormat=1 Then
   BPP:=8;
If Global_KeepTrueFormat=2 Then
   BPP:=24;
End;

Procedure DetColorVGA (Var PValue:Byte;MapValue:Byte);
Begin
PValue:=MapValue Div 4;
End;

Procedure PaletteDefaults;
Var
  J,I:Word;
Begin
For J:=0 To 15 Do
    Begin
    For I:=1 To 3 Do
        PaletteVGA[J,I]:=Global_PaletteDef[J,I];
    End;
End;

Procedure SetUpMaskAndColorMap;
Var
  R,G,B,PalBlue,PalGreen,PalRed:Byte;
  I:Integer;
  ColorMapSize:Integer;

Begin
{
{ Handle black and white images
}
ColorMapSize:=1 shl BitsPerPixel;
If BitsPerPixel=24 Then
   SetUpMaskGrayPalette
Else
    Begin
    For I:=0 to ColorMapSize-1 do
    Begin
              PalRed  :=FastGetbyte;
              PalGreen:=FastGetbyte;
              PalBlue :=FastGetbyte;
              If PcxVersion=2 Then
                 Begin
                 If PalRed<4 Then
                    PalRed:=PalRed*$55;
                 If PalGreen<4 Then
                    PalGreen:=PalGreen*$55;
                 If PalBlue<4 Then
                    PalBlue:=PalBlue*$55;
                 End;

    DetColorVGA (R,PalRed  );
    DetColorVGA (G,PalGreen);
    DetColorVGA (B,PalBlue );

    PaletteVGA[I,1]:=R;
    PaletteVGA[I,2]:=G;
    PaletteVGA[I,3]:=B;
    End;
    End;
End;

{
============================================
}

Procedure ReadPCXLine;
Var
   N,MaximumN,Z:Word;
   I:SmallInt;
   TmpB1,B1,C,CurrentPlane:Byte;
   CX:SmallInt;
   RealWidth:Integer;
Begin
N:=0;
Z:=0;
CurrentPlane:=0;
MaximumN:=PCXBytesPerLine*PcxColorPlanes;
RealWidth:=PcxBytesPerLine*8;
Repeat
    Begin
    B1:=FastGetByte;
    If B1 And $C0=$C0 Then
       Begin
       I:=B1 And $3F;
       C:=FastGetByte;
       While I>0 Do
             Begin
             Case BitsPerPixel Of
                  1:Begin
                    If (MyKeepTrueFormat=True) And (PcxColorPlanes=1) Then
                       Begin
                       TempArrayDBIG[Z]:=TempArrayDBIG[Z]+C;
                       Inc(Z);
                       End
                    Else
                       Begin
                       {
                       { 16 Color 4 planes or KEEP FORMAT=FALSE
                       }
                       For CX:=7 DownTo 0 Do
                          Begin
                          TmpB1:=0;
                          If C And (1 Shl CX) <>0 Then
                             TmpB1:=1;
                          TmpB1:=TmpB1 Shl CurrentPlane;
                          TempArrayDBIG[Z]:=TempArrayDBIG[Z]+TmpB1;
                          Inc(Z);
                          If Z>=RealWidth Then
                             Begin
                             Z:=0;
                             Inc(CurrentPlane);
                             End;
                          End;
                       End;
                    End;
                  8:Begin
                    TempArrayDBIG[Z]:=C;
                    Inc(Z);
                    End;
                End;
             Dec(I);
             Inc(N);
             End;
       End
    Else
       Begin
       Case BitsPerPixel Of
            1:Begin
              If (MyKeepTrueFormat=True) And (PcxColorPlanes=1) Then
                 Begin
                 TempArrayDBIG[Z]:=TempArrayDBIG[Z]+B1;
                 Inc(Z);
                 End
              Else
                 Begin
                 For CX:=7 DownTo 0 Do
                  Begin
                  TmpB1:=0;
                  If B1 And (1 Shl CX) <>0 Then
                     TmpB1:=1;
                  TmpB1:=TmpB1 Shl CurrentPlane;
                  TempArrayDBIG[Z]:=TempArrayDBIG[Z]+TmpB1;
                  Inc(Z);
                  If Z>=RealWidth Then
                     Begin
                     Z:=0;
                     Inc(CurrentPlane);
                     End;
                  End;
                 End;
              End;
            8:Begin
              TempArrayDBIG[Z]:=B1;
              Inc(Z);
              End;
            End;
       Inc(N);
       End;
    End;
Until N>=MaximumN;
End;

Procedure ReadPcxHeader(Var FileOk:Boolean;Var ErrorString:ShortString);
Label
  ExitIt;
Var
  B1:Byte;
  B2,X:Word;
  TopOfs,LeftOfs:Word;
Begin
B1:=FastGetByte;
If B1<>10 Then
   Begin
   ErrorString:='Not a PCX file, or header read error.';
   FileOk:=False;
   Goto ExitIt;
   End;
PcxVersion:=FastGetByte;
PcxEncoding:=FastGetByte;
BitsPerPixel:=FastGetByte;
LeftOfs:=FastGetWord;
TopOfs:=FastGetWord;
Width:=FastGetWord;
Height:=FastGetWord;
Width:=Width-LeftOfs+1;
Height:=Height-TopOfs+1;
FastGetWord;
FastGetWord;
B2:=BitsPerPixel;
BitsPerPixel:=4;
SetupMaskAndColorMap;
BitsPerPixel:=B2;
FastGetByte;
PcxColorPlanes:=FastGetByte;
PcxBytesPerLine:=FastGetWord;
PcxPaletteType:=FastGetWord;
For X:=1 To 58 Do
   FastGetByte;
If NOT(BitsPerPixel In [1,4,8,16,24,32]) Then
   Begin
   FileOk:=False;
   ErrorString:='Not a valid PCX file!';
   End;
ExitIt:;

End;

Procedure LoadFromFileX;
Var
  B1:Byte;
  I:SmallInt;
  NewWidth:Word;
  L1,L2:LongInt;
  PaletteOk:Boolean;
  FileOk:Boolean;
  Ptr1:Pointer;
Procedure UpDatePalette;
Var
  I:Integer;
begin
For I:=0 To 255 Do
    Syspal.LPal.PalPalEntry[I].peflags:=0;
   Case BitsPerPixel Of
        1:Begin
          If PcxColorPlanes=1 Then
             Begin
             If MyKeepTrueFormat Then
                BitMap.PixelFormat:=pf1bit
             Else
                Begin
                Case MyKeepTrueBits Of
                     8:BitMap.PixelFormat:=pf8bit;
                    24:BitMap.PixelFormat:=pf24bit;
                    End;
                End;
             MakePalBW(BitMap);
             End
          Else
             Begin
             BitMap.IgnorePalette:=False;
             SysPal.LPal.palVersion:=$300;
             SysPal.LPal.palNumEntries:=17;
             For I:=0 To 16 Do
                 Begin
                 Syspal.LPal.PalPalEntry[I].peRed:=  (PaletteVga[I,1]+1)*4-1;
                 Syspal.LPal.PalPalEntry[I].peGreen:=(PaletteVga[I,2]+1)*4-1;
                 Syspal.LPal.PalPalEntry[I].peBlue:= (PaletteVga[I,3]+1)*4-1;
                 End;
             If MyKeepTrueFormat Then
                BitMap.PixelFormat:=pf8bit
             Else
                Begin
                Case MyKeepTrueBits Of
                     8:BitMap.PixelFormat:=pf8bit;
                    24:BitMap.PixelFormat:=pf24bit;
                    End;
                End;
             Bitmap.Palette:= CreatePalette(Syspal.LPal);
             End;
          End;
        8:Begin
          If PcxColorPlanes=1 Then
             Begin
             If MyKeepTrueFormat Then
                BitMap.PixelFormat:=pf8bit
             Else
                Begin
                Case MyKeepTrueBits Of
                     8:BitMap.PixelFormat:=pf8bit;
                    24:BitMap.PixelFormat:=pf24bit;
                    End;
                End;
             MakePalPalette(BitMap);
             End
          Else
             Begin
             If MyKeepTrueFormat=True Then
                Begin
                BitMap.PixelFormat:=pf24bit;
                MakeGenPalette;
                End
             Else
                Begin
                Case MyKeepTrueBits Of
                     8:Begin
                       BitMap.PixelFormat:=pf8bit;
                       SetUpMaskGrayPalette
                       End;
                    24:Begin
                       BitMap.PixelFormat:=pf24bit;
                       MakeGenPalette;
                       End;
                    End;
                BitMap.IgnorePalette:=True;
                End;
             MakePalPalette(BitMap);
             End;
          End;
        End;
End;
Procedure Do8;
Var
  J:Word;
Begin
For J:=0 To Width-1 Do
    Begin
    TempArrayDBIG^[J]:=PCXGrayValue(
                       TempArrayDBIG^[J],
                       TempArrayDBIG^[PcxBytesPerLine+J],
                       TempArrayDBIG^[(PcxBytesPerLine Shl 1)+J]);
    End;
End;
Procedure Do24;
Var
  J,Z0,Z1,Z2,Z3:Word;
Begin
Z0:=0;
Z1:=0;
Z2:=PcxBytesPerLine;
Z3:=Z2+Z2;
For J:=0 To Width-1 Do
    Begin
    TempArrayDBIG16^[Z0+0]:=TempArrayDBIG^[Z3];
    TempArrayDBIG16^[Z0+1]:=TempArrayDBIG^[Z2];
    TempArrayDBIG16^[Z0+2]:=TempArrayDBIG^[Z1];
    Z0:=Z0+Global_HiColor;
    Inc(Z1);
    Inc(Z2);
    Inc(Z3);
    End;
Move(TempArrayDBIG16^,TempArrayDBIG^,NewWidth);
End;
Procedure Do8Adjust;
Begin
Move(TempArrayDBIG^,Ptr1^,Width);
End;
Procedure Do24Adjust;
Var
  X,Z:Word;
  B1:Byte;
Begin
Z:=0;
For X:=0 To Width-1 Do
    Begin
    B1:=TempArrayDBIG^[X];
    DataLineArray(Ptr1^)[Z+0]:=PaletteVGA[B1,3]*4+3;
    DataLineArray(Ptr1^)[Z+1]:=PaletteVGA[B1,2]*4+3;
    DataLineArray(Ptr1^)[Z+2]:=PaletteVGA[B1,1]*4+3;
    Z:=Z+Global_HiColor;
    End;
End;

Begin
MyKeepTrueFormat:=ShouldIKeepTrueFormat(MyKeepTrueBits);
ErrorString:='';
FileOk:=True;
OpenFile(FileName,FileOk);
ReadPcxHeader(FileOK,ErrorString);
If FileOk Then
   Begin
   BitMap.Height:=1;
   BitMap.Width:=1;
   BitMap.Height:=Height;
   BitMap.Width:=Width;
   UpdatePalette;
   {
   { Check version number for FAKE palette!
   }
   NewWidth:=Width*Global_HiColor;
   TempArrayDBIG:=Nil;
   TempArrayDBIG16:=Nil;
   GetMem(TempArrayDBig,Width*4+20{Slack Bytes});
   GetMem(TempArrayDBig16,NewWidth+20);
   PaletteOk:=True;
   If PcxVersion=3 Then
      Begin
      PaletteDefaults;
      End;
   If (BitsPerPixel=1) And (PcxColorPlanes=1) Then
      Begin
      PaletteVGA[0,1]:=0;
      PaletteVGA[0,2]:=0;
      PaletteVGA[0,3]:=0;
      PaletteVGA[1,1]:=63;
      PaletteVGA[1,2]:=63;
      PaletteVGA[1,3]:=63;
      End;
   If (BitsPerPixel=8) And (PcxColorPlanes=1) Then
      Begin
      {
      { Fast PALETTE Read On Picture (Could be wrong!)
      }
      L1:=FilePos(PictureFile);
      If SizeOf(IndexData)>L1 Then
         L1:=SizeOf(IndexData);
      L2:=L1-Index1;
      Seek(PictureFile,FileSize(PictureFile));
      L1:=FilePos(PictureFile);
      L1:=L1-(3*256+1);
      Seek(PictureFile,L1);
      FileIoReset;
   {
   { Reset GetByte Stuff!
   }
      B1:=FastGetByte;
      If B1<>$0C Then
         PaletteOk:=False;
      SetupMaskAndColorMap;
      Seek(PictureFile,L2);
      FileIoReset;
      End;
   If (BitsPerPixel=8) And (PcxColorPlanes=3) Then
      SetupMaskGrayPalette;
   I:=0;
   UpDatePalette;
   Repeat
    Begin
    If BitsPerPixel<>8 Then
       Begin
       FillerUp(TempArrayDBIG^[0],Width*PcxColorPlanes+20{Slack Bytes},0);
       End;
    ReadPCXLine;
    If (PCXColorPlanes=3) And
       (BitsPerPixel=8)   Then
       Begin
       {
       { 24 Bit Image!
       }
       If MyKeepTrueFormat Then
          Do24
       Else
          Begin
          Case MyKeepTrueBits Of
               8:Do8;
              24:Do24;
              End;
          End;
       End
    Else
       Begin
       {
       { 1,4 or 8 Bit file!
       }
       End;
    {
    { Put line into memory!
    }
    Ptr1:=BitMap.ScanLine[I];
    Case BitsPerPixel Of
         1:Begin
           If (MyKeepTrueFormat) And (PcxColorPlanes=1) Then
              Begin
              {
              { B&W Keep It
              }
              Move(TempArrayDBIG^,Ptr1^,PcxBytesPerLine*PcxColorPlanes)
              End
           Else
              Begin
              {
              { No KEEP or 16 Color
              }
              If MyKeepTrueFormat Then
                 Do8Adjust
              Else
                 Begin
                 Case MyKeepTrueBits Of
                      8:Do8Adjust;
                     24:Do24Adjust;
                     End;
                 End;
              End;
           End;
         8:Begin
           If PcxColorPlanes=1 Then
              Begin
              If MyKeepTrueFormat=True Then
                 Move(TempArrayDBIG^,Ptr1^,PcxBytesPerLine*PcxColorPlanes)
              Else
                 Begin
                 Case MyKeepTrueBits Of
                      8:Move(TempArrayDBIG^,Ptr1^,PcxBytesPerLine);
                     24:Do24Adjust;
                     End;
                 End;
              End
           Else
              Begin
              {
              { 24 bit file
              }
              If MyKeepTrueFormat Then
                 Move(TempArrayDBIG^,Ptr1^,PcxBytesPerLine*PcxColorPlanes)
              Else
                 Begin
                 Case MyKeepTrueBits Of
                      8:Move(TempArrayDBIG^,Ptr1^,Width);
                     24:Move(TempArrayDBIG^,Ptr1^,PcxBytesPerLine*PcxColorPlanes);
                     End;
                 End;
              End;
           End;
         End;
    Inc(I);
    End;
   Until I>=Height;
   {
   { Now read in REAL Palette!
   }
   If (BitsPerPixel=8) And (PcxColorPlanes=1) Then
      Begin
      If PaletteOk=False Then
         Begin
         FastGetByte;
         SetupMaskAndColorMap;
         End;
      End;
   If (BitsPerPixel=8) And (PcxColorPlanes=3) Then
      BitsPerPixel:=24;
   If (BitsPerPixel=1) And (PcxColorPlanes=4) Then
      BitsPerPixel:=4;
   FreeMem(TempArrayDBig16,NewWidth+20);
   FreeMem(TempArrayDBig,Width*4+20{Slack Bytes});
   If IoResult<>0 Then ;
   Close(PictureFile);
   End;
If IoResult<>0 Then ;
End;

Var
  TempArrayDBig2:^DataLineArray; {0-MaxWidth*4}
Var
  LocalPCXType:Word;
  CurrentColorPlane:Byte;
  MyWidth,MyHeight:Word;
  CurrBitsPerPixel:Word;
  InputBitsPerPixel:Word;
Procedure SaveToFileX(FileName:String;Const BitMap:TBitMap;PcxType:Byte);
Procedure WritePcxFile(FileName:String;MyPcxType:Byte);
Label
  ErrExitClose;
Var
  File1:File;
  B1:Byte;
  ResultStatus:Boolean;

Procedure DoBlockWriteF(Var B1:Byte);
Begin
BlockWrite(File1,B1,1);
End;

Procedure WriteHeader;
Var
  B1,B2:Byte;
  MyTopOfs,MyLeftOfs:Integer;
  X,Y:Word;
Begin
B1:=10;
DoBlockWriteF(B1);
B1:=5;
DoBlockWriteF(B1);
B1:=1;
DoBlockWriteF(B1);
Case MyPcxType Of
     1:B1:=1;
     2:B1:=1;
     3:B1:=8;
     4:B1:=8;
     End;
DoBlockWriteF(B1);
MyLeftOfs:=0;
MyTopOfs:=0;
B1:=Lo(MyLeftOfs);
DoBlockWriteF(B1);
B2:=Hi(MyLeftOfs);
DoBlockWriteF(B2);
B1:=Lo(MyTopOfs);
DoBlockWriteF(B1);
B2:=Hi(MyTopOfs);
DoBlockWriteF(B2);
B1:=Lo(MyLeftOfs+MyWidth-1);
DoBlockWriteF(B1);
B2:=Hi(MyLeftOfs+MyWidth-1);
DoBlockWriteF(B2);
B1:=Lo(MyTopOfs+MyHeight-1);
DoBlockWriteF(B1);
B2:=Hi(MyTopOfs+MyHeight-1);
DoBlockWriteF(B2);

B1:=Lo(MyWidth);
DoBlockWriteF(B1);
B2:=Hi(MyWidth);
DoBlockWriteF(B2);
B1:=Lo(MyHeight);
DoBlockWriteF(B1);
B2:=Hi(MyHeight);
DoBlockWriteF(B2);
{
{ Write Palette
}
For X:=0 To 15 Do
    Begin
    For Y:=1 To 3 Do
        Begin
        B1:=(PaletteVga[X,Y]*255) Div 63;
        DoBlockWriteF(B1);
        End;
    End;
B1:=0;
DoBlockWriteF(B1);
Case MyPcxType Of
     1:Begin
       PcxColorPlanes:=1;
       PcxBytesPerLine:=((MyWidth+7) Div 8);
       End;
     2:Begin
       PcxColorPlanes:=4;
       PcxBytesPerLine:=((MyWidth+7) Div 8);
       End;
     3:Begin
       PcxColorPlanes:=1;
       PcxBytesPerLine:=MyWidth;
       End;
     4:Begin
       PcxColorPlanes:=3;
       PcxBytesPerLine:=MyWidth;
       End;
     End;
B1:=PcxColorPlanes;
DoBlockWriteF(B1);
B1:=Lo(PcxBytesPerLine);
DoBlockWriteF(B1);
B1:=Hi(PcxBytesPerLine);
DoBlockWriteF(B1);
B1:=1;
DoBlockWriteF(B1);
B1:=0;
DoBlockWriteF(B1);
For X:=1 To 58 Do
    DoBlockWriteF(B1);
End;

(*
Procedure WritePcxLine(Var MyTempArray;Var Z:Word);
Var
  CurrentColorPlane,NumBytes,MaxX,W,X,Y:Integer;
  Ch,Dup:Byte;
Function MyGetByte(X:Word):Byte;
Var
  NewCh:Byte;
  Y:Integer;
Begin
Case MyPcxType Of
     1:Begin
       NewCh:=0;
       For Y:=7 DownTo 0 Do
           Begin
           NewCh:=NewCh Or ((DataLineArray(MyTempArray)[X] And 1) Shl Y);
           Inc(X);
           End;
       End;
     2:Begin
       {
       { Take 1st bit from next 8 bytes
       }
       NewCh:=0;
       For Y:=7-CurrentColorPlane DownTo 0-CurrentColorPlane Do
           Begin
           If Y<0 Then
              NewCh:=NewCh Or ((DataLineArray(MyTempArray)[X] And (1 Shl CurrentColorPlane)) Shr Abs(Y))
           Else
              NewCh:=NewCh Or ((DataLineArray(MyTempArray)[X] And (1 Shl CurrentColorPlane)) Shl Y);
           Inc(X);
           End;
       End;
     3:Begin
       NewCh:=DataLineArray(MyTempArray)[X];
       End;
     End;
MyGetByte:=NewCh;
End;

Begin
Case MyPcxType Of
     1:Begin
       W:=8;
       End;
     2:Begin
       W:=8;
       End;
     3:Begin
       W:=1;
       End;
     End;
MaxX:=PcxBytesPerLine*W;
Z:=0;
X:=0;
NumBytes:=0;
CurrentColorPlane:=0;
Repeat
    Begin
    {
    { Get whole BYTE!
    }
    {
    { Get runs!
    { Repeat
    { Until X=Width or DUP>63
    {
    }
    Dup:=1;
    While
          (X<Width-1)                   And
          (MyGetByte(X)=MyGetByte(X+W)) And
          (Dup<63)                      And
          (Dup+NumBytes<PcxBytesPerLine) Do
          Begin
          Inc(Dup);
          X:=X+W;
          End;
    Ch:=MyGetByte(X);
    If (Dup>1) Or (Ch>=$C0) Then
       Begin
       TempArrayDBIG[Z]:=$C0+Dup;
       Inc(Z);
       TempArrayDBIG[Z]:=Ch;
       Inc(Z);
       End
    Else
       Begin
       TempArrayDBIG[Z]:=Ch;
       Inc(Z);
       End;
    X:=X+W;
    NumBytes:=NumBytes+Dup;
    If X>=MaxX Then
       Begin
       Inc(CurrentColorPlane);
       X:=0;
       NumBytes:=0;
       End;
    End;
Until CurrentColorPlane>=PcxColorPlanes;
End;
*)

Procedure WritePcxLine(Var MyTempArray;Var ZZ:Word);
Label
  DOWP1,DOWP2,DOWP3,DOWPX,WPRLOOP1,WPWLOOP1,WPCONT1,
  DODUP,DOWPONE,DODUPEND,LJA,WPEX;
Var
  DUP,W,X,NumBytes,MaxX:Word;
  OldAX,FastAX:Word;
Function MyGetByte(X:Word):Byte;
Label
  DOMG1,DOMG2,DOMG3,DOMGX,LOOP1A,LOOP2A,L3B,L3X,IS8;
Var
  MyAX:Word;
Begin
        ASM
        PUSH    ESI
        MOV     EBX,0
        MOV     BX,AX
        CMP     LOCALPCXTYPE,1
        JZ      DOMG1
        CMP     LOCALPCXTYPE,2
        JZ      DOMG2
        CMP     LOCALPCXTYPE,3
        JZ      DOMG3
        CMP     LOCALPCXTYPE,4
        JZ      DOMG3
        JMP     DOMGX
DOMG1:
{
{ If we already are in 2 color mode, then just get the byte
}
        CMP     CURRBITSPERPIXEL,1
        JNZ     IS8
        SHR     EBX,3
        MOV     AL,[EDI+EBX]
        JMP     DOMGX
IS8:
        MOV     DX,8
        MOV     AL,0
LOOP1A:
        MOV     AH,[EDI+EBX]
        AND     AH,1
        MOV     CL,DL
        DEC     CL
        SHL     AH,CL
        OR      AL,AH
        INC     BX
        DEC     DX
        JNZ     LOOP1A
        JMP     DOMGX
DOMG2:
        MOV     DH,CURRENTCOLORPLANE
        MOV     DL,7
        SUB     DL,DH
        MOV     AL,0
LOOP2A:
        CMP     DL,0
        JGE     L3B
        MOV     AH,[EDI+EBX]
        MOV     CH,1
        MOV     CL,DH
        SHL     CH,CL
        AND     AH,CH

        MOV     CL,DL
        NEG     CL
        SHR     AH,CL
        OR      AL,AH
        JMP     L3X
L3B:
        MOV     AH,[EDI+EBX]
        MOV     CH,1
        MOV     CL,DH
        SHL     CH,CL
        AND     AH,CH

        MOV     CL,DL
        SHL     AH,CL
        OR      AL,AH
L3X:
        INC     BX
        DEC     DL
        MOV     CL,0
        SUB     CL,DH
        CMP     DL,CL                           {;AT BOTTOM YET?}
        JGE     LOOP2A
        JMP     DOMGX
DOMG3:
        MOV     AL,[EDI+EBX]
DOMGX:
        MOV     AH,0
        MOV     MYAX,AX
        POP     ESI
        END;
MyGetByte:=MyAX;
End;


Begin
LocalPCXType:=MyPcxType;
        ASM
        PUSHA
        PUSH    ESI
        PUSH    EDI
        MOV     EDI,MYTEMPARRAY;
        MOV     ESI,TEMPARRAYDBIG
        CMP     LOCALPCXTYPE,1
        JZ      DOWP1
        CMP     LOCALPCXTYPE,2
        JZ      DOWP2
        CMP     LOCALPCXTYPE,3
        JZ      DOWP3
        CMP     LOCALPCXTYPE,4
        JZ      DOWP3
        JMP     DOWPX
DOWP1:  MOV     W,8
        JMP     DOWPX
DOWP2:  MOV     W,8
        JMP     DOWPX
DOWP3:  MOV     W,1
DOWPX:
        MOV     AX,PCXBYTESPERLINE
        MOV     BX,W
        MUL     BX
        MOV     MAXX,AX
        MOV     EAX,ZZ
        MOV     WORD PTR [EAX],0
        MOV     X,0
        MOV     NUMBYTES,0
        MOV     CURRENTCOLORPLANE,0
WPRLOOP1:
        MOV     DUP,1
        MOV     AX,X
        PUSH    EBP
        CALL    MYGETBYTE
        POP     ECX
        MOV     FASTAX,AX
WPWLOOP1:
        MOV     AX,FASTAX
        MOV     OLDAX,AX
        PUSH    AX
        MOV     AX,X
        ADD     AX,W
        PUSH    EBP
        CALL    MYGETBYTE
        POP     ECX
        POP     BX
        MOV     FASTAX,BX
        CMP     AX,BX
        JNZ     WPCONT1
        MOV     AX,X
        INC     AX
        CMP     AX,MYWIDTH
        JGE     WPCONT1
        CMP     DUP,63
        JGE     WPCONT1
        MOV     AX,DUP
        ADD     AX,NUMBYTES
        CMP     AX,PCXBYTESPERLINE
        JGE     WPCONT1
        INC     DUP
        MOV     AX,W
        ADD     X,AX
        JMP     WPWLOOP1
WPCONT1:
        MOV     AX,OLDAX
        CMP     DUP,1
        JG      DODUP
        CMP     AL,0C0H
        JGE     DODUP
        JMP     DOWPONE
DODUP:
        MOV     ECX,ZZ
        MOVZX   EBX,WORD PTR [ECX]
        MOV     AH,0C0H
        OR      AH,BYTE PTR DUP
        MOV     [ESI+EBX],AH                   {;TEMPARRAYDBIG}
        MOV     ECX,ZZ
        INC     WORD PTR [ECX]
        MOV     [ESI+EBX+1],AL                 {;TEMPARRAYDBIG}
        MOV     ECX,ZZ
        INC     WORD PTR [ECX]
        JMP     DODUPEND
DOWPONE:
        MOV     ECX,ZZ
        MOVZX   EBX,WORD PTR [ECX]
        MOV     [ESI+EBX],AL                   {;TEMPARRAYDBIG}
        MOV     ECX,ZZ
        INC     WORD PTR [ECX]
DODUPEND:
        MOV     AX,W
        ADD     X,AX
        MOV     AX,DUP
        ADD     NUMBYTES,AX
        MOV     AX,X
        CMP     AX,MAXX
        JL      LJA
        INC     CURRENTCOLORPLANE
        MOV     X,0
        MOV     NUMBYTES,0
LJA:
        MOV     AL,CURRENTCOLORPLANE
        CMP     AL,PCXCOLORPLANES
        JGE     WPEX
        JMP     WPRLOOP1
WPEX:
        POP     EDI
        POP     ESI
        POPA
        END;
End;

Procedure PixelConvertRGBLines(Var TempArrayD,TempArrayDBIG2:DataLineArray);
Label
   PCRL_1,PCRL_24,PCRL_EXIT;
Begin
        ASM
        PUSHA
        PUSH    ESI
        PUSH    EDI
        MOVZX   ECX,MYWIDTH
        MOV     EDI,TEMPARRAYDBIG2
        MOV     ESI,TEMPARRAYD
        CMP     CURRBITSPERPIXEL,8
        JZ      PCRL_1
        CMP     CURRBITSPERPIXEL,24
        JZ      PCRL_24
PCRL_1:
        MOV     AH,0
        MOV     AL,[ESI]
{
{ GET PALETTE COLORS
}
        MOV     EBX,0
        MOVZX   EBX,AX
        SHL     EBX,1
        ADD     BX,AX
        MOV     AL,BYTE PTR PALETTEVGA[EBX+0]
        MOV     DL,BYTE PTR PALETTEVGA[EBX+1]
        MOV     DH,BYTE PTR PALETTEVGA[EBX+2]
        MOVZX   EBX,MYWIDTH
        SHL     AL,2
        SHL     DL,2
        SHL     DH,2
        MOV     [EDI],AL
        MOV     [EDI+EBX],DL
        SHL     EBX,1
        MOV     [EDI+EBX],DH
        INC     ESI
        INC     EDI
        LOOP    PCRL_1
        JMP     PCRL_EXIT
PCRL_24:
        MOV     AL,[ESI+2]
        MOV     DL,[ESI+1]
        MOV     DH,[ESI+0]
        MOVZX   EBX,MYWIDTH
        MOV     [EDI],AL
        MOV     [EDI+EBX],DL
        SHL     EBX,1
        MOV     [EDI+EBX],DH
        ADD     ESI,3
        INC     EDI
        LOOP    PCRL_24
        JMP     PCRL_EXIT
PCRL_EXIT:
        POP     EDI
        POP     ESI
        POPA
        END;
End;

Procedure WriteBody(Var ResultStatus:Boolean);
Var
  Z:Word;
  TmpPCXColorPlanes:Word;
  I:Integer;
Begin
TmpPCXColorPlanes:=PCXColorPlanes;
I:=0;
ResultStatus:=True;
Repeat
    Begin
    TempArrayD:=BitMap.ScanLine[I];
    {
    { Convert Any From
    }
    Case InPutBitsPerPixel Of
         1,4,8:Begin
               ConvertXBitsToYBits(TempArrayD^,TempArrayD2^,InputBitsPerPixel,8,MyWidth);
               CurrBitsPerPixel:=8;
               End;
            24:Begin
               ConvertXBitsToYBits(TempArrayD^,TempArrayD2^,InputBitsPerPixel,24,MyWidth);
               End;
         End;
    Case MyPcxType Of
         1..3:Begin
              WritePCXLine(TempArrayD2^,Z);
              BlockWrite(File1,TempArrayDBIG^[0],Z);
              End;
            4:Begin
              PCXColorPlanes:=1;
              {
              { Special Triple Plane Thingy :)
              }
              PixelConvertRGBLines(TempArrayD2^,TempArrayDBIG2^);
              WritePCXLine(TempArrayDBIG2^[0],Z);
              BlockWrite(File1,TempArrayDBIG^[0],Z);
              WritePCXLine(TempArrayDBIG2^[MyWidth],Z);
              BlockWrite(File1,TempArrayDBIG^[0],Z);
              WritePCXLine(TempArrayDBIG2^[MyWidth*2],Z);
              BlockWrite(File1,TempArrayDBIG^[0],Z);
              End;
            End;
    If IoResult<>0 Then
       ResultStatus:=False;
    Inc(I);
    End;
Until (I>=MyHeight) Or (ResultStatus=False);
PCXColorPlanes:=TmpPCXColorPlanes;
End;

Procedure Write256Palette;
Var
  X,Y:Word;
  B1:Byte;
Begin
For X:=0 To 255 Do
    Begin
    For Y:=1 To 3 Do
        Begin
        B1:=(PaletteVga[X,Y]*255) Div 63;
        DoBlockWriteF(B1);
        End;
    End;
End;

Begin
{
{ Write PCX File Write out either 2,16,256 colors
{
}
SaveThePalette(BitMap.Palette,PaletteVGA);
MyWidth:=BitMap.Width;
MyHeight:=BitMap.Height;
Case BitMap.PixelFormat Of
     pf1bit:CurrBitsPerPixel:=1;
     pf4bit:CurrBitsPerPixel:=4;
     pf8bit:CurrBitsPerPixel:=8;
     pf24bit:CurrBitsPerPixel:=24;
     End;
InputBitsPerPixel:=CurrBitsPerPixel;
TempArrayDBIG:=Nil;
TempArrayDBIG2:=Nil;
GetMem(TempArrayDBig,MyWidth*4);
GetMem(TempArrayDBig2,MyWidth*4);
GetMem(TempArrayD2,MyWidth*4);
Assign(File1,FileName);
ReWrite(File1,1);
WriteHeader;
WriteBody(ResultStatus);
If ResultStatus=False Then
   Begin
   {
   { Put ERROR handler here if you like!
   { ###ERROR###
   }
   Goto ErrExitClose;
   End;
B1:=$0C;
DoBlockWriteF(B1);
Write256Palette;
ErrExitClose:;
Close(File1);
FreeMem(TempArrayD2,Width*4);
FreeMem(TempArrayDBig2,Width*4);
FreeMem(TempArrayDBig,Width*4);
End;

Begin
WritePcxFile(FileName,PcxType);
End;

End.


