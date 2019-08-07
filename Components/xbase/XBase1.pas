// =============== XBASE1.PAS du 16/07/2000 CHABANT =================
// install้ dans CHABANT.BPL   **
// ATTENTION : Limitations เ 128 CHAMPS et RECORD maximum = 4000 bytes          }
//             voir KMAXFLD et KMAXSIZ
{
  Changes made by Richard B. Winston (rbwinst@usgs.gov)
  Mar. 23, 2004
    1. I added DesignIde.dcp to the list of required packages and
       modified Uses statement to make it the component
       compatible with Kylix 3 and Delphi 7.
       (See: http://www.delphifaq.com/fq/q3072.shtml
       and http://community.borland.com/article/0,1410,27717,00.html)
    2. I modified the RegisterPropertyEditor to make sure the
       property editor was properly registered by changing
       TypeInfo(String) to TypeInfo(WString).
    3. I added xbfBlob to TFieldType to represent BLOBs
       (binary large object).
    4. In TXBase.GetFieldType, I added code for detecting
       F as a numeric and B as a BLOB.
  Mar. 31, 2004
    5. The destructor was modified to fix a memory leak.
  Aug. 19, 2004
    6. Field names are allowed to have spaces.
    7. Increased the maximum number of fields to 4000.
  Sept. 3, 2006
    8. In GetFieldByName and GetFieldByNumber,
       null characters now identified as end of field.
  Sept. 26, 2008
    9. For compatibilty with Delphi 2009, DecimalSeparator, DateSeparator
       and certain other characters
       are cast to AnsiChar where required. An assertion similar to that used
       in CharInSet is used to verify that the cast is valid.
    10. For compatibilty with Delphi 2009, most variables previously
        declared as Char are now declared as AnsiChar.
    11. For compatibility with Delphi 2009, certain variables previously
        declared as string are now declared as AnsiString.
    12. For compatiblity with Delphi 2009, in calls to TextToFloat, the
        text to be converted is cast to PAnsiChar.
    13. For compatiblity with Delphi 2009, in certain cases where an
        array of Char was previously cast to a string, these have been
        converted so that an array of AnsiChar is cast to an AnsiString.
  July 14, 2009
    14. Added overloaded versions of GetFieldNum and GetFieldInt that retrieve
        a field by number rather than by field name.
  Sept. 18, 2016 for numbers whose absolute value is less than 1E-10,
    UpdFieldNum saves the number is general format rather than fixed format.


  See http://www.dbase.com/KnowledgeBase/int/db7_file_fmt.htm.
}

// -----------------------------------------------
// V1.11  29/03/2003  Correctif car message ennuyeux et inutile ErreurGlobale(20)
//                    "Error 20 * DBf File Closed , New Close impossible !
//                              on File FCLI.DBF par exemple " 
// -----------------------------------------------
// V1.10  06/01/2003  ajout de plusieurs methodes r้alisant des traitements globaux
//                    CREATE         :  pour CREER une Structure de FICHIER
//                    EXTRACTSTRUCT  :  pour obtenir la  Structure du fichier DBF
//                    COPYSTRUCT     :  pour cr้er un autre fichier de m๊me structure
//                    PACK           :  pour ้liminer les records supprim้s
//                    ZAP            :  pour r้initialiser เ 0 records le fichier
//                    DBFReadRecordToBuffer : lecture d'un record complet
//
//                    FActive et IsOpen semblent etre redondants !
//                    regroupement des ERREURS  dans une seule procedure
//                    diverses retouches de d้tail
// -----------------------------------------------
// V1.00  diffus้e sur les Sites DELPHI Components le 01/01/2003
// 29/12/2002  rajout d'une Variante UpdFieldNum5D ( 5 d้cimales ) <<V211A>>
// 19/12/2002  rectif erreur dans "FillHeaderInfo" :
//             calcul NOMBRE de CHAMPS : en donnait un de moins !
(* 10/07/2000  17h20 *)
(* 09/07/2000  Routines pour afficher les DATES        *)
(* 08/07/2000  Correction ReadARecord si fichier VIDE  *)
(*             Correction EOF qui doit etre APRES le dernier Rec  *)
(*             Transforme l'ancien EOF en LASTREC                 *)
(* 07/07/2000  optimisation  *)
(* 06/07/2000  Avec Debugging possible des cas d'erreurs     *)
(* 02/07/2000 *)
(* Modifi้ CHABANT 27/06/00 :
      M้thodes UpdFieldStr , UpdFieldInt , UpdFieldNum
   Modifi CHABANT 25/06/00 :
      M้thodes GetFieldStr , GetFieldInt , GetFieldNum  *)

{******************************************************************************}
{* This code was written by : Jamie Hart                                      *}
{******************************************************************************}
{* TXBase is a Non-visual component for Delphi 2/3 which gives access to      *}
{* dBase III+ data files without needing the BDE installed.                   *}
{*                                                                            *}
{* This component is freeware: any comments or suggestions are welcome at:    *}
{* jay@bitsmart.com                                                           *}
{*                                                                            *}
{* Version 0.99a (beta)                                                       *}
{******************************************************************************}
{* Users of the TXBase component must accept the following disclaimer of      *}
{* warranty:                                                                  *}
{*                                                                            *}
{* TXBase is supplied as is. The author disclaims all warranties, expressed   *}
{* or implied, including, without limitation, the warranties of               *}
{* merchantability and of fitness for any purpose. The author assumes no      *}
{* liability for damages, direct or consequential, which may result from the  *}
{* use of TXBase.                                                             *}
{******************************************************************************}

{$ifdef CONDITIONALEXPRESSIONS}
  {$if CompilerVersion>=20}
    {$DEFINE Delphi_2009_UP}
  {$ifend}
{$endif}


Unit XBase1 ;

Interface

{$IFDEF MSWINDOWS}

  {$IF CompilerVersion >= 23}
Uses  Types, SysUtils, Classes, VCL.Dialogs;
  {$ELSE}
Uses  Types, SysUtils, Classes, Dialogs;
  {$IFEND}
{$ENDIF}
{$IFDEF LINUX}
Uses
  Types, SysUtils, Classes, QDialogs;
{$ENDIF}


Const
// rbw begin change
//  //..... ces 2 constantes peuvent etre augment้es si besoin .....
//  KMAXFLD =4000 ;
//  KMAXSIZ =4800 ; // au moins le Header Maximum 32 + 128*32 + 1 = 4.129
// rbw end change
  //....
  KSIZHDR =  32 ; // taille officielle du HEADER
  KSIZFLD =  32 ; // taille officielle de chaque DESCRITEUR de CHAMP
  KSIZNAM =  11 ; // taille officielle du NOM du CHAMP
  DELETE_FLAG = '*' ;  // le 1er Byte d'un Record "Logiquement" SUPPRIM้

Type
  EXBaseException = Class(Exception);
        {$H-   avec les ShortString  CHABANT 07/07/00 }
  WString = String[255] ;

{-------- Premiere partie de l' EN-TETE d'un fichier DBASE 32 bytes ----}
// 20/12/2002 : le mieux est d'interdire l' ALIGNEMENT
       {$A-  directive ALIGNEMENT DES CHAMPS , voir aussi PACKED }
Type
  THeader = Record
    VersionNumber: Byte;
    LastUpdated  : Array[1..3] Of Byte;  // ANNEE MOIS JOUR
    NumRecords   : Cardinal ; { DWORD   G.CHABANT  32bits NON signe }
    //... 20/12/2002 : par bonheur , on est ici en pos=08 , ce qui
    //    fait que les WORD sont cadr้s sans rajouts !!! OUF
    HeaderLen    : Word;      { 16bits NON signe }
    RecordLen    : Word;
    Reserved     : Word;
    IncTrans     : Byte;
    EncFlag      : Byte;
// rbw begin change
//    Reserved2    : Array[0..11] Of Char ; // pour faire des Pchar ; 12 positions
    Reserved2    : Array[0..11] Of AnsiChar ; // pour faire des Pchar ; 12 positions
// rbw end change
    MdxFlag      : Byte;
    LangDriver   : Byte;
    Filler1 , Filler2 : byte ; // 20/12/2002 Reserved3    : Word;
  End;
  
// rbw begin change
const
//  //..... ces 2 constantes peuvent etre augment้es si besoin .....
//  KMAXFLD =2046 ; 
  KMAXFLD = (High(Word)-SizeOf(THeader)) div KSIZFLD;
  KMAXSIZ =SizeOf(THeader) + KMAXFLD*32+1; // au moins le Header Maximum 32 + 128*32 + 1 = 4.129
// rbw end change

{-------- Partie DESCRIPTEUR de CHAMP   d'un fichier DBASE 32 bytes ----}
//... gros probl่me d้couvert le 20/12/2002
//... je d้couvre le pot aux roses : TFieldStruct
//...     fait 36 octets et  non pas 32 comme attendu !!
//...     les responsables zones WORD situ้es en pos=18 , et DELPHI
//...     le cadre sur un multiple de 4 , donc rajoute 2 bytes devant
Type
  TFieldStruct = Record
// rbw begin change
//    FieldName  : Array[1..KSIZNAM] Of Char ; // 11 caracteres padd้ เ #00
    FieldName  : Array[1..KSIZNAM] Of AnsiChar ; // 11 caracteres padd้ เ #00
//    FieldType  : Char;
    FieldType  : AnsiChar;
// rbw end change
    Address    : DWORD;
    FieldLength: Byte;
    Decimals   : Byte;
    Filler1 , Filler2 : byte ; // 20/12/2002 Reserved   : Word;
    WorkArea   : Byte;
    Filler3 , Filler4 : byte ; // 20/12/2002 Reserved2   : Word;
    SetFields  : Byte;
// rbw begin change
//    Reserved3  : Array[0..6] Of Char ;  // 7 char (au lieu de bytes)
    Reserved3  : Array[0..6] Of AnsiChar ;  // 7 char (au lieu de bytes)
// rbw end change
    IndexFlag  : Byte;
  End;

// l' EN-TETE DBF3 est form้ normalement :
//    du Header de 32 octets
//    d'un Descripteur TFieldStruct de 32 octets pour chaque champ
//    d'un octet final valant $1D = 13 dec
// la taille totale est donc = 32 + 32*F + 1 , et est stock้e dans HeaderLen


{-------- Partie EN-TETE d'un fichier MEMO ( *.DBT ) = 512 bytes    ----}
Type
  TDBTHeader = Record
    NextBlock: DWORD;  { liens vers les blocks logiquement adjacents }
    BlockSize: DWORD;
// rbw begin change
//    Reserved : Array[1..504] Of Char;
    Reserved : Array[1..504] Of AnsiChar;
// rbw end change
  End;

Type
  TXBChangeEvent = Procedure (Sender: TObject; Var Allowed: Boolean) Of Object;
  TXBDeleteEvent = Procedure (Sender: TObject; Var Allowed: Boolean) Of Object;

Type
  TFieldType = (xbfUnknown, xbfChar, xbfDate, xbfNumber, xbfLogic, xbfMemo, xbfBlob);

//======================================================================
//------- Notre classe TXBASE = composant simple Fichier DBASE3     ----
Type
  TXBase = Class(TComponent)
  Private
    { Private Declarations }
    FFileName     : String  ; (* Nom du fichier, avec son Path *)
    FCurrentRecord: Cardinal ; (* +CHAB./Longint/ NUMERO du RECORD courant *)
    NumFields     : Integer  ; (* NOMBRE de CHAMPS : 1..128 *)
    FActive       : Boolean  ; (* Fichier OUVERT ou NON Ouvert  *)
    //IsOpen        : Boolean  ;
    FModified     : Boolean  ; (* Indique si le Record courant a ้t้ modifi้ *)
    FAutoUpdate   : Boolean  ; (* +CHAB. RE-ECRITURE automatique si  modifi้ *)
    FDebugErr     : Boolean  ; (* +CHAB. pour visualiser les erreurs *)
    Header        : THeader  ; (* son En-Tete *)
    DBFile        : TFileStream ;
    FieldStruct   : Array[1..KMAXFLD] Of TFieldStruct ; (* ses CHAMPS *)
// rbw begin change
//    RecordBuffer  : Array[1..KMAXSIZ] Of Char ; (* zone de Lecture/Ecriture *)
    RecordBuffer  : Array[1..KMAXSIZ] Of AnsiChar ; (* zone de Lecture/Ecriture *)
// rbw end change
    (*-- fonctions particulieres gestion Evenements --*)
    FOnChanging   : TXBChangeEvent;
    FOnDeleting   : TXBDeleteEvent;
    FOnAppended   : TNotifyEvent;
    (*-- ้l้ments pour gestion des champs MEMO      --*)
    MemoFile      : TFileStream ;
    DBTHeader     : TDBTHeader  ;
    FDBFUpdated   : boolean    ; //<<V110>> indique que le fichier est modifi้
    // <<V120>> pour SetFind .......
    FFindNum      : smallint   ; // Nฐ du Champ
    FFindField    : String[11] ; // nom du champ
// rbw begin change
//    FFindType     : Char       ; // type de Champ
//    FFindOpe      : Char       ; // Op้rateur = ou > ou <  ou G ou L
    FFindType     : AnsiChar       ; // type de Champ
    FFindOpe      : AnsiChar       ; // Op้rateur = ou > ou <  ou G ou L
// rbw end change
    FFindValS     : AnsiString    ; // Valeur เ comparer
    FFindValN     : Extended   ; // Valeur Num้rique
    (*----------------------------*)
    Function  GetFileName     : String ;
    Procedure SetFileName(Name: String);
    Procedure SetActive(Val: Boolean);
    Procedure FillHeaderInfo;
    Procedure ReadARecord;
    Procedure WriteARecord;
    Function  IsBof     : Boolean;
    Function  IsEof     : Boolean;
    Function  IsLastRec : Boolean;
    Function  IsDelete  : boolean;
    Procedure SetDeleteStatus(Delete: boolean);
    //---- fonctions particulieres gestion Evenements ----
    Function  CanNavigate  : Boolean; (* authorise ou interdit les GoTo  *)
    Function  CanDelete    : Boolean; (* authorise ou interdit le DELETE *)
    //---- regroupement des ERREURS -------
    Procedure ErreurGlobale( XERR : byte ; XDIV : integer ) ;
  Protected
    { Protected Declarations }
  Public
    { Public Declarations }
    Constructor Create(AOwner: TComponent); Override;
    Destructor  Destroy; Override;
    Procedure GotoBOF ;
    Procedure GotoEOF ;
    Procedure GotoNext;
    Procedure GotoPrev;
    Procedure GotoRecord(Recnum: Cardinal);
    Procedure RefreshRecord;
    // sont mieux ici en "public" <<V1.10>>
    Function  GetFieldNumberFromName(Text: AnsiString) : Integer ;
    //    Procedure UpdateFieldData(FieldNo: integer; Text: WString);
    //
    Function  GetFieldByName(FieldName: AnsiString) : String;
    Function  GetFieldByNumber(FieldNum: Integer): String;
    Function  GetFieldName(FieldNo: Integer)     : AnsiString;
    Function  GetFieldType(FieldNo: Integer)     : TFieldType;
// rbw begin change
//    Function  GetFieldTypeChar(FieldNo: Integer) : Char ; // ajout CHABANT
    Function  GetFieldTypeChar(FieldNo: Integer) : AnsiChar ; // ajout CHABANT
// rbw end change
    Function  GetFieldSize(FieldNo: Integer)     : Integer;
    Function  GetFieldDecimals(FieldNo: Integer) : Integer; // ajout CHABANT
    Procedure PutFieldByName(FieldName, Data: AnsiString);
    Procedure PutFieldByNumber(FieldNo : Integer; TEXT : AnsiString);
    Procedure PostChanges;
    Procedure AppendBlank;
    (*อออออออออ CHABANT ออออออออออ*)
    Function  GetFieldStr(FieldName : AnsiString ) : String  ;
    Function  GetFieldInt(FieldName : AnsiString ) : Integer  ; overload;
    Function  GetFieldInt(FieldNum : integer ) : Integer  ; overload;
    Function  GetFieldNum(FieldName : AnsiString ) : Extended ; overload; // Real 30/12/2002
    Function  GetFieldNum(FieldNum : integer ) : Extended ; overload; // Real 30/12/2002
    Function  GetFieldDat(FieldName : AnsiString ) : String  ;
    Procedure UpdFieldStr(FieldName, Data: AnsiString ) ;
    Procedure UpdFieldInt(FieldName : AnsiString ; Data: Integer ) ;
    Procedure UpdFieldDat(FieldName : WString ; Data: WString ) ;
    // 29/12/2002  rajout d'une Variante UpdFieldNum5D ( 5 dcimales ) <<V211A>>
    Procedure UpdFieldNum(  FieldName : WString ; Data: Extended ) ; // au lieu de "REAL"
    Procedure UpdFieldNum2D(FieldName : WString ; Data: Extended ) ; // au lieu de "REAL"
    Procedure UpdFieldNum5D(FieldName : WString ; Data: Extended ) ;
    (*ออออออออออออออออออออออออออออ*)
    (*-- ้l้ments pour gestion des champs MEMO      --*)
    Function  GetMemoData(FieldNo: Longint): AnsiString; (* Chaine longue *)
    Procedure UpdateMemoData(FieldNo: integer; Text: AnsiString);
    (*ออออออออออออออออออออออออออออ*)
    (* nouvelles fonctions Version <<V1.10>>  *)
// rbw begin change
//    Function DBFReadRecordToBuffer(var BUFFER : array of char ;
//                                CPT : integer ) : integer ;
    Function DBFReadRecordToBuffer(var BUFFER : array of AnsiChar ;
                                CPT : integer ) : integer ;
// rbw end change
    Function DBFZap(CPTREC : integer ) : boolean  ;
    Function DBFPack(BIDON : byte    ) : integer  ;
    Function DBFExtractStruct(var DBFFields : TStringList ;
                                VFIX : boolean ) : boolean ;
    Function DBFCopyStruct(DBFName : String ) : boolean ;
    Function DBFCopyFile(DBFName : String ; CPTREC : integer ;
                         CopyDELETED : boolean ) : boolean ;
    Function DBFCreate(DBFName : String ; DBFFields :TStringList) : boolean ;
    (*ออออออออออออออออออออออออออออ*)
    Function  PadLeft( Text: AnsiString; CPT : Integer): AnsiString ;
    Function  PadRight(Text: AnsiString; CPT : Integer): AnsiString ;
    Function  StandardDBFName(Text: String): String ;
    procedure DBFSetDateAMJ(var ZAMJ3 : array of byte) ; // arg = zone de 3 bytes
    // *ออออออออออออออออออออออออออออ  <<V1.20>>
// rbw begin change
//    Function  DBFFormatDate( TEXT : WString ; COPT : Char ) : WString ;
    Function  DBFFormatDate( TEXT : AnsiString ; COPT : AnsiChar ) : AnsiString ;
// rbw end change
    Function  DBFFormatNumeric( TEXT : AnsiString ; XLEN , XDEC : byte ) : AnsiString ;
    Function  SetFind(NameField , Ope , ValComp : ansiString ): Integer;
// rbw begin change
//    Function  FindRecord(SearchType : String ): Integer;
    Function  FindRecord(SearchType : AnsiString ): Integer;
//    Function  UpdFieldX(FieldName: WString ; TEXT : WString ; COPT : char ) : boolean ;
    Function  UpdFieldX(FieldName: WString ; TEXT : AnsiString ; COPT : AnsiChar ) : boolean ;
// rbw end change


  Published
    Property FileName   : String  Read GetFileName     Write SetFileName ;
    Property Active     : Boolean  Read FActive         Write SetActive   ;
    Property AutoUpDate : Boolean  Read FAutoUpdate     Write FAutoUpdate ;
    Property DebugErr   : Boolean  Read FDebugErr       Write FDebugErr   ;
    Property Deleted    : Boolean  Read IsDelete        Write SetDeleteStatus;
    Property Recno      : Cardinal Read FCurrentRecord; (* CHABANT longint *)
    Property RecordCount: Cardinal Read Header.NumRecords; { Integer CHABANT }
    Property FieldCount : Integer  Read NumFields;
    Property Bof        : Boolean  Read IsBof ;
    Property Eof        : Boolean  Read IsEof ;
    Property LastRec    : Boolean  Read IsLastRec ;
    Property Modified   : boolean  Read FModified ;
    Property OnChanging : TXBChangeEvent Read FOnChanging Write FOnChanging;
    Property OnDeleting : TXBDeleteEvent Read FOnDeleting Write FOnDeleting;
    Property OnAppended : TNotifyEvent   Read FOnAppended Write FOnAppended;
    // 05/01/2003 rajouts CHABANT <<V211A>>
    Property RecordLength : Word  Read Header.RecordLen ;

  End;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ                 DEBUT des FONCTIONS ออออออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Implementation

resourcestring
  StrSizeOfRecordsToo = 'Size of records too large in %s. Try using fewer or' +
  ' smaller fields.';

Const
  //... valeurs de THeader.VersionNumber = TYPE de fichier DBASE
  dB3     = $03;
  dB4     = $04;
  dB5     = $05;
  dB3Memo = $83;
  dB4Memo = $84;
  VIDE    = ''  ;
  SPACE: AnsiChar = ' ' ;
  SPACE32 = 32  ;
  POINT   = '.' ;
  ZERO    = '0' ;
  VIRGULE = ',' ;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Constructor TXBase.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);
  FActive    := False ; (* IsOpen *)
  DebugErr   := False ; (* ++ CHABANT  *)
  AutoUpDate := True  ; (* ++ CHABANT  *)
  FFileName  := VIDE  ;
  FCurrentRecord := 0 ;
  NumFields      := 0 ;
  FDBFUpdated    := False ; //<<V110>> indique que le fichier est modifi้

End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Destructor TXBase.Destroy;
Begin
  If Active then
    Active := False;
  Inherited Destroy;
End;

             //****************************************
             //**       METHODES et PROPRIETES       **
             //**            PRIVEES                 **
             //****************************************

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Centralisation des Messages d'ERREURS  อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.ErreurGlobale( XERR : byte ; XDIV : integer ) ;
var  W1 , W2 : String ; // message d'erreur
begin
   W2 := IntToStr(XDIV) ;
   case XERR of
     00 : W1 := 'DBF FILE ALREADY OPEN ! ' ;
     01 : W1 := 'File Open : SetFileName not allowed ' ;
     02 : W1 := 'FillHeaderInfo  : SIZE   Read Field  # ' + W2 ;
     03 : W1 := 'ReadARecord     : OUT OF RANGE   Rec # ' + W2 ;
     04 : W1 := 'WriteARecord    : OUT OF RANGE   Rec # ' + W2 ;
   //  05 : W1 := 'GotoRecord      : OUT OF RANGE   Rec # ' + W2 ;
     06 : W1 := 'GetFieldName    : Bad FIELD number   # ' + W2 ;
     07 : W1 := 'GetFieldType    : Bad FIELD number   # ' + W2 ;
     08 : W1 := 'GetFieldSize    : Bad FIELD number   # ' + W2 ;
     09 : W1 := 'GetFieldDecimal : Bad FIELD number   # ' + W2 ;
     10 : W1 := 'Information for Debugging ! Field not found ';
     11 : W1 := 'UpdateMEMOData  : Bad FIELD number   # ' + W2 ;
     12 : W1 := 'GetFieldByNumber: Bad FIELD number   # ' + W2 ;
     13 : W1 := 'GetFieldByName  : FIELD NOT Found ' ;
     14 : W1 := 'UpdateFieldData : Bad FIELD number   # ' + W2 ;
   //  15 : W1 := 'PutFieldByNumber: Bad FIELD number   # ' + W2 ;
     16 : W1 := 'GetMEMOData     : READ FILE    field # ' + W2 ;
     17 : W1 := 'PutFieldByName  : FIELD NOT Found ' ;
     18 : W1 := 'FindRecord : NO active FILTER ' ;
     20 : W1 := 'DBF FILE CLOSED : New Close Impossible ! ' ;
     21 : W1 := 'DECIMAL POINT OUT OF GOOD POSITION  field # ' + W2 ;

     50 : W1 := 'DBase3 FILE NOT OPEN ' ;
     51 : W1 := 'FillHeaderInfo  : HEADER Read Fail   # ' + W2 ;
     52 : W1 := 'FillHeaderInfo  : BUFF   Read Field  # ' + W2 ;
     53 : W1 := 'ReadARecord     : READBUFFER  Record # ' + W2 ;
     54 : W1 := 'WriteARecord    : WRITE ERROR Record # ' + W2 ;
     55 : W1 := 'AppendBlank     : WRITE ERROR Record # ' + W2 ;
     60 : W1 := 'DBF FILE NOT EXISTING ; OPEN impossible ' ;
     99 : W1 := 'COMPILING PROBLEM : SIZE OF Structures  ' ;
     else W1 := 'UNKNOWN ERROR ??  # ' + W2
   end ;
   W1 := 'ERROR:' + IntToStr(XERR) + ' *'#013 +
         W1 + #013' on File ' + string(FFileName) ;
   if DebugErr then begin
        ShowMessage(W1)
   end else begin
        if XERR >= 50 then
             Raise EXBaseException.Create(W1) ;
   end ;
end ;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de Manipulation de CHAINES    อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(* ajoute des ESPACES en d้but pour atteindre la longueur indiqu้e       *)
(* J'optimise cette routine    ; et Ajustement เ la longueur EXACTE      *)
Function TXBase.PadLeft(Text: AnsiString; CPT: Integer): AnsiString ;
var X1 : smallint ;
Begin
  Assert(Length(Text) <= 255);
  X1 := length(Text) ;
  if X1 = CPT then begin
      Result := TEXT ;
  end else begin
      if X1 > CPT then begin
           Result := copy(TEXT , 1 + X1 - CPT  , CPT ) ; // RIGHT( CPT car)
      end else begin
           X1 := CPT - X1 ;  { Nb BLANCS … rajouter }
           Result := StringOfChar(SPACE , X1) + Text ;
      end ;
  end ;
  Assert(Length(Result) <= 255);
end;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.PadRight(Text: AnsiString; CPT : Integer): AnsiString;
(* ajoute des ESPACES en Fin   pour atteindre la longueur indique       *)
(* J'optimise cette routine    ; et Ajustement เ la longueur EXACTE      *)
var X1 : smallint ;
Begin
  Assert(Length(Text) <= 255);
  X1 := length(Text) ;
  if X1 = CPT then
       Result := Text
  else begin
       if X1 > CPT then
           Result := copy(Text, 1 , CPT)
       else begin
           X1 := CPT - X1 ;  { Nb BLANCS … rajouter }
           Result := Text + StringOfChar(SPACE , X1) ;
       end ;
  end ;
  Assert(Length(Result) <= 255);
end;


(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines DIVERSES                      อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
procedure TXBase.DBFSetDateAMJ(var ZAMJ3 : array of byte) ;
          // arg ZAMJ3 = zone de 3 bytes ANNEE - MOIS - JOUR
var
   AA1 , MM1 , JJ1 : word ;
   W_LastUpdated   : array[1..3] of byte ;
begin
   DecodeDate( Now , AA1 , MM1 , JJ1 ) ;
   AA1 := AA1 mod 100 ;
   // DBASE3 original refuse les Dates au dela de 1999
   // l'ann้e doit etre sur DEUX chiffres 00..99
   W_LastUpdated[1] := lo(AA1) ;  // Octet de poids faible
   W_LastUpdated[2] := lo(MM1) ;
   W_LastUpdated[3] := lo(JJ1) ;
   move(W_LastUpdated , ZAMJ3 , 3 ) ;
end ;

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
Function TXBase.GetFileName: String;
Begin
  Result := FFileName ;
End;


(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
// Standardise le NOM d'un fichier Dbase3  ( extension .DBF )
Function  TXBase.StandardDBFName(Text: String): String ;
begin
   Result := Trim( Text ) ;
   if length(Result) = 0 then
             Result := 'WORK.DBF' ;
   if Pos(POINT, Result) = 0 then begin
      Result := Result + '.DBF' ;
   end;
end ;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.SetFileName(Name: String);
begin
  if FActive = true then begin
       ErreurGlobale(01 , 0) ;
  end else begin
       FFileName := StandardDBFName( Name ) ;
  end ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(*:ออ             OUVERTURE du FICHIER DBF                            ออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.SetActive(VAL : Boolean);    (* OPEN ou CLOSE fichier *)
var W_Name : String ;
    J1     : Smallint ;
Begin
  W_Name  := FFileName ;
         //***** OUVERTURE *****
  If VAL = true then Begin
     //... controle fichier d้ja ouvert  ...
     if FActive then begin
           ErreurGlobale(00 , 0) ;
           exit ;
     end ;
     //... controle existence du fichier ...
     if not FileExists(W_Name) then begin
           ErreurGlobale(60 , 0) ;
           exit ;
     end ;
     //... OPEN v้ritable ......
     DBFile  := TFileStream.Create(W_Name ,
                fmOpenReadWrite or fmShareDenyWrite ) ; // = $0020;
     FActive := True;
     FDBFUpdated := False ; //<<V110>> indique que le fichier est modifi้
     FillHeaderInfo;
     { fichier MEMO }
     If (Header.versionNumber = dB3Memo) Or (Header.versionNumber = dB4Memo) Then Begin
        J1 := pos(POINT , W_Name) - 1 ;
        if J1 <= 0 then J1 := length(W_Name) ;
        W_Name := copy(W_Name , 1 , J1) + '.DBT' ;
        MemoFile := TFileStream.Create(W_Name , fmOpenReadWrite);
        MemoFile.Seek(0, soFromBeginning);
        MemoFile.ReadBuffer(DBTHeader, Sizeof(DBTHeader));
     End;
         //***** FERMETURE *****
  end else begin
     //... controle fichier encore ouvert  ...
     if FActive then begin
           if FAutoUpdate then
                    PostChanges ;
           //... M-A-J de s้curit้ du HEADER ........... <<V110>>
           if FDBFUpdated then begin
               DBFSetDateAMJ( Header.LastUpdated ) ;
               DbFile.Seek(0, soFromBeginning)     ;  //MAJ-HEADER
               DBFile.WriteBuffer(Header, KSIZHDR) ;  //...
           end ;
           //...
           FreeAndNil(DBFile);
           FreeAndNil(MemoFile);
           FActive := False;
          end
     else begin
           ErreurGlobale(20 , 0) ;
           exit ;
     end ;
  End;
End;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de BAS NIVEAU ( TFILESTREAM ) อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.FillHeaderInfo;
Var
  TmpC , TmpA , X1 , XL1 , XL2 : Integer;
// rbw begin change
//  C1 : char ;
  C1 : AnsiChar ;
// rbw end change
Begin
  // KSIZHDR =  32 ; // taille officielle du HEADER
  // KSIZFLD =  32 ; // taille officielle de chaque DESCRITEUR de CHAMP
  if (Sizeof(Header)       <> KSIZHDR) or
     (Sizeof(TFieldStruct) <> KSIZFLD) then begin
        ErreurGlobale(99 , 0) ;
        halt ;
  end ;

  {Get Header Info}
  TmpA := 2;
  XL1  := KSIZHDR ;  // en principe = 32
  Try
    //.... Le fichier doit etre OUVERT pour le lire !
    DBFile.Seek(0, soFromBeginning);
    DBFile.ReadBuffer(Header, XL1);
    //.... 19/12/2002 ERREUR l'en-tete est 32 bytes et non pas 34 !
    //.... NumFields := Trunc((Header.HeaderLen - 34) / 32);
    NumFields := Trunc((Header.HeaderLen - 32) / 32);
    //---- GetFieldInfo -----
    For TmpC := 1 To NumFields Do Begin
      //..... 20/12/2002 ... TRY suppl้mentaire
      Try
         DBFile.Seek(TmpC * 32, soFromBeginning);
         //... 20/12/2002 : probl่me de lecture impossible เ debugger
         //... je d้couvre le pot aux roses : TFieldStruct
         //...     occupe 36 octets alors qu'il doit etre 32 comme attendu !!
         //    DBFile.ReadBuffer(FieldStruct[TmpC], Sizeof(TFieldStruct) );
         XL2 := DBFile.Read(FieldStruct[TmpC], KSIZFLD);
         if XL2 <> KSIZFLD then begin
              ErreurGlobale(02 , TmpC) ;
         end ;
      Except
         FillChar( FieldStruct[TmpC], KSIZFLD , #00 ) ;
         ErreurGlobale(52 , TmpC) ;
      end ; // TRY
      //....
      with FieldStruct[TmpC] do begin
           Address := TmpA;
           TmpA := TmpA + FieldLength;
           //**  garantir NOM des Champs en MAJUSCULES
           //**  "FieldName est un ARRAY et non pas un String "
           for X1 := 1 to KSIZNAM do begin
               C1 := FieldName[X1] ;
               if C1 >= 'a' then FieldName[X1] := upcase(C1) ;
           end ;
      end ;
    End;
    FCurrentRecord := 1 ;  // Lire le 1er Record en mmoire
    ReadARecord;
  Except
    ErreurGlobale(51 , XL1 ) ;
  End;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.ReadARecord ;    // LECTURE du numero-rec "FCurrentRecord"
var
  X1 , X2 , X5ADR : Integer ; X3 : cardinal ;
Begin
  X1 := Header.HeaderLen ;  // Multiple de 32 + 1
  X2 := Header.RecordLen ;
  X3 := FCurrentRecord   ;
  {-- s้curit้ necessaire sinon "EREADERROR , Erreur de lecture de Flux" ---}
  if (X3 < 1) or (X3 > Header.NumRecords) then begin
      //** je remplace par un record fictif a BLANC
      Assert(X2 <= Length(RecordBuffer));
      FillChar( RecordBuffer, X2 , SPACE32 ) ;
      //** 0 = simule Begin Of File , N+1 = simule End Of File
      if (X3 = 0) or (X3 = Header.NumRecords + 1) then begin
               FCurrentRecord := X3 ; // ceci est O.K.
      end else begin
               ErreurGlobale(03 , X3  ) ;
               FCurrentRecord := Header.NumRecords + 1 ;
      end ;
  end else begin
      Try
        dec(X3) ;  { compter เ partir de ZERO }
        X5ADR := X1 + (integer(X3) * X2) ;
        DBFile.Seek(X5ADR , soFromBeginning);
        DBFile.ReadBuffer(RecordBuffer, X2);
      Except
        ErreurGlobale(53 , X3+1 ) ;
      End;
  end ;
  FModified := False ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.RefreshRecord;
Begin
  If FActive Then Begin
      ReadARecord;
  End Else Begin
      ErreurGlobale(50 , 0 ) ;
  End;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(*:ออ MISE-a-JOUR d'un RECORD ( ReEcriture ) mais pas NOUVEAU RECORD  ออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.WriteARecord;
var
  X1 , X2 : Integer ;   X3 : cardinal ;
//  X5ADR: Integer;
  X5ADR: Int64;
Begin
  X1 := Header.HeaderLen ;
  X2 := Header.RecordLen ;
  X3 := FCurrentRecord   ;
  //--- securite = ne pas ecrire si N๘ RECORD en dehors des limites ----
  if (integer(X3) < 1) or (X3 > Header.NumRecords) then begin
        ErreurGlobale(04 , X3 ) ;
        end
  else  begin
        Try
          dec(X3) ;
          X5ADR := X1 + (integer(X3) * X2) ;
          DBFile.Seek(X5ADR , soFromBeginning);
          DBFile.WriteBuffer(RecordBuffer, X2);
          FDBFUpdated := true ; //<<V110>>
        Except
          ErreurGlobale(54 , X3+1 ) ;
        End;
  end ;
  FModified := False;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.IsDelete: Boolean;
Begin
  If FActive Then  Result := (RecordBuffer[1] = DELETE_FLAG)
             else  Result := False ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.SetDeleteStatus(Delete: Boolean);
begin
  If FActive Then Begin
     //.... ma version <<V1.10>>
     If CanDelete then begin
           if Delete then begin
                  RecordBuffer[1] := DELETE_FLAG ;
           end else begin
                  RecordBuffer[1] := SPACE ;
           end ;
           FModified := true ;
     End;
  end ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.IsBof: Boolean;
Begin
  Result := False;
  If FCurrentRecord <= 1 Then Result := True;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.IsLastRec : Boolean;
Begin
  Result := False;
  If FCurrentRecord = Header.NumRecords then Result := True;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(* อ EOF doit etre donn้ seulement quand on va au-dela du dernier Rec  อ *)
Function TXBase.IsEof: Boolean;
Begin
  Result := False;
  If FCurrentRecord > Header.NumRecords Then Result := True;
End;


             //****************************************
             //**       METHODES et PROPRIETES       **
             //**            PUBLIQUES               **
             //****************************************


//*:ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(*:ออ REWRITE du record courant aprs des Modifications               ออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.PostChanges;
Begin
    if FModified then
          WriteARecord ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(*:ออ CREATION d'un NOUVEAU RECORD TOUT … BLANC , en FIN de Fichier   ออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.AppendBlank;
Var
  // TmpI: Integer  ;  TmpC: Char;
  X1 , X2  : integer ;
Begin
  If FActive Then Begin
    X1 := Header.NumRecords + 1 ;    // nouveau nombre de records
    Header.NumRecords := X1 ;
    Try
      //......... mon codage , plus sur et performant ........
      FCurrentRecord := X1 ; // position sur NOUVEAU record
      X2 := Header.RecordLen ;
      Assert(X2 <= Length(RecordBuffer));
      FillChar( RecordBuffer, X2 , SPACE32 ) ;
      FModified := true ;
      PostChanges ;   // ci-dessus lg433
      //.... Mise a Jour imm้diate du HEADER pour qu'il refl่te la r้alit้ ....
      DbFile.Seek(0, soFromBeginning)     ;  //MAJ-HEADER
      DBFile.WriteBuffer(Header, KSIZHDR) ;  //...
      FDBFUpdated := true ; //<<V110>>
      if Assigned(FOnAppended) then begin
          FOnAppended(Self);
      end;
    Except
      ErreurGlobale(55 , Header.NumRecords ) ;
      //.. r้tablir les valeurs initiales
      dec (X1) ;
      Header.NumRecords := X1 ;
    End;
  end else Begin
    ErreurGlobale(50 , 0 ) ;
  End;
End;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de RECHERCHE de RECORDS sur Crit่re อออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
// rbw begin change
//Function TXBase.FindRecord(SearchType : String ): Integer;
Function TXBase.FindRecord(SearchType : AnsiString ): Integer;
// rbw end change
// arg = 'First' ou 'Next' or 'Last' or 'Prev'
// seule la 1ERE lettre compte , par d้faut c'est SEARCH NEXT
// NEXT recherche เ partir de la position courante ( excluse )
// le r้sultat est le Nฐ de RECORD trouv้ ;
// si aucun trouv้ , le r้sultat est ZERO ;
var
   X1 , J1 , J2 : integer ;
// rbw begin change
//   C1 : char ;
   C1 : AnsiChar ;
// rbw end change
   M1 : extended ;
   W1 : AnsiString ;
   VOK , VFIN : boolean ;

begin
  Result := 0 ;
  if not FActive then Begin
      ErreurGlobale(50 , 0 ) ;
      exit ;
  end ;
  // X1 := Header.NumRecords ;
  X1 := RecordCount ;
  if X1 = 0 then begin
      exit ;
  end ;
  if FFindNum < 1 then begin
      ErreurGlobale(18 , 0 ) ;
      exit ;
  end ;
  C1 := UpCase(SearchType[1]) ;
  case C1 of
     'F' : begin
           GotoBOF ;
           end ;
     'L' : begin
           GotoEOF ;
           end ;
     'P' : begin
           GotoPrev ;
           end ;
     else  begin
           GotoNext ;
           end ;
  end ;
  VFIN := false ;
  repeat
     //...... TEST DU FILTRE .....
     VOK := false ;
     W1  := AnsiString(GetFieldByNumber(FFindNum)) ;
     case FFindType of  // type de Champ
         'N' : begin
               W1[length(W1) + 1] := #00 ;
               {$IFDEF Delphi_2009_UP}
               if FormatSettings.DecimalSeparator <> POINT then
               {$ELSE}
               if DecimalSeparator <> POINT then
               {$ENDIF}
               begin
                  J2 := pos(POINT , string(W1)) ;
                  if J2 > 0 then
// rbw begin change
//                     W1[J2] := DecimalSeparator ;
                  begin
                    {$IFDEF Delphi_2009_UP}
                    Assert(FormatSettings.DecimalSeparator <= #$00FF);
                    W1[J2] := AnsiChar(FormatSettings.DecimalSeparator);
                    {$ELSE}
                    Assert(DecimalSeparator <= #$00FF);
                    W1[J2] := AnsiChar(DecimalSeparator);
                    {$ENDIF}
                  end;
// rbw end change
               end ;
// rbw begin change
//               TextToFloat(@W1[1] , M1 , fvExtended) ;
               TextToFloat(PAnsiChar(@W1[1]) , M1 , fvExtended) ;
// rbw end change
               case FFindOpe of
                   '=' :  if M1 =  FFindValN then VOK := true ;
                   'E' :  if M1 =  FFindValN then VOK := true ;
                   '#' :  if M1 <> FFindValN then VOK := true ;
                   '<' :  if M1 <  FFindValN then VOK := true ;
                   '>' :  if M1 >  FFindValN then VOK := true ;
                   'G' :  if M1 >= FFindValN then VOK := true ;
                   'L' :  if M1 <= FFindValN then VOK := true ;
               end ;  //case
               end ;
         else  begin
               M1 := 0 ;
               case FFindOpe of
                   '=' :  begin // ้galit้ sur le d้but du string
                          J1 := length(FFindValS) ;
                          if length(W1) > J1 then SetLength(W1 , J1) ;
                          if W1 =  FFindValS then VOK := true ;
                          end ;
                   'E' :  // ้galit้ stricte
                          if W1 =  FFindValS then VOK := true ;
                   '#' :  if W1 <> FFindValS then VOK := true ;
                   '<' :  if W1 <  FFindValS then VOK := true ;
                   '>' :  if W1 >  FFindValS then VOK := true ;
                   'G' :  if W1 >= FFindValS then VOK := true ;
                   'L' :  if W1 <= FFindValS then VOK := true ;
               end ;  //case
               end ;
     end ;
     //......
     if not VOK then begin
         if (C1 = 'L') or (C1 = 'P') then begin
             if Recno > 1 then GotoPrev
                          else VFIN := true ;
         end else begin
             if not LastRec then GotoNext
                            else VFIN := true ;
         end ;
     end ;
  until VOK or VFIN ;
  if VOK then
     Result := RecNo ;
end ;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.SetFind(NameField , Ope , ValComp : AnsiString ): Integer;
var X1, X2 : smallint ;
    C1 : AnsiChar ;
    M1 : Extended ;
    V1 : Boolean ;
begin
    Assert(Length(NameField) <= 255);
    Assert(Length(Ope) <= 255);
    Assert(Length(ValComp) <= 255);
    Result := 0 ;
    FFindField    := NameField ;
    if length(Ope) = 0 then FFindOpe := '='
                       else FFindOpe := Ope[1] ;
    if length(Ope) >= 2 then begin
       if FFindOpe = '<' then begin
           if Ope[2] = '='  then FFindOpe := 'L' ;
           if Ope[2] = '>'  then FFindOpe := '#' ;
       end ;
       if FFindOpe = '>' then begin
           if Ope[2] = '='  then FFindOpe := 'G' ;
           if Ope[2] = '<'  then FFindOpe := '#' ;
       end ;
       if FFindOpe = '=' then begin
           if Ope[2] = '='  then FFindOpe := 'E' ;
       end ;
    end ;
    if pos(string(FFindOpe) , '=#><LGE') = 0 then
           FFindOpe := '=' ;
    FFindValS := ValComp ;
    X1 := GetFieldNumberFromName(NameField) ;
    FFindNum  := X1 ;
    FFindValN := 0  ;
    if X1 > 0 then begin
        C1 := GetFieldTypeChar(X1) ;
        FFindType := C1 ;
        if C1 = 'N' then begin
             X2 := length(ValComp) + 1 ;
             ValComp[X2] := #00 ; // chaine ZERO terminal
// rbw begin change
//             V1 := TextToFloat(@ValComp[1] , M1 , fvExtended) ;
             V1 := TextToFloat(PAnsiChar(@ValComp[1]) , M1 , fvExtended) ;
// rbw end change
             if V1 then  FFindValN := M1
                   else  Result := 2 ;  // ERREUR
        end ;
    end else begin
        Result := 1 ; // ERREUR
    end ;
end ;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de RECHERCHE des CHAMPS       อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
function TXBase.GetFieldNum(FieldNum: integer): Extended;
Var
  TmpS: String;
  SaveDecSep : Char ;
Begin
  {$IFDEF Delphi_2009_UP}
  SaveDecSep := FormatSettings.DecimalSeparator ;
  FormatSettings.DecimalSeparator := POINT ;  // '.' K_POINT
  {$ELSE}
  SaveDecSep := DecimalSeparator ;
  DecimalSeparator := POINT ;  // '.' K_POINT
  {$ENDIF}
  TmpS   := string(GetFieldByNumber( FieldNum )) ;
  Result := 0 ;
  if length(TmpS) > 0 then  begin
      try
          Result := StrToFloat( TmpS ) ;
      except
          on E: EConvertError do Result := 0 ;
      end ;
  end ;
  {$IFDEF Delphi_2009_UP}
  FormatSettings.DecimalSeparator := SaveDecSep ;
  {$ELSE}
  DecimalSeparator := SaveDecSep ;
  {$ENDIF}
end;

Function TXBase.GetFieldNumberFromName(Text: AnsiString): Integer;
Var
  VOK   : boolean ;
  XFLD ,  {XLEN1 ,} XLEN2 : smallint;
  WTXT ,  CNOM  : String ;
Begin
  result := 0;
  try
    Assert(Length(Text) <= 255);
    if Text = '' then
    begin
      Exit;
    end;
    VOK  := false   ;
    //** les Champs DBASE3 ont un nom en MAJUSCULE de 10 car ,
    //**      padd้ avec des NULLS et non pas des SPACES
    WTXT := string(Text) ;
    if WTXT[1] >= 'a' then  WTXT := UpperCase(string(Text)) ;

    // RBW, Aug. 19, 2004: According the the above comment, spaces are not
    // allowed in field names.  However, I have not been able to confirm
    // this restriction and at least one application (Argus ONE) creates
    // DBASE files (as part of a Shape File) that can contain spaces and
    // these DBASE files can be used.  Therefore, I have changed the code
    // below to remove leading and trailing spaces.  Previously, Field names
    // were truncated at the position of the first space.

    // RBW July 18, 2008. Some DBASE files contain fields than end with a
    // space character. so the trimming of names has been eliminated.
  //  WTXT := Trim(WTXT);
    {XLEN1 := pos( #32 , WTXT ) ;
    if XLEN1 > 1 then begin
            dec(XLEN1) ; SetLength(WTXT , XLEN1) ;
    end ;}

    XFLD := 1;
    while XFLD <= NumFields do begin
        // est deja  UpperCase( String( FieldStruct[ XFLD ].Fieldname ) )
  // rbw begn change
  //      CNOM := String( FieldStruct[ XFLD ].FieldName ) ;
        CNOM := string(FieldStruct[ XFLD ].FieldName);
  // rbw begn change
           //**** ATTENTION , ceci retourne une chaine de 11 CAR
           //  Y compris le #00 final , ce qui est surprenant !!!!
           //  Par ailleurs , dans le Header DBASE3 les noms sont
           //  padd้s avec des #00 , et non pas des #32 , attention
           //  Pour faire une comparaison correcte , il faut donc
           //  ้liminer ces #00 du string เ comparer avec WTXT
           //****
        //** Alignement des 2 longueurs pour la comparaison **
        XLEN2 := pos( #00 , CNOM) ;
        if XLEN2 >= 1 then dec(XLEN2) else XLEN2 := 10 ; // Vraie Longueur utile
        SetLength(CNOM , XLEN2 )  ;
        if CNOM = WTXT  then begin
                  VOK := true ;
                  break ; { exit of loop }
                  end ;
        inc(XFLD);
    end ;
    if VOK  then  Result := XFLD  else  Result := 0  ;
  finally
    //** un field not found est un cas normal
    if FDebugErr and (Result = 0) then
          ErreurGlobale(10 , 0 ) ;
    //if FDebugErr and (Result = 0) then
    //    showmessage('~~FROMNAME~~'+FFileName+'#'+Text+'/'+WTXT+'/'+CNOM+'#'
    //               +inttostr(XLEN1)+'#'+inttostr(XLEN2)+'#' ) ;
  end;
end;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldName(FieldNo: Integer): AnsiString;
var X1 : smallint ;
Begin
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
      // copie des X octets de la Z-Chaine contenant le Nom du Champ
// rbw begin change
//      Result := String( FieldStruct[ FieldNo ].Fieldname ) ;
      Result := AnsiString( FieldStruct[ FieldNo ].Fieldname ) ;
// rbw begin change
           { deja en UpperCase }
      //... il faut ้liminer les $00 finaux ( Null char )  <<11/01/2003>>
      X1 := pos( #00 , string(Result) ) ;
      if (X1 <= 0) or (X1 > 10) then X1 := 11 ;
      dec(X1) ;
      SetLength(Result , X1) ; 
  End Else Begin
      ErreurGlobale(06 , FieldNo ) ;
      Result := VIDE ;
  End;
  Assert(Length(Result) <= 255);
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldType(FieldNo: Integer): TFieldType;
Begin
  Result := xbfUnknown;
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
      Case ord(FieldStruct[FieldNo].FieldType) Of
         66: result := xbfBlob  ;    { B }
         67: Result := xbfChar  ;    { C }
         68: Result := xbfDate  ;    { D }
         70: Result := xbfNumber;    { F }
         76: Result := xbfLogic ;    { L }
         77: Result := xbfMemo  ;    { M }
         78: Result := xbfNumber;    { N }
      End;
  End Else Begin
      ErreurGlobale(07 , FieldNo ) ;
  End;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
// rbw begin change
//Function TXBase.GetFieldTypeChar(FieldNo: Integer): Char ;
Function TXBase.GetFieldTypeChar(FieldNo: Integer): AnsiChar ;
// rbw end change
Begin
  If (FieldNo >= 1) And (FieldNo <= NumFields) then
        Result := FieldStruct[FieldNo].FieldType
  else  begin
        Result := SPACE ;
        ErreurGlobale(07 , FieldNo ) ;
  end;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldSize(FieldNo: Integer): Integer;
Begin
  Result := 0 ;
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
        Result := FieldStruct[FieldNo].FieldLength ;
  End Else Begin
        ErreurGlobale(08 , FieldNo ) ;
  End;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldDecimals(FieldNo: Integer): Integer;
Begin
  Result := 0 ;
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
      Result := FieldStruct[FieldNo].Decimals ;
  End Else Begin
      ErreurGlobale(09 , FieldNo ) ;
  End;
End;

function TXBase.GetFieldInt(FieldNum: integer): Integer;
Var
  TmpS: String;
Begin
  TmpS   := GetFieldByNumber( FieldNum ) ;
  if length(TmpS) > 0 then
       Result := StrToIntDef( TmpS , 0 )
  else Result := 0 ;
end;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines Sp้cifiques aux Champs MEMO   อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.UpdateMemoData(FieldNo: integer; Text: AnsiString);
Var
  TmpL  : Longint;
  CMEMO : AnsiString ;
  Block : AnsiString;

Begin
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
    CMEMO := AnsiString(IntToStr(DBTHeader.NextBlock));
    // UpdateFieldData(FieldNo, CMEMO )
    PutFieldByNumber(FieldNo, CMEMO );    // le pointeur en 10 chiffres Ascii
    MemoFile.Seek( - 1, soFromEnd);
    Block := Chr(26) + Chr(26);
    For TmpL := 1 To Length(Block) Do Begin
        MemoFile.Write(Block[TmpL], 1);
    End;
    MemoFile.Seek(DBTHeader.NextBlock * 512, soFromBeginning);
    Text := Text + chr(26) + Chr(26);
    For TmpL := 1 To Length(Text) Do Begin
        MemoFile.Write(Text[TmpL], 1);
    End;
    //.. cela me semble faux, ne faut-il pas incrementer du nombre de
    //.. blocks de 512 octets consomms = (length(Text) div 512) + 1
    inc(DBTHeader.NextBlock);
    FModified := True;
  End Else Begin
      ErreurGlobale(11 , FieldNo ) ;
  End;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetMemoData(FieldNo: integer): AnsiString;
Var
  TmpS: AnsiString;
// rbw begin change
//  Buff: Array[1..512] Of Char;
  Buff: Array[1..512] Of AnsiChar;
// rbw begin change
  Flag: Boolean;
  X1  : smallint ;
Begin
  TmpS := VIDE ;
  Try
    Flag := False;
    MemoFile.Seek(FieldNo * 512, soFromBeginning);
    While (Not Flag) Do Begin
      If MemoFile.Read (Buff, 512) < 512 Then Begin
        Flag := True;
      End;
      X1 := Pos(#26#26, string(Buff)) - 1 ;
      If X1 >= 0 Then Begin
        TmpS := TmpS + Copy(Buff, 1, X1 );
        Flag := True;
      End Else Begin
        TmpS := TmpS + Copy(Buff, 1, 512 );
      End;
    End;
    //... transformation des pseudo-CR en CarriageReturn .. OPTIMISATION <<V1.10>>
    repeat
        X1 := Pos(#141, TmpS) ;
        if X1 > 0 then
              TmpS[X1] := #13;
    until X1 = 0 ;
  Except
      ErreurGlobale(16 , FieldNo ) ;
  End;
  Result := TmpS;
End;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de Lecture des   CHAMPS       อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldByNumber(FieldNum: Integer): String;
Var
  TmpS : String ;
  Index : integer;   
Begin
  TmpS := VIDE ;
  If (FieldNum >= 1) And (FieldNum <= NumFields) Then Begin
      with FieldStruct[FieldNum] do begin
           TmpS := Copy(string(RecordBuffer), Address, FieldLength);
      end ;
     Index := Pos(#0, TmpS);
     if Index > 0 then
     begin
       TmpS := Copy(TmpS, 1, Index-1);
     end;
      // si c'est un champ MEMO , cela donne un Pointeur Ascii sur 10 chiffres
  End Else Begin
      ErreurGlobale(12 , FieldNum ) ;
  End;
  Result := TmpS;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldByName(FieldName: AnsiString): String;
Var
  FieldNum: Integer ;
  TmpS    : String ;
  Index : integer;
//  AString: string;
  {$IF CompilerVersion < 23}
  CharArray: array of AnsiChar;
  {$IFEND}
Begin
  TmpS := VIDE ;
  FieldNum := GetFieldNumberFromName(FieldName);
  If FieldNum > 0 Then Begin
     with FieldStruct[FieldNum] do begin
//          Move(Addr(RecordBuffer[Address]), CharArray[0], FieldLength*Size(AnsiChar));
//          AString := string(RecordBuffer);
//          TmpS := Copy(AString, Address, FieldLength);
  {$IF CompilerVersion >= 23}
          TmpS := Copy(string(RecordBuffer), Address, FieldLength);
  {$ELSE}
          // In Delphi 2006, string(RecordBuffer) returns a shortstring.
          SetLength(CharArray, FieldLength+1);
          CharArray[FieldLength] := Char(0);
          for Index := 0 to FieldLength-1 do
          begin
            CharArray[Index] := RecordBuffer[Address+Index];
          end;
          TmpS := Ansistring(PAnsiChar(CharArray));
  {$IFEND}
     end ;
     Index := Pos(#0, TmpS);
     if Index > 0 then
     begin
       TmpS := Copy(TmpS, 1, Index-1);
     end;
     // si c'est un champ MEMO , cela donne un Pointeur Ascii sur 10 chiffres
  End Else Begin
      ErreurGlobale(13 , 0 ) ;
  End;
  Result := TmpS;
  Assert(Length(result) <= 255);
End;

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldStr(FieldName: AnsiString): String;
Begin
  Result := GetFieldByName( FieldName ) ;
end ;

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldInt(FieldName: AnsiString): Integer ;
Var
  TmpS: String;
Begin
  TmpS   := GetFieldByName( FieldName ) ;
  if length(TmpS) > 0 then
       Result := StrToIntDef( TmpS , 0 )
  else Result := 0 ;
end ;

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldNum(FieldName: AnsiString): Extended ;
Var
  TmpS: String;
  SaveDecSep : Char ;
Begin
  {$IFDEF Delphi_2009_UP}
  SaveDecSep := FormatSettings.DecimalSeparator ;
  FormatSettings.DecimalSeparator := POINT ;  // '.' K_POINT
  {$ELSE}
  SaveDecSep := DecimalSeparator ;
  DecimalSeparator := POINT ;  // '.' K_POINT
  {$ENDIF}
  TmpS   := GetFieldByName( FieldName ) ;
  Result := 0 ;
  if length(TmpS) > 0 then  begin
      try
          Result := StrToFloat( TmpS ) ;
      except
          on E: EConvertError do Result := 0 ;
      end ;
  end ;
  {$IFDEF Delphi_2009_UP}
  FormatSettings.DecimalSeparator := SaveDecSep ;
  {$ELSE}
  DecimalSeparator := SaveDecSep ;
  {$ENDIF}
end ;

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Function TXBase.GetFieldDat(FieldName: AnsiString): String;
{ utilise les variables generales de formatage
     ShortDateFormat
  et DateSeparator
  pour la FRANCE cela est normalement dd/mm/yyyy  et  /
}
// rbw begin change
//var W_CSEP : Char ;
//    W_CTYP : Char ;
var W_CSEP : Char ;
    W_CTYP : Char ;
// rbw begin change
    WD1    : String ;
Begin
// rbw begin change
//    W_CSEP := DateSeparator ;
    {$IFDEF Delphi_2009_UP}
    Assert(FormatSettings.DateSeparator <= #$00FF);
    W_CSEP := FormatSettings.DateSeparator;
    {$ELSE}
    Assert(DateSeparator <= #$00FF);
    W_CSEP := DateSeparator;
    {$ENDIF}
// rbw end change
       { '/' ou '-' ou '.'  }
// rbw begin change
//    W_CTYP := upcase( ShortDateFormat[1] ) ;
    {$IFDEF Delphi_2009_UP}
    Assert(FormatSettings.ShortDateFormat[1] <= #$00FF);
    W_CTYP := upcase( (FormatSettings.ShortDateFormat[1]) ) ;
    {$ELSE}
    Assert(ShortDateFormat[1] <= #$00FF);
    W_CTYP := upcase( AnsiChar(ShortDateFormat[1]) ) ;
    {$ENDIF}
// rbw end change
       { 'D' pour DAY   ==> FRANCE et EUROPE }
       { 'M' pour MONTH ==> USA              }
       { 'Y' pour YEAR  ==> ANSI , JAPON ?   }
    { valeurs par defaut }
    if pos(string(W_CSEP) , '/-.') = 0 then  W_CSEP := '/' ;
    if pos(string(W_CTYP) , 'DMY') = 0 then  W_CSEP := 'D' ;
    WD1 := GetFieldByName( FieldName ) ; { en Dbase, c'est AAAAMMJJ }
    WD1 := Copy(WD1, 1, 12);
    case W_CTYP of
       'D' : Result := copy(WD1,7,2) + W_CSEP +
                       copy(WD1,5,2) + W_CSEP + copy(WD1,1,4) ;
       'M' : Result := copy(WD1,5,2) + W_CSEP +
                       copy(WD1,7,2) + W_CSEP + copy(WD1,1,4) ;
       'Y' : Result := copy(WD1,1,4) + W_CSEP +
                       copy(WD1,5,2) + W_CSEP + copy(WD1,7,2) ;
       else  Result := WD1 ;
    end ;
    Assert(Length(Result) <= 255);
end ;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de MODIFICATION des CHAMPS    อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.PutFieldByNumber(FieldNo : Integer; TEXT : AnsiString);
Var
  XLEN , XDEC : smallint ;
// rbw begin change
//  CTYP : Char     ;
  CTYP : AnsiChar     ;
// rbw end change
  AFLD : DWORD    ; { adresse du Champ dans le record }
Begin
  Assert(Length(TEXT) <= 255);
  If (FieldNo >= 1) And (FieldNo <= NumFields) Then Begin
      with FieldStruct[FieldNo]  do begin
           XLEN := FieldLength ;
           XDEC := Decimals    ;
           CTYP := FieldType   ;
           AFLD := Address     ;
      end ;
      if length(TEXT) <> XLEN  then begin
         if (CTYP = 'N') or (CTYP = 'M') then begin
             TEXT := PadLeft( TEXT, XLEN) ; // ajuste exactement
         end else begin
             if length(TEXT) > XLEN  then
                   SetLength(TEXT , XLEN)
             else  TEXT := PadRight(TEXT, XLEN) ; // ajuste exactement
         end ;
      end ;
      //.... Controle de la bonne position du POINT DECIMAL ...
      if (CTYP = 'N') and (XDEC > 0) then begin
          if TEXT[XLEN - XDEC] <> POINT then begin
                ErreurGlobale(21 , FieldNo ) ;
          end ;
      end ;
      //....
      Move( TEXT[1] , RecordBuffer[AFLD] , XLEN ) ;
      FModified := True;
      end
  else begin
      ErreurGlobale(14 , FieldNo ) ;
  end;
end;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.PutFieldByName(FieldName, Data: AnsiString);
Var
  FieldNum: Integer;
Begin
  Assert(Length(FieldName) <= 255);
  Assert(Length(Data) <= 255);
  FieldNum := GetFieldNumberFromName(FieldName);
  If FieldNum > 0 Then Begin
     PutFieldByNumber(FieldNum , Data ) ;
  end else begin
     ErreurGlobale(17 , 0 ) ;
  end;
end;

//=========================================================================
//****  MISES A JOUR de CHAMPS suivant leur TYPE                       ****
//****  *CHABANT*   V:1.20 01/02/2003                                  ****
//=========================================================================
//*:ออออออออออออออออออออออออออ V1.20 ออออออออออออออออออออออออออออออออ

// Mise เ Jour s้curis้e เ partir d'une donn้e saisie par l'usager
// dans un composant du genre TEDIT  ( Tedit1.Text )
// rbw begin change
//Function TXBase.UpdFieldX(FieldName: WString ; TEXT : WString ; COPT : char ) : boolean ;
Function TXBase.UpdFieldX(FieldName: WString ; TEXT : AnsiString ; COPT : AnsiChar ) : boolean ;
// rbw end change
Var
  AFLD   : DWORD   ; { adresse du Champ dans le record }
  XNUM   : integer ;
  XLEN   : byte    ;
  XDEC   : byte    ;
// rbw begin change
//  CTYP   : char    ;
//  C1     : char    ;
  CTYP   : AnsiChar    ;
  C1     : AnsiChar    ;
// rbw end change
  W_FIELD: TFieldStruct ;
Begin
  Assert(Length(TEXT) <= 255);
  XNUM := GetFieldNumberFromName(FieldName);
  If XNum = 0 Then Begin
     // ErreurGlobale(17 , 0 ) ;
     Result := false ;
     exit ;
  end;
  W_FIELD := FieldStruct[XNUM] ;
  CTYP    := W_FIELD.FieldType   ;  // voir GETFIELDTYPECHAR( N )
  XLEN    := W_FIELD.FieldLength ;  // voir GETFIELDSIZE( N )
  XDEC    := W_FIELD.Decimals    ;  // voir GETFIELDDECIMALS( N )
  AFLD    := W_FIELD.Address     ;

  case CTYP of
       'C' : begin
             if (COPT >= 'A') and (TEXT[1] = SPACE) then
			TEXT := AnsiString(trimleft(string(TEXT))) ; // suppression des espaces de gauche
             if COPT =  'U' then
			TEXT := AnsiString(UpperCase(string(TEXT))) ; // Mise en Majuscule
             if COPT =  'L' then
			TEXT := AnsiString(LowerCase(string(TEXT))) ; // Mise en Minuscule
             if length(TEXT) > XLEN  then
                  SetLength(TEXT , XLEN)         // troncature เ droite
             else begin
                  if length(TEXT) < XLEN then
                        TEXT := PadRight(TEXT, XLEN) ; // ajuste exactement
             end ;
             end ;
       'D' : begin
             TEXT := DBFFormatDate(TEXT , COPT) ;
             end ;
       'N' : begin
             TEXT := DBFFormatNumeric(TEXT , XLEN , XDEC) ;
             end ;
       'L' : begin
             C1 := Upcase( TEXT[1] ) ;
             if pos(string(C1) , 'NF0') >= 1 then
                      C1 := 'F'
                else  C1 := 'T' ;
                // FALSE = Not , Non , False , 0
                // TRUE  = tout le reste : Yes , Oui , 1 , space , etc ...
             TEXT := C1 ;
             end ;
       'M' : begin
             // en principe un pointeur en 10 chiffres ASCII
             TEXT := PadLeft(TEXT , 10) ;
             end ;
  end ;
  //..... ecrire le TEXT  dans le Champ ...
  Move( TEXT[1] , RecordBuffer[AFLD] , XLEN ) ;
  FModified := True;
  Result := true ;
end ;


(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Procedure TXBase.UpdFieldStr(FieldName, Data : AnsiString);
Begin
  Data := Copy(Data, 1, 255);
     PutFieldByName(FieldName, Data );
end ;

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Procedure TXBase.UpdFieldInt(FieldName : AnsiString ; Data: Integer );
Var TmpS : AnsiString ;
Begin
     TmpS := AnsiString(IntToStr( Data ) );
     PutFieldByName(FieldName, TmpS );
end ;

//:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ
// 29/12/2002  rajout d'une Variante UpdFieldNum5D ( 5 dcimales ) <<V211A>>
Procedure TXBase.UpdFieldNum5D(FieldName : WString ; Data: Extended ) ;
Var TmpS : String ;
    SaveDecSep : char ;
Begin
  {$IFDEF Delphi_2009_UP}
  SaveDecSep := FormatSettings.DecimalSeparator ;
  FormatSettings.DecimalSeparator := POINT ;  //  '.' K_POINT
  {$ELSE}
  SaveDecSep := DecimalSeparator ;
  DecimalSeparator := POINT ;  //  '.' K_POINT
  {$ENDIF}
  // .... pour les champs a CINQ DECIMALES exactement ........ !!!!!
  try
      TmpS := FloatToStrF( Data , ffFixed , 18 , 5 ) ;

  except
      TmpS := '0.00000' ;
  end ;
  Assert(Length(TmpS) <= 23);
  PutFieldByName(FieldName, AnsiString(TmpS) );
  {$IFDEF Delphi_2009_UP}
  FormatSettings.DecimalSeparator := SaveDecSep ;
  {$ELSE}
  DecimalSeparator := SaveDecSep ;
  {$ENDIF}
end ;

//:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ
// 29/12/2002  rajout d'une Variante UpdFieldNum2D ( 2 dcimales ) <<V1.10>>
Procedure TXBase.UpdFieldNum2D(FieldName : WString ; Data: Extended ) ;
Var TmpS : String ;
    SaveDecSep : char ;
Begin
  {$IFDEF Delphi_2009_UP}
  SaveDecSep := FormatSettings.DecimalSeparator ;
  FormatSettings.DecimalSeparator := POINT ;  //  '.' K_POINT
  {$ELSE}
  SaveDecSep := DecimalSeparator ;
  DecimalSeparator := POINT ;  //  '.' K_POINT
  {$ENDIF}
  // .... pour les champs a DEUX DECIMALES exactement ........ !!!!!
  try
      TmpS := FloatToStrF( Data , ffFixed , 18 , 2 ) ;
  except
      TmpS := '0.00' ;
  end ;
  Assert(Length(TmpS) <= 23);
  PutFieldByName(FieldName, AnsiString(TmpS) );
  {$IFDEF Delphi_2009_UP}
  FormatSettings.DecimalSeparator := SaveDecSep ;
  {$ELSE}
  DecimalSeparator := SaveDecSep ;
  {$ENDIF}
end ;

//:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ
Procedure TXBase.UpdFieldNum(FieldName : WString ; Data: Extended );
Var TmpS       : String ;
    SaveDecSep : char     ;
    FieldNum   : Integer  ;
    XDEC       : SmallInt ;
    Precision: Integer;
// RBW begin change
    LocalFieldSize: integer;
// RBW end change
Begin
   //....... Indispensable de controler le NOMBRE DE DECIMALES .......
   FieldNum := GetFieldNumberFromName(FieldName);
   If FieldNum > 0 then
   begin
    XDEC := FieldStruct[FieldNum].Decimals;
// RBW end change
    LocalFieldSize := GetFieldSize(FieldNum);
// RBW end change
   end
   else
   begin
     XDEC := 2 ;
// RBW end change
     LocalFieldSize := 10;
// RBW end change
   end;
   //...
  {$IFDEF Delphi_2009_UP}
   SaveDecSep := FormatSettings.DecimalSeparator ;
   FormatSettings.DecimalSeparator := POINT ; // '.' K_POINT
  {$ELSE}
   SaveDecSep := DecimalSeparator ;
   DecimalSeparator := POINT ; // '.' K_POINT
  {$ENDIF}
   // maxi 18 positions
// RBW begin change
//   try
//       TmpS := FloatToStrF( Data , ffFixed , 18 , XDEC ) ;
//   except
//       TmpS := '0.00' ;
//   end ;
// RBW begin change
   Precision := 18;
   repeat
     try
       if (Data = 10) or (Abs(Data) > 1e-10) then
       begin
         TmpS := FloatToStrF( Data , ffFixed , 18 , XDEC ) ;
         Dec(XDEC)
       end
       else
       begin
         TmpS := FloatToStrF( Data , ffGeneral , Precision , XDEC ) ;
         Dec(Precision)
       end;
     except
         TmpS := '0.00' ;
     end ;
   until (Length(TmpS) <= LocalFieldSize);
   Assert(Length(TmpS) <= 23);
//   repeat
//     try
//         TmpS := FloatToStrF( Data , ffFixed , 17 , XDEC-1 ) ;
//     except
//         TmpS := '0.00' ;
//     end ;
//     TmpS := TmpS + AnsiChar(0);
//     Dec(XDEC)
//   until (Length(TmpS) <= LocalFieldSize);
// RBW end change
   PutFieldByNumber(FieldNum , AnsiString(TmpS) ) ; // <<V1.10>>
  {$IFDEF Delphi_2009_UP}
   FormatSettings.DecimalSeparator := SaveDecSep ;
  {$ELSE}
   DecimalSeparator := SaveDecSep ;
  {$ENDIF}
end ;

//======================================================================
//********************* V:1.20 *****************************************
//** Fonction utilitaire de formatage d'un nombre en ascii
Function  TXBase.DBFFormatNumeric( TEXT : AnsiString ; XLEN , XDEC : byte ) : AnsiString ;
var  Z1 , Z2 , Z3 : String[23] ;
// rbw begin change
//     S1 , C3 , DecSep  : char ;
     S1 , C3 , DecSep  : AnsiChar ;
// rbw end change
     X1 , J1 : SmallInt ;
begin
  Assert(Length(TEXT) <= 255);
     //... controle des donn้es ...
     if XLEN < 01 then XLEN := 10 ;
     if XLEN > 23 then XLEN := 23 ;
     if XDEC > XLEN - 1 then XDEC := XLEN - 1 ;
     if length(TEXT) = 0 then TEXT := ZERO ;
     //... recherche d'un s้parateur d้cimal ...
// rbw begin change
//     DecSep := DecimalSeparator ;
     {$IFDEF Delphi_2009_UP}
     Assert(FormatSettings.DecimalSeparator <= #$00FF);
     DecSep := AnsiChar(FormatSettings.DecimalSeparator);
     {$ELSE}
     Assert(DecimalSeparator <= #$00FF);
     DecSep := AnsiChar(DecimalSeparator);
     {$ENDIF}
// rbw end change
     X1 := pos(DecSep , TEXT ) ;
     if X1 = 0 then begin
        if DecSep = POINT then DecSep := VIRGULE
                          else DecSep := POINT ;
        X1 := pos(DecSep , TEXT ) ;
     end ;
     //... s้paration des 2 parties ...
     C3 := SPACE ;
     Z1 := ZERO ; Z2 := ZERO ;
     J1 := length(TEXT) ;
     if X1 = 0 then begin
         if J1 >= 1 then
              Z1 := TEXT ;
     end else begin
         if X1 >= 2 then
              Z1 := copy(TEXT , 1 , X1-1) ;
         if X1 < J1 then
              Z2 := copy(TEXT , X1+1 , J1-X1) ;
     end ;
     //... formattage correct avec que des chiffres ...
     Z3 := StringOfChar(SPACE , XLEN) ;
     //... transfert de la partie d้cimale  ..  de gauche เ droite ...
     if XDEC >= 1 then begin
         J1 := XLEN - XDEC ;
         Z3[J1] := POINT ;
         for X1 := 1 to length(Z2) do begin
             S1 := Z2[X1] ;
             if (S1 >= ZERO) and (S1 <= '9') then begin
                 inc(J1) ;
                 if J1 <= XLEN then
                          Z3[J1] := S1 ;
             end else begin
                 if (S1 = '+') or (S1 = '-') then
                          C3 := S1 ;
             end ;
         end ;
     end ;
     //... transfert de la partie entiere  .. de droite เ gauche ...
     if XDEC >= 1 then J1 := XLEN - XDEC
                  else J1 := XLEN + 1 ;
     for X1 := length(Z1) downto 1 do begin
             S1 := Z1[X1] ;
             if (S1 >= ZERO) and (S1 <= '9') then begin
                 dec(J1) ;
                 if J1 >= 1 then
                          Z3[J1] := S1 ;
             end else begin
                 if (S1 = '+') or (S1 = '-') then
                          C3 := S1 ;
             end ;
     end ;
     //... signe NEGATIF ...
     if (XLEN >= 2) and ((XLEN - XDEC) >= 2) and (C3 = '-') then
           Z3[1] := '-' ;
  Assert(Length(Result) <= 255);
end ;

//********************* V:1.20 *****************************************
//** Fonction utilitaire de formatage d'une date en 'aaaammjj'
// rbw begin change
//Function  TXBase.DBFFormatDate( TEXT : WString ; COPT : Char ) : WString ;
Function  TXBase.DBFFormatDate( TEXT : AnsiString ; COPT : AnsiChar ) : AnsiString ;
// rbw end change
// rbw begin change
//var W_CTYP , C1  : Char ;
var W_CTYP , C1  : AnsiChar ;
// rbw end change
    XS1 , J0 , J1 , J2 , J3 , DD1 , MM1 , AA1 : SmallInt ;
    WDAT, WD1, WM1, WA1 : String[15] ;
    V1 : boolean ;
begin
  Assert(Length(TEXT) <= 255);
    { ici , le s้parateur peut etre quelconque , TOUT caract่re
      qui n'est pas un CHIFFRE est consid้r้ comme ้tant un
      SEPARATEUR : cela peut etre les caracteres ordinaires tels
      que  / - . ou BLANC , mais aussi tous les autres par
      exemple  :  ;  ,  _  \  #  ~  !  *   etc .... }
// rbw begin change
//    W_CTYP := upcase( ShortDateFormat[1] ) ;
    {$IFDEF Delphi_2009_UP}
    Assert(FormatSettings.ShortDateFormat[1] <= #$00FF);
    W_CTYP := upcase( AnsiChar(FormatSettings.ShortDateFormat[1]) ) ;
    {$ELSE}
    Assert(ShortDateFormat[1] <= #$00FF);
    W_CTYP := upcase( AnsiChar(ShortDateFormat[1]) ) ;
    {$ENDIF}
// rbw end change
       { 'D' pour DAY   ==> FRANCE et EUROPE }
       { 'M' pour MONTH ==> USA              }
       { 'Y' pour YEAR  ==> ANSI , JAPON ?   }
    { valeurs par defaut }
    if pos(string(W_CTYP) , 'DMY') = 0 then  W_CTYP := 'D' ;
    (*-- Extraire les 3 zones de la date            --*)
    J0  := 1 ; J1 := 0 ; J2 := 0 ; J3 := 0 ;
    V1  := false ;
    XS1 := 1 ;
    repeat
        C1 := TEXT[XS1] ;
        if (C1 < ZERO) or (C1 > '9') then begin
             //.... changer de zone ....
             if V1 then
                   inc(J0) ;
             V1 := false ;
             //.... sauter tous les s้parateurs jusqu'au prochain chiffre ...
             // continue ;  // loop repeat
        end else begin {traiter chaque caractere decimal }
             case J0 of
                  1 : J1 := (10 * J1) + ord(C1) - 48 ;
                  2 : J2 := (10 * J2) + ord(C1) - 48 ;
                  3 : J3 := (10 * J3) + ord(C1) - 48 ;
             end ;
             V1 := true ;
        end ;
        inc(XS1) ;
    until XS1 > length(TEXT) ;
    (*-- Suivant le Format , extraire JOUR, MOIS, ANNEE --*)
    case W_CTYP of
       'D' : begin DD1 := J1 ; MM1 := J2 ; AA1 := J3 ; end ;
       'M' : begin DD1 := J2 ; MM1 := J1 ; AA1 := J3 ; end ;
       'Y' : begin DD1 := J3 ; MM1 := J2 ; AA1 := J1 ; end ;
       else  begin DD1 := J1 ; MM1 := J2 ; AA1 := J3 ; end ;
    end ;
    (*-- Controler la Validite des    JOUR, MOIS, ANNEE --*)
    if DD1 < 01    then  DD1 := 01 ;
    if DD1 > 31    then  DD1 := 31 ;
    if MM1 < 01    then  MM1 := 01 ;
    if MM1 > 12    then  MM1 := 12 ;
    if AA1 < 00    then  AA1 := 00 ;
    if AA1 > 9999  then  AA1 := 9999 ;
   //... cas de l'ann้e sur 2 chiffres ...
   if AA1 < 100 then begin
          case COPT of
               'A' :  AA1 := 1900 + AA1 ;
               'B' :  AA1 := 2000 + AA1 ;
               else   if AA1 < 50 then
                           AA1 := 2000 + AA1
                      else AA1 := 1900 + AA1 ;
          end ;
    end ;
    (*-- fabriquer la date format DBASE3 : AAAAMMJJ     --*)
    str( DD1:2 , WD1 ) ;
    str( MM1:2 , WM1 ) ;
    str( AA1:4 , WA1 ) ;
    WDAT := WA1 + WM1 + WD1 ;
    (*-- remplacer les blancs par des ZERO              --*)
    for XS1 := 1 to 8 do begin
        if WDAT[XS1] = SPACE  then  WDAT[XS1] := ZERO ;
    end ;
    Result := WDAT ;
  Assert(Length(Result) <= 255);
end ;
//======================================================================

(*:ออออออออออออออออออออออออออ +CHABANT+ ออออออออออออออออออออออออออออออออ *)
Procedure TXBase.UpdFieldDat(FieldName: WString ; DATA : WString ) ;
{ utilise les variables generales de formatage
     ShortDateFormat
  pour la FRANCE cela est normalement dd/mm/yyyy  et  /
}
var
    WDAT : String[15] ;
Begin
    WDAT := DBFFormatDate(DATA , 'X') ;
    PutFieldByName(FieldName, WDAT );
end ;

(*ฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑฑ*)
(*:ออ Routines de DEPLACEMENT dans le DBF    อออออออออออออออออออออออออออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.GotoBOF;
var  X1 : Cardinal ;
Begin
     X1 := 1 ;
     GotoRecord( X1 ) ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.GotoEOF;
var  X1 : Cardinal ;
Begin
     X1 := Header.NumRecords ;
     GotoRecord( X1 ) ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.GotoNext;
var  X1 : Cardinal ;
Begin
     X1 := FCurrentRecord + 1 ;
     GotoRecord( X1 ) ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.GotoPrev;
var  X1 : Cardinal ;
Begin
     X1 := FCurrentRecord - 1 ;
     GotoRecord( X1 ) ;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Procedure TXBase.GotoRecord(Recnum: Cardinal);
Begin
  if FActive Then Begin
      if FAutoUpDate and FModified then
              PostChanges  ;    (* ++ CHABANT  *)
      if CanNavigate then begin
              FCurrentRecord := RecNum ;
              ReadARecord;
      end;
  end else begin
      ErreurGlobale(50 , 0 ) ;
  end;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
(*:ออ Routines speciales pour executer des Procedures EXTERIEURES     ออ *)
(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.CanNavigate : Boolean;
{ le but est de permettre l'execution d'une procedure speciale FOnChanging
  qui verifiera si l'on peut NAVIGEUR dans le Fichier  }
Var
  Allowed: Boolean;
Begin
  Allowed := True;
  If Assigned(FOnChanging) Then Begin
     FOnChanging(Self, Allowed);
  End;
  Result := Allowed;
End;

(*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *)
Function TXBase.CanDelete: Boolean;
{ le but est de permettre l'execution d'une procedure speciale FOnDeleting
  qui verifiera si l'on peut supprimer le record }
Var
  Allowed: Boolean;
Begin
  Allowed := True;
  If Assigned(FOnDeleting) Then Begin
     FOnDeleting(Self, Allowed);
  End;
  Result := Allowed;
End;

              //***********************************************
              //***********************************************
              //*** ajouts version <<V1.10>> du 06/01/2003   **
              //***********************************************
              //***********************************************

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//** Equivalent de la commande "PACK" de DBASE3                          **
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
// BIDON : un parametre fictif , sans utilit้
// effectue l'้quivalent d'une copie du fichier DBF ,
//    en y ้liminant physiquement les records flagg้s "DELETED"
// mais nous faisons ceci "en place" , en d้placant les records conserv้s
// fournit le NB de Records du fichier apr่s PACK
Function TxBase.DBFPack(BIDON : byte    ) : integer ;
var
  X1 , X2 , X3REC , X4MAX , X5ADR , X6REC , X7ADR : Integer ;
  VDEL : boolean ;
begin
  VDEL := false ; // indicateurs DELETED trouv้s
  X1 := Header.HeaderLen  ;  // Multiple de 32 + 1
  X2 := Header.RecordLen  ;
  // X3REC := 1 ;  // d้marre avec le 1er RECORD // FCurrentRecord   ;
  X4MAX := Header.NumRecords ;
  X5ADR := X1    ; // position de d้part
  X6REC := 0     ; // NB records conserv้s ou r้้crits
  X7ADR := X1    ; // position de d้part
  X3REC := 1 ;
  while X3REC <= X4MAX do begin
      //.... Lecture RECORD ...
      Try
        // X5ADR := X1 + (X2 * (X3REC - 1)) ;
        DBFile.Seek( X5ADR , soFromBeginning);
        DBFile.ReadBuffer(RecordBuffer, X2);
      Except
        Break ;
      End;
      //.... Ecriture RECORD ...
      if RecordBuffer[1] <> DELETE_FLAG then begin
          if VDEL then begin
               // il faut effectivement d้placer le Record
               DBFile.Seek( X7ADR , soFromBeginning);
               DBFile.WriteBuffer(RecordBuffer, X2);
          end ;
          inc(X7ADR , X2) ;  // position de Copie suivante
          inc(X6REC) ;
      end else begin
          VDEL := true ; // des records DELETED ont ้t้ trouv้s
      end ;
      //....
      inc(X5ADR , X2) ;  // position du Record Suivant
      inc(X3REC) ;
  end ; // while
  dec(X3REC) ;
  if X3REC < X4MAX then begin
      ErreurGlobale(54 , X3REC ) ;
  end else begin
      //.... Mise a Jour du Header ....
      Header.NumRecords := X6REC ;
      DBFSetDateAMJ( Header.LastUpdated ) ;
      DbFile.Seek(0, soFromBeginning)     ;  //MAJ-HEADER
      DBFile.WriteBuffer(Header, KSIZHDR) ;  //...
  end ;
  if VDEL then
       FDBFUpdated := true ; //<<V110>>
  FCurrentRecord := 0 ;
  Result := X3REC ;
end ;

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//** Equivalent de la commande "COPY TO ... for Recno() <= X" de DBASE3  **
//** ou aussi  "GO TOP ; COPY TO ... NEXT X record"
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
// CPTREC = nombre de records เ conserver ; ZERO signifie TOUS
Function TXBase.DBFCopyFile(DBFName : String ; CPTREC : integer ;
                        CopyDELETED : boolean ) : boolean ;
var
    XLUS , XCPY , XREC , XMAX : cardinal    ;
    X1 , X2  : Integer ;
    FDBF : TFileStream ;
// rbw begin change
//    W_BUFFER : Array[1..KMAXSIZ] Of Char ; (* zone de Lecture/Ecriture *)
    W_BUFFER : Array[1..KMAXSIZ] Of AnsiChar ; (* zone de Lecture/Ecriture *)
// rbw end change
    W_HEADER : THeader absolute W_BUFFER ;
// rbw begin change
//    C1   : char ;
    C1   : AnsiChar ;
// rbw end change
begin
    Result := true ;
    if not FActive then begin
          ErreurGlobale(50 , 0) ;
          exit ;
    end ;
    X1   := Header.HeaderLen  ;
    X2   := Header.RecordLen  ;
    XREC := Header.NumRecords ;
    // X3 := FCurrentRecord   ;
    if CPTREC <= 0    then XMAX := high(cardinal)
                      else XMAX := CPTREC ;
    if XMAX    > XREC then XMAX := XREC ; // nombre de Records เ copier
    FDBF := TFileStream.Create(DBFName , fmCreate );
    // recopie du HEADER
    DBFile.Seek(0, soFromBeginning);
    DBFile.ReadBuffer(W_BUFFER , X1 ) ;
    W_HEADER.NumRecords := XMAX ;
    FDBF.WriteBuffer( W_BUFFER , X1 ) ;
    // recopie des X records
    XCPY := 0 ; XLUS := 0 ;
    while (XCPY < XMAX) and (XLUS < XREC) do begin
          DBFile.ReadBuffer(W_BUFFER , X2 ) ;
          inc(XLUS) ;
          C1 := W_BUFFER[1]  ;  // byte FLAG de suppression
          if CopyDELETED or ( C1 <> DELETE_FLAG) then begin
               FDBF.WriteBuffer( W_BUFFER , X2 ) ;
               inc(XCPY) ;
          end ;
    end ;
    FDBF.Free ;
end ;

(* const fmOpenRead       = $0000;
   const fmOpenWrite      = $0001;
   const fmOpenReadWrite  = $0002;
   const fmShareCompat    = $0000;
   const fmShareExclusive = $0010;
   const fmShareDenyWrite = $0020;
   const fmShareDenyRead  = $0030;
   const fmShareDenyNone  = $0040;
*)

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//***  Equivalent de la commande "ZAP" de DBASE3                         **
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
// CPTREC = nombre de records เ conserver, si besoin , en principe ZERO
Function TXBase.DBFZap(CPTREC : integer ) : boolean ;
var
// rbw begin change
//    FDBF : File of Char ;
//    C1   : char ;
    FDBF : File of AnsiChar ;
    C1   : AnsiChar ;
// rbw end change
    J1 , X2 , X3 , XMAX : integer ;
begin
     //... controle fichier d้ja ouvert  ...
     Result := false ;
     if not FActive then begin
           ErreurGlobale(50 , 0) ;
           exit ;
     end ;
     X2 := Header.HeaderLen  ; // taille du Header complet
     X3 := Header.RecordLen  ; // Taille d'un record
     if CPTREC <= 0 then
           XMAX := X2
     else  XMAX := X2 + ( CPTREC * X3 ) ;
     //... fermeture et parcours pour TRONCATURE ....
     SetActive(false) ;
     AssignFile(FDBF , FFileName ) ;
     Reset(FDBF) ;
     for J1 := 1 to XMAX do begin
         if system.eof(FDBF) then break  ;
         read(FDBF , C1 ) ;
     end ;
     truncate(FDBF) ;
     CloseFile(FDBF) ;
     //... r้ouverture du fichier ZAPP้
     SetActive(true) ;
     Result := true ;
     FDBFUpdated := true ; //<<V110>>
end ;

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//*** transfert complet du record courant dans un BUFFER                ***
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
// la valeur CPT indique la taille du BUFFER , et donc le nombre
// maximum de bytes เ transf้rer ;
//    si la taille X du record est inf้rieure , seuls X bytes sont copi้s
//    si X est sup้rieur เ CPT , seuls les CPT premiers bytes sont copi้s
// rbw begin change
//Function TXBase.DBFReadRecordToBuffer(var BUFFER : array of char ;
//                                CPT : integer ) : integer ;
Function TXBase.DBFReadRecordToBuffer(var BUFFER : array of AnsiChar ;
                                CPT : integer ) : integer ;
// rbw end change
var  X1 , X2 : integer ;
begin
     X1 := CPT ;
     X2 := Header.RecordLen ;
     if X1 > X2      then  X1 := X2 ;
     if X1 > KMAXSIZ then  X1 := KMAXSIZ ;
     move(RecordBuffer , BUFFER , X1 ) ;
     Result := X1 ;
end ;
// RecordBuffer  : Array[1..KMAXSIZ] Of Char ; (* zone de Lecture/Ecriture *)


//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//***  Equivalent de la commande "Copy Structure " de DBASE3             **
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
Function TXBase.DBFCopyStruct(DBFName : String ) : boolean ;
var
    my_DBFFields : TStringList ;
    V1 : boolean ;
begin
    my_DBFFields := TStringList.Create ;
    V1 := DBFExtractStruct(my_DBFFields , false) ;
    if V1 then begin
          V1 := DBFCreate(DBFName , my_DBFFields ) ;
    end ;
    Result := V1 ;
end ;

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//****  Extraction de la STRUCTURE d'un fichier DBF ouvert               **
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//* DBFFields = liste des champs et de leur description
//*      doit avoir ้t้ cr้้e par l'appellant
//*      format de chaque chaine NOM-DE-CHAMP=TYPE LONGUEUR , DECIMALES
//*      cela est identique เ l'argument DBFFields de CreateDBF
//*      mais ici, je formatte en longueur fixe chaque ้l้ment
//*            NOM sur 10 = TYPE sur 1 LONGUEUR sur 3 , DECIMALES sur 3
//*      si VFIX = true , LONGUEUR Fixe , sinon compact้
Function TXBase.DBFExtractStruct(var DBFFields : TStringList ;
                VFIX : boolean ) : boolean ;
const
   SPACE10X = '          ' ;  // 10 espaces
var
   J1 , X1 : smallint ;
   W1 , W2 , W3 : AnsiString ;
begin
   Result := false ;
   //.... fichier NON ouvert
   if Factive = false then begin
         exit ;
   end ;
   //.... LISTE non existante
   if DBFFields = nil then begin
         exit ;
   end ;
   Result := true ;
   DBFFields.Clear ;
   for J1 := 1 to NumFields do begin
       with FieldStruct[J1] do begin
            SetLength(W1 , KSIZNAM) ;
            move(FieldName[1] , W1[1] , KSIZNAM*SizeOf(AnsiChar)) ;
            X1 := pos( #00 , string(W1) ) ;
            if X1 <= 1 then X1 := 10
                       else dec(X1) ;
            SetLength(W1 , X1) ;
            if VFIX then begin
                 W1 := copy(W1 + SPACE10X , 1 , 10 ) ;
                 system.str(FieldLength:3 , W2) ;
                 system.str(Decimals:3 , W3) ;
                 W2 := W2 + VIRGULE + W3 ;
                 for X1 := 1 to length(W2) do begin
                     if W2[X1] = SPACE then  W2[X1] := ZERO ;
                 end ;
                 W1 := W1 + '=' + FieldType + W2 ;
            end else begin
                 W1 := W1 + '=' + FieldType +
                       AnsiString(IntToStr(FieldLength)) + VIRGULE +
                       AnsiString(IntToStr(Decimals)) ;
            end ;
            DBFFields.Add(string(W1)) ;
       end ;
   end ;
end ;

//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//****  CREATION d'un fichier DBF avec la Structure d้sir้e              **
//*:อออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ *
//* DBFNAme = Nom du Fichier เ cr้er ; celui-ci ne doit pas exister
//* DBFFields = liste des champs et de leur description
//*      Chaque Chaine de cette Liste  doit avoir la forme
//*      NOM-DE-CHAMP=TYPE LONGUEUR , DECIMALES
//*      exemples:  NUM_TELEPH=C28
//*                 TAUX_COT=N6,2
//*      nota: ne pas mettre d'espaces entre les ้l้ments

Function TXBase.DBFCreate(DBFName : String ; DBFFields :TStringList) : boolean ;
const
     _KC255X07 = #255#255#255#255#255#255#255 ;
     _KC000X12 = #000#000#000#000#000#000#000#000#000#000#000#000 ;

var  J1 , X1 , XFLD1 {, XLEN1 , XDEC1} : smallint ;
     XLEN1, XDEC1: DWORD;
     W1, W2_LEN, W2_DEC  : String ;
     W2_NAME, W2_VALUE : AnsiString;
// rbw begin change
//     W2_TYPE : char ;
//     S1      : array[0..7] of char ;  // AnsiString ;
     W2_TYPE : AnsiChar ;
     S1      : array[0..7] of AnsiChar ;  // AnsiString ;
// rbw end change
     W9_RECLEN , W9_HDRLEN : integer ;
     W9_ADR          : dword ;
     my_Header       : THeader      ;
     my_FieldStruct  : TFieldStruct ;
     my_DBFile       : TFileStream  ;
     my_FFileName    : String      ;
  ALength: integer;
begin
   Result := true ;
   // The first character of each record is used to mark whether the
   // record is deleted or not so 1 extra character needs to be included
   // in each record to account for that.
   W9_RECLEN := 1 ;
   W9_ADR    := 1 ;
   XFLD1 := DBFFields.Count ;  // nombre de Chaines, donc de champs
   if XFLD1 > KMAXFLD then
            XFLD1 := KMAXFLD ;
   //......... Initialisation pr้alable .......
   my_FFileName := StandardDBFName( DBFName ) ;
   my_DBFile := nil;
   try
   //.... ECRITURE et CREATION du Fichier sur Disque ....
   try
      my_DBFile := TFileStream.Create( my_FFileName , fmCreate ) ;
      DBFSetDateAMJ( my_Header.LastUpdated ) ;
      my_DbFile.Seek(0, soFromBeginning)     ;  //MAJ-HEADER
      my_DBFile.WriteBuffer(my_Header, KSIZHDR) ;  //...
          // pseudo-header provisoire
   except
      Result := false ;
      exit ;
   end ;
   //.........
   for J1 := 0 to XFLD1 - 1  do begin  // num้ros depuis ZERO
       W1 := DBFFields.Strings[J1] ;
       //.... analyse de la ligne ....
       X1 := pos('=' , W1) ;
       if X1 > 0 then begin
           W2_NAME  := AnsiString(copy(W1,1,X1 - 1)) ; // DBFFields.Names[J1]
           W2_VALUE := AnsiString(copy(W1,X1+1,255)) ; // DBFFields.Values[J1]
       end else begin
           W2_NAME  := 'FIELD' + AnsiString(IntToStr(J1)) ;
           W2_VALUE := AnsiString(W1) ;
       end ;
       W2_NAME  := AnsiString(UpperCase( Trim( string(W2_NAME) ) )) ;
       if length(W2_NAME) > 10 then
                 SetLength(W2_NAME , 10) ;
       W2_VALUE := AnsiString(Trim(string(W2_VALUE)))+SPACE  ;
       W2_TYPE  := upcase( W2_VALUE[1] ) ;
       if pos( string(W2_TYPE) , 'CNDLM' ) = 0 then
               W2_TYPE := 'C'
       else    W2_VALUE := AnsiString(copy(string(W2_VALUE), 2, 255)) ;
       X1 := pos(VIRGULE , string(W2_VALUE)) ;
       if X1 > 0 then begin
           W2_LEN := trim(copy(string(W2_VALUE),1,X1 - 1)) ;
           W2_DEC := trim(copy(string(W2_VALUE),X1+1,255)) ;
       end else begin
           W2_LEN := trim(string(W2_VALUE)) ;
           W2_DEC := ZERO ;
       end ;
       if W2_LEN = VIDE then W2_LEN := '10' ;
       if W2_DEC = VIDE then W2_DEC := ZERO  ;
       XLEN1 := StrToIntDef( W2_LEN , 8) ;
       XDEC1 := StrToIntDef( W2_DEC , 0) ;
       if XLEN1 <   1 then XLEN1 :=   1 ;
       if XLEN1 > 255 then XLEN1 := 255 ;
//       if XDEC1 <   0 then XDEC1 :=   0 ;
       case W2_TYPE of
            'C' : XDEC1 := 0 ;
            'D' : begin
                  XDEC1 := 0 ; XLEN1 := 8 ;
                  end ;
            'M' : begin
                  XDEC1 := 0 ; XLEN1 := 10 ;
                  end ;
            'L' : begin
                  XDEC1 := 0 ; XLEN1 :=  1 ;
                  end ;
            'N' : begin
                  if XLEN1 > 18 then XLEN1 := 18 ;  // maximum Dbase3
                  if XLEN1 <  3 then XDEC1 :=  0 ;  // decimales il faut 3 car au moins
                  if (XDEC1 > 0) and (XDEC1 > XLEN1 - 2) then
                      XDEC1 := XLEN1 - 2 ; // format minimum "9.9"
                  end ;
       end ;
       //.... Construction du FIELD ....
       with my_FieldStruct do begin
            FieldType   := W2_TYPE ;
            Address     := W9_ADR  ;
            FieldLength := XLEN1   ;
            Decimals    := XDEC1   ;
            Filler1  := 00 ; Filler2  := 00 ; Filler3  := 00 ; Filler4 := 00 ;
            WorkArea := 00 ; SetFields:= 00 ; IndexFlag:= 00 ;
            Reserved3:= _KC255X07 ;
            W2_NAME  := W2_NAME + _KC000X12 ;
            move(W2_NAME[1] , FieldName[1] , KSIZNAM * SizeOf(AnsiChar)) ;
       end ;
       //..... ECRITURE du DESCRIPTIF de Champ ...
       try
          my_DBFile.WriteBuffer( my_FieldStruct , KSIZFLD )  ; // 32
       except
          Result := false ;
       end ;
       //.....
       ALength := XLEN1;
       W9_RECLEN := W9_RECLEN + ALength ;
//       W9_RECLEN := W9_RECLEN + XLEN1 ;
       W9_ADR    := W9_ADR    + XLEN1 ;  // pour le suivant
   end ;
   W9_HDRLEN := KSIZHDR + (XFLD1 * KSIZFLD) + 1 ;

   // rbw begin change
   if W9_RECLEN > High(Word) then
   begin
     raise EXBaseException.Create(Format(StrSizeOfRecordsToo,
       [my_FFileName]));
   end;
   // rbw end change
                // en principe = 32 + 32 * XFLD1 + 1 ( because byte $0D
   //.... Construction du HEADER du Fichier DBF ....
   with my_Header do begin
        VersionNumber := dB3 ;
        DBFSetDateAMJ( my_Header.LastUpdated ) ;
        NumRecords     := 0  ;
        Reserved := 0 ; IncTrans := 0 ; EncFlag    := 0 ; MdxFlag := 0 ;
        Filler1  := 0 ; Filler2  := 0 ; LangDriver := 0 ;
        Reserved2:= _KC000X12 ;
        HeaderLen      := W9_HDRLEN ;
        RecordLen      := W9_RECLEN ;
   end ;
   try
      //.... ECRITURES FINALES ....
      S1 := #13#13 ;
      my_DBFile.WriteBuffer( S1 , 01 )  ; // Terminateur standard DBase3 (CR)
      //.... ECRITURES du HEADER ....
      my_DBFile.Seek(0 , soFromBeginning)          ; //MAJ-HEADER
      my_DBFile.WriteBuffer( my_Header , KSIZHDR ) ; //.... 32
   except
      Result := false ;
   end ;
   //.... CLOTURE ....
   finally
     my_DBFile.Free ; // CLOSE
   end;
end ;


(* ฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒฒ *)

(* ***********  A ETUDIER pour Gestion des MEMOS *********
     If GetFieldType(FieldNum) <> xbfMemo Then Begin
     End Else Begin
        XA1 := StrToIntDef(TmpS, 0) ;
        If XA1 > 0 Then Begin
           TmpS := GetMemoData( XA1 ) ;
        End ;
     End;
   ***********
     If GetFieldType(FieldNum) <> xbfMemo Then Begin
          TmpS := TrimRight(TmpS);
          end
     else begin
          XMEM := StrToIntDef(TmpS, 0) ;
          If XMEM > 0 Then Begin
              TmpS := GetMemoData( XMEM );
          End Else Begin
              TmpS := VIDE ;
          End;
     End;
   ***********
     Procedure TXBase.PutFieldByNumber(FieldNum: Integer; Data: WString);
     Begin
       If (FieldNum > 0) And (FieldNum <= NumFields) then Begin
           If FieldStruct[FieldNum].FieldType = 'M' then
                 UpdateMemoData( FieldNum, Data)
           else  UpdateFieldData(FieldNum, Data) ;
           end 
       else begin
           ErreurGlobale(15 , FieldNo ) ;
       end;
     end;
*********** *)

end.




