unit DBaseReaderUnit;

interface

uses Types;

type
  TDBase3TableFieldDescriptor = packed record
    FieldName: array[0..10] of char; // Field name in ASCII (zero-filled).
    FieldType: Char;  // Field type in ASCII (C, D, L, M, or N).
    FieldDataAdress: Pointer; // Field data address (address is set in memory; not useful on disk).
    FieldLength: byte; // Field length in binary.
    FieldCount: byte; // Field decimal count in binary.
    Reserved1: array[0..1] of byte; // Reserved for dBASE III PLUS on a LAN.
    WorkAreaID: byte;  // Work area ID.
    Reserved2: array[0..1] of byte; // Reserved for dBASE III PLUS on a LAN.
    SetFieldFlags: byte; // SET FIELDS flag.
    Reserved3: array[0..7] of byte; // Reserved bytes.
  end;

  TDBase3Header = packed record
    InfoByte: byte; // Valid dBASE III PLUS table file (03h without a memo (.DBT file; 83h with a memo).
    LastUpdateYear: byte;
    LastUpdateMonth: byte;
    LastUpdateDay: byte;
    NumberOfRecords: dword;  // Number of records in the table.
    BytesInTheHeader: word;  // Number of bytes in the header.
    BytesInTheRecord: word;  // Number of bytes in the record.
    ReservedBytes: array[0..2] of byte; // Reserved bytes.
    Reserved2: array[0..12] of char; // Reserved for dBASE III PLUS on a LAN.
    Reserved3: array[0..3] of byte;  // Reserved bytes.
    FieldDescriptors: array of TDBase3TableFieldDescriptor; // Field descriptor array (the structure of this array is shown above)
    OADH: byte; // 0Dh stored as the field terminator
  end;

{Table Records
=============

The records follow the header in the table file. Data records are preceded
by one byte, that is, a space (20h) if the record is not deleted, an
asterisk (2Ah) if the record is deleted. Fields are packed into records
without field separators or record terminators. The end of the file is
marked by a single byte, with the end-of-file marker, an OEM code page
character value of 26 (1Ah). You can input OEM code page data as indicated
below.

Allowable Input for dBASE Data Types
====================================

Data Type      Data Input
-------------- -----------------------------------------------------------
C (Character)  All OEM code page characters.
D (Date)       Numbers and a character to separate month, day, and year
               (stored internally as 8 digits in YYYYMMDD format).
N (Numeric)    - . 0 1 2 3 4 5 6 7 8 9
L (Logical)    ? Y y N n T t F f (? when not initialized).
M (Memo)       All OEM code page characters (stored internally as 10
               digits representing a .DBT block number).

Binary, Memo, and OLE Fields And .DBT Files
===========================================

Memo fields store data in .DBT files consisting of blocks numbered
sequentially (0, 1, 2, and so on). The size of these blocks are internally
set to 512 bytes. The first block in the .DBT file, block 0, is the .DBT
file header.

Memo field of each record in the .DBF file contains the number of the
block (in OEM code page values) where the field's data actually begins. If
a field contains no data, the .DBF file contains blanks (20h) rather than
a number.

When data is changed in a field, the block numbers may also change and the
number in the .DBF may be changed to reflect the new location.

This information is from the Using dBASE III PLUS manual, Appendix C.}

  TDBase3Record = packed record
  end;

  TDBase4TableFieldDescriptor = packed record
    FieldName: array[0..10] of char;
    FieldType: Char;
    Reserved0: array[0..3] of byte;
    FieldLength: byte;
    FieldCount: byte;
    Reserved1: array[0..1] of byte; // Reserved
    WorkAreaID: byte;
    Reserved2: array[0..9] of byte; // Reserved
    HasMDXFile: byte;
  end;

  TDBase4Header = packed record
    InfoByte: byte;  {Valid dBASE IV file; bits 0-2 indicate version
                      number, bit 3 the presence of a dBASE IV memo
                      file, bits 4-6 the presence of an SQL table, bit
                      7 the presence of any memo file (either dBASE III
                      PLUS or dBASE IV).}
    LastUpdateYear: byte;
    LastUpdateMonth: byte;
    LastUpdateDay: byte;
    NumberOfRecords: dword;
    BytesInTheHeader: word;
    BytesInTheRecord: word;
    ReservedBytes: array[0..1] of byte;
    InTransaction: byte; // Flag indicating incomplete transaction.
    EncryptionFlag: byte;
    Reserved2: array[0..11] of char; // Reserved for dBASE III PLUS on a LAN.
    HasMDXFile: byte;
    LanguageDriver: byte;
    Reserved3: array[0..1] of byte;
    FieldDescritpors: array of TDBase4TableFieldDescriptor; // 0Dh stored as the field terminator.
    OADH: byte; // 0Dh stored as the field terminator
  end;

  TDBase5DosTableFieldDescriptor = packed record
    FieldName: array[0..10] of char;
    FieldType: Char;
    Reserved0: array[0..3] of byte;
    FieldLength: byte;
    FieldCount: byte;
    Reserved1: array[0..1] of byte; // Reserved
    WorkAreaID: byte;
    Reserved2: array[0..9] of byte; // Reserved
    HasMDXFile: byte; {Production .MDX field flag; 01h if field has an index
                  tag in the production .MDX file; 00h if the field is not
                  indexed.}
  end;

  TDBase5DosHeader = packed record
    InfoByte: byte;  {Valid dBASE for Windows table file; bits 0-2 indicate
                  version number; bit 3 indicates presence of a dBASE IV
                  or dBASE for Windows memo file; bits 4-6 indicate the
                  presence of a dBASE IV SQL table; bit 7 indicates the
                  presence of any .DBT memo file (either a dBASE III PLUS
                  type or a dBASE IV or dBASE for Windows memo file).}
    LastUpdateYear: byte;
    LastUpdateMonth: byte;
    LastUpdateDay: byte;
    NumberOfRecords: dword;
    BytesInTheHeader: word;
    BytesInTheRecord: word;
    ReservedBytes: array[0..1] of byte;
    InTransaction: byte; // Flag indicating incomplete transaction.
    EncryptionFlag: byte;
    Reserved2: array[0..11] of char; // Reserved for multi-user processing.
    HasMDXFile: byte;  {Production MDX flag; 01h stored in this byte if a prod-
                  uction .MDX file exists for this table; 00h if no .MDX
                  file exists.}
    LanguageDriver: byte;
    Reserved3: array[0..1] of byte;
    FieldDescritpors: array of TDBase5DosTableFieldDescriptor; // 0Dh stored as the field terminator.
    OADH: byte; // 0Dh stored as the field terminator
  end;

implementation

end.
