unit CDISO;
interface

uses
  SysUtils, windows;
  
type
  PDirEntry = ^TDEntry;
  PFileEntry = ^TFEntry;
  TFEntry = record
    ShortName: String;
    LongName: String;
    Path: String;
    Imported: Boolean;
    Time: TDateTime;
    FileSize, FileSizeJ: Int64;
    SpaceReqOnDisc: Int64;
    Address: Integer;
    AddressJ: Integer;
    Attr: Integer;
    ResetArchiveBit: Boolean;
    DirRec: PDirEntry;
    Next: PFileEntry;
    Prev: Boolean;
    Buffer: PChar;
  end;

  TDEntry = record
    ShortName: String;
    LongName: String;
    Imported: Boolean;
    Size, SizeJ: Int64;
    Path: String;
    Address,
    AddressJ: Integer;
    Depth: Byte;
    Number: Integer;
    Parent: PDirEntry;
    Files: PFileEntry;
    Order: Integer;
  end;

  TDirectoryDescriptor = packed record
    LenDr: Byte;                        // 01-01
    Extended: Byte;                     // 02-02
    Address,                            // 03-06
    AddressBE: LONGWORD;                // 07-10
    DataLength: LONGWORD;               // 11-14
    DataLengthBE: LONGWORD;             // 15-18
    Year, Month, Day: Byte;             // 19-21
    Hour, Min, Sec: Byte;               // 22-24
    TimeDiffernce: Shortint;            // 25-25
    FileFlag: Byte;                     // 26-26
    FileUnitSize: Byte;                 // 27-27
    InterleaveGap: Byte;                // 28-28
    VolSeqnumber: WORD;                 // 29-32
    VolSeqnumberBE: WORD;               // 29-32
    LenOfFileIdentifier: Byte;          // 33-33
    FileName: array[0..255] of char;
  end;

  TDirectoryDescriptor2 = packed record
    LenDr: Byte;                        // 01-01
    Extended: Byte;                     // 02-02
    Address,                            // 03-06
    AddressBE: LONGWORD;                // 07-10
    DataLength: LONGWORD;               // 11-14
    DataLengthBE: LONGWORD;             // 15-18
    Year, Month, Day: Byte;             // 19-21
    Hour, Min, Sec: Byte;               // 22-24
    TimeDiffernce: Byte;                // 25-25
    FileFlag: Byte;                     // 26-26
    FileUnitSize: Byte;                 // 27-27
    InterleaveGap: Byte;                // 28-28
    VolSeqnumber: WORD;                 // 29-32
    VolSeqnumberBE: WORD;               // 29-32
    LenOfFileIdentifier: Byte;          // 33-33
    Dummy: Byte;                        // 34-34
  end;

  TPathTableRecord = packed record
    LenDI: Byte;
    ExtAttr: Byte;
    Address: LongWord;
    ParentNumber: Word;
    Name: Array[0..255] of char;
    BytesToWrite: Word;
  end;
  
  TVolumeDescriptor = packed record
    pdType: Byte;                              //   1 -   1
    Identifier: array[1..5] of char;           //   2 -   6
    Version: Byte;                             //   7 -   7
    VolumeFlag: Byte;                          //   8 -   8
    IdSystem: array[0..31] of char;            //   9 -  40
    IdVolume: array[0..31] of char;            //  41 -  72
    IdBC: array[0..7] of char;                 //  73 -  80
    NoOfSectors: LONGWORD;                     //  81 -  84
    NoOfSectorsBE: LONGWORD;                   //  85 -  88
    EscapeChars: array[0..31] of Char;         //  89 - 120
    VolSetSize: WORD;                          // 121 - 124
    VolSetSizeBE: WORD;                        // 121 - 124
    VolSeqNumber: WORD;                        // 125 - 128
    VolSeqNumberBE: WORD;                      // 125 - 128
    SectorSize: WORD;                          // 129 - 132
    SectorSizeBE: WORD;                        // 129 - 132
    PathTableSize: LONGWORD;                   // 133 - 136
    PathTableSizeBE: LONGWORD;                 // 137 - 140
    Type1PathTable: LONGWORD;                  // 141 - 144
    Type1PathTableBE: LONGWORD;                // 145 - 148
    TypeMPathTable: LONGWORD;                  // 149 - 152
    TypeMPathTableBE: LONGWORD;                // 153 - 156
    RootDirRec: TDirectoryDescriptor2;         // 157 - 190
    IdVolumeSet: Array[0..127] of char;        // 191 - 318
    IdPublisher: Array[0..127] of char;        // 319 - 446
    IdPreparer: Array[0..127] of char;         // 447 - 574
    IdApplication: Array[0..127] of char;      // 575 - 702
    FileCopyright: Array[0..36] of char;       // 703 - 739
    FileAbstract: Array[0..36] of char;        // 740 - 776
    FileBibliographic: Array[0..36] of char;   // 777 - 813
    DateCreation: Array[0..16] of char;        // 814 - 830
    DateModification: Array[0..16] of char;    // 831 - 847
    DateExpiration: Array[0..16] of char;      // 848 - 864
    DateEffective: Array[0..16] of char;       // 865 - 881
    FileStructureVer: Byte;                    // 882 - 882
    UnUsed1: Byte;                             // 883 - 883
    ApplicationData: array[884..1024] of char; // 884
    ApplicationData2: array[1025..1032] of char; // 884 - 1395
    ApplicationData3: array[1033..1395] of char; // 884 - 1395
    Unused2: array[1396..2048] of byte;        // 1396 - 2048
  end;

  TBootVolumeDescriptor = packed record
    pdType: Byte;                              //   0
    Identifier: array[1..5] of char;           //   1 - 5
    Version: Byte;                             //   6
    Ident: array[7..$26] of char;              //   7 - $26
    Unused1: array[$27..$46] of byte;          // $27 - $46
    BootCatLocation: Cardinal;
    Unused2: array[$4B..$7FF] of byte;
  end;
  TBootCatalog = packed record
    Header: Byte;                              
    PlatformID: Byte;
    Reserved1: Word;
    Developer: array[$4..$1b] of char;
    Checksum: Word;
    KeyByte1, KeyByte2: Byte;
    BootIndicator: Byte;
    BootMediaType: Byte;
    LoadSegment: Word;
    SystemType: Byte;
    Unused1: Byte;
    SectorCount: Word;
    LoadRBA: DWORD;
    Unused2: array[$0c..$1f] of byte;
    Unused3: array[$30..$7ef] of byte;
  end;

Const
  MaxDepth  = $FF;
  DefaultSectorSize = 2048;
var
  MaxFiles: Integer;
  MaxDirs: Integer;
  Files: array of PFileEntry;
  Dirs: array of pointer;
  PathTable: array of pointer;
  PathTableJ: array of pointer;
  DirCounter: Integer;
  FileCounter: Integer;
  DirectoryFirst: Boolean = False;
  TimeZoneDiff: SmallInt;
  TimeZoneInformation: TTimeZoneInformation;
  
procedure CopyToArrayW(src: String; var Dest:array of char; Len: Integer);
procedure CopyToArray(src: String; var Dest: array of char; Max: Integer);
procedure SortFiles(var List: PFileEntry; Joliet: Boolean);
procedure SortParent(var A : array of Pointer; N : integer);
procedure SortNumber(var A : array of Pointer; N : integer);
procedure SortLevel(var A : array of Pointer; N : integer);
procedure SortPN(var A : array of Pointer; N : integer; Joliet: Boolean);
procedure SortLN(var A : array of Pointer; N : integer; Joliet: Boolean);
procedure SetDateTime(Time: TDateTime; var d: TDirectoryDescriptor);
function  MSF2LBA(h, m, s, f: Byte): Cardinal;
procedure LBA2MSFH(lba: Cardinal; var m, s, f: Byte);
procedure LBA2MSF(LBA: Cardinal; var m, s, f: Byte);
function  LBA2MSFStr(LBA: Cardinal): String;
function  Sectors(bytes: Int64): Int64;
function  Sectors2(var P: PFileEntry): Int64;
function  IntToDec(i: Int64; n: Integer=2; pad: String = '0'): String;
function  ExtractLastDir(Path: String): String;

implementation

procedure LBA2MSFH(lba: Cardinal; var m, s, f: Byte);
begin
  f := lba mod 75;
  s := (lba div 75) mod 60;
  m := (lba div 75) div 60;
  f := ((f div 10) shl 4) or (f mod 10);
  m := ((m div 10) shl 4) or (m mod 10);
  s := ((s div 10) shl 4) or (s mod 10);
end;
procedure LBA2MSF(lba: Cardinal; var m, s, f: Byte);
begin
  f := lba mod 75;
  s := (lba div 75) mod 60;
  m := (lba div 75) div 60;
end;

function LBA2MSFStr(LBA: Cardinal): String;
var
  m, s, f: Byte;
begin
  LBA2MSF(LBA, m, s, f);
  Result := Format('%2.2d:%2.2d:%2.2d',[m, s, f]);
end;

procedure CopyToArrayW(src: String; var Dest:array of char; Len: Integer);
var
  i: Integer;
  ws: WideString;
  sa: array[0..4096] of char;
begin
  src := Copy(src+'                                                                                                                                                                 ', 1, len+12);
  ws := src;
  move(ws[1], sa[0], len*2);
  i := 0;
  while i < (len) do
  begin
    dest[i] := sa[i+1];
    dest[i+1] := sa[i];
    inc(i, 2);
  end;
end;

procedure CopyToArray(Src: String; var Dest: array of char; Max: Integer);
var
  i: Integer;
begin
  src := Copy(src+'                                                                                                                                                                                                                           ', 1, Max);
  for i:=1 to Max do
    dest[i-1] := src[i]
end;

procedure Swap(var X, Y : Pointer);
var
  Temp : PDirEntry;
begin
  Temp := X;
  X := Y;
  Y := Temp
end;

function FileName(s: PFileEntry): String; overload;
begin
  if ((s.Attr and faDirectory) = faDirectory) then
    result := '1'+s.ShortName
  else    
    result := '2'+s.ShortName;
end;

function FileName(s: String): String; overload;
begin
  result := s;
end;

function FileNameJ(s: PFileEntry): String;
begin
  result := s.ShortName;
end;

procedure SortFiles(var List: PFileEntry; Joliet: Boolean);
var
  node, node2, List2: PFileEntry;
begin
  if (List = nil) or (List.Next = nil) then exit;
  new(List2);
  list2.Next := list;
  list := list.Next;
  list2.Next.Next := nil;
  While List <> nil do
  begin
    Node  := List;
    list  := List.Next;
    Node2 := List2;
    if DirectoryFirst then
    begin
      if Joliet then
        While (Node2.Next <> nil) and (FileName(Node) > FileName(Node2.Next)) do Node2 := Node2.Next
      else
        While (Node2.Next <> nil) and (FileNameJ(Node) > FileNameJ(Node2.Next)) do Node2 := Node2.Next;
    end
    else
    begin
      if Joliet then
        While (Node2.Next <> nil) and (Node.ShortName > Node2.Next.ShortName) do Node2 := Node2.Next
      else
        While (Node2.Next <> nil) and (FileNameJ(Node) > FileNameJ(Node2.Next)) do Node2 := Node2.Next;
    end;
    Node.Next  := Node2.Next;
    Node2.Next := Node;
  end;
  List := List2.Next;
  Dispose(List2);
end;

procedure SortLevel(var A : array of Pointer; N : integer);
var
  Done: boolean;
  Jump, I, J: integer;
  AA, BB: PDirEntry;
begin
  Jump := N;
  while (Jump > 1) do
  begin
    Jump := Jump div 2;
    repeat
      Done := true;
      for J := 1 to (N - Jump) do
      begin
        I := J + Jump;
        AA := A[J];
        BB := A[I];
        if (AA.Depth > BB.Depth) then
        begin
          Swap(A[J], A[I]);
          Done := false
        end;
      end;
    until Done;
  end;
  for i:=0 to N do
  begin
    AA := A[i];
    AA.Number := i+1;
  end;
end;

procedure SortNumber(var A : array of Pointer; N : integer);
var
  Done: boolean;
  Jump, I, J: integer;
  AA, BB: PDirEntry;
begin
  Jump := N;
  while (Jump > 1) do
  begin
    Jump := Jump div 2;
    repeat
      Done := true;
      for J := 1 to (N - Jump) do
      begin
        I := J + Jump;
        AA := A[J];
        BB := A[I];
        if (AA.Number > BB.Number) then
        begin
          Swap(A[J], A[I]);
          Done := false
        end;
      end;
    until Done;
  end;
  for i:=0 to N do
  begin
    AA := A[i];
    AA.Number := i+1;
  end;
end;

procedure SortParent(var A : array of Pointer; N : integer);
var
  Done: boolean;
  Jump, I, J: integer;
  AA, BB: PDirEntry;
begin
  Jump := N;
  while (Jump > 1) do
  begin
    Jump := Jump div 2;
    repeat
      Done := true;
      for J := 1 to (N - Jump) do
      begin
        I := J + Jump;
        AA := A[J];
        BB := A[I];
        if (AA.Parent.Number > BB.Parent.Number) then
        begin
          Swap(A[J], A[I]);
          Done := false
        end;
      end;
    until Done;
  end;
  for i:=0 to N do
  begin
    AA := A[i];
    AA.Number := i+1;
  end;
end;

function IntToStrF(i, l: Integer): String;
var
  s: string;
begin
  s := '%.'+IntToStr(l)+'d ';
  result := Format(s,[ i ]);
end;

procedure SortLN(var A : array of Pointer; N : integer; Joliet: Boolean);
var
  Done: boolean;
  Jump, I, J: integer;
  AA, BB: PDirEntry;
  sa, sb: string;
begin

  Jump := N;
  while (Jump > 1) do
  begin
    Jump := Jump div 2;
    repeat
      Done := true;
      for J := 1 to (N - Jump) do
      begin
        I := J + Jump;
        AA := A[J];
        BB := A[I];
        if Joliet then
        begin
          sa := IntToStrF(AA.Depth, 4)+FileName(AA.LongName);
          sb := IntToStrF(BB.Depth, 4)+FileName(BB.LongName);
        end
        else
        begin
          sa := IntToStrF(AA.Depth, 4)+FileName(AA.ShortName);
          sb := IntToStrF(BB.Depth, 4)+FileName(BB.ShortName);
        end;
        if (SA > SB) then
        begin
          Swap(A[J], A[I]);
          Done := false
        end;
      end;
    until Done;
  end;
  for i:=0 to N do
  begin
    AA := A[i];
    AA.Number := i+1;
  end;
end;

procedure SortPN(var A : array of Pointer; N : integer; Joliet: Boolean);
var
  Done: boolean;
  Jump, I, J: integer;
  AA, BB: PDirEntry;
  s, sa, sb: string;
begin
  s := '                                                                                                                                                                                                                                                               ';
  Jump := N;
  while (Jump > 1) do
  begin
    Jump := Jump div 2;
    repeat
      Done := true;
      for J := 1 to (N - Jump) do
      begin
        I := J + Jump;
        AA := A[J];
        BB := A[I];
        if Joliet then
        begin
          sa := IntToStrF(AA.Parent.Number, 4)+FileName(AA.LongName);
          sb := IntToStrF(BB.Parent.Number, 4)+FileName(BB.LongName);
        end
        else
        begin
          sa := IntToStrF(AA.Parent.Number, 4)+FileName(AA.ShortName);
          sb := IntToStrF(BB.Parent.Number, 4)+FileName(BB.ShortName);
        end;
        if (SA > SB) then
        begin
          Swap(A[J], A[I]);
          Done := false
        end;
      end;
    until Done;
  end;
  for i := 0 to N do
  begin
    AA := A[i];
    AA.Number := i + 1;
  end;
end;

function Sectors2(var P: PFileEntry): Int64;
var
  bytes: Int64;
begin
  bytes := p.FileSize;
  if bytes = 0 then
  begin
    result := 0;
    exit;
  end;
  if bytes mod (DefaultSectorSize+276) = 0 then
    Result := bytes div (DefaultSectorSize+276)
  else
    Result := (bytes div (DefaultSectorSize+276))+1;
  p.FileSize := Result * 2048;
  p.FileSizeJ := Result * 2048;
  if Result < 150 then Result := 150;
  p.SpaceReqOnDisc := Result * (DefaultSectorSize);
end;

function Sectors(bytes: Int64): Int64;
begin
  if bytes = 0 then
  begin
    result := 0;
    exit;
  end;
  if bytes mod DefaultSectorSize = 0 then
    Result := bytes div DefaultSectorSize
  else
    Result := (bytes div DefaultSectorSize)+1
end;

function YearOf(const AValue: TDateTime): Word;
var
  LMonth, LDay: Word;
begin
  DecodeDate(AValue, Result, LMonth, LDay);
end;

function MonthOf(const AValue: TDateTime): Word;
var
  LYear, LDay: Word;
begin
  DecodeDate(AValue, LYear, Result, LDay);
end;

function DayOf(const AValue: TDateTime): Word;
var
  LYear, LMonth: Word;
begin
  DecodeDate(AValue, LYear, LMonth, Result);
end;

function HourOf(const AValue: TDateTime): Word;
var
  LMinute, LSecond, LMilliSecond: Word;
begin
  DecodeTime(AValue, Result, LMinute, LSecond, LMilliSecond);
end;

function MinuteOf(const AValue: TDateTime): Word;
var
  LHour, LSecond, LMilliSecond: Word;
begin
  DecodeTime(AValue, LHour, Result, LSecond, LMilliSecond);
end;

function SecondOf(const AValue: TDateTime): Word;
var
  LHour, LMinute, LMilliSecond: Word;
begin
  DecodeTime(AValue, LHour, LMinute, Result, LMilliSecond);
end;

procedure SetDateTime(Time: TDateTime; var d: TDirectoryDescriptor);
begin
  d.Year := LoByte(YearOf(Time)-1900);
  d.Month := LoByte(MonthOf(Time));
  d.Day := LoByte(DayOf(Time));
  d.Hour := LoByte(HourOf(Time));
  d.Min := LoByte(MinuteOf(Time));
  d.Sec := LoByte(SecondOf(Time));
  d.TimeDiffernce := TimeZoneDiff;
end;

function MSF2LBA(h, m, s, f: Byte): Cardinal;
begin
  result := (h * 60 * 60 * 75) + (m * 60 * 75)+(s * 75) + (f);
end;

function  IntToDec(i: Int64; n: Integer=2; pad: String = '0'): String;
var
  s1, s2: String;
  l, j: Integer;
begin
  s1 := IntToStr(i);
  l := Length(s1);
  for j:=l to n-1 do
    s2 := s2 + Pad;
  result := s2+s1;
end;

function ExtractLastDir(Path: String): String;
var
  i, j: Integer;
begin
  j := 1;
  for i := Length(Path)-1 downto 1 do
  if path[i] = '\' then
  begin
    j:=i;
    Break;
  end;
  result := Copy(path, j, Length(Path));
end;

end.




 
