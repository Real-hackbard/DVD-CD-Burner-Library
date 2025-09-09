
unit CovertFuncs;

interface

uses Windows, SysUtils, Classes,Math, TypInfo,  ScsiDefs,scsitypes;


const
   OS_UNKNOWN = -1;
   OS_WIN95 = 0;
   OS_WIN98 = 1;
   OS_WINNT35 = 2;
   OS_WINNT4 = 3;
   OS_WIN2K = 4;
   OS_WINXP = 5;


   SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority = (Value: (0, 0, 0, 0, 0, 5));
   SECURITY_BUILTIN_DOMAIN_RID = $00000020;
   DOMAIN_ALIAS_RID_ADMINS = $00000220;



type
  TCharArr = array of Char;

Type
  TBothEndianWord = Packed Record
    LittleEndian,
    BigEndian     : Word;
  End;

  TBothEndianDWord = Packed Record
    LittleEndian,
    BigEndian     : LongWord;
  End;


Type
  TVolumeDateTime = Packed Record
    Year      : Array[0..3] Of Char;
    Month     : Array[0..1] Of Char;
    Day       : Array[0..1] Of Char;
    Hour      : Array[0..1] Of Char;
    Minute    : Array[0..1] Of Char;
    Second    : Array[0..1] Of Char;
    MSeconds  : Array[0..1] Of Char;
    GMTOffset : Byte;
  End;

Type
  TDirectoryDateTime = Packed Record
    Year      : Byte; // since 1900
    Month     : Byte;
    Day       : Byte;
    Hour      : Byte;
    Minute    : Byte;
    Second    : Byte;
    GMTOffset : Byte; // in 15 minutes steps
  End;



function EnumToStr(ArgType: PTypeInfo; var Arg): string;
function SetToStr(ArgType: PTypeInfo; var Arg): string;
function HexToStrings(Buf: pointer; BufLen: DWORD): TStrings;
function Swap32(value : dword) : dword ;

function ConvertDataBlock(DataBlock: Integer): Integer;
function GetFileSize(const FileName: string): LongInt;
procedure ZeroMemory(Destination: Pointer; Length: DWORD);
function getOsVersion: integer;
function RoundUp(X: Extended): Integer;
function ArrOfChar(AStr: String): TCharArr;

function IntToMB(const ASize: Int64): string;
function VolumeDateTimeToStr(const VDT: TVolumeDateTime): string;
function SwapWord(const AValue: Word): Word;
function SwapDWord(const AValue: LongWord): LongWord;
function BuildBothEndianWord(const AValue: Word): TBothEndianWord;
function BuildBothEndianDWord(const AValue: LongWord): TBothEndianDWord;
function BuildDirectoryDateTime(const ADateTime: TDateTime; const AGMTOffset: Byte): TDirectoryDateTime;
function BuildVolumeDateTime(const ADateTime: TDateTime; const AGMTOffset: Byte): TVolumeDateTime;
function RetrieveFileSize(const AFileName: string): LongWord;
function IsAdministrator: Boolean;
function Endian(const Source; var Destination; const Count: Integer): Boolean;
function EndianToIntelBytes(const AValue: array of Byte; Count: Byte): Integer;
function GetLBA(const Byte1, Byte2, Byte3, Byte4: Byte): LongWord;
function HMSFtoLBA(const AHour, AMinute, ASecond, AFrame: Byte): LongWord;
function HiWord(Lx: LongWord): Word;
function LoWord(Lx: LongWord): Word;
function HiByte(Lx: Word): Byte;
function LoByte(Lx: Word): Byte;
function IsBitSet(const Value: LongWord; const Bit: Byte): Boolean;
function BitOn(const Value: LongWord; const Bit: Byte): LongWord;
function BitOff(const Value: LongWord; const Bit: Byte): LongWord;
function BitToggle(const Value: LongWord; const Bit: Byte): LongWord;
function ByteToBin(Value: Byte): string;
function ScsiErrToString(Err : TScsiError) : string;
Function StrToUnicode(Name : String):Widestring;
function DOSchars_Len(str: string; Sze: integer):string;
function GetISOFilename(const FileName: String): String;

Function BigEndianW(Arg: WORD): WORD;
Function BigEndianD(Arg: DWORD): DWORD;
Procedure BigEndian(Const Source; Var Dest; Count: integer);
Function GatherWORD(b1, b0: byte): WORD;
Function GatherDWORD(b3, b2, b1, b0: byte): DWORD;
Procedure ScatterDWORD(Arg: DWORD; Var b3, b2, b1, b0: byte);
Procedure ASPIstrCopy(Src: PChar; Var Dst: ShortString; Leng: Integer);




implementation


function getOsVersion: integer;
var
   os: OSVERSIONINFO;
begin
   ZeroMemory(@os, sizeof(os));
   os.dwOSVersionInfoSize := sizeof(os);
   GetVersionEx(os);

   if os.dwPlatformId = VER_PLATFORM_WIN32_NT then
   begin
      if (os.dwMajorVersion = 3) and (os.dwMinorVersion >= 51) then
      begin
         Result := OS_WINNT35;
         Exit;
      end
      else
         if os.dwMajorVersion = 4 then
         begin
            Result := OS_WINNT4;
            Exit;
         end
         else
            if (os.dwMajorVersion = 5) and (os.dwMinorVersion = 0) then
            begin
               Result := OS_WIN2K;
               Exit;
            end
            else
            begin
               Result := OS_WINXP;
               Exit;
            end;
   end
   else
      if os.dwPlatformId = VER_PLATFORM_WIN32_WINDOWS then
      begin
         if os.dwMinorVersion = 0 then
         begin
            Result := OS_WIN95;
            Exit;
         end
         else
         begin
            Result := OS_WIN98;
            Exit;
         end;
      end;

   Result := OS_UNKNOWN;
end;


Function BigEndianW(Arg: WORD): WORD;
Begin
   result := ((Arg Shl 8) And $FF00) Or
      ((Arg Shr 8) And $00FF);
End;

Function BigEndianD(Arg: DWORD): DWORD;
Begin
   result := ((Arg Shl 24) And $FF000000) Or
      ((Arg Shl 8) And $00FF0000) Or
      ((Arg Shr 8) And $0000FF00) Or
      ((Arg Shr 24) And $000000FF);
End;

Procedure BigEndian(Const Source; Var Dest; Count: integer);
Var
   pSrc, pDst: PChar;
   i: integer;
Begin
   pSrc := @Source;
   pDst := PChar(@Dest) + Count;
   For i := 0 To Count - 1 Do Begin
      Dec(pDst);
      pDst^ := pSrc^;
      Inc(pSrc);
   End;
End;

Function GatherWORD(b1, b0: byte): WORD;
Begin
   result := ((WORD(b1) Shl 8) And $FF00) Or
      ((WORD(b0)) And $00FF);
End;

Function GatherDWORD(b3, b2, b1, b0: byte): DWORD;
Begin
   result := ((LongInt(b3) Shl 24) And $FF000000) Or
      ((LongInt(b2) Shl 16) And $00FF0000) Or
      ((LongInt(b1) Shl 8) And $0000FF00) Or
      ((LongInt(b0)) And $000000FF);
End;

Procedure ScatterDWORD(Arg: DWORD; Var b3, b2, b1, b0: byte);
Begin
   b3 := (Arg Shr 24) And $FF;
   b2 := (Arg Shr 16) And $FF;
   b1 := (Arg Shr 8) And $FF;
   b0 := Arg And $FF;
End;

Procedure ASPIstrCopy(Src: PChar; Var Dst: ShortString; Leng: Integer);
Var i: integer;
Begin
   i := 0;
   While (i < Leng) And (Src[i] >= ' ') Do
   Begin Dst[i + 1] := Src[i]; inc(i); End;
   While (i > 0) And (Dst[i] = ' ') Do Dec(i); // Trim it Right
   Dst[0] := CHR(i);
End;




function Swap32(value : dword) : dword ;
assembler ;
  asm
   bswap eax
  end;



Function StrToUnicode(Name : String):Widestring;
var
     WS : WideString;
begin
      WS := Name;
      Result := WS;
end;



function DOSchars_Len(str: string; Sze: integer):string;
//filters out non DOS chars, max length = Sze, including extension
var
 temp : string;
 i : integer;
begin
 result:= '';//important
 temp := UpperCase(str);
 if Pos('.',temp) > 0 then
  begin
   result:= DOSchars_Len(Copy(temp,1,Pos('.',temp)-1),Sze-4) + Copy(temp,Pos('.',temp),4);
   exit;
  end;
 for i:= 1 to length(temp) do
  if  temp[i] in ['0'..'9','A'..'Z','_'] then
    result:= result + temp[i];
  result:= Copy(result,1, Sze);
end;




procedure ZeroMemory(Destination: Pointer; Length: DWORD);
begin
   FillChar(Destination^, Length, 0);
end;


function EnumToStr(ArgType: PTypeInfo; var Arg): string;
begin
   case (GetTypeData(ArgType))^.OrdType of
      otSByte, otUByte: Result := GetEnumName(ArgType, BYTE(Arg));
      otSWord, otUWord: Result := GetEnumName(ArgType, WORD(Arg));
      otSLong: Result := GetEnumName(ArgType, LongInt(Arg));
   end;
end;


function ScsiErrToString(Err : TScsiError) : string;
begin
   Result := EnumToStr(TypeInfo(TScsiError), Err);
end;



type TIntegerSet = set of 0..SizeOf(Integer) * 8 - 1;
   PIntegerSet = ^TIntegerSet;

function SetToStr(ArgType: PTypeInfo; var Arg): string;
var Info: PTypeInfo;
   Data: PTypeData;
   I: Integer;
begin
   Result := '[';
   Info := (GetTypeData(ArgType))^.CompType^;
   Data := GetTypeData(Info);
   for I := Data^.MinValue to Data^.MaxValue do
      if I in PIntegerSet(@Arg)^ then begin
         if Length(Result) <> 1 then Result := Result + ', ';
         Result := Result + GetEnumName(Info, I);
      end;
   Result := Result + ']';
end;


function HexToStrings(Buf: pointer; BufLen: DWORD): TStrings;
const BytesPerLine = 16;
   BytesPerTab = 4;
   CharsInAddress = 4;
var CurLine, CurByte, CurOffset: integer;
   s: string;
   b: char;
begin
   Result := TStringList.Create;
   if (BufLen <= 0) or not Assigned(Buf) then exit;
   try
      for CurLine := 0 to (BufLen - 1) div BytesPerLine do begin
         CurOffset := CurLine * BytesPerLine;
         s := IntToHex(CurOffset, CharsInAddress);
         for CurByte := 0 to BytesPerLine - 1 do begin
            if (CurByte mod BytesPerTab) = 0 then s := s + ' ';
            if CurOffset < BufLen
               then s := s + IntToHex(BYTE((PChar(Buf) + CurOffset)^), 2) + ' '
            else s := s + '   ';
            Inc(CurOffset);
         end;
         s := s + '|';
         CurOffset := CurLine * BytesPerLine;
         for CurByte := 0 to BytesPerLine - 1 do begin
            if CurOffset < BufLen then begin
               b := (PChar(Buf) + CurOffset)^;
               if b < ' ' then b := ' ';
               s := s + b;
            end else s := s + ' ';
            Inc(CurOffset);
         end;
         Result.Add(s);
      end;
   except
      Result.Clear;
   end;
end;





function ConvertDataBlock(DataBlock: Integer): Integer;
var
   DataSize: Integer;
begin
   DataSize := 2048;
   case DataBlock of
      RAW_DATA_BLOCK: DataSize := 2352;
      RAW_DATA_P_Q_SUB: DataSize := 2368;
      RAW_DATA_P_W_SUB: DataSize := 2448;
      RAW_DATA_P_W_SUB2: DataSize := 2448;
      MODE_1: DataSize := 2048;
      MODE_2: DataSize := 2336;
      MODE_2_XA_FORM_1: DataSize := 2048;
      MODE_2_XA_FORM_1_SUB: DataSize := 2056;
      MODE_2_XA_FORM_2: DataSize := 2324;
      MODE_2_XA_FORM_2_SUB: DataSize := 2324;
   end;
   result := DataSize;

end;


function GetFileSize(const FileName: string): LongInt;
var
   SearchRec: TSearchRec;
begin
   try
      if FindFirst(ExpandFileName(FileName), faAnyFile, SearchRec) = 0 then
         Result := SearchRec.Size
      else Result := -1;
   finally
      SysUtils.FindClose(SearchRec);
   end;
end;



function IntToMB(const ASize: Int64): string;
begin
   Result := IntToStr(ASize div 1024 div 1024);
end;



function VolumeDateTimeToStr(const VDT: TVolumeDateTime): string;
begin
   Result := string(VDT.Day) + '.' + string(VDT.Month) + '.' +
      string(VDT.Year) + ' ' + string(VDT.Hour) + ':' +
      string(VDT.Minute) + ':' + string(VDT.Second) + '.' +
      string(VDT.MSeconds) + ' ' + IntToStr(VDT.GMTOffset * 15) + ' min from GMT';
end;



function ArrOfChar(AStr: String): TCharArr;
var
   j: integer;
begin
   SetLength( Result, Length(AStr));
   for j := 0 to Length(AStr) - 1 do Result[j] := AStr[j + 1];
end;



function SwapWord(const AValue: Word): Word;
begin
   Result := ((AValue shl 8) and $FF00) or ((AValue shr 8) and $00FF);
end;


function SwapDWord(const AValue: LongWord): LongWord;
begin
   Result := ((AValue shl 24) and $FF000000) or
      ((AValue shl 8) and $00FF0000) or
      ((AValue shr 8) and $0000FF00) or
      ((AValue shr 24) and $000000FF);
end;


function BuildBothEndianWord(const AValue: Word): TBothEndianWord;
begin
   Result.LittleEndian := AValue;
   Result.BigEndian := SwapWord(AValue);
end;


function BuildBothEndianDWord(const AValue: LongWord): TBothEndianDWord;
begin
   Result.LittleEndian := AValue;
   Result.BigEndian := SwapDWord(AValue);
end;


function BuildVolumeDateTime(const ADateTime: TDateTime; const AGMTOffset: Byte): TVolumeDateTime;
var
   Hour, Min, Sec, MSec,
      Year, Month, Day: Word;
   s: string;
begin
   DecodeTime(ADateTime, Hour, Min, Sec, MSec);
   DecodeDate(ADateTime, Year, Month, Day);
   Result.GMTOffset := AGMTOffset;
   s := IntToStr(Hour);
   StrPCopy(Result.Hour, s);
   s := IntToStr(Min);
   StrPCopy(Result.Minute, s);
   s := IntToStr(Sec);
   StrPCopy(Result.Second, s);
   s := IntToStr(MSec);
   StrPCopy(Result.MSeconds, s);
   s := IntToStr(Year);
   StrPCopy(Result.Year, s);
   s := IntToStr(Month);
   StrPCopy(Result.Month, s);
   s := IntToStr(Day);
   StrPCopy(Result.Day, s);
end;



function BuildDirectoryDateTime(const ADateTime: TDateTime; const AGMTOffset: Byte): TDirectoryDateTime;
var
   Hour, Min, Sec, MSec,
      Year, Month, Day: Word;
   s: string;
begin
   DecodeTime(ADateTime, Hour, Min, Sec, MSec);
   DecodeDate(ADateTime, Year, Month, Day);
   Result.GMTOffset := AGMTOffset;
   Result.Hour :=  Hour;
   Result.Minute := Min;
   Result.Second := Sec;
   Result.Year := Year;
   Result.Month := Month;
   Result.Day :=  Day;
end;




function RetrieveFileSize(const AFileName: string): LongWord;
var
   SR: TSearchRec;
begin
   Result := 0;

   if (FileExists(AFileName)) and
      (FindFirst(AFileName, faAnyFile, SR) = 0) then
   begin
      if ((SR.Attr and faDirectory) = 0) and
         ((SR.Attr and faVolumeID) = 0) then
         Result := SR.Size;
   end;
end;





function Sgn(X: Extended): Integer;
  begin
      if X < 0 then
        Result := -1
      else if X = 0 then
        Result := 0
      else
        Result := 1;
    end;



function RoundUp(X: Extended): Integer;
Var
   Temp : Extended;
begin
   Temp := Int(X) + Sgn(Frac(X));
   Result := round(Temp);
end;






function IsAdministrator: Boolean;
var
   hAccessToken: THandle;
   ptgGroups: PTokenGroups;
   dwInfoBufferSize: DWORD;
   psidAdministrators: PSID;
   x: Integer;
   bSuccess: BOOL;
begin
   Result := False;
   bSuccess := OpenThreadToken(GetCurrentThread, TOKEN_QUERY, True,
      hAccessToken);
   if not bSuccess then
   begin
      if GetLastError = ERROR_NO_TOKEN then
         bSuccess := OpenProcessToken(GetCurrentProcess, TOKEN_QUERY,
            hAccessToken);
   end;
   if bSuccess then
   begin
      GetMem(ptgGroups, 1024);
      bSuccess := GetTokenInformation(hAccessToken, TokenGroups,
         ptgGroups, 1024, dwInfoBufferSize);
      CloseHandle(hAccessToken);
      if bSuccess then
      begin
         AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
            SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
            0, 0, 0, 0, 0, 0, psidAdministrators);
{$R-}
         for x := 0 to ptgGroups.GroupCount - 1 do
            if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then
            begin
               Result := True;
               Break;
            end;
{$R+}
         FreeSid(psidAdministrators);
      end;
      FreeMem(ptgGroups);
   end;
end;




function Endian(const Source; var Destination; const Count: Integer): Boolean;
var
   PSource, PDestination: PChar;
   I: Integer;
begin
   Result := False;
   PSource := @Source;
   PDestination := PChar(@Destination) + Count;
   for i := 0 to Count - 1 do
   begin
      Dec(PDestination);
      pDestination^ := PSource^;
      Inc(PSource);
      Result := True;
   end;
end;



function EndianToIntelBytes(const AValue: array of Byte; Count: Byte): Integer;
var
   I: Integer;
begin
   Result := 0;
   for I := 0 to Count - 1 do
   begin
      Result := (AValue[I] shl ((Count - (I + 1)) * 8) or Result);
   end;
end;



function GetLBA(const Byte1, Byte2, Byte3, Byte4: Byte): LongWord;
begin
   Result := (Byte1 shl 24) or (Byte2 shl 16) or (Byte3 shl 8) or Byte4;
end;


function HMSFtoLBA(const AHour, AMinute, ASecond, AFrame: Byte): LongWord;
begin
   Result := (AHour * 60 * 60 * 75) + (AMinute * 60 * 75) + (ASecond * 75) + AFrame;
end;


function HiWord(Lx: LongWord): Word;
begin
   Result := (Lx shr 16) and $FFFF;
end;


function LoWord(Lx: LongWord): Word;
begin
   Result := Lx;
end;


function HiByte(Lx: Word): Byte;
begin
   Result := (Lx shr 8) and $FF;
end;


function LoByte(Lx: Word): Byte;
begin
   Result := Lx and $FF;
end;


function IsBitSet(const Value: LongWord; const Bit: Byte): Boolean;
begin
   Result := (Value and (1 shl Bit)) <> 0;
end;


function BitOn(const Value: LongWord; const Bit: Byte): LongWord;
begin
   Result := Value or (1 shl Bit);
end;


function BitOff(const Value: LongWord; const Bit: Byte): LongWord;
begin
   Result := Value and ((1 shl Bit) xor $FFFFFFFF);
end;


function BitToggle(const Value: LongWord; const Bit: Byte): LongWord;
begin
   Result := Value xor (1 shl Bit);
end;


function ByteToBin(Value: Byte): string;
var
   I: Integer;
begin
   Result := StringOfChar('0', 8);

   for I := 0 to 7 do
   begin
      if (Value mod 2) = 1 then
         Result[8 - i] := '1';

      Value := Value div 2;
   end;
end;



function GetShortFilename(const FileName: TFileName): TFileName;
var
  buffer: array[0..MAX_PATH-1] of char;
begin
  SetString(Result, buffer, GetShortPathName(pchar(FileName), buffer, MAX_PATH-1));
end;


function GetISOFilename(const FileName: String): String;
var
    TempRes : String;
    Ext : String;
begin
   if length(Filename) > 12 then
   begin
     Ext := extractfileext(Filename);
     TempRes := copy(Filename,1,6);
     TempRes := TempRes + '~1' + Ext + ';1';
   end
      else
          TempRes := Filename;
   Result := UpperCase(TempRes);
end;


end.
