                     
Unit SptiUnit;


Interface

Uses Windows,CovertFuncs,wnaspi32,skSCSI, CDROMIOCTL,dialogs,SCSITypes, scsidefs, sysutils, Registry;




{Const //=======  Possible values of Direction parameter ========
   SRB_NODIR = 2; // No data I/O is performed
   SRB_DIR_IN = 1; // Transfer from SCSI target to host
   SRB_DIR_OUT = 0; // Transfer from host to SCSI target
}



type
  SCSI_ADDRESS = record
    Length : LongInt;
    PortNumber : Byte;
    PathId : Byte;
    TargetId : Byte;
    Lun : Byte;
  end;
  PSCSI_ADDRESS = ^SCSI_ADDRESS;

  ENotAdmin = Exception;

  NTSCSIDRIVE = record
    ha : byte;
    tgt : byte;
    lun : byte;
    driveLetter : Char; //Was byte
    bUsed : Bool;
    hDevice : THandle;
    inqData : array[0..36 - 1] of byte;
  end;
  PNTSCSIDRIVE = ^NTSCSIDRIVE;



Type


   

   {CDB types for Spti commands}
   TCDB12 = Array[0..11] Of BYTE;
   PCDB12 = ^TCDB12;
   TCDB10 = Array[0..9] Of BYTE;
   PCDB10 = ^TCDB10;
   TCDB6  = Array[0..5] Of BYTE;
   PCDB6 = ^TCDB6;


Type TScsiInt13info = Packed Record
         Support,
         DosSupport: BOOLEAN;
         DriveNumber,
         Heads,
         Sectors: BYTE;
   End;


// Request for information about host adapter.
Type TScsiHAinfo = Packed Record
      ScsiId: BYTE; // SCSI Id of selected host adapter
      MaxTargetCount: BYTE; // Max target count for selected HA
      ResidualSupport: BOOLEAN; // True if HA supports residual I/O
      MaxTransferLength: DWORD; // Max transfer length in bytes
      BufferAlignMask: WORD; // Buffer for data I/O must be aligned by:
                   // 0=byte, 1=word, 3=dword, 7=8-byte, etc. 65536 bytes max
      ScsiManagerId, // MustBe = 'ASPI for WIN32'
      HostAdapterId: String[16]; // String describing selected HA
   End;



Type
   TScsiDeviceType = (TSDDisk, TSDTape, TSDPrinter, TSDProcessor,
      TSDWORM, TSDCDROM, TSDScanner, TSDOptical,
      TSDChanger, TSDCommunication,
      TSDInvalid, TSDAny, TSDOther);
Var
   TScsiDeviceTypeName: Array[TScsiDeviceType] Of String = ('Disk Drive',
      'Tape Drive', 'Printer', 'Processor', 'WORM Drive', 'CD-ROM Drive',
      'Scanner', 'Optical Drive', 'Changer', 'Communication Device',
      'Invalid', 'Any Type Device', 'Other Type Device');


   




{Aspi Functions}

Function GetAdaptorName(ID: Integer): String;
Function GetSCSIID(ID: Integer): String;
Function ISCDROM(ID, Target, LUN: Integer): Boolean;
Function GetCDRegName(ID, Target, LUN: Integer): String;
Function ResetAspi(ID, Target, LUN: Integer) : Boolean;
Function AttachLUN(Var Arg: BYTE; DeviceID: TBurnerID): BYTE;

Function CloseDriveHandle(DeviceID: TCDBurnerInfo): Boolean;
Function GetDriveTempHandle(DeviceID: TCDBurnerInfo) :Thandle;
Procedure GetDriveHandle(var DeviceID: TCDBurnerInfo);


// =================== Helper routines ======================
// Intel/Windows/Delphi <-> Motorola/ASPI format conversion routines
Function BigEndianW(Arg: WORD): WORD;
Function BigEndianD(Arg: DWORD): DWORD;
Procedure BigEndian(Const Source; Var Dest; Count: integer);
Function GatherWORD(b1, b0: byte): WORD;
Function GatherDWORD(b3, b2, b1, b0: byte): DWORD;
Procedure ScatterDWORD(Arg: DWORD; Var b3, b2, b1, b0: byte);
Procedure ASPIstrCopy(Src: PChar; Var Dst: ShortString; Leng: Integer);


// ASPI Error decoding routines
Function GetAspiError(Status, HaStat, TargStat: BYTE): TScsiError;
Function GetAspiErrorSense(Status, HaStat, TargStat: BYTE;
   Sense: PscsiSenseInfo): TScsiError;
Function AspiCheck(Err: TScsiError): boolean;


// TBurnerID helper definitions and functions

Procedure FillWORD(Src: WORD; Var Dst: BYTE);
Procedure FillDWORD(Src: DWORD; Var Dst: BYTE);

Function GatherDeviceID(Adapter, Target, Lun: byte; Letter: char): TBurnerID;

Function ScatterDeviceID(DeviceID: TBurnerID;
   Var Adapter, Target, Lun: byte): char;

Function DeviceIDtoLetter(DeviceID: TBurnerID): char;

Function ASPIgetDeviceIDflag(DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag): boolean;

Procedure ASPIsetDeviceIDflag(Var DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag; Value: boolean);


// ============= Base-level ASPI request routines ==============

Function ASPIhaInquiry(HaId: BYTE; Var sh: TScsiHAinfo): TScsiError;
// Request for device type.
Function ASPIgetDeviceType(DeviceID: TBurnerID;
   Var DeviceType: TScsiDeviceType): TScsiError;
// SCSI command execution.
//   DeviceID     identifies the device to be accessed.
//   Pcdb/CdbLen  SCSI Command Descriptor Block pointer/size
//   Pbuf/BufLen  Data buffer pointer/size.
//                Must be nil/0 if command does not requires data I/O.
//   Direction    Data transfer direction. Must be one of SRB_DIR constants.
Function ASPIsendScsiCommand(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
// Abort issued by ASPIsendScsiCommand() request for a given host adapter.
Procedure ASPIabortCommand(HaId: BYTE; Psrb: pointer);
// Soft reset for the given device.
Function ASPIresetDevice(DeviceID: TCDBurnerInfo; Timeout: DWORD): TScsiError;
// Retrieves some DOS-related info about device.



Function ASPIgetDriveInt13info(DeviceID: TCDBurnerInfo;
   Var Info: TScsiInt13info): TScsiError;

//=================== Device enumerator routine ====================
//  Callback function definition.
//    lpUserData  specifies the user-defined value given in AspiEnumDevices
//    Device      identifies the device found
//    Return Value  To continue enumeration, the callback function must
//                  return TRUE; to stop enumeration, it must return FALSE.
//  Enumerator routine definition.
//    DeviceType  Type of devices to enumerate. Set it to TSDAny to
//                obtain all devices available.
//    CallBack    Points to an user-defined callback function (see above).
//    lpUserData  Specifies a user-defined value to be passed to the callback.
//    Return Value  Number of devices found. Zero if no devices of specified
//                  type exists, -1 if search fails.
Type TAspiDeviceEnumCallBack
   = Function(lpUserData: pointer; Device: TCDBurnerInfo ;FoundName :String): boolean;


// ================== Mid-level SCSI request routines ================
// Three most frequent cases of ASPISendScsiCommand(),
// for CDB of 6, 10 and 12 bytes length.
Function ASPIsend6(DeviceID: TCDBurnerInfo;
   OpCode: BYTE; Lba: DWORD; Byte4: BYTE;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend10(DeviceID: TCDBurnerInfo; OpCode: BYTE;
   Byte1: BYTE; Lba: DWORD; Byte6: BYTE; Word7: WORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend12(DeviceID: TCDBurnerInfo; OpCode: BYTE;
   Byte1: BYTE; Lba: DWORD; TLength: DWORD; Byte10: BYTE;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

   {With new TCDB command struct}
Function ASPIsend6CDB(DeviceID: TCDBurnerInfo; CDB :TCDB6; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend10CDB(DeviceID: TCDBurnerInfo; CDB :TCDB10; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend12CDB(DeviceID: TCDBurnerInfo; CDB :TCDB12; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;


// ++++++++++++++base SPTI commands all new+++++++++++++++++++

Type
  TSPTIWriter = record
    HaId : Byte;
    Target : Byte;
    Lun : Byte;
    Vendor : ShortString;
    ProductId : ShortString;
    Revision : ShortString;
    VendorSpec : ShortString;
    Description : ShortString;
    DriveLetter : Char;
    DriveHandle : Thandle;
  end;

  TSPTIWriters = record
    ActiveCdRom : Byte;
    CdRomCount : Byte;
    CdRom : array[0..25] of TSPTIWriter;
  end;



function ScsiErrToString(Err : TScsiError) : string;
function ScsiErrToStr(Err : TScsiError) : string;
function ScsiDeviceIDtoStr(Device : TBurnerID) : string;



function GetDriveNumbers( var CDRoms : TSPTIWriters ) : integer;
function GetSPTICdRomDrives( var CdRoms : TSPTIWriters ) : Boolean;
procedure GetDriveInformation( i : byte; var CdRoms : TSPTIWriters );




Implementation

Uses Scsiunit;


Function AttachLUN(Var Arg: BYTE; DeviceID: TBurnerID): BYTE;
Var i, j, Lun: BYTE;
Begin
   ScatterDeviceID(DeviceID, i, j, Lun);
   Result := ((Lun And 7) Shl 5) Or (Arg And $1F);
End;


Procedure FillWORD(Src: WORD; Var Dst: BYTE);
Begin
   BigEndian(Src, Dst, 2);
End;


Procedure FillDWORD(Src: DWORD; Var Dst: BYTE);
Begin
   BigEndian(Src, Dst, 4);
End;

function ScsiErrToString(Err : TScsiError) : string;
begin
   Result := EnumToStr(TypeInfo(TScsiError), Err);
end;

function ScsiErrToStr(Err : TScsiError) : string;
begin
   Result := '() result is ' + ScsiErrToString(Err);
end;

function ScsiDeviceIDtoStr(Device : TBurnerID) : string;
var Adapter, Target, Lun : byte;
    Letter : Char;
begin
   Letter := ScatterDeviceID(Device, Adapter, Target, Lun);
   if Letter < 'A' then Letter := '?';
   Result := IntToStr(Adapter) + ','
           + IntToStr(Target)  + ','
           + IntToStr(Lun)     + ','
           + Letter            + ': ';
end;



{*******************************************************************************
                                                                    AspiIntalled
*******************************************************************************}

function AspiInstalled : Integer;
var
  AspiStatus : Cardinal;
begin
  if WNASPI_LOADED then
  begin
    AspiStatus := GetASPI32SupportInfo;
    if HIBYTE( LOWORD( AspiStatus ) ) = SS_COMP then
    begin
      // get number of host installed on the system
      Result := LOBYTE( LOWORD( AspiStatus ) );
    end
    else
      Result := -1
  end
  else
    Result := -1
end;


Function CheckAspiLayer: Boolean;
Begin
  Result := True;
  if AspiInstalled = -1 then Result := False;
End;



function GetDriveNumbers( var CDRoms : TSPTIWriters ) : integer;
var
  i : integer;
  szDrives : array[0..105] of Char;
  p : PChar;
  DriveCount : integer;
begin
  GetLogicalDriveStrings( 105, szDrives );
  p := szDrives;
  i := 0;
  DriveCount := 0;
  while p^ <> '' do
  begin
    if GetDriveType( p ) = DRIVE_CDROM then
    begin
      CdRoms.CdRom[i].DriveLetter := p^; // + ':\';
      i := CdRoms.CdRomCount + 1;
      CdRoms.CdRomCount := CdRoms.CdRomCount + 1;
    end;
    p := p + lstrlen( p ) + 1;
  end;
  Result := CdRoms.CdRomCount;
end;



Function GetAdaptorName(ID: Integer): String;
Begin
End;


Function GetSCSIID(ID: Integer): String;
Begin 
End;



Function ISCDROM(ID, Target, LUN: Integer): Boolean;
Begin
End;



Function GetCDRegName(ID, Target, LUN: Integer): String;
Var
   DEVName: String;
   Registry: TRegistry;
   Root2000: String;
   Root98: String;
   FormatKey: String;
Begin
   DEVName := 'Cannot Find Name';
   Root2000 := 'HKEY_LOCAL_MACHINE';
   Root98 := 'HKEY_LOCAL_MACHINE\Enum\Scsi';
   FormatKey := 'HARDWARE\DEVICEMAP\Scsi\Scsi Port ' + inttostr(ID) + '\Scsi Bus 0\Target Id ' + inttostr(Target) + '\Logical Unit Id ' + inttostr(LUN);
   Registry := TRegistry.Create;
   Registry.RootKey := HKEY_LOCAL_MACHINE;

   Registry.OpenKey(FormatKey, False);
   DEVName := Registry.ReadString('Identifier');
   Registry.Free;
   Result := DEVName;
End;


Function ResetAspi(ID, Target, LUN: Integer) : Boolean;
Begin
End;


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

Function GetAspiError(Status, HaStat, TargStat: BYTE): TScsiError;
Begin
   result := Err_Unknown;
   Case Status Of
      0, 1: result := Err_None; // No error, all OK
      2, 3: result := Err_Aborted;
      $80: result := Err_InvalidRequest; // This command is
                                        // not supported by ASPI manager
      $81: result := Err_InvalidHostAdapter;
      $82: result := Err_NoDevice;
      $E0: result := Err_InvalidSrb;
      $E1: result := Err_BufferAlign;
      $E5: result := Err_AspiIsBusy;
      $E6: result := Err_BufferTooBig;
      4: Case HaStat Of
            $09: result := Err_CommandTimeout;
            $0B: result := Err_SrbTimeout;
            $0D: result := Err_MessageReject;
            $0E: result := Err_BusReset;
            $0F: result := Err_ParityError;
            $10: result := Err_RequestSenseFailed;
            $11: result := Err_SelectionTimeout;
            $12: result := Err_DataOverrun;
            $13: result := Err_UnexpectedBusFree;
            $14: result := Err_BusPhaseSequence;
            $00: Case TargStat Of
                  0, 2: result := Err_CheckCondition;
                  $08: result := Err_TargetBusy;
                  $18: result := Err_TargetReservationConflict;
                  $28: result := Err_TargetQueueFull;
               End;
         End;
   End;
End;



Function GetAspiErrorSense(Status, HaStat, TargStat: BYTE;
   Sense: PscsiSenseInfo): TScsiError;
Begin
   Result := GetAspiError(Status, HaStat, TargStat);
   If (Result = Err_CheckCondition) And Assigned(Sense) Then
      If Sense^[0] = 0
         Then Result := Err_None
      Else If (Sense^[0] And $7E) <> $70 // recognized values
         Then Result := Err_SenseUnknown
      Else Case (Sense^[2] And $0F) Of
            0: Begin // Skey_NoSense
                  If (Sense^[2] And $80) <> 0 // FileMark flag
                     Then Result := Err_SenseFileMark
                  Else If (Sense^[2] And $40) <> 0 // EndOfMedia flag
                     Then Result := Err_SenseEndOfMedia
                  Else If (Sense^[2] And $20) <> 0 // IllegalLength flag
                     Then Result := Err_SenseIllegalLength
                  Else If (Sense^[3] And $80) <> 0 // ResidualCount < 0
                     Then Result := Err_SenseIncorrectLength
                  Else Result := Err_SenseNoSense;
               End;
            1: Result := Err_SenseRecoveredError; //Skey_RecoveredError
            2: Result := Err_SenseNotReady; //Skey_NotReady
            3: Result := Err_SenseMediumError; //Skey_MediumError
            4: Result := Err_SenseHardwareError; //Skey_HardwareError
            5: Result := Err_SenseIllegalRequest; //Skey_IllegalRequest
            6: Result := Err_SenseUnitAttention; //Skey_UnitAttention
            7: Result := Err_SenseDataProtect; //Skey_DataProtect
            8: Result := Err_SenseBlankCheck; //Skey_BlankCheck
            9: Result := Err_SenseVendorSpecific; // Skey_VendorSpecific
            10: Result := Err_SenseCopyAborted; // Skey_CopyAborted
            11: Result := Err_SenseAbortedCommand; // Skey_AbortedCommand
            12: Result := Err_SenseEqual; // Skey_Equal
            13: Result := Err_SenseVolumeOverflow; // Skey_VolumeOverflow
            14: Result := Err_SenseMiscompare; // Skey_Miscompare
            15: Result := Err_SenseReserved; // Skey_Reserved
         End;
End;


Function AspiCheck(Err: TScsiError): boolean;
Begin
   Result := Err In [Err_None, Err_DataOverrun, Err_SenseRecoveredError];
End;

Function GatherDeviceID(Adapter, Target, Lun: byte; Letter: char): TBurnerID;
Begin
   Result := GatherDWORD(Adapter, Target,
      ((Lun And 7) Shl 5) Or (ORD(Letter) And $1F), 0);
End;

Function ScatterDeviceID(DeviceID: TBurnerID;
   Var Adapter, Target, Lun: byte): char;
Var Res: BYTE;
Begin
   ScatterDWORD(DeviceID, Adapter, Target, Lun, Res);
   Result := CHR((Lun And $1F) Or $40);
   Lun := (Lun Shr 5) And 7;
End;

Function DeviceIDtoLetter(DeviceID: TBurnerID): char;
Var Adapter, Target, Lun: byte;
Begin
   Result := ScatterDeviceID(DeviceID, Adapter, Target, Lun);
End;


Function ASPIgetDeviceIDflag(DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag): boolean;
Begin
  Result := (DeviceID And (1 Shl ORD(Flag))) <> 0;
End;


Procedure ASPIsetDeviceIDflag(Var DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag; Value: boolean);
Begin
   If Value
      Then DeviceID := DeviceID Or (1 Shl ORD(Flag))
   Else DeviceID := DeviceID And Not (1 Shl ORD(Flag));
End;


Function ASPIhaInquiry(HaId: BYTE; Var sh: TScsiHAinfo): TScsiError;

Begin
End;


Function ASPIgetDeviceType(DeviceID: TBurnerID;
   Var DeviceType: TScsiDeviceType): TScsiError;
Type SRB_GetDeviceType = Packed Record
      SRB_Cmd: BYTE; // ASPI command code = 1 = SC_GET_DEV_TYPE
      SRB_Status: BYTE; // ASPI command status byte
      SRB_HaId: BYTE; // ASPI host adapter number
      SRB_Flags: BYTE; // Reserved
      SRB_Hdr_Rsvd: DWORD; // Reserved
      SRB_Target: BYTE; // Target number for specified HA
      SRB_Lun: BYTE; // Logical unit number of selected target
      SRB_DeviceType: BYTE; // Selected HA/Target/Lun device type
      SRB_Rsvd: BYTE; // Reserved for alignment
   End;
Var Gsrb: SRB_GetDeviceType;
Begin
   FillChar(Gsrb, sizeof(Gsrb), 0);
   Gsrb.SRB_Cmd := 1;
   ScatterDeviceID(DeviceID, Gsrb.SRB_HaId, Gsrb.SRB_Target, Gsrb.SRB_Lun);
//   SendASPI32Command(@Gsrb);
   Result := GetAspiError(Gsrb.SRB_Status, $FF, $FF);
   If (Result = Err_None) And (Gsrb.SRB_DeviceType < ORD(TSDInvalid))
      Then DeviceType := TScsiDeviceType(Gsrb.SRB_DeviceType)
   Else DeviceType := TSDInvalid;
End;







Procedure GetDriveHandle(var DeviceID: TCDBurnerInfo);
var
  fh : THandle;
  buf2 : array[0..31] of Char;
  DriveLetter : Char;
  dwFlags : DWord;
begin
  dwFlags := GENERIC_READ;
  if getOsVersion >= OS_WIN2K then dwFlags := dwFlags or GENERIC_WRITE;
   DriveLetter := DeviceIDtoLetter(DeviceID.DriveID);
   StrPCopy( @buf2, Format( '\\.\%s:', [DriveLetter] ) );
  fh := CreateFile( buf2, dwFlags, FILE_SHARE_READ Or FILE_SHARE_WRITE, nil,OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if fh = INVALID_HANDLE_VALUE then
  begin
    showmessage('cannot use need admin');
    CloseHandle( fh );
    Exit;
  end; 
end;




Function GetDriveTempHandle(DeviceID: TCDBurnerInfo) :Thandle;
var
  DriveLetter : Char;
begin
  DriveLetter := DeviceIDtoLetter(DeviceID.DriveID);
  Result := CreateFile( PChar('\\.\'+ DriveLetter +':'),
                        GENERIC_READ Or GENERIC_WRITE, 
                        FILE_SHARE_READ Or FILE_SHARE_WRITE, 
                        Nil, 
                        OPEN_EXISTING, 
                        FILE_ATTRIBUTE_NORMAL, 
                        0); 
end;


Function CloseDriveHandle(DeviceID: TCDBurnerInfo): Boolean;
begin
  Result := CloseHandle(DeviceID.SptiHandle);
end;





{seperate test function}
Function ASPIsendScsiCommandInternal(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
var
  status : Byte;
  dwFlags : Cardinal;
  ErrorInt : Integer;
  skSCSI : TskSCSI;
  CDB : TCDB;
  CDBSize : Cardinal;
begin
  Result := Err_None;
  skSCSI := TskSCSI.Create;
  if skSCSI.InitOK then
  begin
   CDBSize := CDBLen;
   Move(TCDB(pcdb^),CDB,CDBSize);
   dwFlags := Direction;
   status := skSCSI.ExecCmd(Deviceid.HaId,DeviceID.Target,DeviceID.Lun,CDB,CDBSize,dwFlags,pbuf,BufLen);
   skSCSI.Destroy;
  end;

 // Move(TCDB12(Pcdb^), pswb^.spt.Cdb, pswb^.spt.CdbLength);

  if not status = 0 then
  begin
    ErrorInt := GetLastError;
    Result := Err_Unknown;
    Exit;
  end;
end;






Function ASPIsendScsiCommand(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Begin
   Result := Err_None;
   FillChar(Sdf.Sense, sizeof(TscsiSenseInfo), 0);
   If Assigned(Sdf.fOnCommandSending)
      Then Sdf.fOnCommandSending(DeviceID, Pcdb, CdbLen, Pbuf, BufLen,
         Direction, @Sdf, Result);

   Result := ASPIsendScsiCommandInternal(DeviceID,
           Pcdb, CdbLen, Pbuf, BufLen, Direction, Sdf);

   If Assigned(Sdf.fOnCommandSent)
      Then Sdf.fOnCommandSent(DeviceID, Pcdb, CdbLen, Pbuf, BufLen,
         Direction, @Sdf, Result);
End;




Procedure ASPIabortCommand(HaId: BYTE; Psrb: pointer);
Type SRB_Abort = Packed Record
      SRB_Cmd: BYTE; // ASPI command code = 3 = SC_ABORT_SRB
      SRB_Status: BYTE; // ASPI command status byte
      SRB_HaId: BYTE; // ASPI host adapter number
      SRB_Flags: BYTE; // Reserved
      SRB_Hdr_Rsvd: DWORD; // Reserved
      SRB_ToAbort: pointer; // Pointer to SRB to abort
   End;
Var Asrb: SRB_Abort;
Begin
   FillChar(Asrb, sizeof(Asrb), 0);
   Asrb.SRB_Cmd := 3;
   Asrb.SRB_HaId := HaId;
   Asrb.SRB_ToAbort := Psrb;
//   SendASPI32Command(@Asrb);
End;


Function ASPIresetDevice(DeviceID: TCDBurnerInfo; Timeout: DWORD): TScsiError;
Type SRB_ResetDevice = Packed Record
      SRB_Cmd: BYTE; // ASPI command code = 4 = SC_RESET_DEV
      SRB_Status: BYTE; // ASPI command status byte
      SRB_HaId: BYTE; // ASPI host adapter number
      SRB_Flags: BYTE; // Reserved
      SRB_Hdr_Rsvd: DWORD; // Reserved
      SRB_Target: BYTE; // Target's SCSI ID
      SRB_Lun: BYTE; // Target's LUN number
      SRB_Rsvd1: Array[0..11] Of BYTE; // Reserved for Alignment
      SRB_HaStat: BYTE; // Host Adapter Status
      SRB_TargStat: BYTE; // Target Status
      SRB_PostProc: THandle; // Post routine
      SRB_Rsvd2: POINTER; // Reserved
      SRB_Rsvd3: Array[0..31] Of BYTE; // Reserved for alignment
   End;
Var
   Rsrb: SRB_ResetDevice;
   hEvent: THandle;
Begin
   Result := Err_None;
   hEvent := CreateEvent(Nil, true, false, Nil); // event to notify completion
   If hEvent = 0 Then Begin Result := Err_NoEvent; exit; End;
   ResetEvent(hEvent);
   FillChar(Rsrb, sizeof(Rsrb), 0);
   With Rsrb Do Begin
      SRB_Cmd := 4; //  SC_RESET_DEV
      ScatterDeviceID(DeviceID.DriveID, SRB_HaId, SRB_Target, SRB_Lun);
      SRB_PostProc := hEvent;
   End;
{   If SendASPI32Command(@Rsrb) = 0 Then Begin // SS_PENDING
      If WaitForSingleObject(hEvent, Timeout) <> WAIT_OBJECT_0
         Then Begin
         Result := Err_NotifyTimeout;
         ASPIabortCommand(Rsrb.SRB_HaId, @Rsrb);
      End;
   End Else Result := Err_NoDevice; 
   }

   CloseHandle(hEvent);
   If Result = Err_None Then With Rsrb Do
         Result := GetAspiError(SRB_Status, SRB_HaStat, SRB_TargStat);
End;


Function ASPIgetDriveInt13info(DeviceID: TCDBurnerInfo;
   Var Info: TScsiInt13info): TScsiError;
Type SRB_Int13info = Packed Record
      SRB_Cmd: BYTE; // ASPI command code=6=SC_GET_DISK_INFO
      SRB_Status: BYTE; // ASPI command status byte
      SRB_HaId: BYTE; // ASPI host adapter number
      SRB_Flags: BYTE; // Reserved
      SRB_Hdr_Rsvd: DWORD; // Reserved
      SRB_Target: BYTE; // Target's SCSI ID
      SRB_Lun: BYTE; // Target's LUN number
      SRB_DriveFlags: BYTE; // Driver flags
      SRB_Int13Drive: BYTE; // Host Adapter Status
      SRB_Heads: BYTE; // Preferred number of heads translation
      SRB_Sectors: BYTE; // Preferred number of sectors translation
      SRB_Rsvd: Array[0..9] Of BYTE; // Reserved
   End;
Var Isrb: SRB_Int13info;
Begin
   FillChar(Isrb, sizeof(Isrb), 0);
   With Isrb Do Begin
      SRB_Cmd := 6;
      ScatterDeviceID(DeviceID.DriveID, SRB_HaId, SRB_Target, SRB_Lun);
   End;
//   SendASPI32Command(@Isrb);
   With Info, Isrb Do Begin
      Result := GetAspiError(SRB_Status, $FF, $FF);
      Support := (Result = Err_None) And ((SRB_DriveFlags And 3) <> 0);
      DosSupport := (Result = Err_None) And ((SRB_DriveFlags And 1) <> 0);
      DriveNumber := SRB_Int13Drive;
      Heads := SRB_Heads;
      Sectors := SRB_Sectors;
   End; End;



function GetSPTICdRomDrives( var CdRoms : TSPTIWriters ) : Boolean;
var
  hEvent : THandle;
  HaId, Target, Lun : Byte;
  HostCount : Integer;
  TARG : Byte;
  Index : integer;
begin
  Result := False;
      if GetDriveNumbers( CdRoms ) > 0 then
      begin
        for Index := 0 to CdRoms.CdRomCount - 1 do
        begin
          GetDriveInformation( Index, CdRoms );
        end;
        Result := True;
      end;
end;



procedure GetDriveInformation( i : byte; var CdRoms : TSPTIWriters );
var
  fh : THandle;
  buf : array[0..1023] of Char;
  buf2 : array[0..31] of Char;
  status : Bool;
  pswb : PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  pscsiAddr : PSCSI_ADDRESS;
  length, returned : integer;
  inqData : array[0..99] of Char; // was array[0..99] of Byte;
  dwFlags : DWord;
  DriveString : PChar;
begin
  dwFlags := GENERIC_READ;
  if getOsVersion >= OS_WIN2K then dwFlags := dwFlags or GENERIC_WRITE;
  StrPCopy( @buf2, Format( '\\.\%s:', [CdRoms.CdRom[i].DriveLetter] ) );
  fh := CreateFile( buf2, dwFlags, FILE_SHARE_READ Or FILE_SHARE_WRITE, nil,OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if fh = INVALID_HANDLE_VALUE then
  begin
    // It seems that with no Administrator privileges
    // the handle value will be invalid
    Exit;
  end;

  (*
   * Get the drive inquiry data
   *)
  ZeroMemory( @buf, 1024 );
  ZeroMemory( @inqData, 100 );
  pswb := PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER( @buf );
  pswb^.spt.Length := sizeof( SCSI_PASS_THROUGH );
  pswb^.spt.CdbLength := 6;
  pswb^.spt.SenseInfoLength := 24;
  pswb^.spt.DataIn := SCSI_IOCTL_DATA_IN;
  pswb^.spt.DataTransferLength := 100;
  pswb^.spt.TimeOutValue := 2;
  pswb^.spt.DataBuffer := @inqData;
  pswb^.spt.SenseInfoOffset := SizeOf( pswb^.spt ) + SizeOf( pswb^.Filler );
  pswb^.spt.Cdb[0] := $12;
  pswb^.spt.Cdb[4] := $64;

  length := sizeof( SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER );
  status := DeviceIoControl( fh,
    IOCTL_SCSI_PASS_THROUGH_DIRECT,
    pswb,
    length,
    pswb,
    length,
    Cardinal( returned ),
    nil );

  if not status then
  begin
   // CloseHandle( fh );
    Exit;
  end;

  DriveString := @inqData;
  Inc( DriveString, 8 );

  CdRoms.CdRom[i].Vendor := Copy( DriveString, 1, 8 ); // Vendor
  CdRoms.CdRom[i].ProductId := Copy( DriveString, 8 + 1, 16 );
  // Product ID
  CdRoms.CdRom[i].Revision := Copy( DriveString, 24 + 1, 4 );
  // Revision
  CdRoms.CdRom[i].VendorSpec := Copy( DriveString, 28 + 1, 20 );
  // Vendor Spec.
  CdRoms.CdRom[i].Description := CdRoms.CdRom[i].Vendor +
  CdRoms.CdRom[i].ProductId + CdRoms.CdRom[i].Revision;
  CdRoms.CdRom[i].DriveHandle := fh;
  (*
   * get the address (path/tgt/lun) of the drive via IOCTL_SCSI_GET_ADDRESS
   *)
  ZeroMemory( @buf, 1024 );
  pscsiAddr := PSCSI_ADDRESS( @buf );
  pscsiAddr^.Length := sizeof( SCSI_ADDRESS );
  if ( DeviceIoControl( fh, IOCTL_SCSI_GET_ADDRESS, nil, 0,
    pscsiAddr, sizeof( SCSI_ADDRESS ), Cardinal( returned ),
    nil ) ) then
  begin
    CDRoms.CdRom[i].HaId := pscsiAddr^.PortNumber;
    CDRoms.CdRom[i].Target := pscsiAddr^.TargetId;
    CDRoms.CdRom[i].Lun := pscsiAddr^.Lun;
  end
  else
  begin
    Exit;
  end;

 // CloseHandle( fh );
end;



Function ASPIsend6(DeviceID: TCDBurnerInfo;
   OpCode: BYTE; Lba: DWORD; Byte4: BYTE;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Var cdb: Array[0..5] Of BYTE;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);

   cdb[5] := 0;
   cdb[4] := Byte4;
   FillDWORD(LBA, cdb[0]);
   cdb[1] := AttachLUN(cdb[1], DeviceID.DriveID);
   cdb[0] := OpCode;
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 6,
                Pbuf, BufLen, Direction, Sdf);
End;


Function ASPIsend10(DeviceID: TCDBurnerInfo; OpCode: BYTE;
   Byte1: BYTE; Lba: DWORD; Byte6: BYTE; Word7: WORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Var cdb: Array[0..9] Of BYTE;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);

   cdb[9] := 0;
   FillWORD(Word7, cdb[7]);
   cdb[6] := Byte6;
   FillDWORD(LBA, cdb[2]);
   cdb[1] := AttachLUN(Byte1, DeviceID.DriveID);
   cdb[0] := OpCode;
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 10,
      Pbuf, BufLen, Direction, Sdf);
End;




Function ASPIsend12(DeviceID: TCDBurnerInfo; OpCode: BYTE;
   Byte1: BYTE; Lba: DWORD; TLength: DWORD; Byte10: BYTE;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Var cdb: Array[0..11] Of BYTE;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);
   cdb[11] := 0;
   cdb[10] := Byte10;
   FillDWORD(TLength, cdb[6]);
   FillDWORD(LBA, cdb[2]);
   cdb[1] := AttachLUN(Byte1, DeviceID.DriveID);
   cdb[0] := OpCode;
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 12,
      Pbuf, BufLen, Direction, Sdf);
End;


Function ASPIsend12CDB(DeviceID: TCDBurnerInfo; CDB :TCDB12; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 12, Pbuf, BufLen, Direction, Sdf);
End;


Function ASPIsend10CDB(DeviceID: TCDBurnerInfo; CDB :TCDB10; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 10, Pbuf, BufLen, Direction, Sdf);
End;



Function ASPIsend6CDB(DeviceID: TCDBurnerInfo; CDB :TCDB6; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Begin
   If Assigned(Pbuf) And (Direction = SRB_DIR_IN)
      Then FillChar(Pbuf^, BufLen, 0);
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 6,  Pbuf, BufLen, Direction, Sdf);
End;




End.

