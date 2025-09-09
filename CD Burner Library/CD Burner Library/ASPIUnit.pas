

unit ASPIUnit;

interface

Uses windows,wnaspi32,registry,scsidefs,SCSITypes,sysutils;




type
   {CDB types for Aspi commands}
   TCDB12 = Array[0..11] Of BYTE;
   TCDB10 = Array[0..9] Of BYTE;
   TCDB6  = Array[0..5] Of BYTE;



   TScsiInt13info = Packed Record
         Support,
         DosSupport: BOOLEAN;
         DriveNumber,
         Heads,
         Sectors: BYTE;
   End; 




Type TAspiDeviceEnumCallBack
   = Function(Caller : pointer; Device: TCDBurnerInfo; FoundName :String): boolean;



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






Function ScatterCDDevice(CDDevice: DWORD; Var Adapter, Target, Lun: byte): char;
Function CDDevicetoLetter(CDDevice: DWORD): char;
Procedure ScatterDWORD(Arg: DWORD; Var b3, b2, b1, b0: byte);
Function GatherDeviceID(Adapter, Target, Lun: byte; Letter: char): TBurnerID;
Procedure FillWORD(Src: WORD; Var Dst: BYTE);
Procedure FillDWORD(Src: DWORD; Var Dst: BYTE);
Function AttachLUN(Var Arg: BYTE; DeviceID: TBurnerID): BYTE;
Function ScatterDeviceID(DeviceID: TBurnerID;
   Var Adapter, Target, Lun: byte): char;


Function AspiEnumDevices(CallBack: TAspiDeviceEnumCallBack; Caller: pointer): integer;

Function AspiCheck(Err: TScsiError): boolean;
function AspiInstalled : Integer;
Function GetAdapterNumbers: Integer;
Function ASPIhaInquiry(HaId: BYTE; Var sh: TScsiHAinfo): TScsiError;
Function GetCDRegName(ID, Target, LUN: Integer): String;
Function BigEndianW(Arg: WORD): WORD;
Function BigEndianD(Arg: DWORD): DWORD;
Procedure BigEndian(Const Source; Var Dest; Count: integer);
Function GatherWORD(b1, b0: byte): WORD;
Function GatherDWORD(b3, b2, b1, b0: byte): DWORD;
Procedure ASPIstrCopy(Src: PChar; Var Dst: ShortString; Leng: Integer);



Procedure ASPIsetDeviceIDflag(Var DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag; Value: boolean);

Function ASPIgetDeviceType(DeviceID: TBurnerID;
   Var DeviceType: TScsiDeviceType): TScsiError;

Function ASPIgetDriveInt13info(DeviceID: TBurnerID;
   Var Info: TScsiInt13info): TScsiError;

Function ASPIgetDeviceIDflag(DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag): boolean;   


Function GetAspiErrorSense(Status, HaStat, TargStat: BYTE;
   Sense: PscsiSenseInfo): TScsiError;
   
Procedure ASPIabortCommand(HaId: BYTE; Psrb: pointer);

Function ASPIsendScsiCommand(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;


Function ASPIsend6CDB(DeviceID: TCDBurnerInfo; CDB :TCDB6; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend10CDB(DeviceID: TCDBurnerInfo; CDB :TCDB10; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Function ASPIsend12CDB(DeviceID: TCDBurnerInfo; CDB :TCDB12; Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;



implementation


Uses SCSIUnit;



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



Function GetAspiError(Status, HaStat, TargStat: BYTE): TScsiError;
Begin
   result := Err_Unknown;
   Case Status Of
      0, 1: result := Err_None;
      2, 3: result := Err_Aborted;
      $80: result := Err_InvalidRequest;
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


Procedure ASPIstrCopy(Src: PChar; Var Dst: ShortString; Leng: Integer);
Var i: integer;
Begin
   i := 0;
   While (i < Leng) And (Src[i] >= ' ') Do
   Begin Dst[i + 1] := Src[i]; inc(i); End;
   While (i > 0) And (Dst[i] = ' ') Do Dec(i); // Trim it Right
   Dst[0] := CHR(i);
End;


Procedure ScatterDWORD(Arg: DWORD; Var b3, b2, b1, b0: byte);
Begin
   b3 := (Arg Shr 24) And $FF;
   b2 := (Arg Shr 16) And $FF;
   b1 := (Arg Shr 8) And $FF;
   b0 := Arg And $FF;
End;


Function ScatterDeviceID(DeviceID: TBurnerID;
   Var Adapter, Target, Lun: byte): char;
Var Res: BYTE;
Begin
   ScatterDWORD(DeviceID, Adapter, Target, Lun, Res);
   Result := CHR((Lun And $1F) Or $40);
   Lun := (Lun Shr 5) And 7;
End;


Function ScatterCDDevice(CDDevice: DWord; Var Adapter, Target, Lun: byte): char;
Var Res: BYTE;
Begin
   ScatterDWORD(CDDevice, Adapter, Target, Lun, Res);
   Result := CHR((Lun And $1F) Or $40);
   Lun := (Lun Shr 5) And 7;
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


Function CDDevicetoLetter(CDDevice: DWord): char;
Var Adapter, Target, Lun: byte;
Begin
   Result := ScatterCDDevice(CDDevice, Adapter, Target, Lun);
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


Function ASPIgetDeviceIDflag(DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag): boolean;
Begin
   Result := (DeviceID And (1 Shl ORD(Flag))) <> 0; 
End;



Function GetAdapterNumbers: Integer;
Var
   AspiStatus: DWord;
   Adaptors: Byte;
Begin
   Result := 0;
   Try
      AspiStatus := GetASPI32SupportInfo;
      Adaptors := Lo(loword(AspiStatus));
      Result := Adaptors;
   Except
      Result := 0;
   End;
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




Function AspiEnumDevices(CallBack: TAspiDeviceEnumCallBack; Caller: pointer): integer;
Var
   DID: TCDBurnerInfo;
   DIDtype: TScsiDeviceType;
   Dadapter, Dtarget, Dlun, HAcount: BYTE;
   HAinfo: TScsiHAinfo;
   DevInfo: TScsiInt13info;
   CDName : String;
   ModeSenseBuf: Array[0..255] Of BYTE;

   Function TestModeSense: boolean;
   Begin
//      Result := Not AspiCheck(SCSImodeSense(DID, $3F, @ModeSenseBuf, 255, SCSI_Def));
   End;

Begin
   Result := 0;
   HAcount := GetAdapterNumbers;
   If HAcount = 0 // no ASPI hosts, no devices
      Then
      Begin
           Result := -1;
           exit;
      End;
   For Dadapter := 0 To HAcount - 1 Do
      If ASPIhaInquiry(Dadapter, HAinfo) = Err_None Then
         For Dtarget := 0 To HAinfo.MaxTargetCount - 1 Do
            For Dlun := 0 To 7 Do
            Begin

               DID.DriveID := GatherDeviceID(Dadapter, Dtarget, Dlun, #0);
               CDName := GetCDRegName(Dadapter,Dtarget,Dlun);

               If ASPIgetDeviceType(DID.DriveID, DIDtype) = Err_None Then //if device exists
                  If (DIDtype = TSDCDROM) Then
                  Begin

                  If (ASPIgetDriveInt13info(DID.DriveID, DevInfo) = Err_None)
                        And (DevInfo.DriveNumber > 0)
                        Then DID.DriveID := GatherDeviceID(Dadapter, Dtarget, Dlun,CHR(DevInfo.DriveNumber + $41));

                  If TestModeSense Then
                     Begin
                        ASPIsetDeviceIDflag(DID.DriveID, ADIDmodeSense6, True);
                        If TestModeSense Then
                        Begin
                           ASPIsetDeviceIDflag(DID.DriveID, ADIDmodeSense6, False);
                           ASPIsetDeviceIDflag(DID.DriveID, ADIDmodeSenseDBD, True);
                           If TestModeSense Then
                              ASPIsetDeviceIDflag(DID.DriveID, ADIDmodeSense6, True);
                        End;
                     End;
                     If Not CallBack(Caller, DID, CDName) Then exit;
                     Inc(Result);
                  End;
            End;
End;





Procedure ASPIsetDeviceIDflag(Var DeviceID: TBurnerID;
   Flag: TAspiDeviceIDflag; Value: boolean);
Begin
   If Value
      Then DeviceID := DeviceID Or (1 Shl ORD(Flag))
   Else DeviceID := DeviceID And Not (1 Shl ORD(Flag));
End;





Function ASPIgetDeviceType(DeviceID: TBurnerID;
   Var DeviceType: TScsiDeviceType): TScsiError;
Var
  Gsrb: SRB_GetDeviceType;
Begin
   FillChar(Gsrb, sizeof(Gsrb), 0);
   Gsrb.SRB_Cmd := 1;
   ScatterDeviceID(DeviceID, Gsrb.SRB_HaId, Gsrb.SRB_Target, Gsrb.SRB_Lun);
   SendASPI32Command(@Gsrb);
   Result := GetAspiError(Gsrb.SRB_Status, $FF, $FF);
   If (Result = Err_None) And (Gsrb.SRB_DeviceType < ORD(TSDInvalid))
      Then DeviceType := TScsiDeviceType(Gsrb.SRB_DeviceType)
   Else DeviceType := TSDInvalid;
End;




Function ASPIgetDriveInt13info(DeviceID: TBurnerID;
   Var Info: TScsiInt13info): TScsiError;
Var
   Isrb: SRB_Int13info;
Begin
   FillChar(Isrb, sizeof(Isrb), 0);
   With Isrb Do
    Begin
      SRB_Cmd := 6;
      ScatterDeviceID(DeviceID, SRB_HaId, SRB_Target, SRB_Lun);
    End;
   SendASPI32Command(@Isrb);
   With Info, Isrb Do
    Begin
      Result := GetAspiError(SRB_Status, $FF, $FF);
      Support := (Result = Err_None) And ((SRB_DriveFlags And 3) <> 0);
      DosSupport := (Result = Err_None) And ((SRB_DriveFlags And 1) <> 0);
      DriveNumber := SRB_Int13Drive;
      Heads := SRB_Heads;
      Sectors := SRB_Sectors;
   End;
End;



Function ASPIhaInquiry(HaId: BYTE; Var sh: TScsiHAinfo): TScsiError;
Var
 Isrb: SRB_Inquiry;
Begin
   FillChar(Isrb, sizeof(Isrb), 0);
   Isrb.SRB_Cmd := 0;
   Isrb.SRB_HaId := HaId;
   SendASPI32Command(@Isrb);
   With Isrb Do
    Begin
      Result := GetAspiError(SRB_Status, $FF, $FF);
      sh.ScsiId := SRB_HA_SCSIID;
      ASPIstrCopy(SRB_ManagerID, sh.ScsiManagerId, 16);
      ASPIstrCopy(SRB_AdapterID, sh.HostAdapterId, 16);
      sh.BufferAlignMask := SRB_BufAlign;
      sh.ResidualSupport := (SRB_Residual And 2) <> 0;
      If SRB_Targets = 0 Then sh.MaxTargetCount := 8
      Else sh.MaxTargetCount := SRB_Targets;
      sh.MaxTransferLength := SRB_TransfLen;
   End;
End;


Function ResetAspi(ID, Target, LUN: Integer) : Boolean;
Var
   AdaptorSRB: PSRB_GDEVBlock;
   ASPI_Status: DWord;
Begin
   result := False;
   New(AdaptorSRB);
   FillChar(AdaptorSRB^,Sizeof(SRB_HAInquiry),#0);
   AdaptorSRB^.SRB_Cmd := SC_RESET_DEV;
   AdaptorSRB^.SRB_HaId := ID;
   AdaptorSRB^.SRB_Target := Target;
   AdaptorSRB^.SRB_Lun := LUN;
   AdaptorSRB^.SRB_Flags := 0;
   AdaptorSRB^.SRB_Hdr_Rsvd := 0;
   ASPI_Status := SendASPI32Command(AdaptorSRB);

   If AdaptorSRB^.SRB_Status <> SS_COMP Then
        result := False
      else
        result := True;
   Dispose(AdaptorSRB);
End;




Function GetAdaptorName(ID: Integer): String;
Var
   AdaptorSRB: PSRB_HAInquiry;
   ASPI_Status: DWord;
   Res: String;
Begin
   setlength(Res, 16);
   New(AdaptorSRB);
   FillChar(AdaptorSRB^,Sizeof(SRB_HAInquiry),#0);
   AdaptorSRB^.SRB_Cmd := SC_HA_INQUIRY;
   AdaptorSRB^.SRB_HaId := ID;
   AdaptorSRB^.SRB_Flags := 0;
   AdaptorSRB^.SRB_Hdr_Rsvd := 0;
   ASPI_Status := SendASPI32Command(AdaptorSRB);

   If AdaptorSRB^.SRB_Status <> SS_COMP Then
      RES := 'Inquery Error'
   Else
   Begin
      Res := AdaptorSRB^.HA_Identifier;
   End;
   Result := Res;
   Dispose(AdaptorSRB);
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

   Result := ASPIsendScsiCommand(DeviceID, @cdb, 6, Pbuf, BufLen, Direction, Sdf);
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
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 10, Pbuf, BufLen, Direction, Sdf);
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
   Result := ASPIsendScsiCommand(DeviceID, @cdb, 12,Pbuf, BufLen, Direction, Sdf);
End;




Function ASPIsendScsiCommandInternal(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;

Var
   Esrb: SRB_ExecSCSICmd;
   hEvent: THandle;
Begin
   Result := Err_None;
   hEvent := CreateEvent(Nil, true, false, Nil); // event to notify completion
   If hEvent = 0 Then
    Begin
     Result := Err_NoEvent;
     exit;
    End;
   ResetEvent(hEvent);
   FillChar(Esrb, sizeof(Esrb), 0); // Scsi Request Block init
   With Esrb Do Begin
      SRB_Cmd := 2; // SC_EXEC_SCSI_CMD
      ScatterDeviceID(DeviceID.DriveID, SRB_HaId, SRB_Target, SRB_Lun);
      SRB_Flags := Direction Or $40; // set SRB_EVENT_NOTIFY flag
      SRB_BufLen := BufLen;
      SRB_BufPtr := Pbuf;
      SRB_SenseLen := sizeof(TscsiSenseInfo) - 2;
      If CdbLen > 16 Then SRB_CDBLen := 16 Else SRB_CDBLen := CdbLen;
      SRB_PostProc := hEvent;
      Move(Pcdb^, SRB_CDBByte, SRB_CDBLen);
   End;
      SendASPI32Command(@Esrb); // send command to aspi
   If Esrb.SRB_Status = 0 Then
    Begin // signaled SS_PENDING  >> WAIT !
      If WaitForSingleObject(hEvent, Sdf.Timeout) <> WAIT_OBJECT_0 Then
       Begin
         Result := Err_NotifyTimeout;
         ASPIabortCommand(Esrb.SRB_HaId, @Esrb);
      End;
   End;
   if Esrb.SRB_Status <> 1 then Result := Err_NoDevice;

   CloseHandle(hEvent);
   If Result = Err_None Then With Esrb Do
    Begin
         Sdf.Sense := SRB_Sense;
         Result := GetAspiErrorSense(SRB_Status, SRB_HaStat,
            SRB_TargStat, @SRB_Sense);
      End;
End;


Function ASPIsendScsiCommand(DeviceID: TCDBurnerInfo;
   Pcdb: pointer; CdbLen: DWORD;
   Pbuf: pointer; BufLen: DWORD;
   Direction: DWORD; Var Sdf: TScsiDefaults): TScsiError;
Begin
   Result := Err_None;
   FillChar(Sdf.Sense, sizeof(TscsiSenseInfo), 0);

   Result := ASPIsendScsiCommandInternal(DeviceID,
      Pcdb, CdbLen, Pbuf, BufLen, Direction, Sdf);

   If Assigned(Sdf.fOnCommandSent)
      Then Sdf.fOnCommandSent(DeviceID, Pcdb, CdbLen, Pbuf, BufLen, Direction, @Sdf, Result);
End;


Procedure ASPIabortCommand(HaId: BYTE; Psrb: pointer);
Var
  Asrb: SRB_Abort;
Begin
   FillChar(Asrb, sizeof(Asrb), 0);
   Asrb.SRB_Cmd := 3;
   Asrb.SRB_HaId := HaId;
   Asrb.SRB_ToAbort := Psrb;
   SendASPI32Command(@Asrb);
End;






end.
 