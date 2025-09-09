unit SPTI;
{$DEFINE ___DEBUGINFO}
{$IFNDEF ___DEBUGINFO}
  {$D-}
  {$O+}
  {$L-}{$Y-}
  {$REFERENCEINFO OFF}{DEFINITIONINFO OFF}
{$ENDIF}
{$ALIGN ON}

interface
uses
  Windows;
var
  tgn: byte = 0;
  CloseFH: Boolean = True;
  inqData: array[0..1024] of char = 'CD\DVD Tools component BlackCash Soft';
  {$I SPTI.inc}
  const s= 0;

function GetFileHandle(i: Byte): LongWord;
var
  buf: string;
begin
  buf := '\\.\'+char(i+65)+':'#0;
  result := CreateFile (@buf[1], GENERIC_WRITE or GENERIC_READ, FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0 );
  if ( result = INVALID_HANDLE_VALUE ) then
    result := CreateFile(@buf[1], GENERIC_READ, FILE_SHARE_READ, nil, OPEN_EXISTING, 0, 0 );

end;

//*********************************************************************************************************************
procedure GetDriveInformation( i: Byte; var pDrive: TSCSIDRIVE );
var
  fh: THandle;
  buf: array[0..1023] of char;
  pswb: PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  pscsiAddr: PSCSI_ADDRESS;
  length, returned: Cardinal;
  status: Boolean;
begin
  fh := GetFileHandle( i );
  if ( fh = INVALID_HANDLE_VALUE ) then
    exit;
  ZeroMemory(@buf, 1024 );
  ZeroMemory(@inqData, sizeof(inqData));
  pswb                      := @buf;
  pswb.spt.Length          := sizeof(SCSI_PASS_THROUGH);
  pswb.spt.CdbLength       := 6;
  pswb.spt.SenseInfoLength := 24;
  pswb.spt.DataIn          := SCSI_IOCTL_DATA_IN;
  pswb.spt.DataTransferLength := 96;
  pswb.spt.TimeOutValue    := 120;
  pswb.spt.DataBuffer      := @inqData;
  pswb.spt.SenseInfoOffset := 48;
  pswb.spt.Cdb[0]          := SCSI_INQUIRY;
  pswb.spt.Cdb[4]          := 96;

  length := sizeof(SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER);
  status := DeviceIoControl( fh, IOCTL_SCSI_PASS_THROUGH_DIRECT, pswb, length, pswb, length, returned, nil );
  if ( not status ) then
  begin
    CloseHandle( fh );
    exit;
  end;

  move( inqData, pDrive.inqData, 40);
  FillChar(buf, Sizeof(buf), 0);
  pscsiAddr := @buf;
  pscsiAddr.Length := sizeof(SCSI_ADDRESS);
  if ( DeviceIoControl( fh, IOCTL_SCSI_GET_ADDRESS, nil, 0, pscsiAddr, sizeof(SCSI_ADDRESS), returned, nil ) ) then
  begin
    pDrive.Used    := TRUE;
    pDrive.ha      := pscsiAddr.PortNumber;
    pDrive.Target  := pscsiAddr.TargetId;
    pDrive.LUN     := pscsiAddr.Lun;
    pDrive.Drive   := chr(i);
    pDrive.DeviceHandle := INVALID_HANDLE_VALUE;
  end
  else
  begin
    pDrive.Used    := TRUE;
    pDrive.ha      := 0;
    pDrive.Target  := tgn;
    pDrive.LUN     := 250;
    pDrive.Drive   := chr(i);
    pDrive.DeviceHandle := INVALID_HANDLE_VALUE;
    inc(tgn);
  end;
  CloseHandle( fh );

end;

function NtHandleHaInquiry(xpsrb: Pointer): DWord;
var
  lpsrb: PSRB_HAInquiry;
begin
  lpsrb := xpsrb;
  lpsrb.HA_Count    := ScsiDrives.numAdapters;
  if ( lpsrb.HaId >= ScsiDrives.numAdapters ) then
  begin
    lpsrb.Status := SS_INVALID_HA;
    result := SS_INVALID_HA;
    exit;
  end;
  lpsrb.HA_SCSI_ID := 7;
  lpsrb.HA_ManagerId  := 'BlackCashSoft';
  lpsrb.HA_Identifier := 'SPTI  '#0#0#0#0#0#0#0#0#0;
  lpsrb.HA_Identifier[5] := char($30+lpsrb.HaId);
  FillChar(lpsrb.HA_Unique, 13, 0 );
  lpsrb.HA_Unique[0] := #7;
  lpsrb.HA_Unique[3] := #8;
  lpsrb.HA_Unique[4] := #00;
  lpsrb.HA_Unique[5] := #00;
  lpsrb.HA_Unique[6] := #$FF;
  lpsrb.Status := SS_COMP;
  result := SS_COMP;
end;

function GetDeviceIndex( ha, tgt, lun: Byte ): Byte;
var
  i: Byte;
  lpd: TSCSIDRIVE;
begin
  for i := 2 to 26 do
  begin
    if ( ScsiDrives.drive[i].Used ) then
    begin
      lpd := ScsiDrives.drive[i];
      if ( (lpd.ha = ha) and (lpd.Target = tgt) and (lpd.LUN = lun) ) then
      begin
        result := i;
        exit;
      end;
    end
  end;
  result := 0;
end;

function GetDeviceType( xlpsrb: Pointer): DWORD;
var
  lpsrb: PSRB_GDEVBlock;
begin
  lpsrb := xlpsrb;
  lpsrb.Status := SS_NO_DEVICE;
  if ( GetDeviceIndex( lpsrb.HaId, lpsrb.Target, lpsrb.Lun ) <> 0 ) then
    lpsrb.Status := SS_COMP;
  if ( lpsrb.Status = SS_COMP ) then
    lpsrb.DeviceType := DTYPE_CDROM
  else
    lpsrb.DeviceType := DTYPE_UNKNOWN;

  result := lpsrb.Status;
end;

function SendASPI32Command(lpsrb: PSRB_EXECSCSICMD; Again: Boolean): DWORD;
var
  status: Boolean;
  swb: SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  Length, Returned: Cardinal;
  index: Byte;
label ExecCommand;
begin
  if ( lpsrb = nil ) then
  begin
    result := SS_ERR;
    exit;
  end;

  case lpsrb.Command  of
    SC_EXEC_SCSI_CMD:
      goto ExecCommand;

    SC_HA_INQUIRY:
    begin
      result :=  NtHandleHaInquiry( lpsrb );
      exit;
    end;

    SC_GET_DEV_TYPE:
    begin
      result := GetDeviceType( lpsrb );
      exit;
    end;

    SC_GET_DISK_INFO:
    begin
      //result := GetDeviceType( lpsrb );
      lpsrb.Status := $0;
      result := $4;
      exit;
    end;

    else
    begin
      lpsrb.Status := SS_ERR;
      result := SS_ERR;
      exit;
    end;
  end;

ExecCommand:
  index := GetDeviceIndex( lpsrb.HaId, lpsrb.Target, lpsrb.Lun );
  if ( index = 0 ) then
  begin
    lpsrb.Status := SS_NO_DEVICE;
    result := SS_NO_DEVICE;
    exit;
  end;

  if ( lpsrb.CDBCmd = SCSI_INQUIRY ) then
  begin
    if ( lpsrb.HaId >= ScsiDrives.numAdapters ) then
    begin
      lpsrb.Status := SS_INVALID_HA;
      result := SS_INVALID_HA;
      exit;
    end;
    lpsrb.Status := SS_COMP;
    move(ScsiDrives.drive[index].inqData, lpsrb.BufPointer[0], 36);
    result := SS_COMP;
    exit;
  end;

  if ( ScsiDrives.drive[index].DeviceHandle = INVALID_HANDLE_VALUE ) then
    ScsiDrives.drive[index].DeviceHandle := GetFileHandle( ord(ScsiDrives.drive[index].Drive) );

  FillChar(swb, sizeof(swb), 0 );
  if ( lpsrb.Flags AND SRB_DIR_IN <> 0 ) then
    swb.spt.DataIn          := SCSI_IOCTL_DATA_IN
  else if ( lpsrb.Flags and SRB_DIR_OUT <> 0 ) then
    swb.spt.DataIn          := SCSI_IOCTL_DATA_OUT
  else
    swb.spt.DataIn          := SCSI_IOCTL_DATA_UNSPECIFIED;

  swb.spt.Length            := sizeof(SCSI_PASS_THROUGH_DIRECT);
  swb.spt.CdbLength         := lpsrb.CDBLen;
  swb.spt.SenseInfoLength   := lpsrb.SenseLen;
  swb.spt.DataTransferLength := lpsrb.BufLen;
  swb.spt.TimeOutValue      := 120;
  swb.spt.DataBuffer        := lpsrb.BufPointer;
  swb.spt.SenseInfoOffset   := 48; 
  move(lpsrb.CDBCmd, swb.spt.Cdb, lpsrb.CDBLen );
  length := sizeof(swb);
  status := DeviceIoControl(ScsiDrives.drive[index].DeviceHandle, IOCTL_SCSI_PASS_THROUGH_DIRECT, @swb, length, @swb, length, returned, nil );
  if ( swb.spt.ScsiStatus = 0 ) and status then
    lpsrb.Status := SS_COMP
  else
  begin
    lpsrb.Status := SS_ERR;
    move(swb.ucSenseBuf, lpsrb.SenseArea, swb.spt.SenseInfoLength);
    lpsrb.TargStat := swb.spt.ScsiStatus;
  end;
  if CloseFH and (ScsiDrives.drive[index].DeviceHandle <> INVALID_HANDLE_VALUE) then
  begin
    if CloseHandle( ScsiDrives.drive[index].DeviceHandle ) then
      ScsiDrives.drive[index].DeviceHandle := INVALID_HANDLE_VALUE;
  end; 
  result := lpsrb.Status;
end;

function __SendASPI32Command(lpsrb: PSRB_EXECSCSICMD): DWORD;
begin
  result := SendASPI32Command(lpsrb, false);
end;

//******************************************************************
function __GetASPI32SupportInfo: DWORD;
begin
  if ( ScsiDrives.numAdapters = 0 ) then
    result := (MAKEWORD(0, SS_NO_ADAPTERS))
  else
    result := (MAKEWORD(ScsiDrives.numAdapters, SS_COMP));
end;

function SPTIGetNumAdapters: Byte;
var
  i: WORD;
  numAdapters: Byte;
begin
  numAdapters := 0;
  for i := 1 to 26 do
  begin
     if numAdapters < ScsiDrives.drive[i].HA then numAdapters := ScsiDrives.drive[i].HA;
  end;
  inc(numAdapters);
  result := numAdapters;
  exit;
end;

function SPTIInit: Byte;
var
  i: Byte;
  buf: string;
  uDriveType: Byte;
  retVal: Byte;

begin
  if ( SPTIAvailable ) then
  begin
    result := 0;
    exit;
  end;
  retVal := 0;
  tgn := 0;
  fillchar(ScsiDrives, sizeof(ScsiDrives), 0 );
  for i := 2 to 26 do
    ScsiDrives.drive[i].DeviceHandle := INVALID_HANDLE_VALUE;

  for i := 2 to 26 do
  begin
    buf := char(65+i)+':\';
    uDriveType := GetDriveType( @buf[1] );
    if ( uDriveType = DRIVE_CDROM ) {or ( uDriveType = DRIVE_FIXED )} then
    begin
      GetDriveInformation( i, ScsiDrives.drive[i] );
      if ( ScsiDrives.drive[i].Used ) then inc(retVal);
    end
    else
      ScsiDrives.drive[i].Target := 120;
  end;
  ScsiDrives.numAdapters := SPTIGetNumAdapters;
  if tgn <> 0 then
  begin
    for i := 2 to  26 do
    begin
      if ( ScsiDrives.drive[i].Used ) then
      if ScsiDrives.drive[i].LUN = 250 then
      begin
        ScsiDrives.drive[i].LUN := 0;
        ScsiDrives.drive[i].HA := ScsiDrives.numAdapters;
      end;
    end;
    ScsiDrives.numAdapters := SPTIGetNumAdapters;
  end;

  SPTIAvailable := TRUE;

  result := retVal;
end;

function SPTIDeInit: Integer;
var
  i: Integer;
begin
  if ( not SPTIAvailable ) then
  begin
    result := 0;
    exit;
  end;
  for i := 2 to 26 do
    if ( ScsiDrives.drive[i].Used ) then
      if ScsiDrives.drive[i].DeviceHandle <> INVALID_HANDLE_VALUE then
      CloseHandle( ScsiDrives.drive[i].DeviceHandle );

  ScsiDrives.numAdapters := SPTIGetNumAdapters( );

  FillChar( ScsiDrives, sizeof(ScsiDrives), 0);
  SPTIAvailable := FALSE;
  result := -1;
end;

initialization
begin
  SPTIAvailable := False;
end;
finalization
  if SPTIAvailable then
    SPTIDeInit;

end.