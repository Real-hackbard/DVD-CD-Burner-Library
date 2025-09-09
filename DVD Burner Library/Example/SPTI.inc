const
  SENSE_LEN                 = 24;  { Default sense buffer length}
  SRB_DIR_SCSI              = $00; { Direction determined by SCSI}
  SRB_POSTING               = $01; { Enable ASPI posting}
  SRB_ENABLE_RESIDUAL_COUNT = $04; { Enable residual byte count reporting}
  SRB_DIR_IN                = $08; { Transfer from SCSI target to host}
  SRB_DIR_OUT               = $10; { Transfer from host to SCSI target}
  SRB_EVENT_NOTIFY          = $40; { Enable ASPI event notification}
  RESIDUAL_COUNT_SUPPORTED  = $02; { Extended buffer flag}
  MAX_SRB_TIMEOUT           = 108000; { 30 hour maximum timeout in sec}
  DEFAULT_SRB_TIMEOUT       = 108000; { Max timeout by default}

  SC_HA_INQUIRY             = $00; { Host adapter inquiry}
  SC_GET_DEV_TYPE           = $01; { Get device type}
  SC_EXEC_SCSI_CMD          = $02; { Execute SCSI command}
  SC_ABORT_SRB              = $03; { Abort an SRB}
  SC_RESET_DEV              = $04; { SCSI bus device reset}
  SC_SET_HA_PARMS           = $05; { Set HA parameters}
  SC_GET_DISK_INFO          = $06; { Get Disk information}
  SC_RESCAN_SCSI_BUS        = $07; { ReBuild SCSI device map}
  SC_GETSET_TIMEOUTS        = $08; { Get/Set target timeouts}

{******************************************************************************}
{                                  SRB Status                                  }
{******************************************************************************}

  SS_PENDING                = $00; { SRB being processed}
  SS_COMP                   = $01; { SRB completed without error}
  SS_ABORTED                = $02; { SRB aborted}
  SS_ABORT_FAIL             = $03; { Unable to abort SRB}
  SS_ERR                    = $04; { SRB completed with error}

  SS_INVALID_CMD            = $80; { Invalid ASPI command}
  SS_INVALID_HA             = $81; { Invalid host adapter number}
  SS_NO_DEVICE              = $82; { SCSI device not installed}

  SS_INVALID_SRB            = $E0; { Invalid parameter set in SRB}
  SS_OLD_MANAGER            = $E1; { ASPI manager doesn't support Windows}
  SS_BUFFER_ALIGN           = $E1; { Buffer not aligned (replaces OLD_MANAGER in Win32)}
  SS_ILLEGAL_MODE           = $E2; { Unsupported Windows mode}
  SS_NO_ASPI                = $E3; { No ASPI managers resident}
  SS_FAILED_INIT            = $E4; { ASPI for windows failed init}
  SS_ASPI_IS_BUSY           = $E5; { No resources available to execute cmd}
  SS_BUFFER_TO_BIG          = $E6; { Buffer size to big to handle!}
  SS_MISMATCHED_COMPONENTS  = $E7; { The DLLs/EXEs of ASPI don't version check}
  SS_NO_ADAPTERS            = $E8; { No host adapters to manage}
  SS_INSUFFICIENT_RESOURCES = $E9; { Couldn't allocate resources needed to init}
  SS_ASPI_IS_SHUTDOWN       = $EA; { Call came to ASPI after PROCESS_DETACH}
  SS_BAD_INSTALL            = $EB; { The DLL or other components are installed wrong}
(*
{******************************************************************************}
{                              Host Adapter Status                             }
{******************************************************************************}

  HASTAT_OK                   = $00; { Host adapter did not detect an  error}
  HASTAT_SEL_TO               = $11; { Selection Timeout}
  HASTAT_DO_DU                = $12; { Data overrun data underrun}
  HASTAT_BUS_FREE             = $13; { Unexpected bus free}
  HASTAT_PHASE_ERR            = $14; { Target bus phase sequence  failure}
  HASTAT_TIMEOUT              = $09; { Timed out while SRB was waiting to beprocessed.}
  HASTAT_COMMAND_TIMEOUT      = $0B; { Adapter timed out processing SRB.}
  HASTAT_MESSAGE_REJECT       = $0D; { While processing SRB, the  adapter received a MESSAGE}
  HASTAT_BUS_RESET            = $0E; { A bus reset was detected.}
  HASTAT_PARITY_ERROR         = $00; { A parity error was detected.}
  HASTAT_REQUEST_SENSE_FAILED = $10; { The adapter failed in issuing}
*)

  DTYPE_DASD                  = $00;   { Disk Device               }
  DTYPE_SEQD                  = $01;   { Tape Device               }
  DTYPE_PRNT                  = $02;   { Printer                   }
  DTYPE_PROC                  = $03;   { Processor                 }
  DTYPE_WORM                  = $04;   { Write-once read-multiple  }
  DTYPE_CROM                  = $05;   { CD-ROM device             }
  DTYPE_CDROM                 = $05;   { CD-ROM device             }
  DTYPE_SCAN                  = $06;   { Scanner device            }
  DTYPE_OPTI                  = $07;   { Optical memory device     }
  DTYPE_JUKE                  = $08;   { Medium Changer device     }
  DTYPE_COMM                  = $09;   { Communications device     }
  DTYPE_RESL                  = $0A;   { Reserved (low)            }
  DTYPE_RESH                  = $1E;   { Reserved (high)           }
  DTYPE_UNKNOWN               = $1F;   { Unknown or no device type }

  SCSI_INQUIRY                = $12;
(*
  SCG_RECV_DATA               = $0001;	{ DMA direction to Sun }
  SCG_DISRE_ENA               = $0002;	{ enable disconnect/reconnect }
  SCG_SILENT                  = $0004;	{ be silent on errors }
  SCG_CMD_RETRY               = $0008;	{ enable retries }
  SCG_NOPARITY                = $0010;	{ disable parity for this command }
  SC_G0_CDBLEN                = 06;        { Len of Group 0 commands }
  SC_G1_CDBLEN                = 10;        { Len of Group 1 commands }
  SC_G5_CDBLEN                = 12;        { Len of Group 5 commands }
  DEF_SENSE_LEN               = 16;        { Default Sense Len }
  CCS_SENSE_LEN               = 18;        { Sense Len for CCS compatible devices }
*)
//***************************************************************************
//                      %%% Request Sense Data Format %%%
//***************************************************************************
  type TSenseArea = packed record
    ErrorCode,          // Error Code (70H or 71H)
    SegmentNum,         // Number of current segment descriptor
    SenseKey,           // Sense Key(See bit definitions too)
    InfoByte0,          // Information MSB
    InfoByte1,          // Information MID
    InfoByte2,          // Information MID
    InfoByte3,          // Information LSB
    AddSenLen,          // Additional Sense Length
    ComSpecInf0,        // Command Specific Information MSB
    ComSpecInf1,        // Command Specific Information MID
    ComSpecInf2,        // Command Specific Information MID
    ComSpecInf3,        // Command Specific Information LSB
    AddSenseCode,       // Additional Sense Code
    AddSenQual,         // Additional Sense Code Qualifier
    FieldRepUCode,      // Field Replaceable Unit Code
    SenKeySpec15,       // Sense Key Specific 15th byte
    SenKeySpec16,       // Sense Key Specific 16th byte
    SenKeySpec17: Byte; // Sense Key Specific 17th byte
    AddSenseBytes:      // Additional Sense Bytes
    Array[18..$20] of byte;
  end;

{*****************************************************************************}
{                 SRB - HOST ADAPTER INQUIRY - SC_HA_INQUIRY (0)              }
{*****************************************************************************}
type
  PSRB_HAInquiry = ^TSRB_HAInquiry;
  TSRB_HAInquiry = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    HA_Count: BYTE;
    HA_SCSI_ID: BYTE;
    HA_ManagerId: Array[0..15] of char;
    HA_Identifier: Array[0..15] of char;
    HA_Unique: Array[0..15] of char;
    HA_Rsvd1: Word;
  end;
{******************************************************************************}
{                    SRB - GET DEVICE TYPE - SC_GET_DEV_TYPE (1)               }
{******************************************************************************}
type
  PSRB_GDEVBlock = ^TSRB_GDEVBlock;
  TSRB_GDEVBlock = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    Target: BYTE;
    Lun: BYTE;
    DeviceType: BYTE;
    Rsvd1: BYTE;
  end;
{*****************************************************************************}
{                 SRB - EXECUTE SCSI COMMAND - SC_EXEC_SCSI_CMD (2)           }
{*****************************************************************************}
type
  TSRB_ExecSCSICmd = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    Target: BYTE;
    Lun: BYTE;
    Rsvd1: Word;
    BufLen: LongInt;
    BufPointer: PChar;
    SenseLen: BYTE;
    CDBLen: BYTE;
    HaStat: BYTE;
    TargStat: BYTE;
    PostProc: LongWord;
    Rsvd2: array[0..19] of byte;
    CDBCmd: Byte;
    CDBByte: array[1..15] of byte;
    SenseArea: TSenseArea;
  end;

{******************************************************************************}
{                       SRB - ABORT AN SRB - SC_ABORT_SRB (3)                  }
{******************************************************************************}
type
  TSRB_Abort = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    ToAbort: Pointer;
  end;
{*****************************************************************************}
{                    SRB - BUS DEVICE RESET - SC_RESET_DEV (4)                }
{*****************************************************************************}
type
  SRB_BusDeviceReset = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    Target: BYTE;
    Lun: BYTE;
    Rsvd1: Array[0..12-1] of BYTE;
    HaStat: BYTE;
    TargStat: BYTE;
    PostProc: Pointer;
    Rsvd2: Array[0..36-1] of BYTE;
  end;
{******************************************************************************}
{                SRB - GET DISK INFORMATION - SC_GET_DISK_INFO                 }
{******************************************************************************}
type
  TSRB_GetDiskInfo = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    Target: BYTE;
    Lun: BYTE;
    DriveFlags: BYTE;
    Int13HDriveInfo: BYTE;
    Heads: BYTE;
    Sectors: BYTE;
    Rsvd1: Array[0..9] of BYTE;
  end {SRB_GetDiskInfo};

{******************************************************************************}
{                      SRB - RESCAN SCSI BUS(ES) ON SCSIPORT                   }
{******************************************************************************}

type
  SRB_RescanPort = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
  end {RescanPort};

{******************************************************************************}
{                         SRB - GET/SET TARGET TIMEOUTS                        }
{******************************************************************************}
type
  TSRB_GetSetTimeouts = packed record
    Command: BYTE;
    Status: BYTE;
    HaId: BYTE;
    Flags: BYTE;
    Hdr_Rsvd: LongInt;
    Target: BYTE;
    Lun: BYTE;
    Timeout: LongInt;
  end;
{******************************************************************************}
{                 ASPIBUFF - Structure For Controllng I/O Buffers              }
{******************************************************************************}

type
  tag_ASPI32BUFF = packed record
    AB_BufPointer: Pointer;
    AB_BufLen: LongInt;
    AB_ZeroFill: LongInt;
    AB_Reserved: LongInt;
  end;
  PSRB_EXECSCSICMD = ^TSRB_EXECSCSICMD;
const
  SCSI_IOCTL_DATA_OUT            = 0;
  SCSI_IOCTL_DATA_IN             = 1;
  SCSI_IOCTL_DATA_UNSPECIFIED    = 2;
  IOCTL_SCSI_PASS_THROUGH_DIRECT = 315412;
  IOCTL_SCSI_GET_ADDRESS         = 266264;
type
  {$A+}
  TSCSIDRIVE = record
    HA,
    Target,
    LUN: Byte;
    Drive: Char;
    Used: Boolean;
    DeviceHandle: THANDLE;
    inqData: array[0..64] of Char;
  end;
  TSCSIDRIVES = record
    numAdapters: Byte;
    drive: array[0..26] of TSCSIDRIVE;
  end;

  PSCSI_PASS_THROUGH = ^SCSI_PASS_THROUGH;

  SCSI_PASS_THROUGH = record
    Length: WORD;
    ScsiStatus,
    PathId, TargetId, Lun,
    CdbLength, SenseInfoLength,
    DataIn: Byte;
    DataTransferLength,
    TimeOutValue: Cardinal;
    DataBufferOffset: PChar;
    SenseInfoOffset: Cardinal;
    Cdb: array[0..15] of byte;
  end;

  SCSI_PASS_THROUGH_DIRECT = record
    Length: WORD;
    ScsiStatus,
    PathId, TargetId, Lun,
    CdbLength, SenseInfoLength,
    DataIn: Byte;
    DataTransferLength,
    TimeOutValue: Cardinal;
    DataBuffer: PChar;
    SenseInfoOffset: Cardinal;
    Cdb: array[0..15] of byte;
  end;

  PSCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = ^SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER;
  SCSI_PASS_THROUGH_DIRECT_WITH_BUFFER = record
    spt: SCSI_PASS_THROUGH_DIRECT;
    Filler: Cardinal;
    ucSenseBuf: array[0..31] of byte;
  end;

  PSCSI_ADDRESS = ^SCSI_ADDRESS;
  SCSI_ADDRESS = packed record
    Length: Cardinal;
    PortNumber,
    PathId,
    TargetId,
    Lun: Byte;
  end;




var
  ScsiDrives: TScsiDrives;
  SPTIAvailable: Boolean = False;
  function __SendASPI32Command(lpsrb: PSRB_EXECSCSICMD): LongWord; cdecl;
  function __GetASPI32SupportInfo: LongWord; cdecl;
  function SPTIInit: Byte;
  function SPTIDeInit: Integer;
  function GetDriveLetterBMASPI(h, t, l: Byte): char;


implementation

  function GetDriveLetterBMASPI(h, t, l: Byte): char;
  var
    i: Integer;
  begin
    for i:=2 to 26 do
    begin
      if (ScsiDrives.Drive[i].Ha = h) and (ScsiDrives.Drive[i].Target = t) and (ScsiDrives.Drive[i].LUN = l) then
      begin
        result := Chr(Ord(ScsiDrives.Drive[i].Drive)+65);
        exit;
      end;
    end;
    result := #127;
  end;

