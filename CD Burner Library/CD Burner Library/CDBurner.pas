
unit CDBurner;

interface


uses Windows, Dialogs, SCSIUnit, SCSITypes, Classes, AudioUnit,WaveUtils, SysUtils, scsidefs, BurnerConsts;



type
   TCopyStatusEvent = procedure(CurrentSector, PercentDone: Integer) of object;
   TCDStatusEvent = procedure(CurrentStatus: string) of object;
   TCDBufferProgressEvent = procedure(Percent: Integer) of object;
   TCDBufferStatusEvent = procedure(BufferSize , FreeSize : Integer) of object;
   TCDWriteStatusEvent = procedure(BytesWritten: Integer) of object;




   TCDBurner = class
   private
      FLastError: TScsiError;
      FDefaults: TScsiDefaults;
      FBurnerInfo: TCDBurnerInfo;
      FOnCopyStatus: TCopyStatusEvent;
      FOnCDStatus: TCDStatusEvent;
      FOnBufferProgress : TCDBufferProgressEvent;
      FOnBufferStatus : TCDBufferStatusEvent;
      FOnWriteStatusEvent : TCDWriteStatusEvent;
      FRegistryName: string;
      FsectorWrite: LongInt;
      FBytesWritten: Longint;
      function GetBufferCapacity: Integer;
      function GetBufferFreeSpace: Integer;
      function GetBurnerInfo: TCDBurnerInfo;
      procedure SetBurnerInfo(Info: TCDBurnerInfo);
      function GetCapabilities: TCdRomCapabilities;
      function GetCDDriveSpeeds: TCDReadWriteSpeeds;
      function GetDefaults: PScsiDefaults;
      procedure SetDefaults(Value: PScsiDefaults);
      function GetIsReady: boolean;
      function GetBurnerID: TBurnerID;
      function GetBufferSize: WORD;
      function GetTOC: TScsiTOC;
      function GetCapacity: integer;
      function GetSessions: TScsiSessionInfo;
      function GetSectorType(aLBA: integer): TScsiReadCdSectorType;
      function GetISRC(TrackNumber: integer): TScsiISRC;
      function GetMaxBuffer: Longint;
      function GetDiscType: TScsiProfileDeviceDiscTypes;
      function GetDVDescriptor: TScsiDVDLayerDescriptorInfo;
      function GetFormatCapacity: TFormatCapacity;
      function GetTrackInfo(ATrack: Byte): TTrackInformation;
   protected
      BufferSize , BufferFreeSpace : Integer;
      function SyncCache: boolean;
      function WriteData(GLBA: DWORD; SectorCount: WORD; Buf: pointer; BufLen: DWORD): boolean;
      function WriteAudio(GLBA, SectorCount: DWORD; Buf: pointer; BufLen: DWORD): boolean;
   public
      CDTracks: TCDTrackList;
      constructor Create(DeviceInfo: TCDBurnerInfo);
      destructor Destroy; override;
{read functions}
      function ReadData(GLBA, SectorCount: DWORD; Buf: pointer; BufLen: DWORD): boolean;
      function ReadAudio(GLBA, SectorCount: DWORD; Buf: pointer; BufLen: DWORD): boolean;
      function Seek(GLBA: DWORD): boolean;
{end read functions}

{write functions}
      function SetWriteMode(Write_Type, Data_Block_type, Track_Mode, Session_Format,
         Packet_Size, Audio_Pause_Length: integer; Test_Write, Burn_Proof: Boolean): boolean;
      function GetWriteParameters: string;
      function CopyDiskToISOFile(Filename: string): boolean;
      function WriteISOToCD(Filename: string; CloseSession: Boolean): boolean;
      function WriteAudioTrack(TrackID: Integer; CloseSession: Boolean): boolean;
      function BlankCDRom(BlankType: byte; GLBA: longint): boolean;
      function CloseSession: boolean;
      function CloseTrack(TrackNo: Byte): boolean;
      function SetDriveReadWriteSpeed(ReadSpeed, WriteSpeed: Integer): Boolean;
      property CDBufferSize : WORD read GetBufferSize;
      property CDBufferCapacity : Integer read GetBufferCapacity;
{end write functions }

{Misc functions}
      function GetVendorName(SubName: string): string;
      function AddAudioTrack(Filename: string) : Boolean;
      Function SaveAudioTrack(TrackID : Integer; Filename : String) : Boolean;
      property BurnerInfo: TCDBurnerInfo read GetBurnerInfo write SetBurnerInfo;
      function GetCapabilityText: Tstringlist;
      function GetCDSpeedsText: Tstringlist;
      property Capabilities: TCdRomCapabilities read GetCapabilities;
      property Defaults: PScsiDefaults read GetDefaults write SetDefaults;
      property LastError: TScsiError read FLastError;
      procedure LockMedium;
      procedure UnLockMedium;
      property OnCopyStatus: TCopyStatusEvent read FOnCopyStatus write FOnCopyStatus;
      property OnCDStatus: TCDStatusEvent read FOnCDStatus write FOnCDStatus;
      property OnBufferProgress : TCDBufferProgressEvent read FOnBufferProgress write FOnBufferProgress;
      property OnBufferStatus : TCDBufferStatusEvent read FOnBufferStatus write FOnBufferStatus;
      property OnWriteStatusEvent : TCDWriteStatusEvent read FOnWriteStatusEvent write FOnWriteStatusEvent;
      procedure LoadMedium(DontWait: boolean);
      procedure EjectMedium(DontWait: boolean);
      property CDBurnerID: TBurnerID read GetBurnerID;
      property GetError: TScsiError read fLastError;
      property IsReady: boolean read GetIsReady;
      property TOC: TScsiTOC read GetTOC;
      property Capacity: integer read GetCapacity;
      property Sessions: TScsiSessionInfo read GetSessions;
      property SectorType[aLBA: integer]: TScsiReadCdSectorType read GetSectorType;
      property ISRC[TrackNumber: integer]: TScsiISRC read GetISRC;
      property DeviceDiscType: TScsiProfileDeviceDiscTypes read GetDiscType;
      property DVDescriptor: TScsiDVDLayerDescriptorInfo read GetDVDescriptor;
      property FormatCapacity: TFormatCapacity read GetFormatCapacity;
      property TrackInformation[ATrack: Byte]: TTrackInformation read GetTrackInfo;
   end;

implementation



uses CovertFuncs;


procedure ScatterDWORD(Arg: DWORD; var b3, b2, b1, b0: byte);
begin
   b3 := (Arg shr 24) and $FF;
   b2 := (Arg shr 16) and $FF;
   b1 := (Arg shr 8) and $FF;
   b0 := Arg and $FF;
end;


function ScatterDeviceID(DeviceID: TBurnerID;
   var Adapter, Target, Lun: byte): char;
var Res: BYTE;
begin
   ScatterDWORD(DeviceID, Adapter, Target, Lun, Res);
   Result := CHR((Lun and $1F) or $40);
   Lun := (Lun shr 5) and 7;
end;


function DeviceIDtoLetter(DeviceID: TBurnerID): char;
var Adapter, Target, Lun: byte;
begin
   Result := ScatterDeviceID(DeviceID, Adapter, Target, Lun);
end;



{************************CD Burner ***********************************}



constructor TCDBurner.Create(DeviceInfo: TCDBurnerInfo);
begin
   inherited Create;
   FBurnerInfo := DeviceInfo;
   FDefaults := SCSI_DEF;
   FLastError := Err_None;
   CDTracks := TCDTrackList.Create(nil, TCDTrackItem);
end;


destructor TCDBurner.Destroy;
begin
   UnLockMedium;
   CDTracks.Free;
   inherited;
end;


function TCDBurner.GetBurnerInfo: TCDBurnerInfo;
begin
   Result := FBurnerInfo;
end;

procedure TCDBurner.SetBurnerInfo(Info: TCDBurnerInfo);
begin
   FBurnerInfo := Info;
end;



function TCDBurner.GetVendorName(SubName: string): string;
var VendorNames: TStringList;
   Name: string;
begin
   Result := SubName;
   VendorNames := TStringList.Create;
   try
      VendorNames.CommaText := ScsiVendorNameList;
      Name := VendorNames.Values[SubName];
      if Name <> '' then Result := Name;
   finally
      VendorNames.Free;
   end;
end;



function TCDBurner.GetBurnerID: TBurnerID;
begin
   Result := FBurnerInfo.DriveID
end;



function TCDBurner.GetDefaults: PScsiDefaults;
begin
   Result := @FDefaults;
end;

procedure TCDBurner.SetDefaults(Value: PScsiDefaults);
begin
   FDefaults := Value^;
end;


procedure TCDBurner.LockMedium;
begin
   FLastError := SCSIpreventMediumRemoval(FBurnerInfo, True, fDefaults);
end;

procedure TCDBurner.UnLockMedium;
begin
   FLastError := SCSIpreventMediumRemoval(FBurnerInfo, False, fDefaults);
end;


procedure TCDBurner.LoadMedium(DontWait: boolean);
begin
   FLastError := SCSIstartStopUnit(FBurnerInfo, True, True, DontWait, fDefaults);
end;


procedure TCDBurner.EjectMedium(DontWait: boolean);
begin
   FLastError := SCSIstartStopUnit(FBurnerInfo, False, True, DontWait, fDefaults);
end;



function TCDBurner.GetCDDriveSpeeds: TCDReadWriteSpeeds;
begin
   FLastError := SCSIGetDriveSpeeds(FBurnerInfo, Result, fDefaults);
end;


function TCDBurner.GetCDSpeedsText: Tstringlist;
var
   Strings: Tstringlist;
   CDROMSpeeds: TCDReadWriteSpeeds;
begin
   SCSIGetDriveSpeeds(FBurnerInfo, CDROMSpeeds, fDefaults);
   Strings := Tstringlist.create;
   Strings.Add('-- Device Reading Speeds --');
   Strings.Add('       Current Read Speed = ' + inttostr(CDRomSpeeds.CurrentReadSpeed));
   Strings.Add('       Maximum Read Speed = ' + inttostr(CDRomSpeeds.MaxReadSpeed));
   Strings.Add('-- Device Writing Speeds --');
   Strings.Add('       Current Write Speed = ' + inttostr(CDRomSpeeds.CurrentWriteSpeed));
   Strings.Add('       Maximum Write Speed = ' + inttostr(CDRomSpeeds.MaxWriteSpeed));
   result := Strings;
end;



function TCDBurner.GetCapabilities: TCdRomCapabilities;
begin
   FLastError := SCSIgetCdRomCapabilities(FBurnerInfo, Result, fDefaults);
end;


function TCDBurner.GetCapabilityText: Tstringlist;
var
   Strings: Tstringlist;
   CDROMCap: TCdRomCapabilities;
begin
   SCSIgetCdRomCapabilities(FBurnerInfo, CDROMCap, fDefaults);
   Strings := Tstringlist.create;
   Strings.Add('-- Device Reading Methods --');
   if cdcReadCDR in CDROMCap then Strings.Add('    Read CD-R media');
   if cdcReadCDRW in CDROMCap then Strings.Add('    Read CD-RW media');
   if cdcReadMethod2 in CDROMCap then Strings.Add('    Read CD-R written using fixed packets');
   if cdcReadDVD in CDROMCap then Strings.Add('    Read DVD-ROM media');
   if cdcReadDVDR in CDROMCap then Strings.Add('    Read DVD-R / DVD-RW media');
   if cdcReadDVDRAM in CDROMCap then Strings.Add('    Read DVD-RAM media');
   Strings.Add('-- Device Writing Methods --');
   if cdcWriteCDR in CDROMCap then Strings.Add('    Write CD-R media');
   if cdcWriteCDRW in CDROMCap then Strings.Add('    Write CD-RW media');
   if cdcWriteDVDR in CDROMCap then Strings.Add('    Write DVD-R / DVD-RW media');
   if cdcWriteDVDRAM in CDROMCap then Strings.Add('    Write DVD-RAM media');
   if cdcWriteTestMode in CDROMCap then Strings.Add('    Write in test mode');
   if cdcWriteBurnProof in CDROMCap then Strings.Add('    Can Use Burn Proof');
   Strings.Add('-- Device Extended Methods --');
   if cdcReadMode2form1 in CDROMCap then Strings.Add('        Capable to read Mode 2 Form 1 (CD-XA format)');
   if cdcReadMode2form2 in CDROMCap then Strings.Add('        Capable to read Mode 2 Form 2 (CD-XA format)');
   if cdcReadMultisession in CDROMCap then Strings.Add('        Capable to read PhotoCD format (multiple sessions)');
   if cdcCddaBarCode in CDROMCap then Strings.Add('       Capable to read CD disc bar code');
   result := Strings;
end;




function TCDBurner.GetIsReady: boolean;
begin
   FLastError := SCSItestReady(FBurnerInfo, fDefaults);
   Result := FLastError = Err_None;
end;


function TCDBurner.GetTOC: TScsiTOC;
begin
   FLastError := SCSIgetTOC(FBurnerInfo, Result, fDefaults);
end;

function TCDBurner.GetSectorType(aLBA: integer): TScsiReadCdSectorType;
begin
   FLastError := SCSIreadHeader(FBurnerInfo, aLBA, Result, fDefaults);
end;


function TCDBurner.GetCapacity: integer;
var
   temp: cardinal;
begin
   temp := result;
   FLastError := SCSIreadCapacity(FBurnerInfo, temp, fDefaults);
end;


function TCDBurner.GetSessions: TScsiSessionInfo;
begin
   FLastError := SCSIgetSessionInfo(FBurnerInfo, Result, fDefaults);
end;


function TCDBurner.GetISRC(TrackNumber: integer): TScsiISRC;
begin
   FLastError := SCSIgetISRC(FBurnerInfo, TrackNumber, Result, fDefaults);
end;




{++++++++++++++++++Read Data Functions++++++++++++++}

function TCDBurner.Seek(GLBA: DWORD): boolean;
begin
   fLastError := SCSIseek10(FBurnerInfo, GLBA, fDefaults);
   Result := fLastError = Err_None;
end;


function TCDBurner.ReadData(GLBA, SectorCount: DWORD;
   Buf: pointer; BufLen: DWORD): boolean;
begin
   fLastError := SCSIread10(FBurnerInfo, GLBA, SectorCount, Buf, BufLen, fDefaults);
   Result := fLastError = Err_None;
end;


function TCDBurner.ReadAudio(GLBA, SectorCount: DWORD;
   Buf: pointer; BufLen: DWORD): boolean;
begin
   fLastError := SCSIreadCdEx(FBurnerInfo, GLBA, SectorCount, csfAudio, [cffUserData], Buf, BufLen, fDefaults);
   Result := fLastError = Err_None;
end;


function TCDBurner.CopyDiskToISOFile(Filename: string): boolean;
var
   ISOStream: TFileStream;
   Buf: pointer;
   BufLen: integer;
   IndexBlock: integer;
   LastBlock: integer;
   Written: longint;

begin
   LastBlock := self.TOC.Tracks[self.TOC.TrackCount - 1].AbsAddress;
   if Assigned(FOnCDStatus) then FOnCDStatus('Get Last Block Address : ' + inttostr(LastBlock));
   ISOStream := TFileStream.Create(Filename, fmCreate);
   if Assigned(FOnCDStatus) then FOnCDStatus('Assign File Stream');
   if LastBlock < 1 then exit;
   BufLen := MAX_DATABLOCKS * 2048;
   if Assigned(FOnCDStatus) then FOnCDStatus('Allocate memory');
   Buf := nil;
   ReAllocMem(Buf, BufLen);
   if Assigned(FOnCDStatus) then FOnCDStatus('Start Streaming...');
   IndexBlock := 0;
   while IndexBlock < LastBlock - 1 do
   begin
      if self.ReadData(IndexBlock, MAX_DATABLOCKS, Buf, BufLen) then
         FBytesWritten := ISOStream.Write(pchar(Buf)^, BufLen);

      FsectorWrite := IndexBlock;
      IndexBlock := IndexBlock + MAX_DATABLOCKS;

      if Assigned(FOnCDStatus) then FOnCDStatus('Write Sector : ' + inttostr(FsectorWrite));

      if Assigned(FOnCopyStatus) then FOnCopyStatus(FsectorWrite, (FsectorWrite div ((LastBlock - 1) div 100)));
   end;

   if Assigned(FOnCDStatus) then FOnCDStatus('DeAllocate Memory');
   ReallocMem(Buf, 0);
   if Assigned(FOnCDStatus) then FOnCDStatus('Close Stream');
   ISOStream.Free;
end;

{++++++++++++++++++End Read Data Functions++++++++++++++}





{ *************Write Data commands*****************}


function TCDBurner.GetMaxBuffer: LongInt;
var
   BufSize: Word;
begin
   FLastError := SCSIgetMaxBufferSize(FBurnerInfo, BufSize, fDefaults);
   // FMAXBuffer := BufSize;
   // Result := FMAXBuffer;
end;



function TCDBurner.GetBufferSize: WORD;
var
   Temp: Word;
begin
   Temp := 0;
   FLastError := SCSIgetBufferSize(FBurnerInfo, Temp, fDefaults);
   Result := Temp;
end;



function TCDBurner.WriteISOToCD(Filename: string; CloseSession: Boolean): boolean;
var
   ISOFilestream: TFilestream;
   Buf: Pointer;
   BufLen: integer;
   BytesWritten: integer;
   IndexBlock: integer;
   LastBlock: integer;
   CDBurnerHandle: THandle;
begin
   BytesWritten := 0;
   if not
      self.SetWriteMode(TRACK_AT_ONCE, MODE_1, CDR_MODE_DATA, CDROM_CDDA, 0, 150, False, True) then
   begin
      if Assigned(FOnCDStatus) then FOnCDStatus('HardWare : Set Write Mode Error');
      exit;
   end
   else
      if Assigned(FOnCDStatus) then FOnCDStatus('HardWare : Set Write Mode OK');

   ISOFilestream := TFilestream.Create(Filename, fmOpenRead);
   BufLen := 2048;
   if (ISOFilestream.size mod 2048) > 0 then
   begin
      if Assigned(FOnCDStatus) then FOnCDStatus('Error : Error in Image Size');
      ISOFilestream.free;
      exit;
   end;
   LastBlock := (ISOFilestream.size div 2048);
   Buf := nil;
   ReallocMem(Buf, BufLen);
   for IndexBlock := 0 to LastBlock - 1 do
   begin
      try
         BytesWritten := BytesWritten + ISOFilestream.read(pchar(Buf)^, BufLen);
         self.WriteData(IndexBlock, 1, buf, BufLen);
      finally
         if Assigned(FOnCopyStatus) then FOnCopyStatus(IndexBlock, (IndexBlock div ((LastBlock - 1) div 100)));
         if Assigned(FOnWriteStatusEvent) then FOnWriteStatusEvent(BytesWritten);
         if Assigned(FOnBufferProgress) then FOnBufferProgress(GetBufferCapacity);
         if Assigned(FOnBufferStatus) then FOnBufferStatus(BufferSize,BufferFreeSpace);
      end;
   end; {for loop}

   ReallocMem(Buf, 0);
   if Assigned(FOnBufferProgress) then FOnBufferProgress(GetBufferCapacity);
   if Assigned(FOnCDStatus) then FOnCDStatus('Finished Writing : Sync Cache');
   self.SyncCache;

   if Assigned(FOnCDStatus) then FOnCDStatus('Finished Writing : Closing Track');

   self.CloseTrack(1);
   self.SyncCache;
   
   if CloseSession = true then
   begin
      if Assigned(FOnCDStatus) then FOnCDStatus('Finished Writing : Closing Session');
      self.CloseSession;
      self.SyncCache;
   end;
   ISOFilestream.Free;
   if Assigned(FOnCDStatus) then FOnCDStatus('Finished Writing : Job Done!');
end;




function TCDBurner.WriteData(GLBA: DWORD; SectorCount: WORD;
   Buf: pointer; BufLen: DWORD): boolean;
begin
   FLastError := SCSIWrite10(FBurnerInfo, GLBA, SectorCount, Buf, BufLen, fDefaults);
   Result := fLastError = Err_None;
end;



function TCDBurner.WriteAudio(GLBA, SectorCount: DWORD;
   Buf: pointer; BufLen: DWORD): boolean;
begin
   fLastError := SCSIWriteCDDA(FBurnerInfo,
      GLBA, SectorCount, csfAudio, [cffUserData],
      Buf, BufLen, fDefaults);
   Result := fLastError = Err_None;
end;




function TCDBurner.BlankCDRom(BlankType: byte; GLBA: longint): boolean;
begin
   FLastError := SCSIBlankCD(FBurnerInfo, BlankType, GLBA, fDefaults);
   Result := FLastError = Err_None;
end;




function TCDBurner.GetBufferFreeSpace: Integer;
var
   BufferInfo: TScsiCDBufferInfo;
   BufSpace: DWord;
   FreeSpace: DWord;
   Percent: Integer;

begin
   FillChar(BufferInfo, sizeof(TScsiCDBufferInfo), 0);
   SCSIgetBufferCapacity(FBurnerInfo, BufferInfo, fDefaults);
   BufSpace := BufferInfo.SizeOfBuffer;
   FreeSpace := BufferInfo.BlankLength;
   BufSpace := Swap32(BufSpace);
   FreeSpace := Swap32(FreeSpace);
   Result := FreeSpace;
end;


function TCDBurner.GetBufferCapacity: Integer;
var
   BufferInfo: TScsiCDBufferInfo;
   BufSpace: DWord;
   FreeSpace: DWord;
   Percent , Divisor: Integer;

begin
   FillChar(BufferInfo, sizeof(TScsiCDBufferInfo), 0);
   SCSIgetBufferCapacity(FBurnerInfo, BufferInfo, fDefaults);
   BufSpace := BufferInfo.SizeOfBuffer;
   FreeSpace := BufferInfo.BlankLength;
   BufferSize := Swap32(BufSpace);
   BufferFreeSpace := Swap32(FreeSpace);
   Divisor := (BufferSize div 100);
   Percent := ((BufferSize - BufferFreeSpace) div Divisor);
   if (Percent < 0) then Percent := 0;
   if (Percent > 100) then Percent := 100;
   Result := Percent;
end;


function TCDBurner.CloseSession: boolean;
begin
   FLastError := SCSICloseSession(FBurnerInfo, fDefaults);
   Result := FLastError = Err_None;
end;


function TCDBurner.CloseTrack(TrackNo: Byte): boolean;
begin
   FLastError := SCSICloseTrack(FBurnerInfo, TrackNo, fDefaults);
   Result := FLastError = Err_None;
end;




function TCDBurner.SetDriveReadWriteSpeed(ReadSpeed, WriteSpeed: Integer): Boolean;
begin
   FLastError := SCSISetSpeed(FBurnerInfo, ReadSpeed, WriteSpeed, fDefaults);
   Result := fLastError = Err_None;
end;




function TCDBurner.SetWriteMode(Write_Type, Data_Block_type, Track_Mode, Session_Format,
   Packet_Size, Audio_Pause_Length: integer; Test_Write, Burn_Proof: Boolean): boolean;

begin
   FLastError := SCSISetWriteParameters(FBurnerInfo, 0,
      Write_Type, Data_Block_type, Track_Mode, Session_Format,
      Packet_Size, Audio_Pause_Length, Test_Write, Burn_Proof, fDefaults);
   Result := fLastError = Err_None;
end;


function TCDBurner.SyncCache: boolean;
begin
   FLastError := SCSISYNCCACHE(FBurnerInfo, fDefaults);
   Result := FLastError = Err_None;
end;



function TCDBurner.WriteAudioTrack(TrackID: Integer; CloseSession: Boolean): boolean;
var
   Buf: Pointer;
   BufLen: integer;
   BytesWritten: integer;
   TOC: TScsiTOC;
   CurrentTrack: Integer;
   LastTrackLBA: Integer;
   IndexBlock: integer;
   LastBlock: integer;
begin
   TOC := Self.TOC;
   if TOC.TrackCount > 0 then
   begin
      LastTrackLBA := TOC.Tracks[TOC.TrackCount - 1].AbsAddress;
      CurrentTrack := TOC.LastTrack + 1;
   end
   else
   begin
      LastTrackLBA := 0;
      CurrentTrack := 0;
   end;
   if not
      self.SetWriteMode(TRACK_AT_ONCE, RAW_DATA_BLOCK, CDR_MODE_AUDIO, CDROM_CDDA, 0, 150, False, True)
      then
   begin
      if Assigned(FOnCDStatus) then FOnCDStatus('HardWare : Set Write Mode Error');
      exit;
   end
   else
      if Assigned(FOnCDStatus) then FOnCDStatus('HardWare : Set Write Mode OK');


   FOnCDStatus('Burning :'+ CDTracks.Tracks[TrackID].CDTrack.TrackName);

   BufLen := ConvertDataBlock(RAW_DATA_BLOCK);

   if (CDTracks.Tracks[TrackID].CDTrack.DataSize mod BufLen) > 0 then
      LastBlock := (LastTrackLBA + (CDTracks.Tracks[TrackID].CDTrack.DataSize div BufLen)) + 1
   else
      LastBlock := (LastTrackLBA + (CDTracks.Tracks[TrackID].CDTrack.DataSize div BufLen));
      //set data offsett past header
   CDTracks.Tracks[TrackID].CDTrack.Seek(soFromBeginning ,CDTracks.Tracks[TrackID].CDTrack.DataOffset);

   for IndexBlock := LastTrackLBA to LastBlock - 1 do
   begin
      Buf := nil;
      ReallocMem(Buf, BufLen);
      FOnCDStatus('Allocating Audio Burn Buffer');
      try
         BytesWritten := BytesWritten + CDTracks.Tracks[TrackID].CDTrack.Read(pchar(Buf)^, BufLen);
        // FOnCDStatus('Reading Audio Stream ' + inttostr(BytesWritten));
         begin
            self.WriteAudio(IndexBlock, 1, buf, BufLen);
            if Assigned(FOnBufferProgress) then FOnBufferProgress(GetBufferCapacity);
            if Assigned(FOnCopyStatus) then FOnCopyStatus(IndexBlock, (IndexBlock div ((LastBlock - 1) div 100)));
            while (GetBufferFreeSpace < 2048) do
             begin
               if Assigned(FOnBufferProgress) then FOnBufferProgress(GetBufferCapacity);
             end;
         end;
      finally
         ReallocMem(Buf, 0);
         if Assigned(FOnWriteStatusEvent) then FOnWriteStatusEvent(BytesWritten);
         if Assigned(FOnBufferStatus) then FOnBufferStatus(BufferSize,BufferFreeSpace);
      end;
   end; //for loop

   FOnCDStatus('Finished Writing : Sync Cache');
   self.SyncCache;

   FOnCDStatus('Finished Writing : Closing Track :' + inttostr(CurrentTrack));
   self.CloseTrack(CurrentTrack);

   if CloseSession = True then
   begin
      FOnCDStatus('Finished Writing : Closing Session');
      self.CloseSession;
   end;
end;



function TCDBurner.GetWriteParameters: string;
var
   Params: string;
begin
   FLastError := ScsiGetWriteParams(FBurnerInfo, 0, Params, fDefaults);
   Result := Params;
end;


//+++++++++++++++++++++++++++++++++++++== end of writing functions +++++++++++++++++++++++++++++


function TCDBurner.GetDiscType: TScsiProfileDeviceDiscTypes;
begin
   FLastError := SCSIGetDevConfigProfileMedia(FBurnerInfo, Result, fDefaults);
end;

function TCDBurner.GetDVDescriptor: TScsiDVDLayerDescriptorInfo;
begin
   FLastError := SCSIReadDVDStructure(FBurnerInfo, Result, fDefaults);
end;

function TCDBurner.GetFormatCapacity: TFormatCapacity;
begin
   FLastError := SCSIReadFormatCapacity(FBurnerInfo, Result, fDefaults);
end;

function TCDBurner.GetTrackInfo(ATrack: Byte): TTrackInformation;
begin
   FLastError := SCSIReadTrackInformation(FBurnerInfo, ATrack, Result, fDefaults);
end;


function TCDBurner.AddAudioTrack(Filename: string): Boolean;
var
   Track: TCDTrackItem;
begin
   Result := False;
   if Fileexists(Filename) then
   begin
      Track := CDTracks.Add;
      Track.LoadWaveFile(FileName);
      Track.CDTrack.ConvertToPCM(Stereo16bit44100Hz);
      Result := True;
   end;
end;


Function TCDBurner.SaveAudioTrack(TrackID : Integer; Filename : String) : Boolean;
begin
   Result := False;
   if (TrackID < CDTracks.Count) then
   begin
      CDTracks.Tracks[TrackID].SaveWaveFile(FileName);
      Result := True;
   end;
end;





end.
