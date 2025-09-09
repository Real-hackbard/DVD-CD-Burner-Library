////////////////////////////////////////////////////////////////////////////
//                                                     									  //
// CD Tools unit (BlackCash soft 2007)			                              //
// Autor - BlackCash                                                		  //
// EMail - BlackCash2006@Yandex.ru			                            		  //
//                                                                			  //
////////////////////////////////////////////////////////////////////////////
// При использовании данного модуля в своих проектах обязательно сделайте //
// ссылку на автора этого модуля или укажите в форме о программе имя      //
// "BlackCash", как одного из авторов. 				                        	  //
////////////////////////////////////////////////////////////////////////////

unit CDToolComponent;

interface

uses
  Windows, Messages, SysUtils, Classes, CDISO, CDStream, Math, CDTools, CDCache, CDConst, SyncObjs, Dialogs, SPTI;

type
  {$IFNDEF ACTIVEX}
  TAddFileEvent = procedure (Sender: TObject; const FullPath: String; var LongFileName, ShortFileName: String; var DateTime: TDateTime; Attr: Integer; FileSize: Int64; var Skip: Boolean) of object;
  {$else}
  TAddFileEvent = procedure (Sender: TObject; const FullPath: String; var LongFileName, ShortFileName: String; var DateTime: TDateTime; Attr: Integer; FileSize: Integer; var Skip: Boolean) of object;
  {$ENDIF}
  TAddDirEvent = procedure (Sender: TObject; var LongName, ShortName: String; var Skip: Boolean) of object;
  TFileTestFailedEvent = procedure (Sender: TObject; const FullPath: String; var Stop: Boolean) of object;
  TWriteDoneEvent = procedure (Sender: TObject; Error: String) of object;

  TThWrite = class(TThread)
  private
    fImageSize: Int64;
    fCacheSize: Integer;
    fSaveISO: Boolean;
  protected
    procedure Execute; override;
    property ImageSize: Int64 read fImageSize write fImageSize;
  end;
  TCDToolBurner = class(TSCSIDevice)
  private
    { Private declarations }
    fOnAddFile: TAddFileEvent;
    fOnAddDir: TAddDirEvent;
    fOnTestFileFails: TFileTestFailedEvent;
    fIdVolume,
    fIdSystem,
    fIdVolumeSet,
    fIdPublisher,
    fIdPreparer,
    fIdApplication,
    fApplicationData2,
    fFileCopyright,
    fFileAbstract,
    fFileBibliographic: String;
    fDateCreation,
    fDateModification,
    fDateEffective,
    fDateExpiration: TDateTime;
    SettingsCanBeChanged,
    fParentDirectoryOnly,
    fJoliet: Boolean;
    fPrepared: Boolean;
    Depth: Integer;
    fBufferSize: Integer;
    fFilesSizeOnDisc: Int64;
    fStartAddress: Cardinal;
    BootCatalogLocation,
    BootImageLocation,
    BootImageSize,
    PathTableRecsLocationL,
    PathTableRecsLocationM,
    PathTableRecsLocationJL,
    PathTableRecsLocationJM,
    iPathTableSize,
    iPathTableSizeJ,
    FileDirDescriptorLocation,
    FileDirDescriptorLocationJ,
    iFileAndDirDescriptorWidth,
    iFileAndDirDescriptorWidthJ,
    fDataLocation: Integer;
    TotalNoOfSectors: Int64;
    fISOFileName: String;
    fRoot: PDirEntry;
    fOnWriteDone: TWriteDoneEvent;
    fOnFinalizingTrack: TNotifyEvent;
    fFinalizeTrack,
    fBootable,
    fReplaceFile,
    fWritePostGap,
    fPerformOPC: Boolean;
    fSessionToImport: SmallInt;
    FFilesSize: Int64;
    fBootImage: String;
    impVD: TVolumeDescriptor;
    fImageSize,
    fBytesWritten: Int64;
    GetAddress, WithOldSession: Boolean;
    fCDToolVersion: String;
    WriteDoneError:String;
    fFileInProcess: String;
    BuildHeaderISOFile: Boolean;
    BuildHeaderTargetDir: PDirEntry;
    procedure WriteDoneEvent;


  protected
    FileDirDescriptorExtentStart, FileDirDescriptorExtentEnd: Cardinal;
    ISOHeader: TMemoryStream;
    CurrentFile: PFileEntry;
    fAborted: Boolean;
    Pads: Int64;
  private
    procedure New_D(var P: PDirEntry);
    procedure New_F(var P: PFileEntry);
    procedure SetFileAddress;
    function  ImportSessionDirectoryJ(DirLocation, Size: Integer; DestinationDir: PDirEntry): Boolean;
    function  ImportSessionDirectory(DirLocation, Size: Integer; DestinationDir: PDirEntry): Boolean;
    function  GetNextWritableAddress: Boolean;
    function  GetLastRecordedAddress: DWORD;
    function  PathTableWidth: Integer;
    function  PathTableWidthJ: Integer;
    function  FileAndDirDescriptorWidth: Integer;
    function  FileAndDirDescriptorWidthJ: Integer;
    function  DoBurn(ISO: Boolean): Boolean;
    function  GetDirsCount: Integer;
    function  GetFilesCount: Integer;
    function  GetImageSize: Int64;
    procedure WritePVD;
    procedure WriteJVD;
    procedure WriteTVD;
    procedure WriteBVD;
    procedure WriteBootCatalog;
    procedure WritePathTable(Start: Integer; Most: Boolean);
    procedure WritePathTableJ(Start: Integer; Most: Boolean);
    procedure WriteFileAndDirDescriptor_r(d: PDirEntry; Start, FileStart: Integer);
    procedure WriteFileAndDirDescriptor(Start, FileStart: Integer);
    procedure WriteFileAndDirDescriptorJ_r(d: PDirEntry; Start, FileStart: Integer);
    procedure WriteFileAndDirDescriptorJ(Start, FileStart: Integer);
    procedure WriteFiles;
    procedure SetBufferSize(Value: Integer);
    procedure Print_Files(Files: PFileEntry);
    procedure SetCDtoolVersion(Value: String);
    function  MakeDir(DirName: String): PDirEntry;
  public
    { Pufblic declarations }
    procedure Print_D1;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Abort;

    function  Initialize: Boolean;
    function  GetDirSize(Path: String): Int64;

    function  CreateDir(DestinationPath: PDirEntry; DirName: String; Attr: Integer = faDirectory): PDirEntry; overload;
    function  CreateDir(DestinationPath: String; DirName: String): PDirEntry; overload;
    function  CreateDir(DirName: String): PDirEntry; overload;
    function  CreateDir(DestinationPath: PDirEntry; sr: TSearchRec): PDirEntry; overload;

    function  MoveFile(DestinationPath, SourcePath: PDirEntry; SourceFile: PFileEntry): Boolean; overload;
    function  MoveFile(DestinationPath, SourcePath, SourceFile: String): Boolean; overload;

    function  RemoveFile(SourceDir: String; SourceFile: String): Boolean; overload;
    function  RemoveFile(var SourceDir: PDirEntry; var SourceFile: PFileEntry): Boolean; overload;
    function  RemoveDir(var SourceDir: PDirEntry): Boolean; overload;
    function  RemoveDir(SourceDir: String): Boolean; overload;
    function  RemoveEmptyDirs: Boolean;
    function  ResetFilesArchiveBit: Boolean;
    function  TestFiles: Boolean;
    function  ResetAllFilesArchiveBit: Boolean;
    function  RemoveDir_r(var SourceFile: PFileEntry): Boolean;

    function  RenameFile(SourceDir, SourceFile: String; NewLongName, NewShortName: String): Boolean; overload;
    function  RenameFile(SourceFile: PFileEntry; NewLongName, NewShortName: String): Boolean; overload;

    function  InsertFile(DestinationPath: String; FilePath: String; SavePath: Boolean = False): Integer; overload;
    function  InsertFile(DestinationPath: PDirEntry; sr: TSearchRec; FilePath: String; OrignalAddress: Integer = 0; ResetArchiveBit: Boolean = False): Integer; overload;
    function  InsertFileWithName(DestinationPath: String; FilePath: String; ShortNameOnDisc, LongNameOnDisc: String): Integer;
    function  InsertMemoryFile(DestinationPath, LongFileName, ShortFileName: String; Attr: Byte; Memory: Pointer; Size: Cardinal): Integer;
    
    function  InsertDir(DestinationPath, SourcePath: String; FileSpecs: String='*.*'; Attr: Integer = faAnyFile; Recursive: Boolean=True; SavePath: Boolean = False; ArchiveOnly: Boolean = False): Integer; overload;
    function  InsertDir(DestinationPath: PDirEntry; SourcePath: String; FileSpecs: String='*.*'; Attr: Integer = faAnyFile; Recursive: Boolean=True; SavePath: Boolean = False; ArchiveOnly: Boolean = False): Integer; overload;


    function  FindFile(Dir: PDirEntry; FileName: String): PFileEntry;
    function  FindDir(DirName: String): PDirEntry;

    function  ExtractFile(FileToExtract: PFileEntry; TargetFile: String): Boolean;
    function  BurnISOImage(ISOFileName: String): Boolean;
    function  Prepare(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
    function  PrepareHeader(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
    function  BuildHeader(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
    function  PrepareCD: Boolean;
    function  PrepareISO: Boolean;
    function  BurnCD: Boolean;
    function  SaveToISOFile(ISOFileName: String; QuickSave: Boolean = False): Integer;

    function  ClearAll(Max_Files: Integer = 65535; Max_Dirs: Integer = 8191): Boolean;
    function  BufferProgress: Integer;
    function  ImportSession(SessionNo: ShortInt; DestinationDir: PDirEntry): Boolean;
    function  GetDevice(Num: Byte): String;
    function  DevicesFound: Byte; overload;
    property  ApplicationData2: String read fApplicationData2 write fApplicationData2;  
    property  SessionToImport: SmallInt read fSessionToImport write fSessionToImport;
    property  RootDir: PDirEntry read fRoot;
    property  BytesWritten: Int64 read fBytesWritten;
    property  ImageSize: Int64 read GetImageSize;
    property  DirsCount: Integer read GetDirsCount;
    property  FilesCount: Integer read GetFilesCount;
    property  FilesSize: Int64 read FFilesSize;
    property  FileInProcess: String read fFileInProcess;
    property  Bootable: Boolean read fBootable write fBootable default False;
    property  ParentDirectoryOnly: Boolean read fParentDirectoryOnly write fParentDirectoryOnly default False;
    property  BootImage: String read fBootImage write fBootImage;
    property  Disc;
    property  Devices;
    property  DeviceName;
    property  HostAdapters;
    property  ErrorString;
    property  DeviceCapabilities;
    property  InquriyData;

  published
    { Published declarations }
    property ASPIInitialized;
    property DeviceMaxWriteSpeed;
    property DeviceMaxReadSpeed;
    property DeviceBufferSize;
    property DeviceFreeBufferSize;
    property UnderrunProtection;
    property DoDebug;
    property OnDeviceChange;
    property OnEraseDone;
    property ReadSpeed;
    property WriteSpeed;
    property Erasing;
    property FinalizeDisc;
    property TestWrite;
    property ReplaceFile: Boolean read fReplaceFile write fReplaceFile;
    property FinalizeTrack: Boolean read fFinalizeTrack write fFinalizeTrack default True;
    property PerformOPC: Boolean read fPerformOPC write fPerformOPC;
    property IdVolume: String read fIdVolume write fIdVolume;
    property IdSystem: String read fIdSystem write fIdSystem;
    property IdVolumeSet: String read fIdVolumeSet write fIdVolumeSet;
    property IdPublisher: String read fIdPublisher write fIdPublisher;
    property IdPreparer: String read fIdPreparer write fIdPreparer;
    property IdApplication: String read fIdApplication write fIdApplication;
    property FileCopyright: String read fFileCopyright write fFileCopyright;
    property FileAbstract: String read fFileAbstract write fFileAbstract;
    property FileBibliographic: String read fFileBibliographic write fFileBibliographic;
    property DateCreation: TDateTime read fDateCreation write fDateCreation;
    property DateModification: TDateTime read fDateModification write fDateModification;
    property DateEffective: TDateTime read fDateEffective write fDateEffective;
    property DateExpiration: TDateTime read fDateExpiration write fDateExpiration;
    property CacheSize: Integer read fBufferSize write SetBufferSize;
    property ISOFileName: String read fISOFileName write fISOFileName;
    property JolietFileSystem: Boolean read fJoliet write fJoliet default True;
    property OnAddFile: TAddFileEvent read fOnAddFile write fOnAddFile;
    property OnAddDir: TAddDirEvent read fOnAddDir write fOnAddDir;
    property OnWriteDone: TWriteDoneEvent read fOnWriteDone write fOnWriteDone;
    property OnFinalizingTrack: TNotifyEvent read fOnFinalizingTrack write fOnFinalizingTrack;
    property Version: String read fCDToolVersion write SetCDToolVersion;
    property WritePostGap: Boolean read fWritePostGap write fWritePostGap;

  end;

procedure Register;

const
  fSuf = ';1';
var
  buffer: array [0..253] of Byte;
  WriteBuffer: array[0..MaxWord * 100] of char;
  vds: array[0..32 * 2048] of char;
  vdsSize: Integer;
  IsPrev: Boolean = False;
  Valid: Boolean = True;
  ELen: Integer = 0;

implementation

uses ASPI;

var
  CDToolBurner: TCDToolBurner;
  ZEROS: array[0..DefaultSectorSize] of char;
  f: File;
  wt: TThWrite;
  Burning: Boolean = False;

procedure Register;
begin
  RegisterComponents('CD\DVD Tools v1.00 Demo', [TCDToolBurner]);
end;

procedure TCDToolBurner.SetCDToolVersion(Value: String);
begin
  Value := Value;
end;

function  TCDToolBurner.GetDevice(Num: Byte): String;
begin
  result := Devices[Num];
end;
 
{                                                                              }
 
function  TCDToolBurner.DevicesFound: Byte;
begin
  result := Devices.Count;
end;
 
 
 
constructor TCDToolBurner.Create;
begin
  ISOHeader := TMemoryStream.Create;
  ISOHeader.SetSize(16777216 * 4);
  fWritePostGap := True;
  Initialize;
  SetBufferSize(4 * 1024 * 1024); // 4 MB
  fJoliet := True;
  fFinalizeTrack := True;
  fCDToolVersion := '1.21';
  UnderrunProtection := True;
  Inherited;
end;
 
 
 
function  TCDToolBurner.GetDirsCount: Integer;
begin
  result := DirCounter-1;
end;
 
 
 
function  TCDToolBurner.GetFilesCount: Integer;
begin
  result := FileCounter;
end;

function TCDToolBurner.GetImageSize: Int64;
begin
  if fWritePostGap and (fImageSize < 300) then
    result := 300
  else
    result := fImageSize;
end;

 
 
 
destructor TCDToolBurner.Destroy;
var
  i: Integer;
  d: PDirEntry;
  f: PFileEntry;
begin
  ISOHeader.Clear;
  ISOHeader.SetSize(16777216 * 4);
  for i:=0 to DirCounter-1 do
  begin
    if Dirs[i] <> nil then
    begin
      d := Dirs[i];
      d.ShortName := '';
      d.LongName := '';
      d.Path := '';
      Dispose(Dirs[i]);
      Dirs[i] := nil;
    end;
  end;

  for i:=0 to FileCounter-1 do
  begin
    if Files[i] <> nil then
    begin
      f := Files[i];
      f.Path := '';
      f.ShortName := '';
      f.LongName := '';
      Dispose(Files[i]);
      Files[i] := nil;
    end;
  end;
  ISOHeader.Free;
  Inherited;
end;
 
 
 
function LToMW(w: Word): Word;
begin
  result := ((w shl 8) and $FF00) or ((w shr 8) and $00FF);
end;
 
 
 
procedure TCDToolBurner.SetBufferSize(Value: Integer);
begin
  if Value < 2 * 1024 * 1024 then
    Value := 2 * 1024 * 1024;
  Value := Value div DefaultSectorSize;
  Value := Value * DefaultSectorSize;
  fBufferSize := Value;
end;
 
 
 
function TCDToolBurner.ClearAll(Max_Files, Max_Dirs: Integer): Boolean;
var
  i: Integer;
  d: PDirEntry;
  f: PFileEntry;
begin
  GetTimeZoneInformation(TimeZoneInformation);
  TimeZoneDiff := (-TimeZoneInformation.Bias div 15) {+ (-TimeZoneInformation.DaylightBias div 15)};
  ISOHeader.Clear;
  for i:=0 to DirCounter-1 do
  begin
    if Dirs[i] <> nil then
    begin
      d := Dirs[i];
      d.ShortName := '';
      d.LongName := '';
      d.Path := '';
      Dispose(Dirs[i]);
      Dirs[i] := nil;
    end;
  end;
  for i:=0 to FileCounter-1 do
  begin
    if Files[i] <> nil then
    begin
      f := Files[i];
      f.ShortName := '';
      f.LongName := '';
      f.Path := '';
      Dispose(Files[i]);
      Files[i] := nil;
    end;
  end;
  SetLength(Dirs, 0);
  SetLength(PathTable, 0);
  SetLength(PathTableJ, 0);
  SetLength(Files, 0);
  MaxDirs := Max_Dirs;
  MaxFiles := Max_Files;
  SetLength(Dirs, Max_Dirs);
  SetLength(PathTable, Max_Dirs);
  SetLength(PathTableJ, Max_Dirs);
  SetLength(Files, Max_Files);

  FFilesSize := 0;
  SettingsCanBeChanged := True;
  FileCounter := 0;
  DirCounter := 0;
  fillchar(ZEROS, DefaultSectorSize, 0);
  Depth := 1;
  New_D(fRoot);
  fRoot.Files := nil;
  fRoot.Path := '\';
  fRoot.ShortName := 'CD_ROOT';
  fRoot.LongName := 'CD Root Directory';
  fRoot.Depth := Depth;
  fRoot.Parent := fRoot;
  Result := True;
end;
 
 
 
procedure TCDToolBurner.New_D(var P: PDirEntry);
begin
  if DirCounter >= MaxDirs then
  begin
    DebugMsg(Format('>>> '+ERR_MAXDIRS, [MaxDirs]), mtNONFATALERROR);
    P := nil;
    exit;
  end;
  New(P);
  P.Imported := False;
  P.Order := 0;
  Dirs[DirCounter] := p;
  PathTable[DirCounter] := p;
  PathTableJ[DirCounter] := p;
  Inc(DirCounter);
  SettingsCanBeChanged := False;
end;
 
 
 
procedure TCDToolBurner.New_F(var P: PFileEntry);
begin
  if FileCounter >= MaxFiles then
  begin
    DebugMsg(Format(ERR_MAXFILES, [MaxFILES]), mtNONFATALERROR);
    P := nil;
    exit;
  end;
  New(P);
  Files[FileCounter] := p;
  Inc(FileCounter);
  P.Imported := False;
  P.Prev := False;
  P.SpaceReqOnDisc := 0;
  P.Buffer := nil;
  SettingsCanBeChanged := False;
end;
 
 
 
procedure TCDToolBurner.WriteFiles;
var
  i: Integer;
  p: PFileEntry;
  ChunkSize: Integer;
  BytesLeft, BytesToRead: Integer;
  NumRead, NumWritten: Integer;
  Buf: array[1..128*1024] of char;
  src: File;
begin
  ChunkSize := 128*1024;
  for i := 0 to FileCounter-1 do
  begin
    p := Files[i];
    if ((p.Attr and faDirectory) <> faDirectory) then
    begin
      FileMode := $0; AssignFile(src, p.Path); Reset(src, 1); FileMode := $2;
      if p.FileSize <> 0 then
      begin
        if (p.AddressJ+(fDataLocation)) <> filepos(f) div 2048 then
          DebugMsg('>>> '+ERR_4 + ' '+p.Path+' '+IntToStr(p.AddressJ+(fDataLocation))+'<>'+IntToStr(filepos(f) div 2048), mtFATALERROR);
        BytesLeft := p.FileSize;
        repeat
          if ChunkSize > BytesLeft then
          begin
            BytesToRead := BytesLeft;
            Fillchar(buf[BytesToRead], ChunkSize-BytesLeft, 0);
          end
          else
            BytesToRead := ChunkSize;
          BlockRead(src, Buf, BytesToRead, NumRead);
          BytesLeft := BytesLeft - BytesToRead;
          BlockWrite(F, Buf, Sectors(BytesToRead)*2048, NumWritten);
        until (NumRead = 0) or (Sectors(BytesToRead)*2048 <> NumRead) or (BytesLeft = 0);
        CloseFile(src);
      end;
    end;
  end;
end;
 
 
 
function FileAndDirDescriptorWidth_r(d: PDirEntry): Integer;
var
  l, W, bytes: Integer;
  p: PFileEntry;
  fn: String;
begin
  p := d.Files;
  if (d.Files = nil) or (p.ShortName <> '.') then
  begin
    w := 68+ELen+ELen;
    bytes := DefaultSectorSize-w;
  end
  else
  begin
    bytes := DefaultSectorSize;
    w := 0;
  end;
  while p <> nil do
  begin
    if (p.Attr and faDirectory <> faDirectory) then
      fn := p.ShortName+fSuf
    else
      fn := p.ShortName;
    l := Length(fn);
    if (fn = '.') or (fn = '..') then
      l := 1
    else if l mod 2 = 0 then
      l := l + 1;
    l := l + 33+ELen;
    if bytes - l < 0 then
    begin
      w := w + bytes + l;
      bytes := DefaultSectorSize - l;
    end
    else
    begin
      bytes := bytes - l;
      w := w + l;
    end;
    p := p.Next;
  end;
  w := w + bytes;
  result := Sectors(w)*DefaultSectorSize;
end;
 
 
 
function TCDToolBurner.FileAndDirDescriptorWidth: Integer;
var
  i: Integer;
  a, w, ww: Integer;
  d: PDirEntry;
begin
  ww := 0;
  a := 0;
  for i:=0 to DirCounter-1 do
  begin
    d := dirs[i];
    w := FileAndDirDescriptorWidth_r(d);
    ww := ww + w;
    d.Address := a;
    d.Size := w;
    a := a + w div DefaultSectorSize;
  end;
  result := ww;
end;
 
 
 
function SetExt(Buffer: PChar; Flag: Word; Ext: Boolean; Number: Byte): Boolean;
begin
  if (Flag and 2) = 2 then
    Buffer[5] := Chr(ELen+127)
  else
    if Ext then
      Buffer[5] := Chr(ELen+7)
    else
      Buffer[5] := Chr(ELen-1);
  Buffer[1] := #0;
  Buffer[2] := #0;
  Buffer[3] := #0;
  Buffer[4] := #0;
  Buffer[6] := Chr(ELen+71);
  Buffer[7] := Chr(ELen+74);
  Buffer[8] := Chr(ELen+51);
  Buffer[9] := Chr(Number);
  Buffer[10] := #0;
  Buffer[11] := #0;
  Buffer[12] := #0;
  Buffer[13] := #0;
  Buffer[14] := #0;
  if ((Flag and 2) = 2) or not Ext then
    Buffer[9] := #0;
  Result := True;
end;
 
 
 
procedure TCDToolBurner.WriteFileAndDirDescriptor_r(d: PDirEntry; Start, FileStart: Integer);
var
  l, ll,  bytes: Integer;
  p: PFileEntry;
  fn: String;
  fd: TDirectoryDescriptor;
begin
  ll := 0;
  p := d.Files;
  fillchar(fd, sizeof(fd), 0);
  fd.FileUnitSize := 0;
  fd.InterleaveGap := 0;
  if (d.Files = nil) or (p.ShortName <> '.')  then
  begin
    fd.LenDr := 34+ELen;
    fd.FileName[0] := #0;
    fd.LenOfFileIdentifier := 1;
    fd.FileFlag := 2;
    fd.Address := d.Address+Start;
    fd.AddressBE := L2MDW(d.Address+Start);
    fd.DataLength := d.Size;
    fd.DataLengthBE := L2MDW(d.Size);
    fd.VolSeqnumber := 1;
    fd.VolSeqnumberBE := 256;
    SetDateTime(Now, fd);
    if ELen <> 0 then
      SetExt(@fd.FileName[0], fd.FileFlag, False, 0);
    ISOHeader.write(fd, 34+ELen);

    fd.FileName[0] := #1;
    fd.Address := d.Parent.Address+Start;
    fd.AddressBE := L2MDW(d.Parent.Address+Start);
    fd.DataLength := d.Parent.Size;
    fd.DataLengthBE := L2MDW(d.Parent.Size);
    if ELen <> 0 then
      SetExt(@fd.FileName[0], fd.FileFlag, False, 0);
    ISOHeader.Write(fd, 34+ELen);
    bytes := DefaultSectorSize-(68+ELen+ELen);
  end
  else
  begin
    bytes := DefaultSectorSize;
  end;
  while p <> nil do
  begin
    if p.Imported then
    fillchar(fd, sizeof(fd), 0);
    if (p.Attr and faDirectory <> faDirectory) then
    begin
      fn := p.ShortName+fSuf;
      fd.FileFlag := 0;
      if not p.Imported then
      begin
        fd.Address := p.Address+FileStart;
        fd.AddressBE := L2MDW(p.Address+FileStart);
      end
      else
      begin
        fd.Address := p.Address;
        fd.AddressBE := L2MDW(p.Address);
      end;
      fd.DataLength := p.FileSize;
      fd.DataLengthBE := L2MDW(p.FileSize);
    end
    else
    begin
      fd.FileFlag := 2;
      fn := p.ShortName;
      fd.Address := p.Address+Start;
      fd.AddressBE := L2MDW(p.Address+Start);
      fd.DataLength := p.FileSize;
      fd.DataLengthBE := L2MDW(p.FileSize);
    end;
    l := Length(fn);
    if fn = '.' then
    begin
      l := 1;
      fd.FileName[0] := #0;
      fd.LenOfFileIdentifier := l;
    end
    else if fn = '..' then
    begin
      l := 1;
      fd.FileName[0] := #1;
      fd.LenOfFileIdentifier := l;
    end
    else
    if l mod 2 = 0 then
    begin
      CopyToArray(fn, fd.FileName, L);
      fd.FileName[l] := #0;
      fd.LenOfFileIdentifier := l;
      l := l + 1;
      ll := 0;
    end
    else
    begin
      CopyToArray(fn, fd.FileName, L);
      fd.LenOfFileIdentifier := l;
      ll := -1;
    end;
    if (p.Attr and faHidden = faHidden) then
       fd.FileFlag := fd.FileFlag or 1;
    l := l + 33+ELen;
    if ELen <> 0 then
      SetExt(@fd.FileName[fd.LenOfFileIdentifier+ll], fd.FileFlag, p.Prev, 1);

    if bytes - l < 0 then
    begin
      ISOHeader.Write(ZEROS, bytes);
      bytes := DefaultSectorSize - l;
    end
    else
    begin
      bytes := bytes - l;
    end;
    fd.Extended := 0;
    fd.LenDr := l;
    fd.VolSeqnumber := 1;
    fd.VolSeqnumberBE := 256;
    if (p.Attr and faDirectory <> faDirectory) and (p.FileSize = 0) then
    begin
      fd.Address := 0;
      fd.AddressBE := 0;
    end;
    SetDateTime(p.Time, fd);
    ISOHeader.Write(fd, l);
    p := p.Next;
  end;
  ISOHeader.Write(ZEROS, bytes);
end;
 
 
 
procedure TCDToolBurner.WriteFileAndDirDescriptor(Start, FileStart: Integer);
var
  i: Integer;
  d: PDirEntry;
begin
  for i:=0 to DirCounter-1 do
  begin
    d := dirs[i];
    WriteFileAndDirDescriptor_r(d, Start, FileStart);
  end;
end;
 
 
 
function FileAndDirDescriptorWidthJ_r(d: PDirEntry): Integer;
var
  l, W, bytes: Integer;
  p: PFileEntry;
  fn: String;
  wfn: WideString;
begin
  p := d.Files;
  if (d.Files = nil) or (p.LongName <> '.') then
  begin
    w := 68+ELen+ELen;
    bytes := DefaultSectorSize-w;
  end
  else
  begin
    bytes := DefaultSectorSize;
    w := 0;
  end;
  while p <> nil do
  begin
    if (p.Attr and faDirectory <> faDirectory) then
      fn := p.LongName+fSuf
    else
      fn := p.LongName;
    wfn := fn;
    l := Length(wfn)*2;
    if (fn = '.') or (fn = '..') then
      l := 1
    else if l mod 2 = 0 then
      l := l + 1;
    l := l + 33+ELen;
    if bytes - l < 0 then
    begin
      w := w + bytes + l;
      bytes := DefaultSectorSize - l;
    end
    else
    begin
      bytes := bytes - l;
      w := w + l;
    end;
    p := p.Next;
  end;
  w := w + bytes;
  result := Sectors(w)*DefaultSectorSize;

end;
 
 
 
function TCDToolBurner.FileAndDirDescriptorWidthJ: Integer;
var
  i: Integer;
  a, w, ww: Integer;
  d: PDirEntry;
begin
  ww := 0;
  a := 0;
  for i:=0 to DirCounter-1 do
  begin
    d := dirs[i];
    w := FileAndDirDescriptorWidthJ_r(d);
    ww := ww + w;
    d.AddressJ := a;
    d.SizeJ := w;
    a := a + w div DefaultSectorSize;
  end;
  result := ww;
end;
 
 
 
procedure TCDToolBurner.WriteFileAndDirDescriptorJ_r(d: PDirEntry; Start, FileStart: Integer);
var
  l, ll, bytes: Integer;
  p: PFileEntry;
  fn: String;
  wfn: WideString;
  fd: TDirectoryDescriptor;

begin
  ll := 0;
  fillchar(fd, sizeof(fd), 0);
  p := d.Files;
  fd.FileUnitSize := 0;
  fd.InterleaveGap := 0;
  if (d.Files = nil) or (p.LongName <> '.') then
  begin
    fd.LenDr := 34+ELen;
    fd.FileName[0] := #0;
    fd.LenOfFileIdentifier := 1;
    fd.FileFlag := 2;
    fd.Address := d.AddressJ+Start;
    fd.AddressBE := L2MDW(d.AddressJ+Start);
    fd.DataLength := d.SizeJ;
    fd.DataLengthBE := L2MDW(d.Size);
    fd.VolSeqnumber := 1;
    fd.VolSeqnumberBE := 256;
    SetDateTime(Now, fd);
    if ELen <> 0 then
      SetExt(@fd.FileName[0], fd.FileFlag, False, 0);
    ISOHeader.write(fd, 34+ELen);

    fd.FileName[0] := #1;
    fd.Address := d.Parent.AddressJ+Start;
    fd.AddressBE := L2MDW(d.Parent.AddressJ+Start);
    fd.DataLength := d.Parent.SizeJ;
    fd.DataLengthBE := L2MDW(d.Parent.Size);
    if ELen <> 0 then
      SetExt(@fd.FileName[0], fd.FileFlag, False, 1);
    ISOHeader.Write(fd, 34+ELen);
    bytes := DefaultSectorSize-(68+ELen+ELen);
  end
  else
    bytes := DefaultSectorSize;
  while p <> nil do
  begin
    fillchar(fd, sizeof(fd), 0);
    if (p.Attr and faDirectory <> faDirectory) then
    begin
      fn := p.LongName+fSuf;
      fd.FileFlag := 0;
      if not p.Imported then
      begin
        fd.Address := p.AddressJ+FileStart;
        fd.AddressBE := L2MDW(p.AddressJ+FileStart);
      end
      else
      begin
        fd.Address := p.AddressJ;
        fd.AddressBE := L2MDW(p.AddressJ);
      end;
      fd.DataLength := p.FileSizeJ;
      fd.DataLengthBE := L2MDW(p.FileSizeJ);
    end
    else
    begin
      fd.FileFlag := 2;
      fn := p.LongName;
      fd.Address := p.AddressJ+Start;
      fd.AddressBE := L2MDW(p.AddressJ+Start);
      fd.DataLength := p.FileSizeJ;
      if p.dirRec.SizeJ <> p.FileSizeJ then
      begin
        //.. error
      end;
      fd.DataLengthBE := L2MDW(p.FileSizeJ);
    end;
    wfn := fn;
    l := Length(wfn)*2;
    if fn = '.' then
    begin
      l := 1;
      fd.FileName[0] := #0;
      fd.LenOfFileIdentifier := l;
    end
    else if fn = '..' then
    begin
      l := 1;
      fd.FileName[0] := #1;
      fd.LenOfFileIdentifier := l;
    end
    else if l mod 2 = 0 then
    begin
      CopyToArrayW(fn, fd.FileName, L);
      fd.FileName[l] := #0;
      fd.LenOfFileIdentifier := l;
      l := l + 1;
      ll := 0;
    end
    else
    begin
      CopyToArrayW(fn, fd.FileName, L);
      fd.LenOfFileIdentifier := l;
      ll := -1;
    end;
    if (p.Attr and faHidden = faHidden) then
     fd.FileFlag := fd.FileFlag or 1;
    fd.LenDr := l+ELen;
    l := l + 33+ELen;
    if ELen <> 0 then
      SetExt(@fd.FileName[fd.LenOfFileIdentifier+ll], fd.FileFlag, p.Prev, 1);

    if bytes - l < 0 then
    begin
      ISOHeader.Write(ZEROS, bytes);
      bytes := DefaultSectorSize - l;
    end
    else
    begin
      bytes := bytes - l;
    end;
    fd.Extended := 0;
    if (l > 230) then
      l := l;
    fd.LenDr := l;
    fd.VolSeqnumber := 1;
    fd.VolSeqnumberBE := 256;
    SetDateTime(p.Time, fd);
    if (p.Attr and faDirectory <> faDirectory) and (p.FileSize = 0) then
    begin
      fd.Address := 0;
      fd.AddressBE := 0;
    end;
    ISOHeader.Write(fd, l);
    p := p.Next;
  end;
  ISOHeader.Write(ZEROS, bytes);
end;
 
 
 
procedure TCDToolBurner.WriteFileAndDirDescriptorJ(Start, FileStart: Integer);
var
  i: Integer;
  d: PDirEntry;
begin
  for i:=0 to DirCounter-1 do
  begin
    d := dirs[i];
    WriteFileAndDirDescriptorJ_r(d, Start, FileStart);
  end;
end;
 
 
 
function TCDToolBurner.PathTableWidthJ: Integer;
var
  i, Len, TotalLen: Integer;
  p: PDirEntry;
  wfn: WideString;
begin
  TotalLen := 10;
  for i := 1 to DirCounter-1 do
  begin
    p := Dirs[i];
    wfn := p.LongName;
    Len := Length(wfn)*2;
    if len mod 2 <> 0 then
      Len := len + 1;
    Len := Len+8;
    Totallen := TotalLen + Len;
  end;
  iPathTableSizeJ := TotalLen;
  result := Sectors(TotalLen)*DefaultSectorSize;
end;
 
 
 
procedure TCDToolBurner.WritePathTableJ(Start: Integer; Most: Boolean);
var
  i, TotalLen, Len: Integer;
  p: PDirEntry;
  wfn: WideString;
  pt: TPathTableRecord;
begin
  TotalLen := 10;
  pt.Name[0] := #0;
  pt.Name[1] := #0;
  pt.ExtAttr := 0;
  if Most then
  begin
    pt.Address := L2MDW(Start);
    pt.ParentNumber := LTOMW(1);
  end
  else
  begin
    pt.Address := Start;
    pt.ParentNumber := 1;
  end;
  pt.LenDI := 1;
  ISOHeader.Write(pt, 10);
  for i := 1 to DirCounter-1 do
  begin
    p := PathTableJ[i];
    wfn := p.LongName;
    Len := Length(wfn)*2;
    CopyToArrayW(p.LongName, pt.Name, Len);
    pt.LenDI := Len;
    if len mod 2 <> 0 then
    begin
      Len := len + 1;
      pt.Name[Len-1] := #0;
    end;
    Len := Len+8;
    if Most then
    begin
      pt.Address := L2MDW(p.AddressJ+Start);
      pt.ParentNumber := LToMW(p.Parent.Number);
    end
    else
    begin
      pt.Address := p.AddressJ+Start;
      pt.ParentNumber := p.Parent.Number;
    end; 
    ISOHeader.Write(pt, Len);
    Totallen := TotalLen + Len;
  end;
  if TotalLen - DefaultSectorSize <> 0 then
    ISOHeader.Write(ZEROS, DefaultSectorSize-(TotalLen mod DefaultSectorSize));
end;
 
 
 
function TCDToolBurner.PathTableWidth: Integer;
var
  i, Len, TotalLen: Integer;
  p: PDirEntry;
begin
  TotalLen := 10;
  for i := 1 to DirCounter-1 do
  begin
    p := Dirs[i];
    Len := Length(p.ShortName);
    if len mod 2 <> 0 then
      Len := len + 1;
    Len := Len+8;
    Totallen := TotalLen + Len;
  end;
  iPathTableSize := TotalLen;
  result := Sectors(TotalLen)*DefaultSectorSize;
end;
 
 
 
procedure TCDToolBurner.WritePathTable(Start: Integer; Most: Boolean);
var
  i, TotalLen, Len: Integer;
  p: PDirEntry;
  pt: TPathTableRecord;
begin

  TotalLen := 10;
  pt.Name[0] := #0;
  pt.Name[1] := #0;
  pt.ExtAttr := 0;
  if Most then
  begin
    pt.Address := L2MDW(Start);
    pt.ParentNumber := LTOMW(1);
  end
  else
  begin
    pt.Address := Start;
    pt.ParentNumber := 1;
  end;
  pt.LenDI := 1;
  ISOHeader.Write(pt, 10);
  for i := 1 to DirCounter-1 do
  begin
    p := PathTable[i];
    Len := Length(p.ShortName);
    CopyToArray(p.ShortName, pt.Name, Len);
    pt.LenDI := Len;
    if len mod 2 <> 0 then
    begin
      Len := len + 1;
      pt.Name[Len-1] := #0;
    end;
    Len := Len+8;
    if Most then
    begin
      pt.Address := L2MDW(p.Address+Start);
      pt.ParentNumber := LToMW(p.Parent.Number);
    end
    else
    begin
      pt.Address := p.Address+Start;
      pt.ParentNumber := p.Parent.Number;
    end;
    ISOHeader.Write(pt, Len);
    Totallen := TotalLen + Len;
  end;
  if TotalLen - DefaultSectorSize <> 0 then
    ISOHeader.Write(ZEROS, DefaultSectorSize-(TotalLen mod DefaultSectorSize));
end;
 
 
 
procedure TCDToolBurner.SetFileAddress;
var
  i: Integer;
  p: PFileEntry;
begin
  fFilesSizeOnDisc := 0;
  for i:=0 to FileCounter-1 do
  begin
    p := Files[i];
    if not p.Imported then
    begin
      if (p.Attr and faDirectory <> faDirectory) then
      begin
        if p.FileSize = 0 then
        begin
          p.FileSizeJ := 0;
          p.Address := 0;
          p.AddressJ := 0;
        end
        else
        begin
          p.FileSizeJ := p.FileSize;
          p.Address := fFilesSizeOnDisc;
          p.AddressJ := fFilesSizeOnDisc;
        end;
      end
      else
      begin
        p.Address := p.DirRec.Address;
        p.AddressJ := p.DirRec.AddressJ;
        p.FileSize := p.DirRec.Size;;
        p.FileSizeJ := p.DirRec.SizeJ;
      end;
      if (p.Attr and faDirectory <> faDirectory) then
      begin
        if p.Prev then
        begin
          fFilesSizeOnDisc := fFilesSizeOnDisc + Sectors2(p);
        end
        else
          fFilesSizeOnDisc := fFilesSizeOnDisc + Sectors(p.FileSize);
      end;
    end;
  end;
end;
 
 
 
procedure TCDToolBurner.WritePVD;
var
  rd: PDirEntry;
  PVD: TVolumeDescriptor;
begin
  FillChar(PVD, SizeOf(PVD), 0);
  with PVD do
  begin
    pdType := 1;
    Identifier := 'CD001';
    Version := 1;
    VolumeFlag := 0;
    rd := Dirs[0];
    RootDirRec.LenDr := 34;
    RootDirRec.Extended := 0;
    RootDirRec.FileFlag := 2;
    RootDirRec.Address := FileDirDescriptorLocation;
    RootDirRec.AddressBE := L2MDW(FileDirDescriptorLocation);
    RootDirRec.VolSeqnumber := 1;
    RootDirRec.VolSeqnumberBE := 256;
    RootDirRec.DataLength := rd.Size;
    RootDirRec.DataLengthBE := L2MDW(rd.Size);
    RootDirRec.LenOfFileIdentifier := 1;
    NoOfSectors := TotalNoOfSectors;
    NoOfSectorsBE := L2MDW(TotalNoOfSectors);
    Type1PathTable := PathTableRecsLocationL;
    TypeMPathTable := L2MDW(PathTableRecsLocationM);
    TypeMPathTableBE := 0;
    if fJoliet then
    begin
      PVD.EscapeChars[0] := '%'; PVD.EscapeChars[1] := '/'; PVD.EscapeChars[2] := '@';
    end;
    PathTableSize  := iPathTableSize;
    PathTableSizeBE := L2MDW(iPathTableSize);
    CopyToArray(UpperCase(fIdVolume), IdVolume, SizeOf(PVD.IdVolume));
    CopyToArray(UpperCase(fIdSystem), IdSystem, SizeOf(PVD.IdSystem));
    CopyToArray(UpperCase(fIdVolumeSet), IdVolumeSet, SizeOf(PVD.IdVolumeSet));
    CopyToArray(UpperCase(fIdPublisher), IdPublisher, SizeOf(PVD.IdPublisher));
    CopyToArray(UpperCase(fIdPreparer), IdPreparer, SizeOf(PVD.IdPreparer));
    CopyToArray(UpperCase(fIdApplication), IdApplication, SizeOf(PVD.IdApplication));
    CopyToArray(UpperCase(fFileCopyright), FileCopyright, SizeOf(PVD.FileCopyright));
    CopyToArray(UpperCase(fFileAbstract), FileAbstract, SizeOf(PVD.FileAbstract));
    CopyToArray(UpperCase(fFileBibliographic), FileBibliographic, SizeOf(PVD.FileBibliographic));
    CopyToArray(UpperCase(fFileBibliographic), FileBibliographic, SizeOf(PVD.FileBibliographic));
    CopyToArray(FormatDateTime('yyyymmddhhnnsszz',fDateCreation), DateCreation,16);
    CopyToArray(FormatDateTime('yyyymmddhhnnsszz',fDateModification), DateModification,16);
    CopyToArray(UpperCase(fApplicationData2), ApplicationData2, 8);
    CopyToArray('0000000000000000', DateExpiration,16);
    CopyToArray('0000000000000000', DateEffective,16);
    PVD.SectorSize := DefaultSectorSize;
    PVD.SectorSizeBE := LToMW(DefaultSectorSize);
    VolSetSize := 1;
    VolSetSizeBE := 256;
    VolSeqNumber := 1;
    VolSeqNumberBE := 256;
    FileStructureVer := 1;
  end;
  ISOHeader.Write(PVD, SizeOf(PVD)); // First Copy
end;
 
 
 
procedure TCDToolBurner.WriteJVD;
var
  rd: PDirEntry;
  JVD: TVolumeDescriptor;
begin
  FillChar(JVD, SizeOf(JVD), 0);
  with JVD do
  begin
    pdType := 2;
    Identifier := 'CD001';
    Version := 1;
    VolumeFlag := 0;
    rd := Dirs[0];
    RootDirRec.LenDr := 34;
    RootDirRec.Extended := 0;
    RootDirRec.FileFlag := 2;
    RootDirRec.Address := FileDirDescriptorLocationJ;
    RootDirRec.AddressBE := L2MDW(FileDirDescriptorLocationJ);
    RootDirRec.VolSeqnumber := 1; RootDirRec.VolSeqnumberBE := 256;
    RootDirRec.DataLength := rd.SizeJ;
    RootDirRec.DataLengthBE := L2MDW(rd.SizeJ);
    RootDirRec.LenOfFileIdentifier := 1;
    NoOfSectors := TotalNoOfSectors;
    NoOfSectorsBE := L2MDW(TotalNoOfSectors);
    Type1PathTable := PathTableRecsLocationJL;
    TypeMPathTable := L2MDW(PathTableRecsLocationJM);
    TypeMPathTableBE := 0;
    PathTableSize  := iPathTableSizeJ;
    PathTableSizeBE := L2MDW(iPathTableSizeJ);  
    JVD.EscapeChars[0] := '%';JVD.EscapeChars[1] := '/';JVD.EscapeChars[2] := '@';
    CopyToArrayW(fIdVolume, IdVolume, SizeOf(JVD.IdVolume));
    CopyToArrayW(fIdSystem, IdSystem, SizeOf(JVD.IdSystem));
    CopyToArrayW(fIdVolumeSet, IdVolumeSet, SizeOf(JVD.IdVolumeSet));
    CopyToArrayW(fIdPublisher, IdPublisher, SizeOf(JVD.IdPublisher));
    CopyToArrayW(fIdPreparer, IdPreparer, SizeOf(JVD.IdPreparer));
    CopyToArrayW(fIdApplication, IdApplication, SizeOf(JVD.IdApplication));
    CopyToArrayW(fFileCopyright, FileCopyright, SizeOf(JVD.FileCopyright)-1);
    CopyToArrayW(fFileAbstract, FileAbstract, SizeOf(JVD.FileAbstract)-1);
    CopyToArrayW(fFileBibliographic, FileBibliographic, SizeOf(JVD.FileBibliographic)-1);
    CopyToArray(FormatDateTime('yyyymmddhhnnsszz',fDateCreation), DateCreation,16);
    CopyToArray(FormatDateTime('yyyymmddhhnnsszz',fDateModification), DateModification,16);
    CopyToArray(UpperCase(fApplicationData2), ApplicationData2, 8);
    CopyToArray('0000000000000000', DateExpiration,16);
    CopyToArray('0000000000000000', DateEffective,16);
    JVD.SectorSize := DefaultSectorSize;
    JVD.SectorSizeBE := LToMW(DefaultSectorSize);
    VolSetSize := 1;
    VolSetSizeBE := 256;
    VolSeqNumber := 1;
    VolSeqNumberBE := 256;
    FileStructureVer := 1;
  end;
  ISOHeader.Write(JVD, SizeOf(JVD));
end;
 
 
 
procedure TCDToolBurner.WriteTVD;
var
  TVD: TVolumeDescriptor;
begin
  FillChar(TVD, SizeOf(TVD), 0);
  with TVD do
  begin
    pdType := 255;
    Identifier := 'CD001';
    Version := 1;
  end;
  ISOHeader.Write(TVD, SizeOf(TVD)); // Terminator
end;
 
 
 
procedure TCDToolBurner.WriteBootCatalog;
var
  BootCatalog: TBootCatalog;
  tmp: array[0..$1f] of byte;
  i: Integer;
  w: Word;
  t: Integer;
begin
  t := 0;
  FillChar(BootCatalog, sizeof(BootCatalog), 0);
  with BootCatalog do
  begin
    Header := 1;
    Developer := 'Magic CD/DVD Burner'#0#0#0#0#0;
    KeyByte1 := $55;
    KeyByte2 := $AA;
    BootIndicator := $88;
    SectorCount := 1;
    if BootImageSize = 1228800 then
      BootMediaType := 1
    else if BootImageSize = 1474560 then
      BootMediaType := 2
    else if BootImageSize = 2949120 then
      BootMediaType := 3
    else
      BootMediaType := 4;
    LoadRBA := BootImageLocation;
  end;
  move(BootCatalog, tmp, 32);
  i := 0;
  while i < 32 do
  begin
    move(tmp[i], w, 2);
    t := t + w;
    inc(i, 2);
  end;
  w := (MaxWord + 1) - Word(t);
  BootCatalog.Checksum := w;
  ISOHeader.Write(BootCatalog, SizeOf(BootCatalog));
end;

 
 
 
procedure TCDToolBurner.WriteBVD;
var
  BVD: TBootVolumeDescriptor;
begin
  FillChar(BVD, SizeOf(BVD), 0);
  with BVD do
  begin
    pdType := 00;
    Identifier := 'CD001';
    Version := 1;
    Ident := 'EL TORITO SPECIFICATION'#0#0#0#0#0#0#0#0#0;
    BootCatLocation := BootCatalogLocation;
  end;
  ISOHeader.Write(BVD, SizeOf(BVD)); // Terminator
end;
 
 
 
function spc(s: String; i: Integer): String;
begin
  result := copy(s+'                                                                           ', 1, i); 
end;

procedure TCDToolBurner.Print_Files(Files: PFileEntry);
var
  i: Integer;
begin
  i := 0;
  while Files <> nil do
  begin
   inc(i);
   if Files.Attr = faDirectory then
     DebugMsg(IntToDec(i, 3, ' ')+' '+SPC(Files.LongName, 40)+'  <<dir>> ', mtMessage)
   else
     DebugMsg(IntToDec(i, 3, ' ')+' '+SPC(Files.LongName, 40)+' '+IntToDec(Files.FileSize, 8, ' ')+'   '+Files.Path, mtMessage);
   Files := Files.Next;
  end;
  DebugMsg(' ', mtMessage);

end;
 
 
 
procedure TCDToolBurner.Print_D1;
var
  i: Integer;
  P: PDirEntry;
begin
  for i:=0 to DirCounter-1 do
  begin
    p := Dirs[i];
    DebugMsg('('+IntToDec(i+1,2,' ')+') '+p.Path, mtMessage);
    DebugMsg('------------------------------------------------------------------------------------------------------', mtMessage);
    if p.Files <> nil then print_Files(p.Files)
  end;
  DebugMsg(' ', mtMessage);
end;
 
 
 
function TCDToolBurner.Initialize;
begin
  result := true;
  ClearAll;
end;
 
 
 
function TCDToolBurner.GetLastRecordedAddress;
var
  i: Integer;
begin
  for i:=32 downto 16 do
  begin
    fillchar(impvd, SizeOf(impvd), 0);
    if Read10(i, 1, @impvd, DefaultSectorSize) then
    with impvd do if (Identifier[1] = 'C') and (Identifier[2] = 'D') and (Identifier[3] = '0') and (Identifier[4] = '0') and (Identifier[5] = '1') then if (PdType = 2) or (PdType = 1) then
    begin
      result := impvd.NoOfSectors;
      exit;
    end;
  end;
  result := 0;
end;
 
 
 
function TCDToolBurner.GetNextWritableAddress;
var
  OrgFin: Boolean;
  ti: Integer;
  dt: Byte;
begin
  fStartAddress := 0;
  WaitForReady(10000, 500);
  dt := DiscType;
  OrgFin := FinalizeDisc;
  FinalizeDisc := True;
  SetWriteParams(TestWrite, UnderrunProtection, FinalizeDisc, MediumIs);
  FinalizeDisc := OrgFin;
  WaitForReady(10000, 500);
  if (dt = mtDVD_RAM) or (dt = mtDVD_PLUSRW) then
  begin
    fStartAddress := GetLastRecordedAddress;
    result := True;
    exit;
  end;
  ReadDiscInformation;
  if (dt = mtDVD_RW_RO) or (dt = mtDVD_RW_SR) then
  begin
    result := ReadTrackInformation(1);
    if result then
    begin
      if TrackInformation.NextWritableAddress <> 0 then
         fStartAddress := TrackInformation.NextWritableAddress;
      exit;
    end;
  end
  else
  begin
    ti := SessionsOnDisc+1;
    result := ReadTrackInformation(ti);
    if result then
      fStartAddress := TrackInformation.NextWritableAddress
    else
      fStartAddress := 0;
  end;
end;
 
 
 
function TCDToolBurner.PrepareISO: Boolean;
begin
  Prepare(True, nil);
  result := True;
end;
 
 
 
function TCDToolBurner.PrepareCD: Boolean;
begin
  Prepare(False, nil);
  result := True;
end;
 
 
 
function TCDToolBurner.PrepareHeader(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
var
  iCursor: Int64;
  i: Integer;
  p: PDirEntry;
  lp, ln: Integer;
  fs: TFileStreamEx;

Label
  SortAgain, SortAgainJ;
begin
  Valid := True;
  if not FileExists(fBootImage) then
  begin
    fBootable := False;
  end
  else if (fBootImage = '') then
     fBootable := False;
  DoDebug := False; 
  fStartAddress := 0;
  if not ISOFile then
  begin
    MediumIs := DiscType;
    if (GetAddress) then
    begin
       if not GetNextWritableAddress then
         DebugMsg('>>> '+ERR_NEXTADRESS, mtUNKNOWN);
       if fStartAddress > $FF000000 then
         fStartAddress := 0;
    end;
    if (fSessionToImport <> 0) then
    begin
      ImportSession(fSessionToImport, TargetDir);
      WithOldSession := True;
    end
    else
      WithOldSession := False;
  end;
  if DoDebug then DebugMsg(IntToStr(FileCounter)+' Files Added to CD, now sorting', mtMESSAGE);
  if DoDebug then DebugMsg('Sorting .... ISO9660 LN-PN', mtMESSAGE);  /// Sort Path Table Record - 6.9.1
  SortLN(PathTable, DirCounter-1, False); SortPN(PathTable, DirCounter-1, False);
  if DoDebug then DebugMsg('Sorting .... Joliet  LN-PN', mtMESSAGE);
  SortLN(PathTableJ, DirCounter-1, True); SortPN(PathTableJ, DirCounter-1, True);
SortAgain:
  if DoDebug then DebugMsg('Sorting .... ISO9660', mtMESSAGE);
  SortNumber(PathTable, DirCounter-1); SortParent(PathTable, DirCounter-1);
  SortNumber(PathTable, DirCounter-1); SortPN(PathTable, DirCounter-1, False);
  lp := 0; ln := 0;
  for i:=1 to DirCounter-1 do
  begin
    p := PathTable[i];
    if lp > p.Parent.Number then
      goto SortAgain;
    if ln > p.Number then
      goto SortAgain;
    lp := p.Parent.Number;
    ln := p.Number;
  end;
SortAgainJ:
  if DoDebug then DebugMsg('Sorting .... Joliet', mtMESSAGE);
  SortNumber(PathTableJ, DirCounter-1);
  SortParent(PathTableJ, DirCounter-1);
  lp := 0; ln := 0;
  for i:=0 to DirCounter-1 do
  begin
    p := PathTableJ[i];
    if lp > p.Parent.Number then
      goto SortAgain;
    if ln > p.Number then
      goto SortAgain;
    lp := p.Parent.Number;
    ln := p.Number;
  end;
  //// ..........................................................................
  if DoDebug then DebugMsg('Sorting .... Done', mtMESSAGE);
  iCursor := fStartAddress + 16;
  iCursor := iCursor;
  if DoDebug then DebugMsg(' Primary Volume Descriptor Location   :'+IntToDec(iCursor, 8), mtMESSAGE);
  if fBootable then
  begin
    iCursor := iCursor + 1;
    if DoDebug then DebugMsg(' Boot Record    Descriptor Location   :'+IntToDec(iCursor, 8), mtMESSAGE);
  end;
  { iCursor := iCursor + DefaultSectorSize;  Prim2VDLocation := iCursor;
  if DoDebug then DebugMsg(' Secondry Volume Descriptor Location  :'+IntToHex(iCursor, 8), mtMESSAGE); }
  if fJoliet then
  begin
    iCursor := iCursor + 1;
    if DoDebug then DebugMsg(' Joliet Volume Descriptor Location    :'+IntToDec(iCursor, 8), mtMESSAGE);
  end;
  iCursor := iCursor + 1;
  if DoDebug then DebugMsg(' Terminator Vol Descriptor Location   :'+IntToDec(iCursor, 8), mtMESSAGE);
  // XYZ-FS iCursor := iCursor + $0000;
  // if DoDebug then DebugMsg(' XYZ-FS                            :'+IntToDex(iCursor, 8));
  // .......................................................................................
  iCursor := iCursor + 1;
  PathTableRecsLocationL := iCursor;
  if DoDebug then DebugMsg(' Path Table Records Location (Least)  :'+IntToDec(iCursor, 8), mtMESSAGE);
  PathTableWidth; iCursor := iCursor + Sectors(iPathTableSize);
  PathTableRecsLocationM := iCursor;
  if DoDebug then DebugMsg(' Path Table Records Location (Most)   :'+IntToDec(iCursor, 8), mtMESSAGE);
  iCursor := iCursor + Sectors(iPathTableSize);
  PathTableRecsLocationJL := iCursor;
  if fJoliet then
  begin
    if DoDebug then DebugMsg(' Path Table Records Location (Least) J:'+IntToDec(iCursor, 8), mtMESSAGE);
    PathTableWidthJ; iCursor := iCursor + Sectors(iPathTableSizeJ);
    PathTableRecsLocationJM := iCursor;
    if DoDebug then DebugMsg(' Path Table Records Location (Most)  J:'+IntToDec(iCursor, 8), mtMESSAGE);
    iCursor := iCursor + Sectors(iPathTableSizeJ);
  end;
  FileDirDescriptorExtentStart := iCursor;
  FileDirDescriptorLocation := iCursor;
  if DoDebug then DebugMsg(' File and Directory Record Location   :'+IntToDec(iCursor, 8), mtMESSAGE);
  iFileAndDirDescriptorWidth := FileAndDirDescriptorWidth;
  iCursor := iCursor + Sectors(iFileAndDirDescriptorWidth);
  if fJoliet then
  begin
    FileDirDescriptorLocationJ := iCursor;
    if DoDebug then DebugMsg(' File and Directory Record Location  J:'+IntToDec(iCursor, 8), mtMESSAGE);
    iFileAndDirDescriptorWidthJ := FileAndDirDescriptorWidthJ;
    iCursor := iCursor + Sectors(iFileAndDirDescriptorWidthJ);
  end;
  FileDirDescriptorExtentEnd := iCursor;
  //----------------------------------------------------------------------- Bootable CD/DVD
  if fBootable then
  begin
    fs := TFileStreamEx.Create(fBootImage, fmOpenRead+fmShareDenyNone);
    BootCatalogLocation := iCursor;
    iCursor := iCursor + 1;
    BootImageLocation := iCursor;
    BootImageSize := Sectors(fs.Size);
    iCursor := iCursor + BootImageSize;
    fs.Destroy;
  end;
  //---------------------------------------------------------------------------------------
  if iCursor < 150 then
  begin
    Pads := 150 - iCursor;
    fDataLocation := 150;
    iCursor := fDataLocation;
    Valid := True;
  end
  else
  begin
    Pads := 150 - iCursor;
    fDataLocation := iCursor;
    Valid := False;
  end;

  if DoDebug then DebugMsg(' First File Location                  :'+IntToDec(iCursor, 8), mtMESSAGE);
  SetFileAddress;
  TotalNoOfSectors := iCursor + fFilesSizeOnDisc;
  //---------------------------------------------------------------------------------------
  if not ISOFile and wms then
    TotalNoOfSectors := TotalNoOfSectors + 150;
  fImageSize := TotalNoOfSectors - (fStartAddress);
  result := True;
end;
 
 
 
function TCDToolBurner.BuildHeader(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
var
  i: Integer;
begin
  BuildHeaderISOFile := ISOFile;
  BuildHeaderTargetDir := TargetDir;
  ISOHeader.Clear;
  if wms then for i := 1 to 150 do
    ISOHeader.Write(ZEROS, DefaultSectorSize);
  for i := 1 to 16 do
    ISOHeader.Write(ZEROS, DefaultSectorSize);
  WritePVD;
  if fBootable then
    WriteBVD;
  if fJoliet then
    WriteJVD;
  WriteTVD;
  fillchar(vds, sizeof(vds), 255);
  ISOHeader.Seek(0, 0);
  ISOHeader.Read(vds[0], ISOHeader.Size);
  vdsSize := ISOHeader.Size;
  WritePathTable(FileDirDescriptorLocation, False);
  WritePathTable(FileDirDescriptorLocation, True);
  if fJoliet then
  begin
    WritePathTableJ(FileDirDescriptorLocationJ, False);
    WritePathTableJ(FileDirDescriptorLocationJ, True);
  end;
  WriteFileAndDirDescriptor(FileDirDescriptorLocation, fDataLocation);
  if fJoliet then
    WriteFileAndDirDescriptorJ(FileDirDescriptorLocationJ, fDataLocation);

  if fBootable then
    WriteBootCatalog;
  if Pads > 0 then
  begin
    for i := 1 to Pads do
      ISOHeader.Write(ZEROS, DefaultSectorSize);
  end;
  fImageSize := TotalNoOfSectors - (fStartAddress);
  result := True;
  fPrepared := True;
  DoDebug := True;
end;
 
 
 

function TCDToolBurner.Prepare(ISOFile: Boolean = False; TargetDir: PDirEntry = nil): Boolean;
begin
  result := PrepareHeader(ISOFile, TargetDir);
  //result := BuildHeader(ISOFile, TargetDir);
end;
 
 
 
function GetLastFile(Files: PFileEntry): PFileEntry;
begin
  result := nil;
  while Files <> nil do
  begin
    result := Files;
    Files := Files.Next;
  end;
end;
 
 
 
function CompletePath(d: PDirEntry): String;
var
  s: String;
begin
  s := '\';
  while d.Parent <> d do
  begin
    s := '\'+d.LongName+s;
    d := d.parent;
  end;
  s := Copy(s, 1, Length(s)-1);
  result := UpperCase(s);
end;
 
 
 
function TCDToolBurner.FindDir(DirName: String): PDirEntry;
var
  i: Integer;
begin
  if (DirName = '') or (DirName = '\') then
  begin
    result := fRoot;
    exit;
  end;
  if Copy(DirName, Length(DirName), 1) = '\' then
    DirName := Copy(DirName, 1, Length(DirName)-1);
  DirName := UpperCase(DirName);
  for i:=1 to DirCounter -1 do
  begin
    if (Dirs[i] <> nil) and (DirName = PDirEntry(Dirs[i]).Path) then
    begin
      result := Dirs[i];
      exit;
    end;
  end;
  result := nil;
end;
 
 
 
function MakeShortFileName(FileName: String; var sr: TSearchRec): Boolean;
var
  i: Integer;
begin
  for i:=1 to Min(length(FileName), 13) do
  begin
    sr.FindData.cAlternateFileName[i-1] := UpCase(FileName[i]); //short name
    if (sr.FindData.cAlternateFileName[i-1] < #33) or (sr.FindData.cAlternateFileName[i-1] > 'z') then sr.FindData.cAlternateFileName[i-1] := '_';
  end;
  i := Min(length(FileName), 13);
  while (i < 13) do
  begin
    sr.FindData.cAlternateFileName[i] := #0;
    inc(i);
  end;
  sr.FindData.cAlternateFileName[13] := #0;
  result := true;
end;
 
 
 
function TCDToolBurner.CreateDir(DirName: String): PDirEntry;
begin
  result := MakeDir(DirName);
end;
 
 
 
function TCDToolBurner.CreateDir(DestinationPath: String; DirName: String): PDirEntry;
var
  d: PDirEntry;
begin
  d := FindDir(DestinationPath);
  if d = nil then
  begin
    DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
    result := nil;
    exit;
  end;
  result := CreateDir(d, DirName);
end;
 
 
 
function TCDToolBurner.CreateDir(DestinationPath: PDirEntry; DirName: String; Attr: Integer = faDirectory): PDirEntry;
var
  sr: TSearchRec;
begin
  if Copy(DirName, 1, 1) = '\' then DirName := Copy(DirName, 2, Length(DirName));
  if Copy(DirName, Length(DirName), 1) = '\' then DirName := Copy(DirName, 1, Length(DirName)-1);
  sr.Name := DirName;
  MakeShortFileName(DirName, sr);
  sr.Time := DateTimeToFileDate(Now);
  sr.Attr := Attr;
  result := CreateDir(DestinationPath, sr);
end;
 
 
 
function TCDToolBurner.CreateDir(DestinationPath: PDirEntry; sr: TSearchRec): PDirEntry;
var
  d, d1: PDirEntry;
  f, f1: PFileEntry;
  FullPath: String;
  FileNameJ, FileNameD: String;
begin
  FullPath := DestinationPath.Path+'\'+sr.Name;
  if Copy(FullPath, 1, 2) = '\\' then
    FullPath := Copy(FullPath, 2, DefaultSectorSize);
  d := FindDir(FullPath);
  if d <> nil then
  begin
    result := d;
    exit;
  end;
  Result := d;
  New_D(d1);
  if d1 = nil then exit;
  New_F(f1);
  if f1 = nil then exit;
  if DestinationPath.Files <> nil then
  begin
    f := GetLastFile(DestinationPath.Files);
    f.Next := f1;
  end
  else
    DestinationPath.Files := f1;
  FileNameD := sr.FindData.cAlternateFileName;
  FileNameJ := sr.Name;
  if FileNameD = '' then FileNameD := UpperCase(FileNameJ);
  d1.Parent := DestinationPath; //11d1.Child := nil;
  d1.Files := nil;
  d1.LongName := FileNameJ; d1.ShortName := FileNameD;
  f1.LongName := FileNameJ; f1.ShortName := FileNameD; f1.Attr := sr.Attr; f1.FileSize:= 0; f1.FileSizeJ:= 0;
  f1.DirRec := d1; f1.FileSize  := 0;
  try
    f1.Time := FileDateToDateTime(sr.Time);
  except
    f1.Time := Now;
  end;
  d1.Path := CompletePath(d1);
  f1.Next := nil;
  result := d1;
  fPrepared := False;
end;
 
 
 
var
  k: Integer = 0;
  LastFile: PFileEntry;

function TCDToolBurner.FindFile(Dir: PDirEntry; FileName: String): PFileEntry;
var
  f: PFileEntry;
begin
{  if Pos('.', FileName) = 0 then
    FileName := FileName + '.';}
  FileName := UpperCase(FileName);
  f := dir.Files;
  LastFile := nil;
  while f <> nil do
  begin
    if (uppercase(f.LongName) = FileName) then
    begin
      result := f;
      exit;
    end;
    LastFile := f;
    f := f.next;
  end;
  result := nil;
end;
 
 
 
function TCDToolBurner.InsertFile(DestinationPath: PDirEntry; sr: TSearchRec; FilePath: String; OrignalAddress: Integer = 0; ResetArchiveBit: Boolean = False): Integer;
var
  tmp, f, f2: PFileEntry;
  FileNameD, FileNameJ: String;
  FileDateTime: TDateTime;
  Overwrite, Skip: Boolean;
label
  SkipThisFile;
begin
  Result := 0;
  if (sr.Attr and faDirectory <> faDirectory) then
  begin
    if Pos('.', FileNameD) = 0 then FileNameD := FileNameD+'.';
    if Pos('.', sr.Name) = 0 then sr.Name := sr.Name+'.';
  end;
  f2 := FindFile(DestinationPath, sr.Name);
  f := GetLastFile(DestinationPath.Files);
  FileNameD := sr.FindData.cAlternateFileName;
  if FileNameD = '' then FileNameD := UpperCase(sr.Name);
  FileNameJ := sr.Name;
  try
    FileDateTime := FileDateToDateTime(sr.Time);
  except
    FileDateTime := Now;
  end;
  Skip := False;
  if (f2 <> nil) and fReplaceFile then
    Overwrite := True
  else
    Overwrite := False;
  if Length(FileNameJ) > 109 then
    Skip := True;
  if Assigned(fOnAddFile) then
    fOnAddFile(Self, FilePath, FileNameJ, FileNameD, FileDateTime, sr.Attr, sr.size, Skip);
  if Skip then
  begin
    result := 0;
    goto SkipThisFile;
  end;
  if Overwrite then
    tmp := f2
  else
  begin
    if f2 <> nil then
    begin
      result := 0;
      goto SkipThisFile;
    end
    else
    begin
      New_F(tmp);
      if tmp = nil then
        exit;
      if f <> nil then
        f.Next := tmp
      else
        DestinationPath.Files := tmp;
    end;
  end;
  tmp.ShortName := UpperCase(FileNameD); tmp.LongName := FileNameJ; tmp.FileSizeJ := (sr.FindData.nFileSizeHigh * MAXDWORD) + sr.FindData.nFileSizeLow; tmp.FileSize := tmp.FileSizeJ;
  tmp.Attr := sr.Attr;
  tmp.Time := FileDateTime;
  tmp.Path := FilePath;
  tmp.ResetArchiveBit := ResetArchiveBit;
  tmp.Prev := IsPrev;
  if not Overwrite then
    tmp.Next := nil
  else
    FFilesSize := FFilesSize - Sectors(tmp.FileSizeJ) * DefaultSectorSize;
  if OrignalAddress <> 0 then
  begin
    tmp.Address := OrignalAddress;
    tmp.AddressJ := OrignalAddress;
    tmp.Imported := True;
  end
  else
  begin
    FFilesSize := FFilesSize + Sectors(sr.Size) * DefaultSectorSize;
    tmp.Imported := false;
  end;
  result := 1;
  fPrepared := False;
  CurrentFile := tmp;
SkipThisFile:
end;
 
 
 
function TCDToolBurner.InsertFileWithName(DestinationPath: String; FilePath: String; ShortNameOnDisc, LongNameOnDisc: String): Integer;
var
  sr: TSearchRec;
  d: PDirEntry;
  ShortName: String;
begin
  Result := 0;
  d := FindDir(DestinationPath);
  if (d = nil) or (LongNameOnDisc = '') then
  begin
    DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
    exit;
  end
  else
  begin
    if FindFirst(FilePath, faAnyFile, sr) = 0 then
    begin
      sr.Name := LongNameOnDisc;
      FillChar(sr.FindData.cAlternateFileName[0], 13, 0);
      if ShortNameOnDisc = '' then
         ShortNameOnDisc := LongNameOnDisc;
      ShortName := UpperCase(ShortNameOnDisc);
      Move(ShortName[1], sr.FindData.cAlternateFileName[0], Length(ShortNameOnDisc));
      InsertFile(d, sr, FilePath);
      result := 1;
    end
    else
      result := -1;
    FindClose(sr);
  end;
end;
 
 
 
function TCDToolBurner.InsertMemoryFile(DestinationPath, LongFileName, ShortFileName: String; Attr: Byte; Memory: Pointer; Size: Cardinal): Integer;
var
  sr: TSearchRec;
  d: PDirEntry;
  ShortName: String;
begin
  Result := 0;
  d := FindDir(DestinationPath);
  if (d = nil) or (LongFileName = '') then
  begin
    DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
    exit;
  end
  else
  begin
    sr.Name := LongFileName;
    FillChar(sr.FindData.cAlternateFileName[0], 13, 0);
    if ShortFileName = '' then
       ShortFileName := LongFileName;
    ShortName := UpperCase(ShortFileName);
    sr.Size := Size;
    sr.FindData.nFileSizeLow := Size;
    sr.FindData.nFileSizeHigh := 0;
    sr.Time := DateTimeToFileDate(Now);
    sr.Attr := Attr;
    Move(ShortName[1], sr.FindData.cAlternateFileName[0], Length(ShortFileName));
    result := InsertFile(d, sr, '');
    if  result > 0 then
      CurrentFile.Buffer := Memory;
  end;
end;
 
 
 
function TCDToolBurner.InsertFile(DestinationPath: String; FilePath: String; SavePath: Boolean = False): Integer;
var
  sr: TSearchRec;
  d: PDirEntry;
  SourcePath, DirsToMake: String;
  tmpstr: String;
begin
  Result := 0;
  if SavePath then
  begin
    SourcePath := ExtractFilePath(FilePath);
    tmpstr := SourcePath;
    if copy(tmpstr, 1, 2) = '\\' then
    begin
      tmpstr := copy(tmpstr, 3, 1024);
      tmpstr := copy(tmpstr, Pos('\', tmpstr)+1, 1024);
      tmpstr := copy(tmpstr, Pos('\', tmpstr), 1024);
      DirsToMake := tmpstr;
    end
    else
    begin
      SourcePath := ExtractFilePath(FilePath);
      DirsToMake := Copy(SourcePath, 3, Length(sourcepath));
    end;
    if fParentDirectoryOnly then
      DirsToMake := ExtractLastDir(DirsToMake);
    MakeDir(DirsToMake);
    d := FindDir(DirsToMake);
  end
  else
    d := FindDir(DestinationPath);
  if d = nil then
  begin
    DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
    exit;
  end
  else
  begin
    if FindFirst(FilePath, faAnyFile, sr) = 0 then
    begin
      InsertFile(d, sr, FilePath);
      result := 1;
    end
    else
      result := -1;
    FindClose(sr);
  end;
end;
 
 
 
function TCDToolBurner.InsertDir(DestinationPath: PDirEntry; SourcePath: String; FileSpecs: String='*.*'; Attr: Integer = faAnyFile; Recursive: Boolean=True; SavePath: Boolean = False; ArchiveOnly: Boolean = False): Integer;
var
  sr: TSearchRec;
  res: Integer;
  dd: PDirEntry;
  Skip: Boolean;
  FileName, ShortFileName: String;
  DirsToMake: String;
  tmpstr, tmpstr2: String;
label
  SkipEntry, SkipDir;
begin
  result := 0;
  if SavePath then
  begin
    tmpstr := SourcePath;
    if copy(tmpstr, 1, 2) = '\\' then
    begin
      tmpstr := copy(tmpstr, 3, 1024);
      tmpstr := copy(tmpstr, Pos('\', tmpstr)+1, 1024);
      tmpstr := copy(tmpstr, Pos('\', tmpstr), 1024);
      DirsToMake := tmpstr;
    end
    else
    begin
      if Copy(SourcePath, 2, 1) = ':' then
        DirsToMake := Copy(SourcePath, 3, Length(sourcepath))
      else
        DirsToMake := Copy(SourcePath, 1, Length(sourcepath));
    end;
    if (DestinationPath.Path <> '\') and (DestinationPath.Path <> '') then
      DirsToMake := DestinationPath.Path+DirsToMake;
    if fParentDirectoryOnly then
      DirsToMake := ExtractLastDir(DirsToMake);
    MakeDir(DirsToMake);
    dd := FindDir(DirsToMake);
    if dd <> nil then
      DestinationPath := dd
    else
    begin
      DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
      exit;
    end;
  end;
  if Copy(SourcePath, Length(SourcePath), 1) <> '\' then SourcePath := SourcePath + '\';
  if FileSpecs = '' then
    FileSpecs := '*.*';
  if Recursive then
  begin
    res := FindFirst(SourcePath+'*.*', faAnyFile or faDirectory, sr);
    while res = 0 do
    begin
      result := 1;
      if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
      begin
        FileName := sr.Name;
        ShortFileName := sr.FindData.cAlternateFileName;
        Skip := False;
        if ShortFileName = '' then ShortFileName := UpperCase(FileName);
        Skip := False;
        if Assigned(fOnAddDir) then
          fOnAddDir(Self, FileName, ShortFileName, Skip);
        if Skip then goto SkipDir;
        dd := CreateDir(DestinationPath, sr);
        if dd <> nil then
        begin
          if Sr.Name = 'Installer' then
            Messagebeep(0);
          result := InsertDir(dd, SourcePath+Sr.Name, FileSpecs, attr, Recursive, False, ArchiveOnly);
          if dd.Files = nil then
            removedir(dd);
          if result = 0 then exit;
        end
        else
        begin
          result := 0;
          exit;
        end;
      SkipDir:
      end;
      res := FindNext(sr);
    end;
  end;
  FindClose(sr);
  //----------------------------------------------------------------------------
  res := FindFirst(SourcePath+FileSpecs, attr, sr);
  while res = 0 do
  begin
    if (sr.Name = '.') or (sr.Name = '..') then
    begin
      result := 1;
      goto SkipEntry;
    end;
    if ((sr.Attr and faDirectory) <> faDirectory) then
    begin
      if ArchiveOnly and ((sr.Attr and faArchive) <> faArchive) then goto SkipEntry;
      tmpstr2 := SourcePath+sr.Name;
      sr.FindData.cFileName[1] := #1;
      result := InsertFile(DestinationPath, sr, tmpstr2, 0, ArchiveOnly);
    end;
SkipEntry:
    res := FindNext(sr);
  end;
  FindClose(sr);
  //..SortFiles(DestinationPath.Files, fJoliet);
  fPrepared := False;
end;
 
 
 
function TCDToolBurner.InsertDir(DestinationPath, SourcePath: String; FileSpecs: String='*.*'; Attr: Integer = faAnyFile; Recursive: Boolean=True; SavePath: Boolean = False; ArchiveOnly: Boolean = False): Integer;
var
  d: PDirEntry;
begin
  result := 0;
  d := FindDir(DestinationPath);
  if d <> nil then
  begin
    result := InsertDir(d, SourcePath, FileSpecs, Attr, Recursive, SavePath, ArchiveOnly);
  end
  else
  begin
    DebugMsg(ERR_INVALIDDESTDIR, mtFATALERROR);
    exit;
  end;
end;
 
 
 
function  TCDToolBurner.MakeDir(DirName: String): PDirEntry;
  //----------------------------------------------------------------------------
  function Last(What: String; Where: String): Integer;
  var
    Ind : Integer;
  begin
    Result := 0;
    for Ind := (Length(Where)-Length(What)+1) downto 1 do
      if Copy(Where, Ind, Length(What)) = What then
      begin
        Result := Ind;
        Break;
      end;
  end;
  //----------------------------------------------------------------------------
var
  PrevDir, DirToCreate : String;
  Ind     : Integer;
  d1, d2  : PDirEntry;
begin
  result := nil;
  d1 := FindDir(DirName);
  if d1 = nil then
  begin
     Ind := Last('\', DirName);         //  Position of the last '\'
     PrevDir := Copy(DirName, 1, Ind-1);    //  Previous directory
     d2 := FindDir(PrevDir);
     if d2 = nil then
       MakeDir(PrevDir);
     d2 := FindDir(PrevDir);
     DirToCreate := Copy(DirName, ind+1, Length(DirName));
     if DirToCreate <> '' then
       result := CreateDir(d2, DirToCreate);
  end
  else
    result := d1;
end;
 
 
 
var
  th: TCacheThread;
  //ith: TISOBurnerThread;
  AbortWrite: Boolean;

function TCDToolBurner.BufferProgress: Integer;
var
  ct: Integer;
  r: Integer;
begin
  if not Burning then
  begin
    result := 0;
    exit;
  end;
  if th = nil then
    result := 0
  else
  begin
    ct := th.BytesAvailable;
    r := round((ct / CacheSize ) * 100);
    result := r;
  end;
end;

 
 
 
procedure TThWrite.Execute;
var
  tries, Size: Integer;
  i: Cardinal;
  ImageSize: Cardinal;
  CheckBuffer: Integer;
  f: TFileStreamEx;
  xDeviceBufferSize, xDeviceFreeBufferSize: Cardinal;
  OrgFin, fake: Boolean;
  Tracks: Integer;
  Err: String;
  mm, ss, ff: byte;
  PVD: TVolumeDescriptor;
  ChkBuffer: Boolean;

label
  CloseDiscSkipped, TryAgain, tryagain2, again3, retrywrite, closedisc, WaitForReady;

begin
  //ith := nil;
  Tracks := 1;
  FreeOnTerminate := True;
  AbortWrite := False;
  fake := False;
  CheckBuffer := 5;
  Burning := True;
  CDToolBurner.fBytesWritten := 0;
  th.Error := ERR_NONE;
  f := nil;
  fwms := wms;
  if fSaveISO then
  begin
    try
      f := TFileStreamEx.Create( CDToolBurner.fISOFileName, fmCreate );
    except
      th.Error := Format(ERR_CREATEFILE, [CDToolBurner.fISOFileName]);;
      th.Abort;
      xDeviceBufferSize := 0;
      xDeviceFreeBufferSize := 0;
    end;
    if th.Error <> ERR_NONE then goto CloseDisc;
  end
  else
  begin
    if CDToolBurner.ReadBufferCapacity(xDeviceBufferSize, xDeviceFreeBufferSize) then
      ChkBuffer := True
    else
      ChkBuffer := False;

    MediumIs := CDToolBurner.DiscType;
    CDToolBurner.LoadMedium;
    if (MediumIs <> mtDVD_PLUSRW) then
      CDToolBurner.Rewind;
    CDToolBurner.SetCDSpeed(CDToolBurner.fReadSpeed, CDToolBurner.fWriteSpeed);
    CDToolBurner.WaitForReady(10000, 1000);
    OrgFin := CDToolBurner.FinalizeDisc;
    i :=  CDToolBurner.CurrentWriteSpeed;
    CDToolBurner.FinalizeDisc := True;
    CDToolBurner.SetWriteParams(CDToolBurner.TestWrite, CDToolBurner.UnderrunProtection, CDToolBurner.FinalizeDisc, MediumIs);
    CDToolBurner.FinalizeDisc := OrgFin;

    Tracks := CDToolBurner.SessionsOnDisc+1;
    if (MediumIs = mtDVD_R) then
    begin
      buf1 := #$00#$00#$00#$00#$00#$00#$00#$00#$00#$23#$12#$7F#$00#$00#$AB#$BD#$00#$00#$03#$E8#$00#$00#$05#$8A#$00#$00#$03#$E8;
      CDToolBurner.SetStreaming(@buf1, $1c);
      CDToolBurner.SendDVDStructureTimeStamp(Now);
      fillchar(buf1, sizeof(buf1), 0);
      ImageSize := CDToolBurner.GetImageSize;
      if ((ImageSize mod 32) <> 0) then
        ImageSize := ((ImageSize div 32) + 1) * 32;
      CDToolBurner.ReserveTrack(ImageSize);
      CDToolBurner.WaitForReady(10000, 1000);
    end;

    if (MediumIs <> mtDVD_RAM) and (not CDToolBurner.TestWrite) and (CDToolBurner.fPerformOPC) then
      CDToolBurner.SendOPC;
    if (MediumIs <> mtDVD_R) and (MediumIs <> mtDVD_RW_RO) and (MediumIs <> mtDVD_RW_SR) and (MediumIs <> mtDVD_PLUSR) and (MediumIs <> mtDVD_PLUSRW) then
    begin
      if fwms then
      begin
        Buf1 := #$41#$00#$00#$14#$00#$00#$00#$00#$41#$01#$00#$10#$00#$00#$00#$00#$41#$01#$01#$10#$00#$00#$02#$00#$41#$AA#$01#$14#$00#$54#$BE#$52;
        if CDToolBurner.GetImageSize > 450000 then
        begin
          th.Error := ERR_TOOMUCHDATA;
          th.Abort;
          Sleep(1000);
          goto CloseDiscSkipped;
        end;
        lba2msf(CDToolBurner.GetImageSize+1, mm, ss, ff);
        Buf1[29] := chr(mm);
        Buf1[30] := chr(ss);
        Buf1[31] := chr(ff);
        if CDToolBurner.SendCueSheet(Buf1, 32) = False then
        begin
          th.Abort;
          Sleep(1000);
          th.Error := CUESEND_ERR;
          goto CloseDiscSkipped;
        end;
      end;
    end;
    if (MediumIs = mtDVD_RAM) or (MediumIs = mtDVD_R) or (MediumIs = mtDVD_RW) or (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) or (MediumIs = mtDVD_PLUSRW) or (MediumIs = mtDVD_PLUSR) then
      CDToolBurner.DebugMsg('>>> '+format(MSG_WRITESTART,[DiscTypeString[MediumIs], FormatFloat('0.0', i / 1385)+FormatFloat('X (#,##0 KB/s)', i)]), mtMessage)
    else
      CDToolBurner.DebugMsg('>>> '+format(MSG_WRITESTART,[DiscTypeString[MediumIs], FormatFloat('0', i / 176.4)+FormatFloat('X (#,##0 KB/s)', i)]), mtMessage);
    if CDToolBurner.TestWrite then
      CDToolBurner.DebugMsg('>>> '+MSG_TESTWRITE, mtMessage);
  end;
  i := 0;
  CDToolBurner.Lock;
  Size := th.GetFirst(@WriteBuffer[0]);
  CDToolBurner.Unlock;
  if (th.ISOFileName = '') and (not fSaveISO) then
  begin
    if (MediumIs = mtDVD_PLUSRW) or (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) or (MediumIs = mtDVD_RAM) then
    begin
      //if CDToolBurner.fStartAddress <> 0 then
      begin
        move(vds[$8000+2048], PVD, 2048);
        CDToolBurner.DontShowError := True;
again3:
        if not fake and not CDToolBurner.Write10(0, Size div 2048, vds, Size) then
        begin

          if ((CDToolBurner.LastSense.AddSenseCode = $30) and (MediumIs = mtDVD_PLUSRW)) then
          begin
            buf1 := #$00#$82#$00#$08#$00#$23#$05#$40#$98#$00#$00#$00;
            if (CDToolBurner.FormatUnit($11, @buf1, 12)) then
            repeat
               Sleep(2000);
               CDToolBurner.ReadDiscInformation;
            until (CDToolBurner.LastSense.AddSenseCode <> 04);
            inc(tries);
            if (tries = 1) then
              goto again3;
          end;

          if (CDToolBurner.TargetBusy) or (CDToolBurner.LastSense.AddSenseCode = $4) or (CDToolBurner.LastSense.AddSenseCode = $0) then
          begin
            Sleep(600);
            Inc(Tries);
            if tries < 10000 then goto retrywrite;
            th.Abort;
            th.Error := ERR_DEVICEBUSY;
          end
          else
          begin
            th.Abort;
            th.Error := ERR_WRITEERROR+' ['+CDToolBurner.ErrorString+']';
          end;
          xDeviceBufferSize := 0;
          xDeviceFreeBufferSize := 0;
          goto CloseDisc;
        end;
        CDToolBurner.FlushCache(60000 * 30, FALSE);
      end;
    end;
  end
  else
    CDToolBurner.fStartAddress := 0;

  while Size <> 0 do
  begin
    if (th.ISOFileName = '') then if not fSaveISO then
      if (MediumIs <> mtCD_R) and (MediumIs <> mtCD_RW) then
        Size := 32*2048;
    CDToolBurner.fFileInProcess := th.FileName;
    if Size = -1 then goto TryAgain2;
    CDToolBurner.fBytesWritten := CDToolBurner.fBytesWritten + Size;
    if ((CDToolBurner.fBytesWritten div 2048) > CDToolBurner.ImageSize) then
      CDToolBurner.fBytesWritten := CDToolBurner.ImageSize * 2048;
    if fSaveISO then
    begin
      f.Write(WriteBuffer[0], Size);
    end
    else
    begin
      if (CheckBuffer = 0) and ChkBuffer and (CDToolBurner.ReadBufferCapacity(xDeviceBufferSize, xDeviceFreeBufferSize)) then
        CheckBuffer := 5;
      dec(CheckBuffer);
      tries := 0;
retrywrite:
      if AbortWrite then
      begin
        th.Abort;
        th.Error := ERR_ABORTED;
        xDeviceBufferSize := 0;
        xDeviceFreeBufferSize := 0;
        goto CloseDisc;
      end;
      if not fake and not CDToolBurner.Write10(i+CDToolBurner.fStartAddress, Size div DefaultSectorSize, WriteBuffer, Size) then
      begin
        if (CDToolBurner.TargetBusy) or (CDToolBurner.LastSense.AddSenseCode = $4) or (CDToolBurner.LastSense.AddSenseCode = $0) then
        begin
          Sleep(600);
          Inc(Tries);
          if tries < 10000 then goto retrywrite;
          th.Abort;
          th.Error := ERR_DEVICEBUSY;
        end
        else
        begin
          th.Abort;
          th.Error := ERR_WRITEERROR+' ['+CDToolBurner.ErrorString+']';
        end;
        xDeviceBufferSize := 0;
        xDeviceFreeBufferSize := 0;
        goto CloseDisc;
      end;
    end;
    inc(i, Size div DefaultSectorSize);
TryAgain2:
    if AbortWrite then
    begin
      th.Error := ERR_ABORTED;
      th.Abort;
      xDeviceBufferSize := 0;
      xDeviceFreeBufferSize := 0;
      goto CloseDisc;
    end;
    CDToolBurner.Lock;
    Size := th.GetFirst(@WriteBuffer[0]);
    CDToolBurner.Unlock;
    if th.Error <> '' then
      goto CloseDisc;
    if Size = -1 then
    begin
      Sleep(1);
      goto TryAgain2;
    end;
   end;
   if size mod DefaultSectorSize <> 0 then
   begin
     th.Error := ERR_4;
     th.Abort;
   end;
  if fSaveISO then
    f.Destroy
  else
  begin
CloseDisc:
    fwms := False;
    if MediumIs <> mtDVD_RAM then
      CDToolBurner.ReadBufferCapacity(xDeviceBufferSize, xDeviceFreeBufferSize);
    if (MediumIs = mtDVD_R) or (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) then
    begin
      //CDToolBurner.Lock;
      if Assigned(CDToolBurner.fOnFinalizingTrack) then
        CDToolBurner.fOnFinalizingTrack(CDToolBurner);
      //CDToolBurner.UnLock;
      CDToolBurner.FlushCache(60000 * 30, True);
      CDToolBurner.DeviceFreeBufferSize := CDToolBurner.DeviceBufferSize;
      while not CDToolBurner.ReadDiscInformation do
      begin
        CDToolBurner.DeviceFreeBufferSize := CDToolBurner.DeviceFreeBufferSize - (CDToolBurner.DeviceBufferSize div 960);
        Sleep(1000);
      end;
    end
    else
    begin
      //CDToolBurner.Lock;
      if Assigned(CDToolBurner.fOnFinalizingTrack) then
        CDToolBurner.fOnFinalizingTrack(CDToolBurner);
      if not CDToolBurner.FlushCache(60000 * 02, False) then
        CDToolBurner.FlushCache(60000 * 02, True);
      //CDToolBurner.UnLock;
    end;
    CDToolBurner.DeviceBufferSize := 0;
    CDToolBurner.DeviceFreeBufferSize := 0;
    if th <> nil then if (th.Error <> ERR_NONE) then
      goto CloseDiscSkipped;
    if not fake then if (not CDToolBurner.TestWrite) then
    begin
      if (MediumIs <> mtDVD_RAM) {nd (MediumIs <> mtDVD_RW_SR) and (MediumIs <> mtDVD_RW_RO)}and CDToolBurner.FinalizeTrack then
      begin
        if (MediumIs <> mtDVD_R) and (MediumIs <> mtDVD_R) then
        CDToolBurner.SetWriteParams(CDToolBurner.TestWrite, CDToolBurner.UnderrunProtection, CDToolBurner.FinalizeDisc, MediumIs);
        Tries := 0;
WaitForReady:
        if (MediumIs = mtDVD_PLUSR) then
        begin
          CDToolBurner.CloseTrackDVD(False, 1, 0, 0, Tracks);
          CDToolBurner.CloseTrackDVD(False, 2, 0, 0, $ff);
        end
        else if (MediumIs = mtDVD_R) then
        begin
          CDToolBurner.CloseTrackDVD(False, 1, 0, 0, Tracks);
          CDToolBurner.CloseTrackDVD(False, 1, 0, 0, Tracks+1);
          CDToolBurner.CloseTrackDVD(False, 2, 0, 0, Tracks+2);
        end
        else if (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) then
        begin
          CDToolBurner.CloseTrackDVD(True, 2, 0, 0, 00);
        end
        else if (MediumIs = mtDVD_PLUSRW) then
        begin
          CDToolBurner.CloseTrackDVD(False, 3, 0, 0, $ff);
        end
        else if not fwms then
        begin
          if not CDToolBurner.CloseTrack(False, fImmedCT) then 
          begin
            if (CDToolBurner.TargetBusy) or (CDToolBurner.LastSense.AddSenseCode = $4) then
            begin
              inc(tries);
              Sleep(1000);
              if Tries < 1000 then goto WaitForReady;
            end
            else
            begin
              CDToolBurner.CloseTrack(False, fImmedCT);
            end;
          end;
        end;
      end;
    end;
    CDToolBurner.WaitForReady(60000*4, 5000);
    if MediumIs <> mtDVD_RAM then
      CDToolBurner.ReadBufferCapacity(xDeviceBufferSize, xDeviceFreeBufferSize);
    if not fSaveISO then
    begin
      CDToolBurner.SetWriteParams(False, False, False, 1);
      CDToolBurner.SetCDSpeed(CDToolBurner.MaxReadSpeed, 2);
      CDToolBurner.Rewind;
      CDToolBurner.LockMedium(True);
      CDToolBurner.LoadMedium;
      CDToolBurner.Rewind;
      CDToolBurner.TestUnitReady;
    end;
  end;
CloseDiscSkipped:
  fwms := False;
  CDToolBurner.Lock;
{  if ASPILayerName = 'BMASPI32' then
    _ReInitializeASPI;}
  if th <> nil then
    CDToolBurner.fFileInProcess := th.FileName
  else
    CDToolBurner.fFileInProcess := '';
  Err := th.Error;
  CDToolBurner.Unlock;
  Burning := False;
  if Assigned(CDToolBurner.fOnWriteDone) then
  Begin
    //CDToolBurner.Lock;
    if CDToolBurner.ConsoleApplication then
    begin
      if (Assigned(CDToolBurner.fOnWriteDone)) then
        CDToolBurner.FOnWriteDone(Self, CDToolBurner.WriteDoneError);
    end
    else
    begin
      CDToolBurner.WriteDoneError := Err;
      Synchronize(CDToolBurner.WriteDoneEvent);
    end;

    //..Synchronize(CDToolBurner.WriteDoneEvent);
    //CDToolBurner.UnLock;
  End;
  CDToolBurner.LockDrive;
  CDToolBurner.fFileInProcess := '';

end;

 
 
 

procedure TCDToolBurner.WriteDoneEvent;
begin
  if (Assigned(fOnWriteDone))
    then FOnWriteDone(Self, WriteDoneError);
end;

 
 
 
procedure TCDToolBurner.Abort;
begin
  AbortWrite := True;
  fAborted := True;
end;

 
 
 
function TCDToolBurner.DoBurn;
begin
  wt := TThWrite.Create(True);
  wt.FreeOnTerminate := True;
  wt.Priority := tpTimeCritical;
  wt.fCacheSize := self.CacheSize;
  wt.fImageSize := TotalNoOfSectors - fStartAddress;
  wt.fSaveISO := ISO;
  wt.Resume;
  result := True;
end;

 
 
 
function TCDToolBurner.BurnISOImage;
var
  b: String;
  fs: TFileStreamEx;
  dt: Byte;
  MaxPacketSize: Integer;
begin
  CDToolBurner := Self;
  MaxPacketSize := 32*2048;
  dt := CDToolBurner.DiscType;
  if (dt = mtCD_R) or (dt = mtCD_RW) then
    MaxPacketSize := 27*2048;
  try
    fs := TFileStreamEx.Create(ISOFileName, fmOpenRead+fmShareDenyNone);
  except
    result := False;
    if Assigned(CDToolBurner.fOnWriteDone) then
      CDToolBurner.fOnWriteDone(Self, Format(ERR_FILEINUSE, [ISOFileName]));
    exit;
  end;
  CDToolBurner.fImageSize := Sectors(fs.Size);
  fs.Free;
  CDToolBurner := Self;
  th := TCacheThread.Create(Self.CacheSize, RootDir, ISOHeader, CDToolBurner.fImageSize * 2048, 0, b, CDToolBurner.fWritePostGap, MaxPacketSize, CDToolBurner.CriticalSection);
  th.ISOFileName := ISOFileName;
  while not th.CanStart do
  begin
    if th.Error <> ERR_NONE then
    begin
      result := False;
      CDToolBurner.fFileInProcess := th.FileName;
      if Assigned(CDToolBurner.fOnWriteDone) then
        CDToolBurner.fOnWriteDone(Self, th.Error);
      exit;
    end;
    Sleep(100);
  end;
  result := DoBurn(False);
end;
{
  ith := TISOBurnerThread.Create(Self.CacheSize, ISOFileName, CDToolBurner.fImageSize);
  CDToolBurner.fImageSize := CDToolBurner.fImageSize div DefaultSectorSize;
  it := TISOWrite.Create(True);
  it.Priority := tpTimeCritical;
  it.fCacheSize := self.CacheSize;
  it.fImageSize := TotalNoOfSectors - fStartAddress;
  while not ith.CanStart do
  begin
    if ith.Error <> ERR_NONE then
    begin
      result := False;
      if Assigned(CDToolBurner.fOnWriteDone) then
        CDToolBurner.fOnWriteDone(Self, ith.Error);
      exit;
    end;
    Sleep(10);
  end;
  it.Resume;
  result := True;
end;}
 
 
 
function TCDToolBurner.BurnCD;
var
  b: String;
  dt: Byte;
  MaxPacketSize: Integer;
begin
  CDToolBurner := Self;
  MaxPacketSize := 32*2048;
  dt := CDToolBurner.DiscType;
  if (dt = mtCD_R) or (dt = mtCD_RW) then
    MaxPacketSize := 27*2048;

  if Bootable then b := fBootImage else b := '';
  if (MediumIs = mtDVD_RW_RO) or (MediumIs = mtDVD_RW_SR) then
  begin
    if (MediumIs = mtDVD_RW_SR) then
      buf1 := #$00#$A2#$00#$08#$00#$00#$03#$E8#$54#$00#$00#$10
    else
      buf1 := #$00#$82#$00#$08#$00#$00#$00#$00#$4C#$00#$00#$10;
    if not (CDToolBurner.FormatUnit($11, @buf1, 12)) then
    begin
      buf1 := #$00#$82#$00#$08#$00#$00#$00#$00#$54#$00#$00#$10;
      CDToolBurner.FormatUnit($11, @buf1, 12);
    end;
    repeat
       Sleep(5000);
       CDToolBurner.ReadDiscInformation;
    until (CDToolBurner.LastSense.AddSenseCode <> 04);

  end;
  GetAddress := True;
  //CDToolBurner.GetNextWritableAddress;
  PrepareHeader(CDToolBurner.BuildHeaderISOFile, CDToolBurner.BuildHeaderTargetDir);
  BuildHeader(CDToolBurner.BuildHeaderISOFile, CDToolBurner.BuildHeaderTargetDir);
  GetAddress := False;
  th := TCacheThread.Create(Self.CacheSize, RootDir, ISOHeader, CDToolBurner.fImageSize * DefaultSectorSize, CDToolBurner.BootImageSize, b, CDToolBurner.fWritePostGap, MaxPacketSize, CDToolBurner.CriticalSection);
  th.ISOFileName := '';
  while not th.CanStart do
  begin
    if th.Error <> ERR_NONE then
    begin
      result := False;
      CDToolBurner.fFileInProcess := th.FileName;
      if Assigned(CDToolBurner.fOnWriteDone) then
        CDToolBurner.fOnWriteDone(Self, th.Error);
      exit;
    end;
    Sleep(100);
  end;
  result := DoBurn(False);
end;
 
 
 
function TCDToolBurner.SaveToISOFile;
var
  b: String;
begin
  if not QuickSave then
  begin

    fISOFileName := ISOFileName;
    PrepareHeader(True, BuildHeaderTargetDir);
    BuildHeader(True, BuildHeaderTargetDir);
    
    CDToolBurner := Self;
    if Bootable then b := fBootImage else b := '';
    th := TCacheThread.Create(Self.CacheSize, RootDir, ISOHeader, fImageSize * DefaultSectorSize, CDToolBurner.BootImageSize, b, fWritePostGap, 32*2048, CDToolBurner.CriticalSection);
    while not th.CanStart do
    begin
      if th.Error <> ERR_NONE then
      begin
        CDToolBurner.fFileInProcess := th.FileName;
        if Assigned(CDToolBurner.fOnWriteDone) then
          CDToolBurner.fOnWriteDone(Self, th.Error);
        result := -1;
        exit;
      end;
      SleepEx(100, False);
    end;
    DoBurn(True);
    result := -1;
    exit;
  end
  else
  begin
    PrepareHeader(True, BuildHeaderTargetDir);
    BuildHeader(True, BuildHeaderTargetDir);
  
    ISOHeader.SaveToFile(ISOFileName);
    AssignFile(f, ISOFileName);
    Reset(f, 1);
    Seek(f, FileSize(f));
    WriteFiles;
    CloseFile(f);
    if Assigned(fOnWriteDone) then
      fOnWriteDone(Self, '');
    result := 1;
  end;
end;
 
 
 
procedure ConvertFileNameJ(var dd: TDirectoryDescriptor);
var
  i: Integer;
  t: Char;
  WS: WideString;
  s: String;
begin

  SetLength(WS, dd.LenOfFileIdentifier div 2);
  i := 0;
  while ( i < dd.LenOfFileIdentifier div 2) do
  begin
    t := dd.FileName[i*2];
    dd.FileName[i*2] := dd.FileName[i*2+1];
    dd.FileName[i*2+1] := t;
    inc(i);
  end;
  move(dd.FileName, WS[1], dd.LenOfFileIdentifier);
  s := ws;
  fillchar(dd.FileName[0], 250, 0);
  move(s[1], dd.FileName, dd.LenOfFileIdentifier div 2);
  for i:= 2 to 249 do
  begin
    if dd.FileName[i] = ';' then
    begin
      fillchar(dd.FileName[i], 250-i, 0);
      exit;
    end;
  end;
end;
 
 
 
procedure ConvertFileName(var dd: TDirectoryDescriptor);
var
  i: Integer;
begin
  i := 0;
  while ( i < dd.LenOfFileIdentifier) do
  begin
    dd.FileName[i] := dd.FileName[i];
    inc(i);
  end;
  FillChar(dd.FileName[i], dd.LenOfFileIdentifier-i, 0);
  for i:= 0 to 250 do
  begin
    if dd.FileName[i] = #0 then exit;
    if dd.FileName[i] = ';' then
    begin
      FillChar(dd.FileName[i], dd.LenOfFileIdentifier-i, 0);
      exit;
    end;
  end;
end;
 
 
 
function TCDToolBurner.ImportSessionDirectoryJ(DirLocation, Size: Integer; DestinationDir: PDirEntry): Boolean;
var
  i: Integer;
  dd: TDirectoryDescriptor;
  tmpDir: PDirEntry;
  tmpDateTime: TDateTime;
  sr: TSearchRec;
  NumberOfSectorsToProcess: Integer;
  LocalBuffer: array[0..2352] of char;
label noMoreFile, again;
begin
  NumberOfSectorsToProcess := Sectors(Size);
  result := False;
  i := 0; fillchar(dd, Sizeof(dd), 0);
  fillchar(WriteBuffer[0], DefaultSectorSize, 0);
  if not Read10(DirLocation, 1, @WriteBuffer, DefaultSectorSize) then exit;
  move(WriteBuffer[0], LocalBuffer, DefaultSectorSize);
  move(LocalBuffer[i], dd, 34);
  i := i + dd.LenDr;
  move(LocalBuffer[i], dd, 34);
  i := i + dd.LenDr;
again:
  while i < DefaultSectorSize-33 do
  begin
    move(LocalBuffer[i], dd, 33);
    if dd.LenOfFileIdentifier < 215 then
      move(LocalBuffer[i+33], dd.FileName, dd.LenOfFileIdentifier)
    else
    begin
      DebugMsg(ERR_INVALIDFILENAME+#10#13+dd.FileName, mtFATALERROR);
    end;

    if dd.LenDr = 0 then goto noMoreFile;
    i := i + dd.LenDr;
    if i > DefaultSectorSize then goto noMoreFile;
    inc(k);
    ConvertFileNameJ(dd);
    sr.Size := dd.DataLength;
    sr.FindData.nFileSizeHigh := 0;
    sr.FindData.nFileSizeLow := dd.DataLength;
    sr.Attr := 0;
    try
      tmpDateTime := EncodeDate(dd.Year+1900, dd.Month, dd.Day);
    except
      tmpDateTime := 0;
    end;
    tmpDateTime := tmpDateTime + EncodeTime(dd.Hour, dd.Min, dd.Sec, 0);
    sr.Time := DateTimeToFileDate(tmpDateTime);
    sr.Name := dd.FileName;
    MakeShortFileName(sr.Name, sr);
    if ((dd.FileFlag and 1) = 1) then sr.Attr := sr.Attr or faHidden;
    if ((dd.FileFlag and 2) = 2) then
    begin
      sr.Attr := sr.Attr or faDirectory;
      tmpdir := CreateDir(DestinationDir, sr);
      ImportSessionDirectoryJ(dd.Address, dd.DataLength, tmpdir);
    end
    else
    begin
      InsertFile(DestinationDir, sr, '', dd.Address);
    end;
    result := True;
  end;
noMoreFile:
  dec(NumberOfSectorsToProcess);
  inc(DirLocation);
  i := 0;
  if NumberOfSectorsToProcess > 0 then
  begin
    if not Read10(DirLocation, 1, @WriteBuffer, DefaultSectorSize) then exit;
    move(WriteBuffer[0], LocalBuffer, DefaultSectorSize);
    goto again;
  end;
end;
 
 
 
function TCDToolBurner.ImportSessionDirectory(DirLocation, Size: Integer; DestinationDir: PDirEntry): Boolean;
var
  i: Integer;
  dd: TDirectoryDescriptor;
  tmpDir: PDirEntry;
  tmpDateTime: TDateTime;
  sr: TSearchRec;
  NumberOfSectorsToProcess: Integer;
  LocalBuffer: array[0..2352] of char;
label noMoreFile, again, skip;
begin
  NumberOfSectorsToProcess := Sectors(Size);
  result := False;
  i := 0;
  move(LocalBuffer[i], dd, 33);
  //i := 34;
  fillchar(dd, Sizeof(dd), 0);
again:
  fillchar(WriteBuffer[0], DefaultSectorSize, 0);
  if not Read10(DirLocation, 1, @WriteBuffer, DefaultSectorSize) then exit;
  move(WriteBuffer[0], LocalBuffer, DefaultSectorSize);
  while i < DefaultSectorSize-33 do
  begin
    if (LocalBuffer[i] = #0) and (LocalBuffer[i+1] = #0) and (LocalBuffer[i+2] = #0) and (LocalBuffer[i+3] = #0) and (LocalBuffer[i+5] = 'U') and (LocalBuffer[i+6] = 'X') and (LocalBuffer[i+7] = 'A') then
    begin
      inc( i, 14 );
      goto skip;
    end;
    move(LocalBuffer[i], dd, 33);
    if dd.LenOfFileIdentifier < 240 then
    begin
      if dd.LenOfFileIdentifier mod 2 = 0 then dd.LenOfFileIdentifier := dd.LenOfFileIdentifier + 1;
      move(LocalBuffer[i+33], dd.FileName, dd.LenOfFileIdentifier); 
      dd.FileName[dd.LenOfFileIdentifier] := #0;
    end
    else
    begin
      DebugMsg(ERR_INVALIDFILENAME+#10#13+dd.FileName, mtFATALERROR);
    end;

    //if FileNum < 2 then goto skip;
    if dd.LenDr = 0 then goto noMoreFile;
    i := i + dd.LenDr; 
    if i > DefaultSectorSize then goto noMoreFile;
    if ((dd.FileName[0] = #1) or (dd.FileName[1] = #0)) and (dd.FileName[2] = #0) then
      goto skip;
    inc(k);
    ConvertFileName(dd);
    sr.Size := dd.DataLength;
    sr.FindData.nFileSizeHigh := 0;
    sr.FindData.nFileSizeLow := dd.DataLength;
    sr.Attr := 0;
    tmpDateTime := EncodeDate(dd.Year+1900, dd.Month, dd.Day);
    tmpDateTime := tmpDateTime + EncodeTime(dd.Hour, dd.Min, dd.Sec, 0);
    sr.Time := DateTimeToFileDate(tmpDateTime);
    sr.Name := dd.FileName;
    MakeShortFileName(sr.Name, sr);
    if ((dd.FileFlag and 1) = 1) then sr.Attr := sr.Attr or faHidden;
    if ((dd.FileFlag and 2) = 2) then
    begin
      sr.Attr := sr.Attr or faDirectory;
      tmpdir := CreateDir(DestinationDir, sr);
      ImportSessionDirectory(dd.Address, dd.DataLength, tmpdir);
    end
    else
    begin
      InsertFile(DestinationDir, sr, '', dd.Address);
    end;
skip:
    result := True;
  end;
noMoreFile:
  dec(NumberOfSectorsToProcess);
  inc(DirLocation);
  i := 0;
  if NumberOfSectorsToProcess > 0 then goto again;

end;

 
 
 
function TCDToolBurner.ImportSession(SessionNo: ShortInt; DestinationDir: PDirEntry): Boolean;
var
  i: Integer;
  FirstSector: Cardinal;
  TryISO: Boolean;
  LastSession: Integer;
label Again, TermVDFound;
begin
  result := False;
  TryISO := False;
  if DestinationDir = nil then DestinationDir := RootDir;
  LastSession := SessionsOnDisc;
  if (LastSession = 0) then
    exit;
  if (SessionNo = -1) or (SessionNo > LastSession) then
    SessionNo := LastSession;
  if SessionNo = 0 then
    exit;
  DebugMsg('>>> '+Format(MSG_IMPORTINGSESSION, [SessionNo]), mtMESSAGE);
  fillchar(impvd, SizeOf(impvd), 0);
  fillchar(buffer, SizeOf(buffer), 0);
  if ReadTrackInformation(SessionNo) then
  begin
    FirstSector := TrackInformation.TrackStartAddress;
  end
  else
  begin
    DebugMsg('>>> '+ERR_IMPORTSESSION+' # '+IntToStr(SessionNo), mtFATALERROR);
    exit;
  end;
Again:  
  for i:=FirstSector+16 to FirstSector+36 do
  begin
    fillchar(impvd, SizeOf(impvd), 0);
    if Read10(i, 1, @impvd, DefaultSectorSize) then
    with impvd do if not TryISO then if (Identifier[1] = 'C') and (Identifier[2] = 'D') and (Identifier[3] = '0') and (Identifier[4] = '0') and (Identifier[5] = '1') then if (PdType = 2) then
    begin
      ImportSessionDirectoryJ(impvd.RootDirRec.Address, impvd.RootDirRec.DataLength, DestinationDir);
      result := True;
      exit;
    end;
    with impvd do if TryISO then if (Identifier[1] = 'C') and (Identifier[2] = 'D') and (Identifier[3] = '0') and (Identifier[4] = '0') and (Identifier[5] = '1') then if (PdType = 1) then
    begin
      ImportSessionDirectory(impvd.RootDirRec.Address, impvd.RootDirRec.DataLength, DestinationDir);
      result := True;
      exit;
    end;
    with impvd do if (Identifier[1] = 'C') and (Identifier[2] = 'D') and (Identifier[3] = '0') and (Identifier[4] = '0') and (Identifier[5] = '1') then if PdType = 255 then
      goto TermVDFound;
  end;
TermVDFound:
  if not TryISO then
  begin
    TryISO := True;
    goto again;
  end;
  DebugMsg('>>> '+Format(ERR_ISOIMAGENOTFOUND, [SessionNo]), mtFATALERROR);
end;
 
 
 
function  TCDToolBurner.MoveFile(DestinationPath, SourcePath: PDirEntry; SourceFile: PFileEntry): Boolean;
var
  f: PFileEntry;
begin
  result := False;
  f := GetLastFile(DestinationPath.Files);
  if f = nil then
  begin
    DestinationPath.Files := SourceFile;
  end
  else
    f.Next := SourceFile;
  if LastFile <> nil then
  begin
    LastFile.Next := SourceFile.Next;
  end
  else
  begin
    if SourceFile.Next = nil then
      SourcePath.Files := nil
    else
      SourcePath.Files := SourceFile.Next;
  end;
  SourceFile.Next := nil;
end;
 
 
 
function  TCDToolBurner.MoveFile(DestinationPath, SourcePath, SourceFile: String): Boolean;
var
  SrcDir, DestDir: PDirEntry;
  SrcFile: PFileEntry;
begin
  result := False;
  DestDir := FindDir(DestinationPath);
  SrcDir := FindDir(SourcePath);
  if (DestDir = nil) or (SrcDir = nil) then
  begin
    exit;
  end;
  SrcFile := FindFile(SrcDir, SourceFile);
  if srcFile = nil then
  begin
    exit;
  end;
  result := MoveFile(DestDir, SrcDir, SrcFile);
end;
 
 
 
function IndexOf_D(F: PDirEntry): Integer;
var
  found: Boolean;
begin
  result := 0;
  found := False;
  while (result < DirCounter) and (Dirs[Result] <> f) do
  begin
    found := True;
    Inc(Result);
  end;
  if not found then
    Result := -1;
end;
 
 
 
procedure Delete_D(var D: PDirEntry);
var
  index: Integer;
begin
  Index := IndexOf_D(D); 
  Dec(DirCounter);
  move(Dirs[Index+1], Dirs[Index], (DirCounter - Index) * 4);
  move(PathTable[Index+1], PathTable[Index], (DirCounter - Index) * 4);
  move(PathTableJ[Index+1], PathTableJ[Index], (DirCounter - Index) * 4);

  Dispose(D);
  D := nil;
  Dirs[DirCounter] := nil;
  PathTable[DirCounter] := nil;
  PathTableJ[DirCounter] := nil;
end;
 
 
 
function IndexOf_F(F: PFileEntry): Integer;
var
  found: Boolean;
  i: Integer;
begin
  result := 0;
  found := False;
  for i := 0 to FileCounter-1 do
  if (Files[i] = f) then
  begin
    found := True;
    Result := i;
    Break;
  end;
  if not found then
    Result := -1;
end;
 
 
 
procedure Delete_F(var F: PFileEntry);
var
  index: Integer;
begin
  Index := IndexOf_F(f);
  if Index >= 0 then
  begin
    Dec(FileCounter);
    move(Files[Index+1], Files[Index], (FileCounter - Index) * 4);
    Dispose(f);
    f := nil;
    Files[FileCounter] := nil;
  end;
end;
 
 
 
function TCDToolBurner.RemoveDir(SourceDir: String): Boolean;
var
  SrcDir : PDirEntry;
begin
  SrcDir := FindDir(SourceDir);
  result := RemoveDir(SrcDir);
end;
 
 
 
function TCDToolBurner.RemoveDir(var SourceDir: PDirEntry): Boolean;
var
  f, tmp: PFileEntry;
  d: PDirEntry;
begin
  result := False;
  if SourceDir.Path = '\' then exit;
  f := SourceDir.Files;
  while f <> nil do
  begin
    tmp := f;
    f := f.Next;
    RemoveFile(SourceDir, tmp);
  end;
  if (SourceDir.Path <> '\') and (SourceDir.Path <> '') then
  begin
    d := SourceDir.Parent;
    RemoveFile(d.Path, SourceDir.LongName);
    SourceDir := nil;
  end
  else
  begin
     SourceDir.Files := nil;
  end;
  result := True;
end;
 
 
 
function TCDToolBurner.ResetFilesArchiveBit: Boolean;
var
  i: Integer;
  p: PFileEntry;
  attr: Integer;
begin
  for i:=0 to FilesCount-1 do
  begin
    p := Files[i];
    attr := p.Attr;
    if ((p.Attr and faDirectory) <> faDirectory) then
    begin
      if ((p.Attr and faArchive) = faArchive) and p.ResetArchiveBit then
      begin
        Attr := Attr - faArchive;
        FileSetAttr(p.Path, attr);
      end;
    end;
  end;
  Result := True;
end;
 
 
 
function TCDToolBurner.TestFiles: Boolean;
var
  i: Integer;
  p: PFileEntry;
  fs: TFileStream;
  Stop: Boolean;
begin
  Result := True;
  for i:=0 to FilesCount-1 do
  begin
    p := Files[i];
    if ((p.Attr and faDirectory) <> faDirectory) then
    begin
      try
        fs := TFileStream.Create(p.Path, fmOpenRead+fmShareDenyNone);
        fs.Destroy;
      except
        Result := False;
        Stop := True;
        if Assigned(fOnTestFileFails) then
          fOnTestFileFails(Self, p.Path, Stop);
        if Stop then exit;
      end; 
    end;
  end;
end;
 
 
 
function TCDToolBurner.ResetAllFilesArchiveBit: Boolean;
var
  i: Integer;
  p: PFileEntry;
  attr: Integer;
begin
  for i:=0 to FilesCount-1 do
  begin
    p := Files[i];
    attr := p.Attr;
    if ((p.Attr and faDirectory) <> faDirectory) then
    begin
      if ((p.Attr and faArchive) = faArchive) then
      begin
        Attr := Attr - faArchive;
        FileSetAttr(p.Path, attr);
      end;
    end;
  end;
  Result := True;
end;
 
 
 
function TCDToolBurner.RemoveEmptyDirs: Boolean;
var
  i, j: Integer;
  d: PDirEntry;
  f: Boolean;
label start, again;
begin
  result := True;
start:
  if DirsCount < 1 then exit;
  j := 0;
  f := true;
  while f do
  begin
    f := false;
    for i:=1 to DirsCount do
    begin
      inc(j);
      d := Dirs[i];
      if (d <> nil) and (d.Files = nil) then
      begin
        RemoveDir(d);
        f := true;
        goto again;
      end;
    end;
again:
  end;
  d := Dirs[j];
  if DirsCount <> 0 then if (d <> nil) and (d.Files = nil) then
  begin
    RemoveDir(d);
    goto start;
  end;
end;
 
 
 
function  TCDToolBurner.RemoveDir_r(var SourceFile: PFileEntry): Boolean;
var
  f, tmp: PFileEntry;     
begin
  f := SourceFile.DirRec.Files;
  while f <> nil do
  begin
    tmp := f;
    f := f.Next;
    RemoveFile(SourceFile.DirRec, tmp);
  end;
  SourceFile.DirRec.Files := nil;
  result := True;
end;
 
 
 
function  TCDToolBurner.RemoveFile(var SourceDir: PDirEntry; var SourceFile: PFileEntry): Boolean;
var
  f: PFileEntry;
begin
  result := True;
  LastFile := nil;
  f :=  SourceDir.Files;
  while (f <> nil) and (SourceFile <> f) do
  begin
    LastFile := f;
    f := f.Next;
  end;
  if (SourceFile.Attr and faDirectory) = faDirectory then
  begin
    RemoveDir_r(SourceFile);
    Delete_D(SourceFile.DirRec);
  end;
  if LastFile = nil then
  begin
    SourceDir.Files := SourceFile.Next;
  end
  else
    LastFile.Next := SourceFile.Next;
  Delete_F(SourceFile);
end;
 
 
 
function TCDToolBurner.GetDirSize(Path: String): Int64;
var
  sr: TSearchRec;
  res: Integer;
label
  SkipEntry, SkipDir;
begin
  if Copy(path, Length(path), 1) = '\' then
    res := FindFirst(Path+'*.*', faAnyFile or faDirectory, sr)
  else
    res := FindFirst(Path, faAnyFile or faDirectory, sr);
  result := 0;
  while res = 0 do
  begin
    if ((sr.Attr and faDirectory) = faDirectory) and (sr.Name <> '.') and (sr.Name <> '..') then
      result := result + GetDirSize(Path+sr.Name+'\')
    else
      result := result + (sr.FindData.nFileSizeHigh * MAXDWORD) + sr.FindData.nFileSizeLow;
    res := FindNext(sr);
  end;
  FindClose(sr);
end;

 
 
 
function  TCDToolBurner.RemoveFile(SourceDir: String; SourceFile: String): Boolean;
var
  SrcDir: PDirEntry;
  SrcFile: PFileEntry;
begin
  result := False;
  SrcDir := FindDir(SourceDir);
  if SrcDir = nil then exit;
  SrcFile := FindFile(SrcDir, SourceFile);
  if SrcFile = nil then exit;
  result := RemoveFile(SrcDir, SrcFile);
end;
 
 
 
function  TCDToolBurner.RenameFile(SourceFile: PFileEntry; NewLongName, NewShortName: String): Boolean;
begin
  Result := False;
  SourceFile.LongName := NewLongName;
  if NewShortName = '' then
    NewShortName := NewLongName;
  if Length(NewShortName) > 13 then
    exit;
  SourceFile.ShortName := UpperCase(NewShortName);
  Result := True;
end;
 
 
 
function TCDToolBurner.RenameFile(SourceDir, SourceFile: String; NewLongName, NewShortName: String): Boolean;
var
  SrcDir: PDirEntry;
  SrcFile: PFileEntry;
begin
  result := False;
  SrcDir := FindDir(SourceDir);
  if SrcDir = nil then exit;
  SrcFile := FindFile(SrcDir, SourceFile);
  if SrcFile = nil then exit;
  result := RenameFile(SrcFile, NewLongName, NewShortName);
end;
 
 

function  TCDToolBurner.ExtractFile(FileToExtract: PFileEntry; TargetFile: String): Boolean;
var
  BytesToRead, SectorsToRead, BytesLeft, i: Cardinal;
  Buf: array[0..MaxWord] of char;
  F: TFileStream;
begin

  DebugMsg('>>> '+format(MSG_EXTRACTING_FILE,[FileToExtract.LongName, TargetFile]), mtMessage);
  i := FileToExtract.Address;
  fillchar(buf, sizeof(buf), 0);
  BytesLeft := FileToExtract.FileSize;
  f := TFileStream.Create(TargetFile, fmCreate);
  while BytesLeft <> 0 do
  begin
    if BytesLeft < 31*DefaultSectorSize then
      BytesToRead := BytesLeft
    else
      BytesToRead := 31 * DefaultSectorSize;
    SectorsToRead := Sectors(BytesToRead);
    read10(i, SectorsToRead, buf, SectorsToRead * DefaultSectorSize);
    f.Write(buf[0], BytesToRead);
    inc(i, SectorsToRead);
    Dec(BytesLeft, BytesToRead);
  end;
  FileSetDate(f.Handle, DateTimeToFileDate(FileToExtract.Time));
  f.Destroy;
  result := True;
end;
end.
