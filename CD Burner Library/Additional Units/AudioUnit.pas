unit AudioUnit;

interface

uses
  Windows, Messages,dialogs, Classes, mmSystem, WaveUtils;


Type
  TCDTrack = class(TMemoryStream)
  private
    fDirty: Boolean;
    fValid: Boolean;
    fDataSize: DWORD;
    fDataOffset: DWORD;
    fData: Pointer;
    fWaveFormat: PWaveFormatEx;
    fOnChange: TNotifyEvent;
    FTrackFileName : String;
    FTrackName : String;
    function GetValid: Boolean;
    function GetData: Pointer;
    function GetDataSize: DWORD;
    function GetDataOffset: DWORD;
    function GetLength: DWORD;
    function GetBitRate: DWORD;
    function GetPeakLevel: Integer;
    function GetPCMFormat: TPCMFormat;
    function GetWaveFormat: PWaveFormatEx;
    function GetAudioFormat: String;
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
    function UpdateDetails: Boolean; virtual;
    function MSecToByte(MSec: DWORD): DWORD;
    procedure DoChange;
    property Dirty: Boolean read fDirty;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function Equals(Track: TCDTrack): Boolean;
    function SameFormat(Track: TCDTrack): Boolean;
    procedure Crop;
    function Invert: Boolean;
    function ChangeVolume(Percent: Integer): Boolean;
    function ConvertTo(const pTargetWaveFormat: PWaveFormatEx): Boolean;
    function ConvertToPCM(TargetFormat: TPCMFormat): Boolean;
    function Delete(Pos: DWORD; Len: DWORD): Boolean;
    function Insert(Pos: DWORD; Wave: TCDTrack): Boolean;
    function InsertSilence(Pos: DWORD; Len: DWORD): Boolean;
    function Write(const Buffer; Count: Longint): Longint; override;
    Property TrackName : String Read FTrackName Write FTrackName;
    Property TrackFileName : String Read FTrackFileName Write FTrackFileName;
    property Valid: Boolean read GetValid;
    property Data: Pointer read GetData;
    property DataSize: DWORD read GetDataSize;
    property DataOffset: DWORD read GetDataOffset;
    property PCMFormat: TPCMFormat read GetPCMFormat;
    property WaveFormat: PWaveFormatEx read GetWaveFormat;
    property AudioFormat: String read GetAudioFormat;
    property Length: DWORD read GetLength;              // in milliseconds
    property BitRate: DWORD read GetBitRate;            // in kbps
    property PeakLevel: Integer read GetPeakLevel;      // in percent
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
  end;


  TCDTrackItem = class;
  TCDTrackList = class;
  TCDTrackItemClass = class of TCDTrackItem;

 // TTrack item
  TCDTrackItem = class(TCollectionItem)
  private
    fName: String;
    fCDTrack: TCDTrack;
    fTag: Integer;
    procedure SetWave(Value: TCDTrack);
    procedure ReadData(Stream: TStream);
    procedure WriteData(Stream: TStream);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: String; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    Procedure LoadWaveFile(Filename : String);
    Procedure SaveWaveFile(Filename : String);
  published
    property CDTrack: TCDTrack read fCDTrack write fCDTrack;
    property Name: String read fName write fName;
    property Tag: Integer read fTag write fTag default 0;
  end;


  // TTrackList
  TCDTrackList = class(TCollection)
  private
    fOwner: TPersistent;
    function GetItem(Index: Integer): TCDTrackItem;
    procedure SetItem(Index: Integer; Value: TCDTrackItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TPersistent; ItemClass: TCDTrackItemClass); virtual;
    function Add: TCDTrackItem;
    function Insert(Index: Integer): TCDTrackItem;
    property Tracks[Index: Integer]: TCDTrackItem read GetItem write SetItem; default;
  end;




implementation

uses
  SysUtils;



{ TCDTrack }

constructor TCDTrack.Create;
begin
  inherited Create;
  fDirty := False;
  fWaveFormat := nil;
end;

destructor TCDTrack.Destroy;
begin
  if Assigned(fWaveFormat) then
    FreeMem(fWaveFormat);
  inherited Destroy;
end;

function TCDTrack.Realloc(var NewCapacity: Integer): Pointer;
begin
  Result := inherited Realloc(NewCapacity);
  if not Dirty then DoChange;
end;

function TCDTrack.Write(const Buffer; Count: Integer): Longint;
begin
  Result := inherited Write(Buffer, Count);
  if not Dirty then DoChange;
end;

procedure TCDTrack.DoChange;
begin
  fDirty := True;
  if Assigned(fOnChange) then
    fOnChange(Self);
end;

function TCDTrack.MSecToByte(MSec: DWORD): DWORD;
begin
  with fWaveFormat^ do
    Result := MulDiv(nAvgBytesPerSec, MSec, 1000)
          and ($FFFFFFFF shl (nChannels * wBitsPerSample div 16));
end;

function TCDTrack.UpdateDetails: Boolean;
begin
  if fDirty then
  begin
    fValid := False;
    fDirty := False;
    if Assigned(fWaveFormat) then
    begin
      FreeMem(fWaveFormat);
      fWaveFormat := nil;
    end;
    if GetStreamWaveAudioInfo(Self, fWaveFormat, fDataSize, fDataOffset) then
    begin
      fData := Pointer(DWORD(Memory) + fDataOffset);
      fValid := True;
    end;
  end;
  Result := fValid;
end;

function TCDTrack.GetAudioFormat: String;
begin
  if UpdateDetails then
    Result := GetWaveAudioFormat(fWaveFormat)
  else
    Result := '';
end;

function TCDTrack.GetBitRate: DWORD;
begin
  if UpdateDetails then
    Result := GetWaveAudioBitRate(fWaveFormat)
  else
    Result := 0;
end;

function TCDTrack.GetPeakLevel: Integer;
begin
  if PCMFormat <> nonePCM then
    Result := GetWaveAudioPeakLevel(fData, fDataSize, fWaveFormat.wBitsPerSample)
  else
    Result := -1;
end;

function TCDTrack.GetLength: DWORD;
begin
  if UpdateDetails then
    Result := GetWaveAudioLength(fWaveFormat, fDataSize)
  else
    Result := 0;
end;

function TCDTrack.GetData: Pointer;
begin
  if UpdateDetails then
    Result := fData
  else
    Result := nil;
end;

function TCDTrack.GetDataSize: DWORD;
begin
  if UpdateDetails then
    Result := fDataSize
  else
    Result := 0;
end;

function TCDTrack.GetDataOffset: DWORD;
begin
  if UpdateDetails then
    Result := fDataOffset
  else
    Result := 0;
end;

function TCDTrack.GetValid: Boolean;
begin
  Result := UpdateDetails;
end;

function TCDTrack.GetPCMFormat: TPCMFormat;
begin
  if UpdateDetails then
    Result := GetPCMAudioFormat(fWaveFormat)
  else
    Result := nonePCM;
end;

function TCDTrack.GetWaveFormat: PWaveFormatEx;
begin
  if UpdateDetails then
    Result := fWaveFormat
  else
    Result := nil;
end;

function TCDTrack.Equals(Track: TCDTrack): Boolean;
begin
  if Valid = Track.Valid then
    if fValid and Track.fValid then
      Result :=
        (fDataSize = Track.fDataSize) and
        (fWaveFormat^.cbSize = Track.fWaveFormat^.cbSize) and
         CompareMem(fWaveFormat, Track.fWaveFormat,
                    SizeOf(TWaveFormatEx) + fWaveFormat^.cbSize) and
         CompareMem(fData, Track.fData, fDataSize)
    else
      Result :=
       (Size = Track.Size) and
        CompareMem(Memory, Track.Memory, Size)
  else
    Result := False;
end;

function TCDTrack.SameFormat(Track: TCDTrack): Boolean;
begin
  if Valid and Track.Valid then
    Result :=
      (fWaveFormat^.cbSize = Track.fWaveFormat^.cbSize) and
       CompareMem(fWaveFormat, Track.fWaveFormat,
                  SizeOf(TWaveFormatEx) + fWaveFormat^.cbSize)
  else
    Result := False;
end;


procedure TCDTrack.Crop;
begin
  Size := DataOffset + DataSize;
end;

function TCDTrack.Invert: Boolean;
begin
  Result := False;
  if PCMFormat <> nonePCM then
  begin
    InvertWaveAudio(fData, fDataSize, fWaveFormat.wBitsPerSample);
    Result := True;
  end;
end;

function TCDTrack.ChangeVolume(Percent: Integer): Boolean;
begin
  Result := False;
  if PCMFormat <> nonePCM then
  begin
    ChangeWaveAudioVolume(fData, fDataSize, fWaveFormat.wBitsPerSample, Percent);
    Result := True;
  end;
end;

function TCDTrack.ConvertTo(const pTargetWaveFormat: PWaveFormatEx): Boolean;
var
  NewData: Pointer;
  NewDataSize: DWORD;
  ckInfo, ckData: TMMCKInfo;
  mmIO: HMMIO;
begin
  Result := False;
  if Valid then
  begin
    if (fWaveFormat.cbSize <> pTargetWaveFormat^.cbSize) or
       not CompareMem(fWaveFormat, pTargetWaveFormat, SizeOf(TWaveFormatEx) + fWaveFormat.cbSize) then
    begin
      if ConvertWaveFormat(fWaveFormat, fData, fDataSize, pTargetWaveFormat, NewData, NewDataSize) then
        try
          mmIO := CreateStreamWaveAudio(Self, pTargetWaveFormat, ckInfo, ckData);
          try
            mmioWrite(mmIO, NewData, NewDataSize);
          finally
            CloseWaveAudio(mmio, ckInfo, ckData);
          end;
          Result := True;
        finally
          ReallocMem(NewData, 0);
        end;
    end
    else
      Result := True;
  end;
end;

function TCDTrack.ConvertToPCM(TargetFormat: TPCMFormat): Boolean;
var
  NewWaveFormat: TWaveFormatEx;
begin
  Result := False;
  if TargetFormat <> nonePCM then
  begin
    SetPCMAudioFormatS(@NewWaveFormat, TargetFormat);
    Result := ConvertTo(@NewWaveFormat);
  end;
end;


function TCDTrack.Delete(Pos, Len: DWORD): Boolean;
var
  Index: DWORD;
  NewWave: TCDTrack;
  ckInfo, ckData: TMMCKInfo;
  mmIO: HMMIO;
begin
  Result := False;
  if Valid and (Len > 0) and (Pos < Length) then
  begin
    NewWave := TCDTrack.Create;
    try
      mmIO := CreateStreamWaveAudio(NewWave, fWaveFormat, ckInfo, ckData);
      try
        Index := MSecToByte(Pos);
        if Index > fDataSize then
          Index := fDataSize;
        if Index > 0 then
          mmioWrite(mmIO, fData, Index);
        Inc(Index, MSecToByte(Len));
        if Index < fDataSize then
          mmioWrite(mmIO, Pointer(DWORD(fData) + Index), fDataSize - Index);
      finally
        CloseWaveAudio(mmio, ckInfo, ckData);
      end;
      LoadFromStream(NewWave);
      Result := True;
    finally
      NewWave.Free;
    end;
  end;
end;

function TCDTrack.Insert(Pos: DWORD; Wave: TCDTrack): Boolean;
var
  Index: DWORD;
  NewWave: TCDTrack;
  ckInfo, ckData: TMMCKInfo;
  mmIO: HMMIO;
begin
  Result := False;
  if SameFormat(Wave) then
  begin
    NewWave := TCDTrack.Create;
    try
      mmIO := CreateStreamWaveAudio(NewWave, fWaveFormat, ckInfo, ckData);
      try
        Index := MSecToByte(Pos);
        if Index > fDataSize then
          Index := fDataSize;
        if Index > 0 then
          mmioWrite(mmIO, fData, Index);
        mmioWrite(mmIO, Wave.fData, Wave.fDataSize);
        if Index < fDataSize then
          mmioWrite(mmIO, Pointer(DWORD(fData) + Index), fDataSize - Index);
      finally
        CloseWaveAudio(mmio, ckInfo, ckData);
      end;
      LoadFromStream(NewWave);
      Result := True;
    finally
      NewWave.Free;
    end;
  end;
end;

function TCDTrack.InsertSilence(Pos, Len: DWORD): Boolean;
var
  Index: DWORD;
  SilenceBytes: DWORD;
  Silence: Byte;
  NewWave: TCDTrack;
  ckInfo, ckData: TMMCKInfo;
  mmIO: HMMIO;
begin
  Result := False;
  if (PCMFormat <> nonePCM) and (Len > 0) then
  begin
    NewWave := TCDTrack.Create;
    try
      mmIO := CreateStreamWaveAudio(NewWave, fWaveFormat, ckInfo, ckData);
      try
        Index := MSecToByte(Pos);
        if Index > fDataSize then
          Index := fDataSize;
        if Index > 0 then
          mmioWrite(mmIO, fData, Index);
        if fWaveFormat.wBitsPerSample = 8 then
          Silence := 128
        else
          Silence := 0;
        SilenceBytes := MSecToByte(Len);
        while SilenceBytes > 0 do
        begin
          mmioWrite(mmIO, PChar(@Silence), 1);
          Dec(SilenceBytes);
        end;
        if Index < fDataSize then
          mmioWrite(mmIO, Pointer(DWORD(fData) + Index), fDataSize - Index);
      finally
        CloseWaveAudio(mmio, ckInfo, ckData);
      end;
      LoadFromStream(NewWave);
      Result := True;
    finally
      NewWave.Free;
    end;
  end;
end;



// CDTracks


constructor TCDTrackItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  fCDTrack := TCDTrack.Create;
end;

destructor TCDTrackItem.Destroy;
begin
  fCDTrack.Free;
  inherited Destroy;
end;

procedure TCDTrackItem.SetWave(Value: TCDTrack);
begin
  if CDTrack <> Value then
  begin
    if Assigned(Value) then
      CDTrack.LoadFromStream(Value)
    else
      CDTrack.Clear;
  end;
end;


Procedure TCDTrackItem.LoadWaveFile(Filename : String);
begin
    if FileExists(Filename) then
    begin
      CDTrack.LoadFromFile(Filename);
      CDTrack.TrackFileName := Filename;
      CDTrack.TrackName := ExtractFileName(Filename);
     end
    else
      CDTrack.Clear;
end;


Procedure TCDTrackItem.SaveWaveFile(Filename : String);
begin
      CDTrack.SaveToFile(Filename);
      CDTrack.TrackFileName := Filename;
end;


procedure TCDTrackItem.ReadData(Stream: TStream);
begin
  CDTrack.LoadFromStream(Stream);
end;

procedure TCDTrackItem.WriteData(Stream: TStream);
begin
  CDTrack.SaveToStream(Stream);
end;

procedure TCDTrackItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, CDTrack.Size > 0);
end;

procedure TCDTrackItem.Assign(Source: TPersistent);
begin
  if Source is TCDTrackItem then
  begin
    CDTrack := TCDTrackItem(Source).CDTrack;
    Name := TCDTrackItem(Source).Name;
    Tag := TCDTrackItem(Source).Tag;
  end
  else
    inherited Assign(Source);
end;

function TCDTrackItem.GetDisplayName: String;
var
  WaveInfo: String;
begin
  if (CDTrack <> nil) and (CDTrack.Size <> 0) then
  begin
    if CDTrack.Valid then
      WaveInfo := CDTrack.AudioFormat + ', ' +
                  IntToStr(CDTrack.BitRate) + ' kbps, ' +
                  MS2Str(CDTrack.Length, msAh) + ' sec.'
    else
      WaveInfo := 'Invalid Content';
  end
  else
    WaveInfo := 'Empty';
  Result := Name + ' (' + WaveInfo + ')';
end;



{ TTrackList }

constructor TCDTrackList.Create(AOwner: TPersistent; ItemClass: TCDTrackItemClass);
begin
  inherited Create(ItemClass);
  fOwner := AOwner;
end;

function TCDTrackList.GetOwner: TPersistent;
begin
  Result := fOwner;
end;

function TCDTrackList.Add: TCDTrackItem;
var
   Track : TCDTrackItem;
begin
try
   Result := TCDTrackItem(Inherited Add);
  except
     on e:exception do
        showmessage(e.Message);
  end;
end;


function TCDTrackList.Insert(Index: Integer): TCDTrackItem;
begin
  Result := TCDTrackItem(inherited Insert(Index));
end;


function TCDTrackList.GetItem(Index: Integer): TCDTrackItem;
begin
  Result := TCDTrackItem(inherited Items[Index]);
end;

procedure TCDTrackList.SetItem(Index: Integer; Value: TCDTrackItem);
begin
  inherited Items[Index] := Value;
end;





end.
