unit CDCache;
interface

uses
  Windows, SysUtils, Classes, SyncObjs, CDISO, CDConst, CDStream;

type
  PPacket = ^TPacket;
  TPacket = record
    Data: array[0..32*2048] of char;
    Size: Integer;
  end;

  TCacheThread = class(TThread)
  private
    Index: TList;
    PostGap: Boolean;
    BootImageSize: Integer;
    CacheSize: Integer;
    IndexCapacity: Integer;
    LastBlockWas: Integer;
    fCriticalSection: TCriticalSection;
    vPriority : TThreadPriority;
    Busy1, Busy2: Boolean;
  protected
    procedure Execute; override;
    procedure On_Terminate(Sender: TObject);
  public
    Aborted,
    Finished: Boolean;
    ISOFileName: String;
    BytesAvailable: Int64;
    TotalImageSize: Int64;
    BufferSize: Int64;
    CanStart: Boolean;
    MaxPacketSize: Integer;
    FileName: String;
    Error: String;
    RemainingBytes: Int64;
    ISOHeader: TMemoryStream;
    BootImagePath: String;
    tmpbuf: array[0..32*2048] of char;
    procedure Abort;
    procedure Lock;
    procedure unLock;
    function GetFirst(Buf: PChar): Integer;
    constructor Create(Size: Integer; RootDir: PDirEntry; fISOHeader: TMemoryStream; ImageSize: Int64; BISize: Integer; bi: String; WritePostGap: Boolean; MaxPacketSize: Integer; CriticalSection: TCriticalSection);
  end;

implementation
var
  MaxBytesToRead : Integer = (32 * 2048);
  PacketSize: Integer = 32 * 2048;

{$WARNINGS OFF}

constructor TCacheThread.Create(Size: Integer; RootDir: PDirEntry; fISOHeader: TMemoryStream; ImageSize: Int64; BISize: Integer; bi: String; WritePostGap: Boolean; MaxPacketSize: Integer; CriticalSection: TCriticalSection);
begin

  inherited Create(True);
  Index := TList.Create;
  PacketSize := MaxPacketSize;
  IndexCapacity := Size div (PacketSize);
  CacheSize := Size;
  ISOHeader := fISOHeader;
  LastBlockWas := -1;
  Error := ERR_NONE;
  Priority := tpLower;
  vPriority := tpLower;
  CanStart := False;
  BootImageSize := BISize;
  OnTerminate := On_Terminate;
  RemainingBytes := ImageSize;
  TotalImageSize := ImageSize;
  BootImagePath := bi;
  fCriticalSection := CriticalSection;
  if (ISOFileName = '') then
  begin
    if WritePostGap and (ImageSize < 614400) then
      PostGap := True
    else
      PostGap := False;
  end;
  Resume;
end;
procedure TCacheThread.On_Terminate;
begin
  Index.Destroy;
end;
procedure TCacheThread.Abort;
begin
  Aborted := True;
end;
var
  xcount: Integer = 0;
procedure TCacheThread.Lock;
begin
  Inc(xCount);
  fCriticalSection.Enter;
end;
procedure TCacheThread.Unlock;
begin
  fCriticalSection.Leave;
  Dec(xCount);
end;
procedure TCacheThread.Execute;
var
  fs: TFileStreamEx;
  f: PFileEntry;
  bf: TFEntry;
  Packet: PPacket;
  Count, BytesToRead: Integer;
  BytesLeft: Int64;
  i: Integer;
  Position,
  FileSize: Int64;
  AvailableBytesInPacket: Integer;
  src: String;
  NoOfFiles: Integer;
label fillagain;
begin

  BytesAvailable := 0;
  Aborted := False;
  Finished := False;
  Error := ERR_NONE;  fs := nil;
  BytesLeft := ISOHeader.Size;
  ISOHeader.Seek(0, soFromBeginning);
  Position := 0; AvailableBytesInPacket := 0;
  if (ISOFileName = '') then
  begin
    NoOfFiles := FileCounter-1;
    while BytesLeft > 0 do
    begin
      if BytesLeft < PacketSize then
        BytesToRead := BytesLeft
      else
        BytesToRead := PacketSize;
      New(Packet);
      ISOHeader.Read(Packet.Data[0], BytesToRead);
      AvailableBytesInPacket := PacketSize - BytesToRead;
      if BytesToRead = PacketSize then
      begin
        Packet.Size := BytesToRead;
        Index.Add(Packet);
        Position := 0;
      end
      else
      begin
        Position := BytesToRead;
        move(Packet.Data[0], tmpbuf[0], BytesToRead);
      end;
      BytesLeft := BytesLeft - BytesToRead;
      BytesAvailable := BytesAvailable + BytesToRead;
      Count := Index.Count;
      while Count >= IndexCapacity-1 do
      begin
        CanStart := True;
        Sleep(1);
        Lock;
        Count := Index.Count;
        Unlock;
      end;

    end;

    ISOHeader.Clear;
    while Index.Count >= IndexCapacity-1 do
    begin
      CanStart := True;
      while Busy2 do
        Sleep(10);
      Busy1 := True;

      Sleep(1);
      if Aborted then
      begin
        Lock;
        while Index.Count <> 0 do
        begin
          Packet := PPacket(Index.Items[0]);
          Dispose(Packet);
          Index.Delete(0);
        end;
        if Error = ERR_NONE then
          Error := ERR_ABORTED;
        Finished := True;
        Unlock;
        Terminate;
        exit;
      end;
    end;
  end
  else
    NoOfFiles := -1;
  for i:=-1 to NoOfFiles do
  begin
    if (i <> -1) then
    begin
      try
        f := Files[i];
      except
        Lock;
        Error := Format(ERR_5, [src]);
        Finished := True;
        Unlock;
        Terminate;
        exit;
      end;
    end
    else
    begin
      f := @bf;
      if ISOFileName = '' then
      begin

        if (BootImagePath <> '') then
        begin
          bf.Path := BootImagePath;
          bf.FileSize := BootImageSize * 2048;
          bf.Attr := 0;
        end
        else
        begin
          bf.Attr := faDirectory;
        end;
      end
      else
      begin
        f.Path := ISOFileName;
        bf.FileSize := RemainingBytes;
        bf.Attr := 0;

      end;

    end;
    begin
      src := f.Path;
      if ((f.Attr and faDirectory) <> faDirectory) and (Src <> '') and (f.FileSize <> 0) then
      begin
        FileName := src;
        if fs <> nil then fs.Destroy;
        try
          fs := TFileStreamEx.Create(src, fmOpenRead+fmShareDenyNone);
        except
          Lock;
          Error := Format(ERR_FILEINUSE, [src]);
          Finished := True;
          Unlock;
          Terminate;
          exit;
        end;
        FileSize := f.FileSize;
        BytesLeft := FileSize;
        repeat
          if AvailableBytesInPacket = 0 then
          begin
            New(Packet); Packet.Size := 0; Position := 0; AvailableBytesInPacket := PacketSize;
          end;
          if BytesLeft < AvailableBytesInPacket then
            BytesToRead := BytesLeft
          else
            BytesToRead := AvailableBytesInPacket;
          fs.Read(tmpbuf[Position], BytesToRead);
          Inc(Position, (Sectors(BytesToRead) * DefaultSectorSize));
          AvailableBytesInPacket := AvailableBytesInPacket - (Sectors(BytesToRead) * DefaultSectorSize);
          if Aborted then
          begin
            while Busy2 do
              Sleep(10);
            Busy1 := True;
            Lock;
            while Index.Count <> 0 do
            begin
              Packet := PPacket(Index.Items[0]);
              Dispose(Packet);
              Index.Delete(0);
            end;
            if Error = ERR_NONE then
              Error := ERR_ABORTED;
            Finished := True;
            fs.Destroy;
            Unlock;
            Terminate;
            exit;
          end;
          Lock; Busy1 := False; 
          if (i = FileCounter - 1) or (AvailableBytesInPacket = 0) then
          begin
            move(tmpbuf, Packet.Data[0], Position);
            Packet.Size := Position;
            Index.Add(Packet);
            Position := 0;
          end;
          Count := Index.Count;
          BytesAvailable := BytesAvailable + Sectors(BytesToRead) * DefaultSectorSize;
          BytesLeft := BytesLeft - BytesToRead;
          Unlock;

          if (Count < IndexCapacity div 4) and (vPriority <> tpNormal) then
          begin
            vPriority := tpNormal;
            Priority := tpNormal;
          end
          else if (Count < IndexCapacity div 2) and (vPriority <> tpLower) then
          begin
            vPriority := tpLower;
            Priority := tpLower;
          end
          else if vPriority <> tpLowest then
          begin
            vPriority := tpLowest;
            Priority := tpLowest;
          end;

          while Count >= IndexCapacity-1 do
          begin
            if vPriority <> tpIdle then
            begin
              vPriority := tpIdle;
              Priority := tpIdle;
            end;
            if Aborted then
            begin
              while Busy2 do
                Sleep(10);
              Busy1 := True;

              Lock;
              while Index.Count <> 0 do
              begin
                Packet := PPacket(Index.Items[0]);
                Dispose(Packet);
                Index.Delete(0);
              end;
              if Error = ERR_NONE then
                Error := ERR_ABORTED;
              Finished := True;
              fs.Destroy;
              Unlock;
              Busy1 := False;
              Terminate;
              exit;
            end;
            CanStart := True;
            Sleep(1);
            Lock;
            Count := Index.Count;
            Unlock;
          end;
        until (BytesLeft = 0);
      end;
    end;
  end;
  if Position <> 0 then
  begin
    move(tmpbuf, Packet.Data[0], Position);
    Packet.Size := Position;
    Index.Add(Packet);
    BytesAvailable := BytesAvailable + Position;
  end;
  if fs <> nil then
    fs.Destroy;

  if (PostGap) and (ISOFileName = '') then
  begin
    Lock;
    if TotalImageSize < 614400 then
      TotalImageSize := 614400 - TotalImageSize;
    RemainingBytes := RemainingBytes + TotalImageSize;
fillagain:
    if TotalImageSize > PacketSize then
      BytesToRead := PacketSize
    else
      BytesToRead := TotalImageSize;
    New(Packet);   //
    fillchar(Packet.Data[0], BytesToRead, $00);
    Packet.Size := BytesToRead;
    Index.Add(Packet);
    TotalImageSize := TotalImageSize - BytesToRead;
    if TotalImageSize <> 0 then goto fillagain;
    Unlock;
  end;
  CanStart := True;
  while (RemainingBytes > 0) do
  begin
    if Aborted then
    begin
      while Busy2 do
        Sleep(10);
      Busy1 := True;
      Finished := True;
      exit;
    end;
    Sleep(10);
  end;

  Finished := True;
  Sleep(1000);
end;

function TCacheThread.GetFirst(Buf: PChar): Integer;
var
  p: PPacket;
  Count: Integer;
begin
  if (RemainingBytes = 0) and Finished then
  begin
    Result := 0;
    exit;
  end;
  Count := Index.Count;
  if Count > 0 then
  begin
    while Busy1 do
    begin
      Sleep(1);
      if Aborted then
      begin
        result := 0;
        exit;
      end;
    end;
    Lock;
    Busy2 := True;
    if Count <> 0 then
    begin
      p := PPacket(Index.Items[0]);
      result := p.Size;
      move(p.Data[0], buf[0], result);
      Dispose(p);
      Index.Delete(0);
      BytesAvailable := BytesAvailable - result;
      RemainingBytes := RemainingBytes - result;
    end
    else
    begin
      if Finished then
        Result := 0
      else
        Result := -1;
    end;
    Busy2 := False;
    Unlock;
  end
  else
  begin
    if Finished then
      result := 0
    else
      result := -1;
  end;
end;
end.


