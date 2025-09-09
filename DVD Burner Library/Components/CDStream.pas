unit CDStream;

{$IFDEF VER150}
{$DEFINE DELPHI6+}
{$DEFINE DELPHI7+}
{$ENDIF}
{$IFDEF ver140}
{$DEFINE DELPHI6+}
{$ENDIF}
{$IFDEF DELPHI6+}
{$WARN SYMBOL_PLATFORM OFF}
{$WARN UNIT_PLATFORM OFF}
{$IFDEF DELPHI7+}
{$WARN UNSAFE_CODE OFF}
{$WARN UNSAFE_TYPE OFF}
{$ENDIF}
{$ENDIF}

interface

uses
  Classes, SysUtils, Windows, CDConst;

type

  TStreamEx = class(TObject)
  private
    function GetPosition: Int64;
    procedure SetPosition(Pos: Int64);
    function GetSize: Int64;
  protected
    procedure SetSize(NewSize: Int64); virtual;
  public
    function Read(var Buffer; Count: Longint): Longint; virtual; abstract;
    function Write(const Buffer; Count: Longint): Longint; virtual; abstract;
    function Seek(Offset: Int64; Origin: Word): Int64; virtual; abstract;
    procedure ReadBuffer(var Buffer; Count: Longint);
    procedure WriteBuffer(const Buffer; Count: Longint);
    function CopyFrom(Source: TStreamEx; Count: Longint): Longint;
    procedure ReadResHeader;
    property Position: Int64 read GetPosition write SetPosition;
    property Size: Int64 read GetSize write SetSize;
  end;
  
  THandleStreamEx = class(TStreamEx)
  private
    FHandle: Integer;
  protected
    procedure SetSize(NewSize: Int64); override;
  public
    constructor Create(AHandle: Integer);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Int64; Origin: Word): Int64; override;
    property Handle: Integer read FHandle;
  end;

  TFileStreamEx = class(THandleStreamEx)
  public
    constructor Create(const FileName: string; Mode: Word);
    destructor Destroy; override;
  end;

implementation

function TStreamEx.GetPosition: Int64;
begin
  Result := Seek(0, 1);
end;

procedure TStreamEx.SetPosition(Pos: Int64);
begin
  Seek(Pos, 0);
end;

function TStreamEx.GetSize: Int64;
var
  Pos: Int64;
begin
  Pos := Seek(0, 1);
  Result := Seek(0, 2);
  Seek(Pos, 0);
end;

procedure TStreamEx.SetSize(NewSize: Int64);
begin

end;

procedure TStreamEx.ReadBuffer(var Buffer; Count: Longint);
begin

end;

procedure TStreamEx.WriteBuffer(const Buffer; Count: Longint);
begin

end;

function TStreamEx.CopyFrom(Source: TStreamEx; Count: Longint): Longint;
const
  MaxBufSize = $F000;
var
  BufSize, N: Integer;
  Buffer: PChar;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end;
  Result := Count;
  if Count > MaxBufSize then BufSize := MaxBufSize else BufSize := Count;
  GetMem(Buffer, BufSize);
  try
    while Count <> 0 do
    begin
      if Count > BufSize then N := BufSize else N := Count;
      Source.ReadBuffer(Buffer^, N);
      WriteBuffer(Buffer^, N);
      Dec(Count, N);
    end;
  finally
    FreeMem(Buffer, BufSize);
  end;
end;

procedure TStreamEx.ReadResHeader;
begin

end;

constructor THandleStreamEx.Create(AHandle: Integer);
begin
  FHandle := AHandle;
end;

function THandleStreamEx.Read(var Buffer; Count: Longint): Longint;
begin
  Result := FileRead(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStreamEx.Write(const Buffer; Count: Longint): Longint;
begin
  Result := FileWrite(FHandle, Buffer, Count);
  if Result = -1 then Result := 0;
end;

function THandleStreamEx.Seek(Offset: Int64; Origin: Word): Int64;
begin
  Result := FileSeek(FHandle, Offset, Origin);
end;

procedure THandleStreamEx.SetSize(NewSize: Int64);
begin
  Seek(NewSize, soFromBeginning);
  Win32Check(SetEndOfFile(FHandle));
end;

constructor TFileStreamEx.Create(const FileName: string; Mode: Word);
begin
  if Mode = fmCreate then
  begin
    FHandle := FileCreate(FileName);
    if FHandle < 0 then
  end
  else
  begin
    FHandle := FileOpen(FileName, Mode);
    if FHandle < 0 then
  end;
end;

destructor TFileStreamEx.Destroy;
begin
  if FHandle >= 0 then FileClose(FHandle);
end;

end.
