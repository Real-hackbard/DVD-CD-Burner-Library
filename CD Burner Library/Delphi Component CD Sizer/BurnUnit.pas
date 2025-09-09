

unit BurnUnit;

interface

uses WinTypes, WinProcs, Messages,dialogs, SysUtils, Classes, Controls, 
     Forms, Graphics,AspiFunctions,scsidefs,wnaspi32,CDROM,CovertFuncs;



type
  TCopyStatusEvent = procedure (CurrentSector,PercentDone : Integer) of object;
  TCDStatusEvent = procedure (CurrentStatus:String) of object;





//  TBlankType = (BLANK_DISC,BLANK_MINIMAL,BLANK_TRACK,UN_RESERVE_TRACK,
//                 BLANK_TRACK_TAIL, UNCLOSE_LAST_SESSION, ERASE_SESSION);

//  TSessionType = (CDROM_CDDA,CDI_DISK,CDROM_XA);

//  TDataBlockType = (MODE_1,MODE_2,RAW_DATA_BLOCK,RAW_DATA_P_Q_SUB,RAW_DATA_P_W_SUB,RAW_DATA_P_W_SUB2,
//  MODE_2_XA_FORM_1,MODE_2_XA_FORM_1_SUB,MODE_2_XA_FORM_2,MODE_2_XA_FORM_2_SUB);

  TCDSpeedType = (SCDS_MAXSPEED,SCDS_NONE);

  TWriteType = (PACKET,TAO,SAO,RAW);


type
  TCDBurner = class(TComponent)
    private
      { Private fields of TCDBurner }

//         FBlankTypes: TBlankType;
//         FSessionType : TSessionType;
//         FDataBlockType : TDataBlockType;
         FCDSpeedType : TCDSpeedType;
         FWriteType : TWriteType;

        { Storage for property Abstract }
        FAbstract : String;
        { Storage for property Bibliography }
        FBibliography : String;
        { Storage for property BlankType }
        FBlankType : Integer;
        { Storage for property Copyright }
        FCopyright : String;
        { Storage for property EjectAfterWrite }
        FEjectAfterWrite : Boolean;
        { Storage for property ISOFilename }
        FISOFilename : String;
        FISOFileSize : Integer;
        { Storage for property Joliet }
        FJoliet : Boolean;
        { Storage for property LUN }
        FLUN : Integer;
        { Storage for property PreparedBy }
        FPreparedBy : String;
        { Storage for property PublisherName }
        FPublisherName : String;
        { Storage for property SCSIController }
        FSCSIController : Integer;
        { Storage for property ShowMessages }
        FShowMessages : Boolean;
        { Storage for property Target }
        FTarget : Integer;
        { Storage for property AudioPause }
        FAudioPauseLength : Integer;
        { Storage for property TestMode }
        FTestMode : Boolean;
        { Storage for property VolumeLabel }
        FVolumeLabel : String;

        FCapable : String;

        ASPILayerOK : Boolean;
        AdaptorList : TStringList;
        ISOFileList : TStringList;
        WaveCueList : TStringList;
        FWaveCueFileSize : Integer;
        fDrives: TCDDrives;
        fDrive: TCDDrive;
        fCursor: TCursor;
        FOnCopyStatus : TCopyStatusEvent;
        FOnCDStatus : TCDStatusEvent;
        FCloseSession : Boolean;

      { Private methods of TCDBurner }
        { Method to set variable and property values and create objects }
        procedure AutoInitialize;
        { Method to free any objects created by AutoInitialize }
        procedure AutoDestroy;
        { Read method for property Abstract }
        function GetAbstract : String;
        { Write method for property Abstract }
        procedure SetAbstract(Value : String);
        { Read method for property Bibliography }
        function GetBibliography : String;
        { Write method for property Bibliography }
        procedure SetBibliography(Value : String);
        { Read method for property BlankType }
        function GetBlankType : Integer;
        { Write method for property BlankType }
        procedure SetBlankType(Value : Integer);
        { Read method for property Copyright }
        function GetCopyright : String;
        { Write method for property Copyright }
        procedure SetCopyright(Value : String);
        { Read method for property EjectAfterWrite }
        function GetEjectAfterWrite : Boolean;
        { Write method for property EjectAfterWrite }
        procedure SetEjectAfterWrite(Value : Boolean);
        { Read method for property ISOFilename }
        function GetISOFilename : String;
        { Write method for property ISOFilename }
        procedure SetISOFilename(Value : String);
        { Read method for property Joliet }
        function GetJoliet : Boolean;
        { Write method for property Joliet }
        procedure SetJoliet(Value : Boolean);
        { Read method for property LUN }
        function GetLUN : Integer;
        { Write method for property LUN }
        procedure SetLUN(Value : Integer);
        { Read method for property PreparedBy }
        function GetPreparedBy : String;
        { Write method for property PreparedBy }
        procedure SetPreparedBy(Value : String);
        { Read method for property PublisherName }
        function GetPublisherName : String;
        { Write method for property PublisherName }
        procedure SetPublisherName(Value : String);
        { Read method for property SCSIController }
        function GetSCSIController : Integer;
        { Write method for property SCSIController }
        procedure SetSCSIController(Value : Integer);
        { Read method for property ShowMessages }
        function GetShowMessages : Boolean;
        { Write method for property ShowMessages }
        procedure SetShowMessages(Value : Boolean);
        { Read method for property Target }
        function GetTarget : Integer;
        { Write method for property Target }
        procedure SetTarget(Value : Integer);

        { Read method for property Audio }
        function GetAudioPauseLength : Integer;
        { Write method for property Audio }
        procedure SetAudioPauseLength(Value : Integer);

        { Read method for property TestMode }
        function GetTestMode : Boolean;
        { Write method for property TestMode }
        procedure SetTestMode(Value : Boolean);
        { Read method for property VolumeLabel }
        function GetVolumeLabel : String;
        { Write method for property VolumeLabel }
        procedure SetVolumeLabel(Value : String);
        procedure SetWaveCueSize;

    protected
      { Protected fields of TCDBurner }
        CurrentErrorCode : String;
        CurrentISOFilename : String;
        FirstBlock: Integer;
        LastBlock: Integer;

      { Protected methods of TCDBurner }
        procedure Loaded; override;

    public
      { Public fields and properties of TCDBurner }

      { Public methods of TCDBurner }

        procedure AddFileToISO(Filename:String);
        procedure AddPathToISO(Path:String;Subs:Boolean);
        function AddWaveToCue(FileName :String) : Boolean;
        function BlankDisk : Boolean;
        procedure ClearISOCue;
        procedure ClearWaveCue;
        function CreateISOFile(Filename:string) : Boolean;
        function CreateMultiSessionISO(Filename :String) : Boolean;
        constructor Create(AOwner: TComponent); override;
        destructor Destroy; override;
        function Execute : Boolean;
        function GetHostAdaptorName(ID: Integer) : String;
        function GetNumOfAdaptors : Integer;
        function IsTaskRunning :Boolean;
        function IsWaveInCue(Filename : String) :Boolean;
        procedure ResetDrive;
        function GetSCSIManager:string;
        procedure ScanSCSIBus(ID : integer);
        function WriteISOToCDR :Boolean;
        function WriteWaveToCDR : Boolean;
        function WriteCDRToISO(FileName : String) :Boolean;
        function GetLastError:string;
        function GetSectorCount:Integer;
        Function SelectAdaptor(ID:Integer):boolean;
        function GetAdaptorString(ID: Integer) : String;
        function IsCDReady:Boolean;
        Function SaveWaveCue(Filename : String) :Boolean;
        Function LoadWaveCue(Filename : String) :Boolean;
        Function ResetWriteParameters : Boolean;

    published
      { Published properties of TCDBurner }
        property OnCopyStatus : TCopyStatusEvent read FOnCopyStatus write FOnCopyStatus;
        property OnCDStatus : TCDStatusEvent read FOnCDStatus write FOnCDStatus;

        property Abstract : String read GetAbstract write SetAbstract;
        property Bibliography : String
             read GetBibliography write SetBibliography;
        property Copyright : String read GetCopyright write SetCopyright;
        property EjectAfterWrite : Boolean
             read GetEjectAfterWrite write SetEjectAfterWrite;
        { Name Of ISO File }
        property ISOFilename : String read GetISOFilename write SetISOFilename;
        property Joliet : Boolean read GetJoliet write SetJoliet;
        property LUN : Integer read GetLUN write SetLUN default 0;
        property PreparedBy : String read GetPreparedBy write SetPreparedBy;
        property PublisherName : String
             read GetPublisherName write SetPublisherName;
        property SCSIController : Integer
             read GetSCSIController write SetSCSIController
             default 0;
        property ShowMessages : Boolean
             read GetShowMessages write SetShowMessages;
        property Target : Integer read GetTarget write SetTarget default 0;
        property AudioPauseLength : Integer read GetAudioPauseLength write SetAudioPauseLength default 0;
        property TestMode : Boolean
             read GetTestMode write SetTestMode
             default False;
        property VolumeLabel : String read GetVolumeLabel write SetVolumeLabel;
        property Capablities : String read FCapable write FCapable;
        property ISOFileSize : Integer read FISOFileSize;
        property CloseSession : Boolean read FCloseSession write FCloseSession;
        property WaveCueFileSize : Integer read FWaveCueFileSize;
//        property CDBlankType : TBlankType read FBlankTypes write FBlankTypes;
//        Property SessionType : TSessionType read FSessionType write FSessionType;
//        Property DataBlockType : TDataBlockType read FDataBlockType write FDataBlockType;
        Property CDSpeed : TCDSpeedType read FCDSpeedType write FCDSpeedType;
        Property WriteType : TWriteType read FWriteType write FWriteType;

  end;

procedure Register;

implementation


procedure Register;
begin
     { Register TCDBurner with Additional as its
       default page on the Delphi component palette }
     RegisterComponents('Additional', [TCDBurner]);
end;

{ Method to set variable and property values and create objects }
procedure TCDBurner.AutoInitialize;
begin
     AdaptorList := TStringList.Create;
     ISOFileList := TStringList.Create;
     WaveCueList := TStringList.Create;
     FBlankType := 0;
     FLUN := 0;
     FSCSIController := 0;
     FTarget := 0;
     FWaveCueFileSize := 0;
     FISOFileSize := 0;
     FTestMode := False;
     ASPILayerOK := True;
     fDrives := TCDDrives.Create;
end; { of AutoInitialize }

{ Method to free any objects created by AutoInitialize }
procedure TCDBurner.AutoDestroy;
begin
     AdaptorList.Free;
     ISOFileList.Free;
     WaveCueList.Free;
     fDrives.Free;
end; { of AutoDestroy }

{ Read method for property Abstract }
function TCDBurner.GetAbstract : String;
begin
     Result := FAbstract;
end;

{ Write method for property Abstract }
procedure TCDBurner.SetAbstract(Value : String);
begin
     FAbstract := Value;
end;

{ Read method for property Bibliography }
function TCDBurner.GetBibliography : String;
begin
     Result := FBibliography;
end;

{ Write method for property Bibliography }
procedure TCDBurner.SetBibliography(Value : String);
begin
     FBibliography := Value;
end;

{ Read method for property BlankType }
function TCDBurner.GetBlankType : Integer;
begin
     Result := FBlankType;
end;

{ Write method for property BlankType }
procedure TCDBurner.SetBlankType(Value : Integer);
begin
     FBlankType := Value;
end;

{ Read method for property Copyright }
function TCDBurner.GetCopyright : String;
begin
     Result := FCopyright;
end;

{ Write method for property Copyright }
procedure TCDBurner.SetCopyright(Value : String);
begin
     FCopyright := Value;
end;

{ Read method for property EjectAfterWrite }
function TCDBurner.GetEjectAfterWrite : Boolean;
begin
     Result := FEjectAfterWrite;
end;

{ Write method for property EjectAfterWrite }
procedure TCDBurner.SetEjectAfterWrite(Value : Boolean);
begin
     FEjectAfterWrite := Value;
end;

{ Read method for property ISOFilename }
function TCDBurner.GetISOFilename : String;
begin
     Result := FISOFilename;
end;

{ Write method for property ISOFilename }
procedure TCDBurner.SetISOFilename(Value : String);
begin
     FISOFilename := Value;
     FISOFileSize := (GetFileSize(FISOFilename) div (1024*1024));
end;

{ Read method for property Joliet }
function TCDBurner.GetJoliet : Boolean;
begin
     Result := FJoliet;
end;

{ Write method for property Joliet }
procedure TCDBurner.SetJoliet(Value : Boolean);
begin
     FJoliet := Value;
end;

{ Read method for property LUN }
function TCDBurner.GetLUN : Integer;
begin
     Result := FLUN;
end;

{ Write method for property LUN }
procedure TCDBurner.SetLUN(Value : Integer);
begin
     FLUN := Value;
end;

{ Read method for property PreparedBy }
function TCDBurner.GetPreparedBy : String;
begin
     Result := FPreparedBy;
end;

{ Write method for property PreparedBy }
procedure TCDBurner.SetPreparedBy(Value : String);
begin
     FPreparedBy := Value;
end;

{ Read method for property PublisherName }
function TCDBurner.GetPublisherName : String;
begin
     Result := FPublisherName;
end;

{ Write method for property PublisherName }
procedure TCDBurner.SetPublisherName(Value : String);
begin
     FPublisherName := Value;
end;

{ Read method for property SCSIController }
function TCDBurner.GetSCSIController : Integer;
begin
     Result := FSCSIController;
end;

{ Write method for property SCSIController }
procedure TCDBurner.SetSCSIController(Value : Integer);
begin
     FSCSIController := Value;
end;


{ Read method for property ShowMessages }
function TCDBurner.GetShowMessages : Boolean;
begin
     Result := FShowMessages;
end;

{ Write method for property ShowMessages }
procedure TCDBurner.SetShowMessages(Value : Boolean);
begin
     FShowMessages := Value;
end;

{ Read method for property Target }
function TCDBurner.GetTarget : Integer;
begin
     Result := FTarget;
end;

{ Write method for property Target }
procedure TCDBurner.SetTarget(Value : Integer);
begin
     FTarget := Value;
end;

{ Read method for property Audio }
function TCDBurner.GetAudioPauseLength : Integer;
begin
     Result := FAudioPauseLength;
end;

{ Write method for property Audio }
procedure TCDBurner.SetAudioPauseLength(Value : Integer);
begin
     FAudioPauseLength := Value;
end;

{ Read method for property TestMode }
function TCDBurner.GetTestMode : Boolean;
begin
     Result := FTestMode;
end;

{ Write method for property TestMode }
procedure TCDBurner.SetTestMode(Value : Boolean);
begin
     FTestMode := Value;
end;

{ Read method for property VolumeLabel }
function TCDBurner.GetVolumeLabel : String;
begin
     Result := FVolumeLabel;
end;

{ Write method for property VolumeLabel }
procedure TCDBurner.SetVolumeLabel(Value : String);
begin
     FVolumeLabel := Value;
end;


Function TCDBurner.ResetWriteParameters : Boolean;
begin
  result := false;
  If Assigned(fDrive) then
  begin
 //  result := fDrive.SetWriteMode(TRACK_AT_ONCE,MODE_1,4,CDROM_CDDA,0,0,0);
  end;
end;



procedure TCDBurner.AddFileToISO(Filename:String);
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

Function TCDBurner.SelectAdaptor(ID:Integer):boolean;
var
     Capstrings : tstringlist;
     aAdapter, aTarget, aLun : BYTE;
begin
   Result := False;
   fDrive := fDrives.Device[ID];
   If Assigned(fDrive) then
   begin
     ScatterDeviceID(fDrive.DeviceID, aAdapter, aTarget, aLun);
     Capstrings := fDrive.GetCapabilityStrings;
     FTarget := aTarget;
     FLun := aLun;
     FCapable := Capstrings.Text;
     Capstrings.Free;
     Result := true;
   end;
end;



function TCDBurner.GetSCSIManager:string;
begin
   If Assigned(fDrive) then Result := fDrive.HostInfo.ScsiManagerId;
end;

function TCDBurner.GetSectorCount:Integer;
begin
   If Assigned(fDrive) then Result := fDrive.Capacity;
end;


function TCDBurner.GetLastError:string;
begin
   If Assigned(fDrive) then ScsiErrToString(fDrive.LastError);
end;


function TCDBurner.IsCDReady:Boolean;
begin
   Result := False;
   If Assigned(fDrive) then Result := fDrive.IsReady;
end;


procedure TCDBurner.AddPathToISO(Path:String;Subs:Boolean);
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

procedure TCDBurner.SetWaveCueSize;
Var index : integer;
begin
for index := 0 to WaveCueList.Count -1 do
begin
    FWaveCueFileSize := FWaveCueFileSize + (GetFileSize(WaveCueList.Strings[index]) div (1024*1024));
end;
end;


function TCDBurner.AddWaveToCue(FileName :String) : Boolean;
begin
   WaveCueList.Add(FileName);
   SetWaveCueSize;
end;

function TCDBurner.BlankDisk : Boolean;
var
     blanktype : byte;
begin
  blanktype := $00;
{ case FBlanktypes of
 BLANK_DISC                :blanktype := $00;
 BLANK_MINIMAL             :blanktype := $01;
 BLANK_TRACK               :blanktype := $02;
 UN_RESERVE_TRACK          :blanktype := $03;
 BLANK_TRACK_TAIL          :blanktype := $04;
 UNCLOSE_LAST_SESSION      :blanktype := $05;
 ERASE_SESSION             :blanktype := $06;
 end;   }
    If Assigned(fDrive) then
    fDrive.BlankCDRom(blanktype,0);
end;


procedure TCDBurner.ClearISOCue;
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

procedure TCDBurner.ClearWaveCue;
begin
  WaveCueList.Clear;
  SetWaveCueSize;
end;


constructor TCDBurner.Create(AOwner: TComponent);
begin
     { Call the Create method of the parent class }
     inherited Create(AOwner);
     AutoInitialize;
     { Code to perform other tasks when the component is created }
     ASPILayerOK := CheckAspiLayer;
     If ASPILayerOK = false then showmessage('Win Aspi Layer Is Missing or Out of Date!');
end;

function TCDBurner.CreateISOFile(Filename:string) : Boolean;
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

function TCDBurner.CreateMultiSessionISO(Filename :String) : Boolean;
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

destructor TCDBurner.Destroy;
begin
     { AutoDestroy, which is generated by Component Create, frees any   }
     { objects created by AutoInitialize.                               }
     AutoDestroy;
     inherited Destroy;
end;

function TCDBurner.Execute : Boolean;
begin
     { Perform the component operation }

     { Return True if the operation was successful, False otherwise }
     Result := True
end;

function TCDBurner.GetHostAdaptorName(ID: Integer) : String;
begin
  Result := GetAdaptorName(ID);
end;

function TCDBurner.GetAdaptorString(ID: Integer) : String;
Var
        Name : String;
begin
  Name := fDrives.VendorID[ID] +' '+ fDrives.ProductID[ID];
  Result := Name;
end;

function TCDBurner.GetNumOfAdaptors : Integer;
begin
   Result := fDrives.Count;
end;

function TCDBurner.IsTaskRunning :Boolean;
begin
   Result := False;
   If Assigned(fDrive) then Result := Not fDrive.IsReady;
end;


function TCDBurner.IsWaveInCue(Filename : String) :Boolean;
begin
  Result := False;
  if (WaveCueList.IndexOf(FileName)> -1) Then Result := True;
end;


Function TCDBurner.SaveWaveCue(Filename : String) :Boolean;
begin
  Result := False;
  if filename <> '' then
  begin
     WaveCueList.SaveToFile(Filename);
     Result := True;
  end;
end;


Function TCDBurner.LoadWaveCue(Filename : String) :Boolean;
begin
  Result := False;
  if filename <> '' then
  begin
     WaveCueList.LoadFromFile(Filename);
     Result := True;
  end;
end;



procedure TCDBurner.Loaded;
begin
     inherited Loaded;
end;

procedure TCDBurner.ResetDrive;
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;

procedure TCDBurner.ScanSCSIBus(ID : integer);
     { Internal declarations for method }
     { type }
     { . . . }
     { var }
     { . . . }
begin

end;


function TCDBurner.WriteISOToCDR :Boolean;
begin
   if FISOFilename <> '' then
   if Assigned(fDrive) then
   begin
   If assigned(FOnCDStatus) then
       fDrive.OnCDStatus := FOnCDStatus;
   If assigned(FOnCopyStatus) then
       fDrive.OnCopyStatus := FOnCopyStatus;
    fDrive.WriteISOToCD(FISOFilename,FCloseSession);
//    if FEjectAfterWrite then fDrive.EjectMedium(false);
   end;
end;


function TCDBurner.WriteWaveToCDR : Boolean;
Var
      WaveIndex : integer;
      Wavefilename : String;
begin
     if Assigned(fDrive) then
     begin
       If assigned(FOnCopyStatus) then
       fDrive.OnCopyStatus := FOnCopyStatus;
       If assigned(FOnCDStatus) then
       fDrive.OnCDStatus := FOnCDStatus;

      for WaveIndex := 0  to WaveCueList.Count -1 do
      begin
         Wavefilename := WaveCueList.Strings[WaveIndex];
         if Fileexists(Wavefilename) then
          fDrive.WriteAudioTrack(Wavefilename,FCloseSession);
      end;
      if FEjectAfterWrite then fDrive.EjectMedium(false);
   end;
end;


function TCDBurner.WriteCDRToISO(FileName : String) :Boolean;
begin
     if Assigned(fDrive) then
     begin
       If assigned(FOnCopyStatus) then
       fDrive.OnCopyStatus := FOnCopyStatus;
       If assigned(FOnCDStatus) then
       fDrive.OnCDStatus := FOnCDStatus;
       fDrive.CopyDiskToISOFile(FileName);
     end;
end;


end.
