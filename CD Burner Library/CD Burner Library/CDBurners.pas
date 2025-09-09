
unit CDBurners;

interface

Uses Windows,sysutils,SCSIUnit,SCSITypes,CDROMIOCTL,scsidefs,Classes,CDBurner,BurnerConsts;



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

Type
  TSPTIWriters = record
    ActiveCdRom : Byte;
    CdRomCount : Byte;
    CdRom : array[0..25] of TSPTIWriter;
  end;

Type
  SCSI_ADDRESS = record
    Length : LongInt;
    PortNumber : Byte;
    PathId : Byte;
    TargetId : Byte;
    Lun : Byte;
  end;
  PSCSI_ADDRESS = ^SCSI_ADDRESS;




Type
  TCDBurnerList = class
  private
    FBurners    : TStringList;
    function GetCount : integer;
    function GetBurner(index : integer) : TCDBurner;
    function GetBurnerInfo(index : integer) : TCDBurnerInfo;
    procedure DeleteBurner(Burner: TCDBurner);
    function AddCDDrives : boolean;
  public
    SPTICDs : TSPTIWriters;
    constructor Create;
    destructor  Destroy;  override;
    property Count : integer read GetCount;
    property Burner[index : integer] : TCDBurner  read GetBurner;
    property BurnerInfo[index : integer] : TCDBurnerInfo  read GetBurnerInfo;
  end;





implementation

Uses CovertFuncs;



Function GatherDeviceID(Adapter, Target, Lun: byte; Letter: char): TBurnerID;
Begin
   Result := GatherDWORD(Adapter, Target,
      ((Lun And 7) Shl 5) Or (ORD(Letter) And $1F), 0);
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


{*************************end of find cd rom functions **************************}





procedure TCDBurnerList.DeleteBurner(Burner: TCDBurner);
var Index : integer;
begin
   Index := FBurners.IndexOfObject(Burner);
   if Index >= 0 then FBurners.Objects[Index] := nil;
end;



function TCDBurnerList.GetCount : integer;
begin
   Result := FBurners.Count;
end;



function TCDBurnerList.GetBurner(index : integer) : TCDBurner;
begin
   if (Index < FBurners.count) then
   begin
      Result := TCDBurner(FBurners.Objects[index]);
   end
    else Result := nil;
end;




function TCDBurnerList.GetBurnerInfo(index : integer) : TCDBurnerInfo;
var
    BurnerInfo : TCDBurnerInfo;
begin
   if (Index < FBurners.count) then
   begin
      BurnerInfo := TCDBurner(FBurners.Objects[index]).BurnerInfo;
      Result := BurnerInfo;
    end;
end;




function TCDBurnerList.AddCDDrives : boolean;
var
    CDBurnerInfo : TCDBurnerInfo;
    CDBurner : TCDBurner;
    DevID : TBurnerID;
    index :integer;
    SDInfo : TScsiDeviceInfo;

begin
  GetSPTICdRomDrives(SPTICDs);
  for index := 0 to SPTICDs.CdRomCount - 1 do
  begin
     CDBurnerInfo.VendorSpec := SPTICDs.CdRom[index].VendorSpec;
     CDBurnerInfo.Revision := SPTICDs.CdRom[index].Revision;
     CDBurnerInfo.VendorID  := SPTICDs.CdRom[index].Vendor;
     CDBurnerInfo.ProductID := SPTICDs.CdRom[index].ProductId;
     CDBurnerInfo.VendorName := CDBurnerInfo.VendorID + ' ' + CDBurnerInfo.ProductID; 
     CDBurnerInfo.DriveLetter := SPTICDs.CdRom[index].DriveLetter;
     DevID := GatherDeviceID(SPTICDs.CdRom[index].HaId, SPTICDs.CdRom[index].Target, SPTICDs.CdRom[index].Lun, CDBurnerInfo.DriveLetter);
     CDBurnerInfo.DriveID := DevID;
     CDBurnerInfo.Lun := SPTICDs.CdRom[index].Lun;
     CDBurnerInfo.HaId := SPTICDs.CdRom[index].HaId;
     CDBurnerInfo.Target := SPTICDs.CdRom[index].Target;
     CDBurnerInfo.DriveIndex := index;
     CDBurnerInfo.SptiHandle := SPTICDs.CdRom[index].DriveHandle;
     CDBurner := TCDBurner.Create(CDBurnerInfo);   // create new burner
     FBurners.AddObject(CDBurner.BurnerInfo.VendorID,TObject(CDBurner));
   end;
   Result := True;
end;




constructor TCDBurnerList.Create;
begin
   inherited;
   FBurners := TStringList.Create;
   AddCDDrives;
end;



destructor TCDBurnerList.Destroy;
var Index : integer;
begin
   for Index := 0 to FBurners.Count-1 do Burner[Index].Free;
   inherited;
end;







end.
