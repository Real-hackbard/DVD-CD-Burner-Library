unit BurnUnit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, Gauges,cdburner, ComCtrls;

type
  TBurnForm = class(TForm)
    ListBox1: TListBox;
    Gauge1: TGauge;
    Label1: TLabel;
    Gauge2: TGauge;
    Label2: TLabel;

    StatusBar1: TStatusBar;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    procedure copystats(CurrentSector,PercentDone : Integer) ;
    Procedure CDStatus(CurrentStatus:String);
    procedure BufferProgress(Percent : Integer);
    procedure BufferStatus(BufferSize , FreeSize : Integer);
  private
    { Private declarations }
  public
    { Public declarations }
    CDBurner : TCDBurner;
    Procedure StartAudioWrite(TrackID : Integer; CloseSession : Boolean);
    Procedure StartDataWrite(FileName : String);
    Procedure DumpISOFile(FileName : String);
    Procedure BlankThisCD;
  end;

var
  BurnForm: TBurnForm;

implementation

{$R *.dfm}



Procedure TBurnform.StartAudioWrite(TrackID : Integer; CloseSession : Boolean);
begin
    CDBurner.OnCopyStatus := copystats;
    CDBurner.OnCDStatus := CDStatus;
    CDBurner.OnBufferProgress := BufferProgress;
    CDBurner.OnBufferStatus := BufferStatus;
    CDBurner.WriteAudioTrack(TrackID,CloseSession);
    if CloseSession = true then Showmessage('Audio Tracks Finished!');
end;



Procedure TBurnform.StartDataWrite(FileName : String);
begin
    CDBurner.OnCopyStatus := copystats;
    CDBurner.OnCDStatus := CDStatus;
    CDBurner.OnBufferProgress := BufferProgress;
    CDBurner.OnBufferStatus := BufferStatus;
    CDBurner.WriteISOToCD(FileName,True);
    Showmessage('Writing Finished!');
end;


Procedure TBurnform.DumpISOFile(FileName : String);
begin
    CDBurner.OnCopyStatus := copystats;
    CDBurner.OnCDStatus := CDStatus;
    CDBurner.OnBufferProgress := BufferProgress;
    CDBurner.OnBufferStatus := BufferStatus;
    CDBurner.CopyDiskToISOFile(FileName);
    Showmessage('Copy Finished!');
end;



procedure TBurnForm.copystats(CurrentSector, PercentDone : Integer) ;
begin
  gauge2.progress := percentdone;
  statusbar1.simpletext := 'Sector : '+ inttostr(CurrentSector);
  Refresh;
end;


procedure TBurnForm.BufferProgress(Percent : Integer);
begin
    gauge1.progress := Percent;
end;


procedure TBurnForm.BufferStatus(BufferSize , FreeSize : Integer);
begin
   Label3.caption := 'Burner Buffer Size : '+ inttostr(buffersize div 1024)+ ' kb';
   Label4.caption := 'Burner Free Buffer : '+ inttostr(FreeSize div 1024)+ ' kb';
end;



Procedure TBurnForm.CDStatus(CurrentStatus:String);
begin
   ListBox1.items.Insert(0,CurrentStatus);
end;


Procedure TBurnForm.BlankThisCD;
var
     blanktype : byte;
begin
    blanktype := $00;
    CDBurner.OnCopyStatus := copystats;
    CDBurner.OnCDStatus := CDStatus;
    CDBurner.OnBufferProgress := BufferProgress;
    CDBurner.OnBufferStatus := BufferStatus;
    CDBurner.BlankCDRom(blanktype,0);
    Showmessage('Blanking Finished!');
end;

end.
