unit TestUnit;

interface

uses
  Windows, Messages, SysUtils,AudioUnit,WaveUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    OpenDialog1: TOpenDialog;
    Button2: TButton;
    SaveDialog1: TSaveDialog;
    TrackListBox: TListBox;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CDTracks : TCDTrackList;
    Procedure ListTracks;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}




Procedure TForm1.ListTracks;
Var
 Index : Integer;
begin
  TrackListBox.Items.Clear;
  For Index := 0 to CDTracks.Count -1 do
  begin
    TrackListBox.Items.Add(CDTracks.Tracks[index].CDTrack.TrackName + ' : '+ CDTracks.Tracks[index].DisplayName);
  end;
end;




procedure TForm1.Button1Click(Sender: TObject);
Var
    Track : TCDTrackItem;
begin
if Opendialog1.execute then
begin
   Track := CDTracks.Add;
   Track.LoadWaveFile(Opendialog1.FileName);
   Track.CDTrack.ConvertToPCM(Stereo16bit44100Hz);
   ListTracks;
end;
end;


procedure TForm1.FormCreate(Sender: TObject);
begin
   CDTracks := TCDTrackList.Create(self,TCDTrackItem);
end;


procedure TForm1.FormDestroy(Sender: TObject);
begin
   CDTracks.Free;
end;


procedure TForm1.Button2Click(Sender: TObject);
begin
if savedialog1.Execute then
begin
   CDTracks.Tracks[0].SaveWaveFile(Savedialog1.FileName);
end;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   ListTracks;
end;

end.
