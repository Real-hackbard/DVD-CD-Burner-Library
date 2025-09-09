unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,CDBurners,SCSITypes,CovertFuncs, StdCtrls, ComCtrls, CDSizer,
  Menus, ExtCtrls, ToolWin,scsidefs,BurnUnit, ImgList;

type
  TForm1 = class(TForm)
    Button1: TButton;
    PageControl1: TPageControl;
    TabSheet6: TTabSheet;
    ReadTocListView: TListView;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    ToolButton5: TToolButton;
    Panel1: TPanel;
    CDSize1: TCDSize;
    MainMenu1: TMainMenu;
    File1: TMenuItem;
    Options1: TMenuItem;
    Functions1: TMenuItem;
    CDCapabilities1: TMenuItem;
    ShowWriterParameters1: TMenuItem;
    SaveCDToISOImage1: TMenuItem;
    N1: TMenuItem;
    BurnISOToCD1: TMenuItem;
    BurnWaveFileToCD1: TMenuItem;
    N2: TMenuItem;
    FormatCD1: TMenuItem;
    BlankCD1: TMenuItem;
    StatusBar2: TStatusBar;
    SaveDialog1: TSaveDialog;
    OpenDialog1: TOpenDialog;
    ViewTOC1: TMenuItem;
    N3: TMenuItem;
    SetWriteModetoCDDA1: TMenuItem;
    ImageList1: TImageList;
    ToolButton7: TToolButton;
    ISOFunctions1: TMenuItem;
    CreateISO9660File1: TMenuItem;
    ShowReadWriteSpeeds1: TMenuItem;
    GetCDDVDStructure1: TMenuItem;
    GetBufferBits1: TMenuItem;
    SetWriteModetoDataWrite1: TMenuItem;
    N4: TMenuItem;
    N5: TMenuItem;
    ToolButton8: TToolButton;
    ToolBar2: TToolBar;
    DriveCombo: TComboBox;
    Label1: TLabel;
    ToolButton9: TToolButton;
    AddTrackToTrackList1: TMenuItem;
    ToolButton6: TToolButton;
    TabSheet1: TTabSheet;
    TrackListBox: TListBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CDCapabilities1Click(Sender: TObject);
    procedure ShowWriterParameters1Click(Sender: TObject);
    procedure SaveCDToISOImage1Click(Sender: TObject);
    procedure ViewTOC1Click(Sender: TObject);
    procedure SetWriteModetoCDDA1Click(Sender: TObject);
    procedure BurnWaveFileToCD1Click(Sender: TObject);
    procedure BurnISOToCD1Click(Sender: TObject);
    procedure CDSize1OverBurn(Sender: TObject);
    procedure BlankCD1Click(Sender: TObject);
    procedure ShowReadWriteSpeeds1Click(Sender: TObject);
    procedure GetCDDVDStructure1Click(Sender: TObject);
    procedure GetBufferBits1Click(Sender: TObject);
    procedure SetWriteModetoDataWrite1Click(Sender: TObject);
    procedure AddTrackToTrackList1Click(Sender: TObject);
  private
    { Private declarations }
    Procedure ShowLastError;
    Procedure RefreshTrackList;
  public
    { Public declarations }
    CDBurnerList : TCDBurnerList;
    CurrentISOFilename :String;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
var
     Index : Integer;
begin
    CDBurnerList := TCDBurnerList.create;
   for Index := 0 to CDBurnerList.Count -1 do
   begin
       DriveCombo.Items.Add(CDBurnerList.Burner[Index].BurnerInfo.VendorName);
   end;
   DriveCombo.ItemIndex := 0;
end;



procedure TForm1.FormDestroy(Sender: TObject);
begin
   CDBurnerList.Free;
end;



procedure TForm1.CDCapabilities1Click(Sender: TObject);
var
    CapString : String;
begin
    CapString := CDBurnerList.Burner[DriveCombo.ItemIndex].GetCapabilityText.Text;
    Showmessage(CapString);
end;



procedure TForm1.ShowWriterParameters1Click(Sender: TObject);
Var
   TestString : String;
begin
  TestString := CDBurnerList.Burner[DriveCombo.ItemIndex].GetWriteParameters;
  Showmessage(TestString);
  ShowLastError;
end;



Procedure TForm1.ShowLastError;
Begin
      Statusbar2.Panels[0].Text := ScsiErrToString(CDBurnerList.Burner[DriveCombo.ItemIndex].LastError);
      Statusbar2.Refresh;
End;



procedure TForm1.SaveCDToISOImage1Click(Sender: TObject);
Var
   Filename: String;
Begin
   If Savedialog1.Execute Then
   Begin
      Filename := Savedialog1.FileName;
      BurnForm.CDBurner := CDBurnerList.Burner[DriveCombo.ItemIndex];
      BurnForm.Show;
      BurnForm.DumpISOFile(FileName);
   end;
   ShowLastError;
End;



procedure TForm1.ViewTOC1Click(Sender: TObject);

Var TOC: TScsiTOC;
   i: integer;
   FirstBlock ,LastBlock : Integer;
Begin
   TOC :=  CDBurnerList.Burner[DriveCombo.ItemIndex].TOC;
   ReadTocListView.Items.Clear;
   FirstBlock := 0;
   LastBlock := TOC.Tracks[TOC.TrackCount - 1].AbsAddress;
   For i := 0 To TOC.TrackCount - 1 Do
      With ReadTocListView.Items.Add Do
      Begin
         Caption := IntToStr(i);
         Subitems.Add(IntToStr(TOC.Tracks[i].TrackNumber));
         Subitems.Add(IntToStr(TOC.Tracks[i].AbsAddress));
         Subitems.Add(SetToStr(TypeInfo(TScsiSubQinfoFlags), TOC.Tracks[i].Flags));
      End;
End;


procedure TForm1.SetWriteModetoCDDA1Click(Sender: TObject);
begin
  CDBurnerList.Burner[DriveCombo.ItemIndex].SetWriteMode(TRACK_AT_ONCE,RAW_DATA_BLOCK,16,CDROM_CDDA,0,150,False,True);
  ShowLastError;
end;


procedure TForm1.BurnWaveFileToCD1Click(Sender: TObject);
Var
  TrackID : integer;
begin
    TrackID := 0;
    BurnForm.CDBurner := CDBurnerList.Burner[DriveCombo.ItemIndex];
    BurnForm.Show;
    for TrackID := 0 to CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Count -1 do
    begin
       CDSize1.MemShaded := (CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Tracks[TrackID].CDTrack.DataSize div (1024*1024));
       if TrackID = CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Count -1 then
       BurnForm.StartAudioWrite(TrackID,True)
        else
           BurnForm.StartAudioWrite(TrackID,False);
    end;
end;



procedure TForm1.BurnISOToCD1Click(Sender: TObject);
Var
   filesize :integer;
begin
 if opendialog1.Execute then
 begin
    CurrentISOFilename := opendialog1.filename;
    filesize := (GetFileSize(CurrentISOFilename) div (1024*1024));
    CDSize1.MemShaded := filesize;
    BurnForm.CDBurner := CDBurnerList.Burner[DriveCombo.ItemIndex];
    BurnForm.Show;
    BurnForm.StartDataWrite(CurrentISOFilename);
 end;
end;


procedure TForm1.CDSize1OverBurn(Sender: TObject);
begin
   showmessage('Size of data is too big!');
end;


procedure TForm1.BlankCD1Click(Sender: TObject);
begin
    BurnForm.CDBurner := CDBurnerList.Burner[DriveCombo.ItemIndex];
    BurnForm.Show;
    BurnForm.BlankThisCD;
end;

procedure TForm1.ShowReadWriteSpeeds1Click(Sender: TObject);
var
    CapString : String;
begin
    CapString := CDBurnerList.Burner[DriveCombo.ItemIndex].GetCDSpeedsText.Text;
    Showmessage(CapString);
end;

procedure TForm1.GetCDDVDStructure1Click(Sender: TObject);
var
    Desc : TScsiDVDLayerDescriptorInfo;
    CapString : String;
begin
    Desc := CDBurnerList.Burner[DriveCombo.ItemIndex].DVDescriptor;
    CapString := 'Book Type     : '+Desc.BookType + #10#13;
    CapString := CapString + 'Track Density : '+Desc.TrackDensity + #10#13;
    CapString := CapString + 'Disc Size     : '+Desc.DiscSize + #10#13;
    CapString := CapString + 'Maximum Rate  : '+Desc.MaximumRate + #10#13;
    CapString := CapString + 'Linear Density: '+Desc.LinearDensity + #10#13;
    CapString := CapString + 'Layer Type    : '+Desc.LayerType + #10#13;
    Showmessage(CapString);
end;


procedure TForm1.GetBufferBits1Click(Sender: TObject);
begin
  CDBurnerList.Burner[DriveCombo.ItemIndex].CDBufferCapacity;
end;

procedure TForm1.SetWriteModetoDataWrite1Click(Sender: TObject);
begin
  CDBurnerList.Burner[DriveCombo.ItemIndex].SetWriteMode(TRACK_AT_ONCE,MODE_1,CDR_MODE_DATA,CDROM_CDDA,0,0,False,True);
  ShowLastError;
end;


Procedure TForm1.RefreshTrackList;
Var
 Index : Integer;
 name,Disp : String;
begin
  TrackListBox.Items.Clear;
  For Index := 0 to CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Count -1 do
  begin
    name :=  CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Tracks[index].CDTrack.TrackName;
    Disp :=  CDBurnerList.Burner[DriveCombo.ItemIndex].CDTracks.Tracks[index].DisplayName;
    TrackListBox.Items.Add(name + ' : '+ Disp);
  end;
end;


procedure TForm1.AddTrackToTrackList1Click(Sender: TObject);
begin
   if Opendialog1.Execute then
   begin
     CDBurnerList.Burner[DriveCombo.ItemIndex].AddAudioTrack(OpenDialog1.FileName);
     RefreshTrackList;
   end;
end;

end.
