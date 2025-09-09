unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ImgList, XPMan, ComCtrls, StdCtrls, ExtCtrls, Menus, CDTools,
  CDToolComponent, Buttons, ShellAPI;

type
  TForm1 = class(TForm)
    ImageList1: TImageList;
    Panel1: TPanel;
    Panel2: TPanel;
    ComboBox1: TComboBox;
    Panel3: TPanel;
    ListView1: TListView;
    Timer1: TTimer;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    CDToolBurner1: TCDToolBurner;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SpeedButton1Click(Sender: TObject);
    procedure ComboBox1Select(Sender: TObject);
    procedure CDToolBurner1EraseDone(Sender: TObject; WithError: Boolean);
    function  BurnCD: boolean;
    procedure WMDROPFILES (var Msg: TMessage); message WM_DROPFILES;
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
    procedure CDToolBurner1WriteDone(Sender: TObject; Error: String);
    procedure N2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses EraseFrm, Options, WriteFrm;

{$R *.dfm}

procedure TForm1.WMDROPFILES (var Msg: TMessage);  
var  
  i,
  amount,  
  size: integer;  
  Filename: PChar;  
begin  
  inherited;
  Amount := DragQueryFile(Msg.WParam, $FFFFFFFF, Filename, 255);
  for i := 0 to (Amount - 1) do
  begin
    size := DragQueryFile(Msg.WParam, i , nil, 0) + 1;  
    Filename:= StrAlloc(size);  
    DragQueryFile(Msg.WParam,i , Filename, size);
    With ListView1.Items.Add do begin

    if DirectoryExists(StrPas(Filename)) then begin
    Caption := (StrPas(Filename)+'\');
    CDToolBurner1.InsertDir('\',StrPas(Filename));
    ImageIndex := 0;
    end
    else begin
    CDToolBurner1.InsertFile('\',StrPas(Filename));
    Caption := (StrPas(Filename));
    ImageIndex := 1;
    end;

    end;
    StrDispose(Filename);  
  end;  
  DragFinish(Msg.WParam);  
end;

function TForm1.BurnCD: boolean;
begin
  result := false;
  CDToolBurner1.FinalizeDisc := Form3.CheckBox1.Checked;
  CDToolBurner1.WriteSpeed := 0;
  CDToolBurner1.IdVolume := Form3.Edit1.Text;
  Application.ProcessMessages;
  if (CDToolBurner1.DirsCount = 0) and (CDToolBurner1.FilesCount = 0) then exit;
  CDToolBurner1.Prepare;
  if CDToolBurner1.FreeBlocksOnDisc < CDToolBurner1.ImageSize then
  begin

    exit;
  end;

  if CDToolBurner1.BurnCD then
    Form4.ProgressBar1.Max := CDToolBurner1.ImageSize;
  result := true;

  Form4.ShowModal;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
DragAcceptFiles(Form1.Handle, true);

CDToolBurner1.InitializeASPI(True);
ComboBox1.Items := CDToolBurner1.Devices;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
CDToolBurner1.DeInitializeASPI;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
var
  str: String;
  Speed: Word;
begin
  if not (CDToolBurner1.Erasable)and (CDToolBurner1.DiscType <> mtDVD_RAM) then
    ShowMessage('Error!')
  else
  if Application.MessageBox('DVD Format ?','Confirm',
                            MB_DEFBUTTON2+MB_ICONWARNING+MB_YESNO) = ID_YES then
  begin
    CDToolBurner1.WriteSpeed := 1764;
    CDToolBurner1.EraseDisc(etQuick);
    Form2.ShowModal;
  end;

end;

procedure TForm1.ComboBox1Select(Sender: TObject);
begin
CDToolBurner1.Device := ComboBox1.Text;
end;

procedure TForm1.CDToolBurner1EraseDone(Sender: TObject;
  WithError: Boolean);
begin
Form2.Close;
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
Form4.ProgressBar1.Position := CDToolBurner1.BytesWritten div 2048;
Form4.ProgressBar2.Position := CDToolBurner1.BufferProgress;
end;

procedure TForm1.SpeedButton2Click(Sender: TObject);
begin
Form3.ShowModal;
BurnCD;
end;

procedure TForm1.CDToolBurner1WriteDone(Sender: TObject; Error: String);
begin
Timer1.Enabled := false;
CDToolBurner1.WaitForReady;
form4.close;
ShowMessage('Прожиг успешно завершен.');
if Form3.CheckBox1.Checked then
CDToolBurner1.LoadMedium(True);
end;

procedure TForm1.N2Click(Sender: TObject);
begin
Form3.ShowModal;
end;

end.
