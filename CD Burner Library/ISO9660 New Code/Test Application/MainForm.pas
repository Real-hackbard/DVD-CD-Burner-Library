unit MainForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, ComCtrls, StdCtrls, ISO9660ISOImageClass, ISO9660ImageTree,
  ImgList;

type
  TForm1 = class(TForm)
    StatusBar1: TStatusBar;
    MainMenu1: TMainMenu;
    mm_File: TMenuItem;
    sm_File_Open: TMenuItem;
    sm_File_Close: TMenuItem;
    sm_File_Break1: TMenuItem;
    sm_File_Quit: TMenuItem;
    dlg_OpenImage: TOpenDialog;
    SaveDialog1: TSaveDialog;
    sm_File_SaveAs: TMenuItem;
    New1: TMenuItem;
    Image1: TMenuItem;
    N1: TMenuItem;
    CheckDirs1: TMenuItem;
    ImageList1: TImageList;
    CreateTestImage1: TMenuItem;
    CreateTestImageAndSavetodisk1: TMenuItem;
    Panel1: TPanel;
    mem_DebugOut: TMemo;
    tv_Directory: TTreeView;
    Panel2: TPanel;
    VolIDEdit: TEdit;
    Label1: TLabel;
    PopupMenu1: TPopupMenu;
    CreateDirctory1: TMenuItem;
    N2: TMenuItem;
    DeleteDirectory1: TMenuItem;
    AddFile1: TMenuItem;
    OpenDialog2: TOpenDialog;
    procedure sm_File_QuitClick(Sender: TObject);
    procedure sm_File_OpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure tv_DirectoryDblClick(Sender: TObject);
    procedure sm_File_CloseClick(Sender: TObject);
    procedure tv_DirectoryChange(Sender: TObject; Node: TTreeNode);
    procedure Image1Click(Sender: TObject);
    procedure CheckDirs1Click(Sender: TObject);
    procedure sm_File_SaveAsClick(Sender: TObject);
    procedure CreateTestImageAndSavetodisk1Click(Sender: TObject);
    procedure CreateDirctory1Click(Sender: TObject);
    procedure AddFile1Click(Sender: TObject);
    procedure DeleteDirectory1Click(Sender: TObject);
  private
    { Private-Deklarationen }
    TreeObj : TObject;

    FISOImage  : TISOImage;

    Procedure  BuildStructureTree(ATV: TTreeView; RootNode : TTreeNode; ADirEntry : TDirectoryEntry);

  public
    { Public-Deklarationen }
    ISOFilename : String;
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}


procedure TForm1.sm_File_QuitClick(Sender: TObject);
begin
  Close;
end;


procedure TForm1.sm_File_OpenClick(Sender: TObject);
Var
  Node : TTreeNode;
begin
  If ( dlg_OpenImage.Execute ) Then
  Begin
    If ( Assigned(FISOImage) ) Then  FreeAndNil(FISOImage);

    mem_DebugOut.Clear;
    tv_Directory.Items.Clear;

    FISOImage := TISOImage.Create(dlg_OpenImage.FileName, mem_DebugOut.Lines);

    Try
      FISOImage.OpenImage;


      Node := tv_Directory.Items.Add(Nil, '/');
      Node.Data := fISOImage.Structure.RootDirectory;
      BuildStructureTree(tv_Directory, Node, FISOImage.Structure.RootDirectory);

      // sm_File_SaveAs.Enabled := True; not yet ready
      sm_File_Close.Enabled := True;

    Except
      mem_DebugOut.Lines.Add('Exception: ' + Exception(ExceptObject).ClassName + ' -> ' + Exception(ExceptObject).Message);
      Raise;

      fISOImage.CloseImage;
    End;
  End;
end;



procedure TForm1.FormCreate(Sender: TObject);
begin
  fISOImage := Nil;   // not necessary, but safety first...
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  If ( Assigned(fISOImage) ) Then FreeAndNil(fISOImage);
end;



procedure TForm1.tv_DirectoryDblClick(Sender: TObject);
Var
  Node : TTreeNode;
  Obj  : TObject;
begin
  Node := TTreeView(Sender).Selected;

  If Assigned(Node.Data) Then
  Begin
    Obj := TObject(Node.Data);
    If ( Obj Is TFileEntry ) And ( SaveDialog1.Execute ) Then
      fISOImage.ExtractFile(TFileEntry(Obj), SaveDialog1.FileName);
  End;
end;



Procedure TForm1.BuildStructureTree(ATV: TTreeView; RootNode : TTreeNode; ADirEntry : TDirectoryEntry);
Var
  i : Integer;
  Node : TTreeNode;
  Dir  : TDirectoryEntry;
  Fil  : TFileEntry;
Begin
  For i:=0 To ADirEntry.DirectoryCount-1 Do
  Begin
    Dir := ADirEntry.Directories[i];
    Node := ATV.Items.AddChild(RootNode, Dir.Name + '/');
    Node.ImageIndex := 1;
    Node.SelectedIndex := 1;
    Node.Data := Pointer(Dir);
    BuildStructureTree(ATV, Node, Dir);
  End;

  For i:=0 To ADirEntry.FileCount-1 Do
  Begin
    Fil := ADirEntry.Files[i];
    Node := ATV.Items.AddChild(RootNode, Fil.Name);
    Node.ImageIndex := 2;
    Node.SelectedIndex := 2;
    Node.Data := Pointer(Fil);
  End;
End;



procedure TForm1.sm_File_CloseClick(Sender: TObject);
begin
  If ( Assigned(fISOImage) ) Then fISOImage.CloseImage;

  sm_File_Close.Enabled  := False;
  sm_File_SaveAs.Enabled := False;
end;



procedure TForm1.tv_DirectoryChange(Sender: TObject; Node: TTreeNode);

begin
  If Assigned(Node) Then
  Begin
    TreeObj := TObject(Node.Data);
 End;
end;



procedure TForm1.Image1Click(Sender: TObject);

var
     DirEntry : TDirectoryEntry;
     FileEntry : TFileEntry;

begin
    If ( Assigned(FISOImage) ) Then FreeAndNil(FISOImage);
    if savedialog1.execute then
    begin
       ISOFilename := savedialog1.filename;
       FISOImage := TISOImage.Create(ISOFilename, mem_DebugOut.Lines);
       CheckDirs1Click(nil);
    end;
end;



procedure TForm1.CheckDirs1Click(Sender: TObject);
Var
  Node : TTreeNode;
begin
    tv_Directory.Items.Clear;
    Try
      Node := tv_Directory.Items.Add(Nil, '/');
      Node.ImageIndex := 0;
      Node.Data := FISOImage.Structure.RootDirectory;
      BuildStructureTree(tv_Directory, Node, fISOImage.Structure.RootDirectory);
      tv_Directory.Items[0].Expand(true);
    Except
      mem_DebugOut.Lines.Add('Exception: ' + Exception(ExceptObject).ClassName + ' -> ' + Exception(ExceptObject).Message);
      Raise;
    End;
end;



procedure TForm1.sm_File_SaveAsClick(Sender: TObject);
begin
  FISOImage.Volume_ID := VolIDEdit.text;
  FISOImage.SaveImageToDisk(1);
  ShowMessage('ISO Disk Image Saved to HD');
end;


procedure TForm1.CreateTestImageAndSavetodisk1Click(Sender: TObject);
var
     DirEntry : TDirectoryEntry;
     FileEntry : TFileEntry;

begin
   If ( Assigned(FISOImage) ) Then FreeAndNil(FISOImage);
   If Fileexists('C:\Temp.iso') then deletefile('C:\Temp.iso');
    FISOImage := TISOImage.Create('C:\Temp.iso', mem_DebugOut.Lines);
    Try
      DirEntry := TDirectoryEntry.Create(FISOImage.Structure,FISOImage.Structure.RootDirectory,dsfFromImage);
      DirEntry.Name := 'HELPME';

      FileEntry := TFileEntry.Create(DirEntry,dsfFromLocal);
      FileEntry.Name := 'SQLOutput.TXT';
      FileEntry.SourceFileName := 'C:\SQLOutput.TXT';

      DirEntry := TDirectoryEntry.Create(FISOImage.Structure,DirEntry,dsfFromImage);
      DirEntry.Name := 'HELPTHEM';

      DirEntry := TDirectoryEntry.Create(FISOImage.Structure,FISOImage.Structure.RootDirectory,dsfFromImage);
      DirEntry.Name := 'HELPYOU';

      FileEntry := TFileEntry.Create(DirEntry,dsfFromLocal);
      FileEntry.Name := 'Errors.txt';
      FileEntry.SourceFileName := 'C:\Errors.txt';
    Except
      mem_DebugOut.Lines.Add('Exception: ' + Exception(ExceptObject).ClassName + ' -> ' + Exception(ExceptObject).Message);
      Raise;
      FISOImage.CloseImage;
    End;

    FISOImage.Volume_ID := 'FISH_TEST';
    FISOImage.SaveImageToDisk(1);
    ShowMessage('ISO Disk Image Saved to HD');
end;



procedure TForm1.CreateDirctory1Click(Sender: TObject);
var
        DirName : String;
        DirEntry : TDirectoryEntry;
begin
    If Assigned(TreeObj) Then
    Begin
      If ( TreeObj Is TDirectoryEntry ) Then
      Begin
        DirEntry := TDirectoryEntry(TreeObj);
        DirName := InputBox('New Dir : ','Dir : ','');
        if DirName <> '' then
        begin
           DirEntry := TDirectoryEntry.Create(FISOImage.Structure,DirEntry,dsfFromImage);
           DirEntry.Name := DirName;
        end;   
        CheckDirs1Click(nil);
      End;
    End;
end;



procedure TForm1.AddFile1Click(Sender: TObject);
var
        DirName : String;
        DirEntry : TDirectoryEntry;
        FileEntry : TFileEntry;
begin
    If Assigned(TreeObj) Then
    Begin
      If ( TreeObj Is TDirectoryEntry ) Then
      Begin
        DirEntry := TDirectoryEntry(TreeObj);
        if OpenDialog2.execute then
        begin
           FileEntry := TFileEntry.Create(DirEntry,dsfFromLocal);
           FileEntry.Name := ExtractFilename(Opendialog2.filename);
           FileEntry.SourceFileName := Opendialog2.filename;
        end;
        CheckDirs1Click(nil);
      End;
    End;
end;


procedure TForm1.DeleteDirectory1Click(Sender: TObject);
var
        DirName : String;
        DirEntry : TDirectoryEntry;
begin
    If Assigned(TreeObj) Then
    Begin
      If ( TreeObj Is TDirectoryEntry ) Then
      Begin
        DirEntry := TDirectoryEntry(TreeObj).Parent;
        DirEntry.DelDirectory(TDirectoryEntry(TreeObj));
        CheckDirs1Click(nil);
      End;
    End;
end;

end.
