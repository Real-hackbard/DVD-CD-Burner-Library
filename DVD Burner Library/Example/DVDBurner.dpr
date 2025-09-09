program DVDBurner;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  EraseFrm in 'EraseFrm.pas' {Form2},
  Options in 'Options.pas' {Form3},
  WriteFrm in 'WriteFrm.pas' {Form4};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := '';
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.CreateForm(TForm3, Form3);
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
