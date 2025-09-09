program Project1;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  BurnUnit in 'BurnUnit.pas' {BurnForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TBurnForm, BurnForm);
  Application.Run;
end.
