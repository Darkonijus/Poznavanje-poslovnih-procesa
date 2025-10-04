program MyCinema;

{$APPTYPE GUI}

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
