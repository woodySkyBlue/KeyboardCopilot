program KeyboardCopilot;

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain},
  UnitKeyboardCopilot in 'UnitKeyboardCopilot.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
