program KeyboardCopilot;

uses
  Winapi.Windows,
  Vcl.Forms,
  System.SysUtils,
  Vcl.Dialogs,
  Main in 'Main.pas' {FormMain},
  UnitKeyboardCopilot in 'UnitKeyboardCopilot.pas';

{$R *.res}

//二重起動禁止処理
//http://mrxray.on.coocan.jp/Halbow/Notes/N011.html
function IsPrevAppExist(UniqueName: string): Boolean;
begin
  CreateMutex(nil, True, PWideChar(UniqueName));
  Result := (GetLastError = ERROR_ALREADY_EXISTS);
end;

var
  FUniqueName: string;

begin
  FUniqueName := 'KeyboardCopilot'; // + FormatDateTime('mmnnss', Now);
  if IsPrevAppExist(FUniqueName) then begin
    ShowMessage('KeyboardCopilotは既に起動しています。');
    Exit;
  end;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
