unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  PsAPI, UnitKeyboardCopilot, Vcl.Menus, Vcl.ExtCtrls, Vcl.AppEvnts;

type
  TFormMain = class(TForm)
    ApplicationEvents: TApplicationEvents;
    TrayIcon: TTrayIcon;
    PopupMenu: TPopupMenu;
    MenuItemShowForm: TMenuItem;
    MenuItemCloseForm: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ApplicationEventsMinimize(Sender: TObject);
    procedure MenuItemShowFormClick(Sender: TObject);
    procedure MenuItemCloseFormClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    FApplicationManager: TApplicationManager;
    FIsClose: Boolean;
    function GetForgroundApplication: TApplicationInfo;
    function GetGetModifierSet: TModifierSet;
  public
    procedure ProcTrayIconMessage(S: string);
    procedure CreateParams(var Params: TCreateParams); override;
  end;

var
  FormMain: TFormMain;

implementation

{$R *.dfm}

type
  // https://learn.microsoft.com/ja-jp/windows/win32/api/winuser/ns-winuser-kbdllhookstruct
  PKBDLLHOOKSTRUCT = ^KBDLLHOOKSTRUCT;
  KBDLLHOOKSTRUCT = record
    vkCode: DWORD;   // 仮想キーコード
    scanCode: DWORD; // キーのハードウェアスキャンコード
    flags: DWORD;    // イベントフラグ
    time: DWORD;     // イベントのタイムスタンプ
    dwExtraInfo: ULONG_PTR; // 追加情報
  end;

var
  hHook: Winapi.Windows.HHOOK;

const
  LLKHF_INJECTED = $00000010;


function KeyHookProc(nCode: Integer; wParam: WPARAM; lParam: LPARAM): LRESULT; stdcall;
var
  p: PKBDLLHOOKSTRUCT;
begin
  Result := CallNextHookEx(hHook, nCode, wParam, lParam);
  // nCode が負のときはフックチェーンを介して次のフックに処理を渡す必要がある
  if (nCode < 0) then Exit;
  // lParam値をPKBDLLHOOKSTRUCT型のポインタ型に変換
  p := PKBDLLHOOKSTRUCT(lParam);
  // 自身が生成した入力は無視する（注: Injectedフラグの確認）
  if (p^.flags and LLKHF_INJECTED) <> 0 then Exit;
  if (wParam = WM_KEYDOWN) or (wParam = WM_SYSKEYDOWN) then begin
    // キーを押したとき
    // アクティブなアプリケーションの情報を取得
    var FAppInfo := FormMain.GetForgroundApplication;
    // 拡張キーコード情報を取得
    var FModifierSet := FormMain.GetGetModifierSet;
    if FormMain.FApplicationManager.KeyConvert(FAppInfo, p^.vkCode, FModifierSet) then
      Result := 1;
  end;
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  ReportMemoryLeaksOnShutdown := True;
  FormMain.BorderIcons := [TBorderIcon.biSystemMenu];
  FIsClose := False;
  PopupMenu.AutoHotkeys := maManual;
  TrayIcon.Icon := Application.Icon;
  TrayIcon.Visible := True;
  TrayIcon.PopupMenu := PopupMenu;
  TrayIcon.OnClick := MenuItemShowFormClick;
  // フックを登録（グローバルにキーボードを監視）
  if hHook = 0 then
    hHook := SetWindowsHookEx(WH_KEYBOARD_LL, @KeyHookProc, HInstance, 0);
  if hHook > 0 then begin
    FApplicationManager := TApplicationManager.Create;
    FApplicationManager.LoadFromFile('NotePad.txt');
    FApplicationManager.LoadFromFile('Delphi.txt');
    FApplicationManager.LoadFromFile('VSCode.txt');
    FApplicationManager.LoadFromFile('OneNote.txt');
    FApplicationManager.LoadFromFile('Global.txt');
  end
  else begin
    RaiseLastOSError;
  end;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  if not Self.FIsClose then begin
    CanClose := False;
    Hide;
  end;
end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  if hHook <> 0 then begin
    UnhookWindowsHookEx(hHook);
    hHook := 0;
  end;
  if Assigned(FApplicationManager) then
    FApplicationManager.Free;
end;

function TFormMain.GetForgroundApplication: TApplicationInfo;
var
  FName: array[0..255] of Char;
  FId: DWORD;
  buf: array[0..MAX_PATH - 1] of Char;
begin
  Result.AppName := '';
  Result.ExeName := '';
  // アクティブアプリケーションのハンドル取得
  var FWnd := GetForegroundWindow;
  if FWnd <> 0 then begin
    // クラス名の取得
    if GetClassName(FWnd, FName, Length(FName)) > 0 then
      Result.AppName := FName;
    // アプリケーション実行ファイル名の取得
    GetWindowThreadProcessId(FWnd, FId);
    if FId <> 0 then begin
      var FProc := OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ, False, FId);
      if FProc <> 0 then begin
        try
          if GetModuleFileNameEx(FProc, 0, buf, MAX_PATH) > 0 then
            Result.ExeName := ExtractFileName(buf);
        finally
          CloseHandle(FProc);
        end;
      end;
    end;
  end;
end;

function TFormMain.GetGetModifierSet: TModifierSet;
begin
  Result := [];
  if (GetAsyncKeyState(VK_SHIFT) and $8000) <> 0 then Include(Result, tmsShiftKey);
  if (GetAsyncKeyState(VK_CONTROL) and $8000) <> 0 then Include(Result, tmsCtrlKey);
  if (GetAsyncKeyState(VK_MENU) and $8000) <> 0 then Include(Result, tmsAltKey);
  if (GetAsyncKeyState(VK_LWIN) and $8000) <> 0 then Include(Result, tmsWinKey);
end;

procedure TFormMain.CreateParams(var Params: TCreateParams);
begin
  inherited;
  Application.ShowMainForm := False;
end;

procedure TFormMain.ProcTrayIconMessage(S: string);
begin
  TrayIcon.BalloonTitle := FormMain.Caption;
  TrayIcon.BalloonHint := S;
  TrayIcon.BalloonFlags := bfError;
  TrayIcon.ShowBalloonHint;
end;

procedure TFormMain.ApplicationEventsMinimize(Sender: TObject);
begin
  if Self.Visible then begin
    Self.Visible := False;
  end;
end;

procedure TFormMain.MenuItemCloseFormClick(Sender: TObject);
begin
  Self.FIsClose := True;
  Self.Visible := False;
  Self.Close;
end;

procedure TFormMain.MenuItemShowFormClick(Sender: TObject);
begin
  if Self.Visible = False then begin
    Self.WindowState := wsNormal;
    Self.Visible := True;
  end;
end;

end.
