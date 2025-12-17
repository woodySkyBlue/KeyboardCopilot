unit UnitKeyboardCopilot;

interface

uses
  Winapi.Windows, System.Classes, System.SysUtils, System.StrUtils, System.Generics.Collections,
  UnitUtils, PsAPI;

type

  TModifierState = (tmsShiftKey, tmsCtrlKey, tmsAltKey, tmsWinKey);
  TModifierSet = set of TModifierState;

  TKeyMessageEvent = procedure(Sender: TObject; S: String) of Object;

  TKeyData = record
    ModifierSet: TModifierSet;
    KeyCode: Word;
  end;

  TKeyControlData = class
    HotKey: TKeyData;
    OperateKeyList: TList<TKeyData>;
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function GetKeyCode(AIndex: Integer): Word;
    function GetOperateModifierSet(AIndex: Integer): TModifierSet;
  end;

  TApplicationInfo = record
    AppName: string;
    ExeName: string;
  end;

  TKeyManager = record
  private
    FInputList: array of TInput;
  public
    procedure Add(AKey: Word; AFlg: DWORD);
    procedure KeyConvert;
  end;

  TApplicationData = class
  private
    FAppName: string;
    FExeName: string;
    FKeyManager: TKeyManager;
    FKeyControlList: TObjectList<TKeyControlData>;
    FOnMessage: TKeyMessageEvent;
    function ProcStrToModifierState(S: string): TModifierSet;
    function ProcIsRegisteredHotKey(AIndex: Integer; ACode: DWORD; ASet: TModifierSet): Boolean;
    function ProcModifierKeyToStr(const ASet: TModifierSet): string;
    procedure ProcKeyUp(AKey: Word);
    procedure ProcKeyDown(AKey: Word);
    procedure ProcModifierKeyUp(ASet: TModifierSet);
    procedure ProcModifierKeyDown(ASet: TModifierSet);
  protected
    procedure DoMessage(S: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function IsMatch(AInfo: TApplicationInfo): Boolean;
    function KeyControlCount: Integer;
    function KeyConvert(ACode: DWORD; ASet: TModifierSet): Boolean;
    procedure Add(ACommaText: string);
    property AppName: string read FAppName write FAppName;
    property ExeName: string read FExeName write FExeName;
    property OnMessage: TKeyMessageEvent read FOnMessage write FOnMessage;
  end;

  TApplicationManager = class
  private
    FApplicationList: TObjectList<TApplicationData>;
    FOnMessage: TKeyMessageEvent;
    function ProcIsRegisteredApplication(AIndex: Integer; AInfo: TApplicationInfo): Boolean;
    procedure WMOnMessage(Sender: TObject; S: string);
  protected
    procedure DoMessage(S: string); virtual;
  public
    constructor Create;
    destructor Destroy; override;
    function Count: Integer;
    function KeyConvert(AInfo: TApplicationInfo; ACode: DWORD; ASet: TModifierSet): Boolean;
    procedure LoadFromFile(const AFileName: string);
    property OnMessage: TKeyMessageEvent read FOnMessage write FOnMessage;
  end;

function ModifierSetToByte(ASet: TModifierSet): Byte;
procedure OutputDebugOnlyString(const S: string);

implementation

function ModifierSetToByte(ASet: TModifierSet): Byte;
begin
  Result := 0;
  if tmsShiftKey in ASet then Result := Result or 1;
  if tmsCtrlKey in ASet then Result := Result or 2;
  if tmsAltKey in ASet then Result := Result or 4;
  if tmsWinKey in ASet then Result := Result or 8;
end;

procedure OutputDebugOnlyString(const S: string); inline;
begin
{$IFDEF DEBUG}
  OutputDebugString(PChar(Format('%s %s', [FormatDateTime('hh:nn:ss.zzz', Now), S])));
{$ENDIF}
end;

{ TKeyControlData }

function TKeyControlData.Count: Integer;
begin
  Result := Self.OperateKeyList.Count;
end;

constructor TKeyControlData.Create;
begin
  Self.OperateKeyList := TList<TKeyData>.Create;
end;

destructor TKeyControlData.Destroy;
begin
  if Assigned(Self.OperateKeyList) then Self.OperateKeyList.Free;
  inherited;
end;

function TKeyControlData.GetKeyCode(AIndex: Integer): Word;
begin
  Result := 0;
  if (AIndex >= 0) and (AIndex < Self.OperateKeyList.Count) then
    Result := Self.OperateKeyList[AIndex].KeyCode;
end;

function TKeyControlData.GetOperateModifierSet(AIndex: Integer): TModifierSet;
begin
  Result := [];
  if (AIndex >= 0) and (AIndex < Self.OperateKeyList.Count) then
    Result := Self.OperateKeyList[AIndex].ModifierSet;
end;

{ TKeyManager }

procedure TKeyManager.Add(AKey: Word; AFlg: DWORD);
begin
  SetLength(Self.FInputList, Length(Self.FInputList)+1);
  var FIndex := Length(Self.FInputList)-1;
  Self.FInputList[FIndex].Itype := INPUT_KEYBOARD;
  Self.FInputLIst[FIndex].ki.wVk := AKey;
  Self.FInputList[FIndex].ki.dwFlags := AFlg;
end;

procedure TKeyManager.KeyConvert;
begin
  var FCount := Length(Self.FInputList);
  if FCount > 0 then begin
    SendInput(FCount, @Self.FInputList[0], SizeOf(TInput));
    SetLength(Self.FInputList, 0);
    //Finalize(Self.FInputList);
  end;
end;

{ TApplicationData }

constructor TApplicationData.Create;
begin
  Self.FKeyControlList := TObjectList<TKeyControlData>.Create;
end;

destructor TApplicationData.Destroy;
begin
  if Assigned(Self.FKeyControlList) then
    Self.FKeyControlList.Free;
  inherited;
end;

procedure TApplicationData.DoMessage(S: string);
begin
  if Assigned(FOnMessage) then FOnMessage(Self, S);
end;

function TApplicationData.ProcIsRegisteredHotKey(AIndex: Integer; ACode: DWORD; ASet: TModifierSet): Boolean;
begin
  // アプリケーションごとに登録されているHotKeyが一致しているか（True=一致）
  Result := False;
  if (AIndex >= 0) and (AIndex < Self.FKeyControlList.Count) then begin
    var F := Self.FKeyControlList[AIndex];
    Result := (ACode = F.HotKey.KeyCode) and (ASet = F.HotKey.ModifierSet);
  end;
end;

function TApplicationData.ProcStrToModifierState(S: string): TModifierSet;
begin
  Result := [];
  var FValue := StrToIntDef(S, 0);
  if (FValue and 1) <> 0 then Include(Result, tmsShiftKey);
  if (FValue and 2) <> 0 then Include(Result, tmsCtrlKey);
  if (FValue and 4) <> 0 then Include(Result, tmsAltKey);
  if (FValue and 8) <> 0 then Include(Result, tmsWinKey);
end;

procedure TApplicationData.Add(ACommaText: string);
var
  FKeyData: TKeyData;
  FKeyControlData: TKeyControlData;
begin
  //              0       ,1              ,2                  ,3              ,4               ,5
  // ACommaText : [HotKey],[ModifierState],[OperationKeyCount],[OperationKey1],[ModifierState1],[],[]....
  // ＜注意＞ ACommaTextの先頭文字が "//" の時はコメント行
  var FList := SplitString(ACommaText, ',');
  if (Copy(ACommaText, 1, 2) <> '//') and (Length(FList) >= 5) then begin
    // コメント行でなく、コントロール設定情報が最低の5項目以上あるとき
    FKeyControlData := TKeyControlData.Create;
    FKeyControlData.HotKey.KeyCode := StrToIntDef(FList[0], VK_ESCAPE);
    FKeyControlData.HotKey.ModifierSet := ProcStrToModifierState(FList[1]);
    var FSize := StrToIntDef(FList[2], 0);
    if (FSize > 0) and (Length(FList) >= FSize * 2 + 3) then begin
      for var Cnt := 0 to FSize-1 do begin
        FKeyData.KeyCode := StrToIntDef(FList[Cnt * 2 + 3], VK_ESCAPE);
        FKeyData.ModifierSet := ProcStrToModifierState(FList[Cnt * 2 + 4]);
        FKeyControlData.OperateKeyList.Add(FKeyData);
      end;
    end;
    Self.FKeyControlList.Add(FKeyControlData);
  end;
end;

function TApplicationData.IsMatch(AInfo: TApplicationInfo): Boolean;
begin
  Result := ((Self.FAppName = '***') and (Self.FExeName = '***'))
            or (SameText(Self.FAppName, AInfo.AppName) and SameText(Self.FExeName, AInfo.ExeName));
end;

procedure TApplicationData.ProcKeyDown(AKey: Word);
begin
  if AKey > 0 then begin
    DoMessage(Format('KeyDown[%d]', [AKey]));
    Self.FKeyManager.Add(AKey, 0);
  end;
end;

procedure TApplicationData.ProcKeyUp(AKey: Word);
begin
  if AKey > 0 then begin
    DoMessage(Format('KeyUp[%d]', [AKey]));
    Self.FKeyManager.Add(AKey, KEYEVENTF_KEYUP);
  end;
end;

procedure TApplicationData.ProcModifierKeyUp(ASet: TModifierSet);
begin
  DoMessage(Format('ModifierKeyUp[%s]', [ProcModifierKeyToStr(ASet)]));
  if tmsShiftKey in ASet then Self.FKeyManager.Add(VK_SHIFT, KEYEVENTF_KEYUP);
  if tmsCtrlKey in ASet then Self.FKeyManager.Add(VK_CONTROL, KEYEVENTF_KEYUP);
  if tmsAltKey in ASet then Self.FKeyManager.Add(VK_MENU, KEYEVENTF_KEYUP);
  if tmsWinKey in ASet then Self.FKeyManager.Add(VK_LWIN, KEYEVENTF_KEYUP);
end;

procedure TApplicationData.ProcModifierKeyDown(ASet: TModifierSet);
begin
  DoMessage(Format('ModifierKeyDown[%s]', [ProcModifierKeyToStr(ASet)]));
  if tmsShiftKey in ASet then Self.FKeyManager.Add(VK_SHIFT, 0);
  if tmsCtrlKey in ASet then Self.FKeyManager.Add(VK_CONTROL, 0);
  if tmsAltKey in ASet then Self.FKeyManager.Add(VK_MENU, 0);
  if tmsWinKey in ASet then Self.FKeyManager.Add(VK_LWIN, 0);
end;

function TApplicationData.ProcModifierKeyToStr(const ASet: TModifierSet): string;
begin
  var SData := '';
  if tmsShiftKey in ASet then SData := SData + 'Shift,';
  if tmsCtrlKey in ASet then SData := SData + 'Ctrl,';
  if tmsAltKey in ASet then SData := SData + 'Alt,';
  if tmsWinKey in ASet then SData := SData + 'Win,';
  if SData <> '' then SetLength(SData, Length(SData) - 1); // 末尾のカンマを削除
  Result := '[' + SData + ']';
end;

function TApplicationData.KeyConvert(ACode: DWORD; ASet: TModifierSet): Boolean;
begin
  Result := False;
  for var Cnt := 0 to Self.FKeyControlList.Count-1 do begin
    if ProcIsRegisteredHotKey(Cnt, ACode, ASet) then begin
      // Hotキーが一致しているとき
      ProcModifierKeyUp(ASet);
      var FKeyControlData := Self.FKeyControlList[Cnt];
      for var i := 0 to FKeyControlData.Count-1 do begin
        var FSet := FKeyControlData.GetOperateModifierSet(i);
        var FKey := FKeyControlData.GetKeyCode(i);
        ProcModifierKeyDown(FSet);
        ProcKeyDown(FKey);
        ProcKeyUp(FKey);
        ProcModifierKeyUp(FSet);
      end;
      ProcModifierKeyDown(ASet);
      // 仮想キーコード操作
      Self.FKeyManager.KeyConvert;
      Result := True;
    end;
  end;
end;

function TApplicationData.KeyControlCount: Integer;
begin
  Result := Self.FKeyControlList.Count;
end;

{ TApplicationManager }

constructor TApplicationManager.Create;
begin
  Self.FApplicationList := TObjectList<TApplicationData>.Create;
end;

destructor TApplicationManager.Destroy;
begin
  if Assigned(Self.FApplicationList) then
    Self.FApplicationList.Free;
  inherited;
end;

procedure TApplicationManager.DoMessage(S: string);
begin
  if Assigned(FOnMessage) then FOnMessage(Self, S);
end;

procedure TApplicationManager.WMOnMessage(Sender: TObject; S: string);
begin
  DoMessage(S);
end;

function TApplicationManager.ProcIsRegisteredApplication(AIndex: Integer; AInfo: TApplicationInfo): Boolean;
begin
  Result := False;
  if (AIndex >= 0) and (AIndex < Self.Count) then
    Result := FApplicationList[AIndex].IsMatch(AInfo);
end;

function TApplicationManager.Count: Integer;
begin
  Result := Self.FApplicationList.Count;
end;

function TApplicationManager.KeyConvert(AInfo: TApplicationInfo; ACode: DWORD; ASet: TModifierSet): Boolean;
begin
  Result := False;
  for var Cnt := 0 to Self.Count-1 do begin
    if ProcIsRegisteredApplication(Cnt, AInfo) then
      // 登録アプリケーションのとき
      Result := Self.FApplicationList[Cnt].KeyConvert(ACode, ASet);
  end;
end;

procedure TApplicationManager.LoadFromFile(const AFileName: string);
begin
  // 1行目：[対象アプリケーションクラス名],[対象アプリケーションEXE名]
  //        0           ,1              ,2                  ,3              ,4               ,5
  // 2行目：[ControlKey],[ModifierState],[OperationKeyCount],[OperationKey1],[ModifierState1],[],[]....
  if FileExists(AFileName) then begin
    var FList := TStringList.Create;
    try
      FList.LoadFromFile(AFileName);
      if FList.Count >= 2 then begin
        var FData := TApplicationData.Create;
        FData.OnMessage := WMOnMessage;
        FData.AppName := Split(FList[0], 0);
        FData.ExeName := Split(FList[0], 1);
        for var Cnt := 1 to FList.Count-1 do
          FData.Add(FList[Cnt]);
        Self.FApplicationList.Add(FData);
      end;
    finally
      FList.Free;
    end;
  end;
end;

end.
