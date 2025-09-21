object FormMain: TFormMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'KeyBoardCopilot'
  ClientHeight = 110
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -18
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 144
  TextHeight = 25
  object ApplicationEvents: TApplicationEvents
    OnMinimize = ApplicationEventsMinimize
    Left = 60
    Top = 24
  end
  object TrayIcon: TTrayIcon
    Left = 192
    Top = 24
  end
  object PopupMenu: TPopupMenu
    Left = 304
    Top = 24
    object MenuItemShowForm: TMenuItem
      Caption = #12458#12503#12471#12519#12531
      OnClick = MenuItemShowFormClick
    end
    object MenuItemCloseForm: TMenuItem
      Caption = #32066#20102
      OnClick = MenuItemCloseFormClick
    end
  end
end
