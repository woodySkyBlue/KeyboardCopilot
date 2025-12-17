object FormMain: TFormMain
  Left = 0
  Top = 0
  Margins.Left = 5
  Margins.Top = 5
  Margins.Right = 5
  Margins.Bottom = 5
  Caption = 'KeyBoardCopilot'
  ClientHeight = 304
  ClientWidth = 482
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
  DesignSize = (
    482
    304)
  TextHeight = 25
  object MemoMessage: TMemo
    Left = 0
    Top = 58
    Width = 482
    Height = 246
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object CheckBoxShowMessage: TCheckBox
    Left = 10
    Top = 16
    Width = 146
    Height = 26
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Caption = #12525#12464#35352#37682
    TabOrder = 1
  end
  object ButtonCopy: TButton
    Left = 301
    Top = 10
    Width = 173
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akTop, akRight]
    Caption = #12463#12522#12483#12503#12508#12540#12489#12395#12467#12500#12540
    TabOrder = 2
    OnClick = ButtonCopyClick
  end
  object ButtonClearMessage: TButton
    Left = 166
    Top = 10
    Width = 125
    Height = 38
    Margins.Left = 5
    Margins.Top = 5
    Margins.Right = 5
    Margins.Bottom = 5
    Anchors = [akTop, akRight]
    Caption = #12513#12483#12475#12540#12472#28040#21435
    TabOrder = 3
    OnClick = ButtonClearMessageClick
  end
  object ApplicationEvents: TApplicationEvents
    OnMinimize = ApplicationEventsMinimize
    Left = 60
    Top = 72
  end
  object TrayIcon: TTrayIcon
    Left = 192
    Top = 72
  end
  object PopupMenu: TPopupMenu
    Left = 304
    Top = 72
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
