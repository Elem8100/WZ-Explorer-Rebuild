object frmSearch: TfrmSearch
  Left = 0
  Top = 0
  Caption = 'Search'
  ClientHeight = 313
  ClientWidth = 578
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LVRes: TListView
    Left = 0
    Top = 72
    Width = 578
    Height = 241
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Columns = <
      item
        Caption = 'Path'
        Width = 350
      end
      item
        Caption = 'Type'
        Width = 120
      end
      item
        Caption = 'Value'
        Width = 80
      end>
    MultiSelect = True
    ReadOnly = True
    RowSelect = True
    TabOrder = 5
    ViewStyle = vsReport
    OnColumnClick = LVResColumnClick
  end
  object edtQuery: TEdit
    Left = 8
    Top = 16
    Width = 153
    Height = 21
    TabOrder = 0
    TextHint = 'Name'
  end
  object btnSearch: TButton
    Left = 167
    Top = 14
    Width = 75
    Height = 25
    Caption = 'Search'
    TabOrder = 4
    OnClick = btnSearchClick
  end
  object cbFullMatch: TCheckBox
    Left = 167
    Top = 45
    Width = 73
    Height = 17
    Caption = 'Full Match'
    TabOrder = 2
  end
  object edtVal: TEdit
    Left = 8
    Top = 43
    Width = 153
    Height = 21
    TabOrder = 1
    TextHint = 'Value (optional)'
  end
  object cbBoth: TCheckBox
    Left = 256
    Top = 45
    Width = 97
    Height = 17
    Caption = 'Match both'
    TabOrder = 3
  end
  object cbChildCount: TCheckBox
    Left = 256
    Top = 18
    Width = 121
    Height = 17
    Caption = 'Where ChildCount > '
    TabOrder = 6
  end
  object seChildCount: TSpinEdit
    Left = 382
    Top = 16
    Width = 42
    Height = 22
    MaxValue = 0
    MinValue = 0
    TabOrder = 7
    Value = 0
  end
end
