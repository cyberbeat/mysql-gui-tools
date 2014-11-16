object MyxViewEditorForm: TMyxViewEditorForm
  Left = 656
  Top = 339
  Caption = 'View Editor'
  ClientHeight = 352
  ClientWidth = 707
  Color = clBtnFace
  DefaultMonitor = dmDesktop
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object BottomPnl: TTntPanel
    Left = 0
    Top = 301
    Width = 707
    Height = 51
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      707
      51)
    object ApplyChangesBtn: TTntButton
      Left = 476
      Top = 13
      Width = 101
      Height = 25
      Action = ApplyChangesAction
      Anchors = [akTop, akRight]
      TabOrder = 0
    end
    object CancelBtn: TTntButton
      Left = 592
      Top = 13
      Width = 101
      Height = 25
      Action = CloseAction
      Anchors = [akTop, akRight]
      Cancel = True
      TabOrder = 1
    end
  end
  object HeaderPnl: TTntPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 12
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object LeftPnl: TTntPanel
    Left = 0
    Top = 12
    Width = 13
    Height = 289
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
  end
  object RightPnl: TTntPanel
    Left = 694
    Top = 12
    Width = 13
    Height = 289
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
  end
  object ViewPageControl: TTntPageControl
    Left = 13
    Top = 12
    Width = 681
    Height = 289
    ActivePage = ViewTabSheet
    Align = alClient
    TabOrder = 4
    object ViewTabSheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'View'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object ViewSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 237
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          649
          237)
        object ViewSheetBgShape: TTntShape
          Left = 0
          Top = 0
          Width = 649
          Height = 237
          Align = alClient
          Brush.Color = clBtnFace
          Pen.Color = clBtnFace
          Visible = False
        end
        object NameLbl: TTntLabel
          Left = 63
          Top = 6
          Width = 56
          Height = 13
          Alignment = taRightJustify
          Caption = 'View Name:'
        end
        object EditorIcon: TTntImage
          Left = 0
          Top = 0
          Width = 48
          Height = 48
          AutoSize = True
          Picture.Data = {
            0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000003000
            00003008060000005702F9870000000467414D410000AFC837058AE900000019
            74455874536F6674776172650041646F626520496D616765526561647971C965
            3C00000DE74944415478DAED990B7054559AC7FFFD7EA6F37E279040D88884E1
            1118253A0A0485D9B182104645C71D764A1847079DD99DD9D262591FE3AC6BA9
            CCEC80C522E0221683F4286474901048202124261020EFF79390773A8F4EFADD
            F7EE776E77DF740B44533B5BB255735227F7F6E97B4FFF7FDFF9BEEF7CB75BC2
            F33CFE3F37C9DF00EE0400894422CF7EA5E8C2F8986511F02D01F97D6C58A822
            FBD8AB6B727DA3D319D907A07FFAB0A9D16493C57D3BEA039BDEDCF0CCB17FBE
            E7109DBABF2940D8EA3F0CD4F64C2A62BE6DF1ACC599CB7E5EF0EFEBDEA753E7
            3706487DADABB6C524BB2300167297B75FDBBD7EDF8C0016EC36D5B64DAAEE08
            80BB468AB65F7DEBFB3303D87676A27642A1BD23006C1567B61FFFA7B5FB366E
            DCE89C989880C3E110200A0B0B6F0FF09B935D9F1B226333C27512AC9A230BB8
            88F7FEF33F0A5D1CE3C1F95E7B8DE5FFDAD739EF7C1CBBDE3B09E79D6378C28D
            B2568B20F46ADE1F37E5BCFBFC67870E1D72363636A2AEAE0EA3A3A3D303BC71
            BCC518163B2B332648820D0BE401E27DA27C8245515E31FEE35CC07B81631EB1
            7CC06BDF79F790193535F5085603E12A6BB54EADF88BC964FAD3FAF5EBAB692A
            F7CA952BF9F3E7CFCF0C804DDCD4E9C0E0A8DB6379DE0F0A1EDFF4774FCE6F75
            741A295293555F81F088E7FC606D560B9AAE1523D4A043424202828282A056AB
            05B7696F6FB7EFDDBB77DDC18307CBE9162B6EB1494DB9D0A7CDC6D0E85999B1
            C1126C4C538856DCF6EB6E54968DCFD88FE3E7A8B1FFBDE400E1BC9FCBB0E3F0
            601FDAAA4AB070611A66CD9A058542215C3B383888D2D252E7ABAFBEFADBDADA
            5A66F61AEAA669015EFB5393312C263133C62045F642A5E812AFBC67425BA36D
            C60061D172ECFC45E44DE27DE7939313B8742E07DF5DBE0C494949904AA5B0DB
            EDE8ECEC44454585FB8D37DED8DFD6D6564A5355506FFFDA1578E55883B00271
            B402D98BD4E20A14B4BAD13ACC096EC5DC4B10413EE0A6A39B5EB8E85CEC3468
            75F2F8A2DA8270BD14AB523537C580EFBCE4CCA788890CC5D2A54B059771B95C
            E8EAEAC2E5CB15786BD7EE2F1AAA2B3EA18F2FF789CFC8C8E04B4A4A6E1F033B
            3FAE37864625920B49F1C3C51A7105CEB5BAD032440069724405493DE2050012
            EFF613CF795E3380D3D5668407C9B0FA2E5D8068DFD16A9944DEF18358B776AD
            E0F7F4F9B0582C28BF7409BFFB281F55A5A77F3FD4557788243433F12B56AC10
            2C4F6E8513274ECCDAB061C3755F8E1101761CA9A5156000323CB6542B669A82
            16275A87DC78344D8948026056777B45B3A3D3ED15EF3DB739389CAD1EA51520
            80B4E05B5ABFEE5A09FA3BEAB196008283830500A7D389CFF28AF0E1C96A5C6F
            287DA1AD22F730091CF78967ABB479F3E6458B172F2E6F696959FFE4934F5EA0
            618B08F0F24735C690A884CCB810191E4FD74FB950B31D2D032EAC5FA816007C
            C25D5EE143631CAA9AECC21807CF2A740E5AA151489014A511F70E5FE661E7C3
            2D9F234463C3AA55ABA0D7EB050016C06EB71B39672FE1CD5DFFB5B5A9F4F891
            2D5BB658D93EA052A9B069D3A645A1A1A1E56BD6AC51523AFDF8F1C71FDF49B3
            758A00FFF261953124920068059EF8AE414C930514C0ADFD0E3CB2488B08BD5C
            B43EF377271D4B2B2C78E7B5AE1905F88AF4627C274D2A02B0A6542A85CE56A2
            A0B0A422FBD11F3C4AC3BDAB57AF766767672F92C964E5B41ACAE8E868E4E4E4
            5C7AF6D9675FA4F72B45805F7D70D5181C9998191F22C7E67B83C51C9FDF6041
            4B9F8D000CE4D772D1F24EAFFF57363AF0C951D38C0062820A91143B28B890C1
            60106B1D6669D619C4E5CB97731F7EF8E12DBB76ED8AD76AB5A5A9A9A94AEAC2
            8E6C341ACB28C5BEC4329408F0CBFD5708205E00782A2354F4DBFC3A335A7AAD
            F8FBC5A1080B528896175DC80254F77358102DC503C99E12E4EB32D5C9C24AD8
            3A8B41EE2002F82098AFB3EE8528A55A6859444484222626060482A6A6261C38
            70E0F8D1A347FF932EBF2202BCB8EFB2D1104100A10AFCE8BE303188F36B46D1
            DC63C1DA25E104A0142DEF03315176AE1FE4707794040FCE910BA2BF2E53E515
            1421841FC492254B04B7012042B0FD808DC9E572615F309BCDC218B99090A9AE
            5DBBC66DDDBA7507BD9747B7D58900CFBF572600248429F00FDF8B148BAFB395
            C368E99EC4434BA3106250DDE442262B8FE64137EE8E91E27BC90AD1EA3765AA
            E63C70963154D9E23070BD0991919182C065CB9689D6F7876081CD1ADB1F38B2
            082B2D5A5B5B71F1E2C5FAB7DF7EFB1D7A8BEDD01D22C0B3BB4B8D867006A0C4
            8F1F8C12B3D0998A4134DF3023333D9600D41ECBBBA72C3A4200AD834EDC1D2B
            C7FDC94A8FD5BF92A9DC6D0508BEF41F34A11BC5FCBDE0673D80F1F171747474
            20393919E9E9E9016EE483618D652666F9EEEE6E50FAB4EED8B1E34D02BFC8DC
            87FAA808B0F577C5C62002480C57E11F57C588F5CA99CB7D68BE3E4E13729E49
            7D1FE43DCA29E80CB17104A040C61CD59478AF9BB9BB0A11DE7E00529D1A1C91
            3B6EF4A37FF626E8E6AF437F7F3F8E1D3B26ECC62C406D364FC9C2DC85351607
            63636382F8818101EB9E3D7BF6F6F5F5B1F2E212F51B6C8144809FBC5368D487
            C567CE8A50E12799F122C097B583E8EA354F59C8EF28D4F432056008C35DB14A
            AC98ABF50AF7589EEBC84778C701C88329D7CBE430E596C2DE3F02C3F2548CA7
            6D41BFEA2EE4E7E70BE249207A7A7AC440666E64B55A1904373232D24815E951
            B27C3DBD5D459DE56D5B4029F1E3B70A0480C47035B63E9C1850F2DEAE2063C7
            A109172E368C23354E857BE6EA45CB73AD7988A87A1772B51452991496D61E38
            8627E0B2B98520D7A544A33E763D7AD4F3851AE84A632F6ADAC79018A54398C2
            DC467BFE68737373755151117B1E6079BA937A2BF57EEA765F6127023CF5DBB3
            041097392B52836D6B67FB01F0B72DC88492D8ECC485BA11A4C6ABB17CAE4100
            E09B4E21BCEC75C87927CD4D6E64770B37B8296DB92C2E386D2E0A071EAA083D
            CE691E446E7708D50421A8A20A47A6D4A2B3E4C35F0DB597557A854E501F61FE
            EE3D77DDB2987BE2F5D3465D685CE66CDAFE7FFAFD39014F5BB72AC87C6343E3
            0E14D70C615EBC16CB52423CE22FBE02A9DB0E29CD2295CB20557A7C9A6701ED
            A0389870C2657541A65140A2D7E0B47C1DCADDE9286E7042AAD0A0FBCAA79B3A
            BF3C7C8E6E7178BB93029DA732FBF6D5E8A67F3B4500B104A0C5CF1E49B9E9D1
            EF56201E001B8A2B079092A0C372493942CFEF84D44986E3DC50EA299FABE9E9
            CE9B1299555C04E0343BE0A6A592A86963D44660DFC44398085B86B357272856
            14E8AB3999D551BC2F97443BFDC54E0BB061C75F8CDA90D8CCA4681D9ECF9A17
            1003B70760C59C0D17AEF662095F8A8CF65D90D86D64690E2A9D02728D5CF07F
            5A0A48E8CF4D59C8C95C689274D1CAB88222F1FBC17B6135FC1D42E217E0B362
            4A2C1219FA6B4F657595BC3F3380AC97FE6CD40413408C1EDB1F4DBD2D802F2E
            7CE726B31DF6CA1C2C6D789BF2028927CB2AB532B23CB98E5C2A0230F7616E63
            A798E1A532708648BC3B740FACFA3998B7201D2E79383EFAFC9A30E740DDA9AC
            EE2F0FE4D6D7D70700CC9F3FFFF6003FF8F509A3DA109B994C002F64CF9F127B
            13081F00A56ECD4558C1BF82B792780A52B95202994AEA112F74892780C975EC
            66DA55493C1F1C893D23F7624C93847969CB103B3B057DC376ECFFB8589873B0
            2137ABA7ECE0CC00D6FEF2135A81180208C28B3F4C0B148D2997F11FD778C573
            56BB205E10AC9041A2F01C657404AD005B15C7B84D58052E3802EF79C5A72C60
            E2E742ADD5A3E3BA09FBFE58241868B8312FABF7D207330358F3829156203A73
            4E9C01BF78EC3BB7701F3E6035B46DA71176F135E6DC823FD3C31DF31308151E
            C709D9474210EC7DD7B8153CD5328278D33D1865E2EF4E475C528A209E5DD47E
            7D187B3F3A2F00989ACE64F557FCF7CC00563EF73101446546046B90B93C41D0
            E27319FFFD8065A778473DEEABDFC96EA61D490D99410B894A01CE41F9DDE220
            77720AF749A824E02C54AE526073FA70BCDE9E864E67346293D310169D04955A
            273E7B0F9A2670BEB441B86FB4E54CD6C0950F670670FF4F8F18D5415199BE92
            C1275CF079CE7F8CC7D17907A036B5414E0F38321D9508AD16D8546E442F4A86
            D44635D324599B4138289D32CBEB82B1ED4222CAFB345087254315140B895C45
            7349028A38DF673280A16B87670690F1CC61A38A00A6C40281309EB139411378
            57FD26EC940E63D72780EB77C2DA65C1C480150E1D87C8B949E046A96418B7D0
            66E6825B138C670AE351DEAB8186C42BF4D1245EEDF73DEAD467F9CEC75BF3B3
            862A670660A0F314EA09D4D598A6BDF3DC230F3E11DAFEDCE48803F35E7F997C
            5E05C7D5A3B0B50DC1366A8326319A56805C667C129C5ACF6F3B3E5C76BCA89A
            D5336DD47BE129C2A6FB1D8B950D0DEC5A0208281BA603A092122C9A34D465D3
            4C8E1D5BD6CEFD5962EFB9F1610B927FFE22D449F7811FC881A3FE225C2356B8
            CDE41ACCF26E25FFC2E7A35F1CC9AB605F7F3000F605D5E4D788678D950E66EA
            560208B8763A007C9376E6A9143C74A445D6F9878D03CECEFA303B2F43F2D31B
            A10A1E80E37A23AC3536C82552B8643AFEA593E6E3EF7F56CA1EFBD883471313
            F55541336D7F2D00C9D19D4F3C72BFEE468E636458EAA6AA524EC59A8AEA1EB5
            969E6FD506FEE55324FECFA567E0F95E937DBB366EDEB382EFCEFCE07FA3FFAF
            03C0DAE6131D9AC7D62C5DB57DCDEC4341D69E08056F9380FC7D40163BF6F4EE
            0B87AB9A6F54512EB80A4FFD6E9ED89B21FCB6717DE5C1FF1B806FDACEFE689E
            7064AB40071D3C413F2F48AB4E305B6C2C7658FDDE47BD85E9A56E21682EE301
            CF2F575D0FECBF3300FC20D8772241F02400F6AB08CB1AECA18305A183C40B93
            DF31007762FB1BC0B7DDFE0744C066CA1F7702E70000000049454E44AE426082}
        end
        object TntLabel6: TTntLabel
          Left = 312
          Top = 6
          Width = 335
          Height = 30
          Anchors = [akTop, akRight]
          AutoSize = False
          Caption = 
            'The name of the view. It is recommended to use only alpha-numeri' +
            'c characters. Spaces should be avoided and be replaced by _'
          Transparent = True
          WordWrap = True
        end
        object TntLabel1: TTntLabel
          Left = 96
          Top = 46
          Width = 23
          Height = 13
          Alignment = taRightJustify
          Caption = 'SQL:'
        end
        object NameEd: TTntEdit
          Left = 124
          Top = 2
          Width = 171
          Height = 21
          Anchors = [akLeft, akTop, akRight]
          TabOrder = 0
          OnChange = DataChange
        end
        object SqlUce: TUniCodeEdit
          Left = 124
          Top = 44
          Width = 525
          Height = 187
          Cursor = crIBeam
          Anchors = [akLeft, akTop, akRight, akBottom]
          BevelInner = bvNone
          BevelOuter = bvNone
          CharWidth = 7
          Ctl3D = True
          Font.Charset = ANSI_CHARSET
          Font.Color = clWindowText
          Font.Height = -11
          Font.Name = 'Bitstream Vera Sans Mono'
          Font.Pitch = fpFixed
          Font.Style = []
          GutterColor = clBtnFace
          GutterWidth = 0
          HighLighter = UCESQLHighlighter
          Keystrokes = <
            item
              ShortCut = 8230
              Command = ecSelectUp
            end
            item
              ShortCut = 38
              Command = ecUp
            end
            item
              ShortCut = 16422
              Command = ecScrollUp
            end
            item
              ShortCut = 40
              Command = ecDown
            end
            item
              ShortCut = 8232
              Command = ecSelectDown
            end
            item
              ShortCut = 16424
              Command = ecScrollDown
            end
            item
              ShortCut = 37
              Command = ecLeft
            end
            item
              ShortCut = 8229
              Command = ecSelectLeft
            end
            item
              ShortCut = 16421
              Command = ecWordLeft
            end
            item
              ShortCut = 24613
              Command = ecSelectWordLeft
            end
            item
              ShortCut = 39
              Command = ecRight
            end
            item
              ShortCut = 8231
              Command = ecSelectRight
            end
            item
              ShortCut = 16423
              Command = ecWordRight
            end
            item
              ShortCut = 24615
              Command = ecSelectWordRight
            end
            item
              ShortCut = 34
              Command = ecPageDown
            end
            item
              ShortCut = 8226
              Command = ecSelectPageDown
            end
            item
              ShortCut = 16418
              Command = ecPageBottom
            end
            item
              ShortCut = 24610
              Command = ecSelectPageBottom
            end
            item
              ShortCut = 33
              Command = ecPageUp
            end
            item
              ShortCut = 8225
              Command = ecSelectPageUp
            end
            item
              ShortCut = 16417
              Command = ecPageTop
            end
            item
              ShortCut = 24609
              Command = ecSelectPageTop
            end
            item
              ShortCut = 36
              Command = ecLineStart
            end
            item
              ShortCut = 8228
              Command = ecSelectLineStart
            end
            item
              ShortCut = 16420
              Command = ecEditorTop
            end
            item
              ShortCut = 24612
              Command = ecSelectEditorTop
            end
            item
              ShortCut = 35
              Command = ecLineEnd
            end
            item
              ShortCut = 8227
              Command = ecSelectLineEnd
            end
            item
              ShortCut = 16419
              Command = ecEditorBottom
            end
            item
              ShortCut = 24611
              Command = ecSelectEditorBottom
            end
            item
              ShortCut = 45
              Command = ecToggleMode
            end
            item
              ShortCut = 16470
              Command = ecPaste
            end
            item
              ShortCut = 8237
              Command = ecPaste
            end
            item
              ShortCut = 46
              Command = ecDeleteChar
            end
            item
              ShortCut = 8
              Command = ecDeleteLastChar
            end
            item
              ShortCut = 16392
              Command = ecDeleteLastWord
            end
            item
              ShortCut = 16474
              Command = ecUndo
            end
            item
              ShortCut = 32776
              Command = ecUndo
            end
            item
              ShortCut = 24666
              Command = ecRedo
            end
            item
              ShortCut = 40968
              Command = ecRedo
            end
            item
              ShortCut = 13
              Command = ecLineBreak
            end
            item
              ShortCut = 16461
              Command = ecLineBreak
            end
            item
              ShortCut = 16449
              Command = ecSelectAll
            end
            item
              ShortCut = 16451
              Command = ecCopy
            end
            item
              ShortCut = 16429
              Command = ecCopy
            end
            item
              ShortCut = 16472
              Command = ecCut
            end
            item
              ShortCut = 8238
              Command = ecCut
            end
            item
              ShortCut = 24649
              Command = ecBlockIndent
            end
            item
              ShortCut = 16462
              Command = ecInsertLine
            end
            item
              ShortCut = 16468
              Command = ecDeleteWord
            end
            item
              ShortCut = 24661
              Command = ecBlockUnindent
            end
            item
              ShortCut = 16430
              Command = ecDelete
            end
            item
              ShortCut = 16473
              Command = ecDeleteLine
            end
            item
              ShortCut = 24665
              Command = ecDeleteEOL
            end
            item
              ShortCut = 16432
              Command = ecGotoMarker0
            end
            item
              ShortCut = 16433
              Command = ecGotoMarker1
            end
            item
              ShortCut = 16434
              Command = ecGotoMarker2
            end
            item
              ShortCut = 16435
              Command = ecGotoMarker3
            end
            item
              ShortCut = 16436
              Command = ecGotoMarker4
            end
            item
              ShortCut = 16437
              Command = ecGotoMarker5
            end
            item
              ShortCut = 16438
              Command = ecGotoMarker6
            end
            item
              ShortCut = 16439
              Command = ecGotoMarker7
            end
            item
              ShortCut = 16440
              Command = ecGotoMarker8
            end
            item
              ShortCut = 16441
              Command = ecGotoMarker9
            end
            item
              ShortCut = 24624
              Command = ecToggleMarker0
            end
            item
              ShortCut = 24625
              Command = ecToggleMarker1
            end
            item
              ShortCut = 24626
              Command = ecToggleMarker2
            end
            item
              ShortCut = 24627
              Command = ecToggleMarker3
            end
            item
              ShortCut = 24628
              Command = ecToggleMarker4
            end
            item
              ShortCut = 24629
              Command = ecToggleMarker5
            end
            item
              ShortCut = 24630
              Command = ecToggleMarker6
            end
            item
              ShortCut = 24631
              Command = ecToggleMarker7
            end
            item
              ShortCut = 24632
              Command = ecToggleMarker8
            end
            item
              ShortCut = 24633
              Command = ecToggleMarker9
            end>
          LineNumberFont.Charset = DEFAULT_CHARSET
          LineNumberFont.Color = clBlack
          LineNumberFont.Height = -8
          LineNumberFont.Name = 'Terminal'
          LineNumberFont.Style = []
          MaxUndo = 32000
          Options = [eoAutoIndent, eoAutoUnindent, eoCursorThroughTabs, eoGroupUndo, eoInserting, eoTripleClicks, eoUseUndoRedo, eoUseSyntaxHighlighting, eoWantTabs]
          ParentColor = False
          ParentCtl3D = False
          ParentFont = False
          RightMargin = -1
          ScrollBars = ssNone
          ScrollHintColor.Background = clAppWorkSpace
          ScrollHintColor.Foreground = clWhite
          ScrollHintColor.FontStyles = []
          ScrollHintColor.ForceFontStyles = False
          SelectedColor.Background = clHighlight
          SelectedColor.Foreground = clHighlightText
          SelectedColor.FontStyles = []
          SelectedColor.ForceFontStyles = False
          TabOrder = 1
          TabSize = 2
          WorkWidth = 521
        end
      end
    end
    object CommentTabSheet: TTntTabSheet
      BorderWidth = 12
      Caption = 'Comment'
      ExplicitLeft = 0
      ExplicitTop = 0
      ExplicitWidth = 0
      ExplicitHeight = 0
      object CommentSheetPnl: TTntPanel
        Left = 0
        Top = 0
        Width = 649
        Height = 237
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        DesignSize = (
          649
          237)
        object CommentsSheetBgShape: TTntShape
          Left = 0
          Top = 0
          Width = 649
          Height = 237
          Align = alClient
          Brush.Color = clBtnFace
          Pen.Color = clBtnFace
          Visible = False
        end
        object TntImage1: TTntImage
          Left = 0
          Top = 0
          Width = 48
          Height = 48
          AutoSize = True
          Picture.Data = {
            0A54504E474F626A65637489504E470D0A1A0A0000000D494844520000003000
            00003008060000005702F9870000000467414D410000AFC837058AE900000019
            74455874536F6674776172650041646F626520496D616765526561647971C965
            3C00000DE74944415478DAED990B7054559AC7FFFD7EA6F37E279040D88884E1
            1118253A0A0485D9B182104645C71D764A1847079DD99DD9D262591FE3AC6BA9
            CCEC80C522E0221683F4286474901048202124261020EFF79390773A8F4EFADD
            F7EE776E77DF740B44533B5BB255735227F7F6E97B4FFF7FDFF9BEEF7CB75BC2
            F33CFE3F37C9DF00EE0400894422CF7EA5E8C2F8986511F02D01F97D6C58A822
            FBD8AB6B727DA3D319D907A07FFAB0A9D16493C57D3BEA039BDEDCF0CCB17FBE
            E7109DBABF2940D8EA3F0CD4F64C2A62BE6DF1ACC599CB7E5EF0EFEBDEA753E7
            3706487DADABB6C524BB2300167297B75FDBBD7EDF8C0016EC36D5B64DAAEE08
            80BB468AB65F7DEBFB3303D87676A27642A1BD23006C1567B61FFFA7B5FB366E
            DCE89C989880C3E110200A0B0B6F0FF09B935D9F1B226333C27512AC9A230BB8
            88F7FEF33F0A5D1CE3C1F95E7B8DE5FFDAD739EF7C1CBBDE3B09E79D6378C28D
            B2568B20F46ADE1F37E5BCFBFC67870E1D72363636A2AEAE0EA3A3A3D303BC71
            BCC518163B2B332648820D0BE401E27DA27C8245515E31FEE35CC07B81631EB1
            7CC06BDF79F790193535F5085603E12A6BB54EADF88BC964FAD3FAF5EBAB692A
            F7CA952BF9F3E7CFCF0C804DDCD4E9C0E0A8DB6379DE0F0A1EDFF4774FCE6F75
            741A295293555F81F088E7FC606D560B9AAE1523D4A043424202828282A056AB
            05B7696F6FB7EFDDBB77DDC18307CBE9162B6EB1494DB9D0A7CDC6D0E85999B1
            C1126C4C538856DCF6EB6E54968DCFD88FE3E7A8B1FFBDE400E1BC9FCBB0E3F0
            601FDAAA4AB070611A66CD9A058542215C3B383888D2D252E7ABAFBEFADBDADA
            5A66F61AEAA669015EFB5393312C263133C62045F642A5E812AFBC67425BA36D
            C60061D172ECFC45E44DE27DE7939313B8742E07DF5DBE0C494949904AA5B0DB
            EDE8ECEC44454585FB8D37DED8DFD6D6564A5355506FFFDA1578E55883B00271
            B402D98BD4E20A14B4BAD13ACC096EC5DC4B10413EE0A6A39B5EB8E85CEC3468
            75F2F8A2DA8270BD14AB523537C580EFBCE4CCA788890CC5D2A54B059771B95C
            E8EAEAC2E5CB15786BD7EE2F1AAA2B3EA18F2FF789CFC8C8E04B4A4A6E1F033B
            3FAE37864625920B49F1C3C51A7105CEB5BAD032440069724405493DE2050012
            EFF613CF795E3380D3D5668407C9B0FA2E5D8068DFD16A9944DEF18358B776AD
            E0F7F4F9B0582C28BF7409BFFB281F55A5A77F3FD4557788243433F12B56AC10
            2C4F6E8513274ECCDAB061C3755F8E1101761CA9A5156000323CB6542B669A82
            16275A87DC78344D8948026056777B45B3A3D3ED15EF3DB739389CAD1EA51520
            80B4E05B5ABFEE5A09FA3BEAB196008283830500A7D389CFF28AF0E1C96A5C6F
            287DA1AD22F730091CF78967ABB479F3E6458B172F2E6F696959FFE4934F5EA0
            618B08F0F24735C690A884CCB810191E4FD74FB950B31D2D032EAC5FA816007C
            C25D5EE143631CAA9AECC21807CF2A740E5AA151489014A511F70E5FE661E7C3
            2D9F234463C3AA55ABA0D7EB050016C06EB71B39672FE1CD5DFFB5B5A9F4F891
            2D5BB658D93EA052A9B069D3A645A1A1A1E56BD6AC51523AFDF8F1C71FDF49B3
            758A00FFF261953124920068059EF8AE414C930514C0ADFD0E3CB2488B08BD5C
            B43EF377271D4B2B2C78E7B5AE1905F88AF4627C274D2A02B0A6542A85CE56A2
            A0B0A422FBD11F3C4AC3BDAB57AF766767672F92C964E5B41ACAE8E868E4E4E4
            5C7AF6D9675FA4F72B45805F7D70D5181C9998191F22C7E67B83C51C9FDF6041
            4B9F8D000CE4D772D1F24EAFFF57363AF0C951D38C0062820A91143B28B890C1
            60106B1D6669D619C4E5CB97731F7EF8E12DBB76ED8AD76AB5A5A9A9A94AEAC2
            8E6C341ACB28C5BEC4329408F0CBFD5708205E00782A2354F4DBFC3A335A7AAD
            F8FBC5A1080B528896175DC80254F77358102DC503C99E12E4EB32D5C9C24AD8
            3A8B41EE2002F82098AFB3EE8528A55A6859444484222626060482A6A6261C38
            70E0F8D1A347FF932EBF2202BCB8EFB2D1104100A10AFCE8BE303188F36B46D1
            DC63C1DA25E104A0142DEF03315176AE1FE4707794040FCE910BA2BF2E53E515
            1421841FC492254B04B7012042B0FD808DC9E572615F309BCDC218B99090A9AE
            5DBBC66DDDBA7507BD9747B7D58900CFBF572600248429F00FDF8B148BAFB395
            C368E99EC4434BA3106250DDE442262B8FE64137EE8E91E27BC90AD1EA3765AA
            E63C70963154D9E23070BD0991919182C065CB9689D6F7876081CD1ADB1F38B2
            082B2D5A5B5B71F1E2C5FAB7DF7EFB1D7A8BEDD01D22C0B3BB4B8D867006A0C4
            8F1F8C12B3D0998A4134DF3023333D9600D41ECBBBA72C3A4200AD834EDC1D2B
            C7FDC94A8FD5BF92A9DC6D0508BEF41F34A11BC5FCBDE0673D80F1F171747474
            20393919E9E9E9016EE483618D652666F9EEEE6E50FAB4EED8B1E34D02BFC8DC
            87FAA808B0F577C5C62002480C57E11F57C588F5CA99CB7D68BE3E4E13729E49
            7D1FE43DCA29E80CB17104A040C61CD59478AF9BB9BB0A11DE7E00529D1A1C91
            3B6EF4A37FF626E8E6AF437F7F3F8E1D3B26ECC62C406D364FC9C2DC85351607
            63636382F8818101EB9E3D7BF6F6F5F5B1F2E212F51B6C8144809FBC5368D487
            C567CE8A50E12799F122C097B583E8EA354F59C8EF28D4F432056008C35DB14A
            AC98ABF50AF7589EEBC84778C701C88329D7CBE430E596C2DE3F02C3F2548CA7
            6D41BFEA2EE4E7E70BE249207A7A7AC440666E64B55A1904373232D24815E951
            B27C3DBD5D459DE56D5B4029F1E3B70A0480C47035B63E9C1850F2DEAE2063C7
            A109172E368C23354E857BE6EA45CB73AD7988A87A1772B51452991496D61E38
            8627E0B2B98520D7A544A33E763D7AD4F3851AE84A632F6ADAC79018A54398C2
            DC467BFE68737373755151117B1E6079BA937A2BF57EEA765F6127023CF5DBB3
            041097392B52836D6B67FB01F0B72DC88492D8ECC485BA11A4C6ABB17CAE4100
            E09B4E21BCEC75C87927CD4D6E64770B37B8296DB92C2E386D2E0A071EAA083D
            CE691E446E7708D50421A8A20A47A6D4A2B3E4C35F0DB597557A854E501F61FE
            EE3D77DDB2987BE2F5D3465D685CE66CDAFE7FFAFD39014F5BB72AC87C6343E3
            0E14D70C615EBC16CB52423CE22FBE02A9DB0E29CD2295CB20557A7C9A6701ED
            A0389870C2657541A65140A2D7E0B47C1DCADDE9286E7042AAD0A0FBCAA79B3A
            BF3C7C8E6E7178BB93029DA732FBF6D5E8A67F3B4500B104A0C5CF1E49B9E9D1
            EF56201E001B8A2B079092A0C372493942CFEF84D44986E3DC50EA299FABE9E9
            CE9B1299555C04E0343BE0A6A592A86963D44660DFC44398085B86B357272856
            14E8AB3999D551BC2F97443BFDC54E0BB061C75F8CDA90D8CCA4681D9ECF9A17
            1003B70760C59C0D17AEF662095F8A8CF65D90D86D64690E2A9D02728D5CF07F
            5A0A48E8CF4D59C8C95C689274D1CAB88222F1FBC17B6135FC1D42E217E0B362
            4A2C1219FA6B4F657595BC3F3380AC97FE6CD40413408C1EDB1F4DBD2D802F2E
            7CE726B31DF6CA1C2C6D789BF2028927CB2AB532B23CB98E5C2A0230F7616E63
            A798E1A532708648BC3B740FACFA3998B7201D2E79383EFAFC9A30E740DDA9AC
            EE2F0FE4D6D7D70700CC9F3FFFF6003FF8F509A3DA109B994C002F64CF9F127B
            13081F00A56ECD4558C1BF82B792780A52B95202994AEA112F74892780C975EC
            66DA55493C1F1C893D23F7624C93847969CB103B3B057DC376ECFFB8589873B0
            2137ABA7ECE0CC00D6FEF2135A81180208C28B3F4C0B148D2997F11FD778C573
            56BB205E10AC9041A2F01C657404AD005B15C7B84D58052E3802EF79C5A72C60
            E2E742ADD5A3E3BA09FBFE58241868B8312FABF7D207330358F3829156203A73
            4E9C01BF78EC3BB7701F3E6035B46DA71176F135E6DC823FD3C31DF31308151E
            C709D9474210EC7DD7B8153CD5328278D33D1865E2EF4E475C528A209E5DD47E
            7D187B3F3A2F00989ACE64F557FCF7CC00563EF73101446546046B90B93C41D0
            E27319FFFD8065A778473DEEABDFC96EA61D490D99410B894A01CE41F9DDE220
            77720AF749A824E02C54AE526073FA70BCDE9E864E67346293D310169D04955A
            273E7B0F9A2670BEB441B86FB4E54CD6C0950F670670FF4F8F18D5415199BE92
            C1275CF079CE7F8CC7D17907A036B5414E0F38321D9508AD16D8546E442F4A86
            D44635D324599B4138289D32CBEB82B1ED4222CAFB345087254315140B895C45
            7349028A38DF673280A16B87670690F1CC61A38A00A6C40281309EB139411378
            57FD26EC940E63D72780EB77C2DA65C1C480150E1D87C8B949E046A96418B7D0
            66E6825B138C670AE351DEAB8186C42BF4D1245EEDF73DEAD467F9CEC75BF3B3
            862A670660A0F314EA09D4D598A6BDF3DC230F3E11DAFEDCE48803F35E7F997C
            5E05C7D5A3B0B50DC1366A8326319A56805C667C129C5ACF6F3B3E5C76BCA89A
            D5336DD47BE129C2A6FB1D8B950D0DEC5A0208281BA603A092122C9A34D465D3
            4C8E1D5BD6CEFD5962EFB9F1610B927FFE22D449F7811FC881A3FE225C2356B8
            CDE41ACCF26E25FFC2E7A35F1CC9AB605F7F3000F605D5E4D788678D950E66EA
            560208B8763A007C9376E6A9143C74A445D6F9878D03CECEFA303B2F43F2D31B
            A10A1E80E37A23AC3536C82552B8643AFEA593E6E3EF7F56CA1EFBD883471313
            F55541336D7F2D00C9D19D4F3C72BFEE468E636458EAA6AA524EC59A8AEA1EB5
            969E6FD506FEE55324FECFA567E0F95E937DBB366EDEB382EFCEFCE07FA3FFAF
            03C0DAE6131D9AC7D62C5DB57DCDEC4341D69E08056F9380FC7D40163BF6F4EE
            0B87AB9A6F54512EB80A4FFD6E9ED89B21FCB6717DE5C1FF1B806FDACEFE689E
            7064AB40071D3C413F2F48AB4E305B6C2C7658FDDE47BD85E9A56E21682EE301
            CF2F575D0FECBF3300FC20D8772241F02400F6AB08CB1AECA18305A183C40B93
            DF31007762FB1BC0B7DDFE0744C066CA1F7702E70000000049454E44AE426082}
        end
        object TntLabel2: TTntLabel
          Left = 65
          Top = 6
          Width = 54
          Height = 13
          Alignment = taRightJustify
          Caption = 'Comments:'
        end
        object CommentMemo: TTntMemo
          Left = 124
          Top = 2
          Width = 525
          Height = 235
          Anchors = [akLeft, akTop, akRight, akBottom]
          TabOrder = 0
          OnChange = DataChange
        end
      end
    end
  end
  object UCESQLHighlighter: TUCESQLHighlighter
    DefaultFilter = 'SQL script files (*.sql)|*.sql'
    CommentAttributes.Background = clWindow
    CommentAttributes.Foreground = clGray
    CommentAttributes.Style = [fsItalic]
    EmbeddedCommandAttributes.Background = clWindow
    EmbeddedCommandAttributes.Foreground = clWindowText
    EmbeddedCommandAttributes.Style = []
    IdentifierAttributes.Background = clWindow
    IdentifierAttributes.Foreground = clWindowText
    IdentifierAttributes.Style = []
    KeyAttributes.Background = clWindow
    KeyAttributes.Foreground = clBlue
    KeyAttributes.Style = [fsBold]
    NumberAttributes.Background = clWindow
    NumberAttributes.Foreground = clFuchsia
    NumberAttributes.Style = []
    QuotedIDAttributes.Background = clWindow
    QuotedIDAttributes.Foreground = clTeal
    QuotedIDAttributes.Style = []
    SpaceAttributes.Background = clWindow
    SpaceAttributes.Foreground = clWindowText
    SpaceAttributes.Style = []
    StringAttributes.Background = clWindow
    StringAttributes.Foreground = clPurple
    StringAttributes.Style = []
    SymbolAttributes.Background = clWindow
    SymbolAttributes.Foreground = clWindowText
    SymbolAttributes.Style = []
    SystemVariableAttributes.Background = clWindow
    SystemVariableAttributes.Foreground = clWindowText
    SystemVariableAttributes.Style = []
    UserVariableAttributes.Background = clWindow
    UserVariableAttributes.Foreground = clWindowText
    UserVariableAttributes.Style = []
    Left = 540
    Top = 16
  end
  object TntActionList1: TTntActionList
    Left = 510
    Top = 16
    object ApplyChangesAndCloseAction: TTntAction
      Caption = 'ApplyChangesAndCloseAction'
      ShortCut = 16397
      OnExecute = ApplyChangesAndCloseActionExecute
    end
    object ApplyChangesAction: TTntAction
      Caption = 'Apply Changes'
      Enabled = False
      OnExecute = ApplyChangesActionExecute
      OnUpdate = ApplyChangesActionUpdate
    end
    object DiscardChangesAction: TTntAction
      Caption = 'Discard Changes'
      Enabled = False
      OnExecute = DiscardChangesActionExecute
      OnUpdate = ApplyChangesActionUpdate
    end
    object CloseAction: TTntAction
      Caption = 'Close'
      OnExecute = CloseActionExecute
    end
  end
end
