object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'bet'
  ClientHeight = 1035
  ClientWidth = 1997
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  WindowState = wsMaximized
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 1035
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      300
      1035)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 15
      Caption = 'Test Tree'
    end
    object LblStatus: TLabel
      Left = 148
      Top = 983
      Width = 35
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Ready.'
      ExplicitTop = 748
    end
    object TestTree: TTreeView
      Left = 8
      Top = 28
      Width = 284
      Height = 915
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      ReadOnly = True
      StateImages = StateImages
      TabOrder = 0
      OnClick = TestTreeClick
    end
    object BtnPopulate: TButton
      Left = 8
      Top = 949
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = BtnPopulateClick
    end
    object BtnRunSelected: TButton
      Left = 92
      Top = 949
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run Sel'
      TabOrder = 2
      OnClick = BtnRunSelectedClick
    end
    object BtnCheckAll: TButton
      Left = 8
      Top = 979
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'All'
      TabOrder = 3
      OnClick = BtnCheckAllClick
    end
    object BtnUncheckAll: TButton
      Left = 78
      Top = 979
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'None'
      TabOrder = 4
      OnClick = BtnUncheckAllClick
    end
    object BtnRunAll: TButton
      Left = 176
      Top = 949
      Width = 116
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run All'
      TabOrder = 5
      OnClick = BtnRunAllClick
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 1010
      Width = 284
      Height = 16
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 6
    end
  end
  object PanelRight: TPanel
    Left = 300
    Top = 0
    Width = 1697
    Height = 1035
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object SplitterOutput: TSplitter
      Left = 0
      Top = 735
      Width = 1697
      Height = 5
      Cursor = crVSplit
      Align = alBottom
      ExplicitTop = 0
      ExplicitWidth = 480
    end
    object PanelToolbar: TPanel
      Left = 0
      Top = 0
      Width = 1697
      Height = 33
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object Button1: TButton
        Left = 6
        Top = 5
        Width = 90
        Height = 25
        Caption = 'Run'
        TabOrder = 0
        OnClick = Button1Click
      end
      object BtnRunGame: TButton
        Left = 100
        Top = 5
        Width = 90
        Height = 25
        Caption = 'Run Game'
        TabOrder = 1
        OnClick = BtnRunGameClick
      end
      object Button2: TButton
        Left = 198
        Top = 4
        Width = 75
        Height = 25
        Caption = 'Click'
        TabOrder = 2
        OnClick = Button2Click
      end
      object Button3: TButton
        Left = 278
        Top = 4
        Width = 75
        Height = 25
        Caption = 'Esc'
        TabOrder = 3
        OnClick = Button3Click
      end
    end
    object Memo1: TSynEdit
      Left = 0
      Top = 33
      Width = 1697
      Height = 702
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'Consolas'
      Font.Style = []
      Font.Quality = fqClearTypeNatural
      TabOrder = 1
      UseCodeFolding = False
      Gutter.Font.Charset = DEFAULT_CHARSET
      Gutter.Font.Color = clGray
      Gutter.Font.Height = -11
      Gutter.Font.Name = 'Consolas'
      Gutter.Font.Style = []
      Gutter.Font.Quality = fqClearTypeNatural
      Gutter.ShowLineNumbers = True
      Gutter.Bands = <
        item
          Kind = gbkMarks
          Width = 13
        end
        item
          Kind = gbkLineNumbers
        end
        item
          Kind = gbkFold
        end
        item
          Kind = gbkTrackChanges
        end
        item
          Kind = gbkMargin
          Width = 3
        end>
      Lines.Strings = (
        '// ============================================================'
        '// Space Invaders '#8212' written in Lox'
        '// ============================================================'
        ''
        '// Classic arcade cabinet aspect: tall portrait window.'
        'setCanvasSize(480, 720);'
        ''
        'var w = canvasWidth();'
        'var h = canvasHeight();'
        ''
        '// --- Sprite scale ---'
        
          '// Everything that gets drawnSpriteScaled / measured for collisi' +
          'on uses'
        
          '// this multiplier so the artwork is legible on the tall canvas.' +
          ' Bump'
        
          '// it to 3 for a chunkier feel; collision math reads from the sc' +
          'aled'
        '// values below so it stays consistent.'
        'var SPR = 2;'
        ''
        '// --- Player ---'
        
          'var playerW = 13 * SPR;   // matches rendered sprite (13x7) * SP' +
          'R'
        'var playerH = 7 * SPR;'
        'var playerX = w / 2 - playerW / 2;'
        'var playerY = h - 30;'
        'var playerSpeed = 140;'
        'var leftHeld = false;'
        'var rightHeld = false;'
        ''
        '// --- Player bullets (multiple allowed) ---'
        'var maxPlayerBullets = 3;'
        'var bulletX = newArray();'
        'var bulletY = newArray();'
        'var bulletActive = newArray();'
        'var bulletW = 3;'
        'var bulletH = 8;'
        'var bulletSpeed = 260;'
        ''
        'var i = 0;'
        'while (i < maxPlayerBullets) {'
        '  arrayPush(bulletX, 0);'
        '  arrayPush(bulletY, 0);'
        '  arrayPush(bulletActive, false);'
        '  i = i + 1;'
        '}'
        ''
        '// --- Invaders grid ---'
        'var cols = 7;'
        'var rows = 4;'
        'var invW = 12 * SPR;   // sprites are 12x10'
        'var invH = 10 * SPR;'
        'var invPadX = 6;'
        'var invPadY = 6;'
        'var invGridW = cols * (invW + invPadX) - invPadX;'
        ''
        '// Invader alive flags stored in a flat array: row * cols + col'
        'var invCount = cols * rows;'
        'var invAlive = newArray();'
        'i = 0;'
        'while (i < invCount) {'
        '  arrayPush(invAlive, true);'
        '  i = i + 1;'
        '}'
        'var aliveCount = invCount;'
        ''
        '// Invader movement'
        'var invOffsetX = (w - invGridW) / 2;'
        'var invOffsetY = 40;'
        'var invDirX = 1;'
        'var invSpeedX = 18;'
        'var invDropAmount = 12;'
        'var invMoveTimer = 0;'
        'var invMoveInterval = 0.6;  // seconds between steps'
        ''
        '// --- Enemy bullets ---'
        'var maxEnemyBullets = 4;'
        'var eBulletX = newArray();'
        'var eBulletY = newArray();'
        'var eBulletActive = newArray();'
        'var eBulletW = 3;'
        'var eBulletH = 8;'
        'var eBulletSpeed = 130;'
        'var enemyShootTimer = 0;'
        'var enemyShootInterval = 1.2;'
        ''
        'i = 0;'
        'while (i < maxEnemyBullets) {'
        '  arrayPush(eBulletX, 0);'
        '  arrayPush(eBulletY, 0);'
        '  arrayPush(eBulletActive, false);'
        '  i = i + 1;'
        '}'
        ''
        '// --- Game state ---'
        'var score = 0;'
        'var lives = 3;'
        'var running = true;'
        'var gameOver = false;'
        'var lastTime = clock();'
        ''
        '// --- Helpers ---'
        'fun rectsOverlap(ax, ay, aw, ah, bx, by, bw, bh) {'
        '  if (ax + aw <= bx) return false;'
        '  if (bx + bw <= ax) return false;'
        '  if (ay + ah <= by) return false;'
        '  if (by + bh <= ay) return false;'
        '  return true;'
        '}'
        ''
        'fun invaderX(col) {'
        '  return invOffsetX + col * (invW + invPadX);'
        '}'
        ''
        'fun invaderY(row) {'
        '  return invOffsetY + row * (invH + invPadY);'
        '}'
        ''
        'fun pickShooterColumn() {'
        
          '  // Pick a random alive invader from the bottom-most row that h' +
          'as one'
        '  var r = rows - 1;'
        '  while (r >= 0) {'
        '    // Collect alive columns in this row'
        '    var aliveCols = newArray();'
        '    var c = 0;'
        '    while (c < cols) {'
        '      if (arrayGet(invAlive, r * cols + c)) {'
        '        arrayPush(aliveCols, c);'
        '      }'
        '      c = c + 1;'
        '    }'
        '    if (arrayLen(aliveCols) > 0) {'
        '      var pick = floor(random() * arrayLen(aliveCols));'
        '      return r * cols + arrayGet(aliveCols, pick);'
        '    }'
        '    r = r - 1;'
        '  }'
        '  return -1;'
        '}'
        ''
        'fun resetPlayer() {'
        '  playerX = w / 2 - playerW / 2;'
        '  var b = 0;'
        '  while (b < maxPlayerBullets) {'
        '    arraySet(bulletActive, b, false);'
        '    b = b + 1;'
        '  }'
        '}'
        ''
        '// --- Sprites (multi-color palette) ---'
        'var invSprites = newArray();'
        'var invFrame = 0;'
        'var marchStep = 0;'
        ''
        '// Row 0 invaders '#8212' red squid, orange eyes'
        'setPaletteColor("R", 255, 50, 50);'
        'setPaletteColor("O", 255, 150, 50);'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "....RRRR...." +'
        '  "..RRRRRRRR.." +'
        '  ".RRO.RR.ORR." +'
        '  ".RRRRRRRRRR." +'
        '  "..RRRRRRRR.." +'
        '  "...RRRRRR..." +'
        '  "..RR.RR.RR.." +'
        '  ".RR..RR..RR." +'
        '  "RR...RR...RR" +'
        '  ".R........R."'
        '));'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "....RRRR...." +'
        '  "..RRRRRRRR.." +'
        '  ".RRO.RR.ORR." +'
        '  ".RRRRRRRRRR." +'
        '  "..RRRRRRRR.." +'
        '  "...RRRRRR..." +'
        '  ".RRR.RR.RRR." +'
        '  "RR...RR...RR" +'
        '  ".RR..RR..RR." +'
        '  "..R..RR..R.."'
        '));'
        ''
        '// Row 1 invaders '#8212' orange crab, yellow eyes'
        'setPaletteColor("B", 255, 150, 50);'
        'setPaletteColor("Y", 255, 255, 50);'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "....BBBB...." +'
        '  "..BBBBBBBB.." +'
        '  ".BBY.BB.YBB." +'
        '  "BBBBBBBBBBBB" +'
        '  ".BBBBBBBBBB." +'
        '  "..BBBBBBBB.." +'
        '  ".BB.BBBB.BB." +'
        '  "BB..BBBB..BB" +'
        '  ".B...BB...B." +'
        '  "B..........B"'
        '));'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "....BBBB...." +'
        '  "..BBBBBBBB.." +'
        '  ".BBY.BB.YBB." +'
        '  "BBBBBBBBBBBB" +'
        '  ".BBBBBBBBBB." +'
        '  "..BBBBBBBB.." +'
        '  "..BB.BB.BB.." +'
        '  "..B.BBBB.B.." +'
        '  ".BB..BB..BB." +'
        '  "B....BB....B"'
        '));'
        ''
        '// Row 2 invaders '#8212' yellow beetle, white eyes'
        'setPaletteColor("Y", 255, 255, 50);'
        'setPaletteColor("W", 255, 255, 255);'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  ".Y........Y." +'
        '  "..Y.YYYY.Y.." +'
        '  "..YYYYYYYY.." +'
        '  ".YYW.YY.WYY." +'
        '  ".YYYYYYYYYY." +'
        '  "YYYYYYYYYYYY" +'
        '  ".YYY.YY.YYY." +'
        '  "..YY.YY.YY.." +'
        '  "..Y..YY..Y.." +'
        '  ".Y...YY...Y."'
        '));'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  ".Y........Y." +'
        '  "..Y.YYYY.Y.." +'
        '  "..YYYYYYYY.." +'
        '  ".YYW.YY.WYY." +'
        '  ".YYYYYYYYYY." +'
        '  "YYYYYYYYYYYY" +'
        '  ".YYY.YY.YYY." +'
        '  "..YYYYYYYY.." +'
        '  ".Y...YY...Y." +'
        '  "..Y..YY..Y.."'
        '));'
        ''
        '// Row 3 invaders '#8212' green spider, lime eyes'
        'setPaletteColor("G", 50, 255, 50);'
        'setPaletteColor("L", 150, 255, 100);'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "...GGGGGG..." +'
        '  "..GGGGGGGG.." +'
        '  ".GGL.GG.LGG." +'
        '  "GGGGGGGGGGGG" +'
        '  "GGGGGGGGGGGG" +'
        '  ".GGG.GG.GGG." +'
        '  ".GG..GG..GG." +'
        '  "GG...GG...GG" +'
        '  "G....GG....G" +'
        '  ".G..G..G..G."'
        '));'
        'arrayPush(invSprites, createPaletteSprite(12, 10,'
        '  "...GGGGGG..." +'
        '  "..GGGGGGGG.." +'
        '  ".GGL.GG.LGG." +'
        '  "GGGGGGGGGGGG" +'
        '  "GGGGGGGGGGGG" +'
        '  ".GGG.GG.GGG." +'
        '  "GG...GG...GG" +'
        '  ".GG..GG..GG." +'
        '  "..GG.GG.GG.." +'
        '  ".G..G..G..G."'
        '));'
        ''
        '// Player '#8212' cyan body, white cockpit, dark blue base'
        'setPaletteColor("C", 0, 200, 255);'
        'setPaletteColor("W", 255, 255, 255);'
        'setPaletteColor("D", 0, 80, 160);'
        'var playerSprite = createPaletteSprite(13, 7,'
        '  "......W......" +'
        '  ".....CWC....." +'
        '  "...CCCCCCC..." +'
        '  "..CCCCCCCCC.." +'
        '  ".CCCCCCCCCCC." +'
        '  "DDDDDDDDDDDDD" +'
        '  "DDDDDDDDDDDDD"'
        ');'
        ''
        '// Explosion '#8212' yellow core, orange mid, red outer'
        'setPaletteColor("Y", 255, 255, 100);'
        'setPaletteColor("O", 255, 200, 50);'
        'setPaletteColor("R", 255, 100, 0);'
        'var explSprite1 = createPaletteSprite(12, 10,'
        '  "R...O..O...R" +'
        '  "..O.YYYY.O.." +'
        '  ".O.YYYYYY.O." +'
        '  "O.YYYYYYYY.O" +'
        '  "..YYYYYYYY.." +'
        '  "..YYYYYYYY.." +'
        '  "O.YYYYYYYY.O" +'
        '  ".O.YYYYYY.O." +'
        '  "..O.YYYY.O.." +'
        '  "R...O..O...R"'
        ');'
        'var explSprite2 = createPaletteSprite(12, 10,'
        '  ".R........R." +'
        '  "....O..O...." +'
        '  "..O......O.." +'
        '  "...O.YY.O..." +'
        '  "....YYYY...." +'
        '  "....YYYY...." +'
        '  "...O.YY.O..." +'
        '  "..O......O.." +'
        '  "....O..O...." +'
        '  ".R........R."'
        ');'
        ''
        '// Mystery UFO '#8212' magenta dome, red body, yellow lights'
        'setPaletteColor("M", 220, 50, 220);'
        'setPaletteColor("R", 200, 40, 40);'
        'setPaletteColor("Y", 255, 255, 0);'
        'setPaletteColor("G", 180, 180, 180);'
        'var ufoSprite = createPaletteSprite(16, 5,'
        '  "......MMMM......" +'
        '  "....MMMMMMMM...." +'
        '  "..RRRRRRRRRRRR.." +'
        '  ".RRY.RRRRRR.YRR." +'
        '  "..GGGGGGGGGGGG.."'
        ');'
        ''
        '// --- Background decoration sprites (faked tilemap layer) ---'
        '// Distant ringed planet '#8212' dim purple with ring'
        'setPaletteColor("D", 25, 15, 50);'
        'setPaletteColor("P", 45, 30, 80);'
        'setPaletteColor("L", 60, 45, 100);'
        'setPaletteColor("R", 40, 35, 70);'
        'var bgPlanet = createPaletteSprite(14, 8,'
        '  ".....DDDD....." +'
        '  "...DDPPPPDD..." +'
        '  "..DPPPPPPPPDR." +'
        '  "RRDPPPPPPPPDRR" +'
        '  "RRDPPPPPPLLDRR" +'
        '  "..DLLLLLLLLD.." +'
        '  "...DDLLLLDD..." +'
        '  ".....DDDD....."'
        ');'
        ''
        '// Small galaxy '#8212' dim blue spiral'
        'setPaletteColor("B", 20, 25, 55);'
        'setPaletteColor("C", 30, 40, 75);'
        'setPaletteColor("W", 45, 55, 90);'
        'var bgGalaxy = createPaletteSprite(7, 7,'
        '  "...B..." +'
        '  "..BBB.." +'
        '  ".BCCB.." +'
        '  "BCWWCB." +'
        '  "..BCCB." +'
        '  "..BBB.." +'
        '  "...B..."'
        ');'
        ''
        '// Distant star cluster '#8212' tiny bright dots'
        'setPaletteColor("D", 35, 35, 45);'
        'setPaletteColor("B", 50, 50, 65);'
        'var bgCluster = createPaletteSprite(6, 5,'
        '  "D....B" +'
        '  "..B..." +'
        '  ".D..D." +'
        '  "...B.." +'
        '  "B....D"'
        ');'
        ''
        '// Small crescent moon '#8212' dim grey'
        'setPaletteColor("G", 40, 40, 50);'
        'setPaletteColor("L", 55, 55, 65);'
        'var bgMoon = createPaletteSprite(6, 6,'
        '  "..GGG." +'
        '  ".GLLL." +'
        '  "GLLL.." +'
        '  "GLLL.." +'
        '  ".GLLL." +'
        '  "..GGG."'
        ');'
        ''
        '// Create flipped variants for visual variety'
        'var bgMoonFlipped = flipSprite(bgMoon, "h");'
        'var bgPlanetFlipped = flipSprite(bgPlanet, "h");'
        ''
        '// Background element positions (fixed, scattered)'
        'var bgCount = 8;'
        'var bgSprite = newArray();'
        'var bgX_pos = newArray();'
        'var bgY_pos = newArray();'
        'var bgScale = newArray();'
        ''
        '// Planet '#8212' upper right area (flipped so ring faces other way)'
        
          'arrayPush(bgSprite, bgPlanetFlipped);  arrayPush(bgX_pos, floor(' +
          'w * 0.78)); arrayPush(bgY_pos, 55);  arrayPush(bgScale, 3);'
        '// Galaxy '#8212' left mid'
        
          'arrayPush(bgSprite, bgGalaxy);  arrayPush(bgX_pos, 35);  arrayPu' +
          'sh(bgY_pos, 130); arrayPush(bgScale, 3);'
        '// Star cluster '#8212' center low'
        
          'arrayPush(bgSprite, bgCluster);  arrayPush(bgX_pos, floor(w * 0.' +
          '45)); arrayPush(bgY_pos, floor(h * 0.72)); arrayPush(bgScale, 3)' +
          ';'
        '// Moon '#8212' upper left (original, crescent faces right)'
        
          'arrayPush(bgSprite, bgMoon);  arrayPush(bgX_pos, floor(w * 0.18)' +
          '); arrayPush(bgY_pos, 40);  arrayPush(bgScale, 3);'
        '// Another galaxy '#8212' right mid'
        
          'arrayPush(bgSprite, bgGalaxy);  arrayPush(bgX_pos, floor(w * 0.8' +
          '8)); arrayPush(bgY_pos, floor(h * 0.48)); arrayPush(bgScale, 2);'
        '// Cluster '#8212' lower left'
        
          'arrayPush(bgSprite, bgCluster);  arrayPush(bgX_pos, floor(w * 0.' +
          '1)); arrayPush(bgY_pos, floor(h * 0.58)); arrayPush(bgScale, 4);'
        '// Flipped moon '#8212' lower right (crescent faces left)'
        
          'arrayPush(bgSprite, bgMoonFlipped);  arrayPush(bgX_pos, floor(w ' +
          '* 0.85)); arrayPush(bgY_pos, floor(h * 0.65)); arrayPush(bgScale' +
          ', 2);'
        '// Small planet '#8212' center-left area'
        
          'arrayPush(bgSprite, bgPlanet);  arrayPush(bgX_pos, floor(w * 0.3' +
          '5)); arrayPush(bgY_pos, floor(h * 0.38)); arrayPush(bgScale, 2);'
        ''
        'clearPalette();'
        ''
        '// --- UFO state ---'
        'var ufoActive = false;'
        'var ufoX = 0;'
        'var ufoY = 18;'
        'var ufoDir = 1;'
        'var ufoSpeed = 160;'
        'var UFO_SCALE = 3;'
        'var ufoW = 16 * UFO_SCALE;'
        'var ufoH = 5 * UFO_SCALE;'
        'var ufoTimer = 0;'
        'var ufoInterval = 8;  // seconds between spawns'
        'var ufoPoints = 50;'
        'var ufoSoundTimer = 0;'
        'var ufoSoundFlip = false;'
        ''
        '// --- Explosion tracking ---'
        'var maxExplosions = 8;'
        'var explX = newArray();'
        'var explY = newArray();'
        'var explTimer = newArray();'
        'var explActive = newArray();'
        'i = 0;'
        'while (i < maxExplosions) {'
        '  arrayPush(explX, 0);'
        '  arrayPush(explY, 0);'
        '  arrayPush(explTimer, 0);'
        '  arrayPush(explActive, false);'
        '  i = i + 1;'
        '}'
        'var explDuration = 0.3;'
        ''
        '// --- Starfield ---'
        'var numStars = 60;'
        'var starX = newArray();'
        'var starY = newArray();'
        'var starSpeed = newArray();'
        'var starBright = newArray();'
        'i = 0;'
        'while (i < numStars) {'
        '  arrayPush(starX, floor(random() * w));'
        '  arrayPush(starY, floor(random() * h));'
        '  var spd = 20 + random() * 60;'
        '  arrayPush(starSpeed, spd);'
        '  // Dimmer stars move slower, brighter stars move faster'
        '  arrayPush(starBright, floor(80 + spd * 2));'
        '  i = i + 1;'
        '}'
        ''
        '// --- Background music (space invader theme) ---'
        'var TRI = 1;'
        'var SAW = 2;'
        'var PULSE = 0;'
        'var bgMusic = ['
        '  // --- A: Core ostinato ---'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [98, 100, SAW], [0, 60],     // G2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [73, 100, SAW], [0, 60],     // D2'
        '  [69, 150, SAW], [0, 60],     // C#2'
        ''
        '  // --- A'#39': Bb stinger variant ---'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [98, 100, SAW], [0, 60],     // G2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [116, 100, SAW], [0, 60],    // Bb2 '#8212' tritone punch'
        '  [82, 150, SAW], [0, 60],     // E2 '#8212' snap back'
        ''
        '  // --- B: Same groove, drop to E1 octave ---'
        '  [41, 100, SAW], [0, 60],     // E1'
        '  [41, 100, SAW], [0, 60],     // E1'
        '  [41, 100, SAW], [0, 60],     // E1'
        '  [49, 100, SAW], [0, 60],     // G1'
        '  [41, 100, SAW], [0, 60],     // E1'
        '  [41, 100, SAW], [0, 60],     // E1'
        '  [55, 100, SAW], [0, 60],     // A1'
        '  [58, 150, SAW], [0, 60],     // Bb1'
        ''
        '  // --- A again ---'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [98, 100, SAW], [0, 60],     // G2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [82, 100, SAW], [0, 60],     // E2'
        '  [73, 100, SAW], [0, 60],     // D2'
        '  [69, 150, SAW], [0, 60],     // C#2'
        ''
        '  // --- C: Tighter rhythm, same key ---'
        '  [82, 70, SAW], [0, 40],      // E2 '#8212' tighter'
        '  [82, 70, SAW], [0, 40],      // E2'
        '  [98, 70, SAW], [0, 40],      // G2'
        '  [82, 70, SAW], [0, 40],      // E2'
        '  [73, 70, SAW], [0, 40],      // D2'
        '  [82, 70, SAW], [0, 40],      // E2'
        '  [116, 70, SAW], [0, 40],     // Bb2'
        '  [82, 100, SAW], [0, 80]      // E2 '#8212' breathe'
        '];'
        'var musicPlaying = false;'
        ''
        '// --- Shields (destructible barriers) ---'
        'var numShields = 4;'
        'var shieldCols = 11;'
        'var shieldRows = 8;'
        'var shieldCellSize = 3;'
        'var shieldW = shieldCols * shieldCellSize;'
        'var shieldH = shieldRows * shieldCellSize;'
        'var shieldY = playerY - 44;'
        ''
        'var shieldX_pos = newArray();'
        
          'var shieldCells = newArray();   // flat: shieldIdx * (cols*rows)' +
          ' + row*cols + col'
        'var cellsPerShield = shieldCols * shieldRows;'
        ''
        '// Compute shield X positions (evenly spaced)'
        'var totalShieldsW = numShields * shieldW;'
        'var shieldSpacing = (w - totalShieldsW) / (numShields + 1);'
        'i = 0;'
        'while (i < numShields) {'
        
          '  arrayPush(shieldX_pos, floor(shieldSpacing + i * (shieldW + sh' +
          'ieldSpacing)));'
        '  i = i + 1;'
        '}'
        ''
        
          '// Classic shape: solid rectangle with rounded top corners and a' +
          'n'
        '// arch (notch) cut out of the bottom-middle.'
        'fun shieldCellAlive(row, col) {'
        '  // Top corners chipped off'
        
          '  if (row == 0 and (col == 0 or col == shieldCols - 1)) return f' +
          'alse;'
        '  // Bottom-middle arch'
        '  if (row >= shieldRows - 3) {'
        '    if (col >= 3 and col <= shieldCols - 4) return false;'
        '  }'
        '  return true;'
        '}'
        ''
        'fun resetShields() {'
        '  var s = 0;'
        '  while (s < numShields) {'
        '    var r = 0;'
        '    while (r < shieldRows) {'
        '      var c = 0;'
        '      while (c < shieldCols) {'
        '        var idx = s * cellsPerShield + r * shieldCols + c;'
        '        arraySet(shieldCells, idx, shieldCellAlive(r, c));'
        '        c = c + 1;'
        '      }'
        '      r = r + 1;'
        '    }'
        '    s = s + 1;'
        '  }'
        '}'
        ''
        '// One-time fill so resetShields can use arraySet.'
        'i = 0;'
        'while (i < numShields * cellsPerShield) {'
        '  arrayPush(shieldCells, false);'
        '  i = i + 1;'
        '}'
        'resetShields();'
        ''
        
          '// Returns true if the bullet hit a live shield cell. Destroys o' +
          'ne cell.'
        
          '// dyDir is the bullet'#39's vertical direction: -1 = moving up (pla' +
          'yer),'
        
          '// +1 = moving down (enemy). We scan rows in the direction the b' +
          'ullet'
        '// is coming FROM so the cell facing the bullet erodes first.'
        'fun bulletHitsShield(bx, by, bw, bh, dyDir) {'
        '  var s = 0;'
        '  while (s < numShields) {'
        '    var sx = arrayGet(shieldX_pos, s);'
        
          '    if (rectsOverlap(bx, by, bw, bh, sx, shieldY, shieldW, shiel' +
          'dH)) {'
        '      var base = s * cellsPerShield;'
        '      // Player bullets travel up -> scan bottom-up.'
        '      // Enemy bullets travel down -> scan top-down.'
        '      var rStart = 0;'
        '      var rEnd = shieldRows;'
        '      var rStep = 1;'
        '      if (dyDir < 0) {'
        '        rStart = shieldRows - 1;'
        '        rEnd = -1;'
        '        rStep = -1;'
        '      }'
        '      var r = rStart;'
        '      while (r != rEnd) {'
        '        var c = 0;'
        '        while (c < shieldCols) {'
        '          var idx = base + r * shieldCols + c;'
        '          if (arrayGet(shieldCells, idx)) {'
        '            var cx = sx + c * shieldCellSize;'
        '            var cy = shieldY + r * shieldCellSize;'
        
          '            if (rectsOverlap(bx, by, bw, bh, cx, cy, shieldCellS' +
          'ize, shieldCellSize)) {'
        '              arraySet(shieldCells, idx, false);'
        '              return true;'
        '            }'
        '          }'
        '          c = c + 1;'
        '        }'
        '        r = r + rStep;'
        '      }'
        '    }'
        '    s = s + 1;'
        '  }'
        '  return false;'
        '}'
        ''
        'fun startMusic() {'
        '  playSequence(bgMusic, true);'
        '  musicPlaying = true;'
        '}'
        ''
        'fun stopMusic() {'
        '  stopSequence();'
        '  musicPlaying = false;'
        '}'
        ''
        '// Start the music'
        'startMusic();'
        ''
        '// ============================================================'
        '// Game loop'
        '// ============================================================'
        'while (running) {'
        '  processMessages();'
        '  var now = clock();'
        '  var dt = now - lastTime;'
        '  lastTime = now;'
        '  // Cap dt to avoid huge jumps'
        '  if (dt > 0.1) dt = 0.1;'
        ''
        '  // --- Input ---'
        '  while (events.hasItems()) {'
        '    var e = events.dequeue();'
        '    if (e == "keydown:left") leftHeld = true;'
        '    if (e == "keyup:left") leftHeld = false;'
        '    if (e == "keydown:right") rightHeld = true;'
        '    if (e == "keyup:right") rightHeld = false;'
        '    if (e == "keydown:escape") {'
        '      stopMusic();'
        '      running = false;'
        '    }'
        '    if (e == "keydown:space") {'
        '      if (!gameOver) {'
        '        // Find a free bullet slot'
        '        var bSlot = 0;'
        '        var fired = false;'
        '        while (bSlot < maxPlayerBullets) {'
        '          if (!arrayGet(bulletActive, bSlot) and !fired) {'
        '            arraySet(bulletActive, bSlot, true);'
        
          '            arraySet(bulletX, bSlot, playerX + playerW / 2 - bul' +
          'letW / 2);'
        '            arraySet(bulletY, bSlot, playerY);'
        '            playSweep(1200, 400, 80, 0.3);'
        '            fired = true;'
        '          }'
        '          bSlot = bSlot + 1;'
        '        }'
        '      }'
        '    }'
        '    if (e == "keydown:enter") {'
        '      if (gameOver) {'
        '        // Restart'
        '        gameOver = false;'
        '        score = 0;'
        '        lives = 3;'
        '        aliveCount = invCount;'
        '        i = 0;'
        '        while (i < invCount) {'
        '          arraySet(invAlive, i, true);'
        '          i = i + 1;'
        '        }'
        '        invOffsetX = (w - invGridW) / 2;'
        '        invOffsetY = 40;'
        '        invDirX = 1;'
        '        invSpeedX = 40;'
        '        invMoveInterval = 0.6;'
        '        invMoveTimer = 0;'
        '        marchStep = 0;'
        '        ufoActive = false;'
        '        ufoTimer = 0;'
        '        resetShields();'
        '        resetPlayer();'
        '        startMusic();'
        '      }'
        '    }'
        '  }'
        ''
        '  if (!gameOver) {'
        '    // --- Update player ---'
        '    if (leftHeld) playerX = playerX - playerSpeed * dt;'
        '    if (rightHeld) playerX = playerX + playerSpeed * dt;'
        '    if (playerX < 0) playerX = 0;'
        '    if (playerX > w - playerW) playerX = w - playerW;'
        ''
        '    // --- Update player bullets ---'
        '    i = 0;'
        '    while (i < maxPlayerBullets) {'
        '      if (arrayGet(bulletActive, i)) {'
        
          '        arraySet(bulletY, i, arrayGet(bulletY, i) - bulletSpeed ' +
          '* dt);'
        
          '        if (arrayGet(bulletY, i) + bulletH < 0) arraySet(bulletA' +
          'ctive, i, false);'
        '        // Hit a shield from below?'
        '        if (arrayGet(bulletActive, i)) {'
        
          '          if (bulletHitsShield(arrayGet(bulletX, i), arrayGet(bu' +
          'lletY, i), bulletW, bulletH, -1)) {'
        '            arraySet(bulletActive, i, false);'
        '            playNoise(120, 0.15);'
        '          }'
        '        }'
        '      }'
        '      i = i + 1;'
        '    }'
        ''
        '    // --- Invader movement ---'
        '    invMoveTimer = invMoveTimer + dt;'
        '    if (invMoveTimer >= invMoveInterval) {'
        '      invMoveTimer = invMoveTimer - invMoveInterval;'
        '      invOffsetX = invOffsetX + invSpeedX * invDirX;'
        '      invFrame = 1 - invFrame;'
        '      // 4-step mechanical march'
        '      if (marchStep == 0) playTone(60, 30, 0.25);'
        '      if (marchStep == 1) playTone(70, 30, 0.25);'
        '      if (marchStep == 2) playTone(80, 30, 0.25);'
        '      if (marchStep == 3) playTone(70, 30, 0.25);'
        
          '      marchStep = (marchStep + 1) - floor((marchStep + 1) / 4) *' +
          ' 4;'
        ''
        '      // Check edges'
        '      var hitEdge = false;'
        '      var c = 0;'
        '      while (c < cols) {'
        '        var r = 0;'
        '        while (r < rows) {'
        '          if (arrayGet(invAlive, r * cols + c)) {'
        '            var ix = invaderX(c);'
        '            if (ix < 0 or ix + invW > w) hitEdge = true;'
        '          }'
        '          r = r + 1;'
        '        }'
        '        c = c + 1;'
        '      }'
        '      if (hitEdge) {'
        '        invDirX = -invDirX;'
        '        invOffsetX = invOffsetX + invSpeedX * invDirX * 2;'
        '        invOffsetY = invOffsetY + invDropAmount;'
        '      }'
        '    }'
        ''
        '    // --- UFO spawn & movement ---'
        '    ufoTimer = ufoTimer + dt;'
        '    if (!ufoActive and ufoTimer >= ufoInterval) {'
        '      ufoActive = true;'
        '      ufoTimer = 0;'
        '      ufoInterval = 6 + random() * 10;  // randomize next spawn'
        '      if (random() < 0.5) {'
        '        ufoDir = 1;'
        '        ufoX = -ufoW;'
        '      } else {'
        '        ufoDir = -1;'
        '        ufoX = w;'
        '      }'
        '    }'
        '    if (ufoActive) {'
        '      ufoX = ufoX + ufoSpeed * ufoDir * dt;'
        '      if (ufoDir > 0 and ufoX > w + ufoW) ufoActive = false;'
        '      if (ufoDir < 0 and ufoX < -ufoW * 2) ufoActive = false;'
        '      // Warbling hum while UFO is on screen'
        '      ufoSoundTimer = ufoSoundTimer + dt;'
        '      if (ufoSoundTimer >= 0.18) {'
        '        ufoSoundTimer = 0;'
        '        if (ufoSoundFlip) {'
        '          playTone(130, 60, 0.05);'
        '        } else {'
        '          playTone(138, 60, 0.05);'
        '        }'
        '        ufoSoundFlip = !ufoSoundFlip;'
        '      }'
        '    }'
        ''
        '    // --- Bullets vs UFO collision ---'
        '    if (ufoActive) {'
        '      var bi = 0;'
        '      while (bi < maxPlayerBullets) {'
        '        if (arrayGet(bulletActive, bi)) {'
        
          '          if (rectsOverlap(arrayGet(bulletX, bi), arrayGet(bulle' +
          'tY, bi), bulletW, bulletH, ufoX, ufoY, ufoW, ufoH)) {'
        '            ufoActive = false;'
        '            arraySet(bulletActive, bi, false);'
        '            score = score + ufoPoints;'
        '            playSweep(800, 200, 150, 0.4);'
        '            // Spawn explosion at UFO position'
        '            var eSlot = 0;'
        '            while (eSlot < maxExplosions) {'
        '              if (!arrayGet(explActive, eSlot)) {'
        '                arraySet(explX, eSlot, ufoX);'
        '                arraySet(explY, eSlot, ufoY);'
        '                arraySet(explTimer, eSlot, explDuration);'
        '                arraySet(explActive, eSlot, true);'
        '                eSlot = maxExplosions;'
        '              }'
        '              eSlot = eSlot + 1;'
        '            }'
        '          }'
        '        }'
        '        bi = bi + 1;'
        '      }'
        '    }'
        ''
        '    // --- Bullets vs invaders collision ---'
        '    var bi = 0;'
        '    while (bi < maxPlayerBullets) {'
        '      if (arrayGet(bulletActive, bi)) {'
        '        var r = 0;'
        '        while (r < rows) {'
        '          var c = 0;'
        '          while (c < cols) {'
        '            var idx = r * cols + c;'
        '            if (arrayGet(invAlive, idx)) {'
        '              var ix = invaderX(c);'
        '              var iy = invaderY(r);'
        
          '              if (rectsOverlap(arrayGet(bulletX, bi), arrayGet(b' +
          'ulletY, bi), bulletW, bulletH, ix, iy, invW, invH)) {'
        '                arraySet(invAlive, idx, false);'
        '                arraySet(bulletActive, bi, false);'
        '                aliveCount = aliveCount - 1;'
        '                score = score + 10;'
        '                playNoise(180, 0.4);'
        '                // Spawn explosion'
        '                var eSlot = 0;'
        '                while (eSlot < maxExplosions) {'
        '                  if (!arrayGet(explActive, eSlot)) {'
        '                    arraySet(explX, eSlot, ix);'
        '                    arraySet(explY, eSlot, iy);'
        '                    arraySet(explTimer, eSlot, explDuration);'
        '                    arraySet(explActive, eSlot, true);'
        '                    eSlot = maxExplosions; // break'
        '                  }'
        '                  eSlot = eSlot + 1;'
        '                }'
        '                // Speed up as invaders die'
        '                if (aliveCount > 0) {'
        '                  invMoveInterval = 0.6 * aliveCount / invCount;'
        
          '                  if (invMoveInterval < 0.05) invMoveInterval = ' +
          '0.05;'
        '                }'
        '              }'
        '            }'
        '            c = c + 1;'
        '          }'
        '          r = r + 1;'
        '        }'
        '      }'
        '      bi = bi + 1;'
        '    }'
        ''
        '    // --- Enemy shooting ---'
        '    enemyShootTimer = enemyShootTimer + dt;'
        
          '    if (enemyShootTimer >= enemyShootInterval and aliveCount > 0' +
          ') {'
        '      enemyShootTimer = 0;'
        '      // Find a free bullet slot'
        '      var slot = -1;'
        '      var s = 0;'
        '      while (s < maxEnemyBullets) {'
        '        if (!arrayGet(eBulletActive, s)) {'
        '          slot = s;'
        '          s = maxEnemyBullets; // break'
        '        }'
        '        s = s + 1;'
        '      }'
        '      if (slot >= 0) {'
        '        var shooter = pickShooterColumn();'
        '        if (shooter >= 0) {'
        '          var sc = shooter - floor(shooter / cols) * cols;'
        '          var sr = floor(shooter / cols);'
        '          arraySet(eBulletX, slot, invaderX(sc) + invW / 2);'
        '          arraySet(eBulletY, slot, invaderY(sr) + invH);'
        '          arraySet(eBulletActive, slot, true);'
        '          playSweep(600, 900, 50, 0.25);'
        '        }'
        '      }'
        '    }'
        ''
        '    // --- Update enemy bullets ---'
        '    i = 0;'
        '    while (i < maxEnemyBullets) {'
        '      if (arrayGet(eBulletActive, i)) {'
        
          '        arraySet(eBulletY, i, arrayGet(eBulletY, i) + eBulletSpe' +
          'ed * dt);'
        '        // Off screen'
        '        if (arrayGet(eBulletY, i) > h) {'
        '          arraySet(eBulletActive, i, false);'
        '        }'
        '        // Hit a shield from above?'
        '        if (arrayGet(eBulletActive, i)) {'
        
          '          if (bulletHitsShield(arrayGet(eBulletX, i) - eBulletW ' +
          '/ 2, arrayGet(eBulletY, i), eBulletW, eBulletH, 1)) {'
        '            arraySet(eBulletActive, i, false);'
        '            playNoise(120, 0.15);'
        '          }'
        '        }'
        
          '        // Hit player. Use the actual rendered rectangles for bo' +
          'th.'
        '        if (arrayGet(eBulletActive, i)) {'
        
          '          if (rectsOverlap(arrayGet(eBulletX, i) - eBulletW / 2,' +
          ' arrayGet(eBulletY, i), eBulletW, eBulletH, playerX, playerY - 2' +
          ', playerW, playerH)) {'
        '            arraySet(eBulletActive, i, false);'
        '            lives = lives - 1;'
        '            playNoise(400, 0.35);'
        '            if (lives <= 0) {'
        '              gameOver = true;'
        '              stopMusic();'
        '            } else {'
        '              resetPlayer();'
        '            }'
        '          }'
        '        }'
        '      }'
        '      i = i + 1;'
        '    }'
        ''
        '    // --- Update explosions ---'
        '    i = 0;'
        '    while (i < maxExplosions) {'
        '      if (arrayGet(explActive, i)) {'
        '        arraySet(explTimer, i, arrayGet(explTimer, i) - dt);'
        '        if (arrayGet(explTimer, i) <= 0) {'
        '          arraySet(explActive, i, false);'
        '        }'
        '      }'
        '      i = i + 1;'
        '    }'
        ''
        '    // --- Invaders reach player row = game over ---'
        '    var r = 0;'
        '    while (r < rows) {'
        '      var c = 0;'
        '      while (c < cols) {'
        '        if (arrayGet(invAlive, r * cols + c)) {'
        '          if (invaderY(r) + invH >= playerY) {'
        '            gameOver = true;'
        '            stopMusic();'
        '          }'
        '        }'
        '        c = c + 1;'
        '      }'
        '      r = r + 1;'
        '    }'
        ''
        '    // --- All dead = win / new wave ---'
        '    if (aliveCount <= 0) {'
        '      // Reset invaders for a new wave'
        '      aliveCount = invCount;'
        '      i = 0;'
        '      while (i < invCount) {'
        '        arraySet(invAlive, i, true);'
        '        i = i + 1;'
        '      }'
        '      invOffsetX = (w - invGridW) / 2;'
        '      invOffsetY = 40;'
        '      invDirX = 1;'
        '      invMoveInterval = 0.6;'
        '      invMoveTimer = 0;'
        '      marchStep = 0;'
        '      // Clear enemy bullets'
        '      i = 0;'
        '      while (i < maxEnemyBullets) {'
        '        arraySet(eBulletActive, i, false);'
        '        i = i + 1;'
        '      }'
        '    }'
        '  }'
        ''
        
          '  // ===========================================================' +
          '='
        '  // Draw'
        
          '  // ===========================================================' +
          '='
        '  clearCanvas();'
        ''
        
          '  // --- Draw starfield (batched: one native call instead of 60x' +
          ' setColor+drawPixel) ---'
        '  i = 0;'
        '  while (i < numStars) {'
        
          '    arraySet(starY, i, arrayGet(starY, i) + arrayGet(starSpeed, ' +
          'i) * dt);'
        '    if (arrayGet(starY, i) >= h) {'
        '      arraySet(starY, i, 0);'
        '      arraySet(starX, i, floor(random() * w));'
        '    }'
        '    i = i + 1;'
        '  }'
        '  drawPixelsGray(starX, starY, starBright);'
        ''
        '  // --- Draw background decorations ---'
        '  i = 0;'
        '  while (i < bgCount) {'
        
          '    drawSpriteScaled(arrayGet(bgSprite, i), arrayGet(bgX_pos, i)' +
          ', arrayGet(bgY_pos, i), arrayGet(bgScale, i));'
        '    i = i + 1;'
        '  }'
        ''
        '  // --- Draw UFO ---'
        '  if (ufoActive) {'
        '    drawSpriteScaled(ufoSprite, floor(ufoX), ufoY, UFO_SCALE);'
        '  }'
        ''
        '  // --- Draw invaders ---'
        '  var r = 0;'
        '  while (r < rows) {'
        '    var c = 0;'
        '    while (c < cols) {'
        '      if (arrayGet(invAlive, r * cols + c)) {'
        
          '        drawSpriteScaled(arrayGet(invSprites, r * 2 + invFrame),' +
          ' invaderX(c), invaderY(r), SPR);'
        '      }'
        '      c = c + 1;'
        '    }'
        '    r = r + 1;'
        '  }'
        ''
        '  // --- Draw explosions ---'
        '  i = 0;'
        '  while (i < maxExplosions) {'
        '    if (arrayGet(explActive, i)) {'
        '      var t = arrayGet(explTimer, i);'
        '      if (t > explDuration / 2) {'
        
          '        drawSpriteScaled(explSprite1, arrayGet(explX, i), arrayG' +
          'et(explY, i), SPR);'
        '      } else {'
        
          '        drawSpriteScaled(explSprite2, arrayGet(explX, i), arrayG' +
          'et(explY, i), SPR);'
        '      }'
        '    }'
        '    i = i + 1;'
        '  }'
        ''
        '  // --- Draw shields (green pixel blocks) ---'
        '  setColor(60, 220, 90);'
        '  var sIdx = 0;'
        '  while (sIdx < numShields) {'
        '    var sx = arrayGet(shieldX_pos, sIdx);'
        '    var base = sIdx * cellsPerShield;'
        '    var r = 0;'
        '    while (r < shieldRows) {'
        '      var c = 0;'
        '      while (c < shieldCols) {'
        '        if (arrayGet(shieldCells, base + r * shieldCols + c)) {'
        
          '          fillRect(sx + c * shieldCellSize, shieldY + r * shield' +
          'CellSize,'
        '                   shieldCellSize, shieldCellSize);'
        '        }'
        '        c = c + 1;'
        '      }'
        '      r = r + 1;'
        '    }'
        '    sIdx = sIdx + 1;'
        '  }'
        ''
        '  // --- Draw player ---'
        
          '  drawSpriteScaled(playerSprite, floor(playerX), playerY - 2, SP' +
          'R);'
        ''
        '  // --- Draw player bullets ---'
        '  setColor(255, 255, 255);'
        '  i = 0;'
        '  while (i < maxPlayerBullets) {'
        '    if (arrayGet(bulletActive, i)) {'
        
          '      fillRect(arrayGet(bulletX, i), arrayGet(bulletY, i), bulle' +
          'tW, bulletH);'
        '    }'
        '    i = i + 1;'
        '  }'
        ''
        '  // --- Draw enemy bullets ---'
        '  setColor(255, 80, 80);'
        '  i = 0;'
        '  while (i < maxEnemyBullets) {'
        '    if (arrayGet(eBulletActive, i)) {'
        
          '      fillRect(arrayGet(eBulletX, i) - eBulletW / 2, arrayGet(eB' +
          'ulletY, i), eBulletW, eBulletH);'
        '    }'
        '    i = i + 1;'
        '  }'
        ''
        '  // --- HUD ---'
        '  setColor(255, 255, 255);'
        '  drawText(4, 4, "SCORE: " + str(score));'
        '  drawText(w - 60, 4, "LIVES: " + str(lives));'
        ''
        '  // --- Game over overlay ---'
        '  if (gameOver) {'
        '    setColor(255, 0, 0);'
        '    drawText(w / 2 - 27, h / 2 - 8, "GAME OVER");'
        '    setColor(255, 255, 255);'
        
          '    drawText(w / 2 - 78, h / 2 + 4, "ENTER to restart, ESC to qu' +
          'it");'
        '  }'
        ''
        '  present();'
        '}'
        ''
        'print "Final score: " + str(score);')
      SelectedColor.Alpha = 0.400000005960464500
    end
    object Memo2: TMemo
      Left = 0
      Top = 740
      Width = 1697
      Height = 295
      Align = alBottom
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssBoth
      TabOrder = 2
      WordWrap = False
    end
  end
  object StateImages: TImageList
    Left = 200
    Top = 40
  end
end
