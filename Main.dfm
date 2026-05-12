object Form4: TForm4
  Left = 0
  Top = 0
  Caption = 'Lox Interpreter'
  ClientHeight = 800
  ClientWidth = 1400
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  KeyPreview = True
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  TextHeight = 15
  object Splitter1: TSplitter
    Left = 300
    Top = 0
    Width = 5
    Height = 800
  end
  object PanelLeft: TPanel
    Left = 0
    Top = 0
    Width = 300
    Height = 800
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
    DesignSize = (
      300
      800)
    object Label2: TLabel
      Left = 8
      Top = 8
      Width = 46
      Height = 15
      Caption = 'Test Tree'
    end
    object LblStatus: TLabel
      Left = 148
      Top = 748
      Width = 35
      Height = 15
      Anchors = [akLeft, akBottom]
      Caption = 'Ready.'
    end
    object TestTree: TTreeView
      Left = 8
      Top = 28
      Width = 284
      Height = 680
      Anchors = [akLeft, akTop, akRight, akBottom]
      Indent = 19
      ReadOnly = True
      StateImages = StateImages
      TabOrder = 0
      OnClick = TestTreeClick
    end
    object BtnPopulate: TButton
      Left = 8
      Top = 714
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Refresh'
      TabOrder = 1
      OnClick = BtnPopulateClick
    end
    object BtnRunSelected: TButton
      Left = 92
      Top = 714
      Width = 80
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run Sel'
      TabOrder = 2
      OnClick = BtnRunSelectedClick
    end
    object BtnCheckAll: TButton
      Left = 8
      Top = 744
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'All'
      TabOrder = 3
      OnClick = BtnCheckAllClick
    end
    object BtnUncheckAll: TButton
      Left = 78
      Top = 744
      Width = 66
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'None'
      TabOrder = 4
      OnClick = BtnUncheckAllClick
    end
    object BtnRunAll: TButton
      Left = 176
      Top = 714
      Width = 116
      Height = 25
      Anchors = [akLeft, akBottom]
      Caption = 'Run All'
      TabOrder = 5
      OnClick = BtnRunAllClick
    end
    object ProgressBar1: TProgressBar
      Left = 8
      Top = 775
      Width = 284
      Height = 16
      Anchors = [akLeft, akRight, akBottom]
      TabOrder = 6
    end
  end
  object PanelRight: TPanel
    Left = 305
    Top = 0
    Width = 1095
    Height = 800
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    object Splitter2: TSplitter
      Left = 0
      Top = 445
      Width = 1095
      Height = 5
      Align = alNone
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 1095
      Height = 439
      Align = alTop
      Caption = 'Panel1'
      TabOrder = 0
      object PaintBox1: TPaintBox
        Left = 481
        Top = 1
        Width = 613
        Height = 437
        Align = alClient
        ExplicitLeft = 608
        ExplicitWidth = 486
      end
      object Memo1: TSynEdit
        Left = 1
        Top = 1
        Width = 480
        Height = 437
        Align = alLeft
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        Font.Quality = fqClearTypeNatural
        TabOrder = 0
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
          'var w = canvasWidth();'
          'var h = canvasHeight();'
          ''
          '// --- Player ---'
          'var playerW = 40;'
          'var playerH = 16;'
          'var playerX = w / 2 - playerW / 2;'
          'var playerY = h - 40;'
          'var playerSpeed = 250;'
          'var leftHeld = false;'
          'var rightHeld = false;'
          ''
          '// --- Player bullet ---'
          'var bulletActive = false;'
          'var bulletX = 0;'
          'var bulletY = 0;'
          'var bulletW = 3;'
          'var bulletH = 10;'
          'var bulletSpeed = 400;'
          ''
          '// --- Invaders grid ---'
          'var cols = 8;'
          'var rows = 4;'
          'var invW = 30;'
          'var invH = 16;'
          'var invPadX = 12;'
          'var invPadY = 10;'
          'var invGridW = cols * (invW + invPadX) - invPadX;'
          ''
          '// Invader alive flags stored in a flat array: row * cols + col'
          'var invCount = cols * rows;'
          'var invAlive = newArray();'
          'var i = 0;'
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
          'var invSpeedX = 40;'
          'var invDropAmount = 14;'
          'var invMoveTimer = 0;'
          'var invMoveInterval = 0.6;  // seconds between steps'
          ''
          '// --- Enemy bullets ---'
          'var maxEnemyBullets = 4;'
          'var eBulletX = newArray();'
          'var eBulletY = newArray();'
          'var eBulletActive = newArray();'
          'var eBulletSpeed = 180;'
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
          '  bulletActive = false;'
          '}'
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
          '    if (e == "keydown:escape") running = false;'
          '    if (e == "keydown:space") {'
          '      if (!bulletActive and !gameOver) {'
          '        bulletActive = true;'
          '        bulletX = playerX + playerW / 2 - bulletW / 2;'
          '        bulletY = playerY;'
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
          '        resetPlayer();'
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
          '    // --- Update player bullet ---'
          '    if (bulletActive) {'
          '      bulletY = bulletY - bulletSpeed * dt;'
          '      if (bulletY + bulletH < 0) bulletActive = false;'
          '    }'
          ''
          '    // --- Invader movement ---'
          '    invMoveTimer = invMoveTimer + dt;'
          '    if (invMoveTimer >= invMoveInterval) {'
          '      invMoveTimer = invMoveTimer - invMoveInterval;'
          '      invOffsetX = invOffsetX + invSpeedX * invDirX;'
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
          '    // --- Bullet vs invaders collision ---'
          '    if (bulletActive) {'
          '      var r = 0;'
          '      while (r < rows) {'
          '        var c = 0;'
          '        while (c < cols) {'
          '          var idx = r * cols + c;'
          '          if (arrayGet(invAlive, idx)) {'
          '            var ix = invaderX(c);'
          '            var iy = invaderY(r);'
          
            '            if (rectsOverlap(bulletX, bulletY, bulletW, bulletH,' +
            ' ix, iy, invW, invH)) {'
          '              arraySet(invAlive, idx, false);'
          '              bulletActive = false;'
          '              aliveCount = aliveCount - 1;'
          '              score = score + 10;'
          '              // Speed up as invaders die'
          '              if (aliveCount > 0) {'
          '                invMoveInterval = 0.6 * aliveCount / invCount;'
          
            '                if (invMoveInterval < 0.05) invMoveInterval = 0.' +
            '05;'
          '              }'
          '            }'
          '          }'
          '          c = c + 1;'
          '        }'
          '        r = r + 1;'
          '      }'
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
          '        // Hit player'
          
            '        if (rectsOverlap(arrayGet(eBulletX, i) - 1, arrayGet(eBu' +
            'lletY, i), 3, 10, playerX, playerY, playerW, playerH)) {'
          '          arraySet(eBulletActive, i, false);'
          '          lives = lives - 1;'
          '          if (lives <= 0) {'
          '            gameOver = true;'
          '          } else {'
          '            resetPlayer();'
          '          }'
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
          '  // --- Draw invaders ---'
          '  var r = 0;'
          '  while (r < rows) {'
          '    var c = 0;'
          '    while (c < cols) {'
          '      if (arrayGet(invAlive, r * cols + c)) {'
          '        // Alternate row colors'
          '        if (r == 0) { setColor(255, 50, 50); }'
          '        else if (r == 1) { setColor(255, 150, 50); }'
          '        else if (r == 2) { setColor(255, 255, 50); }'
          '        else { setColor(50, 255, 50); }'
          '        fillRect(invaderX(c), invaderY(r), invW, invH);'
          '      }'
          '      c = c + 1;'
          '    }'
          '    r = r + 1;'
          '  }'
          ''
          '  // --- Draw player ---'
          '  setColor(0, 200, 255);'
          '  fillRect(playerX, playerY, playerW, playerH);'
          '  // Gun turret'
          '  fillRect(playerX + playerW / 2 - 2, playerY - 6, 4, 6);'
          ''
          '  // --- Draw player bullet ---'
          '  if (bulletActive) {'
          '    setColor(255, 255, 255);'
          '    fillRect(bulletX, bulletY, bulletW, bulletH);'
          '  }'
          ''
          '  // --- Draw enemy bullets ---'
          '  setColor(255, 80, 80);'
          '  i = 0;'
          '  while (i < maxEnemyBullets) {'
          '    if (arrayGet(eBulletActive, i)) {'
          
            '      fillRect(arrayGet(eBulletX, i) - 1, arrayGet(eBulletY, i),' +
            ' 3, 10);'
          '    }'
          '    i = i + 1;'
          '  }'
          ''
          '  // --- HUD ---'
          '  setColor(255, 255, 255);'
          '  drawText(10, 8, "SCORE: " + str(score));'
          '  drawText(w - 120, 8, "LIVES: " + str(lives));'
          ''
          '  // --- Game over overlay ---'
          '  if (gameOver) {'
          '    setColor(255, 0, 0);'
          '    drawText(w / 2 - 80, h / 2 - 20, "GAME OVER");'
          '    setColor(255, 255, 255);'
          
            '    drawText(w / 2 - 100, h / 2 + 10, "ENTER to restart, ESC to ' +
            'quit");'
          '  }'
          ''
          '  present();'
          '}'
          ''
          'print "Final score: " + str(score);')
        SelectedColor.Alpha = 0.400000005960464500
      end
    end
    object Panel2: TPanel
      Left = 0
      Top = 439
      Width = 1095
      Height = 361
      Align = alClient
      Caption = 'Panel2'
      TabOrder = 1
      object Memo2: TMemo
        Left = 1
        Top = 42
        Width = 1093
        Height = 318
        Align = alClient
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -13
        Font.Name = 'Consolas'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 1
        Top = 1
        Width = 1093
        Height = 41
        Align = alTop
        Caption = 'Panel3'
        TabOrder = 1
        DesignSize = (
          1093
          41)
        object Label1: TLabel
          Left = 8
          Top = 8
          Width = 53
          Height = 15
          Caption = 'Script Pad'
        end
        object Label3: TLabel
          Left = 8
          Top = -273
          Width = 38
          Height = 15
          Anchors = [akLeft, akBottom]
          Caption = 'Output'
          ExplicitTop = 486
        end
        object Button1: TButton
          Left = 1
          Top = 6
          Width = 120
          Height = 25
          Anchors = [akLeft, akBottom]
          Caption = 'Run Script (F5)'
          TabOrder = 0
          OnClick = Button1Click
        end
        object Button2: TButton
          Left = 127
          Top = 6
          Width = 75
          Height = 25
          Caption = 'BtnTestClick'
          TabOrder = 1
          OnClick = Button2Click
        end
        object Button3: TButton
          Left = 208
          Top = 5
          Width = 75
          Height = 25
          Caption = 'Button3'
          TabOrder = 2
          OnClick = Button3Click
        end
      end
    end
  end
  object StateImages: TImageList
    Left = 200
    Top = 40
  end
end
