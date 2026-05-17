# Graphics Engine Specification

## Overview

A retro-style 2D software renderer targeting pixel-art games. All rendering is done to a fixed-size 32-bit BGRA bitmap via direct scanline writes. There is no GPU acceleration, no hardware blending, and no floating-point coordinate system.

**Logical resolution:** Fixed at **320 × 240** (the classic VGA / Mode-X target). All Lox-visible coordinates address this 320×240 logical surface; `canvasWidth()` and `canvasHeight()` always return `320` and `240`.
**Presentation:** The 320×240 back buffer is scaled to the host panel using the **largest integer factor** that still fits (`N` where `N*320 ≤ panelW` and `N*240 ≤ panelH`), centered, with black letterbox/pillarbox bars. Scaling uses GDI `StretchBlt` with `COLORONCOLOR` mode (nearest-neighbor) so pixel art stays crisp.
**Color depth:** 32-bit BGRA internally. Sprites use 1-bit transparency (magenta color key). Surfaces use alpha=0 as a "not drawn" flag — no blending occurs.
**Frame rate:** Capped by `DwmFlush()` after each present (typically the monitor refresh — 60 / 120 / 144 Hz). Your game loop drives simulation timing via `clock()`.

---

## Coordinate System

- Origin `(0, 0)` is the **top-left** corner.
- X increases rightward, Y increases downward.
- All coordinates are **integer pixels** — fractional values are truncated via `Trunc()`.
- Drawing outside canvas bounds is safely clipped (no crash), but no partial drawing of primitives that straddle edges (each pixel is independently clipped).

---

## Color Model

There is a single **global draw color** set by `setColor(r, g, b)`.

| Constraint | Detail |
|---|---|
| No alpha blending | There is no per-pixel opacity blending. Pixels overwrite, they never mix. |
| No per-pixel color in primitives | `drawLine`, `fillCircle`, `fillRect`, `drawPixel` all use the current global color. |
| Color range | Each channel is 0–255 integer. |
| Default color | Opaque white (`255, 255, 255`) until `setColor()` is called. |

### Transparency Model

The engine has **two distinct transparency systems**:

| Context | Mechanism | Detail |
|---|---|---|
| **Sprites** | Magenta color key | Pixels equal to `$FF00FF` are fully transparent. All others are fully opaque. 1-bit. |
| **Surfaces** | Alpha=0 skip | When composited via `drawSurface()`, pixels with alpha=0 are skipped. Non-zero alpha pixels are copied verbatim (no blending). |

These are **not** the same system. Sprites use color-key masking. Surfaces use the alpha channel as a binary "drawn / not drawn" flag. In neither case does alpha blending occur.

---

## Drawing Primitives

| Function | Algorithm | Notes |
|---|---|---|
| `drawPixel(x, y)` | Direct scanline write | Single pixel at current color |
| `drawLine(x1, y1, x2, y2)` | Bresenham's | 1px wide, no anti-aliasing, no line width option |
| `fillRect(x, y, w, h)` | Manual scanline fill | Axis-aligned only, no rotation |
| `drawRect(x, y, w, h)` | Direct scanline writes per edge | 1px outline, axis-aligned |
| `fillCircle(x, y, radius)` | Midpoint + horizontal span fill | Filled solid circle |
| `drawCircle(x, y, radius)` | Midpoint algorithm, 8-way symmetric | 1px outline only |
| `drawText(x, y, text [, scale])` | Built-in 5×7 bitmap font | Monospace, ASCII 32–126, integer scale (default 1), 6px advance |
| `measureText(text [, scale])` | Returns pixel width | `len(text) * 6 * scale`. No drawing, no canvas required. |

### What's NOT available

- No anti-aliasing on anything
- No line width/thickness control
- No Bézier curves or arcs
- No gradient fills
- No polygon fill
- No ellipse
- No rotation on primitives (only on sprites)

---

## Sprites

Sprites are immutable bitmaps stored in an indexed list. Once created, a sprite's pixels cannot be modified.

### Creating Sprites

| Function | Description |
|---|---|
| `createSprite(w, h, pixels)` | Monochrome — non-`.` chars become current color, `.` = transparent |
| `createPaletteSprite(w, h, pixels)` | Multi-color — each char maps to a palette color set by `setPaletteColor()` |
| `flipSprite(id, "h"\|"v")` | Creates a **new** sprite (flipped copy). Does not modify the original. |
| `freeSprite(id)` | Frees the sprite bitmap. ID is recycled. Drawing a freed sprite raises a runtime error. |

### Drawing Sprites

| Function | Description |
|---|---|
| `drawSprite(id, x, y)` | 1:1 pixel scale |
| `drawSpriteScaled(id, x, y, scale)` | Integer scale only (1, 2, 3…). Nearest-neighbor. |
| `drawSpriteRotated(id, x, y, scale, angle)` | Integer scale, angle in degrees. Nearest-neighbor inverse mapping. |

### Sprite Constraints

| Constraint | Detail |
|---|---|
| **Integer scale only** | No fractional scaling (no 1.5×, no 0.5×). Minimum scale is 1. |
| **No runtime pixel editing** | Once created, you can't change individual pixels. Create a new sprite instead. |
| **1-bit transparency** | Magenta (`$FF00FF`) = transparent. No semi-transparency, no alpha per pixel. |
| **No sprite sheets / atlas** | Each sprite is a separate bitmap. No sub-rect drawing. |
| **No tinting/recoloring** | You can't tint a sprite at draw time. To get a different color, create a new sprite. |
| **Rotation is expensive** | Inverse-maps every pixel in the bounding box. Large sprites at high scale cost more. |
| **No pivot point control** | Rotation always happens around the sprite center. |
| **Nearest-neighbor only** | No bilinear/bicubic filtering. Rotated sprites will have jagged edges. |
| **Freelist reuse** | Freed sprite IDs are recycled. Drawing a freed sprite raises a runtime error. |

### Palette System

```
setPaletteColor("A", 255, 0, 0);   // 'A' = red
setPaletteColor("B", 0, 255, 0);   // 'B' = green
var spr = createPaletteSprite(3, 3, "AAA.B.AAA");
```

- Up to 256 palette entries (one per ASCII character).
- `.` is always transparent (cannot be remapped).
- Undefined chars fall back to current `setColor()` value.
- Palette is global — changing it after sprite creation does NOT update existing sprites.
- `clearPalette()` resets all entries.

---

## Tilemaps

Grid-based tile rendering with scrolling support.

| Function | Description |
|---|---|
| `createTilemap(cols, rows, tileW, tileH)` | Create a tile grid. Tile size is in pixels. |
| `setTile(mapId, col, row, spriteId)` | Assign a sprite to a cell. `-1` = empty. |
| `getTile(mapId, col, row)` | Returns sprite ID at cell, or `-1`. |
| `drawTilemap(mapId, scrollX, scrollY)` | Renders visible portion with pixel-level scroll offset. |

### Tilemap Constraints

| Constraint | Detail |
|---|---|
| **No per-tile rotation/flip** | Every tile draws its sprite at 1:1, unrotated. Flip by assigning a flipped sprite ID. |
| **No tile animation** | Swap sprite IDs manually each frame for animated tiles. |
| **No multiple layers** | One tilemap = one layer. Overlay by drawing multiple tilemaps. |
| **Tiles scale to tileW×tileH** | If sprite size ≠ tile size, it's nearest-neighbor stretched. |
| **No wrapping** | Off-grid tiles are not drawn. No infinite/wrapping maps. |
| **Integer scroll only** | Scroll values are truncated to integers. |

---

## Camera

A global offset applied to all draw calls.

```
setCamera(x, y);  // shifts everything by (-x, -y)
```

| Behaviour | Detail |
|---|---|
| Affects | All drawing functions: primitives, sprites, text, tilemaps |
| Tilemap special case | Camera is **added** to scroll (effectively shifts the viewport) |
| Reset | Set to `(0, 0)` between runs; your script should reset it each frame if not shaking |
| Use case | Screen shake, camera follow |

### Camera Constraints

- No zoom (camera is pure translation, no scale).
- No rotation.
- No per-layer parallax built in (do it manually with different offsets).
- Camera does NOT affect `clearCanvas()` or `present()`.

---

## Rendering Pipeline

```
clearCanvas()          ← zeroes current render target (back buffer or surface)
  setColor(...)
  drawLine(...)        ← all drawing goes to current render target
  drawSprite(...)
  fillRect(...)
  drawTilemap(...)
present()              ← swaps back buffer → front buffer, clears new back buffer
```

### Offscreen Surfaces

You can create offscreen render targets and redirect all drawing to them. This enables pre-rendered backgrounds, HUD layers, screen transitions, and multi-pass effects.

| Function | Description |
|---|---|
| `createSurface(w, h)` | Create an offscreen bitmap (max 4096×4096). Returns surface ID. Starts transparent. |
| `setRenderTarget(id)` | Redirect all draw calls to surface `id`. Pass `-1` to target the main back buffer. |
| `drawSurface(id, x, y)` | Composite a surface onto the current render target. Skips alpha=0 pixels. |
| `freeSurface(id)` | Free the surface bitmap. ID is recycled via freelist. |

**Key behaviours:**
- `present()` always presents the main back buffer regardless of current target.
- All draw functions (`clearCanvas`, `fillRect`, `drawSprite`, `drawText`, etc.) write to whichever target is active.
- Camera offset applies to `drawSurface` position.
- Freeing the currently-targeted surface auto-resets to back buffer.
- The main back buffer is always presented as fully opaque to the display surface. Transparency (alpha=0) only has meaning during surface compositing via `drawSurface()`.

### Pipeline Constraints

| Constraint | Detail |
|---|---|
| **No layers/z-order** | Draw order = painter's algorithm. Last thing drawn is on top. |
| **No blending modes** | No additive, multiply, screen, etc. Opaque overwrites (surfaces use alpha=0 skip). |
| **Clipping is axis-aligned** | `setClipRect` restricts drawing to a rectangle; no arbitrary polygonal clips. |
| **No scrolling without redraw** | `clearCanvas()` wipes everything — you must redraw the entire scene each frame. |
| **Single back buffer** | No partial updates. Full-screen redraw every frame. |

---

## Clipping

| Function | Description |
|---|---|
| `setClipRect(x, y, w, h)` | Restrict all drawing to the given rectangle. Clamped to render target bounds. |
| `clearClipRect()` | Remove clip restriction (reset to full target). |

**Key behaviours:**
- All draw functions respect the active clip rect (pixels outside are discarded).
- `clearCanvas()` ignores the clip rect — it always clears the full target.
- `setRenderTarget()` automatically resets the clip rect to the new target's full bounds.
- The clip rect applies to the current render target only.
- Camera offset does NOT affect the clip rect (clip is in screen space).

---

## clearCanvas

| Signature | Description |
|---|---|
| `clearCanvas()` | Zero-fill the entire current render target (transparent black — alpha=0). |
| `clearCanvas(r, g, b)` | Fill the entire current render target with an opaque solid color. |

Both forms ignore the clip rect and always fill the full target.

> **Surface caveat:** `clearCanvas()` (no args) on a surface zeroes the alpha channel, so a subsequent `drawSurface()` will treat every pixel as "not drawn" until something is rendered on top. Use `clearCanvas(0, 0, 0)` if you want an opaque-black surface.

### Out-of-range characters in `drawText`

Bytes outside the printable ASCII range (32–126) are silently substituted with a space when drawn. There is no replacement glyph indicator; non-ASCII strings just appear with gaps where the unknown bytes were.

---

## Frame Presentation & VSync

The engine uses **GDI `StretchBlt`** (with `COLORONCOLOR` mode) to present the 320×240 front buffer to the host panel at an integer scale factor, followed by `DwmFlush()` to pace against the Desktop Window Manager's composition cycle.

### Conceptual separation

Three distinct timing concerns are at play:

| Concern | Description |
|---|---|
| **Simulation timing** | When game state advances (typically per frame, using `dt`). |
| **Presentation timing** | When the rendered frame is handed off to the OS for display. |
| **Scanout synchronization** | When the monitor physically draws the pixels. |

The engine controls the first two. The third is owned by DWM.

### How `present()` works today

```
present()
  ├─ Swap back buffer ↔ front buffer (pointer swap)
  ├─ ClearBuffer(new back buffer)
  ├─ GetDC(GameSurface)
  ├─ PresentScaled(DC, panelW, panelH)
  │    ├─ Compute integer scale N = min(floor(panelW/320), floor(panelH/240))
  │    ├─ Center the N×320 by N×240 destination rect
  │    ├─ PatBlt black bars (top/bottom/left/right) for letterbox/pillarbox
  │    └─ StretchBlt(front buffer → control DC, COLORONCOLOR)
  ├─ ValidateRect(GameSurface)   ← suppress redundant WM_PAINT
  └─ DwmFlush                    ← block until next compositor cycle
```

`present()` returns once DWM acknowledges the frame. This gives compositor-paced presentation without requiring a D3D/Vulkan swap chain.

### Observed consequences

### Status of common pitfalls (now handled)

| Pitfall | Resolution |\n|---|---|\n| Tearing on fast horizontal motion | `DwmFlush()` paces presentation to compositor cycles. |\n| Inconsistent frame timing | Loop is implicitly capped at refresh rate via `DwmFlush`. |\n| Thermal/CPU spikes from uncapped loop | Same \u2014 `DwmFlush` blocks until the next cycle, freeing the core. |\n| `WM_PAINT` round-trip latency | Bypassed: `present()` calls `GetDC` + `StretchBlt` directly, then `ValidateRect` to suppress redundant paints. The control's `WMPaint` handler is only used as a fallback (uncover/restore) and calls the same `PresentScaled` path under `BeginPaint`/`EndPaint`. |\n| Backward jitter from coalesced paints | Same fix \u2014 direct DC blit + `ValidateRect`. |\n\n### Remaining limitations (architectural)\n\nRegardless of effort spent within this GDI architecture:\n\n- No true flip-model presentation\n- No direct swap-chain control\n- No adaptive sync (G-Sync / FreeSync)\n- No guaranteed tear-free scanout at sub-refresh latency\n- No scanline synchronization\n\nGDI is fundamentally a composited-desktop API on modern Windows; DWM owns final presentation. `DwmFlush` is the closest practical approximation of VSync available without a D3D / Vulkan swap chain.\n\n### Possible future enhancements\n\n| Enhancement | Benefit |\n|---|---|\n| Explicit frame pacing via `QueryPerformanceCounter` | Smooths motion further; can target sub-refresh intervals for high-Hz monitors. Combines well with `DwmFlush`. |\n| Triple buffering (`displayed` / `ready` / `rendering`) | Smooths occasional render overruns. |\n| Borderless fullscreen at native resolution | More compositor-friendly than windowed; cleaner integer scaling when the host is an exact multiple of 320\u00d7240. |\n| Dirty-rectangle presentation | Lower bandwidth on largely-static scenes. Low priority at 320\u00d7240 \u2014 full-frame copies are trivially cheap. |

### Practical sweet spot for this engine

The aspirational stack that preserves the engine's identity:

```
Software rasterizer          ← unchanged
    ↓
Backbuffer in system RAM     ← unchanged
    ↓
BitBlt directly via GetDC    ← bypass WM_PAINT
    ↓
DwmFlush()                   ← compositor sync
    ↓
Fixed frame pacing (60/120)  ← QPC-based
```

This preserves:

- Software-renderer identity
- Deterministic rasterization
- Pixel-art aesthetics
- Zero GPU dependency for rendering logic

…while getting surprisingly close to smooth modern presentation.

### The hybrid alternative (not currently pursued)

Many modern "retro" engines render in software internally, then **upload the framebuffer as a texture** and present via D3D/OpenGL/Vulkan. That gives:

- Real VSync (flip-model swap chain)
- Integer scaling
- CRT shaders / filters
- Adaptive sync

…while preserving software-rendering semantics. This is the most common production pattern today, but represents a significant architectural shift.

### Current status

`present()` blits the back buffer directly via `GetDC` + `StretchBlt` (no `WM_PAINT` round-trip), calls `ValidateRect` to suppress redundant repaints, and finishes with `DwmFlush()` to pace against the compositor cycle. The `WMPaint` handler on `TLoxGameCanvas` calls the same `PresentScaled` path under `BeginPaint`/`EndPaint`, so uncover/restore repaints stay consistent with the active frame.

Effects:

- Frame rate naturally caps at the monitor refresh rate (60/120/144 Hz)
- Greatly reduced tearing and micro-jitter on motion
- CPU freed during the wait (no busy-waiting required)
- No `WM_PAINT` latency on the hot path

Not true hardware VSync — scanout timing is still owned by DWM, not the application. But for a GDI software-renderer architecture, this is the best achievable result without abandoning the design.

For scripts where smoothness matters, use **delta-time movement** (`pixels/sec × dt`) so velocity remains correct regardless of frame rate.

---

## Performance Characteristics

| Operation | Cost | Notes |
|---|---|---|
| `clearCanvas()` | O(width × height) | Memset of entire target buffer |
| `drawSprite` | O(sprite_w × sprite_h) | Per-pixel transparency check |
| `drawSpriteScaled` | O(w×scale × h×scale) | Every output pixel sampled |
| `drawSpriteRotated` | O(bounding_box_area) | Inverse map per pixel, most expensive draw call |
| `drawTilemap` | O(visible_tiles × tile_area) | Only visible tiles rendered |
| `drawLine` | O(max(dx, dy)) | One pixel per step |
| `drawRect` | O(perimeter) | Four edge scans |
| `drawCircle` | O(circumference) | 8-way symmetric pixel writes |
| `fillCircle` | O(π × r²) | Horizontal spans |
| `drawText` | O(chars × 5×7 × scale²) | Per-glyph, per-pixel |
| `drawSurface` | O(surface_w × surface_h) | Per-pixel alpha skip check |
| `present()` | Near-zero | Pointer swap + invalidate |

**CPU-bound.** All rendering is single-threaded on the main thread. Large sprites at high scale, many rotated sprites, or full-screen tilemaps will slow your frame rate.

---

## Practical Limits

| Resource | Limit |
|---|---|
| Max sprites | Unbounded (memory-limited). Freed IDs are recycled. |
| Max surfaces | Unbounded (memory-limited, max 4096×4096 each). Freed IDs are recycled. |
| Max tilemaps | Unbounded (memory-limited). |
| Sprite size | Limited by available memory. Practical max ~4096×4096. |
| Canvas size | Fixed at **320 × 240** logical pixels. Cannot be changed at runtime. Presentation scales to host panel with integer factor + letterbox. |
| Color key | Always magenta `$FF00FF`. Cannot be changed. |
| Font | Built-in 5×7 bitmap, monospace, ASCII 32–126. Integer scale 1–N. |

---

## Quick Reference

```lox
// Setup
var w = canvasWidth();
var h = canvasHeight();

// Color
setColor(r, g, b);

// Primitives
drawPixel(x, y);
drawLine(x1, y1, x2, y2);
fillRect(x, y, w, h);
drawRect(x, y, w, h);
fillCircle(x, y, radius);
drawCircle(x, y, radius);
drawText(x, y, "string");
drawText(x, y, "string", scale);
var width = measureText("string");      // or measureText("string", scale)

// Sprites
var id = createSprite(w, h, "pixel_string");
var id = createPaletteSprite(w, h, "pixel_string");
var flipped = flipSprite(id, "h");  // or "v"
drawSprite(id, x, y);
drawSpriteScaled(id, x, y, scale);
drawSpriteRotated(id, x, y, scale, angleDegrees);
freeSprite(id);

// Palette
setPaletteColor("X", r, g, b);
clearPalette();

// Tilemaps
var map = createTilemap(cols, rows, tileW, tileH);
setTile(map, col, row, spriteId);
var t = getTile(map, col, row);
drawTilemap(map, scrollX, scrollY);

// Camera
setCamera(offsetX, offsetY);

// Offscreen Surfaces
var surf = createSurface(w, h);
setRenderTarget(surf);    // draw to surface
setRenderTarget(-1);      // draw to main back buffer
drawSurface(surf, x, y); // composite surface onto current target
freeSurface(surf);

// Clipping
setClipRect(x, y, w, h); // restrict drawing to rectangle
clearClipRect();          // remove clip restriction

// Input
var held = keyHeld("left");  // true if key is currently down

// Frame
clearCanvas();            // clear to transparent black
clearCanvas(r, g, b);    // clear to solid color
// ... draw everything ...
present();
```

---

## Input

Keyboard input is available via two complementary mechanisms.

### Event Queue (edge-triggered)

```lox
while (events.hasItems()) {
  var e = events.dequeue();
  if (e == "keydown:space") jump();
  if (e == "keyup:left") stopMoving();
}
```

Events are strings: `"keydown:<name>"` or `"keyup:<name>"`. The `events` object is a native queue injected at startup.

### Key State Polling (level-triggered)

| Function | Description |
|---|---|
| `keyHeld(name)` | Returns `true` if the named key is currently held down. |

```lox
if (keyHeld("left")) player.x = player.x - 2;
if (keyHeld("right")) player.x = player.x + 2;
if (keyHeld("space")) fire();
```

### Supported Key Names

`"left"`, `"right"`, `"up"`, `"down"`, `"space"`, `"enter"`, `"escape"`, `"a"`–`"z"`

### Input Constraints

- No mouse input.
- No gamepad input.
- No key-repeat detection (use `pollEvent` for press edges, `keyHeld` for continuous state).
- Key state is reset between script runs.

---

## What You CAN'T Do (Summary)

- Semi-transparency / alpha blending (per-pixel)
- Fractional or sub-pixel positioning
- Fractional sprite scaling
- Runtime sprite pixel editing
- Custom fonts or font sizes (built-in 5×7 only, scaleable)
- GPU shaders or post-processing
- Mouse input (not yet implemented)
- Gamepad input (not yet implemented)
- Anti-aliasing
- Line width/thickness
- Bézier curves or arcs
- Gradient fills
- Polygon fill
- Ellipse
